;;; -*- Mode:Common-lisp;  Package: TV; Base:10. ; Fonts: (CPTFONT CPTFONTB HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated.  All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 10/31/86   TWE	Changed used of DEFINE-BLINKER to use MAKE-BLINKER.  Split up this
;;;			file into two parts.  The other part is SCRED2.
;;; 10/27/86   KDB	Removed some debug code from  MOUSE-SPECIFY-RECTANGLE.
;;; 10/27/86   KDB	Fixed  FOLLOWING-ARROW-BLINKER :blink method. SYSTEM:%DRAW-shaded-TRIANGLE
;;;			was called improperly.
;;; 10/16/86   TWE    Fixed MOUSE-SPECIFY-RECTANGLE to handle the middle click properly.
;;;			Fixed MOUSE-SET-WINDOW-POSITION as per bug 2243 to have it handle
;;;			a window with a superior whose upper left corner is not at (0,0).
;;; 09/08/86   KDB	Changed edit-attributes margin choice "DONE" to "Do It", 
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 08/07/86   GRH	Changed :blink to use the new primitive %draw-shaded-triangle.

;;; The screen editor

(DEFUN MOUSE-SET-SHEET-THEN-CALL (SHEET FUNCTION &REST ARGS
				  &AUX (OLD-MOUSE-SHEET MOUSE-SHEET))
  "Apply FUNCTION to ARGS with MOUSE-SHEET set to SHEET."
  (VALUES-LIST
    (UNWIND-PROTECT
      (PROGN
	(AND (NEQ MOUSE-SHEET SHEET)
	     (MOUSE-SET-SHEET SHEET))
	(MULTIPLE-VALUE-LIST (APPLY FUNCTION ARGS)))
      (AND (NEQ MOUSE-SHEET OLD-MOUSE-SHEET)
	   (MOUSE-SET-SHEET OLD-MOUSE-SHEET)))))

(DEFUN MOUSE-SPECIFY-RECTANGLE-SET-SHEET
       (&OPTIONAL LEFT TOP RIGHT BOTTOM (SHEET MOUSE-SHEET)
        (MINIMUM-WIDTH 0) (MINIMUM-HEIGHT 0) ABORTABLE)
  "Ask user to specify a rectangle, with MOUSE-SHEET set to SHEET.
The other args, and the values, are as for
MOUSE-SPECIFY-RECTANGLE."
  (MOUSE-SET-SHEET-THEN-CALL
    SHEET #'MOUSE-SPECIFY-RECTANGLE LEFT TOP RIGHT BOTTOM SHEET
    MINIMUM-WIDTH MINIMUM-HEIGHT ABORTABLE))

(DEFUN MOUSE-SPECIFY-RECTANGLE
       (&OPTIONAL LEFT TOP RIGHT BOTTOM (SHEET MOUSE-SHEET)
	(MINIMUM-WIDTH 0) (MINIMUM-HEIGHT 0) ABORTABLE
	&AUX LEFT1 TOP1 WIDTH HEIGHT BUTTON ABORT)
  "Ask user to specify a rectangle with the mouse, by clicking at two
corners.  Returns four values, the left, top, right, and bottom of the
rectangle, all relative to SHEET.

The left button puts a corner down, the right button puts it down at
the nearest 'good' place, the middle button aborts if that is possible.
Specifying a rectangle of zero or negative size instead gives the full
screen.  Our arguments are where to start the corners out: The upper
left corner goes at LEFT and TOP, or where the mouse is if they are
NIL; the lower right corner goes near the other one by default, unless
all four args are present, in which case it starts off so as to make a
rectangle congruent to the one specified by the arguments.

SHEET specifies the area within which we are allowed to act.
If ABORTABLE is T, this can return NIL."
  (AND (EQ CURRENT-PROCESS MOUSE-PROCESS)
       (FERROR NIL "MOUSE-SPECIFY-RECTANGLE cannot be called in the mouse process"))
  (OR (SHEET-ME-OR-MY-KID-P SHEET MOUSE-SHEET)
      (FERROR NIL "MOUSE-SPECIFY-RECTANGLE attempted on ~S which is not inferior of MOUSE-SHEET"
	      SHEET))
  (WITH-MOUSE-GRABBED
    (DO ()
	(NIL)
      ;; Change the mouse to an upper left angle bracket.
      (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 0 :ON :SET-CHARACTER 17)
      (MOUSE-WARP (OR LEFT MOUSE-X) (OR TOP MOUSE-Y))
      (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION
	    (IF ABORTABLE
		'(:MOUSE-L-1 "Select upper left corner of rectangle" :MOUSE-M-1 "aborts" :MOUSE-R-1
			     "nearest edge.")
		'(:MOUSE-L-1 "Select upper left corner of rectangle" :MOUSE-R-1 "nearest edge.")))
      ;; In case this was called in response to a mouse click, wait for
      ;; the buttons to be released.
       (PROCESS-WAIT "Release Button" #'(LAMBDA ()
					 (ZEROP MOUSE-LAST-BUTTONS)))
      ;; Wait for the user to press a mouse button.
      (PROCESS-WAIT "Button" #'(LAMBDA ()
				 (NOT (ZEROP MOUSE-LAST-BUTTONS))))
      (SETQ BUTTON MOUSE-LAST-BUTTONS)
      ;; Wait for the user to release the mouse button.
      (PROCESS-WAIT "Release Button" #'(LAMBDA ()
					 (ZEROP MOUSE-LAST-BUTTONS)))
      ;; The first click determines the upper left corner.
      (AND ABORTABLE (LOGTEST 2 BUTTON)		; Test for a middle click.
	   ;; Exit out of the DO () (NIL).
	   (RETURN (SETQ ABORT T)))
      (MULTIPLE-VALUE-SETQ (LEFT1 TOP1)
        (MOUSE-SPECIFIED-POINT SHEET MOUSE-X MOUSE-Y (LOGTEST 4 BUTTON)	; Right mouse button
			       NIL))
      ;; Set up the mouse for finding the lower right corner angle bracket.
      (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 12 12 :ON :SET-CHARACTER 18)
      (COND
	((AND LEFT TOP RIGHT BOTTOM)
	 (MOUSE-WARP (+ LEFT1 (- RIGHT LEFT)) (+ TOP1 (- BOTTOM TOP))))
	(T (MOUSE-WARP (+ MOUSE-X 20) (+ MOUSE-Y 20))))
      (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION
	    (IF ABORTABLE
		'(:MOUSE-L-1 "Select lower right corner of rectangle" :MOUSE-M-1 "aborts"
			     :MOUSE-R-1 "nearest edge.")
		'(:MOUSE-L-1 "Select lower right corner of rectangle" :MOUSE-R-1 "nearest edge.")))
      ;; Leave the auxiliary blinker behind to continue to show the first corner.
      (LET ((MOUSE-RECTANGLE-BLINKER (MOUSE-GET-BLINKER :RECTANGLE-CORNER-BLINKER)))
	(UNWIND-PROTECT (PROGN
			  ;; Move the mouse blinker.
			  (BLINKER-SET-CURSORPOS MOUSE-RECTANGLE-BLINKER LEFT1 TOP1)
			  ;; Turn the mouse blinker on.
			  (BLINKER-SET-VISIBILITY MOUSE-RECTANGLE-BLINKER T)
			  ;; The next click fixes the lower right corner.
			  ;; Wait for the user to hit a mouse button.
			  (PROCESS-WAIT "Button" #'(LAMBDA ()
						     (NOT (ZEROP MOUSE-LAST-BUTTONS))))
			  (SETQ BUTTON MOUSE-LAST-BUTTONS))
	  ;; Turn the mouse blinker off.
	  (BLINKER-SET-VISIBILITY MOUSE-RECTANGLE-BLINKER NIL)))
      ;; Change the blinker back to a normal one.
      (MOUSE-STANDARD-BLINKER)
      (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION NIL)
      (AND ABORTABLE (LOGTEST 2 BUTTON)		; Middle mouse button.
	   ;; Exit from the DO () (NIL) about 50 lines up.
	   (RETURN (SETQ ABORT T)))
      (MULTIPLE-VALUE-BIND (X Y)
	  ;; Adjust X and Y if the user clicked a right mouse button.
	  (MOUSE-SPECIFIED-POINT SHEET (1+ MOUSE-X) (1+ MOUSE-Y) (LOGTEST 4 BUTTON) T)
	(SETQ WIDTH (- X LEFT1)
	      HEIGHT (- Y TOP1)))
      (COND
	((AND (PLUSP WIDTH) (PLUSP HEIGHT))
	 (MULTIPLE-VALUE-BIND (XOFF YOFF)
	     (SHEET-CALCULATE-OFFSETS SHEET MOUSE-SHEET)
	   (SETQ LEFT1 (- LEFT1 XOFF)
		 TOP1 (- TOP1 YOFF)))
	 ;; Check for errors.
	 (IF (OR (< WIDTH MINIMUM-WIDTH)
		 (< HEIGHT MINIMUM-HEIGHT)
		 (MINUSP LEFT1)
		 (MINUSP TOP1)
		 (> (+ LEFT1 WIDTH) (SHEET-WIDTH SHEET))
		 (> (+ TOP1 HEIGHT) (SHEET-HEIGHT SHEET)))
	     (BEEP)
	     (RETURN NIL)))
	(T
	 ;; Either the Width or Height were negative.  Make the window
	 ;; dimensions fit inside of SHEET.  That is, this is an error case
	 ;; for which we can do something useful.
	 (SETQ LEFT1 (SHEET-INSIDE-LEFT SHEET)
	       TOP1 (SHEET-INSIDE-TOP SHEET)
	       WIDTH (SHEET-INSIDE-WIDTH SHEET)
	       HEIGHT (SHEET-INSIDE-HEIGHT SHEET))
	 (RETURN NIL)))))
  (block ()
	(if ABORT
            NIL
            ;;ELSE
	    (RETURN LEFT1 TOP1 (+ LEFT1 WIDTH) (+ TOP1 HEIGHT)))))

(DEFUN MOUSE-SPECIFIED-POINT (SHEET X Y RIGHT-BUTTON-IS-SMART LOWER-RIGHT)
  "Return X and Y, optionally adjusted to match the edge of some
window.
RIGHT-BUTTON-IS-SMART non-NIL says adjust them; otherwise, X and
		Y are returned unchanged.
LOWER-RIGHT says that X and Y are to be used as the lower right
		corner of a new window; otherwise, they are the upper
		left corner.

The adjusted X and Y are kept inside the margins of SHEET."
  (AND RIGHT-BUTTON-IS-SMART
       (LET ((X1 X)
	     (Y1 Y)
	     (X2 X)
	     (Y2 Y)
	     (Z SHEET)
	     ;; Multiplier to compare properly if (X1,Y1) is specifying
	     ;; either a upper left corner or a lower right corner.
	     (MULT (IF LOWER-RIGHT
		       1
		       -1)))
	 (DECLARE (SPECIAL X1 X2 Y1 Y2 Z MULT))
	 (MAP-OVER-EXPOSED-SHEET
	   #'(LAMBDA (SH)
	       (MULTIPLE-VALUE-BIND (XO YO)
		   (SHEET-CALCULATE-OFFSETS SH Z)
		 ;; (X3,Y3) specifies the upper left corner of the window.
		 ;; (X4,Y4) specifies the bottom right corner of the window.
		 (LET ((X3 XO)
		       (X4 (+ XO (SHEET-WIDTH SH)))
		       (Y3 YO)
		       (Y4 (+ YO (SHEET-HEIGHT SH))))
		   ;; Compare (X1,Y1) with (X3,Y3).
		   ;; If X1 is within the window AND X1 is close to the
		   ;; left side THEN
		   ;;   X2 <-- X3.
		   ;; If X1 is within the window AND Y1 is close to top
		   ;; side THEN
		   ;;   Y2 <-- Y3.
		   (AND (SUITABLY-CLOSE (* (- X3 X1) MULT))
			(SETQ X2 X3))
		   (AND (SUITABLY-CLOSE (* (- Y3 Y1) MULT))
			(SETQ Y2 Y3))
		   ;; Compare (X1,Y1) with (X4,Y4).
		   ;; If X1 is within the window AND X1 is close to the
		   ;; right side THEN
		   ;;   X2 <-- X4.
		   ;; If X1 is within the window AND Y1 is close to the
		   ;; bottom side THEN
		   ;;   Y2 <-- Y4.
		   (AND (SUITABLY-CLOSE (* (- X4 X1) MULT))
			(SETQ X2 X4))
		   (AND (SUITABLY-CLOSE (* (- Y4 Y1) MULT))
			(SETQ Y2 Y4)))))
	   SHEET)
	 (SETQ X (MIN (MAX X2 (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT SHEET))
	       Y (MIN (MAX Y2 (SHEET-INSIDE-TOP SHEET)) (SHEET-INSIDE-BOTTOM SHEET)))))
  (block ()
	(RETURN X Y))) 

;; 40 is 4 character-widths
(DEFUN SUITABLY-CLOSE (DELTA)
  (AND (PLUSP DELTA) (< DELTA #o40)))

;;; Put a window someplace using the mouse
(DEFUN MOUSE-SET-WINDOW-SIZE
       (WINDOW &OPTIONAL (MOVE-P T) &AUX LEFT TOP RIGHT BOTTOM ERROR)
  "Ask user for new edges for WINDOW, return them, and usually set
edges of WINDOW.  WINDOW's edges are set unless MOVE-P is NIL.
The values returned are the new edges, or NIL if the user aborted."
  (DECLARE (VALUES LEFT TOP RIGHT BOTTOM))
  (MULTIPLE-VALUE-SETQ (LEFT TOP)
		       (SHEET-CALCULATE-OFFSETS WINDOW MOUSE-SHEET))
  (SETQ RIGHT (+ LEFT (SHEET-WIDTH WINDOW))
	BOTTOM (+ TOP (SHEET-HEIGHT WINDOW)))
  (DO ()
      (NIL)
    (MULTIPLE-VALUE-SETQ
      (LEFT TOP RIGHT BOTTOM)
      (MOUSE-SPECIFY-RECTANGLE LEFT TOP RIGHT BOTTOM (SHEET-SUPERIOR WINDOW) 0	; Minimum width
			       0		; Minimum height
			       T))		; Abortable
    (COND
      ((NULL LEFT)				; Aborted
       (BEEP)					; Leave it where it is
       (SETQ MOVE-P NIL)
       (MULTIPLE-VALUE-SETQ (LEFT TOP RIGHT BOTTOM)
			    (SEND WINDOW :EDGES))
       (RETURN))				; Exit the DO block
      ((NULL (MULTIPLE-VALUE-SETQ
	       (NIL ERROR)
	       (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM :VERIFY)))
       (BEEP)
       (POP-UP-FORMAT "Illegal edges for ~S:~%~A" WINDOW ERROR))	;Edges no good, try again
      (T (RETURN))))				;Good
  (AND MOVE-P (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM))
  (block ()
	(RETURN LEFT TOP RIGHT BOTTOM))) 

(DEFFLAVOR MOUSE-BOX-BLINKER () (MOUSE-BLINKER-MIXIN BOX-BLINKER))
(DEFFLAVOR MOUSE-BOX-STAY-INSIDE-BLINKER ()
	   (MOUSE-BLINKER-MIXIN STAY-INSIDE-BLINKER-MIXIN BOX-BLINKER))
