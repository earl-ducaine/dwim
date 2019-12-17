;;; -*- Mode:Common-lisp;  Package: TV; Base:10. ; Fonts: (CPTFONT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  10/21/88 LG     	Use send-draw-filled-polygon to draw a following-arrow blinker on the Mac.
;;;    8/27/87   KWW       	Made changes from code reading, principal work was making screen edit attributes
;;;                         	configured to only ask for color stuff when color is active.
;;;    6/11/87  KWW      	Added foreground and background and border color and label color capability
;;;    3/16/87  KK		Minor improvements to item mouse documentation in MAKE-ATTRIBUTES-LIST.
;;;    2/19/87  KDB	"Sealed" the cracks in FOLLOWING-ARROW-BLINKER. Fix involved not drawing third
;;;                          	edges of triangles in SYS:%DRAW-SHADED-TRIANGLE.
;;;   2/18/87  TWE	Changed two constants to be the mouse-glyph symbols instead.  This makes the code
;;;			make more sense.
;;;   2/13/87  TWE	Fixed up MAKE-ATTRIBUTES-LIST to only allow a fixnum for vertical spacing in a window.
;;;			This closes bug 3444.  Also forced fixnums for the border width, border margin width
;;;			and screen manager priority (NIL is allowed too).
;;;   2/03/87  TWE	Changed uses of the function tv:substq to use the Common Lisp equivalent.  Also
;;;			removed the function tv:substq (why bother duplicating kernel code?).  Also changed
;;;			"Abort" and "Do It" to use the corresponding variables whose values are those strings.
;;; 12/18/86   SLM	Fixed package name in Suggestions macros to SYS:
;;; 12/17/86   KDB	Fixed CVV Abort problem in SCREEN-EDITOR-EDIT-ATTRIBUTES. TV:*THROW => THROW.
;;; 11/20/86   SLM	Added calls to Suggestions macros for TV:SEC-MOVE-WINDOW.
;;; 10/31/86   TWE	Changed used of DEFINE-BLINKER to use MAKE-BLINKER.  Split up this
;;;			file into two parts.  This is the second parg.
;;; 10/27/86   KDB	Removed some debug code from  MOUSE-SPECIFY-RECTANGLE.
;;; 10/27/86   KDB	Fixed FOLLOWING-ARROW-BLINKER :blink method. SYSTEM:%DRAW-shaded-TRIANGLE
;;;			was called improperly.
;;; 10/16/86   TWE	Fixed MOUSE-SPECIFY-RECTANGLE to handle the middle click properly.
;;;			Fixed MOUSE-SET-WINDOW-POSITION as per bug 2243 to have it handle
;;;			a window with a superior whose upper left corner is not at (0,0).
;;; 09/08/86   KDB	Changed edit-attributes margin choice "DONE" to "Do It",
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 08/07/86   GRH	Changed :blink to use the new primitive %draw-shaded-triangle.


(COMPILE-FLAVOR-METHODS MOUSE-BOX-BLINKER MOUSE-BOX-STAY-INSIDE-BLINKER)
(MOUSE-DEFINE-BLINKER-TYPE :BOX-BLINKER
			   #'(LAMBDA (SCREEN)
			       (MAKE-BLINKER SCREEN 'MOUSE-BOX-BLINKER :VISIBILITY NIL)))
(MOUSE-DEFINE-BLINKER-TYPE :BOX-STAY-INSIDE-BLINKER
			   #'(LAMBDA (SCREEN)
			       (MAKE-BLINKER SCREEN 'MOUSE-BOX-STAY-INSIDE-BLINKER :VISIBILITY
					     NIL)))

(DEFUN MOUSE-SET-WINDOW-POSITION
       (WINDOW
	&OPTIONAL (MOVE-P T)
	&AUX (SUPERIOR (SHEET-SUPERIOR WINDOW)) (X (SHEET-X WINDOW))
	(Y (SHEET-Y WINDOW)) XOFF YOFF BD w-xoff w-yoff)
  "Move a window around using the mouse.  Ask user for new position
for WINDOW, return it, and usually move WINDOW.

WINDOW is moved unless MOVE-P is NIL.
If MOVE-P is NIL move just an outline of it and return where it would
		have moved to.

The values returned are the new X and Y values of the upper left
corner, or NIL if the user aborted."
  (DECLARE (VALUES X Y))
  (OR (SHEET-ME-OR-MY-KID-P WINDOW MOUSE-SHEET)
      (FERROR NIL "Attempt to set position of ~S, which is not inferior to MOUSE-SHEET" WINDOW))
  (MULTIPLE-VALUE-SETQ (XOFF YOFF)
    ;; Get the X Y offset of the top left corner.
    (SHEET-CALCULATE-OFFSETS SUPERIOR MOUSE-SHEET))
  (when (mac-window-p window)
    (MULTIPLE-VALUE-SETQ (w-XOFF w-YOFF)
      ;; Get the X Y offset of the top left corner of this window's superior in mouse-sheet coordinates...
      (SHEET-CALCULATE-OFFSETS window MOUSE-SHEET)))

  (block MOUSE-SET-WINDOW-POSITION
	()					;because multiple values not passed by UNWIND-PROTECT
	(WITH-MOUSE-GRABBED
	  (WITHOUT-INTERRUPTS
	    ;; Create a mouse blinker that is the same size as the window we
	    ;; want to move.
	    (if (mac-window-p window)
		(MOUSE-SET-BLINKER-DEFINITION
		  :BOX-STAY-INSIDE-BLINKER (- mouse-x w-xoff) (- mouse-y w-yoff)
		  NIL :SET-SIZE (SHEET-WIDTH WINDOW) (SHEET-HEIGHT WINDOW))
		(progn
		  (MOUSE-SET-BLINKER-DEFINITION
		    :BOX-STAY-INSIDE-BLINKER 0 0 NIL :SET-SIZE
		    (SHEET-WIDTH WINDOW) (SHEET-HEIGHT WINDOW))
		  (MOUSE-WARP (+ X XOFF) (+ Y YOFF)))))
	  ;; Make the blinker visible.
	  (BLINKER-SET-VISIBILITY MOUSE-BLINKER T)
	  (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION
		'(:MOUSE-L-1 "selects position of window" :MOUSE-M-1
			     "aborts the positioning operation."))
	  (DO ()
	      (NIL)
	    ;; In case this was called in response to a mouse click, wait for
	    ;; the buttons to be released.
	    (PROCESS-WAIT "Release Button" #'(LAMBDA ()
					       (ZEROP MOUSE-LAST-BUTTONS)))
	    ;; Wait for the user to press a mouse button.
	    (PROCESS-WAIT "Button" #'(LAMBDA ()
				       (NOT (ZEROP MOUSE-LAST-BUTTONS))))
	    (SETQ BD MOUSE-LAST-BUTTONS)
	    (MULTIPLE-VALUE-SETQ (X Y)
				 (SEND MOUSE-BLINKER :READ-CURSORPOS))
            (SETQ X (- X XOFF)
                  Y (- Y YOFF))
	    (COND
	      ((ZEROP BD))
	      ((LOGTEST 2 BD)
	       ;; If the user clicks the middle button then get out.
	       (RETURN-FROM MOUSE-SET-WINDOW-POSITION NIL))
	      ((SEND WINDOW :SET-POSITION X Y :VERIFY)	; Consistency check.
	       ;; Turn off the big mouse blinker.
	       (BLINKER-SET-VISIBILITY MOUSE-BLINKER NIL)
	       ;; Change the location of the window.
	       (IF MOVE-P
		   (SEND WINDOW :SET-POSITION X Y))
	       (RETURN-FROM MOUSE-SET-WINDOW-POSITION X Y))
	      (T (BEEP)))))))			;Illegal position

(DEFUN EXPAND-WINDOW (WINDOW &OPTIONAL (MOVE-P T))
  "Make WINDOW bigger as much as possible without overlapping
anything else exposed.

If MOVE-P is NIL, just return the new edges but don't actually change
		WINDOW.

Returns the new edges of WINDOW."

  (DECLARE (VALUES LEFT TOP RIGHT BOTTOM))
  (LET ((X-OFFSET (SHEET-X-OFFSET WINDOW))
	(Y-OFFSET (SHEET-Y-OFFSET WINDOW)))
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
        ;; Get the largest rectangle.
	(EXPAND-RECTANGULAR-AREA
	  (SHEET-SUPERIOR WINDOW)	
	  X-OFFSET
	  Y-OFFSET
	  (+ X-OFFSET (SHEET-WIDTH WINDOW))
	  (+ Y-OFFSET (SHEET-HEIGHT WINDOW)))
      (AND MOVE-P (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM))
      (VALUES LEFT TOP RIGHT BOTTOM))))

(DEFUN EXPAND-RECTANGULAR-AREA
       (SUPERIOR LEFT TOP RIGHT BOTTOM &OPTIONAL IGNORED-WINDOW)
  "Return the largest expansion within SUPERIOR of LEFT, TOP,
RIGHT, BOTTOM.

Returns the four edges of the largest rectangle containing the
specified one, which does not overlap any exposed inferior of
SUPERIOR (except IGNORED-WINDOW)."
  (DECLARE (VALUES MAX-LEFT MAX-TOP MAX-RIGHT MAX-BOTTOM))
  (LET ((SIBLINGS (SHEET-EXPOSED-INFERIORS SUPERIOR))
	(MAX-LEFT (SHEET-INSIDE-LEFT SUPERIOR))
	(MAX-RIGHT (SHEET-INSIDE-RIGHT SUPERIOR))
	(MAX-TOP (SHEET-INSIDE-TOP SUPERIOR))
	(MAX-BOTTOM (SHEET-INSIDE-BOTTOM SUPERIOR)))
    ;; Expand to the left and right by looking for screen space to the
    ;; left and right of the rectangle specified by LEFT RIGHT TOP
    ;; BOTTOM.
    (DOLIST (W SIBLINGS)
      (COND ((OR (NULL IGNORED-WINDOW) (NEQ W IGNORED-WINDOW))
	     (AND (SHEET-OVERLAPS-EDGES-P W MAX-LEFT TOP LEFT BOTTOM)
		  (SETQ MAX-LEFT (+ (SHEET-X-OFFSET W) (SHEET-WIDTH W))))
	     (AND (SHEET-OVERLAPS-EDGES-P W RIGHT TOP MAX-RIGHT BOTTOM)
		  (SETQ MAX-RIGHT (SHEET-X-OFFSET W))))))
    ;; Expand to the top and bottom by looking for screen space at the
    ;; top and bottom.
    (DOLIST (W SIBLINGS)
      (COND ((OR (NULL IGNORED-WINDOW) (NEQ W IGNORED-WINDOW))
	     (AND (SHEET-OVERLAPS-EDGES-P W MAX-LEFT MAX-TOP MAX-RIGHT TOP)
		  (SETQ MAX-TOP (+ (SHEET-Y-OFFSET W) (SHEET-HEIGHT W))))
	     (AND (SHEET-OVERLAPS-EDGES-P W MAX-LEFT BOTTOM MAX-RIGHT MAX-BOTTOM)
		  (SETQ MAX-BOTTOM (SHEET-Y-OFFSET W))))))
    (VALUES MAX-LEFT MAX-TOP MAX-RIGHT MAX-BOTTOM)))


;;; The hairy window whitespace reclaimer.  This is used to access
;;; elements of the list created by SEC-EXPAND-ALL.
(DEFSTRUCT (EXPAND-WINDOWS-ITEM (:CONSTRUCTOR NIL) (:CONC-NAME NIL) (:CALLABLE-CONSTRUCTORS NIL)
  (:ALTERANT ALTER-EXPAND-WINDOWS-ITEM) (:PREDICATE NIL) (:COPIER NIL) (:TYPE :LIST))
  EXPAND-WINDOWS-WINDOW
  EXPAND-WINDOWS-LEFT
  EXPAND-WINDOWS-TOP
  EXPAND-WINDOWS-RIGHT
  EXPAND-WINDOWS-BOTTOM
  EXPAND-WINDOWS-LEFT-TOP-WINNERS
  EXPAND-WINDOWS-RIGHT-BOTTOM-WINNERS
  EXPAND-WINDOWS-MAX-LEFT-TOP
  EXPAND-WINDOWS-MAX-RIGHT-BOTTOM)

(DEFUN EXPAND-WINDOWS (TOP-WINDOW &AUX WINDOW-LIST)
  "Expand all the exposed inferiors of TOP-WINDOW maximally."
  (SETQ WINDOW-LIST
	(MAPCAR
	 #'(LAMBDA (W &AUX LEFT TOP)
	     (LIST W (SETQ LEFT (SHEET-X-OFFSET W)) (SETQ TOP (SHEET-Y-OFFSET W))
		   (+ LEFT (SHEET-WIDTH W)) (+ TOP (SHEET-HEIGHT W)) NIL NIL NIL NIL))
	 (SHEET-EXPOSED-INFERIORS TOP-WINDOW)))
  (EXPAND-WINDOWS-LEFT-AND-RIGHT TOP-WINDOW WINDOW-LIST)
  (EXPAND-WINDOWS-TOP-AND-BOTTOM TOP-WINDOW WINDOW-LIST)
  ;; Now we are ready to set all the edges
  (DELAYING-SCREEN-MANAGEMENT
   (DOLIST (ITEM WINDOW-LIST)
     (SEND (EXPAND-WINDOWS-WINDOW ITEM) :SET-EDGES (EXPAND-WINDOWS-LEFT ITEM)
	(EXPAND-WINDOWS-TOP ITEM) (EXPAND-WINDOWS-RIGHT ITEM) (EXPAND-WINDOWS-BOTTOM ITEM)))))

(DEFUN EXPAND-WINDOWS-LEFT-AND-RIGHT (TOP-WINDOW WINDOW-LIST)
  "Expand the lefts and rights."
  (DO ((L WINDOW-LIST (CDR L))
       (ITEM)
       (SUPERIOR-INSIDE-LEFT (SHEET-INSIDE-LEFT TOP-WINDOW))
       (SUPERIOR-INSIDE-RIGHT (SHEET-INSIDE-RIGHT TOP-WINDOW)))
      ((NULL L))
    (SETQ ITEM (CAR L))
    (DO ((L WINDOW-LIST (CDR L))
	 (NITEM)
	 (LEFT (EXPAND-WINDOWS-LEFT ITEM))
	 (TOP (EXPAND-WINDOWS-TOP ITEM))
	 (RIGHT (EXPAND-WINDOWS-RIGHT ITEM))
	 (BOTTOM (EXPAND-WINDOWS-BOTTOM ITEM))
	 (MAX-LEFT SUPERIOR-INSIDE-LEFT)
	 (MAX-RIGHT SUPERIOR-INSIDE-RIGHT)
	 (LEFT-WINNERS)
	 (RIGHT-WINNERS)
	 (NITEM-LEFT)
	 (NITEM-RIGHT))
	((NULL L)
	 (SETF (EXPAND-WINDOWS-LEFT-TOP-WINNERS ITEM) LEFT-WINNERS)
	 (SETF (EXPAND-WINDOWS-RIGHT-BOTTOM-WINNERS ITEM) RIGHT-WINNERS)
	 (SETF (EXPAND-WINDOWS-MAX-LEFT-TOP ITEM) MAX-LEFT)
	 (SETF (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM ITEM) MAX-RIGHT))
      (OR (EQ ITEM (SETQ NITEM (CAR L)))	;For all other windows
	  (> TOP (EXPAND-WINDOWS-BOTTOM NITEM))	;Which share some space on y
	  (< BOTTOM (EXPAND-WINDOWS-TOP NITEM))
	  (COND
	    ((>= LEFT (SETQ NITEM-RIGHT (EXPAND-WINDOWS-RIGHT NITEM)))
	     (AND (>= NITEM-RIGHT MAX-LEFT)
		  (IF (= NITEM-RIGHT MAX-LEFT)	;More than one window along the edge
		      (PUSH NITEM LEFT-WINNERS)
		      (SETQ MAX-LEFT NITEM-RIGHT
			    LEFT-WINNERS (LIST NITEM)))))
	    ((<= RIGHT (SETQ NITEM-LEFT (EXPAND-WINDOWS-LEFT NITEM)))
	     (AND (<= NITEM-LEFT MAX-RIGHT)
		  (IF (= NITEM-LEFT MAX-RIGHT)
		      (PUSH NITEM RIGHT-WINNERS)
		      (SETQ MAX-RIGHT NITEM-LEFT
			    RIGHT-WINNERS (LIST NITEM)))))))))
  (DOLIST (ITEM WINDOW-LIST)
    (LET ((WINDOW (EXPAND-WINDOWS-WINDOW ITEM))
	  (LEFT (EXPAND-WINDOWS-LEFT ITEM))
	  (TOP (EXPAND-WINDOWS-TOP ITEM))
	  (RIGHT (EXPAND-WINDOWS-RIGHT ITEM))
	  (BOTTOM (EXPAND-WINDOWS-BOTTOM ITEM))
	  (MAX-LEFT (EXPAND-WINDOWS-MAX-LEFT-TOP ITEM))
	  (MAX-RIGHT (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM ITEM))
	  (LEFT-WINNERS (EXPAND-WINDOWS-LEFT-TOP-WINNERS ITEM))
	  (RIGHT-WINNERS (EXPAND-WINDOWS-RIGHT-BOTTOM-WINNERS ITEM)))
      (AND (NOT (= MAX-LEFT LEFT))		;If not already adjacent
	   (LET ((WINNERS-MAX-RIGHT (AND LEFT-WINNERS
					 (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM (CAR LEFT-WINNERS))))
		 (LEFT-MIDDLE (+ MAX-LEFT (TRUNCATE (- LEFT MAX-LEFT) 2))))
	     (AND WINNERS-MAX-RIGHT
		  (SETQ LEFT-MIDDLE (MIN LEFT-MIDDLE WINNERS-MAX-RIGHT)))
	     (IF (NOT (SEND WINDOW :SET-EDGES LEFT-MIDDLE TOP RIGHT BOTTOM :VERIFY))
		 ;;This window will not move, get as many of the others as will allow
		 (DOLIST (LEFT-WINNER LEFT-WINNERS)
		   (AND
		     (SEND (EXPAND-WINDOWS-WINDOW LEFT-WINNER) :SET-EDGES
			   (EXPAND-WINDOWS-LEFT LEFT-WINNER) (EXPAND-WINDOWS-TOP LEFT-WINNER)
			   WINNERS-MAX-RIGHT (EXPAND-WINDOWS-BOTTOM LEFT-WINNER) :VERIFY)
		     (SET-EXPAND-WINDOWS-RIGHT LEFT-WINNER WINNERS-MAX-RIGHT)))
		 ;;Otherwise expand this window left
		 (IF (AND
		       (OR (NULL LEFT-WINNERS)	;If no windows to the left,
			   (DOLIST (LEFT-WINNER LEFT-WINNERS)	;or some won't budge
			     (OR
			       (SEND (EXPAND-WINDOWS-WINDOW LEFT-WINNER) :SET-EDGES
				     (EXPAND-WINDOWS-LEFT LEFT-WINNER) (EXPAND-WINDOWS-TOP LEFT-WINNER)
				     LEFT-MIDDLE (EXPAND-WINDOWS-BOTTOM LEFT-WINNER) :VERIFY)
			       (RETURN T))))
		       (SEND WINDOW :SET-EDGES MAX-LEFT TOP RIGHT BOTTOM :VERIFY))
		     ;;Go all the way left
		     (SET-EXPAND-WINDOWS-LEFT ITEM (SETQ LEFT MAX-LEFT))
		     ;;Share with winners
		     (PROGN
		       (SET-EXPAND-WINDOWS-LEFT ITEM (SETQ LEFT LEFT-MIDDLE))
		       (DOLIST (LEFT-WINNER LEFT-WINNERS)
			 (SET-EXPAND-WINDOWS-RIGHT LEFT-WINNER LEFT-MIDDLE)))))))
      (AND (/= MAX-RIGHT RIGHT)
	   (LET ((WINNERS-MAX-LEFT
		   (AND RIGHT-WINNERS (EXPAND-WINDOWS-MAX-LEFT-TOP (CAR RIGHT-WINNERS))))
		 (RIGHT-MIDDLE (- MAX-RIGHT (TRUNCATE (- MAX-RIGHT RIGHT) 2))))
	     (AND WINNERS-MAX-LEFT (SETQ RIGHT-MIDDLE (MAX RIGHT-MIDDLE WINNERS-MAX-LEFT)))
	     (IF (NOT (SEND WINDOW :SET-EDGES LEFT TOP RIGHT-MIDDLE BOTTOM :VERIFY))
		 (DOLIST (RIGHT-WINNER RIGHT-WINNERS)
		   (AND
		     (SEND (EXPAND-WINDOWS-WINDOW RIGHT-WINNER) :SET-EDGES WINNERS-MAX-LEFT
			   (EXPAND-WINDOWS-TOP RIGHT-WINNER) (EXPAND-WINDOWS-RIGHT RIGHT-WINNER)
			   (EXPAND-WINDOWS-BOTTOM RIGHT-WINNER) :VERIFY)
		     (SET-EXPAND-WINDOWS-LEFT RIGHT-WINNER WINNERS-MAX-LEFT)))
		 (IF (AND
		       (OR (NULL RIGHT-WINNERS)
			   (DOLIST (RIGHT-WINNER RIGHT-WINNERS)
			     (OR
			       (SEND (EXPAND-WINDOWS-WINDOW RIGHT-WINNER) :SET-EDGES RIGHT-MIDDLE
				     (EXPAND-WINDOWS-TOP RIGHT-WINNER) (EXPAND-WINDOWS-RIGHT RIGHT-WINNER)
				     (EXPAND-WINDOWS-BOTTOM RIGHT-WINNER) :VERIFY)
			       (RETURN T))))
		       (SEND WINDOW :SET-EDGES LEFT TOP MAX-RIGHT BOTTOM :VERIFY))
		     (SET-EXPAND-WINDOWS-RIGHT ITEM (SETQ RIGHT MAX-RIGHT))
		     (PROGN
		       (SET-EXPAND-WINDOWS-RIGHT ITEM (SETQ RIGHT RIGHT-MIDDLE))
		       (DOLIST (RIGHT-WINNER RIGHT-WINNERS)
			 (SET-EXPAND-WINDOWS-LEFT RIGHT-WINNER RIGHT-MIDDLE))))))))))

(DEFUN SET-EXPAND-WINDOWS-LEFT (ITEM VAL)
  (SETF (EXPAND-WINDOWS-LEFT ITEM) VAL)
  (SETF (EXPAND-WINDOWS-MAX-LEFT-TOP ITEM) VAL)
  (DOLIST (WINNER (EXPAND-WINDOWS-LEFT-TOP-WINNERS ITEM))
    (SETF (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM WINNER)
	  (MIN VAL (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM WINNER)))))

(DEFUN SET-EXPAND-WINDOWS-RIGHT (ITEM VAL)
  (SETF (EXPAND-WINDOWS-RIGHT ITEM) VAL)
  (SETF (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM ITEM) VAL)
  (DOLIST (WINNER (EXPAND-WINDOWS-RIGHT-BOTTOM-WINNERS ITEM))
    (SETF (EXPAND-WINDOWS-MAX-LEFT-TOP WINNER)
	  (MAX VAL (EXPAND-WINDOWS-MAX-LEFT-TOP WINNER)))))

;;; Same as EXPAND-WINDOWS-LEFT-AND-RIGHT except for the obvious
;;; differences.
(DEFUN EXPAND-WINDOWS-TOP-AND-BOTTOM (TOP-WINDOW WINDOW-LIST)
  (DO ((L WINDOW-LIST (CDR L))
       (ITEM)
       (SUPERIOR-INSIDE-TOP (SHEET-INSIDE-TOP TOP-WINDOW))
       (SUPERIOR-INSIDE-BOTTOM (SHEET-INSIDE-BOTTOM TOP-WINDOW)))
      ((NULL L))
    (SETQ ITEM (CAR L))
    (DO ((L WINDOW-LIST (CDR L))
	 (NITEM)
	 (LEFT (EXPAND-WINDOWS-LEFT ITEM))
	 (TOP (EXPAND-WINDOWS-TOP ITEM))
	 (RIGHT (EXPAND-WINDOWS-RIGHT ITEM))
	 (BOTTOM (EXPAND-WINDOWS-BOTTOM ITEM))
	 (MAX-TOP SUPERIOR-INSIDE-TOP)
	 (MAX-BOTTOM SUPERIOR-INSIDE-BOTTOM)
	 (TOP-WINNERS)
	 (BOTTOM-WINNERS)
	 (NITEM-TOP)
	 (NITEM-BOTTOM))
	((NULL L)
	 (SETF (EXPAND-WINDOWS-LEFT-TOP-WINNERS ITEM) TOP-WINNERS)
	 (SETF (EXPAND-WINDOWS-RIGHT-BOTTOM-WINNERS ITEM) BOTTOM-WINNERS)
	 (SETF (EXPAND-WINDOWS-MAX-LEFT-TOP ITEM) MAX-TOP)
	 (SETF (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM ITEM) MAX-BOTTOM))
      (OR (EQ ITEM (SETQ NITEM (CAR L)))	;For all other windows
	  (> LEFT (EXPAND-WINDOWS-RIGHT NITEM))	;Which share some space on y
	  (< RIGHT (EXPAND-WINDOWS-LEFT NITEM))
	  (COND
	    ((>= TOP (SETQ NITEM-BOTTOM (EXPAND-WINDOWS-BOTTOM NITEM)))
	     (AND (>= NITEM-BOTTOM MAX-TOP)
		  (IF (= NITEM-BOTTOM MAX-TOP)	;More than one window along the edge
		      (PUSH NITEM TOP-WINNERS)
		      (SETQ MAX-TOP NITEM-BOTTOM
			    TOP-WINNERS (LIST NITEM)))))
	    ((<= BOTTOM (SETQ NITEM-TOP (EXPAND-WINDOWS-TOP NITEM)))
	     (AND (<= NITEM-TOP MAX-BOTTOM)
		  (IF (= NITEM-TOP MAX-BOTTOM)
		      (PUSH NITEM BOTTOM-WINNERS)
		      (SETQ MAX-BOTTOM NITEM-TOP
			    BOTTOM-WINNERS (LIST NITEM)))))))))
  (DOLIST (ITEM WINDOW-LIST)
    (LET ((WINDOW (EXPAND-WINDOWS-WINDOW ITEM))
	  (LEFT (EXPAND-WINDOWS-LEFT ITEM))
	  (TOP (EXPAND-WINDOWS-TOP ITEM))
	  (RIGHT (EXPAND-WINDOWS-RIGHT ITEM))
	  (BOTTOM (EXPAND-WINDOWS-BOTTOM ITEM))
	  (MAX-TOP (EXPAND-WINDOWS-MAX-LEFT-TOP ITEM))
	  (MAX-BOTTOM (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM ITEM))
	  (TOP-WINNERS (EXPAND-WINDOWS-LEFT-TOP-WINNERS ITEM))
	  (BOTTOM-WINNERS (EXPAND-WINDOWS-RIGHT-BOTTOM-WINNERS ITEM)))
      (AND (NOT (= MAX-TOP TOP))		;If not already adjacent
	   (LET ((WINNERS-MAX-BOTTOM
		   (AND TOP-WINNERS (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM (CAR TOP-WINNERS))))
		 (TOP-MIDDLE (+ MAX-TOP (TRUNCATE (- TOP MAX-TOP) 2))))
	     (AND WINNERS-MAX-BOTTOM
		  (SETQ TOP-MIDDLE (MIN TOP-MIDDLE WINNERS-MAX-BOTTOM)))
	     (IF (NOT (SEND WINDOW :SET-EDGES LEFT TOP-MIDDLE RIGHT BOTTOM :VERIFY))
		 ;;This window will not move, get as many of the others as will allow
		 (DOLIST (TOP-WINNER TOP-WINNERS)
		   (AND
		     (SEND (EXPAND-WINDOWS-WINDOW TOP-WINNER) :SET-EDGES
			   (EXPAND-WINDOWS-LEFT TOP-WINNER) (EXPAND-WINDOWS-TOP TOP-WINNER)
			   (EXPAND-WINDOWS-RIGHT TOP-WINNER) WINNERS-MAX-BOTTOM :VERIFY)
		     (SET-EXPAND-WINDOWS-BOTTOM TOP-WINNER WINNERS-MAX-BOTTOM)))
		 ;;Otherwise expand this window top
		 (IF (AND
		       (OR (NULL TOP-WINNERS)	;If no windows to the top,
			   (DOLIST (TOP-WINNER TOP-WINNERS)	;or some won't budge
			     (OR
			       (SEND (EXPAND-WINDOWS-WINDOW TOP-WINNER) :SET-EDGES
				     (EXPAND-WINDOWS-LEFT TOP-WINNER) (EXPAND-WINDOWS-TOP TOP-WINNER)
				     (EXPAND-WINDOWS-RIGHT TOP-WINNER) TOP-MIDDLE :VERIFY)
			       (RETURN T))))
		       (SEND WINDOW :SET-EDGES LEFT MAX-TOP RIGHT BOTTOM :VERIFY))
		     ;;Go all the way top
		     (SET-EXPAND-WINDOWS-TOP ITEM (SETQ TOP MAX-TOP))
		     ;;Share with winners
		     (PROGN
		       (SET-EXPAND-WINDOWS-TOP ITEM (SETQ TOP TOP-MIDDLE))
		       (DOLIST (TOP-WINNER TOP-WINNERS)
			 (SET-EXPAND-WINDOWS-BOTTOM TOP-WINNER TOP-MIDDLE)))))))
      (AND (/= MAX-BOTTOM BOTTOM)
	   (LET ((WINNERS-MAX-TOP
		   (AND BOTTOM-WINNERS (EXPAND-WINDOWS-MAX-LEFT-TOP (CAR BOTTOM-WINNERS))))
		 (BOTTOM-MIDDLE (- MAX-BOTTOM (TRUNCATE (- MAX-BOTTOM BOTTOM) 2))))
	     (AND WINNERS-MAX-TOP
		  (SETQ BOTTOM-MIDDLE (MAX BOTTOM-MIDDLE WINNERS-MAX-TOP)))
	     (IF (NOT (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM-MIDDLE :VERIFY))
		 (DOLIST (BOTTOM-WINNER BOTTOM-WINNERS)
		   (AND
		     (SEND (EXPAND-WINDOWS-WINDOW BOTTOM-WINNER) :SET-EDGES
			   (EXPAND-WINDOWS-LEFT BOTTOM-WINNER) WINNERS-MAX-TOP
			   (EXPAND-WINDOWS-RIGHT BOTTOM-WINNER) (EXPAND-WINDOWS-BOTTOM BOTTOM-WINNER)
			   :VERIFY)
		     (SET-EXPAND-WINDOWS-TOP BOTTOM-WINNER WINNERS-MAX-TOP)))
		 (IF (AND
		       (OR (NULL BOTTOM-WINNERS)
			   (DOLIST (BOTTOM-WINNER BOTTOM-WINNERS)
			     (OR
			       (SEND (EXPAND-WINDOWS-WINDOW BOTTOM-WINNER) :SET-EDGES
				     (EXPAND-WINDOWS-LEFT BOTTOM-WINNER) BOTTOM-MIDDLE
				     (EXPAND-WINDOWS-RIGHT BOTTOM-WINNER)
				     (EXPAND-WINDOWS-BOTTOM BOTTOM-WINNER) :VERIFY)
			       (RETURN T))))
		       (SEND WINDOW :SET-EDGES LEFT TOP RIGHT MAX-BOTTOM :VERIFY))
		     (SET-EXPAND-WINDOWS-BOTTOM ITEM (SETQ BOTTOM MAX-BOTTOM))
		     (PROGN
		       (SET-EXPAND-WINDOWS-BOTTOM ITEM (SETQ BOTTOM BOTTOM-MIDDLE))
		       (DOLIST (BOTTOM-WINNER BOTTOM-WINNERS)
			 (SET-EXPAND-WINDOWS-TOP BOTTOM-WINNER BOTTOM-MIDDLE))))))))))

(DEFUN SET-EXPAND-WINDOWS-TOP (ITEM VAL)
  (SETF (EXPAND-WINDOWS-TOP          ITEM) VAL)
  (SETF (EXPAND-WINDOWS-MAX-LEFT-TOP ITEM) VAL)
  (DOLIST (WINNER (EXPAND-WINDOWS-LEFT-TOP-WINNERS ITEM))
    (SETF (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM WINNER)
	  (MIN VAL (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM WINNER)))))

(DEFUN SET-EXPAND-WINDOWS-BOTTOM (ITEM VAL)
  (SETF (EXPAND-WINDOWS-BOTTOM           ITEM) VAL)
  (SETF (EXPAND-WINDOWS-MAX-RIGHT-BOTTOM ITEM) VAL)
  (DOLIST (WINNER (EXPAND-WINDOWS-RIGHT-BOTTOM-WINNERS ITEM))
    (SETF (EXPAND-WINDOWS-MAX-LEFT-TOP WINNER)
	  (MAX VAL (EXPAND-WINDOWS-MAX-LEFT-TOP WINNER)))))


;;; The argument to :VALUE is the name of a function which, when called,
;;; will perform the appropriate action and then return either the
;;; second parameter passed to the function or 'ABORT.  The exception to
;;; this is SEC-UNDO which returns something different.
(DEFVAR SCREEN-EDITOR-ITEM-LIST
	'(("Bury" :VALUE SEC-BURY
	   :DOCUMENTATION "Point at a window and put it underneath all other windows.")
	  ("Expose" :VALUE SEC-EXPOSE
	   :DOCUMENTATION "Point at a window and expose it.")
	  ("Expose (menu)" :VALUE SEC-EXPOSE-MENU
	   :DOCUMENTATION
   "Choose a window (from a menu) and expose it.  This can get at more windows than Expose.")
	  ("Create" :VALUE SEC-CREATE
	   :DOCUMENTATION
  "Choose a flavor of window (from a menu) and corners, and create a window of that flavor.")
	  ("Create (expand)" :VALUE SEC-CREATE-EXPAND
	   :DOCUMENTATION "Create followed by Expand Window.")
	  ("Kill" :VALUE SEC-KILL
	   :DOCUMENTATION "Point at a window and kill it.  Asks for confirmation.")
	  ("Move window" :VALUE SEC-MOVE-WINDOW
	   :DOCUMENTATION "Point at a window and move it.")
	  ("Reshape" :VALUE SEC-RESHAPE
	   :DOCUMENTATION "Point at a window, and specify new corners for it.")
	  ("Move multiple" :VALUE SEC-MULTIPLE-MOVE
	   :DOCUMENTATION "Choose a group of edges and corners and move them as a unit.")
	  ("Move single" :VALUE SEC-SINGLE-MOVE
	   :DOCUMENTATION "Point at an edge or corner and move it.")
	  ("Expand window" :VALUE SEC-EXPAND-WINDOW
	   :DOCUMENTATION
"Point at a window and change its size so it fills as much empty space around it as possible."
	   )
	  ("Expand all" :VALUE SEC-EXPAND-ALL
	   :DOCUMENTATION
	   "Change the size of all windows to fill as much empty space as possible.")
	  ("Attributes" :VALUE SEC-ATTRIBUTES
	   :DOCUMENTATION
	   "Edit the attributes of a specified window.")
	  ("Undo" :VALUE SEC-UNDO
	   :DOCUMENTATION "Undo the last screen editor command.  Can't undo Create or Kill.")
	  ("Exit" :VALUE SEC-QUIT
	   :FONT :MENU-STANDOUT
	   :DOCUMENTATION "Leave the screen editor."))
  "Menu alist for the screen editor menu.")

(DEFWINDOW-RESOURCE SCREEN-EDITOR-MENU ()
  :INITIAL-COPIES 0
  :MAKE-WINDOW  (W:MENU
    :ITEM-LIST-POINTER 'SCREEN-EDITOR-ITEM-LIST :DYNAMIC T :PERMANENT NIL
    :SCROLLING-P NIL
    :foreground-color *default-menu-foreground*
    :background-color *default-menu-background*
    :SAVE-BITS T)
  :REUSABLE-WHEN :DEEXPOSED)

(DEFVAR SCREEN-EDITOR-MENU)
(DEFVAR SCREEN-EDITOR-PREVIOUS-ALIST)

(DEFUN EDIT-SCREEN
       (TOP-SHEET
	&AUX WINDOW-EDGE-ALIST SCREEN-EDITOR-PREVIOUS-ALIST (OLD-MOUSE-SHEET MOUSE-SHEET)
	(OLD-SELECTED-WINDOW SELECTED-WINDOW))
  "The actual screen editor.
The WINDOW-EDGE-ALIST is in sheet visibility order and has elements
	(window exposed-p left top right bottom)
:BURY in exposed-p is a special kludge to make burying to deexposed
windows work.  Only problem with this is that undoing a bury of a
de-exposed window does not work; we do not have window operations
to do things like bring a window to the top of the de-exposed ones.

Commands work by modifying this alist and the command loop here does
the actual side-effects, allowing for undoing."
  (USING-RESOURCE (SCREEN-EDITOR-MENU SCREEN-EDITOR-MENU TOP-SHEET)
    (UNWIND-PROTECT
        (CATCH 'EXIT-SCREEN-EDITOR
          (LET-GLOBALLY ((WHO-LINE-PROCESS CURRENT-PROCESS))
            (MOUSE-SET-SHEET TOP-SHEET)
            (DO ((COMMAND)
                 (NEW-ALIST 'FIRST))
                (NIL)
              (EXPOSE-WINDOW-NEAR SCREEN-EDITOR-MENU '(:MOUSE))
              (SETQ COMMAND (SEND SCREEN-EDITOR-MENU :CHOOSE))
              (IF (NULL COMMAND)
                  ;; The user pressed the ABORT key.
                  (THROW 'EXIT-SCREEN-EDITOR NIL)
                  ;;ELSE
                  (DELAYING-SCREEN-MANAGEMENT
                    (SEND SCREEN-EDITOR-MENU :DEACTIVATE)
                    ;; Now, just before executing the command, pick up the
                    ;; state of the screen.  We defer it until now so that we
                    ;; see the results of screen management and of things done
                    ;; to the screen by other processes.  Also save the state
                    ;; before the previous command for Undo. See the SEC-UNDO
                    ;; function for more details.
                    (OR (EQ NEW-ALIST 'ABORT)
                        (SETQ SCREEN-EDITOR-PREVIOUS-ALIST WINDOW-EDGE-ALIST))
                    (SETQ WINDOW-EDGE-ALIST (GET-WINDOW-EDGE-ALIST TOP-SHEET))
                    (AND (EQ NEW-ALIST 'FIRST)
                         (SETQ SCREEN-EDITOR-PREVIOUS-ALIST WINDOW-EDGE-ALIST))
                    ;; Execute the command that the user moused on.
                    (SETQ NEW-ALIST (SEND COMMAND TOP-SHEET WINDOW-EDGE-ALIST))
                    (COND
                      ((NEQ NEW-ALIST 'ABORT)   ;Don't change history if command aborted
                       (DOLIST (NEW NEW-ALIST)
                         (LET ((OLD (ASSOC (CAR NEW) WINDOW-EDGE-ALIST :TEST #'EQ)))
                           (OR (EQUAL (CDDR OLD) (CDDR NEW))    ;Edges not the same?
                               (MULTIPLE-VALUE-BIND (WIN LOSE)
                                   (SEND (FIRST NEW) :SET-EDGES (THIRD NEW) (FOURTH NEW)
                                         (FIFTH NEW) (SIXTH NEW) :VERIFY)
                                 (IF WIN
                                     (APPLY (CAR NEW) :SET-EDGES (CDDR NEW))
                                     (PROGN
                                       (BEEP)
                                       (POP-UP-FORMAT "Illegal edges for ~S:~%~A"
                                                      (CAR NEW) LOSE)))))
                           ;; Try to fix exposure and ordering of de-exposed sheets.
                           ;; This may not be quite right, e.g. if undoing an expose
                           ;; because the window will go in the wrong place in the
                           ;; de-exposed sheets, and Undo twice will not be a no-op.
                           ;; It will just have to do for now though.
                           (COND
                             ((EQ (CADR NEW) T)
                              (OR (CADR OLD) (SEND (CAR NEW) :EXPOSE)))
                             ((EQ (CADR NEW) :BURY) (SEND (CAR NEW) :BURY)))))
                       ;; Doing the buries in a second pass makes the
                       ;; above-mentioned inaccuracy less
                       (DOLIST (NEW NEW-ALIST)
                         (AND (NOT (CADR NEW)) (SHEET-EXPOSED-P (CAR NEW))
                              (SEND (CAR NEW) :BURY))))))))))

      (MOUSE-SET-SHEET OLD-MOUSE-SHEET)))
  (IF (SCREEN-EDITOR-SHOULD-RESELECT OLD-SELECTED-WINDOW)
      (SEND OLD-SELECTED-WINDOW :SELECT)
      (SEND TOP-SHEET :SCREEN-MANAGE-AUTOEXPOSE-INFERIORS)))

(DEFUN GET-WINDOW-EDGE-ALIST (TOP-SHEET &AUX WINDOW-EDGE-ALIST TEM)
  "Return an alist of inferiors of TOP-SHEET versus their exposed-p's
and edges.  Each element looks like (inferior exposed-p left top right
bottom).  Such a list represents the complete state of the screen
being edited and is used as the basis for undoing."
  (DOLIST (SHEET (SHEET-INFERIORS TOP-SHEET))
    (AND (OR (SETQ TEM (SHEET-EXPOSED-P SHEET))
	     (SEND SHEET :SCREEN-MANAGE-DEEXPOSED-VISIBILITY))
	 (PUSH (LIST* SHEET TEM (MULTIPLE-VALUE-LIST (SEND SHEET :EDGES)))
	       WINDOW-EDGE-ALIST)))
  (NREVERSE WINDOW-EDGE-ALIST)                  ;Get into same order as TOP-SHEET
  )

(DEFUN SCREEN-EDITOR-SHOULD-RESELECT (W)
  "Returns T if W and all superiors of W are exposed, up to the
screen."
  (AND W
       (DO ((W W (SHEET-SUPERIOR W)))
	   ((NULL W) T)
	 (OR (SHEET-EXPOSED-P W)
	     (RETURN NIL)))))

(DEFUN SCREEN-EDITOR-FIND-SCREEN-TO-EDIT (BOTTOM-WINDOW &AUX LIST)
  (DO ((SHEET BOTTOM-WINDOW (SHEET-SUPERIOR SHEET)))
      ((NULL SHEET)
       NIL)
    (IF (SHEET-EXPOSED-P SHEET)
	(IF (TYPEP SHEET 'BASIC-FRAME)
	    (PUSH SHEET LIST))
	(SETQ LIST NIL)))
  ;; LIST is now all the frames under the mouse that are VISIBLE!
  (IF (NULL LIST)
      MOUSE-SHEET
      (PROGN
	(OR (MEMBER MOUSE-SHEET LIST :TEST #'EQ)
	    (PUSH MOUSE-SHEET LIST))
	(W:MENU-CHOOSE
	  (MAPCAR #'(LAMBDA (W)
		      (CONS (OR (AND (GET-HANDLER-FOR W :NAME-FOR-SELECTION)
				     (SEND W :NAME-FOR-SELECTION))
				(SHEET-NAME W))
			    W))
		  LIST)
	  :SCROLLING-P NIL
	  :LABEL "Edit inferiors of which screen or frame:"))))

;;; The screen editor commands and their friends; called with the top-sheet and edge-alist
;;; as arguments, they return the new edge alist.
(DEFUN SEC-QUIT (IGNORE IGNORE)
  (THROW 'EXIT-SCREEN-EDITOR T))

(DEFUN SEC-UNDO (IGNORE IGNORE)
  SCREEN-EDITOR-PREVIOUS-ALIST)

(DEFUN SEC-BURY (IGNORE WINDOW-EDGE-ALIST &AUX WINDOW)
  (COND
    ((SETQ WINDOW (SCREEN-EDITOR-FIND-WINDOW
		    WINDOW-EDGE-ALIST NIL
		    ;; Actually a RIGHT mouse click will abort too.
		    "Bury window"
		    '(:MOUSE-L-1 "Choose a window to bury" :MOUSE-M-1
				 "aborts the bury operation.")))
     ;; Remove the window and put it at the bottom of the WINDOW-EDGE-ALIST.
     (SETQ WINDOW-EDGE-ALIST
	   (NREVERSE
	    (CONS (SETQ WINDOW (COPY-LIST WINDOW))
		  (DELETE WINDOW (THE LIST (REVERSE WINDOW-EDGE-ALIST)) :TEST #'EQ))))
     (SETF (SECOND WINDOW) :BURY) WINDOW-EDGE-ALIST)
    (T 'ABORT)))

;This is not really undoable, in that the window cannot be "unkilled"
(DEFUN SEC-KILL (IGNORE WINDOW-EDGE-ALIST &AUX WINDOW)
  (COND
    ((AND
       (SETQ WINDOW
	     (SCREEN-EDITOR-FIND-WINDOW WINDOW-EDGE-ALIST NIL "Kill window"
					'(:MOUSE-L-1 "Choose a window to be killed" :MOUSE-M-1
						     "do not kill any window.")))
       (MOUSE-Y-OR-N-P (FORMAT NIL "Kill ~A" (SHEET-NAME (CAR WINDOW)))))
     (SEND (CAR WINDOW) :KILL)
     (SETQ WINDOW-EDGE-ALIST (REMOVE WINDOW (THE LIST WINDOW-EDGE-ALIST) :TEST #'EQ))
     WINDOW-EDGE-ALIST)
    (T 'ABORT)))

;;; Undoing this won't kill this window, just bury it
(DEFUN SEC-CREATE (SUPERIOR WINDOW-EDGE-ALIST)
  (IF (SYSTEM-MENU-CREATE-WINDOW SUPERIOR)
      WINDOW-EDGE-ALIST
      'ABORT))

;;; Undoing this won't kill this window, just bury it
(DEFUN SEC-CREATE-EXPAND (SUPERIOR WINDOW-EDGE-ALIST)
  (IF (SYSTEM-MENU-CREATE-WINDOW SUPERIOR 'EXPAND)
      WINDOW-EDGE-ALIST
      'ABORT))

(DEFUN SEC-EXPOSE (IGNORE WINDOW-EDGE-ALIST &AUX WINDOW)
  (COND
    ((SETQ WINDOW
	   (SCREEN-EDITOR-FIND-WINDOW WINDOW-EDGE-ALIST NIL "Expose window"
				      '(:MOUSE-L-1 "Choose a window to be exposed" :MOUSE-M-1
						   "aborts the expose operation.")))
     (SETQ WINDOW-EDGE-ALIST (REMOVE WINDOW (THE LIST WINDOW-EDGE-ALIST) :TEST #'EQ))
     (PUSH (SETQ WINDOW (COPY-LIST WINDOW)) WINDOW-EDGE-ALIST)
     (SETF (SECOND WINDOW) T)
     WINDOW-EDGE-ALIST)
    (T 'ABORT)))

(DEFUN SEC-EXPOSE-MENU (TOP-SHEET WINDOW-EDGE-ALIST &AUX WINDOW ELEM)
  ;; Get a list of exposed window names suitable for W:MENU-CHOOSE.
  (LET ((LIST
	  (MAPCAN
	    #'(LAMBDA (W)
		(AND (NOT (MEMBER W (SHEET-EXPOSED-INFERIORS TOP-SHEET) :TEST #'EQ))
		     (CONS (CONS (OR (AND (GET-HANDLER-FOR W :NAME-FOR-SELECTION)
					  (SEND W :NAME-FOR-SELECTION))
				     (SHEET-NAME W))
				 W)
			   NIL)))
	    (SHEET-INFERIORS TOP-SHEET))))
    (COND
      ((NULL LIST)
       (BEEP)
       (POP-UP-MESSAGE "Error: there are no windows to be exposed.") 'ABORT)
      ((SETQ WINDOW (W:MENU-CHOOSE LIST :LABEL "Expose:" :SCROLLING-P NIL))
       ;; Check WINDOW-EDGE-ALIST to see if the window is there.
       (COND
	 ((SETQ ELEM (ASSOC WINDOW WINDOW-EDGE-ALIST :TEST #'EQ))
	  (SETQ WINDOW-EDGE-ALIST (REMOVE ELEM (THE LIST WINDOW-EDGE-ALIST) :TEST #'EQ))
	  (SETQ ELEM (COPY-LIST ELEM)) (SETF (SECOND ELEM) T))
	 (T
	  ;; Couldn't find the window.  Create an entry suitable
	  ;; for adding to WINDOW-EDGE-ALIST.
	  (SETQ ELEM (LIST* WINDOW T (MULTIPLE-VALUE-LIST (SEND WINDOW :EDGES))))))
       (PUSH ELEM WINDOW-EDGE-ALIST) WINDOW-EDGE-ALIST)
      (T 'ABORT))))

(sys:declare-suggestions-for 'TV:sec-move-window :after
			 ;;This code should run whether Suggestions is on or not so that windows will not
			 ;;be resized inappropriately when Suggestions is turned on.
			 '(locally (declare (special sys:function-values))
				   (when (consp (car sys:function-values))
					 (sys:find-and-update-moved-windows (car sys:function-values)))))

(DEFUN SEC-MOVE-WINDOW (IGNORE WINDOW-EDGE-ALIST &AUX WINDOW)
  (sys:with-suggestions-menus-for tv:sec-move-window
  (COND ((SETQ WINDOW (SCREEN-EDITOR-FIND-WINDOW WINDOW-EDGE-ALIST NIL
			"Move window"
                        '(:MOUSE-L-1 "Choose a window to be moved"
                          :MOUSE-M-1 "aborts the moving operation.")))
	 (MULTIPLE-VALUE-BIND (X Y)
	     (MOUSE-SET-WINDOW-POSITION (CAR WINDOW) NIL)
	   (IF X
               ;; Replace the old edges with the new ones.
	       (SETQ WINDOW-EDGE-ALIST (SUBST (LIST (CAR WINDOW) (CADR WINDOW) X Y
						     (+ X (SHEET-WIDTH (CAR WINDOW)))
						     (+ Y (SHEET-HEIGHT (CAR WINDOW))))
					       WINDOW WINDOW-EDGE-ALIST :TEST #'EQ))
	       'ABORT)))
	(T 'ABORT))))

(DEFUN SEC-RESHAPE (IGNORE WINDOW-EDGE-ALIST &AUX WINDOW)
  (COND ((SETQ WINDOW (SCREEN-EDITOR-FIND-WINDOW WINDOW-EDGE-ALIST NIL
			"Reshape window"
                        '(:MOUSE-L-1 "Choose a window to be reshaped"
                          :MOUSE-M-1 "aborts the reshape operation.")))
	 (SETQ WINDOW-EDGE-ALIST
	       (SUBST (LIST* (CAR WINDOW) (CADR WINDOW)
			      (MULTIPLE-VALUE-LIST (MOUSE-SET-WINDOW-SIZE (CAR WINDOW) NIL)))
		       WINDOW WINDOW-EDGE-ALIST :TEST #'EQ))
	 WINDOW-EDGE-ALIST)
	(T 'ABORT)))

(DEFUN SEC-EXPAND-WINDOW (IGNORE WINDOW-EDGE-ALIST &AUX WINDOW)
  (COND ((SETQ WINDOW (SCREEN-EDITOR-FIND-WINDOW WINDOW-EDGE-ALIST NIL
			"Expand window"
                        '(:MOUSE-L-1 "Choose a window to expand"
                          :MOUSE-M-1 "aborts the expand operation.")))
	 (SETQ WINDOW-EDGE-ALIST
	       (SUBST (LIST* (CAR WINDOW) (CADR WINDOW)
			      (MULTIPLE-VALUE-LIST (EXPAND-WINDOW (CAR WINDOW) NIL)))
		       WINDOW WINDOW-EDGE-ALIST :TEST #'EQ))
	 WINDOW-EDGE-ALIST)
	(T 'ABORT)))

(DEFUN SEC-EXPAND-ALL (TOP-WINDOW WINDOW-EDGE-ALIST &AUX WINDOW-LIST)
  (SETQ WINDOW-LIST (DO ((L WINDOW-EDGE-ALIST (CDR L))
			 (LIST NIL))
			((NULL L)
			 (NREVERSE LIST))
		      (AND (CADAR L)		;Exposed
			   (PUSH (CONS (CAAR L) (APPEND (CDDAR L) (MAKE-LIST 4))) LIST))))
  (EXPAND-WINDOWS-LEFT-AND-RIGHT TOP-WINDOW WINDOW-LIST)
  (EXPAND-WINDOWS-TOP-AND-BOTTOM TOP-WINDOW WINDOW-LIST)
  (NCONC
   (MAPCAR
    #'(LAMBDA (ITEM)
	(LIST (EXPAND-WINDOWS-WINDOW ITEM) T (EXPAND-WINDOWS-LEFT ITEM)
	      (EXPAND-WINDOWS-TOP ITEM) (EXPAND-WINDOWS-RIGHT ITEM) (EXPAND-WINDOWS-BOTTOM ITEM)))
    WINDOW-LIST)
   (DO ((L WINDOW-EDGE-ALIST (CDR L)))
       ((NULL L)
	NIL)
     (AND (NULL (CADAR L))	;All the de-exposed guys
	  (RETURN L)))))

(DEFUN SCREEN-EDITOR-FIND-WINDOW
       (WINDOW-EDGE-ALIST CHAR PROMPT DOC
	&AUX X Y WINDOW)
  "Ask the user to choose a window by pointing at it with the mouse.

WINDOW-EDGE-ALIST is the current screen editor state.
CHAR if non-NIL is the character to use for the mouse cursor.  If NIL
		then use the default.
PROMPT appears in the who line, and DOC in the mouse documentation
		string."
  (OR CHAR (SETQ CHAR 20))			;Default is the bombsight
  (WITH-MOUSE-GRABBED
    ;; Wait until mouse buttons are released.
    (PROCESS-WAIT "Button up" #'(LAMBDA ()
				  (ZEROP MOUSE-LAST-BUTTONS)))
    (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 0 :ON :SET-CHARACTER CHAR)
    (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION DOC)
    ;; Wait until a mouse button is pressed.
    (PROCESS-WAIT PROMPT #'(LAMBDA ()
			     (NOT (ZEROP MOUSE-LAST-BUTTONS))))
    (SETQ X MOUSE-X
	  Y MOUSE-Y)
    (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION NIL)
    (AND (LOGTEST 1 MOUSE-LAST-BUTTONS)		;Check left mouse button.
	 ;; Find a window which contains the mouse.
	 (DOLIST (W WINDOW-EDGE-ALIST)
	   (AND (>= X (THIRD W)) (>= Y (FOURTH W)) (< X (FIFTH W)) (< Y (SIXTH W))
		(RETURN (SETQ WINDOW W))))))
  WINDOW)

(DEFUN SCREEN-EDITOR-FIND-POINT (CHAR PROMPT DOC &AUX X Y)
  "Ask the user to specify a point in MOUSE-SHEET with the mouse.

CHAR if non-NIL is used for the mouse cursor,
PROMPT appears in the who line, and DOC in the mouse documentation
		string.

The values are relative to MOUSE-SHEET."
  (DECLARE (VALUES X Y))
  (OR CHAR (SETQ CHAR 20))			;Default is the bombsight
  (WITH-MOUSE-GRABBED
    ;; Wait until mouse buttons are released.
    (PROCESS-WAIT "Button up" #'(LAMBDA ()
				  (ZEROP MOUSE-LAST-BUTTONS)))
    (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 0 :ON :SET-CHARACTER CHAR)
    (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION DOC)
    ;; Wait until a mouse button is pressed.
    (PROCESS-WAIT PROMPT #'(LAMBDA ()
			     (NOT (ZEROP MOUSE-LAST-BUTTONS))))
    (IF (LOGTEST 1 MOUSE-LAST-BUTTONS)		;Check left mouse button.
	(SETQ X MOUSE-X
	      Y MOUSE-Y)))
  (VALUES X Y))

(DEFUN MOUSE-SPECIFY-EXPAND (SUPERIOR)
  "Ask user for point and return edges from expanding a window at
that point.  Returns NIL if user aborts."
  (MULTIPLE-VALUE-BIND (X Y)
      (SCREEN-EDITOR-FIND-POINT
        NIL
        "Button"
        '(:MOUSE-L-1 "Choose a point to expand around"
          :MOUSE-M-1 "aborts the expand operation."))
    (COND ((NULL X) NIL)
	  (T (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
		 (EXPAND-RECTANGULAR-AREA SUPERIOR X Y X Y
					  (EXPOSED-INFERIOR-CONTAINING-POINT SUPERIOR X Y))
	       (IF (OR (<= RIGHT LEFT)
		       (<= BOTTOM TOP))
		   ;; No expansion, return NIL.
		   NIL
		   ;; Worked OK, return the new edges.
		   (VALUES LEFT TOP RIGHT BOTTOM)))))))

(DEFUN EXPOSED-INFERIOR-CONTAINING-POINT (SUPERIOR X Y)
  "Return the exposed inferior of SUPERIOR that contains point X and
Y.  X and Y are relative to SUPERIOR.  Returns NIL if there is no such
exposed inferior."
  (DOLIST (W (SHEET-EXPOSED-INFERIORS SUPERIOR))
    (IF (SHEET-CONTAINS-SHEET-POINT-P W SUPERIOR X Y)
	(RETURN W))))

;;; Attribute command.

(DEFUN SEC-ATTRIBUTES (IGNORE WINDOW-EDGE-ALIST)
  (LET ((WINDOW
	 (SCREEN-EDITOR-FIND-WINDOW WINDOW-EDGE-ALIST NIL
	     "Attributes"
             '(:MOUSE-L-1 "Choose a window to edit the attributes of"
               :MOUSE-M-1 "aborts the change attributes operation."))))
    (COND ((NULL WINDOW)
	   'ABORT)
	  (T
	   (SCREEN-EDITOR-EDIT-ATTRIBUTES (CAR WINDOW))
	   WINDOW-EDGE-ALIST))))

(DEFUN MAKE-ATTRIBUTES-LIST (WINDOW BORDERS-P LABEL-P NAME-P)
  `((CURRENT-FONT-VALUE "Current font" :DOCUMENTATION
     "Set the current font to one of the fonts in the font map." :ASSOC
     ,(LOOP FOR FONT BEING THE ARRAY-ELEMENTS OF (SEND WINDOW :FONT-MAP)
           WHEN (AND (NOT (NULL FONT)) (NOT (MEMBER FONT FONT-LIST :TEST #'EQUAL)))
           COLLECT (CONS (FONT-NAME FONT) FONT) INTO ANSWER AND
           COLLECT FONT INTO FONT-LIST
           FINALLY (RETURN ANSWER)))
    (MORE-P-VALUE "More processing enabled" :DOCUMENTATION
     "Enable typing **MORE** and waiting for typein when there is too much typeout." :ASSOC
     (("Yes" . T) ("No")))
    (REVERSE-VIDEO-P "Reverse video" :DOCUMENTATION
     "Reverse default foreground/background colors for this window." :ASSOC (("Yes" . T) ("No")))
    (VSP "Vertical spacing" :DOCUMENTATION
     "The number of pixels between successive lines of printed text" :FIXNUM)
    (IN-ACTION "Deexposed typein action" :DOCUMENTATION
     "What to do if input is attempted while this window is deexposed." :ASSOC
     (("Wait until exposed" . :NORMAL) ("Notify user" . :NOTIFY)))
    (OUT-ACTION "Deexposed typeout action" :DOCUMENTATION
     "What to do if output is attempted while this window is deexposed." :ASSOC
     (("Wait until exposed" . :NORMAL) ("Notify user" . :NOTIFY) ("Let it happen" . :PERMIT)
      ("Signal error" . :ERROR) ("Other" . :OTHER)))
    (OTHER-OUT-ACTION "(\"Other\" value of above)" :DOCUMENTATION
     "If \"Deexposed typeout action\" is Other, use this form instead." :SEXP)
    (CHAR-ALU-FCN "ALU function for drawing" :DOCUMENTATION
     "The ALU function for drawing characters and graphics." :ASSOC
     (("IOR" . ,ALU-IOR) ("ANDCA" . ,ALU-ANDCA) ("XOR" . ,ALU-XOR)))
    (ERASE-ALU-FCN "ALU function for erasing" :DOCUMENTATION
     "The ALU function for erasing pieces of the window." :ASSOC
     (("IOR" . ,ALU-IOR) ("ANDCA" . ,ALU-ANDCA) ("XOR" . ,ALU-XOR)))
    (PRIORITY-VALUE "Screen manager priority" :DOCUMENTATION
     "Set screen manager priority.  NIL is the usual thing." :FIXNUM-OR-NIL)
    (SAVE-BITS-VALUE "Save bits" :DOCUMENTATION
     "Should the contents of the window be saved away when the window is deexposed?" :ASSOC
     (("Yes" . T) ("No")))
    ,@(COND
	(NAME-P
	 '((LABEL-OR-NAME "Name of window" :DOCUMENTATION "Set the name of the window" :STRING)))
	(LABEL-P '((LABEL-OR-NAME "Label" :DOCUMENTATION "Set the label of the window" :STRING))))
    ,@(COND
	(BORDERS-P
	 '((BORDERS-SPEC "Width of borders" :DOCUMENTATION "Set the widths of the borders"
	    :FIXNUM)
	   (BORDER-MARGIN-WIDTH-VALUE "Width of border margins" :DOCUMENTATION
	    "Set the width of the margin in between the borders and the window contents" :FIXNUM))))))



;;; use this next version when color is active

(DEFUN color-MAKE-ATTRIBUTES-LIST (WINDOW BORDERS-P LABEL-P NAME-P)
 (let ((local-side-effect (gensym))
       fore-list back-list border-list label-list label-back-list)
  (setf (symbol-function local-side-effect)
	#'(lambda (w s o n)
	    (w:CVV-GET-COLOR w s o n window)))
  (setq fore-list `(foreground "Foreground Color"
			       :side-effect ,local-side-effect  ;;w:CVV-GET-COLOR
			       :documentation "The color text is drawn."  
			       :menu `(("Color Names" :font fonts:hl12b :color ,white :no-select)
				       ,@w:color-alist ("" :no-select nil)
				       ("Other" :font fonts:hl12b :color ,white :no-select)
				       ("Show Color Map" :value :color-map)
				       ("Enter Color Number" :value :color-number))))
  (setq back-list `(background "Background Color" :side-effect ,local-side-effect ;;w:CVV-GET-COLOR
			       :documentation "The color the window becomes when erased."  
			       :menu `(("Color Names" :font fonts:hl12b :color ,white :no-select)
				       ,@w:color-alist ("" :no-select nil)
				       ("Other" :font fonts:hl12b :color ,white :no-select)
				       ("Show Color Map" :value :color-map)
				       ("Enter Color Number" :value :color-number))))
  (setq border-list `(border-color "Border Color" :side-effect ,local-side-effect  ;;W:CVV-GET-COLOR	
				   :documentation "The color of the window's border"  
				   :menu `(("Color Names" :font fonts:hl12b :color ,white :no-select)
					   ,@w:color-alist ("" :no-select nil)
					   ("Other" :font fonts:hl12b :color ,white :no-select)
					   ("Show Color Map" :value :color-map)
					   ("Enter Color Number" :value :color-number))))
  (setq label-list `(label-color "Label Color" :side-effect ,local-side-effect  ;;W:CVV-GET-COLOR  
				 :documentation "The color of the LABEL text."  
				 :menu `(("Color Names" :font fonts:hl12b :color ,white :no-select)	
					 ,@w:color-alist ("" :no-select nil)
					 ("Other" :font fonts:hl12b :no-select)
					 ("Show Color Map" :value :color-map)
					 ("Enter Color Number" :value :color-number))))
  (setq label-back-list `(label-background "Label Background Color" :side-effect ,local-side-effect  ;;W:CVV-GET-COLOR  
					   :documentation "The color of the label area"  
					   :menu `(("Color Names" :font fonts:hl12b :color ,white :no-select)
						   ,@w:color-alist ("" :no-select nil)
						   ("Other" :font fonts:hl12b :no-select)
						   ("Show Color Map" :value :color-map)
						   ("Enter Color Number" :value :color-number))))
  `((CURRENT-FONT-VALUE "Current font" :DOCUMENTATION
     "Set the current font to one of the fonts in the font map." :ASSOC
     ,(LOOP FOR FONT BEING THE ARRAY-ELEMENTS OF (SEND WINDOW :FONT-MAP)
           WHEN (AND (NOT (NULL FONT)) (NOT (MEMBER FONT FONT-LIST :TEST #'EQUAL)))
           COLLECT (CONS (FONT-NAME FONT) FONT) INTO ANSWER AND
           COLLECT FONT INTO FONT-LIST
           FINALLY (RETURN ANSWER)))
    (MORE-P-VALUE "More processing enabled" :DOCUMENTATION
     "Enable typing **MORE** and waiting for typein when there is too much typeout." :ASSOC
     (("Yes" . T) ("No")))
    (REVERSE-VIDEO-P "Reverse video" :DOCUMENTATION
     "Reverse default foreground/background colors for this window." :ASSOC (("Yes" . T) ("No")))
    (VSP "Vertical spacing" :DOCUMENTATION
     "The number of pixels between successive lines of printed text" :FIXNUM)
;    (FOREGROUND "Foreground Color" :DOCUMENTATION
;     "The color that text is drawn" :fixnum)
    ,fore-list
;    (BACKGROUND "Background Color" :DOCUMENTATION
;     "The color underneath the text" :fixnum )
    ,back-list
    (IN-ACTION "Deexposed typein action" :DOCUMENTATION
     "What to do if input is attempted while this window is deexposed." :ASSOC
     (("Wait until exposed" . :NORMAL) ("Notify user" . :NOTIFY)))
    (OUT-ACTION "Deexposed typeout action" :DOCUMENTATION
     "What to do if output is attempted while this window is deexposed." :ASSOC
     (("Wait until exposed" . :NORMAL) ("Notify user" . :NOTIFY) ("Let it happen" . :PERMIT)
      ("Signal error" . :ERROR) ("Other" . :OTHER)))
    (OTHER-OUT-ACTION "(\"Other\" value of above)" :DOCUMENTATION
     "If \"Deexposed typeout action\" is Other, use this form instead." :SEXP)
    (CHAR-ALU-FCN "ALU function for drawing" :DOCUMENTATION
     "The ALU function for drawing characters and graphics." :ASSOC
     (("IOR" . ,ALU-IOR) ("ANDCA" . ,ALU-ANDCA) ("XOR" . ,ALU-XOR) ("TRANSPARENT" . ,ALU-TRANSP)))
    (ERASE-ALU-FCN "ALU function for erasing" :DOCUMENTATION
     "The ALU function for erasing pieces of the window." :ASSOC
     (("IOR" . ,ALU-IOR) ("ANDCA" . ,ALU-ANDCA) ("XOR" . ,ALU-XOR) ("BACKGROUND" . ,ALU-BACK)))
    (PRIORITY-VALUE "Screen manager priority" :DOCUMENTATION
     "Set screen manager priority.  NIL is the usual thing." :FIXNUM-OR-NIL)
    (SAVE-BITS-VALUE "Save bits" :DOCUMENTATION
     "Should the contents of the window be saved away when the window is deexposed?" :ASSOC
     (("Yes" . T) ("No")))
    ,@(COND
	(NAME-P
	 '((LABEL-OR-NAME "Name of window" :DOCUMENTATION "Set the name of the window" :STRING)))
	(LABEL-P
	 `( (LABEL-OR-NAME "Label" :DOCUMENTATION "Set the label of the window" :STRING)
             ,label-list
             ,label-back-list
;            (LABEL-COLOR "Label Color" :DOCUMENTATION "The color that labels are drawn with" :fixnum)
;            (LABEL-BACKGROUND  "Label Background Color" :DOCUMENTATION "The color underneath labels" :fixnum)
          )
	 ))
    ,@(COND
	(BORDERS-P
	 `((BORDERS-SPEC "Width of borders" :DOCUMENTATION "Set the widths of the borders"
	    :FIXNUM)
	   (BORDER-MARGIN-WIDTH-VALUE "Width of border margins" :DOCUMENTATION
	    "Set the width of the margin in between the borders and the window contents" :FIXNUM)
            ,border-list
;           (BORDER-COLOR "Border Color" :DOCUMENTATION
;            "The color that borders are drawn with" :fixnum)
	  ) )))))

(DEFUN SCREEN-EDITOR-EDIT-ATTRIBUTES (WINDOW)
  (LET ((BORDERS-P (SEND WINDOW :OPERATION-HANDLED-P :SET-BORDERS))
	(LABEL-P (SEND WINDOW :OPERATION-HANDLED-P :SET-LABEL))
	(NAME-P (SEND WINDOW :OPERATION-HANDLED-P :SET-NAME)))
    ;; We need to save the old/new values because that is the only way
    ;; we can tell if something has been changed.
    (LET* ((CURRENT-FONT-VALUE (SEND WINDOW :CURRENT-FONT))
	   (OLD-CURRENT-FONT-VALUE CURRENT-FONT-VALUE)
	   (MORE-P-VALUE (SEND WINDOW :MORE-P))
	   (OLD-MORE-P-VALUE MORE-P-VALUE)
	   (REVERSE-VIDEO-P (SEND WINDOW :REVERSE-VIDEO-P))
	   (OLD-REVERSE-VIDEO-P REVERSE-VIDEO-P)
	   (VSP (SEND WINDOW :VSP))
	   (OLD-VSP VSP)
	   (foreground (SEND window :foreground-color))
	   (old-foreground foreground)
	   (background (SEND window :background-color))
	   (old-background background)
	   (IN-ACTION (SEND WINDOW :DEEXPOSED-TYPEIN-ACTION))
	   (OLD-IN-ACTION IN-ACTION)
	   (OUT-ACTION (SEND WINDOW :DEEXPOSED-TYPEOUT-ACTION))
	   (OLD-OUT-ACTION OUT-ACTION)
	   (OTHER-OUT-ACTION)
	   (OLD-OTHER-OUT-ACTION)
	   (CHAR-ALU-FCN (SEND WINDOW :CHAR-ALUF))
	   (OLD-CHAR-ALU-FCN CHAR-ALU-FCN)
	   (ERASE-ALU-FCN (SEND WINDOW :ERASE-ALUF))
	   (OLD-ERASE-ALU-FCN ERASE-ALU-FCN)
	   (PRIORITY-VALUE (SEND WINDOW :PRIORITY))
	   (OLD-PRIORITY-VALUE PRIORITY-VALUE)
	   (SAVE-BITS-VALUE (SEND WINDOW :SAVE-BITS))
	   (OLD-SAVE-BITS-VALUE SAVE-BITS-VALUE)
	   (LABEL-OR-NAME (COND
			    (NAME-P (SEND WINDOW :NAME))
			    (LABEL-P (SEND WINDOW :LABEL))))
	   (OLD-LABEL-OR-NAME LABEL-OR-NAME)
	   (label-color (IF label-p
	     (SEND window :label-color)))
	   (old-label-color label-color)
	   (label-background (IF label-p
	     (SEND window :label-background)))
	   (old-label-background label-background)
	   (BORDERS-SPEC (IF BORDERS-P
			     (SEND WINDOW :BORDERS)))
	   (OLD-BORDERS-SPEC BORDERS-SPEC)
	   (border-color (IF borders-p
			     (SEND window :border-color)))
	   (old-border-color border-color)
	   (BORDER-MARGIN-WIDTH-VALUE (IF BORDERS-P
					  (SEND WINDOW :BORDER-MARGIN-WIDTH)))
	   (OLD-BORDER-MARGIN-WIDTH-VALUE BORDER-MARGIN-WIDTH-VALUE))
      (DECLARE
	(SPECIAL BORDER-MARGIN-WIDTH-VALUE CURRENT-FONT-VALUE MORE-P-VALUE REVERSE-VIDEO-P
		 BORDERS-SPEC LABEL-OR-NAME VSP foreground background border-color 
                 label-color label-background IN-ACTION CHAR-ALU-FCN
		 ERASE-ALU-FCN PRIORITY-VALUE
		 SAVE-BITS-VALUE OUT-ACTION OTHER-OUT-ACTION))
      (IF (CONSP BORDERS-SPEC)			;********************
	  (IF (MEMBER (CAR BORDERS-SPEC) '(NIL :ZERO) :TEST #'EQ)
	      (SETQ BORDERS-SPEC 0)
	      (SETQ BORDERS-SPEC (- (FOURTH (FIRST BORDERS-SPEC)) (SECOND (FIRST BORDERS-SPEC))))))
      (IF (CONSP LABEL-OR-NAME)
	  (SETQ LABEL-OR-NAME (SIXTH LABEL-OR-NAME)))
      (COND
	((NOT (MEMBER OUT-ACTION '(:NORMAL :NOTIFY :PERMIT :ERROR) :TEST #'EQ))
	 (SETQ OTHER-OUT-ACTION OUT-ACTION
	       OLD-OTHER-OUT-ACTION OTHER-OUT-ACTION
	       OUT-ACTION :OTHER
	       OLD-OUT-ACTION OUT-ACTION)))
      (LET ((ABORT-P
	      (CATCH 'ABORT-EDIT
		(CHOOSE-VARIABLE-VALUES   (if (color-system-p window)
                                             (color-MAKE-ATTRIBUTES-LIST WINDOW BORDERS-P LABEL-P NAME-P)
                                             (MAKE-ATTRIBUTES-LIST WINDOW BORDERS-P LABEL-P NAME-P)
                                          )
					:LABEL `(:string 
					  ,(FORMAT NIL "Edit window attributes of ~A." WINDOW)
					  :color ,tv:*default-menu-label-foreground*
					  :background ,tv:*default-menu-label-background*)
					:MARGIN-CHOICES
					`((,W:MARGIN-CHOICE-ABORT-STRING (THROW 'ABORT-EDIT T))
                                          ,W:MARGIN-CHOICE-COMPLETION-STRING) :FUNCTION
					'ATTRIBUTE-EDITOR-HOOK))))
	(COND
	  (ABORT-P (BEEP))
	  (T
	   ;; Here is where we check to see if something has changed
	   ;; and update its value.
	   (IF (NEQ MORE-P-VALUE OLD-MORE-P-VALUE)
	       (SEND WINDOW :SET-MORE-P MORE-P-VALUE))
	   (IF (NEQ REVERSE-VIDEO-P OLD-REVERSE-VIDEO-P)
	       (SEND WINDOW :SET-REVERSE-VIDEO-P REVERSE-VIDEO-P))
	   (IF (NEQ foreground old-foreground)
	       (progn
                 (SEND window :set-foreground-color foreground)
;		 (SEND window :refresh)
	       ))
	   (IF (NEQ background old-background)
	       (progn
                 (SEND window :set-background-color background)
;		 (SEND window :refresh)
	       ))
	   (IF (NEQ border-color old-border-color)
	       (IF borders-p
                   (SEND window :set-border-color border-color)))
	   (IF (NEQ label-color old-label-color)
	       (IF label-p
        	       (SEND window :set-label-color label-color)))
	   (IF (NEQ label-background old-label-background)
	       (IF label-p
        	       (SEND window :set-label-background label-background)))
	   (IF (NEQ VSP OLD-VSP)
	       (SEND WINDOW :SET-VSP VSP))
	   (IF (NEQ IN-ACTION OLD-IN-ACTION)
	       (SEND WINDOW :SET-DEEXPOSED-TYPEIN-ACTION IN-ACTION))
	   (COND
	     ((NEQ OUT-ACTION OLD-OUT-ACTION)
	      (SEND WINDOW :SET-DEEXPOSED-TYPEOUT-ACTION
		    (IF (EQ OUT-ACTION :OTHER)
			OTHER-OUT-ACTION
			OUT-ACTION)))
	     ((AND (EQ OUT-ACTION :OTHER) (NEQ OTHER-OUT-ACTION OLD-OTHER-OUT-ACTION))
	      (SEND WINDOW :SET-DEEXPOSED-TYPEOUT-ACTION OTHER-OUT-ACTION)))
	   (IF (NEQ CHAR-ALU-FCN OLD-CHAR-ALU-FCN)
	       (SEND WINDOW :SET-CHAR-ALUF CHAR-ALU-FCN))
	   (IF (NEQ ERASE-ALU-FCN OLD-ERASE-ALU-FCN)
	       (SEND WINDOW :SET-ERASE-ALUF ERASE-ALU-FCN))
	   (IF (NEQ PRIORITY-VALUE OLD-PRIORITY-VALUE)
	       (SEND WINDOW :SET-PRIORITY PRIORITY-VALUE))
	   (IF (NEQ SAVE-BITS-VALUE OLD-SAVE-BITS-VALUE)
	       (SEND WINDOW :SET-SAVE-BITS SAVE-BITS-VALUE))
	   (IF (NEQ LABEL-OR-NAME OLD-LABEL-OR-NAME)
	       (COND
		 (NAME-P (SEND WINDOW :SET-NAME LABEL-OR-NAME))
		 ((AND LABEL-P OLD-LABEL-OR-NAME)
		  (SETF (SIXTH OLD-LABEL-OR-NAME) LABEL-OR-NAME)
		  (SEND WINDOW :SET-LABEL OLD-LABEL-OR-NAME))))
	   ; Set the font after the label, so that the label stuff doesn't undo a new current font.
	   (IF (NEQ CURRENT-FONT-VALUE OLD-CURRENT-FONT-VALUE)
	       (SEND WINDOW :SET-CURRENT-FONT CURRENT-FONT-VALUE))
	   (COND
	     (BORDERS-P
	      (IF (NEQ BORDERS-SPEC OLD-BORDERS-SPEC)
		  (SEND WINDOW :SET-BORDERS BORDERS-SPEC))
	      (IF (NEQ BORDER-MARGIN-WIDTH-VALUE OLD-BORDER-MARGIN-WIDTH-VALUE)
		  (SEND WINDOW :SET-BORDER-MARGIN-WIDTH BORDER-MARGIN-WIDTH-VALUE))))))))))

;;; removed obsolete checks for reverse-video differences in color and B&W
;;; PMH  4/6/88
(DEFUN ATTRIBUTE-EDITOR-HOOK (WINDOW VARIABLE OLD-VALUE NEW-VALUE)
  "Called every time an item in the Edit Attributes menu is changed."
  OLD-VALUE;unused
  (LOCALLY (DECLARE (SPECIAL OTHER-OUT-ACTION))
     (COND
       ((AND (EQ VARIABLE 'OUT-ACTION)
	     (NEQ NEW-VALUE :OTHER)
	     OTHER-OUT-ACTION)
	(SETQ OTHER-OUT-ACTION NIL)
	(SEND WINDOW :REFRESH)
	T)
       (T NIL))))



;;; Hairy movement commands.  These are used by the MOVE MULTIPLE menu
;;; item in the System Manu.
(DEFFLAVOR FOLLOWING-ARROW-BLINKER
	(
         ;; From the base flavor BLINKER, the instance variables X-POS
         ;; and Y-POS specify the location of the upper left part of
         ;; the blinker.  Here, if the X-ORIGIN or Y-ORIGIN are
         ;; non-nil they specify a second point at which the arrow
         ;; blinker should point.
         X-ORIGIN Y-ORIGIN
         ;; The following four instance variables specify parameters
         ;; which describe the following arrow.  The arrow can be
         ;; arbitrarily large.  The following are what the parameters
         ;; describe:
         ;;
         ;;                                                     /
         ;;                                                   /
         ;;                                                 /
         ;;                                                    TRI-HEIGHT
         ;;                                              |\              /
         ;;                                              |XX\          /
         ;;----------        .---------------------------'XXXX\      /
         ;;RECT-WIDTH        |XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
         ;;----------        |XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX>    ---------
         ;;                  |XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX/
         ;;                  `---------------------------.XXXX/        TRI-WIDTH
         ;;                                              |XX/
         ;;                                              |/            _________
         ;;
         ;;
         ;;                  |                           |
         ;;                  |        RECT-HEIGHT        |
         ;;                  |                           |
         ;;
         ;; The RECT-WIDTH is half of the width of the shaft.
         ;; The unit of these instance variables is pixels.
	 TRI-WIDTH TRI-HEIGHT
	 RECT-WIDTH RECT-HEIGHT
         ;; The STATE instance variable contains a list of the
         ;; vertices used to draw the arrow.  It is used mostly
         ;; make moving the arrow a reasonably fast operation.
	 STATE)
	(BLINKER)
  (:INITABLE-INSTANCE-VARIABLES X-ORIGIN Y-ORIGIN TRI-WIDTH TRI-HEIGHT
				RECT-WIDTH RECT-HEIGHT))

(DEFMETHOD (FOLLOWING-ARROW-BLINKER :BEFORE :INIT) (IGNORE)
  (SETQ STATE (MAKE-LIST 12.)))

(DEFUN MAKE-FOLLOWING-ARROW-BLINKER
       (SHEET X-ORIGIN Y-ORIGIN TRI-WIDTH TRI-HEIGHT RECT-WIDTH RECT-HEIGHT &REST OPTIONS)
  (APPLY #'MAKE-BLINKER SHEET 'FOLLOWING-ARROW-BLINKER :X-ORIGIN X-ORIGIN :Y-ORIGIN Y-ORIGIN
	 :TRI-WIDTH TRI-WIDTH :TRI-HEIGHT TRI-HEIGHT :RECT-WIDTH RECT-WIDTH :RECT-HEIGHT
	 RECT-HEIGHT OPTIONS))

(DEFMETHOD (FOLLOWING-ARROW-BLINKER :SIZE) ()
  (BLOCK () (RETURN (MAX TRI-WIDTH RECT-WIDTH)
		   (+ TRI-HEIGHT RECT-HEIGHT))))

(DEFUN SET-FOLLOWING-ARROW-BLINKER-ORIGIN (BLINKER X-ORIGIN Y-ORIGIN)
  (SEND BLINKER :SET-ORIGIN X-ORIGIN Y-ORIGIN))					

(DEFMETHOD (FOLLOWING-ARROW-BLINKER :SET-ORIGIN) (NX-ORIGIN NY-ORIGIN)
  (WITHOUT-INTERRUPTS
    ;; Turn the blinker off before moving it.  This insures that we
    ;; will not have a ghost blinker.
    (OPEN-BLINKER SELF)
    (SETQ X-ORIGIN NX-ORIGIN Y-ORIGIN NY-ORIGIN)))

(DEFMETHOD (FOLLOWING-ARROW-BLINKER :BLINK) (&AUX X0 Y0 X2 Y2 X3 Y3 X4 Y4 X5 Y5 X6 Y6 X7 Y7)
  "Draw the following-arrow blinker."
  ;; The arrow is actually drawn as a series of 5 filled triangles each
  ;; having the point of the arrow as a common point.  The figure below
  ;; shows each of the 5 filled triangles that are drawn.  The 6
  ;; vertices are labelled with digits 2-7 and the letters A-E indicate
  ;; the part shaded in by a particular filled triangle.  The vertices
  ;; of the 5 triangles that are drawn are:
  ;;
  ;;	A -- 2  4  CENTER
  ;;	B -- 4  6  CENTER
  ;;	C -- 6  7  CENTER
  ;;	D -- 7  5  CENTER
  ;;	E -- 5  3  CENTER
  ;;
  ;;                                         2
  ;;                                         |\
  ;;                                         |AA\
  ;;                                         |AAAA\
  ;;          6                            4 |AAAAAA\
  ;;           .-----------------------------'AAAAAAAA\
  ;;           |CCCCCCCCCCCCCC BBBBBBBBBBBBBBBBB AAAAAAA\
  ;;           |CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC BBBBBBB AAA\
  ;;           |CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC> CENTER
  ;;           |CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC DDDDDDD EEE/
  ;;           |CCCCCCCCCCCCCC DDDDDDDDDDDDDDDDD EEEEEEE/
  ;;           `-----------------------------.EEEEEEEE/
  ;;          7                            5 |EEEEEE/
  ;;                                         |EEEE/
  ;;                                         |EE/
  ;;                                         |/
  ;;                                         3
  ;; The reason for drawing the arrow in this fashion is to make certain
  ;; that a solid arrow is drawn for every case.  An optimization of
  ;; this case is to only draw 3 triangles (for example, (2 3 CENTER),
  ;; (4 5 6) and (5 6 7)).  For this optimization there are cases where
  ;; there is a gap near vertex 4 or vertex 5, making the arrow contain
  ;; a crack.  Therefore, we draw the arrow by drawing 5 triangles.

  ;; The digits of the vertices were chosen to match with the variable
  ;; names used in the code.  The vertex 2 corresponds to the point
  ;; (X2,Y2), the vertex 3 corresponds to the point (X3,Y3), etc.

  (COND ((NOT PHASE)
	 ;;Making it visible, recompute the vertices.
	 (LET (DX DY LEN)
	   (SETQ X0 (OR X-ORIGIN X-POS))
	   (SETQ Y0 (OR Y-ORIGIN Y-POS))
	   (SETQ DX (- X-POS X0)
		 DY (- Y-POS Y0)
		 LEN (ISQRT (+ (* DX DX) (* DY DY))))
	   (AND (ZEROP LEN)			;Right on top of where it's pointing
		(COND ((NULL X-ORIGIN)		;Straight up
		       (SETQ DY 1 LEN 1))
		      ((NULL Y-ORIGIN)		;Straight left
		       (SETQ DX 1 LEN 1))
		      (T (SETQ DX 1 DY 1 LEN 1))))	;Top-left corner
	   (SETQ X4 (+ X-POS (TRUNCATE (* DX TRI-HEIGHT) LEN))
		 Y4 (+ Y-POS (TRUNCATE (* DY TRI-HEIGHT) LEN))
		 X6 (+ X-POS (TRUNCATE (* DX (+ TRI-HEIGHT RECT-HEIGHT)) LEN))
		 Y6 (+ Y-POS (TRUNCATE (* DY (+ TRI-HEIGHT RECT-HEIGHT)) LEN)))
	   (LET ((DX1 (TRUNCATE (* TRI-WIDTH DY) LEN))
		 (DY1 (TRUNCATE (* TRI-WIDTH DX) LEN)))
	     (SETQ X2 (- X4 DX1)
		   Y2 (+ Y4 DY1)
		   X3 (+ X4 DX1)
		   Y3 (- Y4 DY1)))
	   (LET ((DX1 (TRUNCATE (* RECT-WIDTH DY) LEN))
		 (DY1 (TRUNCATE (* RECT-WIDTH DX) LEN)))
	     (SETQ X5 (+ X4 DX1)
		   Y5 (- Y4 DY1)
		   X4 (- X4 DX1)
		   Y4 (+ Y4 DY1))
	     (SETQ X7 (+ X6 DX1)
		   Y7 (- Y6 DY1)
		   X6 (- X6 DX1)
		   Y6 (+ Y6 DY1))))
	 (SETF (NTH 0 STATE) X2)
	 (SETF (NTH 1 STATE) Y2)
	 (SETF (NTH 2 STATE) X3)
	 (SETF (NTH 3 STATE) Y3)
	 (SETF (NTH 4 STATE) X4)
	 (SETF (NTH 5 STATE) Y4)
         (SETF (NTH 6 STATE) X5)
	 (SETF (NTH 7 STATE) Y5)
	 (SETF (NTH #o10 STATE) X6)
	 (SETF (NTH #o11 STATE) Y6)
	 (SETF (NTH #o12 STATE) X7)
	 (SETF (NTH #o13 STATE) Y7))
	(T
	 ;;Erasing it, use old parameters
	 (SETQ X2 (NTH 0 STATE))
	 (SETQ Y2 (NTH 1 STATE))
	 (SETQ X3 (NTH 2 STATE))
	 (SETQ Y3 (NTH 3 STATE))
	 (SETQ X4 (NTH 4 STATE))
	 (SETQ Y4 (NTH 5 STATE))
	 (SETQ X5 (NTH 6 STATE))
	 (SETQ Y5 (NTH 7 STATE))
	 (SETQ X6 (NTH #o10 STATE))
	 (SETQ Y6 (NTH #o11 STATE))
	 (SETQ X7 (NTH #o12 STATE))
	 (SETQ Y7 (NTH #o13 STATE))))
;;; >>> change to mouse alu for color system
  (IF (mac-window-p sheet)
    (WITH-STACK-LIST (x-coords x-pos x2 x4 x6 x7 x5 x3)
      (WITH-STACK-LIST (y-coords y-pos y2 y4 y6 y7 y5 y3)
	(send-draw-filled-polygon x-coords y-coords (mouse-alu phase) w:black sheet nil)))
    ;; else...
    (SYSTEM:%DRAW-shaded-TRIANGLE X-POS Y-POS X2 Y2 X4 Y4 (mouse-alu phase) t t nil  nil SHEET)
    (SYSTEM:%DRAW-shaded-TRIANGLE X-POS Y-POS X4 Y4 X6 Y6 (mouse-alu phase) t t nil  nil SHEET)
    (SYSTEM:%DRAW-shaded-TRIANGLE X-POS Y-POS X6 Y6 X7 Y7 (mouse-alu phase) t t nil  nil SHEET) 
    (SYSTEM:%DRAW-shaded-TRIANGLE X-POS Y-POS X7 Y7 X5 Y5 (mouse-alu phase) t t nil  nil SHEET)
    (SYSTEM:%DRAW-shaded-TRIANGLE X-POS Y-POS X5 Y5 X3 Y3 (mouse-alu phase) t t nil  nil SHEET))
  )

(DEFFLAVOR MOUSE-FOLLOWING-ARROW-BLINKER () (MOUSE-BLINKER-MIXIN FOLLOWING-ARROW-BLINKER))
(COMPILE-FLAVOR-METHODS MOUSE-FOLLOWING-ARROW-BLINKER)
(MOUSE-DEFINE-BLINKER-TYPE :FOLLOWING-ARROW
			   #'(LAMBDA (SCREEN)
			       (MAKE-BLINKER SCREEN 'MOUSE-FOLLOWING-ARROW-BLINKER :X-ORIGIN 0
					     :Y-ORIGIN 0 :TRI-WIDTH 10 :TRI-HEIGHT 20
					     :RECT-WIDTH 4 :RECT-HEIGHT 32 :VISIBILITY NIL)))

(DEFUN FIND-EDGE-OR-CORNER (WINDOW-EDGE-ALIST DOC-INSIDE DOC-OUTSIDE)
  (block KLUDGE
	()
	(WITH-MOUSE-GRABBED
	  ;; Initialize mouse blinker to small X
	  (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 3 3 :ON :SET-CHARACTER MOUSE-GLYPH-THIN-CROSS)
	  (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION DOC-OUTSIDE)
	  (DO ((MODE :OUT)
	       (X0)
	       (Y0)
	       (OLD-X MOUSE-LAST-X MOUSE-LAST-X)	;Don't use MOUSE-X, it changes too fast, so
	       (OLD-Y MOUSE-LAST-Y MOUSE-LAST-Y)	;this process runs too much and slows down
	       (WINDOW-AND-EDGES)		;the tracking of the following arrow
	       (NEW-WINDOW-AND-EDGES)
	       (NEW-MODE))
	      (NIL)
	    (COND
	      ((SETQ NEW-WINDOW-AND-EDGES
		     ;; Find a window that contains the mouse.
		     (DOLIST (WINDOW-AND-EDGES WINDOW-EDGE-ALIST)
		       (AND (>= OLD-X (THIRD  WINDOW-AND-EDGES))
			    (>= OLD-Y (FOURTH WINDOW-AND-EDGES))
			    (<  OLD-X (FIFTH  WINDOW-AND-EDGES))
			    (<  OLD-Y (SIXTH  WINDOW-AND-EDGES))
			    (RETURN WINDOW-AND-EDGES))))
	       (AND (EQ MODE ':OUT)
		    (WITHOUT-INTERRUPTS
		      (MOUSE-SET-BLINKER ':FOLLOWING-ARROW)
		      (MOUSE-WAKEUP)
		      (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION DOC-INSIDE)))
	       (LET ((LEFT (THIRD NEW-WINDOW-AND-EDGES))
		     (TOP (FOURTH NEW-WINDOW-AND-EDGES))
		     (RIGHT (FIFTH NEW-WINDOW-AND-EDGES))
		     (BOTTOM (SIXTH NEW-WINDOW-AND-EDGES)))
		 (LET (LEFT-P
		       TOP-P
		       LEFT-RIGHT-CORNER-P
		       TOP-BOTTOM-CORNER-P)
		   (LET ((ONE-THIRD (TRUNCATE (- RIGHT LEFT) 3)))
		     (SETQ LEFT-RIGHT-CORNER-P
			   (IF (SETQ LEFT-P (< OLD-X (TRUNCATE (+ LEFT RIGHT) 2)))
			       (< OLD-X (+ LEFT ONE-THIRD))
			       (> OLD-X (- RIGHT ONE-THIRD)))))
		   (LET ((ONE-THIRD (TRUNCATE (- BOTTOM TOP) 3)))
		     (SETQ TOP-BOTTOM-CORNER-P
			   (IF (SETQ TOP-P (< OLD-Y (TRUNCATE (+ TOP BOTTOM) 2)))
			       (< OLD-Y (+ TOP ONE-THIRD))
			       (> OLD-Y (- BOTTOM ONE-THIRD)))))
		   (IF (AND LEFT-RIGHT-CORNER-P TOP-BOTTOM-CORNER-P)
		       (SETQ NEW-MODE
			     (IF LEFT-P
				 (IF TOP-P
				     :TOP-LEFT
				     :BOTTOM-LEFT)
				 (IF TOP-P
				     :TOP-RIGHT
				     :BOTTOM-RIGHT)))
		       ;; DX and DY are the percentage of the way that the
		       ;; mouse is across (down for DY) the window.
		       (LET ((DX
			       (TRUNCATE (* 100 (IF LEFT-P
						    (- OLD-X LEFT)
						    (- RIGHT OLD-X)))
					 (- RIGHT LEFT)))
			     (DY
			       (TRUNCATE (* 100 (IF TOP-P
						    (- OLD-Y TOP)
						    (- BOTTOM OLD-Y)))
					 (- BOTTOM TOP))))
			 (SETQ NEW-MODE (IF (< DX DY)
					    (IF LEFT-P
						:LEFT
						:RIGHT)
					    (IF TOP-P
						:TOP
						:BOTTOM))))))
		 (COND
		   ((OR (NEQ NEW-WINDOW-AND-EDGES WINDOW-AND-EDGES) (NEQ NEW-MODE MODE))
		    (SETQ X0
			  (COND
			    ((MEMBER NEW-MODE '(:LEFT :TOP-LEFT :BOTTOM-LEFT) :TEST #'EQ) LEFT)
			    ((MEMBER NEW-MODE '(:RIGHT :TOP-RIGHT :BOTTOM-RIGHT) :TEST #'EQ) RIGHT)
			    (T NIL)))
		    (SETQ Y0
			  (COND
			    ((MEMBER NEW-MODE '(:TOP :TOP-LEFT :TOP-RIGHT) :TEST #'EQ) TOP)
			    ((MEMBER NEW-MODE '(:BOTTOM :BOTTOM-LEFT :BOTTOM-RIGHT) :TEST #'EQ)
			     BOTTOM)
			    (T NIL)))
		    ;; Set the mouse to point to the corner it is nearest.
		    (SEND MOUSE-BLINKER :SET-ORIGIN X0 Y0)
		    ;; Make the mouse blinker visible.
		    (BLINKER-SET-VISIBILITY MOUSE-BLINKER T)
		    (SETQ MODE NEW-MODE
			  WINDOW-AND-EDGES NEW-WINDOW-AND-EDGES)))))
	      ((NEQ MODE :OUT)			;Not already out
	       (SETQ MODE :OUT)
	       (WITHOUT-INTERRUPTS
		 (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 3 3 :ON :SET-CHARACTER MOUSE-GLYPH-THIN-CROSS)
		 (MOUSE-WAKEUP)
		 (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION DOC-OUTSIDE))))
	    ;; Wait for a mouse click or mouse movement.
	    (PROCESS-WAIT "Pick something"
			  #'(LAMBDA (OLD-X OLD-Y)
			      (OR (NOT (ZEROP MOUSE-LAST-BUTTONS))
				  (NOT (= MOUSE-LAST-X OLD-X))
				  (NOT (= MOUSE-LAST-Y OLD-Y))))
			  OLD-X OLD-Y)
	    ;; Get out if the user hit a mouse button.
	    (OR (ZEROP MOUSE-LAST-BUTTONS)
		(RETURN-FROM KLUDGE WINDOW-AND-EDGES MODE))))))

;;; Display a set of filled in rectangles
(DEFFLAVOR MULTIPLE-RECTANGLE-BLINKER
	((RECTANGLE-LIST))
	(BLINKER)
  (:INITABLE-INSTANCE-VARIABLES RECTANGLE-LIST))

(DEFMETHOD (MULTIPLE-RECTANGLE-BLINKER :SET-RECTANGLE-LIST) (NEW-RECTANGLE-LIST)
  (WITHOUT-INTERRUPTS
    ;; Turn off the blinker to prevent a ghost from appearing.
    (OPEN-BLINKER SELF)
    (SETQ RECTANGLE-LIST NEW-RECTANGLE-LIST)))

(DEFMETHOD (MULTIPLE-RECTANGLE-BLINKER :SIZE) ()
  (DO ((RECTS RECTANGLE-LIST (CDR RECTS))
       (RECT)
       (MIN-X0 0) (MIN-Y0 0)
       (MAX-X1 0) (MAX-Y1 0))
      ((NULL RECTS)
       (BLOCK () (RETURN (- MAX-X1 MIN-X0) (- MAX-Y1 MIN-Y0))))
    (SETQ RECT (CAR RECTS))
    (SETQ MIN-X0 (MIN MIN-X0 (FIRST RECT))
	  MIN-Y0 (MIN MIN-Y0 (SECOND RECT)))
    (SETQ MAX-X1 (MAX MAX-X1 (+ (FIRST  RECT) (THIRD RECT)))
	  MAX-Y1 (MAX MAX-Y1 (+ (SECOND RECT) (FOURTH RECT))))))

(DEFMETHOD (MULTIPLE-RECTANGLE-BLINKER :BLINK) ()
  (DOLIST (RECT RECTANGLE-LIST)
    (%DRAW-RECTANGLE-CLIPPED (THIRD RECT) (FOURTH RECT)
			     (+ X-POS (FIRST RECT)) (+ Y-POS (SECOND RECT))
			     (mouse-alu phase) SHEET)))

;;; Rectangle merger, makes an XORable set
(DEFUN ADD-RECT (LIST X Y WIDTH HEIGHT &AUX (RIGHT (+ X WIDTH)) (BOTTOM (+ Y HEIGHT)))
  (DO ((RLIST LIST (CDR RLIST))
       (RECT)
       (RBOTTOM)
       (RRIGHT))
      (NIL)
    (COND ((OR (NULL RLIST)			;If above all others, just add this one
	       (< BOTTOM (SECOND (SETQ RECT (CAR RLIST)))))
	   (PUSH (LIST X Y WIDTH HEIGHT) LIST)
	   (RETURN)))
    (COND ((< Y (SECOND RECT))			;Handle part above all others
	   (COND ((AND (= X (FIRST RECT)) (= WIDTH (THIRD RECT)))
		  (SETF (FOURTH RECT) (- (+ (SECOND RECT) (FOURTH RECT)) Y))
		  (SETF (SECOND RECT) Y)
		  (SETQ Y (+ Y (FOURTH RECT))))
		 (T
		  (PUSH (LIST X Y WIDTH (- (SECOND RECT) Y)) LIST)
		  (SETQ Y (SECOND RECT))))
	   (OR (PLUSP (SETQ HEIGHT (- BOTTOM Y))) (RETURN))))
    (COND ((> Y (SETQ RBOTTOM (+ (SECOND RECT) (FOURTH RECT)))))
	  ((= Y RBOTTOM)			;Can extend to the bottom
	   (COND ((AND (= X (FIRST RECT)) (= WIDTH (THIRD RECT)))
		  (SETF (FOURTH RECT) (- BOTTOM (SECOND RECT)))
		  (RETURN))))
	  (T					;Consider part that overlaps this rectangle
	   (COND ((NOT (OR (< RIGHT (FIRST RECT))
			   (> X (SETQ RRIGHT (+ (FIRST RECT) (THIRD RECT))))))
		  (COND ((OR (< X (FIRST RECT)) (> RIGHT RRIGHT))
			 (COND ((> Y (SECOND RECT))	;Fragment the top
				(PUSH (LIST (FIRST RECT) (SECOND RECT) (THIRD RECT)
					    (- Y (SECOND RECT)))
				      LIST)
				(SETF (SECOND RECT) Y)
				(SETF (FOURTH RECT) (- RBOTTOM Y))))
			 (COND ((< BOTTOM RBOTTOM)	;Fragment the bottom
				(PUSH (LIST (FIRST RECT) BOTTOM (THIRD RECT)
					    (- RBOTTOM BOTTOM))
				      LIST)
				(SETF (FOURTH RECT) (- Y (SECOND RECT)))))))
		  (SETF (FIRST RECT) (MIN X (FIRST RECT)))
		  (SETF (THIRD RECT) (- (MAX RIGHT RRIGHT) (FIRST RECT)))
		  (SETQ Y RBOTTOM)
		  (OR (PLUSP (SETQ HEIGHT (- BOTTOM Y))) (RETURN)))))))
  LIST)

(DEFVAR CORNER-LENGTH #o100)			;The length of displayed corners
(DEFVAR EDGE-WIDTH 8)				;The width of displayed edges
		;This width has to be more than twice the width of any borders
		;the user might be trying to move, or it looks really confusing
		;due to the XOR.

;;; Add just the corner of a window
(DEFUN ADD-CORNER (LIST LEFT-P TOP-P LEFT TOP RIGHT BOTTOM)
  (SETQ LIST (ADD-RECT LIST (IF LEFT-P LEFT (- RIGHT CORNER-LENGTH))
		       (IF TOP-P TOP (- BOTTOM EDGE-WIDTH))
		       CORNER-LENGTH EDGE-WIDTH))
  (ADD-RECT LIST (IF LEFT-P LEFT (- RIGHT EDGE-WIDTH))
	    (IF TOP-P TOP (- BOTTOM CORNER-LENGTH))
	    EDGE-WIDTH CORNER-LENGTH))

;;; Add a window's corners or edge to the movement list
;;; ON-P means only turn things on, not off
(DEFUN ADD-MOVING-WINDOW (WINDOW-AND-EDGES EDGE-OR-CORNER WINDOW-MOVEMENT-ALIST &OPTIONAL ON-P &AUX EDGES)
  (OR (SETQ EDGES (ASSOC WINDOW-AND-EDGES WINDOW-MOVEMENT-ALIST :TEST #'EQ))
      (PUSH (SETQ EDGES (LIST WINDOW-AND-EDGES NIL NIL NIL NIL)) WINDOW-MOVEMENT-ALIST))
  (CASE EDGE-OR-CORNER
	(:LEFT
	 (SETF (SECOND EDGES) (SETQ ON-P (OR (NOT (SECOND EDGES)) ON-P))))
	(:TOP
	 (SETF (THIRD EDGES) (SETQ ON-P (OR (NOT (THIRD EDGES)) ON-P))))
	(:RIGHT
	 (SETF (FOURTH EDGES) (SETQ ON-P (OR (NOT (FOURTH EDGES)) ON-P))))
	(:BOTTOM
	 (SETF (FIFTH EDGES) (SETQ ON-P (OR (NOT (FIFTH EDGES)) ON-P))))
	(:TOP-LEFT
	 (SETF (THIRD EDGES) (SETQ ON-P (OR (NOT (AND (SECOND EDGES) (THIRD EDGES))) ON-P)))
	 (SETF (SECOND EDGES) ON-P))
	(:TOP-RIGHT
	 (SETF (FOURTH EDGES) (SETQ ON-P (OR (NOT (AND (THIRD EDGES) (FOURTH EDGES))) ON-P)))
	 (SETF (THIRD EDGES) ON-P))
	(:BOTTOM-LEFT
	 (SETF (FIFTH EDGES) (SETQ ON-P (OR (NOT (AND (SECOND EDGES) (FIFTH EDGES))) ON-P)))
	 (SETF (SECOND EDGES) ON-P))
	(:BOTTOM-RIGHT
	 (SETF (FIFTH EDGES) (SETQ ON-P (OR (NOT (AND (FOURTH EDGES) (FIFTH EDGES))) ON-P)))
	 (SETF (FOURTH EDGES) ON-P))
	(:OTHERWISE (FERROR NIL "~S invalid edge/corner descriptor." EDGE-OR-CORNER)))
  (block ()
	(RETURN WINDOW-MOVEMENT-ALIST ON-P)))

;;; Return the corner or edge of a window associated with another
;;; This could somehow take frames into account, i have no idea how though
(DEFUN ASSOCIATED-CORNER-OR-EDGE
       (WINDOW-AND-EDGES CORNER-OR-EDGE OTHER-WINDOW-AND-EDGES
	&AUX LEFT TOP RIGHT BOTTOM OLEFT OTOP ORIGHT OBOTTOM)
  (SETQ LEFT (THIRD WINDOW-AND-EDGES)
	TOP (FOURTH WINDOW-AND-EDGES)
	RIGHT (FIFTH WINDOW-AND-EDGES)
	BOTTOM (SIXTH WINDOW-AND-EDGES)
	OLEFT (THIRD OTHER-WINDOW-AND-EDGES)
	OTOP (FOURTH OTHER-WINDOW-AND-EDGES)
	ORIGHT (FIFTH OTHER-WINDOW-AND-EDGES)
	OBOTTOM (SIXTH OTHER-WINDOW-AND-EDGES))
  (AND (NEQ WINDOW-AND-EDGES OTHER-WINDOW-AND-EDGES)
     (CASE CORNER-OR-EDGE
       (:LEFT (AND (= LEFT ORIGHT) (<= TOP OTOP) (>= BOTTOM OBOTTOM) :RIGHT))
       (:TOP (AND (= TOP OBOTTOM) (<= LEFT OLEFT) (>= RIGHT ORIGHT) :BOTTOM))
       (:RIGHT (AND (= RIGHT OLEFT) (<= TOP OTOP) (>= BOTTOM OBOTTOM) :LEFT))
       (:BOTTOM (AND (= BOTTOM OTOP) (<= LEFT OLEFT) (>= RIGHT ORIGHT) :TOP))
       (:TOP-LEFT
	(COND
	  ((= LEFT OLEFT) (AND (= TOP OBOTTOM) :BOTTOM-LEFT))
	  ((NOT (= LEFT ORIGHT)) NIL)
	  ((= TOP OTOP) :TOP-RIGHT)
	  ((= TOP OBOTTOM) :BOTTOM-RIGHT)))
       (:TOP-RIGHT
	(COND
	  ((= RIGHT ORIGHT) (AND (= TOP OBOTTOM) :BOTTOM-RIGHT))
	  ((NOT (= RIGHT OLEFT)) NIL)
	  ((= TOP OTOP) :TOP-LEFT)
	  ((= TOP OBOTTOM) :BOTTOM-LEFT)))
       (:BOTTOM-LEFT
	(COND
	  ((= LEFT OLEFT) (AND (= BOTTOM OTOP) :TOP-LEFT))
	  ((NOT (= LEFT ORIGHT)) NIL)
	  ((= BOTTOM OBOTTOM) :BOTTOM-RIGHT)
	  ((= BOTTOM OTOP) :TOP-RIGHT)))
       (:BOTTOM-RIGHT
	(COND
	  ((= RIGHT ORIGHT) (AND (= BOTTOM OTOP) :TOP-RIGHT))
	  ((NOT (= RIGHT OLEFT)) NIL)
	  ((= BOTTOM OBOTTOM) :BOTTOM-LEFT)
	  ((= BOTTOM OTOP) :TOP-LEFT))))))

(DEFUN CONSTRUCT-MOVEMENT-RECTANGLE-LIST (WINDOW-MOVEMENT-ALIST)
  "Make the rectangle list for a given list of window movements."
  (DO ((ALIST WINDOW-MOVEMENT-ALIST (CDR ALIST))
       (WINDOW-AND-MOVING-EDGES)
       (LIST NIL)
       (LEFT)
       (TOP)
       (RIGHT)
       (BOTTOM))
      ((NULL ALIST)
       LIST)
    (SETQ WINDOW-AND-MOVING-EDGES (CAR ALIST))
    (SETQ LEFT (THIRD (FIRST WINDOW-AND-MOVING-EDGES))
	  TOP (FOURTH (FIRST WINDOW-AND-MOVING-EDGES))
	  RIGHT (FIFTH (FIRST WINDOW-AND-MOVING-EDGES))
	  BOTTOM (SIXTH (FIRST WINDOW-AND-MOVING-EDGES)))
    ;; If there is just one corner, light it up as a corner
    (IF (MEMBER (CDR WINDOW-AND-MOVING-EDGES)
		'((T T NIL NIL) (NIL T T NIL) (NIL NIL T T) (T NIL NIL T)) :TEST #'EQUAL)
	(SETQ LIST
	      (ADD-CORNER LIST (SECOND WINDOW-AND-MOVING-EDGES) (THIRD WINDOW-AND-MOVING-EDGES)
			  LEFT TOP RIGHT BOTTOM))
	(PROGN
	  (AND (SECOND WINDOW-AND-MOVING-EDGES)	;Left
	       (SETQ LIST (ADD-RECT LIST LEFT TOP EDGE-WIDTH (- BOTTOM TOP))))
	  (AND (THIRD WINDOW-AND-MOVING-EDGES)	;Top
	       (SETQ LIST (ADD-RECT LIST LEFT TOP (- RIGHT LEFT) EDGE-WIDTH)))
	  (AND (FOURTH WINDOW-AND-MOVING-EDGES)	;Right
	       (SETQ LIST (ADD-RECT LIST (- RIGHT EDGE-WIDTH) TOP EDGE-WIDTH (- BOTTOM TOP))))
	  (AND (FIFTH WINDOW-AND-MOVING-EDGES)	;Bottom
	       (SETQ LIST (ADD-RECT LIST LEFT (- BOTTOM EDGE-WIDTH) (- RIGHT LEFT) EDGE-WIDTH)))))))

(DEFVAR MULTIPLE-MOVE-BLINKER)
(DEFVAR MULTIPLE-MOVE-RELEASE-TIME 60.)		;1 second hold-down to start moving things
(DEFFLAVOR MOUSE-MULTIPLE-RECTANGLE-BLINKER
	()
	(MOUSE-BLINKER-MIXIN MULTIPLE-RECTANGLE-BLINKER))

(COMPILE-FLAVOR-METHODS MOUSE-MULTIPLE-RECTANGLE-BLINKER)
(MOUSE-DEFINE-BLINKER-TYPE :MULTIPLE-RECTANGLE
			   #'(LAMBDA (SCREEN)
			       (MAKE-BLINKER SCREEN 'MOUSE-MULTIPLE-RECTANGLE-BLINKER
					     :RECTANGLE-LIST NIL :VISIBILITY NIL :FOLLOW-P NIL
					     :X-POS 0 :Y-POS 0)))

(DEFUN SEC-MULTIPLE-MOVE (TOP-SHEET WINDOW-EDGE-ALIST)
  "Screen editor multiple moving command."
  (INITIALIZE-MULTIPLE-MOVE-BLINKER TOP-SHEET)
  (PROCESS-WAIT "Button up" #'(LAMBDA ()
				(ZEROP MOUSE-LAST-BUTTONS)))
  (WITH-MOUSE-GRABBED
    (UNWIND-PROTECT
      (block abort
	(DO
	  ((MOVEMENT-LIST NIL)
	   (WINDOW-AND-EDGES)
	   (CORNER-OR-EDGE)
	   (ON-P)
	   (RECTANGLE-LIST))
	  (NIL)
	  (MULTIPLE-VALUE-SETQ
	    (WINDOW-AND-EDGES CORNER-OR-EDGE)
	    (FIND-EDGE-OR-CORNER WINDOW-EDGE-ALIST
				 ;; Doc inside.
				 '(:MOUSE-L-1 "Choose edge or corner"
				   :MOUSE-M-1 "aborts the moving operation"
				   :MOUSE-R-1 "starts moving.")
				 ;; Doc outside.
				 '(:MOUSE-L-1 "chooses edge or corner big arrow points to"
				   :MOUSE-M-1 "aborts the moving operation"
				   :MOUSE-R-1 "starts moving.")))
	  (AND (LOGTEST 2 MOUSE-LAST-BUTTONS)	;Middle button aborts
	       (RETURN-FROM ABORT))
	  (COND
	    ((EQ CORNER-OR-EDGE :OUT))		;Click while not pointing at anything
	    ((LOGTEST 1 MOUSE-LAST-BUTTONS)	;Left button changes things
	     (MULTIPLE-VALUE-SETQ (MOVEMENT-LIST ON-P)
				  (ADD-MOVING-WINDOW WINDOW-AND-EDGES CORNER-OR-EDGE MOVEMENT-LIST))
	     ;; If we turned things on, also turn on the associated things
	     (AND ON-P
		  (DOLIST (OTHER-WINDOW-AND-EDGES WINDOW-EDGE-ALIST)
		    (LET ((ASSOCIATED-CORNER-OR-EDGE
			    (ASSOCIATED-CORNER-OR-EDGE WINDOW-AND-EDGES CORNER-OR-EDGE
						       OTHER-WINDOW-AND-EDGES)))
		      (AND ASSOCIATED-CORNER-OR-EDGE
			   (SETQ MOVEMENT-LIST
				 (ADD-MOVING-WINDOW OTHER-WINDOW-AND-EDGES
						    ASSOCIATED-CORNER-OR-EDGE
						    MOVEMENT-LIST T))))))
	     (SETQ RECTANGLE-LIST (CONSTRUCT-MOVEMENT-RECTANGLE-LIST MOVEMENT-LIST))
	     (SEND MULTIPLE-MOVE-BLINKER :SET-RECTANGLE-LIST RECTANGLE-LIST)
	     (PROCESS-WAIT "Button"
			   #'(LAMBDA (TIME)
			       (OR (ZEROP MOUSE-LAST-BUTTONS)
				   (> (TIME-DIFFERENCE (TIME) TIME)
				      MULTIPLE-MOVE-RELEASE-TIME)))
			   (TIME))))
	  (COND
	    ((NOT (ZEROP MOUSE-LAST-BUTTONS))	;Still held down (Left or Right)?
	     (AND MOVEMENT-LIST			;Could be nothing to move
		  (SETQ WINDOW-EDGE-ALIST
			(DO-MULTIPLE-MOVE TOP-SHEET WINDOW-EDGE-ALIST RECTANGLE-LIST
					  MOVEMENT-LIST)))
	     (RETURN T)))))
      (BLINKER-SET-VISIBILITY MULTIPLE-MOVE-BLINKER NIL)))
  WINDOW-EDGE-ALIST)

(DEFUN GET-MOVEMENT-DELTA (&AUX (STARTING-X MOUSE-X) (STARTING-Y MOUSE-Y))
  (PROCESS-WAIT "Release" #'(LAMBDA () (ZEROP MOUSE-LAST-BUTTONS)))
  (PROCESS-WAIT "Moving" #'(LAMBDA () (NOT (ZEROP MOUSE-LAST-BUTTONS))))
  (block ()
    (RETURN (- MOUSE-X STARTING-X) (- MOUSE-Y STARTING-Y))))

(DEFUN RELATIVE-RECTANGLE-LIST (LIST &AUX (MIN-X #o177777) (MIN-Y #o177777))
  "Make an absolute rectangle list relative to its upper-left corner for
moving with mouse."
  (DOLIST (RECT LIST)
    (SETQ MIN-X (MIN MIN-X (FIRST  RECT))
	  MIN-Y (MIN MIN-Y (SECOND RECT))))
  (DOLIST (RECT LIST)
    (SETF (FIRST  RECT) (- (FIRST  RECT) MIN-X))
    (SETF (SECOND RECT) (- (SECOND RECT) MIN-Y)))
  (BLOCK () (RETURN MIN-X MIN-Y LIST)))

(DEFUN INITIALIZE-MULTIPLE-MOVE-BLINKER (TOP-SHEET)
  (LET ((BL (MOUSE-GET-BLINKER :MULTIPLE-RECTANGLE TOP-SHEET)))
    (SEND BL :SET-RECTANGLE-LIST NIL)
    (SEND BL :SET-CURSORPOS 0 0)
    (SEND BL :SET-VISIBILITY T)
    (SETQ MULTIPLE-MOVE-BLINKER BL)))

(DEFUN DO-MULTIPLE-MOVE (TOP-SHEET WINDOW-EDGE-ALIST RECTANGLE-LIST MOVEMENT-LIST)
  (WITH-MOUSE-GRABBED
    (WITHOUT-INTERRUPTS				;Don't spastically appear at the top left,
      (OPEN-BLINKER MULTIPLE-MOVE-BLINKER)	; and open while destructively modifying
      (MULTIPLE-VALUE-BIND (X Y L)		; the blinker's RECTANGLE-LIST.
	  (RELATIVE-RECTANGLE-LIST RECTANGLE-LIST)
	(MOUSE-SET-BLINKER-DEFINITION :MULTIPLE-RECTANGLE 0 0 T :SET-RECTANGLE-LIST L)
	(MOUSE-WARP X Y)))
    (SETQ WHO-LINE-MOUSE-GRABBED-DOCUMENTATION
	  '(:MOUSE-L-1 "Move edges and corners"
	    :MOUSE-M-1 "aborts the moving operation."))
    (DO ((MIN-X (SHEET-INSIDE-LEFT   TOP-SHEET))
	 (MAX-X (SHEET-INSIDE-RIGHT  TOP-SHEET))
	 (MIN-Y (SHEET-INSIDE-TOP    TOP-SHEET))
	 (MAX-Y (SHEET-INSIDE-BOTTOM TOP-SHEET))
	 (NEW-EDGE-ALIST)
	 (DELTA-X)
	 (DELTA-Y))
	(NIL)
      (SETQ NEW-EDGE-ALIST WINDOW-EDGE-ALIST)
      (MULTIPLE-VALUE-SETQ (DELTA-X DELTA-Y)
			   (GET-MOVEMENT-DELTA))
      (IF (LOGTEST 2 MOUSE-LAST-BUTTONS)	;Middle aborts
	  (RETURN NIL)
	  (IF (DOLIST (MOVE MOVEMENT-LIST)
		(LET ((NEW-EDGES (COPY-LIST (CAR MOVE))))
		  (AND (SECOND MOVE)
		       (LET ((NEW-LEFT (+ (THIRD NEW-EDGES) DELTA-X)))
			 (AND (OR (< NEW-LEFT MIN-X) (>= NEW-LEFT MAX-X))
			      (RETURN T))
			 (SETF (THIRD NEW-EDGES) NEW-LEFT)))
		  (AND (THIRD MOVE)
		       (LET ((NEW-TOP (+ (FOURTH NEW-EDGES) DELTA-Y)))
			 (AND (OR (< NEW-TOP MIN-Y) (>= NEW-TOP MAX-Y))
			      (RETURN T))
			 (SETF (FOURTH NEW-EDGES) NEW-TOP)))
		  (AND (FOURTH MOVE)
		       (LET ((NEW-RIGHT (+ (FIFTH NEW-EDGES) DELTA-X)))
			 (AND (OR (< NEW-RIGHT MIN-X) (>= NEW-RIGHT MAX-X))
			      (RETURN T))
			 (SETF (FIFTH NEW-EDGES) NEW-RIGHT)))
		  (AND (FIFTH MOVE)
		       (LET ((NEW-BOTTOM (+ (SIXTH NEW-EDGES) DELTA-Y)))
			 (AND (OR (< NEW-BOTTOM MIN-Y) (>= NEW-BOTTOM MAX-Y))
			      (RETURN T))
			 (SETF (SIXTH NEW-EDGES) NEW-BOTTOM)))
		  (SETQ NEW-EDGE-ALIST (SUBST NEW-EDGES (CAR MOVE) NEW-EDGE-ALIST :TEST #'EQ))))
	      (BEEP)				;Something off the screen
	      (PROGN
		(SETQ WINDOW-EDGE-ALIST NEW-EDGE-ALIST)
		(RETURN T))))))
  WINDOW-EDGE-ALIST)

(DEFUN SEC-SINGLE-MOVE (TOP-SHEET WINDOW-EDGE-ALIST)
  (INITIALIZE-MULTIPLE-MOVE-BLINKER TOP-SHEET)
  (PROCESS-WAIT "Button up" #'(LAMBDA ()
				(ZEROP MOUSE-LAST-BUTTONS)))
  (WITH-MOUSE-GRABBED
    (UNWIND-PROTECT
	(MULTIPLE-VALUE-BIND
	  (WINDOW-AND-EDGES CORNER-OR-EDGE)
	    (FIND-EDGE-OR-CORNER WINDOW-EDGE-ALIST
				 '(:MOUSE-L-1 "Choose an edge or corner"
				   :MOUSE-M-1 "aborts the moving operation.")
				 '(:MOUSE-L-1 "Move inside a window to choose an edge or corner"
				   :MOUSE-M-1 "aborts the moving operation."))
	  (COND
	    ((NOT (LOGTEST 6 MOUSE-LAST-BUTTONS))	;Middle (4) or right (2) aborts
	     (LET* ((MOVEMENT-LIST
		      (ADD-MOVING-WINDOW WINDOW-AND-EDGES CORNER-OR-EDGE NIL))
		    (RECTANGLE-LIST (CONSTRUCT-MOVEMENT-RECTANGLE-LIST MOVEMENT-LIST)))
	       (SETQ WINDOW-EDGE-ALIST
		     (DO-MULTIPLE-MOVE TOP-SHEET WINDOW-EDGE-ALIST RECTANGLE-LIST
				       MOVEMENT-LIST))))))
      (BLINKER-SET-VISIBILITY MULTIPLE-MOVE-BLINKER NIL)))
  WINDOW-EDGE-ALIST)
