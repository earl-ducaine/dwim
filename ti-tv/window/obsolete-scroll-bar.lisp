;;; -*- Mode:Common-lisp;  Package: TV; Base:10.; Fonts: (CPTFONT hl12b HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1985-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  6/25/87   KWW      Chaned alu's to use char-alu and erase-aluf
;;;   3/12/87  GRH	Added compiler:make-obsolete forms to all flavors and functions in this file.
;;;   3/06/87  TWE	Removed references to %% symbols and replaced them with the appropriate
;;;			calls to kernel functions.
;;; 12/23/86   TWE	Made several changes to make the build process cleaner.
;;;			Moved the flashy code to the beginning of this file since some other flavors (such as
;;;			SCROLL-STUFF-ON-OFF-MIXIN) require it.
;;; 10/31/86   GRH	Changed DEFINE-BLINKER to MAKE-BLINKER.
;;; 10/01/86   GRH	Deleted a required flavor option from flavor MARGIN-SCROLL-MIXIN.
;;; 10/01/86   GRH	Moved obsolete flavor WINDOW-WITH-INSIDE-SCROLL-BAR here from Baswin.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.

#|
This file contains the old window system code for basic-scroll-bar, 
margin-region-scrolling, and flashy-bump scrolling from file CHOICE
and MOUSE.  -GRH 2-20-86.
|#


;Flashy Scrolling

;If you move the mouse slowly out the top or bottom of a window that
;has this flavor, it gets scrolled up or down by a line, and the mouse
;jumps back in so that if you keep moving it, it keeps getting scrolled.
(DEFFLAVOR FLASHY-SCROLLING-MIXIN
           ;; The specification is a list of two components.  The first
           ;; component (i.e. the CAR) corresponds to the top of the
           ;; window and the second component (i.e. the CADR)
           ;; corresponds to the bottom of the window.  The CAR of each
           ;; component specifies a horizontal (i.e. top or bottom) edge
           ;; in which the scrolling is to be in effect.  The CADR of
           ;; each component specifies a left vertical edge in which the
           ;; scrolling is to be in effect.  The third part of each
           ;; component specifies a right vertical edge in which the
           ;; scrolling is to be in effect.  The horizontal edge not
           ;; specified is the window boundary.  A vertical edge can be
           ;; one of the following:
           ;;
           ;;   Fixnum - a distance in pixels
           ;;
           ;;   Flonum - a fraction of the window's width
           ;;
           ;;   :LEFT - the left edge of the window
           ;;
           ;;   :RIGHT - the right edge of the window
           
           ((FLASHY-SCROLLING-REGION '((32. 0.8 :RIGHT) (32. 0.8 :RIGHT)))
            ;;*** I'm not sure there's any point to making this an instance variable --Moon ***
            (FLASHY-SCROLLING-MAX-SPEED 6)      ;Default to 6 inches per second
            (FLASHY-SCROLLING-BLINKER NIL))
           ()
  (:INITABLE-INSTANCE-VARIABLES FLASHY-SCROLLING-REGION FLASHY-SCROLLING-MAX-SPEED)
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:REQUIRED-METHODS :SCROLL-TO)
  (:DOCUMENTATION :MIXIN "Automatic scrolling when moving over the
margins Moving slowly out of the top or bottom of a window that includes
this and keep moving, and it will scroll up or down by a single line and
the mouse will be moved back."))

;;; Define a mouse blinker which is the up pointing fat arrow.
(MOUSE-DEFINE-BLINKER-TYPE 'FLASHY-CHARACTER
                           #'(LAMBDA (SCREEN)
                               (MAKE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
                                 :VISIBILITY NIL
                                 :FONT (SEND SCREEN :MOUSE-FONT)
                                 :CHAR 8.)))

(DEFMETHOD (FLASHY-SCROLLING-MIXIN :OVERRIDE :WHO-LINE-DOCUMENTATION-STRING) ()
  (WHEN FLASHY-SCROLLING-BLINKER	
    (IF (= (SEND MOUSE-BLINKER :CHARACTER) 8.)
        ;; Character 8. is upward pointing arrow for top of window
        "Bump blinker against top of window to scroll down by one line."
        "Bump blinker against bottom of window to scroll up by one line.")))


(DEFUN FLASHY-SCROLLING-MOUSE-MOVES (W X Y &AUX REGION TOP-P)
  (DECLARE (:SELF-FLAVOR FLASHY-SCROLLING-MIXIN))
  (COND ((AND (>= X 0) (< X WIDTH))
	 (SETQ REGION (IF (SETQ TOP-P (< Y (TRUNCATE HEIGHT 2)))
			  (FIRST FLASHY-SCROLLING-REGION)
			  (SECOND FLASHY-SCROLLING-REGION)))
	 ;; Make sure is within the appropriate region
	 (COND ((AND (SEND SELF :ENABLE-SCROLLING-P)
		     (IF TOP-P
			 (< Y (FIRST REGION))
			 ;;ELSE
			 (> Y (- HEIGHT (FIRST REGION))))
		     (> X  (FLASHY-SCROLLING-PARSE-X-SPEC (SECOND REGION)))
		     (<= X (FLASHY-SCROLLING-PARSE-X-SPEC (THIRD  REGION)))
		     (SEND SELF (IF TOP-P :SCROLL-MORE-ABOVE :SCROLL-MORE-BELOW)))
		(OR FLASHY-SCROLLING-BLINKER
		    (SETQ FLASHY-SCROLLING-BLINKER MOUSE-BLINKER))
		(COND (TOP-P
		       ;; Change the mouse blinker to the up pointing fat arrow.
		       (MOUSE-SET-BLINKER-DEFINITION 'FLASHY-CHARACTER 6 0 :ON
						     :SET-CHARACTER 8.))
		      (T
		       ;; Change the mouse blinker to the down pointing fat arrow.
		       (MOUSE-SET-BLINKER-DEFINITION 'FLASHY-CHARACTER 6 13. :ON
						     :SET-CHARACTER 10.)))
		(AND ;; If mouse is moving slowly enough
		  (OR (NULL FLASHY-SCROLLING-MAX-SPEED)
		      (< MOUSE-SPEED FLASHY-SCROLLING-MAX-SPEED))
		  ;; and out the top or bottom
		  (OR (SETQ TOP-P (<= Y 0)) (>= Y (1- HEIGHT)))
		  ;; then warp the mouse and send the appropriate message and return T
		  (MULTIPLE-VALUE-BIND (IGNORE WINDOW-Y-OFFSET)
		      (SHEET-CALCULATE-OFFSETS W MOUSE-SHEET)
		    (MOUSE-WARP MOUSE-X (+ (IF TOP-P 10. (- HEIGHT 10.)) WINDOW-Y-OFFSET))
		    ;; Express scrolling 1 line up or down by relations of lines 0 and 1
		    (SEND SELF :SCROLL-TO (IF TOP-P -1 1) :RELATIVE)
		    T)))
	       (FLASHY-SCROLLING-BLINKER
		(MOUSE-SET-BLINKER FLASHY-SCROLLING-BLINKER)
		(SETQ FLASHY-SCROLLING-BLINKER NIL)
		NIL)))))

(DEFUN FLASHY-SCROLLING-PARSE-X-SPEC (SPEC)
  (DECLARE (:SELF-FLAVOR FLASHY-SCROLLING-MIXIN))
  (COND ((FLOATP   SPEC ) (TRUNCATE (* SPEC WIDTH)))
	((INTEGERP SPEC ) SPEC)
	((EQ SPEC :RIGHT) WIDTH)
	((EQ SPEC :LEFT ) 0)
	(T (FERROR NIL "~A is illegal X position specification for flashy scrolling" SPEC))))

;Arguments are window-relative position of the mouse
;This only does the rest of the processing if the flashy scrolling didn't happen
(DEFWRAPPER (FLASHY-SCROLLING-MIXIN :MOUSE-MOVES) ((X Y) . BODY)
  `(COND ((NOT
	    ;; W:scroll window uses the flashy mixin so the combined method
	    ;; will get the obsolete compiler warning, unless...
	    (inhibit-style-warnings (FLASHY-SCROLLING-MOUSE-MOVES SELF X Y)))
          . ,BODY)))


; obsolete flavor
(DEFFLAVOR WINDOW-WITH-INSIDE-SCROLL-BAR
	() (STREAM-MIXIN BORDERS-MIXIN LABEL-MIXIN BASIC-SCROLL-BAR
	    SELECT-MIXIN DELAY-NOTIFICATION-MIXIN GRAPHICS-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "Simple window with scroll bar inside borders."))

;; used to be used by mouse-default-handler
(DEFPARAMETER SCROLL-BAR-WIDTH 40.
  "Width of scroll bar region.")

;;; The Scroll-Bar

;;; Moving out the left edge of a window that has a scroll bar causes the
;;; window's mouse handler to call this routine.
;;; While the mouse is inside the scroll bar, the mouse buttons are used
;;; to scroll the window for which the scroll bar was brought up.
;;; You can tell that you are in the scroll bar, because the mouse cursor
;;; changes to a fat doubleheaded arrow and part of the left margin of
;;; of the window is darkened, showing where in the buffer is the visible portion.

;;; The commands in the scroll bar are:
;;;   Left: Move the line next to the mouse to the top.
;;;   Left-double: Move the line next to the mouse to the bottom.
;;;   Right: Move the top line to the place where the mouse is.
;;;   Right-double: Move the bottom line to the place where the mouse is.
;;;		Note that this does not call the system menu as you would
;;;		normally expect.
;;;   Middle: Jump to a place in the "buffer" as far (proportionally) from
;;;	     the beginning as the mouse is from the top.
;;;   Middle-double: not used


;;; The window should handle the :SCROLL-POSITION message by returning these four values:
;;;  TOP-LINE-NUM - the line-number of the line currently at the top of the window
;;;  TOTAL-LINES - the total number of lines available to scroll through.
;;;  LINE-HEIGHT - the height (in pixels) of a line
;;;  N-ITEMS - the number of items displayed on the screen (there are occaisions where
;;;	       this is not trivially calcuable from the other information)
;;; The window should handle the message :SCROLL-TO line-number by scrolling
;;;  to that line, or trying to.  This handler should truncate its arg into range.

;;; A window which can use the scroll bar should send a :NEW-SCROLL-POSITION
;;;  message to itself whenever it scrolls, for any reason, mouse-related or not.
;;; This causes the scroll bar to update its display.
(DEFFLAVOR BASIC-SCROLL-BAR
           ((SCROLL-BAR T) (SCROLL-BAR-ALWAYS-DISPLAYED NIL) (SCROLL-BAR-IN NIL))
           ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:REQUIRED-METHODS :SCROLL-TO)
  (:GETTABLE-INSTANCE-VARIABLES SCROLL-BAR SCROLL-BAR-ALWAYS-DISPLAYED)
  (:INITABLE-INSTANCE-VARIABLES SCROLL-BAR SCROLL-BAR-ALWAYS-DISPLAYED))

(DEFMETHOD (BASIC-SCROLL-BAR :OVERRIDE :WHO-LINE-DOCUMENTATION-STRING) ()
  (AND SCROLL-BAR-IN
       '(:MOUSE-L-1 "this line to top"
         :MOUSE-L-2 "this line to bottom"
         :MOUSE-M-1 "percentage-wise"
         :MOUSE-R-1 "top line to here"
         :MOUSE-R-2 "bottom line to here.")))

(DEFUN DECODE-SCROLL-BAR (SPEC)
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-BAR))
  (block ()
        (RETURN (FIRST SPEC)                    ; Left
                (SECOND SPEC)                   ; Top
                (THIRD SPEC)                    ; Right
                (+ (FOURTH SPEC) HEIGHT)        ; Bottom
                )))

(DEFUN SCROLL-BAR-DRAW (&AUX ITEM-HEIGHT TOP-ITEM-NUM TOTAL-ITEMS LEFT TOP RIGHT BOTTOM
                        BAR-HEIGHT N-ITEMS BAR-TOP BAR-BOTTOM BAR-WIDTH)
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-BAR))
  (COND (SCROLL-BAR
         (MULTIPLE-VALUE-SETQ (TOP-ITEM-NUM TOTAL-ITEMS ITEM-HEIGHT N-ITEMS)
                              (SEND SELF :SCROLL-POSITION))
         (SETQ TOTAL-ITEMS (MAX TOP-ITEM-NUM TOTAL-ITEMS))      ;In case we get a bad number, don't
                                                ; blow the whole mouse process away
         (MULTIPLE-VALUE-SETQ (LEFT TOP RIGHT BOTTOM)
                              (DECODE-SCROLL-BAR SCROLL-BAR))
         (SETQ BAR-HEIGHT (MAX 0 (- BOTTOM TOP 5)))
         (OR N-ITEMS (SETQ N-ITEMS (TRUNCATE (SHEET-INSIDE-HEIGHT) ITEM-HEIGHT)))
         (SETQ BAR-TOP (FLOOR (+ TOP
                                 (* BAR-HEIGHT (IF (ZEROP TOTAL-ITEMS)
                                                   0
                                                   ;;ELSE
                                                   (/ TOP-ITEM-NUM (SMALL-FLOAT TOTAL-ITEMS)))))))
         (SETQ BAR-BOTTOM (CEILING (+ TOP (MIN 5 (- BOTTOM TOP))
                                      (* BAR-HEIGHT (IF (ZEROP TOTAL-ITEMS)
                                                        1
                                                        ;;ELSE
                                                        (/ (+ TOP-ITEM-NUM N-ITEMS)
                                                           (SMALL-FLOAT TOTAL-ITEMS)))))))
         (SETQ BAR-WIDTH (- RIGHT LEFT))
         (PREPARE-SHEET (SELF)
           ;; Erase anything there first.
           (%DRAW-RECTANGLE BAR-WIDTH (- BOTTOM TOP) LEFT TOP erase-aluf SELF)
           ;; Now we can draw the scroll bar.
           (%DRAW-RECTANGLE BAR-WIDTH (MAX 1 (- (MIN BOTTOM BAR-BOTTOM) BAR-TOP))
                            LEFT BAR-TOP char-aluf SELF)))))

(DEFUN SCROLL-BAR-ERASE (&AUX LEFT TOP RIGHT BOTTOM)
  (DECLARE (:SELF-FLAVOR BASIC-SCROLL-BAR))
  (COND (SCROLL-BAR
	 (MULTIPLE-VALUE-SETQ (LEFT TOP RIGHT BOTTOM)
			      (DECODE-SCROLL-BAR SCROLL-BAR))
	 (PREPARE-SHEET (SELF)
	   (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ERASE-ALUF SELF)))))

(DEFMETHOD (BASIC-SCROLL-BAR :SET-SCROLL-BAR) (NEW-SCROLL-BAR)
 ;; Just make sure SCROLL-BAR's value is valid and canonicalized.
  (SEND SELF :SET-SCROLL-BAR-SPEC NEW-SCROLL-BAR 0 0 0 0)
  (SEND SELF :REDEFINE-MARGINS))

(DEFMETHOD (BASIC-SCROLL-BAR :AFTER :REFRESH-MARGINS) ()
  (AND (OR (EQ SCROLL-BAR-IN T) SCROLL-BAR-ALWAYS-DISPLAYED)
       (SCROLL-BAR-DRAW)))

(DEFMETHOD (BASIC-SCROLL-BAR :COMPUTE-MARGINS) (LM TM RM BM)
  (SEND SELF :SET-SCROLL-BAR-SPEC SCROLL-BAR LM TM RM BM))

(DEFMETHOD (BASIC-SCROLL-BAR :SET-SCROLL-BAR-SPEC) (SPEC LM TM RM BM &AUX BAR-WIDTH)
  (COND (SPEC
         (AND (EQ SPEC T) (SETQ SPEC 1))
         (IF (NUMBERP SPEC)
             (SETQ BAR-WIDTH SPEC
                   SPEC (MAKE-LIST 4))
             (SETQ BAR-WIDTH (- (THIRD SPEC) (FIRST SPEC))))
         (SETF (FIRST  SPEC) LM)
         (SETF (SECOND SPEC) TM)
         (SETF (THIRD  SPEC) (SETQ LM (+ LM BAR-WIDTH)))
         (SETF (FOURTH SPEC) (- BM))))
  (SETQ SCROLL-BAR SPEC)
  (VALUES LM TM RM BM))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-BAR-P) ()
  (NOT (NULL SCROLL-BAR)))

;;; Next two methods are defaults, some flavors do these more efficiently
(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-MORE-ABOVE) ()
  (PLUSP (SEND SELF :SCROLL-POSITION)))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-MORE-BELOW) ()
  (MULTIPLE-VALUE-BIND (TOP-LINE N-LINES LN-HEIGHT N-SCREEN-LINES)
      (SEND SELF :SCROLL-POSITION)
    ;; Some bag-chompers forget to return this value
    (OR N-SCREEN-LINES
        (SETQ N-SCREEN-LINES (TRUNCATE (SHEET-INSIDE-HEIGHT) LN-HEIGHT)))
    (< (+ TOP-LINE N-SCREEN-LINES) N-LINES)))

(DEFMETHOD (BASIC-SCROLL-BAR :SET-SCROLL-BAR-ALWAYS-DISPLAYED) (NEW)
  (SETQ SCROLL-BAR-ALWAYS-DISPLAYED NEW)
  (COND ((EQ SCROLL-BAR-IN T))
        (SCROLL-BAR-ALWAYS-DISPLAYED (SCROLL-BAR-DRAW))
        (T (SCROLL-BAR-ERASE))))

(DEFMETHOD (BASIC-SCROLL-BAR :HANDLE-MOUSE-SCROLL) (&AUX Y-OFF BOTTOM)
  "Called when the mouse enters the scroll bar"
  (SETQ SCROLL-BAR-IN T)
  ;; Give feedback by changing mouse cursor before calling SCROLL-BAR-DRAW, which pages a lot
  (SEND SELF :SET-MOUSE-POSITION 2 NIL)
  ;; Change the mouse to a fat double-headed up-and-down arrow.
  (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 7 :ON
                                :SET-CHARACTER 12.)
  (WITHOUT-INTERRUPTS
   (IF (SHEET-CAN-GET-LOCK SELF)
       (SCROLL-BAR-DRAW)
       (PROCESS-RUN-FUNCTION "Draw Scroll Bar"
                             #'(LAMBDA (SELF)
                                 (SHEET-FORCE-ACCESS (SELF)
                                  ;; It is possible that the mouse moved out while we were
                                  ;; waiting.  If this is the case, punt drawing.
                                  (AND SCROLL-BAR-IN
                                       (SCROLL-BAR-DRAW))))
                             SELF)))
  (DO () (())
    (MOUSE-DEFAULT-HANDLER SELF :IN)
    (MULTIPLE-VALUE-SETQ (NIL Y-OFF)
      (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
    (COND ((< MOUSE-Y Y-OFF)
           (MOUSE-WARP MOUSE-X Y-OFF))
          ((>= MOUSE-Y (SETQ BOTTOM (+ Y-OFF HEIGHT)))
           (MOUSE-WARP MOUSE-X (1- BOTTOM)))
          (T (RETURN T))))
  (WITHOUT-INTERRUPTS
    ;;There is this funny case where the sheet could be locked by the person waiting
    ;; for us to back out.  For us to block here would be a disaster, so undraw the
    ;; scroll bar in another process
    (COND ((SHEET-CAN-GET-LOCK SELF)
           (OR SCROLL-BAR-ALWAYS-DISPLAYED
               (SHEET-FORCE-ACCESS (SELF) (SCROLL-BAR-ERASE)))
           (SETQ SCROLL-BAR-IN NIL))
          (T (SETQ SCROLL-BAR-IN NIL)
             (OR SCROLL-BAR-ALWAYS-DISPLAYED
                 (PROCESS-RUN-FUNCTION "Undraw Scroll Bar"
                                       #'(LAMBDA (SELF)
                                           (WITH-SELF-ACCESSIBLE BASIC-SCROLL-BAR
                                             (SHEET-FORCE-ACCESS (SELF)
                                               ;; It is possible that the user reentered the
                                               ;; scroll bar before this code ran.  In that
                                               ;; case, don't actually erase it.
                                               (OR SCROLL-BAR-IN
                                                   (SCROLL-BAR-ERASE)))))
                                       SELF))))))

(DEFWRAPPER (basic-scroll-bar :MOUSE-BUTTONS) ((bd x y) . BODY)
  "If in scroll-bar do scroll-bar mouse-buttons else do default mouse-buttons."
  `(if (= (send mouse-blinker :character) 12.) 	; the fat double arrow
       (send self :mouse-buttons-scroll bd x y)
       . ,body))

(DEFWRAPPER (basic-scroll-bar :MOUSE-MOVES) ((X Y) . BODY)
  "If in scroll-bar use normal :mouse-moves definition."
  `(if (= (send mouse-blinker :character) 12.) 	; the fat double arrow
       (MOUSE-SET-BLINKER-CURSORPOS x y)
       . ,body))

(DEFMETHOD (BASIC-SCROLL-BAR :MOUSE-BUTTONS-SCROLL) (BD IGNORE Y &AUX CHAR TOP BOTTOM SHEIGHT)
  ;; Hack alert: The following forms looks weird because of a bug in SETF on CHAR-BIT.
  ;; The problem is that the SETF changes the type of the argument into a fixnum, even
  ;; if was originally a character object.
  (SETQ CHAR (MOUSE-CHARACTER-BUTTON-ENCODE BD))
  (SETF (CHAR-BIT CHAR :MOUSE) T)
  (SETQ CHAR (INT-CHAR CHAR))
  (MULTIPLE-VALUE-SETQ (NIL TOP NIL BOTTOM)
    (DECODE-SCROLL-BAR SCROLL-BAR))
  (SETQ SHEIGHT (- BOTTOM TOP))
  (CASE CHAR
    (#\MOUSE-L-1
     ;; Left: Here to top
      (SEND SELF :SCROLL-RELATIVE Y :TOP))
    (#\MOUSE-R-1
     ;; Right: Top to here
      (SEND SELF :SCROLL-RELATIVE :TOP Y))
    (#\MOUSE-L-2
     ;; Double Left: Here to bottom
      (SEND SELF :SCROLL-RELATIVE Y :BOTTOM))
    (#\MOUSE-R-2
     ;; Double right: Bottom to here
      (SEND SELF :SCROLL-RELATIVE :BOTTOM Y))
    (#\MOUSE-M-1
     ;; Middle: Jump to a proportional place in the "buffer"
     ;; If we are n% of the window down, we want the point
     ;; n% through the buffer to appear at the top of the window. 
     (MULTIPLE-VALUE-BIND (IGNORE TOTAL-ITEMS)
         (SEND SELF :SCROLL-POSITION)
       (SEND SELF :SCROLL-ABSOLUTE
                (TRUNCATE (+ 0.5s0 (TRUNCATE (* TOTAL-ITEMS (- Y TOP))
					     (SMALL-FLOAT SHEIGHT)))))))
    (OTHERWISE (BEEP))))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-RELATIVE) (FROM TO)
  "Put the FROM Y-position on the TO Y-position.  This assumes that each item is LINE-HEIGHT
high, and that there is a :SCROLL-TO message which accepts a line number to scroll to,
or a relative number of lines to scroll by.
Each argument is :TOP, :BOTTOM or a number of pixels from the top of the window."
  (MULTIPLE-VALUE-BIND (IGNORE IGNORE ITEM-HEIGHT)
      (SEND SELF :SCROLL-POSITION)
    (SETQ FROM (COND ((EQ FROM :TOP) 0)
                     ((EQ FROM :BOTTOM) (TRUNCATE (- (SHEET-INSIDE-HEIGHT) (1- ITEM-HEIGHT))
                                                  ITEM-HEIGHT))
                     ((NUMBERP FROM) (TRUNCATE (- FROM TOP-MARGIN-SIZE) ITEM-HEIGHT))
                     (T (FERROR NIL "~A illegal arg to :SCROLL-RELATIVE" FROM)))
          TO (COND ((EQ TO :TOP) 0)
                   ((EQ TO :BOTTOM) (TRUNCATE (- (SHEET-INSIDE-HEIGHT) (1- ITEM-HEIGHT))
                                              ITEM-HEIGHT))
                   ((NUMBERP TO) (TRUNCATE (- TO TOP-MARGIN-SIZE) ITEM-HEIGHT))
                   (T (FERROR NIL "~A illegal arg to :SCROLL-RELATIVE" TO))))
    ;; We now know what item we are scrolling from, and what item we are scrolling to.
    ;; Scroll that relative amount.
    (SEND SELF :SCROLL-TO (- FROM TO) :RELATIVE)))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-ABSOLUTE) (TO)
  "Scroll to the specified item"
  (SEND SELF :SCROLL-TO TO :ABSOLUTE))

(DEFMETHOD (BASIC-SCROLL-BAR :AFTER :NEW-SCROLL-POSITION) (&OPTIONAL IGNORE)
  (AND (OR SCROLL-BAR-IN SCROLL-BAR-ALWAYS-DISPLAYED)
       (SHEET-FORCE-ACCESS (SELF)
         (SCROLL-BAR-DRAW))))



;;; Special scrolling windows that tell when there is more above or
;;; below and scroll if you click there.
(DEFFLAVOR MARGIN-SCROLL-MIXIN () ()
  (:REQUIRED-FLAVORS MARGIN-REGION-MIXIN );;;;;;BASIC-SCROLL-BAR) - could also be scroll-bar-mixin
  (:INIT-KEYWORDS :MARGIN-SCROLL-REGIONS)
  (:DOCUMENTATION :MIXIN "Shows if there is more above or below."))

(DEFMETHOD (MARGIN-SCROLL-MIXIN :BEFORE :INIT) (INIT-PLIST &AUX TOP-P FONT)
  (DOLIST (REGION (GET INIT-PLIST :MARGIN-SCROLL-REGIONS))
    (COND ((MEMBER REGION '(:TOP :BOTTOM) :TEST #'EQ)
	   (SETQ TOP-P (EQ REGION :TOP)
		 REGION (LIST 'MARGIN-SCROLL-REGION REGION 0 0 0 0 0
			      NIL NIL NIL NIL)))
	  ((MEMBER (CAR REGION) '(:TOP :BOTTOM) :TEST #'EQ)
	   (SETQ TOP-P (EQ (CAR REGION) :TOP)
		 REGION (LIST 'MARGIN-SCROLL-REGION (CAR REGION) 0 0 0 0 0
			      (CADR REGION)
			      (CADDR REGION) (CADDDR REGION) NIL)))
	  (T
	   (SETQ TOP-P (EQ (MARGIN-REGION-MARGIN REGION) :TOP))))
    (OR (MARGIN-SCROLL-REGION-EMPTY-MSG REGION)
	(SETF (MARGIN-SCROLL-REGION-EMPTY-MSG REGION)
	      (IF TOP-P *NO-MORE-BACKWARD-STANDARD-MESSAGE*
		  *NO-MORE-FORWARD-STANDARD-MESSAGE*)))
    (OR (MARGIN-SCROLL-REGION-MORE-MSG REGION)
	(SETF (MARGIN-SCROLL-REGION-MORE-MSG REGION)
	      (IF TOP-P *MORE-BACKWARD-STANDARD-MESSAGE*
		  *MORE-FORWARD-STANDARD-MESSAGE*)))
    (SETQ FONT (OR (MARGIN-SCROLL-REGION-MSG-FONT REGION) FONTS:TR10I))
    (SETF (MARGIN-SCROLL-REGION-MSG-FONT REGION)
	  (SETQ FONT (FUNCALL (SHEET-GET-SCREEN SELF)
			      :PARSE-FONT-DESCRIPTOR FONT)))
    (SETF (MARGIN-REGION-SIZE REGION) (+ 2 (FONT-CHAR-HEIGHT FONT)))
    (PUSH REGION REGION-LIST)))

(DEFMETHOD (MARGIN-SCROLL-MIXIN :AFTER :NEW-SCROLL-POSITION) (&REST IGNORE)
  (DOLIST (REGION REGION-LIST)
    (AND (EQ (MARGIN-REGION-FUNCTION REGION) 'MARGIN-SCROLL-REGION)
	 (MARGIN-SCROLL-REGION :REFRESH REGION T))))

(DEFUN MARGIN-SCROLL-REGION (OP &OPTIONAL REGION OLD-VALID REGION2 IGNORE)
  (DECLARE (:SELF-FLAVOR MARGIN-SCROLL-MIXIN))
  (CASE OP
    (:REFRESH
     (LET (MORE-P LEFT TOP RIGHT BOTTOM)
       (MULTIPLE-VALUE-SETQ (LEFT TOP RIGHT BOTTOM)
			    (MARGIN-REGION-AREA REGION))
       (SETQ MORE-P (SEND SELF (IF (EQ (MARGIN-REGION-MARGIN REGION) :TOP)
				   :SCROLL-MORE-ABOVE :SCROLL-MORE-BELOW)))
       (COND ((ZEROP (MARGIN-REGION-SIZE REGION)))	;Turned off
	     ((OR (NOT (EQUAL (MARGIN-SCROLL-REGION-CURRENT-STRING REGION)
			      (SETF (MARGIN-SCROLL-REGION-CURRENT-STRING REGION)
				    (EVAL
				      (IF MORE-P
					  (MARGIN-SCROLL-REGION-MORE-MSG REGION)
					  ;;ELSE
					  (MARGIN-SCROLL-REGION-EMPTY-MSG REGION))))))
		  (NOT OLD-VALID))
	      (SHEET-FORCE-ACCESS (SELF)
		(AND OLD-VALID
		     (PREPARE-SHEET (SELF)
		       (%DRAW-RECTANGLE (- RIGHT LEFT)
					(- BOTTOM TOP)
					LEFT TOP ERASE-ALUF SELF)))
		(SEND SELF :STRING-OUT-CENTERED-EXPLICIT
		      (MARGIN-SCROLL-REGION-CURRENT-STRING REGION)
		      LEFT TOP RIGHT NIL
		      (MARGIN-SCROLL-REGION-MSG-FONT REGION) CHAR-ALUF
		      0 NIL NIL))))))
    ((:MOUSE-ENTERS-REGION :MOUSE-LEAVES-REGION :MOUSE-MOVES) NIL)
    (:MOUSE-CLICK 
     (IF (SEND SELF (IF (EQ (MARGIN-REGION-MARGIN REGION2) :TOP)
			:SCROLL-MORE-ABOVE :SCROLL-MORE-BELOW))
	 (LET ((FROM (MARGIN-REGION-MARGIN REGION2)))
	   (SEND SELF :SCROLL-RELATIVE FROM (IF (EQ FROM :TOP)
						:BOTTOM
						;;ELSE
						:TOP)))
	 (BEEP)))
    (:WHO-LINE-DOCUMENTATION-STRING "Any button to scroll one page.")))

;;; No longer needed.
(DEFFLAVOR MARGIN-SCROLL-REGION-ON-AND-OFF-WITH-SCROLL-BAR-MIXIN () ()
  (:REQUIRED-FLAVORS MARGIN-SCROLL-MIXIN)
  (:DOCUMENTATION :MIXIN
    "Makes the margin-scroll-regions disappear if the scroll-bar is set
to NIL."))

(DEFFLAVOR MARGIN-SCROLLING-WITH-FLASHY-SCROLLING-MIXIN () ()
	   (:REQUIRED-FLAVORS MARGIN-SCROLL-MIXIN
                              MARGIN-REGION-MIXIN FLASHY-SCROLLING-MIXIN)
	   (:DEFAULT-INIT-PLIST :FLASHY-SCROLLING-REGION
                                '((32. 0.4s0 0.6s0)
                                  (32. 0.4s0 0.6s0))))

(DEFMETHOD (MARGIN-SCROLLING-WITH-FLASHY-SCROLLING-MIXIN
             :OVERRIDE :WHO-LINE-DOCUMENTATION-STRING) ()
  (AND FLASHY-SCROLLING-BLINKER
       (IF (= (FUNCALL MOUSE-BLINKER :CHARACTER) MOUSE-GLYPH-THICK-UP-ARROW)
	   "Bump blinker against top to scroll down by one line.  Any button to scroll one page."
	   "Bump blinker against bottom to scroll up by one line.  Any button to scroll one page.")))

;;;added the following method for GRH -- GSM - 9 Dec 85
;;;replaced the following method on 19 Dec 85 for GRH by GSM
(DEFMETHOD (MARGIN-SCROLLING-WITH-FLASHY-SCROLLING-MIXIN :AROUND :MOUSE-CLICK)
           (CONT MT IGNORE BUTTON X Y)
  "This method causes mouse clicking when given a fat scrolling arrow to do the expected thing (scroll),
 even though the scroll region is not the same area as the flashy scrolling area."      ;;-GRH
  (IF FLASHY-SCROLLING-BLINKER
      ;; Always scroll when clicking on fat arrow, whether there is a current region or not.
      (LET (REG KEYWORD)
	(SETQ KEYWORD (IF (= (SEND MOUSE-BLINKER :CHARACTER) MOUSE-GLYPH-THICK-UP-ARROW)
			  :TOP :BOTTOM)
              REG (DOLIST (REG REGION-LIST)
                    (AND (EQ (CAR REG) 'MARGIN-SCROLL-REGION)
                         (EQ (CADR REG) KEYWORD)
                         (RETURN REG))))
	(FUNCALL (MARGIN-REGION-FUNCTION REG) :MOUSE-CLICK X Y REG BUTTON))
      (FUNCALL-WITH-MAPPING-TABLE CONT MT :MOUSE-CLICK BUTTON X Y)))

(DEFFLAVOR FLASHY-MARGIN-SCROLLING-MIXIN ()
	   (MARGIN-SCROLLING-WITH-FLASHY-SCROLLING-MIXIN
            MARGIN-SCROLL-MIXIN
	    FLASHY-SCROLLING-MIXIN)
  (:INCLUDED-FLAVORS MARGIN-REGION-MIXIN)
  (:DOCUMENTATION
    :MIXIN
    "Provides margin scrolling and flashy scrolling.  Interfaces them
properly, and provides appropriate support."))

(DEFFLAVOR SCROLL-STUFF-ON-OFF-MIXIN
           ((MAKING-SCROLL-DECISION NIL))       ;Internal, prevents infinite recursion
	   (FLASHY-MARGIN-SCROLLING-MIXIN
             MARGIN-REGION-MIXIN
             BASIC-SCROLL-BAR)
  (:REQUIRED-METHODS :ENABLE-SCROLLING-P        ;T if scrolling needed
                     :ADJUSTABLE-SIZE-P)        ;T if outside size can change
                                                ; to preserve inside size,
                                                ; NIL if something like a pane
  (:DOCUMENTATION
    :MIXIN
    "Scroll bar, flashy scrolling, and margin scrolling, which turn
on and off with :ENABLE-SCROLLING-P")
  (:DEFAULT-INIT-PLIST :SCROLL-BAR 2            ;This 2 is unmodular, sigh.
    :MARGIN-SCROLL-REGIONS '(:TOP :BOTTOM)))


(DEFMETHOD (SCROLL-STUFF-ON-OFF-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS)
           (&REST IGNORE)
  (OR MAKING-SCROLL-DECISION
      (SEND SELF :DECIDE-IF-SCROLLING-NECESSARY)))

;;; This assumes that the pane-size value will be the amount that the
;;; window wants to use in order to scroll, and therefore that the
;;; window should assume no margin scroll regions when computing it!
(DEFMETHOD (SCROLL-STUFF-ON-OFF-MIXIN :AROUND :PANE-SIZE)
           (CONT MT ARGS &REST IGNORE)
  (BIND (LOCF REGION-LIST) (COPY-LIST REGION-LIST))
  (DO ((RS REGION-LIST (CDR RS)))
      ((NULL RS))
    (LET ((R (CAR RS)))
      (WHEN (EQ (MARGIN-REGION-FUNCTION R) 'MARGIN-SCROLL-REGION)
	(SETF (CAR RS) (COPY-LIST R))
	(SETF (MARGIN-REGION-SIZE (CAR RS)) 0))))
  (MULTIPLE-VALUE-BIND (NIL TOP NIL BOT)
      (SEND SELF :COMPUTE-MARGINS 0 0 0 0)
    (BIND (LOCF TOP-MARGIN-SIZE   ) TOP)
    (BIND (LOCF BOTTOM-MARGIN-SIZE) BOT)
    (AROUND-METHOD-CONTINUE CONT MT ARGS)))

;;; Window should send this message to itself after changing the
;;; number of displayable items, but before doing the associated
;;; redisplay.  This method will decide whether to turn the scroll
;;; bar, flashy scrolling, and margin-scroll regions on and off.
;;; If :ADJUSTABLE-SIZE-P, then if changing the number of displayable
;;; items changes the height of the window, that should be done
;;; before sending this message.
;;; This can change the inside-height of the window, unless the
;;; :ADJUSTABLE-SIZE-P message returns T.
;;; Note that redisplay can happen inside this method, you may want
;;; to do a WITH-SHEET-DEEXPOSED to avoid letting the user see
;;; gratuitous double redisplays, or to suppress the redisplay
;;; entirely if there is no bit-save-array.
(DEFMETHOD (SCROLL-STUFF-ON-OFF-MIXIN :DECIDE-IF-SCROLLING-NECESSARY) ()
  (BIND (LOCATE-IN-INSTANCE SELF 'MAKING-SCROLL-DECISION) T)
  (LET ((IW (SHEET-INSIDE-WIDTH)) (IH (SHEET-INSIDE-HEIGHT)) (CHANGEP NIL)
        SCROLL-NOW)
	;; When we ask whether everything fits, pretend there are no scroll regions.
    (LET ()
      (BIND (LOCF REGION-LIST) (COPY-LIST REGION-LIST))
      (DO ((RS REGION-LIST (CDR RS)))
	  ((NULL RS))
	(LET ((R (CAR RS)))
	  (WHEN (EQ (MARGIN-REGION-FUNCTION R) 'MARGIN-SCROLL-REGION)
	    (SETF (CAR RS) (COPY-LIST R))
	    (SETF (MARGIN-REGION-SIZE (CAR RS)) 0))))
      (MULTIPLE-VALUE-BIND (NIL TOP NIL BOT)
          (SEND SELF :COMPUTE-MARGINS 0 0 0 0)
	(BIND (LOCF TOP-MARGIN-SIZE   ) TOP)
	(BIND (LOCF BOTTOM-MARGIN-SIZE) BOT)
	(SETQ SCROLL-NOW (SEND SELF :ENABLE-SCROLLING-P))))
    ;; Now SCROLL-NOW says whether we must now have scrolling.
    (DOLIST (R REGION-LIST)
      (AND (EQ (MARGIN-REGION-FUNCTION R) 'MARGIN-SCROLL-REGION)
	   (LET ((MARGIN-REGION-NEW-HEIGHT
		  (IF (NULL SCROLL-NOW) 0
		      (+ 2 (FONT-CHAR-HEIGHT
                             (MARGIN-SCROLL-REGION-MSG-FONT R))))))
	     (UNLESS (= (MARGIN-REGION-SIZE R) MARGIN-REGION-NEW-HEIGHT)
	       (SETQ CHANGEP T)
	       (SETF (MARGIN-REGION-SIZE R) MARGIN-REGION-NEW-HEIGHT)))))
    (WHEN CHANGEP (SEND SELF :REDEFINE-MARGINS))
    (COND ((SEND SELF :ENABLE-SCROLLING-P)      ;Need scrolling?
           (COND ((NOT SCROLL-BAR)              ;If scroll stuff not on, turn on
                  (SETQ CHANGEP T)
                  (SEND SELF :SET-SCROLL-BAR 2))))
          (T                                    ;Doesn't need scrolling
           (MULTIPLE-VALUE-BIND (IGNORE N-ITEMS IGNORE)
               (SEND SELF :SCROLL-POSITION)
             (COND ((ZEROP N-ITEMS))            ;Obviously not set up yet
                   ((NULL SCROLL-BAR))          ;Already off
                   (T (SETQ CHANGEP T)          ;Turn scroll stuff off
                      (SEND SELF :SET-SCROLL-BAR NIL))))))
    (AND CHANGEP (SEND SELF :ADJUSTABLE-SIZE-P)
         (SEND SELF :SET-INSIDE-SIZE IW IH))))


;; Define these flavors as obsolete.

(compiler:make-obsolete-flavor 'SCROLL-STUFF-ON-OFF-MIXIN "use W:SCROLL-BAR-MIXIN flavor instead")
(compiler:make-obsolete-flavor 'FLASHY-MARGIN-SCROLLING-MIXIN "use W:SCROLL-BAR-MIXIN flavor instead")
(compiler:make-obsolete-flavor 'MARGIN-SCROLLING-WITH-FLASHY-SCROLLING-MIXIN "use W:SCROLL-BAR-MIXIN flavor instead")
(compiler:make-obsolete-flavor 'MARGIN-SCROLL-REGION-ON-AND-OFF-WITH-SCROLL-BAR-MIXIN
			       "use W:SCROLL-BAR-MIXIN flavor instead")
(compiler:make-obsolete-flavor 'MARGIN-SCROLL-MIXIN "use W:SCROLL-BAR-MIXIN flavor instead")
(compiler:make-obsolete-flavor 'BASIC-SCROLL-BAR "use W:SCROLL-BAR-MIXIN flavor instead")
(compiler:make-obsolete-flavor 'WINDOW-WITH-INSIDE-SCROLL-BAR "use W:SCROLL-BAR-MIXIN flavor instead")
(compiler:make-obsolete-flavor 'FLASHY-SCROLLING-MIXIN "use W:SCROLL-BAR-MIXIN flavor instead")

;; Define these functions as obsolete.

(COMPILER:MAKE-OBSOLETE FLASHY-SCROLLING-MOUSE-MOVES "see new flavor w:scroll-bar-mixin")
(COMPILER:MAKE-OBSOLETE MARGIN-SCROLL-REGION "see new flavor w:scroll-bar-mixin")
(COMPILER:MAKE-OBSOLETE SCROLL-BAR-ERASE "see new flavor w:scroll-bar-mixin")
(COMPILER:MAKE-OBSOLETE SCROLL-BAR-DRAW "see new flavor w:scroll-bar-mixin")
(COMPILER:MAKE-OBSOLETE DECODE-SCROLL-BAR "see new flavor w:scroll-bar-mixin")
(COMPILER:MAKE-OBSOLETE FLASHY-SCROLLING-PARSE-X-SPEC "see new flavor w:scroll-bar-mixin")

