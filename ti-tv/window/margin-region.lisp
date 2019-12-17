;;; -*- Mode:COMMON-LISP; Package:TV; Base:10.; Fonts:(CPTFONT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated.  All rights reserved.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  12/21/88  MAY	Changed MARGIN-CHOICE-REGION to use left-margin-size instead of trying to look for
;;; 			the scroll-bar (incorrectly). SPR 5936, 8983
;;;  11/04/88  LG	Use send-draw-shaded-polygon to draw "X" in draw-choice-box.
;;;  09/14/88  MAY	Changed MARGIN-CHOICE-REGION for spr 8645 to check for w:scroll-bar-mixin before calling
;;;			w:scroll-bar-area-width. Also re-instated lost? patch by TWE below for SPR 1986 & 8304.
;;;  01/22/88  LG         Allowed margin items to be drawn on a microExplorer.
;;;   3/27/87  KDB	Fixed spacing of margin choice items. Space between them now adjusts depending on
;;;                          number of items AND availabel space. Added BETWEEN-ITEM-SPACE to modularize.
;;;                          Closed SPR# 3835.
;;; 01/06/87   KDB	Defined HOLLOW-MARGIN-CHOICE-BLINKER and RECTANGULAR-MARGIN-CHOICE-BLINKER
;;;			resources to speed handling of margin-choice boxes.
;;; 12/22/86   TWE	Made several changes to make the build process cleaner.
;;;			Added a (:REQUIRED-FLAVORS ESSENTIAL-WINDOW) clause to the MARGIN-CHOICE-MIXIN
;;;			flavor so its methods would be able to reference more instance variables.
;;; 11/07/86   TWE	Changed the call to :SCROLL-BAR-ON-OFF to use :SEND-IF-HANDLES instead of
;;;			assuming that the method is there.
;;; 10/30/86   TWE	Made the compiler happy by removing unused symbols and changing others to IGNORE.
;;; 10/23/86   TWE	Changed `funcall-self' to `funcall self' to confirm to Common Lisp.
;;; 10/21/86   KDB	Made margin-choice-box x positions understand *scroll-bar-default-icon-width*.
;;; 10/21/86   TWE	Moved the definition of the `Do It' and `Abort' strings from MENU to here.
;;; 10/15/86   TWE	Fixed the margin-choice-region function to use TRUNCATE instead of the non-Common
;;;			Lisp function FIX.
;;; 10/10/86   KDB       Added flag to allow hollow or reverse-video margin choice boxes. *hollow-m-choice-box-p*
;;; 10/08/86   TWE	Fixed up the copy-tree that was being done for margin-choices to only to the copy
;;;			during initialization or when the margin choices are changed.  This fixes bug 1986.
;;;			Also removed the local constants defining positions in the mouse font.  The references
;;;			to these constants were changed to use the constants defined in the
;;;			define-mouse-mapping file.
;;; 09/17/86   KDB       Sped up Margin-choice-region :mouse-moves.
;;; 09/15/86   KDB       Made both choice name and box mouse sensitive to make margin choosing easier.
;;; 09/02/86   KDB       Added Choice-box-width function to obtain choice box width in general fashion.
;;; 08/31/86   KDB       Added a defparameter to allow box-width factor to change and be noticed from top level.
;;; 08/28/86   KDB	Fixed %Draw-shaded-triangle. Sheet argument => (Sheet-screen-array sheet)
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 08/07/86   GRH	Changed draw-choice-box to call %draw-shaded-triangle instead of %draw-triangle.
;;; 08/04/86   TWE	Changed type checks for font to use the FONT-OBJECT-P function.

;;; Making this larger will make the choice box longer.
(DEFPARAMETER BOX-WIDTH-FACTOR 2.0)

(DEFPARAMETER *HOLLOW-M-CHOICE-BOX-P* T
  "Specifies the kind of blinker used for choice boxes.
T means use a hollow blinker;
NIL means use a solid rectangular blinker.")

#| The following variables are the defaults for margin choices.  If the
user specifies a  keyword for  a margin  choice, one  of these  will be
substituted.  |#
(DEFPARAMETER W:MARGIN-CHOICE-COMPLETION-STRING "Do It"
  "What the user sees on a margin choice which completes the selection.")

(DEFPARAMETER W:MARGIN-CHOICE-ABORT-STRING "Abort"
  "What the user sees on a margin choice which aborts the selection.")

;;; Margin region windows, various special areas can be defined within
;;; the window's margins that are allowed to handle the mouse.
(DEFFLAVOR MARGIN-REGION-MIXIN
	   ((REGION-LIST    NIL)                ;A list of active regions
	    (CURRENT-REGION NIL)		;The one currently owning the mouse
	    )
	   ()
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE ESSENTIAL-WINDOW)
  (:INITABLE-INSTANCE-VARIABLES REGION-LIST)
  (:DOCUMENTATION
    :MIXIN "Allows separate mouse handling in parts of the margins."))

(DEFSTRUCT (MARGIN-REGION
	     (:CONSTRUCTOR           NIL)
	     (:CONC-NAME             NIL)
	     (:CALLABLE-CONSTRUCTORS NIL)
	     (:ALTERANT              ALTER-MARGIN-REGION)
	     (:PREDICATE             NIL)
	     (:COPIER                NIL)
	     (:TYPE                  :LIST))
  MARGIN-REGION-FUNCTION			;A DTP-SELECT-METHOD for this one
  MARGIN-REGION-MARGIN				;Name of the margin occupied
  MARGIN-REGION-SIZE				;Amount of that to occupy
  MARGIN-REGION-LEFT				;Its area of the screen
  MARGIN-REGION-TOP
  MARGIN-REGION-RIGHT
  MARGIN-REGION-BOTTOM)


;;; Move this defstruct to the file OBSOLETE-SCROLL-BAR.LISP after defstruct gets fixed to not
;;; redefine the accessors for included structures when :CONC-NAME is nil.
(DEFSTRUCT (MARGIN-SCROLL-REGION
	     (:INCLUDE               MARGIN-REGION)
	     (:CONSTRUCTOR           NIL)
	     (:CONC-NAME             NIL)
	     (:CALLABLE-CONSTRUCTORS NIL)
	     (:ALTERANT              ALTER-MARGIN-SCROLL-REGION)
	     (:PREDICATE             NIL)
	     (:COPIER                NIL)
	     (:TYPE                  :LIST))
  MARGIN-SCROLL-REGION-EMPTY-MSG		;Message when nothing more to scroll
  MARGIN-SCROLL-REGION-MORE-MSG			;Other message
  MARGIN-SCROLL-REGION-MSG-FONT			;Font for that
  MARGIN-SCROLL-REGION-CURRENT-STRING		;String now displayed in region
  )

(DEFMETHOD (MARGIN-REGION-MIXIN :OVERRIDE :WHO-LINE-DOCUMENTATION-STRING) ()
  (AND CURRENT-REGION
       (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION)
		:WHO-LINE-DOCUMENTATION-STRING CURRENT-REGION)))

(DEFMETHOD (MARGIN-REGION-MIXIN :SET-REGION-LIST) (NEW-REGION-LIST)
  (SETQ REGION-LIST NEW-REGION-LIST)
  (SEND SELF :REDEFINE-MARGINS))

(DEFMETHOD (MARGIN-REGION-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (SEND SELF :DECODE-REGION-LIST REGION-LIST LM TM RM BM))

(DEFMETHOD (MARGIN-REGION-MIXIN :DECODE-REGION-LIST) (SPEC LM TM RM BM)
  (DO ((SPEC SPEC (CDR SPEC))
       (REGION) (SIZE))
      ((NULL SPEC))
    (SETQ REGION (CAR SPEC)
          SIZE (MARGIN-REGION-SIZE REGION))
    (AND (>= SIZE 0)
	 ;; Negative size means the region took care of setting this stuff.
	 (MULTIPLE-VALUE-SETQ (NIL LM TM RM BM)
	   (MARGIN-REGION-SET-SIZE REGION SIZE LM TM RM BM))))
  (SETQ REGION-LIST SPEC)
  (VALUES LM TM RM BM))


(DEFUN MARGIN-REGION-SET-SIZE (REGION SIZE LM TM RM BM)
  "Given margins outside REGION, set REGION's edges and return
margins inside REGION.
LM, TM, RM and BM are the widths of the four margins, not counting
REGION.  We compute where REGION should go, based on them, and
update them to take account of the space used up by REGION.  The
values are REGION and the four updated margin-widths, one of which
has been incremented by SIZE."
  (DECLARE (VALUES REGION LM TM RM BM))
  (LET ((LEFT LM)
	(TOP  TM)
	(RIGHT  (- RM))
	(BOTTOM (- BM)))
    (CASE (MARGIN-REGION-MARGIN REGION)
      (:LEFT   (SETQ RIGHT     (SETQ LM (+ LM SIZE))))
      (:TOP    (SETQ BOTTOM    (SETQ TM (+ TM SIZE))))
      (:RIGHT  (SETQ LEFT   (- (SETQ RM (+ RM SIZE)))))
      (:BOTTOM (SETQ TOP    (- (SETQ BM (+ BM SIZE))))))
    (SETF (MARGIN-REGION-LEFT   REGION) LEFT)
    (SETF (MARGIN-REGION-TOP    REGION) TOP)
    (SETF (MARGIN-REGION-RIGHT  REGION) RIGHT)
    (SETF (MARGIN-REGION-BOTTOM REGION) BOTTOM))
  (VALUES REGION LM TM RM BM))

(DEFMETHOD (MARGIN-REGION-MIXIN :AFTER :REFRESH-MARGINS) ()
  (DOLIST (REGION REGION-LIST)
    (FUNCALL (MARGIN-REGION-FUNCTION REGION) :REFRESH REGION)))

(DEFWRAPPER (MARGIN-REGION-MIXIN :HANDLE-MOUSE) (IGNORE . BODY)
  `(UNWIND-PROTECT
       (PROGN . ,BODY)
     (IF CURRENT-REGION
	 (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION) :MOUSE-LEAVES-REGION
		  CURRENT-REGION)
	 (FUNCALL SELF :MOUSE-LEAVES-REGION))
     (SETQ CURRENT-REGION NIL)))

(DEFUN MARGIN-REGION-AREA (REGION &AUX LEFT TOP RIGHT BOTTOM)
  "Return the four edges of REGION, relative to the window SELF."
  (DECLARE (:SELF-FLAVOR MARGIN-REGION-MIXIN)
	   (VALUES LEFT TOP RIGHT BOTTOM))
  (SETQ LEFT   (MARGIN-REGION-LEFT   REGION)
	TOP    (MARGIN-REGION-TOP    REGION)
	RIGHT  (MARGIN-REGION-RIGHT  REGION)
	BOTTOM (MARGIN-REGION-BOTTOM REGION))
  (AND (<  LEFT   0) (SETQ LEFT   (+ WIDTH  LEFT)))
  (AND (<  TOP    0) (SETQ TOP    (+ HEIGHT TOP)))
  (AND (<= RIGHT  0) (SETQ RIGHT  (+ WIDTH  RIGHT)))
  (AND (<= BOTTOM 0) (SETQ BOTTOM (+ HEIGHT BOTTOM)))
  (VALUES LEFT TOP RIGHT BOTTOM))

(DEFMETHOD (MARGIN-REGION-MIXIN :AFTER :MOUSE-MOVES) (X Y &AUX REGION)
  (DOLIST (REG REGION-LIST)
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
	(MARGIN-REGION-AREA REG)
      (AND (>= X LEFT) (< X RIGHT) (>= Y TOP) (< Y BOTTOM)
	   (RETURN (SETQ REGION REG)))))
  (COND ((NOT (EQ REGION CURRENT-REGION))
	 (IF CURRENT-REGION
	     (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION)
		      :MOUSE-LEAVES-REGION
		      CURRENT-REGION)
	     (SEND SELF :MOUSE-LEAVES-REGION))
	 (IF REGION
	     (FUNCALL (MARGIN-REGION-FUNCTION REGION)
		      :MOUSE-ENTERS-REGION REGION)
	     (SEND SELF :MOUSE-ENTERS-REGION))))
  (COND ((SETQ CURRENT-REGION REGION)
	 (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION)
		  :MOUSE-MOVES X Y CURRENT-REGION))))


				   
(DEFMETHOD (MARGIN-REGION-MIXIN :MOUSE-CLICK) (BUTTON X Y)
  (COND ((AND CURRENT-REGION (NOT (= BUTTON #\MOUSE-R-2)))
	 (FUNCALL (MARGIN-REGION-FUNCTION CURRENT-REGION) :MOUSE-CLICK X Y
                  CURRENT-REGION
                  BUTTON)
	 T)))

(DEFMETHOD (MARGIN-REGION-MIXIN :MOUSE-ENTERS-REGION) ())

(DEFMETHOD (MARGIN-REGION-MIXIN :MOUSE-LEAVES-REGION) ())

(DEFFLAVOR MARGIN-CHOICE-MIXIN
           ((MARGIN-CHOICES NIL)
            (MARGIN-CHOICE-FONT 0)
            (LAST-CHARACTER MOUSE-GLYPH-NORTH-WEST-ARROW))
	   ()
           (:INITABLE-INSTANCE-VARIABLES MARGIN-CHOICES)
	   (:INCLUDED-FLAVORS MARGIN-REGION-MIXIN)
           (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
	   (:DOCUMENTATION :MIXIN "Provides a few boxes in the bottom margin."))


(DEFSTRUCT (CHOICE-BOX (:CONSTRUCTOR           NIL)
                       (:CONC-NAME             NIL)
                       (:CALLABLE-CONSTRUCTORS NIL)
                       (:ALTERANT              ALTER-CHOICE-BOX)
                       (:PREDICATE             NIL)
                       (:COPIER                NIL)
                       (:TYPE                  :LIST))
  CHOICE-BOX-NAME
  CHOICE-BOX-STATE
  CHOICE-BOX-FUNCTION
  CHOICE-BOX-X1
  CHOICE-BOX-X2)


(DEFUN DRAW-CHOICE-BOX
       (SHEET X Y ON-P
        &OPTIONAL (SIZE (TRUNCATE
                          (* (FONT-BLINKER-HEIGHT (SHEET-CURRENT-FONT SHEET))
                             1)))
        &AUX
        ;; This controls the width of the border for the box.
        (WIDTH (TRUNCATE SIZE 8)))
  "Draw a choice box for multiple-choice or choose-variable-values.
Returns the (X,Y) coordinate of the upper right of the box as two values."
  (PREPARE-SHEET (SHEET)
    (let-if (mac-window-p sheet)
       ((*dont-clip-at-the-margins* t))
    (LET ((CHAR-ALUF  (SHEET-CHAR-ALUF  SHEET))
          (ERASE-ALUF (SHEET-ERASE-ALUF SHEET))
          (TEM-X (- (TRUNCATE  (* SIZE BOX-WIDTH-FACTOR)) (* WIDTH 2)))
          (TEM-Y (- SIZE (* WIDTH 2)))
          (X1    (+ X (* WIDTH 1)))
          (Y1    (+ Y    WIDTH)))
      ;; Draw the entire box with no hole in the middle.
      (%DRAW-RECTANGLE (TRUNCATE (* SIZE BOX-WIDTH-FACTOR)) SIZE X Y CHAR-ALUF SHEET)
      ;; Draw the hole in the middle of the box.
        (%DRAW-RECTANGLE TEM-X TEM-Y X1 Y1 ERASE-ALUF SHEET)
        ;; If the box is to be in the ON state then put an X in it.
        (AND ON-P
             (LET* ((N 2)                       ; Width of the lines of the X
                    (X1+N (+ X1 N))
                    (X2   (+ X1 (* TEM-X 1)))
                    (X2-N (- X2 N))
                    (Y1+N (+ Y1 N))
                    (Y2   (+ Y1 TEM-Y))
                    (Y2-N (- Y2 N)))
               ;; This is what a diagonal hexagon looks like.  Notation
               ;; has been included to aid in determining which call to
               ;; %DRAW-shadedTRIANGLE performs which drawing operation.  The
               ;; symbols around the outside indicate where each of the
               ;; coordinates are located.  The numbers in the interior
               ;; indicate the particular triangle being drawn by the
               ;; correspondingly numbered call to %DRAW-shaded-TRIANGLE.
               ;; Triangles 1, 3, 5 and 7 each split a long rectangle
               ;; into half (triangles 2, 4, 6 and 8 should be easy to
               ;; identify).
               ;;
               ;;
               ;;             X1        X1+N                   X2-N          X2
               ;;   Y1        ____________                        ____________
               ;;             |          /\                     / \          |
               ;;             |  8     /   \\                 / /   \    2   |
               ;;             |      /      \ \             /  /      \      |
               ;;             |    /         \  \         /   /         \    |
               ;;             |  /            \   \     /    /            \  |
               ;;   Y1+N      |/               \    \ /     /               \|
               ;;               \       7       \   / \    /        3        /
               ;;                 \              \/     \ /                /
               ;;                   \           / \      /\              /
               ;;                     \       /    \    /   \          /
               ;;                       \   /       \  /      \      /
               ;;                         \          \/         \  /
               ;;                       /   \        /\          /\
               ;;                     /       \     /  \       /    \
               ;;                   /           \  /    \    /        \
               ;;                 /               \      \ /            \
               ;;               /                /  \    /\               \
               ;;   Y2-N      /       1         /     \/   \      5         \
               ;;             | \              /     /  \   \               /|
               ;;             |   \           /    /      \  \            /  |
               ;;             |     \        /   /          \ \         /    |
               ;;             |   4   \     /  /              \\      /   6  |
               ;;             |         \  / /                  \   /        |
               ;;   Y2        |___________\/                      /__________|
               ;;
               ;;
               ;;
	       (IF (mac-window-p sheet)
		   (LET ()
		     (WITH-STACK-LIST (x-coords x1 x1+n x2 x2 x2-n x1)
		       (WITH-STACK-LIST (y-coords y1 y1 y2-n y2 y2 y1+n)
			 (send-draw-filled-polygon x-coords y-coords char-aluf w:black sheet)))
		     (WITH-STACK-LIST (x-coords x2 x2 x1+n x1 x1 x2-n)
		       (WITH-STACK-LIST (y-coords y1 y1+n y2 y2 y2-n y1)
			 (send-draw-filled-polygon x-coords y-coords char-aluf w:black sheet))))
		 ;; else...
		 (SYSTEM:%DRAW-shaded-TRIANGLE X2-N Y1   X1+N Y2   X1  Y2-N CHAR-ALUF t t t nil SHEET)   ;1
		 (SYSTEM:%DRAW-shaded-TRIANGLE X2-N Y1   X2   Y1   X2  Y1+N CHAR-ALUF t t t nil SHEET)   ;2
		 (SYSTEM:%DRAW-shaded-TRIANGLE X2-N Y1   X2   Y1+N X1+N Y2  CHAR-ALUF t t t nil SHEET)   ;3
		 (SYSTEM:%DRAW-shaded-TRIANGLE X1+N Y2   X1   Y2   X1  Y2-N CHAR-ALUF t t t nil SHEET)   ;4
		 (SYSTEM:%DRAW-shaded-TRIANGLE X1+N Y1   X2   Y2-N X2-N Y2  CHAR-ALUF t t t nil SHEET)   ;5
		 (SYSTEM:%DRAW-shaded-TRIANGLE X2   Y2-N X2   Y2   X2-N Y2  CHAR-ALUF t t t nil SHEET)   ;6
		 (SYSTEM:%DRAW-shaded-TRIANGLE X2-N Y2   X1   Y1+N X1+N Y1  CHAR-ALUF t t t nil SHEET)   ;7
		 (SYSTEM:%DRAW-shaded-TRIANGLE X1   Y1+N X1   Y1   X1+N Y1  CHAR-ALUF t t t nil SHEET)   ;8
		 ))))))
  (BLOCK NIL (RETURN (+ X (VALUES (FLOOR (* SIZE BOX-WIDTH-FACTOR)))) Y)))

(DEFMETHOD (MARGIN-CHOICE-MIXIN :BEFORE :INIT) (IGNORE)
  ;; Do a copy-tree in case the margin-choices list is inside of a defun
  ;; on a loaded xfasl file.  If this is the case then the list is in
  ;; write protected memory and can not be modified without getting an
  ;; error.  By copying the list we can guarantee that it goes into
  ;; read/write memory and can modify it.
  (SETQ MARGIN-CHOICES (COPY-TREE MARGIN-CHOICES))
  (PUSH (LIST 'MARGIN-CHOICE-REGION :BOTTOM
              (IF (NULL MARGIN-CHOICES) 0 (1+ (SHEET-LINE-HEIGHT SUPERIOR)))
              0 0 0 0)
        REGION-LIST))

(DEFMETHOD (MARGIN-CHOICE-MIXIN :SET-MARGIN-CHOICES) (NEW-MARGIN-CHOICES)
  ;; Do a copy-tree in case the margin-choices list is inside of a defun
  ;; on a loaded xfasl file.  If this is the case then the list is in
  ;; write protected memory and can not be modified without getting an
  ;; error.  By copying the list we can guarantee that it goes into
  ;; read/write memory and can modify it.
  (SETQ MARGIN-CHOICES (COPY-TREE NEW-MARGIN-CHOICES))
  (LET ((REGION (ASSOC 'MARGIN-CHOICE-REGION REGION-LIST :TEST #'EQ))
	(SIZE (IF (NULL MARGIN-CHOICES) 0 (1+ (SHEET-LINE-HEIGHT SUPERIOR)))))
    (IF (= (MARGIN-REGION-SIZE REGION) SIZE)
	(SHEET-FORCE-ACCESS (SELF T)
          (FUNCALL (MARGIN-REGION-FUNCTION REGION) :REFRESH REGION T))
	(PROGN
	  (SETF (MARGIN-REGION-SIZE REGION) SIZE)
	  (SEND SELF :REDEFINE-MARGINS)))))

(DEFUN CHOICE-BOX-WIDTH (SHEET &AUX SIZE WIDDTH)
  "Return width of choice box, based on height of current blinker multiplied by the box width factor."
  (SETQ SIZE   (TRUNCATE (* (FONT-BLINKER-HEIGHT (SHEET-CURRENT-FONT SHEET)) 1)))
  (SETQ WIDDTH (TRUNCATE (* SIZE BOX-WIDTH-FACTOR))))

(DEFUN HANDLE-CHOICE-BUTTON (BOXES X Y THING &AUX CHOSEN)
  (DECLARE (:SELF-FLAVOR MARGIN-CHOICE-MIXIN))
  (IF (SETQ CHOSEN (DOLIST (BOX BOXES)
		     (AND (>= X (CHOICE-BOX-X1 BOX))
			  (<  X (CHOICE-BOX-X2 BOX))
			  (RETURN BOX))))
      (PROGN
        ;; The following is somewhat of a hack.  It was done to make the choice-box-function
        ;; execute immediately but to not have it run in the mouse process.  By having the
        ;; choice-box-function execute now, instead of when the mouse process goes into a wait
        ;; state, any side-effects that are done there can be recognized immediately.  This is
        ;; needed by the menu code to handle highlighting command menus properly.  For that
        ;; case, the user mouses on an item, but the mouse doesn't go into a wait state for a
        ;; while.  The problem is that when the user mouses on an item, a blip needs to be put
        ;; into the user-specified I/O buffer telling the user that something was moused.  For
        ;; margin-choices, it is the choice-box-function which sets a flag indicating what was
        ;; moused.  By having the choice-box-function run now, we can be assured that the
        ;; flag-setting gets done before we need to look at.       (TWE)
        (PROCESS-RUN-FUNCTION `(:NAME "Choice" :PRIORITY ,(1+ (SEND CURRENT-PROCESS :PRIORITY)))
                              SELF ':FUNCALL-INSIDE-YOURSELF
                              (CHOICE-BOX-FUNCTION CHOSEN) CHOSEN THING Y)
        (PROCESS-ALLOW-SCHEDULE))
      (BEEP)))

;;; Next three forms added to make new margin choice boxing work fast.
(DEFUN GET-BLINKER-TYPE (BLINKER-LIST TYPE)
  "Return a blinker from blinker list.  If blinker not found, return NIL."
  (DOLIST (STRG-BOX-BLINKER BLINKER-LIST)
    (WHEN (TYPEP STRG-BOX-BLINKER TYPE)
      (RETURN STRG-BOX-BLINKER))))

			    
(DEFRESOURCE HOLLOW-STRING-BOX-BLINKER (WINDOW)
  :CONSTRUCTOR (MAKE-BLINKER  WINDOW
			     'HOLLOW-margin-choice-BLINKER))
			   
(DEFRESOURCE STRING-BOX-BLINKER (WINDOW)
  :CONSTRUCTOR (MAKE-BLINKER WINDOW
			     'RECTANGULAR-margin-choice-BLINKER))

(DEFUN BETWEEN-ITEM-SPACE (LEFT RIGHT  MARGIN-CHOICES)
  "RETURNS THE CORRECT SPACING BETWEEN MARGIN CHOICE ITEMS."
  (IF (= 1 (LENGTH MARGIN-CHOICES))
      0            ;; NO SPACE NEEDED
      (LET ((WID (- RIGHT LEFT)))  ;; SUBTRACT 4 DUE TO CHOICE BOX ITEM ALIGNMENT RELATIVE TO LEFT AND RIGHT BORDERS.
	(TRUNCATE (- WID 4 (CHOICE-ITEM-LAYOUT MARGIN-CHOICES)) ;; WIDTH MINUS THAT USED BY CHOICES AND BOXES
		  (LENGTH MARGIN-CHOICES) ))))

(DEFUN MARGIN-CHOICE-REGION (OP &OPTIONAL ARG1 ARG2 ARG3 IGNORE
			     &AUX  STRG-BOX-STARTX STRG-BOX-ENDX STRG-BLINKER STRING-OF-BOX)
  (DECLARE (:SELF-FLAVOR MARGIN-CHOICE-MIXIN))
  (CASE OP
    (:WHO-LINE-DOCUMENTATION-STRING
     (LET (DOCUMENTATION)
       (LET ((X (- MOUSE-X (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))))
	 (DOLIST (BOX MARGIN-CHOICES)
           (WHEN (AND (>= X (CHOICE-BOX-X1 BOX))
		      (<  X (CHOICE-BOX-X2 BOX)))
	     (SETQ STRING-OF-BOX (STRING (CHOICE-BOX-NAME BOX)))	; Find name of box...
	     ;; If the user specified documentation, use that.  Otherwise
	     ;; supply default who line documentation.
	     (IF (SETQ DOCUMENTATION (MEMBER :DOCUMENTATION BOX :TEST #'EQ))
		 (RETURN (CADR DOCUMENTATION))
		 ;;ELSE
                 (COND  ((STRING-EQUAL STRING-OF-BOX W:MARGIN-CHOICE-ABORT-STRING)	; "Abort"
                         (RETURN "Abort any changes and exit."))
                        ((STRING-EQUAL STRING-OF-BOX W:MARGIN-CHOICE-COMPLETION-STRING)	; "Do It"
                         (RETURN "Complete selection and exit."))
                        (T (RETURN "Any button to select choice.")))))))))
    (:REFRESH
     (let-if (mac-window-p self)
	     ((*dont-clip-at-the-margins* t))	  
       (LET ((REGION ARG1)
	     (ERASE-P ARG2)
	     LEFT TOP RIGHT BOTTOM)
	 (COND ((NOT (ZEROP (MARGIN-REGION-SIZE REGION)))
		(MULTIPLE-VALUE-SETQ (LEFT TOP RIGHT BOTTOM)
		  (MARGIN-REGION-AREA REGION))
		(PREPARE-SHEET (SELF)
		  (AND ERASE-P
		       (%DRAW-RECTANGLE (- RIGHT LEFT)
					(- BOTTOM TOP)
					LEFT TOP ERASE-ALUF SELF))
		  
		  (%DRAW-RECTANGLE (- RIGHT LEFT) 1 LEFT TOP CHAR-ALUF SELF))
		(SETQ TOP (+ TOP 2)) ;; provide 1 pixel margin between text and top line
		(SETQ MARGIN-CHOICES (COPY-list MARGIN-CHOICES)) ;; may 9-14-88
		(SETQ MARGIN-CHOICES (SORT MARGIN-CHOICES  #'ALPHALESSP :KEY #'CAR))
		(LET ((SPACE (BETWEEN-ITEM-SPACE LEFT RIGHT MARGIN-CHOICES))
		      (STRG-BOX-ENDX 0))
		  (DO ((CHOICES MARGIN-CHOICES (CDR CHOICES))
		       (X (+ LEFT 1) (+ X SPACE))
		       (FONT
			 (COND ((FONT-OBJECT-P MARGIN-CHOICE-FONT)
				MARGIN-CHOICE-FONT)
			       ((INTEGERP MARGIN-CHOICE-FONT)
				(AREF FONT-MAP MARGIN-CHOICE-FONT))
			       (T
				;; This should never happen, but it does because we can't find
				;; all old instances of flavors with margin-choice-mixin.
				(AREF FONT-MAP 0)))))
		      ((NULL CHOICES))
		    (LET* ((FONT (FUNCALL (SHEET-GET-SCREEN SELF)
					  :PARSE-FONT-DESCRIPTOR FONT))
			   (CHOICE (CAR CHOICES))
			   ;; Determined by box-width-factor and current blinker WIDTH.
			   (BOX-SIZE (CHOICE-BOX-WIDTH SELF))
			   (BOX-AND-SPACE (+ BOX-SIZE  (TRUNCATE (* (SHEET-CHAR-WIDTH SELF) BOX-WIDTH-FACTOR)))))
		      (SETQ STRG-BOX-STARTX   X )
		      (SETF (CHOICE-BOX-X1 CHOICE) STRG-BOX-STARTX)
		      (SETF (CHOICE-BOX-X2 CHOICE) (SETQ STRG-BOX-ENDX (+ (SEND SELF :STRING-OUT-EXPLICIT
										(string (CHOICE-BOX-NAME CHOICE))
										STRG-BOX-STARTX 
										TOP RIGHT NIL FONT CHAR-ALUF 0 NIL NIL)
									  BOX-AND-SPACE)))
		      (SETQ X  STRG-BOX-ENDX )
		      (DRAW-CHOICE-BOX SELF  (- STRG-BOX-ENDX  BOX-SIZE) TOP (CHOICE-BOX-STATE CHOICE))))))))))
    (:MOUSE-MOVES
     (LET* ((X ARG1)
	    (Y ARG2)
	    (TEMP-REGION CURRENT-REGION))	; Current region passed in by :after :mouse moves, above
       (IF (DOLIST (BOX MARGIN-CHOICES)
	     ;; sometimes it is possible to process the :mouse-move before the :refresh
	     ;; so check that the coordinates are already established.  PMH 11/23/87
	     (WHEN  (AND (CHOICE-BOX-X1 BOX)(CHOICE-BOX-X2 BOX)
			 (>= X (CHOICE-BOX-X1 BOX))
			 (<  X (CHOICE-BOX-X2 BOX)))
               (RETURN (SETQ STRG-BOX-STARTX (CHOICE-BOX-X1 BOX)
                             STRG-BOX-ENDX (CHOICE-BOX-X2 BOX)))))
           (MULTIPLE-VALUE-BIND (IGNORE TOP IGNORE BOTTOM)
	       (MARGIN-REGION-AREA TEMP-REGION)
             (LET ((STRG-BOX-WID  (+ 3 (- STRG-BOX-ENDX STRG-BOX-STARTX)))
                   ;; Handle multple line margin regions.
                   (STRG-BOX-HITE (+ 3 (- BOTTOM TOP))))
               (IF *HOLLOW-M-CHOICE-BOX-P*
                   (SETQ STRG-BLINKER (GET-BLINKER-TYPE (SEND SELF :BLINKER-LIST) 'HOLLOW-margin-choice-BLINKER))
                   ;;ELSE
                   (SETQ STRG-BLINKER (GET-BLINKER-TYPE (SEND SELF :BLINKER-LIST) 'RECTANGULAR-margin-choice-BLINKER)))
               (SEND STRG-BLINKER :SET-VISIBILITY :ON)
               (SEND STRG-BLINKER :SET-SIZE-AND-CURSORPOS STRG-BOX-WID STRG-BOX-HITE
		     (- STRG-BOX-STARTX left-margin-size) Y) ;; may 12-21-88 used left-margin-size instead of 4
               (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 0 :ON
                                             :SET-CHARACTER MOUSE-GLYPH-THIN-CROSS
                                             'FONTS:MOUSE))))))
    (:MOUSE-ENTERS-REGION
     (IF *HOLLOW-M-CHOICE-BOX-P*
         (ALLOCATE-RESOURCE 'HOLLOW-STRING-BOX-BLINKER SELF)	; Hollow margin-choice  blinker
         ;; ELSE (want a rectangular blinker resource)
         (ALLOCATE-RESOURCE 'STRING-BOX-BLINKER SELF))	; Rectangular margin-choice blinker
     (SETQ LAST-CHARACTER (SEND MOUSE-BLINKER :CHARACTER)))
    (:MOUSE-LEAVES-REGION
     (IF *HOLLOW-M-CHOICE-BOX-P*
         (AND (SETQ STRG-BLINKER (GET-BLINKER-TYPE (SEND SELF :BLINKER-LIST) 'HOLLOW-margin-choice-BLINKER))
              (SEND STRG-BLINKER  :SET-VISIBILITY :OFF)	
              (DEALLOCATE-RESOURCE 'HOLLOW-STRING-BOX-BLINKER STRG-BLINKER))
         ;; ELSE
         (AND  (SETQ STRG-BLINKER  (GET-BLINKER-TYPE (SEND SELF :BLINKER-LIST) 'RECTANGULAR-margin-choice-BLINKER))
               (SEND STRG-BLINKER  :SET-VISIBILITY :OFF)
               (DEALLOCATE-RESOURCE 'STRING-BOX-BLINKER STRG-BLINKER)))
     (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 0 :ON
				   :SET-CHARACTER LAST-CHARACTER
				   'FONTS:MOUSE))	; Set cursor back to arrow
    (:MOUSE-CLICK
     (LET ((X ARG1)
	   (Y ARG2)
	   (REGION ARG3))
       (HANDLE-CHOICE-BUTTON MARGIN-CHOICES X Y REGION)))))



