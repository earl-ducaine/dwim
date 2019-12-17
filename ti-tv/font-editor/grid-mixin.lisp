;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; GRID-MIXIN Methods

;;; CHANGE HISTORY
;;;
;;;   3/22/87  DKM  - changed (:method grid-mixin :redisplay) to take into account the size of the Gray Plane
;;;                         when calculating the min/max for area of the grid that needs to be displayed.

(DEFMETHOD (GRID-MIXIN :AFTER :INIT) (INIT-PLIST)
  (SEND SELF :DEDUCE-WINDOW-ARRAY-SIZE
	(OR (GET INIT-PLIST :WINDOW-ARRAY-TYPE) 'ART-1B))) 


(DEFMETHOD (GRID-MIXIN :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (LET ((OLD-X-SIZE WINDOW-X-SIZE)
	(OLD-Y-SIZE WINDOW-Y-SIZE))
    (SEND SELF :DEDUCE-WINDOW-ARRAY-SIZE)
    ;; Contrive to preserve which font pixel is in the center of the window.
    (DECF WINDOW-X-POS (TRUNCATE (- WINDOW-X-SIZE OLD-X-SIZE) 2))
    (DECF WINDOW-Y-POS (TRUNCATE (- WINDOW-Y-SIZE OLD-Y-SIZE) 2)))) 


(DEFMETHOD (GRID-MIXIN :DEDUCE-WINDOW-ARRAY-SIZE) (&OPTIONAL ARRAY-TYPE)
  "Set WINDOW-ARRAY to an array of type ARRAY-TYPE, with the correct size.
The correct size is enough to record enough boxes of size BOX-X-SIZE
by BOX-Y-SIZE to fill up the window.
Also sets WINDOW-X-SIZE and WINDOW-Y-SIZE to the size values."
  (OR ARRAY-TYPE (SETQ ARRAY-TYPE (ARRAY-TYPE WINDOW-ARRAY)))
  (LET ((LAST-ROW-OF-DOTS (IF (AND (> BOX-X-SIZE MIN-BOX-SIZE) (> BOX-Y-SIZE MIN-BOX-SIZE))
			          2
			          0)))
    (SETQ WINDOW-X-SIZE (TRUNCATE (- (TV:SHEET-INSIDE-WIDTH) LAST-ROW-OF-DOTS) BOX-X-SIZE)
	  WINDOW-Y-SIZE (TRUNCATE (- (TV:SHEET-INSIDE-HEIGHT) LAST-ROW-OF-DOTS) BOX-Y-SIZE))
    (OR (AND (VARIABLE-BOUNDP WINDOW-ARRAY)
	        (<= WINDOW-X-SIZE (ARRAY-DIMENSION WINDOW-ARRAY 0))
	        (<= WINDOW-Y-SIZE (ARRAY-DIMENSION WINDOW-ARRAY 1)))
             (SETQ WINDOW-ARRAY (MAKE-ARRAY (LIST WINDOW-X-SIZE WINDOW-Y-SIZE) :TYPE ARRAY-TYPE))))) 

;;; If we didn't come back, remember that the screen is clobbered.


(DEFMETHOD (GRID-MIXIN :AFTER :REFRESH) (&REST IGNORE)
  (OR TV:RESTORED-BITS-P (SETQ REDISPLAY-DEGREE REDISPLAY-ALL))) 

;;; Note that something has changed for the redisplay loop.


(DEFMETHOD (GRID-MIXIN :MUST-REDISPLAY) (DEGREE &OPTIONAL MIN-X MIN-Y MAX-X MAX-Y)
  (IF (= DEGREE REDISPLAY-ONE)			;Just one box to hack
    (SETQ MAX-X MIN-X
	  MAX-Y MIN-Y))
  (COND
    ((>= REDISPLAY-DEGREE REDISPLAY-ONE)
     ;; If some redisplay already requested, merge in new request.
     (AND MIN-X (SETQ MIN-CHANGED-X (MIN MIN-CHANGED-X MIN-X)))
     (AND MIN-Y (SETQ MIN-CHANGED-Y (MIN MIN-CHANGED-Y MIN-Y)))
     (AND MAX-X (SETQ MAX-CHANGED-X (MAX MAX-CHANGED-X MAX-X)))
     (AND MAX-Y (SETQ MAX-CHANGED-Y (MAX MAX-CHANGED-Y MAX-Y))))
    ;; This is the first redisplay requested => take just what is requested now.
    (T (AND MIN-X (SETQ MIN-CHANGED-X MIN-X))
       (AND MAX-X (SETQ MAX-CHANGED-X MAX-X))
       (AND MIN-Y (SETQ MIN-CHANGED-Y MIN-Y))
       (AND MAX-Y (SETQ MAX-CHANGED-Y MAX-Y))))
  (SETQ REDISPLAY-DEGREE (MAX REDISPLAY-DEGREE DEGREE))) 

;;; Function is an argument of the two grid points which returns the correct array value
;;;  from the other data structure


(DEFMETHOD (GRID-MIXIN :REDISPLAY) (&OPTIONAL (FORCE-TO-COMPLETION) &AUX PLANE-EDGES)
  (SETQ REDISPLAY-SUPPRESSED nil)
  (COND
    ((= REDISPLAY-DEGREE REDISPLAY-NONE));No redisplay needed
    ((AND (NOT FORCE-TO-COMPLETION) (SEND SELF :LISTEN))
        (SETQ REDISPLAY-SUPPRESSED T))
    (T
     (COND
       ((= REDISPLAY-DEGREE REDISPLAY-ALL)
	(SEND SELF :CLEAR-SCREEN)		; Every box is now clear on the screen.
	(ARRAY-INITIALIZE WINDOW-ARRAY 0)
	;; but every box must be checked for redisplay.
	(SETQ MIN-CHANGED-X 0
	      MIN-CHANGED-Y 0
	      MAX-CHANGED-X (1- WINDOW-X-SIZE)
	      MAX-CHANGED-Y (1- WINDOW-Y-SIZE))
	(SETQ REDISPLAY-DEGREE REDISPLAY-SOME)))
     ;; Since the commands don't seem to clip the change boundaries, do so here
     ;;  in case the font is too big to fit in the window.
     (SETQ MIN-CHANGED-X (MAX MIN-CHANGED-X 0)
	   MIN-CHANGED-Y (MAX MIN-CHANGED-Y 0)
	   MAX-CHANGED-X (MIN MAX-CHANGED-X (1- WINDOW-X-SIZE))
	   MAX-CHANGED-Y (MIN MAX-CHANGED-Y (1- WINDOW-Y-SIZE)))
     ;; Realize that nothing outside the stored area of the plane
     ;; can possibly have changed.
     ;; Also merge in the PREVIOUS-EDGES, which is an additional
     ;; area of screen that may have changed and is not limited
     ;; to the currently existing plane areas.
     (SETQ PLANE-EDGES (MAPCAR 'FUNCALL '(MIN MIN MAX MAX)
			       (SEND SELF :PLANE-EDGES)
			       (SEND SELF :GRAY-PLANE-EDGES)
			       ))
     (SETQ MIN-CHANGED-X
	   (MIN (MAX 0 (FIRST PREVIOUS-EDGES))
		(MAX MIN-CHANGED-X (- (FIRST PLANE-EDGES) WINDOW-X-POS))))
     (SETQ MIN-CHANGED-Y
	   (MIN (MAX 0 (SECOND PREVIOUS-EDGES))
		(MAX MIN-CHANGED-Y (- (SECOND PLANE-EDGES) WINDOW-Y-POS))))
     (SETQ MAX-CHANGED-X
	   (MAX (1- (MIN WINDOW-X-SIZE (THIRD PREVIOUS-EDGES)))
		(MIN MAX-CHANGED-X (- (THIRD PLANE-EDGES) WINDOW-X-POS))))
     (SETQ MAX-CHANGED-Y
	   (MAX (1- (MIN WINDOW-Y-SIZE (FOURTH PREVIOUS-EDGES)))
		(MIN MAX-CHANGED-Y (- (FOURTH PLANE-EDGES) WINDOW-Y-POS))))
     ;; Now, for each box which isn't already displayed in the right state,
     ;;  update it.
     (TV:PREPARE-SHEET (SELF)
	(BLOCK ABORT-REDISPLAY
	  (DO ((J MIN-CHANGED-Y (1+ J))
	       (AREF-HANDLER (GET-HANDLER-FOR SELF :AREF)); For speed.
	       (LISTEN-HANDLER (GET-HANDLER-FOR SELF :LISTEN))
	       (DRAW-HANDLER (GET-HANDLER-FOR SELF :REDISPLAY-POINT)))
	      ((> J MAX-CHANGED-Y)
	       (SETQ PREVIOUS-EDGES '(0 0 0 0))
	       (SETQ MAX-CHANGED-X MIN-CHANGED-X
		     MAX-CHANGED-Y MIN-CHANGED-Y)
	       (SETQ REDISPLAY-DEGREE REDISPLAY-NONE))
	    (DO ((I MIN-CHANGED-X (1+ I))
		 (OLD-VALUE)
		 (NEW-VALUE))
		((> I MAX-CHANGED-X))
	      (OR
	       (=
		(SETQ NEW-VALUE (SEND AREF-HANDLER :AREF (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS)))
		(SETQ OLD-VALUE (AREF WINDOW-ARRAY I J)))
	       (SEND DRAW-HANDLER :REDISPLAY-POINT I J NEW-VALUE OLD-VALUE)))
	    (COND
	      ((AND (NOT FORCE-TO-COMPLETION) (SEND LISTEN-HANDLER :LISTEN))
	       (SETQ MIN-CHANGED-Y (1+ J)) (SETQ REDISPLAY-SUPPRESSED T)
	       (RETURN-FROM ABORT-REDISPLAY))))))))) 


(DEFMETHOD (GRID-MIXIN :AFTER :CLEAR-SCREEN) ()
  (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE)
  (SEND SELF :DRAW-GRID)) 

;;; This is a message so you can put some daemons on it to draw other things (like the
;;;   character box).


(DEFVAR GRID-BITBLT-KLUDGE (MAKE-ARRAY '(64. 64.) :TYPE ART-1B)) 

(DEFVAR GRID-BITBLT-ONES (MAKE-ARRAY '(32. 32.) :TYPE ART-1B)) 


(DEFMETHOD (GRID-MIXIN :DRAW-GRID) ()
  (LET ((DEFAULT-WINDOW-X-SIZE (TRUNCATE  (TV:SHEET-INSIDE-WIDTH SELF)  DEFAULT-BOX-SIZE))          ;; added -mrr 
	(DEFAULT-WINDOW-Y-SIZE (TRUNCATE  (TV:SHEET-INSIDE-HEIGHT SELF )  DEFAULT-BOX-SIZE))) 

 ;; Now add in the grid points, unless the grid is too small.
    (COND
      ((NOT (OR (< BOX-X-SIZE MIN-BOX-SIZE) (< BOX-Y-SIZE MIN-BOX-SIZE)))
       ;; Make an array containing the necessary dots.
       (BITBLT 0 64. 64. GRID-BITBLT-KLUDGE 0 0 GRID-BITBLT-KLUDGE 0 0)
       (BITBLT 15. 32. 32. GRID-BITBLT-ONES 0 0 GRID-BITBLT-ONES 0 0)
       (DO ((I 0 (+ I BOX-X-SIZE)))
	   ((> (+ I GRID-POINT-SIZE) 64.))
	 (DO ((J 0 (+ J BOX-Y-SIZE)))
	     ((> (+ J GRID-POINT-SIZE) 64.))
	   (BITBLT TV:ALU-IOR GRID-POINT-SIZE GRID-POINT-SIZE GRID-BITBLT-ONES 0 0
		   GRID-BITBLT-KLUDGE I J)))
       ;; Smear the array over the window.
       (LOOP WITH XINC = (* (TRUNCATE 64. BOX-X-SIZE) BOX-X-SIZE)
	     WITH XSIZE = (* (1+ DEFAULT-WINDOW-X-SIZE) DEFAULT-BOX-SIZE)             ;use default to cover entire grid -mrr
	     FOR I FROM 0 BY XINC BELOW XSIZE
	     DO (LOOP WITH YINC = (* (TRUNCATE 64. BOX-Y-SIZE) BOX-Y-SIZE)
		      WITH YSIZE = (* (1+ DEFAULT-WINDOW-Y-SIZE) DEFAULT-BOX-SIZE)    ;use default to cover entire grid -mrr
		      FOR J FROM 0 BY YINC BELOW YSIZE
		      DO (SEND SELF :BITBLT TV:ALU-SETA (MIN (- XSIZE I) XINC) (MIN (- YSIZE J) YINC)
			       GRID-BITBLT-KLUDGE 0 0 I J))))))) 

;;; Complement the state of a point in the grid, and store the new value in our array
;;;  FROM-REDISPLAY means that this value came from the other data structure, so don't
;;;  bother trying to update it.


(DEFMETHOD (GRID-MIXIN :REDISPLAY-POINT) (I J &OPTIONAL NEW-VALUE OLD-VALUE)
  (OR (= NEW-VALUE OLD-VALUE)
     (SYS:%DRAW-RECTANGLE BOX-X-SIZE BOX-Y-SIZE (+ (* I BOX-X-SIZE) (TV:SHEET-INSIDE-LEFT SELF))
		      (+ (* J BOX-Y-SIZE) (TV:SHEET-INSIDE-TOP SELF)) TV:ALU-XOR SELF))
  (SETF (AREF WINDOW-ARRAY I J) NEW-VALUE)) 

;This is what mouse commands use to alter a point to a new value.

(DEFMETHOD (GRID-MIXIN :DRAW-POINT) (I J NEW-VALUE)
 ;; First set the value (normal plane only)
  (SEND SELF :ASET NEW-VALUE (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS))
  ;; Then redisplay both planes.
  (TV:PREPARE-SHEET (SELF)
     (SEND SELF :REDISPLAY-POINT I J (SEND SELF :AREF (+ I WINDOW-X-POS) (+ J WINDOW-Y-POS))
	(AREF WINDOW-ARRAY I J)))) 
  

(DEFVAR GRAY-ARRAY) 


(ADD-INITIALIZATION "Fed GRAY-ARRAY"
		    '(LET ((ARRAY (MAKE-ARRAY '(4 40) :TYPE ART-1B))) 
		       (DOTIMES (I 4)
			 (DOTIMES (J 40)
			   (SETF (AREF ARRAY I J) (LOGXOR I J) )))
		       (SETQ GRAY-ARRAY ARRAY))
		    '(:NOW) 'ARRAY-ORDER-INITIALIZATION-LIST) 


(DEFMETHOD (GRID-MIXIN :GRAY-POINT) (X Y)
  (SEND SELF :BITBLT TV:ALU-XOR BOX-X-SIZE BOX-Y-SIZE GRAY-ARRAY 0 0 (* X BOX-X-SIZE)
     (* Y BOX-Y-SIZE))) 


(DEFMETHOD (GRID-MIXIN :SET-BOX-SIZE) (&OPTIONAL (NEW-X-SIZE DEFAULT-BOX-SIZE) (NEW-Y-SIZE NEW-X-SIZE))
  (COND
    ((NOT (AND (= BOX-X-SIZE NEW-X-SIZE) (= BOX-Y-SIZE NEW-Y-SIZE)))
     (SETQ BOX-X-SIZE NEW-X-SIZE
	   BOX-Y-SIZE NEW-Y-SIZE
	   REDISPLAY-DEGREE REDISPLAY-ALL)
     (SEND SELF :DEDUCE-WINDOW-ARRAY-SIZE)))) 

;;; Flip, Set, or Clear the current grid point that the mouse is over. BOOLE is the boolean to apply to the
;;; square. CLICK-XPOS and CLICK-YPOS are the mouse coordinates, relative to the FED window 
;;; (not tv:mouse-x and tv:mouse-y which are raw screen coordinates).  If use-prev-x-y? is non-nil,
;;; it will not do the operation if special variables PREV-X and PREV-Y are the same grid points
;;;as the grid point indicated by the current mouse position.  This is used with the mouse to support
;;;continuous left click, without constantly flipping the grid point.  When use-prev-x-y? is non-nil,
;;;it also sets PREV-X and PREV-y to the current grid-point.

(DEFMACRO BOOLE-SQUARE (BOOLE CLICK-XPOS CLICK-YPOS &OPTIONAL USE-PREV-X-Y?)
  '(DECLARE (:SELF-FLAVOR GRID-MIXIN))
  `(LET ((X (TRUNCATE (- ,CLICK-XPOS TV:LEFT-MARGIN-SIZE 1) BOX-X-SIZE))
	 (Y (TRUNCATE (- ,CLICK-YPOS TV:TOP-MARGIN-SIZE 1) BOX-Y-SIZE)))
    (DECLARE (SPECIAL PREV-X PREV-Y))
    (COND
      ((NOT (AND (< -1 X WINDOW-X-SIZE) (< -1 Y WINDOW-Y-SIZE))) NIL)
      ((AND ,USE-PREV-X-Y? (= X PREV-X) (= Y PREV-Y)) T)
      (T (LET* ((OLD-VALUE (LOGAND 1 (AREF WINDOW-ARRAY X Y)))
		(NEW-VALUE (BOOLE ,BOOLE 1 OLD-VALUE)))
	   (OR (= OLD-VALUE NEW-VALUE) (SEND SELF :DRAW-POINT X Y NEW-VALUE)))
	 (WHEN ,USE-PREV-X-Y?
	    (SETQ PREV-X X
		  PREV-Y Y))
	 T))))

;;; This performs the indicated operation on the grid until you release the button.
(DEFMETHOD (GRID-MIXIN :MOUSE-BOOLE-SQUARES) (BOOLE CLICK-XPOS CLICK-YPOS)
  (SEND SELF :REDISPLAY T);Force redisplay to completion first
  (LET ((WINDOW-X-OFFSET 0)
	(WINDOW-Y-OFFSET 0)
	(PREV-X -1)
	(PREV-Y -1)
	WINDOW-X WINDOW-Y)
    (DECLARE (SPECIAL PREV-X PREV-Y))
    (SEND SELF :MOUSE-STANDARD-BLINKER)
    (MULTIPLE-VALUE-SETQ (WINDOW-X-OFFSET WINDOW-Y-OFFSET)
      (TV:SHEET-CALCULATE-OFFSETS SELF TV:MOUSE-SHEET))
    (DO ((WAIT-FLAG NIL T)
	 OLD-X
	 OLD-Y)
	(NIL)
      (COND
	(WAIT-FLAG (TV:MOUSE-WAIT OLD-X OLD-Y) (SETQ OLD-X TV:MOUSE-X
						     OLD-Y TV:MOUSE-Y))
	(T (SETQ OLD-X (+ CLICK-XPOS WINDOW-X-OFFSET)
		 OLD-Y (+ CLICK-YPOS WINDOW-Y-OFFSET))))
      (SETQ WINDOW-X (- OLD-X WINDOW-X-OFFSET)
	    WINDOW-Y (- OLD-Y WINDOW-Y-OFFSET))
      ;; Update the position of the mouse before checking for button clicks, so
      ;; that button clicks get processed with knowledge of where the mouse
      ;; was when the button was first clicked.  The click-xpos and click-ypos arguments
      ;; are where the mouse was when the button was clicked, whereas the
      ;; mouse cursor follows TV:MOUSE-X and TV:MOUSE-Y, which may be different.
      (TV:MOUSE-SET-BLINKER-CURSORPOS :MOUSE-MOVES WINDOW-X WINDOW-Y)
      (UNLESS (BOOLE-SQUARE BOOLE WINDOW-X WINDOW-Y T)
	 (RETURN nil))
      (IF (ZEROP (TV:MOUSE-BUTTONS T))
	  (RETURN nil)))
    (TV:MOUSE-BUTTONS)))

(DEFMETHOD (GRID-MIXIN :MOVE-MOUSE-CURSOR) (X Y)
  ;;Put the mouse in the center of a grid box, after moving X Y grid boxes.  Used by keyboard
  ;;cursor commands.  This actually moves the mouse cursor instead of a separate cursor.
  (LET* ((DELTA-X (* X BOX-X-SIZE))
	 (DELTA-Y (* Y BOX-Y-SIZE))
	 (NEW-X (MAX 0 (MIN (+ DELTA-X (- TV:MOUSE-X TV:X-OFFSET TV:LEFT-MARGIN-SIZE)) ;Shifted x, relative to grid
			    (- TV:WIDTH TV:LEFT-MARGIN-SIZE TV:RIGHT-MARGIN-SIZE BOX-X-SIZE))))   
	 (NEW-Y (MAX 0 (MIN (+ DELTA-Y (- TV:MOUSE-Y TV:Y-OFFSET TV:TOP-MARGIN-SIZE))  ;Shifted y, relative to grid
			    (- TV:HEIGHT TV:TOP-MARGIN-SIZE TV:BOTTOM-MARGIN-SIZE BOX-Y-SIZE))))
	 (CENTERED-X (+ (* BOX-X-SIZE (TRUNCATE NEW-X BOX-X-SIZE))
			(TRUNCATE BOX-X-SIZE 2)
			1))
	 (CENTERED-Y (+ (* BOX-Y-SIZE (TRUNCATE NEW-Y BOX-Y-SIZE))
			(TRUNCATE BOX-Y-SIZE 2)
			1)))
    (TV:MOUSE-WARP (+ CENTERED-X TV:X-OFFSET TV:LEFT-MARGIN-SIZE)
		  (+ CENTERED-Y TV:Y-OFFSET TV:TOP-MARGIN-SIZE))))

;;; Does the equivalent of clicking on a grid point the mouse is over.  This insures that the mouse is
;;; over a grid point before applying the boole operation to it.
(DEFMETHOD (GRID-MIXIN :KEYPAD-BOOLE-SQUARE) (BOOLE X Y)
  (LET ((X (MAX TV:LEFT-MARGIN-SIZE (MIN (- X TV:X-OFFSET)
					 (- TV:WIDTH TV:LEFT-MARGIN-SIZE TV:RIGHT-MARGIN-SIZE))))
	(Y (MAX TV:TOP-MARGIN-SIZE (MIN (- Y TV:Y-OFFSET)
					(- TV:HEIGHT TV:TOP-MARGIN-SIZE TV:BOTTOM-MARGIN-SIZE)))))
    (BOOLE-SQUARE BOOLE X Y))) 

(DEFMETHOD (GRID-MIXIN :SET-OFFSET) (NEW-X-POS NEW-Y-POS)
 ;; Must redisplay the areas we occupied BEFORE
  (SEND SELF :MUST-REDISPLAY-CURRENT-PLANE-AREA)
  (SETQ WINDOW-X-POS NEW-X-POS
	WINDOW-Y-POS NEW-Y-POS)
  ;; and the ones we occupy AFTER.
  (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE)) 


(DEFMETHOD (GRID-MIXIN :MOVE-PLANE) (X-MOTION Y-MOTION &OPTIONAL HOME-FIRST)
  (IF HOME-FIRST
    (SEND SELF :SET-OFFSET (- X-MOTION) (- Y-MOTION))
    ;;else
    (SEND SELF :SET-OFFSET (- WINDOW-X-POS X-MOTION) (- WINDOW-Y-POS Y-MOTION))))


(DEFMETHOD (GRID-MIXIN :DRAW-GRID-LINE) (X0 Y0 X1 Y1 DRAW-MODE &AUX DX DY YI FLAG)
  (SETQ DX (- X1 X0)
	DY (- Y1 Y0))
  (AND (MINUSP DX) (SETQ DX (- DX)
			 X0 X1
			 DY (- DY)
			 Y0 Y1))
  (IF (MINUSP DY)
    (SETQ DY (- DY)
	  YI -1)
    (SETQ YI 1))
  (AND (SETQ FLAG (> DY DX)) (PSETQ DX DY DY DX))
  (DO ((A (TRUNCATE DX 2))
       (C DX (1- C)))
      ((< C 0))
    (CASE DRAW-MODE
     ;; IOR
      (7 (AND (ZEROP (AREF WINDOW-ARRAY X0 Y0)) (SEND SELF :DRAW-POINT X0 Y0 1)))
      ;; ANDCA
      (2 (OR (ZEROP (AREF WINDOW-ARRAY X0 Y0)) (SEND SELF :DRAW-POINT X0 Y0 0)))
      ;; XOR
      (6 (SEND SELF :DRAW-POINT X0 Y0 (- 1 (AREF WINDOW-ARRAY X0 Y0)))))
    (COND
      ((MINUSP (SETQ A (- A DY))) (SETQ A (+ A DX)) (SETQ X0 (1+ X0)
							  Y0 (+ Y0 YI)))
      (FLAG (SETQ Y0 (+ Y0 YI)))
      (T (SETQ X0 (1+ X0)))))) 


(DEFMETHOD (GRID-MIXIN :DRAW-CURVE) (PX PY &OPTIONAL END (DRAW-MODE 7))
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH PX)))
  (DO ((I 1 (1+ I))
       (X0)
       (X1 (VALUES (FLOOR (AREF PX 0))))
       (Y0)
       (Y1 (VALUES (FLOOR (AREF PY 0))))
       (HANDLER (GET-HANDLER-FOR SELF :DRAW-GRID-LINE)))
      ((>= I END))
    (SETQ X0 X1)
    (OR (SETQ X1 (AREF PX I)) (RETURN nil))
    (SETQ X1 (VALUES (FLOOR X1)))
    (SETQ Y0 Y1)
    (OR (SETQ Y1 (AREF PY I)) (RETURN nil))
    (SETQ Y1 (VALUES (FLOOR Y1)))
    (SEND HANDLER :DRAW-GRID-LINE X0 Y0 X1 Y1 DRAW-MODE))) 


