;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; PLANE-GRID-MIXIN METHODS

;;; Grid windows that display a plane.
(DEFMETHOD (PLANE-GRID-MIXIN :AREF) (I J)
  (PLANE-AREF PLANE I J)) 


(DEFMETHOD (PLANE-GRID-MIXIN :ASET) (VAL I J)
  (PLANE-ASET VAL PLANE I J)) 


(DEFMETHOD (PLANE-GRID-MIXIN :PLANE-EDGES) ()
  (PLANE-EDGES PLANE)) 

;;; Note that the entire contents of the planes is new.

(DEFMETHOD (PLANE-GRID-MIXIN :MUST-REDISPLAY-ENTIRE-PLANE) ()
  (APPLY SELF :MUST-REDISPLAY REDISPLAY-SOME
	 (MAPCAR '- (SEND SELF :PLANE-EDGES)
		 (LIST WINDOW-X-POS WINDOW-Y-POS WINDOW-X-POS WINDOW-Y-POS)))) 

;;; When we are about to change the size or offsets of the planes,
;;; note that every box NOW in the planes must be redisplayed.

(DEFMETHOD (PLANE-GRID-MIXIN :MUST-REDISPLAY-CURRENT-PLANE-AREA) ()
  (SEND SELF :MUST-REDISPLAY REDISPLAY-SOME MIN-CHANGED-X MIN-CHANGED-Y MAX-CHANGED-X
     MAX-CHANGED-Y)
  (LET ((CURRENT-PLANE-AREAS
	 (MAPCAR '- (SEND SELF :PLANE-EDGES)
		 (LIST WINDOW-X-POS WINDOW-Y-POS WINDOW-X-POS WINDOW-Y-POS))))
    (SETQ PREVIOUS-EDGES (MAPCAR 'FUNCALL '(MIN MIN MAX MAX) PREVIOUS-EDGES CURRENT-PLANE-AREAS)))) 

;;; Rename the old :erase-all method to erase-black and do just that.
(DEFMETHOD (PLANE-GRID-MIXIN :ERASE-BLACK) ()
  (SEND SELF :MUST-REDISPLAY-CURRENT-PLANE-AREA)
  (SETQ PLANE (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10)))



;;; GRAY-GRID-MIXIN Methods

(DEFMETHOD (GRAY-GRID-MIXIN :AREF) (I J)
  (IF GRAY-PLANE
    (DPB (PLANE-AREF GRAY-PLANE (+ I GRAY-X-OFFSET) (+ J GRAY-Y-OFFSET)) (BYTE 1 1)
	 (PLANE-AREF PLANE I J))
    (PLANE-AREF PLANE I J))) 


(DEFMETHOD (GRAY-GRID-MIXIN :GRAY-PLANE-EDGES) ()
  (LET* ((GRAY-PLANE-EDGES (AND GRAY-PLANE (PLANE-EDGES GRAY-PLANE))))
      (LIST (- (FIRST GRAY-PLANE-EDGES) GRAY-X-OFFSET)
	    (- (SECOND GRAY-PLANE-EDGES) GRAY-Y-OFFSET)
	    (- (THIRD GRAY-PLANE-EDGES) GRAY-X-OFFSET)
	    (- (FOURTH GRAY-PLANE-EDGES) GRAY-Y-OFFSET)))) 


(DEFMETHOD (GRAY-GRID-MIXIN :MUST-REDISPLAY-CURRENT-GRAY-AREA) ()
  (SEND SELF :MUST-REDISPLAY REDISPLAY-SOME MIN-CHANGED-X MIN-CHANGED-Y MAX-CHANGED-X
     MAX-CHANGED-Y)
  (LET ((CURRENT-PLANE-AREAS
	 (MAPCAR '- (SEND SELF :GRAY-PLANE-EDGES)
		 (LIST WINDOW-X-POS WINDOW-Y-POS WINDOW-X-POS WINDOW-Y-POS))))
    (SETQ PREVIOUS-EDGES (MAPCAR 'FUNCALL '(MIN MIN MAX MAX) PREVIOUS-EDGES CURRENT-PLANE-AREAS)))) 

(DEFMETHOD (GRAY-GRID-MIXIN :MUST-REDISPLAY-ENTIRE-GRAY-PLANE) ()
  (APPLY SELF :MUST-REDISPLAY REDISPLAY-SOME
	 (MAPCAR '- (SEND SELF :GRAY-PLANE-EDGES)
		 (LIST WINDOW-X-POS WINDOW-Y-POS WINDOW-X-POS WINDOW-Y-POS)))) 

(DEFMETHOD (GRAY-GRID-MIXIN :MOVE-GRAY-PLANE) (X-MOTION Y-MOTION &OPTIONAL HOME-FIRST)
  (SEND SELF :MUST-REDISPLAY-CURRENT-GRAY-AREA)
  (IF HOME-FIRST
    (SETQ GRAY-X-OFFSET 0
	  GRAY-Y-OFFSET 0))
  (SETQ GRAY-X-OFFSET (- GRAY-X-OFFSET X-MOTION))
  (SETQ GRAY-Y-OFFSET (- GRAY-Y-OFFSET Y-MOTION))
  (SEND SELF :MUST-REDISPLAY-ENTIRE-GRAY-PLANE)) 


(DEFMETHOD (GRAY-GRID-MIXIN :ERASE-GRAY) ()
  (SEND SELF :MUST-REDISPLAY-CURRENT-GRAY-AREA)
  (SETQ GRAY-PLANE (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))) 

;Array of gray tone with only 1 out of 4 points black.

(DEFVAR LIGHT-GRAY-ARRAY) 


(ADD-INITIALIZATION "Fed LIGHT-GRAY-ARRAY"
		    '(LET ((ARRAY (MAKE-ARRAY '(4 40) :TYPE ART-1B)))
		       (DOTIMES (I 4)
			 (DOTIMES (J 40)
			   (SETF (AREF ARRAY I J) (LOGAND 1 I (LOGXOR J 1)) )))
		       (SETQ LIGHT-GRAY-ARRAY ARRAY))
		    '(:NOW) 'ARRAY-ORDER-INITIALIZATION-LIST) 


(DEFMETHOD (GRAY-GRID-MIXIN :REDISPLAY-POINT) (I J NEW-VALUE OLD-VALUE)
  (IF (LOGTEST 1 (LOGXOR NEW-VALUE OLD-VALUE))
    (SYS:%DRAW-RECTANGLE BOX-X-SIZE BOX-Y-SIZE (+ 1 (* I BOX-X-SIZE) (TV:SHEET-INSIDE-LEFT SELF))
		     (+ 1 (* J BOX-Y-SIZE) (TV:SHEET-INSIDE-TOP SELF)) TV:ALU-XOR SELF))
  (IF (LOGTEST 2 (LOGXOR NEW-VALUE OLD-VALUE))
    (BITBLT TV:ALU-XOR BOX-X-SIZE BOX-Y-SIZE LIGHT-GRAY-ARRAY 0 0 (TV:SHEET-SCREEN-ARRAY SELF)
	    (+ 1 (* I BOX-X-SIZE) (TV:SHEET-INSIDE-LEFT SELF))
	    (+ 1 (* J BOX-Y-SIZE) (TV:SHEET-INSIDE-TOP SELF))))
  (SETF (AREF WINDOW-ARRAY I J) NEW-VALUE)) 


;;; CHAR-BOX-GRID-MIXIN Methods

;;; Plane windows with a special outline someplace (the character box and baseline).

;;; The initial default box size when the char-box initially comes up.
(DEFMETHOD (CHAR-BOX-GRID-MIXIN :AFTER :INIT) (IGNORE)
    (SETQ CHAR-BOX-X1 0
	  CHAR-BOX-Y1 0
	  CHAR-BOX-X2 7
	  CHAR-BOX-Y2 11
	  CHAR-BOX-Y3 14)) 

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :MOVE-CHAR-BOX) (X-MOTION Y-MOTION)
  (INCF CHAR-BOX-X1 X-MOTION)
  (INCF CHAR-BOX-X2 X-MOTION)
  (INCF CHAR-BOX-Y1 Y-MOTION)
  (INCF CHAR-BOX-Y2 Y-MOTION)
  (INCF CHAR-BOX-Y3 Y-MOTION)) 

;;; When the grid gets drawn, draw the character box as well.


(DEFMETHOD (CHAR-BOX-GRID-MIXIN :AFTER :DRAW-GRID) ()
  (COND
    ((NOT INHIBIT-CHAR-BOX)
     (SETQ DISPLAYED-CHAR-BOX-X1 (- CHAR-BOX-X1 WINDOW-X-POS)
	   DISPLAYED-CHAR-BOX-X2 (- CHAR-BOX-X2 WINDOW-X-POS)
	   DISPLAYED-CHAR-BOX-Y1 (- CHAR-BOX-Y1 WINDOW-Y-POS)
	   DISPLAYED-CHAR-BOX-Y2 (- CHAR-BOX-Y2 WINDOW-Y-POS)
	   DISPLAYED-CHAR-BOX-Y3 (- CHAR-BOX-Y3 WINDOW-Y-POS))
     (SEND SELF :DISPLAY-CHAR-BOX))
    (T (SETQ DISPLAYED-CHAR-BOX-X1 nil)))) 

;;; After redisplay, check that the character box is correct.

;;; rb 10/8/85 - make the box follow the character at any scale 

(DEFMETHOD (CHAR-BOX-GRID-MIXIN :AFTER :REDISPLAY) (&REST IGNORE)
  (COND
    (REDISPLAY-SUPPRESSED)
    ((AND DISPLAYED-CHAR-BOX-X1 (= DISPLAYED-CHAR-BOX-X1 (- CHAR-BOX-X1 WINDOW-X-POS))
	(= DISPLAYED-CHAR-BOX-X2 (- CHAR-BOX-X2 WINDOW-X-POS))
	(= DISPLAYED-CHAR-BOX-Y1 (- CHAR-BOX-Y1 WINDOW-Y-POS))
	(= DISPLAYED-CHAR-BOX-Y2 (- CHAR-BOX-Y2 WINDOW-Y-POS))
	(= DISPLAYED-CHAR-BOX-Y3 (- CHAR-BOX-Y3 WINDOW-Y-POS))))
    (T (AND DISPLAYED-CHAR-BOX-X1 (SEND SELF :DISPLAY-CHAR-BOX))
     (COND
       ((NULL INHIBIT-CHAR-BOX) (SETQ DISPLAYED-CHAR-BOX-X1 (- CHAR-BOX-X1 WINDOW-X-POS))
	(SETQ DISPLAYED-CHAR-BOX-X2 (- CHAR-BOX-X2 WINDOW-X-POS))
	(SETQ DISPLAYED-CHAR-BOX-Y1 (- CHAR-BOX-Y1 WINDOW-Y-POS))
	(SETQ DISPLAYED-CHAR-BOX-Y2 (- CHAR-BOX-Y2 WINDOW-Y-POS))
	(SETQ DISPLAYED-CHAR-BOX-Y3 (- CHAR-BOX-Y3 WINDOW-Y-POS)) (SEND SELF :DISPLAY-CHAR-BOX)))))) 

;;; XOR the char box and the baseline line in.


(DEFMETHOD (CHAR-BOX-GRID-MIXIN :DISPLAY-CHAR-BOX) ()
  (LET ((X1 (* BOX-X-SIZE DISPLAYED-CHAR-BOX-X1))
	(Y1 (* BOX-Y-SIZE DISPLAYED-CHAR-BOX-Y1))
	(X2 (* BOX-X-SIZE DISPLAYED-CHAR-BOX-X2))
	(Y2 (* BOX-Y-SIZE DISPLAYED-CHAR-BOX-Y2))
	(Y3 (* BOX-Y-SIZE DISPLAYED-CHAR-BOX-Y3)))
    (SEND SELF :DRAW-RECTANGLE 2 (- Y2 Y1) X1 Y1 TV:ALU-XOR)
    (COND
      ((= X1 X2))
      (T (SEND SELF :DRAW-RECTANGLE (- X2 X1) 2 (+ 2 X1) Y1 TV:ALU-XOR)
       (SEND SELF :DRAW-RECTANGLE 2 (- Y2 Y1) X2 (+ 2 Y1) TV:ALU-XOR)
       (SEND SELF :DRAW-RECTANGLE (- X2 X1) 2 X1 Y2 TV:ALU-XOR)
       (OR (= Y2 Y3) (SEND SELF :DRAW-RECTANGLE (- X2 -2 X1) 2 X1 Y3 TV:ALU-XOR)))))) 


;;; Push this button when the mouse is near an edge or corner of the character box,
;;;  and then as long as you hold the button down you are moving that corner.


(DEFMETHOD (CHAR-BOX-GRID-MIXIN :MOUSE-MOVE-CHAR-BOX) ()

  (MULTIPLE-VALUE-BIND (XOFF YOFF)
      (TV:SHEET-CALCULATE-OFFSETS SELF TV:MOUSE-SHEET)

;;; Since we aren't going to allow them to move the individual corners (must click in the label pane to do that)
;;; Then any right click will allow you to move the char box in all directions.  The commented out code
;;; below is what was used to determine what sides to move.  I would suggest we turn on this "rubber-banding"
;;; of the character box, if they click inside the box (pick a corner based on quadrant), or pick the base-line,
;;; or if outside of the box altogether, move the whole wad.  ... but for now it stays simple.  -dkm

;  ;; Decide which corner or edge of the character box we will move
;  ;;  (or maybe we aren't in range of any of them).
;  ;;  All horizontal edges move together,
;  ;;  and so do all vertical edges, since the size of the box
;  ;;  is set by mouse-sensitive items in FED-LABEL-WINDOW
;  (COND
;    ((< (ABS (- TV:MOUSE-X (* (- CHAR-BOX-X1 WINDOW-X-POS) BOX-X-SIZE) XOFF TV:LEFT-MARGIN-SIZE))
;	(TRUNCATE BOX-X-SIZE 2))
;     (SETQ X-POS-NAME 'CHAR-BOX-X1))
;    ((< (ABS (- TV:MOUSE-X (* (- CHAR-BOX-X2 WINDOW-X-POS) BOX-X-SIZE) XOFF TV:LEFT-MARGIN-SIZE))
;	(TRUNCATE BOX-X-SIZE 2))
;     (SETQ X-POS-NAME 'CHAR-BOX-X2)))
;  (COND
;    ((< (ABS (- TV:MOUSE-Y (* (- CHAR-BOX-Y1 WINDOW-Y-POS) BOX-Y-SIZE) YOFF TV:TOP-MARGIN-SIZE))
;	(TRUNCATE BOX-Y-SIZE 2))
;     (SETQ Y-POS-NAME 'CHAR-BOX-Y1))
;    ((< (ABS (- TV:MOUSE-Y (* (- CHAR-BOX-Y2 WINDOW-Y-POS) BOX-Y-SIZE) YOFF TV:TOP-MARGIN-SIZE))
;	(TRUNCATE BOX-Y-SIZE 2))
;     (SETQ Y-POS-NAME 'CHAR-BOX-Y2))
;    ((< (ABS (- TV:MOUSE-Y (* (- CHAR-BOX-Y3 WINDOW-Y-POS) BOX-Y-SIZE) YOFF TV:TOP-MARGIN-SIZE))
;	(TRUNCATE BOX-Y-SIZE 2))
;     (SETQ Y-POS-NAME 'CHAR-BOX-Y3)))
;  (IF (NOT (OR X-POS-NAME Y-POS-NAME)); If not in range to move any edge,
;    (BEEP);  complain.
			
    (DO ((NOT-FIRST NIL T)
	 X Y
	 OX OY
	 OLD-M-X OLD-M-Y
	 DELTA-X DELTA-Y)
	((AND NOT-FIRST (ZEROP TV:MOUSE-LAST-BUTTONS)))
      (AND NOT-FIRST (TV:MOUSE-WAIT OLD-M-X OLD-M-Y))
      (OR (EQ SELF (TV:WINDOW-OWNING-MOUSE)) (RETURN nil))
      (SETQ OLD-M-X TV:MOUSE-X
	    OLD-M-Y TV:MOUSE-Y)
      (SETQ X (TRUNCATE (+ (TRUNCATE BOX-X-SIZE 2) (- TV:MOUSE-X XOFF TV:LEFT-MARGIN-SIZE)) BOX-X-SIZE))
      (SETQ Y (TRUNCATE (+ (TRUNCATE BOX-Y-SIZE 2) (- TV:MOUSE-Y YOFF TV:TOP-MARGIN-SIZE)) BOX-Y-SIZE))
      ;; Exit if mouse is outside of FED grid area.
      (OR (AND (< -1 X (1+ WINDOW-X-SIZE)) (< -1 Y (1+ WINDOW-Y-SIZE))) (RETURN nil))
      (SETQ X (+ X WINDOW-X-POS)
	    Y (+ Y WINDOW-Y-POS))
      ;; Try moving the edges, remember where they used to be.
      (COND
	(NOT-FIRST
;;; If we aren't going to allow them to move the individual corners (must click in the label pane to do that)
;;; Then any right click will allow you to move the char box in all directions.  The commented out code
;;; here is what it used to do.  This wasn't good anyway as it restricted movement in only one direction
;;; at a time.
;	  (SETQ DELTA-X (IF X-POS-NAME (- X OX) 0))
;	  (INCF CHAR-BOX-X1 DELTA-X)
;	  (INCF CHAR-BOX-X2 DELTA-X)
;	  (SETQ DELTA-Y (IF Y-POS-NAME (- Y OY) 0))
;	  (INCF CHAR-BOX-Y1 DELTA-Y)
;	  (INCF CHAR-BOX-Y2 DELTA-Y)
;	  (INCF CHAR-BOX-Y3 DELTA-Y)

	  (SETQ DELTA-X (- X OX)
		DELTA-Y (- Y OY))
	  (INCF CHAR-BOX-X1 DELTA-X)
	  (INCF CHAR-BOX-X2 DELTA-X)
	  (INCF CHAR-BOX-Y1 DELTA-Y)
	  (INCF CHAR-BOX-Y2 DELTA-Y)
	  (INCF CHAR-BOX-Y3 DELTA-Y)
	  ;; If we are really moving an edge to a new place, redisplay.
	  (OR (AND (ZEROP DELTA-X) (ZEROP DELTA-Y))
	      (SEND SELF :REDISPLAY))))
      (SETQ OX X
	    OY Y)))) 


