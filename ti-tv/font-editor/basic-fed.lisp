;;; -*- Mode:Common-Lisp; Package:FED; Base:8; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B) -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; The Font editor itself.

;;; Add cursor-flavor information to the basic fed frame.

(DEFMETHOD (BASIC-FED :SELECTED-FONT) ()
  CURRENT-FONT) 

(DEFMETHOD (BASIC-FED :AFTER :INIT) (&REST IGNORE)
   (SETQ PLANE (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
   (SETQ GRAY-PLANE (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
   (SEND SELF :ERASE-ALL)
   (SEND SELF :HOME-BOX)
   (SETQ mouse-list				 ;;; initialize the list of available "mice"
	 '((6. 0. 0.  fonts:mouse) (25. 7. 0. fonts:mouse) (102. 6. 6. fonts:mouse) (62. 7. 7.  fonts:mouse)
	   (36. 7. 7. fonts:mouse) (37. 7. 7. fonts:mouse) (34. 7. 7.  fonts:mouse) (35. 7. 7.  fonts:mouse)
	   (32. 2. 7. fonts:mouse) (30. 6. 7. fonts:mouse) (29. 10. 7. fonts:mouse) (28. 14. 7. fonts:mouse)
	   (7. 4. 5.  fonts:mouse) (60. 7. 7. fonts:mouse) (97. 6. 6.  fonts:mouse) (38. 7. 7.  fonts:mouse)
	   (20. 5. 8. fonts:mouse) (26. 5. 7. fonts:mouse) (31. 7. 4.  fonts:mouse) (33. 8. 11. fonts:mouse)
	   (0. 4. 0.  fonts:mouse) (1. 15. 5. fonts:mouse) (2. 4. 14.  fonts:mouse) (3. 0. 5.   fonts:mouse)
	   (8. 6. 0.  fonts:mouse) (9. 15. 7. fonts:mouse) (10. 6. 14. fonts:mouse) (11. 7. 0.  fonts:mouse)))
   (SETQ basic-fed-flip-cursor (MAKE-INSTANCE 'FED:cursor	 ;; establish a default cursor for flip-mode
					      :cursor-character  102.
					      :cursor-font       fonts:mouse
					      :cursor-visibility nil
					      :cursor-window     self
					      :cursor-x-offset   6.
					      :cursor-y-offset   6.))
   (SETQ basic-fed-draw-cursor (MAKE-INSTANCE 'FED:cursor	 ;; establish a default cursor for draw-mode
					      :cursor-character  35.
					      :cursor-font       fonts:mouse
					      :cursor-visibility nil
					      :cursor-window     self
					      :cursor-x-offset   7.
					      :cursor-y-offset   7.))
   (SETQ basic-fed-erase-cursor (MAKE-INSTANCE 'FED:cursor	 ;; establish a default cursor for erase-mode
					       :cursor-character  34.
					       :cursor-font       fonts:mouse
					       :cursor-visibility nil
					       :cursor-window     self
					       :cursor-x-offset   7.
					       :cursor-y-offset   7.))
   (SETQ basic-fed-cursor basic-fed-flip-cursor))

(DEFMETHOD (BASIC-FED :MOUSE-STANDARD-BLINKER) ()
  "Sets the mouse cursor for this window.  It is automatically
called by the window manager when the mouse enters the screen
area occupied by this window."
   (LET (ch mfont x-off y-off)
     (COND (basic-fed-cursor
	    (MULTIPLE-VALUE-SETQ (ch mfont) (SEND basic-fed-cursor :character))
	    (MULTIPLE-VALUE-SETQ (x-off y-off) (SEND basic-fed-cursor :offset)))
	   (t (SETQ ch  6.			 ;; if none set use the default arrow
		    mfont fonts:mouse
		    x-off 0.
		    y-off 0.)))
     ;; Make sure that this window and all of its superiors are visible before we
     ;; change the mouse blinker definition.
     (DO ((mwindow self (tv:sheet-superior mwindow)))
	 ((NULL mwindow)	
	  (tv:mouse-set-blinker-definition :character
					   x-off
					   y-off
					   :on :set-character ch mfont))
       (COND ((NOT (tv:sheet-exposed-p mwindow))
	      (RETURN NIL))))))


(DEFMETHOD (BASIC-FED :AFTER :ERASE-ALL) ()
  (SETQ UNSAVED-CHANGES (NOT (NULL CURRENT-CHARACTER)))
  (AND CURRENT-FONT
       (NULL CURRENT-CHARACTER)
       (LET ((FD (FONT-GET-FD CURRENT-FONT)))
	 (SETQ CHAR-BOX-Y2 (FD-BASELINE FD)
	       CHAR-BOX-X2 (VALUES (ROUND (FD-SPACE-WIDTH FD)))
	       CHAR-BOX-Y3 (FD-LINE-SPACING FD))))) 


(DEFWRAPPER (BASIC-FED :DRAW-POINT) (IGNORE . BODY)
  `(PROGN
     (SETQ UNSAVED-CHANGES T)
     ,@BODY)) 


(DEFMETHOD (BASIC-FED :CONTENTS) ()
  (LIST PLANE CHAR-BOX-X1 CHAR-BOX-Y1 CHAR-BOX-X2 CHAR-BOX-Y2 CHAR-BOX-Y3)) 


(DEFMETHOD (BASIC-FED :SET-CONTENTS) (OTHER-PLANE BOX-X1 BOX-Y1 BOX-X2 BOX-Y2 BOX-Y3)
  (SETQ PLANE (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
  (MERGE-OTHER-PLANE OTHER-PLANE 0 0)
  (SETQ CHAR-BOX-X1 BOX-X1
	CHAR-BOX-Y1 BOX-Y1
	CHAR-BOX-X2 BOX-X2
	CHAR-BOX-Y2 BOX-Y2
	CHAR-BOX-Y3 BOX-Y3)
  (SETQ REDISPLAY-DEGREE REDISPLAY-ALL)
  (SETQ UNSAVED-CHANGES T)
  (SEND SELF :HOME-BOX)) 


(DEFUN MERGE-OTHER-PLANE (OTHER-PLANE OTHER-X-OFFSET OTHER-Y-OFFSET &OPTIONAL (OP :SET) (TO-PLANE PLANE))
  "Merge the contents of OTHER-PLANE into that of TO-PLANE.
OP is SET, CLEAR or FLIP.  The two offset args are the coordinates
of the point in OTHER-PLANE that corresponds to 0, 0 in TO-PLANE."
  (DECLARE (:SELF-FLAVOR BASIC-FED))
  (LET* ((OTHER-ORIGIN (PLANE-ORIGIN OTHER-PLANE))
	 (OTHER-END (PLANE-END OTHER-PLANE)))
    (COND
      ((NEQ OP :CLEAR)
       (PLANE-ENSURE-EXISTS TO-PLANE (- (FIRST OTHER-ORIGIN) OTHER-X-OFFSET)
			    (- (SECOND OTHER-ORIGIN) OTHER-Y-OFFSET))
       (PLANE-ENSURE-EXISTS TO-PLANE (- (FIRST OTHER-END) OTHER-X-OFFSET 1)
			    (- (SECOND OTHER-END) OTHER-Y-OFFSET 1))))
    (LET* ((NORM-ORIGIN (PLANE-ORIGIN TO-PLANE))
	   (BOOLE-OP
	    (CASE OP
	      (:SET TV:ALU-IOR)
	      (:CLEAR TV:ALU-ANDCA)
	      (:FLIP TV:ALU-XOR)
	      (OTHERWISE (FERROR nil "Invalid OP - ~S" OP))))
	   (X-OFFSET (+ (FIRST OTHER-ORIGIN) (- OTHER-X-OFFSET) (- (FIRST NORM-ORIGIN))))
	   (Y-OFFSET (+ (SECOND OTHER-ORIGIN) (- OTHER-Y-OFFSET) (- (SECOND NORM-ORIGIN))))
	   (X-BEG (MAX 0 (- X-OFFSET)))
	   (Y-BEG (MAX 0 (- Y-OFFSET)))
	   (X-END
	    (MIN (FIRST (ARRAY-DIMENSIONS OTHER-PLANE))
		 (- (FIRST (ARRAY-DIMENSIONS TO-PLANE)) X-OFFSET)))
	   (Y-END
	    (MIN (SECOND (ARRAY-DIMENSIONS OTHER-PLANE))
		 (- (SECOND (ARRAY-DIMENSIONS TO-PLANE)) Y-OFFSET))))
      (DO ((I X-BEG (1+ I)))
	  ((= I X-END))
	(DO ((J Y-BEG (1+ J)))
	    ((= J Y-END))
	  (SETF (AREF TO-PLANE (+ I X-OFFSET) (+ J Y-OFFSET))
		(BOOLE BOOLE-OP (AREF OTHER-PLANE I J)
		       (AREF TO-PLANE (+ I X-OFFSET) (+ J Y-OFFSET))))))
      (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE))))

(DEFMETHOD (BASIC-FED :MERGE-CONTENTS) (MERGE-OP OTHER-PLANE BOX-X1 BOX-Y1 BOX-X2 BOX-Y2 BOX-Y3)
  BOX-X2
  BOX-Y1
  BOX-Y3
  (IF (EQ MERGE-OP :COPY)
    (SETQ PLANE (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10)
	  MERGE-OP :SET))
  (SETQ UNSAVED-CHANGES T)
  (MERGE-OTHER-PLANE OTHER-PLANE (- BOX-X1 CHAR-BOX-X1) (- BOX-Y2 CHAR-BOX-Y2) MERGE-OP)) 


(DEFMETHOD (BASIC-FED :MERGE-GRAY) (MERGE-OP OTHER-PLANE BOX-X1 BOX-Y1 BOX-X2 BOX-Y2 BOX-Y3)
  BOX-X2
  BOX-Y1
  BOX-Y3
  (IF (EQ MERGE-OP :COPY)
    (SETQ GRAY-PLANE (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10)
	  MERGE-OP :SET))
  (MERGE-OTHER-PLANE OTHER-PLANE (- BOX-X1 CHAR-BOX-X1 GRAY-X-OFFSET)
		     (- BOX-Y2 CHAR-BOX-Y2 GRAY-Y-OFFSET) MERGE-OP GRAY-PLANE)) 

;;; Return the window of the FED window to home position.


(DEFMETHOD (BASIC-FED :HOME-BOX) ()
  (SEND SELF :SET-OFFSET
	(- (TRUNCATE (+ CHAR-BOX-X1 CHAR-BOX-X2) 2) (TRUNCATE WINDOW-X-SIZE 2))
	(- (TRUNCATE (+ CHAR-BOX-Y1 CHAR-BOX-Y3) 2) (TRUNCATE WINDOW-Y-SIZE 2)))
  (SETQ CURSOR-X 0
	CURSOR-Y 0)) 


(DEFMETHOD (BASIC-FED :BEFORE :SET-OFFSET) (X Y)
  (SETQ CURSOR-X (MAX 0 (MIN WINDOW-X-SIZE (- CURSOR-X (- X WINDOW-X-POS)))))
  (SETQ CURSOR-Y (MAX 0 (MIN WINDOW-Y-SIZE (- CURSOR-Y (- Y WINDOW-Y-POS)))))) 


(DEFMETHOD (BASIC-FED :AFTER :REDISPLAY) (&REST IGNORE)
  (COND
    ((AND CURSOR-ON (NOT REDISPLAY-SUPPRESSED))
     (MULTIPLE-VALUE-BIND (X Y)
	 (TV:SHEET-CALCULATE-OFFSETS SELF TV:MOUSE-SHEET)
       (TV:BLINKER-SET-CURSORPOS TV:MOUSE-BLINKER
	  (+ X (VALUES (FLOOR (* BOX-X-SIZE (+ 0.5s0 CURSOR-X)))))
	  (+ Y (VALUES (FLOOR (* BOX-Y-SIZE (+ 0.5s0 CURSOR-Y)))))))
     (TV:BLINKER-SET-VISIBILITY TV:MOUSE-BLINKER :BLINK)))) 

