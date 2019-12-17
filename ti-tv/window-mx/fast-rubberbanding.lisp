;;; -*- Mode:Common-Lisp; Package:TV; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB); Patch-file:T; Base:10 -*-



;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1989 Texas Instruments Incorporated. All rights reserved.

;;; This file defines the mixin for using the mX's fast rubberbanding capability.

;;;                              Patch
;;;     Date    Author  Number   Description
;;;--------------------------------------------------------------------
;;; 02/07/89   LG                       Original.

(DEFFLAVOR fast-rubberbanding-mixin
	   ((mouse-in-me nil)			       ; T iff mouse was in this window as of the
						       ;    last :mouse-moves message.
	    (base-x 0)				       ; Screen-relative coordinates of base point
	    (base-y 0)				       ;    for rubber banding.  -1 if no
						             ;    rubberbanding desired.
	    (arrowhead-direction :to)		       ;  :none => draw no arrowhead.
						       ;  :to => draw arrowhead pointing toward mouse.
						       ;  :from => draw arrowhead pointing toward base.
	    (line-type :solid)			       ;  :solid => draw arrow's long-line solid.
						       ;  :dashed => draw arrow's long-line dashed.
	    (erase-rubberband-on-exit 1)	       ; 1 if last rubberband line should be erased
						       ;    when the mouse leaves the window, -1 if
						             ;    it should not be erased.
	    (old-x -1)				       ; SELF-relative coordinates of end of last
	    (old-y -1)				       ;    rubberband line drawn.  -1 if none.
	    (old-blinker nil)
	    (rubberbanding-suppression-count 2)
	    )
	   ()
  (:inittable-instance-variables)
  (:gettable-instance-variables mouse-in-me)
  (:settable-instance-variables
    base-x base-y arrowhead-direction erase-rubberband-on-exit old-x old-y rubberbanding-suppression-count line-type))


(DEFMETHOD (fast-rubberbanding-mixin :after :mouse-moves)
	   (new-x new-y &aux (old-mouse-in-me mouse-in-me)
	    (frb-window (window-under-mouse :rubberbanding-suppression-count)))
  
  ;;  Remember: NEW-X and NEW-Y are relative to SELF, not relative to the mouse sheet...

  (WHEN (AND (INTEGERP base-x) (< -1 base-x)
	     (INTEGERP base-y) (< -1 base-y))
    ;; If the mouse just entered this window and we are on an mX, then tell the Mac the new base point
    ;; (from BASE-X and BASE-Y instance variables), tell it whether or not to draw an arrowhead and if so
    ;; in what direction (from ARROWHEAD-DIRECTION instance variable), and turn on Mac-based
     ;; rubberbanding...
    (UNLESS mouse-in-me
      (SETF old-blinker tv:mouse-blinker)
      (SEND old-blinker :set-visibility :off)
      (WHEN (mac-system-p)
	(SEND *mac* :set-mouse-blinker (- 40 16))
	(WHEN (AND (ZEROP rubberbanding-suppression-count) (SEND self :exposed-p))
	  (SEND *mac* :open-rubber-band base-x base-y arrowhead-direction line-type self))))
    
    ;;  Set MOUSE-IN-ME to T iff mouse is within this window as of this call...
    (SETF mouse-in-me (AND (< -1 new-x (tv:sheet-width self))
			   (< -1 new-y (tv:sheet-height self))
			   ;; (NOT (ZEROP tv:mouse-speed))
			   ))
    (mac:remember-call :lars2 self old-mouse-in-me mouse-in-me rubberbanding-suppression-count
		       frb-window (SEND self :exposed-p) tv:mouse-speed)
    ;;  If the mouse is now leaving this window and we are on an mX, turn off Mac-based rubberbanding.
    ;;  Also tell the Mac whether or not to erase the last rubberband line drawn (from the
    ;;  ERASE-RUBBERBAND-ON-EXIT instance variable)...
    (WHEN (AND (tv:mac-system-p) (NOT mouse-in-me))
      (WHEN (AND (ZEROP rubberbanding-suppression-count) (SEND self :exposed-p))
	(SEND *mac* :close-rubber-band)))
    
    ;;  Implement Explorer-based rubberbanding if not on the Mac...
    (UNLESS (mac-system-p)
      (MULTIPLE-VALUE-BIND (origin-x origin-y)
	  (tv:sheet-calculate-offsets self nil)
	(UNLESS mouse-in-me
	  (LET ((sector-code (w:sector-code new-x new-y
					    (tv:sheet-left-margin-size self)
					    (tv:sheet-top-margin-size self)
					    (tv:sheet-inside-right self)
					    (tv:sheet-inside-bottom self))))
	    (COND-EVERY
	      ((ZEROP (LOGXOR 8 sector-code)) (SETF new-y (tv:sheet-top-margin-size self)))
	      ((ZEROP (LOGXOR 4 sector-code)) (SETF new-y (tv:sheet-inside-bottom self)))
	      ((ZEROP (LOGXOR 2 sector-code)) (SETF new-x (tv:sheet-inside-right self)))
	      ((ZEROP (LOGXOR 1 sector-code)) (SETF new-x (tv:sheet-left-margin-size self))))))
	(WHEN (AND (< -1 old-x) (NOT mouse-in-me) (PLUSP erase-rubberband-on-exit))
	  (draw-rubber-banded-line self -1000 -1000 -1000 -1000 7 14 arrowhead-direction))
	(PROGN (SETF old-x new-x
		     old-y new-y)
	       (draw-rubber-banded-line self (- base-x origin-x) (- base-y origin-y) old-x
					old-y 7 14 arrowhead-direction))
	;; else...
	(IF (AND (NOT mouse-in-me) (PLUSP erase-rubberband-on-exit))
	    (SETF old-x -1
		  old-y -1))))
    
    (UNLESS mouse-in-me
      (SEND old-blinker :set-visibility :on)
      (tv:mouse-set-blinker old-blinker)))
  
  )

(DEFUN suppress-mac-based-rubberbanding (window)
  (LET (mouse-in-me base-x base-y rubberbanding-suppression-count)
    (WHEN (AND (mac-window-p window)
	       (SETF rubberbanding-suppression-count (SEND window :send-if-handles :rubberbanding-suppression-count)))
      (SETF mouse-in-me (SEND window :mouse-in-me))
      (SETF base-x (SEND window :base-x))
      (SETF base-y (SEND window :base-y))
      (WHEN (AND  ;mouse-in-me
		 (INTEGERP base-x) (< -1 base-x)
		 (INTEGERP base-y) (< -1 base-y)
		 (OR (sheet-lock window) (NOT (SEND window :exposed-p)))
		 (ZEROP rubberbanding-suppression-count))
	  (SEND *mac* :close-rubber-band)
	  (SETF (SEND window :rubberbanding-suppression-count) 1))
      (mac:remember-call :lars2 window base-x base-y  rubberbanding-suppression-count
			 (SEND window :rubberbanding-suppression-count)
			 (sheet-lock window) (SEND window :exposed-p)))))

(DEFUN unsuppress-mac-based-rubberbanding (window)
  (LET (mouse-in-me base-x base-y rubberbanding-suppression-count)
    (WHEN (AND (mac-window-p window)
	       (SETF rubberbanding-suppression-count (SEND window :send-if-handles :rubberbanding-suppression-count)))
      (SETF mouse-in-me (SEND window :mouse-in-me))
      (SETF base-x (SEND window :base-x))
      (SETF base-y (SEND window :base-y))
      (WHEN (< 0 rubberbanding-suppression-count)
	(WHEN (AND  ;mouse-in-me
		   (INTEGERP base-x) (< -1 base-x)
		   (INTEGERP base-y) (< -1 base-y)
		   (= 1 rubberbanding-suppression-count)
		   (AND (NULL (sheet-lock window)) (SEND window :exposed-p)))
	  (SEND *mac* :open-rubber-band base-x base-y
		(SEND window :arrowhead-direction)
		(SEND window :line-type) window)
	  (SETF (SEND window :rubberbanding-suppression-count) 0)))
      (mac:remember-call :lars32 window base-x base-y  rubberbanding-suppression-count
			 (SEND window :rubberbanding-suppression-count)
			 (sheet-lock window) (SEND window :exposed-p)))))

(DEFMETHOD (fast-rubberbanding-mixin :before :deexpose) (&rest ignore)
  (suppress-mac-based-rubberbanding self)
  (WHEN mouse-in-me
    (SEND old-blinker :set-visibility :on)
    (tv:mouse-set-blinker old-blinker)))

(DEFMETHOD (fast-rubberbanding-mixin :after :expose) (&rest ignore)
  (unsuppress-mac-based-rubberbanding self)
  (WHEN mouse-in-me
    (SETF old-blinker tv:mouse-blinker)
    (SEND old-blinker :set-visibility :off)))

(DEFMETHOD (fast-rubberbanding-mixin :turn-on) ()
  (unsuppress-mac-based-rubberbanding self))

(DEFMETHOD (fast-rubberbanding-mixin :turn-off) ()
  (suppress-mac-based-rubberbanding self))

(DEFMETHOD (fast-rubberbanding-mixin :define-arrow)
	   (new-base-x new-base-y new-arrowhead-direction new-line-type new-erase-rubberband-on-exit)
  (suppress-mac-based-rubberbanding self)
  (SETF base-x new-base-x
	base-y new-base-y
	arrowhead-direction new-arrowhead-direction
	line-type new-line-type
	erase-rubberband-on-exit new-erase-rubberband-on-exit)
  (unsuppress-mac-based-rubberbanding self))



;;;
;;;	Window system internal functions modified to support fast rubberbanding...
;;;

(DEFUN SHEET-GET-LOCK (SHEET &OPTIONAL (UNIQUE-ID CURRENT-PROCESS))
  "Lock SHEET's lock, waiting if necessary.
The locks of SHEET's inferiors are locked also."
  (DO ((INHIBIT-SCHEDULING-FLAG T T))
      (())
    (COND ((SHEET-CAN-GET-LOCK SHEET UNIQUE-ID)
	   (RETURN (PROG1 (SHEET-GET-LOCK-INTERNAL SHEET UNIQUE-ID)
			  (suppress-mac-based-rubberbanding sheet))))
	  (T
	   (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	   (PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SHEET UNIQUE-ID)))))

(DEFUN SHEET-RELEASE-LOCK
       (SHEET &OPTIONAL (UNIQUE-ID CURRENT-PROCESS)
        &AUX (INHIBIT-SCHEDULING-FLAG T) LOCK)
  "Release a lock on a sheet and its inferiors"
  (COND ((OR (EQ UNIQUE-ID (SETQ LOCK (SHEET-LOCK SHEET)))
	     (AND LOCK (NOT (ZEROP (SHEET-LOCK-COUNT SHEET)))))
	 ;; If we own the lock, or if temp locked and the lock count is
         ;; non-zero, then we must decrement the lock count.
	 (SETF (SHEET-LOCK-COUNT SHEET) (1- (SHEET-LOCK-COUNT SHEET)))	 
	 (WHEN (AND (ZEROP (SHEET-LOCK-COUNT SHEET))
		    (NOT (CONSP LOCK)))
	   ;; If the count is currently zero, and the sheet is not
	   ;; temp-locked, then clear out the lock cell.
	   (PROG1 (SETF (SHEET-LOCK SHEET) NIL)
		  (unsuppress-mac-based-rubberbanding sheet)) )
	 (DOLIST (INFERIOR (SHEET-INFERIORS SHEET))
	   (SHEET-RELEASE-LOCK INFERIOR UNIQUE-ID)))))

(DEFUN SHEET-GET-TEMPORARY-LOCK (SHEET REQUESTOR)
  "Get a temporary lock on SHEET.
REQUESTOR is the temporary sheet that is going to cover SHEET."
  (DO ((INHIBIT-SCHEDULING-FLAG T T))
      ((SHEET-CAN-GET-TEMPORARY-LOCK SHEET REQUESTOR)
       ;; Make sure we lock in appropriate fashion (i.e. if the window
       ;; is already temp locked add another locker, else start the
       ;; list).  We don't have to worry about plus states, since
       ;; SHEET-CAN-GET-TEMPORARY-LOCK already worried for us.
       (LET ((LOCK (SHEET-LOCK SHEET)))
	 (PROG1 (SETF (SHEET-LOCK SHEET)
		      (IF (CONSP LOCK)
			  (CONS REQUESTOR LOCK)
			(CONS REQUESTOR nil)))
		(suppress-mac-based-rubberbanding sheet))))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (PROCESS-WAIT "Window Lock"
                  #'SHEET-CAN-GET-TEMPORARY-LOCK SHEET REQUESTOR)))

(DEFUN SHEET-RELEASE-TEMPORARY-LOCK
       (SHEET REQUESTOR &AUX (INHIBIT-SCHEDULING-FLAG T))
  "Release a temporary lock on a sheet."
  (LET ((LOCK (DELETE REQUESTOR (THE LIST (SHEET-LOCK SHEET)) :TEST #'EQ)))
    (PROG1 (SETF (SHEET-LOCK SHEET)
		 (OR LOCK (IF (ZEROP (SHEET-LOCK-COUNT SHEET))
			      NIL
			    (SHEET-FIND-LOCKER SHEET))))
	     (unsuppress-mac-based-rubberbanding sheet))))


;;;
;;;	The Explorer-based support for drawing a rubberbanded mouse cursor...
;;;

(DEFUN draw-rubber-banded-line (window x0 y0 x1 y1 h m arrowhead-direc)
  (DECLARE (self-flavor fast-rubberbanding-mixin))
  (IF (EQ arrowhead-direc :none)
      (SEND window :draw-line x0 y0 x1 y1 1 w:black w:alu-xor)
       ;;else...
    (WHEN (EQ :from arrowhead-direc)
      (PSETQ x0 x1 x1 x0 y0 y1 y1 y0))
    (IF (SEND window :send-if-handles :fast-rubberbanding-enabled)
	(SEND tv:*mac* :draw-rubberband-line window x0 y0 x1 y1
	      (ECASE arrowhead-direc (:none 0) (:from -1) (:to 1)))
      ;;else...
      (IF (< x0 0)
	  (PROGN 
	    (SEND window :draw-polyline *old-x-coords* *old-y-coords* 1 w:black 9 w:alu-xor nil)
	    (SETF (AREF *old-x-coords* 0) -1000))
	;; else...
	(UNLESS (= (AREF *old-x-coords* 0) -1000)
	    (SEND window :draw-polyline *old-x-coords* *old-y-coords* 1 w:black 9 w:alu-xor nil))
	(LET* ((x-dist (- x1 x0)) (y-dist (- y1 y0))
	       (length-of-full-line (SQRT (+ (* x-dist x-dist) (* y-dist y-dist)))))
	  (UNLESS (ZEROP length-of-full-line)
	    (LET* ((sin-theta (/ y-dist length-of-full-line))
		   (cos-theta (/ x-dist length-of-full-line))
		   (m-sin-theta (ROUND (* m sin-theta)))
		   (m-cos-theta (ROUND (* m cos-theta)))
		   (h-sin-theta (ROUND (* h sin-theta)))
		   (h-cos-theta (ROUND (* h cos-theta))))
	      (WITH-STACK-LIST (x-coord-list
				 (+ x0 m-cos-theta)
				 (+ x0 h-sin-theta)
				 (- x0 h-sin-theta)
				 (+ x0 m-cos-theta)
				 (- x1 m-cos-theta)
				 (- x1 m-cos-theta h-sin-theta)
				 x1
				 (- x1 (- m-cos-theta h-sin-theta))
				 (- x1 m-cos-theta))
		(LOOP for i from 0
		      for value in x-coord-list
		      do
		      (SETF (AREF *x-coords* i) value)))
	      (WITH-STACK-LIST (y-coord-list
				 (+ y0 m-sin-theta)
				 (- y0 h-cos-theta)
				 (+ y0 h-cos-theta)
				 (+ y0 m-sin-theta)
				 (- y1 m-sin-theta)
				 (- y1 (- m-sin-theta h-cos-theta))
				 y1
				 (- y1 m-sin-theta h-cos-theta)
				 (- y1 m-sin-theta))
		(LOOP for i from 0
		      for value in y-coord-list
		      do
		      (SETF (AREF *y-coords* i) value)))
	      (SEND window :draw-polyline *x-coords* *y-coords* 1 w:black 9 w:alu-xor nil)
	      (COPY-ARRAY-CONTENTS *x-coords* *old-x-coords*)
	      (COPY-ARRAY-CONTENTS *y-coords* *old-y-coords*)
	      )))))))