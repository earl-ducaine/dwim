;;; -*- Mode:Common-Lisp; Package:TV; Base:10; Fonts:(CPTFONT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983- 1989 Texas Instruments Incorporated.  All rights reserved.
;;; ** (c) Copyright 1980 by Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  05/09/89  MAY     removed "mac:" remember-calls for source build - mac pkg not defined yet.
;;;  05/08/89   LG	Created.


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
	    (rubberbanding-suppression-count 2)	       ; Start out forcibly suppressed.  Somebody must call
						       ;    (unsuppress-mac-based-rubberbanding window t) to
						       ;    make us go.
	    )
	   ()
  (:inittable-instance-variables)

  (:settable-instance-variables mouse-in-me old-blinker
    base-x base-y arrowhead-direction erase-rubberband-on-exit old-x old-y rubberbanding-suppression-count line-type))


(DEFMETHOD (fast-rubberbanding-mixin :after :mouse-moves)
	   (new-x new-y &aux (old-mouse-in-me mouse-in-me)
	    (frb-window (window-under-mouse :rubberbanding-suppression-count)))
  
  (IF (NOT (mac-system-p))
      (ERROR "The methods of the FAST-RUBBERBANDING-MIXIN are microExplorer-specific.")
    ;; else...
    
    ;;  Remember: NEW-X and NEW-Y are relative to SELF, not relative to the mouse sheet...
    (WHEN (AND (INTEGERP base-x) (< -1 base-x)
	       (INTEGERP base-y) (< -1 base-y))
      ;; If the mouse just entered this window and we are on an mX, then turn off the Mac's mouse cursor
      (UNLESS mouse-in-me
	(SETF old-blinker tv:mouse-blinker)
	(SEND old-blinker :set-visibility :off)
	(WHEN (mac-system-p)
	  (SEND *mac* :set-mouse-blinker (- 40 16))
	  ))
      
      ;;  Set MOUSE-IN-ME to T iff mouse is within this window as of this call...
      (SETF mouse-in-me (AND (< -1 new-x (tv:sheet-width self))
			     (< -1 new-y (tv:sheet-height self))
			     ;; (NOT (ZEROP tv:mouse-speed))
			     ))
;      (mac:remember-call :lars2 self old-mouse-in-me mouse-in-me rubberbanding-suppression-count
;			 frb-window (SEND self :exposed-p) tv:mouse-speed)
      ;;  If the mouse is now leaving this window and we are on an mX and we're supposed to erased the rubber
      ;;  band on exit, then turn off Mac-based rubberbanding and turn it back on.  Because we are outside the
      ;;  clipping region given the Mac, the rubberband will not reappear until the mouse reenters this clipping
      ;;  region.  This just erases the rubberband from the screen.
      (WHEN (AND (tv:mac-system-p) (NOT mouse-in-me))
	(WHEN (AND (ZEROP rubberbanding-suppression-count)
		   (SEND self :exposed-p)
		   (= 1 erase-rubberband-on-exit))
	  (SEND *mac* :close-rubber-band)
	  (SEND *mac* :open-rubber-band base-x base-y arrowhead-direction line-type self))
	)
      
;;;      ;;  Implement Explorer-based rubberbanding if not on the Mac...
;;;      (UNLESS (mac-system-p)
;;;	(MULTIPLE-VALUE-BIND (origin-x origin-y)
;;;	    (tv:sheet-calculate-offsets self nil)
;;;	  (UNLESS mouse-in-me
;;;	    (LET ((sector-code (w:sector-code new-x new-y
;;;					      (tv:sheet-left-margin-size self)
;;;					      (tv:sheet-top-margin-size self)
;;;					      (tv:sheet-inside-right self)
;;;					      (tv:sheet-inside-bottom self))))
;;;	      (COND-EVERY
;;;		((ZEROP (LOGXOR 8 sector-code)) (SETF new-y (tv:sheet-top-margin-size self)))
;;;		((ZEROP (LOGXOR 4 sector-code)) (SETF new-y (tv:sheet-inside-bottom self)))
;;;		((ZEROP (LOGXOR 2 sector-code)) (SETF new-x (tv:sheet-inside-right self)))
;;;		((ZEROP (LOGXOR 1 sector-code)) (SETF new-x (tv:sheet-left-margin-size self))))))
;;;	  (WHEN (AND (< -1 old-x) (NOT mouse-in-me) (PLUSP erase-rubberband-on-exit))
;;;	    (draw-rubber-banded-line self -1000 -1000 -1000 -1000 7 14 arrowhead-direction))
;;;	  (PROGN (SETF old-x new-x
;;;		       old-y new-y)
;;;		 (draw-rubber-banded-line self (- base-x origin-x) (- base-y origin-y) old-x
;;;					  old-y 7 14 arrowhead-direction))
;;;	  ;; else...
;;;	  (IF (AND (NOT mouse-in-me) (PLUSP erase-rubberband-on-exit))
;;;	      (SETF old-x -1
;;;		    old-y -1))))
      
      (UNLESS mouse-in-me
	(SEND old-blinker :set-visibility :on)
	(tv:mouse-set-blinker old-blinker))))
  
  )


(DEFUN suppress-mac-based-rubberbanding (window &optional regardless-p)
  (IF (NOT (mac-system-p))
      (ERROR "The function SUPPRESS-MAC-BASED-RUBBERBANDING is microExplorer-specific.")
    ;; else...
    (LET (base-x base-y rubberbanding-suppression-count old-blinker)
      (WHEN (AND (mac-window-p window)
		 (SETF rubberbanding-suppression-count (SEND window :send-if-handles :rubberbanding-suppression-count)))
	(CASE rubberbanding-suppression-count
	  (0 (SETF base-x (SEND window :base-x))
	     (SETF base-y (SEND window :base-y))
	     (WHEN (OR regardless-p
		       (AND (INTEGERP base-x) (< -1 base-x)
			    (INTEGERP base-y) (< -1 base-y)
			    (am-I-or-any-of-my-superiors-deexposed-or-locked? window)))
	       (SEND *mac* :close-rubber-band)
	       (WHEN (AND (SETF old-blinker (SEND window :old-blinker))
			  (SEND window :mouse-in-me))
		 (SETF (SEND window :old-blinker)  nil)
		 ;;  In case the blinker's sheet is output-held, do this in a separate process...
		 (PROCESS-RUN-FUNCTION "Restore Normal Blinker"
				       #'(lambda (blinker)
					   (SEND blinker :set-visibility :on)
					   (tv:mouse-set-blinker blinker))
				       old-blinker))
	       )
	     (SETF (SEND window :rubberbanding-suppression-count) (IF regardless-p 2 1)))
	  (1 (WHEN regardless-p (SETF (SEND window :rubberbanding-suppression-count) 2))))
	(SETF (SEND window :mouse-in-me) nil)
;	(mac:remember-call :lars32 window base-x base-y  rubberbanding-suppression-count
;			   (SEND window :rubberbanding-suppression-count)
;			   (am-I-or-any-of-my-superiors-deexposed-or-locked? window))
	))))

(DEFUN unsuppress-mac-based-rubberbanding (window &optional regardless-p)
  (IF (NOT (mac-system-p))
      (ERROR "The function UNSUPPRESS-MAC-BASED-RUBBERBANDING is microExplorer-specific.")
    ;; else...
    (LET (mouse-in-me base-x base-y rubberbanding-suppression-count)
      (WHEN (AND (mac-window-p window)
		 (SETF rubberbanding-suppression-count (SEND window :send-if-handles :rubberbanding-suppression-count)))
	(SETF mouse-in-me (SEND window :mouse-in-me))
	(SETF base-x (SEND window :base-x))
	(SETF base-y (SEND window :base-y))
;	(mac:remember-call :lars32 window base-x base-y  rubberbanding-suppression-count
;			   (SEND window :rubberbanding-suppression-count)
;			   (am-I-or-any-of-my-superiors-deexposed-or-locked? window))
	(WHEN (< 0 rubberbanding-suppression-count)
	  (WHEN regardless-p
	    (SETF rubberbanding-suppression-count (SETF (SEND window :rubberbanding-suppression-count) 1)))
	  (WHEN (AND (INTEGERP base-x) (< -1 base-x)
		     (INTEGERP base-y) (< -1 base-y)
		     (= 1 rubberbanding-suppression-count)
		     (NOT (am-I-or-any-of-my-superiors-deexposed-or-locked? window)))
	    (SETF (SEND window :rubberbanding-suppression-count) 0)
	    (SEND *mac* :open-rubber-band base-x base-y
		  (SEND window :arrowhead-direction) (SEND window :line-type) window)
	    (WHEN mouse-in-me 
	      (UNLESS (SEND window :old-blinker)
		(SETF (SEND window :old-blinker) tv:mouse-blinker)
		(SEND tv:mouse-blinker :set-visibility :off)
		(WHEN (mac-system-p)
		  (SEND *mac* :set-mouse-blinker (- 40 16)))))))))))

(DEFMETHOD (fast-rubberbanding-mixin :before :deexpose) (&rest ignore)
  (suppress-mac-based-rubberbanding self))

(DEFMETHOD (fast-rubberbanding-mixin :after :expose) (&rest ignore)
  (unsuppress-mac-based-rubberbanding self))

(DEFMETHOD (fast-rubberbanding-mixin :turn-on) (&optional regardless-p)
  (unsuppress-mac-based-rubberbanding self regardless-p))

(DEFMETHOD (fast-rubberbanding-mixin :turn-off) (&optional regardless-p)
  (suppress-mac-based-rubberbanding self regardless-p))

(DEFMETHOD (fast-rubberbanding-mixin :define-arrow)
	   (new-base-x new-base-y new-arrowhead-direction new-line-type new-erase-rubberband-on-exit)
  (suppress-mac-based-rubberbanding self t)
  (SETF base-x new-base-x
	base-y new-base-y
	arrowhead-direction new-arrowhead-direction
	line-type new-line-type
	erase-rubberband-on-exit new-erase-rubberband-on-exit)
  (unsuppress-mac-based-rubberbanding self t))

(DEFUN am-I-or-any-of-my-superiors-deexposed-or-locked? (self)
  (LOOP for window first self then (sheet-superior window)
	until (NULL window)
	when (OR (sheet-lock window)
		 (NOT (sheet-exposed-p window)))
	return window))
