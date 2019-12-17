;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; CHANGE HISTORY

;;;  6/22/87  DKM  - Add :who-line-documentation-string method to give correct system-menu mouse doc

;;; This is a new method to add, just so that it says the right thing in the who line
;;; (otherwise says R1 to get system menu which isn't true.

(DEFMETHOD (FED-LABEL-WINDOW  :WHO-LINE-DOCUMENTATION-STRING) ()
  '(:mouse-r-2 "System Menu"))

(DEFMETHOD (FED-LABEL-WINDOW :AFTER :INIT) (&REST IGNORE)
   (SETQ fed-label-window-cursor (MAKE-INSTANCE 'FED:cursor
						:cursor-character  7.	 ;menu-select character
						:cursor-font       fonts:mouse
						:cursor-visibility nil
						:cursor-window     self
						:cursor-x-offset   4.
						:cursor-y-offset   5.)))

(DEFMETHOD (FED-LABEL-WINDOW :MOUSE-STANDARD-BLINKER) ()
   "Sets the mouse cursor for this window.  It is automatically
called by the window manager when the mouse enters the screen
area occupied by this window."
   (LET (ch mfont x-off y-off)
     (COND (fed-label-window-cursor
	    (MULTIPLE-VALUE-SETQ (ch mfont) (SEND fed-label-window-cursor :character))
	    (MULTIPLE-VALUE-SETQ (x-off y-off) (SEND fed-label-window-cursor :offset)))
	   (t (SETQ ch  7.
		    mfont fonts:mouse
		    x-off 4.
		    y-off 5.)))
     ;; Make sure that this window and all of its superiors are visible before we
     ;; change the mouse blinker definition.
     (DO ((mwindow self (tv:sheet-superior mwindow)))
	 ((NULL mwindow)
	  (tv:mouse-set-blinker-definition :character
					   x-off
					   y-off
					   :on :set-character ch mfont))
       (COND ((NOT (tv:sheet-exposed-p mwindow)) (RETURN NIL))))))

(DEFMETHOD (FED-LABEL-WINDOW :SECOND-LINE-HEIGHT) ()
  (MAX TV:LINE-HEIGHT
       (LET ((FONT (AND FED-WINDOW (SEND FED-WINDOW :SELECTED-FONT))))
	 (IF FONT
	     (FD-LINE-SPACING (FONT-GET-FD FONT))
	   0))))

;;; Increase the number lines added (10 to 16) for second line height to prevent
;;; window from being overwritten by sample string.
(DEFMETHOD (FED-LABEL-WINDOW :LABEL-WINDOW-SIZE) (&REST IGNORE)
  (SETF (TV:SHEET-LINE-HEIGHT SELF)
	(TV:FONT-CHAR-HEIGHT (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN SELF))))
  (+ 32. TV:LINE-HEIGHT (SEND SELF :SECOND-LINE-HEIGHT)))





