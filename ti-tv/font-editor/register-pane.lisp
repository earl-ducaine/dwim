;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


;;; REGISTER-PANE is a kind of basic-fed that only displays its contents
;;; and passes mouse-clicks along to the FED-WINDOW itself.

;;; CHANGE HISTORY

;;;  6/22/87  DKM  - Change :who-line-documentation-string to show correct button for system menu

(DEFMETHOD (REGISTER-PANE :WHO-LINE-DOCUMENTATION-STRING) ()
  '(:MOUSE-L-1 "Copy to register from drawing pane" 
    :MOUSE-M-1 "Copy to drawing pane from register" 
    :MOUSE-R-1 "Menu of register operations"
    :MOUSE-R-2 "System Menu"))

(DEFMETHOD (REGISTER-PANE :AFTER :INIT) (&REST IGNORE)
   "Define a new cursor for use in the regsiter panes."
  (SETQ register-pane-cursor (MAKE-INSTANCE 'FED:cursor
					  :cursor-character  99.	 ;a large dot.
					  :cursor-font       fonts:mouse
					  :cursor-visibility nil
					  :cursor-window     self
					  :cursor-x-offset   7.
					  :cursor-y-offset   7.)))

(DEFMETHOD (REGISTER-PANE :MOUSE-STANDARD-BLINKER) ()
  "Sets the mouse cursor for this window.  It is automatically
called by the window manager when the mouse enters the screen
area occupied by this window."
   (LET (ch mfont x-off y-off)
     (COND (register-pane-cursor
	    (MULTIPLE-VALUE-SETQ (ch mfont) (SEND register-pane-cursor :character))
	    (MULTIPLE-VALUE-SETQ (x-off y-off) (SEND register-pane-cursor :offset)))
	   (t (SETQ ch  99.
		    mfont fonts:mouse
		    x-off 7.
		    y-off 7.)))
     ;; Make sure that this window and all of its superiors are visible before we
     ;; change the mouse blinker definition.
     (DO ((mwindow self (tv:sheet-superior mwindow)))
	 ((NULL mwindow)
	  (tv:mouse-set-blinker-definition :character
					   x-off
					   y-off
					   :on :set-character ch mfont))
       (COND ((NOT (tv:sheet-exposed-p mwindow)) (RETURN NIL))))))


(DEFMETHOD (REGISTER-PANE :AFTER :REFRESH) (&REST IGNORE)
  (OR TV:RESTORED-BITS-P INHIBIT-CHAR-BOX
     (PROGN
       (SEND SELF :MUST-REDISPLAY REDISPLAY-ALL)
       (SEND SELF :REDISPLAY T)))) 


(DEFMETHOD (REGISTER-PANE :BEFORE :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (SEND SELF :SET-BOX-SIZE (SEND TV:SUPERIOR :REGISTER-BOX-SIZE))) 


(DEFMETHOD (REGISTER-PANE :AFTER :ERASE-ALL) ()
  (SETQ INHIBIT-CHAR-BOX T)) 


(DEFMETHOD (REGISTER-PANE :MOUSE-CLICK) (BUTTON X Y)
  X
  Y
  (AND (= BUTTON #\MOUSE-L)
       (NEQ SELF TV:SELECTED-WINDOW)
       (TV:MOUSE-SELECT TV:SUPERIOR))
  (SEND SELF :FORCE-KBD-INPUT `(:TYPEOUT-EXECUTE :REGISTER-CLICK ,SELF ,BUTTON))
  T) 


(DEFMETHOD (REGISTER-PANE :AFTER :MERGE-CONTENTS) (&REST IGNORE)
  (SETQ INHIBIT-CHAR-BOX ())
  (SEND SELF :REDISPLAY T)) 


(DEFMETHOD (REGISTER-PANE :AFTER :SET-CONTENTS) (&REST IGNORE)
  (SETQ INHIBIT-CHAR-BOX nil)
  (SEND SELF :REDISPLAY T)) 
