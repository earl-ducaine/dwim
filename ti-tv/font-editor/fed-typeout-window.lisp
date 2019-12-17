;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; CHANGE HISTORY

;;;  2/06/87 DKM - removed methods :before :item, and :after :item.  Not needed.
;;;  2/06/87 DKM - changed :before :expose-for-typeout to send SELF that message
;;;                       instead of *terminal-io* (sometimes erased the wrong window).
;;;  6/22/87 DKM - change :more-exception method to not stuff space, rubout, or return back on the stream
;;;                    - add :who-line-documentation-string method to correctly document system menu click in who line

;;; A new method to put correct mouse doc in who line

(DEFMETHOD (FED-TYPEOUT-WINDOW :WHO-LINE-DOCUMENTATION-STRING) ()
   '(:MOUSE-R-2 "System Menu"))


;;; Create a menu-select type cursor for the typeout window
(DEFMETHOD (FED-TYPEOUT-WINDOW :AFTER :INIT) (&REST IGNORE)
  (SETQ fed-typeout-cursor (MAKE-INSTANCE 'FED:cursor
					  :cursor-character  7. ;menu-select character
					  :cursor-font       fonts:mouse
					  :cursor-visibility nil
					  :cursor-window     self
					  :cursor-x-offset   4.
					  :cursor-y-offset   5.)))

(DEFMETHOD (FED-TYPEOUT-WINDOW :MOUSE-STANDARD-BLINKER) ()
  "Sets the mouse cursor for this window.  It is automatically
called by the window manager when the mouse enters the screen
area occupied by this window."
  (LET (ch mfont x-off y-off)
    (COND (fed-typeout-cursor
	    (MULTIPLE-VALUE-SETQ (ch mfont) (SEND fed-typeout-cursor :character))
	    (MULTIPLE-VALUE-SETQ (x-off y-off) (SEND fed-typeout-cursor :offset)))
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

(DEFMETHOD (FED-TYPEOUT-WINDOW :MORE-EXCEPTION) (&AUX CH)
  (COND ((NOT (ZEROP (TV:SHEET-MORE-FLAG)))
	 (SETQ CH (TV:SHEET-MORE-HANDLER :MORE-TYI)) 
	 (COND ((OR (consp ch)
		    (NOT (MEMBER CH '(#\RUBOUT #\CR #\SPACE) :test #'equal))) ;don't put these back in the io-buffer
		(FUNCALL SELF :UNREAD-ANY CH))))))


(DEFMETHOD (FED-TYPEOUT-WINDOW :MORE-TYI) ()
  (DO ((CH)) (NIL)
    (AND (OR (CHARACTERP (SETQ CH (SEND SELF :READ-ANY)))
	     (AND (CONSP CH) (EQ (FIRST CH) :TYPEOUT-EXECUTE)))
	 (RETURN CH))))

; These guys aren't necessary anymore.  dkm  2/6/87
;(DEFMETHOD (FED-TYPEOUT-WINDOW :BEFORE :ITEM) (&REST IGNORE)  
;   (DECLARE (SPECIAL typeout-pane))
;   (LET ((*terminal-io* typeout-pane))
;     (PRINC " " typeout-pane)))

;(DEFMETHOD (FED-TYPEOUT-WINDOW :AFTER :ITEM) (&REST IGNORE)
;   (DECLARE (SPECIAL typeout-pane))
;   (LET ((*terminal-io* typeout-pane))
;  ;   (PRINC "      " typeout-pane)
;     ))

(DEFMETHOD (FED-TYPEOUT-WINDOW :AFTER :EXPOSE-FOR-TYPEOUT) (&REST IGNORE)
     (SEND self :clear-screen))



