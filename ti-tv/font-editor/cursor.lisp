;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; The cursor for the mouse in various FED windows.

(DEFMETHOD (cursor :offset) ()
  "Returns the offset values for this cursor object."
  (DECLARE (VALUES cursor-x-offset cursor-y-offset))
  (VALUES cursor-x-offset cursor-y-offset))

(DEFMETHOD (cursor :position) ()
  "Returns the current position of this cursor object."
  (DECLARE (VALUES x y))
  (VALUES cursor-x-position cursor-y-position))

(DEFUN point-in-extents-p (x y left top right bottom)
  "This determines if a point lies within the specified rectangular extents."
  (DECLARE (VALUES point-in-extents?))
  (AND (<= left x right) (<= top y bottom)))

(DEFMETHOD (cursor :redraw) ()
  "Displays this cursor at its current location if it is in the window."
  (DECLARE (VALUES inside-window?))
  (LET (x y)
    (SETQ x (- cursor-x-position cursor-x-offset)
          y (- cursor-y-position cursor-y-offset))
    (SEND blinker :set-visibility nil)
    (MULTIPLE-VALUE-BIND (left top right bottom) (SEND cursor-window :inside-edges)
      ;; :set-cursorpos wants x,y in terms of inside window coordinates, so subtract the margins
      (SEND blinker :set-cursorpos (- x left) (- y top))
      (COND ((point-in-extents-p x y left top right bottom)
             (SEND blinker :set-visibility cursor-visibility) t)))))

(DEFMETHOD (cursor :set-offset) (x y)
  "Changes this cursor object's offset and redisplays it if necessary."
  (DECLARE (VALUES inside-window?))
  (SETQ cursor-x-offset x
        cursor-y-offset y)
  (SEND SELF :redraw))

(DEFMETHOD (cursor :set-position) (x y)
  "Changes this cursor object's location and redisplays it if necessary."
  (DECLARE (VALUES inside-window?))
  (SETQ cursor-x-position x
        cursor-y-position y)
  (SEND SELF :redraw))

(DEFMETHOD (cursor :after :set-visibility) (new-visibility)
  "Changes this cursor object's visibility and redisplays if necessary."
  (DECLARE (VALUES inside-window?))
  new-visibility
  (SEND SELF :redraw))

(DEFMETHOD (cursor :character) ()
  "Returns the character and font which define this cursor object."
  (DECLARE (VALUES cursor-character cursor-font))
  (VALUES cursor-character cursor-font))

(DEFMETHOD (cursor :after :init) (ignore)
  "Defines a tv:blinker for the cursor and displays it if required."
  (DECLARE (VALUES inside-window?))
  (SETQ blinker (tv:make-blinker cursor-window 'tv:character-blinker
                                 :char       cursor-character
                                 :font       cursor-font
                                 :visibility nil))
  (SEND SELF :redraw))

(DEFMETHOD (cursor :set-character) (new-character new-font)
  "Changes the character and font for the cursor object."
  (SETQ cursor-character new-character  
        cursor-font      new-font)
  (SEND blinker :set-character cursor-character cursor-font))
