;;; -*- Mode:Common-Lisp; Package:MAC-WINDOWS; Base:10; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB) -*-


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
;;; Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.

;;;--------------------------------------------------------------------
;;;                 Patch
;;;   Date  Author  Number   Description
;;;--------------------------------------------------------------------
;;;  05/10/88    LG      4-51   Add a word to the ACB for GetScreenSize so's a flag can be
;;;  			  returned to indicate whether or not automatic screen resizing
;;;  			  should be performed at boot time.

(DEFCONSTANT parm1 0 "Index into command block of the first parameter")
(DEFCONSTANT parm2 1 "Index into command block of the second parameter")
(DEFCONSTANT parm3 2 "Index into command block of the third parameter")
(DEFCONSTANT parm4 3 "Index into command block of the fourth parameter")
(DEFCONSTANT parm5 4 "Index into command block of the fifth parameter")
(DEFCONSTANT parm6 5 "Index into command block of the sixth parameter")
(DEFCONSTANT parm7 6 "Index into command block of the seventh parameter")
(DEFCONSTANT parm8 7 "Index into command block of the eighth parameter")
(DEFCONSTANT parm9 8. "Index into command block of the ninethixth parameter")
(DEFCONSTANT parm10 9. "Index into command block of the tenth parameter")
(DEFCONSTANT parm11 10. "Index into command block of the eleventh parameter")
(DEFCONSTANT parm12 11. "Index into command block of the twelveth parameter")
(DEFCONSTANT parm13 12. "Index into command block of the thirteenth parameter")
(DEFCONSTANT parm14 13. "Index into command block of the fourtheenth parameter")
(DEFCONSTANT parm15 14. "Index into command block of the fifteenth parameter")
(DEFCONSTANT parm16 15. "Index into command block of the sixteenth parameter")



;;; Some command block definitions

(DEFCONSTANT CopyBits-parms 13	   
  "Number of parameters (excluding the array itself) passed by the CopyBits command.
Parm1 = precision of array elements.
Parm2 = alu
Parm3 = width
Parm4 = height
Parm5 = source array; 0 means LISPM, a number is the Mac window ID.
Parm6 = destination array; 0 means LISPM, a number is the Mac window ID.
Parm7 = from-x
Parm8 = from-y
Parm9 = to-x
Parm10 = to-y
parm11 = source-x
parm12 = source-y
parm13 = delay-updates")

(DEFCONSTANT DrawString-max-size 256
  "Largest number of characterrs which can be handled by DrawString.")
(DEFCONSTANT DrawString-parms 14
  "Number of parameters (excluding the string itself) passed by the DrawString command.
Parm1 = font
Parm2 = font style
Parm3 = font size
Parm4 = alu
Parm5 = start index
Parm6 = end index
Parm7 = x
Parm8 = y
Parm9 = x clipping limit
Parm10 = window")


;;;
;;; The Macintosh object.
;;;

(DEFVAR *mac* :unbound "Holds the instance of the Mac window flavor") ;-ab


(DEFFLAVOR mac-flavor
	   (
            channel			   ;display-io channel object
	    
	    ;; Instance variables which keep track of the Mac state.
	    (screen-width nil)
	    (screen-height nil)
	    
	    (current-LISPM-font nil)	   ;Current LISPM font
	    (current-Mac-font nil)
	    (current-Mac-font-style nil)
	    (current-Mac-font-size nil)
	    (current-Mac-font-baseline nil))
	   ()
  (:gettable-instance-variables screen-width screen-height)
 )

(PROCLAIM '(inline transfer-mode))
(DEFUN transfer-mode (alu)
  "Translates a LISPM ALU to a Mac transfer mode."
  (AREF *alu/transfer-mode-table* alu))


(DEFUN Mac-window-id (window)
  "Returns a Mac window ID given a window object. For now this just
passes window right through.  It is provided so that eventually a window can be specified
and a window ID can be returned which is suitable for use in a Mac command block."
  (IF (NUMBERP window) window (mac-window-p window)))


(DEFMACRO font-cache (font)
  "This macro is for use within Mac command blocks. It tests to see if font is
the currently cached font. If it is it just returns; if not it caches it then returns."
  `(UNLESS (EQ ,font current-LISPM-font)
     (WITHOUT-INTERRUPTS
       (SETF current-LISPM-font ,font
	     current-mac-font (mac-font ,font)
	     current-mac-font-style (mac-font-style ,font)
	     current-mac-font-size (mac-font-size ,font)
	     current-mac-font-baseline (tv:font-baseline ,font)))))

