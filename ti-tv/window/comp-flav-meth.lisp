;; -*- Mode:Common-Lisp; Package:TV; Fonts:(CPTFONT HL12B HL12BI); Base:10. -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 8/24/87  KWW        Added control-register and block-flavor
;;; 11/03/86 TWE	Added in some font flavors at the end.


#|

This file contains compile flavor methods for all of the instantiable window
system flavors, and is near the end of the defsystem.  These flavors are
ordered by the window system hierarchy, with the lowest level flavor being the
first.

|#


(COMPILE-FLAVOR-METHODS SHEET
                        SCREEN
                        STANDARD-SCREEN
                        RECTANGULAR-BLINKER
                        CHARACTER-BLINKER
                        IBEAM-BLINKER
                        BOX-BLINKER
                        HOLLOW-RECTANGULAR-BLINKER
                        BITBLT-BLINKER
                        MAGNIFYING-BLINKER
                        REVERSE-CHARACTER-BLINKER
                        MOUSE-CHARACTER-BLINKER
                        MOUSE-RECTANGULAR-BLINKER
                        MOUSE-HOLLOW-RECTANGULAR-BLINKER
                        TEMPORARY-MULTIPLE-CHOICE-WINDOW
                        CHOOSE-VARIABLE-VALUES-WINDOW
                        TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW
                        CHOOSE-VARIABLE-VALUES-PANE
                        WHO-LINE-SCREEN
                        WHO-LINE-SHEET
                        WHO-LINE-FILE-SHEET
                        WHO-LINE-WINDOW
                        LISP-INTERACTOR
                        BACKGROUND-LISP-INTERACTOR
			WINDOW
			w:window
			SIMPLE-LISP-LISTENER
                        POP-UP-TEXT-WINDOW
                        POP-UP-NOTIFICATION-WINDOW
                        TRUNCATING-POP-UP-TEXT-WINDOW
                        TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET
                        BASIC-FRAME
                        CONSTRAINT-FRAME-NO-FORWARDING
                        CONSTRAINT-FRAME
                        BORDERED-CONSTRAINT-FRAME
                        CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER
                        BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER
                        W:MENU
                        MENU
                        TEMPORARY-MENU
                        COMMAND-MENU
                        MOMENTARY-MENU
                        MARGIN-CHOICE-MENU
                        MULTIPLE-MENU
                        MOMENTARY-MULTIPLE-MENU
                        MOMENTARY-WINDOW-HACKING-MENU
                        DYNAMIC-MOMENTARY-MENU
                        DYNAMIC-MOMENTARY-WINDOW-HACKING-MENU
                        DYNAMIC-TEMPORARY-MENU
                        DYNAMIC-TEMPORARY-COMMAND-MENU
                        DYNAMIC-TEMPORARY-ABORT-ON-DEEXPOSE-COMMAND-MENU
                        DYNAMIC-MULTICOLUMN-MOMENTARY-MENU
                        DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU
                        SEQUENCE-SELECTION-OUTPUT-WINDOW
                        SEQUENCE-SELECTION-MULTIPLE-MENU
                        CONFIRMATION-WINDOW
                        TRACE-OR-ERROR-WINDOW
                        DISPLAY-LAYOUT-WINDOW
                        TYPEOUT-WINDOW
                        TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS
                        W:CACHE-WINDOW
			w:sprite-window
                        W:FONT
                        W:RASTER-CHARACTER
                        W:VECTOR-CHARACTER
			w:pop-up-prompt-and-read-window
;;; Some strictly color related flavors
			 control-register-flavor
			 block-flavor)


;;; Reason: Define (:method tv:sheet :after :init) and (:method tv:borders-mixin :after :init)
;;; These were defined in release 3.2 but were not needed in 4.x.  To remain object
;;; compatible with 3.2 they are added in a special way to not impact subsequently
;;; compiled combined methods.   PMH 3/10/88
;;; This hack can be deleted after the next release when we require customers to recompile
(flet ((fix-missing-deamon-methods ()
	  (bind (locf #'sys:recompile-flavor) #'ignore)
	  (deff (:method tv:sheet :after :init) #'ignore)
	  (deff (:method tv:borders-mixin :after :init) #'ignore)
       ))
     (fix-missing-deamon-methods))