;;; -*- Mode:Common-Lisp; Package:MAC-WINDOWS; Base:10; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB); Patch-file:T -*-


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


;;
;; Debug support
;;

;;;   Make sure all the common debugging classes are available...
(DEFMACRO xxx () `(UNION (LIST ,@*all-debugging-classes*)
			  (LIST :fatal :drawing :keyboard :cursorpos
				:bit-arrays :selection :id-assignment :mouse)))
(SETF *all-debugging-classes* (xxx))


;;
;; Special Keys
;;

;;;   Turn on the TERM SYS X keystroke sequence for starting application X in a new screen...
(tv:add-terminal-key
  #\SYSTEM  '(tv:kbd-sys nil nil t)
  "Create a new virtual screen containing new instance of application selected by SYS X."
  :typeahead)

;; Add our definition for TERM TERM to the list of TERM x key pairs
(tv:add-terminal-key
  #\term 'toggle-mouse
  "Switches control of the mouse between the Explorer screen & the Macintosh screen"
  :keyboard-process)

(DEFUN remove-term-term ()
  (WHEN (si:mx-p)
    (tv:remove-terminal-key #\term)) )

(ADD-INITIALIZATION "Remove TERM-TERM support for MX"
		    '(remove-term-term) :warm)



;; These three predicates are redefined for the MX Window System.
;; The base system versions (in WINDOW; ADD-IN-SUPPORT) just return NIL.
;; These definitions should not be loaded into a running environment
;; until *ALL* the rest of the mac-window system is present.

(DEFVAR *mtv-debug* nil)		   ;ab 1/6

;; Next two identical to definitions in KERNEL; MX-TV-SUPPORT.
;; Remove from here after next source build.

;;(setq *mac-system-p* nil)

;;(proclaim '(notinline mac-system-p))
;;(defun mac-system-p ()
;;  "Returns true if the window system should behave as a MicroExplorer."
;;  *mac-system-p*)

;;(proclaim '(inline mac-window-p))
;;(defun mac-window-p (window)
;;  "Returns true if WINDOW was made for MicroExplorer."
;;  (AND window
;;       (NOT (ARRAYP window))
;;       (tv:sheet-window-id window)))


;; This one has different definition now...
(proclaim '(notinline mac-screen-p))
(DEFUN Mac-Screen-P (screen)
  (TYPEP screen 'mac:mac-screen))



;;
;; Top-level forms to get MX Windows started
;;

(LOAD "sys:fonts;tiny")
(setq *mac-system-p* t)			   ;; Signals MX-WINDOWS installed.
(setup-basic-comm)

;; Install Module-Op functions in support vector & turn them on.
(LET ((vector-list
	'((#o35 0)
	  (#o36 0)
	  (#o37 tv:%draw-rectangle)
	  (#o40 6.)
	  (#o41 0)
	  (#o42 0)
	  (#o43 tv:%draw-character)
	  (#o44 7.)
	  (#o45 0)      ;; tv:%scroll
	  (#o46 0)      ;; 6
          (#o47 tv:%draw-shaded-triangle)
	  (#o50 12.)
	  (#o51 tv:%draw-shaded-raster-line)
	  (#o52 7.)
	  (#o53 0)      ;; tv:%draw-string
	  (#o54 0)      ;; 8  (returns NIL)
	  (#o55 tv:mac-bitblt)
	  (#o56 9.))))
  (DOLIST (el vector-list)
    (si:%add-to-support-vector (SECOND el) (FIRST el))))

(setf si:module-op-override 1)

(w:set-scrolling-style :MAC)

;;;  Jam the set of all debugging classes...
(SETF mac:*all-debugging-classes*
      '(:fatal :drawing :keyboard :cursorpos :bit-arrays :selection :id-assignment :mouse))