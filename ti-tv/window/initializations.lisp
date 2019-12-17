;;; -*- Mode: Common-Lisp; Package: TV; Base: 10.; Fonts: MEDFNT,HL12B,HL12BI -*-

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

;;; Change history:
;;;
;;;  Date	     Author   Description
;;; -------------------------------------------------------------------------------------
;;; 03/23/89   MAY   Added build init to ONE-TIME-WINDOW-INITIALIZATIONS to clear the variable
;;;                      sys:cold-load-stream-owns-keyboard - hung up sheet-prepare-sheet-internal.
;;; 03/09/89   MAY   Cleaned up :once initializations using (window-initialize)
;;; 02/28/89   JLM    Changed cold-window-initializations and warm-window-initializations to support MP cool-boot.
;;; 05-24-88   jp     Clear who line before disk save.
;;; 05/23/88   KJF    Removed code which was setting up properties on property-list.  Was able to do this
;;;                      since source build is being done and it will happen at WINDOW build time.  See file:
;;;                      SHWARM, function: create-acceptable-initial-screen
;;; 04/10/88   KJF    Change to COLD-WINDOW-INITIALIZATIONS for multiple screen/dual monitor
;;;                       support.  Also, slight change to previous fix.
;;; 03/18/88   KJF    Re-create cache-window and sprite-window for microExplorer.
;;; 1/30/88    ab     Add si:during-cold-boot to SYSTEM-WINDOW-INITIALIZATIONS for MX.
;;; 12/12/86   RLA    Put warm initializations at head-of-list.
;;; 11/06/86   TWE   Made sys:cold-load-stream-owns-keyboard special in system-window-initializations.
;;; 11/03/86   TWE   Added the one-time-window-initializations function to add in the sprite initialization.
;;; 10/29/86   TWE   Put the SYS package prefix in for COLD-LOAD-STREAM-OWNS-KEYBOARD.
;;; 09/30/86   TWE   Added a call to create an instance of the searching windows as part of the cold
;;;		    initializations.
;;; 07/25/86   TWE   Moved the WINDOW-INITIALIZE so that it would be executed with the other `SYSTEM'
;;;		    initializations.  This makes the window system come alive after being shut down
;;;		    (during disk-save).


;;; This file contains all of the initializations for the Window System.
;;; The idea is to have all  of these semi-strange actions to  happen at
;;; once, instead of being scattered throughout the Window System code.



(DEFUN COLD-WINDOW-INITIALIZATIONS ()			;jlm 2/28/89
  (unless (or (mac-system-p)
	      (si:cool-boot-p))
    (SEND WHO-LINE-SCREEN :REFRESH))
  (unless (si:cool-boot-p)
    (SEND INITIAL-LISP-LISTENER :REFRESH))
  (W:PREALLOCATE-SEARCH-WINDOW-RESOURCES)
  ;; For the microExplorer, these get set to NIL since they are not MAC windows.
  ;; See (after-tv-initialized).
  ;; They must get re-created and will then be MAC windows.  03/18/88 KJF.
  ;; We'll do a check for mac-system since if these end up NIL for any other reason,
  ;; we'll assume someone knew what they were doing.  04/10/88 KJF.
  (WHEN (mac-system-p)
    (UNLESS w:cache-window
      (SETQ w:cache-window (MAKE-INSTANCE 'w:cache-window)))
    (UNLESS w:sprite-window
      (SETQ w:sprite-window (MAKE-INSTANCE 'w:sprite-window)))))

;;ab 1/30/88
(DEFUN SYSTEM-WINDOW-INITIALIZATIONS ()
  (DECLARE (SPECIAL SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD))
  (WHEN (AND (si:addin-p) (FBOUNDP 'si:during-cold-boot))
    (si:during-cold-boot))
    
  (SETQ KBD-INTERCEPTED-CHARACTERS KBD-STANDARD-INTERCEPTED-CHARACTERS
	SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD NIL)
  ;;The keyboard process wants to be permanent, to have medium-high priority, and
  ;;to restart sooner than other processes during booting.
  (UNLESS KBD-PROCESS
    (SETQ KBD-PROCESS (PROCESS-RUN-FUNCTION
			'(:NAME "Keyboard" :PRIORITY 30.
				:RESTART-AFTER-RESET T
				:WARM-BOOT-ACTION SI:PROCESS-WARM-BOOT-RESTART)
			'KBD-PROCESS-MAIN-LOOP)))
  (FUNCALL WHO-LINE-FILE-STATE-SHEET ':DELETE-ALL-STREAMS)
  (WINDOW-INITIALIZE))

(DEFUN WARM-WINDOW-INITIALIZATIONS ()			;jlm 2/28/89
  (SET-UP-CURRENT-SCREEN-COLOR)
  (INITIALIZE-SOUND)
  (SETQ LOCKED-ERROR-WINDOWS NIL)
  (when (and (si:mp-system-p)
	     (not (si:cool-boot-p)) ;; marky 12-20-88 was stealing it unecessarily
	     (= (logand #xf si:processor-slot-number) (min (values-list si:*MP-EXPLORER-SLOT-NUMBERS*)))
	     (> (funcall mp-get-number-of-active-processors) 1))
    ;; grab each processor to activate the KBD-SWITCH-PROCESS and setip the who-line
    ;; to indicate the keyboard owner, and to blank-out the mouse
    (dolist (slot (funcall mp-GET-ACTIVE-PROCESSOR-SLOT-NUMBERS) (funcall mp-grab-processor-internal 1))
      (let ((id (funcall mp-get-logical-processor-id slot)))
	(funcall mp-grab-processor-internal id)
	(sleep 1)))
    ))

(DEFUN ONE-TIME-WINDOW-INITIALIZATIONS ()
  "Run only during initial system bulid." ;; may 03/09/89
  (DECLARE (SPECIAL si:cold-load-stream-owns-keyboard)) ;; may 03/23/89
  ;; may 03/23/89 : SI:COLD-LOAD-STREAM-OWNS-KEYBOARD the HISTORY ...
  ;; Since some build bands are disk-saved withOUT the window-system, this defvar
  ;; evidently this was getting nil'ed out by the build operator by going into to debugger
  ;; at least ONCE ! Sometimes it did NOT (the bug) and sheet-prepare-sheet-internal called by
  ;; the initial who-line-screen :expose - the new color stuff handles a lot of stuff in 
  ;; (:method standard-screen :around :expose) - locked up system in scheduler.
  ;;   Originally the defvar was in BASSTR and loaded in make-system of window code,
  ;; but someone moved it to kernel;cold-load-stream. Now the question :
  ;; Why set it to T in lisp-reinitialize (init-cold-load-disk-and-paging) ? Evidently because of some
  ;; bug that only occurred between lisp-reinitialize and the window-system loading ( or startup of
  ;; KBD-PROCESS in SYSTEM-WINDOW-INITIALIZATIONS ).
  ;;   The REAL nil-ing out of si:cold-load-stream-owns-keyboard is done by
  ;; SYSTEM-WINDOW-INITIALIZATIONS just before starting up the kbd process ( makes sense )
  ;; but the system-window-initializations that are called in an intermediate disk-saved build band
  ;; does NOT contain the setq of cold-load-stream-owns-keyboard to nil because
  ;; SYSTEM-WINDOW-INITIALIZATIONS has not yet been put in the system-initialization-list
  ;; because the window system is NOT yet built ! (So we need the SETQ here, too)
  ;;   si:cold-load-stream-owns-keyboard was not set in lisp-reinitialize in rel2
  ;; - in fact, the defvar was originally in the tv package before moved to the 'sys package.
  ;;
  (setq si:cold-load-stream-owns-keyboard nil)		;; may 03/23/89 
  (window-initialize) ;; may 03/09/89 This was a separate :once initialization
  (INITIALIZE-SCREEN-MANAGER)
  (W:INITIALIZE-SPRITE-WINDOW))

(ADD-INITIALIZATION "Window-Once"   '(ONE-TIME-WINDOW-INITIALIZATIONS) '(:ONCE))

(ADD-INITIALIZATION "Window-System" '(SYSTEM-WINDOW-INITIALIZATIONS) :SYSTEM) ;; also calls (window-initialize)

(ADD-INITIALIZATION "Window-Cold"   '(COLD-WINDOW-INITIALIZATIONS)       :COLD)

;;; NOTE [RLA]: Reversed the order of these since putting each at head-of-list (last one will end up first)
(ADD-INITIALIZATION "Window-Warm"   '(WARM-WINDOW-INITIALIZATIONS)       '(:WARM :HEAD-OF-LIST))
(ADD-INITIALIZATION "Mouse"  '(MOUSE-INITIALIZE) '(:WARM :FIRST :HEAD-OF-LIST))


(ADD-INITIALIZATION "Forget old notifications"
		    '(SETQ notification-history nil)
		    '(:before-cold))

(ADD-INITIALIZATION "FRAME blanking array"
		    '(SETQ BLANKING-ARRAY (MAKE-BLANKING-ARRAY))
		    '(:NOW)
		    'ARRAY-ORDER-INITIALIZATION-LIST)

(ADD-INITIALIZATION "Screen manager bit arrays"
		    '(CLEAR-RESOURCE 'SCREEN-MANAGER-BIT-ARRAY-RESOURCE)
		    '(:NOW)
		    'ARRAY-ORDER-INITIALIZATION-LIST)

;;jp 5.24.88
(ADD-INITIALIZATION "Clear whostate"
		    '(SEND initial-lisp-listener :eval-inside-yourself
			   '(SETF who-line-string nil
				  function-doc nil
				  who-line-refresh-flag t))
		    :before-cold)

