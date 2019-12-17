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


;----------------------------------------------------------------------------------------

;;;
;;; These functions are used to get the physical address of some basic data structures. 
;;; 

(DEFUN byte-address (byte-base-address word-relative-offset)
  "Returns the byte address word-relative-offset words from the byte-base-address."
  (+ byte-base-address (* 4 word-relative-offset)))

(DEFUN setup-basic-comm ()
  "This sets up the Mac communication data structure."
  (map-LISPM-fonts)
  (SETF *mac* (MAKE-INSTANCE 'mac-flavor))
  (enable))

(DEFUN force-even (value)
  "If value is odd returns value+1."
  (LOGAND -2 (1+ value)))

(defun force-word (size)
  (let ((rem (rem size 4)))
    (+ size (if (> rem 0) (- 4 rem) 0))))

(DEFVAR basic-comm-command)
(DEFVAR acb)
(DEFVAR new-instance-kludge nil)
(defvar *tv-acbs* nil "Used to save acbs across reinits.")
(add-initialization "Delete tv-acbs*" '(setf *tv-acbs* nil) :before-cold)

(DEFMETHOD (mac-flavor :after :init)
	   (&rest ignore)
  "Sets up all the Mac-window communication data structures."
  (SETF channel (add:find-channel 'add:display-io))
  (LOOP for command in (si:flavor-all-instance-variables
			 (si:instance-flavor self))
	when (SEARCH "-command"
		     (SYMBOL-NAME command)
		     :test 'STRING-EQUAL)
					   ;Allocate ACBs for all commands.
	do
	(SETF basic-comm-command command)

	(setf acb (or (find basic-comm-command *tv-acbs* :key #'add:owner)
		      (add:make-allocation-acb
			(force-word (* 2 (SECOND (SYMEVAL-IN-INSTANCE self command)))))))
	(pushnew acb *tv-acbs* :test #'eq)
	(IF (NOT new-instance-kludge)
		(add:clear-acb acb))
	(SETF (add:opcode acb)
	      (FIRST (SYMEVAL-IN-INSTANCE self command)))
	(SETF (add:type acb) 'tv)
	(SETF (add:owner acb) command)
	(setf (add:deallocatable acb) nil)
	(setf (add:requestor-complete acb) t)
	(setf (add:servicer-complete acb) t)
	(setf (add:input-complete acb) t)
	(SET-IN-INSTANCE self command acb))
  );;:after :init

(DEFMETHOD (mac-flavor :enable-commands)
	   ()
  "Enables execution of all command blocks by setting the status bits to input and command complete
and clearing command-initiate." 
  (SEND (tv-channel) :reset)
  (LOOP for command in (si:flavor-all-instance-variables (si:instance-flavor self))
	when (SEARCH "-command"
		     (SYMBOL-NAME command)
		     :test 'STRING-EQUAL)
	do
	
	(setf (add:deallocatable acb) nil)
	(setf (add:requestor-complete acb) t)
	(setf (add:servicer-complete acb) t)
	(setf (add:input-complete acb) t)))

(DEFMETHOD (mac-flavor :reset-mac-state) ()
  "Flushes the cached mac state variables."
    (SETF current-LISPM-font nil))

(DEFUN tv-channel ()
  (SYMEVAL-IN-INSTANCE *mac* 'channel))

;; ab 9/27/88.  Since the printer code shares this channel now, don't turn on DEBUG by default.
;; Printing can take much longer than 20 seconds!
(DEFUN enable ()
  "Enables all commands by setting input and command complete bits on all command blocks.
Also resets the cached Mac state variables and command initiate."
  (DECLARE (SPECIAL *Mac-wait-time*))
  (LET ((ch (tv-channel)))
    (SEND ch :reset t)
;;    (SEND ch :turn-debug-on #'tv-debugger     ;ab 9/27/88
;;	  *Mac-wait-time* *Mac-wait-time* *Mac-wait-time* *Mac-wait-time*)
    (SEND *mac* :enable-commands)
    )
 ) 

;;ab 10/24/88.  Moved to FONT-TRANSLATIONS.
;;(DEFUN demap-mac-font (font-symbol)
;;  "Marks a font as not mac-translated so that the next parse-font-descriptor will
;;reaquire font parameters from the Mac."
;;  (WHEN (AND (SYMBOLP font-symbol)
;;	     (BOUNDP font-symbol)
;;	     (TYPEP (SYMBOL-VALUE font-symbol) 'tv:font))
;;    (DOLIST
;;      (font-name
;;        (LIST font-symbol			       ; Do both Explorer name
;;	      (tv:font-name (SYMBOL-VALUE font-symbol))))      ;and MAC-
;;      (WHEN (GET font-name :mac-translated)
;;        (SETF (GET font-name :mac-translated) nil))
;;      (WHEN (GET font-name :mac-fd)
;;        (SETF (GET font-name :mac-fd) nil)))))

;;(DEFUN demap-mac-fonts ()
;;  "Marks all fonts as not Mac-translated."
;;  (DO-LOCAL-SYMBOLS (symbol 'FONTS)
;;    (demap-mac-font symbol)))

(DEFMETHOD (mac-flavor :describe-command)
	   (command-name &optional (verbose nil))
  "Prints a description of the command.
`command-name' is a symbol which is the name of the command block, e.g. MoveTo-command"
  (LET ((command-block (SYMEVAL-IN-INSTANCE self command-name)))
    (add:print-acb command-block :verbose verbose)))

;----------------------------------------------------------------------------------------

;;; Basic command delivery methods
;;;
;;; The protocol sequence for sending a command to the Mac is:
;
;  1)  LISPM sends a :draw-foo message to the Mac-flavor.
;  2)  Check to be sure Mac has read the parameters from this command-block on
;       the previous call to :draw-foo; wait if it has not.  This is checked by looking
;       at the input-complete bit in the status word of the command block.
;  3)  Put the new parameters in the command-block.
;  4)  Clear the input-complete and command-complete bits in the status word.
;  5)  Put the physical address of the :draw-foo command block in command-initiate.
;  6)  Wait to be sure Mac has accepted the command by watching for
;       command initiate to clear.
;  7)  If this command requires a response (either returned values or success
;       acknowledgement) then wait for the command-complete bit in the status
;       work to come on.
;  8)  Return to LISP. Return values to LISP if any. LISP code is responsible for
;       copying returned values into a safe place before sending another :draw-foo.

(DEFVAR *Mac-wait-time* 20.
  "Time in seconds we will wait for the Mac to respond before we do something drastic.")

(DEFVAR *command-error-stack* nil
  "Records the history of any command which did not complete properly.")


(DEFUN reset-Mac-explorer-connection (&optional function-and-args-to-execute)
  (DECLARE (SPECIAL *ignore-commands-for-the-Mac*))
  (WHEN (si:mx-p)
    (si:%crash *attempt-to-call-reset-mac-explorer-connection* t t))
  (SETF w:default-screen w:main-screen
	w:mouse-sheet w:main-screen
	w:selected-window (FIRST (w:sheet-exposed-inferiors w:main-screen))
	mac:*mac-keyboard-p* nil)
  ;(cold-load-error "proceed" "resetting connection")
  (LET ((*tracing-off* nil)		   ; Remember why we came here...
	(*enabled-debugging-classes* *all-debugging-classes*))
    (remember-call :fatal))
  (SETF *ignore-commands-for-the-Mac* 1)
  ; Force the mouse to the Explorer even if usurped...
  (tv:mouse-initialize w:main-screen)
  (LOOP FOR window BEING THE ARRAY-ELEMENTS OF tv:previously-selected-windows
	WHEN (AND (EQ tv:main-screen (tv:sheet-get-screen window))
		  (OR (TYPEP window 'w:lisp-listener)
		      (TYPEP window 'zwei:zmacs-frame)
		      (TYPEP window 'tv:inspect-frame)))
	DO (SEND window :select)
	(RETURN))
  (BEEP)
  (IF function-and-args-to-execute
      (APPLY (FIRST function-and-args-to-execute)
	     (REST function-and-args-to-execute)))
  (PROCESS-RUN-FUNCTION "Cleanup Mac"
			#'(lambda ()
			    (enable)
			    (PROCESS-SLEEP 300.)
			    (SETF *ignore-commands-for-the-Mac* nil)))
  (IF (OR (EQ current-process tv:kbd-process)
	  (EQ current-process tv:mouse-process))
      (PROCESS-RESET-AND-ENABLE current-process)
    ;; else...
    (SEND current-process :kill))) 
  
(defvar *error-on-reset* t
  "If T signal an error on the cold-load-stream when resetting the tv channel.")

(DEFUN tv-debugger (error-string acb error-type channel &rest args)
  "Records any command protocol error on *command-error-stack*, 
returns the mouse and keyboard to the Explorer, activates 
the debugger, then optionally reenables all command 
blocks and returns t as if the Mac had responded properly."
  (DECLARE (IGNORE error-type args))
  (PUSH (LIST error-string
	      acb
	      (SEND channel :last-command)
	      nil ;;(SEND channel :port-contents)
	      (if (arrayp acb) (add:address acb))
	      (WHEN (arrayp (SEND channel :last-command))
		(add:address (SEND channel :last-command)))
	      (time:get-universal-time))
	*command-error-stack*)
  (when *error-on-reset*
    (let ((si:cold-load-stream-owns-keyboard t)
	  (*terminal-io* si:cold-load-stream))
      (eh:save-screen-for-cold-load-stream t)
      (cerror "Proceed" error-string)
      (eh:restore-screen-for-cold-load-stream))))

(DEFUN restart (&optional query (from-another-process t))
  "Cleans up things after the Mac stopped responding."
  (LET* ((dead (IF (NULL query)
		   t
		   (Y-OR-N-P "Do you really want to Restart?"))))
    (COND (from-another-process
	   (PROCESS-RUN-FUNCTION "Restart MAC" 'restart nil nil))
	  (dead
	   (si:before-disk-save t from-another-process)
	   (si:during-cold-boot)
	   (after-tv-initialized nil))
	  ((SEND *mac* :are-you-there? 60)
	   (enable))
	  (t (FORMAT t "Mac still not responding"))))
  nil)

(DEFMETHOD (mac-flavor :are-you-there?)
	   (&optional (timeout nil))
  "Sends Nop command to MAC and waits for MAC to acknowledge the
command, set input complete, and set command complete.  Returns
3 values: ACKNOWLEDGED-COMMAND INPUT-COMPLETE and COMMAND-COMPLETE
status.  Optional TIMEOUT is in seconds."
;;  (add:wait-port-ready Nop-Command channel)
  (let ((acb (add:get-acb 2 t)))
    (setf (add:opcode acb) #xff)
  (add:transmit-packet acb channel)
  (LET (input-complete command-complete)
    
      (SETQ input-complete
	    (OR (LOOP repeat 1000
		      when (add:input-complete acb)
		      do (RETURN t))
		(PROCESS-WAIT-WITH-TIMEOUT
		  "Wait Input Complete"
		  timeout
		  #'(lambda () (add:input-complete acb)))))
    (WHEN input-complete
      (SETQ command-complete
	    (OR (LOOP repeat 1000
		      when (add:command-complete acb)
		      do (RETURN t))
		(PROCESS-WAIT-WITH-TIMEOUT
		  "Wait Command Complete"
		  timeout
		  #'(lambda () (add:command-complete acb))))))
    
    (setf (add:requestor-complete acb) t)
    (add:return-acb acb)
    (SEND (tv-channel) :reset t)
    (VALUES t input-complete command-complete))))
