;;; -*- Mode:Common-lisp;  Package: TV; Base:10.; Fonts: (CPTFONT CPTFONTB HL12BI) -*-

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
;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains:  IO buffers, keyboard process

;;;                         CHANGE HISTORY
;;;
;;;  Date   Author	Description
;;; -------------------------------------------------------------------------------------
;;;   04-26-89  DAB    Changed kbd-intercept-abort-all  to clear-input buffers on abort.
;;;  01/26/89 MAY    Changed KEY-STATE for build process.
;;;  5/25/88  jjp     Add MX support for key-state
;;;  04/23/88 KJF    Added 2 terminal key sequences:
;;;                  -> TERM-CTRL-S for switching screens (like TERM-S for windows).
;;;                  -> TERM-CTRL-CLEAR-SCREEN for clearing some screens.
;;;  02/23/88 LG     Changed kbd-sys-1 to not create application X when
;;;  			the Mac cannot create the screen on a TERM SYS X.
;;;  02/21/88 KJF    Changed kbd-sys-1 to make a color map if color-system-p or not.
;;;  1/22/88  LAS       Modified add-system-key to use unnamed defstruct when creating keys and
;;;                     modified the kbd-sys-1 to check for the new style system key that has
;;;                     its find-instance and create-instance installed in the defsystem.  The
;;;                     system name is put on the system-key list for these new-style system keys.
;;;  10/22/87  KWW   Changed *color-system* to (color-system-p default-screen)
;;;   6/3/87   KWW   Changed 'bit to (sheet-array-type-cl default-screen)
;;;   7/2/87   PHM   Modified KBD-TERMINAL to get ric of compile warning
;;;   4/24/87  TWE	Changed the CLEAR-IO-BUFFER-RECORD function to be named
;;;			CLEAN-OUT-IO-BUFFER.  Also, it has different arguments since it gets
;;;			called automatically when a window instance gets returned to a
;;;			resource.
;;;  4/17/87 TWE	Fixed up CLEAR-IO-BUFFER-RECORD to be more careful in obtaining the
;;;			io-buffer from a window.  Not all windows have one.
;;;  4/16/87 TWE,KDB	Hacked up SHEET :DEACTIVATE to clean up non-garbage garbage in the
;;;			io buffer record.
;;;  3/06/87 TWE	Removed references to %% symbols and replaced them with the appropriate
;;;			calls to kernel functions.  Fixed up io-buffer-unget to not output debug
;;;			information to the screen but to put it into a local variable so it will
;;;			appear in the backtrace of a bug report.
;;; 12/22/86 TWE	Made several changes to make the build process cleaner.
;;;			Moved the DEFVARs for KBD-TYI-HOOK INITIAL-REPEAT-DELAY,
;;;			CONTINUOUS-REPEAT-DELAY, and PROCESS-IS-IN-ERROR from here to TVDEFS.
;;;			Moved the DEFVAR for NOTIFICATION-HISTORY closer to the front of this
;;;			file.
;;; 12/18/86 TWE	Fixed kbd-terminal to use SAFE-CHAR= to ASSOC down the *terminal-keys*
;;;			list.  That list contains NILs for separation.
;;; 12/10/86 TWE	Fixed the references to suggestions symbol function-arglist to have
;;;			the sys: package prefix.
;;; 12/04/86 TWE	Fixed help string to refer to the `status line' instead of `who line'
;;; 12/03/86 TWE	Fixed up a bug in find-window as per LGO's recommendation.
;;; 11/20/86 SLM	Added calls to Suggestions macros for TV:NOTIFY.
;;; 11/17/86 TWE	Cleaned up the term help by removing linefeed and return and doing
;;;			some formatting changes to avoid getting more processing.
;;; 11/10/86 TWE	Removed the view-mail and terminal-q code since they are in the
;;;			mailer and printer code, respectively.
;;; 11/05/86 TWE	Made the references to COLD-LOAD-STREAM-OWNS-KEYBOARD special in the
;;;			functions which use it.
;;; 11/03/86 LGO	Raised the priority of the KBD-SYS and Terminal-key processes from
;;;			0 to (1+ *selected-process-priority*)
;;; 10/28/86 GRH	Initialized *system-keys* to simple-lisp-listener.
;;; 10/28/86 TWE	Fixed find-window to use w:menu-choose.
;;; 10/23/86 TWE	Changed COLD-LOAD-STREAM and COLD-LOAD-STREAM-OWNS-KEYBOARD by adding
;;;			the SYS package prefix.  Also added *'s around the symbol
;;;			EH:ABORT-OBJECT.  Made cold-load-stream special in those functions
;;;			that referenced it since it is not a defvar anymore.
;;; 10/22/86 TWE	Changed GLOBAL:EVAL to just EVAL everywhere except in view-mail,
;;;			which is probably junk anyway.
;;; 10/22/86 LGO	Added the FIND-WINDOW function.
;;; 10/20/86 TWE	Changed CHOOSE-PROCESS-IN-ERROR and FIND-PROCESS-IN-ERROR to use
;;;			TYPEP properly.
;;; 10/10/86 TWE	Changed "keyboard" for the who state to use the global variable instead.
;;; 10/09/86 TWE	Fixed up FIND-WINDOW-OF-FLAVOR to work better with selection substitutes.
;;;  9/16/86 TWE	Changed "keyboard" back to "Keyboard" for the who state.
;;;  7/31/86 TWE	Fixed the resource of hardcopy-bit-array-resource to properly use
;;;			make-array in terms of the dimensions.
;;;  7/31/86 TWE	Changed use of :type 'art-1b to :element-type 'bit.
;;;  7/29/86 TWE	Changed to use Common Lisp functions.
;;;  7/28/86 TWE	Modified references to the pixel functions to use ARRAY-DIMENSION
;;;			and MAKE-ARRAY instead.
;;;  7/21/86 TWE	Changed several functions to use SAFE-CHAR= instead of CHAR=.
;;;  7/09/86 TWE	Changed the *system-key* DEFVAR to use simple-lisp-listener if it is
;;;			there. Fixed ADD-SYSTEM-KEY, ADD-TERMINAL-KEY and KBD-SYS-1 to use
;;;			the common lisp SORT functions instead of the undefined function
;;;			Zetalisp versions.
;;;  7/01/86 TWE	Changed uses of the delaying-screen-management macro to return nil.
;;;			This is an efficiency hack for the compiler.
;;;  6/11/86 TWE	Moved the value for the DEFVAR for KBD-PROCESS to the INITIALIZATIONS
;;;			file.  Also changed VIEW-FILE to do an OPEN :PROBE using the Common
;;;			Lisp syntax.
;;;  4/16/86 KDB	Changed instances of  "type" and "flush" to agree with consistency
;;;			guidelines.
;;;  2/28/86 LGO	Added control-bit check to ADD-SYSTEM-KEY. Provded by DLC
;;;  2/18/86 LGO	Moved background-lisp-interactor stuff to BASWIN
;;;			Moved versa-hardcopy stuff to obsolete
;;;			Converted system and terminal key handlers to use character objects
;;;			Added the following terminal keys:
;;;			  E      Selects an error background window.
;;;			  R      Resets process in selected window
;;;			  U      Gets TRACE menu
;;;			  Meta-E Expand selected window
;;;			  Meta-K Kill selected window
;;;			  Meta-M Move a window
;;;			  Meta-R Reshape a window
;;;			  Meta-S Select a window from a menu
;;;			Echo terminal parameters in the whostate.
;;;			Allow SYSTEM keys to have meta super hyper prefixes.
;;;			Made the following obsolete: KBD-TYI KBD-TYI-NO-HANG KBD-CHAR-AVAILABLE
;;;  2/03/86 GRH	Converted to Common-lisp.  A couple of calls to zetalisp
;;;			EVAL are left in which may be changed later when there
;;;			arguments are known to contain Common-lisp forms.

;;; IO buffers (definition in NTVDEF)

#| I think that the DEFUN below  this commented out code is all  that is
needed.  Note that the DEFUN does  not handle explicitly the cases  like
:GET, :REMPROP, etc., but they probably aren't used anyway.  |#
;;;(DEFSELECT ((:property io-buffer named-structure-invoke))
;;;  (:print-self (self *standard-output* ignore &optional ignore)
;;;    (si:printing-random-object (self *standard-output* :no-pointer :typep)
;;;      (FORMAT t "~O: " (%POINTER self))
;;;      (IF (= (io-buffer-input-pointer self)
;;;	     (io-buffer-output-pointer self))
;;;	  (PRINC "empty, ")
;;;	(FORMAT t "~D entr~:@P, "
;;;		(LET ((diff (- (io-buffer-input-pointer self)
;;;			       (io-buffer-output-pointer self))))
;;;		  (IF (< diff 0)
;;;		      (+ diff (io-buffer-size self))
;;;		    diff))))
;;;      (FORMAT t "State: ~a" (io-buffer-state self))))
;;;  ((:get :get-location-or-nil :get-location :getl :putprop :remprop :push-property :plist
;;;	 :plist-location :property-list-location :setplist :set)
;;;   . io-buffer-property-list-handler))
;;;(DEFUN io-buffer-property-list-handler (op self &rest args)
;;;  (APPLY 'si:property-list-handler op (LOCF (io-buffer-plist self)) args))


(DEFUN (:property io-buffer named-structure-invoke) (op buffer &rest args)
  "Printer for IO-BUFFER named structures"
  (CASE op
    (:which-operations '(:print-self))
    ((:print-self)
     (printing-random-object (buffer (CAR args) :no-pointer)
			     (FORMAT (CAR args) "IO-BUFFER ~O: " (%POINTER buffer))
			     (COND
			       ((= (io-buffer-input-pointer buffer)
				   (io-buffer-output-pointer buffer))
				(PRINC "empty, " (CAR args)))
			       (t
				(FORMAT (CAR args) "~D entr~:@P, "
					(LET ((diff
					       (- (io-buffer-input-pointer buffer)
						  (io-buffer-output-pointer buffer))))
					  (IF (< diff 0) (+ diff (io-buffer-size buffer)) diff)))))
			     (FORMAT (CAR args) "State: ~a" (io-buffer-state buffer))))
    (otherwise (FERROR NIL "I don't know about ~s" op))))  


(DEFUN make-io-buffer (size &optional in-fun out-fun plist state &aux buffer)
  "Create a new IO buffer of specified size.
The actual size is 2 greater, since that 2 elements must always be empty."
  (SETQ buffer (MAKE-ARRAY (+ 2 size)
			   :leader-length io-buffer-leader-size
			   :named-structure-symbol t))
  (STORE-ARRAY-LEADER 'io-buffer   buffer 1)
  (SETF (io-buffer-fill-pointer    buffer) 0)
  (SETF (io-buffer-size            buffer) size)
  (SETF (io-buffer-input-pointer   buffer) 0)
  (SETF (io-buffer-output-pointer  buffer) 0)
  (SETF (io-buffer-input-function  buffer) in-fun)
  (SETF (io-buffer-output-function buffer) out-fun)
  (SETF (io-buffer-state           buffer) state)
  (SETF (io-buffer-plist           buffer) plist)
  (SETF (io-buffer-record          buffer) (MAKE-ARRAY
					     io-buffer-record-length
					     :leader-list `(,io-buffer-record-length 0)))
  buffer)

(DEFCONSTANT DEFAULT-IO-BUFFER-SIZE 64.)

(DEFUN make-default-io-buffer ()
  (make-io-buffer DEFAULT-IO-BUFFER-SIZE nil 'kbd-default-output-function))

(DEFUN io-buffer-put (buffer elt &optional (no-hang-p nil))
  "Store a new element in an IO buffer, at the end (FIFO)."
  (DO ((inhibit-scheduling-flag t t)
       (ignore-p)
       (input-pointer)
       (in-fun (io-buffer-input-function buffer)))
      (nil)
    (COND
      ((OR (NULL (io-buffer-state buffer)) (EQ (io-buffer-state buffer) :input))
       (COND
	 (in-fun
	  ;; Call function with INHIBIT-SCHEDULING-FLAG turned on and bound.
	  ;; Since this function may change the state of the buffer either directly
	  ;; or indirectly, loop in order to check the state.  Set the function to
	  ;; NIL, though, so it won't be run again
	  
	  (MULTIPLE-VALUE-SETQ (ELT ignore-p)
	    (FUNCALL in-fun buffer elt))
	  (AND ignore-p (RETURN t)) (SETQ in-fun NIL))
	 (t
	  (COND
	    ((NOT (io-buffer-full-p buffer))
	     (SETF (io-buffer-last-input-process buffer) current-process)
	     (SETQ input-pointer (io-buffer-input-pointer buffer))
	     (SETF (AREF buffer input-pointer) elt)
	     (SETF (io-buffer-input-pointer buffer)
		   (REM (1+ input-pointer) (io-buffer-size buffer)))
	     (RETURN t))
	    (no-hang-p (RETURN NIL))
	    (t (SETQ inhibit-scheduling-flag NIL)
	     (PROCESS-WAIT "Buffer full" #'(lambda (buf)
					     (NOT (io-buffer-full-p buf)))
			   buffer))))))
      (no-hang-p (RETURN NIL))
      (t (SETQ inhibit-scheduling-flag NIL)
       (PROCESS-WAIT "Buffer state"
		     #'(lambda (buf)
			 (OR (NULL (io-buffer-state buf)) (EQ (io-buffer-state buf) :input)))
		     buffer))))) 

(DEFUN io-buffer-get (buffer &optional (no-hang-p nil))
  "Get an element from an IO buffer.  First value is ele, second is T if got one, else nil"
  (SETF (io-buffer-last-output-process buffer) current-process)
  (DO ((inhibit-scheduling-flag t t)
       (ELT)
       (ignore-p)
       (output-pointer)
       (out-fun (io-buffer-output-function buffer)))
      (nil)
    (COND
      ((OR (NULL (io-buffer-state buffer)) (EQ (io-buffer-state buffer) :output))
       (COND
	 ((NOT (io-buffer-empty-p buffer))
	  (SETQ output-pointer (io-buffer-output-pointer buffer))
	  (SETQ elt (AREF buffer output-pointer))
	  (SETF (io-buffer-output-pointer buffer)
		(REM (1+ output-pointer) (io-buffer-size buffer)))
	  ;; Stick this element into the record of the last few input chars.
	  
	  (AND (io-buffer-record buffer)
	       (LET ((input-record (io-buffer-record buffer)))
		 (INCF (io-buffer-record-pointer input-record))
		 (AND
		  (= (io-buffer-record-pointer input-record) (ARRAY-TOTAL-SIZE input-record))
		  (SETF (io-buffer-record-pointer input-record) 0))
		 (SETF (AREF input-record (io-buffer-record-pointer input-record)) elt)))
	  (COND
	    ((AND out-fun
		  ;; Call function with INHIBIT-SCHEDULING-FLAG on and bound.
		  ;; If element is to be ignored, loop back, else return element
		  
		  (PROG2
		    (MULTIPLE-VALUE-SETQ (ELT ignore-p)
		      (FUNCALL out-fun buffer elt))
		    ignore-p)))
	    (t (RETURN elt t))))
	 (no-hang-p (RETURN NIL NIL))
	 (t (SETQ inhibit-scheduling-flag NIL)
	  (PROCESS-WAIT "Buffer empty" #'(lambda (buf)
					   (NOT (io-buffer-empty-p buf)))
			buffer))))
      (no-hang-p (RETURN NIL NIL))
      (t (SETQ inhibit-scheduling-flag NIL)
       (PROCESS-WAIT "Buffer state"
		     #'(lambda (buf)
			 (OR (NULL (io-buffer-state buf)) (EQ (io-buffer-state buf) :output)))
		     buffer)))))

(DEFUN io-buffer-unget (buffer elt)
  "Reinsert ELT into the IO-BUFFER by backing up the pointer.
ELT should be the last thing read from the buffer."
  (WITHOUT-INTERRUPTS
   (LET ((output-pointer (1- (io-buffer-output-pointer buffer)))
	 BUFFER-ELT)
     (AND (< output-pointer 0) (SETQ output-pointer (1- (io-buffer-size buffer))))
     (WHEN (AND (NOT (EQ elt (SETQ BUFFER-ELT (AREF buffer output-pointer))))
		;; The objects were not eq.  Try again after converting them both to fixnums.
		(NOT (EQ (CHAR-INT ELT) (CHAR-INT BUFFER-ELT))))
       ;; Documentation alert!!  The following debug code is present to aid in identifying
       ;; exactly how this error occurs.  If this is a user error then a bug report should
       ;; be submitted.  The purpose of the LET is to put some useful information into the
       ;; backtrace so we can try an figure out what went wrong.  By looking at this and the
       ;; backtrace we should be able to identify where the problem occurred, or have a
       ;; pretty good idea of what to look at.  Please do not remove this debug code.
       ;; Bugs of this nature often are very difficult to reproduce so it is important to
       ;; put this kind of information into the bug report.
       (LET ((IMPORTANT-INFORMATION (CONCATENATE 'SIMPLE-STRING
                                                 (FORMAT NIL "~%Error occured in WINDOW;BASSTR io-buffer-unget.  ~
                                                              ELT=~D, BUFFER-ELT=~D, OUTPUT-POINTER=~D.~%Buffer contents="
                                                         ELT BUFFER-ELT OUTPUT-POINTER)
                                                 (LOOP FOR I FROM 0 BELOW (IO-BUFFER-SIZE BUFFER)
                                                       FOR STR = "" THEN (CONCATENATE 'SIMPLE-STRING STR
                                                                                      (FORMAT NIL "~D=#x~16R, "
                                                                                              I (AREF BUFFER I)))
                                                       FINALLY (RETURN STR)))))
         IMPORTANT-INFORMATION                  ; Make the compiler happy.
       (FERROR NIL "Attempt to un-get something different than last element gotten from io-buffer~
                    ~%If this is not a user error then please press Control-M to submit a bug report with the backtrace.")))
     ;; Remove this element from the record of the last few input chars.
     (AND (io-buffer-record buffer)
	  (LET ((input-record (io-buffer-record buffer)))
	    (DECF (io-buffer-record-pointer input-record))
	    (AND (MINUSP (io-buffer-record-pointer input-record))
		 (SETF (io-buffer-record-pointer input-record)
		       (1- (ARRAY-TOTAL-SIZE input-record))))))
     (SETF (io-buffer-output-pointer buffer) output-pointer)))) 

(DEFUN io-buffer-push (buffer elt)
  "Insert ELT into the IO-BUFFER at the front (LIFO fashion)."
  (WITHOUT-INTERRUPTS
    (LET ((output-pointer (1- (io-buffer-output-pointer buffer))))
      (AND (< output-pointer 0)
	   (SETQ output-pointer (1- (io-buffer-size buffer))))
      (IF (= output-pointer (io-buffer-input-pointer buffer))
	  (FERROR nil "IO-BUFFER ~S is full." buffer))
      (SETF (AREF buffer output-pointer) elt)
      (SETF (io-buffer-output-pointer buffer) output-pointer))))

(DEFUN io-buffer-clear (buffer)
  "Clears out an IO buffer"
  (WITHOUT-INTERRUPTS
    (SETF (io-buffer-input-pointer  buffer) 0)
    (SETF (io-buffer-output-pointer buffer) 0)
    t))

;;; The following function is used to aid in cleaning up non-garbage garbage.  NGG is
;;; data which should be reclaimed as garbage but something still points to it.  The
;;; io buffer record can do that for mouse blips, which contain window object pointers.
;;; If such a window gets killed, the pointer remains in the io buffer record so it
;;; can't be reclaimed as garbage.  This function is called in the SHEET :DEACTIVATE
;;; method so that these pointers won't remain when the window is removed from the
;;; window hierarchy.  Pop-up windows and typeout windows get cleaned up when the user
;;; is done using them.  Other windows must be explicitly killed (or deactivated, but
;;; that is harder since one can't do that from the system menu) to cause the io buffer
;;; record to get cleaned up.  Note that we also need to do the same thing to the
;;; io-buffer itself too.
(DEFUN CLEAN-OUT-IO-BUFFER (RESOURCE WINDOW &REST IGNORE)
  (DECLARE (IGNORE RESOURCE))
  (LET ((IO-BUFFER (SEND WINDOW :SEND-IF-HANDLES :IO-BUFFER))
        IO-RECORD)
    (WHEN IO-BUFFER
      ;; Note that the io-buffer has a fill-pointer which indicates how much current
      ;; data is present.  When all characters have been read this will be 0, so in
      ;; order to clear out the io-buffer we need to find out how big it is and then
      ;; clear that amount.
      (FILL (THE ARRAY IO-BUFFER) NIL :START 0 :END (ARRAY-DIMENSION IO-BUFFER 0))
      (SETQ IO-RECORD (IO-BUFFER-RECORD IO-BUFFER))
      ;; The io-buffer-record's fill pointer indicates the real size of the array, so
      ;; we can use FILL directly and not have to specify any boundaries.
      (FILL (THE ARRAY IO-RECORD) NIL)
      (STORE-ARRAY-LEADER 0 IO-RECORD 1))))

#| Define a resource of I/O buffers.  This lets other code simply get
one from the resource, instead of creating one from scratch.  |#
(DEFUN MAKE-IO-BUFFER-RESOURCE (IGNORE SIZE INPUT-FUNCTION OUTPUT-FUNCTION)
  (MAKE-IO-BUFFER SIZE INPUT-FUNCTION OUTPUT-FUNCTION))

(DEFUN CLEAR-IO-BUFFER-RESOURCE (IGNORE OBJECT &REST IGNORE)
  (IO-BUFFER-CLEAR OBJECT))

(DEFRESOURCE IO-BUFFER (&OPTIONAL (SIZE DEFAULT-IO-BUFFER-SIZE)
                                  (INPUT-FUNCTION NIL)
                                  (OUTPUT-FUNCTION 'KBD-DEFAULT-OUTPUT-FUNCTION))
  :CONSTRUCTOR MAKE-IO-BUFFER-RESOURCE
  :INITIAL-COPIES 0
  :INITIALIZER CLEAR-IO-BUFFER-RESOURCE)


(DEFUN process-typeahead (io-buffer function)
  "Apply FUNCTION to each element of IO-BUFFER.
The value returned by FUNCTION is put back in place of the original element,
except that if the value is NIL, the element is simply deleted."
  (DO ((input-pointer (io-buffer-input-pointer io-buffer))
       (ch))
      ((OR (= input-pointer (io-buffer-output-pointer io-buffer))
	   (NULL (SETQ ch (io-buffer-get io-buffer t)))))
    (AND (SETQ ch (FUNCALL function ch))
	 (io-buffer-put io-buffer ch t))))

;;
;; Supports UCL keystroke macro code.  The forcing of
;;a macro's keys is nested in a call to this macro. (Hogge)
;;
(DEFMACRO with-expanding-io-buffer ((max-chars extension . nil) &rest body)
  "Executes BODY after making sure the selected IO buffer (W:SELECTED-IO-BUFFER)
is big enough to hold MAX-CHARS more characters//blips.  If it is not big enough,
it is increased by EXTENSION elements."
  `(LET ((size (io-buffer-size selected-io-buffer))
	 (output-pointer (io-buffer-output-pointer selected-io-buffer))
	 (input-pointer (io-buffer-input-pointer selected-io-buffer)))
     ;;Max number of characters we can input is size - number of chars already input.
     (WHEN (< (- size (COND ((= output-pointer input-pointer)
			     0)
			    ((> input-pointer output-pointer)
			     (- input-pointer output-pointer))
			    (t
			     (- size (- output-pointer input-pointer)))))
	      ,max-chars)
       (io-buffer-extend selected-io-buffer ,extension))
     . ,body))

(DEFUN io-buffer-extend (buffer extension)
  "Extend BUFFER by EXTENSION array elements.  BUFFER is a W:IO-BUFFER defstruct.
Shifts current buffer contents so that the extension does not affect IO opertations."
  (WITHOUT-INTERRUPTS
    (LET* ((old-size (io-buffer-size buffer))
	   (new-size (+ old-size extension))
	   (output-pointer (io-buffer-output-pointer buffer))
	   (new-output-pointer (+ output-pointer extension)))
      ;;Increase the buffer size, physically and logically
      (ADJUST-ARRAY buffer new-size)
      (SETF (io-buffer-size buffer) new-size)
      ;;Make room in buffer for new input by shifting old input into the new array slots
      (COPY-ARRAY-PORTION buffer output-pointer old-size
			  buffer new-output-pointer new-size)
      ;;Adjust buffer pointers
      (SETF (io-buffer-output-pointer buffer) new-output-pointer
	    (io-buffer-input-pointer buffer) (+ (io-buffer-input-pointer buffer) extension)))))


(DEFVAR kbd-io-buffer (make-io-buffer 512.))	;Intermediate buffer so char is read out of
						; hardware immediatly
(DEFVAR kbd-terminal-happened nil)		;A TERMINAL key was pressed
(DEFVAR kbd-terminal-time nil
  "If non-NIL, this is the time we started processing a Terminal or
System which is still in process.  We try not to look at the keyboard
while one is still in process to provide more predictable behavior with
typeahead.  However, we don't wait forever so that if the process hangs
forever the system does not die.")

;;;Moved to SYS:WINDOW;COLD since is used in cold build before window system is loaded.
;(DEFVAR COLD-LOAD-STREAM-OWNS-KEYBOARD NIL
;  "Non-NIL means something reading from cold load stream, so turn off KBD-PROCESS.")

(DEFUN kbd-process-main-loop ()
  "This function runs in the keyboard process.  It is responsible for reading characters
from the hardware, and performing any immediate processing associated with the character."
  (DECLARE (SPECIAL SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD))
  (ERROR-RESTART-LOOP ((sys:abort error) "Return to top level of kbd-process.")
    (io-buffer-clear kbd-io-buffer)
    (SETQ kbd-terminal-happened nil)
    (DO () (nil)
      (PROCESS-WAIT *DEFAULT-READ-WHOSTATE*
		    #'(lambda ()
			(OR kbd-terminal-happened
			    (AND (NOT sys:cold-load-stream-owns-keyboard)
				 (NOT (io-buffer-full-p kbd-io-buffer))
				 (kbd-hardware-char-available)))))
      (COND (kbd-terminal-happened
	     (APPLY (SECOND kbd-terminal-happened)
		    (FIRST  kbd-terminal-happened)
		    selected-window
		    (CDDR kbd-terminal-happened))
	     (PROCESS-WAIT "TERM finish"
			   #'(lambda () (LET ((x kbd-terminal-time))
					  (OR (NULL x)	 ;Wait at most 10 seconds
					      (> (TIME-DIFFERENCE (TIME) x) 600.)))))
	     (SETQ kbd-terminal-happened nil)))
      (kbd-process-main-loop-internal))))   

;Note that KBD-CONVERT-TO-SOFTWARE-CHAR must be called in order,
;since for the new keyboards it does shifts and keeps state.

(DEFPARAMETER kbd-standard-asynchronous-characters
	      '((#\c-abort kbd-asynchronous-intercept-character
		 (:name "abort" :priority 50) kbd-intercept-abort)
		(#\c-m-abort kbd-asynchronous-intercept-character
		 (:name "Abort all" :priority 50) kbd-intercept-abort-all)
		(#\c-break kbd-asynchronous-intercept-character
		 (:name "break" :priority 40) kbd-intercept-break)
		(#\c-m-break kbd-asynchronous-intercept-character
		 (:name "Error break" :priority 40) kbd-intercept-error-break))
  "Default alist of asynchronous characters for a window's keyboard input.")    

(DEFPARAMETER kbd-global-asynchronous-characters
	      '((#\term kbd-terminal)
		(#\system kbd-sys)
		(#\c-clear-input kbd-terminal-clear))
	      "Default alist of keys handled like Terminal and system.") 

;;;removed per GRH's instructions -- unnecessary after the
;;;he changed some function definitions to fix the keyboard
;;;repeat problems -- GSM 21 Nov 85
;(DEFVAR KBD-REPEAT-STATE           'NO
; "Other states are 'NEW and 'CONT.")

(DEFVAR kbd-repeat-soft-char         0
  "Last valid repeatable converted ascii keystroke.")

(DEFVAR kbd-hdw-out-ptr              0
  "Saved buffer address of the hardware character.")

;;;replaced kbd-repeat-char definition per GRH's instructions on 21 Nov 85
;;;as part of a fix for a repeat key bug -- GSM

(DEFUN kbd-repeat-char ()
  "This function repeats a held-down key until it is lifted or a new one is pushed down.
  It should probably only be called by KBD-PROCESS-MAIN-LOOP-INTERNAL.
  
  KBD-HDW-OUT-PTR - must have been bound (not set) to the hardware character
  buffer back pointer when the character was origially gotten.  This is
  so that if this is called from another stack group while snarfing or
  something the variables will maintain the right values.
  
  KBD-REPEAT-SOFT-CHAR - should be bound to the character to repeat."
  (UNLESS (PROCESS-WAIT-WITH-TIMEOUT		;wait for keystroke or timeout
	    *DEFAULT-READ-WHOSTATE* initial-repeat-delay #'kbd-hardware-char-available)
    ;; Else start repeating the character
    (LOOP
      (WITHOUT-INTERRUPTS
	 ;; Since other routines grab a hardware character themselves (sigh),
	 ;; this checks to see if that indeed has happened and shuts off the
	 ;; repeat state.  This problem specifically showed up when the 
	 ;; debugger got called in during a repeat -- It scarfed up the "up
	 ;; stroke" of the repeating char and caused a no-stop condition.
	(WHEN (NEQ kbd-hdw-out-ptr
		   (si::aref-32b si::*keyboard* %kbd-buffer-back))
	  (RETURN (SETQ kbd-repeat-soft-char NIL)))
	(IF (char-bit kbd-repeat-soft-char :mouse) ;mouse char?
	    (kbd-process-mouse-character kbd-repeat-soft-char)
	    (AND (NOT (io-buffer-full-p kbd-io-buffer))	 ;push on buffer
		 (io-buffer-put kbd-io-buffer kbd-repeat-soft-char))))
      (WHEN (PROCESS-WAIT-WITH-TIMEOUT		;wait for new keystroke or timeout
	      *DEFAULT-READ-WHOSTATE* continuous-repeat-delay #'kbd-hardware-char-available)
	(RETURN (SETQ kbd-repeat-soft-char NIL))))))  
   

(DEFUN kbd-process-mouse-character (CHAR)
  "Execute a mouse character which the user has entered from the
keyboard."
  (COND ((= char #\mouse-m-2)  (mouse-warp  0 -1 t))	 ; Up
	((= char #\mouse-m-3)  (mouse-warp  0  1 t))	 ; Down
	((= char #\mouse-l-2)  (mouse-warp -1  0 t))	 ; Left
	((= char #\mouse-r-2)  (mouse-warp  1  0 t))	 ; Right
	(t
	 (chaparral-insert-mouse-buttons char)	 ; Mouse clicks
	 )))

;;;replaced the following function definition on 21 Nov 85
;;;per GRH's instructions as part of a repeat key fix
;;;  -- GSM

(DEFUN kbd-process-main-loop-internal
       (&aux buffer plist raw-p asynch-chars tem lowercase-control-chars
	char kbd-soft-char repeatable?
	KBD-HDW-OUT-PTR				; Rebind special: Need this to have a copy for each stack group.
	kbd-repeat-soft-char)			; Same for this one, since this code sometimes is executed also
						; from another stack group when "snarfing".
  (DO ()
      ((OR (SETQ kbd-repeat-soft-char NIL)	; Always reset to nil.
	   kbd-terminal-happened (NOT (kbd-hardware-char-available))))
    (WITHOUT-INTERRUPTS
      (COND
	((SETQ buffer (kbd-get-io-buffer)) (SETQ plist (LOCF (io-buffer-plist buffer)))
					   (SETQ asynch-chars (GET plist :asynchronous-characters))
					   (IF (GET plist :super-image) (SETQ asynch-chars NIL)
					       (UNLESS asynch-chars
						 (SETQ asynch-chars kbd-standard-asynchronous-characters)))
					   (SETQ lowercase-control-chars (GET plist :dont-upcase-control-characters))
					   (SETQ raw-p (GET plist :raw)))
	(t (SETQ asynch-chars kbd-standard-asynchronous-characters)))
      
      (DO ()
	  ((OR kbd-terminal-happened
	       (NOT (kbd-hardware-char-available))))
	(SETQ char (kbd-get-hardware-char)	;grab a character off the hardware buffer
	      kbd-repeat-soft-char NIL)		; Reset repeat char
	(IF raw-p
	    (OR (io-buffer-full-p buffer)	;in case they just want the raw hdw scan code
		(io-buffer-put buffer char))
	    (PROGN
	      (MULTIPLE-VALUE-SETQ (kbd-soft-char repeatable?)	  ;convert hdw to soft ascii char
				   (kbd-convert-to-software-char char (NOT lowercase-control-chars)))
	      (UNLESS (NULL kbd-soft-char)
		(SETQ si::who-line-just-cold-booted-p NIL)
		;; Save repeat state
		(WHEN repeatable?
		  (SETQ kbd-repeat-soft-char kbd-soft-char kbd-hdw-out-ptr
			(si::aref-32b si::*keyboard* %kbd-buffer-back)))
		;; Don't count the Terminal key as keyboard activity.
		(OR (= kbd-soft-char #\term)
		    (SETQ kbd-last-activity-time (TIME)))

		;; Handle the mouse movement and mouse clicking requests made
		;; by the user from the keyboard.  Note that we are treating
		;; these character as normal characters (to allow them to be
		;; repeated) but they are not placed into the keyboard IO buffer.
		(IF (char-bit kbd-soft-char :mouse)
                    (kbd-process-mouse-character kbd-soft-char)
                    (COND
                      ((SETQ kbd-terminal-happened
                             (ASSOC kbd-soft-char kbd-global-asynchronous-characters :test #'CHAR=)))
                      ((SETQ tem (ASSOC kbd-soft-char asynch-chars :test #'char=))
                       (APPLY (CADR tem) (CAR tem) selected-window (CDDR tem)))
                      ((NOT (io-buffer-full-p kbd-io-buffer))   ;put char in io-buffer
                       (io-buffer-put kbd-io-buffer kbd-soft-char))))))
	    ;; Scan code converted to nil -(up strokes, cntl, etc.)
	    )))
    (AND KBD-REPEAT-SOFT-CHAR			; repeat if we have a repeatable char
	 (PLUSP initial-repeat-delay)		; and initial-repeat-delay is not zero
	 (kbd-repeat-char)))) 		 
  
(DEFUN kbd-io-buffer-get (buffer &optional
			  (no-hang-p nil)
			  (whostate *default-read-whostate*)
			  &aux (scheduling-was-inhibited inhibit-scheduling-flag))
  "Get the next element from io-buffer BUFFER or from the keyboard.
The keyboard is checked for input only if this buffer is the selected one.
NO-HANG-P says return NIL immediately if no input is available.
WHOSTATE appears in the who line if we have to wait.
Note that BUFFER's output function is executed even if the input
comes from the keyboard \"directly\"."
  (DO ((inhibit-scheduling-flag t t)
       (update-state-p (NEQ current-process (io-buffer-last-output-process buffer)))
       (ok)
       (ELT))
      (nil)
    (MULTIPLE-VALUE-SETQ (ELT ok)
      (io-buffer-get buffer t))

    ;; If new process reading, better update wholine run state
    (AND update-state-p (EQ buffer selected-io-buffer) (who-line-run-state-update))

    ;; Got something from the normal buffer, just return it
    (AND ok (RETURN elt))

    ;; OK is NIL here.  If we aren't selected, don't look at system's io buffer
    (AND (EQ buffer selected-io-buffer)
	 (MULTIPLE-VALUE-SETQ (ELT ok)
	   (io-buffer-get kbd-io-buffer t)))
    (COND
      (ok      ;; Got something from the kbd buffer, put it into the normal buffer and loop
       (io-buffer-put buffer elt t))		;Can't hang, but...

      ;; Nothing for baby!!!  What should we do?
      (t
;;; If scheduling was inhibited when we entered this function its possible that the
;;; keyboard process is being locked out, and thus will never be able to queue up
;;; characters for us.  For that reason, if scheduling was disabled, go enter the
;;; main loop processing.   PMH 11/17/87  SPR #2286
	   (AND scheduling-was-inhibited
		(NOT (IO-BUFFER-FULL-P KBD-IO-BUFFER))
		(KBD-HARDWARE-CHAR-AVAILABLE)
       ;; If there is a possibility that a character of interest exists in
       ;; the hardware, get it
		(KBD-PROCESS-MAIN-LOOP-INTERNAL))
       (IF
	 (OR (NOT (io-buffer-empty-p buffer))
	     (AND (EQ buffer (kbd-get-io-buffer))
		  (NOT (io-buffer-empty-p kbd-io-buffer))))
	 NIL					;Have a character, so loop and get it
	
	(PROGN
	  (AND no-hang-p (RETURN NIL))
	  (SETQ inhibit-scheduling-flag NIL)
	  (PROCESS-WAIT whostate
			#'(lambda (buffer)
			    (OR (NOT (io-buffer-empty-p buffer))
				(AND (EQ buffer (kbd-get-io-buffer))
				     (NOT (io-buffer-empty-p kbd-io-buffer)))))
			buffer)))))))
 

(DEFUN kbd-wait-for-input-with-timeout
       (buffer timeout &optional (whostate *DEFAULT-READ-WHOSTATE*))
  "Wait until there is input available in BUFFER or the keyboard, or
until TIMEOUT.  The keyboard is checked for input only if this buffer is
the selected one.  TIMEOUT is in units of 60'ths of a second, like the 
TIME function.  WHOSTATE appears in the who line while we wait."
  (PROCESS-WAIT whostate #'(lambda (buffer start-time interval)
			     (OR (>= (TIME-DIFFERENCE (TIME) start-time)
				    interval)
				 (NOT (io-buffer-empty-p buffer))
				 (AND (EQ buffer (kbd-get-io-buffer))
				      (NOT (io-buffer-empty-p kbd-io-buffer)))))
		buffer
		(TIME) timeout))

(DEFUN kbd-wait-for-input-or-deexposure (buffer window &optional (whostate *DEFAULT-READ-WHOSTATE*))
  "Wait until BUFFER or the keyboard has input, or WINDOW is not exposed.
The keyboard is checked for input only if this buffer is the selected one.
WHOSTATE appears in the who line while we are waiting."
  (PROCESS-WAIT whostate #'(lambda (buffer window)
			     (OR (NOT (SEND window :exposed-p))
				 (NOT (io-buffer-empty-p buffer))
				 (AND (EQ buffer (kbd-get-io-buffer))
				      (NOT (io-buffer-empty-p kbd-io-buffer)))))
		buffer window))

(DEFUN kbd-snarf-input (buffer &optional no-hardware-chars-p)
  "Transfer any input that the keyboard has into BUFFER.
The keyboard is checked for input only if this buffer is the selected one."
  (WITHOUT-INTERRUPTS
    (COND ((NULL buffer))			;This can happen due to timing error
	  ((EQ buffer (kbd-get-io-buffer))
	   ;; There is potentially input for us
	   (OR no-hardware-chars-p (kbd-process-main-loop-internal))
	   (DO ((ok)
		(ELT))
	       ((io-buffer-empty-p kbd-io-buffer))
	     (MULTIPLE-VALUE-SETQ (ELT ok)
	       (io-buffer-get kbd-io-buffer t))
	     (OR ok (RETURN nil))		;Some ignored characters, we are done
	     (AND elt (io-buffer-put buffer elt t))))))) 

(DEFPARAMETER kbd-standard-intercepted-characters
	      '((#\abort kbd-intercept-abort)
		(#\m-abort kbd-intercept-abort-all)
		(#\break kbd-intercept-break)
		(#\m-break kbd-intercept-error-break))) 
(DEFVAR kbd-intercepted-characters kbd-standard-intercepted-characters
  "List of characters to be processed by the low levels of reading keyboard input.
Each element is a list (character function).  The function is called with
the character as its argument, and should return two values:
the character (or another character, to translate it), and
a flag saying whether to ignore the character.")

(DEFUN kbd-default-output-function (IGNORE char)
  "System standard IO-BUFFER output function.
Intercepts those characters in KBD-INTERCEPTED-CHARACTERS.
Must be called with INHIBIT-SCHEDULING-FLAG bound to T, and this may SETQ it to nil."
  (IF (AND kbd-tyi-hook
	   (SEND kbd-tyi-hook char))
      (VALUES char t)
    (LET ((tem (AND (ATOM char) ;; Don't send mouse blips into ASSOC
		    (ASSOC char kbd-intercepted-characters :test #'safe-char=))))
      (IF tem (SEND (CADR tem) char) char))))

(DEFUN kbd-intercept-abort (CHAR &rest ignore)
  "Perform the action normally associated with the Abort character.
This function is intended to be called from IO-BUFFER output functions."
  char
  (SETQ inhibit-scheduling-flag nil)		;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (OR (AND (TYPEP *terminal-io* 'sheet)		;Kludge to avoid being unable to abort
	   (sheet-output-held-p *terminal-io*))
      (SEND *terminal-io* :send-if-handles :inhibit-output-for-abort-p)
      (PROGN
	(SEND *terminal-io* :clear-eol)
	(SEND *terminal-io* :string-out "[abort]")))
  (SIGNAL-CONDITION
#-elroy    eh:abort-object
#+elroy    eh:*abort-object*
    ))

(DEFUN kbd-intercept-abort-all (CHAR &rest ignore)
  "Perform the action normally associated with the Meta-Abort character.
This function is intended to be called from IO-BUFFER output functions."
  char
  (SETQ inhibit-scheduling-flag nil)		;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (OR (AND (TYPEP *terminal-io* 'sheet)		;Kludge to avoid being unable to abort
	   (sheet-output-held-p *terminal-io*))
      (SEND *terminal-io* :send-if-handles :inhibit-output-for-abort-p)
      (PROGN
	(send *terminal-io* :clear-input)   ; DAB 04-26-89 Clear residue if user hit meta-ctrl-abort.
	(SEND *terminal-io* :clear-eol)
	(SEND *terminal-io* :string-out "[Abort all]"))
      )
  (SEND current-process :reset :always))

(DEFUN kbd-intercept-break (CHAR &rest ignore)
  "Perform the action normally associated with the Break character.
This function is intended to be called from IO-BUFFER output functions."
  (SETQ inhibit-scheduling-flag nil)		;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (BREAK "break")
  (VALUES char t))

(SETF (GET 'kbd-intercept-error-break :error-reporter) t)
(DEFUN kbd-intercept-error-break (CHAR &rest ignore)
  "Perform the action normally associated with the Meta-Break character.
This function is intended to be called from IO-BUFFER output functions."
  (DECLARE (SPECIAL sys:cold-load-stream))
  (SETQ inhibit-scheduling-flag nil)		;It was T in the IO-BUFFER-OUTPUT-FUNCTION
;  (LET ((EH:CONDITION-PROCEED-TYPES '(:NO-ACTION)))
  (MULTIPLE-VALUE-BIND (buffer position)
      (SEND *standard-input* :send-if-handles :save-rubout-handler-buffer)
;   (LET ((EH:ERROR-DEPTH (1+ EH:ERROR-DEPTH)))
;     (EH:INVOKE-DEBUGGER '(BREAK)))
    (UNWIND-PROTECT
	(SIGNAL-CONDITION (MAKE-CONDITION 'BREAK) '(:no-action) t)
      (UNLESS (EQ *debug-io* sys:cold-load-stream)
	(IF buffer (SEND *standard-input* :restore-rubout-handler-buffer buffer position)))))
  (VALUES char t))

;;; This function is called, possibly in the keyboard process, when one of the
;;; standard asynchronous intercepted characters, of the sort that mungs over the
;;; process, is typed.  Scheduling is inhibited.
;;; This does the actual munging of the process in a separate process, in case
;;; it has to wait for the process' stack-group to get out of some weird state.
(DEFUN kbd-asynchronous-intercept-character (CHAR window &optional process-run-options function
					     &aux p)
  ;; Forget chars typed before "CTRL-abort", even those inside window's io buffer.
  (kbd-terminal-clear NIL)
  (WHEN (EQ function 'kbd-asynchronous-intercept-character)
    (SETQ function NIL))
  (AND WINDOW					;Find process to be hacked
       (SETQ p (SEND window :process))
       (IF function
	   (PROCESS-RUN-FUNCTION process-run-options p :interrupt function char window)
						; note that we maybe comparing fixnums to characters
	   (COND ((= char #\c-abort)
		  (PROCESS-RUN-FUNCTION '(:name "abort" :priority 50) p :interrupt
					#'kbd-intercept-abort
                                        (set-char-bit char :control nil)))
		 ((= char  #\c-m-abort)
		  (PROCESS-RUN-FUNCTION '(:name "abort" :priority 50) p :interrupt
					#'kbd-intercept-abort-all
					(set-char-bit char :control nil)))
		 ((= char #\c-break)
		  (PROCESS-RUN-FUNCTION '(:name "break" :priority 40) p :interrupt
					'BREAK 'BREAK))
		 ((= char #\c-m-break)
		  (PROCESS-RUN-FUNCTION '(:name "break" :priority 40) p :interrupt
					%error-handler-stack-group '(:break)))))))

(DEFUN kbd-get-software-char (&optional (whostate *DEFAULT-READ-WHOSTATE*))
  "Returns the next char from the hardware converted to software codes.
This is meant to be used only by things that run in the keyboard
process, and not by any user code."
  (DO ((ch)) (nil)
    (PROCESS-WAIT whostate #'kbd-hardware-char-available)
    (AND (SETQ ch (kbd-convert-to-software-char (kbd-get-hardware-char)))
	 (RETURN ch))))

(DEFUN kbd-char-typed-p (&aux (buffer (kbd-get-io-buffer)))
  "Kludge to return T when a character has been typed.  First checks the
selected window's IO buffer, and if it is empty then checks the
microcode's buffer.  This is useful for programs which want to stop when
a character is typed, but don't want to allow interrupts and
scheduling."
  (OR (AND buffer (NOT (io-buffer-empty-p buffer)))
      (kbd-hardware-char-available)))

(DEFUN kbd-clear-io-buffer ()
  "Clear the keyboard buffer and the hardware buffer"
  (io-buffer-clear kbd-io-buffer)
  (DO ((ch))
      ((NULL (SETQ ch (kbd-get-hardware-char))))
    ;; Call this to process shifts
    (kbd-convert-to-software-char ch)))

(DEFUN kbd-clear-selected-io-buffer ()
  "Clear the selected io buffer"              ;flush => clear, kdb, 3/27/86 to agree with consistency guidelines
  (SETQ selected-io-buffer nil))

(DEFUN kbd-get-io-buffer ()
  "Returns the selected IO buffer -- the one that is allowed to read
from the keyboard.  If there is no current buffer, the selected window
is interrogated.  If there is no selected window, or the window has no
buffer, returns nil."
  (COND ((NULL selected-window)
	 ;; This shouldn't be necessary, but try not to lose too big
	 (kbd-clear-selected-io-buffer))
	(selected-io-buffer selected-io-buffer)
	(t (PROG1 (SETQ selected-io-buffer (SEND selected-window :io-buffer))
		  (who-line-run-state-update)))))	 ;May have just switched processes

(DEFUN kbd-call (buffer)
  "Handle the CALL character."
  BUFFER					;Not used
  (io-buffer-clear kbd-io-buffer)		;Forget chars typed before "call"
  (PROCESS-RUN-FUNCTION "call" #'(lambda (window)
				   (IF window
				       (SEND window :call)
				       (PROGN
					 (SETQ window (kbd-default-call-window))
					 (SEND window :mouse-select))))
			selected-window))  

(DEFUN kbd-default-call-window (&optional (screen default-screen) &aux previous-window)
  "Return a suitable window for the CALL character to select."
  (IF (AND (SETQ previous-window (AREF previously-selected-windows 0))
	   (EQ (SEND previous-window :lisp-listener-p) :idle))
      ;; CALL should always get a Lisp Listener, but try to be smart about
      ;; the one that it really gets
      previous-window
      (SEND screen :idle-lisp-listener)))

;;;replced key-state definition per TWE's instructions
;;;  -- GSM 22 Nov 85
(defconstant nokb 255.)		;;;these keys are not on the mac kbd.
(DEFPARAMETER *Explorer-to-Mac-kbd-code-map*
	      (MAKE-ARRAY 256. :element-type '(unsigned-byte 8)
			  :initial-contents
      ;;       0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
      #x(LIST Nokb 7c Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb 7e Nokb Nokb Nokb Nokb   ;0
	      Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb 7b 7d Nokb 35 Nokb Nokb Nokb Nokb   ;1
	      31 12 27 14 15 17 1a 27 21 1e 1c 18 2b 1B 2f 2c   ;2
	      1d 12 13 14 15 17 16 1a 1c 19 29 29 2b 18 2f 2c   ;3
	      13 00 0b 08 02 0e 03 05 04 22 26 28 25 2e 2d 1f   ;4
	      23 0c 0f 01 11 20 09 0d 07 10 06 77 2a 79 16 1b   ;5
	      32 00 0b 08 02 0e 03 05 04 22 26 28 25 2e 2d 1f   ;6
	      23 0c 0f 01 11 20 09 0d 07 10 06 21 2a 1e 32 nokb   ;7
	      Nokb 6b 6d Nokb 64 Nokb 72 Nokb Nokb Nokb 75 Nokb 65 24 Nokb Nokb   ;8
	      Nokb 69 71 62 6f 7a 78 63 76 Nokb Nokb Nokb Nokb 60 61 67   ;9
	      Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb   ;a
	      Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb   ;b
	      Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb   ;c
	      Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb   ;d
	      Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb   ;e
	      Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb Nokb))   ;f

  "This table maps an Explorer keyboard code to the Mac keyboard code.")
(defvar keystate-acb nil)

(DEFUN key-state (key &aux tem)
  "T if the specified key  is now depressed.  
KEY is the name of a shift key, or the character for a non-shift key.
Names allowed are :SHIFT, :SYMBOL, :CONTROL, :META, :SUPER, :HYPER, or
any of those with LEFT- or RIGHT-, as in :LEFT-SHIFT, and two other names
that do not allow LEFT- or RIGHT- : :MODE-LOCK and :CAPS-LOCK.  :SHIFT
is T if either shift key is depressed; :LEFT-SHIFT and :RIGHT-SHIFT test
one individual shift key.  For the locking keys, it is the
depressedreleased state of the key which is being tested, not the state
of the lock."
  (DECLARE (SPECIAL si:*kbd-zmacs-mode*)) ;; may 01/26/89 for build process
   (COND
    ((OR (NUMBERP key)
	 (CHARACTERP key))
     (WHEN (CHARACTERP key) 
       (SETQ key (CHAR-INT key)))
     (IF (<= 0 key (1- (ARRAY-TOTAL-SIZE si::kbd-key-state-array)))
	 (if (si:addin-p)
	     (progn
	       (SETQ tem (AREF *explorer-to-mac-kbd-code-map* key))
	       (when (NOT (= tem nokb))  ; only send command if key possible
		   (let ((acb (add:get-acb-fast 2)) 
			 (ch  (add:find-channel si:%Chan-Type-Misc-Addin)))
		     (unwind-protect
			 (progn
			   (add:init-acb acb si:%MC-tvcalls si:%TC-KEYSTATE)
			   ; Execute
			   (add:set-parm-8b acb 0 tem)
			   (add:transmit-packet-and-wait acb ch)
			   (add:check-error acb)
			   (not (zerop (add:parm-8b acb 0))))
		       (setf (add:requestor-complete acb) t)
		       (add:return-acb-fast acb)))
		   ))
	       (NOT (ZEROP (AREF si::kbd-key-state-array key))))
	 (FERROR NIL "~D is out of range for a numeric keyboard character, must be between 0 and ~d."
		 key (1- (ARRAY-TOTAL-SIZE si::kbd-key-state-array)))))
    ((IF (AND (si:addin-p) (NOT (EQ si:current-process tv:mouse-process)))
	(SETQ tem
	      (IF si:*kbd-zmacs-mode*  ;; the following values are fixed for the Mac kbd.
		  ;; since no discrete hyper and symbol and mode-lock on mx, use page-up, home and num-lock - respectively
	        (ASSOC key '((:shift   #x38) (:left-shift #x38) (:right-shift #x38)
			(:symbol  #x73) (:left-symbol  #x73) (:right-symbol #x73)
			(:control #x37)	(:left-control #x37) (:right-control #x37)
		  	(:meta    #x3a) (:left-meta #x3a) (:right-meta #x3a)
			(:super   #x3b)	(:left-super #x3b) (:right-super #x3b)
			(:hyper   #x74) (:left-hyper   #x74) (:right-hyper   #x74)
			   (:caps-lock #x39) (:mode-lock   #x47))
		     :test #'EQ)
		(ASSOC key '((:shift   #x38) (:left-shift #x38) (:right-shift #x38)
			(:symbol  #x73) (:left-symbol  #x73) (:right-symbol #x73)
			(:control #x3b)	(:left-control #x3b) (:right-control #x3b)
			(:meta    #x3a) (:left-meta #x3a) (:right-meta #x3a)
			(:super   #x37)	(:left-super #x37) (:right-super #x37)
			(:hyper   #x74) (:left-hyper   #x74) (:right-hyper   #x74)
			   (:caps-lock #x39) (:mode-lock   #x47))
		     :test #'EQ)))
	(SETQ tem
	   (ASSOC key '((:shift   #o100) (:left-shift   0) (:right-shift   #o40)
			(:symbol  #o101) (:left-symbol  1) (:right-symbol  #o41)
			(:control #o104) (:left-control 4) (:right-control #o44)
			(:meta    #o105) (:left-meta    5) (:right-meta    #o45)
			(:super   #o106) (:left-super   6) (:right-super   #o46)
			(:hyper   #o107) (:left-hyper   7) (:right-hyper   #o47)
			(:caps-lock 3) (:mode-lock   #o11))
		  :test #'EQ)))
     (IF (AND (si:addin-p) (NOT (EQ si:current-process tv:mouse-process)))
	 (let ((acb (add:get-acb-fast 2)))  
	   (unwind-protect
	       (progn
		 (add:init-acb acb
			       si:%MC-tvcalls
			       si:%TC-KEYSTATE)
		 (add:set-parm-8b acb 0 (CADR tem))
		 (add:transmit-packet-and-wait acb si:%Chan-Type-Misc-Addin)
		 (add:check-error acb)
		 (not (zerop (add:parm-8b acb 0))))
	     (setf (add:requestor-complete acb) t)
	     (add:return-acb-fast acb)))
	   (LOGTEST (LSH 1 (LOGAND (SETQ tem (CADR tem)) 31))
		    (COND
		      ((< tem 32) si::kbd-left-shifts)
		      ((< tem 64) si::kbd-right-shifts)
		      (t (LOGIOR si::kbd-left-shifts si::kbd-right-shifts))))))
    (t (FERROR NIL "~S illegal key; must be character or symbol for shift key" key))))

(DEFUN setup-keyboard-keyclick (&optional (state nil))
  "Change the click/silent state of the keys on the keyboard.
STATE can be either T or NIL
  NIL - make the keys be silent.
  T   - when a key is pressed or released make an audible
        clicking sound."
  (if (mac-system-p)
      (when state (notify nil "Keyclick is not supported on this machine."))
      (write-keyboard (IF (EQUAL state t)
			  'keyclick-on
			  'keyclick-off))))


(DEFVAR keyboard-transmit-codes
	'((power-up-self-test #x00)
	  (caps-key-led-on    #x01)
	  (caps-key-led-off   #x02)
	  (keyclick-on        #x03)
	  (keyclick-off       #x04)
	  (italic-key-led-on  #x05)
	  (italic-key-led-off #x06)
	  (reset              #x07)
	  (return-version     #x08)
	  (bold-key-led-on    #x09)
	  (bold-key-led-off   #x0a)
	  (mode-key-led-on    #x0b)
	  (mode-key-led-off   #x0c))
  "Mapping table which is used to map transmit names to their
corresponding numbers.  These transmit names are sent to the Explorer
keyboard to tell it to do something.")

(DEFUN write-keyboard (&optional (code-name 'keyclick-off)
		       &aux mapping code-number (slot sib-slot-number))
  "Write a code to the Explorer keyboard."
  ;; Map the code-name into a code-number.
  (SETQ mapping (ASSOC code-name keyboard-transmit-codes :test #'EQ))
  (WHEN (NULL mapping)
    (SETQ mapping (ASSOC 'keyclick-off keyboard-transmit-codes :test #'EQ)))
  (SETQ code-number (CADR mapping))
  (if sib-is-csib
      (WITHOUT-INTERRUPTS
	;; Check to see if the transmit ready and transmit buffer empty bits
	;; are set in the keyboard USART.
	(let ((ready-test #x81)			;buffer empty + transmit ready
	      (transmit-status #x59)
	      (command #x5d))
	  (WHEN (LOGTEST ready-test (%NUBUS-READ-8b slot (+ transmit-status time::keyboard-base)))
	    ;; To write to the keyboard we need to enable the USART transmit mode.
	    (%NUBUS-WRITE-8b slot (+ transmit-status  time::keyboard-base) 1)
	    ;; Here is where we actually write out the code.
	    (%NUBUS-WRITE-8b slot (+ time::keyboard-base command) code-number))))
      (WITHOUT-INTERRUPTS
	;; Check to see if the transmit ready and transmit buffer empty bits
	;; are set in the keyboard USART.
	(WHEN (LOGTEST 5 (%NUBUS-READ slot time::keyboard-base))
	  ;; To write to the keyboard we need to enable the USART transmit mode.
	  (%NUBUS-WRITE slot time::keyboard-base 1)
	  ;; Here is where we actually write out the code.
	  (%NUBUS-WRITE slot (+ time::keyboard-base 4) code-number)))
      ;; Make sure we can still read characters by enabling the USART receive mode.
      (%NUBUS-WRITE slot time::keyboard-base 4))) 


;;; "TERM key"

; Unknown or misspelled keywords are ignored.
(DEFPARAMETER *terminal-keys*
     '((#\CLEAR kbd-terminal-clear "Discard type-ahead" :keyboard-process)
       (#\RESUME (kbd-terminal-resume) 
	"Allow deexposed typeout in window that TERM-0-S would select.")
       (#\c-UNDO (restore-default-colors selected-window)
	"Restore all default colors for the selected window.")
       (#\UNDO (download-color-defaults)
	"Temporarily download default foreground, background, and color map.")
#-EXPLORER (#\GREATER-OR-EQUAL system-menu-set-mouse-screen "Set Mouse screen")
       (#\FORM (kbd-screen-redisplay)
	"Clear and redisplay all WINDOWS")
       (#\c-page (kbd-screen-redisplay-some)
	"Clear and redisplay SOME SCREENS.")
       (#\A kbd-terminal-arrest
	"Arrest process in status line (minus means unarrest)" :keyboard-process)
       (#\B kbd-bury
	"Bury the selected window" :typeahead)
       (#\C kbd-complement
	'("Complement video black-on-white state"
	  "  With an argument, complement the who-line documentation window")
	:keyboard-process)
       (#\E select-interesting-window "Selects an error background window.")
       (#\G (kbd-gc-status) "Show the current state of all garbage collection.")
;; The following two commands were moved to net;servers.lisp to remove 
;; dependency on chaos from the window system - RAF       
;;       (#\H (KBD-HOSTAT) "Show status of CHAOSnet hosts" :TYPEAHEAD)
;;       (#\F KBD-FINGER (FINGER-ARG-PROMPT) :TYPEAHEAD)
       (#\I kbd-terminal-i
	"Selected window deexposed input notify flag (complement, or arg=1 on, 0 off)")
       (#\M kbd-terminal-more
	"Selected window **MORE** enable (complement, or arg=1 on, 0 off)"
	:keyboard-process)
       (#\N kbd-terminal-notifications "Allow notifications to come out."
	"  TERM 1 N  means print all notifications (even old ones)"
	"  TERM 2 N  means defer notifications, reset who-line"
	:typeahead)
       (#\O kbd-other-exposed-window "Select another exposed window" :typeahead)
       (#\R (system-menu-reset-window selected-window nil nil) "Reset process in selected window")
       (#\S kbd-switch-windows
	'("Select the most recently selected WINDOW.  With an argument, select the nth"
	  "  previously selected window and rotate the top n windows.  (Default arg is 2)."
	  "  With an arg of 1, rotate through all the windows."
	  "  With a negative arg rotate in the other direction."
	  "  With an argument of 0, select a window that wants to type out.")
	:typeahead)
       (#\c-s kbd-switch-screens
	'("Select the most recently selected SCREEN.  With an argument, select the nth"
	  "  previously selected screen and rotate the top n screens.  (Default arg is 2)."
	  "  With an arg of 1, rotate through all the screens."
	  "  With a negative arg rotate in the other direction.")
	:typeahead)
       (#\T kbd-terminal-t
	"Selected window deexposed typeout action.  0 - wait, 1 - notify, 2 - permit.")
       (#\U (trace-via-menus) "Trace via menus")
       (#\W kbd-terminal-w
	'("Switch which process the wholine looks at.  Default is just to refresh it"
	  "  0: a menu of all processes       2: freeze on this process         4: rotate other direction"
          "  1: selected-window's process     3: rotate among all processes"))
#-explorer (#\HOLD-OUTPUT kbd-terminal-output-hold "Expose window on which we have /"output hold/"")
       (#\? kbd-terminal-help nil :typeahead)
       (#\HELP kbd-terminal-help nil :typeahead)

       NIL					;Ones after here are "for wizards"
       (#\C-C (kbd-use-cold-load-stream) "Get to cold-load stream" :typeahead)
       (#\C-T kbd-clear-temporary-windows "Clear temporary windows")  ; flush => clear ... kdb 3/26/86
       (#\C-CLEAR kbd-clear-locks "Clear window-system locks")
       (#\C-A kbd-terminal-arrest-all "Arrest nearly all processes" :keyboard-process)

       nil					;Window modification commands
;; doesn't work (#\M-A (system-menu-edit-window-attributes selected-window nil nil) "Edit attributes of selected window")
       (#\M-E terminal-expand-window "Expand the selected window to full screen,
   With argument, expand the window you mouse.")
       (#\M-K (system-menu-kill-window selected-window nil nil) "Kill selected window")
       (#\M-M terminal-move-window "Move window")
       (#\M-R terminal-reshape-window "Reshape window")
       (#\M-S (system-menu-select-window) "Select window from menu")
       )
  "Determines what to do with characters typed after the TERM key.  A
list of elements (CHAR FUNCTION DOCUMENTATION . OPTIONS).  CHAR is what
character this element applies to.  If FUNCTION is a list, it is
evaluated; otherwise, it is called with one arg, which is either NIL or
the numeric arg (1 in TERM 1 F).  The evaluation or calling is normally
done in a separate process.  DOCUMENTATION can be a string, a function
that returns a string, or NIL.  NIL means the this entry will not appear
in the help message.  It can also be a list of strings that go on
separate lines.  OPTIONS are keywords (with no values).  Defined options
are:
    :TYPEAHEAD - copy the contents of the software buffer into the
	currently selected IO-BUFFER.  This has the effect of treating
	everything typed before the TERM as typeahead to the currently
	selected window.  Useful for TERM commands that change the
	selected window.  These commands should set KBD-TERMINAL-TIME to
	NIL as soon as they change the selected window, unless they
	complete quickly (input should never be done with
	KBD-TERMINAL-TIME non-NIL).
    :KEYBOARD-PROCESS - run the function in the keyboard process instead
	of starting a new process for it.")

(DEFVAR saved-format-font nil
  "Place to save the normal font used to display with format.")
(DEFUN save-format-font (&optional ignore)
  "Saves the font used in FORMAT so we can restore it later."
  (SETQ saved-format-font (SEND *standard-output* :current-font)))

(DEFUN change-format-font (&optional (new-font fonts:cptfont))
  "Set or revert the font used in FORMAT.
NEW-FONT can take on one of two values:
  a font - the font to change to
  nil - use the font in saved-format-font"
 (SEND *standard-output* :set-current-font
       (IF new-font new-font
           saved-format-font)
       t))

(DEFUN remove-terminal-key (CHAR &rest ignore)
  "Remove the character CHAR from the list of terminal- keys."
  (SETQ *terminal-keys*
	(DELETE (ASSOC (CHAR-UPCASE (INT-CHAR char)) *terminal-keys* :test #'safe-char=)
		(THE list *terminal-keys*) :test #'EQ)))

(compiler:make-obsolete remove-escape-key "use remove-terminal-key")
;;; Keep the definition around so that people can still use it.
(DEFF remove-escape-key #'remove-terminal-key)


(DEFVAR *user-defined-terminal-keys* nil
  "List of TERMINAL keys that the user has added, so the user won't lose
his definitions.")

(DEFUN add-terminal-key (CHAR function &optional documentation &rest options &aux c entry before during after
			 (state :before))
  "Add CHAR to the list of actions to be performed and are prefaced by
typing TERMINAL.  FUNCTION should be the function to be called when that
key is depressed, DOCUMENTATION is what to show up when the user types
Terminal Help.	 OPTIONS can include either :TYPEAHEAD or
:KEYBOARD-PROCESS, or :SYSTEM meaning this is a redefinition of system
code rather than a user overriding the system code."
  (CHECK-ARG char (OR (CHARACTERP char) (FIXNUMP char)) "character or fixnum" character)
  (CHECK-ARG char #'STRINGP documentation "a valid documentation string")
  (SETQ char (CHAR-UPCASE (INT-CHAR char)))
  (SETQ entry (LIST* char function documentation (COPY-LIST options)))
  (UNLESS (MEMBER :system options :test #'EQ)
    (PUSH entry *user-defined-terminal-keys*))
  ;;remove character from list.
  (remove-terminal-key char)
  ;; Logic: before means we haven't found the alphabetics yet, during
  ;; means we're hacking them now and after means that we are hacking
  ;; the post alphabetics.  we also invert the order, so we are really
  ;; hacking the ones at the end first.
  (DOLIST (item (NREVERSE *terminal-keys*))
    (SETQ c (CAR item))
    (AND (EQ state :during) (NOT (NULL c)) (NOT (ALPHA-CHAR-P c)) (SETQ state :after))
    (AND (EQ state :before) (NOT (NULL c)) (ALPHA-CHAR-P c) (SETQ state :during))
    (CASE state
	  (:after (PUSH item after))
	  (:during (PUSH item during))
	  (:before (PUSH item before))))
  ;;we're all done, now where does that key go
  (IF (ALPHA-CHAR-P char) (PUSH entry during) (PUSH entry before))
  (SETQ *terminal-keys*
	;;alphabatize
	(APPEND after (SORT during #'ALPHALESSP :key #'car) before))
  NIL)						;return something nice.

(compiler:make-obsolete add-escape-key "use add-terminal-key")
;;; Keep the definition around so that people can still use it.
(DEFF add-escape-key #'add-terminal-key)

;;; when you patch sys code, be sure to call this or call
;;; ADD-TERMINAL-KEY with 2nd arg of T.
;;; Put on initialization list ?
(DEFUN redo-user-terminal-modifications ()   
  "Make sure the user gets the changes he wants on the *terminal-keys*"
  (IF (NOT (NULL *user-defined-terminal-keys*))
      (DOLIST (entry *user-defined-terminal-keys*)
	(APPLY #'add-terminal-key entry))))

(DEFVAR *error-windows* nil "List of windows with errors")

(DEFUN find-interesting-window (&optional n)
  (MULTIPLE-VALUE-BIND (nil w)
      (find-process-in-error)
    (COND (w (PUSH w *error-windows*) w)
	  ((CAAR background-interesting-windows))
	  (t (LOOP for window = (NTH (OR n 0) *error-windows*)
		   while window
		   do (IF (SEND window :active-p)
			  (RETURN window)
			(SETQ *error-windows* (DELETE window (THE list *error-windows*) :test #'EQ))))))))

(DEFUN kbd-terminal-resume ()
  "Handle a terminal-resume typed on the keyboard by allowing
interesting window to type out."
  (LET ((w (find-interesting-window)))
    (IF w
	(SEND w :set-deexposed-typeout-action :permit)
      (BEEP))))
(compiler:make-obsolete kbd-esc-resume "use kbd-terminal-resume")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-resume #'kbd-terminal-resume)

(DEFVAR *saved-cold-booted-p* nil
  "Used to tell if we should consider ourselves to be cold booted.")

(DEFVAR *saved-last-activity-time* 0.		; A very long time ago indeed
  "Used to record how long we were idle at some point back in time.")
  

(DEFUN save-idle-info ()
  "Save away the idle info that is currently valid."
  (SETQ *saved-cold-booted-p* si:who-line-just-cold-booted-p
	*saved-last-activity-time* kbd-last-activity-time))

(DEFUN restore-idle-info ()
  "Restore the idle info that was saved away earlier."
  (AND (> *saved-last-activity-time* 0)		; This means we never called save-idle-info
       (SETQ si:who-line-just-cold-booted-p *saved-cold-booted-p*
	     kbd-last-activity-time      *saved-last-activity-time*)))


(DEFVAR KBD-PROCESS NIL)

(DEFUN kbd-terminal (&rest ignore &aux ch arg minus fcn ent tem)
  "Handle Terminal typed on keyboard"
  (save-idle-info)   ;;but something was probably already clobbered, sigh
  (LET-GLOBALLY ((who-line-process current-process))
    (who-line-run-state-update)			;Necessary to make above take effect
    (LOOP with whostate = "terminal-" do
	  (WHEN (OR arg minus)
	    (SETQ whostate (FORMAT nil "Terminal~:[+~;-~]~d-" minus (OR arg 1))))
	  (SETQ ch (CHAR-UPCASE (INT-CHAR (kbd-get-software-char whostate))))
	  (SETF (CHAR-BIT CH :KEYPAD) NIL)	;TURN OFF KEYPAD BIT   PMH
	  (COND ((SETQ tem (DIGIT-CHAR-P ch)) (SETQ arg (+ (* (OR arg 0) 10) tem)))
		((CHAR= ch #\-) (SETQ minus t))
		(t (RETURN)))))
  (who-line-run-state-update)			;Switch LAST-WHO-LINE-PROCESS back
  (WHEN minus (SETQ arg (- (OR arg 1))))
  (COND
    ((SETQ ent (ASSOC ch *terminal-keys* :test #'SAFE-CHAR=))
     (WITHOUT-INTERRUPTS
       (WHEN (MEMBER :typeahead (CDDDR ent) :test #'EQ)
	 (kbd-get-io-buffer)
	 (kbd-snarf-input selected-io-buffer t) (SETQ kbd-terminal-time (TIME))))
     (SETQ fcn (SECOND ent))
     (WHEN (CONSP fcn)
       (SETQ arg fcn fcn #'EVAL))
     (COND
       ((MEMBER :keyboard-process (CDDDR ent) :test #'EQ)
	(FUNCALL fcn arg)
	(SETQ kbd-terminal-time NIL))
       (t
	(PROCESS-RUN-FUNCTION `(:name ,(FORMAT NIL "Handle terminal-~:c" ch) :priority ,(1+ *selected-process-priority*))
			      #'(lambda (fcn
					 arg)
				  (LET ((kbd-last-activity-time kbd-last-activity-time))
				    (FUNCALL fcn arg))
				  (SETQ kbd-terminal-time NIL)
				  (restore-idle-info))
			      fcn arg))))
    ;; quote asynchronous characters
    ((OR
       (AND (SETQ tem (kbd-get-io-buffer))
	    (ASSOC ch
		   (GETF (io-buffer-plist tem) :asynchronous-characters
			 kbd-standard-asynchronous-characters)
		   :test #'CHAR=))
       (ASSOC ch kbd-global-asynchronous-characters :test #'CHAR=))
     (UNLESS (io-buffer-full-p kbd-io-buffer)
       (io-buffer-put kbd-io-buffer ch)))	;((SETQ TEM (ASSQ (set-char-bit (char-code CH) :control t)
						;           KBD-STANDARD-ASYNCHRONOUS-CHARACTERS))
						; (APPLY (CADR TEM) (CAR TEM) TV:SELECTED-WINDOW (CDDR TEM)))
    ((NOT (EQL ch #\rubout)) (BEEP)))		; This used to be NEQ PMH 7/1/87
  ;; No unwind-protect needed -- this is for good measure
  (restore-idle-info))
 
(compiler:make-obsolete kbd-esc "use kbd-terminal")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc #'kbd-terminal)

(DEFUN kbd-complement (ARG)			;TERM C
  (IF arg
      (SEND who-line-documentation-window :set-reverse-video-p
	       (NOT (SEND who-line-documentation-window :reverse-video-p)))
      (complement-bow-mode)))

(DEFUN kbd-terminal-more (ARG)			;TERM M
  (COND (selected-window
	 (SEND selected-window :set-more-p
		  (COND ((NULL arg) (NOT (SEND selected-window :more-p)))
			((< arg 1) nil)		;TERM 0 M, TERM - M, MORE PROC OFF
			(t t))))))		;TERM 1 M, MORE PROC ON
(compiler:make-obsolete kbd-esc-more "use kbd-terminal-more")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-more #'kbd-terminal-more)

(DEFUN kbd-terminal-clear (tem &optional ignore)
  "Terminal clear-input and c-clear-input."
  (AND (SETQ tem (kbd-get-io-buffer))
       (io-buffer-clear tem))
  (io-buffer-clear kbd-io-buffer))
(compiler:make-obsolete kbd-esc-clear "use kbd-terminal-clear")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-clear #'kbd-terminal-clear)

(DEFUN kbd-terminal-arrest (ARG &aux p)
  (COND ((NULL (SETQ p last-who-line-process)) (BEEP))
	((AND arg (MINUSP arg))
	 (DOLIST (r (SEND p :arrest-reasons))
	   (SEND p :revoke-arrest-reason r)))
	(t (SEND p :arrest-reason :user))))
(compiler:make-obsolete kbd-esc-arrest "use kbd-terminal-arrest")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-arrest #'kbd-terminal-arrest)

(DEFUN kbd-terminal-arrest-all (ARG)
  (DOLIST (p all-processes)
    (OR (NULL p)
	(EQ p kbd-process)
	(EQ p mouse-process)
	(EQ p screen-manager-background-process)
	(EQ p last-who-line-process)
	(SEND p
	      (IF (AND arg (MINUSP arg))
		  :revoke-arrest-reason :arrest-reason)
	      :user))))
(compiler:make-obsolete kbd-esc-arrest-all "use kbd-terminal-arrest-all")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-arrest-all #'kbd-terminal-arrest-all)

(DEFUN kbd-bury (ARG)				;TERM B
  ARG						;unused for now
  (COND (selected-window
	 (SEND (SEND selected-window :alias-for-selected-windows) :bury)))
  (SETQ kbd-terminal-time nil))

(DEFUN kbd-other-exposed-window (IGNORE)
  ;; TERM O selects the least recently-selected window that is exposed.
  ;; Thus repeated TERM O cycles among all the selectable exposed windows 
  ;; on all the screens.  Real useful with split-screen!
  (DO ((i 0 (1+ i))
       (n (ARRAY-TOTAL-SIZE previously-selected-windows))
       (tem)
       (window nil))
      ((>= i n)
       (IF window (SEND window :mouse-select)
	   (BEEP)))
    (AND (SETQ tem (AREF previously-selected-windows i))
	 (EQ (SEND tem :status) :exposed)
	 (NOT (NULL (SEND tem :name-for-selection)))
	 (SETQ window tem)))) 

(DEFUN kbd-switch-windows (ARG &aux tem)	;TERM S
  ;; TERM n S rotates the n most recently selected windows, selecting the nth
  ;; TERM S = TERM 2 S
  ;; TERM 1 S selects the next most recent window but rotates all the windows
  ;; TERM -n S rotates the same set of windows in the other direction
  ;; TERM 0 S selects a window which has an error pending (or otherwise wants attention)
  (OR arg (SETQ arg 2))
  (COND ((= arg 0) (select-interesting-window))
	(t (DELAYING-SCREEN-MANAGEMENT		;Inhibit auto-selection
	     (COND ((SETQ tem selected-window)	;Put current window on front of array
		    (SEND tem :deselect nil)
		    (AND (SETQ tem (SEND tem :io-buffer))
			 (kbd-snarf-input tem t))))
	     (WITHOUT-INTERRUPTS		;Get rid of any non-mouse-selectable ones
	       (DOTIMES (i (ARRAY-TOTAL-SIZE previously-selected-windows))
		 (OR (SETQ tem (AREF previously-selected-windows i)) (RETURN))
		 (COND ((NOT (SEND tem :name-for-selection))
			(remove-from-previously-selected-windows tem)
			(SETQ i (1- i)))))
	       (rotate-top-of-array previously-selected-windows arg))
	     (AND (SETQ tem (AREF previously-selected-windows 0))
		  (SEND tem :mouse-select))
	     NIL))))

(COMMENT
(DEFUN kbd-status (IGNORE &aux w tem)
  "Display what the effects of typing terminal-N-s would be."
  ;;use terminal s's logic
  ;; TERM n S rotates the n most recently selected windows, selecting the nth
  ;; TERM S = TERM 2 S
  ;; TERM 1 S selects the next most recent window but rotates all the windows
  ;; TERM -n S rotates the same set of windows in the other direction
  ;; TERM 0 S selects a window which has an error pending (or otherwise wants attention)
  (USING-RESOURCE (window pop-up-finger-window)
    (SETF (sheet-truncate-line-out-flag window) 1)
    (SEND window :set-label "Information on recently selected windows.")
    (SEND window :set-process current-process)
    (window-call (window :deactivate)
      (LET ((*terminal-io* window))
	(SETQ kbd-terminal-time nil)
	(FORMAT window "To select one of the windows below, type „ <number> s~&")
	(DO ((i 1 (1+ i)))
	    ((= i (ARRAY-LENGTH previously-selected-windows)))
	  (OR (SETQ w (AREF previously-selected-windows i)) (RETURN))
	  (IF (SETQ tem (SEND w :name-for-selection))
	      (FORMAT window "~2D: ~a~&" (1+ i) tem)
	    (remove-from-previously-selected-windows w)
	    (SETQ i (1- i))))
	(IF (NULL (SETQ w (find-interesting-window)))
	    (FORMAT window "~%[There is currently no /"interesting/" window]~&")
	  (FORMAT window
		  "~%~A is an /"interesting/" window~%   Type „-0-S to select it.~&"
		  (SEND w :name-for-selection)))
	(SETQ w (AREF previously-selected-windows 0))
	(IF (NULL w)
	    (FORMAT window "~&There is currently no window selected.")
	  (FORMAT window "The currently selected window is ~:[~S~;~:*~a~]"
		  (SEND w :name-for-selection) w))
	(FORMAT window "~2%~a" *remove-typeout-standard-message*)
	(PROCESS-WAIT *DEFAULT-READ-WHOSTATE* #'(lambda (window)
				     (NEQ selected-window window)
				     (SEND window :tyi-no-hang))
		      window)))))
) ;; end comment

;This is like ZWEI:ROTATE-TOP-OF-LIST but for a NIL-padded array
;Rotate nth (1-origin!) element to the front of the array, rotating the
;part of the array before it.  With a negative arg rotate the same amount
;backwards.  With an arg of 1 rotate the whole array BACKWARDS, i.e. bring
;up the same element as with an arg of 2 but store the old front at the back.
;Zero arg is undefined, do nothing I guess.  Note that 2 and -2 do the same thing.
;Doesn't barf if N is too big.
(DEFUN rotate-top-of-array (ARRAY n &aux (LENGTH (ARRAY-TOTAL-SIZE array)))
  (DO ()
      ((ZEROP length))
    (AND (AREF array (1- length)) (RETURN))
    (SETQ length (1- length)))
  (AND (= (ABS n) 1) (SETQ n (* n -1 length)))
  (COND
    ((PLUSP n) (SETQ n (MIN length n))
     (DO ((i 0 (1+ i))
	  (NTH (AREF array (1- n)) old)
	  (old))
	 ((>= i n))
       (SETQ old (AREF array i))
       (SETF (AREF array i) nth)))
    ((MINUSP n) (SETQ n (MIN length (- n)))
     (DO ((i 1 (1+ i))
	  (front (AREF array 0)))
	 ((>= i n)
	  (SETF (AREF array (1- i)) front))
       (SETF (AREF array (1- i)) (AREF array i)))))
  array)

(DEFUN kbd-screen-redisplay ()
  "Like SCREEN-REDISPLAY, but goes over windows by hand, and never waits for a lock."
  (DOLIST (screen all-the-screens)
    (COND ((sheet-exposed-p screen)
	   (DOLIST (i (sheet-exposed-inferiors screen))
	     (AND (sheet-can-get-lock i)
		  (SEND i :refresh)))
	   (SEND screen :screen-manage))))
  (who-line-clobbered))

(DEFUN kbd-clear-locks (IGNORE)			;TERM C-CLEAR
  (kbd-clear-temporary-windows nil)		;First flush any temporary windows
  (sheet-clear-locks))

(DEFUN kbd-clear-temporary-windows (IGNORE)	;TERM C-T
  (map-over-sheets #'(lambda (sheet)
		       (AND (sheet-temporary-p  sheet)
			    (sheet-exposed-p    sheet)
			    (sheet-can-get-lock sheet)
			    (CATCH-ERROR (SEND sheet :deexpose) nil)))))

(DEFUN kbd-use-cold-load-stream ()
  (DECLARE (SPECIAL sys:cold-load-stream sys:cold-load-stream-owns-keyboard))
  (LET-GLOBALLY ((sys:cold-load-stream-owns-keyboard t))
    (BLOCK top
      (eh:save-screen-for-cold-load-stream)
      (DOLIST (w locked-error-windows)
	(LET ((*query-io* sys:cold-load-stream))
	  (WHEN (FQUERY NIL "Handle error in ~S in cold load stream?" w)
	    ;; Deleting the window from the list wakes up the process that
	    ;; is waiting for an unlocked window.
	    (eh:restore-screen-for-cold-load-stream t)
	    (RETURN-FROM top
	      (SETQ locked-error-windows
		    (DELETE w (THE list locked-error-windows) :test #'EQ))))))
      (UNWIND-PROTECT (CATCH-ERROR-RESTART
			((abort error) "Exit from the cold-load-stream breakpoint.")
			(LET ((inhibit-scheduling-flag nil)	  ;NIL or BREAK would complain
			      (*terminal-io* sys:cold-load-stream))
			  (FORMAT *terminal-io* "~&Package ~a." (PACKAGE-NAME *package*))
			  (BREAK "using cold load stream.")))
	(eh:restore-screen-for-cold-load-stream))))) 

(DEFUN kbd-terminal-output-hold (IGNORE)
  (DECLARE (SPECIAL sys:cold-load-stream sys:cold-load-stream-owns-keyboard))
  (PROG (p w locked ans)
    (COND ((AND (SETQ p last-who-line-process)
		(MEMBER (PROCESS-WHOSTATE p) '("Output hold" "lock" "Window lock") :test 'EQUALP)
		(TYPEP (SETQ w (CAR (PROCESS-WAIT-ARGUMENT-LIST p))) 'sheet))
	   ;; Bludgeon our way past any deadlocks, e.g. due to the process P holding
	   ;; the lock on the window we are trying to expose, or on something we need
	   ;; to de-expose in order to expose it.  This code probably doesn't do a good
	   ;; enough job explaining what is going on to the user.
	   (COND ((AND (CONSP (sheet-lock w))	;Only temp-locked?
		       (ZEROP (sheet-lock-count w))
		       (LOOP for tw in (sheet-lock w)
			     always (sheet-can-get-lock tw)))
		  (sheet-free-temporary-locks w))
		 ((OR (NOT (sheet-can-get-lock (SETQ locked w)))
		      (AND (sheet-superior w)
			   (LOOP for i in (sheet-exposed-inferiors (sheet-superior w))
				 thereis (AND (sheet-overlaps-sheet-p w i)
					      (NOT (sheet-can-get-lock (SETQ locked i)))))))
		  (LET-GLOBALLY ((sys:cold-load-stream-owns-keyboard t))
		    (eh:save-screen-for-cold-load-stream)
		    (LET ((*query-io* sys:cold-load-stream))
		      (FORMAT *query-io* "Cannot expose ~S because~@
					~:[~S~;~*it~] is locked by ~s."
			      w (EQ w locked) locked (sheet-lock locked))
		      (WHEN (AND (TYPEP (sheet-lock locked) 'si:process)
				 (NOT (sheet-exposed-p w))
				 (FQUERY NIL "Attempt to expose, pretending to be ~S? "
					 (sheet-lock locked)))
			(eh:restore-screen-for-cold-load-stream t)
			(LET ((result (expose-locked-window w (sheet-lock locked))))
			  (WHEN (EQ result t)
			    (RETURN NIL))
			  (eh:save-screen-for-cold-load-stream t)
			  (IF result
			      (FORMAT *query-io* "~&That got an error:~%~a" result)
			      (FORMAT *query-io* "~&That did not finish in 10 seconds."))))
		      (SETQ ans (FQUERY '(:choices (((t "yes.")
						     #\y #\space
						     #\t)
						    ((nil "no.")
						     #\n #\rubout)
						    ((EH "To debugger.")
						     #\d #\e))
						   :beep t)
				 "Forcibly unlock all window-system locks? "))
		      (eh:restore-screen-for-cold-load-stream t)))
		  (COND ((EQ ans 'EH)
			 (SETQ *debug-io* sys:cold-load-stream)
			 (SEND p :interrupt %error-handler-stack-group '(:break))
			 (RETURN NIL))		;Don't try to expose
			(ans (sheet-clear-locks))))
		 ((AND (sheet-exposed-p w)	;This can happen, I don't know how
		       (NOT (sheet-lock w))
		       (sheet-output-held-p w))
		  (eh:save-screen-for-cold-load-stream)
		  (IF (LET ((*query-io* sys:cold-load-stream))
			(FQUERY '(:beep t)
				"~S is output-held for no apparent reason.~@
				 If you know the circumstances that led to this, please~@
				 mail in a bug report describing them.  ~
				 Do you want to forcibly clear output-hold? "
				w))
		      (SETF (sheet-output-hold-flag w) 0))
		  (eh:restore-screen-for-cold-load-stream t)))
	   (SEND w :expose))
	  ((BEEP))))) 
(compiler:make-obsolete kbd-esc-output-hold "use kbd-terminal-output-hold")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-output-hold #'kbd-terminal-output-hold)

(DEFUN expose-locked-window (window locking-process)
  (LET* ((result (LIST NIL))
	 (process
	  (PROCESS-RUN-FUNCTION "Expose locked window"
				#'(lambda (window
					   current-process
					   result)
				    (CONDITION-CASE (ERROR)
					(SEND window :expose)
				      (ERROR (SETF (CAR result) error))
				      (:no-error (SETF (CAR result) t))))
				window locking-process result)))
    (PROCESS-WAIT-WITH-TIMEOUT "Expose window" 600 'CAR result)
    (UNLESS (CAR result)
      (SEND process :reset))
    (CAR result)))  


;; This used to be a macro, but foo.
(DEFUN await-user-typeahead (window)
  (SEND window :clear-input)			;I prefer to lose this way.
  (FORMAT window "~&~%~a" *remove-typeout-standard-message*)	  ;well any character, anyway
  (READ-CHAR window))


(DEFVAR notification-history nil)		;Each entry is list of time and string

(DEFUN kbd-terminal-notifications (ARG &aux nn tem)	 ;TERM N
  "Display notifications in a pop-up window.
With no argument, pending notifications only are printed. If there are none, we say so.
With an argument of T, pending notifications only are printed.
  If there are none, we return immediately.
With an argument of 1, displays all notifications.
With an argument of 2, prints nothing, but marks pending
 notifications as no longer pending."
  ;;do this before selecting new window -- don't get screwed by the wait-to-notify process
  (WITHOUT-INTERRUPTS
   (SETQ nn (APPEND pending-notifications deferred-notifications)
	 deferred-notifications NIL
	 pending-notifications NIL))
  (IF (CASE arg
	((nil) t)
	((t) nn)
	(1 t)
	(2 (SETQ deferred-notifications nn) nil))
      (USING-RESOURCE (window pop-up-finger-window)
	(SETF (sheet-truncate-line-out-flag window) 0)
	(SEND window :set-label "Notifications -- most recent first")
	(window-call (window :deactivate)
	  (SETQ kbd-terminal-time NIL)
	  ;;crock to let us catch more notifications while this window is exposed
	  (DO () (nil)
	    (WITHOUT-INTERRUPTS
	      (SETQ nn (APPEND pending-notifications nn)
		    pending-notifications NIL))
	    (SEND window :clear-screen)
	    (FORMAT window "~:[[There are no pending notifications]~;New notifications:~]~%"
		    nn)
	    (SETQ tem notification-history)
	    (IF nn
		(DOLIST (n nn)
		  (time:print-brief-universal-time (FIRST n) window)
		  (FORMAT window ": ~a~&" (SECOND n))
		  (POP tem)))			;don't duplicate messages -- unseen messages are also in tem
	    (WHEN arg
	      (FORMAT window "~%~:[[No Notifications]~;Previous notifications:~]~%" tem)
	      (DOLIST (n tem)
		(time:print-brief-universal-time (FIRST n) window)
		(FORMAT window ": ~a~&" (SECOND n))))
	    (FORMAT window "~%~a" *remove-typeout-standard-message*)
	    (PROCESS-WAIT *DEFAULT-READ-WHOSTATE*
			  #'(lambda (window)
			      (OR pending-notifications
				  (NEQ selected-window window)
				  (SEND window :listen)))
			  window)
	    (OR pending-notifications
		(NEQ selected-window window)
		(SEND window :tyi-no-hang))
	    (OR pending-notifications
		(RETURN t)))))))
(compiler:make-obsolete kbd-esc-notifications "use kbd-terminal-notifications")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-notifications #'kbd-terminal-notifications)

;(DEFUN KBD-TERMINAL-NOTIFICATIONS (&REST IGNORE)
;  (LET ((LL (FIND-WINDOW-OF-FLAVOR 'NOTIFICATION-MIXIN)))
;    (WHEN LL (SEND LL :SELECT))))

(DEFUN kbd-gc-status ()
  "Show the user the state of the garbage collector."
  (USING-RESOURCE (window pop-up-finger-window)
    (SETF (sheet-truncate-line-out-flag window) 1)
    (SEND window :set-label "GC status")
    (SEND window :set-process current-process)
    (window-call (window :deactivate)
      (LET ((*terminal-io* window))
	(SETQ kbd-terminal-time nil)
	(si:gc-status window)
	(await-user-typeahead window)))))

(DEFUN kbd-terminal-i (ARG)
  "Control the selected window's deexposed type-IN action.
0 => wait, 1 => notify."
  (COND
    ((NOT (OR (NULL arg) (<= 0 arg 1))) (BEEP))
    ((NULL selected-window) (BEEP))
    (t
     (LET ((current-action (SEND selected-window :deexposed-typein-action)))
       (CASE arg
	 ((nil)
	  (SEND selected-window :set-deexposed-typein-action
		(IF (EQ current-action :notify) :normal :notify)))
	 (0 (SEND selected-window :set-deexposed-typein-action :normal))
	 (1 (SEND selected-window :set-deexposed-typein-action :notify))))))) 
(compiler:make-obsolete kbd-esc-i "use kbd-terminal-i")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-i #'kbd-terminal-i)

(DEFUN kbd-terminal-t (ARG)
  "Control the selected window's deexposed typeout action.
0 => wait, 1 => notify, 2 => permit."
  (COND
    ((NOT (OR (NULL arg) (<= 0 arg 2))) (BEEP))
    ((NULL selected-window) (BEEP))
    (t
     (LET ((current-action (SEND selected-window :deexposed-typeout-action)))
       (IF (NOT (MEMBER current-action '(:notify :normal :permit) :test #'EQ))
	   (BEEP)				;If it's something funny, don't change it!
	   (CASE arg
	     ((nil)
	      (SEND selected-window :set-deexposed-typeout-action
		    (IF (EQ current-action :notify) :normal :notify)))
	     (0 (SEND selected-window :set-deexposed-typeout-action :normal))
	     (1 (SEND selected-window :set-deexposed-typeout-action :notify))
	     (2 (SEND selected-window :set-deexposed-typeout-action :permit)))))))) 
(compiler:make-obsolete kbd-esc-t "use kbd-terminal-t")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-t #'kbd-terminal-t)

(DEFUN kbd-terminal-w (ARG &aux proc)
  (SETQ proc last-who-line-process)
  (CASE arg
	(nil (SEND who-line-screen :refresh))
	(0 (SETQ who-line-process
		 (LET ((alist (MAPCAR #'(lambda (p)
					  (CONS (PROCESS-NAME p) p))
				      all-processes)))
		   (w:menu-choose alist :LABEL "Who-line process:" :DEFAULT-ITEM (RASSOC proc alist :test #'EQUAL)))))
	(1 (SETQ who-line-process NIL))
	(2 (SETQ who-line-process proc))
	(3 (SETQ who-line-process
		 (DO ((l all-processes (CDR l)))
		     ((NULL l)
		      (CAR all-processes))
		   (AND (EQ (CAR l) proc) (RETURN (OR (CADR l) (CAR all-processes)))))))
	(4 (SETQ who-line-process
		 (OR
		   (DO ((l all-processes (CDR l))
			(ol nil l))
		       ((NULL l)
			nil)
		     (AND (EQ (CAR l) proc) (RETURN (CAR ol))))
		   (CAR (LAST all-processes))))))
  (who-line-run-state-update)
  (who-line-update)) 

(compiler:make-obsolete kbd-esc-w "use kbd-terminal-w")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-w #'kbd-terminal-w)


(DEFRESOURCE hardcopy-bit-array-resource ()
  :constructor (MAKE-ARRAY `(,main-screen-width			;Big enough for
				 ,(+ main-screen-height 64.))	; for (SET-TV-SPEED 60.)
				 :element-type (sheet-array-type-cl default-screen))
  :initial-copies 0)

(DEFUN snapshot-screen (from-array to-array &optional width height)
  (WITHOUT-INTERRUPTS
    (COND ((ARRAYP from-array)
	   (OR width  (SETQ width  (ARRAY-DIMENSION from-array 1)))
	   (OR height (SETQ height (ARRAY-DIMENSION from-array 0))))
	  (t
	   (OR width  (SETQ width  (sheet-width  from-array)))
	   (OR height (SETQ height (sheet-height from-array)))
	   (SETQ from-array (OR (sheet-screen-array from-array)
				(FERROR nil "Window ~S does not have an array" from-array)))))
    (who-line-update)
    (BITBLT alu-setz (ARRAY-DIMENSION to-array 1) (ARRAY-DIMENSION to-array 0)
	    to-array 0 0 to-array 0 0)
    (BITBLT alu-seta width height from-array 0 0 to-array 0 0))
  (VALUES to-array width height))

(DEFUN kbd-terminal-help (IGNORE &aux doc (indent 17))
  "Provides a help screen when one presses term-help."
  (USING-RESOURCE (window pop-up-finger-window)
    (SETF (sheet-truncate-line-out-flag window) 0)
    (SEND window :set-label "Keyboard documentation")
    (window-mouse-call (window :deactivate)
      (SETQ kbd-terminal-time NIL)
      (FORMAT window " Press TERM~47T~vq~vqHelp Screen for the ~vqTERM~vq key~vq ~
 ~% Then:           In order to:

 ~:C~VTDo nothing. (Use this if you typed TERM by accident and want to cancel it.)
 0-9, -~VTGive a numeric argument to following command~%"
	      NIL 'save-format-font fonts:hl12b
	      'change-format-font fonts:hl12bi
	      'change-format-font fonts:hl12b 'change-format-font
	      NIL 'change-format-font
	      ;#\TERMINAL
	      #\rubout indent
	      ;#\TERMINAL
	      indent)
      (DOLIST (x *terminal-keys*)
	(COND ((NULL x)
	       (SETQ indent 20)
	       (FORMAT window "~%"))
	      ((SETQ doc (eval (CADDR x)))
	       (FORMAT window " ~:@c" (CAR x))
	       (print-string-with-indentation
		 window
		 (IF (ATOM doc) doc (CAR doc))
		 indent)
	       (OR (ATOM doc)
		   (DOLIST (line (CDR doc))
		     (print-string-with-indentation window line indent))))))
      ;; Documentation alert!!  A lot of thought went into the following keystroke
      ;; description.  A main concern is to minimize the number of lines taken up by the
      ;; text.  Three columns seems to fit this well, and even though this squeezes the text
      ;; together, the use of columns helps to visually separate the items from one another.
      ;; There are two spaces between the end of the longest column and the start of the
      ;; following column.  Any more than that will result in wrap-around. 
      ;;
      ;; The ordering was carefully chosen.  The SYSTEM key and HELP keys are at extreme
      ;; boundaries, which should aid the user in locating these important keys.  A key
      ;; hidden in the middle will be more difficult for the user to visually locate.
      ;; Collectively, keys SYSTEM, TERM and ABORT are very important, hence their position
      ;; as the first three items in the first column.  Likewise, the SYSTEM, BREAK and
      ;; RESUME keys are an important collection; grouping them as the first row emphasizes
      ;; this importance. 
      ;;
      ;; Other major groupings are the ABORT and BREAK key sequences, each of which are
      ;; collected in a column.  Note that with a total of 6 rows and 4 items in a major
      ;; grouping, one of them (ABORT was chosen) fits best in the first column.  The
      ;; alternative was to put each major grouping in its own column, but this breaks up
      ;; the column-row importance discussed in the previous paragraph.
      ;;
      ;; The minor groupings for the CLEAR and STATUS key sequences are also collected in a
      ;; column.  The location of these groupings did not seem to matter much, since they
      ;; are minor key sequences.  The location remaining key sequences were placed where
      ;; there was room.
      (FORMAT window "~%~47T~vq~vqHelp for other keyboard function keys:~vq
 SYSTEM           Select a Program         BREAK            Enter read-eval-print loop  RESUME       Continue from Break/Error
 TERM             The above commands       CTRL-BREAK       BREAK now                   STATUS       Print Input History
 ABORT            Throw to command level   META-BREAK       Enter Debugger              CTRL-STATUS  Print Kill History
 CTRL-ABORT       To command level now     CTRL-META-BREAK  Enter Debugger now          NETWORK      Telnet commands
 META-ABORT       Throw out of all levels  CLEAR INPUT      Erase type-in               END          Terminate input
 CTRL-META-ABORT  Out of all levels now    CLEAR SCREEN     Refresh screen              HELP         Print documentation"
	      NIL 'save-format-font fonts:hl12b
	      'change-format-font NIL 'change-format-font)
      (await-user-typeahead window)))) 

(compiler:make-obsolete kbd-esc-help "use kbd-terminal-help")
;;; Keep the definition around so that people can still use it.
(DEFF kbd-esc-help #'kbd-terminal-help)

(DEFUN print-string-with-indentation (STREAM string indent &aux pos)
  "Print STRING on STREAM, indenting each line in STRING by indent."
  (DO ((start 0)
       (end (LENGTH string)))
      ((>= start end))
    (FORMAT stream "~vt" indent)
    (SETQ pos (POSITION #\newline (THE string (STRING string)) :start start :test #'CHAR-EQUAL))
    (PRINC (SUBSEQ string start pos) stream)
    (TERPRI stream)
    (SETQ start (IF pos (1+ pos) end))))  

(DEFVAR *system-keys*
	;; Simple-lisp-listener will be replaced here with UCL listener when it is built.
	'((#\L listener-mixin "Lisp Listener - evaluate Lisp forms." simple-lisp-listener))
  "Defines keys you can press after the System key.                     ;; type -> press 4/16/86 kdb
Each element is a list containing the following:
     KEY         is the character pressed after SYSTEM.
     WINDOW-OR-FLAVOR is a flavor name, meaning look for a window of that flavor,
                 a window, meaning select that window,  an expression
                 which returns either a window to select or a flavor name
     DOCUMENTATION is the documentation string in system-help
     CREATE tells what should be done in the case of SYSTEM C-key
     SYSTEM refers to the system whose defsystem contains the information
                 on how to access the system and documentation
     NAME is the string that appears on the system menu
     COLUMN tells what column on the system menu the system name
            should appear on.  
      ")

(DEFSTRUCT (SYSTEM-KEY (:TYPE :LIST))
  CHAR
  WINDOW-OR-FLAVOR
  DOCUMENTATION
  CREATE
  SYSTEM
  PRINT-NAME
  COLUMN)

(DEFUN remove-system-key (CHAR &OPTIONAL (SYSTEM-NAME NIL))
  "Remove any definition for CHAR typed after the System key."
  (WHEN CHAR (SETQ char (CHAR-UPCASE (INT-CHAR char))))
  (SETQ *system-keys*
	(DELETE-IF #'(lambda (ELT) (AND (EQL (SYSTEM-KEY-CHAR ELT) CHAR)
					(OR (NOT SYSTEM-NAME)
					    (EQL (SYSTEM-KEY-SYSTEM ELT)
						 SYSTEM-NAME) )))
		   *system-keys* :count 1)))



(DEFUN add-system-key (CHAR window-or-flavor documentation &optional (create t) (system nil) (print-name nil) (COLUMN :NONE))
  "Make typing the System key followed by KEY select the application APPLICATION.
APPLICATION may be:
  an actual window
  a name of a flavor of window
  a list to evaluate to get a window or a flavor name
  a system name that has added the system key and menu options through its defsystem.
APPLICATION-DESCRIPTOR is the documentation for a window or flavor or system 
CREATE says whether and how to create a new window if Control-char is pressed.
  It may be:
  T meaning create a window of flavor APPLICATION, or
  the name of a flavor to create, or
  a list to evaluate for effect, to create and select a window.
SYSTEM is the system whose defsystem defines how to access the system and its
  documentation
PRINT-NAME is the string that is used in the system menu for this item
COLUMN is used with the system-name option of the APPLICATION argument and used
  to specify the column in the system menu that this system name should appear
  in.  Use of this option is through W:MODIFY-SYSTEM-ACCESS-SPEC function only.
If CHAR is already defined to select a flavor window, then the old version is
  remembered. To restore the former definition, use (W:REMOVE-SYSTEM-KEY CHAR)"
  
  (declare (arglist key application application-descriptor
		    &optional (create t) (system nil) (print-name nil) (column :none) ))
  
  (WHEN CHAR
    (ASSERT (NOT (CHAR-BIT char :control)) (CHAR)	                                       ;Patch -dlc
	  "The SYSTEM key character you are adding (~:@c) contains a CONTROL bit. ~@
           The CONTROL bit has special meaning for SYSTEM keys (creating new windows)."
	  char))

  (UNLESS PRINT-NAME
    (IF (and SYSTEM (si:find-system-named SYSTEM t t))
      (SETF print-name (si:system-name (si:find-system-named SYSTEM t t)))
      (IF (TYPEP WINDOW-OR-FLAVOR 'TV:ESSENTIAL-WINDOW)
	  (SETF PRINT-NAME (SHEET-NAME WINDOW-OR-FLAVOR))
	  (IF (SYMBOLP WINDOW-OR-FLAVOR)
	      (SETF PRINT-NAME (SYMBOL-NAME WINDOW-OR-FLAVOR))
	      (SETF PRINT-NAME "UN-NAMED")))))

  

  (push (MAKE-SYSTEM-KEY
		      :CHAR (WHEN CHAR
			      (CHAR-UPCASE (INT-CHAR CHAR)))
		      :WINDOW-OR-FLAVOR WINDOW-OR-FLAVOR
		      :DOCUMENTATION   DOCUMENTATION
		      :CREATE CREATE
                      :SYSTEM SYSTEM
		      :PRINT-NAME PRINT-NAME
		      :COLUMN COLUMN)                 
		    *SYSTEM-KEYS*)

  (WHEN (AND COLUMN (NOT (EQL COLUMN :NONE)))
      (ADD-TO-SYSTEM-MENU-COLUMN COLUMN
			  PRINT-NAME
			  NIL 
			  (GETF (SYS:GET-SYSTEM-ACCESS-LIST SYSTEM)
				:DOCUMENTATION)
			  :SORTED PRINT-NAME))
  )

;;; Used to determine whether it is a new-style system key. las 1-6-87




(DEFUN kbd-sys (previous-key in-window &optional preceded-by-TERM-p &aux ch)
  (DECLARE (IGNORE previous-key in-window))
  (LET-GLOBALLY ((who-line-process current-process))
    (who-line-run-state-update)			;Necessary to make above take effect
    (SETQ ch (CHAR-UPCASE (INT-CHAR (kbd-get-software-char "system-")))))
  (who-line-run-state-update)			;Switch LAST-WHO-LINE-PROCESS back
  ;; Anything typed before the System belongs to the currently selected window
  ;; Anything typed after this belongs to the new window we are going to get to.
  (WITHOUT-INTERRUPTS
    (AND (kbd-get-io-buffer)
	 (kbd-snarf-input selected-io-buffer t)))
  (SETQ kbd-terminal-time (TIME))
  (PROCESS-RUN-FUNCTION `(:name "KBD sys" :priority ,(1+ *selected-process-priority*))
			#'kbd-sys-1 ch preceded-by-TERM-p))

(DEFUN kbd-sys-1 (ch &optional create-new-screen-p
		  &aux e w sw makenew flavor-or-window sw-alias
		  (have-screen-for-application-p t) sys-name)
  (SETQ makenew (OR create-new-screen-p
		    (CHAR-BIT ch :control))    ;;Check whether to create a new window  -dlc
	ch (SET-CHAR-BIT ch :control nil))	;Can't have a CONTROL bit in a SYSTEM key  -dlc
  (COND
    ((OR (CHAR= ch #\?) (CHAR= ch #\help))
     (KBD-SYS-HELP))
    ((SETQ e (ASSOC ch *system-keys*))     ;; removed for nil system-keys :test #'CHAR=))
     ;; Find the most recently selected window of the desired type.
     ;; If it is the same type as the selected window, make that the
     ;; least recently selected so as to achieve the cycling-through effect.
     ;; Otherwise the currently selected window becomes the most recently
     ;; selected as usual, and TERM S will return to it.
     ;; In any case, we must fake out :MOUSE-SELECT's typeahead action since
     ;; that has already been properly taken care of and we don't want to snarf
     ;; any characters already typed after the [SYSTEM] command.
     (SETF sys-name (system-key-system e))
     (SETQ flavor-or-window
	   (COND
	     ((and sys-name
		   (SETF sys-name (si:find-system-named sys-name t t)))
	      (W:FIND-SYSTEM-INSTANCE (SYSTEM-KEY-SYSTEM e)
				      MAKENEW create-new-screen-p)
	      nil)
	     ((CONSP (system-key-window-or-flavor e))
	      (eval (system-key-window-or-flavor e)))
	     (t (system-key-window-or-flavor e))))
     (DELAYING-SCREEN-MANAGEMENT		;Inhibit auto selection
       (SETQ sw selected-window)
       (WHEN sw
	 (SETQ sw-alias (SEND sw :alias-for-selected-windows)))
       (COND
	 ((TYPEP flavor-or-window 'essential-window)
	  ;; If the *SYSTEM-KEYS* list has a specific window indicated, use that.
	  (AND sw (SEND sw :deselect NIL)) (SEND flavor-or-window :mouse-select))
	 ;; NIL means he already did whatever he wanted.
	 ((NULL flavor-or-window) nil)
	 
	 ((AND (NOT makenew) (SETQ w (find-window-of-flavor flavor-or-window sw)))
	  ;; Cycle through other windows of this flavor.
	  (WHEN sw
	    (SEND sw :deselect (IF (TYPEP sw-alias flavor-or-window) :end)))
	  (SEND w :mouse-select))
	 ((AND (NOT makenew) sw (TYPEP sw-alias flavor-or-window))
	  ;; There is only one window of this flavor, and this is it.
	  (BEEP))
	 ((NULL (system-key-create e)) (BEEP))		;Cannot create
	 
	 ((ATOM (system-key-create e))
	  ;; Create a new window of this flavor.
	  ;; We create on the default screen.
	  
	  (WHEN (AND (mac-system-p) create-new-screen-p)
	    (SETF have-screen-for-application-p
		  (make-a-Mac-resident-Explorer-screen)))
	  (WHEN have-screen-for-application-p 
	    (AND sw (SEND sw :deselect (IF (TYPEP sw-alias flavor-or-window) :end)))
	    (SEND
	      (make-window (IF (EQ (system-key-create e) t) flavor-or-window (system-key-create e))
			   :superior default-screen
			   ;; Make a color map if color default screen or not.
			   ;; Every screen and window will have one.  02/21/88 KJF.
			   :color-map (copy-color-map (OR (AND default-screen
							       (sheet-color-map default-screen))
							  (AND mouse-sheet
							       (sheet-color-map mouse-sheet))
							  *default-color-map*)))
	      ;; was 02/17/88 KJF
	      ;;			 (if (color-system-p default-screen) ;;; 10/22/87 KWW
	      ;;			     (copy-color-map
	      ;;			       (or (and tv:mouse-sheet (sheet-color-map tv:mouse-sheet))
	      ;;				   *default-color-map*))))
	      :mouse-select)))
	 (t (eval (system-key-create e))))
       NIL))
    ((NOT (CHAR= ch #\rubout)) (BEEP))
    )
  (SETQ kbd-terminal-time NIL))


(DEFUN KBD-SYS-HELP ()
  "Produces type-out window that lists current SYSTEM-keys and explains how to 
    use them.  This is evaluated when SYSTEM HELP or SYSTEM ? is pressed.
    This function will only work if it is COMPILED!"
  (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
     (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 0)
     (SEND WINDOW :SET-LABEL "SYSTEM-key commands")
     (WINDOW-CALL (WINDOW :DEACTIVATE)
       (FORMAT WINDOW
	       "Press ~:@C and one of the following characters to create the first instance~%~
                of or select the most recently used window for the corresponding system.~2%"
	       #\SYSTEM)
       (FORMAT WINDOW
	       "Press ~:@C CTRL and one of the following characters to create a new window~%~
                for the corresponding system.~2%"
	       #\SYSTEM)
       (FORMAT WINDOW
	       "Press ~:@C to do nothing if you pressed ~:@C by accident.~2%"
	       #\RUBOUT #\SYSTEM)
       (LET ((LIST (STABLE-SORT (COPY-LIST *system-keys*) #'ALPHALESSP
				:key #'car))
	     (KEY #\?))
	 (DOLIST (sys-key LIST)
	   (OR (NOT (system-key-char sys-key))
	       (CHAR= KEY
		      (SETQ KEY (system-key-char sys-key)))
		   (FORMAT WINDOW
			   "~&~c~8t~a~26t~a~a" 
			   KEY
			   (SYSTEM-KEY-PRINT-NAME sys-key)
			   " -- "
			   (SYSTEM-KEY-DOCUMENTATION sys-key)))
		    ))
       
       ;; Let kbd process proceed before we TYI.
       (SETQ KBD-TERMINAL-TIME NIL)
       (AWAIT-USER-TYPEAHEAD WINDOW)) ))

;;; ********* Attention ! *********
;;; This function is redefined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;
;;; The find-window-of-flavor function has gotten very complex to handle selection
;;; substitutes properly.  If you want to, you can skip over the documentation alerts.
(DEFUN find-window-of-flavor (flavor &optional (current-window nil))
  "Find a previously selected window whose flavor includes flavor.
If current-window is specified that window will be skipped over."
  (DOTIMES (window-index (ARRAY-TOTAL-SIZE previously-selected-windows))
    (LET ((w (AREF previously-selected-windows window-index)))
      (WHEN (and w (or (not (mac-system-p))
		       (OR (NULL selected-window)
			   (EQ (sheet-get-screen w)
			       (sheet-get-screen selected-window)))))
        ;; Documentation alert!!  We now have a real window.  If W is not a superior
        ;; of current-window then it is a candidate.  If W is a superior of current
        ;; window then we would be finding the window we are already on, so we must go
        ;; on to the next window.
	(WHEN (and (not (sheet-me-or-my-kid-p current-window w)) (TYPEP w flavor) (SEND w :name-for-selection))
          ;; Documentation alert!!  We now have a better candidate window.  Now we
          ;; need to check W's selection substitute, since that will be the window which
          ;; gets selected.  If W's selection substitute is a superior of the current
          ;; window then we skip W and go on to the next window.  If we selected W we
          ;; would go to W's selection substitute and we stay on the window we are
          ;; already on.
          (WHEN  (let ((w w))
		   ;; follow the selection substitute to the end to make sure it is not the current window
		   ;; but be sure and return the original W selection.  PMH 4/6/88
		   (loop (when (sheet-me-or-my-kid-p current-window (send w :selection-substitute))
			   (return nil))
			 (let ((selection-substitute (sheet-selection-substitute w)))
			   (if selection-substitute
			       (setf w selection-substitute)
			       (return T)))))
             (RETURN w)))
        ;; Documentation alert!!  W was not of the proper flavor.  So let's go to
        ;; W's selection substitute.  If W's selection substitute is a superior of the
        ;; current window then we skip W and go on to the next window.  If it isn't
        ;; then we see if it is of the right flavor and has a name.  If so then we
        ;; have found a proper window which has little to do with current window.
	(LET ((wss (SEND w :selection-substitute)))
	  (AND wss (not (sheet-me-or-my-kid-p current-window wss)) (TYPEP wss flavor) (SEND wss :name-for-selection)
               (RETURN wss)))))))

(DEFUN find-window (&optional (SUBSTRING ""))
  "Find a window whose name has SUBSTRING in it.
If more than one window matches SUBSTRING, pop up a menu to choose one.
Note that SUBSTRING can be either a string or a symbol."
  (LET (result
	(search-function (IF (ATOM substring)
                             #'si:simple-string-search
                             ;;ELSE
                             #'si:search-and-or)))
    (map-over-sheets
      #'(lambda (window)
	  (LET ((name (STRING (SEND window :name))))
	    (IF (FUNCALL search-function substring name)        ; Look at name
		(PUSH (LIST name window) result)
                ;;ELSE
                (SETQ name (SEND window :send-if-handles :name-for-selection))
                (IF (AND name (FUNCALL search-function substring name)) ; Then name for selection
                    (PUSH (LIST name window) result)
                    ;;ELSE
                    (SETQ name (SYMBOL-NAME (TYPE-OF window)))
                    (IF (FUNCALL search-function substring name)        ; Finally flavor name
                        (PUSH (LIST name window) result)))))))
    (IF (CDR result)
        ;; If more than one, let user choose.
	(VALUES (w:menu-choose result :label "Pick a window"))  
        ;;ELSE
        (SECOND (FIRST result)))))

;;; Notification (call side)

(DEFUN print-notifications (&optional (from 0) to)
  "Reprint all notifications that have happened, newest first.
If FROM is nonzero, that many of the most recent notifications are skipped.
If TO is non-NIL, we stop after printing the TO'th most recent notification."
  (FORMAT t "~&~:[No notifications.~;Notifications, most recent first:~]~%"
	    notification-history)
  (DO ((nlist notification-history (CDR nlist))
       (i 0 (1+ i)))
      ((OR (NULL nlist)
	   (AND to (>= i to))))
    (UNLESS (< i from)
      (LET ((n (CAR nlist)))
	(time:print-brief-universal-time (FIRST n))
	(FORMAT t " ~a~%" (SECOND n))))))

(sys:declare-suggestions-for 'TV:notify
			 :before
			 '(locally (declare (special sys:*suggestions-menus-on?* sys:function-arglist))
				   (cond (sys:*suggestions-menus-on?*
					   (sys:sugg-before-notify (car sys:function-arglist)))))
			 :use-arglist t)

(DEFUN notify (window-of-interest format-control &rest format-args)
  "Notify the user with an unsolicited message.
The message is generated from FORMAT-CONTROL and FORMAT-ARGS.  If
WINDOW-OF-INTEREST is non-NIL, it is a window to be made available to
Terminal-0-S and maybe another way depending on who prints the
notification."
  (sys:with-suggestions-menus-for tv:notify
  (APPLY #'careful-notify window-of-interest NIL format-control format-args)))

(DEFUN careful-notify (window-of-interest careful-p format-control &rest format-args)
  "Like NOTIFY but will not hang up waiting for locks if CAREFUL-P is T.
If locks are locked or there is no selected-window, returns NIL.  If succeeds
in printing the notification, returns t."
  (LET ((TIME (GET-UNIVERSAL-TIME))
	(message (APPLY #'FORMAT NIL format-control format-args)))
    (PUSH (LIST time message) notification-history)
    (COND (WINDOW-OF-INTEREST			;Make this window "interesting"
	   (WITHOUT-INTERRUPTS
	     (OR (ASSOC window-of-interest background-interesting-windows :test #'EQ)
		 (PUSH (CONS window-of-interest NIL) background-interesting-windows)))
	   (IF (sheet-can-get-lock window-of-interest)	 ;Try to make available to sys menu
	       (SEND window-of-interest :activate))))	 ;but don't bother if locked
    ;;Get a selected-window to which to send the :print-notification message
    (IF (NOT careful-p)
	(IF wait-for-notifications-flag
	    ;; It's going to wait anyway, so don't lock anything!
	    (DO ((sw selected-window selected-window))
		(sw (SEND sw :print-notification time message window-of-interest)
		 t)
	      (PROCESS-WAIT "A selected window" 'SYMBOL-VALUE 'selected-window))
	  ;; What this piece of hair is all about is that we don't want to pick a window
	  ;; to print the notification on and then have that window deexposed out from
	  ;; under us, causing us to hang forever.  So we lock the window while printing
	  ;; the notification, which is assumed is going to be on either the window itself
	  ;; or one of its direct or indirect inferiors.  Any windows which don't print
	  ;; their notification this way must spawn a separate process to do the printing.
	    (LOOP as inhibit-scheduling-flag = t as sw = selected-window
		  when (AND (NOT (NULL sw))
			    (sheet-can-get-lock sw))
		  return (lock-sheet (sw)
			   (SETQ inhibit-scheduling-flag NIL)
			   (SEND sw :print-notification time message window-of-interest))
		  do (SETQ inhibit-scheduling-flag NIL)
		  (PROCESS-WAIT "A selected window"
				#'(lambda (sw)
				    (OR (NEQ selected-window sw)
					(AND sw (sheet-can-get-lock sw))))
				sw)))
      ;; In this case, we simply want to punt if we don't seem to be able to acquire
      ;; the necessary locks.
	(WITHOUT-INTERRUPTS
	  (LET ((sw selected-window))
	    (COND
	      ((OR (NULL sw)			;No one in charge
		   (sheet-output-held-p sw)	;Guy in charge locked or broken
		   (NOT (SHEET-CAN-GET-LOCK	;Anything locked, even by this process,
			  (sheet-get-screen sw) t)))	 ; that would hang Terminal-0-S
	       nil)				;Lose, don't try to notify
	      (T				;Win, go ahead
	       (lock-sheet (sw)
		 (LET ((inhibit-scheduling-flag nil)
		       (wait-for-notifications-flag nil))
		   (SEND sw :print-notification time message window-of-interest)))
	       t))))))) 

;; Note that this stuff is completely different from the LOCKED-ERROR-WINDOWS hack.
;; A window is on there when the window system is locked up against it
;; and its notification cannot be printed.
;; A process is "in-error-p" when its notification HAS been printed.

(DEFUN find-process-in-error (&aux window sg)
  "Return a process waiting for window exposure for error handling.
The value is a process, or nil."
  (WITHOUT-INTERRUPTS
    (DOLIST (p active-processes)
      (AND (SETQ p (CAR p))
	   (TYPEP (SETQ sg (PROCESS-STACK-GROUP p)) 'stack-group)
	   (SETQ window (si:process-is-in-error-p p))
	   (RETURN p window)))))

(DEFUN choose-process-in-error (&aux sg window)
  "Ask user to choose among processes waiting for window exposure for error handling.
The value is a process, or nil."
  (DOLIST (p active-processes)
    (AND (SETQ p (CAR p))
	 (TYPEP (SETQ sg (PROCESS-STACK-GROUP p)) 'stack-group)
	 (SETQ window (si:process-is-in-error-p p))
	 (Y-OR-N-P (FORMAT nil "Use process ~A? " (PROCESS-NAME p)))
	 (RETURN p window))))

;Select an "interesting" background window.
;Called from terminal-0-s and terminal-e
(DEFUN select-interesting-window (&optional n &aux interesting notification-window)
  (COND ((SETQ interesting (find-interesting-window n)) ; Modified to pass in N
	 (SETQ notification-window (CDR (ASSOC interesting background-interesting-windows :test #'EQ)))
	 (AND notification-window
	      (EQ (FUNCALL notification-window ':window-of-interest) interesting)
	      (FUNCALL notification-window ':exposed-p)
	      (FUNCALL notification-window ':deexpose))
	 (FUNCALL interesting ':mouse-select)
	 (SETQ background-interesting-windows
	       (DELETE (ASSOC interesting background-interesting-windows :test #'EQ)
	(THE list background-interesting-windows) :test #'EQ)))))

(DEFUN terminal-move-window (IGNORE &aux window)
  (WHEN (SETQ window (screen-editor-find-window
		       (get-window-edge-alist mouse-sheet)
		       nil
		       "Move window"
		       "Left: Choose a window to be moved.  Middle aborts."))
    (mouse-set-window-position (CAR window) t)))


(DEFUN terminal-reshape-window (IGNORE &aux window)
  (WHEN (SETQ window (screen-editor-find-window
		       (get-window-edge-alist mouse-sheet)
		       nil
		       "Reshape window"
		       "Left: Choose a window to be reshaped.  Middle aborts."))
    (mouse-set-window-size (CAR window) t)))



(DEFUN terminal-expand-window (mouse-it-p &aux window)
  (IF mouse-it-p ;; Select a window with the mouse
      (SETQ window (CAR (screen-editor-find-window
			  (get-window-edge-alist mouse-sheet) nil
			  "Expand window"
			  '(:mouse-l-1 "Choose a window to expand"
				       :mouse-m-1 "aborts the expand operation."))))
    ;; If the selected window is a pane, find its associated outer frame
    (LOOP for previous first selected-window then superior
	  for superior = (sheet-superior previous)
	  while (AND superior (sheet-superior superior))
	  finally (SETQ window previous)))
  (expand-window window))

#+comment ; Old version
(DEFUN terminal-expand-window (IGNORE &aux window)
  (WHEN (SETQ window (screen-editor-find-window
		       (get-window-edge-alist mouse-sheet)
		       nil
		       "Expand window"
		       "Left: Choose a window to expand.  Middle aborts."))
    (expand-window (CAR window) t)))


;;; More or less innocuous functions from the old window system that are called all over the
;;; place.

(DEFUN kbd-tyi (&rest ignore) (SEND *terminal-io* :tyi))
(compiler:make-obsolete kbd-tyi "use read-char")

(DEFUN kbd-tyi-no-hang (&rest ignore) (SEND *terminal-io* :tyi-no-hang))
(compiler:make-obsolete kbd-tyi-no-hang "use read-char-no-hang")

(DEFUN kbd-char-available (&rest ignore) (SEND *terminal-io* :listen))
(compiler:make-obsolete kbd-char-available "use listen")
