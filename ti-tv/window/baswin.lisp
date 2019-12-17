;;; -*- Mode: common-LISP;  Package: TV; Base:10. ; Fonts: (CPTFONT CPTFONTB HL12BI) -*-

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
;;;   04/10/89 MAY  Moved defvar of PREVIOUSLY-SELECTED-WINDOWS to tvdefs.lisp
;;;   04/06/89 MAY  Changed draw-shadow-border to work on color ( & b&w ) monitors. SPR 9504
;;;   01/30/89 KJF  [may] Changes for Multiple Monitor (MMON) support, search for MMON
;;;                 Change to (SELECT-MIXIN :SELECT) to call who-line-update in/of mouse-wakeup
;;;   09-09-88 MAY  Changed screen-has-not-changed-width-p to fix case where screen is deexposed
;;;                 during w:set-number-of-who-line-documentation-lines.
;;;   07/11/88 KJF   (DECLARE (:self-flavor essential-set-edges)) not:
;;;                   (DECLARE (self-flavor :esential-set-edges)) in screen-has-not-changed-width-p
;;;                   function by LG.  Actually fixed 05/27/88, but just now got around
;;;                   to updating Change History.
;;;   05/20/88 LG    Removed select-a-screen call from :before :select
;;;   			method, added :selected-screen method for overt
;;;   			screen changing when a window is selected.
;;;   05/12/88 LG    Added call to select-a-screen in the :before
;;;   			:select method if on a Mac system.
;;;   04/10/88 KJF   Changes to get-screen, :set-label-color, :set-label-background.
;;;   11/30/87   PMH   Modified pop-up-notify to :deselect the note-window; #3146
;;;   10/22/1987 KWW   Changed *color-system* to (color-system-p x)
;;;   9/12/87  PMH   Added :before :expose method to temporary windows to not change the color map
;;;   8/27/87  KWW   Made changes resulting from the code reading
;;;   8/25/87  KWW   Fixed :set-label-background and :set-label-color to look for a nil label
;;;   6/7/87   KWW   Added instance variables for shadow-border and label-color-background
;;;   6/3/87   KWW   Changed 'bit to (sheet-array-type-cl default-screen)
;;;   5/30/87  KWW   Update for color borders, color label - see >>> symbol
;;;   8/10/87  PMH   Make the process instance variable settable in process-mixin.
;;;   7/29/87  PMH   Fix the obsolete use of (PROG NIL in (DEFWRAPPER (ESSENTIAL-SET-EDGES
;;;                     :SET-EDGES)
;;;   3/26/87  TWE	Fixed parse-borders-spec so that borders specified with a list of
;;;			keywords work correctly.  This fixes bug 3330.
;;;   3/20/87  DAN	Added patch to POP-UP-TEXT-WINDOW :AFTER :HANDLE-MOUSE so that extraneous
;;;                   spaces aren't stuffed into UCL:GET-KEYSTROKE-WINDOW.
;;;   3/10/87  TWE	Fixed up POP-UP-NOTIFY to not output anything if the prompt-string
;;;			is empty.  This fixes bug 4032.
;;;   2/18/87  TWE	Fixed up SELECT-MIXIN :MOUSE-SELECT to not do anything if it can't
;;;			find an appropriate :SELECT method to execute.  This closes bug 2661.
;;; 01/22/87 LGO	Add the *DESELECTED-PROCESS-PRIORITY* feature, to fix problems caused
;;;			by the *SELECTED-PROCESS-PRIORITY* feature.  Deselected windows now get
;;;			a lower priority than normal, rather than selected windows getting a
;;;			higher priority.  This ensures that processes running with the default
;;;			priority (like notifications) aren't locked out.
;;; 01/07/87 KDB      Made fix to :AFTER :DRAW-LABEL so it distinguished between temporary and
;;;                     permanent W:MENU :COMMAND-MENUs.
;;; 12/23/86 TWE	Made several changes to make the build process cleaner.
;;;			Moved TRUNCATING-WINDOW from STREAM to here near truncating-pop-up-text-window.
;;; 12/22/86 TWE	Made several changes to make the build process cleaner.
;;;			Moved PREVIOUSLY-SELECTED-WINDOWS towards the top of this file.
;;;			Likewise for the label defstructs.  Added a required-flavors
;;;			ESSENTIAL-WINDOW to the TRUNCATING-POP-UP-TEXT-WINDOW defflavor.
;;;			This allows its methods to reference instance variables properly.
;;; 12/19/86 TWE	Swapped the order of the required flavors for NOTIFICATION-MIXIN in
;;;			an attempt to have the instance variables recognized in the
;;;			:PRINT-NOTIFICATION method.
;;; 11/26/86 TWE	Changed instance variable initialization for color support.
;;; 11/25/86 TWE	Added hooks for color support.
;;; 11/20/86 SLM	Added calls to Suggestions macros for (:METHOD SELECT-MIXIN :SELECT)
;;; 10/28/86 GRH	Changed doc for simple-lisp-listener.
;;; 10/21/86 LGO	Added the *SELECTED-PROCESS-PRIORITY* feature to (:METHOD SELECT-MIXIN :SELECT)
;;; 10/01/86 GRH	Moved obsolete flavor WINDOW-WITH-INSIDE-SCROLL-BAR to obsolete-scroll-bar.
;;; 09/17/86 TWE	Fixed :PARSE-LABEL-SPEC that was changed on 04/04/86 to correctly
;;;			handle top-p.
;;; 09/16/86 TWE	Put *'s around UNEXPECTED-SELECT-DELAY to make it look more like the
;;;			global variable that it really is.
;;; 09/12/86 DLS	Changed UNEXPECTED-SELECT-DELAY to 60. to only sleep one second on pop up notifications.
;;; 09/05/86 KDB	Added reverse-video labels to pop-up windows.
;;; 08/13/86 TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 08/10/86 KDB	Added :after method to Pop-up-Text-Window flavor to avoid wedged state when user
;;;			if user clicked mouse outside of window.                       
;;; 08/06/86 TWE	Fixed the two defstructs for labels to not have the :alterant clause
;;;			included within the :constructor clause.
;;; 08/04/86 TWE	Changed type checks for font to use the FONT-OBJECT-P function.
;;; 07/29/86 TWE	Changed to use Common Lisp functions.
;;; 07/28/86 TWE	Modified references to the pixel functions to use ARRAY-DIMENSION
;;;			and MAKE-ARRAY instead.
;;; 07/23/86 TWE	Moved the resource for POP-UP-FINGER-WINDOW out of COMETH to here,
;;;			where the flavor is defined.
;;; 07/01/86 TWE	Changed some uses of the delaying-screen-management macro to return nil.
;;;			This is an efficiency hack for the compiler.
;;; 06/18/86 TWE	Added a hack to bring back the old Lisp-Listener flavor.  This will
;;;			be removed when UCL comes up in the cold load environment.  There is
;;;			no conflict since this one is named SIMPLE-LISP-LISTENER.
;;; 06/12/86 DLS	Added :MOUSE-SELECT wrapper to ESSENTIAL-WINDOW to fix a bug - Code read by GRH, TWE
;;; 04/21/86 DLS	Fixed EXPOSE-WINDOW-NEAR to warp mouse correctly for :POINT option - Code read by TWE
;;; 04/14/86 LGO	Re-wrote (:METHOD LABEL-MIXIN :PARSE-LABEL-SPEC) - Code read by TWE
;;; 04/08/86 LGO	Remove (:METHOD Shadow-Borders-Mixin :After :expose) - Code read by TWE
;;; 02/21/86 LGO	Put 'notify beep-type in (:method NOTIFICATION-MIXIN :PRINT-NOTIFICATION-ON-SELF)


;; may 04/10/89 Moved this to tvdefs.lisp to quiet compilier warnings
;;(DEFVAR PREVIOUSLY-SELECTED-WINDOWS (MAKE-ARRAY 20. :AREA PERMANENT-STORAGE-AREA))

;;; Label definitions.
(DEFSTRUCT (ESSENTIAL-LABEL-MIXIN (:CONSTRUCTOR NIL) (:CONC-NAME NIL)
				  (:CALLABLE-CONSTRUCTORS NIL) (:ALTERANT ALTER-ESSENTIAL-LABEL-MIXIN) (:PREDICATE NIL)
				  (:COPIER NIL) (:TYPE :LIST))
  LABEL-LEFT					;Coordinates of the label, all relative to the
  LABEL-TOP					;edges of the window
  LABEL-RIGHT
  LABEL-BOTTOM)

(DEFSTRUCT (LABEL-MIXIN (:INCLUDE ESSENTIAL-LABEL-MIXIN) (:CONSTRUCTOR NIL)
			(:SIZE-SYMBOL LABEL-DEFSTRUCT-SIZE) (:CONC-NAME NIL) (:CALLABLE-CONSTRUCTORS NIL)
			(:ALTERANT ALTER-LABEL-MIXIN) (:PREDICATE NIL) (:COPIER NIL) (:TYPE :LIST))
  LABEL-FONT
  LABEL-STRING
  LABEL-VSP
  LABEL-CENTERED
  (LABEL-COLOR *default-label-foreground*)
  (LABEL-BACKGROUND *default-label-background*)
    ;;; >>> added as per HF and Russ's comments that we need control over the label background
  )


;;; WINDOW type flavor
(DEFFLAVOR ESSENTIAL-WINDOW () (SHEET)
  (:INIT-KEYWORDS :EDGES-FROM :MINIMUM-WIDTH :MINIMUM-HEIGHT :ACTIVATE-P :EXPOSE-P)
  (:DOCUMENTATION :LOWLEVEL-MIXIN "The flavor that is part of every window
This had better be at the end of your any hierarchy, it should also always
be a :required-flavor of any window mixin just so that instance variables
are declared properly."))


(DEFWRAPPER (essential-window :mouse-select) (args . body)
  `(IF selection-substitute
       (LEXPR-SEND selection-substitute :mouse-select args)
       (delaying-screen-management
	 (WHEN (SEND superior :inferior-select self)
	   ;; Not all of the flavors we are combined with can be selected -- this prevents
	   ;; a gratuitous error from the compiler
	   . ,body))))


(DEFMETHOD (ESSENTIAL-WINDOW :BEFORE :MOUSE-SELECT) (&REST IGNORE &AUX BUF)
  (WITHOUT-INTERRUPTS
    (AND SELECTED-WINDOW
	 (SETQ BUF (SEND SELECTED-WINDOW :IO-BUFFER))
	 (KBD-SNARF-INPUT BUF))))

(DEFMETHOD (ESSENTIAL-WINDOW :MOUSE-SELECT) (&REST ARGS)
  (APPLY (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS) :SELECT ARGS))  

(DEFMETHOD (ESSENTIAL-WINDOW :LISP-LISTENER-P) () NIL)

(DEFMETHOD (ESSENTIAL-WINDOW :SELECTABLE-WINDOWS) ()
  "Returns inferiors to all levels that are selectable in a form suitable for
use as a menu item-list."
  (LET ((SELECTABLE-WINDOWS (MAPCAN #'(LAMBDA (I) (SEND I :SELECTABLE-WINDOWS))
				    INFERIORS))
	(STRING))
    (AND (SETQ STRING (SEND SELF :NAME-FOR-SELECTION))
	 (PUSH (LIST STRING SELF) SELECTABLE-WINDOWS))))

(DEFMETHOD (ESSENTIAL-WINDOW :BEFORE :INIT) (INIT-PLIST &AUX EDGES-FROM)
  (OR NAME
      (LET ((FLAVOR-NAME (TYPE-of SELF)))
	(LET ((N (1+ (OR (GET FLAVOR-NAME 'UNNAMED-WINDOW-INSTANCE-COUNT) 0))))
	  (SETF (GET FLAVOR-NAME 'UNNAMED-WINDOW-INSTANCE-COUNT) N)
	  (SETQ NAME (STRING-CAPITALIZE-WORDS (FORMAT () "~A-~D" FLAVOR-NAME N) ())))))
  (SETQ EDGES-FROM (GET INIT-PLIST :EDGES-FROM))
  (COND
    ((NULL EDGES-FROM))
    ((STRINGP EDGES-FROM)
     (SETF (GET INIT-PLIST :CHARACTER-WIDTH) EDGES-FROM)
     (SETF (GET INIT-PLIST :CHARACTER-HEIGHT) EDGES-FROM))
    (T
     (SETF (GET INIT-PLIST :EDGES)
	   (COND
	     ((CONSP EDGES-FROM)
	      ;; If a list, means explicit edges specified
	      (OR (= (LENGTH EDGES-FROM) 4)
		  (FERROR () "The :EDGES-FROM list ~S is not of length four." EDGES-FROM))
	      EDGES-FROM)
	     ((EQ EDGES-FROM :MOUSE)
	      ;; Get edges from mouse
	      (LET ((MINIMUM-WIDTH (OR (GET INIT-PLIST :MINIMUM-WIDTH) 0))
		    (MINIMUM-HEIGHT (OR (GET INIT-PLIST :MINIMUM-HEIGHT) 0)))
		(MULTIPLE-VALUE-LIST
		 (MOUSE-SPECIFY-RECTANGLE-SET-SHEET () () () () SUPERIOR MINIMUM-WIDTH
						    MINIMUM-HEIGHT))))
	     ((TYPEP EDGES-FROM 'ESSENTIAL-WINDOW)
	      ;; A window, use its edges
	      (OR (EQ SUPERIOR (SHEET-SUPERIOR EDGES-FROM))
		  (FERROR () "Attempt to get edges from sheet (~S) with different superior"
			  EDGES-FROM))
	      (LIST (SHEET-X EDGES-FROM)
		    (SHEET-Y EDGES-FROM)
		    (+ (SHEET-X EDGES-FROM) (SHEET-WIDTH EDGES-FROM))
		    (+ (SHEET-Y EDGES-FROM) (SHEET-HEIGHT EDGES-FROM))))
	     (T (FERROR () "~S is illegal :EDGES-FROM specification" EDGES-FROM))))))
  (LET ((INSIDE-WIDTH (OR (GET INIT-PLIST :INSIDE-WIDTH)
			  (FIRST (GET INIT-PLIST :INSIDE-SIZE))))
	(INSIDE-HEIGHT
	 (OR (GET INIT-PLIST :INSIDE-HEIGHT)
	     (SECOND (GET INIT-PLIST :INSIDE-SIZE)))))
    (AND INSIDE-WIDTH (SETQ WIDTH (+ INSIDE-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))
    (AND INSIDE-HEIGHT (SETQ HEIGHT (+ INSIDE-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)))))  

(DEFFLAVOR MINIMUM-WINDOW () (ESSENTIAL-EXPOSE ESSENTIAL-ACTIVATE ESSENTIAL-SET-EDGES
			      ESSENTIAL-MOUSE ESSENTIAL-WINDOW)
  (:DOCUMENTATION :COMBINATION "Essential flavors for most normal windows
Most windows should include this at the end of their hierachy or all of its components."))

(DEFFLAVOR WINDOW-WITHOUT-LABEL () (STREAM-MIXIN BORDERS-MIXIN SELECT-MIXIN
				    DELAY-NOTIFICATION-MIXIN GRAPHICS-MIXIN MINIMUM-WINDOW))

(DEFFLAVOR WINDOW () (STREAM-MIXIN BORDERS-MIXIN LABEL-MIXIN SELECT-MIXIN
		      DELAY-NOTIFICATION-MIXIN GRAPHICS-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "This is the simplest practical window
It probably isn't what you want, except for testing purposes; although it is useful for
mixing with one or two simple mixins to get something useful."))

;;; Basic exposure/deexposure
(DEFFLAVOR ESSENTIAL-EXPOSE () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :ESSENTIAL-MIXIN "Handles default exposure behaviour.
Makes sure the screen manager is aware of a window leaving or entering the screen."))

(DEFMETHOD (ESSENTIAL-EXPOSE :AFTER :EXPOSE) (&REST IGNORE)
  (SCREEN-CONFIGURATION-HAS-CHANGED SELF :EXPOSE))

(DEFMETHOD (ESSENTIAL-EXPOSE :AFTER :DEEXPOSE) (&REST IGNORE)
  (SCREEN-CONFIGURATION-HAS-CHANGED SELF :DEEXPOSE))

(DEFWRAPPER (ESSENTIAL-EXPOSE :BURY) (IGNORE . BODY)
  `(AND (OR (NULL SUPERIOR)
	    (SEND SUPERIOR :INFERIOR-BURY SELF))
	(DELAYING-SCREEN-MANAGEMENT . ,BODY)))

;;; Basic activation/deactivation
(DEFFLAVOR ESSENTIAL-ACTIVATE () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :ESSENTIAL-MIXIN "Handles default activation behaviour
Makes sure a window is activated before it can get exposed (see discussion of activation).
Also provides for the :STATUS and :SET-STATUS messages (q.v.)."))

(DEFMETHOD (ESSENTIAL-ACTIVATE :BEFORE :EXPOSE) (&REST IGNORE)
  (WITHOUT-SCREEN-MANAGEMENT
    (SEND SELF :ACTIVATE)))

(DEFMETHOD (ESSENTIAL-ACTIVATE :AFTER :ACTIVATE) ()
  (SCREEN-ACTIVITY-HAS-CHANGED SELF T))

;;; This could simply do (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SELF T) except
;;; this method gets run whether or not the window was already active when
;;; the :ACTIVATE message was sent.  What a drag.
;;; may 01/30/89  from patch window.4.109
(DEFMETHOD (essential-activate :around :activate)
	   (cont mt args)
  (LET ((w (SEND self :alias-for-selected-windows))
	(active-p (SEND self :active-p)))
    (LEXPR-FUNCALL-WITH-MAPPING-TABLE cont mt args)
    (OR active-p
	;; Only windows that are top level for selection should go on the list.
	(NEQ w self)
	;; Can be activating a pane relative to a frame which is not itself activated
	(DO ((sup (sheet-superior w) (sheet-superior sup))
	     (inf w sup))
	    ((NULL sup)
	     nil)
	  (OR (MEMBER inf (sheet-inferiors sup) :test #'EQ)
	      (RETURN t)))
	;; Do nothing if W is already on the list.
	;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
	;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
	(with-screens-previously-selected-windows (w)
	  (DO ((i 0 (1+ i))
	       (n (ARRAY-TOTAL-SIZE previously-selected-windows)))
	      ((>= i n)
	       (add-to-previously-selected-windows w t))
	    (AND (EQ (AREF previously-selected-windows i) w) (RETURN ()))))))) 

(DEFMETHOD (ESSENTIAL-ACTIVATE :AFTER :DEACTIVATE) ()
  ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
  ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
  (with-screens-previously-selected-windows (self)
    (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS SELF))
  (SCREEN-ACTIVITY-HAS-CHANGED SELF NIL))

(DEFMETHOD (ESSENTIAL-ACTIVATE :BURY) () (SYSTEM-BURY SELF))

(DEFUN SYSTEM-BURY (WINDOW &AUX (INHIBIT-SCHEDULING-FLAG T) SUP INFS)
  (SEND SELF :DESELECT :BURY)
  (SETQ SUP (SHEET-SUPERIOR WINDOW))
  (DO ()
      ((NOT (MEMBER WINDOW (SHEET-EXPOSED-INFERIORS SUP) :TEST #'EQ)))
    (SETQ INHIBIT-SCHEDULING-FLAG ())
    (SEND WINDOW :DEEXPOSE)
    (SETQ INHIBIT-SCHEDULING-FLAG T))
  (COND
    ((MEMBER WINDOW (SETQ INFS (SHEET-INFERIORS SUP)) :TEST #'EQ)
     (SETQ INFS (DELETE WINDOW (THE LIST INFS) :TEST #'EQ))
     (SHEET-CONSING
      (COND
	(INFS (RPLACD (LAST (SETQ INFS (COPY-LIST INFS))) (CONS WINDOW ())))
	(T (SETQ INFS (CONS WINDOW ())))))
     (SETF (SHEET-INFERIORS SUP) INFS) (SETQ INHIBIT-SCHEDULING-FLAG ())
     (SCREEN-CONFIGURATION-HAS-CHANGED WINDOW)))) 

(DEFMETHOD (ESSENTIAL-ACTIVATE :STATUS) ()
  (COND
    ((EQ SELF SELECTED-WINDOW) :SELECTED)
    ((DO ((WINDOW SELF (SHEET-SUPERIOR WINDOW)))
	 ((NULL WINDOW)
	  T)
       (OR (SHEET-EXPOSED-P WINDOW) (RETURN ())))
     :EXPOSED)					;Only if really on screen
    ((AND SUPERIOR (MEMBER SELF (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ))
     :EXPOSED-IN-SUPERIOR)			;Would be exposed if superior was
    ((OR (NULL SUPERIOR) (MEMBER SELF (SHEET-INFERIORS SUPERIOR) :TEST #'EQ)) :DEEXPOSED)
    (T :DEACTIVATED))) 

(DEFMETHOD (ESSENTIAL-ACTIVATE :ACTIVE-P) ()
  (OR (NULL SUPERIOR)
      (MEMBER SELF (SHEET-INFERIORS SUPERIOR) :TEST #'EQ))) 

(DEFMETHOD (ESSENTIAL-ACTIVATE :SET-STATUS) (NEW-STATUS)
  (CASE NEW-STATUS
    (:SELECTED (SEND SELF :SELECT))
    (:EXPOSED (SEND SELF :EXPOSE)
	      (AND (EQ SELF SELECTED-WINDOW)
		   (SEND SELF :DESELECT ())));Don't restore-selected!
    (:EXPOSED-IN-SUPERIOR
     (OR (MEMBER SELF (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ)
	 (SEND SELF :EXPOSE))
     (AND (EQ SELF SELECTED-WINDOW)
	  (SEND SELF :DESELECT ())));Don't restore-selected!
    (:DEEXPOSED (OR (MEMBER SELF (SHEET-INFERIORS SUPERIOR) :TEST #'EQ)
		    (SEND SELF :ACTIVATE))
     (AND (MEMBER SELF (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ)
	  (SEND SELF :DEEXPOSE)))
    (:DEACTIVATED
     (AND (MEMBER SELF (SHEET-INFERIORS SUPERIOR) :TEST #'EQ)
	  (SEND SELF :DEACTIVATE)))
    (OTHERWISE
     (FERROR () "~S not one of :DEACTIVATED, :DEEXPOSED, :EXPOSED, :SELECTED" NEW-STATUS)))) 

;; This must come here to prevent lossage defining SELECT-MIXIN
;; The methods come later.
(DEFFLAVOR DELAY-NOTIFICATION-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "Delays printing notifications, but announces them in the who line.
This is the default way of handling them.  See NOTIFICATION-MIXIN for an alternative."))

;;; Basic selection/deselection
(DEFFLAVOR SELECT-MIXIN () ()
  (:INCLUDED-FLAVORS DELAY-NOTIFICATION-MIXIN)
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  (:REQUIRED-INSTANCE-VARIABLES IO-BUFFER)
  (:DOCUMENTATION :MIXIN "Default SELECTion behaviour
Provides a :NAME-FOR-SELECTION message that gives the window's label or name, and simple
:CALL, :BREAK, and :ABORT messages.  Note that any window that can be selected is expected
to handle these messages, and should probably include this flavor somewhere."))

(DEFMETHOD (SELECT-MIXIN :NAME-FOR-SELECTION) (&AUX LABEL)
  (OR
   (COND
     ((GET-HANDLER-FOR SELF :LABEL) (SETQ LABEL (SEND SELF :LABEL))
      (OR (STRINGP LABEL)
	  (SETQ LABEL (LABEL-STRING LABEL)))
      (AND (POSITION #\SPACE (THE STRING (STRING LABEL)) :TEST-NOT #'CHAR-EQUAL)
	   LABEL)))
   NAME)) 

(DEFMETHOD (SELECT-MIXIN :PROCESS) ()
  (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER))

(DEFMETHOD (SELECT-MIXIN :SET-PROCESS) (PROC)
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) PROC))

;;; If process not otherwise known, default to process which is activating
;;; the window which will usually be good enough for the who-line.
(DEFMETHOD (SELECT-MIXIN :AFTER :ACTIVATE) (&REST IGNORE)
  (OR (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER)
      (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) CURRENT-PROCESS)))

(DEFMETHOD (SELECT-MIXIN :CALL) (&AUX WINDOW)
  (LET ((LAST-PROCESS (SEND SELF :PROCESS)))
    (AND LAST-PROCESS (SEND LAST-PROCESS :ARREST-REASON :CALL)))
  (SETQ WINDOW
	(IF (EQ (SEND SELF :LISP-LISTENER-P) :IDLE)		 
	    SELF
	    (KBD-DEFAULT-CALL-WINDOW SUPERIOR)))
  (SHEET-FREE-TEMPORARY-LOCKS WINDOW)
  (SEND WINDOW :SELECT))

(DEFMETHOD (SELECT-MIXIN :ARREST) (&AUX LAST-PROCESS)
  (AND (SETQ LAST-PROCESS (SEND SELF :PROCESS))
       (SEND LAST-PROCESS :ARREST-REASON)))

(DEFMETHOD (SELECT-MIXIN :UN-ARREST) (&AUX LAST-PROCESS)
  (AND (SETQ LAST-PROCESS (SEND SELF :PROCESS))
       (SEND LAST-PROCESS :REVOKE-ARREST-REASON)))

(DEFUN SYSTEM-SELECT (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Select a window.  Make its blinkers blink and input to be read through it."
  (DECLARE (:SELF-FLAVOR SELECT-MIXIN))
  (DO ()
      ((MEMBER SELF (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ))
    (SETQ INHIBIT-SCHEDULING-FLAG ())
    (SEND SELF :EXPOSE T)
    (SETQ INHIBIT-SCHEDULING-FLAG T))
  (COND
    ((NEQ SELECTED-WINDOW SELF)
     (AND SELECTED-WINDOW
	  (SEND SELECTED-WINDOW :DESELECT ()))
     (SELECT-SHEET-BLINKERS SELF)
     (SETQ SELECTED-WINDOW SELF)
     ;;  Remember this screen's last-selected window if on the Mac...
     (when (mac-window-p self)
       (remember-this-screens-last-selected-window self))))
  T) 

(DEFVAR *selected-process* nil "The process associated with the currently selected window")
(DEFVAR *selected-process-previous-priority* nil) ;; Previous priority of the selected process

(DEFUN select-process (new-process)
  ;; Change the priority of NEW-PROCESS to *SELECTED-PROCESS-PRIORITY*
  ;; Change the previously selected process's priority back to what it was before it was selected.
  (UNLESS (EQ new-process *selected-process*)
    (LET (previous-priority)
      (WHEN (AND new-process *selected-process-priority*)
	(SETQ previous-priority (si:process-priority new-process))
	;; Increase the priority of the newly selected process
	(WHEN (> *selected-process-priority* previous-priority)
	  (SEND new-process :set-priority *selected-process-priority*)))
      (WHEN (AND *selected-process* *selected-process-previous-priority*
		 ;; Don't alter priority if user changed it while we were selected
		 (= (si:process-priority *selected-process*) *selected-process-priority*))
	;; When the previous priority was the default priority, change it to the deselected-priority.
	(WHEN (= *selected-process-previous-priority* *selected-process-priority*)
	  (SETQ *selected-process-previous-priority* *deselected-process-priority*))
	;; Decrease the priority of the currently selected process
	(WHEN (< *selected-process-previous-priority* (si:process-priority *selected-process*))
	  (SEND *selected-process* :set-priority *selected-process-previous-priority*)))
      (SETQ *selected-process-previous-priority* previous-priority)
      (SETQ *selected-process* new-process))))

(sys:declare-suggestions-for '(:method TV:SELECT-MIXIN :SELECT)
			 :after '(sys:sugg-after-select-method))

;; may 01/30/89 MMON changes and substituted who-line-update for mouse-wakep - only doc needs refreshing
(DEFMETHOD (SELECT-MIXIN :SELECT) (&OPTIONAL IGNORE)
  (sys:with-suggestions-menus-for (:method tv:select-mixin :select)
    (LET ((LAST-PROCESS (SEND SELF :PROCESS)))
      (when LAST-PROCESS
	(SEND LAST-PROCESS :REVOKE-ARREST-REASON :CALL)
	(select-process last-process)))
    (SYSTEM-SELECT)
    ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
    ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
    (LET ((afsw (SEND self :alias-for-selected-windows)))
      (with-screens-previously-selected-windows (afsw)
	(remove-from-previously-selected-windows afsw)))
    (WHO-LINE-UPDATE)			;wholine doc may need to change  PMH ;; MAY 01/30/89 was mouse-wakeup
    T))					;For frames and :MOUSE-SELECT

(DEFMETHOD (SELECT-MIXIN :MOUSE-SELECT) (&REST ARGS)
  "Form of select used when `mousing' windows.  Clears all temp locks that are on the
window, as well as failing if the window is not fully within its superior."
  ;; Maybe our size has to get adjusted first.
  (SEND SELF :ACTIVATE)
  (COND
    ((SHEET-WITHIN-SHEET-P SELF SUPERIOR)
     ;; Flush all temp windows that cover us.
     (SHEET-FREE-TEMPORARY-LOCKS SELF)
     (LET* ((ALIAS-WINDOW (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS))
            (SELECT-OPERATION (GET-HANDLER-FOR ALIAS-WINDOW :SELECT)))
       ;; If we can't select the window then don't do anything.
       (WHEN SELECT-OPERATION
         (APPLY ALIAS-WINDOW :SELECT ARGS))))))

;;;  This method differs from :select only on an Mx right now.
;;;  Dual-monitor systems would need this modified to have any effect.
;;;  Philosophically, a simple :select should not change the screen
;;;  accepting input.  Only user-action should do that.  There are
;;;  times, however, where, after user-action, the application may have
;;;  to decide which screen to use.  This new method provides that
;;;  capability without compromising :select.
(DEFMETHOD (select-mixin :select-screen) (&OPTIONAL (SAVE-SELECTED T))
  (WHEN (mac-system-p)
    (select-a-screen self))
  (SEND self :select save-selected))

(DEFMETHOD (SELECT-MIXIN :BEFORE :SELECT) (&OPTIONAL (SAVE-SELECTED T) &AUX OSW)
  (SETQ OSW SELECTED-WINDOW)
  (OR EXPOSED-P (SEND SELF :EXPOSE T))
  (DO ((SHEET SUPERIOR (SHEET-SUPERIOR SHEET)))
      ((NULL SHEET) NIL)
    (OR (SHEET-EXPOSED-P SHEET) (SEND SHEET :EXPOSE)))	;Really onto the screen
  (WITHOUT-INTERRUPTS
    (AND OSW SAVE-SELECTED (NEQ SELF OSW) (NEQ (SEND OSW :STATUS) :DEACTIVATED)	;Deexposing can deactivate
	 ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
	 ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
	 (with-screens-previously-selected-windows (osw)
	   (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS OSW))))) 

;; Added ASSERT to try to recover from error of NIL being passed in.  04/10/88 KJF.
(DEFUN GET-SCREEN (WINDOW)
  "Returns the ultimate superior of window, its screen."
  (ASSERT window (window) "WINDOW passed in as NIL.")
  (IF (MEMBER WINDOW ALL-THE-SCREENS :TEST #'EQ)
      WINDOW
      (GET-SCREEN (SEND WINDOW :SUPERIOR))))

(DEFUN mouse-process-lookup ()
  "Tries to find the mouse process.  Returns the process if found, otherwise returns nil"
  (LOOP FOR process IN sys:all-processes
	IF (STRING-EQUAL (SEND process :name) "Mouse")
	  RETURN process
	FINALLY (RETURN nil)))

(DEFMETHOD (SELECT-MIXIN :AFTER :SELECT) (&REST IGNORE)
  "This version will set the mouse to the proper screen if the mouse process 
is running and tv:*set-mouse-to-selected-screen* is non-nil."
  (when (mac-system-p)
    (send-select-window self))
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) (SEND SELF :PROCESS))
  (KBD-GET-IO-BUFFER)
  (install-color-map self)
  (WHEN tv:*set-mouse-to-selected-screen*
    (LET ((mouse (mouse-process-lookup)))
      (WHEN (AND mouse
		 (NEQ (SEND mouse :wait-function) #'FALSE)
		 (NEQ tv:window-owning-mouse 't)
		 (NEQ tv:window-owning-mouse 'tv: stop))
	(MOUSE-SET-SHEET (get-screen self)))))
  ;; Added for multiple monitor support.  CJJ 06/06/88.
  ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
  (WHEN (mmon-p)
    (LET ((screen (sheet-get-screen self)))
      (SEND screen :maybe-make-default-screen :selected-window)
      (SEND screen :maybe-make-associated-who-line-screen-active :selected-window)
      (SEND screen :maybe-move-mouse-to :selected-window))))

(DEFMETHOD (SELECT-MIXIN :DESELECT) (&OPTIONAL (RESTORE-SELECTED T))
  (LET ((SEL-P (EQ SELF SELECTED-WINDOW)))
    (WHEN SEL-P
      (DESELECT-SHEET-BLINKERS SELF)
      (SETQ SELECTED-WINDOW ())
      (COND
        ((EQ RESTORE-SELECTED :DONT-SAVE))
        ((AND RESTORE-SELECTED (NEQ RESTORE-SELECTED :BEGINNING))
	 ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
	 ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
	 (with-screens-previously-selected-windows (self)
	   (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SELF T))
         (IF (MEMBER RESTORE-SELECTED '(:LAST T) :TEST #'EQ)
	     ;; This will come from DEFAULT-SCREEN, not necessarily this window's screen...
	     (SELECT-PREVIOUS-WINDOW () () ()))
         (WHEN (EQ RESTORE-SELECTED :FIRST)
	   ;; This will come from DEFAULT-SCREEN, not necessarily this window's screen...
           (SELECT-PREVIOUS-WINDOW () () ())
	   ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
	   ;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
	   (with-screens-previously-selected-windows (self)
	     (ROTATE-TOP-OF-ARRAY PREVIOUSLY-SELECTED-WINDOWS -1))))
        (T
	 ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
	 ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
	 (with-screens-previously-selected-windows (self)
	   (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SELF))))))) 

(DEFMETHOD (SELECT-MIXIN :AFTER :DESELECT) (&REST IGNORE)
  (KBD-CLEAR-SELECTED-IO-BUFFER))

(DEFMETHOD (SELECT-MIXIN :BEFORE :DEEXPOSE) (&REST IGNORE)
  (SEND SELF :DESELECT NIL))

;;; Selection substitutes

;; By putting this on ESSENTIAL-WINDOW we cause all windows to
;; have a handler, if only for the sake of the wrapper below.
(DEFMETHOD (ESSENTIAL-WINDOW :DEFAULT :SELECT) (&OPTIONAL IGNORE)
  ; No need to show this error message at all.
  ;(FERROR NIL "Attempt to select ~S.  Is this an error?" self)
  NIL)

(DEFWRAPPER (ESSENTIAL-WINDOW :SELECT) (ARGS . BODY)
  `(IF SELECTION-SUBSTITUTE
       (LEXPR-SEND SELECTION-SUBSTITUTE :SELECT ARGS)
     (DELAYING-SCREEN-MANAGEMENT
       (WHEN (SEND SUPERIOR :INFERIOR-SELECT SELF)
	 ;; Not all of the flavors we are combined with can be selected -- this prevents
	 ;; a gratuitous error from the compiler
	 . ,BODY))))

;; By putting this on ESSENTIAL-WINDOW we cause all windows to
;; have a handler, if only for the sake of the wrapper below.
(DEFMETHOD (ESSENTIAL-WINDOW :DEFAULT :DESELECT) (&OPTIONAL IGNORE)
  NIL)

(DEFWRAPPER (ESSENTIAL-WINDOW :DESELECT) (ARGS . BODY)
  `(IF SELECTION-SUBSTITUTE
       (LEXPR-SEND SELECTION-SUBSTITUTE :DESELECT ARGS)
     (DELAYING-SCREEN-MANAGEMENT . ,BODY)))

(DEFMETHOD (ESSENTIAL-WINDOW :SET-SELECTION-SUBSTITUTE) (WINDOW)
  (LET ((FLAG (SEND SELF :SELF-OR-SUBSTITUTE-SELECTED-P)))
    (SETQ SELECTION-SUBSTITUTE (AND (NEQ WINDOW SELF) WINDOW))
    (WHEN FLAG
      (DELAYING-SCREEN-MANAGEMENT
	(SEND SELECTED-WINDOW :DESELECT :DONT-SAVE)
	(SEND SELF :SELECT NIL)
	NIL))))

(DEFMETHOD (ESSENTIAL-WINDOW :REMOVE-SELECTION-SUBSTITUTE) (REMOVE-WINDOW SUGGEST-WINDOW)
  (AND TV:SELECTION-SUBSTITUTE
       (TV:SHEET-ME-OR-MY-KID-P TV:SELECTION-SUBSTITUTE REMOVE-WINDOW)
       (SEND SELF :SET-SELECTION-SUBSTITUTE SUGGEST-WINDOW)))

(DEFMETHOD (ESSENTIAL-WINDOW :ULTIMATE-SELECTION-SUBSTITUTE) ()
  (IF SELECTION-SUBSTITUTE
      (SEND SELECTION-SUBSTITUTE :ULTIMATE-SELECTION-SUBSTITUTE)
    SELF))

(DEFMETHOD (ESSENTIAL-WINDOW :SELF-OR-SUBSTITUTE-SELECTED-P) ()
  (OR (EQ SELF SELECTED-WINDOW)
      (AND SELECTION-SUBSTITUTE
	   (SEND SELECTION-SUBSTITUTE :SELF-OR-SUBSTITUTE-SELECTED-P))))

;;; Every window handles :ALIAS-FOR-SELECTED-WINDOWS.
;;; Normally that returns SELF.  In that case, the window is "top level for selection".
;;; However, it may also return a superior (to one or more levels) of the window.
;;; In that case, the window is not "top level for selection".
;;; It can also return a window not a superior, if this window
;;; is the selection-substitute (to one or more levels) of that one.
;;; The window which is returned ought to be top level for selection.

;; By default, a window is "top level for selection"
;; unless one of its superiors claims to override.
(DEFMETHOD (ESSENTIAL-WINDOW :ALIAS-FOR-SELECTED-WINDOWS) ()
  (OR (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-INFERIORS)) SELF))

(DEFMETHOD (ESSENTIAL-WINDOW :ALIAS-FOR-INFERIORS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-INFERIORS)))

;;; Only "top level for selection" windows appear in the list of previously selected windows.

(DEFFLAVOR DONT-SELECT-WITH-MOUSE-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "Don't allow selection via the mouse and similar ways
Include this for windows that may be selected internally by a program, but which
will not work if just randomly selected, e.g. they do not have their own process.
They will then not show up in the Select system menu, or be gettable to in other
similar ways."))

(DEFMETHOD (DONT-SELECT-WITH-MOUSE-MIXIN :NAME-FOR-SELECTION) () NIL)

(DEFMETHOD (DONT-SELECT-WITH-MOUSE-MIXIN :ALIAS-FOR-SELECTED-WINDOWS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-SELECTED-WINDOWS)))

(DEFMETHOD (DONT-SELECT-WITH-MOUSE-MIXIN :ALIAS-FOR-INFERIORS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-SELECTED-WINDOWS)))

(DEFFLAVOR NOT-EXTERNALLY-SELECTABLE-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "Don't allow selection via the mouse and similar ways
Include this for windows that may be selected internally by a program, but which
will not work if just randomly selected, e.g. they do not have their own process.
They will then not show up in the Select system menu, or be gettable to in other
similar ways."))

(DEFMETHOD (NOT-EXTERNALLY-SELECTABLE-MIXIN :NAME-FOR-SELECTION) () NIL)

(DEFMETHOD (NOT-EXTERNALLY-SELECTABLE-MIXIN :ALIAS-FOR-SELECTED-WINDOWS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-SELECTED-WINDOWS)))

(DEFMETHOD (NOT-EXTERNALLY-SELECTABLE-MIXIN :ALIAS-FOR-INFERIORS) ()
  (AND SUPERIOR (SEND SUPERIOR :ALIAS-FOR-SELECTED-WINDOWS)))

(DEFFLAVOR ALIAS-FOR-INFERIORS-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "This window is top level for selection,
and its inferiors are all included under it for selection"))

(DEFMETHOD (ALIAS-FOR-INFERIORS-MIXIN :ALIAS-FOR-INFERIORS) ()
  (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS))

;;; Stuff for remembering a "ring buffer" of recently-selected windows
;;; This is an array whose 0'th element is the most recently selected
;;; window.  Successive elements are windows that were selected before
;;; that.  After the oldest entry, the rest of the array is NIL.  A
;;; window may only appear once in this array.  The selected-window does
;;; not appear at all, nor do deactivated windows.

;;; Only "top-level-for-selection" windows appear.

(DEFUN ADD-TO-PREVIOUSLY-SELECTED-WINDOWS (WINDOW &OPTIONAL AT-END)
  (WITHOUT-INTERRUPTS
    (SETQ WINDOW (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS))
    (AND WINDOW (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS WINDOW))
    (DO ((I 0 (1+ I))
	 (N (ARRAY-TOTAL-SIZE PREVIOUSLY-SELECTED-WINDOWS)))
	((OR (NULL WINDOW) (= I N))
	 (COND
	   (WINDOW
	    (SETQ PREVIOUSLY-SELECTED-WINDOWS
		  (ADJUST-ARRAY PREVIOUSLY-SELECTED-WINDOWS (+ N 10)))
	    (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS N) WINDOW))))
      (LET ((TEM (AREF PREVIOUSLY-SELECTED-WINDOWS I)))
	(COND
	  ((OR (NOT AT-END) (NULL TEM))
	   (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS I) WINDOW)
	   (SETQ WINDOW TEM)))))
    ())) 

(DEFUN REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS (WINDOW)
  (WITHOUT-INTERRUPTS
   (OR (NULL WINDOW)
       (DO ((I 0 (1+ I))
            (N (ARRAY-TOTAL-SIZE PREVIOUSLY-SELECTED-WINDOWS)))
           ((= I N) (NOT WINDOW))
         (COND
           ((EQ (AREF PREVIOUSLY-SELECTED-WINDOWS I) WINDOW)
            (COND
              ((NULL WINDOW)
	       (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS (1- I)) ()) (RETURN T)))
            (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS I) ())
	    (SETQ WINDOW ()))
           ((NULL WINDOW)
             (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS (1- I))
                   (AREF PREVIOUSLY-SELECTED-WINDOWS I))))))))

(DEFUN CHANGE-IN-PREVIOUSLY-SELECTED-WINDOWS (FROM-WINDOW TO-WINDOW)
  (WITHOUT-INTERRUPTS
    (SETQ FROM-WINDOW (SEND FROM-WINDOW :ALIAS-FOR-SELECTED-WINDOWS) TO-WINDOW
	  (SEND TO-WINDOW :ALIAS-FOR-SELECTED-WINDOWS))
    (COND
      ((AND (NULL FROM-WINDOW) (NULL TO-WINDOW)))
      ((AND FROM-WINDOW (NULL TO-WINDOW))
       ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
       ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
       (with-screens-previously-selected-windows (from-window)
	 (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS FROM-WINDOW)))
      ((NULL FROM-WINDOW)
       ;; This shouldn't happen, but...
       ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
       ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
       (with-screens-previously-selected-windows (to-window)
	 (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS TO-WINDOW)))
      (T
       ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
       ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
       (IF (EQ (sheet-get-screen from-window) (sheet-get-screen to-window))
	   (with-screens-previously-selected-windows (to-window)
	     (DO ((I 0 (1+ I))
		  (N (ARRAY-TOTAL-SIZE PREVIOUSLY-SELECTED-WINDOWS)))
		 ((= I N))
	       (COND
		 ((EQ (AREF PREVIOUSLY-SELECTED-WINDOWS I) FROM-WINDOW)
		  (SETF (AREF PREVIOUSLY-SELECTED-WINDOWS I) TO-WINDOW) (RETURN T)))))
	   (ERROR "Cannot change previously-selected-windows.  Window ~s is not on the
same screen as window ~s." from-window to-window))))))

(DEFUN SELECT-PREVIOUS-WINDOW (&OPTIONAL WINDOW (MOUSE-P T) (DEFAULT-TO-LISP-LISTENER T) MOUSE-SELECT)
  "Select the window that was selected before the current one.
  If WINDOW is non-NIL it tries to select that one, if it is active.
  MOUSE-P T (the default) means consider only windows selectable from the mouse.
  If no previously-selected window can be found, gets a Lisp listener,
  unless DEFAULT-TO-LISP-LISTENER is specified as NIL.
  Moves the current window to the end of the ring buffer rather than the beginning.
  Returns the window that was selected.  If MOUSE-SELECT a :MOUSE-SELECT message is
  sent rather than a :SELECT message."
  (AND WINDOW (SETQ WINDOW (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS))
       (EQ (SEND WINDOW :STATUS) :DEACTIVATED) (SETQ WINDOW ()))
  (OR WINDOW
      (DOTIMES (I (ARRAY-TOTAL-SIZE PREVIOUSLY-SELECTED-WINDOWS))
        (AND (SETQ WINDOW (AREF PREVIOUSLY-SELECTED-WINDOWS I))
             (OR (NOT MOUSE-P)
		 (SEND WINDOW :NAME-FOR-SELECTION))
	     (RETURN WINDOW)))
      (SETQ WINDOW (AND DEFAULT-TO-LISP-LISTENER (IDLE-LISP-LISTENER))))
  (DELAYING-SCREEN-MANAGEMENT			;Avoid auto-select
    (LET ((SW SELECTED-WINDOW))
      (COND
	(SW
	 ;; Changing :DESELECT RESTORE-SELECTED option to :END does the same thing,
	 ;; and guarantees that the deselected windows gets put in it's own screen's
	 ;; PREVIOUSLY-SELECTED-WINDOWS... CJJ 06/02/88.
	 ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
	 (SEND sw :deselect :end)
;;	 (SEND SW :DESELECT ())
;;	    (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SW T)
	 )))
    (COND
      ((AND WINDOW MOUSE-SELECT)
       (SEND WINDOW :MOUSE-SELECT))
      (WINDOW (SHEET-FREE-TEMPORARY-LOCKS WINDOW)
	      (SEND WINDOW :SELECT)))
    NIL)
  WINDOW) 

(DEFUN DESELECT-AND-MAYBE-BURY-WINDOW (WINDOW &OPTIONAL (DESELECT-MODE :LAST))
  "Reselect previously selected window and bury WINDOW if that leaves it deexposed.
DESELECT-MODE may be :FIRST or :LAST, which says where to put WINDOW
on the list of previously selected windows.
:FIRST makes it the one that Terminal S will select."
  (DELAYING-SCREEN-MANAGEMENT
    (SEND WINDOW :DESELECT DESELECT-MODE)
    (UNLESS (SHEET-EXPOSED-P WINDOW) (SEND WINDOW :BURY))
    NIL))

;;; Basic set-edges stuff
(DEFFLAVOR ESSENTIAL-SET-EDGES () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:METHOD-COMBINATION (:OR :BASE-FLAVOR-FIRST :VERIFY-NEW-EDGES))
  (:DOCUMENTATION :ESSENTIAL-MIXIN "Normal EDGES getting//setting messages
Provides :SET-EDGES and related messages such as :SET-SIZE, :SET-POSITION, :FULL-SCREEN,
and :CENTER-AROUND."))

(DEFWRAPPER (ESSENTIAL-SET-EDGES :SET-EDGES) ((NL NT NR NB OPTION) . BODY)
  `(LET ((LIST (MULTIPLE-VALUE-LIST
		(SEND SUPERIOR :INFERIOR-SET-EDGES SELF NL NT NR NB OPTION))))
     (IF (NOT (CAR LIST))
	 ;; This used to be (PROG NIL (VALUES..)) which can't do anything  PHM 7/29/87
	 (CDR LIST)
	 . ,BODY)))


(DEFMETHOD (ESSENTIAL-SET-EDGES :AFTER :INIT) (IGNORE)
  (LET ((ERROR-MESSAGE (SEND SELF :VERIFY-NEW-EDGES X-OFFSET Y-OFFSET WIDTH HEIGHT)))
    (IF (NOT (NULL ERROR-MESSAGE))
	(FERROR NIL ERROR-MESSAGE))))

(DEFUN screen-has-not-changed-width-p ()
  "Returns T if screen and self agree as to the width of the screen, otherwise NIL."
  (DECLARE (:self-flavor essential-set-edges))
    (OR (NULL superior)
	(NULL screen-array)
	(NULL (sheet-screen-array (sheet-get-screen self))) ;; may 9-8-88
	(= (ARRAY-DIMENSION screen-array 1)
	   (ARRAY-DIMENSION (sheet-screen-array (sheet-get-screen self)) 1))))

;;patched on 11 Dec 85 for TWE by GSM
(DEFUN SYSTEM-SET-EDGES (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM &OPTIONAL OPTION &AUX
			 ;;make sure that the arguments are integers
			 (NEW-WIDTH (- (SETQ NEW-RIGHT (TRUNCATE NEW-RIGHT))	;TWE
				       (SETQ NEW-LEFT (TRUNCATE NEW-LEFT))))	;TWE
			 (NEW-HEIGHT (- (SETQ NEW-BOTTOM (TRUNCATE NEW-BOTTOM))	;TWE
					(SETQ NEW-TOP (TRUNCATE NEW-TOP))))	;TWE
			 ERROR WINDOW-TO-BE-DEEXPOSED)
  (DECLARE (:SELF-FLAVOR ESSENTIAL-SET-EDGES))
  (DELAYING-SCREEN-MANAGEMENT
   (DO (DONE
        RESULT)
       (NIL)
     (SETQ WINDOW-TO-BE-DEEXPOSED
           (CATCH 'SET-EDGES
             (LOCK-SHEET (SELF)
                         (SETQ RESULT
                               (COND
                                 ((SETQ ERROR
                                        (SEND SELF :VERIFY-NEW-EDGES NEW-LEFT NEW-TOP NEW-WIDTH
                                              NEW-HEIGHT))
                                  ;; Can't put window there
                                  (CASE OPTION
                                    (:VERIFY NIL)
                                    (OTHERWISE (FERROR () ERROR))))
                                 ((EQ OPTION :VERIFY)
                                  ;; "Only want to know"
				  T )
                                 ((AND (= NEW-WIDTH WIDTH)
				       (screen-has-not-changed-width-p)
				       (= NEW-HEIGHT HEIGHT)
                                       (= NEW-LEFT X-OFFSET)
				       (= NEW-TOP Y-OFFSET))
                                  ;;Not changing size or position, just return T (we do the verify 
                                  ;; anyway in case something in the environment has made the current
                                  ;; size no longer "ok", such as having the size of the
                                  ;; superior change.)
                                  T)
                                 ((AND (= NEW-WIDTH WIDTH)
				       (screen-has-not-changed-width-p)
				       (= NEW-HEIGHT HEIGHT))
                                  ;; Only moving the window, move it's bits behind its back
                                  (LET ((CURRENT-RECTANGLE
                                         (LIST X-OFFSET Y-OFFSET
					       (+ X-OFFSET WIDTH)
                                               (+ Y-OFFSET HEIGHT))))
                                    (COND
                                      ((NOT EXPOSED-P)
                                       (SHEET-SET-DEEXPOSED-POSITION NEW-LEFT NEW-TOP)
                                       (APPLY #'SCREEN-AREA-HAS-CHANGED SELF CURRENT-RECTANGLE)
                                       (SCREEN-CONFIGURATION-HAS-CHANGED SELF))
                                      ((SHEET-TEMPORARY-P)
                                       ;; For temporary windows, just deexpose and reexpose
                                       (LET ((SELECT-P (EQ SELF SELECTED-WINDOW)))
                                         (SEND SELF :DEEXPOSE)
                                         (SEND SELF :EXPOSE () () NEW-LEFT NEW-TOP)
                                         (AND SELECT-P (SEND SELF :SELECT))))
                                      (T
                                       (OR
                                        (SHEET-BOUNDS-WITHIN-SHEET-P NEW-LEFT NEW-TOP WIDTH
                                                                     HEIGHT SUPERIOR)
                                        (FERROR ()
                                                "Attempt to move sheet ~S outside of superior"
                                                SELF))
                                       ;; Make sure everyone under us is deexposed
                                       (WITHOUT-INTERRUPTS
                                        (DOLIST (SISTER (SHEET-EXPOSED-INFERIORS SUPERIOR))
                                          (COND
                                            ((AND (NEQ SELF SISTER)
                                                  (SHEET-OVERLAPS-P SISTER NEW-LEFT NEW-TOP
                                                                    WIDTH HEIGHT))
                                             (THROW 'SET-EDGES
                                                    SISTER)))))
                                       (SHEET-SET-EXPOSED-POSITION NEW-LEFT NEW-TOP)
                                       (APPLY #'SCREEN-AREA-HAS-CHANGED SELF CURRENT-RECTANGLE)
                                       (SCREEN-CONFIGURATION-HAS-CHANGED SELF)))))
                                 (T
                                  (LET ((CURRENT-RECTANGLE
                                         (LIST X-OFFSET Y-OFFSET (+ X-OFFSET WIDTH)
                                               (+ Y-OFFSET HEIGHT))))
                                    (PRESERVE-SUBSTITUTE-STATUS SELF
				      (WITH-SHEET-DEEXPOSED (SELF)
					(AND BIT-ARRAY
					     (PAGE-IN-PIXEL-ARRAY BIT-ARRAY () (LIST WIDTH HEIGHT)))
					(SEND SELF :CHANGE-OF-SIZE-OR-MARGINS 
					      :LEFT NEW-LEFT :TOP NEW-TOP :WIDTH
					      NEW-WIDTH :HEIGHT NEW-HEIGHT)
					(SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
					  (SEND SELF :REFRESH :SIZE-CHANGED))))
                                    (AND BIT-ARRAY (PAGE-OUT-ARRAY BIT-ARRAY))
                                    (SETQ MOUSE-RECONSIDER T)
                                    (APPLY #'SCREEN-AREA-HAS-CHANGED SELF CURRENT-RECTANGLE)
                                    (SCREEN-CONFIGURATION-HAS-CHANGED SELF)))))
                         (SETQ DONE T))))
     (IF DONE
	 (RETURN RESULT ERROR)
	 (SEND WINDOW-TO-BE-DEEXPOSED :DEEXPOSE))))) 

(DEFMETHOD (ESSENTIAL-SET-EDGES :SET-EDGES) (&REST ARGS) (APPLY #'SYSTEM-SET-EDGES ARGS))

(DEFMETHOD (ESSENTIAL-SET-EDGES :VERIFY-NEW-EDGES) (NL NT NW NH)
  "Verifies that the edges are ok.  This method returns NIL unless the edges do not allow
enough room for the margins, or the window is exposed and will not fit within its superior."
  (COND ((OR (< NW (+ LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE))
	     (< NH (+ TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)))
	 "Not enough room for margins")
	((AND EXPOSED-P
	      (NOT (SHEET-BOUNDS-WITHIN-SHEET-P NL NT NW NH SUPERIOR)))
	 "Attempt to expose outside of superior")))

(DEFMETHOD (ESSENTIAL-SET-EDGES :SET-SIZE) (NEW-WIDTH NEW-HEIGHT &OPTIONAL OPTION)
  (SEND SELF :SET-EDGES X-OFFSET Y-OFFSET
	   (+ NEW-WIDTH X-OFFSET) (+ NEW-HEIGHT Y-OFFSET)
	   OPTION))

(DEFMETHOD (ESSENTIAL-SET-EDGES :SET-INSIDE-SIZE) (NEW-WIDTH NEW-HEIGHT &OPTIONAL OPTION)
  (SEND SELF :SET-EDGES X-OFFSET Y-OFFSET
	   (+ X-OFFSET NEW-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)
	   (+ Y-OFFSET NEW-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
	   OPTION))

(DEFMETHOD (ESSENTIAL-SET-EDGES :SET-POSITION) (NEW-X NEW-Y &OPTIONAL OPTION)
  (SEND SELF :SET-EDGES NEW-X NEW-Y
	   (+ WIDTH NEW-X) (+ HEIGHT NEW-Y)
	   OPTION))

(DEFMETHOD (ESSENTIAL-SET-EDGES :FULL-SCREEN) (&OPTIONAL OPTION)
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (SEND SUPERIOR :INSIDE-EDGES)
    (SEND SELF :SET-EDGES LEFT TOP RIGHT BOTTOM OPTION)))

(DEFMETHOD (ESSENTIAL-SET-EDGES :CENTER-AROUND) (X Y)
  (CENTER-WINDOW-AROUND SELF X Y))

(DEFMETHOD (ESSENTIAL-SET-EDGES :EXPOSE-NEAR) (MODE &OPTIONAL (WARP-MOUSE-P T))
  (EXPOSE-WINDOW-NEAR SELF MODE WARP-MOUSE-P))

(DEFUN CENTER-WINDOW-AROUND (WINDOW X Y &AUX (W (SHEET-WIDTH WINDOW))
					     (H (SHEET-HEIGHT WINDOW))
					     (SUPERIOR (SHEET-SUPERIOR WINDOW))
					     SH SW)
  (SETQ X (MAX (SHEET-INSIDE-LEFT SUPERIOR) (- X (TRUNCATE W 2)))
	Y (MAX (SHEET-INSIDE-TOP SUPERIOR) (- Y (TRUNCATE H 2))))
  (AND (> (+ X W) (SETQ SW (SHEET-INSIDE-RIGHT SUPERIOR)))
       (SETQ X (MAX (SHEET-INSIDE-LEFT SUPERIOR) (- SW W))))
  (AND (> (+ Y H) (SETQ SH (SHEET-INSIDE-BOTTOM SUPERIOR)))
       (SETQ Y (MAX (SHEET-INSIDE-TOP SUPERIOR) (- SH H))))
  (SEND WINDOW :SET-POSITION X Y)
  (block nil (RETURN (+ X (TRUNCATE W 2)) (+ Y (TRUNCATE H 2)))))

;;; Expose the window next to the rectangle, to the right if it will fit
(DEFUN MOVE-WINDOW-NEAR-RECTANGLE (WINDOW LEFT TOP RIGHT BOTTOM
				   &OPTIONAL (EXPOSE-P T) (WARP-MOUSE-P T)
				   &AUX WIDTH HEIGHT
				   SUPERIOR NLEFT NTOP NRIGHT NBOTTOM TEM)
  "Move WINDOW near the specified rectangle.
Expose the window there unless EXPOSE-P is NIL.
Move the mouse to the window unless WARP-MOUSE-P is NIL."
  (MULTIPLE-VALUE-SETQ (WIDTH HEIGHT)
    (SEND WINDOW :SIZE))
  (SETQ SUPERIOR (SHEET-SUPERIOR WINDOW))
  ;; Assuming window will go beside rectangle, try to center it vertically
  ;; but if that doesn't work butt it against the bottom of the superior.
  (SETQ NTOP
        (MIN (- (SHEET-INSIDE-BOTTOM SUPERIOR) HEIGHT)
             (MAX (SHEET-INSIDE-TOP SUPERIOR)
                  (- (TRUNCATE (+ TOP BOTTOM) 2) (TRUNCATE HEIGHT 2))))
        NBOTTOM (+ NTOP HEIGHT))
  (COND
    ((>= (SHEET-INSIDE-RIGHT SUPERIOR) (SETQ TEM (+ RIGHT WIDTH)))
     (SETQ NLEFT RIGHT NRIGHT TEM))
    ((<= (SHEET-INSIDE-LEFT SUPERIOR) (SETQ TEM (- LEFT WIDTH)))
     (SETQ NRIGHT LEFT NLEFT TEM))
    (T       ;Not enough room on either side, center it horizontally above or below the rect
     (SETQ NLEFT
           (MIN (- (SHEET-INSIDE-RIGHT SUPERIOR) WIDTH)
                (MAX (SHEET-INSIDE-LEFT SUPERIOR)
                     (- (TRUNCATE (+ LEFT RIGHT) 2) (TRUNCATE WIDTH 2))))
           NRIGHT (+ NLEFT WIDTH))
     (COND
       ((<= (SHEET-INSIDE-TOP SUPERIOR) (SETQ TEM (- TOP HEIGHT)))
	(SETQ NBOTTOM TOP NTOP TEM))
       ((>= (SHEET-INSIDE-BOTTOM SUPERIOR) (SETQ TEM (+ BOTTOM HEIGHT)))
        (SETQ NTOP BOTTOM NBOTTOM TEM))
       (T (SETQ NTOP (SHEET-INSIDE-TOP SUPERIOR) NBOTTOM (+ NTOP HEIGHT))))))
  (SEND WINDOW :SET-EDGES NLEFT NTOP NRIGHT NBOTTOM :TEMPORARY)
  (AND EXPOSE-P
       (SEND WINDOW :EXPOSE))
  (AND WARP-MOUSE-P
       (SEND WINDOW :SET-MOUSE-POSITION (TRUNCATE WIDTH 2) (TRUNCATE HEIGHT 2)))) 

; (compiler:make-obsolete EXPOSE-WINDOW-NEAR "use the :EXPOSE-NEAR operation.")
(DEFUN EXPOSE-WINDOW-NEAR (WINDOW MODE &OPTIONAL (WARP-MOUSE-P T) (EXPOSE-P T))
  "Move WINDOW near a place specified in MODE.
MODE is a list whose car is a keyword saying what the rest of the list means.
 (:POINT x y) - center window around that point and move the mouse there.
 (:MOUSE) - center window around the current mouse position.
 (:RECTANGLE left top right bottom) - put window next to that rectangle.
 (:WINDOW windows...) - put this window near but not on top of the others.
Expose the window there unless EXPOSE-P is NIL.
Move the mouse to the window unless WARP-MOUSE-P is NIL."
  (COND
    ((NOT (SHEET-EXPOSED-P WINDOW))
     (CASE (FIRST MODE)
       (:POINT
	(LET ((X (SECOND MODE))
	      (Y (THIRD MODE)))
	  (MULTIPLE-VALUE-SETQ (x y) (SEND WINDOW :CENTER-AROUND X Y))
	  (IF (SHEET-ME-OR-MY-KID-P (SHEET-SUPERIOR WINDOW) MOUSE-SHEET)
	     (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
		 (SHEET-CALCULATE-OFFSETS (SHEET-SUPERIOR WINDOW) MOUSE-SHEET)
	       (AND WARP-MOUSE-P (MOUSE-WARP (+ X X-OFF) (+ Y Y-OFF)))))))
       (:MOUSE
	(IF (SHEET-ME-OR-MY-KID-P (SHEET-SUPERIOR WINDOW) MOUSE-SHEET)
	   (MULTIPLE-VALUE-BIND (X-OFF Y-OFF) (SHEET-CALCULATE-OFFSETS (SHEET-SUPERIOR WINDOW) MOUSE-SHEET)
	     (MULTIPLE-VALUE-BIND (X Y) (SEND WINDOW :CENTER-AROUND (- MOUSE-X X-OFF) (- MOUSE-Y Y-OFF))
	       (AND WARP-MOUSE-P (MOUSE-WARP (+ X X-OFF) (+ Y Y-OFF)))))
	   ;; If mouse is not on a relevant sheet for this window,
	   ;; pick any old place.
	   (SEND WINDOW :CENTER-AROUND 0 0)))
       (:RECTANGLE
	(MOVE-WINDOW-NEAR-RECTANGLE WINDOW
				    (SECOND MODE) (THIRD MODE) (FOURTH MODE) (FIFTH MODE)
				    () WARP-MOUSE-P))
       (:WINDOW
	(LOOP FOR NEAR-WINDOW IN (CDR MODE) WITH (LEFT1 RIGHT1 TOP1 BOTTOM1 X-OFF Y-OFF) DO
	   (MULTIPLE-VALUE-SETQ (LEFT1 TOP1 RIGHT1 BOTTOM1)
	     (SEND NEAR-WINDOW :EDGES))
	   (MULTIPLE-VALUE-BIND (X-OFF-1 Y-OFF-1)
	       (SHEET-CALCULATE-OFFSETS (SHEET-SUPERIOR WINDOW)
					(SHEET-GET-SCREEN WINDOW))
	     (MULTIPLE-VALUE-BIND (X-OFF-2 Y-OFF-2)
		 (SHEET-CALCULATE-OFFSETS (SHEET-SUPERIOR NEAR-WINDOW)
					  (SHEET-GET-SCREEN NEAR-WINDOW))
	       (SETQ X-OFF (- X-OFF-1 X-OFF-2)
		     Y-OFF (- Y-OFF-1 Y-OFF-2))))
	   MINIMIZE (- LEFT1 X-OFF) INTO LEFT
	   MINIMIZE (- TOP1 Y-OFF) INTO TOP
	   MAXIMIZE (- RIGHT1 X-OFF) INTO RIGHT
	   MAXIMIZE (- BOTTOM1 Y-OFF) INTO BOTTOM
	   FINALLY (MOVE-WINDOW-NEAR-RECTANGLE WINDOW LEFT TOP RIGHT BOTTOM
					       () WARP-MOUSE-P)))
       (OTHERWISE (FERROR () "~S invalid mode" (FIRST MODE))))
     (AND EXPOSE-P (SEND WINDOW :EXPOSE)))))

;;;Things that hack margins (borders and labels and other such things)

;;;In order to interact correctly with adjusting the size of the margins, flavors
;;;that handle an area of the window further outside should come higher in the hierarchy,
;;;that is their pre-daemons should be called first.

(COMMENT
;Here is what you write to make a mixin define something that uses up margin space:  

;; This links this mixin into the computation of how much margin space is used.
;; :PASS-ON method combination is used, so actually four values are expected
;; and they resemble the arguments (but they are different).
(DEFMETHOD (MUMBLE-MARGIN-MIXIN :COMPUTE-MARGINS) (LM TR MM BM)
  (SEND SELF :RECALCULATE-MUMBLE-MARGINS LM TM RM BM))

;This method returns updated values of LM, TM, RM and BM that are made
;larger as appropriate, to take account of the space used up by the "mumbles".
;It should also record 
;:RECALCULATE-MUMBLE-MARGINS is a separate operation so that mixins
;can modify where the mumbles go in the margins by redefining it.
(DEFMETHOD (MUMBLE-MARGIN-MIXIN :RECALCULATE-MUMBLE-MARGINS) (LM TM RM BM)
  ;; Here we assume that CURRENT-MUMBLES is an instance variable
  ;; that specifies what kind of mumbles this window should display now,
  ;; and MUMBLE-MARGIN-WIDTH figures out how wide a space they need.
  ;; MUMBLE-MARGIN-AREA is set to a list describing the rectangle
  ;; where the mumbles should go;
  ;; all four values relative to outside top left corner of window.
  (LET ((WID (MUMBLE-MARGIN-WIDTH CURRENT-MUMBLES)))
    (SETQ MUMBLE-MARGIN-AREA (LIST LM TM (+ LM WID) (- TV:HEIGHT BM)))
    (VALUES (+ LM WID) TM RM BM)))

;; Here is an example of an operation provided to the user
;; whereby he can change the stuff to go in the margins.
(DEFMETHOD (MUMBLE-MARGIN-MIXIN :SET-MUMBLES) (NEW-MUMBLES)
  (SETQ CURRENT-MUMBLES (CANONICALIZE-MUMBLE-SPEC NEW-MUMBLES))
  ;; Cause the changed specs for mumbles to be redigested
  ;; together with all the other kinds of margin items,
  ;; and the window inside size to be changed if necessary.
  (SEND SELF :REDEFINE-MARGINS)
  (WHEN RESTORED-BITS-P
    ;; This is true if the margin area has not changed.
    ;; We must clear out the margin area for mumbles and draw the new mumbles there.
    ;; (If the margin size changed, everything has been updated by :REDEFINE-MARGINS).
    (ERASE-MUMBLE-AREA MUMBLE-MARGIN-AREA)
    (DRAW-MUMBLES CURRENT-MUMBLES MUMBLE-MARGIN-AREA)))

;; This operation's purpose is to redraw everything in the margins.
;; So each mixin that defines something in the margins must add to it.
(DEFMETHOD (MUMBLE-MARGIN-MIXIN :AFTER :REFRESH-MARGINS) ()
  (DRAW-MUMBLES CURRENT-MUMBLES MUMBLE-MARGIN-AREA))
);end COMMENT

(DEFMETHOD (ESSENTIAL-SET-EDGES :REDEFINE-MARGINS) ()
  (SETQ RESTORED-BITS-P T)
  (MULTIPLE-VALUE-BIND (LM TM RM BM)
      (SEND SELF :COMPUTE-MARGINS 0 0 0 0)
    (UNLESS (AND (= LEFT-MARGIN-SIZE   LM)
		 (= TOP-MARGIN-SIZE    TM)
		 (= RIGHT-MARGIN-SIZE  RM)
		 (= BOTTOM-MARGIN-SIZE BM)
		 (OR (NULL superior)
		     (= locations-per-line
			(sheet-locations-per-line superior))))
      (PRESERVE-SUBSTITUTE-STATUS SELF
	(WITH-SHEET-DEEXPOSED (SELF)
	  (AND BIT-ARRAY (SI:PAGE-IN-ARRAY BIT-ARRAY))
	  (LET ((INSIDE-SIZE-CHANGED
		  (SEND SELF :CHANGE-OF-SIZE-OR-MARGINS
			        ;; drastic changes in margins may over-reach current window size
			        ;; In these cases make the window wider.  PMH 12/9/87
			        :WIDTH (MAX WIDTH (+ LM RM))
				:HEIGHT (MAX HEIGHT (+ TM BM))
				:LEFT-MARGIN-SIZE   LM
				:TOP-MARGIN-SIZE    TM
				:RIGHT-MARGIN-SIZE  RM
				:BOTTOM-MARGIN-SIZE BM)))
	    (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
	      (SEND SELF :REFRESH (IF INSIDE-SIZE-CHANGED
					  :SIZE-CHANGED
					:MARGINS-ONLY))))
	  (AND BIT-ARRAY (SI:PAGE-OUT-ARRAY BIT-ARRAY)))))))

(DEFMETHOD (ESSENTIAL-SET-EDGES :BEFORE :INIT) (IGNORE)
  (MULTIPLE-VALUE-SETQ (LEFT-MARGIN-SIZE TOP-MARGIN-SIZE RIGHT-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
    (SEND SELF :COMPUTE-MARGINS 0 0 0 0))) 

(DEFMETHOD (ESSENTIAL-SET-EDGES :DEFAULT :COMPUTE-MARGINS) (LM TM RM BM)
  (VALUES LM TM RM BM))

;;;Borders - a kind of thing to put in a margin.
(DEFFLAVOR BORDERS-MIXIN ((BORDERS T) (BORDER-MARGIN-WIDTH 1)
                          (BORDER-COLOR *DEFAULT-border-color*)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES BORDERS BORDER-MARGIN-WIDTH BORDER-COLOR)
  (:INITABLE-INSTANCE-VARIABLES BORDERS BORDER-MARGIN-WIDTH BORDER-COLOR)
  (:DOCUMENTATION :MIXIN "Normal BORDERS.
This flavor should provide general enough handling of the borders for most uses, see
the description of the :BORDERS init option for the format of the BORDERS instance
variable.  The border is drawn using border color."))

(DEFMETHOD (BORDERS-MIXIN :SET-BORDER-MARGIN-WIDTH) (NEW-WIDTH)
  (SETQ BORDER-MARGIN-WIDTH NEW-WIDTH)
  (SEND SELF :REDEFINE-MARGINS))

;;; Hook for color support.
(DEFMETHOD (BORDERS-MIXIN :SET-BORDER-COLOR) (COLOR-VALUE)
  (PROG1  ;; use a prog1 so the value returned is meaningful...
    (SETQ border-color COLOR-VALUE)
    (draw-borders char-aluf)))

(DEFMETHOD (BORDERS-MIXIN :after :restore-default-colors)()
  (send self :set-border-color *default-border-color*))

(DEFMETHOD (BORDERS-MIXIN :SET-BORDERS) (NEW-BORDERS)
  (SEND SELF :SET-BORDERS-INTERNAL NEW-BORDERS 0 0 0 0)
  (SEND SELF :REDEFINE-MARGINS))

(DEFMETHOD (BORDERS-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (SEND SELF :SET-BORDERS-INTERNAL BORDERS LM TM RM BM))

(DEFMETHOD (BORDERS-MIXIN :SET-BORDERS-INTERNAL) (SPEC LM TM RM BM)
  (MULTIPLE-VALUE-SETQ (BORDERS LM TM RM BM)
    (PARSE-BORDERS-SPEC SPEC LM TM RM BM 'DRAW-RECTANGULAR-BORDER))
  (VALUES LM TM RM BM)) 

;;;This handles the actual drawing of the borders
(DEFUN DRAW-BORDERS (ALU)
  (DECLARE (:SELF-FLAVOR BORDERS-MIXIN))
  (prepare-color (self (SEND self :border-color))
    (SHEET-FORCE-ACCESS (SELF)
      (PREPARE-SHEET (SELF)
        (DOLIST (BORDER BORDERS)
	  (AND BORDER
	     (NEQ BORDER :ZERO)
	     (LET ((LEFT (SECOND BORDER))
		   (TOP (THIRD BORDER))
		   (RIGHT (FOURTH BORDER))
		   (BOTTOM (FIFTH BORDER)))
	       (SEND (FIRST BORDER) SELF ALU
			(IF (MINUSP LEFT) (+ LEFT WIDTH) LEFT)
			(IF (MINUSP TOP) (+ TOP HEIGHT) TOP)
			(IF (PLUSP RIGHT) RIGHT (+ RIGHT WIDTH))
			(IF (PLUSP BOTTOM) BOTTOM (+ BOTTOM HEIGHT))))))))
  )  
)

;;;This is called with the new border specification and the current (relative to this
;;;redefining) margins, and should return the canonical form of the border, and the four new
;;;margins.
(DEFUN PARSE-BORDERS-SPEC (SPEC LM TM RM BM FUNCTION &OPTIONAL DEFAULT-SIZE new-color)
  (DECLARE (:SELF-FLAVOR BORDERS-MIXIN))
  (COND
   ;;NIL means no borders at all
   (SPEC
    ;;A symbol or an number means that type for each of the four, else make a copy
    ;;a plist of (:LEFT FOO :RIGHT BAR) works too
    (IF new-color
      (SETQ border-color new-color)
    )
    (SETQ SPEC
	  (COND
	    ((ATOM SPEC) (SETQ SPEC (LIST SPEC SPEC SPEC SPEC)))
	    ((MEMBER (CAR SPEC) '(:LEFT :RIGHT :TOP :BOTTOM :COLOR) :TEST #'EQ)
	     (DO ((NSPEC
		   (IF (ATOM BORDERS)
		       (LIST BORDERS BORDERS BORDERS BORDERS)
                       ;;ELSE
                       ;; Create an empty borders specification.
                       (LIST NIL NIL NIL NIL)))
		  (SPEC SPEC (CDDR SPEC)))
		 ((NULL SPEC)
		  NSPEC)
	       (SETF
		(NTH (POSITION (CAR SPEC) (THE LIST '(:LEFT :RIGHT :TOP :BOTTOM)) :TEST #'EQ)
		     NSPEC)
		(CADR SPEC))))
	    (T (COPY-LIST SPEC))))
    (DO ((SPEC SPEC (CDR SPEC))
	 (ITEM))
	((NULL SPEC))
      (COND
	((OR (NULL (SETQ ITEM (CAR SPEC)))
	     (EQ ITEM :ZERO)))
	;;A number means that width of the default function
	((NUMBERP ITEM) (SETF (CAR SPEC) (CONS FUNCTION ITEM)))
	;;A symbol means that function and its default width
	((SYMBOLP ITEM)
	 (AND (EQ ITEM T)
	      (SETQ ITEM FUNCTION))
	 (SETF (CAR SPEC) (CONS ITEM (OR DEFAULT-SIZE
					 (GET ITEM 'DEFAULT-BORDER-SIZE)))))))
    (DO ((SPEC SPEC (CDR SPEC))
	 (TYPES '(:LEFT :TOP :RIGHT :BOTTOM) (CDR TYPES))
	 (TYPE)
	 (ITEM)
	 (-WIDTH-))
	((NULL SPEC))
	 ;;A cons of a symbol and a number is the CAR function with the CDR width
      (AND (SETQ ITEM (CAR SPEC))
	   (CONSP ITEM)
	   (SETQ -WIDTH- (CDR ITEM))
	 (IF (ATOM -WIDTH-)
	    (SETF (CDR ITEM)
		  (LIST (IF (EQ (SETQ TYPE (CAR TYPES)) :RIGHT) -WIDTH- 0)
			(IF (EQ TYPE :BOTTOM) -WIDTH- 0) (IF (EQ TYPE :LEFT) -WIDTH- 0)
			(IF (EQ TYPE :TOP) -WIDTH- 0)))
	    ;;Else make entries relative
	    (PROGN
	      (SETQ TYPE (CAR TYPES))
	      (LET ((-WIDTH- (- (FOURTH ITEM) (SECOND ITEM)))
		    (-HEIGHT- (- (FIFTH ITEM) (THIRD ITEM))))
		(SETF (SECOND ITEM) (IF (EQ TYPE :RIGHT) -WIDTH- 0))
		(SETF (THIRD ITEM) (IF (EQ TYPE :BOTTOM) -HEIGHT- 0))
		(SETF (FOURTH ITEM) (IF (EQ TYPE :LEFT) -WIDTH- 0))
		(SETF (FIFTH ITEM) (IF (EQ TYPE :TOP) -HEIGHT- 0)))))))
    ;;Now adjust all non-NIL items for the current margins
    (DO ((SPEC SPEC (CDR SPEC))
	 (TYPES '(:LEFT :TOP :RIGHT :BOTTOM) (CDR TYPES))
	 (TYPE)
	 (ITEM)
	 (-WIDTH-)
	 (-HEIGHT-))
	((NULL SPEC))
      (COND
	((AND (SETQ ITEM (CAR SPEC))
	      (CONSP ITEM))
	 (SETQ TYPE (CAR TYPES))
	 (SETQ -WIDTH- (ABS (- (FOURTH ITEM) (SECOND ITEM)))
	       -HEIGHT- (ABS (- (FIFTH ITEM) (THIRD ITEM))))
	 (COND
	   ((CASE TYPE
	      ((:LEFT :RIGHT) (ZEROP -WIDTH-))
	      ((:TOP :BOTTOM) (ZEROP -HEIGHT-)))
	    (SETF (CAR SPEC) :ZERO))
	   (T
	    ;; Order here is L R T B to give symmetry
	    (SETF (SECOND ITEM)
		  (IF (EQ TYPE :RIGHT)
		      (- (+ (SECOND ITEM) RM))
		      (+ (SECOND ITEM) LM)))
	    (SETF (FOURTH ITEM)
		  (IF (EQ TYPE :LEFT)
		      (+ (FOURTH ITEM) LM)
		      (- (+ (FOURTH ITEM) RM))))
	    (SETF (THIRD ITEM)
		  (IF (EQ TYPE :BOTTOM)
		      (- (+ (THIRD ITEM) BM))
		      (+ (THIRD ITEM) TM)))
	    (SETF (FIFTH ITEM) (IF (EQ TYPE :TOP)
				   (+ (FIFTH ITEM) TM)
				   (- (+ (FIFTH ITEM) BM))))
	    (CASE TYPE
	      (:LEFT (SETQ LM (+ LM -WIDTH-)))
	      (:TOP (SETQ TM (+ TM -HEIGHT-)))
	      (:RIGHT (SETQ RM (+ RM -WIDTH-)))
	      (:BOTTOM (SETQ BM (+ BM -HEIGHT-)))))))))))

  ;;Now allocate the border margin areas.
   (send self :compute-border-margin-area-margins spec lm tm rm bm))
   
(defmethod (borders-mixin :compute-border-margin-area-margins) (spec lm tm rm bm)
  "Allocate space for the border margin area.  Scroll-bar-mixin wraps around
 this method to allocate its space between the border and border margin area."
  (when spec
    (and (first spec) (setq lm (+ lm border-margin-width)))
    (and (second spec) (setq tm (+ tm border-margin-width)))
    (and (third spec) (setq rm (+ rm border-margin-width)))
    (and (fourth spec) (setq bm (+ bm border-margin-width))))
  (values spec lm tm rm bm))

(DEFMETHOD (BORDERS-MIXIN :AFTER :REFRESH-MARGINS) ()
  (DRAW-BORDERS CHAR-ALUF))

(SETF (GET 'DRAW-RECTANGULAR-BORDER 'DEFAULT-BORDER-SIZE) 1)
(DEFUN DRAW-RECTANGULAR-BORDER (WINDOW ALU LEFT TOP RIGHT BOTTOM)
  (prepare-color (self (send window :border-color))
    (prepare-sheet (window)
      (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ALU WINDOW))
  )
)

;;; If the next method takes precedence then the borders mixin is
;;; present and it preceded the label-mixin if indeed it is mixed at all
;;;  ie. the "boxing" is present  PMH 4/6/88
(defmethod (borders-mixin :really-boxed?)()
  t)


(DEFFLAVOR MARGIN-SPACE-MIXIN ((SPACE (LIST 0 0 0 0))) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES SPACE)
  (:INITTABLE-INSTANCE-VARIABLES SPACE))

(DEFMETHOD (MARGIN-SPACE-MIXIN :BEFORE :INIT) (IGNORE)
  (COND
    ((NULL SPACE) (SETQ SPACE '(0 0 0 0)))
    ((EQ SPACE T) (SETQ SPACE '(1 1 1 1)))
    ((INTEGERP SPACE) (SETQ SPACE (MAKE-LIST 4 :INITIAL-ELEMENT SPACE)))
    ((ATOM SPACE) (SETQ SPACE '(0 0 0 0))))) 

(DEFMETHOD (MARGIN-SPACE-MIXIN :SET-SPACE) (NEW-SPACE)
  (COND
    ((NULL NEW-SPACE) (SETQ SPACE '(0 0 0 0)))
    ((EQ NEW-SPACE T) (SETQ SPACE '(1 1 1 1)))
    ((INTEGERP NEW-SPACE) (SETQ SPACE (MAKE-LIST 4 :INITIAL-ELEMENT NEW-SPACE)))
    ((ATOM NEW-SPACE) (SETQ SPACE '(0 0 0 0)))
    (T (SETQ SPACE NEW-SPACE)))
  (SEND SELF :REDEFINE-MARGINS)) 

(DEFMETHOD (MARGIN-SPACE-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (VALUES (+ LM (FIRST SPACE)) (+ TM (SECOND SPACE))
	  (+ RM (THIRD SPACE)) (+ BM (FOURTH SPACE))))

;;;Labels

(DEFFLAVOR ESSENTIAL-LABEL-MIXIN ((LABEL T)) ()
  (:GETTABLE-INSTANCE-VARIABLES LABEL)
  (:INITABLE-INSTANCE-VARIABLES LABEL)
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:REQUIRED-METHODS :PARSE-LABEL-SPEC :DRAW-LABEL)
  (:DOCUMENTATION :LOWLEVEL-MIXIN "Lowlevel LABEL handling
This flavor probably isn't any good without some other label mixin.  See LABEL-MIXIN
for the normal label handler."))



(DEFFLAVOR WINDOW-WITH-ESSENTIAL-LABEL () (STREAM-MIXIN BORDERS-MIXIN ESSENTIAL-LABEL-MIXIN
						 SELECT-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "Simple window for special label handling
Mix this with a special type of label mixin to get the simplest usable case of that mixin."))

(DEFUN ERASE-LABEL (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR essential-LABEL-MIXIN))
  (LET ((saveb (sheet-background-color self))
	(newcolor (label-background label))
        (color-system (color-system-p self)))
    (UNWIND-PROTECT 
	(when LABEL
	  (when color-system (SEND self :set-background-color (IF newcolor newcolor saveb)))
	  (SHEET-FORCE-ACCESS (SELF)
	    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
		(COMPUTE-LABEL-POSITION)
	      (PREPARE-SHEET (SELF)
		(let ((border-margin-width (and (send self :send-if-handles :really-boxed?)
						(send self :send-if-handles :border-margin-width)))
		      (rect-height (- BOTTOM TOP)))
		  ;; if there is boxing, stay just inside  PMH 3/28/88
		  (when border-margin-width
		    (setf right (+ right border-margin-width)	;scoot out to the right
			  left (- left border-margin-width)	; and the left
			  rect-height (+ rect-height border-margin-width))	;and up/down
		    (when (plusp top)		;which was it, up or down?
		      (setf top (- top border-margin-width))))	;it was up
		  (%DRAW-RECTANGLE (- RIGHT LEFT) rect-height LEFT TOP ERASE-ALUF SELF))))))
      (IF color-system (SEND self :set-background-color saveb)))
    ))

(DEFUN DRAW-LABEL (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR ESSENTIAL-LABEL-MIXIN))
  (let-if (mac-system-p)
     ((*dont-clip-at-the-margins* t))	  
  (AND LABEL
       (SHEET-FORCE-ACCESS (SELF)
	 (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
	     (COMPUTE-LABEL-POSITION)
	   (SEND SELF :DRAW-LABEL LABEL LEFT TOP RIGHT BOTTOM))))))

(DEFUN COMPUTE-LABEL-POSITION (&AUX LEFT TOP RIGHT BOTTOM)
  "Return four edges of the rectangle the label of SELF occupies.
These are relative to the top left corner of SELF."
  (DECLARE (:SELF-FLAVOR ESSENTIAL-LABEL-MIXIN)
	   (VALUES LEFT TOP RIGHT BOTTOM))
  (SETQ LEFT   (LABEL-LEFT   LABEL)
        TOP    (LABEL-TOP    LABEL)
	RIGHT  (LABEL-RIGHT  LABEL)
        BOTTOM (LABEL-BOTTOM LABEL))
  (SETQ BOTTOM (- BOTTOM TOP))			;Really height
  (AND (MINUSP TOP) (SETQ TOP (+ HEIGHT TOP)))
  (block nil (RETURN (IF (MINUSP LEFT) (+ WIDTH LEFT) LEFT)
                   TOP
		   (IF (PLUSP RIGHT) RIGHT (+ WIDTH RIGHT))
                   (+ TOP BOTTOM))))

(DEFMETHOD (ESSENTIAL-LABEL-MIXIN :LABEL-SIZE) ()
  (block nil
    (IF LABEL (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
		  (COMPUTE-LABEL-POSITION)
		(RETURN (- RIGHT LEFT) (- BOTTOM TOP)))
	      (RETURN 0 0))))

(DEFMETHOD (ESSENTIAL-LABEL-MIXIN :SET-LABEL) (NEW-LABEL)
  (SETQ LABEL (SEND SELF :PARSE-LABEL-SPEC NEW-LABEL 0 0 0 0))
  (SEND SELF :REDEFINE-MARGINS)
  (COND (RESTORED-BITS-P
         ;; This has the right dimensions, even though it is the new
         ;; label, because it occupies the same margin space.
	 (ERASE-LABEL)
	 (DRAW-LABEL))))

(DEFMETHOD (ESSENTIAL-LABEL-MIXIN :AFTER :REFRESH-MARGINS) ()
  (erase-label) (DRAW-LABEL))

(DEFMETHOD (ESSENTIAL-LABEL-MIXIN :COMPUTE-MARGINS) (LM TM RM BM)
  (MULTIPLE-VALUE-SETQ (LABEL LM TM RM BM)
    (SEND SELF :PARSE-LABEL-SPEC LABEL LM TM RM BM))
  (VALUES LM TM RM BM)) 

(DEFFLAVOR LABEL-MIXIN () (ESSENTIAL-LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Normal LABEL handling.
This is the usual type of label a window will want, it provides for an arbitrary string
in an arbitrary font."))

(DEFMETHOD (LABEL-MIXIN :AFTER :INIT) (IGNORE)
  (AND LABEL (OR (LABEL-STRING LABEL) (SETF (LABEL-STRING LABEL) NAME))))

(DEFMETHOD (label-mixin :label-color) ()
  (label-color label)
)

(DEFMETHOD (label-mixin :label-background) ()
  (label-background label)
)

;;; >>> modified for color system
(DEFMETHOD (LABEL-MIXIN :SET-LABEL-COLOR) (NEW-COLOR)
  (WHEN label
    (prog1
      (SETF (label-color label) new-color)
      ;; In some cases, RESTORED-BITS-P can be unbound.
      ;; Like if window has never been exposed.
      ;; This prevents an error.  04/10/88 KJF.
      (UNLESS (VARIABLE-BOUNDP RESTORED-BITS-P)
	(SETQ RESTORED-BITS-P t))
      (COND (RESTORED-BITS-P
	     ;; This has the right dimensions, even though it is the new
	     ;; label, because it occupies the same margin space.
	     (ERASE-LABEL)
	     (DRAW-LABEL)
	     )))))

;;; >>> added for color system
(DEFMETHOD (LABEL-MIXIN :SET-LABEL-background) (NEW-COLOR)
  (WHEN label
    (prog1
      (SETF (label-background label) new-color)
      ;; In some cases, RESTORED-BITS-P can be unbound.
      ;; Like if window has never been exposed.
      ;; This prevents an error.  04/10/88 KJF.
      (UNLESS (VARIABLE-BOUNDP RESTORED-BITS-P)
	(SETQ RESTORED-BITS-P t))
      (COND (RESTORED-BITS-P
	     ;; This has the right dimensions, even though it is the new
	     ;; label, because it occupies the same margin space.
	     (ERASE-LABEL)
	     (DRAW-LABEL)
	     )))))

(defmethod (label-mixin :after :restore-default-colors)()
  (if (typep self 'menu)
      (progn (send self :set-label-color *default-menu-label-foreground*)
	     (send self :set-label-background *default-menu-label-background*))
      (progn (send self :set-label-color *default-label-foreground*)
	     (send self :set-label-background *default-label-background*))))

;;; We make the label-spec parsing a separate function since it is
;;; called elsewhere in this file by another label mixin.
(DEFUN PARSE-LABEL-SPEC-INTERNAL (SPEC LM TM RM BM &OPTIONAL TOP-P
				       &AUX FONT LSTRING VSP CENTERED (color *default-foreground*)
				                                      (background *default-background*))
  (DECLARE (:SELF-FLAVOR LABEL-MIXIN))
  (WHEN SPEC
    (COND
      ((CONSP SPEC)
       (IF (NOT (MEMBER (CAR SPEC) '(:STRING :FONT :TOP :BOTTOM :VSP :CENTERED :COLOR :BACKGROUND) :TEST #'EQ))
	  ;; Assume SPEC is already a label structure
	  (PROGN
	    ;; If top-p has a value then don't change it.
	    (WHEN (NULL top-p)
	      (SETQ top-p (AND (NUMBERP (label-top spec)) (NOT (MINUSP (label-top spec))))))
	    (SETQ font     (label-font     spec)
		  lstring  (label-string   spec)
		  vsp      (label-vsp      spec)
		  centered (label-centered spec)
		  color    (label-color    spec)
		  background (label-background spec)))
	  ;;ELSE
	  (DO ((LIST SPEC (CDR LIST)))
	      ((NULL LIST))
	    (CASE (CAR LIST)
	      (:STRING (SETQ LSTRING (CADR LIST)
			     LIST (CDR LIST)))
	      (:FONT (SETQ FONT (CADR LIST)
			   LIST (CDR LIST)))
	      (:VSP (SETQ VSP (CADR LIST)
			  LIST (CDR LIST)))
	      (:color (SETQ color (CADR LIST)
			  LIST (CDR LIST)))
	      (:background (SETQ background (CADR LIST)
			  LIST (CDR LIST)))
	      (:CENTERED (SETQ CENTERED T))
	      (:TOP (SETQ TOP-P T))
	      (:BOTTOM (SETQ TOP-P ()))
	      (OTHERWISE (FERROR () "~S is not a recognized keyword" (CAR LIST)))))))
      ((FONT-OBJECT-P spec) (SETQ font spec))
      ((STRINGP spec) (SETQ lstring spec)) 
      ((EQ spec :top) (SETQ top-p t))
      ((EQ spec :bottom) (SETQ top-p nil))
      ((EQ spec :centered) (SETQ centered t)) ;; If :top and :bottom are allowed, :centered belongs also
      ((EQ spec t))
      (t (SETQ lstring (FORMAT () "~a" SPEC)))
      )
    (UNLESS font
      ;; We are using a default font since the caller
      ;; did not specify one.  If the superior is a
      ;; frame then this window is a pane -- use the
      ;; pane label font default.  Otherwise, use the
      ;; high-level window font default.
      (SETQ font (IF (SEND (SHEET-SUPERIOR SELF) :OPERATION-HANDLED-P :CONFIGURATION)
		     *WINDOW-PANE-LABEL-STANDARD-FONT*
		   *FRAME-LABEL-STANDARD-FONT*)))
    (SETQ FONT (SEND (SHEET-GET-SCREEN SELF) :PARSE-FONT-NAME FONT))
    (UNLESS lstring
	    ;; The label-string is empty, but other
	    ;; options were specified.  Default the
	    ;; label-string to the name of the window
	    ;; instance.
      (SETQ lstring (COND ((null name) nil)
			  ((STRINGP name) name)
			  ((SYMBOLP name) (SYMBOL-NAME name))
			  (t (FORMAT () "~a" NAME)))))
    (SETQ SPEC (LIST () () () () FONT LSTRING VSP CENTERED color background))
    (LET ((-height- (LABEL-HEIGHT SELF LSTRING FONT vsp)))
      (SETF (LABEL-LEFT SPEC) LM)
      (SETF (LABEL-RIGHT SPEC) (- RM))
      (LET ((TOP (IF TOP-P TM (- (+ BM -HEIGHT-)))))
	(SETF (LABEL-TOP SPEC) TOP)
	(SETF (LABEL-BOTTOM SPEC) (+ TOP -HEIGHT-)))
      (IF TOP-P
	  (SETQ TM (LABEL-BOTTOM SPEC))
	(SETQ BM (- (LABEL-TOP SPEC))))))
  (VALUES SPEC LM TM RM BM))

(DEFMETHOD (LABEL-MIXIN :PARSE-LABEL-SPEC) (SPEC LM TM RM BM &OPTIONAL TOP-P)
  (PARSE-LABEL-SPEC-INTERNAL SPEC LM TM RM BM TOP-P))

(DEFUN LABEL-HEIGHT (SHEET LABEL-STRING FONT &OPTIONAL LABEL-VSP)
  (COND ((NULL LABEL-STRING)	;Kludge patch for bug that label string may not be set up yet.
	 (FONT-CHAR-HEIGHT (FONT-EVALUATE FONT)))
	(T
	 (LET ((FONT (FONT-EVALUATE FONT)))
	   (MULTIPLE-VALUE-BIND (NIL FINAL-Y)
	       (SHEET-COMPUTE-MOTION SHEET 0 0 LABEL-STRING
				     0 NIL T 0 1.0S10 1.0S10 1.0S10
				     FONT
				     (+ (OR LABEL-VSP 2)
					(FONT-CHAR-HEIGHT FONT)))
	     FINAL-Y)))))

(DEFMETHOD (LABEL-MIXIN :DRAW-LABEL) (SPEC LEFT TOP RIGHT BOTTOM)
  (AND SPEC
       (LET ((FONT (FONT-EVALUATE (LABEL-FONT SPEC)))
	    )
	 (prepare-color (self (label-color spec))
	 (SEND SELF
	   (IF (LABEL-CENTERED SPEC)
	       :STRING-OUT-CENTERED-EXPLICIT
	     :STRING-OUT-EXPLICIT)
	   (LABEL-STRING SPEC) LEFT TOP RIGHT BOTTOM
	   FONT CHAR-ALUF
	   0 NIL
	   (+ (OR (LABEL-VSP SPEC) 2)
	      (FONT-CHAR-HEIGHT FONT)))
	 ))
))

(DEFMETHOD (LABEL-MIXIN :LABEL-SIZE) ()
  (IF LABEL
      (LET ((FONT (FONT-EVALUATE (LABEL-FONT LABEL))))
	(MULTIPLE-VALUE-BIND (NIL FINAL-Y NIL MAXIMUM-X)
	    (SHEET-COMPUTE-MOTION SELF 0 0 (LABEL-STRING LABEL)
				  0 NIL T 0 1.0S10 1.0S10 1.0S10
				  FONT
				  (+ (OR (LABEL-VSP LABEL) 2)
				     (FONT-CHAR-HEIGHT FONT)))
	  (VALUES MAXIMUM-X FINAL-Y)))
    (VALUES 0 0)))

(DEFMETHOD (LABEL-MIXIN :AFTER :CHANGE-OF-DEFAULT-FONT) (OLD-FONT NEW-FONT)
  (COND ((AND LABEL (EQ (LABEL-FONT LABEL) OLD-FONT))
	 (SETF (LABEL-FONT LABEL) NEW-FONT)))
  (SEND SELF :SET-LABEL LABEL))

(DEFFLAVOR DELAYED-REDISPLAY-LABEL-MIXIN ((LABEL-NEEDS-UPDATING NIL)) ()
  (:REQUIRED-FLAVORS LABEL-MIXIN)
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES LABEL-NEEDS-UPDATING)
  (:DOCUMENTATION :MIXIN "Delays the setting of the label until a normal redisplay loop.
Send a :DELAYED-SET-LABEL to cause the label to be changed when a :UPDATE-LABEL message
is sent.  This is especially useful for things with suppressed redisplay for typeahead,
where the user's typein may change the label several times, and where the label wants to
change along with the rest of the window."))

(DEFMETHOD (DELAYED-REDISPLAY-LABEL-MIXIN :DELAYED-SET-LABEL) (NEW-LABEL)
  (SETQ LABEL-NEEDS-UPDATING NEW-LABEL))

(DEFMETHOD (DELAYED-REDISPLAY-LABEL-MIXIN :UPDATE-LABEL) ()
  (COND (LABEL-NEEDS-UPDATING
	 (SEND SELF :SET-LABEL LABEL-NEEDS-UPDATING)
	 (SETQ LABEL-NEEDS-UPDATING NIL))))

(DEFFLAVOR TOP-LABEL-MIXIN () (LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Label positioned at the top
If the label is specified only as a string or defaults to the name of the window, it
will be at the top of the window."))

(DEFMETHOD (TOP-LABEL-MIXIN :PARSE-LABEL-SPEC) (SPEC LM TM RM BM)
  ;; Parse the label specification and force the label to be at the top.
  (PARSE-LABEL-SPEC-INTERNAL SPEC LM TM RM BM T))

(DEFFLAVOR CENTERED-LABEL-MIXIN () ()
  (:REQUIRED-FLAVORS LABEL-MIXIN))

(DEFMETHOD (CENTERED-LABEL-MIXIN :BEFORE :DRAW-LABEL) (&REST IGNORE)
  (IF LABEL
      (SETF (LABEL-CENTERED LABEL) T)))

(DEFFLAVOR BOX-LABEL-MIXIN (LABEL-BOX-P
                             (LABEL-BOX-COLOR *DEFAULT-BORDER-COLOR*)) ()
  (:REQUIRED-FLAVORS LABEL-MIXIN)
  :INITABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :LABEL-BOX-P T)
  (:DOCUMENTATION :MIXIN "Puts lines above and below the label (Lines at the edges of the
window are omitted) When combined with BORDERS-MIXIN, you get a box around the label.  By
specifying the label-box-color as the default-border-color, the resultant box around the
label will be the same color as the border.  By changing label-box-color, the inside line
of the label will be in the label-box-color."))

(DEFMETHOD (BOX-LABEL-MIXIN :SET-LABEL-BOX-COLOR) (COLOR-VALUE)
  (SETF label-box-color COLOR-VALUE))

(DEFMETHOD (BOX-LABEL-MIXIN :after :restore-default-colors)()
  (send self :set-label-box-color *default-border-color*))

;;;  make labels be reverse video.
(DEFMETHOD (BOX-LABEL-MIXIN :AFTER :DRAW-LABEL) (SPEC LEFT TOP RIGHT BOTTOM)
  SPEC
  (prepare-color (self label-box-color)
    (AND LABEL-BOX-P
	 (SHEET-FORCE-ACCESS (SELF)
	   ;;; the reason we don't mess with reverse video in color is that we can color our menus
	   ;;; and make them distinctive without using reverse video!
	   (PREPARE-SHEET (SELF) 	       ;; DRAW LABEL IN REVERSE-VIDEO ONLY IF TEMPORARY-WINDOW BASED on B&W system
	     (if (AND (funcall self :temporary-bit-array) (NOT (color-system-p self))) ;; here is the extra test for color
		 (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
		     (COMPUTE-LABEL-POSITION)
		   (let ((border-margin-width (and (send self :send-if-handles :really-boxed?)
						   (send self :send-if-handles :border-margin-width)))
			 (rect-height (- BOTTOM TOP)))
		     ;; if there is boxing, blast up to the borders including margin region
		     ;; PMH 3/28/88
		     (when  border-margin-width	                 ;are we boxing?
		       (setf right (+ right border-margin-width) ;include right margin area
			     left (- left border-margin-width)   ; and left margin area
			     rect-height (+ rect-height border-margin-width)) ; and up/down
		       (when (plusp top)	                 ;is it top boxing?
			 (setf top (- top border-margin-width)))) ; yes, move up some
		     (%DRAW-RECTANGLE (- RIGHT LEFT) rect-height LEFT TOP alu-xor SELF)
		     ))
		 ;; the following IS ORIGINAL DRAW-LABEL CODE (NO REVERSE-VIDEO)
		 (let ((w (and (send self :send-if-handles :really-boxed?)
			       (send self :send-if-handles :border-margin-width))))
		   (when w (setq right (+ right w)
				 left (- left w))))
		 (OR (>= BOTTOM (SHEET-INSIDE-BOTTOM))
		     (%DRAW-RECTANGLE (- RIGHT LEFT) 1 LEFT (1- BOTTOM) CHAR-ALUF SELF))
		 (OR (<= TOP    (SHEET-INSIDE-TOP))
		     (%DRAW-RECTANGLE (- RIGHT LEFT) 1 LEFT (1- TOP) CHAR-ALUF SELF))))))))

;;; Either this method masks or is masked by the BORDERS method
;;; of the same name.  If this method dominates the mixture then
;;; the label-mixin preceeded the borders mixin (if it were even included)
(defmethod (box-label-mixin :really-boxed?)()
  nil)


(DEFFLAVOR TOP-BOX-LABEL-MIXIN () (BOX-LABEL-MIXIN TOP-LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Label at the top, with a line underneath
If the label is a string or defaults to the name, it is at the top.
When combined with BORDERS-MIXIN, the label will be surrounded by a box."))

(DEFFLAVOR BOTTOM-BOX-LABEL-MIXIN () (BOX-LABEL-MIXIN LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Label at the bottom, with a line above.
If the label is a string or defaults to the name, it is at the bottom.
When combined with BORDERS-MIXIN, the label will be surrounded by a box."))

;;; Flavor that allows you to change the name of the window, and 
;;; if the label is the same as the name, changes the label, too.
(DEFFLAVOR CHANGEABLE-NAME-MIXIN () ()
  (:REQUIRED-FLAVORS LABEL-MIXIN)
  (:DOCUMENTATION :MIXIN "Allows setting of name via :SET-NAME
Also changes the label if it happens to be the same."))

(DEFMETHOD (CHANGEABLE-NAME-MIXIN :NAME) () NAME)

(DEFMETHOD (CHANGEABLE-NAME-MIXIN :SET-NAME) (NEW-NAME)
  (LET ((LABEL-EQUALS-NAME (AND LABEL (EQ (LABEL-STRING LABEL) NAME))))
    (SETQ NAME NEW-NAME)
    (COND (LABEL-EQUALS-NAME
	   (SETF (LABEL-STRING LABEL) NEW-NAME)
	   (SHEET-FORCE-ACCESS (SELF T)
	     (ERASE-LABEL)
	     (DRAW-LABEL))))))

(DEFUN LOWEST-SHEET-UNDER-POINT (SHEET X Y &OPTIONAL OPERATION (ACTIVE-CONDITION :ACTIVE))
  "Return the sheet lowest in the sheet hierarchy which contains the given point."
  ;; Trace down to find the lowest sheet under the point
  (block foo
    (DO
      ((X X (- X (SHEET-X SHEET)))
       (Y Y (- Y (SHEET-Y SHEET))))
      (NIL)
      (DO ((INFERIORS
	     (IF (EQ ACTIVE-CONDITION :EXPOSED)
		 (SHEET-EXPOSED-INFERIORS SHEET)
		 (SHEET-INFERIORS SHEET))
	     (CDR INFERIORS))
	   (INFERIOR))
	  ((NULL INFERIORS)
	   (RETURN-FROM FOO))
	(SETQ INFERIOR (CAR INFERIORS))
	(COND
	  ((AND (NOT (SHEET-INVISIBLE-TO-MOUSE-P INFERIOR))
		(>= X (SHEET-X INFERIOR))
		(>= Y (SHEET-Y INFERIOR))
		(< X (+ (SHEET-X INFERIOR) (SHEET-WIDTH INFERIOR)))
		(< Y (+ (SHEET-Y INFERIOR) (SHEET-HEIGHT INFERIOR)))
		(CASE ACTIVE-CONDITION
		      (:ACTIVE
		       (OR (SHEET-EXPOSED-P INFERIOR)
			   (SEND INFERIOR :SCREEN-MANAGE-DEEXPOSED-VISIBILITY)))
		      (:EXPOSED (NOT (SHEET-OUTPUT-HELD-P INFERIOR)))
		      (OTHERWISE T)))
	   (SETQ SHEET INFERIOR) (RETURN T))))))
  (IF (NULL OPERATION) SHEET
      ;; Now trace back up until we find someone to handle the message
      (DO ((SHEET SHEET (SHEET-SUPERIOR SHEET)))
	  ((NULL SHEET)
	   NIL)
	(AND (GET-HANDLER-FOR SHEET OPERATION) (RETURN SHEET))))) 

(DEFFLAVOR TEMPORARY-WINDOW-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :LOWLEVEL-MIXIN "Windows that save bits underneath and lock when exposed
Causes the temporary-bit-array instance variable to get set, which makes sheet exposure
behave appropriately."))

(DEFMETHOD (TEMPORARY-WINDOW-MIXIN :AFTER :INIT) (IGNORE)
  (OR (AND (VARIABLE-BOUNDP TEMPORARY-BIT-ARRAY) TEMPORARY-BIT-ARRAY)
      ;; T means will get created when needed
      (SETQ TEMPORARY-BIT-ARRAY T)))

;;; we don't want pop-up's to change the color map on :expose
;;; note that temporary windows do not become the selected window;
;;; instead they LOCK the selected window.
(defmethod (temporary-window-mixin :before :expose)(&rest ignore)
  (setf (sheet-color-map self)
	(or (and selected-window (sheet-color-map selected-window))
	    (and superior (sheet-color-map superior))
	    *default-color-map*)))


;;; The shadow borders mixin is used for temporary windows to add a gray
;;; border around the window to make it look like the window comes out of
;;; the screen -- a 3 dimensional appearance.


(DefFlavor Shadow-Borders-Mixin
	   (right-shadow-width
	    bottom-shadow-width
	    (shadow-draw-function 'draw-shadow-border)
	    (shadow-borders nil))
	   ()
  (:Required-Flavors Borders-Mixin)
  (:Initable-Instance-Variables
    right-shadow-width bottom-shadow-width shadow-draw-function)
  (:Default-Init-Plist
    :Borders 3
    :Right-Shadow-Width 6
    :Bottom-Shadow-Width 6)
  (:Documentation "Define a mixin which makes a window look like it comes out
of the screen.")
  )

(Defflavor Temporary-Shadow-Borders-Window-Mixin
  ()
  (Temporary-Window-Mixin Shadow-Borders-Mixin)
  (:Documentation "Define a mixin which can substitute for the
Temporary-Window-Mixin.")
  )

(DefSubst Shadow-Borders-Redefine-Margins (self)
  (Send self :Redefine-Margins)
  )

(DefMethod (Shadow-Borders-Mixin :Set-Right-Shadow-Width) (new-width)
  (Setq right-shadow-width new-width)
  (Shadow-Borders-Redefine-Margins self)
  )

(DefMethod (Shadow-Borders-Mixin :Set-Bottom-Shadow-Width) (new-width)
  (Setq bottom-shadow-width new-width)
  (Shadow-Borders-Redefine-Margins self)
  )

(DefMethod (Shadow-Borders-Mixin :Set-Shadow-Draw-Function) (new-function-name)
  (Setq shadow-draw-function new-function-name)
  (Shadow-Borders-Redefine-Margins self)
  )
	   
(DefMethod (Shadow-Borders-Mixin :Compute-Margins) (lm tm rm bm)
  (Multiple-Value-Bind (my-borders lm tm rm bm)
      (Send Self :Set-Shadow-Border-Internal
	    right-shadow-width bottom-shadow-width lm tm rm bm)
    (Setq shadow-borders my-borders)
    (Values lm tm rm bm))
  )

(DefMethod (Shadow-Borders-Mixin :Set-Shadow-Border-Internal)
           (RIGHT-WIDTH
            BOTTOM-WIDTH
            left-margin
            top-margin
            right-margin
            bottom-margin)
  "Compute positions for shadow borders."
  (Values `(;; Right shadow border
            ,(if (zerop RIGHT-WIDTH)
                 ;; There isn't a top shadow border.
                 :zero
               ;ELSE
               `(,shadow-draw-function
                 ,(- right-margin RIGHT-WIDTH)			; Left
                 ,top-margin                    		; Top
                 ,right-margin               			; Right
                 ,(- bottom-margin)             		; Bottom
                 0
                 ,BOTTOM-WIDTH))
	    ;; Bottom shadow border
            ,(if (zerop BOTTOM-WIDTH)
                 ;; There isn't a bottom shadow border.
                 :zero
               ;ELSE
               `(,shadow-draw-function
                 ,left-margin                   		; Left
                 ,(- (+ bottom-margin BOTTOM-WIDTH))		; Top
                 ,(- right-margin RIGHT-WIDTH)			; Right
                 ,(- bottom-margin)             		; Bottom
                 ,RIGHT-WIDTH
                 0)))
	  left-margin                   			; Left
	  top-margin                            		; Top
	  (+ right-margin RIGHT-WIDTH)				; Right
	  (+ bottom-margin BOTTOM-WIDTH))			; Bottom
  )



(DEFUN DRAW-SHADOW-BORDER (WINDOW ALU LEFT TOP RIGHT BOTTOM WIDTH-OFFSET HEIGHT-OFFSET
			   &AUX (GRAY-LEVEL 50%-gray)
			   (WINDOW-SCREEN-ARRAY (SEND WINDOW :SCREEN-ARRAY))
                           (color-sheet (color-sheet-p window)))
  "Draw the shadow for the window."
  (when color-sheet
    ;; use an array with fewer bits, since the pixels are closer together
    ;; on the color screen as compared with the monochrome screen.
    (SETQ gray-level 33%-gray))

  (PREPARE-SHEET (WINDOW)
    ;; If the window is a temporary window then draw the invisible part.
    (LET ((TEMP-ARRAY (SHEET-TEMPORARY-BIT-ARRAY WINDOW)))
      ;; There will be no temporary-bit-array if the window is created
      ;; as a resource with SAVE-BITS set to T.  For this case (the
      ;; system menu for example), the window is created fully but not
      ;; exposed, hence it is a temporary window but has no
      ;; temporary-bit-array. ( until it is actually exposed - may 04/06/89 )
      
      ;; may 04/06/89 This does NOT work right on color
      ;; sheets, especially in dual-monitor-p mode since the
      ;; information in the temporary-bit-array contains bits
      ;; from ALL planes. Rearranged order of bitblts to make this work for color
      ;; and for b&w.
      (WHEN (ARRAYP TEMP-ARRAY)  ;; may 04/06/89 
	;; Make the existing screen contents bleed through the shadow
	(BITBLT ALU ;; this had better be alu-seta or what will happen is beyond me !
		(- RIGHT LEFT WIDTH-OFFSET)	; Width
		(- BOTTOM TOP HEIGHT-OFFSET)	; Height
		TEMP-ARRAY
		(+ LEFT WIDTH-OFFSET)
		(+ TOP HEIGHT-OFFSET)
		WINDOW-SCREEN-ARRAY
		(+ LEFT WIDTH-OFFSET)
		(+ TOP HEIGHT-OFFSET)))

      ;; Draw the shadow border.
      (BITBLT alu-transp (- RIGHT LEFT WIDTH-OFFSET) (- BOTTOM TOP HEIGHT-OFFSET) GRAY-LEVEL
	      (REM (+ LEFT WIDTH-OFFSET) (ARRAY-DIMENSION GRAY-LEVEL 1))
	      (REM (+ TOP HEIGHT-OFFSET) (ARRAY-DIMENSION GRAY-LEVEL 0))
	      WINDOW-SCREEN-ARRAY (+ LEFT WIDTH-OFFSET) (+ TOP HEIGHT-OFFSET))      
      
      (when (arrayp temp-array) ;; may 04/06/89 
	;; Show the existing screen contents in the part of the temporary
	;; window which has the corners.
	(BITBLT ALU-seta (IF (= 0 WIDTH-OFFSET)			;; Width
			     (- RIGHT LEFT)
			     WIDTH-OFFSET)
		(IF (= 0 HEIGHT-OFFSET)				;; Height
		    (- BOTTOM TOP)
		    HEIGHT-OFFSET)
		TEMP-ARRAY					;; From-array
		(REM LEFT (ARRAY-DIMENSION TEMP-ARRAY 1))	;; From-X
		(REM TOP  (ARRAY-DIMENSION TEMP-ARRAY 0))	;; From-Y
		WINDOW-SCREEN-ARRAY				;; To-array
		LEFT						;; To-X
		TOP						;; To-Y
		))))) 

(Defun-Method DRAW-SHADOW-BORDERS Shadow-Borders-Mixin (alu)
  (Sheet-Force-Access (self)
    (Prepare-Sheet (self)
      (Dolist (border shadow-borders)
	(And border
	     (Neq border :Zero)
	     (Let ((left          (Second  border))
		   (Top           (Third   border))
		   (Right         (Fourth  border))
		   (bottom        (Fifth   border))
		   (width-offset  (Sixth   border))
		   (height-offset (Seventh border)))
	       (send (First border) self alu
                     (If (Minusp  left) (+ left width) left)
                     (If (Minusp   top) (+ top height) top)
                     (If (Plusp  right) right  (+ right width))
                     (If (Plusp bottom) bottom (+ bottom height))
                     width-offset
                     height-offset))))))
  )

#|HACK ALERT - remove this when genasys gets fixed - HACK ALERT |#
(DefMethod (Shadow-Borders-Mixin :After :expose)
           (&optional IGNORE IGNORE IGNORE IGNORE)
  NIL)
;;; Time to actually draw the shadow borders.
;; This isn't needed
;(DefMethod (Shadow-Borders-Mixin :After :expose)
;           (&optional inhibit-blinkers bits-action new-left new-top)
;  inhibit-blinkers bits-action new-left new-top
;  (Draw-Shadow-Borders alu-seta))

(DefMethod (Shadow-Borders-Mixin :After :refresh)
           (&optional inhibit-blinkers bits-action new-left new-top)
  "Makes sure that the shadow is redrawn properly."
  inhibit-blinkers bits-action new-left new-top
  (Draw-Shadow-Borders alu-seta))


(DEFFLAVOR FULL-SCREEN-HACK-MIXIN ((OLD-BORDERS NIL)) ()
  (:REQUIRED-FLAVORS BORDERS-MIXIN)
  (:DOCUMENTATION :MIXIN "Has borders only when not the full size of the screen
For windows like the initial lisp listener which frequently occupy the whole screen and
are immediately recognizable."))

(DEFVAR *FULL-SCREEN-HACKING-WINDOWS* NIL)
(DEFVAR *FULL-SCREEN-WINDOWS-HAVE-BORDERS* T)

;;; Call this to erase (or redraw) the margins
(DEFUN FLUSH-FULL-SCREEN-BORDERS (&OPTIONAL (FLUSH T))
  (SETQ *FULL-SCREEN-WINDOWS-HAVE-BORDERS* (NOT FLUSH))
  (MAPC #'(LAMBDA (WINDOW)
	    (SEND WINDOW :ADJUST-MARGINS))
	*FULL-SCREEN-HACKING-WINDOWS*)) 

(DEFMETHOD (FULL-SCREEN-HACK-MIXIN :BEFORE :KILL) ()
  (SETQ *FULL-SCREEN-HACKING-WINDOWS*
	(DELETE SELF (THE LIST *FULL-SCREEN-HACKING-WINDOWS*) :TEST #'EQ))) 

;;;This unfortunately has to redefine the sheet, since the width and
;;;height are not known at (:BEFORE :INIT) time.
(DEFMETHOD (FULL-SCREEN-HACK-MIXIN :AFTER :INIT) (IGNORE)
  (PUSH SELF *FULL-SCREEN-HACKING-WINDOWS*)
  (IF *FULL-SCREEN-WINDOWS-HAVE-BORDERS*
      (FULL-SCREEN-HACK X-OFFSET Y-OFFSET WIDTH HEIGHT)))

(DEFUN FULL-SCREEN-HACK (LEFT TOP WID HEI)
  (DECLARE (:SELF-FLAVOR FULL-SCREEN-HACK-MIXIN))
  (COND ((AND (= LEFT (SHEET-INSIDE-LEFT SUPERIOR))
	      (= TOP (SHEET-INSIDE-TOP SUPERIOR))
	      (= WID (SHEET-INSIDE-WIDTH SUPERIOR))
	      (= HEI (SHEET-INSIDE-HEIGHT SUPERIOR)))
	 (COND ((AND BORDERS (NULL OLD-BORDERS) (NOT *FULL-SCREEN-WINDOWS-HAVE-BORDERS*))
		(SETQ OLD-BORDERS BORDERS)
		(SEND SELF :SET-BORDERS NIL))
	       ((AND OLD-BORDERS (NULL BORDERS) *FULL-SCREEN-WINDOWS-HAVE-BORDERS*)
		(SEND SELF :SET-BORDERS OLD-BORDERS)
		(SETQ OLD-BORDERS NIL))))
	(T
	 (COND ((AND OLD-BORDERS (NULL BORDERS))
		(SEND SELF :SET-BORDERS OLD-BORDERS)
		(SETQ OLD-BORDERS NIL))))))

(DEFMETHOD (FULL-SCREEN-HACK-MIXIN :ADJUST-MARGINS) ()
  (SEND SELF :CHANGE-OF-SIZE-OR-MARGINS :LEFT X-OFFSET
					 :TOP Y-OFFSET
					 :WIDTH WIDTH
					 :HEIGHT HEIGHT))

(DEFMETHOD (FULL-SCREEN-HACK-MIXIN :BEFORE :CHANGE-OF-SIZE-OR-MARGINS) (&REST OPTIONS
								  &AUX (PLIST (LOCF OPTIONS)))
  (SHEET-FORCE-ACCESS (SELF)
    (ERASE-MARGINS))				;Insure that old margins get erased
  (AND (GET PLIST :LEFT)
       (FULL-SCREEN-HACK (GET PLIST :LEFT) (GET PLIST :TOP) (GET PLIST :WIDTH)
			 (GET PLIST :HEIGHT))))


(DEFFLAVOR PROCESS-MIXIN ((PROCESS NIL)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  :settable-instance-variables
  (:DOCUMENTATION :MIXIN "For a window which has its own process.
To enable this feature, specify the init keyword :PROCESS when you
create the window.  The value should be either a process, a list or a
symbol.

If it is a list, the cdr provides the keyword agrs to MAKE-PROCESS and
the car is then used as the top level function to run in the new
process.  It will receive one arg when it is called: this window.

If PROCESS is a symbol, it is used as the top level function and
MAKE-PROCESS is called with no keyword arguments.  But, as an exception,
if PROCESS is T, the top level function is to send the window a
:PROCESS-TOP-LEVEL message with no arguments.

The first time the window is exposed or selected, the process (if any)
receives the window as a run reason.  Each time the window is exposed or
selected, if the process is cleared, it gets reset and can run again."))

(DEFMETHOD (PROCESS-MIXIN :AFTER :INIT) (IGNORE)
  (WHEN PROCESS
    (LET ((TEM PROCESS))
      (CTYPECASE PROCESS
	(SYMBOL (SETQ PROCESS (MAKE-PROCESS NAME))
		(IF (EQ TEM T)
		    (PROCESS-PRESET PROCESS SELF :PROCESS-TOP-LEVEL))
		(PROCESS-PRESET PROCESS TEM SELF))
	(CONS (SETQ PROCESS (APPLY 'MAKE-PROCESS NAME (CDR TEM)))
	      (PROCESS-PRESET PROCESS (CAR TEM) SELF))
	(SI:PROCESS)))))

;; Not patched in 94 due to SI:FLUSHED-PROCESS
(DEFUN MAYBE-RESET-PROCESS (MESSAGE &REST IGNORE)
  (DECLARE (:SELF-FLAVOR PROCESS-MIXIN))
  (COND ((OR (EQ MESSAGE :SELECT)
	     (LOOP FOR SUP = SUPERIOR THEN (SHEET-SUPERIOR SUP) UNTIL (NULL SUP)
		   ALWAYS (SHEET-EXPOSED-P SUP)))
	 ;; Only touch the process if the window is going to become visible.  This
	 ;; makes many of the processes in the initial cold-load not have run reasons
	 ;; until you first select their window.  This makes booting faster (pages less).
	 ;; Also this is necessary to make the editor work:
	 ;; What was happening was that when the editor created its first
	 ;; pane and exposed it within its deactivated frame, the editor's process was
	 ;; being prematurely started up when it didn't even have all its instance
	 ;; variables yet, never mind enough editor environment set up.  The editor
	 ;; process would thus immediately get an error, which would later be reset
	 ;; asynchronously, leaving a second-level error handler around forever.
	 (COND ((TYPEP PROCESS 'SI:PROCESS)
		;; If we really have a process (not just NIL or something),
		;; Reset the process if it is flushed, then make sure it has a run reason.
		(IF (EQ (PROCESS-WAIT-FUNCTION PROCESS) 'SI:FLUSHED-PROCESS)
		    (SEND PROCESS :RESET))
		(SEND PROCESS :RUN-REASON SELF))))))

;;; *** This is a horrible crock.  If the "program system" is ever implemented,
;;; *** this should be flushed and replaced by the concept that selecting a program
;;; *** does something appropriate to its processes.
;;; I dont know if this is really the right thing
(DEFMETHOD (PROCESS-MIXIN :BEFORE :EXPOSE) MAYBE-RESET-PROCESS)
(DEFMETHOD (PROCESS-MIXIN :BEFORE :SELECT) MAYBE-RESET-PROCESS)

(DEFMETHOD (PROCESS-MIXIN :PROCESSES) ()
  (AND PROCESS (LIST PROCESS)))

(DEFFLAVOR LISTENER-MIXIN-INTERNAL (LISTENER-PACKAGE) (PROCESS-MIXIN FULL-SCREEN-HACK-MIXIN)
  (:DOCUMENTATION :SPECIAL-PURPOSE "An actual LISP window
Includes a process that will run the lisp top level read-eval-print loop.
Use this rather than LISTENER-MIXIN when you want to be invisible to the SYSTEM L key."))

(DEFMETHOD (LISTENER-MIXIN-INTERNAL :PACKAGE) ()
  (IF (VARIABLE-BOUNDP LISTENER-PACKAGE) LISTENER-PACKAGE))

(DEFMETHOD (LISTENER-MIXIN-INTERNAL :SET-PACKAGE) (PKG)
  (SETQ LISTENER-PACKAGE (FIND-PACKAGE PKG)))

(DEFMETHOD (LISTENER-MIXIN-INTERNAL :BEFORE :INIT) (IGNORE)
  (OR PROCESS (SETQ PROCESS '(SI:LISP-TOP-LEVEL1 :REGULAR-PDL-SIZE 16000.
						 :SPECIAL-PDL-SIZE 2000.))))

(DEFFLAVOR LISTENER-MIXIN () (LISTENER-MIXIN-INTERNAL)
  (:DOCUMENTATION :SPECIAL-PURPOSE "An actual LISP window
Includes a process that will run the lisp top level read-eval-print loop.
Use this when you want to be visible to the SYSTEM L key."))


(DEFFLAVOR LISP-INTERACTOR () (NOTIFICATION-MIXIN LISTENER-MIXIN-INTERNAL WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
  (:DOCUMENTATION :COMBINATION "LISP window, but not LISP-LISTENER-P"))


;;; A simple lisp listener flavor till the UCL version gets built.
(DEFFLAVOR SIMPLE-LISP-LISTENER () (NOTIFICATION-MIXIN LISTENER-MIXIN WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
 (:DOCUMENTATION :COMBINATION "Simple (non-UCL LISP window"))

(DEFMETHOD (SIMPLE-LISP-LISTENER :LISP-LISTENER-P) ()
  (IF (SYMEVAL-IN-STACK-GROUP 'SI:LISP-TOP-LEVEL-INSIDE-EVAL (PROCESS-STACK-GROUP PROCESS))
      :BUSY
      :IDLE))


(DEFUN IDLE-LISP-LISTENER (&OPTIONAL (SUPERIOR DEFAULT-SCREEN) (size-not-important? nil)
			   &AUX LL (FULL-SCREEN (MULTIPLE-VALUE-LIST
						  (SEND SUPERIOR :INSIDE-SIZE))))
  "Find the first Lisp Listener that is not in use.  If size-not-important is true, checks
   to see if the size of the LL and the inside size or the superior are equal.
   Creates one if none is available."
  (SETQ LL (DOLIST (I (SHEET-INFERIORS SUPERIOR))
	     (AND (EQ (send I :LISP-LISTENER-P) :IDLE)
		  (OR size-not-important? (EQUAL FULL-SCREEN (MULTIPLE-VALUE-LIST (send I :SIZE))))
		  (RETURN I))))
  (OR LL (MAKE-WINDOW 'LISP-LISTENER :SUPERIOR SUPERIOR)))

;;; Background stream

;(DEFVAR DEFAULT-BACKGROUND-STREAM 'BACKGROUND-STREAM)  ;in COLD
(DEFVAR BACKGROUND-INTERESTING-WINDOWS NIL)

(DEFFLAVOR BACKGROUND-LISP-INTERACTOR () (LISP-INTERACTOR)
  (:DEFAULT-INIT-PLIST :DEEXPOSED-TYPEOUT-ACTION '(:select)	;for GHR by GSM on 5 Jan 86
		       :DEEXPOSED-TYPEIN-ACTION :NOTIFY))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :BEFORE :INIT) (PLIST)
  (SETF (GET PLIST :SAVE-BITS) T)) 

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :SET-PROCESS) (NP)
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) NP)
  (SETQ PROCESS NP))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :AFTER :SELECT) (&REST IGNORE)
  (WITHOUT-INTERRUPTS
   (SETQ BACKGROUND-INTERESTING-WINDOWS
	 (DELETE (ASSOC SELF BACKGROUND-INTERESTING-WINDOWS :TEST #'EQ)
		 (THE LIST BACKGROUND-INTERESTING-WINDOWS) :TEST #'EQ)))) 

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :AFTER :DEACTIVATE) (&REST IGNORE)
  (WITHOUT-INTERRUPTS
   (SETQ BACKGROUND-INTERESTING-WINDOWS
	 (DELETE (ASSOC SELF BACKGROUND-INTERESTING-WINDOWS :TEST #'EQ)
		 (THE LIST BACKGROUND-INTERESTING-WINDOWS) :TEST #'EQ)))) 

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :WAIT-UNTIL-SEEN) ()
  ;; If we have typed out since we were selected last, then wait until we get seen
  (when (ASSOC SELF BACKGROUND-INTERESTING-WINDOWS :TEST #'EQ)
    (PROCESS-WAIT "Seen"
		  #'(LAMBDA (S)
		      (NOT (ASSOC S BACKGROUND-INTERESTING-WINDOWS :TEST #'EQ)))
		  SELF)
    ;; Then wait until we are deselected
    (PROCESS-WAIT "No Longer Seen" #'(LAMBDA (S)
				       (NEQ S SELECTED-WINDOW))
		  SELF))) 

(DEFVAR BACKGROUND-STREAM-WHICH-OPERATIONS)

(DEFUN BACKGROUND-STREAM (OP &REST ARGS)
  "This function is defaultly used as *TERMINAL-IO* for all processes.  If it gets called
at all, it turns *TERMINAL-IO* into a lisp listener window, and notifies the user that
the process wants the terminal."
  (IF (NOT (AND (VARIABLE-BOUNDP INITIAL-LISP-LISTENER)
		INITIAL-LISP-LISTENER))
      ;; Window system not fully turned on yet.
      (APPLY COLD-LOAD-STREAM OP ARGS)
      (IF (EQ *TERMINAL-IO* DEFAULT-BACKGROUND-STREAM)
	  (CASE OP
	    (:WHICH-OPERATIONS
	     ;; Get the which-operations once, but after the flavor has been compiled
	     (OR (BOUNDP 'BACKGROUND-STREAM-WHICH-OPERATIONS)
		 (USING-RESOURCE (WINDOW BACKGROUND-LISP-INTERACTORS)
		   (SETQ BACKGROUND-STREAM-WHICH-OPERATIONS
			 (SEND WINDOW :WHICH-OPERATIONS))
		   (PUSHNEW :BEEP BACKGROUND-STREAM-WHICH-OPERATIONS)
		   (PUSHNEW :INHIBIT-OUTPUT-FOR-ABORT-P
			    BACKGROUND-STREAM-WHICH-OPERATIONS)))
	     BACKGROUND-STREAM-WHICH-OPERATIONS)
	    ;; If the stream hasn't changed since the process was started, do default action
	    
	    (:SEND-IF-HANDLES
	     (when (MEMBER (CAR ARGS) (BACKGROUND-STREAM :WHICH-OPERATIONS) :TEST #'EQ)
	       (APPLY 'BACKGROUND-STREAM ARGS)))
	    (:BEEP
	     (LET ((W
		    (WITHOUT-INTERRUPTS
		     (IF SELECTED-WINDOW (SHEET-GET-SCREEN SELECTED-WINDOW) DEFAULT-SCREEN))))
	       (APPLY W :BEEP ARGS)))
	    (:LISTEN NIL)
	    (:INHIBIT-OUTPUT-FOR-ABORT-P T)
	    (:AWAIT-EXPOSURE NIL)
	    (OTHERWISE
	     (when (EQ %CURRENT-STACK-GROUP SCHEDULER-STACK-GROUP)
	       (FERROR () "Attempt to create a background window while in the scheduler."))
	     (SETQ *TERMINAL-IO* (ALLOCATE-RESOURCE 'BACKGROUND-LISP-INTERACTORS))
	     (SHEET-FORCE-ACCESS (*TERMINAL-IO* :NO-PREPARE)
	       (SEND *TERMINAL-IO* :SET-LABEL
		     (STRING-APPEND (PROCESS-NAME CURRENT-PROCESS)
				    " Background Stream"))
	       (SEND *TERMINAL-IO* :SET-PROCESS CURRENT-PROCESS)
	       (SEND *TERMINAL-IO* :CLEAR-SCREEN))
	     (SEND *TERMINAL-IO* :ACTIVATE)
	     (APPLY *TERMINAL-IO* OP ARGS)))
	  (PROGN
	    (SETQ *TERMINAL-IO* DEFAULT-BACKGROUND-STREAM)
	    (APPLY *TERMINAL-IO* OP ARGS))))) 

(DEFFLAVOR POP-UP-TEXT-WINDOW () (Temporary-Shadow-Borders-Window-Mixin WINDOW)
  (:DOCUMENTATION :COMBINATION "A simple temporary window for stream type output
Useful for things like [ESC] F or qsend, which just want a tv type stream that will not
disturb things underneath."))

(DEFMETHOD (POP-UP-TEXT-WINDOW :WAIT-FOR-INPUT-OR-DEEXPOSURE) ()
  (KBD-WAIT-FOR-INPUT-OR-DEEXPOSURE IO-BUFFER SELF))

(DEFMETHOD (POP-UP-TEXT-WINDOW :AFTER :HANDLE-MOUSE) ()		;; added 8/10/86 by kdb to avoid wedged state
  (UNLESS (TYPEP tv:selected-window 'ucl:get-keystroke-window)  ;;DON'T push a Space if this is in GET-KEYSTROKES. DAN
    (IO-BUFFER-PUT IO-BUFFER #\SPACE)))				;; if user clicked mouse outside of pop-up window.

(DEFFLAVOR TRUNCATING-WINDOW () (LINE-TRUNCATING-MIXIN WINDOW)
  (:DEFAULT-INIT-PLIST :TRUNCATE-LINE-OUT-FLAG 1)
  (:DOCUMENTATION :COMBINATION "A window that truncates line of output."))

(DEFFLAVOR TRUNCATING-POP-UP-TEXT-WINDOW ()
  (Temporary-Shadow-Borders-Window-Mixin TRUNCATING-WINDOW)
  (:DOCUMENTATION :COMBINATION "A pop up window what truncates lines"))

(DEFMETHOD (TRUNCATING-POP-UP-TEXT-WINDOW :WAIT-FOR-INPUT-OR-DEEXPOSURE) ()
  (KBD-WAIT-FOR-INPUT-OR-DEEXPOSURE IO-BUFFER SELF))

(DEFFLAVOR RESET-ON-OUTPUT-HOLD-FLAG-MIXIN () ()
  (:DEFAULT-INIT-PLIST :DEEXPOSED-TYPEOUT-ACTION '(:RESET-ON-OUTPUT-HOLD-FLAG)))

(DEFMETHOD (RESET-ON-OUTPUT-HOLD-FLAG-MIXIN :RESET-ON-OUTPUT-HOLD-FLAG) ()
  (SEND CURRENT-PROCESS :RESET :ALWAYS))

(DEFFLAVOR TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET ()
	   (RESET-ON-OUTPUT-HOLD-FLAG-MIXIN TRUNCATING-POP-UP-TEXT-WINDOW))

(DEFWINDOW-RESOURCE POP-UP-FINGER-WINDOW ()
  :MAKE-WINDOW (TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET
                 :RIGHT-SHADOW-WIDTH 0
                 :BOTTOM-SHADOW-WIDTH 0)
  :REUSABLE-WHEN :DEACTIVATED
  :INITIAL-COPIES 0)

;;; This mixin is useful for those windows that are created during the world-load.
;;; It is disconcerting when you suddenly see them appearing after you reshape
;;; some window.  This mixin causes them to be invisible and immune to autoexposure.
;;; They don't appear on the screen until you explicitly ask for them.  However, they
;;; are still active and appear on the Select menu.
(DEFFLAVOR INITIALLY-INVISIBLE-MIXIN () ()
  (:DEFAULT-INIT-PLIST :PRIORITY -2))

(DEFMETHOD (INITIALLY-INVISIBLE-MIXIN :BEFORE :EXPOSE) (&REST IGNORE)
  (SEND SELF :SET-PRIORITY NIL))


;;; Some notification stuff

(DEFFLAVOR NOTIFICATION-MIXIN () ()
  (:REQUIRED-METHODS :PROCESS)
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW STREAM-MIXIN)
  (:DOCUMENTATION :MIXIN "Prints notifications on itself when selected.
A window which can easily accomodate unsolicited typeout, such as a Lisp listener,
uses this mixin to cause notifications to be printed on it when it is selected.
The user's attention is assumed to be at the cursor of the selected window.
This mixin also interacts with the rubout-handler of STREAM-MIXIN."))

;;; Note: this does not try to do anything smart with the prompt, because doing
;;; that right requires resolving some hairy issues which simply are not worth it.
(DEFMETHOD (NOTIFICATION-MIXIN :PRINT-NOTIFICATION) (TIME STRING WINDOW-OF-INTEREST)
  (IF
   (MULTIPLE-VALUE-BIND (NIL NIL FINAL-INDEX)
       (LET ((END (LENGTH STRING)))
	 (AND (PLUSP END)
	      (= (AREF STRING (1- END)) #\NEWLINE)
	      (DECF END))
	 (SHEET-COMPUTE-MOTION SELF 0 0 STRING 0 END T 0 (- (SHEET-INSIDE-HEIGHT) LINE-HEIGHT)
			    100000.0s0))
     FINAL-INDEX)
   ;; We are too small to print this notification.  Use a pop-up-window.
   (SEND 'POP-UP-NOTIFY :PRINT-NOTIFICATION TIME STRING WINDOW-OF-INTEREST)
   ;; We can print this notification on the seleted window, so do it.
   (SEND SELF :PRINT-NOTIFICATION-ON-SELF TIME STRING WINDOW-OF-INTEREST))) 

;Execute the body and then redisplay any rubout handler input that is on the window
;below what was printed by the body.
(DEFMACRO OUTPUT-BEFORE-RUBOUT-HANDLER ((WINDOW) &BODY BODY)
  `(LET (PROCESS SG RUBOUT-X RUBOUT-Y RUBOUT-X-LOC RUBOUT-Y-LOC)
      (WITHOUT-INTERRUPTS
	(AND (SETQ PROCESS (SEND ,WINDOW :PROCESS))
	     (SETQ SG (SEND PROCESS :STACK-GROUP))
	     (SYMEVAL-IN-STACK-GROUP 'RUBOUT-HANDLER-INSIDE SG)
	     (SETF (VALUES RUBOUT-X RUBOUT-X-LOC)
		   (SYMEVAL-IN-STACK-GROUP 'PROMPT-STARTING-X SG)
		   (VALUES RUBOUT-Y RUBOUT-Y-LOC)
		   (SYMEVAL-IN-STACK-GROUP 'PROMPT-STARTING-Y SG))))
      ;; If the process is in the rubout-handler, back up over the echoed input and erase it.
      (COND (RUBOUT-X (SEND ,WINDOW :SET-CURSORPOS RUBOUT-X RUBOUT-Y)
		      (SEND ,WINDOW :CLEAR-EOL)))
      (UNWIND-PROTECT
	(PROGN . ,BODY)
	;; Reprint rubout-handler buffer if necessary, and change the rubout-handler's
	;; starting cursorpos
	(COND (RUBOUT-X
	       (MULTIPLE-VALUE-BIND (NEW-X NEW-Y)
		   (SEND ,WINDOW :READ-CURSORPOS)
		 (SETF (CONTENTS RUBOUT-X-LOC) NEW-X
		       (CONTENTS RUBOUT-Y-LOC) NEW-Y)
		 (IO-BUFFER-PUSH (SEND ,WINDOW :IO-BUFFER)
				 `(REDISPLAY-RUBOUT-HANDLER))))))))

;Some windows that use the usual test to see whether they are able to print
;put demons on this to do hairy things when they do print.
(DEFMETHOD (NOTIFICATION-MIXIN :PRINT-NOTIFICATION-ON-SELF) (TIME STRING WINDOW-OF-INTEREST)
  WINDOW-OF-INTEREST
  (LOCK-SHEET (SELF)
     (OUTPUT-BEFORE-RUBOUT-HANDLER (SELF)
       (SEND SELF :FRESH-LINE)
       (SEND SELF :BEEP 'notify)
       (SEND SELF :TYO #\[)
       (TIME:PRINT-BRIEF-UNIVERSAL-TIME TIME SELF)
       (SEND SELF :TYO #\SPACE)
       (LET ((END (LENGTH STRING)))
	 (OR (ZEROP END)
	     (SEND SELF :STRING-OUT STRING 0
		(IF (= (AREF STRING (1- END)) #\NEWLINE)
		    (1- END)
		    END))))
       (SEND SELF :TYO #\])
       (SEND SELF :TYO #\NEWLINE)))) 

(DEFFLAVOR POP-UP-NOTIFICATION-MIXIN () (DELAY-NOTIFICATION-MIXIN)
  :ALIAS-FLAVOR)

;This actually appears earlier in the file, to avoid lossage.
;(DEFFLAVOR DELAY-NOTIFICATION-MIXIN () ()
;  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
;  (:DOCUMENTATION :MIXIN "Delays printing notifications, but announces them in the who line.
;This is the default way of handling them.  See NOTIFICATION-MIXIN for an alternative."))

(DEFWINDOW-RESOURCE POP-UP-NOTIFICATION-WINDOW ()
	:MAKE-WINDOW (POP-UP-NOTIFICATION-WINDOW)
	:REUSABLE-WHEN :DEACTIVATED
	:INITIAL-COPIES 0)	;No initial copies, would bomb during system loading

(DEFVAR PENDING-NOTIFICATIONS NIL
  "Notifications waiting for the user to switch windows or do Terminal N.
When this is non-NIL, who-line announces notifications.")
(DEFVAR DEFERRED-NOTIFICATIONS NIL
  "Like PENDING-NOTIFICATIONS, but these don't make who-line blink.")
(DEFPARAMETER WAIT-FOR-NOTIFICATIONS-FLAG ()
   "Non-NIL means wait for user to switch windows rather than pop up a notification window.") 

(DEFMETHOD (DELAY-NOTIFICATION-MIXIN :PRINT-NOTIFICATION) POP-UP-NOTIFY)

(DEFUN POP-UP-NOTIFY (IGNORE TIME STRING WINDOW-OF-INTEREST)
  (DECLARE (:SELF-FLAVOR ESSENTIAL-WINDOW))
  (IF WAIT-FOR-NOTIFICATIONS-FLAG
     (WAIT-TO-NOTIFY TIME STRING WINDOW-OF-INTEREST)
     ;; Now we must spawn a process and return.  See comments in CAREFUL-NOTIFY.
     (PROCESS-RUN-FUNCTION "Notify"
       #'(LAMBDA (TIME
		  STRING
		  WINDOW-OF-INTEREST
		  SLF
		  START-TIME
		  NOTE-WINDOW)
	   (SEND NOTE-WINDOW :SET-WINDOW-OF-INTEREST WINDOW-OF-INTEREST)
	   ;;Above sets up for mouse click.  Caller has already set up for Terminal-0-S
	   (SEND NOTE-WINDOW :SET-LABEL
		 (FORMAT NIL "~A " TV:*REMOVE-TYPEOUT-STANDARD-MESSAGE* ))  ; flush -> press to remove... , kdb 3/27/86
	   ;; If window gets deexposed while we're typing out, typically because
	   ;; user types Terminal-0-S before we finish cranking out our message, punt.
	   (CATCH :DEEXPOSE
	     (CONDITION-BIND
	       ((OUTPUT-ON-DEEXPOSED-SHEET
		  #'(LAMBDA (&REST IGNORE)
		      (THROW :DEEXPOSE ()))))
	       (LET* ((OSW SELECTED-WINDOW)	;Almost certainly SLF
		      (MESSAGE-STRING
			(WITH-OUTPUT-TO-STRING (STR)
			  (TIME:PRINT-BRIEF-UNIVERSAL-TIME TIME STR)
			  (WRITE-CHAR #\SPACE STR)
			  (PRINC STRING STR)
			  (WRITE-CHAR #\NEWLINE STR)))
		      (SCREEN (SHEET-GET-SCREEN SLF))
		      (PROMPT-STRING
			(IF WINDOW-OF-INTEREST
			    (FORMAT ()
				    "Select ~A by typing Terminal-0-S or by clicking the mouse here.
"
				    WINDOW-OF-INTEREST)
			    ""))
		      (STRING-WE-NEED-ROOM-FOR
			(STRING-APPEND MESSAGE-STRING PROMPT-STRING)))
		 (MULTIPLE-VALUE-BIND (NIL FINAL-Y NIL MAXIMUM-X)
		     (SHEET-COMPUTE-MOTION NOTE-WINDOW 0 0
					   STRING-WE-NEED-ROOM-FOR 0 () T 0
					   100000.0s0 100000.0s0
					   ;; Don't let it be wider than can fit on screen.
					   (- (SHEET-WIDTH SCREEN)
					      (- (SHEET-WIDTH NOTE-WINDOW)
						 (SHEET-INSIDE-WIDTH NOTE-WINDOW))))
		   (SEND NOTE-WINDOW :SET-INSIDE-SIZE
			 (MAX (SEND NOTE-WINDOW :LABEL-SIZE) MAXIMUM-X)
			 (MIN
			   (- (SHEET-HEIGHT SCREEN)
			      (- (SHEET-HEIGHT NOTE-WINDOW)
				 (SHEET-INSIDE-HEIGHT NOTE-WINDOW)))
                           FINAL-Y)))
		 (MULTIPLE-VALUE-BIND (X Y) (SHEET-CALCULATE-OFFSETS SLF SCREEN)
		   (SEND NOTE-WINDOW :CENTER-AROUND
			 (+ X (TRUNCATE (SHEET-WIDTH  SLF) 2))
			 (+ Y (TRUNCATE (SHEET-HEIGHT SLF) 2))))
		 (SEND NOTE-WINDOW :SELECT)     		;Exposes blank with homed cursor
		 (SEND NOTE-WINDOW :STRING-OUT MESSAGE-STRING)
		 (FINISH-UNEXPECTED-SELECT START-TIME OSW)      ;By now user has seen what's up
		 (SEND NOTE-WINDOW :CLEAR-INPUT)        	;Flush typeahead before inviting typein
                 ;; If there isn't a string then don't output anything.
                 (WHEN (PLUSP (LENGTH PROMPT-STRING))
                   (SEND NOTE-WINDOW :STRING-OUT PROMPT-STRING))
		 (SEND NOTE-WINDOW :ANY-TYI))))
	   ;;cause the previous seleted window to be re-selected  PMH 11/30/87
	   (send note-window :deselect)		
	   (SEND NOTE-WINDOW :DEACTIVATE))
       TIME STRING WINDOW-OF-INTEREST SELF
       (START-UNEXPECTED-SELECT)
       (ALLOCATE-RESOURCE 'POP-UP-NOTIFICATION-WINDOW
			  (SHEET-GET-SCREEN SELF))))) 

;;;Wait until selected window changes, then print this notification
;;;if it is then possible, or else wait again.
;;;While we wait, PENDING-NOTIFICATIONS is non-NIL
;;;so the mouse doc line says that notifications are waiting.
(DEFUN WAIT-TO-NOTIFY (TIME STRING WINDOW-OF-INTEREST)
  (PUSH (LIST TIME STRING WINDOW-OF-INTEREST) PENDING-NOTIFICATIONS)
  (BEEP 'NOTIFY)
  (DO ()
      (NIL)
    (PROCESS-WAIT "No longer selected"
		  #'(LAMBDA (W)
		      (OR (AND (NOT PENDING-NOTIFICATIONS)
			       (NOT DEFERRED-NOTIFICATIONS))
			 (AND SELECTED-WINDOW
			      (NEQ SELECTED-WINDOW W))))
		  SELECTED-WINDOW)
    (WITHOUT-INTERRUPTS
     ;; Flush any notifications that just tell user
     ;; to go to the window he just selected.
     (DOLIST (NOTE PENDING-NOTIFICATIONS)
       (IF (SHEET-ME-OR-MY-KID-P SELECTED-WINDOW (THIRD NOTE))
	   (SETQ PENDING-NOTIFICATIONS (DELETE NOTE (THE LIST PENDING-NOTIFICATIONS) :TEST #'EQ))))
     (DOLIST (NOTE DEFERRED-NOTIFICATIONS)
       (IF (SHEET-ME-OR-MY-KID-P SELECTED-WINDOW (THIRD NOTE))
	   (SETQ DEFERRED-NOTIFICATIONS
		 (DELETE NOTE (THE LIST DEFERRED-NOTIFICATIONS) :TEST #'EQ)))))
    ;; If any notifications left to print, print them
    ;; if new selected window likes to print them.
    (IF (OR PENDING-NOTIFICATIONS
	    DEFERRED-NOTIFICATIONS)
       (LET ((SW SELECTED-WINDOW))
	 (WHEN (TYPEP SW 'NOTIFICATION-MIXIN)
	   (SETQ PENDING-NOTIFICATIONS (APPEND PENDING-NOTIFICATIONS DEFERRED-NOTIFICATIONS))
	   (SETQ DEFERRED-NOTIFICATIONS ())
	   (DO ()
	       ((NULL PENDING-NOTIFICATIONS)
		(RETURN))
	     (APPLY SW :PRINT-NOTIFICATION (POP PENDING-NOTIFICATIONS))))))
    ;; If could not print them, wait for another window-switch.
    (OR PENDING-NOTIFICATIONS
	DEFERRED-NOTIFICATIONS (RETURN)))) 

;;; These two functions are for unexpected pop-up selectable windows
;;; They give the user a chance to get his typing straightened out

(DEFVAR *UNEXPECTED-SELECT-DELAY* 60.)	;Give user 1 second to notice beep and stop typing

;Beep, return time to be passed back in to FINISH-UNEXPECTED-SELECT
(DEFUN START-UNEXPECTED-SELECT ()
  (BEEP 'NOTIFY)
  (TIME))

;Sleep until enough time has passed, then snarf typeahead into old-selected-window
;which is no longer selected-window because by now the new thing has been exposed
(DEFUN FINISH-UNEXPECTED-SELECT (START-TIME OLD-SELECTED-WINDOW &AUX BUF)
  (PROCESS-WAIT "Sleep"
		#'(LAMBDA (START-TIME)
		    (> (TIME-DIFFERENCE (TIME) START-TIME) *UNEXPECTED-SELECT-DELAY*))
		START-TIME)
  (WITHOUT-INTERRUPTS
   (AND OLD-SELECTED-WINDOW
	(SETQ BUF (SEND OLD-SELECTED-WINDOW :IO-BUFFER))
	(KBD-SNARF-INPUT BUF)))) 

(DEFFLAVOR POP-UP-NOTIFICATION-WINDOW
	((WINDOW-OF-INTEREST NIL))
	(NOTIFICATION-MIXIN POP-UP-TEXT-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES WINDOW-OF-INTEREST)
  (:DEFAULT-INIT-PLIST :SAVE-BITS NIL  ;Thus will not come up with old garbage contents
		       :CHARACTER-HEIGHT 5	;5 lines.  Width is full width of sup.
		       :DEEXPOSED-TYPEOUT-ACTION :ERROR)
  (:DOCUMENTATION :SPECIAL-PURPOSE "Pops down and selects window of interest when clicked on
One of these is created when a notify message is sent to a normal window, it pops up, prints
the notification, and when it is selected with the mouse, pops back down and exposes the
window that got the error, which for background processes will be a slightly larger
pop-up type window."))

;Record that this notification window is being used to notify about
;the window of interest.
(DEFMETHOD (POP-UP-NOTIFICATION-WINDOW :SET-WINDOW-OF-INTEREST) (WINDOW)
  (SETQ WINDOW-OF-INTEREST WINDOW)
  (LET ((TEM (ASSOC WINDOW BACKGROUND-INTERESTING-WINDOWS :TEST #'EQ)))
    (AND TEM
	 (SETF (CDR TEM) SELF)))) 

;;; When clicked on, always send a :MOUSE-SELECT message, even if already selected
;;; so that WINDOW-OF-INTEREST will get selected.
(DEFMETHOD (POP-UP-NOTIFICATION-WINDOW :MOUSE-CLICK) (BUTTON IGNORE IGNORE)
  (COND ((= BUTTON #\MOUSE-1-1)
	 (MOUSE-SELECT SELF)
	 T)))

(DEFMETHOD (POP-UP-NOTIFICATION-WINDOW :MOUSE-SELECT) (&REST ARGS)
  "If selected with the mouse, then deexpose us and really select the guy that we are
notifying about."
  (SEND SELF :DEEXPOSE)				;This will also deactivate us
  (AND WINDOW-OF-INTEREST
       (APPLY WINDOW-OF-INTEREST :MOUSE-SELECT ARGS))) 

;This wakes up the process which is sitting around waiting for the user
;to type something to flush the notification window.  It will deactivate us.
(DEFMETHOD (POP-UP-NOTIFICATION-WINDOW :AFTER :DEEXPOSE) (&REST IGNORE)
  (SEND SELF :FORCE-KBD-INPUT :DEEXPOSE))

;;; Resource to supply reasonably sized bit arrays.  This is especially useful
;;; for window-bind type windows that don't want to go through the overhead of
;;; creating a new bit array every time they get invoked
(DEFRESOURCE BIT-ARRAYS (&OPTIONAL (WIDTH (SHEET-WIDTH DEFAULT-SCREEN))
				   (HEIGHT (SHEET-HEIGHT DEFAULT-SCREEN)))
  :CONSTRUCTOR (MAKE-ARRAY (LIST WIDTH HEIGHT) :element-TYPE (sheet-array-type-cl default-screen))
  :INITIAL-COPIES 0)

(DEFUN AWAIT-WINDOW-EXPOSURE ()
  "Wait until TERMINAL-IO is exposed (if it is a window).
To be called by functions like ED.
If you want to await the re-exposure of the Lisp listener after activating
some other window, call this.  Usually it does nothing, but if the TERMINAL-IO
window is an auto-exposing window, if you didn't call this you would get into
a loop where two windows were fighting for exposure, each de-exposing the other.
If that would happen this function causes a wait until TERMINAL-IO is exposed."
  (SEND *TERMINAL-IO* :SEND-IF-HANDLES :AWAIT-EXPOSURE)
  T)

(DEFMETHOD (ESSENTIAL-WINDOW :AWAIT-EXPOSURE) ()
  (OR (EQ DEEXPOSED-TYPEOUT-ACTION :NORMAL)
      (PROCESS-WAIT "Await exposure" #'CAR (LOCF EXPOSED-P))))
