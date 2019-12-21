;;; -*- Mode: COMMON-LISP;  Package: TV; Base: 10; Fonts: (CPTFONT CPTFONTB HL12BI) -*-

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
;;; Copyright (C) 1983- 1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;;
;;; Change history:
;;;
;;;  Date	Author	Description
;;; -------------------------------------------------------------------------------------
;;; 01/27/89   KJF	[may] Changes for Multiple Monitor (MMON) support, search for MMON
;;; 6/10/87    KWW	CHANGED BLANKING ARRAY TO USE SETA AND ALU-BACK
;;;   7/2/87   PMH	Modified basic-menu :pane-size to get rid of compiler warnings
;;;   2/11/87  TWE	Defined a new w:command-menu-pane for to use the new menu code.  In
;;;			this way, users won't get the old menu code unless they specify a
;;;			TV package prefix.
;;; 12/23/86   TWE	Made several changes to make the build process cleaner.
;;;			Moved the BLANKING-ARRAY DEFVAR and its associated creation function
;;;			up so that the CONSTRAINT-FRAME-WHITE-BLANKING and its corresponding
;;;			black function could reference the BLANKING-ARRAY properly.
;;; 10/31/86	TWE	Changed LEXPR-FUNCALL to APPLY.
;;; 10/20/86	TWE	More Common Lisp conversion for the TYPEP function.
;;; 09/05/86	TWE	Changed PARSE-CONSTRAINT and CONSTRAINT-FRAME-DO-A-CONSTRAINT to be
;;;			DEFUNs instead of DEFUNPs.  They were already converted enough to
;;;			Common Lisp to make that a trivial change.
;;; 07/31/86	TWE	Changed MAKE-BLANKING-ARRAY to call make-array with an :ELEMENT-TYPE
;;;			of 'BIT instead of a :TYPE of 'ART-1B.
;;; 07/29/86	TWE	Changed to use Common Lisp functions.
;;; 07/28/86	TWE	Modified reference to the make-pixel-array function to use MAKE-ARRAY instead.


;;; Superior that supports its inferiors as panes -- windows which are managed
;;; by the superior in some way

;;; Each window that is a pane (an inferior of a frame), should include
;;; the PANE-MIXIN flavor so as to interact correctly
(DEFFLAVOR PANE-MIXIN () ()
  (:INCLUDED-FLAVORS ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN "No-op for compatibility"))

(DEFFLAVOR PANE-NO-MOUSE-SELECT-MIXIN () (DONT-SELECT-WITH-MOUSE-MIXIN)
  :ALIAS-FLAVOR)

(DEFFLAVOR WINDOW-PANE () (WINDOW)
  :ALIAS-FLAVOR)

(DEFFLAVOR LISP-LISTENER-PANE () (LISP-LISTENER)
  :ALIAS-FLAVOR)

(DEFFLAVOR COMMAND-MENU-PANE () (COMMAND-MENU)
  :ALIAS-FLAVOR)

(DEFFLAVOR W:COMMAND-MENU-PANE () (W:MENU)
  (:DEFAULT-INIT-PLIST :COMMAND-MENU T))

;;; Basic frame contains methods that are used by most frames
;;; Recursion variable keeps track of whether or not we are performing an operation
;;;  that will cause us to get notified.  If RECURSION is set, the routine
;;;  should ignore the notification and return immediatly a value of T.  This
;;;  will be the standard.
(DEFFLAVOR BASIC-FRAME ((RECURSION NIL))
  (ESSENTIAL-EXPOSE ESSENTIAL-ACTIVATE ESSENTIAL-SET-EDGES POP-UP-NOTIFICATION-MIXIN
   ESSENTIAL-WINDOW)
  (:REQUIRED-METHODS :INFERIOR-SET-EDGES)
  (:INIT-KEYWORDS :SELECTED-PANE)		;PMH 11/17/87 Added this support
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :MORE-P NIL)
  (:DOCUMENTATION :LOWLEVEL-MIXIN "Pane handling messages used by most frames"))

(defmethod (basic-frame :after :restore-default-colors)()
  (dolist (window inferiors)
    (send window :send-if-handles :restore-default-colors)))

;; Obsolete name.
(DEFFLAVOR FRAME-DONT-SELECT-INFERIORS-WITH-MOUSE-MIXIN () (ALIAS-FOR-INFERIORS-MIXIN)
  (:REQUIRED-FLAVORS BASIC-FRAME)
  (:DOCUMENTATION :MIXIN "Don't list our panes in Select menu."))

(DEFMETHOD (FRAME-DONT-SELECT-INFERIORS-WITH-MOUSE-MIXIN :SELECTABLE-WINDOWS) (&AUX STRING)
  (AND (SETQ STRING (SEND SELF :NAME-FOR-SELECTION))
       (LIST (LIST STRING SELF))))

(DEFFLAVOR INFERIORS-NOT-IN-SELECT-MENU-MIXIN () (ALIAS-FOR-INFERIORS-MIXIN)
  (:REQUIRED-FLAVORS BASIC-FRAME)
  (:DOCUMENTATION :MIXIN "Don't list our panes in Select menu."))

(DEFMETHOD (INFERIORS-NOT-IN-SELECT-MENU-MIXIN :SELECTABLE-WINDOWS) (&AUX STRING)
  (AND (SETQ STRING (SEND SELF :NAME-FOR-SELECTION))
       (LIST (LIST STRING SELF))))

(DEFMETHOD (BASIC-FRAME :BEFORE :INIT)(INIT-PLIST)	;PMH 11/17/87  Added this support
  (LET ((SELECTED-PANE? (GET INIT-PLIST :SELECTED-PANE)))
    (WHEN SELECTED-PANE? (SETF SELECTION-SUBSTITUTE SELECTED-PANE?))))

(DEFMETHOD (BASIC-FRAME :SELECTED-PANE) () SELECTION-SUBSTITUTE)

;This sets what pane should be selected if this window is selected.
(DEFMETHOD (BASIC-FRAME :SELECT-PANE) (PANE)
  (OR (MEMBER PANE INFERIORS :TEST #'EQ)
      (NULL PANE)
      (FERROR NIL "Cannot select ~S, which is not a pane of ~S" PANE SELF))
  (SEND SELF :SET-SELECTION-SUBSTITUTE PANE))

(DEFMETHOD (BASIC-FRAME :NAME-FOR-SELECTION) ()
  (AND SELECTION-SUBSTITUTE (FUNCALL SELECTION-SUBSTITUTE :NAME-FOR-SELECTION)))

;;This is a bad way to do things, and will get flushed
;;once the system is verified to work without it.
(DEFPARAMETER FLUSH-BASIC-FRAME-STATUS-METHOD NIL)

(DEFWRAPPER (BASIC-FRAME :STATUS) (IGNORE . BODY)
  `(COND ((AND SELECTION-SUBSTITUTE
	       (NOT FLUSH-BASIC-FRAME-STATUS-METHOD)
	       (EQ (FUNCALL SELECTION-SUBSTITUTE :STATUS) :SELECTED))
	  :SELECTED)
	 (T . ,BODY)))

(DEFMETHOD (BASIC-FRAME :AFTER :ACTIVATE) (&REST IGNORE)
  (DOLIST (I INFERIORS)
    ;; Give it a chance to put itself in previously-selected-windows.
    (SEND I :ACTIVATE)))

;; may 01/27/89
(DEFMETHOD (BASIC-FRAME :AFTER :DEACTIVATE) (&REST IGNORE)
  ;; Make sure that none of our inferiors are remembered as selectable
  ;; Put these windows in the correct place, in case they're not on DEFAULT-SCREEN.  CJJ 06/02/88.
  ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
  (with-screens-previously-selected-windows (self)
    (DOLIST (I INFERIORS)
      (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS I))))

(DEFWRAPPER (BASIC-FRAME :PRINT-NOTIFICATION) (ARGS . BODY)
  `(IF SELECTION-SUBSTITUTE
       (APPLY SELECTION-SUBSTITUTE :PRINT-NOTIFICATION ARGS)
       . ,BODY))

;;  ;; Removing this should make nested frames win more.
;;(DEFMETHOD (BASIC-FRAME :ALIAS-FOR-SELECTED-WINDOWS) () SELF)
;;

(DEFMETHOD (BASIC-FRAME :INFERIOR-SELECT) (PANE)
  (SETQ SELECTION-SUBSTITUTE PANE)
  T)

;;; Screen management et. al.
(DEFMETHOD (BASIC-FRAME :SCREEN-MANAGE-RESTORE-AREA) (RECTS ARRAY-TO-DRAW-ON X Y ALU &AUX RS)
  (IF BIT-ARRAY
      (SCREEN-MANAGE-RESTORE-AREA RECTS ARRAY-TO-DRAW-ON X Y ALU T)
      (PROGN
        (DOLIST (R RECTS)
          (COND ((EQ (CAR (RECT-SOURCE R)) SELF)
                 (PUSH (LIST (LIST SELF 0 0)
                             (- (RECT-LEFT  R) X-OFFSET) (- (RECT-TOP    R) Y-OFFSET)
                             (- (RECT-RIGHT R) X-OFFSET) (- (RECT-BOTTOM R) Y-OFFSET))
                       RS)
                 (SETQ RECTS (DELETE R (THE LIST RECTS) :TEST #'EQ)))))
        (SCREEN-MANAGE-SHEET SELF RS ARRAY-TO-DRAW-ON (+ X X-OFFSET) (+ Y Y-OFFSET) ALU)
        RECTS)))

;; I think these are no longer relevant.

;;(DEFWRAPPER (BASIC-FRAME :EXPOSE) (IGNORE . BODY)
;;  `(LET-GLOBALLY ((RECURSION T))
;;     . ,BODY))

;;(DEFWRAPPER (BASIC-FRAME :DEEXPOSE) (IGNORE . BODY)
;;  `(LET-GLOBALLY ((RECURSION T))
;;     . ,BODY))
;;

(DEFMETHOD (BASIC-FRAME :SCREEN-MANAGE-DEEXPOSED-VISIBILITY) () T)

(DEFMETHOD (BASIC-FRAME :SCREEN-MANAGE-UNCOVERED-AREA) SCREEN-MANAGE-CLEAR-UNCOVERED-AREA)


(DEFFLAVOR FRAME-FORWARDING-MIXIN () ()
  (:INCLUDED-FLAVORS BASIC-FRAME)
  (:DOCUMENTATION :MIXIN "Used when forwarding of EXPOSE/DEEXPOSE/BURY messages from pane
to frame is desired."))

(DEFMETHOD (FRAME-FORWARDING-MIXIN :INFERIOR-EXPOSE) (PANE)
  PANE
  (COND (RECURSION T)
	(T (SEND SELF :EXPOSE)
	   NIL)))

(DEFMETHOD (FRAME-FORWARDING-MIXIN :INFERIOR-DEEXPOSE) (PANE)
  PANE
  (COND (RECURSION T)
	(T (SEND SELF :DEEXPOSE)
	   NIL)))

(DEFMETHOD (FRAME-FORWARDING-MIXIN :INFERIOR-BURY) (PANE)
  PANE
  (COND (RECURSION T)
	(T (SEND SELF :BURY)
	   NIL)))

(DEFMETHOD (FRAME-FORWARDING-MIXIN :INFERIOR-SET-EDGES) (PANE &REST ARGS)
  (IF RECURSION
      T
    (LET-GLOBALLY ((RECURSION T))
      ;; :SET-EDGES can deexpose and reexpose the pane;
      ;; avoid forwarding that to the frame.
      (LEXPR-SEND PANE :SET-EDGES ARGS))))

(DEFWRAPPER (FRAME-FORWARDING-MIXIN :SCREEN-MANAGE-AUTOEXPOSE-INFERIORS) (IGNORE . BODY)
  `(LET-GLOBALLY ((RECURSION T))
     . ,BODY))

(DEFMETHOD (BASIC-FRAME :PANE-TYPES-ALIST) ()
  NIL)

;;; Simple superior for split-screen
(DEFFLAVOR SPLIT-SCREEN-FRAME () (BASIC-FRAME))

(DEFMETHOD (SPLIT-SCREEN-FRAME :PANE-TYPES-ALIST) ()
  DEFAULT-WINDOW-TYPES-ITEM-LIST)

(DEFMETHOD (SPLIT-SCREEN-FRAME :NAME-FOR-SELECTION) () NAME)

;; may 01/27/89
(DEFMETHOD (SPLIT-SCREEN-FRAME :AFTER :EXPOSE) (&REST IGNORE)
  ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
  ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
  (with-screens-previously-selected-windows (self)
    (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS SELF)))

;; may 01/27/89
(DEFMETHOD (SPLIT-SCREEN-FRAME :AFTER :ACTIVATE) (&REST IGNORE)
  ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/02/88.
  ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
  (with-screens-previously-selected-windows (self)
    (REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS SELF)))

(DEFMETHOD (SPLIT-SCREEN-FRAME :SELECT) (&REST ARGS)
  (SEND SELF :SET-SELECTION-SUBSTITUTE (FIRST EXPOSED-INFERIORS))
  (LEXPR-SEND (FIRST EXPOSED-INFERIORS) :SELECT ARGS))

;;; Constraint frames -- these frames maintain their panes based on a set
;;;  of constraints.  These frames are the right thing for most frame applications.

;PANES is the list describing the panes, specified by the user.
;INTERNAL-PANES is an alist mapping pane-names (as used in PANES and in constraints)
;to actual windows.
;EXPOSED-PANES is a list of panes used in the current configuration.
;CONSTRAINTS is, again, the list specified by the user.
;PARSED-CONSTRAINS is the result of crunching that.
;INTERNAL-CONSTRAINS contains the parsed constraints for this configuration.
(DEFFLAVOR BASIC-CONSTRAINT-FRAME
           (PANES

            ;; Maps a pane name to a window instance.
            ;; ( (pane-name window-instance) ... )
            INTERNAL-PANES

            SELECTION-SUBSTITUTE
            (EXPOSED-PANES NIL)
            CONSTRAINTS
            PARSED-CONSTRAINTS
            INTERNAL-CONSTRAINTS
            (SUBSTITUTIONS NIL)

            ;; Contains either a flag (T/NIL) or a description of the
            ;; blank parts of the frame.  If this is NIL then the blank
            ;; parts of the frame need to be computed.  If this is T
            ;; then the blank parts have been computed.  If this
            ;; instance is not either NIL or T then it is a description
            ;; of the blank parts of the frame.  See
            ;; CONSTRAINT-FRAME-MAKE-BLANK-RECTANGLES for the format of
            ;; this item if it is a description.
            (BLANK-RECTANGLES NIL)

            (CONFIGURATION NIL))
	   (BASIC-FRAME)
  (:INITABLE-INSTANCE-VARIABLES CONSTRAINTS PANES SUBSTITUTIONS SELECTION-SUBSTITUTE CONFIGURATION)
  (:GETTABLE-INSTANCE-VARIABLES CONSTRAINTS PANES CONFIGURATION)
  (:SETTABLE-INSTANCE-VARIABLES EXPOSED-PANES)
  (:DOCUMENTATION :LOWLEVEL-MIXIN "Maintains panes according to specified constraints"))


;There used to be a definition of the :inferior-set-edges operation here.
;Let's see if there is any problem with this being the same as FRAME-FORWARDING-MIXIN
;before flushing the separate name.
(DEFFLAVOR CONSTRAINT-FRAME-FORWARDING-MIXIN () (FRAME-FORWARDING-MIXIN))

(DEFFLAVOR CONSTRAINT-FRAME-NO-FORWARDING () (BASIC-CONSTRAINT-FRAME BASIC-FRAME)
  (:DOCUMENTATION :COMBINATION "Constraint frame, but with no special handling of FORWARDed
messages such as :EXPOSE."))

(DEFFLAVOR CONSTRAINT-FRAME ()
	   (BASIC-CONSTRAINT-FRAME CONSTRAINT-FRAME-FORWARDING-MIXIN BASIC-FRAME)
  (:DOCUMENTATION :MIXIN "Normal constraint frame"))

(DEFFLAVOR BORDERED-CONSTRAINT-FRAME ()
	   (BASIC-CONSTRAINT-FRAME
            CONSTRAINT-FRAME-FORWARDING-MIXIN
            BORDERS-MIXIN
            BASIC-FRAME)
  (:DEFAULT-INIT-PLIST :BORDER-MARGIN-WIDTH 0)
  (:DOCUMENTATION :COMBINATION "Maintains uniform borders around panes"))

(DEFSTRUCT (CONSTRAINT-NODE (:CONC-NAME             NIL)

                            (:ALTERANT              ALTER-CONSTRAINT-NODE)
                            (:PREDICATE             NIL)
                            (:COPIER                NIL)
                            (:TYPE                  :ARRAY))
  ;; This defstruct is used by the INTERNAL-CONSTRAINTS instance variable.
  CONSTRAINT-NAME			;Name of this node
  (CONSTRAINT-MIN -1)			;Minimum limit
  (CONSTRAINT-MAX (EXPT 2 20.))		;Maximum limit (2**20)
  CONSTRAINT-CONSTRAINT			;The constraint as specified by the user
  CONSTRAINT-TYPE			;One of: :WINDOW, :STACKING, :IF, :BLANK
  CONSTRAINT-DATA			;If WINDOW: the window
					;If SPECIAL: (ordering . inferiors)
					;If IF: (conditional . inferiors)
					;If BLANK: function to draw "whitespace"
  (CONSTRAINT-CW 0)			;Current width of this window (if a window)
  (CONSTRAINT-CH 0)                     ;Current height
  (CONSTRAINT-CX 0)                     ;Current X
  (CONSTRAINT-CY 0)                     ;Current Y
  (CONSTRAINT-PW 0)			;Proposed width
  (CONSTRAINT-PH 0)			;Proposed height
  (CONSTRAINT-PX 0)			;Proposed X
  (CONSTRAINT-PY 0)			;Proposed Y
  )

(DEFUN CONSTRAINT-FRAME-PROCESS-CONSTRAINTS (&REST IGNORE)
  "CONSTRAINTS contains a list of unprocessed constraints.  Process them.  Entries
look like:

   constraint := ({:LIMIT (min max {[:LINES | :CHARACTERS]})}
		  [:ASK-WINDOW pane-name message . args |
                   :ASK message . args |
		   :FUNCALL function . args |
		   :EVAL form |
		   [:EVEN | fixnum | flonum] {[:LINES | :CHARACTERS]} |
		   :FIXED
		   ])

   desc := (ordering desc-part)
   desc-part := (desc-group) {desc-part}
   desc-group := [ ('window name' . constraint) |
   		   ('special name' [:HORIZONTAL | :VERTICAL] constraint . desc) |
		   ('special name' :IF [conditional | :ELSE] desc) *** not implemented
		   ('special name' :BLANK [:WHITE | :BLACK] constraint)
		   ] {desc-group}


    Fixnum - absolute number of pixels
    Flonum - percentage of available space
    :EVEN - divide remaining space evenly among all :EVEN constraints
            :EVEN's can only be in the last descriptor group, and must be by themselves
            (No other types of constraints allowed)
    :ASK, :ASK-WINDOW, :FUNCALL - sends the message to the pane with the args as shown below,
      and the specified args, and expects back the height or
      the width that the window wants to be.  :ASK-WINDOW takes the
      name of a window as its first arg.

    :EVAL - evals the specified form

    :FIXED - Only for a window: never change the window's size

    For :FUNCALL the first arg is the node.
    For :EVAL, **CONSTRAINT-NODE** is bound to the node.

      The first five arguments given to the method are as follows:
	**CONSTRAINT-REMAINING-WIDTH** - The maximum width of the window (amount
					 of space remaining for this window)
	**CONSTRAINT-REMAINING-HEIGHT** - The maximum height
	**CONSTRAINT-TOTAL-WIDTH** - The total width of the current section
	**CONSTRAINT-TOTAL-HEIGHT** - The total height of the current section
	**CONSTRAINT-CURRENT-STACKING** - :HORIZONTAL or :VERTICAL, depending upon
					    which dimension is currently being hacked

      (In the case of :EVAL, these special variables are bound)

 A typical frame setup might be (dimension starts out as :HEIGHT):
 ((WA LISP-LISTENER) (WB MENU :ITEM-LIST (foo bar baz quux))
  (WC MY-OWN-LISP-LISTENER) (WD SOME-OTHER-FUNNY-WINDOW :MY-INIT MY-ARG))

 ((WA
   WB
   G0)
  ((WB :ASK :PANE-SIZE))
  ((WA :LIMIT (3 NIL :LINES) :EVEN)
   (G0 :HORIZONTAL (:ASK-WINDOW WD :PANE-SIZE)
       (WD WC)
       ((WC :LIMIT (10. NIL :LINES) :EVEN)
        (WD :LIMIT (10. NIL :LINES) :EVEN)))))"
  (DECLARE (:SELF-FLAVOR BASIC-CONSTRAINT-FRAME))
  ;; First turn constraint list into nodes
  (SETQ INTERNAL-PANES (LET-GLOBALLY ((RECURSION T)) (CONSTRAINT-FRAME-WINDOWS PANES)))
  (SETQ PARSED-CONSTRAINTS NIL)
  (DOLIST (CONSTR CONSTRAINTS)
    (PUSH (CONS (CAR CONSTR)
		(CONSTRAINT-FRAME-PARSE-CONSTRAINTS (CDR CONSTR) INTERNAL-PANES))
	  PARSED-CONSTRAINTS))
  (SETQ PARSED-CONSTRAINTS (NREVERSE PARSED-CONSTRAINTS)))

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :AFTER :INIT) (IGNORE)
  (CONSTRAINT-FRAME-PROCESS-CONSTRAINTS)
  (SETF `(,CONFIGURATION . ,INTERNAL-CONSTRAINTS)
	(IF CONFIGURATION
	    (OR (ASSOC CONFIGURATION PARSED-CONSTRAINTS :TEST #'EQ)
		(FERROR NIL "Configuration ~S not found" CONFIGURATION))
	    (FIRST PARSED-CONSTRAINTS)))	;Default initial configuration
  (AND SELECTION-SUBSTITUTE
       (SYMBOLP SELECTION-SUBSTITUTE)
       (SETQ SELECTION-SUBSTITUTE (SEND SELF :GET-PANE SELECTION-SUBSTITUTE)))
  (CONSTRAINT-FRAME-RECOMPUTE-CONFIGURATION))

(DEFUN CONSTRAINT-FRAME-DRAW-BLANK-SPACE (&OPTIONAL (CONSTRS INTERNAL-CONSTRAINTS))
  "Map over the constraint data structure, and draw all blank area."
  (DECLARE (:SELF-FLAVOR BASIC-CONSTRAINT-FRAME))
  (DOLIST (AENTRY (FIRST CONSTRS))
    (LET ((NODE (CDR AENTRY)))
      (CASE (CONSTRAINT-TYPE NODE)
	(:BLANK (PREPARE-SHEET (SELF)
		  (CONSTRAINT-FRAME-DRAW-A-BLANK (CONSTRAINT-DATA NODE) NODE
						 (CONSTRAINT-CX   NODE) (CONSTRAINT-CY NODE)
						 (CONSTRAINT-CW   NODE) (CONSTRAINT-CH NODE)
						 SCREEN-ARRAY)))
	(:STACKING (CONSTRAINT-FRAME-DRAW-BLANK-SPACE (CONSTRAINT-DATA NODE)))))))

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :AFTER :REFRESH) (&OPTIONAL TYPE)
  (OR (AND RESTORED-BITS-P (NEQ TYPE :SIZE-CHANGED))
      (EQ TYPE :MARGINS-ONLY)
      (CONSTRAINT-FRAME-DRAW-BLANK-SPACE)))

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :AFTER :CHANGE-OF-DEFAULT-FONT) (OLD-FONT NEW-FONT)
  ;; SHEET'S method got the INFERIORS -- get any panes that weren't got then.
  (DOLIST (ELT INTERNAL-PANES)
    (LET ((PANE (CDR ELT)))
      (OR (MEMBER PANE INFERIORS :TEST #'EQ)
	  (NOT (TYPEP PANE 'INSTANCE))
	  (FUNCALL PANE :CHANGE-OF-DEFAULT-FONT OLD-FONT NEW-FONT)))))

;;; Stuff for dealing with panes by name

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :GET-PANE) (PANE-NAME)
  "Returns the pane with specified name or NIL if not found"
  (CDR (ASSOC PANE-NAME INTERNAL-PANES :TEST #'EQ)))

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :SEND-PANE) (PANE-NAME MESSAGE &REST ARGS &AUX W)
  "Send a message to the pane with specified name (error if not found)"
  (IF (SETQ W (CDR (ASSOC PANE-NAME INTERNAL-PANES :TEST #'EQ)))
      (APPLY W MESSAGE ARGS)
      (FERROR NIL "No pane named ~S in this frame" PANE-NAME)))

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :SEND-ALL-PANES) (MESSAGE &REST ARGS)
  "Send a message to all panes, including non-exposed ones"
  (DOLIST (X INTERNAL-PANES)
    (APPLY (CDR X) MESSAGE ARGS)))

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :SEND-ALL-EXPOSED-PANES) (MESSAGE &REST ARGS)
  "Send a message to all exposed panes"
  (DOLIST (X INTERNAL-PANES)
    (AND (MEMBER (CDR X) EXPOSED-INFERIORS :TEST #'EQ)
	 (APPLY (CDR X) MESSAGE ARGS))))

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :PANE-NAME) (PANE)
  "Given a pane, this returns the name for that pane the user gave in his alist.
   NIL if for some reason it is not found."
  (DOLIST (X INTERNAL-PANES)
    (AND (EQ (CDR X) PANE) (RETURN (CAR X)))))


(DEFMETHOD (BASIC-CONSTRAINT-FRAME :GET-CONFIGURATION) (CONFIG-NAME)
  (CDR (ASSOC CONFIG-NAME PARSED-CONSTRAINTS :TEST #'EQ)))

(DEFMETHOD (tv:BASIC-CONSTRAINT-FRAME :REDEFINE-CONFIGURATION) (CONFIG-NAME NEW-CONFIG
								&OPTIONAL (PARSED-P T))
  (OR PARSED-P
      (SETQ NEW-CONFIG (CONSTRAINT-FRAME-PARSE-CONSTRAINTS NEW-CONFIG INTERNAL-PANES)))
  (LET ((CONFIG (ASSOC CONFIG-NAME PARSED-CONSTRAINTS :TEST #'EQ)))
    (WITHOUT-INTERRUPTS
      (OR CONFIG
	  (IF PARSED-CONSTRAINTS
	      (RPLACD (LAST PARSED-CONSTRAINTS)
		      ;; added call to LIST modified 11/17/87
		      ;; each configuration needs to be grouped in a list
		      (list (SETQ CONFIG (CONS CONFIG-NAME NIL))))
	      (SETQ PARSED-CONSTRAINTS
		    ;; added call to LIST 11/17/87
		    (list (SETQ CONFIG (CONS CONFIG-NAME NIL)))))))
    (RPLACD CONFIG NEW-CONFIG)
    NEW-CONFIG))

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :SET-CONFIGURATION) (NEW-CONFIG-NAME)
  (LET ((CONFIG (ASSOC NEW-CONFIG-NAME PARSED-CONSTRAINTS :TEST #'EQ)))
    (OR CONFIG (FERROR NIL "Unknown configuration ~A" NEW-CONFIG-NAME))
    (SETQ CONFIGURATION        (CAR CONFIG)
	  INTERNAL-CONSTRAINTS (CDR CONFIG)
	  BLANK-RECTANGLES     NIL))
  (CONSTRAINT-FRAME-CLEAR-CURRENT-POSITION INTERNAL-CONSTRAINTS)
  (CONSTRAINT-FRAME-RECOMPUTE-CONFIGURATION))

(DEFUN CONSTRAINT-FRAME-CLEAR-CURRENT-POSITION (CONSTRS &AUX NODE)
  (DOLIST (AENTRY (FIRST CONSTRS))
    (SETQ NODE (CDR AENTRY))
    (CASE (CONSTRAINT-TYPE NODE)
      (:WINDOW
       (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
	   (SEND (CONSTRAINT-DATA NODE) :EDGES)
	 (SETF (CONSTRAINT-CX NODE) LEFT)
	 (SETF (CONSTRAINT-CY NODE) TOP)
	 (SETF (CONSTRAINT-CW NODE) (- RIGHT LEFT))
	 (SETF (CONSTRAINT-CH NODE) (- BOTTOM TOP))))
      (:STACKING
       (CONSTRAINT-FRAME-CLEAR-CURRENT-POSITION (CONSTRAINT-DATA NODE)))
      (:IF (FERROR NIL ":IF node type not yet implemented"))
      (OTHERWISE
       (SETF (CONSTRAINT-CX NODE) 0)
       (SETF (CONSTRAINT-CY NODE) 0)
       (SETF (CONSTRAINT-CW NODE) 0)
       (SETF (CONSTRAINT-CH NODE) 0)))))

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :VERIFY-NEW-EDGES) (IGNORE IGNORE NEW-WIDTH NEW-HEIGHT)
  (COND ((VARIABLE-BOUNDP INTERNAL-CONSTRAINTS)
	 (IF (NOT (CONSTRAINT-FRAME-DO-CONSTRAINTS SELF INTERNAL-CONSTRAINTS
					  (- NEW-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)
					  (- NEW-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)))
	     "Some constraint cannot be satisfied"
	     ;; Change our size temporarily
	     ;; so that panes will check their edges against the new size
	     (WITHOUT-INTERRUPTS
	       (LET-GLOBALLY ((WIDTH NEW-WIDTH) (HEIGHT NEW-HEIGHT) (RECURSION T))
		 (COND ((CONSTRAINT-FRAME-SET-EDGES INTERNAL-CONSTRAINTS :VERIFY) NIL)
		       (T "Some pane refused to have its size set"))))))))

(DEFUN CONSTRAINT-FRAME-RECOMPUTE-CONFIGURATION ()
  (DECLARE (:SELF-FLAVOR BASIC-CONSTRAINT-FRAME))
  (PRESERVE-SUBSTITUTE-STATUS SELF
    (LET-GLOBALLY ((RECURSION T))
      (SEND SELF :DEEXPOSE
            :DEFAULT                           ; Save-bits-p
            :NOOP                              ; screen-bits-action
            )
      ;; The following code was removed because it does a losing thing.
      ;; For example in the Font EDitor, if there is a character in a
      ;; register, when the frame is recomputed the register pane will
      ;; be erased, redrawn and then erased again.  The following code
      ;; is what causes the redundant redraw/erase to happen.
;;      (DOLIST (P EXPOSED-PANES)
;;	(SEND P :DEEXPOSE :DEFAULT :NOOP)
;;	;; This assures that old panes won't gratuitously reappear if there is
;;	;; blank space in the new configuration where a pane used to be.  Note
;;	;; that this is careful not to touch inferiors that aren't "panes" of
;;	;; this constraint frame.
;;	(SEND P :DEACTIVATE))
      (OR (CONSTRAINT-FRAME-DO-CONSTRAINTS
            SELF INTERNAL-CONSTRAINTS (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT))
	  (FERROR NIL "Some constraint could not be satisfied."))
      (CONSTRAINT-FRAME-SET-EDGES INTERNAL-CONSTRAINTS NIL)
      ;; Put the panes back on the screen.
      (DOLIST (P EXPOSED-PANES) (SEND P :EXPOSE))

      ;; Indicate that we need to recompute the blank rectangles.
      (SETQ BLANK-RECTANGLES NIL)
      (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
	(CONSTRAINT-FRAME-DRAW-BLANK-SPACE))
      (OR (MEMBER SELECTION-SUBSTITUTE EXPOSED-INFERIORS :TEST #'EQ)
	  (SETQ SELECTION-SUBSTITUTE NIL)))))

;;; When the inside-size changes, rethink the constraints and panes' edges
(DEFMETHOD (BASIC-CONSTRAINT-FRAME :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (CONSTRAINT-FRAME-RECOMPUTE-CONFIGURATION))

(DEFUN CONSTRAINT-FRAME-SCREEN-MANAGE-UNCOVERED-AREA (RECTS ARRAY X Y IGNORE)
  "If there is any blank area, it might be covered by some :BLANK type
constraints.  Check through the constraint list, and draw onto the array
the appropriate swatches of 'blankness'"
  (DECLARE (:SELF-FLAVOR BASIC-CONSTRAINT-FRAME))
  (OR BLANK-RECTANGLES
      ;; If haven't figured out the blank rectangles, compute them now
      (SETQ BLANK-RECTANGLES
	    (OR (CONSTRAINT-FRAME-MAKE-BLANK-RECTANGLES INTERNAL-CONSTRAINTS)
		T)))
  (AND (NEQ BLANK-RECTANGLES T)
       (DOLIST (R RECTS)
	 (COND ((EQ (CAR (RECT-SOURCE R)) SELF)
		;; This is a blank area, hack appropriate portions of it
		(BLOCK DONE (PROG ((REMAINING-BLANK-RECTS (LIST R)))
		  (DOLIST (BLANK-RECT BLANK-RECTANGLES)
		    (DOLIST (REM-BLANK-RECT REMAINING-BLANK-RECTS)
		      (COND ((RECT-NOT-OVERLAP-RECT-P BLANK-RECT REM-BLANK-RECT))
			    (T
			     (LET ((NODE (FOURTH (RECT-SOURCE BLANK-RECT)))
				   (LEFT) (TOP) (RIGHT) (BOTTOM))
			       ;; Draw the overlapping area
			       (SETQ LEFT   (MAX (RECT-LEFT   BLANK-RECT) (RECT-LEFT   REM-BLANK-RECT))
				     TOP    (MAX (RECT-TOP    BLANK-RECT) (RECT-TOP    REM-BLANK-RECT))
				     RIGHT  (MIN (RECT-RIGHT  BLANK-RECT) (RECT-RIGHT  REM-BLANK-RECT))
				     BOTTOM (MIN (RECT-BOTTOM BLANK-RECT) (RECT-BOTTOM REM-BLANK-RECT)))
			       (CONSTRAINT-FRAME-DRAW-A-BLANK
				 (CONSTRAINT-DATA NODE) NODE
				 (+ X LEFT) (+ Y TOP) (- RIGHT LEFT) (- BOTTOM TOP)
				 ARRAY))
			     (SETQ REMAINING-BLANK-RECTS
				   (NCONC (RECTANGLE-NOT-INTERSECTION BLANK-RECT
								      REM-BLANK-RECT)
					  (DELETE REM-BLANK-RECT (THE LIST REMAINING-BLANK-RECTS) :TEST #'EQ)))
			     (AND (NULL REMAINING-BLANK-RECTS) (RETURN-FROM DONE))))))
		  (SETQ RECTS (NCONC REMAINING-BLANK-RECTS RECTS))))
		(SETQ RECTS (DELETE R (THE LIST RECTS) :COUNT 1 :TEST #'EQ))))))
  RECTS)

(DEFWRAPPER (BASIC-CONSTRAINT-FRAME :SCREEN-MANAGE-UNCOVERED-AREA) ((RECTS ARRAY X Y ALU)
								     . BODY)
  `(PROGN (SETF (SECOND SI:.DAEMON-CALLER-ARGS.)
		(CONSTRAINT-FRAME-SCREEN-MANAGE-UNCOVERED-AREA RECTS ARRAY X Y ALU))
	  . ,BODY))

(DEFUN CONSTRAINT-FRAME-MAKE-BLANK-RECTANGLES (CONSTR &AUX RECTS)
  (DOLIST (AENTRY (FIRST CONSTR))
    (LET ((NODE (CDR AENTRY))
	  (X) (Y))
      (CASE (CONSTRAINT-TYPE NODE)
            (:BLANK (PUSH (LIST (LIST SELF 0 0 NODE)
                                (SETQ X (CONSTRAINT-CX NODE)) (SETQ Y (CONSTRAINT-CY NODE))
                                (+ X (CONSTRAINT-CW NODE)) (+ Y (CONSTRAINT-CH NODE)))
                          RECTS))
            (:STACKING
             (SETQ RECTS
                   (NCONC RECTS
                          (CONSTRAINT-FRAME-MAKE-BLANK-RECTANGLES (CONSTRAINT-DATA NODE))))))))
  RECTS)

(DEFVAR CONSTRAINT-FRAME-DEFAULT-STACKING :VERTICAL)

(DEFUN CONSTRAINT-FRAME-SET-EDGES (CONSTRS OPTION &AUX X Y R)
  "Loop over all panes and hack the edges as specified by the option."
  (NOT
    (DOLIST (AENTRY (FIRST CONSTRS))
      (LET ((NODE (CDR AENTRY)))
	(CASE (CONSTRAINT-TYPE NODE)
              (:WINDOW
               (SETQ R (OR (AND (NEQ OPTION :VERIFY)
                                (= (CONSTRAINT-PX NODE) (CONSTRAINT-CX NODE))
                                (= (CONSTRAINT-PY NODE) (CONSTRAINT-CY NODE))
                                (= (CONSTRAINT-PW NODE) (CONSTRAINT-CW NODE))
                                (= (CONSTRAINT-PH NODE) (CONSTRAINT-CH NODE))
				(= (sheet-locations-per-line
				     (constraint-data node))
				   (sheet-locations-per-line
				     (sheet-superior (constraint-data node)))))
                           ;; Proposed position <> current position.
                           (FUNCALL (CONSTRAINT-DATA NODE) :SET-EDGES
                                    (SETQ X (CONSTRAINT-PX NODE)) (SETQ Y (CONSTRAINT-PY NODE))
                                    (+ X (CONSTRAINT-PW NODE)) (+ Y (CONSTRAINT-PH NODE))
                                    OPTION))))
              (:STACKING (SETQ R (CONSTRAINT-FRAME-SET-EDGES (CONSTRAINT-DATA NODE) OPTION)))
              (:IF (FERROR NIL ":IF is unimplemented option"))
              (OTHERWISE (SETQ R T)))
	(IF (EQ OPTION :VERIFY)

	    ;; If verifying, return right away if didn't verify
	    (OR R (RETURN T))
            ;;ELSE
            (PROGN
              ;; If not verifying, proposed data is now current data
              (SETF (CONSTRAINT-CX NODE) (CONSTRAINT-PX NODE))
              (SETF (CONSTRAINT-CY NODE) (CONSTRAINT-PY NODE))
              (SETF (CONSTRAINT-CW NODE) (CONSTRAINT-PW NODE))
              (SETF (CONSTRAINT-CH NODE) (CONSTRAINT-PH NODE))))))))

(DEFUN CONSTRAINT-FRAME-WINDOWS (DESCS &AUX PARSED)
  (DOLIST (DESC DESCS)
    (PUSH (CONS (FIRST DESC)
		(IF (TYPEP (CDR DESC) 'SHEET)
		    (CDR DESC)
		    (APPLY SELF :CREATE-PANE DESC)))
	  PARSED))
  PARSED)

(DEFMETHOD (BASIC-CONSTRAINT-FRAME :CREATE-PANE) (IGNORE FLAVOR &REST OPTIONS)
  (APPLY #'MAKE-WINDOW FLAVOR :SUPERIOR SELF OPTIONS))

(DEFFLAVOR CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER ((IO-BUFFER NIL)) (BASIC-CONSTRAINT-FRAME)
  (:INITABLE-INSTANCE-VARIABLES IO-BUFFER))

(DEFMETHOD (CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER :BEFORE :INIT) (IGNORE)
  ;; Make sure that there is an IO buffer.
  (OR IO-BUFFER (SETQ IO-BUFFER (MAKE-DEFAULT-IO-BUFFER))))

(DEFMETHOD (CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER :CREATE-PANE) (IGNORE FLAVOR &REST OPTIONS)
  (APPLY #'MAKE-WINDOW FLAVOR :SUPERIOR SELF :IO-BUFFER IO-BUFFER OPTIONS))

(DEFFLAVOR BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER ()
  (CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER CONSTRAINT-FRAME-FORWARDING-MIXIN
   BORDERS-MIXIN BASIC-FRAME)
  (:DEFAULT-INIT-PLIST :BORDER-MARGIN-WIDTH 0))

(DEFUN CONSTRAINT-FRAME-SUBSTITUTION (DESC)
  (DECLARE (:SELF-FLAVOR BASIC-CONSTRAINT-FRAME))
  (COND ((CONSP DESC) DESC)
	((SYMBOLP DESC)
	 (OR (CDR (ASSOC DESC SUBSTITUTIONS :TEST #'EQ))
	     (FERROR NIL "~A is a symbol where a list was expected (and it has no substitution)"
	       DESC)))
	(T (FERROR NIL "~A was found where a list (or a symbol with a substitution) was expected"
	     DESC))))

(DEFUN CONSTRAINT-FRAME-PARSE-CONSTRAINTS (CONSTRAINTS PANES
					    &OPTIONAL (STACKING
							CONSTRAINT-FRAME-DEFAULT-STACKING))
  "Given a list of constraints, returns the internal format."
  (LET ((INTERNAL-ORDERING NIL)
	(ORDERING (CAR CONSTRAINTS))
	(INTERNAL-DESCS NIL))
    (OR (CONSP ORDERING)
	(FERROR NIL "Constraint ~S does not start with an ordering" CONSTRAINTS))
    (DO ((WNS ORDERING (CDR WNS))
	 (WS NIL)
	 (WINDOW))
	((NULL WNS)
	 (SETQ INTERNAL-ORDERING (NREVERSE WS)))
      (PUSH (CONS (CAR WNS)
		  (IF (SETQ WINDOW (ASSOC (CAR WNS) PANES :TEST #'EQ))
		      (CDR WINDOW)		;A window, include the window itself
		      (CAR WNS)))		;Must be a special name, will fill it in later
	    WS))
    (DO ((DESC-GROUPS (CDR CONSTRAINTS) (CDR DESC-GROUPS))
	 (INTERNAL-DESC-GROUP NIL NIL)
	 (EVEN-P NIL NIL)
	 (LAST-GROUP-P))
	((NULL DESC-GROUPS))
      ;; Process each descriptor group
      (SETQ LAST-GROUP-P (NULL (CDR DESC-GROUPS)))
      (DOLIST (DESC (CONSTRAINT-FRAME-SUBSTITUTION (CAR DESC-GROUPS)))

	REPARSE					;Yes, this is a GO tag!

	;; For each descriptor, parse it
	(SETQ DESC (CONSTRAINT-FRAME-SUBSTITUTION DESC))
	(LET ((NAME (CAR DESC))
	      (WOS)                             ; Window Or Special name
              (AENTRY)
              (MACRO-P))
	  (OR (SETQ WOS (CDR (SETQ AENTRY (ASSOC NAME INTERNAL-ORDERING :TEST #'EQ))))
	      (FERROR NIL "~A is unspecified in the ordering" NAME))
	  (COND ((TYPEP WOS 'SHEET)
		 ;; A window -- parse the constraint and make the node
		 (MULTIPLE-VALUE-BIND (CONSTR MIN MAX TEM)
		     (PARSE-CONSTRAINT (CDR DESC) PANES WOS LAST-GROUP-P EVEN-P)
		   (SETQ EVEN-P TEM)
		   (PUSH (MAKE-CONSTRAINT-NODE CONSTRAINT-NAME (SYMBOL-NAME NAME)
					       CONSTRAINT-TYPE :WINDOW
					       CONSTRAINT-CONSTRAINT CONSTR
					       CONSTRAINT-DATA WOS
					       CONSTRAINT-MIN MIN
					       CONSTRAINT-MAX MAX)
			 INTERNAL-DESC-GROUP)))
		((NOT (SYMBOLP WOS))
		 (FERROR NIL "~A is not a valid special name at this point" NAME))
		((MEMBER (SECOND DESC) '(:HORIZONTAL :VERTICAL) :TEST #'EQ)
		 (AND (EQ (SECOND DESC) STACKING)
		      (FERROR NIL "Current stacking (~A) same as new stacking" STACKING))
		 (MULTIPLE-VALUE-BIND (CONSTR MIN MAX TEM)
		     (PARSE-CONSTRAINT (THIRD DESC) PANES NIL LAST-GROUP-P EVEN-P)
		   (SETQ EVEN-P TEM)
		   (PUSH (MAKE-CONSTRAINT-NODE
			   CONSTRAINT-NAME (SYMBOL-NAME NAME)
			   CONSTRAINT-TYPE :STACKING
			   CONSTRAINT-CONSTRAINT CONSTR
			   CONSTRAINT-DATA
			    (CONSTRAINT-FRAME-PARSE-CONSTRAINTS (CDDDR DESC)
								PANES
								(SECOND DESC))
			   CONSTRAINT-MIN MIN
			   CONSTRAINT-MAX MAX)
			 INTERNAL-DESC-GROUP)))
		((EQ (SECOND DESC) :IF)
		 (FERROR NIL "Conditionals not currently supported"))
		((EQ (SECOND DESC) :BLANK)
		 (MULTIPLE-VALUE-BIND (CONSTR MIN MAX TEM)
		     (PARSE-CONSTRAINT (CDDDR DESC) PANES NIL LAST-GROUP-P EVEN-P)
		   (SETQ EVEN-P TEM)
		   (PUSH (MAKE-CONSTRAINT-NODE
			   CONSTRAINT-NAME (SYMBOL-NAME NAME)
			   CONSTRAINT-TYPE :BLANK
			   CONSTRAINT-CONSTRAINT CONSTR
			   CONSTRAINT-DATA (CASE (THIRD DESC)
					     (:WHITE #'CONSTRAINT-FRAME-WHITE-BLANKING)
					     (:BLACK #'CONSTRAINT-FRAME-BLACK-BLANKING)
					     (OTHERWISE (THIRD DESC)))
			   CONSTRAINT-MIN MIN
			   CONSTRAINT-MAX MAX)
			 INTERNAL-DESC-GROUP)))
		((SETQ MACRO-P (GET (SECOND DESC) 'CONSTRAINT-MACRO))
		 ;; A macro: expand it and use its expansion in place of the current
		 ;; description
		 (SETQ DESC (CONS (CAR DESC) (FUNCALL MACRO-P DESC STACKING)))
		 (GO REPARSE))

		(T (FERROR NIL "~A is unknown special keyword, perhaps ~A is missing from TV:PANES"
                           (SECOND DESC) (FIRST DESC))))
	  (RPLACD AENTRY (CAR INTERNAL-DESC-GROUP))))
      (PUSH (NREVERSE INTERNAL-DESC-GROUP) INTERNAL-DESCS))
    (DOLIST (ORD INTERNAL-ORDERING)
      (AND (OR (TYPEP (CDR ORD) 'SHEET) (EQ (CAR ORD) (CDR ORD)))
	   (FERROR NIL "~A was specified in ordering, but not specified in any descriptor"
		   (CAR ORD))))
    (CONS INTERNAL-ORDERING (NREVERSE INTERNAL-DESCS))))

(DEFUN PARSE-CONSTRAINT (CONSTR PANES WINDOW LG-P EVEN-P &AUX (MIN -1) (MAX (EXPT 2 15.)))
  "Verify correctness of the specified constraint.  Returns the constraint part
of the constraint, as well as the limits if specified."
  (DECLARE)
  (BLOCK NIL
        (COND ((EQ (FIRST CONSTR) :LIMIT)
               (LET ((LIMITS (SECOND CONSTR))
                     (ROUND) (OFFSET))
                 (COND ((> (LENGTH LIMITS) 2)
                        (OR (SETQ WINDOW (OR (PARSE-CONSTRAINT-GET-PANE (FOURTH LIMITS) PANES)
                                             WINDOW))
                            (FERROR NIL "Illegal format :LIMIT (no window specified)"))
                        (CASE (THIRD LIMITS)
                              (:CHARACTERS (SETQ ROUND (SHEET-CHAR-WIDTH WINDOW)
                                                 OFFSET (+ (SHEET-LEFT-MARGIN-SIZE  WINDOW)
                                                           (SHEET-RIGHT-MARGIN-SIZE WINDOW))))
                              (:LINES (SETQ ROUND (SHEET-LINE-HEIGHT WINDOW)
                                            OFFSET (+ (SHEET-TOP-MARGIN-SIZE    WINDOW)
                                                      (SHEET-BOTTOM-MARGIN-SIZE WINDOW))))
                              (OTHERWISE (FERROR NIL "~A is illegal rounding specification"
                                                 (THIRD LIMITS))))))
                 (SETQ MIN (OR (FIRST  LIMITS) MIN)
                       MAX (OR (SECOND LIMITS) MAX))
                 (SETQ MIN (IF ROUND (+ (* MIN ROUND) OFFSET) MIN)
                       MAX (IF ROUND (+ (* MAX ROUND) OFFSET) MAX)))
               (SETQ CONSTR (CDDR CONSTR))))
        (COND ((OR (IF (NUMBERP (FIRST CONSTR))
                       (OR (NULL EVEN-P) (EQ EVEN-P :NO)
                           (FERROR NIL "Cannot mix :EVEN constraints and other constraints"))
                       NIL)
                   (COND ((EQ (FIRST CONSTR) :EVEN)
                          (OR LG-P (FERROR NIL ":EVEN not in last descriptor group"))
                          (OR (NULL EVEN-P) (EQ EVEN-P :YES)
                              (FERROR NIL "Cannot mix :EVEN constraints and other constraints"))
                          (SETQ EVEN-P :YES)
                          T)))
               (COND ((> (LENGTH CONSTR) 1)
                      (LET ((W (PARSE-CONSTRAINT-GET-PANE (THIRD CONSTR) PANES)))
                        (IF W
                            (SETQ WINDOW W
                                  CONSTR (LIST (FIRST CONSTR) (SECOND CONSTR) W))
                            (OR WINDOW
                                (FERROR NIL "Illegal format constraint -- no window specified"))))
                      (OR (MEMBER (SECOND CONSTR) '(:LINES :CHARACTERS) :TEST #'EQ)
                          (FERROR NIL "Illegal rounding specifier ~A" (SECOND CONSTR)))
                      (AND (INTEGERP (FIRST CONSTR))
                           (SETQ CONSTR (LIST (* (FIRST CONSTR)
                                                 (CASE (SECOND CONSTR)
                                                       (:LINES (SHEET-LINE-HEIGHT WINDOW))
                                                       (:CHARACTERS (SHEET-CHAR-WIDTH WINDOW))))
                                              (SECOND CONSTR)
                                              WINDOW))))))
              ((NOT (OR (NULL EVEN-P) (EQ EVEN-P :NO)))
               (FERROR NIL "Cannot mix :EVEN constraints and other constraints"))
              ((MEMBER (FIRST CONSTR) '(:ASK :FUNCALL :EVAL :FIXED) :TEST #'EQ))
              ((EQ (FIRST CONSTR) :ASK-WINDOW)
               (LET ((W (IF (EQ (SECOND CONSTR) 'SELF)
                            SELF
                            (CDR (ASSOC (SECOND CONSTR) PANES :TEST #'EQ)))))
                 (OR W (FERROR NIL "Unknown pane ~A is :ASK-WINDOW constraint"
                               (SECOND CONSTR)))
                 (SETF (SECOND (SETQ CONSTR (COPY-LIST CONSTR))) W))))
        (RETURN CONSTR MIN MAX (OR EVEN-P :NO))))

(DEFUN PARSE-CONSTRAINT-GET-PANE (PANE-NAME PANES)
  (AND PANE-NAME
       (OR (CDR (ASSOC PANE-NAME PANES :TEST #'EQ))
	   (FERROR NIL "Unknown pane name ~A" PANE-NAME))))

(DEFUN CONSTRAINT-FRAME-DO-CONSTRAINTS (FRAME CONSTRS
					&OPTIONAL (W (SHEET-INSIDE-WIDTH FRAME))
					          (H (SHEET-INSIDE-HEIGHT FRAME)))
  (CATCH :VERTICAL
    (CATCH :HORIZONTAL
      (PROGN
	(CONSTRAINT-FRAME-DO-SIZES W H CONSTRS)
	(FUNCALL FRAME :SET-EXPOSED-PANES
		 (CONSTRAINT-FRAME-DO-POSITIONS CONSTRS CONSTRAINT-FRAME-DEFAULT-STACKING
						(SHEET-LEFT-MARGIN-SIZE FRAME)
						(SHEET-TOP-MARGIN-SIZE  FRAME)))
	T))))

(DEFUN CONSTRAINT-FRAME-DO-SIZES (WIDTH HEIGHT CONSTRS
				   &OPTIONAL (STACKING CONSTRAINT-FRAME-DEFAULT-STACKING))
  "Given that the current width and height of the frame, calculate new values of position
and size for each node.  Constraints are assumed parsed and valid."
;;; **** Does not know about min's or max's yet!
  (LET ((DESC-GROUPS (CDR CONSTRS)))
    (DOLIST (DESC-GROUP DESC-GROUPS)
      ;; For each group, assign widths and heights
      (LET (WIDTH-USED HEIGHT-USED)
	(COND ((NOT (CATCH STACKING
		      (PROGN
			(MULTIPLE-VALUE-SETQ (WIDTH-USED HEIGHT-USED)
			  (CONSTRAINT-FRAME-DO-SIZES-INTERNAL DESC-GROUP
							      WIDTH HEIGHT WIDTH HEIGHT
							      STACKING))
			T)))
	       ;; Some constraint couldn't be satisfied.  Try only allowing each constraint
	       ;; to take 1/Nth of the area, where N is the total number of constraints
	       (MULTIPLE-VALUE-SETQ (WIDTH-USED HEIGHT-USED)
		 (CONSTRAINT-FRAME-DO-SIZES-INTERNAL
                   DESC-GROUP WIDTH HEIGHT
                   (IF (EQ STACKING :HORIZONTAL) (TRUNCATE WIDTH  (LENGTH DESC-GROUP)) WIDTH )
                   (IF (EQ STACKING :VERTICAL  ) (TRUNCATE HEIGHT (LENGTH DESC-GROUP)) HEIGHT)
                   STACKING))))
	(SETQ WIDTH  (- WIDTH  WIDTH-USED)
	      HEIGHT (- HEIGHT HEIGHT-USED))
	(AND (OR (< WIDTH 0) (< HEIGHT 0))
	     (THROW STACKING NIL))))))

(DEFUN CONSTRAINT-FRAME-DO-SIZES-INTERNAL (DESC-GROUP WIDTH HEIGHT AV-WIDTH AV-HEIGHT STACKING
					   &AUX (WIDTH-USED 0) (HEIGHT-USED 0) W H)
  (DOLIST (NODE DESC-GROUP)
    ;; Amount of room available is the smaller of the total amount of room, or the maximum
    ;; allowed by the caller
    (SETQ AV-WIDTH  (MIN (- WIDTH  WIDTH-USED ) AV-WIDTH)
	  AV-HEIGHT (MIN (- HEIGHT HEIGHT-USED) AV-HEIGHT))
    (CASE (CONSTRAINT-TYPE NODE)
      ;; Dispatch on node type
      ((:WINDOW :BLANK)
       ;; A real window, easy -- compute the new values, stick 'em in, and loop
       ;; Blanks get handled in same way
       (MULTIPLE-VALUE-SETQ (W H)
	 (CONSTRAINT-FRAME-DO-A-CONSTRAINT NODE AV-WIDTH AV-HEIGHT WIDTH HEIGHT
					   STACKING DESC-GROUP)))
      (:STACKING
       ;; A special thing, some sort of descent needed.  First fill in size
       (MULTIPLE-VALUE-SETQ (W H)
	 (CONSTRAINT-FRAME-DO-A-CONSTRAINT NODE AV-WIDTH AV-HEIGHT WIDTH HEIGHT
					   STACKING DESC-GROUP))
       ;; Then recurse with new values
       (CONSTRAINT-FRAME-DO-SIZES W H (CONSTRAINT-DATA NODE)
				  (CASE STACKING
				    (:VERTICAL :HORIZONTAL)
				    (:HORIZONTAL :VERTICAL))))
      (:IF (FERROR NIL ":IF not yet supported"))
      (OTHERWISE (FERROR NIL "Unknown node type ~A" (CONSTRAINT-TYPE NODE))))
    (CASE STACKING
      (:VERTICAL
       (SETQ HEIGHT-USED (+ HEIGHT-USED H))
       (AND (> HEIGHT-USED HEIGHT)
	    ;; Ran out of room, try alternate algorithm
	    (THROW :VERTICAL NIL)))
      (:HORIZONTAL
       (SETQ WIDTH-USED (+ WIDTH-USED W))
       (AND (> WIDTH-USED WIDTH)
	    (THROW :HORIZONTAL NIL)))))
  (VALUES WIDTH-USED HEIGHT-USED))


(DEFVAR **CONSTRAINT-NODE**)
(DEFVAR **CONSTRAINT-REMAINING-WIDTH**)
(DEFVAR **CONSTRAINT-REMAINING-HEIGHT**)
(DEFVAR **CONSTRAINT-TOTAL-WIDTH**)
(DEFVAR **CONSTRAINT-TOTAL-HEIGHT**)
(DEFVAR **CONSTRAINT-CURRENT-STACKING**)

(DEFUN CONSTRAINT-FRAME-DO-A-CONSTRAINT
        (NODE AV-W AV-H W H STACKING DESCRIPTION-GROUP &AUX AMOUNT CON)
  "Processes one constraint, setting the proposed width and height in
the node to the ones specified by the constraint."
  (DECLARE)
  (BLOCK NIL
        (SETQ CON (CONSTRAINT-CONSTRAINT NODE))
        (COND
          ;; Dispatch on type of constraint
          ((EQ (FIRST CON) :ASK-WINDOW)
           (SETQ AMOUNT (MIN (CONSTRAINT-MAX NODE)
                             (MAX (CONSTRAINT-MIN NODE)
                                  (APPLY (SECOND CON)   ; Pane name
                                         (THIRD CON)    ; Message name
                                         AV-W AV-H W H STACKING (CDDDR CON))))))
          ((EQ (FIRST CON) :ASK)
           (SETQ AMOUNT (MIN (CONSTRAINT-MAX NODE)
                             (MAX (CONSTRAINT-MIN NODE)
                                  (APPLY (CONSTRAINT-DATA NODE)
                                         (SECOND CON) AV-W AV-H W H STACKING (CDDR CON))))))
          ((EQ (FIRST CON) :FUNCALL)
           (SETQ AMOUNT (MIN (CONSTRAINT-MAX NODE)
                             (MAX (CONSTRAINT-MIN NODE)
                                  (APPLY (SECOND CON) NODE AV-W AV-H W H STACKING
                                         (CDDR CON))))))
          ((EQ (FIRST CON) :EVAL)
           (LET ((**CONSTRAINT-NODE** NODE)
                 (**CONSTRAINT-REMAINING-WIDTH** AV-W)
                 (**CONSTRAINT-REMAINING-HEIGHT** AV-H)
                 (**CONSTRAINT-TOTAL-WIDTH** W)
                 (**CONSTRAINT-TOTAL-HEIGHT** H)
                 (**CONSTRAINT-CURRENT-STACKING** STACKING))
             (SETQ AMOUNT (MIN (CONSTRAINT-MAX NODE)
                               (MAX (CONSTRAINT-MIN NODE)
                                    (EVAL (SECOND CON)))))))
          ((INTEGERP (FIRST CON))
           (SETQ AMOUNT (CONSTRAINT-ROUND (FIRST CON) CON NODE)))
          ((FLOATP (FIRST CON))
           (SETQ AMOUNT (CONSTRAINT-ROUND (* (FIRST CON) (CASE STACKING
                                                               (:VERTICAL   H)
                                                               (:HORIZONTAL W)))
                                          CON NODE)))
          ((EQ (FIRST CON) :EVEN)
           (SETQ AMOUNT (CONSTRAINT-ROUND
                          (TRUNCATE (CASE STACKING
                                          (:HORIZONTAL AV-W)
                                          (:VERTICAL   AV-H))
                                    (- (LENGTH DESCRIPTION-GROUP)
                                       ;; Compute position of current node.
                                       (DO ((I 0 (1+ I))
                                            (L DESCRIPTION-GROUP (CDR L)))
                                           ((NULL L) (FERROR NIL "Node not a node"))
                                         (AND (EQ (CAR L) NODE) (RETURN I)))))
                          CON NODE)))
          ((EQ (FIRST CON) :FIXED)
           (CASE STACKING
                 (:VERTICAL   (SETQ AMOUNT (SHEET-HEIGHT (CONSTRAINT-DATA NODE))
                                    W      (SHEET-WIDTH  (CONSTRAINT-DATA NODE))))
                 (:HORIZONTAL (SETQ H      (SHEET-HEIGHT (CONSTRAINT-DATA NODE))
                                    AMOUNT (SHEET-WIDTH  (CONSTRAINT-DATA NODE))))))
          (T (FERROR NIL "Unknown constraint type ~A" CON)))
        (CASE STACKING
              (:VERTICAL   (SETF (CONSTRAINT-PW NODE) W)
                           (SETF (CONSTRAINT-PH NODE) (SETQ H AMOUNT)))
              (:HORIZONTAL (SETF (CONSTRAINT-PW NODE) (SETQ W AMOUNT))
                           (SETF (CONSTRAINT-PH NODE) H))
              (OTHERWISE (FERROR NIL "Illegal value for stacking ~A" STACKING)))
        (RETURN W H)))

(DEFUN CONSTRAINT-ROUND (SIZE CON NODE &AUX TEM (WINDOW (CONSTRAINT-DATA NODE)))
  "Given a proposed size, a constraint, and the node, don't round, or round to lines
or characters.  Also enforces limits."
  (SETQ SIZE (TRUNCATE SIZE))
  (MIN (CONSTRAINT-MAX NODE)
       (MAX (CONSTRAINT-MIN NODE)
	    (COND ((OR (NUMBERP (FIRST CON)) (EQ (FIRST CON) :EVEN))
		   (SETQ WINDOW (OR (THIRD CON) WINDOW))
		   (CASE (SECOND CON)
                         (:LINES
                          (+ (SHEET-TOP-MARGIN-SIZE WINDOW) (SHEET-BOTTOM-MARGIN-SIZE WINDOW)
                             (* (SETQ TEM (SHEET-LINE-HEIGHT WINDOW))
                                (TRUNCATE SIZE TEM))))
                         (:CHARACTERS
                          (+ (SHEET-LEFT-MARGIN-SIZE WINDOW) (SHEET-RIGHT-MARGIN-SIZE WINDOW)
                             (* (SETQ TEM (SHEET-CHAR-WIDTH WINDOW))
                                (TRUNCATE SIZE TEM))))
                         (T SIZE)))
		  (T SIZE)))))

(DEFUN CONSTRAINT-FRAME-DO-POSITIONS (CONSTRS
                                      &OPTIONAL (STACKING CONSTRAINT-FRAME-DEFAULT-STACKING)
                                      (X 0) (Y 0)
                                      &AUX NODE PANES)
  "Given that proposed size has been set up, set up the proposed positions.  Returns a list
of all involved panes."
  (DOLIST (AENTRY (FIRST CONSTRS))
    ;; Loop over windows in order, and assign positions
    (SETQ NODE (CDR AENTRY))
    (SETF (CONSTRAINT-PX NODE) X)
    (SETF (CONSTRAINT-PY NODE) Y)
    (CASE (CONSTRAINT-TYPE NODE)
          (:WINDOW (PUSH (CONSTRAINT-DATA NODE) PANES))
          (:STACKING (SETQ PANES
                           (NCONC (CONSTRAINT-FRAME-DO-POSITIONS (CONSTRAINT-DATA NODE)
                                                                 (CASE STACKING
                                                                       (:VERTICAL :HORIZONTAL)
                                                                       (:HORIZONTAL :VERTICAL))
                                                                 X Y)
                                  PANES)))
          (:IF (FERROR NIL ":IF not yet supported"))
          (:BLANK)
          (OTHERWISE (FERROR NIL "Unknown node type ~A" (CONSTRAINT-TYPE NODE))))
    (CASE STACKING
          (:VERTICAL (SETQ Y (+ Y (CONSTRAINT-PH NODE))))
          (:HORIZONTAL (SETQ X (+ X (CONSTRAINT-PW NODE))))))
  PANES)

(DEFUN MAKE-BLANKING-ARRAY ()
  (LET ((A (MAKE-ARRAY '(1 32.) :ELEMENT-TYPE 'BIT)))
    (DOTIMES (I 32.)
      (SETF (AREF A 0 I) 1))
    A))

(DEFVAR BLANKING-ARRAY)

(DEFUN CONSTRAINT-FRAME-WHITE-BLANKING (IGNORE X Y W H ARRAY)
  (DECLARE (:SELF-FLAVOR SHEET))
  ;; Zero out the contents of this pane.
  (prepare-sheet (self)
    (BITBLT ERASE-ALUF W H BLANKING-ARRAY 0 0 ARRAY X Y)))

(DEFUN CONSTRAINT-FRAME-BLACK-BLANKING (IGNORE X Y W H ARRAY)
  ;; Make this pane all 1's.
  (DECLARE (:SELF-FLAVOR SHEET))
  (prepare-sheet (self)
    (BITBLT CHAR-ALUF W H BLANKING-ARRAY 0 0 ARRAY X Y)))

(DEFUN CONSTRAINT-FRAME-STIPPLE-BLANKING (IGNORE X Y W H ARRAY GRAY-ARRAY)
  (DECLARE (:SELF-FLAVOR SHEET))
  (prepare-sheet (self)
    (BITBLT CHAR-ALUF W H
	    GRAY-ARRAY (REM X (ARRAY-DIMENSION GRAY-ARRAY 1))
	    (REM Y (ARRAY-DIMENSION GRAY-ARRAY 0))
	    ARRAY X Y)))

(DEFUN CONSTRAINT-FRAME-DRAW-A-BLANK (BLANK-TYPE NODE X Y W H SCREEN-ARRAY)
  "Draw blanking of type BLANK-TYPE for constraint node NODE.
We blank a rectangle of size W, H with upper left corner at X, Y.
The drawing is done in SCREEN-ARRAY.
BLANK-TYPE is either a function (which receives the rest of our args),
a list of a function and some additional args to give it,
or an array of stipple pattern to BITBLT from."
  (COND ((FUNCTIONP BLANK-TYPE)
	 ;; An explicit drawing function
	 (FUNCALL BLANK-TYPE NODE X Y W H SCREEN-ARRAY))
	((CONSP BLANK-TYPE)
	 (APPLY (CAR BLANK-TYPE) NODE X Y W H SCREEN-ARRAY (CDR BLANK-TYPE)))
	((ARRAYP BLANK-TYPE)
	 ;; Stipple array -- draw in standard way
	 (CONSTRAINT-FRAME-STIPPLE-BLANKING NODE X Y W H SCREEN-ARRAY BLANK-TYPE))
	(T (FERROR NIL "~S is an unknown type of blanking" BLANK-TYPE))))

;;; Constraint macros

(DEFUN (:PROPERTY FIXED-WITH-WHITESPACE CONSTRAINT-MACRO) (OLD-DESC STACKING)
  "A constraint-frame macro to take a window, and giving it the :FIXED
attribute leave whitespace around it on all four sides.  Format is:
    (name FIXED-WITH-WHITESPACE name-of-window color-of-border . constraint)
"
  ;; This macro generates the following form:
  ;;   ( :HORIZONTAL     constraint   (sym1 symn sym2)
  ;;     :VERTICAL
  ;;
  ;;   ((symn :VERTICAL    (:ASK-WINDOW name-of-window :PANE-SIZE)
  ;;          :HORIZONTAL
  ;;
  ;;    (sym3 name-of-window sym4)
  ;;
  ;;    ((name-of-window :FIXED))
  ;;
  ;;    (( sym3 :BLANK color-of-border :EVEN)
  ;;     ((sym4 :BLANK color-of-border :EVEN))))
  ;;
  ;;   (( sym1  :BLANK color-of-border :EVEN)
  ;;    ((sym2  :BLANK color-of-border :EVEN)))
  ;;
  (LET ((SYM1 (GENSYM))
        (SYM2 (GENSYM))
        (SYM3 (GENSYM))
        (SYM4 (GENSYM))
	(SYMN (GENSYM))
        (COLOR-OF-BORDER (FOURTH OLD-DESC)))
    `(,(CASE STACKING
             (:VERTICAL   :HORIZONTAL)
             (:HORIZONTAL :VERTICAL))
      ,(CDDDDR OLD-DESC)
      (,SYM1 ,SYMN ,SYM2)
      ((,SYMN ,STACKING (:ASK-WINDOW ,(THIRD OLD-DESC) :PANE-SIZE)
	(,SYM3 ,(THIRD OLD-DESC) ,SYM4)
	((,(THIRD OLD-DESC) :FIXED))
	((,SYM3 :BLANK ,COLOR-OF-BORDER :EVEN) (,SYM4 :BLANK ,COLOR-OF-BORDER :EVEN))))
      ((,SYM1 :BLANK ,COLOR-OF-BORDER :EVEN) (,SYM2 :BLANK ,COLOR-OF-BORDER :EVEN)))))

(DEFUN (:PROPERTY INTERDIGITATED-WHITESPACE CONSTRAINT-MACRO) (OLD-DESC STACKING)
  "Leave whitespace between all specified constraints (alternates stacking):
    (name INTERDIGITATED-WHITESPACE color :INCLUDE-or-:EXCLUDE
          our-constraint
          whitespace-constraint
          . <same as args to a stacking constraint>)

   :EXCLUDE means no whitespace before first and after last, :INCLUDE means include this
whitespace."
  (LET ((COLOR     (THIRD   OLD-DESC))
	(IOE       (FOURTH  OLD-DESC))
	(WSPACECON (SIXTH   OLD-DESC))
	(INFS      (SEVENTH OLD-DESC))
	(WSPACE)
        (NINFS))
    (OR (MEMBER IOE '(:INCLUDE :EXCLUDE) :TEST #'EQ)
	(FERROR NIL "~A must be either :INCLUDE or :EXCLUDE" IOE))
    (DO ((I INFS (CDR I))
	 (GS))
	(NIL)
      ;; Don't do it if IOE is :EXCLUDE or first one or last one
      (COND ((AND (EQ IOE :EXCLUDE)
		  (OR (EQ I INFS) (NULL I))))
	    (T
	     (SETQ GS (GENSYM))
	     (PUSH GS NINFS)
	     (PUSH `(,GS :BLANK ,COLOR . ,WSPACECON) WSPACE)))
      (AND I (PUSH (CAR I) NINFS))
      (AND (NULL I) (RETURN)))
    `(,(CASE STACKING
             (:VERTICAL   :HORIZONTAL)
             (:HORIZONTAL :VERTICAL))
      ,(FIFTH OLD-DESC)
      ,(NREVERSE NINFS)
      ,@(LET ((CONSTRS (COPY-LIST (NTHCDR 7 OLD-DESC))))
	   (DO ((CS CONSTRS (CDR CS))
		(SEEN-* NIL))
	       ((NULL CS)
                ;; Exit form for the DO.  If we hit a * constraint then we
                ;; add another whitespace constraint to the end too.
		(OR SEEN-* (RPLACD (LAST CONSTRS) (CONS WSPACE NIL))))
	     (COND ((MEMBER '* (CAR CS) :TEST #'EQ)
		    (SETQ SEEN-* T)
                    ;; replace * constraints with whitespace.
		    (RPLACA CS (APPEND (REMOVE '* (THE LIST (CAR CS)) :TEST #'EQ) WSPACE)))))
	   CONSTRS))))

;;; Support from other flavors

(DEFMETHOD (BASIC-MENU :PANE-SIZE) (REM-WIDTH REM-HEIGHT IGNORE IGNORE STACKING
					      &OPTIONAL N-ROWS N-COLUMNS)
  (MULTIPLE-VALUE-BIND (IGNORE IGNORE NEW-WIDTH NEW-HEIGHT)
      ;; menu-deduce-size is obsolete but it's usage is correct in this case
      ;; PMH 7/2/87
      (inhibit-style-warnings
      (MENU-DEDUCE-PARAMETERS N-COLUMNS N-ROWS
			      (IF (EQ STACKING :VERTICAL)
				  (- REM-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)
				  :UNCONSTRAINED)
			      (IF (EQ STACKING :HORIZONTAL)
				  (- REM-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
				  :UNCONSTRAINED)
			      (- REM-WIDTH  LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)
                              (- REM-HEIGHT TOP-MARGIN-SIZE  BOTTOM-MARGIN-SIZE)))
    (CASE STACKING
          (:VERTICAL   (+ NEW-HEIGHT TOP-MARGIN-SIZE  BOTTOM-MARGIN-SIZE))
          (:HORIZONTAL (+ NEW-WIDTH  LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))))

(DEFMETHOD (W:MENU :PANE-SIZE) (REM-WIDTH REM-HEIGHT IGNORE IGNORE STACKING
					      &OPTIONAL N-ROWS N-COLUMNS)
  (MULTIPLE-VALUE-BIND (IGNORE IGNORE NEW-WIDTH NEW-HEIGHT)
      (W:MENU-DEDUCE-PARAMETERS N-COLUMNS N-ROWS
				(IF (EQ STACKING :VERTICAL)
				    (- REM-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)
				    :UNCONSTRAINED)
				(IF (EQ STACKING :HORIZONTAL)
				    (- REM-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
				    :UNCONSTRAINED)
				(- REM-WIDTH  LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)
				(- REM-HEIGHT TOP-MARGIN-SIZE  BOTTOM-MARGIN-SIZE))
    (CASE STACKING
          (:VERTICAL   (+ NEW-HEIGHT TOP-MARGIN-SIZE  BOTTOM-MARGIN-SIZE))
          (:HORIZONTAL (+ NEW-WIDTH  LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))))

(DEFMETHOD (ESSENTIAL-WINDOW :PANE-SIZE) (REM-WIDTH REM-HEIGHT IGNORE IGNORE STACKING)
  (CASE STACKING
        (:VERTICAL   (MIN REM-HEIGHT HEIGHT))
        (:HORIZONTAL (MIN REM-WIDTH  WIDTH))))
