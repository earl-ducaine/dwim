;; -*- Mode:Common-lisp; Package: TV; Base:10.; Fonts: CPTFONT,HL12B,HL12BI -*-

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

;; Operations for moving, reshaping and creating windows,
;; and menus to get them from.

;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 01/27/89   KJF       [may] Change to system-menu-select-screen for Multiple Monitor (MMON) support.
;;; 04/27/88   PMH, KJF  Added SYSTEM-MENU-SELECT-SCREEN for selecting a screen from multiple screens.
;;;  2/15/88   LAS        Modifications made for system access menu
;;;  6/29/87   KWW      Added color defaults to window-resource definitions, added color to system menu name
;;;   7/2/87   PMH       Modified save-this-screen-layout to get rid of compiler warnings
;;;   5/07/87  TWE	Removed the `lisp' entry from the default-window-types-item-list defvar.  This is part
;;;			of the fix to only have one entry in the system menu `create' window.
;;;   3/04/87  KDB	Fixed  MOUSE-CONFIRM to work even if mindless max-width arg passed in. Closed spr 3776
;;;   1/21/87  TWE	Set up for a compiler optimization in ADJUST-BY-INTERVAL.
;;; 12/5/86    KDB        Fixed message wrap bug in Mouse-Confirm.
;;; 11/04/86   TWE	Fixed Plain Window in split screen to use WINDOW instead of nothing.  Added back
;;;			Lisp-listener there too.
;;; 10/30/86   TWE	Changed uses of REMAINDER and READLINE to REM and READ-LINE respectively.
;;; 10/28/86   GRH	Removed simple-lisp-listener from default create window types.
;;; 10/22/86   KDB	Commented out SELECTABLE-WINDOWS-MENU resource. No longer used.
;;; 10/22/86   KDB        Fixed "Existing Windows" expose bug SYSTEM-MENU-SPLIT-SCREEN-VIA-MENUS.
;;; 10/21/86   KDB        Reimplemented SYSTEM-MENU-SELECT-WINDOW. Optimized.
;;; 09/22/86   KDB        Modified confirm-window :after :handle-mouse to return nil when mouse moves off
;;;                          Mouse-confirm, per consistency guides.
;;; 09/22/86   KDB        Integrated new Mouse-confirm. Users now have explicit yes/no choices, per
;;;                          consistency guides.
;;; 09/22/86   KDB        Removed "flush" from Pop-up-message, per consistency guides.
;;; 09/22/86   KDB        Reimplemented Mouse-y-or-n-p to have explicit yes/no choices, per consistency guides.
;;; 09/19/86   KDB        Reversed  "abort" and "do it" menu item positions in split screen, per consistency guides.
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 07/16/86   TWE	Fixed up the system menu create window by fixing up the Lisp Listener version to use
;;;			the simple-lisp-listener if it exists.
;;; 07/09/86   TWE	Fixed up the system menu by commenting out the Zwei version and fixing up the Lisp
;;;			Listener version to use the simple-lisp-listener if it exists.
;;; 07/03/86   TWE	Removed an extraneous / from the "Change Layouts" menu item documentation.  This
;;;			should have been removed during the Common Lisp conversion.
;;; 04/25/86   DLS	Fixed MOUSE-CONFIRM and ADJUST-BY-INTERVAL to take already
;;;			existing carriage returns into account when formatting the text strings.
;;; 04/22/86   TWE	Fixed up the system menu add and remove functions to not do the
;;;			blank padding.  Fixed up the system-menu resource use the item
;;;			alignment keyword better.

(DEFSTRUCT (SYSTEM-MENU-ITEM (:TYPE :LIST) (:CONC-NAME SYSTEM-MENU-ITEM-)
  (:ALTERANT ALTER-SYSTEM-MENU-ITEM) (:PREDICATE NIL) (:COPIER NIL))
  "This structure defines the menu items in the system menu."
  NAME
  (KEYWORD :EVAL)
  FORM
  (DOCUMENTATION-KEYWORD :DOCUMENTATION);Not intended to be accessed
  (DOCUMENTATION "This item is not documented."))


(DEFSUBST COLUMN-TYPE-KEYWORD-TO-COLUMN-VARIABLE (COLUMN-TYPE-KEYWORD)
  "Converts the argument, which should be a keyword, to the variable whose value is
the menu item list for the specified column."
  (DECLARE (VALUES COLUMN-VARIABLE))
  (CASE COLUMN-TYPE-KEYWORD
    (:USER-AIDS '*SYSTEM-MENU-USER-AIDS-COLUMN*)
    (:PROGRAMS  '*SYSTEM-MENU-PROGRAMS-COLUMN*)
    (:WINDOWS   '*SYSTEM-MENU-EDIT-WINDOWS-COLUMN*)
    (:DEBUG     '*SYSTEM-MENU-DEBUG-TOOLS-COLUMN*)))


;; Item lists for the System Menu

(DEFVAR *SYSTEM-MENU-USER-AIDS-COLUMN* NIL
  "This is the list of columns that go in the USER AIDS column of the system
menu.  This list is added to by each utility that provides aids to the user.
Additions are made by the function ADD-TO-SYSTEM-MENU-COLUMN")



(DEFVAR *SYSTEM-MENU-PROGRAMS-COLUMN* NIL
  "List of columns that go in the Processes column of the system
menu.  List of the most commonly-needed programs. This list is added
to by processes that are optional software, using the
ADD-TO-SYSTEM-MENU-COLUMN function.")

(DEFVAR *SYSTEM-MENU-EDIT-WINDOWS-COLUMN*
        `(("Arrest" :WINDOW-OP (LAMBDA (WINDOW IGNORE IGNORE)
                                                 (AND WINDOW (SEND WINDOW :ARREST)))
           :DOCUMENTATION
           (:MOUSE-ANY
             "Arrest the process that can be found via the window the mouse is over."))
           ("Bury" :WINDOW-OP SYSTEM-MENU-BURY-WINDOW
            :DOCUMENTATION
            (:MOUSE-ANY
               "Bury the window that the mouse is over, beneath all other active windows."))
	   ("Change Layouts" :FUNCALL SYSTEM-MENU-LAYOUTS
            :DOCUMENTATION
            (:MOUSE-ANY "Save/restore current screen configuration."))
           ("Create" :FUNCALL SYSTEM-MENU-CREATE-WINDOW
            :DOCUMENTATION
            (:MOUSE-ANY "Create a new window of a Flavor to be selected from a menu."))
           ("Edit Attributes" :WINDOW-OP SYSTEM-MENU-EDIT-WINDOW-ATTRIBUTES
            :DOCUMENTATION
            (:MOUSE-ANY
	      "View or change the attributes of the window that the mouse is over."))
           ("Edit Screen"
            :BUTTONS
            ((NIL :EVAL (EDIT-SCREEN MOUSE-SHEET))	; Left   mouse click
             (NIL :EVAL (EDIT-SCREEN MOUSE-SHEET))	; Middle mouse click
             (NIL :WINDOW-OP
                  (LAMBDA (WINDOW IGNORE IGNORE)	; Right  mouse click
                    (SETQ WINDOW (SCREEN-EDITOR-FIND-SCREEN-TO-EDIT WINDOW))
                    (AND WINDOW (EDIT-SCREEN WINDOW)))))
            :DOCUMENTATION
            (:MOUSE-L-1 "Edits the screen that the mouse is on"	;:mouse-m-1 is duplicate of :mouse-l-1
	     :MOUSE-R-1 "Returns a menu of inferior frames to edit."
	     :DOCUMENTATION "Edit a screen"))
           ("Kill" :WINDOW-OP SYSTEM-MENU-KILL-WINDOW
            :DOCUMENTATION
	    (:MOUSE-ANY "Kill the window that the mouse is over."))
           ("Refresh" :WINDOW-OP (LAMBDA (WINDOW IGNORE IGNORE)
                                         (AND WINDOW (SEND WINDOW :REFRESH)))
            :DOCUMENTATION
            (:MOUSE-ANY "Refresh the window that the mouse is over."))
	   ("Reset" :WINDOW-OP SYSTEM-MENU-RESET-WINDOW
            :DOCUMENTATION
	    (:MOUSE-ANY
              "Reset the process associated with the window that the mouse is over."))
           ("Select" :FUNCALL SYSTEM-MENU-SELECT-WINDOW
            :DOCUMENTATION
	    (:MOUSE-ANY "Select a window from a menu."))
	   ("Split Screen"
	    :BUTTONS
	    ((NIL :FUNCALL SYSTEM-MENU-SPLIT-SCREEN-VIA-MENUS)	; Left   mouse click
	     (NIL :FUNCALL SYSTEM-MENU-SPLIT-SCREEN-VIA-MENUS)	; Middle mouse click
	     (NIL :EVAL (PROGN (SETQ *OVERLAP-SPLIT-SCREEN* T)	; right  mouse click
			       (SYSTEM-MENU-SPLIT-SCREEN-VIA-MENUS)
			       (SETQ *OVERLAP-SPLIT-SCREEN* NIL))))
	    :DOCUMENTATION (:MOUSE-L-1 "Create a split screen configuration."
			    :MOUSE-R-1 "Create an overlapping split screen configuration."))
#-EXPLORER ("Set Mouse Screen"
	    :BUTTONS ((NIL :EVAL (SYSTEM-MENU-SET-MOUSE-SCREEN NIL))
		      (NIL :EVAL (SYSTEM-MENU-SET-MOUSE-SCREEN NIL))
		      (NIL :EVAL (SYSTEM-MENU-SET-MOUSE-SCREEN T)))
	    :DOCUMENTATION
	    (:DOCUMENTATION "Set the screen that the mouse is on"
			    :MOUSE-L-1 ""
			    :MOUSE-M-1 "defaults if possible"
			    :MOUSE-R-1 "always uses a menu.")))
  "List of items that go in the Edit Windows column of the system
menu.  These are different window creation, selection, and editing
commands.  This list is added to by window tools that are optional
software, using the ADD-TO-SYSTEM-MENU-COLUMN function.")


(DEFVAR *SYSTEM-MENU-DEBUG-TOOLS-COLUMN* NIL
  "List of items that go in the Debug Tools window of the system
menu.  These are different tools for developers to use while debugging
their products.  This list is added to by debug tools that are optional
software, using the ADD-TO-SYSTEM-MENU-COLUMN function.")


;;;The following is made obsolete because a more general function is available (ADD-TO-SYSTEM-MENU-COLUMN)
(COMPILER:MAKE-OBSOLETE ADD-TO-SYSTEM-MENU-PROGRAMS-COLUMN
                        "use ADD-TO-SYSTEM-MENU-COLUMN")
(DEFUN ADD-TO-SYSTEM-MENU-PROGRAMS-COLUMN
       (NAME FORM DOCUMENTATION &OPTIONAL (AFTER :SORTED))
  "This function is now obsolete.  Use TV:ADD-TO-SYSTEM-MENU-COLUMN instead."
  (ADD-TO-SYSTEM-MENU-COLUMN :PROGRAMS NAME FORM DOCUMENTATION AFTER))


;;; This is a generalized version of ADD-TO-SYSTEM-MENU-PROGRAMS-COLUMN which
;;; allows one to specify which column they want the item to be added to.
(DEFUN ADD-TO-SYSTEM-MENU-COLUMN (COLUMN-TYPE NAME FORM DOCUMENTATION &OPTIONAL (AFTER :SORTED) (PRINT-NAME NIL))
  "Add an item named NAME to the specified column of the system
menu.  See also DELETE-FROM-SYSTEM-MENU-COLUMN.

COLUMN-TYPE is one of :USER-AIDS, :PROGRAMS, :WINDOWS, or :DEBUG, indicating
	which of the four columns should reflect this change.

NAME is a string which is to be displayed in the system menu.

FORM is what to execute if the user clicks on the item, and

DOCUMENTATION is the mouse documentation string.

AFTER can be one of four things:
	(1) a string which is the name of an item to add after,
	(2) AFTER=T means add at the top,
	(3) NIL means add at the bottom.
	(4) :SORTED means add the entry and sort the entire column."
  (ASSERT (NOT (NULL (COLUMN-TYPE-KEYWORD-TO-COLUMN-VARIABLE COLUMN-TYPE)))
	  (COLUMN-TYPE)
	  "The COLUMN-TYPE argument (~S) to ADD-TO-SYSTEM-MENU-COLUMN was an invalid column type."
	  COLUMN-TYPE)
  (SETF NAME (IF PRINT-NAME
		 (STRING-TRIM #\SPACE PRINT-NAME)
                 (STRING-TRIM #\SPACE NAME)))
  (LET* ((SYSTEM-MENU-COLUMN-SYMBOL (COLUMN-TYPE-KEYWORD-TO-COLUMN-VARIABLE COLUMN-TYPE))
	 (SYSTEM-MENU-COLUMN        (SYMBOL-VALUE SYSTEM-MENU-COLUMN-SYMBOL))
	 (NEW-ITEM                  NIL)
	 (ALREADY-THERE             (ASSOC NAME SYSTEM-MENU-COLUMN :TEST #'STRING-EQUAL)))
    ;; Check for new-style additions to system menu
    (if print-name
	(setf new-item `(,PRINT-NAME
			 :BUTTONS
			 ((NIL :EVAL (W:FIND-SYSTEM-INSTANCE ',NAME nil nil))
			  (NIL :EVAL (W:FIND-SYSTEM-INSTANCE ',NAME t nil))
			  NIL)
			 :DOCUMENTATION
			 (:MOUSE-L-1 "Toggle between various instances of this system"
				     :MOUSE-M-1 "Create a new instance of this system"
				     :DOCUMENTATION ,DOCUMENTATION)))
	(setf new-item (LIST NAME :EVAL FORM :DOCUMENTATION
	      (IF (CONSP DOCUMENTATION)
		  DOCUMENTATION
		  `(:MOUSE-ANY ,DOCUMENTATION)))))

    ;; Check to see if the item being added is already there.
    (IF ALREADY-THERE
	(PROGN
	  (SETF (CAR ALREADY-THERE) (CAR NEW-ITEM))
	  (SETF (CDR ALREADY-THERE) (CDR NEW-ITEM)))
	;;ELSE
	(COND ((MEMBER AFTER '(T :SORTED) :TEST #'EQ)
	       (PUSH NEW-ITEM SYSTEM-MENU-COLUMN))
	      ((EQ AFTER NIL)
	       (PUSH-END NEW-ITEM SYSTEM-MENU-COLUMN))
	      (T
	       (SETF SYSTEM-MENU-COLUMN
                     ;; Look through the list the find the one we are to insert after.  We
                     ;; will go through the system menu list creating a new list as we go.
		     (LOOP WITH AFTER = (OR (ASSOC AFTER SYSTEM-MENU-COLUMN :TEST #'STRING-EQUAL)
					    (CAR (LAST SYSTEM-MENU-COLUMN)))
			   FOR X IN SYSTEM-MENU-COLUMN
			   COLLECT X
			   WHEN (EQUAL X AFTER)
			   COLLECT NEW-ITEM)))))
    ;;Do the sort of the list if specified
    (WHEN (EQ AFTER :SORTED)
      (SETF SYSTEM-MENU-COLUMN (SORT SYSTEM-MENU-COLUMN #'STRING-LESSP :KEY #'CAR)))
    (SETF (SYMBOL-VALUE SYSTEM-MENU-COLUMN-SYMBOL) SYSTEM-MENU-COLUMN)))


;;; This function is similar to ADD-TO-SYSTEM-MENU-COLUMN in that it allows
;;; one to delete a specific item from a specified column in system menu.
(DEFUN DELETE-FROM-SYSTEM-MENU-COLUMN (COLUMN-TYPE NAME)
  "Delete an item named NAME from the specified column of the
system menu.  Alphabetic case is ignored.

COLUMN-TYPE is one of :USER-AIDS, :PROGRAMS :WINDOWS or :DEBUG indicating
	which of the four columns should reflect this change.

NAME is a string which is displayed in the system menu."
  (ASSERT (NOT (NULL (COLUMN-TYPE-KEYWORD-TO-COLUMN-VARIABLE COLUMN-TYPE)))
	  (COLUMN-TYPE)
	  "The COLUMN-TYPE argument (~S) to DELETE-FROM-SYSTEM-MENU-COLUMN was an invalid column type."
	  COLUMN-TYPE)
  (LET* ((SYSTEM-MENU-COLUMN-SYMBOL (COLUMN-TYPE-KEYWORD-TO-COLUMN-VARIABLE COLUMN-TYPE))
	 (SYSTEM-MENU-COLUMN        (SYMBOL-VALUE SYSTEM-MENU-COLUMN-SYMBOL))
	 (OLD-NAME                  (SYSTEM-MENU-ITEM-NAME (ASSOC NAME SYSTEM-MENU-COLUMN :TEST #'STRING-EQUAL))))
    ;; Check to see if the item there.
    (WHEN OLD-NAME
      (SETF SYSTEM-MENU-COLUMN (LOOP FOR ONE-SYSTEM-MENU-ITEM IN SYSTEM-MENU-COLUMN
				     WHEN (NOT (STRING-EQUAL (SYSTEM-MENU-ITEM-NAME ONE-SYSTEM-MENU-ITEM) OLD-NAME))
				     COLLECT ONE-SYSTEM-MENU-ITEM))
      (SETF (SYMBOL-VALUE SYSTEM-MENU-COLUMN-SYMBOL) SYSTEM-MENU-COLUMN))))

;;; Resource of system menus

(DEFWINDOW-RESOURCE SYSTEM-MENU ()
  :MAKE-WINDOW
  (W:MENU
    :MULTICOLUMN T
    :ITEM-ALIGNMENT :LEFT
    :COLUMN-SPEC-LIST `(("USER AIDS:" *SYSTEM-MENU-USER-AIDS-COLUMN*
			 :FONT FONTS:HL12B  :color ,tv:black)
			("PROGRAMS:" *SYSTEM-MENU-PROGRAMS-COLUMN*
			 :FONT FONTS:HL12B :color ,tv:black)
			("WINDOWS:" *SYSTEM-MENU-EDIT-WINDOWS-COLUMN*
			 :FONT FONTS:HL12B :color ,tv:black)
			("DEBUG TOOLS:" *SYSTEM-MENU-DEBUG-TOOLS-COLUMN*
			 :FONT FONTS:HL12B :color ,tv:black))
    :DYNAMIC T
    :POP-UP T
    :SCROLLING-P NIL
    :SAVE-BITS NIL				;for GRH by GSM on 5 Jan 86
    :FILL-P T
    :foreground-color *default-menu-foreground*
    :background-color *default-menu-background*
    :LABEL `(:CENTERED :FONT FONTS:METS
		       :STRING "Explorer System Menu"
	  	       :COLOR  ,*default-menu-label-foreground*
                       :background ,*default-menu-label-background*)
            )
  :INITIAL-COPIES 0
  :REUSABLE-WHEN :DEEXPOSED)


;;; HACK ALERT: Remove the following form when UCL is loaded.  It is OK to leave it
;;; here for a while.
(SETF (GET 'LISP-LISTENER 'SI:FLAVOR) (GET 'SIMPLE-LISP-LISTENER 'SI:FLAVOR))

(DEFVAR DEFAULT-WINDOW-TYPES-ITEM-LIST
        `(("Inspector" :VALUE INSPECT-FRAME ;; Inspect => Inspector 9/18/86
	   :DOCUMENTATION "Browse through data structure.")
	  ("Plain Window" :VALUE W:WINDOW
	   :DOCUMENTATION "An ordinary window suitable for input and output.")
	  ("Peek" :VALUE PEEK-FRAME
	   :DOCUMENTATION "Display status information.")
	  ("Any" :VALUE T :FONT :MENU-STANDOUT
	   :DOCUMENTATION "Prompts for any flavor name."))
  "Item list of the Create menu for creating inferiors of screens.")

;;; This variable is usually bound to something appropriate when using
;;; the menus that depend on it.  But it needs a global value so that
;;; the initial copy of the menu can get created.
(DEFVAR WINDOW-TYPES-ITEM-LIST DEFAULT-WINDOW-TYPES-ITEM-LIST
  "Item list for the Create menu to use right now.  When creating a
pane of a frame, this is bound to the pane types of that frame.  At top
level, it DEFAULT-WINDOW-TYPES-ITEM-LIST.")

(DEFUN ADD-WINDOW-TYPE (STRING FLAVOR DOCUMENTATION)
  "Add a new window type for the Create menu.
STRING appears in the menu,
FLAVOR is the flavor to create and
DOCUMENTATION is displayed in the mouse documentation window."
  (LET ((NEW-ITEM (LIST STRING :VALUE FLAVOR :DOCUMENTATION DOCUMENTATION)))
    (PUSH NEW-ITEM DEFAULT-WINDOW-TYPES-ITEM-LIST)
    (PUSH NEW-ITEM         WINDOW-TYPES-ITEM-LIST)))

;;; Resource of menus of flavors of windows user can create with mouse.
(DEFWINDOW-RESOURCE WINDOW-TYPE-MENU ()
  :MAKE-WINDOW (W:MENU
		 :ITEM-LIST-POINTER 'WINDOW-TYPES-ITEM-LIST
		 :DYNAMIC T
		 :POP-UP T
		 :SORT :ASCENDING
		 :ITEM-ALIGNMENT :LEFT
		 :SCROLLING-P NIL
		 :LABEL `(:CENTERED :FONT FONTS:MEDFNT :STRING ,(FORMAT NIL "Window~%Flavors:")
				    :background ,*default-menu-label-background*
				    :color      ,*default-menu-label-foreground*)
		 :foreground-color *default-menu-foreground*
		 :background-color *default-menu-background*
		 :SAVE-BITS T)
  :REUSABLE-WHEN :DEEXPOSED
  :INITIAL-COPIES 0)

(DEFMETHOD (SHEET :PANE-TYPES-ALIST) ()
  (SEND SUPERIOR :PANE-TYPES-ALIST))

(DEFMETHOD (SCREEN :PANE-TYPES-ALIST) ()
  DEFAULT-WINDOW-TYPES-ITEM-LIST)

(DEFUN SELECTABLE-WINDOWS (SUP)
  "Get a list of all windows that should appear in the Select menu."
  (SEND SUP :SELECTABLE-WINDOWS))

;;; ********* Attention ! *********
;;; This function is redefined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;
(DEFUN SYSTEM-MENU-SELECT-WINDOW (&OPTIONAL (SUP MOUSE-SHEET) &aux s-able-windows)
  "Offer the user the Select menu and select a window if he says so.
The menu contains the inferiors of SUP."
  (IF (setq s-able-windows (SELECTABLE-WINDOWS SUP))
	  (let ((WINDOW (w:menu-choose s-able-windows
					    :foreground-color *default-menu-foreground*
					    :background-color *default-menu-background*
					    :LABEL `(:string "Select a Window:"
					       :color ,*default-menu-label-foreground*
					       :background ,*default-menu-label-background*
						    )
					    :POP-UP T
					    )))
		 (AND WINDOW (MOUSE-SELECT WINDOW)))
    ;; The silly user killed off all of the windows, and is now trying
    ;; to select one.
      (PROGN
	(BEEP)
	(POP-UP-MESSAGE "Error: There are no windows that can be selected."))))

;;; old version. keep around for a while just in case.
;(DEFUN SYSTEM-MENU-SELECT-WINDOW (&OPTIONAL (SUP MOUSE-SHEET))
;  "Offer the user the Select menu and select a window if he says so.
;The menu contains the inferiors of SUP."
;  (IF (SELECTABLE-WINDOWS SUP)
;      (LET (( MENU (ALLOCATE-RESOURCE 'SELECTABLE-WINDOWS-MENU SUP)))
;	(COND ((NULL (SEND MENU :ITEM-LIST-POINTER))
;	       (BEEP)
;	       (POP-UP-MESSAGE
;                 "Error: There are no windows that can be selected."))
;	      (T
;	       (LET ((WINDOW (SEND MENU :CHOOSE)))
;		 (AND WINDOW (MOUSE-SELECT WINDOW))))))
;    ;; The silly user killed off all of the windows, and is now trying
;    ;; to select one.
;    (PROGN
;      (BEEP)
;      (POP-UP-MESSAGE "Error: There are no windows that can be selected."))))

(DEFUN system-menu-select-screen ;; may 01/27/89
       ()
  "Offer the user a menu of screens.  Selecting a screen will expose it and make it the DEFAULT-SCREEN."
  (LET ((screen (w:menu-choose (LOOP FOR screen IN all-the-screens
				     WHEN (TYPEP screen 'standard-screen)
				     COLLECTING screen)
			       :foreground-color *default-menu-foreground*
			       :background-color *default-menu-background*
			       :label `(:string "Select a Screen:"
						:color ,*default-menu-label-foreground*
						:background ,*default-menu-label-background*)
			       :pop-up t)))
    (AND screen
	 (PROGN
	   (SEND screen :expose)
	   ;; Make it DEFAULT-SCREEN in addition to exposing it.  It may already be exposed.  CJJ 06/15/88.
	   ;; Added by KJF on 08/20/88 for CJJ during addition of Multiple Monitor (MMON) support.
	   (SEND screen :send-if-handles :make-default-screen)))))


(DEFUN SYSTEM-MENU-CREATE-WINDOW (&OPTIONAL (SUP MOUSE-SHEET) (EDGES-FROM 'MOUSE))
  "Offer the user the Create menu and let him create an inferior of
SUP.  EDGES-FROM is passed to EXPOSE-WINDOW-NEAR; :MOUSE
means ask the user to specify the corners.  Returns a window or NIL."
  (LET* ((WINDOW-TYPES-ITEM-LIST (SEND SUP :PANE-TYPES-ALIST))
	 (WINDOW-TYPE
	   (COND
	     ((NULL WINDOW-TYPES-ITEM-LIST)
	      (BEEP)
	      NIL)
	     ((NULL (CDR WINDOW-TYPES-ITEM-LIST))
	      ;; There is no second item, return the first item.
	      (W:MENU-EXECUTE-NO-SIDE-EFFECTS (CAR WINDOW-TYPES-ITEM-LIST)))
	     (T (SEND (ALLOCATE-RESOURCE 'WINDOW-TYPE-MENU) :CHOOSE))))
	 (MS MOUSE-SHEET))
    (AND WINDOW-TYPE
	 (CONSP WINDOW-TYPE)
	 (SETQ WINDOW-TYPE (EVAL WINDOW-TYPE)))
    (UNWIND-PROTECT
	(PROGN
	  (AND (EQ WINDOW-TYPE T)		;"Any"
	       (SETQ WINDOW-TYPE
		     (GET-WINDOW-TYPE-FROM-KEYBOARD MOUSE-SHEET :EDGES-FROM)))
	  (COND
	    (WINDOW-TYPE
	     (MOUSE-SET-SHEET SUP)
	     (CREATE-WINDOW-WITH-MOUSE WINDOW-TYPE EDGES-FROM))))
      (MOUSE-SET-SHEET MS))))

(DEFUN CREATE-WINDOW-WITH-MOUSE
       (FLAVOR-NAME &OPTIONAL (EDGES-FROM 'MOUSE) &AUX TEM)
  "Create and return a window of FLAVOR-NAME, using mouse to get
edges.  EDGES is how to get the edges, and :MOUSE or :EXPAND will
use the mouse to get them."
  (AND FLAVOR-NAME
       ;; Get the edges before creating the window so can abort.
     (CAR
      (SETQ TEM
	    (CASE EDGES-FROM
	      (MOUSE
	       (LET* ((INIT-PLIST (SI:FLAVOR-DEFAULT-INIT-PLIST FLAVOR-NAME))
		      (MINIMUM-WIDTH (OR (GET INIT-PLIST :MINIMUM-WIDTH) 0))
		      (MINIMUM-HEIGHT (OR (GET INIT-PLIST :MINIMUM-HEIGHT) 0)))
		 (MULTIPLE-VALUE-LIST
		  (MOUSE-SPECIFY-RECTANGLE NIL NIL NIL NIL MOUSE-SHEET
					   MINIMUM-WIDTH MINIMUM-HEIGHT
					   T))))
	      (EXPAND
	       (MULTIPLE-VALUE-LIST (MOUSE-SPECIFY-EXPAND MOUSE-SHEET))))))
     (LET ((WINDOW (MAKE-WINDOW FLAVOR-NAME
				:SUPERIOR MOUSE-SHEET :EDGES TEM
				:color-map (if (color-system-p default-screen)
					       (copy-color-map (or (and tv:mouse-sheet
									(sheet-color-map tv:mouse-sheet))
								   *default-color-map*))))))
       (SEND WINDOW :SELECT)
       WINDOW)))



;; old mouse-y-or-n-p
;;;(DEFUN MOUSE-Y-OR-N-P (MESSAGE)
;;;  "Ask for confirmation from the user with a click on a one-item menu.
;;;Returns selected item or NIL if nothing was selected (i.e. user moved
;;;the mouse out of the window)."
;;;  (W:MENU-CHOOSE
;;;    `((,MESSAGE :VALUE T :DOCUMENTATION
;;;       (:MOUSE-ANY
;;;         "Click on the window to confirm, move mouse away or press the ABORT key to cancel confirmation.")))
;;;    :LABEL "Confirm:" :DEFAULT-ITEM MESSAGE :SCROLLING-P NIL))

(DEFUN MOUSE-Y-OR-N-P (MESSAGE)
  "Ask for confirmation from the user with a click on a two-item menu.
Returns selected item or NIL if nothing was selected (i.e. user moved the mouse out of the window)."
  (W:MENU-CHOOSE
    '((:no :value nil :DOCUMENTATION
	  "Click mouse or press <END> to select NO.   Pressing <ABORT> or moving off menu will also select NO.")
      (:yes :VALUE T :DOCUMENTATION
	   "Click mouse or press <END> to select YES.    Pressing <ABORT> or moving off menu will also select NO."))
     :LABEL `(:string ,message
		      :color ,*default-menu-label-foreground*
		      :background ,*default-menu-label-background*
	     )
     :SCROLLING-P NIL
    :columns 2))

(DEFFLAVOR CONFIRMATION-WINDOW ()
	   (TEMPORARY-SHADOW-BORDERS-WINDOW-MIXIN
            HYSTERETIC-WINDOW-MIXIN
	    STREAM-MIXIN
	    BORDERS-MIXIN
	    SELECT-MIXIN
	    DELAY-NOTIFICATION-MIXIN
	    MINIMUM-WINDOW)
  :INITTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :BORDERS 3
		       :BORDER-MARGIN-WIDTH 7
		       :DEEXPOSED-TYPEOUT-ACTION :PERMIT
		       :HYSTERESIS 10.
		       :MORE-P    NIL
		       :BLINKER-P NIL
		       :SAVE-BITS T
		       :WIDTH 400.)
  (:DOCUMENTATION SPECIAL "Window used to make confirmations with the user."))

(DEFMETHOD (CONFIRMATION-WINDOW :AFTER :HANDLE-MOUSE) ()
  "When the mouse leaves the window, put <delete> in the io buffer."
  (IO-BUFFER-PUT IO-BUFFER nil)) ;; used to return end.


(DEFWINDOW-RESOURCE CONFIRMATION-WINDOW ()
  :MAKE-WINDOW (CONFIRMATION-WINDOW)
  :REUSABLE-WHEN :DEEXPOSED
  :INITIAL-COPIES 0)


(DEFUN ADJUST-BY-INTERVAL (MAXIMUM-NUMBER-OF-CHARACTERS STRING)
  "This function takes an input STRING and a number which is the
maximum number of characters that should be in line.  The returned
result is STRING broken up, if necessary, so that there are at most
MAXIMUM-NUMBER-OF-CHARACTERS in any single line.  The breaks are
made at word boundaries if possible."
  (DECLARE (SIMPLE-STRING STRING))
  (DECF maximum-number-of-characters)
  (LOOP WITH length	= (LENGTH string)
	WITH result	= (MAKE-ARRAY length
				      :element-type 'string-char :fill-pointer 0)
	WITH first-cut
	FOR start-ptr	= 0 THEN end-ptr
	FOR end-ptr	= (MIN (+ start-ptr maximum-number-of-characters 1)
			       length)
	UNTIL (>= end-ptr length)
	DO (COND ((SETQ first-cut (POSITION #\Return string
					    :start start-ptr :end end-ptr
					    :test #'CHAR=))
		  (SETQ end-ptr (1+ first-cut))
		  (STRING-NCONC result (SUBSEQ string start-ptr end-ptr)))
		 ((SETQ first-cut (POSITION #\Space string
					    :start start-ptr :end (1+ end-ptr)
					    :test #'CHAR= :from-end t))
		  (STRING-NCONC result (SUBSEQ string start-ptr first-cut) #\Return)
		  (SETQ end-ptr (1+ first-cut)))
		 (t (STRING-NCONC result (SUBSEQ string start-ptr end-ptr) #\Return)))
	FINALLY (RETURN (STRING-NCONC result (SUBSEQ string start-ptr)))))


(DEFUN COUNT-LINES (MESSAGE &AUX (LINE-COUNT 1) (LAST-MATCH 0))
  "Count the number of line of text that are in the MESSAGE."
  (LOOP
    (IF (SETQ LAST-MATCH
	      (POSITION #\NEWLINE (THE STRING (STRING MESSAGE)) :START LAST-MATCH :TEST #'CHAR-EQUAL))
	(PROGN
	  (SETQ LAST-MATCH (1+ LAST-MATCH))	; Skip over the RETURN character
	  (SETQ LINE-COUNT (1+ LINE-COUNT)))	; Have another line
	(RETURN NIL)))
  LINE-COUNT)


(DEFUN MOUSE-CONFIRM (MESSAGE &OPTIONAL (WHAT-TO-DO "Click mouse, or press <END> if YES.
Move off window, or press <n> if NO.")	;
					(MESSAGE-FONT FONTS:HL12b)	;
					(WHAT-TO-DO-FONT FONTS:HL12)
					(WINDOW-MAX-WIDTH 400.))
  "Ask for confirmation from the user with a click on a window.
MESSAGE		text displayed at the top of the window.
WHAT-TO-DO		text displayed at the bottom of the window.
MESSAGE-FONT		font that MESSAGE is displayed in.
WHAT-TO-DO-FONT	font that WHAT-TO-DO is displayed in.
WINDOW-MAX-WIDTH	maximum number of pixels for the window width or
			NIL if it should be calculated from the messages."
  (USING-RESOURCE (window confirmation-window mouse-sheet)
    (setf message-font
	  (send (tv:sheet-get-screen window) :parse-font-descriptor message-font)
	  what-to-do-font
	  (send (tv:sheet-get-screen window) :parse-font-descriptor what-to-do-font))
    (SEND window :clear-input)
    (SEND window :set-font-map (LIST message-font what-to-do-font))
    ;; If the caller does not specify width (OR ONE TOO SMALL) then calculate how wide the window should
    ;; be from the messages that are provided.
    (UNLESS (< 30 window-max-width)
      (SETQ window-max-width (MAX (MULTIPLE-VALUE-BIND (ignore ignore max-mess-x)
				      (SEND window :string-length message
					    0 nil nil message-font)
				    max-mess-x)
				  (MULTIPLE-VALUE-BIND (ignore ignore  max-what-to-do-x)
				      (SEND window :string-length what-to-do
					    0 nil nil what-to-do-font)
				    max-what-to-do-x))))
    (SETQ window-max-width (MIN window-max-width
				(- (sheet-inside-width (sheet-superior window))
				   (sheet-left-margin-size window)
				   (sheet-right-margin-size window))))
    ;; Break up the two strings to that they will fit into the window.
    ;; ***** This calculation assumes a fixed width font, which is not always the	*****
    ;; ***** case lets fix this sometime and then we can take out the		*****
    ;; ***** recalculation of window-max-width.  dls				*****
    (SETQ MESSAGE    (ADJUST-BY-INTERVAL
		      (TRUNCATE window-max-width (FONT-CHAR-WIDTH message-font))
		      message)
	  WHAT-TO-DO (ADJUST-BY-INTERVAL
		      (TRUNCATE window-max-width (FONT-CHAR-WIDTH what-to-do-font))
		      what-to-do))
    (SETQ window-max-width (MIN (MAX (MULTIPLE-VALUE-BIND (nil nil max-x)
					 (SEND window :string-length
					       message 0 nil nil message-font)
				       max-x)
				     (MULTIPLE-VALUE-BIND (nil nil max-x)
					 (SEND window :string-length
					       what-to-do 0 nil nil what-to-do-font)
				       max-x))
				(- (sheet-inside-width (sheet-superior window))
				   (sheet-left-margin-size window)
				   (sheet-right-margin-size window))))
    ;; The next calculation is somewhat complex.  Overall, we are making the window
    ;; large enough to hold both strings.  The first argument to :SET-INSIDE-SIZE is the
    ;; width of the window.  This was calulated earlier.  The second argument to
    ;; :SET-INSIDE-SIZE is the height of the window.  To get this, we first calculate
    ;; the number of lines which are being displayed and add one for the blank line in
    ;; the middle and multiply this sum times the line height for the window.
    (SEND window :set-inside-size
	  window-max-width (* (+ (COUNT-LINES message) (COUNT-LINES what-to-do) 1)
			      (sheet-line-height window)))
    (SEND window :Set-cursorpos 0 0)
    (SEND window :clear-screen)
    (SEND window :expose-near '(:mouse) nil)
    (window-call (window :deactivate)
      (SEND window :set-current-font message-font)
      (SEND window :line-out message)
      (SEND window :line-out "")
      (SEND window :set-current-font what-to-do-font)
      (SEND window :string-out what-to-do)
	    (LOOP FOR input = (read-any window)
		  DO (COND ((AND (CONSP input)
				 (EQ :mouse-button (FIRST input)))	;

			    (return t)) ;; user clicked on window
			   ((eql input nil) ;; confirmation window :after :handle mouse now stuffs NIL instead of end when user

			    (return nil))    ;; moves mouse off window!
			   ((or (EQL input #\End) (eql input #\space))

			    (return t ))
			   ((or (EQL INPUT #\n) (eql input #\N))
			    (return nil)))
		  ))))


(DEFUN SYSTEM-MENU-KILL-WINDOW (WINDOW IGNORE IGNORE)
  (AND WINDOW
       (MOUSE-Y-OR-N-P
	 (FORMAT NIL "Kill ~A"
		 (SHEET-NAME
		   (SETQ WINDOW (OR (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS)
				    WINDOW)))))
       (SEND WINDOW :KILL)))

(DEFUN SYSTEM-MENU-RESET-WINDOW (WINDOW IGNORE IGNORE &AUX P)
  (AND WINDOW
       (MOUSE-Y-OR-N-P (FORMAT NIL "Reset process in ~A" (SHEET-NAME WINDOW)))
       (SETQ P (SEND WINDOW :SEND-IF-HANDLES :PROCESS))
       (SEND P :RESET)))

(DEFUN SYSTEM-MENU-BURY-WINDOW (WINDOW IGNORE IGNORE)
  (AND WINDOW
       (SEND (OR (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS)
                       WINDOW)
                   :BURY)))

(DEFUN SELECT-OR-CREATE-WINDOW-OF-FLAVOR
        (FIND-FLAVOR &OPTIONAL (CREATE-FLAVOR FIND-FLAVOR))
  (SEND (OR (FIND-WINDOW-OF-FLAVOR FIND-FLAVOR)
	    (MAKE-WINDOW CREATE-FLAVOR :color-map
			 (if (color-system-p default-screen)
			     (copy-color-map (or (and tv:mouse-sheet
						      (sheet-color-map tv:mouse-sheet))
						 *default-color-map*)))))
	:MOUSE-SELECT))

(DEFUN SYSTEM-MENU-EDIT-WINDOW-ATTRIBUTES (WINDOW IGNORE IGNORE)
  (AND WINDOW
       (SCREEN-EDITOR-EDIT-ATTRIBUTES WINDOW)))

;;; Stuff for setting up a screen layout.
;;; Suggested improvements:
;;;  Find out why it thrashes the disk for several seconds before coming up,
;;;   after displaying all the windows.
;;;  Provide the ability to edit saved screen layouts.
;;;  Provide the ability to edit the SPLIT-SCREEN-LAYOUT-WINDOW with the mouse
;;;  Figure out why the choose-variable-values window sometimes fails to
;;;   appear and also why it sometimes fails to use a frame when I clearly told it to.

(DEFVAR SPLIT-SCREEN-ITEM-LIST
	'(("Existing Lisp" :VALUE "Existing Lisp"
	   :DOCUMENTATION
             "An already existing LISP Listener.")
	  ("Existing Window" :VALUE "Existing Window"
	   :DOCUMENTATION
             "An already existing window chosen from a menu.")
	  ("Plain Window" :VALUE "Plain Window"
	   :DOCUMENTATION
             "A window with no special attributes, suitable for simple output.")
	  ("Trace & Error" :VALUE "Trace & Error"
	   :DOCUMENTATION
             "Where trace and error is directed.")
	  ("Trace" :VALUE "Trace"
             :DOCUMENTATION
             "Where trace output is directed.")
	  ("Error" :VALUE "Error"
             :DOCUMENTATION
             "Where the error handler's debugger will run.")
	  ("" :NO-SELECT T) ("" :NO-SELECT T)
	  ("Frame" :VALUE "Frame"
             :DOCUMENTATION
             "Put chosen windows together in a frame.")
	  ("Mouse Corners" :VALUE "Mouse Corners"
	   :DOCUMENTATION
             "Specify the area to fill from the mouse.")
	  ("" :NO-SELECT T)
	  ("Undo" :VALUE "Undo"
             :DOCUMENTATION
             "Undo last selection.")
	  ("Abort" :VALUE "Abort" :FONT :MENU-STANDOUT
             :DOCUMENTATION
             "Abort Split Screen and Exit.")
	  ("Do It" :VALUE "Do It" :FONT :MENU-STANDOUT
             :DOCUMENTATION
             "Complete selection and Exit.")
	  ))

(DEFUN SPLIT-SCREEN-ITEM-LIST ()
  (APPEND WINDOW-TYPES-ITEM-LIST
	  (IF (ODDP (LENGTH WINDOW-TYPES-ITEM-LIST))
	      '(("" :NO-SELECT T)))
	  SPLIT-SCREEN-ITEM-LIST))

(DEFWINDOW-RESOURCE SPLIT-SCREEN-MENU ()
  :MAKE-WINDOW
  (W:MENU
    :NAME "Split Screen Menu"
    :LABEL `(:string "Split screen element:"
		     :color ,*default-menu-label-foreground*
		     :background ,*default-menu-label-background*
	    )
    :background-color *default-menu-background*
    :foreground-color *default-menu-foreground*
    :COLUMNS 2
    :DYNAMIC T
    :PERMANENT NIL
    :ABORT-ON-DEEXPOSE T
    :COMMAND-MENU T
    :SCROLLING-P NIL
    :SAVE-BITS :DELAYED
    :IO-BUFFER (MAKE-IO-BUFFER 8.)
    :ITEM-LIST-POINTER '(SPLIT-SCREEN-ITEM-LIST))
  :REUSABLE-WHEN :DEEXPOSED
  :INITIAL-COPIES 0)

(DEFWINDOW-RESOURCE
  SPLIT-SCREEN-CHOOSE-VALUES ()
  :WINDOW-CREATE
  (TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW
    :NAME "Split Screen Choose Values"
    :LABEL `(:string "Frame characteristics:"
		     :color ,*default-menu-label-foreground*
		     :background ,*default-menu-label-background*
            )
    :foreground-color *default-menu-foreground*
    :background-color *default-menu-background*
    :CHARACTER-WIDTH 40.
    :IO-BUFFER NIL
    :MARGIN-CHOICES
    (LIST (LIST
            "Cancel the Frame" NIL
            'SPLIT-SCREEN-PUNT-FRAME NIL NIL
            :documentation
            '(:documentation
               "Abort the frame.  Mouse the DO IT item to create the frame.")))
    :VARIABLES '((*FRAME-NAME* "Name of frame" :STRING)
                 (*SYSTEM-KEY* "[SYSTEM] <char> selects it"
                               :CHARACTER-OR-NIL)))
  :REUSABLE-WHEN :DEEXPOSED
  ;; Due to order of loading of files.
  :INITIAL-COPIES 0)

(DEFUN SPLIT-SCREEN-PUNT-FRAME (&REST IGNORE)
  (SEND SELF :FORCE-KBD-INPUT '(PUNT-FRAME)))

;(LOCAL-DECLARE ((SPECIAL *FRAME-NAME* *SYSTEM-KEY*)) ;commented this line out per GRH's instructions -- GSM 25 Nov 85

;;replaced the following function per GRH's instructions -- GSM 25 Nov 85
;;;patched on 19 Dec 85 for TWE by GSM

(DEFUN SYSTEM-MENU-SPLIT-SCREEN-VIA-MENUS (&OPTIONAL (SUP MOUSE-SHEET))
  (USING-RESOURCE (SCVM-MENU SPLIT-SCREEN-MENU SUP)
    (USING-RESOURCE (LAYWIN SPLIT-SCREEN-LAYOUT-WINDOW SUP)
      (LET* ((WINDOW-TYPES-ITEM-LIST (SEND SUP :PANE-TYPES-ALIST))
	     (EDGES (LIST (SHEET-INSIDE-LEFT   SUP)
			  (SHEET-INSIDE-TOP    SUP)
			  (SHEET-INSIDE-RIGHT  SUP)
			  (SHEET-INSIDE-BOTTOM SUP)))
	     ;; Interaction windows are the small version of the
	     ;; selected windows that appear on the screen for the user
	     ;; to see what is being built.
	     (INTERACTION-WINDOWS NIL)
	     (CVVW NIL)
	     (USE-FRAME NIL)
	     (*FRAME-NAME* "Split-screen frame")
	     (*SYSTEM-KEY* NIL)
	     (IO-BUFFER)
	     (ITEM))
	(DECLARE (SPECIAL *FRAME-NAME* *SYSTEM-KEY*))
	(SEND LAYWIN :CLEAR-FROBS)
	(SETQ IO-BUFFER (SEND SCVM-MENU :IO-BUFFER))
	(IO-BUFFER-CLEAR IO-BUFFER)
	(EXPOSE-WINDOW-NEAR SCVM-MENU '(:MOUSE))
        (SEND SCVM-MENU :SELECT)
	(PUSH SCVM-MENU INTERACTION-WINDOWS)
	(UNWIND-PROTECT
	    (DO ((WINDOW-TYPE-LIST NIL) (N-WINDOWS 0) (RES))
	      (NIL)
	      (COND
		((AND (PLUSP N-WINDOWS)
		      (NOT (MEMBER LAYWIN INTERACTION-WINDOWS :TEST #'EQ)))
		 (AND CVVW (SEND CVVW :DEEXPOSE))
		 (SEND LAYWIN :MOVE-NEAR-WINDOW SCVM-MENU
		       (CONS (- (THIRD EDGES) (FIRST EDGES))
			     (- (FOURTH EDGES) (SECOND EDGES))))
		 (PUSH LAYWIN INTERACTION-WINDOWS)
		 (AND CVVW
		      (EXPOSE-WINDOW-NEAR CVVW
					  (CONS :WINDOW
						(REMOVE CVVW
							(THE LIST INTERACTION-WINDOWS)
							:TEST #'EQ))))))
	      (PROCESS-WAIT "Choose" #'(LAMBDA (B WINDOW)
                                         (OR (LISTEN WINDOW)
                                             (NOT (IO-BUFFER-EMPTY-P B))))
			    IO-BUFFER SCVM-MENU)
              ;; The following SETQ is to allow one to enter keystrokes to
              ;; move the mouse cursor around in the split screen menu.
              (SETQ RES (IF (IO-BUFFER-EMPTY-P IO-BUFFER)
                            ;; Try to determine which window got the character.
                            (LIST :MENU
                                  (SEND SCVM-MENU :PROCESS-CHARACTER (READ-CHAR SCVM-MENU))
                                  NIL SCVM-MENU)
                            ;;ELSE
                            (PROGN
                              (SETQ RES (IO-BUFFER-GET IO-BUFFER)))))
              (COND
                ((AND (EQ (FIRST RES) :MENU)
                      (EQ (FOURTH RES) SCVM-MENU))
                 (SETQ RES (SEND SCVM-MENU :EXECUTE (SETQ ITEM (SECOND RES))))
                 (AND (EQ RES T)                ;"Any"
                      (SETQ RES
                            (GET-WINDOW-TYPE-FROM-KEYBOARD SUP :EDGES-FROM
                                                           (CONS :WINDOW
                                                                 INTERACTION-WINDOWS))))
                 (COND
                   ((NULL RES))                 ;Maybe failed getting type from keyboard
                   ((EQUALP RES "Abort")
                    (RETURN NIL))
                   ((EQUALP RES "Mouse Corners")
                    (COND
                      ((CAR
                         (SETQ RES
                               (MULTIPLE-VALUE-LIST
                                 (MOUSE-SPECIFY-RECTANGLE NIL NIL NIL NIL SUP 0 0 T))))
                       (SETQ EDGES RES)
                       ;; Next line causes shape of LAYWIN to
                       ;; be recomputed.
                       (SETQ INTERACTION-WINDOWS
                             (DELETE LAYWIN (THE LIST INTERACTION-WINDOWS) :TEST
                                     #'EQ)))))
                   ((EQUALP RES "Undo")
                    (COND
                      ((PLUSP N-WINDOWS)
                       (SETQ N-WINDOWS (1- N-WINDOWS)
                             WINDOW-TYPE-LIST (CDR WINDOW-TYPE-LIST))
                       (SEND LAYWIN :REMOVE-LAST-FROB))))
                   ((EQUALP RES "Frame")
                    (SETQ USE-FRAME T)
                    (COND
                      ((NULL CVVW)
                       (SETQ CVVW
                             (ALLOCATE-RESOURCE 'SPLIT-SCREEN-CHOOSE-VALUES SUP))
                       (SEND CVVW :SET-IO-BUFFER IO-BUFFER)
                       (SEND CVVW :SET-STACK-GROUP %CURRENT-STACK-GROUP)
                       (EXPOSE-WINDOW-NEAR CVVW (CONS :WINDOW INTERACTION-WINDOWS))
                       (PUSH CVVW INTERACTION-WINDOWS)
                       (SEND CVVW :SELECT))))
                   ((EQUALP RES "Existing Lisp")        ;added Existing Lisp to this function instead of
                                                ;split-screen-via-menus-setup-window -- GSM  5 Jan 86
                    (LET ((W (IDLE-LISP-LISTENER SUP T)))
                      (SEND LAYWIN :ADD-FROB (SEND W :NAME-FOR-SELECTION))
                      (PUSH W WINDOW-TYPE-LIST)
                      (SETQ N-WINDOWS (1+ N-WINDOWS))))
                   ((EQUALP RES "Existing Window")
          ;; removed code that relied on menu resource to build up list of windows. Now use W:menu-choose.
                      (LOOP FOR W = (w:menu-choose (MAPCAN #'SELECTABLE-WINDOWS ALL-THE-SCREENS)
						   :near-mode (CONS :WINDOW INTERACTION-WINDOWS)
						   :background-color *default-menu-background*
						   :foreground-color *default-menu-foreground*
						   :LABEL `(:string "Select a Window:"
						      :color ,*default-menu-label-foreground*
		                                      :background ,*default-menu-label-background*
						      )
						   :POP-UP T
						   )
                            THEN (SHEET-SUPERIOR W)
                            AND WW = NIL THEN W
                            WHILE (NOT (NULL W))
                            WHEN (EQ W SUP)
                            RETURN
                            (SEND LAYWIN :ADD-FROB (SEND WW :NAME-FOR-SELECTION))
                            (PUSH WW WINDOW-TYPE-LIST)
                            (SETQ N-WINDOWS (1+ N-WINDOWS)))
                    (APPLY SCVM-MENU :SET-MOUSE-CURSORPOS
                           (MULTIPLE-VALUE-LIST
                             (SEND SCVM-MENU :ITEM-CURSORPOS ITEM))))
                   ((NOT (EQUALP RES "Do It")) (PUSH RES WINDOW-TYPE-LIST)
                                               (SEND LAYWIN :ADD-FROB
                                                     (OR (GET-STRING-FROM-WINDOW-TYPE RES) (STRING RES)))
                                               (SETQ N-WINDOWS (1+ N-WINDOWS)))
                   ((ZEROP N-WINDOWS)
                    (BEEP))                     ;Do It with nothing to do
                   (T
                    (DELAYING-SCREEN-MANAGEMENT
                      ;; Done with these now.
                      (DOLIST (W INTERACTION-WINDOWS)
                        (SEND W :DEACTIVATE))
                      (IF (NOT USE-FRAME)
                          (SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW SUP EDGES
                                                               WINDOW-TYPE-LIST
                                                               N-WINDOWS LAYWIN)
                          ;; SPLIT-SCREEN-FRAME isn't
                          ;; necessarily the right flavor.  Maybe
                          ;; ask user whether it should be a
                          ;; constraint-frame.  Maybe put borders
                          ;; around it, but need a way for them to
                          ;; appear when partially exposed even
                          ;; though it doesn't have a bit-save
                          ;; array.
                          (LET ((FRAME
                                  (MAKE-WINDOW 'SPLIT-SCREEN-FRAME :SUPERIOR SUP
                                               :EDGES-FROM EDGES :NAME *FRAME-NAME*
                                               :EXPOSE-P T
					       :color-map (if (color-system-p default-screen)
							      (copy-color-map (or (and tv:mouse-sheet
										       (sheet-color-map tv:mouse-sheet))
										  *default-color-map*))))))
                            (AND *SYSTEM-KEY*
                                 (SETQ *SYSTEM-KEY* (CHAR-UPCASE *SYSTEM-KEY*)
                                       *SYSTEM-KEYS*
                                       (CONS (LIST *SYSTEM-KEY* FRAME *FRAME-NAME* NIL)
                                             (DELETE
                                               (ASSOC *SYSTEM-KEY* *SYSTEM-KEYS* :TEST #'EQ)
                                               (THE LIST *SYSTEM-KEYS*) :TEST #'EQ))))
                            (LET ((SEL (SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW
                                         FRAME (LIST (SHEET-INSIDE-LEFT   FRAME)
                                                     (SHEET-INSIDE-TOP    FRAME)
                                                     (SHEET-INSIDE-RIGHT  FRAME)
                                                     (SHEET-INSIDE-BOTTOM FRAME))
                                         WINDOW-TYPE-LIST
                                         N-WINDOWS
                                         LAYWIN)))
                              (AND
                                (MEMBER SEL (SHEET-EXPOSED-INFERIORS FRAME) :TEST #'EQ)
                                (SEND FRAME :SELECT-PANE SEL))))))
                    ;; Return from the Do just after the
                    ;; UNWIND-PROTECT located about 123
                    ;; (decimal) lines up.
                    (RETURN))))
                ((EQ (FIRST RES) :VARIABLE-CHOICE)
                 (APPLY #'CHOOSE-VARIABLE-VALUES-CHOICE (CDR RES)))
                ((AND (EQ (FIRST RES) 'PUNT-FRAME) USE-FRAME)
                 (SEND CVVW :DEACTIVATE)
                 (SETQ INTERACTION-WINDOWS
                       (DELETE CVVW (THE LIST INTERACTION-WINDOWS) :TEST #'EQ))
                 (SETQ USE-FRAME NIL
                       CVVW NIL))
                ;;(T (FERROR NIL "Garbage from i//o buffer: ~S" RES))  ;ignore anything else (TWE)
                ))
	  (DELAYING-SCREEN-MANAGEMENT (DOLIST (W INTERACTION-WINDOWS)	;Done with these now
					(SEND W :DEACTIVATE))))))))

(DEFUN GET-STRING-FROM-WINDOW-TYPE (TYPE &AUX TEM)
  "Return the string that was displayed to the user when he chose
type TYPE, or NIL if we can't figure it out for some reason."
  (DOLIST (ITEM WINDOW-TYPES-ITEM-LIST)
    (COND
      ((SETQ TEM (GET ITEM :VALUE))
       (IF (EQ TEM TYPE)
	   (RETURN (CAR ITEM))))
      ((SETQ TEM (GET ITEM :EVAL))
       (IF (EQ (EVAL TEM)
	       TYPE)
	   (RETURN (CAR ITEM)))))))

;;; We now have the list of windows, lay out the screen and set them up.
;;; The general rule for screen layout is that 2 or 3 windows stack
;;; vertically, 4 are in a square, 5 are a square with 1 below it, etc.
;;; To generalize, you have floor(n/2) rows in 2 columns and 1 below if
;;; n is odd.  This returns the window it selects, or NIL.
(DEFUN SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW
       (SUP EDGES WINDOW-TYPE-LIST N-WINDOWS LAYWIN
	&AUX N-COLUMNS N-ROWS WIDTH HEIGHT TEM WINDOW SEL)
  LAYWIN					;ignored for now ;;; Why? --mdm 12/17/84
  (LET (WIDTH-REMAINING)
    (COND
      ((AND *LANDSCAPE-MONITOR* *OVERLAP-SPLIT-SCREEN*)
       (COND
	 ((= N-WINDOWS 1)
	  (SETQ N-ROWS 1
		N-COLUMNS 1))
	 ((= N-WINDOWS 2)
	  (SETQ N-ROWS 1
		N-COLUMNS 2))
	 (T (SETQ N-COLUMNS 2
		  N-ROWS (TRUNCATE (1+ N-WINDOWS) 2)))))
      (T
       (IF (< N-WINDOWS 4)
	   (SETQ N-COLUMNS 1
		 N-ROWS N-WINDOWS)
	   (SETQ N-COLUMNS 2
		 N-ROWS (TRUNCATE (1+ N-WINDOWS) 2)))))
    (SETQ WIDTH
	  (COND
	    ((AND *LANDSCAPE-MONITOR*
		  *OVERLAP-SPLIT-SCREEN*)
	     (* 2 (TRUNCATE (- (THIRD EDGES) (FIRST EDGES))
			    (1+ N-COLUMNS))))
	    (T (TRUNCATE (- (THIRD EDGES) (FIRST EDGES))
			 N-COLUMNS)))
	  HEIGHT (TRUNCATE (- (FOURTH EDGES) (SECOND EDGES))
			   N-ROWS)
	  WIDTH-REMAINING (- (- (THIRD EDGES) (FIRST EDGES))
			     WIDTH))
    (LOCK-SHEET (SUP)
      (DOLIST (WINDOW (SHEET-EXPOSED-INFERIORS SUP))
	(SEND WINDOW :DEEXPOSE))
      (DO ((L (NREVERSE WINDOW-TYPE-LIST) (CDR L))
	   (I 0 (1+ I))
	   (LEFT)
	   (RIGHT)
	   (TOP)
	   (BOTTOM))
	  ((NULL L))
	(SETQ LEFT   (+ (FIRST  EDGES) (* (REM I N-COLUMNS) WIDTH-REMAINING))
	      TOP    (+ (SECOND EDGES) (* (TRUNCATE  I N-COLUMNS) HEIGHT))
	      RIGHT  (+ LEFT WIDTH)
	      BOTTOM (+ TOP HEIGHT))
	;; The bottom-most window is wider if there are an odd number of
	;; them.
	(AND (NULL (CDR L)) (SETQ RIGHT (THIRD EDGES)))
	;;moved Existing Lisp to system-menu-split-screen-via-menus because of change in
	;;idle-lisp-listener function -- GSM  5 Jan 86
	(COND
	  ((SETQ TEM
		 (ASSOC (CAR L)
			'(("Trace" TRACE-OUTPUT)
			  ("Error" DEBUG-IO)
			  ("Trace & Error" TRACE-OUTPUT DEBUG-IO))
			:TEST 'EQUALP))
	   (SETQ WINDOW
		 (MAKE-WINDOW 'TRACE-OR-ERROR-WINDOW :STREAM-VARIABLES (CDR TEM) :SUPERIOR SUP
			      :NAME (AND (CDR TEM) (CAR TEM)) :LEFT LEFT :TOP TOP :RIGHT RIGHT
			      :BOTTOM BOTTOM)))
	  ((NOT (SYMBOLP (CAR L)))		;Window itself
	   (SETQ WINDOW (CAR L)) (SEND WINDOW :SET-SUPERIOR SUP)
	   (SEND WINDOW :SET-EDGES LEFT TOP RIGHT BOTTOM) (OR SEL (SETQ SEL WINDOW)))
	  (T
	   (SETQ WINDOW (MAKE-WINDOW (CAR L) :SUPERIOR SUP :LEFT LEFT
				     :TOP TOP :RIGHT RIGHT :BOTTOM BOTTOM))
	   (OR SEL (SETQ SEL WINDOW))))
	(SEND WINDOW :EXPOSE))
      (AND SEL (SEND SEL :SELECT)))
    SEL))

(DEFFLAVOR TRACE-OR-ERROR-WINDOW
	((STREAM-VARIABLES NIL)
	 (OLD-STREAM-VALUES NIL))
	(WINDOW)
  (:INITABLE-INSTANCE-VARIABLES STREAM-VARIABLES)
  (:DOCUMENTATION
    "Window which, when exposed, serves as the value of some stream
variables.  STREAM-VARIABLES is a list of stream variables which
should be set to this window when this window is exposed.  When the
window is deexposed, those variables are set back to their former
values."))

(DEFMETHOD (TRACE-OR-ERROR-WINDOW :AFTER :EXPOSE) (&REST IGNORE)
  (WHEN STREAM-VARIABLES
    (UNLESS OLD-STREAM-VALUES
      (SETQ OLD-STREAM-VALUES (MAPCAR 'EVAL STREAM-VARIABLES))
      (MAPC 'SET STREAM-VARIABLES (CIRCULAR-LIST SELF)))))

(DEFMETHOD (TRACE-OR-ERROR-WINDOW :BEFORE :DEEXPOSE) (&REST IGNORE)
  (WHEN OLD-STREAM-VALUES
    (MAPC 'SET STREAM-VARIABLES OLD-STREAM-VALUES)
    (SETQ OLD-STREAM-VALUES NIL)))

(DEFMETHOD (TRACE-OR-ERROR-WINDOW :BEFORE :TYI) (&REST IGNORE)
  (SEND SELF :SELECT))

(DEFMETHOD (TRACE-OR-ERROR-WINDOW :AFTER :TYI) (&REST IGNORE)
  (SEND SELF :DESELECT))

;;;replaced defvar on 10 Jan 86 for GRH by PDC
(DEFVAR screen-layout-menu-alist
	'(("Just Lisp" :EVAL (LET* ((sup (SEND self :SUPERIOR))
				    (ll  (IDLE-LISP-LISTENER sup)))
			       (LIST (LIST* ll (SEND ll :STATUS)
					    (MULTIPLE-VALUE-LIST (SEND sup :EDGES))))))
	  ("Save This" :EVAL (PROGN (SAVE-THIS-SCREEN-LAYOUT self) NIL))))

;;;patched on 19 Dec 85 for GRH by GSM
(DEFWINDOW-RESOURCE SCREEN-LAYOUT-MENU ()
  :MAKE-WINDOW
  (W:MENU
    :POP-UP T
    :SCROLLING-P NIL
    :NAME "Screen Layout Menu"
    :LABEL `(:string "Screen Layouts:"
		     :color ,*default-menu-label-foreground*
		     :background ,*default-menu-label-background*
		     )
    :foreground-color *default-menu-foreground*
    :background-color *default-menu-background*)
  :REUSABLE-WHEN :DEEXPOSED
  :INITIAL-COPIES 0)

;;; This grossly needs more error checking!!
;;; patched on 19 Dec 85 for GRH by GSM
(DEFUN SYSTEM-MENU-LAYOUTS (&OPTIONAL (SCREEN MOUSE-SHEET))
  (USING-RESOURCE (MENU SCREEN-LAYOUT-MENU SCREEN)
    (SEND menu :set-item-list screen-layout-menu-alist)	;GRH
    (LET ((X (SEND MENU :CHOOSE)))
      (RESELECT-SCREEN-LAYOUT X))))

(DEFUN RESELECT-SCREEN-LAYOUT (LAYOUT)
  "Reselect/expose the windows described in LAYOUT.  Each element
of LAYOUT is a list of a window followed by four edges for it.  We
reexpose each window with the specified edges.  The first window in
the list is selected as well."
  (COND
    (LAYOUT
     (DELAYING-SCREEN-MANAGEMENT
       (DOLIST (Y LAYOUT)
	 (LET ((WINDOW (CAR Y))
	       (STATUS (CADR Y))		; Exposed-p status
	       (EDGES (CDDR Y)))
	   (SEND WINDOW :SET-EDGES (FIRST EDGES)	; Left
		 (SECOND EDGES)			; Top
		 (THIRD  EDGES)			; Right
		 (FOURTH EDGES))		; Bottom
	   (SEND WINDOW :SET-STATUS (IF (EQ STATUS :SELECTED)
					:EXPOSED
					STATUS))))
       ;; Make sure that inferiors are in same order now as when
       ;; layout saved.
       (DOLIST (Y (REVERSE LAYOUT))
	 (IF (MEMBER (CAR Y) (SHEET-INFERIORS MOUSE-SHEET) :TEST #'EQ)
	     (SETF (SHEET-INFERIORS MOUSE-SHEET)
		   (CONS (CAR Y)
			 (DELETE (CAR Y) (THE LIST (SHEET-INFERIORS MOUSE-SHEET)) :TEST #'EQ))))))
     (SEND (CAAR LAYOUT) :SELECT NIL)
     (SEND MOUSE-SHEET :SCREEN-MANAGE) NIL)))

;;;relaced the following defun on 19 Dec 85 for GRH by GSM
(DEFUN SAVE-THIS-SCREEN-LAYOUT (MENU)
  (LET (*LIST*
	W-DESC-LIST
	SW-DESC-LIST
	(LAYOUT-NAME (GET-LINE-FROM-KEYBOARD "Name for this screen layout"))
	(ITEM-LIST (SEND MENU :ITEM-LIST))
	CHAR)
    ;;(LOCALLY
    (DECLARE (SPECIAL *LIST*))
    (DOLIST (W (SHEET-INFERIORS MOUSE-SHEET))
      (UNLESS (OR (SHEET-TEMPORARY-P W)
		  (EQ W MOUSE-SHEET))
	;; Create the window description list:
	;; (Window Exposed-p left top right bottom).
	(SETQ W-DESC-LIST (LIST* W (SEND W :STATUS) (MULTIPLE-VALUE-LIST (SEND W :EDGES))))
	(IF (EQ W SELECTED-WINDOW)
	    (SETQ SW-DESC-LIST W-DESC-LIST)
	    (SETQ *LIST* (CONS W-DESC-LIST *LIST*)))))
    ;; Move selected window to the front
    (WHEN SW-DESC-LIST
      (SETQ *LIST* (CONS SW-DESC-LIST *LIST*)))
    (SETQ SCREEN-LAYOUT-MENU-ALIST
	  (CONS (LIST LAYOUT-NAME :VALUE *LIST*)
		(DELETE (ASSOC LAYOUT-NAME ITEM-LIST :TEST #'EQUAL)
			(THE LIST ITEM-LIST)
			:TEST #'EQ))		; delete duplicate names
	  CHAR
	  (CHAR-UPCASE
	    (GET-LINE-FROM-KEYBOARD "System key for this screen layout
 (Rubout means none)"
				    MOUSE-SHEET 'TYI)))
    (COND
      ((EQL CHAR #\RUBOUT))			;used to be EQ PMH 7/1/87
      ((OR (EQL CHAR #\SPACE)			;used to be EQ PMH 7/1/87
	   (>= CHAR 128))
       (BEEP))
      (T (ADD-SYSTEM-KEY CHAR `(RESELECT-SCREEN-LAYOUT ',*LIST*) LAYOUT-NAME *LIST*))))
  ;;)
  )

(DEFWINDOW-RESOURCE POP-UP-TEXT-WINDOW ()
  :MAKE-WINDOW (POP-UP-TEXT-WINDOW
		 :foreground-color *default-menu-foreground*
		 :background-color *default-menu-background*)
  :INITIAL-COPIES 0)

(DEFWINDOW-RESOURCE POP-UP-TEXT-WINDOW-WITHOUT-MORE ()
  :MAKE-WINDOW (POP-UP-TEXT-WINDOW :MORE-P NIL
				   :foreground-color *default-menu-foreground*
				   :background-color *default-menu-background*)
  :INITIAL-COPIES 0)


(DEFUN POP-UP-MESSAGE
       (PROMPT &OPTIONAL (SUP MOUSE-SHEET) (POP-UP-NEAR '(:MOUSE)))
  "Pop up a window with a message in it, require user to type a
character to remove."
  (LET ((MESSAGE (STRING-APPEND PROMPT "
Press the space bar to remove this message:  ")))
    (USING-RESOURCE (POP-UP-MESSAGE-WINDOW POP-UP-TEXT-WINDOW-WITHOUT-MORE SUP)
      (SEND POP-UP-MESSAGE-WINDOW :SET-LABEL NIL)
      (SEND POP-UP-MESSAGE-WINDOW :SET-SIZE-IN-CHARACTERS MESSAGE MESSAGE)
      (SEND POP-UP-MESSAGE-WINDOW :CLEAR-INPUT)
      (EXPOSE-WINDOW-NEAR POP-UP-MESSAGE-WINDOW POP-UP-NEAR NIL)
      (WINDOW-CALL (POP-UP-MESSAGE-WINDOW :DEACTIVATE)
	(SEND POP-UP-MESSAGE-WINDOW :STRING-OUT MESSAGE)
	;; Back up the cursor by one.  This is easier than trying to
        ;; make the window come out wider, because of the interface to
        ;; :set-size-in-characters.
	(MULTIPLE-VALUE-BIND (X-POS Y-POS)
	    (SEND POP-UP-MESSAGE-WINDOW :READ-CURSORPOS :CHARACTER)
	(SEND POP-UP-MESSAGE-WINDOW :SET-CURSORPOS
                 (1- X-POS) Y-POS :CHARACTER))
	(SEND POP-UP-MESSAGE-WINDOW :TYI)))))

(DEFUN POP-UP-FORMAT (CONTROL &REST ARGS)
  "Pop up a formatted message near the mouse."
  (POP-UP-MESSAGE (APPLY #'FORMAT NIL CONTROL ARGS)))

(DEFUN GET-LINE-FROM-KEYBOARD
       (PROMPT &OPTIONAL (SUP MOUSE-SHEET) (FUNCTION #'READ-LINE) (POP-UP-NEAR '(:MOUSE)))
  "Pop up a window near where the mouse is, then read a line from it."
  (USING-RESOURCE (GET-LINE-FROM-KEYBOARD-WINDOW POP-UP-TEXT-WINDOW SUP)
     (SEND GET-LINE-FROM-KEYBOARD-WINDOW :SET-SIZE 320 80)
     (SEND GET-LINE-FROM-KEYBOARD-WINDOW :SET-LABEL NIL)
     (SEND GET-LINE-FROM-KEYBOARD-WINDOW :CLEAR-INPUT)
     (EXPOSE-WINDOW-NEAR GET-LINE-FROM-KEYBOARD-WINDOW POP-UP-NEAR NIL)
     (WINDOW-CALL (GET-LINE-FROM-KEYBOARD-WINDOW :DEACTIVATE)
	(FORMAT GET-LINE-FROM-KEYBOARD-WINDOW "~A:~%" PROMPT)
	(FUNCALL FUNCTION GET-LINE-FROM-KEYBOARD-WINDOW))))

(DEFUN GET-WINDOW-TYPE-FROM-KEYBOARD
       (&OPTIONAL (SUP MOUSE-SHEET) REQUIRED-INIT-OPTION (POP-UP-NEAR '(:MOUSE))
	&AUX (WT NIL) FL)
  (CATCH-ERROR-RESTART ((ABORT ERROR) "Abort this screen operation.")
    (SETQ WT (GET-LINE-FROM-KEYBOARD "Flavor of window" SUP #'READ POP-UP-NEAR)))
  (COND
    ((NULL WT) NIL)
    ((OR (NULL (SETQ FL (GET WT 'SI:FLAVOR)))
	 (NOT
	   (SI:MAP-OVER-COMPONENT-FLAVORS 0 NIL T	;T if it's built on SHEET
					  #'(LAMBDA (FL IGNORE)
					      (EQ FL (GET 'SHEET 'SI:FLAVOR)))
					  WT NIL))
	 (AND REQUIRED-INIT-OPTION
	      (NOT (FLAVOR-ALLOWS-INIT-KEYWORD-P WT REQUIRED-INIT-OPTION))))
     (BEEP)
     NIL)
    (T WT)))

;;;Hack window for split screen
(DEFFLAVOR DISPLAY-LAYOUT-WINDOW
  ((FROBS NIL))
  (TEMPORARY-SHADOW-BORDERS-WINDOW-MIXIN BORDERS-MIXIN MINIMUM-WINDOW GRAPHICS-MIXIN)
  (:INITABLE-INSTANCE-VARIABLES FROBS))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :BEFORE :INIT) (INIT-PAIRS)
  (SETF (GET INIT-PAIRS :BLINKER-P) NIL))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :CLEAR-FROBS) ()
  (SETQ FROBS NIL)
  (SHEET-FORCE-ACCESS (SELF T)
    (SHEET-CLEAR SELF)))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :ADD-FROB) (FROB &AUX N)
  (SETQ N (LENGTH FROBS)
	FROBS (NCONC FROBS (CONS FROB NIL)))
  (SHEET-FORCE-ACCESS (SELF)
    (DRAW-FROBS SELF FROBS N ERASE-ALUF)
    (DRAW-FROBS SELF FROBS (1+ N) CHAR-ALUF)))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :REMOVE-LAST-FROB) ()
  (SETQ FROBS (NREVERSE (CDR (NREVERSE FROBS))))
  (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
    (SHEET-CLEAR SELF)
    (DRAW-FROBS SELF FROBS (LENGTH FROBS) CHAR-ALUF)))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR RESTORED-BITS-P
      (DRAW-FROBS SELF FROBS (LENGTH FROBS) CHAR-ALUF)))

(DEFUN DRAW-FROBS (WINDOW FROBS N ALU)
  (OR (ZEROP N)
      (LET* ((HEIGHT (SHEET-INSIDE-HEIGHT WINDOW))
	     (WIDTH (SHEET-INSIDE-WIDTH WINDOW))
	     (MIDDLE (TRUNCATE WIDTH 2))
	     (2ND-LEFT (TRUNCATE WIDTH 3))
	     (1ST-RIGHT (+ 2ND-LEFT 2ND-LEFT))
	     NROW
	     NCOL)
	(COND
	  ((AND *LANDSCAPE-MONITOR* *OVERLAP-SPLIT-SCREEN*)
	   (COND
	     ((= N 1) (SETQ NROW 1 NCOL 1))
	     ((= N 2) (SETQ NROW 1 NCOL 2))
	     (T (SETQ NROW (TRUNCATE (1+ N) 2)
		      NCOL 2))))
	  (T
	   (IF (< N 4)
	       (SETQ NROW N
		     NCOL 1
		     MIDDLE NIL)
	       (SETQ NROW (TRUNCATE (1+ N) 2)
		     NCOL 2
		     2ND-LEFT MIDDLE
		     1ST-RIGHT MIDDLE))))
	(DO ((I NROW (1- I))
	     (J 0 (1+ J))
	     (FROBS FROBS (CDR FROBS))
	     (Y)
	     (Y1)
	     (LHEIGHT (TRUNCATE HEIGHT NROW))
	     (ODDP (ODDP N)))
	    ((<= I 0))
	  ;; Y = size of top, Y1 = size of bottom.
	  (SETQ Y (TRUNCATE (* HEIGHT J) NROW)
		Y1 (IF (= I 1)
		       (1- HEIGHT)
		       (+ Y LHEIGHT)))
	  (OR (= I 1)
	      (SEND WINDOW :DRAW-LINE 1 Y1 WIDTH Y1 ALU T))
	  (DRAW-LAYOUT-LABEL WINDOW (CAR FROBS) 0 Y1
			     (IF (OR (NULL MIDDLE)
				     (AND (= I 1)
					  ODDP))
				 WIDTH
				 2ND-LEFT)
			     LHEIGHT ALU)
	  (COND
	    ((NOT (OR (NULL MIDDLE)
		      (AND (= I 1)
			   ODDP)))
	     ;; Draw vertical divider.
	     (SEND WINDOW :DRAW-LINE 2ND-LEFT Y 2ND-LEFT Y1 ALU T)
	     (SEND WINDOW :DRAW-DASHED-LINE 1ST-RIGHT Y 1ST-RIGHT (- Y1 7) ALU
		   (MIN 5 (MAX (truncate (- Y1 Y) 5)
			       2)))
	     (SETQ FROBS (CDR FROBS))
	     (DRAW-LAYOUT-LABEL WINDOW (CAR FROBS) 2ND-LEFT Y1 WIDTH LHEIGHT ALU)))))))

(DEFUN DRAW-LAYOUT-LABEL (WINDOW STRING FROM-X FROM-Y XLIM LHEIGHT ALU)
  ;;; hacked this to draw labels using inside coordinates   --mdm 12/17/84
  ;;; is there a need to use anything besides a 5X5 font?   --mdm 12/17/84
  (MULTIPLE-VALUE-BIND (LEFT TOP)
      (SEND WINDOW :MARGINS)
    (LET ((X (+ LEFT FROM-X))
	  (Y (+ TOP FROM-Y))
	  (XLIMIT (+ LEFT XLIM)))
      (COND
	((< LHEIGHT 3))				;Too small for anything
	((< LHEIGHT 7)				;Too small for 5X5
	 (DRAW-LAYOUT-DOTS WINDOW STRING (1+ X) (- Y 2) XLIMIT ALU))
	(T
	 (SEND WINDOW :STRING-OUT-EXPLICIT (STRING-UPCASE STRING) (1+ X) (- Y 6) XLIMIT NIL
	       (SEND (SHEET-GET-SCREEN WINDOW) :PARSE-FONT-DESCRIPTOR FONTS:|5X5|) ALU 0 NIL NIL))))))

(DEFUN DRAW-LAYOUT-DOTS (SHEET STRING X Y XLIM ALU)
  "For every non-blank character in STRING, set a bit in the
screen-array of SHEET.  The bits are separated by 1 bit.  (X,Y)
indicate where to start for the first character in STRING."
  (DO ((I 0 (1+ I))
       (X X (+ X 2))
       (ARRAY (SHEET-SCREEN-ARRAY SHEET))
       (LEN (LENGTH STRING)))
      ((OR (>= I LEN)
	   (>= X XLIM)))
    (OR (= (AREF STRING I) #\SPACE)
	(setf (aref array y x) (SELECT ALU (ALU-XOR (1+ (AREF ARRAY Y X)))
				       (ALU-IOR 1) (ALU-ANDCA 0))))))

(DEFMETHOD (DISPLAY-LAYOUT-WINDOW :MOVE-NEAR-WINDOW)
	   (WINDOW &OPTIONAL (DIMENSIONS '(5 . 4)))
  "Move along side a window.  Try to make the same height as that
window, but if that won't fit because it comes out too wide then
become shorter, and center.  DIMENSIONS argument controls the width
to height ratio.  The CAR of DIMENSIONS corresponds to the width
and the CDR of DIMENSIONS corresponds to the height."
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (SEND WINDOW :EDGES)
    (LET* ((NEW-HEIGHT (- BOTTOM TOP))
	   (NEW-WIDTH (truncate (* (CAR DIMENSIONS) NEW-HEIGHT) (CDR DIMENSIONS)))
	   (SLEFT   (SHEET-INSIDE-LEFT   SUPERIOR))
	   (STOP    (SHEET-INSIDE-TOP    SUPERIOR))
	   (SRIGHT  (SHEET-INSIDE-RIGHT  SUPERIOR))
	   (SBOTTOM (SHEET-INSIDE-BOTTOM SUPERIOR))
	   (DY1 (- TOP STOP))
	   (DY2 (- SBOTTOM BOTTOM))
	   (NTOP TOP)
	   (NBOTTOM BOTTOM)
	   NLEFT
	   NRIGHT
	   NCENTER)
      (COND
	((>= (SETQ NLEFT (- LEFT NEW-WIDTH)) SLEFT)
	 (SETQ NRIGHT LEFT))			;Fits on the left
	((< (SETQ NRIGHT (+ RIGHT NEW-WIDTH)) SRIGHT)
	 (SETQ NLEFT RIGHT))			;Fits on the right
	;; Make it short enough to fit in largest remaining area
	;; and scale the width to match otherwise clip to SUP edges.
	((>= DY1 DY2)				;Fits on the top
	 (SETQ NEW-HEIGHT (MAX (MIN NEW-HEIGHT DY1)
			       (MIN NEW-HEIGHT (truncate (* (CDR DIMENSIONS) (- RIGHT LEFT))
							 (CAR DIMENSIONS))))
	       NEW-WIDTH (MAX (MIN NEW-WIDTH (- SRIGHT SLEFT))
			      (MIN NEW-WIDTH (truncate (* (CAR DIMENSIONS) NEW-HEIGHT)
						       (CDR DIMENSIONS))))
	       NCENTER (truncate (+ SLEFT SRIGHT) 2)
	       NLEFT (MAX (- NCENTER (truncate NEW-WIDTH 2))
			  SLEFT)
	       NTOP (MAX (- TOP NEW-HEIGHT) STOP)
	       NRIGHT (MIN (+ NCENTER (truncate NEW-WIDTH 2)) SRIGHT)
	       NBOTTOM TOP))
	((< DY1 DY2)				;Fits on the bottom
	 (SETQ NEW-HEIGHT (MAX (MIN NEW-HEIGHT DY2)
			       (MIN NEW-HEIGHT (truncate (* (CDR DIMENSIONS) (- RIGHT LEFT))
							 (CAR DIMENSIONS))))
	       NEW-WIDTH (MAX (MIN NEW-WIDTH (- SRIGHT SLEFT))
			      (MIN NEW-WIDTH (truncate (* (CAR DIMENSIONS) NEW-HEIGHT)
						       (CDR DIMENSIONS))))
	       NCENTER (truncate (+ SLEFT SRIGHT) 2)
	       NLEFT (MAX (- NCENTER (truncate NEW-WIDTH 2))
			  SLEFT)
	       NTOP BOTTOM
	       NRIGHT (MIN (+ NCENTER (truncate NEW-WIDTH 2))
			   SRIGHT)
	       NBOTTOM (MIN (+ BOTTOM NEW-HEIGHT) SBOTTOM)))
	(T (FERROR NIL "Insufficient room to display layout window.")))
      (SEND SELF :SET-EDGES NLEFT NTOP NRIGHT NBOTTOM)))
  (SEND SELF :EXPOSE))

(DEFWINDOW-RESOURCE SPLIT-SCREEN-LAYOUT-WINDOW ()
  :MAKE-WINDOW (DISPLAY-LAYOUT-WINDOW
                 :HEIGHT (TRUNCATE (SHEET-HEIGHT MOUSE-SHEET) 4.))
  :REUSABLE-WHEN :DEEXPOSED
  :INITIAL-COPIES 0)


; A simple listener until the UCL listener is built.
(ADD-TO-SYSTEM-MENU-COLUMN :PROGRAMS "Lisp Listener"
			   '(SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'LISTENER-MIXIN 'simple-lisp-listener)
			   "Select a Lisp Listener to evaluate Lisp forms."
			   :SORTED)

;;;********
;;;THESE NEED TO BE REMOVED FROM THIS FILE AND INCLUDED IN THE FILES DEFINING EACH OF THESE UTILITIES.
;;;********

;;;(ADD-TO-SYSTEM-MENU-COLUMN :PROGRAMS "ZMacs Editor"
;;;			   '(SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'ZWEI:ZMACS-FRAME)
;;;			   "Select a ZMacs Text editor to edit text or write a program."
;;;			   :SORTED)

;;;(ADD-WINDOW-TYPE "Edit"      'ZWEI:ZMACS-FRAME    "An editor, sharing buffers with other editors.")
;;;(ADD-WINDOW-TYPE "Font Edit" 'FED:FED-FRAME       "Edit characters in fonts.")
;;;(ADD-WINDOW-TYPE "Telnet"    'TELNET:TELNET       "Login to a remote host.")
;;;(ADD-WINDOW-TYPE "Converse"  'ZWEI:CONVERSE-FRAME "Send messages to another user.")
