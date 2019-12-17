;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; The flavor definitions for the font editor.  The flavor structure looks like this:

;;;   FED           \                 -> GRAY-GRID-MIXIN       ->  PLANE-GRID-MIXIN  ->  GRID-MIXIN
;;;                   -> BASIC-FED   -> CHAR-BOX-GRID-MIXIN  ->     (requires grid-mixin)
;;; REGISTER-PANE /                 -> CURSOR
;;;
;;; FED-TYPEOUT-WINDOW
;;; FED-LABEL-WINDOW
;;; FED-MENU-PANE
;;;
;;; FED-FRAME  is the actual Font Editor window, and points at the instinces of:
;;;               FED, REGISTER-PANE, FED-TYPEOUT-WINDOW, FED-LABEL-WINDOW, FED-MENU-PANE, PROMPT-WINDOW

;;; CHANGE HISTORY

;;;  2/06/87 DKM - changed fed-menu-pane to use w:menu with initializations instead of obsolete command-menu flavor
;;;                    - changed fed-frame flavor to center heading of menus, as new default in w:menu is to left justify.
;;;  6/22/87 DKM - changed cursor flavor to initialize cursor-window with self instead of tv:selected-window whcih can be nil.

(DEFFLAVOR GRID-MIXIN
	   (WINDOW-ARRAY				 ; This represents the displayed image.
	    WINDOW-X-SIZE				  ; Size in pixels in the X direction.
	    WINDOW-Y-SIZE				  ; Size in pixels in the Y direction.
	    (BOX-X-SIZE DEFAULT-BOX-SIZE)		  ; The size of an element of the grid.
	    (BOX-Y-SIZE DEFAULT-BOX-SIZE)
	    (WINDOW-X-POS 0)				  ; The offset position of our array.
	    (WINDOW-Y-POS 0)
	    (REDISPLAY-DEGREE REDISPLAY-NONE)		  ; A number, REDISPLAY-<n>. (see above).
	    (MIN-CHANGED-X 0)				  ; Range of area to bother checking.
	    (MIN-CHANGED-Y 0)
	    (MAX-CHANGED-X 0)
	    (MAX-CHANGED-Y 0)
	    ;; PREVIOUS-EDGES describes an area of boxes which must be
	    ;; redisplayed independent of MAX/MIN-CHANGED-X/Y and the current :PLANE-EDGES
	    ;; because the planes occupied those areas prior to erasing or moving them.
	    ;; The elements are relative to the window, not the planes.
	    (PREVIOUS-EDGES '(0 0 0 0))
	    REDISPLAY-SUPPRESSED)			  ; The last redisplay did not complete.
	   ()
  (:REQUIRED-FLAVORS TV:ESSENTIAL-WINDOW)
  (:INIT-KEYWORDS :WINDOW-ARRAY-TYPE)
  (:INITABLE-INSTANCE-VARIABLES)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :MORE-P NIL)
  (:REQUIRED-METHODS :AREF :ASET :PLANE-EDGES :LISTEN)
  (:DOCUMENTATION :MIXIN
   "Displays a set of points within a grid
    and allows for incremental redisplay of points and updating
    the data structure for changes in the display.")) 

(DEFFLAVOR PLANE-GRID-MIXIN
	   (PLANE)
	   (GRID-MIXIN)
  :GETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :SPECIAL-PURPOSE
   "A grid window that displays a plane. The plane instance variable is displayed
     in the grid and updated when it is changed via the mouse.")) 

(DEFFLAVOR GRAY-GRID-MIXIN
	   ((GRAY-PLANE NIL)
	    ;; Add these offsets to a co-ordinate in the regular plane
	    ;; to get the corresponding coordinate in the gray plane.
	    (GRAY-X-OFFSET 0)
	    (GRAY-Y-OFFSET 0))
	   ()
  (:REQUIRED-FLAVORS PLANE-GRID-MIXIN)
  (:DEFAULT-INIT-PLIST :WINDOW-ARRAY-TYPE :ART-2B)) 

(DEFFLAVOR CURSOR
	   ((CURSOR-VISIBILITY T)
	    (CURSOR-WINDOW SELF)                ;init with SELF instead of tv:selected-window  dkm 6/87
	    (CURSOR-X-POSITION 0)
	    (CURSOR-Y-POSITION 0)
	    (CURSOR-X-OFFSET 0)			; non-zero if not to be centered around x,y position
	    (CURSOR-Y-OFFSET 0)
	    (BLINKER NIL)
	    (CURSOR-CHARACTER 6)		; default mouse cursor
	    (CURSOR-FONT FONTS:MOUSE))		; defaults to standard mouse font

	   ()
  (:INITABLE-INSTANCE-VARIABLES CURSOR-VISIBILITY CURSOR-X-POSITION CURSOR-Y-POSITION
   CURSOR-X-OFFSET CURSOR-Y-OFFSET CURSOR-WINDOW CURSOR-CHARACTER CURSOR-FONT)
  (:GETTABLE-INSTANCE-VARIABLES CURSOR-VISIBILITY CURSOR-X-POSITION CURSOR-Y-POSITION
   CURSOR-X-OFFSET CURSOR-Y-OFFSET CURSOR-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES CURSOR-VISIBILITY)
  (:DOCUMENTATION :SPECIAL-PURPOSE
   "This is a character cursor positioned in window coordinates.
The character may be any character from any fonts: type of font."
   ))

(DEFFLAVOR CHAR-BOX-GRID-MIXIN
	   ((CHAR-BOX-X1 0)
	    (CHAR-BOX-Y1 0); The real position.
	    (CHAR-BOX-X2 0)
	    (CHAR-BOX-Y2 0)
	    (CHAR-BOX-Y3 0)
	    (INHIBIT-CHAR-BOX NIL)
	    DISPLAYED-CHAR-BOX-X1
	    DISPLAYED-CHAR-BOX-Y1; The displayed position.
	    DISPLAYED-CHAR-BOX-X2
	    DISPLAYED-CHAR-BOX-Y2
	    DISPLAYED-CHAR-BOX-Y3)
	   ()
  :SETTABLE-INSTANCE-VARIABLES
  :INITABLE-INSTANCE-VARIABLES
  (:REQUIRED-FLAVORS GRID-MIXIN)
  (:DOCUMENTATION :SPECIAL-PURPOSE
   "Grid windows with a special outline. The outline is used to show
     the actual character area and baseline by the font-editor.")) 

(DEFFLAVOR BASIC-FED ((CURRENT-FONT NIL)
		     (CURRENT-CHARACTER NIL)
		     (UNSAVED-CHANGES NIL)
		     (CURSOR-X 0)
		     (CURSOR-Y 0)
		     (CURSOR-ON NIL)
		     (basic-fed-cursor)	         ;; current drawing pane cursor
		     (basic-fed-flip-cursor)	 ;; current flip-mode cursor value
		     (basic-fed-draw-cursor)	 ;; current draw-mode cursor value
		     (basic-fed-erase-cursor)    ;; current erase-mode cursor value
		     mouse-list)		 ;; list of available "mice" for the drawing pane
	   (GRAY-GRID-MIXIN
	    PLANE-GRID-MIXIN
	    CHAR-BOX-GRID-MIXIN
	    CURSOR
	    TV:LIST-MOUSE-BUTTONS-MIXIN)
  (:GETTABLE-INSTANCE-VARIABLES CURRENT-CHARACTER UNSAVED-CHANGES)
  (:SETTABLE-INSTANCE-VARIABLES CURRENT-CHARACTER UNSAVED-CHANGES)
  (:SPECIAL-INSTANCE-VARIABLES mouse-list)
  (:DOCUMENTATION :SPECIAL-PURPOSE 
		  "The font editor itself uses its grid for displaying the character being edited."))

(DEFFLAVOR REGISTER-PANE
	    (register-pane-cursor)		 ;; add new cursor for register panes
	    (BASIC-FED TV:WINDOW)
   (:DEFAULT-INIT-PLIST :LABEL NIL
     :BLINKER-P NIL
     :BOX-X-SIZE 4
     :BOX-Y-SIZE 4
     :INHIBIT-CHAR-BOX T))

(DEFFLAVOR FED					    
	  ((DRAW-MODE 6)			; Initially XOR.
	   (LABEL-WINDOW NIL)
	   PROMPT-WINDOW
	   (SPECIAL-COMMAND-MOUSE-DOCUMENTATION NIL)
	   (SAMPLE-STRING "\"Sample string:  AaBbCc / 0123...\""))
  (BASIC-FED TV:INTRINSIC-NO-MORE-MIXIN TV:WINDOW-WITH-TYPEOUT-MIXIN
   TV:PROCESS-MIXIN TV:WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES PROMPT-WINDOW LABEL-WINDOW)
  (:DOCUMENTATION :COMBINATION "The actual FED window"))

;;; The actual FED-WINDOW.
(DEFFLAVOR FED-TYPEOUT-WINDOW
	   (fed-typeout-cursor)			 ;add menu-select-type cursor to the typeout window
	   (TV:ANY-TYI-MIXIN
	    TV:TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS))

;;; The FED-LABEL-WINDOW displays the font name, character code, sample string.
(DEFFLAVOR FED-LABEL-WINDOW
	    ((FED-WINDOW NIL)
	     fed-label-window-cursor)		 ;add menu-select-type cursor to the typeout window
	    (TV:BASIC-MOUSE-SENSITIVE-ITEMS
	      TV:TRUNCATING-WINDOW)
   :SETTABLE-INSTANCE-VARIABLES
   (:DEFAULT-INIT-PLIST
     :ITEM-TYPE-ALIST
     '((FONT :PROMPT-LINE-SELECT-FONT "Select a new font for editing.")
       (LINE-HEIGHT :PROMPT-LINE-SET-LINE-HEIGHT "Set font's total line height, including descenders.")
       (BASELINE :PROMPT-LINE-SET-BASELINE "Set font's height above baseline.")
       (BLINKER-HEIGHT :PROMPT-LINE-SET-BLINKER-HEIGHT "Set font's blinker height.")
       (BLINKER-WIDTH :PROMPT-LINE-SET-BLINKER-WIDTH "Set the font's blinker width.")
       (CHAR :PROMPT-LINE-SELECT-CHAR "Select new character for editing.")
       (CHAR-BY-NUMBER :PROMPT-LINE-SELECT-CHAR-CODE "Select new character to edit by its decimal character code.")
       (WIDTH :PROMPT-LINE-SET-CHAR-WIDTH "Specify a new width for the current character.")
       (SAMPLE-STRING :PROMPT-LINE-SET-SAMPLE "Specify a new sample string to display in this field.")
       (MOUSE-EDIT-MODE :CHANGE-MOUSE-EDIT-MODE "Toggle the mouse edit mode (Flip//Draw//Erase).")
       (DOCUMENT-ITEM :DOCUMENT-LABEL-PANE-ITEM "Display documentation about this Label Pane item."))))

(DEFFLAVOR FED-MENU-PANE
	   ()
	   (W:MENU)
  (:DOCUMENTATION :COMBINATION "Command menus for UCL.")
  (:DEFAULT-INIT-PLIST
    :background-color w:33%-gray-color
    :COMMAND-MENU T          ;make it a command menu type.
    :DYNAMIC T               ;UCL needs these guys to be dynamic
    :SCROLLING-P NIL         ;don't want any scroll bars out there
    :border-margin-width 3)) ;this keeps the menu items away from the border a bit

;;; Added new documentation menus.  Each basic menu has as its title a menu
;;; that can be clicked on to present documentation on those menu selections.
(DEFFLAVOR FED-FRAME
	((min-registers 2)
	 (max-registers 8.)
	 (register-panes NIL)
	 (fed-frame nil)
	 (fed-pane nil)
	 (prompt-pane nil)
	 (typeout-pane nil)
	 (label-pane nil))
	(ucl:basic-command-loop
         tv:frame-dont-select-inferiors-with-mouse-mixin
	 tv:bordered-constraint-frame-with-shared-io-buffer
	 tv:label-mixin
	 tv:box-label-mixin)
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :INITTABLE-INSTANCE-VARIABLES
  :SPECIAL-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST
    :active-command-tables '(fed-cmd-table)
    :all-command-tables '(fed-cmd-table)
    :menu-panes '((register-title ucl-register-title)
		  (font-io-doc ucl-fed-font-io-doc)
		  (font-io-menu ucl-fed-font-io-menu)
		  (char-io-doc ucl-fed-char-io-doc)
		  (char-io-menu ucl-fed-char-io-menu)
		  (editing-doc ucl-fed-editing-doc)
		  (editing-menu ucl-fed-editing-menu)
		  (screen-doc ucl-fed-screen-doc)
		  (screen-menu ucl-fed-screen-menu)
		  (help-doc ucl-fed-help-doc)
		  (help-menu ucl-fed-help-menu)
		  (fed-exit ucl-fed-exit)           ;new exit command added 
		  (all-registers-menu ucl-fed-all-registers-menu))
    :blip-alist '((:MENU :handle-menu-input)
		  (:MOUSE-BUTTON :handle-mouse-input)		  (:DIRECT-COMMAND-ENTRY :handle-direct-command-input)
	          (:typeout-execute :fed-handle-blip)
	          (redisplay        :fed-handle-blip))
    :typein-handler :handle-typein-input
    :prompt "> "
    :border-margin-width 2
    :basic-help '(general-doc-cmd)
    :label NIL
    :label-box-p T
    :PANES  `((fed-title-window tv:window
				:borders (0 1 0 0)
				:label (:string " font editor - frame nn" :font fonts:tr8b)
				:blinker-deselected-visibility :off
				:more-p nil)
	      (FED-WINDOW FED			 ; For the Grid.
			  :LABEL NIL
			  :borders (1 0 1 1)
			  :BORDER-MARGIN-WIDTH 8.
			  :BLINKER-P NIL)
	      (prompt-window ucl:command-and-lisp-typein-window
			     :blinker-deselected-visibility :off
			     :label nil
			     :borders (0 0 1 0)
			     :name nil
			     :more-p nil)
	      (LABEL-WINDOW FED-LABEL-WINDOW	 ; For Sample and various font/character parameters
			    :BLINKER-P NIL
			    :LABEL NIL
			    :borders (0 0 1 0)
			    :MORE-P NIL
			    :FONT-MAP ,(LIST fonts:cptfont fonts:hl12b fonts:tr8b))
	      (font-io-doc fed-menu-pane
			   :borders (0 0 0 0)
			   :item-alignment :center
			   :font-map ,(LIST fonts:hl12b))
	      (register-pane-0 register-pane :borders (1 1 0 0))
 	      (register-pane-1 register-pane :borders (1 1 0 0))
	      (register-pane-2 register-pane :borders (1 1 0 0))
	      (register-pane-3 register-pane :borders (1 1 0 0))
	      (register-pane-4 register-pane :borders (1 1 0 0))
	      (register-pane-5 register-pane :borders (1 1 0 0))
	      (register-pane-6 register-pane :borders (1 1 0 0))
	      (register-pane-7 register-pane :borders (1 1 0 0))
	      (font-io-menu fed-menu-pane
			    :borders (0 0 0 0)
			    :font-map ,(LIST fonts:cptfont))
	      (char-io-doc fed-menu-pane
			   :borders (0 0 0 0)
			   :item-alignment :center
			   :font-map ,(LIST fonts:hl12b))
	      (char-io-menu fed-menu-pane
			    :borders (0 0 0 0)
			    :font-map ,(LIST fonts:cptfont))
	      (editing-doc fed-menu-pane
			   :borders (0 0 0 0)
			   :item-alignment :center
			   :font-map ,(LIST fonts:hl12b))
	      (editing-menu fed-menu-pane
			    :borders (0 0 0 0)
			    :font-map ,(LIST fonts:cptfont))
	      (screen-doc fed-menu-pane
			  :borders (0 0 0 0)
			   :item-alignment :center
			  :font-map ,(LIST fonts:hl12b))
	      (screen-menu fed-menu-pane
			   :borders (0 0 0 0)
			   :font-map ,(LIST fonts:cptfont))
	      (help-doc fed-menu-pane
			:borders (0 0 0 0)
			:item-alignment :center
			:font-map ,(LIST fonts:hl12b))
	      (help-menu fed-menu-pane
			 :borders (0 0 0 0)
			 :font-map ,(LIST fonts:cptfont))
	      (fed-exit fed-menu-pane
			:borders (0 0 0 0)
			:item-alignment :center
			:font-map ,(LIST fonts:hl12b))
	      (register-title fed-menu-pane	 ;; this guy used to hold a title over the registers
			   :borders (0 0 0 0)			 ;; and to provide an entry to click on for documentation
			   :item-alignment :center
			   :font-map ,(LIST fonts:hl12b))
	      (all-registers-menu fed-menu-pane
				  :borders (0 1 0 1)	 
				  :item-alignment :center
				  :font-map ,(LIST fonts:TR8B)))
    :SAVE-BITS T
    :MORE-P NIL))

