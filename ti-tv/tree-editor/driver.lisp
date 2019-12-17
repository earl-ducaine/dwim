;;;-*- Mode:Common-Lisp; Package:TREE; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;;;
;;; The data, information, methods, and concepts contained herein are a valuable
;;; trade secret of Texas Instruments.  They are licensed in confidence by Texas
;;; Instruments and may only be used as permitted under the terms of the
;;; definitive license agreement under which such use is licensed.
;;;
;;;			    RESTRICTED RIGHTS LEGEND
;;;
;;; Use, duplication, or disclosure by the Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013.
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987- 1989 Texas Instruments Incorporated.  All rights reserved.
;;;

;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 2-13-87   DLS		Original version for Release 3.
;;;


(DEFUN display-as-process (frame)
  "Runs the tree editor as a process."
  (SETQ *terminal-io* frame))


(DEFUN display
       (tree &key &optional (superior tv:selected-window)
			    (application-type *default-application-type*)
			    (level 0) (vertical? *default-vertical?*)
			    (edit? nil) (init-label *default-init-label*)
			    (adjust-to-sup-size *default-adjust-to-sup-size*)
			    (frame nil) (return-window superior)
			    (allow-new-tree t)) 
  "Top level driver for the tree editor.
Tree:  The tree to be displayed/edited. This is the only required argument.
Superior:  This window is where the tree editor will get its size and position if the
    :ADJUST-TO-SUP-SIZE parameter is true.  The default is TV:SELECTED-WINDOW. 
Application-type:  The application flavor name that defines all the accessor functions to
    handle this kind of data.  If this isn't specified *DEFAULT-APPLICATION-TYPE* is
    used, or if that is NIL and there is only one value in *KNOWN-APPLICATION-TYPES*,
    the value in *KNOWN-APPLICATION-TYPES* is used.  If there is more than one known
    type, a list of the known types is put up in a menu for the user to choose from.
Level:  The maximum depth to be displayed.  The top node displayed is at level one.  Zero
    means to display the entire tree.   The default is zero.
Vertical?:  Controls the orientation of the display.  T means a vertical (top-down)
    orientation and NIL means a horizontal orientation.  The default is what is in the
    variable *DEFAULT-VERTICAL?*.
Edit?:  Controls whether destructive editing is allowed on the tree.  The default is NIL.
Init-label:  The label you want to appear at the bottom of the display pane.  If you don't
    specify one *DEFAULT-INIT-LABEL* is used.
Adjust-to-Sup-Size:  If this is true, the tree window takes on the size and position of
    its superior.  Otherwise it fills the screen.  The default is
    *DEFAULT-ADJUST-TO-SUP-SIZE*.
Frame:  Holds the value of the tree frame process, if it is being run in a process.  If NIL,
    the tree frame is allocated as a resource without a process.  The default is NIL. 
Return-window:  This is the window that will be exposed when the tree editor is
    deactivated.  The default is the value of the :SUPERIOR parameter.
Allow-new-tree:  If this is true, the editor will include the 'New Tree' option in its
    menu.  If getting new trees has nothing to do with an application this should be NIL.
    The default is T."
  (LET	(returned-value)
    (UNLESS frame
      (SETQ frame (ALLOCATE-RESOURCE 'tree-window-resource tv:default-screen)))
    (WHEN adjust-to-sup-size
      (MULTIPLE-VALUE-BIND (width height) (SEND superior :size)
	(SEND frame :set-size width height))
      (MULTIPLE-VALUE-BIND (x y) (SEND superior :position)
	(SEND frame :set-position x y)))
    (SETQ returned-value (*CATCH 'throw-from-handle-node
				 (display-2 frame tree level vertical? edit?
					    application-type init-label
					    allow-new-tree)))
    (tv:delaying-screen-management
      (SEND frame :deactivate)
      (SEND return-window :select))
    (DEALLOCATE-RESOURCE 'tree-window-resource frame)
    returned-value))


(DEFUN display-2
       (window tree level vertical? edit? application-type init-label new-tree? 
	&aux &special
	(*max-level level)
	(*vertical? vertical?)
	(*force-recalculate nil)
	(*tree tree)
	(*system-menu-list (COND ((AND edit? new-tree?) system-menu-list)
				 (edit? (CDR system-menu-list))
				 (new-tree? (APPEND (LIST (CAR system-menu-list))
						    (CDDR system-menu-list)))
				 (t (CDDR system-menu-list))))
	(*first t)
	(*root-node nil)
	(*tree-window   window)
	(*utility-pane  (SEND *tree-window :get-pane 'utility-pane))
	(*display-pane  (SEND *tree-window :get-pane 'display-pane))
	(*world         (SEND *display-pane :world))
	(*aux-menu-pane (SEND *tree-window :get-pane 'aux-menu-pane))
	(*menu-pane     (SEND *tree-window :get-pane 'menu-pane))
	(*scroll-window (tv:make-window 'temporary-scroll-window
					:character-width *scroll-window-width*
					:character-height *scroll-window-height*)))
  "Displays the tree editor.  Creates many special variables unique to each tree editor
so several can coexist."
  (SEND *display-pane :set-up-window-for-user-type application-type init-label
	tree)
  (set-up-system-menu)
  (display-new-tree))


(DEFUN display-new-tree ()
  "Called to generate a new tree from scratch, either at the very beginning, or when a
new tree is entered.  If *TREE equals 'EXIT, then the tree window is exited."
  (DECLARE (SPECIAL *tree-window *tree *root-node *display-pane))
  (DO () ((EQ *tree 'exit)
	  (IF *root-node (SEND *root-node :data)))
    (create-graphics-entity *tree)
    (SEND *display-pane :default-window)
    (tree-redraw)
    (SETQ *tree (system-menu-loop))))

(DEFUN my-break (&rest ignore)
  (LET ((inhibit-scheduling-flag nil)
	(tv:kbd-intercepted-characters (PUSH '(#\abort tv:kbd-intercept-abort)
					       tv:kbd-intercepted-characters)))
    (BREAK)
    (WHEN (TYPEP tv:selected-window 'tree:temporary-typeout-window)
      (LET ((foowind tv:selected-window))
	(SEND foowind :deactivate)
	(SEND (SEND foowind :superior) :select)))))

(DEFUN system-menu-loop ()
  "This is the main loop.  When exited, if 'EXIT is returned, then the tree editor is
exited.  If anything else is returned, then what is returned is interpreted as a new tree
to be displayed."
  (DECLARE (SPECIAL *tree-window *menu-pane *force-recalculate *first
		    *utility-pane *root-node *display-pane *tree))
  (LET ((si:*break-bindings* (cons `(*terminal-io* ,(send *display-pane :typeout-window))
				   (copy-list si:*break-bindings*)))
	(tv:kbd-intercepted-characters (APPEND '((#\break my-break) (#\abort beep))
					       tv:kbd-intercepted-characters)))
    (SEND *tree-window :select)
    (SEND *menu-pane :set-highlighted-values nil)
    (SEND *utility-pane :set-item-list nil)
    (UNLESS *first (set-up-system-menu))
    (DO (blip) (nil)
      (SETQ blip (SEND *tree-window :any-tyi))
      (COND ((NUMBERP blip)
	     (CASE (CODE-CHAR blip)
	       (#\end (RETURN 'exit))
	       (#\clear-screen (SEND *tree-window :refresh)
			       (SEND *tree-window :select))
	       (otherwise (BEEP))))
	    ((ATOM blip)
	     (IF (EQ blip 'T)
		 nil
		 (BEEP)))
	    (t (LET ((type-of-blip (CAR blip)))
		 (selectq type-of-blip
			  (:menu
			    (LET ((item-chosen (THIRD (SECOND blip)))
				  (button-pressed (THIRD blip)))
			      (SELECTOR item-chosen equal
				("EDIT" (IF *root-node (set-up-edit-menu)
					    (complain '("NO TREE TO EDIT"))))
				("PARAMETERS" (edit-parameters))
				("REDRAW" (SETQ *force-recalculate t)
					  (tree-redraw))
				("NEW" (LET ((new-tree (SEND (SEND *display-pane
								   :accessor-functions)
							     :get-new-tree)))
					 (IF new-tree (RETURN new-tree) (BEEP))))
				("EXIT-SYSTEM" (RETURN 'exit))
				("EXIT-EDIT" (set-up-system-menu))
				("RIGHT" (pan-window 1 0 button-pressed))
				("LEFT" (pan-window -1 0 button-pressed))
				("UP" (pan-window 0 -1 button-pressed))
				("DOWN" (pan-window 0 1 button-pressed))
				("RESET" (return-to-default-window))
				("ZOOM-IN" (zoom-window 'IN button-pressed))
				("ZOOM-OUT" (zoom-window 'OUT button-pressed))
				("FILL" (fill-window))
				(t (SEND *menu-pane :set-highlighted-values
					 (LIST item-chosen))
				   (set-up-edit-menu)))))
			  (:typeout-execute		     
			    (IF (EQ (handle-typeout-execute blip) 'new-tree)
				(RETURN *tree)))
			  (otherwise (BEEP)))))))))


(DEFUN set-up-system-menu ()
  "Sets the system menu in the main menu pane."
  (DECLARE (special *menu-pane *display-pane *system-menu-list))
  (SEND *display-pane :set-item-type-alist
	(SEND *display-pane :system-item-type-alist))
  (SEND *menu-pane :set-label "MAIN MENU")
  (SEND *menu-pane :set-item-list *system-menu-list))


(DEFUN set-up-edit-menu ()
  "Sets the edit menu in the main menu pane."
  (DECLARE (SPECIAL *menu-pane *utility-pane *display-pane))
  (SEND *display-pane :set-item-type-alist
   	(IF (EQUAL "ADD-BROTH" (CAR (SEND *menu-pane :highlighted-values)))
	    (SEND *display-pane :add-brother-item-type-alist)
	    (SEND *display-pane :edit-item-type-alist)))
  (SEND *menu-pane :set-label "EDITING MENU")
  (SEND *menu-pane :set-item-list edit-menu-list))


(DEFUN handle-typeout-execute (blip)
  "Called when a node is moused.  If in the edit menu loop the highlighted action in the
edit menu is taken on the node.  If in the system menu loop HANDLE-NODE is called with
the information in the mouse blip.  If HANDLE-NODE returns NIL, then the monitor beeps.
If HANDLE-NODE returns T, then the assumption is that HANDLE-NODE did all of the
node processing itself.  If 'NEW-TREE was returned, that keyword is passed back to the
SYSTEM-MENU-LOOP function where it creates a new tree out of what is in the *TREE
variable.  The assumption there is HANDLE-NODE put the new tree data into *TREE.
Anything else returned is passed to the scroll window as an item.  The item must be an
item acceptable to TV:SCROLL-WINDOW which is documented in the Window System
manual."
  (DECLARE (SPECIAL *scroll-window *display-pane *menu-pane *force-recalculate))
  (LET* ((choice (SECOND blip))
	 (instance (THIRD blip))
	 (button (FOURTH blip))
	 (mode (CAR (SEND *menu-pane :highlighted-values)))
	 (type (SEND instance :type))
	 (user-data (SEND instance :data))
	 (text (SEND (SEND *display-pane :accessor-functions) :handle-node
		     user-data type choice instance)))
    (COND ((NULL text)
	   (SELECTOR mode equal
	     ("DELSUBTREE" (delete-subtree instance))
	     ("DELNODE"    (delete-node instance))
	     ("ADDBEFORE"  (add-node-before instance))
	     ("ADDAFTER"   (add-node-after instance))
	     ("ADD-BROTH"  (add-brother-node instance button))
	     ("REDRAW"     (SETQ *force-recalculate t)
			   (tree-redraw))
	     (otherwise    (BEEP))))
	  ((EQ text t) nil)
	  ((EQ text 'new-tree) text)
	  (t (display-scroll-window text instance)))))


(DEFUN display-scroll-window (item-to-display instance)
  "Put the item-to-display in a scroll window.  The assumption is the item is already in
a form acceptable to the window."
  (DECLARE (SPECIAL *scroll-window))
  (SEND *scroll-window :set-items item-to-display)
  (SEND *scroll-window :set-label
	`(:font fonts:cptfontb :string ,(SEND instance :name)))
  (SEND *scroll-window :expose-near '(:mouse)))


(DEFUN edit-parameters
       (&aux &special (temp-*vertical? *vertical?)
		      (temp-*min-breadth *minimum-breadth-spacing*)
		      (temp-*min-depth *minimum-depth-spacing*))
  "Called when 'EDIT PARAMETERS' is moused from the main menu.
Lets the user change the orientation of the display and how far apart the nodes are
spaced."
  (DECLARE (SPECIAL *vertical? *display-pane *force-recalculate))
  (*catch 'abort
    (tv:choose-variable-values
      '((temp-*vertical? "Orientation"
			 :assoc (("Vertical" . t) ("Horizontal")))
	(temp-*min-depth "Minimum depth spacing" :number)
	(temp-*min-breadth "Minimum breadth spacing" :number))
      :function 'verify-edit-parameters
      :label "Modify Display Parameters"
      :margin-choices '("Done" ("Abort" (*throw 'abort nil))))
    (LET ((redraw nil))
      (COND ((NEQ temp-*vertical? *vertical?)
	     (SETQ *force-recalculate t redraw 'center)
	     (SEND *display-pane :set-starting-x
		   (IF *vertical? *starting-point-offset*
		     (/ (SEND *display-pane  :width) 2.)))
	     (SEND *display-pane :set-starting-y
		   (IF *vertical? (/ (SEND *display-pane :height) 2.)
		     *starting-point-offset*)))
	    ((NEQ temp-*min-breadth *minimum-breadth-spacing*)
	     (SETQ *force-recalculate t redraw 'no-center))
	    ((NEQ temp-*min-depth *minimum-depth-spacing*)
	     (SETQ *force-recalculate t redraw 'no-center)))
      (SETQ *vertical? temp-*vertical?
	    *minimum-breadth-spacing* temp-*min-breadth
	    *minimum-depth-spacing* temp-*min-depth)
      (COND ((EQ redraw 'center)
	     (SEND *display-pane :default-window)
	     (tree-redraw))
	    ((EQ redraw 'no-center)
	     (tree-redraw))))
    (set-up-system-menu)))


(DEFUN verify-edit-parameters (window var old new)
  "Imposes restrictions on what parameters are valid to choose."
  (DECLARE (SPECIAL temp-*vertical? temp-*edit? temp-*min-depth
		    temp-*min-breadth))
  window				; so compiler won't complain
  (COND ((OR (AND (EQ var 'temp-*min-breadth) (< new 0))
	     (AND (EQ var 'temp-*min-depth) (< new 0)))
	 (BEEP)
	 (SET var old)
	 nil)))


(DEFUN create-graphics-entity (tree)
  "Creates a tree out of the tree data passed to it.  Puts the root node data into
*ROOT-NODE, then recurses from root node on down until all the children have been
created as graphics entities.  They are not yet positioned in relation to each other."
  (DECLARE (SPECIAL *root-node *vertical? *display-pane))
  (SETQ *root-node			   
	(LET* ((accessors (SEND *display-pane :accessor-functions))
	       (top (SEND accessors :first-node tree))
	       (print-name (SEND accessors :print-name top)))
	  (MAKE-INSTANCE 'tree-node
			 :data top
			 :edge-color gwin:black
			 :weight 0.
			 :level 1
			 :entities (picture-from-print-name print-name top)
			 :name (name-from-print-name print-name)
			 :parent nil
			 :type (SEND accessors :find-type top)
			 :highlighted-p (SEND accessors :highlight-function
					       top))))
  (expand-node *root-node 2.))
