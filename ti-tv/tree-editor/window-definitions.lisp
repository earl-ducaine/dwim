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
;;; 9-14-88   MAY		Removed ADVISE call and changed tv:typeout-menu-choose, too. SPR 7437

(defflavor TEMPORARY-TYPEOUT-WINDOW
	   ()
	   (w:temporary-window-mixin
	    w:typeout-window))

(DEFMETHOD (temporary-typeout-window :after :expose) (&rest ignore)
  (UNLESS (EQ self tv:selected-window) (SEND self :select)))
;;;
;;; Esssentially the same as a GED window, except mouse-sensitivity is added and mouse-selection is
;;; added. 
;;;


(DEFFLAVOR tree-display-pane
	   ((temp-item-list		 nil)	; Stores the mouse-sensitive items.  This
						; is necessary because the
						; TV:BASIC-MOUSE-SENSITIVE-ITEMS
						; flavor puts an :AFTER method on
						; refresh that sets the list of sensitive
						; items to NIL, while the
						; GWIN:GRAPHICS-WINDOW flavor just
						; defined them in its :AFTER method.
						; Can't reverse the order the flavors
						; are mixed in because of their mouse
						; process definitions, so this kludge
						; saves them for restoration after the
						; refresh.  See (TREE-NODE :AFTER
						; :DRAW) and PUT-TREE-IN-WINDOW.
	    (system-item-type-alist	 nil)
	    (edit-item-type-alist	 nil)
	    (add-brother-item-type-alist nil)
	    (starting-x			 nil)	; Position in window coordinates of the
						; root of the tree. 
	    (starting-y			 nil)	; Position in window coordinates of the
						; root of the tree. 
	    (accessor-functions		 nil))	; Stores the user accessor functions.
	   (w:basic-mouse-sensitive-items
	    w:window-with-typeout-mixin
	    gwin:graphics-window
	    w:select-mixin
	    w:box-label-mixin)
  :initable-instance-variables
  :settable-instance-variables
  (:default-init-plist
    :blinker-p nil
    :typeout-window '(temporary-typeout-window :deexposed-typeout-action (:expose-for-typeout)))
  (:documentation "The display window for the tree editor.  Uses the graphics window
system and mouse-sensitive-items."))

(DEFMETHOD (tree-display-pane :after :init) (&rest ignore)
  (send tv:typeout-window :make-complete))

(defmethod (tree-display-pane :FLUSH-TYPEOUT) ()
  (when (send w:typeout-window :active-p)
    (send w:typeout-window :deactivate)))

(DEFMETHOD (tree-display-pane :add-to-temp-item-list)
	   (type item left top right bottom)
  "Adds this item to the list of mouse sensitive items.  They are saved here because in
the TV:BASIC-MOUSE-SENSTITIVE-ITEMS :AFTER :REFRESH method they are lost if kept
in the TV:ITEM-LIST instance variable of TREE-DISPLAY-PANE.  The TEMP-ITEM-LIST is
restored to ITEM-LIST in an :AFTER :REFRESH method of TREE-DISPLAY-PANE."
  (PUSH (LIST type item left top right bottom) temp-item-list))


(DEFMETHOD (tree-display-pane :set-item-list-to-temp-list) ()
  "Puts the mouse-sensitive item list in tv:item-list where it belongs.  It was stored in
TEMP-ITEM-LIST because TV:ITEM-LIST is set to NIL by the :AFTER :REFRESH method
of TV:BASIC-MOUSE-SENSITIVE-ITEMS, and the :AFTER :REFRESH method of
GWIN:GRAPHICS-WINDOW had just defined them."
  (SETQ tv:item-list temp-item-list))


(DEFMETHOD (tree-display-pane :before :refresh) (&optional IGNORE)
  "Clean out sensitive item list before redrawing."
  (SEND self :flush-typeout)
  (SETQ temp-item-list nil))


(DEFMETHOD (tree-display-pane :after :refresh) (&optional IGNORE)
  "Get sensitive item list from TEMP-ITEM-LIST, where it was temporarily stored and
put it in TV:ITEM-LIST where it belongs.  See documentation for :ADD-TO-TEMP-ITEM-LIST."
  (SEND SELF :set-item-list-to-temp-list))


(DEFMETHOD (tree-display-pane :mouse-click) (button x y)
  "Handles the mouse clicks.  If a mouse-sensitive-item has been selected, this puts a
mouse blip in the io buffer with the right keyword for the mouse button that was
clicked.  The left and middle clicks are redefined here because the item type alist is in a
different form than the TV:BASIC-MOUSE-SENSITIVE-ITEM flavor expects (four keywords
instead of one for single left).  Single right is redefined because the tree editor allows
keywords when defining the wholine string in the ITEM-TYPE-ALIST, and
TV:BASIC-MOUSE-SENSITIVE-ITEMS doesn't."
  (LET* ((item (SEND SELF :mouse-sensitive-item x y))
	 (item-type (ASSQ (tv:typeout-item-type item) tv:item-type-alist)))
    (WHEN item-type
      (COND ((EQUAL button #\mouse-1-1)
	     (SEND SELF :force-kbd-input
		   (LIST :typeout-execute (FIRST (CADR item-type))
			 (tv:typeout-item-item item) '#\mouse-1-1)))
	    ((EQUAL button #\mouse-1-2)
	     (SEND SELF :force-kbd-input
		   (LIST :typeout-execute (SECOND (CADR item-type))
			 (tv:typeout-item-item item) '#\mouse-1-2)))
	    ((EQUAL button #\mouse-2-1)
	     (SEND SELF :force-kbd-input
		   (LIST :typeout-execute (THIRD (CADR item-type))
			 (tv:typeout-item-item item) '#\mouse-2-1)))
	    ((EQUAL button #\mouse-2-2)
	     (SEND SELF :force-kbd-input
		   (LIST :typeout-execute (FOURTH (CADR item-type))
			 (tv:typeout-item-item item) '#\mouse-2-2)))
	    ((EQUAL button #\mouse-3-1)
	     (PROCESS-RUN-FUNCTION "Menu Choose" #'TV:TYPEOUT-MENU-CHOOSE
				   (CDDDR ITEM-TYPE) ITEM SELF
				   ;; Compute a label for the menu.
				   (OR (SEND (SECOND ITEM) :SEND-IF-HANDLES
					     :STRING-FOR-PRINTING)
				       (SEND (SECOND ITEM) :SEND-IF-HANDLES  :NAME)
				       " "))
	     t)
	    (t nil)))))


(DEFUN old-right-string (item-description)
   "Searches item-description for R:.  Returns the entire substring following the keys
    R: or the empty string if the keys are not found.  Called by :set-up-window-for-
   user-type to initialize editing wholine documentation for the right button."
  (LET* ((old-string (THIRD item-description))
	 pos)
    (COND ((LISTP old-string)
	   (SETQ pos (POSITION :mouse-3-1 old-string :test #'eq))
	   (IF pos
	       (NTH (1+ pos) old-string)
	       ""))
	  (t (SETQ pos (STRING-SEARCH "R:" old-string))
	     (IF pos
		 (SUBSTRING old-string pos)
		 "")))))


(DEFMETHOD (tree-display-pane :set-up-window-for-user-type)
	   (accessor-type label tree)
  "Initializes the display pane so its EDIT-ITEM-ALIST is made of the user defined alist
for the right buttons, and the left and middle buttons are redefined for editing.  Also
determines what the accessor functions are for this tree by querying the user if the
ACCESSOR-TYPE is NIL."
  (DECLARE (special *vertical? *tree))
  (SEND SELF :set-label label)
  (SETQ starting-x (IF *vertical?
		       (/ (SEND SELF :width) 2.)
		       *starting-point-offset*)
	starting-y (IF *vertical?
		       *starting-point-offset*
		       (/ (SEND SELF :height) 2.)))
  (UNLESS accessor-type
    (SETQ accessor-type (IF (= (LENGTH *known-application-types*) 1.)
			    (CAR *known-application-types*)
			    (w:menu-choose *known-application-types*
			      :label "Select the application for your data."))))
  (SETQ accessor-type (MAKE-INSTANCE accessor-type))
  (AND (NULL tree) (SETQ *tree (SEND accessor-type :get-new-tree)))
  (LET ((item-alist (SEND accessor-type :item-type-alist)))
    (SETQ accessor-functions     accessor-type
	  tv:item-type-alist     item-alist
	  system-item-type-alist item-alist
	  add-brother-item-type-alist
	    (LOOP with new-list
		  for node in item-alist
		  DO (SETQ new-list
			   (APPEND new-list
				   (LIST (APPEND (LIST (FIRST node)) '(nil)
						 (LIST `(:mouse-1-1 "Insert before this node"
							 :mouse-2-1 "Insert after this node"
							 :mouse-3-1 ,(old-right-string node)))
						 (NTHCDR 3 node)))))
		  finally (RETURN new-list))
	  edit-item-type-alist
	    (LOOP with new-list
		  for node in item-alist
		  DO (SETQ new-list
			   (APPEND new-list
				   (LIST (APPEND (LIST (FIRST node)) '(nil)
						 (LIST `(:mouse-1-1 "Select a node to edit"
							 :mouse-2-1 "Select a node to edit"
							 :mouse-3-1 ,(old-right-string node)))
						 (NTHCDR 3 node)))))
		  finally (RETURN new-list)))))


(DEFMETHOD (tree-display-pane :after :default-window) (&rest IGNORE)
  "Pans the window so the origin is centered at the top if the display is vertical, or
centered in the middle of the right edge if horizontal."
  (DECLARE (SPECIAL *vertical?))
  (MULTIPLE-VALUE-BIND (width height) (SEND SELF :size)
    (IF *vertical?
	(SEND SELF :pan (/ width 2.) 0.)
	(SEND SELF :pan 0. (/ height 2.)))))


(DEFMETHOD (tree-display-pane :who-line-documentation-string)
	   (&aux item item-type x y)
  "Return the documentation string for the selected item.  Redefines the
TV:BASIC-MOUSE-SENSITIVE-ITEMS method so keywords can be used in specifying the
documentation."
  (MULTIPLE-VALUE (x y) (tv:sheet-calculate-offsets self tv:mouse-sheet))
  (SETQ x (- tv:mouse-x x)
	y (- tv:mouse-y y))
  (AND (SETQ item (SEND self :mouse-sensitive-item x y))
       (SETQ item-type (tv:typeout-item-type item))
       (SETQ item-type (assq item-type tv:item-type-alist))
       (THIRD item-type)))


;;;
;;; Used for "MAIN MENU" as well as "EDIT MENU."
;;;

(DEFFLAVOR command-menu-pane-center-label ()
	   (tv:centered-label-mixin tv:command-menu-pane
	    tv:menu-highlighting-mixin)
  (:documentation "Menu panes for the tree editor."))


;;;
;;; The basic constraint window used.
;;;

(DEFFLAVOR tree-frame ()
	   (tv:process-mixin
	    tv:select-mixin
	    tv:bordered-constraint-frame-with-shared-io-buffer
	    tv:stream-mixin)
  (:default-init-plist
   :process	'(display-as-process)
   :panes	`((utility-pane command-menu-pane-center-label
				:item-list nil 
				:label "")
		  (display-pane tree-display-pane)
		  (aux-menu-pane command-menu-pane-center-label
				 :item-list ,screen-option-list
				 :label "DISPLAY SCREEN OPTIONS")
		  (menu-pane command-menu-pane-center-label
			     :label "MAIN MENU"
			     :item-list nil))
   :constraints	'((main . ((top-strip display-pane )
			   ((top-strip :horizontal (.20)
				       (utility-pane
					menu-pane
					aux-menu-pane)
				       ((utility-pane .30)
					(menu-pane .40)
					(aux-menu-pane .30)))
			    (display-pane  .80)))))
   :save-bits	:delayed)
  (:documentation "The frame used to display the tree editor."))


(defmethod (tree-frame :DESIGNATE-IO-STREAMS) ()
  (LET ((display-pane (SEND self :get-pane 'display-pane)))
  (setq *standard-input* display-pane)
  (SETQ *standard-output* display-pane)
  (SETQ *terminal-io* display-pane)
  (SETQ *query-io* (send display-pane :typeout-window))
  (setq *debug-io* (send display-pane :typeout-window))))


(DEFMETHOD (tree-frame :process) ()
  "If there is no current process for the tree editor, it is running in the calling
program's process.  Return that as the process."
  (OR tv:process (tv:io-buffer-last-output-process  tv:io-buffer)))


;;;
;;; When a tree-frame is created without a process, it allocates it as a resource using this function,
;;; which creates tree frames without processes.
;;;

(tv:defwindow-resource tree-window-resource () 
  :make-window (tree-frame)
  :reusable-when :deactivated)


;;;
;;; Used for the pop-up window when an item is moused.
;;;

(DEFUN print-item (item ignore window &optional ignore)
  (IF (STRINGP item)
      (SEND window :string-out item)
      (FORMAT window "~s" item)))


(DEFFLAVOR temporary-scroll-window ()
	   (w:hysteretic-window-mixin
	    w:temporary-window-mixin
	    w:function-text-scroll-window
	    w:box-label-mixin
	    w:margin-region-mixin
	    w:window)
  (:default-init-plist :hysteresis		30
		       :label			fonts:cptfontb
		       :print-function          'print-item)
		       
  (:documentation "The pop-up scrollable window used to display items that are clicked
on single right."))


(DEFMETHOD (temporary-scroll-window :who-line-documentation-string) ()
  "To bury this window, move mouse off it.")


(DEFMETHOD (temporary-scroll-window :after :handle-mouse) ()
  "Method that gets rid of the pop-up window when mouse is moved off of it.
It is essential that PROCESS-RUN-FUNCTION is used to deactivate the window because
this method is called in the mouse process."
  (PROCESS-RUN-FUNCTION '(:name "Menu Deactivate" :priority 20.) self :deactivate)
  (SETQ tv:top-item 0))			  


;;;
;;; This is to overcome the fact that the tv:basic-mouse-sensitive-items flavor defines a single right
;;; click on a node to pop up a menu.  If an application wants no menu to pop up on a single right
;;; click, if he lists no menu items for that item type in his item-alist, this will override the function
;;; call.
;;;

; may 9-14-88 changed tv:typeout-menu-choose instead. ADVICE can disappear to easily.
;(ADVISE tv:typeout-menu-choose :before 'check-for-nil nil
;  (WHEN (NULL (SECOND ARGLIST))
;    (BEEP)
;    (RETURN nil)))
