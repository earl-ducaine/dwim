;;;-*- Mode:Zetalisp; Package:TREE; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;;;
;;; The data, information, methods, and concepts contained herein are a valuable
;;; trade secret of Texas Instruments.  They are licensed in confidence by Texas
;;; Instruments and may only be used as permitted under the terms of the
;;; definitive license agreement under which such use is licensed.
;;;
;;;			    RESTRICTED RIGHTS LEGEND
;;;
;;; Use, duplication, or disclosure by the Government is subject to restrictions
;;; as set forth in subdivision (b)(3)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013.
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.  All rights reserved.
;;;

;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 2-13-87   DLS		Original version for Release 3.
;;;

;;;
;;; FLAVOR DISPLAYER INTERFACE
;;; These are the functions for displaying the dependent flavors and the methods of any flavor.
;;; They assume the tree editor is given a flavor name as its tree data.  The flavor is initially
;;; displayed as the root of a tree with all its mixins and their mixins as its children.  Clicking left on
;;; the root changes the tree so the same flavor is still the root, but the children are the flavors
;;; that depend on it.  Other information can be displayed, such as all the methods, combined or not,
;;; of a flavor, and its instance variables and which ones are settable.
;;;

;;;
;;; Add this application definition to the list of known types.
;;;

(PUSH 'tree:flavor-display *known-application-types*)


;;;
;;;Define the mouse button keyword assignments.
;;;

(DEFCONST flavor-display-alist
	  '((depends-on-flavors
	     (switch-to-depended move SHOW-METHODS def-file)
	     (:mouse-1-1 "Display the flavors that depend on this one"
	      :mouse-1-2 "Move this node to front of display"
	      :mouse-2-1 "Show all methods of this flavor"
	      :mouse-2-2 "Show the file where this flavor is defined."
	      :mouse-3-1 "Menu of examinable data for this node.")
	     ("GETTABLE INSTANCE VARIABLES" :value vars
	      :documentation "List the instance variables of this flavor")
	     ("SETTABLE INSTANCE VARIABLES" :value set-var
	      :documentation "List the settable variables for this flavor instance")
	     ("DEPENDED ON BY" :value depended-on-by
	      :documentation "List the flavor types this flavor is depended on by")
	     ("RETURN THIS NODE" :value return-node
	      :documentation "Exit with the value of this node"))
	    (depended-on-by-flavors
	     (switch-to-depends move SHOW-METHODS def-file)
	     (:mouse-1-1 "Display the flavors this one depends on"
	      :mouse-1-2 "Move this node to front of display"
	      :mouse-2-1 "Show all methods of this flavor"
	      :mouse-2-2 "Show the file where this flavor is defined."
	      :mouse-3-1 "Menu of examinable data for this node.")
	     ("GETTABLE INSTANCE VARIABLES" :value vars
	      :documentation "List the instance variables of this flavor")
	     ("SETTABLE INSTANCE VARIABLES" :value set-var
	      :documentation "List the settable variables for this flavor instance")
	     ("DEPENDS ON" :value depends-on
	      :documentation "List the flavor types this flavor depends on")
	     ("RETURN THIS NODE" :value return-node
	      :documentation "Exit with the value of this node"))
	    (component-flavor
	     (exp-cont move SHOW-METHODS def-file)
	     (:mouse-1-1 "Expand or contract this node."
	      :mouse-1-2 "Move this node to front of display"
	      :mouse-2-1 "Show all methods of this node."
	      :mouse-2-2 "Show the file where this flavor is defined."
	      :mouse-3-1 "Menu of examinable data for this node.")
	     ("GETTABLE INSTANCE VARIABLES" :value vars
	      :documentation "List the instance variables of this flavor")
	     ("SETTABLE INSTANCE VARIABLES" :value set-var
	      :documentation "List the settable variables for this flavor instance")
	     ("MAKE THIS THE ROOT NODE" :value show-new-node
	      :documentation "Make this node the root node of a new tree")
	     ("RETURN THIS NODE" :value return-node
	      :documentation "Exit with the value of this node")))
  "This is the alist that defines the keywords returned when a mouse button is clicked,
and determines what message is in the wholine.  For all types of nodes in this
application, a double left click moves the selected node to the front, and a single middle
puts up a temporary display of all the methods in that flavor.  For the root node, a
single left switches what it displays, so if the mixin flavors are being shown the tree
will be rebuilt to display the flavors that depend on that flavor.  If the flavors that
depend on the root node are the ones in the display already, the mixins will be shown
instead.  Think of it as a toggle switch.  A single left on any other node will make a new
tree with the selected node as the root.")


(DEFFLAVOR flavor-display
    ((item-type-alist	 flavor-display-alist)
     (depends-on-display t))			; When true the display is an expansion
						; of the flavor showing what flavors it
						; depends on.  If NIL, the display shows
						; what flavors it is depended on by.
    ()
  :gettable-instance-variables
 (:documentation "Collects the user written functions and methods for one application
together into one flavor.  This allows several types of editors to coexist at the same
time though they use different accessors.  This flavor defines the flavor-displayer
application, that shows flavors, their mixins and dependent flavors, and their component
methods."))


(DEFMETHOD (flavor-display :first-node) (node)
  "Returns the part of the flavor data to be stored in the root node.
In this case NODE is the flavor name, so it is just returned.  Put a 'FIRST property on it
so later methods can tell it is the root node."
  (PUTPROP node t 'FIRST)
  node)


(DEFMETHOD (flavor-display :children-from-data) (node)
  "What this returns depends on the instance variable DEPENDS-ON-DISPLAY.
If it is true, as it is when a displayer is first created, we are showing the mixins of this
flavor.  Therefore get from node, which is the name of a flavor, the flavors it depends
on.  Otherwise get from it the flavors that depend on it."
  (DECLARE (SPECIAL *root-node))
  (LET ((fl (GET node 'si:flavor)))
    (IF depends-on-display
	(si:flavor-depends-on fl)
	(si:flavor-depended-on-by fl))))


(DEFMETHOD (flavor-display :print-name) (node)
  "Returns the print name of the flavor.  If it is horrendously long, break it up at its
hyphens."
  (break-at-hyphens (STRING-APPEND (FORMAT nil "~A:"
					   (PACKAGE-NAME (SYMBOL-PACKAGE node)))
				   (GET-PNAME node))))


;;;
;;; Returns the font that a given node is to be displayed in.
;;;

(DEFMETHOD (flavor-display :font-type) (node)
  "If the node has any children, put it in a bolder font.  Having children means it has
mixins if this is a depends-on display, or if it has flavors that depend on it if this is a
dependent display."
  (LET ((fl (GET node 'si:flavor)))
    (IF (OR (AND depends-on-display (si:flavor-depends-on fl))
	    (AND (NOT depends-on-display) (si:flavor-depended-on-by fl)))
	'gwin:hl12b-font
	'gwin:hl12-font)))


;;;
;;; Returns whether or not the node should be highlighted when drawn (that is, should have a
;;; background color).  If NIL, it is not highlighted, otherwise it is.  Every time the tree is redrawn,
;;; what this function returns is evaled.  Any nodes with children will be highlighted.
;;;

(DEFMETHOD (flavor-display :highlight-function) (node)
  "Determine if the node has any dependent flavors, which will be the children. 
Those with children will be highlighted, so return a T for those, otherwise NIL.  A
function is not returned because having children will not change in this application, since
editing is not allowed."
  (LET ((fl (GET node 'si:flavor)))
    (IF (OR (AND depends-on-display (si:flavor-depends-on fl))
	    (AND (NOT depends-on-display) (si:flavor-depended-on-by fl)))
	t
	nil)))


(DEFMETHOD (flavor-display :find-type) (node)
 "Every node in the tree has an associated type.  The type of a node dictates whether
or not the node is mouse sensitive, and what action is taken when a node is moused.  In
this example, three types are defined: if it is the root node of the tree and the display
is showing the flavors it depends on, the type is DEPENDS-ON-FLAVORS,  if it's the root
node and the display is showing the flavors it is depended on by, the type is
DEPENDED-ON-BY-FLAVORS, otherwise the type is COMPONENT-FLAVOR."
  (COND ((GET node 'FIRST)
	 (REMPROP node 'FIRST)
         (IF depends-on-display
	     'depends-on-flavors
	     'depended-on-by-flavors))
	(t 'component-flavor)))


(DEFMETHOD (flavor-display :handle-node) (node type choice instance &aux pkg)
  "This function handles the item types defined in the previous ITEM-TYPE-ALIST.
If SHOW-METHODS is the chosen keyword, put up a tree displayer with all the
component methods of this flavor."
  (DECLARE (SPECIAL *root-node *tree *display-pane))
  (CASE choice
    (vars           (LIST nil 
			  (string-item "GETTABLE INSTANCE VARIABLES:")
			  (string-item " ")
			  (grind-item (si:flavor-gettable-instance-variables
				       (GET node 'si:flavor)))))
    (set-var        (LIST nil
			  (string-item "SETTABLE INSTANCE VARIABLES:")
			  (string-item " ")
			  (grind-item (si:flavor-settable-instance-variables
				       (GET node 'si:flavor)))))
    (depends-on     (LIST nil
			  (string-item "FLAVORS IT DEPENDS ON:")
			  (string-item " ")
			  (grind-item (si:flavor-depends-on (GET node 'si:flavor)))))
    (depended-on-by (LIST nil
			  (string-item "FLAVORS THAT DEPEND ON IT:")
			  (string-item " ")
			  (grind-item (si:flavor-depended-on-by
				       (GET node 'si:flavor)))))
    (show-new-node  (SETQ *tree node pkg (PACKAGE-NAME (SYMBOL-PACKAGE node)))
		    (SEND *display-pane :set-label
			  (IF depends-on-display
			      (FORMAT nil "Flavors that ~A:~A depends on" pkg node)
			      (FORMAT nil "Flavors that depend on the ~A~A flavor"
				      pkg node)))
		    'new-tree)
    (def-file       (display-scroll-window
		      (LIST nil
			    (string-item "DEFINING SOURCE FILE(S):")
			    (string-item " ")
			    (grind-item
			      (IF (GET node :source-file-name)
				  (SEND (FIRST (LAST (FIRST (GET node
								 :source-file-name))))
					:string-for-printing)
				  "Source file unknown.")))
		      instance) t)
    (show-methods
      (LET ((flavor (GET node 'si:flavor)) temp)
	;;
	;; If the methods aren't composed, compose them.
	;;
	(UNLESS (si:flavor-depends-on-all flavor)
	  (IGNORE-ERRORS (si:compile-flavor-methods-2 node)))
	(SETQ temp (OR (si:flavor-which-operations flavor)
		       (recurse-with-flavor flavor)))
	(SETQ temp
	      (LOOP with lower-level-methods
		    with no-methword
		    for method in temp
		    DO (SETQ lower-level-methods
			     (si:flavor-all-inheritable-methods
			      node method)
			     no-methword nil)
		    (IF (= (LENGTH lower-level-methods) 1.)
			(SETQ no-methword (make-method-string
					   (CAR lower-level-methods)))
			(DOLIST (meth lower-level-methods)
			  (SETQ no-methword
				(APPEND no-methword
					(LIST (make-method-string meth))))))
		    collect no-methword into result
		    finally (RETURN (APPEND (LIST (break-at-hyphens (GET-PNAME node)))
					    result)))
	      pkg (PACKAGE-NAME (SYMBOL-PACKAGE node)))
	(tree:display temp :edit? nil :application-type 'tree:string-display
		      :init-label (FORMAT nil "Methods of the ~A:~A flavor"
					  pkg node)
		      :allow-new-tree nil))
       t)
    (move	    (move-to-front instance) t)
    (return-node    (*THROW 'throw-from-handle-node node))
    (otherwise	    (CASE type
		      (depends-on-flavors
		        (WHEN (EQ choice 'switch-to-depended)
			  (SETQ *tree node
				depends-on-display nil
				pkg (PACKAGE-NAME (SYMBOL-PACKAGE node)))
			  (SEND *display-pane :set-label
				(FORMAT nil "Flavors that depend on the ~A:~A flavor"
					pkg node))
			  'new-tree))
		      (depended-on-by-flavors
		        (WHEN (EQ choice 'switch-to-depends)
			  (SETQ *tree node
				depends-on-display t
				pkg    (PACKAGE-NAME (SYMBOL-PACKAGE node)))
			  (SEND *display-pane :set-label
				(FORMAT nil "Mixins of the ~A:~A flavor" pkg node))
			  'new-tree))
		      (component-flavor
		        (CASE choice
			  (exp-cont
			    (IF (si:flavor-depends-on (GET node 'si:flavor))
				(expand-contract-with-redraw instance 1 t t))
			    t)
			  (show-this-one
			    (SETQ pkg (PACKAGE-NAME (SYMBOL-PACKAGE node))
				  *tree node)
			    (SEND *display-pane :set-label
				  (IF depends-on-display
				      (FORMAT nil "Mixins of the ~A:~A flavor" pkg node)
				      (FORMAT nil "Flavors that depend on the ~A:~A flavor"
					      pkg node)))
			    'new-tree)
			  (otherwise nil)))
		      (otherwise nil)))))


(DEFMETHOD (flavor-display :get-new-tree) ()
  "Get a new flavor from the user by using a pop up type-in window."
  (DECLARE (SPECIAL *display-pane))
  (LET* ((flavor (get-flavor))
	 (pkg (PACKAGE-NAME (SYMBOL-PACKAGE flavor))))
    (SEND *display-pane :set-label
	  (IF depends-on-display
	      (FORMAT nil "Mixins of the ~A:~A flavor" pkg flavor)
	      (FORMAT nil "Flavors that depend on the ~A:~A flavor" pkg flavor)))
    flavor))


(DEFUN get-flavor (&aux fl (prompt-string "Enter a flavor name"))
  "Get a new flavor from the user via a pop up window."
  (DO-FOREVER
    (SETQ fl (tv:get-line-from-keyboard prompt-string tv:mouse-sheet
					#'READ '(:mouse)))
    (COND  ((AND fl (GET fl 'si:flavor))
	    (RETURN fl))
	   (t (SETQ prompt-string
		    (FORMAT nil "~A is not a flavor name.  Try again." fl))))))


(DEFUN make-method-string (method-name)
  "Makes a string out of the method name represented by the passed variable.
Leaves off the :METHOD keyword.  Appends to it the name of the source file where it
was defined."
  (LET* ((method-info (THIRD (CAR (LAST (ASSQ (CAR (LAST method-name))
					      (si:flavor-method-table
						(GET (SECOND method-name)
						     'si:flavor)))))))
	 (pos-in-list (FIND-POSITION-IN-LIST :source-file-name method-info))
	 source-file return-string)
    (WHEN pos-in-list
      (SETQ source-file (NTH (1+ pos-in-list) method-info))
      (IF (LISTP source-file)
	  (SETQ source-file (FIRST (LAST (FIRST source-file)))))
      (SETQ source-file (SEND source-file :string-for-printing)))
    (SETQ return-string
	  (IF (= (LENGTH method-name) 3)
	      (FORMAT nil "~A~%~A" (THIRD method-name) (SECOND method-name))
	      (FORMAT nil "~A    ~A~%~A" (FOURTH method-name)
		      (THIRD method-name) (SECOND method-name))))
    (FILLARRAY (MAKE-ARRAY (LENGTH return-string) :type 'art-string
		 :leader-length 1 :leader-list
		 (LIST (OR source-file "Source file attribute not found.")))
	       return-string)))


(DEFUN user:run-flavor-displayer (&optional (flavor nil))
  "Create a tree displayer with no editing that shows flavors, their mixins, flavors that
depend on them, and all methods for flavors."
  (UNLESS flavor (SETQ flavor (get-flavor)))
  (tree:display flavor
    :application-type 'tree:flavor-display
    :init-label (FORMAT nil "Mixins of the ~A:~A flavor"
			(PACKAGE-NAME (SYMBOL-PACKAGE flavor)) flavor)
    :edit? nil))


(DEFUN recurse-with-flavor (flavor)
  "Return a list of all operations accepted by this flavor, including mixins."
  (LET ((list-so-far (LOOP for operation in (si:flavor-method-table flavor)
			   collect (CAR operation))))
    (DOLIST (fl (si:flavor-depends-on-all flavor))
      (LOOP with comp-flav = (si:flavor-method-table (GET fl 'si:flavor))
	    for meth in comp-flav
	    DO (IF (NULL (MEMQ (CAR meth) list-so-far))
		   (PUSH (CAR meth) list-so-far))))
    (SORT list-so-far #'STRING-LESSP)))


(DEFUN break-at-hyphens (name)
  "Break up name at hyphens if it is too long."
  (LET ((max-line-length 15))
    (IF (AND (> (LENGTH name) max-line-length)
	     (STRING-SEARCH-CHAR '#/- name))
        (LOOP with min-line-length = 4.
	      with start-ptr = 0
	      with end-ptr = max-line-length
	      with result = ""
	      for hyphen = (STRING-REVERSE-SEARCH-CHAR '#/- name end-ptr start-ptr)
	      DO (IF (NULL hyphen)
		     (SETQ hyphen (STRING-SEARCH-CHAR '#/- name (1+ end-ptr))))
	      (COND ((NULL hyphen) (LOOP-FINISH))
		    ((< (- (1- (LENGTH name)) hyphen) min-line-length)
		     (LOOP-FINISH))
		    (t (SETQ result
			     (STRING-APPEND result
					    (SUBSTRING name start-ptr (1+ hyphen))
					    (FORMAT nil "~%"))
			     start-ptr (1+ hyphen)
			     end-ptr  (+ hyphen max-line-length 1.))
		       (IF (>= end-ptr (1- (LENGTH name)))
			   (LOOP-FINISH))))
	      finally (RETURN (STRING-APPEND result (SUBSTRING name start-ptr))))
	name)))

(compile-flavor-methods flavor-display)
