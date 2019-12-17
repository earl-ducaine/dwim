;;; -*- Mode:Zetalisp; Package:TREE; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

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
;;; These are sample accessors for handling data structures that are symbols or lists of symbols.
;;; The data is arranged so the car of a list is the current node and the cdr gives the children.  For
;;; example, the structure:
;;;
;;;	(This is (a demonstration) (of (the tree editor)))
;;;
;;; would be displayed as
;;;
;;;	------------ This -----------
;;;	|	      |		    |
;;;	is	      a		   of
;;;		      |		   |
;;;		  demonstration	 - the - 
;;;				 |    |
;;;			        tree  editor
;;;;
;;; The symbols can also be strings.
;;;

;;;
;;; *** 1 ***
;;; The flavor name of your application type must be pushed onto a global list of known applications,
;;; called *KNOWN-APPLICATION-TYPES*.  When you build a tree editor if you don't specify the
;;; application type, you will be queried from this list.
;;;

(PUSH 'tree:string-display *known-application-types*)


;;;
;;; *** 2 ***
;;; Next you need to define the keywords for each mouse button.  The constant you store this alist
;;; in can have any name you like, since you write the method that retrieves this value.  Make sure
;;; the string that is displayed in the who-line prefaces the description for each button with a
;;; description like L: L2: M: M2: R: R2:.  This is important because when editing, the only commands
;;; you have defined that are still in effect are those attached to the right mouse button, and when
;;; the tree program adjusts the who-line documentation it looks for the "R1:" part of your string to
;;; reuse.  The form for the alist is as follows:
;;;
;;;	(defconst application-alist-name
;;;		'((node-type1
;;;		   (single-left-keyword double-left-keyword
;;;		   single-middle-keyword double-middle-keyword)
;;;		   "String to be displayed in who-line for this node type"
;;;		   (item for right-click menu)
;;;		   (item for right-click menu)
;;;		   . . . )
;;;		 (node-type2
;;;		   (single-left-keyword double-left-keyword
;;;		    single-middle-keyword double-middle-keyword)
;;;		   "String to be displayed in who-line for this node type"
;;;		   (item for right-click menu)
;;;		   (item for right-click menu)
;;;		   . . . )
;;;		 . . . ))
;;;

(DEFCONST string-display-alist
	  '((non-terminal
	      (exp-cont nil move nil)		; in the order (L L2 M M2)
	      (:mouse-1-1 "Expand or contract the node."
	       :mouse-2-1 "Move the node to front of display"
	       :mouse-3-1 "Menu of examinable data for this node.")
	      ("NODE DATA" :value user-data "Show the user data in selected node.")
	      ("FILE WHERE DEFINED" :value origin
	       :documentation "Show the file name where the displayed string was defined"))
	    (terminal
	      (nil nil move nil)
	      (:mouse-2-1 "Move the node to front of display"
	       :mouse-3-1 "Menu of examinable data for this node.")
	      ("NODE DATA" :value user-data "Show the user data in selected node")
	      ("FILE WHERE DEFINED" :value origin
	       :documentation "Show the file name where the displayed string was defined")
	      ("RETURN THIS NODE" :value return-node "Exit with the value of this node")))
 "This is the alist that determines what action is taken when a node is clicked on.
Clicking left on non-terminal nodes expands them if they have been contracted, or
contracts them if they were expanded.  Single left on terminal nodes does nothing.
Single middle moves that node to the middle left of the screen if the display is
horizontal, otherwise to the middle top.  Clicking right puts up a menu of things you can
examine.  The one selected will appear in a pop-up scrollable window.  For both
non-terminal and terminal nodes the file where the node data was defined can be shown
if the attribute is there.  Nodes also allow the user data they contain to be examined.")


;;;
;;; *** 3 ***
;;; Define the flavor for your application.  This flavor needs no mixins and is used only to collect your
;;; application functions into one entity.  It also needs no instance variables.  You can have one
;;; called ITEM-TYPE-ALIST if you want, and set that equal to the alist constant you defined above.
;;; Or you can have no instance variables and simply write a method called :ITEM-TYPE-ALIST that
;;; returns your alist constant.
;;;

(DEFFLAVOR string-display
	   ((item-type-alist string-display-alist))	; This can either be an
							; instance variable or you can
							; define a method
							; :ITEM-TYPE-ALIST that
							; returns your alist constant.
	   ()
  :gettable-instance-variables
  (:documentation "Collects the user written functions and methods for one application
together into one flavor.  This allows several types of editors to coexist at the same
time though they use different accessors."))


;;;
;;; *** 4 ***
;;; Given the data you originally passed to the tree editor when you called it, extract the amount you
;;; want stored in the root node of the tree and return that.  There needs to be enough information
;;; in it to be able to find its children.
;;;

(DEFMETHOD (string-display :first-node) (node)
  "Returns the data to be stored in the root node of the tree.  Since the car of the
tree is the current node and the cdr is the children, all the data needs to be stored in
the first node.  So the whole tree is returned."
  node)


;;;
;;; *** 5 ***
;;; Given the data stored in any node in your tree, return a list of its children. Each element in the
;;; list should be the part of your data you want stored in that child. This method will be called again
;;; on each element in the list, so there needs to be enough information to be able to find their
;;; children as well.
;;;

(DEFMETHOD (string-display :children-from-data) (node)
  "If node is a list, return the cdr, which is the children.  Otherwise return NIL for
there are no children."
  (IF (LISTP node)
      (CDR node)
      nil))


;;;
;;; *** 6 ***
;;; Given a node, return the print name.  If you return a string, the data will be displayed as a text
;;; object showing that string.  Otherwise you must return a list, where the car is the name of this
;;; node and the cdr is a list of graphics objects useable by the Graphics Window system (that
;;; system is available to you because you couldn't be running this program without it: the tree editor
;;; is built on the Graphics Window system).  This example only returns strings.
;;;

(DEFMETHOD (string-display :print-name) (node)
  "Return the print name for this node.  If the node is a string, return itself.
If an atom, return the string equivalent.  If a list, return the print name of its car."
  (LET ((temp-node (IF (LISTP node) (CAR node) node)))
    (COND ((STRINGP temp-node) temp-node)
	  ((ATOM temp-node) (GET-PNAME temp-node))
	  (t (*THROW 'throw-from-handle-node "Illegal data")))))


;;;
;;; *** 7 ***
;;; This method is only applicable is you are returning string print names in the above method,
;;; :PRINT-NAME.  If you are defining your own graphics objects instead of using text objects, this
;;; can just return NIL.
;;;

(DEFMETHOD (string-display :font-type) (node)
  "Returns the font that a given node is to be displayed in.  If the node has children,
the font is bolder than a terminal font."
  (COND ((ATOM  node) 'gwin:hl12-font)
	(t	      'gwin:hl12b-font)))


;;;
;;; *** 8 ***
;;;

(DEFMETHOD (string-display :highlight-function) (node)
 "Returns whether or not the node should be highlighted when drawn (that is, should
have a background color).  If NIL, it is not highlighted, otherwise it is.  Every time the
tree is redrawn, what this function returns is evaled.  For this example, any nodes with
children, a type defined as non-terminal, will be highlighted."
   node					; to prevent compiler warnings
  '(IF (EQ type 'non-terminal)
       t
       nil))  


;;;
;;; *** 9 ***
;;;

(DEFMETHOD (string-display :find-type) (node)
  "Every node in the tree has an associated type.  The type of a node dictates whether
or not the node is mouse sensitive, and what action is taken when a node is moused.  In
this example, two types are defined: terminal or non-terminal.  Any node with children is
non-terminal, otherwise it is terminal."
  (IF (ATOM node)
      'terminal
      'non-terminal))


;;;
;;; *** 10 ***
;;; This method handles all the keywords you defined in your alist constant.  Since you defined
;;; different keywords for the different node types, you need to check the CHOICE parameter
;;; according to the TYPE parameter and take the appropriate action.  NODE is the data you have
;;; stored in the current tree node instance.  The whole node flavor instance is in INSTANCE, in case
;;; you need it.
;;;
;;; Returning T from this method means you have done whatever was needed, and to take no further
;;; action.  NIL means nothing was done and the monitor will beep.  Returning 'NEW-TREE will cause a
;;; new tree to be displayed using whatever is in the special variable *TREE as the new root node.
;;; So be sure to put the new tree value there before returning 'NEW-TREE.  Returning anything else
;;; will cause whatever was returned to be displayed in a scrollable window.  You can also throw to
;;; THROW-FROM-HANDLE-NODE.  This makes the tree editor exit with whatever value you throw.
;;;
;;; If you want to add or delete any nodes, here is a list of the editing functions that are also called
;;; by the editor.  They take care of getting a new node (if you are inserting), and calling all the
;;; necessary functions to update the tree structure and redraw the tree.
;;;	delete-node (node-to-delete)
;;;	delete-subtree (node-heading-subtree)
;;;	add-brother-node (selected-node)
;;;	add-node-before (selected-node)
;;;	add-node-after  (selected-node)
;;;

(DEFMETHOD (string-display :handle-node) (node type choice instance)
  "This function handles the item types defined in the STRING-DISPLAY-ALIST.
If 'EXP-CONT was returned, the user clicked left once and wants the selected node
expanded or contracted.  'USER-DATA signifies that the data in the selected node is to
be displayed in a scrollable window, while 'TREE-DATA means the flavor instance of the
node should be described.  'USER-DATA and 'TREE-DATA are item types returned from
single right clicks.  'MOVE means the user clicked single middle, and wants this node
moved to the root node position.  'RETURN-NODE means to exit the tree with this node
value, and 'DELETE means to remove this node from the tree."
  (DECLARE (special *root-node))
  (CASE type
    (non-terminal
     (CASE choice
       (exp-cont  (expand-contract-with-redraw instance 1 t t)
		  t)
       (move	  (move-to-front instance)
		  t)
       (user-data (LIST nil 
			(string-item "NODE DATA:")
			(string-item " ")
			(grind-item node)))
       (origin	  (LIST nil
			(string-item "Defining source file:")
			(string-item " ")
			(string-item (IF (AND (ARRAYP (CAR node))
					      (ARRAY-HAS-LEADER-P (CAR node)))
					 (ARRAY-LEADER (CAR node) 0)
					 "Source file attribute not applicable"))))))
    (terminal
     (CASE choice
       (move	    (move-to-front instance)
		    t)
       (user-data   (LIST nil 
			  (string-item "NODE DATA:")
			  (string-item " ")
			  (grind-item node)))
       (origin	    (LIST nil
			  (string-item "Defining source file:")
			  (string-item " ")
			  (string-item (IF (AND (ARRAYP node)
						(ARRAY-HAS-LEADER-P node))
					   (ARRAY-LEADER node 0)
					   "Source file attribute not applicable"))))
       (return-node (*THROW 'throw-from-handle-node node))))
    (otherwise nil)))


;;;
;;; *** 11 ***
;;; Get more data for this application in whatever manner you desire, so it is in the same form as
;;; data given to the tree editor when you first call it.  Return that data, and a new tree will be
;;; displayed with it.  This is called by the "New Tree" menu option.  If you don't want this feature,
;;; return NIL and the tree editor will give the user a warning beep if he tries to execute this.
;;;

(DEFMETHOD (string-display :get-new-tree) ()
  "Returns new user data to be displayed in tree form.  If you don't want this feature
return NIL."
  (LET (new-tree)
    (DO-FOREVER
      (SETQ new-tree
	    (tv:get-line-from-keyboard
	      "Enter a new tree made of symbols, strings, or lists of symbols and strings"
	      tv:mouse-sheet #'READ '(:mouse)))
      (IF (OR (LISTP new-tree)
	      (STRINGP new-tree)
	      (ATOM new-tree))
	  (RETURN new-tree)))))


(DEFUN user:run-string-displayer (&optional (tree nil))
  "Run a sample tree editor interface, that displays strings or symbols and lists of
strings and symbols."
  (display (IF tree
	       tree
	       '("S" ("NP" ("N" "Mary"))
		     ("VP" ("V" "had")
			   ("NP" ("ART" "a")
				 ("NP" ("ADJ" "little")
				       ("N" "lamb"))))))
	   :vertical? t :application-type 'string-display :edit? t))
