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

;;;
;;;      BASIC FUNCTIONS
;;; These are functions used throughout the tree editor.  Generally these shouldn't be called by the
;;; user accessors.  Functions usable by an interface are in the file functions-for-accessors.
;;;
;;;         Functions in this file:
;;;              expand-node
;;;              get-new-node
;;;              make-tree-modified
;;;              name-from-print-name
;;;              picture-from-print-name
;;;              print-this-level
;;;              set-coordinates
;;;


(DEFUN expand-node (parent level &optional (strict-level nil))
  "EXPAND-NODE does the dirty work of transforming the user's representation of the
tree into one that can be displayed by the graphics window.  At this point in the tree
creation procedure, the nodes have not yet been placed.  However, the size of each node
is automatically calculated by the :INIT method for the GED:TEXT-MIXIN being used.
Returns the modified parent after 'children' and 'parent' handling has been completed."
  (DECLARE (SPECIAL *vertical? *display-pane))
  (COND ((print-this-level level)
	 (COND ((SEND parent :children-set-p)
		(IF (NULL (SEND parent :print-children))
		    (SEND parent :set-print-children (SEND parent :children)))
		(LOOP for kid in (SEND parent :children)
		      do (expand-node kid (1+ level) strict-level)))
	       (t (LET* ((accessors (SEND *display-pane :accessor-functions))
			 (kids	    (SEND accessors :children-from-data
					  (SEND parent :data))))
		    (IF kids
			(SEND parent :set-children
			      (LOOP for child in kids
				    for p-name = (SEND accessors :print-name
						       child)
				    for kid-node = (MAKE-INSTANCE 'tree-node
						     :data child
						     :edge-color gwin:black
						     :weight 0
						     :level level
						     :entities (picture-from-print-name
								 p-name child)
						     :name (name-from-print-name p-name)
						     :parent parent
						     :type (SEND accessors :find-type
								 child)
						     :highlighted-p (SEND accessors
									  :highlight-function
									  child))
				    collect kid-node
				    do      (expand-node kid-node (1+ level)
							 strict-level)))
			(SEND parent :set-children nil))))))
	((AND strict-level (SEND parent :print-children))
	 (SEND parent :set-print-children nil)))
  parent)


(DEFUN get-new-node ()
  "This returns a new node with no children, to be inserted into the tree.
For this to work the user must have defined a 'get-user-node' function that returns the
user data to be stored in the node."
  (DECLARE (SPECIAL *display-pane))
  (LET* ((accessors (SEND *display-pane :accessor-functions))
	 (new-node  (SEND accessors :get-user-data))
	 (p-name    (SEND accessors :print-name new-node)))
    (MAKE-INSTANCE 'tree-node
		   :data          new-node
		   :edge-color    gwin:black
		   :weight        0.
		   :entities      (picture-from-print-name p-name new-node)
		   :name          (name-from-print-name p-name)
		   :highlighted-p (SEND accessors :highlight-function
					new-node))))


(DEFUN make-tree-modified (node)
  "Makes the tree modified starting at this node.
Due to the layout algorithm used, this means that the modified flag is set for this node
and its parent, its parent's parent, etc., all the way up the tree."
  (COND ((NULL node)	       nil)
	((SEND node :modified) nil)
	(t		       (SEND node :set-modified t)
			       (make-tree-modified (SEND node :parent)))))


(DEFUN name-from-print-name (print-name)
  "Returns the name of a node extracted from its print name.
If the print-name is an atom, it is used, but if it is a list the car of the list is used."
  (COND ((ATOM print-name) print-name)
	(t		   (CAR print-name))))


(DEFUN picture-from-print-name (print-name node)
  "Returns the graphics objects that will be displayed to represent the node.
If print-name is an atom, a text object is returned whose string is the print-name.  If
it is a list, the cdr of the list is returned, as it is assumed the cdr is a list of
graphics objects."
  (DECLARE (SPECIAL *display-pane))
  (COND ((ATOM print-name)
	 (LIST (MAKE-INSTANCE 'gwin:text
			      :text-string print-name
			      :font-name   (OR (SEND (SEND *display-pane
							   :accessor-functions)
						     :font-type node)
					       'gwin:standard-font))))
	(t (CDR print-name))))


(DEFUN print-this-level (level)
  "Returns nil if level is greater than the highest level to be displayed, otherwise T."
  (DECLARE (SPECIAL *max-level))	   
  (OR (= *max-level 0) (<= level *max-level)))


(DEFUN set-coordinates (node-list depth-so-far)
  "Sweeps through the tree and sets all of the coordinates to their final values
assuming that for each of nodes in the tree, OFFSET-TO-PARENT is correct."
  (LOOP with center-of-self
	with node
	for node-info in node-list
	do (SETQ node (CAR node-info))
	do (SETQ center-of-self (+ (CDR node-info)
				   (SEND node :offset-to-parent)))
	do (SEND node :set-width-coord center-of-self depth-so-far)
	append (LOOP for kid in (SEND node :print-children)
		     collect (CONS kid center-of-self)) into new-node-list
	maximize (SEND node :depth) into max-depth
	finally (IF new-node-list
		    (set-coordinates new-node-list (+ depth-so-far max-depth
						  *minimum-depth-spacing*)))))
