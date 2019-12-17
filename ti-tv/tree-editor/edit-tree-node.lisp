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
;;; EDITING  METHODS FOR DESTRUCTIVELY EDITING A TREE.
;;; For each of the editing operation methods there should be an application dependant method that
;;; appropriately updates the user's data structure if necessary.
;;;


(DEFMETHOD (tree-node :delete-child-subtree) (child)
 "Destructively deletes the subtree headed by the node which is passed to this method.
Returns the altered list of children of the node."
  (SETQ children       (DELETE child children :test #'eq)
	print-children (DELETE child print-children :test #'eq))
  children)


(DEFMETHOD (tree-node :delete-subtree) ()
 "DELETE-SUBTREE destructively deletes this node and all of its children.
Returns the head of the deleted subtree."
  (DECLARE (SPECIAL *force-recalculate *root-node))
  (IF (= level 1)
      (SETQ *root-node nil)		;deleting the whole tree?
      (SEND parent :delete-child-subtree self))
  (make-tree-modified parent)
  (SETQ *force-recalculate t)
  self)


(DEFMETHOD (tree-node :delete-yourself) ()
  "This deletes itself from the tree and shifts all its children up one level.
Its children become the children of its parent.  Returns itself, the deleted node."
  (DECLARE (SPECIAL *force-recalculate *root-node))
  (IF parent
      (LET* ((kids (SEND parent :children))
	     (pos (POSITION self kids :test #'eq)))
	(SEND parent :set-children (APPEND (FIRSTN pos kids)
					   children (NTHCDR (1+ pos) kids))))
      (SETQ *root-node (CAR children)))
  (DOLIST (child children)
    (SEND child :set-parent parent))
  (SEND SELF :adjust-level-of-subtree -1)
  (SETQ *force-recalculate t)
  self)


(DEFMETHOD (tree-node :add-node-before) (inserted-node)
  "Adds the inserted node before itself as its parent in the tree.
Recalculates levels for affected nodes.  Returns the inserted node."
  (DECLARE (SPECIAL *root-node *force-recalculate))
  (SEND inserted-node :set-parent parent)
  (SEND inserted-node :set-children (LIST self))
  (IF parent
      (LET ((children-of-old-parent (SEND parent :children)))
	(SEND parent :set-children
	      (SUBST inserted-node self children-of-old-parent))))
  (SEND inserted-node :set-level level)
  (SETQ parent inserted-node)
  (IF (= level 1.)
      (SETQ *root-node inserted-node))
  (SEND SELF :adjust-level-of-subtree 1)
  (SETQ *force-recalculate t)
  inserted-node)


(DEFMETHOD (tree-node :add-node-after) (inserted-node)
  "Adds the INSERTED-NODE after itself as its only child and assigns its children to the
inserted node.  Recalculates the levels for the downshifted nodes.  The
new, INSERTED-NODE is returned."
  (DECLARE (SPECIAL *force-recalculate))
  (SEND inserted-node :set-parent self)
  (SEND inserted-node :set-children children)
  (SEND inserted-node :set-print-children children)
  (LOOP for child in children
	DO (SEND child :set-parent inserted-node))
  (SETQ children (LIST inserted-node))
  (SETQ print-children children)
  (SEND inserted-node :set-level level)
  (SEND inserted-node :adjust-level-of-subtree 1)
  (SETQ *force-recalculate t)
  inserted-node)


(DEFMETHOD (tree-node :add-brother) (inserted-node button)
  "This adds the new node as a child to the same parent as this node.
The user is asked to use the mouse to specify whether to insert the new node before or
after this node, then the insertion is performed."
  (DECLARE (SPECIAL *vertical? *utility-pane *display-pane *tree-window
		    *force-recalculate))
  (SEND *display-pane :set-item-type-alist nil)
  (LET* ((kid-list (SEND parent :children))
	 (pos (POSITION self kid-list :test #'eq))
	 which-side)
    (SETQ which-side (IF (EQUAL button '#\mouse-1-1)
			 'before
			 'after))	   
    (SEND *display-pane :set-item-type-alist
	  (SEND *display-pane :add-brother-item-type-alist))
    (SEND *utility-pane :set-item-list nil)
    (SELECTQ which-side
      (before (SEND parent :set-children
		    (APPEND (FIRSTN pos kid-list) (LIST inserted-node)
			    (NTHCDR pos kid-list))))
      (after  (SEND parent :set-children
		    (APPEND (FIRSTN (1+ pos) kid-list) (LIST inserted-node)
			    (NTHCDR (1+ pos) kid-list)))))
    (SEND parent :set-print-children (SEND parent :children))
    (SEND inserted-node :set-parent parent)
    (SEND inserted-node :set-children nil)
    (SEND inserted-node :set-level level)
    (SETQ *force-recalculate t)
    inserted-node))


(DEFMETHOD (tree-node :adjust-level-of-subtree) (level-increment)
  "Adds the level increment to the level of the node and its children.
Used when shifting nodes up or down."
  (SETQ level (+ level level-increment))
  (IF (AND print-children (NOT (print-this-level level)))
      (SETQ print-children nil))
  (IF (VARIABLE-BOUNDP children)
      (DOLIST (child children)
	(SEND child :adjust-level-of-subtree level-increment))))


(DEFUN collect-kids (nodes)
  "This returns a list of all the immediate children of nodes."
  (LOOP with kids
	for node in nodes
	DO (SETQ kids (APPEND (SEND node :children) kids))
	finally (RETURN kids)))


(DEFUN iterate-over-subtree (node random-function)
  "Sends the 2nd argument, random-function, as a message to the first argument."
  (COND ((NULL node) nil)
	(t	     (SEND random-function node)
		     (LOOP for child in (SEND node :children)
			   DO (iterate-over-subtree child random-function)))))
