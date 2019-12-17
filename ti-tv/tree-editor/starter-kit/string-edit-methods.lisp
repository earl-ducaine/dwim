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
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 2-13-87   DLS		Original version for Release 3.
;;;

;;;
;;; These are the methods needed to do the editing for the string displayer.  Any application using
;;; editing needs to provide each of these methods for its application flavor.  They should update the
;;; internal structures of the user data to reflect the editing changes.  Each method is passed the
;;; flavor instance of the selected node, and if a node is being inserted into the tree, the new node
;;; as well.  These nodes contain the user data in an instance variable called DATA.  Each has the
;;; flavor instance of its parent in the variable PARENT and a list of their children in CHILDREN.
;;; Sending these messages to the node instances will return these values if you need them to
;;; update your data.
;;;


(DEFMETHOD (string-display :add-node-before) (new-node selected-node)
  "This updates the data structures to show the inserted node."
  (LET ((new-data (APPEND (LIST (SEND new-node :data))
			  (LIST (SEND selected-node :data)))))
    (SEND new-node :set-data new-data) 
    (update-node new-node)
    (UNLESS (NULL (SEND new-node :parent))
      (fix-data-in-parent (SEND new-node :parent) new-data
			  (SEND selected-node :data)))))


(DEFMETHOD (string-display :add-node-after) (new-node selected-node)
  "This updates the data structures to show the inserted node."
  (LET ((new-data (IF (SEND new-node :children)
		      (APPEND (LIST (SEND new-node :data))
			      (SEND SELF :children-from-data
				    (SEND selected-node :data)))
		      (SEND new-node :data))))
    (SEND new-node :set-data new-data)
    (update-node new-node)
    (fix-data-in-parent selected-node (LIST (SEND selected-node :name) new-data)
			(SEND selected-node :data))
    (WHEN (NULL (SEND new-node :children))
      (update-node selected-node))))


(DEFMETHOD (string-display :add-brother) (new-node selected-node)
  "This updates the data structures to include the inserted node."
  (LET* ((parent    (SEND selected-node :parent))
	 (pos	    (1+ (find-position-in-list new-node
					       (SEND parent :children))))
	 (node-info (SEND parent :data)))
    (update-node new-node)
    (SEND parent :set-data
	  (APPEND (FIRSTN pos node-info)
		  (LIST (SEND new-node :data))
		  (NTHCDR pos node-info)))
    (fix-data-in-parent (SEND parent :parent) (SEND parent :data) node-info)))


(DEFMETHOD (string-display :delete-subtree) (selected-node)
  "This updates the data structures to no longer include the deleted subtree."
  (LET ((parent (SEND selected-node :parent)))
    (UNLESS (NULL parent)		;do nothing if deleting the whole tree
      (delete-data-in-parent parent (SEND selected-node :data))
      (IF (NULL (SEND parent :children))
	  (update-node parent)))))


(DEFMETHOD (string-display :delete-yourself) (node-to-delete)
  "This updates the data structures to no longer include the deleted node."
  (LET* ((data   (SEND node-to-delete :data))
	 (parent (SEND node-to-delete :parent))
	 parent-data)
    (UNLESS (NULL parent)		;deleting last node?
      (SETQ parent-data (SEND parent :data))
      (COND ((ATOM data)
	     (IF (= (LENGTH parent-data) 2)
		 (fix-data-in-parent parent (CAR parent-data) parent-data)
		 (fix-data-in-parent parent (REMOVE data parent-data) parent-data)))
	    ((= (LENGTH data) 2)
	     (fix-data-in-parent parent (CADR data) data))
	    (t (LET ((pos (find-position-in-list-equal data  (SEND parent :data))))
		 (SETQ parent-data (APPEND (firstn pos parent-data) (CDR data)
					   (NTHCDR (1+ pos) parent-data)))
		 (fix-data-in-parent (SEND parent :parent) parent-data
				     (SEND parent :data))
		 (SEND parent :set-data parent-data))))
      (IF (NULL (SEND parent :children))
	  (update-node parent)))))


;;;
;;; This must be included if the application is to use editing.
;;;

(DEFMETHOD (string-display :get-user-data) ()
  "This returns user data for a single node with no children.  Used for inserting nodes
into an existing tree."
  (tv:get-line-from-keyboard "Character string for new node"))


;;;
;;; This is a function solely for this application.  Do not write one of your own.
;;;

(DEFUN fix-data-in-parent (current-node new-data old-data)
  "This changes the data in CURRENT-NODE to include the new data."
  (WHEN current-node
    (SEND current-node :set-data
	  (SUBST new-data old-data (SEND current-node :data)))
    (fix-data-in-parent (SEND current-node :parent) new-data old-data)))


;;;
;;; This is a function solely for this application.  Do not write one of your own.
;;;

(DEFUN delete-data-in-parent (current-node data-to-delete)
  "This removes the DATA-TO-DELETE from the CURRENT-NODE and from its parent."
  (WHEN current-node
    (LET ((temp (REMOVE data-to-delete (SEND current-node :data))))
      ;;
      ;; If all this node's children are deleted, strip off the outer parentheses in node.
      ;;
      (IF (= (LENGTH temp) 1)
	  (SETQ temp (CAR temp)))
      (fix-data-in-parent (SEND current-node :parent)
			  temp (SEND current-node :data))
      (SEND current-node :set-data temp))))


(COMPILE-FLAVOR-METHODS string-display)
