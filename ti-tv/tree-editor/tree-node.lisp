;;; -*- Mode:Common-Lisp; Package:TREE; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

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
;;; OVERVIEW:
;;;
;;; This file contains the flavor TREE-NODE and its associated methods as well as a few auxiliary
;;; functions.  Methods and functions that handle editing of the tree are in the file
;;; "EDIT-TREE-NODE".
;;;
;;; This flavor is a graphics entity for the GED.  It is basically the same as the SUBPICTURE flavor
;;; already used by the graphics window system except that it contains information about how to
;;; traverse a tree structure.  Each instance of this flavor corresponds to a node in the tree and
;;; each node knows its parent and chidren.  There are also instance variables SUBTREE-EXTENTS
;;; and OFFSET-TO-PARENT which are used in the process of determining the location of each node
;;; in the tree.  Also included in each flavor instance is the instance variable data which is the node
;;; that the instance corresponds to in the user's representation.  This is to allow editing of the
;;; user's tree structure.
;;;
;;; methods for flavor TREE-NODE:
;;;	DRAW (before &after)
;;;	SET-CHILDREN (after)
;;;	CHILDREN-SET-P
;;;	CHILDREN (before)
;;;	SET-RELATIVE-POSITIONS
;;;	SET-WIDTH-COORD
;;;	HALF-WIDTH
;;;	DEPTH
;;;
;;; functions:
;;;	PREPEND-SELF-TO-EXTENTS
;;;	ADJUST-EXTENTS
;;;	CALCULATE-NEW-EXTENTS
;;;	CALCULATE-NEW-EXTENTS-AUX
;;;	CREATE-NEW-EXTENTS
;;;	REQUIRED-DISTANCE
;;;	LEFT-EXTENT
;;;	RIGHT-EXTENT
;;;

;;;
;;; TREE-NODE FLAVOR
;;;  Essentially the same as GED:SUBPICTURE, except that instance variables have been added that
;;;  contain tree specific information.
;;;

(DEFFLAVOR tree-node
	   ((data		nil)	; Node in user's representation.
	    (parent		nil)	; Parent in tree representation.
	    children			; Children in tree representation.  Originally
					; unbound.  Gets bound the first time that an
					; attempt is made to print the children of the
					; node.
	    (print-children	nil) 	; The children to be displayed.  Normally the same
					; as CHILDREN, except when the node has been
					; contracted.
	    (level		nil)	; The depth of this node in the tree.  The first
					; printable node is at level 1.
	    (modified		t)	; Flag for whether or not this node has been
					; modified.  Being modified essentially means that
					; the SUBTREE-EXTENTS are not necessarily
					; correct.
	    (offset-to-parent	0)	; The offset from this node to its parent.  A
					; negative number signifies that this node is to
					; the left (above) its parent.
	    (subtree-extents	nil)	; A list of the extents at each level of the
					; subtree headed by this node.
	    (type		nil)	; The type of this node returned by NODE-TYPE.
	    (highlighted-p	nil))	; Form that gets evaluated on redraw to decide
					; whether or not to shade in the node.
	   (gwin:subpicture)
  :settable-instance-variables
  :inittable-instance-variables
  :gettable-instance-variables
  (:special-instance-variables children))


(DEFMETHOD (tree-node :before :draw) (IGNORE)
  "Highlights nodes whose highlight functions return non-nil by giving them a fill color."
  (WITH-SELF-VARIABLES-BOUND
   (SETQ gwin:fill-color (IF (EVAL highlighted-p)
			     1
			     nil))))


(DEFMETHOD (tree-node :after :draw) (window)
  "Adds the node to the list of mouse sensitive items."
  (MULTIPLE-VALUE-BIND (wind-left wind-top wind-right wind-bott)
      (SEND window :inside-edges)
    (MULTIPLE-VALUE-BIND (left top)
	(SEND window :transform-point gwin:x-min gwin:y-min)
      (MULTIPLE-VALUE-BIND (right bottom)
	  (SEND window :transform-point gwin:x-max gwin:y-max)
	(SEND window :add-to-temp-item-list type self
	      (MAX (FLOOR left)     wind-left)
	      (MAX (FLOOR top)      wind-top)
	      (MIN (CEILING right)  wind-right)
	      (MIN (CEILING bottom) wind-bott))))))


(DEFMETHOD (tree-node :after :set-children) (kids)
  "Allows the setting of both CHILDREN and PRINT-CHILDREN with only one call to
:SET-CHILDREN.  There is no reason that CHILDREN should ever be set without also
setting PRINT-CHILDREN."
  (SEND SELF :set-print-children kids))


(DEFMETHOD (tree-node :children-set-p) ()
  "Predicate for whether the children have already been set."
  (DECLARE (SPECIAL children))
  (BOUNDP 'children))


(DEFMETHOD (tree-node :before :children) ()
  "Makes sure that (SEND SELF :CHILDREN) does not return an error by setting the
children if they have not previously been set."
  (DECLARE (SPECIAL children *display-pane))
  (IF (NOT (BOUNDP 'children))
      (SEND SELF :set-children (SEND (SEND *display-pane :accessor-functions)
				     :children-from-data data))))


;;;
;;; TREE LAYOUT METHODS
;;; Throughout, the terms "left" and "right" refer to relative positioning assuming a vertical
;;; orientation.  If the orientation is horizontal, Then "left" corresponds to what will appear on the
;;; top and "right" corresponds to what will appear on the bottom.
;;;


(DEFMETHOD (tree-node :set-relative-positions)
	   (&aux &special (separations (LIST 0)))
  "Determines the node's offset to its parent based on the size of the subtree it heads.
Sets the offset-to-parent and subtree-extents instance variables."
  (DECLARE (SPECIAL *force-recalculate))
  (COND ((OR modified *force-recalculate)
	 (SETQ modified nil)
	 (SETQ subtree-extents
	       (prepend-self-to-extents
		 self
		 (COND ((NULL print-children) nil)
		       (t (LET* ((new-extents
				  (calculate-new-extents
				   (LOOP for child in print-children
					 collect (SEND child
						       :set-relative-positions))))
				 (half-spread (/ (CAR (LAST separations)) 2)))
			    (LOOP for child in print-children
				  AND for POSITION in separations
				  DO (SEND child :set-offset-to-parent
					   (- POSITION half-spread)))
			    (adjust-extents new-extents (- half-spread))))))))
	(t subtree-extents)))


(DEFUN prepend-self-to-extents (node extents)
  (LET ((half (SEND node :half-width)))
    (CONS (CONS (- half) half) extents)))


(DEFUN adjust-extents (extent-list amount)
  "Moves EXTENT-LIST by AMOUNT.  Used for recentering EXTENT-LIST about its
center."
  (COND ((NULL extent-list) nil)
	((CONS (CONS (+ (left-extent (CAR extent-list)) amount)
		     (+ (right-extent (CAR extent-list)) amount))
	       (adjust-extents (CDR extent-list)  amount)))))


(DEFUN calculate-new-extents (extent-lists)
  "Determine the layout of the subtrees of a given node.
The EXTENT-LIST for all the subtrees combined is built up in
CALCULATE-NEW-EXTENTS-AUX which successively separates the next (to the right)
subtree from the subtrees that have already been merged."
  (calculate-new-extents-aux nil extent-lists))


(DEFUN calculate-new-extents-aux (so-far REST)
  (DECLARE (SPECIAL separations))
  (COND ((NULL REST) so-far)
	((NULL so-far) (calculate-new-extents-aux (CAR REST) (CDR REST)))
	(t (LET ((distance (required-distance so-far (CAR REST))))
	     (SETQ separations (APPEND separations (LIST distance)))
	     (calculate-new-extents-aux (create-new-extents so-far
							    (CAR REST) distance)
					(CDR REST))))))


(DEFUN create-new-extents (old new new-offset)
  "Given two extent lists and an offset to apply to the right extent list (NEW), this
function generates an extent list that covers the extents of the two subtrees adjoined
separated by a distance of NEW-OFFSET."
  (COND ((NULL new) old)
	((NULL old) (adjust-extents new new-offset))
	(t (APPEND (LIST (CONS (left-extent (CAR old))
			       (+ (right-extent (CAR new)) new-offset)))
		   (create-new-extents (CDR old) (CDR new) new-offset)))))


(DEFUN required-distance (left-subtree right-subtree)
  "This function calculates how far apart two subtrees must be separated based upon
the 'extents' of the subtrees, that is, how far each subtree branches to the left and
right at each level."
  (COND ((NULL right-subtree) nil)
	(t (+ (LOOP for left-level in left-subtree
		    AND for right-level in right-subtree
		    maximize (- (right-extent left-level)
				(left-extent right-level)))
	      *minimum-breadth-spacing*))))


(DEFUN left-extent (x)
  "An extent is a cons of the left extent, that is, how far the subtree extends to the
left and a right extent.  This returns the car."
  (CAR x))


(DEFUN right-extent (x)
  "An extent is a cons of the left-extent, that is, how far the subtree extends to the
left and a right-extent.  This returns the cdr."
  (CDR x))


(DEFMETHOD (tree-node :set-width-coord) (width-center depth)
 "This method moves the node in either the x or the y direction depending on which
orientation is chosen.  It is called with the center width location."
  (DECLARE (SPECIAL *vertical?))
  (SEND SELF :move (- (SEND SELF :x-min)) (- (SEND SELF :y-min)))
  (LET* ((half (SEND SELF :half-width)))
    (COND (*vertical? (SEND SELF :move (- width-center half) depth))
	  (t	      (SEND SELF :move depth (- width-center half))))))


(DEFMETHOD (tree-node :half-width) ()
  "Returns half the width of the node."
  (DECLARE (SPECIAL *vertical?))
  (COND (*vertical? (/ (- (SEND SELF :x-max) (SEND SELF :x-min))
			2))
	(t	    (/ (- (SEND SELF :y-max) (SEND SELF :y-min))
			2))))


(DEFMETHOD (tree-node :depth) ()
  "Returns the height of the node."
  (DECLARE (SPECIAL *vertical?))
  (COND (*vertical? (- (SEND SELF :y-max) (SEND SELF :y-min)))
	(t	    (- (SEND SELF :x-max)
		       (SEND SELF :x-min)))))
