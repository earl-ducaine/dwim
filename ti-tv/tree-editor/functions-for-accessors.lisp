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
;;; FORMATTING FUNCTIONS FOR OUTPUT TO SCROLL WINDOW
;;;


(DEFUN string-item (value &optional (format-string "~A"))
  "Creates an item that is acceptable for use by the flavor
TV:TEMPORARY-SCROLL-WINDOW which uses TV:BASIC-SCROLL-WINDOW as a mixin.
Items are documented in the window system manual.  This function can be used when
value is either a string or something that can be coerced into a string, i.e. (string value)
does not cause an error.  This cannot be used on lists.  This function also eliminates any
imbedded RETURNs because the RETURN character confuses the scrolling mechanism."
;  (CONS nil (LOOP for one-line-string in
;		  (string-eliminate-returns (FORMAT nil format-string value))
;		  collect (w:scroll-parse-item one-line-string))))

	(LOOP for one-line-string in
		  (string-eliminate-returns (FORMAT nil format-string value))
		  collect one-line-string))


(DEFUN string-eliminate-returns (STRING)
  "Eliminates return characters from string."
  (COND ((OR (EQUAL STRING "") (NULL STRING)) nil)
	(t (LET ((index (STRING-SEARCH-CHAR #\RETURN STRING)))
	     (IF (NULL index) (NCONS STRING)
	       (CONS (SUBSTRING STRING 0 index)
		     (string-eliminate-returns (SUBSTRING STRING (1+ index)))))))))


(DEFUN grind-item (form)
  "Creates a string item from an arbitrary Lisp form by grinding it to fit in the scroll
window."
  (string-item (format:output nil (GRIND-TOP-LEVEL form *scroll-window-width*))))



;;;
;;; FUNCTIONS TO REDRAW THE TREE
;;;


(DEFUN tree-redraw ()
  "Recalculates positions as necessary according to which nodes are modified and
whether or not *FORCE-RECALCULATE is T, then draws the tree.  To force this to
recalculate the whole tree, set *FORCE-RECALCULATE to T."
  (DECLARE (SPECIAL *root-node *force-recalculate *vertical?
		    *world *display-pane))
  (WHEN (OR (AND *root-node (SEND *root-node :modified))
	    *force-recalculate)
    (COND (*root-node           
	   (SEND *root-node :set-relative-positions)
	   (SETQ *force-recalculate nil)
	   (set-coordinates (LIST (CONS *root-node
					(IF *vertical?
					    *starting-point-offset*
					    5)))
			    (IF *vertical?
				5
				*starting-point-offset*))
	   (tree-draw-after-small-changes))
	  ;;
	  ;; Don't bother to do any work if no tree
	  ;;
	  (t (SETQ *force-recalculate nil)
	     (SEND *world :set-display-list nil)
	     (SEND *display-pane :refresh)))))


(DEFUN tree-draw-after-small-changes ()
  "Updates the display on the window without any repositioning.
First draws the nodes then calculates and draws the connecting lines."
  (DECLARE (SPECIAL *tree-window *display-pane *first))
  (put-tree-in-window)
  (COND ((SEND *tree-window :exposed-p)
	 (SEND *display-pane :refresh))
	(*first (SEND *display-pane :default-window)
		(SEND *tree-window :expose t :clean)
		(SEND *tree-window :select)
		(SETQ *first nil))
	(t (SEND *display-pane :default-window)
	   (SEND *tree-window :select))))


(DEFUN put-tree-in-window (&aux &special display)
  "Sets the graphics world display list to all the nodes to be displayed in the tree, then
adds the lines between the nodes to the display list."
  (DECLARE (SPECIAL *world *root-node))
  (COND ((NULL *root-node)
	 (SEND *world :set-display-list nil))
	(t (put-tree-in-window-aux *root-node)
	   (SEND *world :set-display-list display)
	   (add-lines *root-node))))


(DEFUN put-tree-in-window-aux (node)
  "Adds the node and all its children to the graphics world display list."
  (DECLARE (SPECIAL display))
  (COND ((NULL node) nil)
	(t	     (SETQ display (CONS node display))
		     (LOOP for child in (SEND node :print-children)
			   DO (put-tree-in-window-aux child)))))


(DEFUN add-lines (node)
  "Adds the line connecting this node to its parent to the graphics world display list,
and recurses on the node's children."
  (DECLARE (SPECIAL *vertical? *world))
  (LET* ((kids	    (SEND node :print-children))
	 (firstkid  (CAR kids))
	 (lastkid   (CAR (LAST kids)))
	 (halfdepth (/ *minimum-depth-spacing* 2.)))
  (WHEN kids
    (COND (*vertical?
	   (COND ((> (LENGTH kids) 1)
		  (SEND *world :insert-line
			(+ (SEND node :x-min) (SEND node :half-width))
			(SEND node :y-max)
			(+ (SEND node :x-min) (SEND node :half-width))
			(- (SEND firstkid :y-min) halfdepth) 0.)
		  (SEND *world :insert-line
			(+ (SEND firstkid :x-min) (SEND firstkid :half-width))
			(- (SEND firstkid :y-min) halfdepth)
			(+ (SEND lastkid :x-min) (SEND lastkid :half-width))
			(- (SEND lastkid :y-min) halfdepth))
		  (LOOP with newx
			for kid in kids
			DO (SETQ newx  (+ (SEND kid :x-min)
					  (SEND kid :half-width)))
			(SEND *world :insert-line
			      newx (SEND kid :y-min)
			      newx (- (SEND kid :y-min) halfdepth) 0.)
			(add-lines kid)))
		 (t (SEND *world :insert-line
			  (+ (SEND node :x-min) (SEND node :half-width))
			  (SEND node :y-max)
			  (+ (SEND node :x-min) (SEND node :half-width))
			  (SEND firstkid :y-min) 0.)
		    (add-lines firstkid))))
	  (t (COND ((> (LENGTH kids) 1)
		    (SEND *world :insert-line
			  (SEND node :x-max)
			  (+ (SEND node :y-min) (SEND node :half-width))
			  (- (SEND firstkid :x-min) halfdepth)
			  (+ (SEND node :y-min) (SEND node :half-width)) 0.)
		    (SEND *world :insert-line
			  (- (SEND firstkid :x-min) halfdepth)
			  (+ (SEND firstkid :y-min) (SEND firstkid :half-width))
			  (- (SEND lastkid :x-min) halfdepth)
			  (+ (SEND lastkid :y-min) (SEND lastkid :half-width)))
		    (LOOP with newy
			  for kid in kids
			  DO (SETQ newy  (+ (SEND kid :y-min)
					    (SEND kid :half-width)))
			  (SEND *world :insert-line
				(SEND kid :x-min) newy
				(- (SEND kid :x-min) halfdepth) newy 0.)
			  (add-lines kid)))
		   (t (SEND *world :insert-line
			    (SEND node :x-max)
			    (+ (SEND node :y-min) (SEND node :half-width))
			    (SEND firstkid :x-min)
			    (+ (SEND node :y-min) (SEND node :half-width))
			    0.)
		      (add-lines firstkid))))))))


;;;
;;; FUNCTIONS TO EXPAND AND CONTRACT THE CHILDREN OF A NODE
;;; These are not used in the original layout of the tree, but are useful if expanding or contracting by
;;; mousing a node is desired.
;;;


(DEFUN expand-node-with-redraw (instance amount &optional (strict-level nil))
  "Expands (adds the children to the display) instance by AMOUNT levels.
INSTANCE is the tree utility's representation of a given node.  That means if you call
this from HANDLE-NODE pass the INSTANCE parameter.  STRICT-LEVEL determines
whether the amount given is interpreted strictly.  If T, then no children beyond AMOUNT
levels will be printed no matter what.  If NIL (the default), then if a child at a depth of
AMOUNT has children that have already been expanded, these children will also be
printed.  A value of zero for amount signifies to expand fully."
  (DECLARE (SPECIAL *display-pane))
  (LET ((*max-level (COND ((= amount 0) 0)
			  (t		(+ amount (SEND instance :level))))))
    (DECLARE (SPECIAL *max-level))
    (expand-node instance (1+ (SEND instance :level)) strict-level)
    (make-tree-modified instance)
    (tree-redraw)))


(DEFUN contract-node-with-redraw (instance &optional (full-redraw nil))
  "Deletes the children of INSTANCE from the display list.
INSTANCE is the tree utility's representation of a given node.  That means, use
INSTANCE from HANDLE-NODE as an argument to this function.  If FULL-REDRAW is T,
then after erasing the node's children, the tree is re-balanced.  Otherwise it is not.
This function returns T if there were actually any children to contract, NIL otherwise."
  (COND ((SEND instance :print-children)
	 (SEND instance :set-print-children nil)
	 (IF full-redraw
	     (make-tree-modified instance))
	 (tree-redraw)
	 t)
	(t nil)))


(DEFUN expand-contract-with-redraw
       (instance amount &optional (strict-level nil) (full-redraw nil))
  "Expands or contracts the node, whichever is appropriate.
That is, if the node already has children being displayed, then the node is contracted.
Otherwise, it is expanded.  See documentation of EXPAND-NODE-WITH-REDRAW and
CONTRACT-NODE-WITH-REDRAW for the meaning of the arguments."
  (IF (NOT (contract-node-with-redraw instance full-redraw))
      (expand-node-with-redraw instance amount strict-level)))


;;;
;;; FUNCTIONS TO CALL FOR EDITING
;;;


(DEFUN delete-subtree (node)
  "Calls all the necessary functions and methods to delete the subtree beginning with
node, and to redraw the resultant tree."
  (DECLARE (SPECIAL *display-pane))
  (LET ((accessors (SEND *display-pane :accessor-functions)))
    (SEND node :delete-subtree)
    (SEND accessors :delete-subtree node)
    (tree-redraw)))


(DEFUN delete-node (node)
  "Calls all the necessary functions and methods to delete the node, and redraws the
resultant tree."
  (DECLARE (SPECIAL *root-node *display-pane))
  (LET ((accessors (SEND *display-pane :accessor-functions)))
    (COND ((AND (EQ *root-node node) (> (LENGTH (SEND node :children)) 1))
	   (complain '("Can't delete the root" "node if it has more"
		       "than 1 child.")))
	  (t (SEND node :delete-yourself)
	     (SEND accessors :delete-yourself node)
	     (tree-redraw)))))


(DEFUN add-node-before (node)
  "Calls all the necessary functions and methods to get a new node, insert it in the tree
after this one, and redraw the resultant tree."
  (DECLARE (SPECIAL *display-pane))
  (LET ((accessors (SEND *display-pane :accessor-functions)) new-node)
    (SEND node :add-node-before (SETQ new-node (get-new-node)))
    (SEND accessors :add-node-before new-node node)
    (tree-redraw)))


(DEFUN add-node-after (node)
  "Calls all the necessary functions and methods to get a new node, insert it in the tree
after this one, and redraw the resultant tree."
  (DECLARE (SPECIAL *display-pane))
  (LET ((accessors (SEND *display-pane :accessor-functions)) new-node)
    (SEND node :add-node-after (SETQ new-node (get-new-node)))
    (SEND accessors :add-node-after new-node node)
    (tree-redraw)))


(DEFUN add-brother-node (node button)
  "Calls all the necessary functions and methods to get a new node, insert it in the tree
to one side of this node (the user clicks the mouse on the side of this node he wants
the new one inserted), and redraw the new tree."
  (DECLARE (SPECIAL *display-pane *root-node))
  (LET ((accessors (SEND *display-pane :accessor-functions)) new-node)
    (COND ((EQ *root-node node)
	   (complain '("Can't add a brother to" "the root node."
		       "Trees can only have" "one root")))
	  (t (SEND node :add-brother (SETQ new-node (get-new-node)) button)
	     (SEND accessors :add-brother new-node node)
	     (tree-redraw)))))


;;;
;;; VARIOUS REWINDOWING FUNCTIONS.
;;;


(DEFUN fill-window ()
  "Scales the display so the entire tree fits into the window as closely as possible."
  (DECLARE (SPECIAL *display-pane))
  (SEND *display-pane :world-extents-window)
  (SEND *display-pane :refresh))


(DEFUN zoom-window (direction button)
  "Zooms the window in or out by a factor of 2 or .5 times."
  (DECLARE (SPECIAL *display-pane))
  (LET* ((factor (COND ((= button 4) 2) (t 1)))
	 (amount (SELECTQ direction
		   ('IN (* 2 factor))
		   ('OUT (/ 0.5 factor)))))
    (SEND *display-pane :zoom amount amount))
  (SEND *display-pane :refresh))


(DEFUN return-to-default-window ()
  "Places the window at its original position."
  (DECLARE (SPECIAL *display-pane))
  (SEND *display-pane :default-window)
  (SEND *display-pane :refresh))


(DEFUN pan-window (dx dy button)
  "Pans the window by a calculated amount dependent on which mouse button was
clicked."
  (DECLARE (SPECIAL *display-pane))
  (LET ((amount (COND ((= button 4) 3) (t 1))))
    (MULTIPLE-VALUE-BIND (width height) (SEND *display-pane :size)
      (SEND *display-pane :pan
	    (/ (* dx width amount) 3.0) (/ (* dy height amount) 3.0))))
  (SEND *display-pane :refresh))


(DEFUN move-to-front (node)
  "Moves a given object on the screen to the front, that is the center of the left side
of the display pane if the display is horizontal, otherwise to the center of the top."
  (DECLARE (SPECIAL *display-pane *vertical?))
  (LET ((x (SEND node :x-min))
	(y (SEND node :y-min)))
    (IF *vertical?
	(SETQ x (+ x (/ (FLOAT (- (SEND node :x-max) x)) 2.)))
	(SETQ y (+ y (/ (FLOAT (- (SEND node :y-max) y)) 2.))))
    (MULTIPLE-VALUE-BIND (start-x start-y) (SEND *display-pane :untransform-point
						 (SEND *display-pane :starting-x)
						 (SEND *display-pane :starting-y))
      (MULTIPLE-VALUE (start-x start-y)
	(SEND *display-pane :transform-deltas (- start-x x) (- start-y y)))
      (SEND *display-pane :pan start-x start-y)))
  (SEND *display-pane :refresh))


;;;
;;;FUNCTION TO SEND AN ERROR MESSAGE
;;;


(DEFUN complain (message)
  "Prints an error message in the utility pane in reverse video and beeps.
User must click a mouse button to proceed."
  (DECLARE (SPECIAL *utility-pane *tree-window))
  (LET ((old-msg (SEND *utility-pane :item-list)))
    (SEND *utility-pane :set-reverse-video-p t)
    (SEND *utility-pane :set-item-list
	  (APPEND message (LIST " "  " "  "PRESS ANY KEY" "TO CONTINUE")))
    (BEEP)
    (SEND *tree-window :any-tyi)
    (SEND *utility-pane :set-item-list old-msg)
    (SEND *utility-pane :set-reverse-video-p nil)))


;;;
;;;FUNCTIONS TO CALL AFTER CHANGING THE SIZE OF A NODE
;;;


(DEFUN update-node (node)
  "Given a node that has been edited, makes sure it has the right name, highlight
function and graphics objects to represent it."
  (DECLARE (SPECIAL *display-pane))
  (LET ((accessors (SEND *display-pane :accessor-functions))
	(data	   (SEND node :data)))
    (SEND node :set-entities
	  (picture-from-print-name (SEND accessors :print-name data) data))
    (SEND node :set-highlighted-p (SEND accessors :highlight-function data))
    (SEND node :set-type (SEND accessors :find-type data))
    (SEND node :init nil)))
