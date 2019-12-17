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


(DEFCONST utility-pane-list
	  '("Click Double" "Left for Help"))


(DEFCONST edit-menu-list
	  '(("Add Node After" :value "ADDAFTER"
	     :documentation "Add a node after the moused node")
	    ("Add Node Before" :value "ADDBEFORE"
	     :documentation "Add a node before the moused node")
	    ("Add a Brother Node" :value "ADD-BROTH"
	     :documentation "Add a node to a set of sons")
	    ("Delete Node" :value  "DELNODE"
	     :documentation "Delete a node from the tree")
	    ("Delete From Node Down" :value "DELSUBTREE"
	     :documentation "Delete subtree.")
	    ("Redraw" :value "REDRAW"
	     :documentation "Recalculate and redraw tree.")
	    ("Return to Main Menu" :value "EXIT-EDIT"
	     :documentation "")))


(DEFCONST system-menu-list
	  '(("New Tree" :value "NEW"
	     :documentation "Enter new tree to be displayed.")
	    ("Edit" :value "EDIT"
	     :documentation "Edit tree.")
	    ("Redraw" :value "REDRAW"
	     :documentation "Recalculate and redraw tree.")
            ("Edit Parameters" :value "PARAMETERS"
	     :documentation "Edit the parameters")
	    ("Exit" :value "EXIT-SYSTEM"
	     :documentation "Exit from tree display to superior.")))


(DEFCONST screen-option-list
	  '(("Move Up" :value "UP"
	     :documentation "Pan screen up.  L:Third page, R:Full page.")	
	    ("Move Down" :value "DOWN"
	     :documentation "Pan screen down.  L:Third page, R:Full page.")
	    ("Move Left" :value "LEFT"
	     :documentation "Pan screen left.  L:Third page, R:Full page.")
	    ("Move Right" :value "RIGHT"
	     :documentation "Pan screen right.  L:Third page, R:Full page.")
	    ("Zoom In" :value "ZOOM-IN"
	     :documentation "Zoom in.  L:Double size, R:Quadruple size.")
	    ("Zoom Out" :value "ZOOM-OUT"
	     :documentation "Zoom out.  L:Half size, R:Fourth size.")
	    ("Fill Window" :value "FILL"
	     :documentation "Fill window with tree.")
	    ("Recenter" :value "RESET"
	     :documentation "Return tree to original position at normal size.")))
