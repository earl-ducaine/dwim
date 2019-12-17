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
;;; GLOBAL VARIABLES
;;; Note: all global variables specific to TREE both begin and end with an asterisk.  Variables local to
;;; a given tree window only begin with an asterisk.
;;;


(DEFVAR *minimum-breadth-spacing* 10
  "The minimum spacing allowed between each node in the tree editor display.
The number is in world coordinates.")


(DEFVAR *minimum-depth-spacing* 30
  "The minimum spacing allowed between each level in the tree editor display.
The number is in world coordinates.")


(DEFVAR *starting-point-offset* 5
  "The offset in pixels of the starting point of the tree from the edge.
If the display is horizontal, this is from the right edge, otherwise from the top.")


(DEFVAR *truncation-for-scroll-window* nil
  "Controls whether or not text is truncated on the scroll window for the tree editor.")


(DEFVAR *scroll-window-width* 50
  "The width in characters of the tree editor scroll window.")


(DEFVAR *scroll-window-height* 10
  "The height in characters of the tree editor scroll window.")


(DEFVAR *known-application-types* nil
  "This holds a list of the known flavors that have been defined for user applications.
Each flavor bundles together a collection of functions for each kind of tree editor.  For
instance, there is a STRING-DISPLAY flavor that defines methods for displaying and
editing lists of strings.  If a user doesn't specify what application he wants to use on
his data when he calls the tree editor, he will be prompted with a list of the known
application types, derived from this variable.  Any time a user defines a new application,
the flavor name should be pushed onto this list.")


(DEFVAR *default-application-type* 'flavor-display
  "If this has a value, when a tree editor process is created this is the application type
that will be used.  Otherwise the user will be queried from a list of known application
types.")


(DEFVAR *default-vertical?* nil
  "Default on whether or not to show the tree editor display vertically.")


(DEFVAR *default-init-label* nil
  "Default label for the tree editor.")


(DEFVAR *default-adjust-to-sup-size* t
  "Default of whether or not to make the tree editor the size of its superior.")
