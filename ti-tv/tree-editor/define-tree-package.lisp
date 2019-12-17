;;; -*- Mode:Common-Lisp; Package:USER; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

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
;;; This file defines the TREE package and all of the symbol-related things that go on with it.  Any
;;; time a new symbol is defined in the TREE package, these lists need to be updated if that
;;; symbol is intended to be external.
;;;


;;;
;;; The tree editor requires the graphics window system so load it if is not already loaded.
;;;

(UNLESS (si:find-system-named 'gwin t t)
  (MAKE-SYSTEM 'gwin :noconfirm))


(EVAL-WHEN (COMPILE LOAD EVAL)
  (UNLESS (FIND-PACKAGE 'tree)
    (MAKE-PACKAGE 'tree :use '(lisp ticl zlc))))

(IN-PACKAGE 'tree)

(EXPORT
  ;; Except where noted, all symbols in this list are functions.
 '(
   *default-adjust-to-sup-size*		; global variable
   *default-application-type*		; global variable
   *default-init-label*			; global variable
   *default-vertical?*			; global variable
   *force-recalculate			; special inside of DISPLAY-2
   *known-application-types*		; global variable
   *max-level				; special inside of DISPLAY-2
   *minimum-breadth-spacing*		; global variable
   *minimum-depth-spacing*		; global variable
   *root-node				; special inside of DISPLAY-2
   *scroll-window-height*		; global variable
   *scroll-window-width*		; global variable
   *starting-point-offset*		; global variable
   *tree				; special inside of DISPLAY-2
   *tree-window				; special inside of DISPLAY-2
   *truncation-for-scroll-window*	; global variable
   *vertical?				; special inside of DISPLAY-2
   complain
   contract-node-with-redraw
   display
   display-as-process
   expand-node-with-redraw
   expand-contract-with-redraw
   fill-window
   grind-item
   move-to-front
   pan-window
   return-to-default-window
   string-item
   tree-redraw
   tree-redraw-after-small-changes
   update-node
   update-sizes
   zoom-window
   )
 'tree)
