;;; -*- Mode: Common-Lisp; Package: TV; Base: 10.; Fonts: CPTFONT,HL12B,HL12BI -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;   Date	Author	Description
;;; -------------------------------------------------------------------------------------
;;  07/23/86	TWE	Moved the resource for POP-UP-FINGER-WINDOW out of COMETH to BASWIN,
;;			where the flavor is defined.

;;; This file has been simplified to just contain some random window
;;; resource definitions.

;;; Resource of general-purpose momentary menus.
(DEFWINDOW-RESOURCE MOMENTARY-MENU ()
	:MAKE-WINDOW (W:MENU :POP-UP T)
	:INITIAL-COPIES 0
	:REUSABLE-WHEN :DEEXPOSED)

(DEFWINDOW-RESOURCE DYNAMIC-MULTICOLUMN-MOMENTARY-MENU ()
        :MAKE-WINDOW (W:MENU :POP-UP T :MULTICOLUMN T :DYNAMIC T :COLUMN-SPEC-LIST NIL)
        :INITIAL-COPIES 0
	:REUSABLE-WHEN :DEEXPOSED)

(DEFWINDOW-RESOURCE MOMENTARY-MULTIPLE-MENU ()
	:MAKE-WINDOW (W:MENU :POP-UP T :HIGHLIGHTING T :MENU-MARGIN-CHOICES '(:DOIT))
	:INITIAL-COPIES 0
	:REUSABLE-WHEN :DEEXPOSED)

(DEFWINDOW-RESOURCE SEQUENCE-SELECTION-MULTIPLE-MENU ()
	:MAKE-WINDOW (SEQUENCE-SELECTION-MULTIPLE-MENU)
	:INITIAL-COPIES 0
	:REUSABLE-WHEN :DEEXPOSED)

(DEFRESOURCE BACKGROUND-LISP-INTERACTORS ()
  :CONSTRUCTOR (MAKE-INSTANCE 'BACKGROUND-LISP-INTERACTOR
                              :PROCESS CURRENT-PROCESS  ;will be set later
                              :SUPERIOR DEFAULT-SCREEN  ;always on this screen
                              :HEIGHT (TRUNCATE (SHEET-HEIGHT DEFAULT-SCREEN) 3))
  :INITIAL-COPIES 0)
