;;; -*- Mode:Common-Lisp; Fonts:(CPTFONT HL12B HL12BI TR10I TR8B); Base:10 -*-

;;;			      RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;			TEXAS INSTRUMENTS INCORPORATED.
;;;				 P.O. BOX 2909
;;;			      AUSTIN, TEXAS 78769
;;;				    MS 2151
;;;
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

(UNLESS (FIND-PACKAGE 'FED)
   (MAKE-PACKAGE 'FED))

(DEFSYSTEM FONT-EDITOR
  (:NAME               "Font Editor" )
  (:SHORT-NAME         "FED")
  (:PATHNAME-DEFAULT   "SYS:FONT-EDITOR;")
  (:PATCHABLE          "SYS:PATCH.FONT-EDITOR;" PATCH)
  (:WARNINGS-PATHNAME-DEFAULT "SYS:CWARNS;FONT-EDITOR.LISP")
  (:DOCUMENTATION "Create new fonts or modify existing fonts")
  (:DEFAULT-MENU-COLUMN :PROGRAMS)
  (:DEFAULT-SYSTEM-KEY #\F)
  (:INSTANCE-TYPE      :FLAVOR)
  (:INSTANCE-FINDER    FED::FED-FRAME)
  (:INSTANCE-CREATOR   (FED:FED))
  (:MODULE DEFS        ("DEFS"
	               "FLAVOR-DEFS"))
  (:MODULE SUPPORT     ("FONT-DESCRIPTOR"
                        "AST-SUPPORT"
			"UCL-COMMANDS"))      
  (:MODULE FLAVORS     ("GRID-MIXIN"
	                "OTHER-GRID-MIXINS"
	                "CURSOR"
		        "BASIC-FED"
		        "FED"
			"FED-CHAR-MODIFIERS"
			"FED-ONLINE-DOC"
		        "REGISTER-PANE"
		        "FED-TYPEOUT-WINDOW"
		        "FED-LABEL-WINDOW"
			"FED-FRAME"))
  (:MODULE COMBINE    "COMPILE-FLAVOR-METHODS")
  (:COMPILE-LOAD DEFS)
  (:COMPILE-LOAD SUPPORT (:FASLOAD DEFS))
  (:COMPILE-LOAD FLAVORS (:FASLOAD DEFS))
  (:COMPILE-LOAD-INIT COMBINE (DEFS FLAVORS) (:FASLOAD DEFS SUPPORT FLAVORS))
   )


