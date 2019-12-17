;;; -*- Mode:Common-Lisp; Package:USER; Base:10 -*-

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
;;; Change History
;;; --------------
;;; 03/28/87  KJF  Changed pathnames to not use translations.
;;; 06/30/88  KTN  Added patch attribute to :patchable.

;;;
;;; This is a DEFSYSTEM for the tree editor that loads in two sample interfaces.
;;;

(DEFSYSTEM TREE
  (:NAME		"Tree Editor")
  (:NICKNAMES		"Tree-Editor")
  (:SHORT-NAME		"Tree")
  (:PATHNAME-DEFAULT	"sys:tree-editor;")
  (:PATCHABLE		"sys:patch.tree-editor;" PATCH)
  (:WARNINGS-PATHNAME-DEFAULT "sys:cwarns;tree-editor.lisp")
  (:MODULE BASIC-FUNS	"basic-functions")		; Functions used throughout editor.
  (:MODULE DEFTREE	"define-tree-package")		; Tree package definition.
  (:MODULE DRIVER	"driver")			; Top level driver.
  (:MODULE EDIT-METH	"edit-tree-node")		; Editing methods for Tree-node.
  (:MODULE GEN-METH	"tree-node")			; Flavor and methods for Tree-node.
  (:MODULE MENUS	"menus")			; Menus that are used.
  (:MODULE USEFUL-FUNS	"functions-for-accessors")	; Useful functions that can be called by the accessors.
  (:MODULE VARS		"variables")			; Global variable definitions.
  (:MODULE WINDOWS	"window-definitions")		; Flavors and methods for window used.

  (:MODULE FLAV-ACC	"sys:tree-editor.starter-kit;flavor-accessors")		;Flavor accessor functions.
  (:MODULE STR-ACC	"sys:tree-editor.starter-kit;string-accessors")		;String accessor functions.
  (:MODULE STR-METH	"sys:tree-editor.starter-kit;string-edit-methods")	;String edit methods.

  (:MODULE AUX		("sys:site;tree.system" "sys:site;tree-editor.system"))

	
  (:AUXILIARY	 AUX)
  (:COMPILE-LOAD DEFTREE)
  (:COMPILE-LOAD VARS
		 (:FASLOAD DEFTREE)
		 (:FASLOAD DEFTREE))
  (:COMPILE-LOAD MENUS
		 (:FASLOAD DEFTREE)
		 (:FASLOAD DEFTREE))
  (:COMPILE-LOAD WINDOWS
		 (:FASLOAD DEFTREE VARS MENUS)
		 (:FASLOAD DEFTREE VARS MENUS))
  (:COMPILE-LOAD BASIC-FUNS
		 (:FASLOAD DEFTREE VARS)
		 (:FASLOAD DEFTREE VARS))
  (:COMPILE-LOAD USEFUL-FUNS
		 (:FASLOAD DEFTREE BASIC-FUNS)
		 (:FASLOAD DEFTREE BASIC-FUNS))
  (:COMPILE-LOAD GEN-METH
		 (:FASLOAD DEFTREE WINDOWS VARS USEFUL-FUNS BASIC-FUNS)
		 (:FASLOAD DEFTREE WINDOWS VARS USEFUL-FUNS BASIC-FUNS))
  (:COMPILE-LOAD EDIT-METH
		 (:FASLOAD DEFTREE GEN-METH USEFUL-FUNS)
		 (:FASLOAD DEFTREE GEN-METH USEFUL-FUNS))
  (:COMPILE-LOAD DRIVER
		 (:FASLOAD DEFTREE WINDOWS BASIC-FUNS USEFUL-FUNS)
		 (:FASLOAD DEFTREE WINDOWS BASIC-FUNS USEFUL-FUNS))
  (:COMPILE-LOAD STR-ACC
		 (:FASLOAD DEFTREE DRIVER)
		 (:FASLOAD DEFTREE DRIVER))
  (:COMPILE-LOAD STR-METH
		 (:FASLOAD DEFTREE STR-ACC EDIT-METH)
		 (:FASLOAD DEFTREE STR-ACC EDIT-METH))
  (:COMPILE-LOAD FLAV-ACC
		 (:FASLOAD DEFTREE DRIVER)
		 (:FASLOAD DEFTREE DRIVER)))

