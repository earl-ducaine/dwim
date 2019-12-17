;;; -*- Mode:Common-Lisp; Package:User; Fonts:(Courier Hl12b Hl12bi); Base:10 -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.

(UNLESS (FIND-PACKAGE "MAC")
  (DEFPACKAGE "MACINTOSH"
    (:use "LISP" "TICL" "USER")
    (:import make-mac)
    (:nicknames "MAC" "MAC-WINDOWS")
    (:prefix-name "MAC")))

(import tv:*mac-support-symbols* 'mac)


;; ============ ============ ============ ============ ============ ============ 	;;
;;			       THE ACTUAL DEFSYSTEM				;;
;; ============ ============ ============ ============ ============ ============ 	;;
	
(DEFSYSTEM Window-MX
  (:name "MX Windows")
  (:short-name "MX TV")
  (:pathname-default "sys:window-mx;")
  (:default-output-directory "sys:window-mx;")
  (:warnings-pathname-default "SYS: CWARNS; window-mx.lisp")  ;; 07.25.88 las
  (:patchable "sys:patch.window-mx;" patch)

  ;; ============ ============ ============ ============ ============ ============;;
  ;;			DEFINE THE MODULES OF THE LIBRARY ITSELF			;;

  (:module defs
	   ("definitions" "variables"))
  (:module basic-comm-definitions
	   ("basic-comm-definitions"))
  (:module basic-comm-protocols
	   ("basic-comm-protocols"))
  (:module font-translations
	   ("font-translations"))
  (:module basic-comm-methods
	   ("basic-comm-methods"))
  (:module new-stuff
	   ("mac-windows" "mac-messages" "fast-rubberbanding"
	    "mac-initiated-commands" "fed-functions"))
  (:module mx-tv-module-ops
	   ("mx-tv-module-ops"))
  (:module mouse-stuff
	   ("mouse-tracking"))
  (:module debug-stuff
	   ("function-name" "qix"))
  (:module wholin-stuff
	   ("mac-wholin"))
  (:module compile-last
	   ("compile-last"))	   

  ;; ============ ============ ============ ============ ============ ============;;
  ;;		       DEFINE THE TRANSFORMATIONS TO BUILD THE LIBRARY		;;

  (:compile-load 		defs)
  (:fasload                     defs)

  (:compile-load		debug-stuff
    (:fasload defs)
    (:fasload defs))

  (:compile-load-init		mx-tv-module-ops
    (defs)
    (:fasload defs)
    (:fasload defs))

  (:compile-load		wholin-stuff)
  
  (:compile-load-init		basic-comm-definitions
    (defs)
    (:fasload defs mx-tv-module-ops)
    (:fasload defs))

  (:compile-load-init		basic-comm-protocols
    ( basic-comm-definitions defs)
    (:fasload defs basic-comm-definitions)
    (:fasload defs basic-comm-definitions debug-stuff))

  (:compile-load-init		font-translations
    (basic-comm-definitions defs)
    (:fasload defs basic-comm-definitions )
    (:fasload defs basic-comm-definitions))

  (:compile-load-init		basic-comm-methods
    (basic-comm-definitions defs)
    (:fasload defs basic-comm-definitions
	      )
    (:fasload defs basic-comm-definitions))

  (:compile-load-init 		new-stuff
    (defs)
    (:fasload defs
	      mx-tv-module-ops
	      basic-comm-definitions )
    (:fasload defs))

  (:compile-load-init 		mouse-stuff
    (defs basic-comm-definitions)
    (:fasload defs )
    (:fasload defs))

  (:compile-load-init		compile-last
    (defs mouse-stuff
	  new-stuff basic-comm-methods
	  font-translations basic-comm-definitions
	  debug-stuff basic-comm-protocols)
    (:fasload defs mouse-stuff
	      new-stuff basic-comm-methods
	      font-translations basic-comm-definitions
	      debug-stuff basic-comm-protocols)
    (:fasload defs mouse-stuff
	       new-stuff basic-comm-methods
	      font-translations basic-comm-definitions
	      debug-stuff basic-comm-protocols))
  )








