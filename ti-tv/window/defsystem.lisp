;;;-*- Mode:Common-Lisp; Package:USER; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1985-1989 Texas Instruments Incorporated. All rights reserved.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  05/08/89  LG	         Added FAST-RUBBERBANDING file.
;;;  05/23/88  KJF           Moved "SCREENS" from MAIN to BASWIN because of defmethods in SCREENS.
;;;  04/23/88  KJF           Added 2 new files: SCREENS and DEBUG.  For multiple screen support.
;;;   3/04/87  TWE	Changed defsystem as per LuAnn's suggestions.  This involved changing the
;;;			:PATCHABLE option and adding the :WARNINGS-PATHNAME-DEFAULT option.
;;;   3/03/87  TWE	Added the raster-font file to make using fonts cleaner.
;;;   1/29/87  TWE	Made the bitmap file an auxiliary file.  Had I known about this feature of defsystem
;;;			earlier I would have done it, as it is the `right thing to do'.
;;;   1/23/87  TWE	Uncommented out the loading of the raster fonts.  This is needed for GWIN.
;;;  1/05/87   TWE	Added the symbol-help code.
;;; 12/30/86   DLS		Added the :PATCHABLE keyword to make the Window System patchable.
;;; 12/23/86   TWE	Made several changes to make the build process cleaner.
;;;			Moved SCROLL-BAR to be compiled/loaded before stream so that symbols it defines
;;;			would be known by the margin region code.
;;;			Made TYPWIN its own module so that the SCROLL and TSCROL files would see the
;;;			WINDOW-WITH-TYPEOUT-MIXIN which they require.
;;;			Made CHOICE and its following methods a module after the scrolling code so that
;;;			it could refer to TEXT-SCROLL-WINDOW which it requires.
;;;			Separated OBSOLETE and OBSOLETE-SCROLL-BAR into separate modules since
;;;			OBSOLETE-SCROLL-BAR depends upon the flashy scrolling mixin which is located in
;;;			OBSOLETE.
;;; 12/22/86   TWE	Made several changes to make the build process cleaner.
;;;			Made BASWIN its own module so that initial-lisp-listener would be loaded before it is
;;;			used.  Made BASSTR be compiled before STREAM.
;;; 12/19/86   TWE	Made sure that BASWIN gets compiled earlier than it used to be.  In this way the
;;;			flavors ESSENTIAL-WINDOW and MINIMUM-WINDOW are loaded into the
;;;			environment before they get referenced.
;;; 12/15/86   TWE	Changed the default host to be SYS instead of LM.
;;; 12/12/86   TWE	Moved CHARIN-STREAM to be compiled after STREAM gets loaded so that it doesn't
;;;			complain about the STREAM-MIXIN flavor not being defined.  Moved
;;;			MOUSE-SENSITIVE-FORMAT after the loading of TYPWIN so that it can do a SETF
;;;			on a defstruct which is defined in TYPWIN.  Both of these result in a cleaner build.
;;; 11/03/86   TWE	Removed the font loading code.  This is to be done earlier by a different system.
;;; 10/31/86   TWE	Moved MENU so it is compiled and loaded before OBSOLETE-MENU, and split SCRED
;;;			into two parts.
;;; 10/23/86   TWE	Made primitives its own module since it refers to things in globals.  Commented out
;;;			the loading of the -font files since the loader can't load them yet.
;;; 09/30/86   TWE	Added the file menu-search to the defsystem.
;;; 09/26/86   TWE	Added the file mouse-sensitive-format to the defsystem.
;;; 07/30/86   TWE	Added support for keywords as mouse characters for CHARACTER-BLINKERs.
;;; 07/24/86   TWE	Moved the macros out of HELP-STREAM and put them into HELP-MACROS, which gets
;;;			loaded along with TVDEFS.
;;; 07/23/86   TWE	Changed the default pathname since the WS files were moved to EH:.  Also moved
;;;			HELP-STREAM so that is gets loaded before the menu code (which uses it).
;;; 07/11/86   TWE	Added the file HELP-STREAM to THE-REST module.  That file was removed from the
;;;			kernel because the opinion was that it is heavily dependent upon the Window System.

;;; This is a list of all of the hacks necessary to make the Window System work for release 3.0.
;;; Much work has been spend on eliminating the magic needed to make the Window System.
;;; Part of this involved defining the defsystem properly so that dependencies would be encountered
;;; in the proper order, as well as doing the same thing to particular files.

;;; The file DEFINE-W-PACKAGE was changed to comment out some of the symbols in the
;;; EXPORT form to get around a compiler (or kernel) problem where it tries to import the
;;; symbol before exporting it.  Since these symbols were already imported, the subsequent
;;; import caused an error.  This needs to be removed when the compiler (or kernel) is
;;; fixed to not cause an error in the EXPORT form.

;;; The file TVDEFS was changed to define a function COMPILER:MAKE-OBSOLETE-1.  This
;;; was done because for now that function isn't defined, so TVDEFS defines it to do an
;;; IGNORE.  This needs to be removed when the COMPILER:MAKE-OBSOLETE-1 function
;;; becomes defined.

;;; The file BASWIN was changed to put back the old Lisp Listener flavor like the old days.
;;; The UCL code is not loaded yet, so we need to have some kind of Lisp Listener so that
;;; we can further test out the Window System.  Also, the COMP-FLAV-METH file was
;;; changed so that a compile-flavor-methods would be done on the Lisp-Listener flavor.
;;; When UCL comes up, this needs to be commented back and the Lisp-Listener removed
;;; from the COMP-FLAV-METH file.

(DEFSYSTEM TV
  (:NAME                       "Window System")
  (:SHORT-NAME                 "Window")
  (:NICKNAMES                  "TV Win")
  (:PATHNAME-DEFAULT           "SYS:WINDOW;")
  (:PATCHABLE		       "SYS:PATCH.WINDOW;" PATCH)
  (:WARNINGS-PATHNAME-DEFAULT  "SYS:CWARNS;WINDOW.LISP")

  (:MODULE DEFS                ("TVDEFS" "HELP-MACROS" "CSIB-DEFS"))
  (:MODULE G-DEFS              ("GLOBALS" "MACROS" "UTILITY"))
  (:MODULE PRIMITIVES          "PRIMITIVES")
  (:MODULE CSIB                ("CONTROL-REGISTER" "BLOCK-REGISTER" "CSIB"))
  (:MODULE MAP                 "MAP")
  (:MODULE MAIN		       ("CHARIN" "SCRMAN" "SHEET" "DEFINE-MOUSE-MAPPING" "BLINKERS"
			        "SHWARM" "SOUND" "CONVERT"))
  (:MODULE BASWIN              ("BASWIN" "SCREENS"))
  (:MODULE OTHER-MAIN          ("TRANSFORM" "HELP-STREAM" "WHOLIN" "BITMAP-READ-WRITE"
			        "MOUSE" "BASSTR" "SCROLL-BAR"))
  (:MODULE STREAM-ET-AL        ("STREAM" "MARGIN-REGION"))
  (:MODULE GRAPH               ("CHARIN-STREAM" "GRAPHICS" "FONT" "WWIN"))
  (:MODULE MENU-NEW            "MENU")
  (:MODULE MENU-OLD            "OBSOLETE-MENU")
  (:MODULE MENU-EXTENSION      "MENU-SEARCH")
  (:MODULE THE-REST            ("COMETH"
			        ;; The above must be loaded before any windows get created
			        "SYSMEN" "SCRED"))
  (:MODULE SAM                 "SAM")
  (:MODULE SCRED2              "SCRED2")
  (:MODULE TYPWIN              "TYPWIN")
  (:MODULE SCROLL              ("SCROLL" "TSCROL" "FRAME"))
  (:MODULE CHOICE-ET-AL        ("CHOICE" "TEXTURE-SELECTOR-PANE" "COLOR-SELECTOR-PANE"
				"CSRPOS" "OBSOLETE" "SYMBOL-HELP"))
  (:MODULE OBSOLETE-SCROLL-BAR "OBSOLETE-SCROLL-BAR")
  (:MODULE SEQ-SEL             "SEQUENCE-SELECTION")	; Needs menu loaded.
  (:MODULE SFONT               ("MOUSE-SENSITIVE-FORMAT" "STANDARD-FONT" "RASTER-FONT"))
  (:MODULE RFONT               ("CPTFONT-FONT" "MEDFNB-FONT"))
  (:MODULE MUSIC               "MUSIC")
  (:MODULE MUSIC-COMPILER      "MUSIC-INTERPRETER")  
  (:MODULE FAST-RB	       "FAST-RUBBERBANDING")
  (:MODULE COMP-FLAV           "COMP-FLAV-METH")
  (:MODULE LAST                ("INITIALIZATIONS" "DEBUG"))
  (:MODULE DEFINE-TV           ("DEFINE-TV-PACKAGE"))
  (:MODULE DEFINE-W            ("DEFINE-W-PACKAGE"))
  (:MODULE BITMAPS             "GODZILLA-MOUSE.BITMAP")

  (:AUXILIARY                  BITMAPS)
  
  (:COMPILE-LOAD               DEFINE-TV)
  (:COMPILE-LOAD               DEFINE-W       (:FASLOAD DEFINE-TV))
  (:COMPILE-LOAD               DEFS           (:FASLOAD DEFINE-TV DEFINE-W))
  (:COMPILE-LOAD               G-DEFS         (:FASLOAD DEFINE-TV DEFINE-W DEFS))
  (:COMPILE-LOAD               CSIB           (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS))
  (:COMPILE-LOAD               MAP            (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS CSIB))
  (:COMPILE-LOAD               PRIMITIVES     (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS))

  (:COMPILE-LOAD               MAIN           (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES))
  (:COMPILE-LOAD               BASWIN         (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN))
  (:COMPILE-LOAD               OTHER-MAIN     (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES BASWIN MAIN))
  (:COMPILE-LOAD               STREAM-ET-AL   (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES BASWIN MAIN
                                                    OTHER-MAIN))

  (:COMPILE-LOAD               GRAPH          (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL))
  (:COMPILE-LOAD               MENU-NEW       (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL))
  (:COMPILE-LOAD               MENU-OLD       (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL MENU-NEW))
  (:COMPILE-LOAD               MENU-EXTENSION (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL MENU-NEW MENU-OLD))
  (:COMPILE-LOAD               THE-REST       (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD))
  (:COMPILE-LOAD               SAM            (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD THE-REST))
  (:COMPILE-LOAD               SCRED2         (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD THE-REST))
  (:COMPILE-LOAD               FAST-RB        (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD THE-REST))
  (:COMPILE-LOAD               TYPWIN         (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD THE-REST
                                                        SCRED2))
  (:COMPILE-LOAD               SCROLL         (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD
                                                        THE-REST SCRED2 TYPWIN))
  (:COMPILE-LOAD               CHOICE-ET-AL   (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD
                                                        THE-REST SCRED2 TYPWIN SCROLL))
  (:COMPILE-LOAD               OBSOLETE-SCROLL-BAR (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN
                                                             BASWIN OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW
                                                             MENU-OLD THE-REST SCRED2 TYPWIN SCROLL
                                                             CHOICE-ET-AL))
  (:COMPILE-LOAD               SEQ-SEL        (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD
                                                        CHOICE-ET-AL OBSOLETE-SCROLL-BAR))
  (:COMPILE-LOAD               SFONT          (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD
                                                        THE-REST SCRED2 TYPWIN SCROLL
                                                        CHOICE-ET-AL OBSOLETE-SCROLL-BAR))
  (:FASLOAD                    RFONT          (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD
                                                        THE-REST SCRED2 TYPWIN SCROLL
                                                        CHOICE-ET-AL OBSOLETE-SCROLL-BAR))
  (:COMPILE-LOAD               MUSIC-COMPILER (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD
                                                        THE-REST SCRED2 TYPWIN SCROLL
                                                        CHOICE-ET-AL OBSOLETE-SCROLL-BAR))
			       ;; Kludge to keep music-compiler from being loaded.  It works
			       ;; by specifying a load condition predicate that returns NIL.
			       ;;NIL NIL IGNORE)
  (:COMPILE-LOAD-INIT          MUSIC (MUSIC-COMPILER) (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES
                                                                MAIN BASWIN OTHER-MAIN STREAM-ET-AL MENU-NEW
                                                                MENU-OLD THE-REST SCRED2 TYPWIN SCROLL
                                                                CHOICE-ET-AL OBSOLETE-SCROLL-BAR
                                                                MUSIC-COMPILER))
  (:COMPILE-LOAD-INIT          COMP-FLAV (DEFS G-DEFS PRIMITIVES MAIN BASWIN OTHER-MAIN STREAM-ET-AL GRAPH
                                               MENU-NEW MENU-OLD THE-REST SEQ-SEL MUSIC-COMPILER)
			       (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN OTHER-MAIN
                                         STREAM-ET-AL GRAPH MENU-NEW MENU-OLD THE-REST SCRED2 TYPWIN SCROLL
                                         CHOICE-ET-AL OBSOLETE-SCROLL-BAR SEQ-SEL MUSIC-COMPILER))
      
      (:COMPILE-LOAD           LAST           (:FASLOAD DEFINE-TV DEFINE-W DEFS G-DEFS PRIMITIVES MAIN BASWIN
                                                        OTHER-MAIN STREAM-ET-AL GRAPH MENU-NEW MENU-OLD
                                                        THE-REST SCRED2 TYPWIN SCROLL
                                                        CHOICE-ET-AL OBSOLETE-SCROLL-BAR SEQ-SEL
                                                        MUSIC-COMPILER)))
