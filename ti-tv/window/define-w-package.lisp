;;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1985-1989 Texas Instruments Incorporated. All rights reserved.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 05/23/88   KJF           Added symbols for needed for multiple screen/dual monitor support.
;;  02/11/88   KJF           Changed 1000%-white to 100%-white.  I'm 99.999999% sure it was a typo.
;;;  8/31/87    PMH         Added changes for color
;;;   4/08/87  TWE	Backed out the changes for the W typeout-window flavors.  This is going to take a lot
;;;			more work to get it right.
;;;   4/01/87  TWE	Completed what was done on 3/26/87 by adding
;;;			ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN and WINDOW-WITH-TYPEOUT-MIXIN
;;;			to the export/shadow list.
;;;   3/26/87  TWE	Fixed up the exporting of TYPEOUT-WINDOW and
;;;			TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS by exporting the W versions of
;;;			these symbols from the W package.
;;;   3/26/87  KK	          Shadow TYPEOUT-WINDOW and TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS
;;;                             so that the TV and W versions of these flavors can be different. This allows
;;;                             such things as the Zmacs typeout window to use W:WINDOW and
;;;                             W:GRAPHICS-MIXIN.
;;;   3/23/87  TWE	Exported some symbols as per Merrill Cornish's message.
;;;   2/11/87  TWE	Added command-menu-pane to complete the fix made to FRAME.
;;; 12/11/86   JEB 	Exported the rest of the symbols defined in Window;Globals from W.
;;; 11/05/86   TWE 	Exported some more menu functions from W to TV.
;;; 11/04/86   TWE 	Added GEOMETRY-FILL-P for obsolete-menu code.
;;; 11/04/86   JEB 	Added CPTFONT-FONT and MEDFNB-FONT to the export list to export from W.
;;; 10/30/86   TWE 	Added TRANSFORM-MIXIN.  Added a new export list to export to TV those symbols
;;;			which are defined in W that used to be defined in TV.
;;; 08/04/86   TWE	Deleted the package W creation from this file.  We don't need it here anymore since
;;;			it is at the end of the define-tv-package file.
;;; 08/04/86   TWE	Fixed the package definition of W to use TICL too.
#|

On 3-june-86 TWE commented out some of  the symbols in the EXPORT form  in
order to get around a problem with  the EXPORT form.  The problem is  that the
preceeding SHADOW form makes its symbols  internal in the W package  and the
EXPORT function  does  another  import,  which  gets a NAME-CONFLICT-ERROR
because the symbol is already there.  When the EXPORT function is fixed to  not
get an error upon IMPORT, then these symbols need to be uncommented.

|#

#|

This file defines the W package and all of the symbol-related things that  go
on with it.  Any time a new symbol  is defined in the W package, these  lists
need to be updated if that symbol is intended to be external or if it has the
same name as a symbol in the TV package.

|#

;;; The following are symbols in W which used to be defined in TV.


(EXPORT '(
	  GEOMETRY-FILL-P
	  GEOMETRY-N-COLUMNS
	  GEOMETRY-N-ROWS
	  GEOMETRY-INSIDE-WIDTH
	  GEOMETRY-INSIDE-HEIGHT
	  GEOMETRY-MAX-WIDTH
	  GEOMETRY-MAX-HEIGHT
	  MENU-EXECUTE-NO-SIDE-EFFECTS
	  MENU-ITEM-WHO-LINE-DOCUMENTATION)
	'TV)

;;; The following are symbols which have equivalent, but different, versions in TV.
(SHADOW '(
          "COMMAND-MENU-PANE"
          "GRAPHICS-MIXIN"
	  "MENU"
	  "MENU-CHOOSE"
	  "MENU-COMPUTE-FONT-MAP"
	  "MENU-COMPUTE-GEOMETRY"
	  "MENU-COMPUTE-ROW-MAP"
	  "MENU-DEDUCE-PARAMETERS"
	  "MENU-DYNAMIC-ITEM-LIST"
	  "MENU-FILL-WIDTH"
	  "MENU-ITEM-SORTER"
	  "MENU-ITEM-STRING-WIDTH"
	  "MENU-MARGIN-CHOICE-FROM-ITEM"
	  "MENU-MARGIN-CHOICE-FUNCTION"
	  "MENU-MAX-WIDTH"
	  "MENU-SIMPLE-MARGIN-CHOICE-FROM-ITEM"
	  "MENU-UPDATE-MOUSE-CURSOR"
	  "MULTICOLUMN-MENU-CHOOSE"
	  "MULTIPLE-MENU-CHOOSE"
	  "WINDOW"
	  "WINDOW-WITH-INSIDE-SCROLL-BAR"
	  "WINDOW-WITHOUT-LABEL")
        'W)

(EXPORT
  ;; Except where noted, all symbols in this list are functions.
  '(

;; Symbols added for multiple screen/dual monitor support.
;; These are only those which we may want to document. 05/23/88 KJF

	  ;; From TVDEFS
	  color-sheet-p
	  *screens-exposed-at-disk-save*
	  *ok-to-expose-color-screens-when-color-presence-test-failed*
	  *initial-screen*

	  *previously-selected-screens*
	  *current-screens*
	  *screens-to-refresh*
	  *convert-color-sheet-to-monochrome*

	  ;; From CSIB
	  swap-default-f-b

	  ;; From SHWARM
	  screen-exposable-p
	  screen-p
	  who-line-screen-p
	  associated-who-line-screen
	  remove-from-screens-previously-selected-windows
	  initial-screen-setup
	  expose-initial-screens
	  compress-array

	  ;; From SCREENS
	  make-a-screen
	  make-who-line-screen
	  create-color-screen
	  move-sheet-to-another-screen
	  kbd-screen-redisplay-some
	  kbd-switch-screens
;;

    100%-white					;new for color
    100%-black					;new for color
    0%-GRAY				   ; Variable
    100%-GRAY				   ; Variable
    *APPLICATION-WINDOW*		   ; VARIABLE
    *BEEP-TYPES*			   ; VARIABLE
    *BEEPING-FUNCTIONS*			   ; VARIABLE
    *CONTROL-REGISTER*			   ; VARIABLE
    *DEFAULT-COLOR-TABLE*		   ; VARIABLE
    *DEFAULT-LABEL-BACKGROUND*             ; VARIABLE
    *DEFAULT-LABEL-FOREGROUND*             ; VARIABLE
    *default-menu-foreground*		   ; VARIABLE
    *default-menu-background*		   ; VARIABLE
    *default-menu-label-foreground*	   ; VARIABLE
    *default-menu-label-background*	   ; VARIABLE
    *default-documentation-foreground*	   ; VARIABLE
    *default-documentation-background*	   ; VARIABLE
    *default-status-foreground*		   ; VARIABLE
    *default-status-background*		   ; VARIABLE
    *default-blinker-offset*		   ; VARIABLE
    *DEFAULT-MENU-SEARCH-DISPLAY-MODE*	   ; VARIABLE
    *FONT-LIST*				   ; VARIABLE
    *SYSTEM-MENU-DEBUG-TOOLS-COLUMN*	   ; VARIABLE
    *SYSTEM-MENU-EDIT-WINDOWS-COLUMN*	   ; VARIABLE
    *SYSTEM-MENU-PROGRAMS-COLUMN*	   ; VARIABLE
    *SYSTEM-MENU-USER-AIDS-COLUMN*	   ; VARIABLE
    ALL-SPRITES
    ALU-ALIST
    B&W-TABLE				   ; Variable
    BLACK
    CACHE-WINDOW
    CHECK-FOR-COLOR-SCREEN		   ; Macro
    CHOOSE-OBJECT-VALUES-PROCESS-MESSAGE
    CHOOSE-VARIABLE-VALUES-PRINT-FUNCTION
    COLOR-ALIST
    COLOR-EXISTS-P
    COLOR-MAP-OFF
    COLOR-MAP-ON
    COLOR-SYSTEM-P
    COMBINE
    COMMAND-MENU-PANE                      ; Flavor
    CONVERT-TO-COLOR
    COPY-COLOR-MAP
    COPY-SPEECH
    CPTFONT-FONT			   ; Variable
    CREATE-COLOR-MAP
    CURRENT-COLOR-LUT-BUFFER
    DEF-BEEP-FUNCTION			   ; MACRO
    DEFAULT-COLOR-ALIST
    DEFSOUND				   ; MACRO
    DETERMINANT
    DIST
    DOWNLOAD-COLOR-LUT-BUFFER
    DRAW-CLIPPED-SOLID-TRIANGLE
    DRAW-ICON
    ERASE
    GRAPHICS-MIXIN                         ; Flavor
    MAKE-SIMPLE-ICON
    FONT
    FONT-OBJECT-P
    G-ROUND
    GET-COLOR-ALIST
    GET-COLOR-LUT-BUFFER
    GET-DISPLAY-TYPE
    GET-LINE-FROM-KEYBOARD
    IDENTITY-ARRAY			   ; Parameter
    INTERACTION-PANE                       ;flavor
    ITEM-TYPE-ALIST			   ; INSTANCE VARIABLE
    KBD-IO-BUFFER			   ; VARIABLE
    LINE-DELTAS
    LOAD-PICTURE
    MAKE-COLOR-MAP
    MAKE-FONT-PURPOSE
    MAXF
    MEDFNB-FONT				   ; Variable
    MENU-CHOOSE
    MENU-COMPUTE-FONT-MAP
    MENU-COMPUTE-GEOMETRY
    MENU-COMPUTE-ROW-MAP
    MENU-DEDUCE-PARAMETERS
    MENU-DEFAULT-COMMAND-CHARACTERS	   ; Variable
    MENU-DYNAMIC-ITEM-LIST
    MENU-FILL-WIDTH
    MENU-ITEM-SORTER
    MENU-ITEM-STRING-WIDTH
    MENU-MARGIN-CHOICE-FROM-ITEM
    MENU-MARGIN-CHOICE-FUNCTION
    MENU-MAX-WIDTH
    MENU-SIMPLE-MARGIN-CHOICE-FROM-ITEM
    MENU-UPDATE-MOUSE-CURSOR
    MENU				   ; Flavor and resource
    MINF
    MULTICOLUMN-MENU-CHOOSE
    MULTIPLE-MENU-CHOOSE
    NORMAL
    ON-VOLUME				   ; VARIABLE
    OPPOSITE
    PICTURE
    PLAY
    POLYLINE-MIN-MAX
    POP-UP-TEXT-WINDOW-WITHOUT-MORE	   ; RESOURCE
    PROCESS-WHO-LINE-DOCUMENTATION-LIST
    RADIANS-PER-DEGREE
    RASTER-CHARACTER
    READ-COLOR-LUT-BUFFER
    READ-COLOR-MAP
    READ-SPEECH
    REC
    RENAME-SPEECH
    REVERSE-ALU
    REVERSE-ALU-TRANSLATION-TABLE	   ; Constant
    SAVE-SPEECH
    SCALE-PIXEL-ARRAY-INTO-WINDOW
    SCALEF
    SECTOR-CODE
    SET-COLOR-MAP
    SPECIAL-CHARS
    SPRITE-WINDOW
    STANDARD				   ; Parameter
    STANDARD-FONT			   ; Variable
    TRANSFER-COLOR-LUT-BUFFER
    TRANSFORM-MIXIN			   ; Flavor
    TRANSFORM-POINT
    TWO-PI
    UNTRANSFORM-DELTAS
    VECTOR-CHARACTER
    VOLUME
    WHITE
    WINDOW                                 ; Flavor
    WINDOW-WITH-INSIDE-SCROLL-BAR          ; Flavor
    WINDOW-WITHOUT-LABEL                   ; Flavor
    WITH-SOUND-ENABLED			   ; MACRO
    WRITE-COLOR-LUT-BUFFER
    WRITE-COLOR-MAP
    X-CENT
    )
  'W)




#|

HACK ALERT!!!  The following form is a hack to get the IMPORT form to be
evaluated instead  of  compiled.   The  problem  is that the compiler
doesn't handle IMPORT properly.  By putting this form into a  DEFVAR,
the form doesn't get evaluated until load time HACK ALERT!!!

|#
(EVAL-WHEN (COMPILE LOAD EVAL)
  (DEFPARAMETER W:HACK-ALERT-FOR-IMPORT '(PROGN
					   (IMPORT '(
						     TV::*DELAY-COMPUTE-GEOMETRY*
						     TV::*MENU-ITEM-STANDARD-FONT*
						     TV::BOTTOM-SHADOW-WIDTH
						     TV::CENTER-WINDOW-AROUND
						     TV::CHOICE-BOX-FUNCTION
						     TV::CHOICE-BOX-NAME
						     TV::DELAYING-COMPUTE-GEOMETRY
						     TV::LOWEST-SHEET-UNDER-POINT
						     TV::KBD-TERMINAL-TIME
						     TV::MOUSE-RECONSIDER
						     TV::RIGHT-SHADOW-WIDTH
						     TV::SCROLL-BAR-ICON-WIDTH
						     TV::SHEET-DISPLAY-CENTERED-STRING
						     TV::TEMPORARY-BIT-ARRAY
						     TV::WHO-LINE-DOCUMENTATION-WINDOW)
						   'W)
					   )))
(EVAL-WHEN (COMPILE LOAD EVAL)
  (EVAL W:HACK-ALERT-FOR-IMPORT))

;(UNLESS (EQ  'TV::RIGHT-SHADOW-WIDTH 'W:RIGHT-SHADOW-WIDTH)
;  (FORMAT T "Hi David, the value is NIL when it should be T."))

;;; Make sure that a user that specifies a W package
;;; prefix also gets the reasonable symbols from TV.
(EXPORT (EVAL (FIND-SYMBOL "ALL-TV-DOCUMENTED-SYMBOLS" 'TV)) 'W)