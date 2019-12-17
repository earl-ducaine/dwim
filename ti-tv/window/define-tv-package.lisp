;;; -*- Mode:Common-Lisp; Package:TV; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1985-1989 Texas Instruments Incorporated.  All rights reserved.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 05/23/88   KJF           Added symbols for needed for multiple screen/dual monitor support.
;;;  02/11/88  KJF           Changes for dual monitor and multiple screen support.  Search for KJF.
;;;   8/31/87   PMH         Added color support
;;;   4/24/87  TWE	Moved the shadowing form for symbols in the W package to this file.
;;;   4/08/87  TWE	Backed out the changes for the W typeout-window flavors.  This is going to take
;;;			a lot more work to get it right.
;;;   4/01/87  TWE	Fixed up ESSENTIAL-WINDOW-WITH-TYPEOUT-WINDOW-MIXIN and
;;;			WINDOW-WITH-TYPEOUT-WINDOW-MIXIN to complete what was done on 3/26/87.
;;;   3/26/87  TWE	Fixed up the shadowing for TYPEOUT-WINDOW and
;;;			TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS by removing them from the W
;;;			export list in this file.
;;;   3/25/87  TWE	Exported mouse-save-image as per bug 4298.
;;;   3/23/87  TWE	Exported some symbols as per Merrill Cornish's message.
;;;   3/16/87  TWE	Added some who line symbols for Anna.
;;;    3/1/87  TWE		Added some mouse and bitmap variables.
;;;   2/19/87  TWE	Added SCROLL-PIXEL-INCREMENT for UCL.
;;;   2/18/87  TWE	Removed the hack alert (see 12/11/86) since the problem symbols were corrected.
;;;   2/17/87  JEB		Added several symbols used by GED.
;;;   2/17/87  TWE	Added many symbols that needed to be exported from TV before they are exported
;;;			from W.
;;;   2/13/87  JEB		Added a CVV variable for GED.
;;;   2/10/87  TWE	Defined the mouse mapping functions and bit functions to be in the W package.
;;;			This closes bugs 3377 and 3379.  Also defined some CVV instance variables for
;;;			GED.  Added more mouse ones for Glenda
;;;   2/09/87  TWE	Added *HOLLOW-M-CHOICE-BOX-P* for bug 3340.
;;;   2/03/87  TWE	Added the following symbols for the test suite: CONSTRAINTS, EXPAND-WINDOW,
;;;			GET-WINDOW-EDGE-ALIST, INITIALIZE-MULTIPLE-MOVE-BLINKER, MOUSE-LAST-X
;;;			MOUSE-LAST-Y, and MULTIPLE-MOVE-BLINKER.
;;;   1/29/87  TWE	Added DEFAULT-BORDER-SIZE to more properly implement the defaulting of border
;;;			border widths for permanent menus.  Also added HYSTERESIS for the menu fix.
;;;   1/22/87  LGO	Added *DESELECTED-PROCESS-PRIORITY*
;;;   1/16/87  TWE	Changed the symbol hack to make the build process cleaner.
;;;   1/15/87  KDB	          Added SHEET-MENU-PERMANENT.
;;; 01/09/87   TWE 	Added EDIT-SCREEN and SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW
;;; 01/05/87   JEB 	Added WITH-CLIPPING-RECTANGLE.
;;; 12/29/86   TWE 	Added BLINK to the W list.
;;; 12/24/86   JEB 	Added BLINK.
;;; 12/11/86   TWE 	Added a hack to remove symbols that are prematurely interned into the W package.
;;; 12/04/86   TWE 	Added SELECTED-IO-BUFFER and RUBOUT-HANDLER.
;;; 12/01/86   TWE 	Added *DEFAULT-SCROLL-BAR-COLOR*.
;;; 11/26/86   TWE 	Added more SHEET-... symbols and DRAW-RECTANGLE-INSIDE-CLIPPED.  Also added
;;;			the typeout-item symbols from the typeout-item defstruct.  Also exported some
;;;			color symbols.
;;; 11/25/86   TWE 	Added MOUSE-PROCESS and BLINKER-VISIBILITY.
;;; 11/20/86   TWE 	Added EXPOSE-WINDOW-NEAR.  Added the instance variables for the blinker flavor.
;;; 11/19/86   TWE 	Added the following: blinker-deselected-visibility, blinker-set-visibility, sheet-clear
;;;			sheet-clear-eof, sheet-clear-eol, sheet-compute-motion, sheet-set-cursorpos
;;;			for Zmacs.
;;; 11/13/86   TWE 	Added SHEET-CAN-GET-LOCK.
;;; 10/31/86   TWE 	Added HIGHLIGHTED-ITEMS.
;;; 10/30/86   TWE 	Removed WINDOW from being exported into TV because it is redefine
;;;			as a new flavor there.
;;; 10/29/86   TWE 	Added SHEET-DEEXPOSED-TYPEIN-NOTIFY and SHEET-TYO.
;;; 10/28/86   TWE 	Added DEFERRED-NOTIFICATIONS.
;;; 10/27/86   TWE 	Added AWAIT-USER-TYPEAHEAD.
;;; 10/24/86   TWE 	Added LABEL-NEEDS-UPDATING and IO-BUFFER for Zmacs.
;;; 10/23/86   TWE 	Made all-tv-documented-symbols a defparameter instead of a defvar.  This means
;;;			that when this file is loaded, the value of all-tv-documented-symbols will always be
;;;			updated.
;;; 10/22/86   TWE 	Added the symbol FONT-FILL-POINTER.
;;; 10/17/86   TWE 	Added the symbol DISPLAY-FONT for Zmacs and the FED.
;;; 10/15/86   TWE 	Added all instance variable symbols from the tv:sheet flavor to the
;;;			all-tv-documented-symbols list.  This was done so that methods in the W package
;;;			can refer to instance variables which are defined in flavor which is in the TV
;;;			package and not have to put a TV package prefix in front of the instance variables.
;;;			(Did I hear you say: "What a pain!"?)
;;; 10/14/86   TWE 	Added symbol FONT-CHAR-MIN-RASTER-WIDTH for Zmacs and the FED.
;;; 10/06/86   JEB 	Added symbols BOTTOM-MARGIN-SIZE, LEFT-MARGIN-SIZE, RIGHT-MARGIN-SIZE,
;;;			TOP-MARGIN-SIZE, CHAR-ALUF and ERASE-ALUF.
;;; 10/03/86   TWE 	Added symbol SCROLL-BAR-MODE.
;;; 10/01/86   JEB 	Added function MAKE-GRAY
;;; 09/25/86   TWE	Added CHOOSE-VARIABLE-VALUES-KEYWORD and
;;;			CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION which are CVV properties.
;;; 09/16/86   TWE	Added *UNEXPECTED-SELECT-DELAY*.
;;; 08/29/86   TWE	Fixed the creation done on 8/3/86 to perform the package test better for 2.x and
;;;			3.0 compatibility.
;;; 08/28/86   TWE	Added the symbols which define the mouse glyph constants.
;;; 08/03/86   TWE	Corrected the W package creation to properly setup the TICL package nickname for
;;;			release 2.x systems.
;;; 08/03/86   TWE	Moved the creation of the W package to this file.
;;; 08/01/86   TWE	Changed the mode line to specify the TV package instead of no package.  When the
;;;			in-package form is fixed so that the proper package prefixes are generated then the
;;;			package component of the mode line should be removed.


#|

This file defines the TV package and all of the symbol-related
things that go on with it.  Any time a new symbol is defined in
the TV package, these lists need to be updated if that symbol is
intended to be external or is intended for general use via the W
package.

Some W symbols are defined here because of potential timing problems.

|#

(EVAL-WHEN (COMPILE LOAD EVAL)
  (UNLESS (FIND-PACKAGE "TV")
    (MAKE-PACKAGE "TV" :SIZE 5000. :USE '("SYS" "TICL" "LISP"))))
(IN-PACKAGE "TV")


;;; The following are symbols which have equivalent, but different, versions in TV.
;;; This form is present here so that if one creates a symbol such as W:MENU in their
;;; file and load the file, when the export is done on TV:MENU it doesn't get a name
;;; conflict.  By putting the shadow here we don't get a name conflict.

(EVAL-WHEN (COMPILE LOAD EVAL)
  (LET ((W-SHADOW-LIST '("COMMAND-MENU-PANE"
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
                         "WINDOW-WITHOUT-LABEL")))
    ;; The following is an attempt to work around a problem with SHADOW.  If the
    ;; symbol argument to SHADOW is a string and the symbol that corresponds to
    ;; that string is already present in the package, then SHADOW incorrectly creates
    ;; the symbol again in that package.  If the symbol argument is a symbol and it
    ;; is already present in the package, then SHADOW works properly.
    (SHADOW (LOOP FOR ELEMENT IN W-SHADOW-LIST
                  FOR SYMBOL = (FIND-SYMBOL ELEMENT 'W)
                  COLLECTING (IF SYMBOL SYMBOL ELEMENT))
            'W)
    (EXPORT (LOOP FOR SYMBOL IN W-SHADOW-LIST
                  COLLECTING (FIND-SYMBOL SYMBOL 'W))
            'W)))

(EXPORT '(

;; Symbols added for multiple screen/dual monitor support.  05-23-88 KJF.

	  ;; From TVDEFS
	  color-sheet-p
	  
	  *screens-exposed-at-disk-save*
	  *SIB-board-type*
	  *ok-to-expose-color-screens-when-color-presence-test-failed*
	  *initial-who-line-screen-bits-per-pixel*
	  *initial-who-line-screen-display-type*
	  *initial-screen*

	  *dual-monitors*
	  *color-who-line*
	  *mono-who-line*
	  *previously-selected-screens*
	  *current-screens*
	  *screens-to-refresh*
	  *default-sib*
	  *color-sprite-window*
	  *mono-sprite-window*
	  *initial-color-lisp-listener*
	  *color-screen*
	  *convert-color-sheet-to-monochrome*

	  ;; From CSIB
	  CSIB-in-slot-p
	  color-monitor-present-on-CSIB-p
	  swap-default-f-b
	  setup-mono-plane

	  ;; From SHEET
	  download-color-map-p
	  compare-properties
	  check-properties

	  ;; From SHWARM
	  screen-exposable-p
	  screen-p
	  who-line-screen-p
	  not-a-who-line-screen-p
	  associated-who-line-screen
	  associated-who-line-screen-is-exposable-p
	  same-monitor-p
	  associated-who-line-screen-is-on-same-monitor-p
	  associated-who-line-screen-does-not-overlap-p
	  *initial-screen-tests*
	  acceptable-initial-screen-p
	  *initial-who-line-screen-tests*
	  acceptable-initial-who-line-screen-p
	  create-acceptable-initial-who-line-screen
	  find-or-create-acceptable-initial-who-line-screen
	  create-acceptable-initial-screen
	  find-or-create-acceptable-initial-screen
	  remove-from-screens-previously-selected-windows
	  initial-screen-setup
	  expose-initial-screens
	  compress-array
	  
	  ;; From CONVERT
	  monochrome-color-conversion-arrays
	  convert-a-sheet-to-monochrome
	  convert-sheet-and-inferiors-to-monochrome
	  convert-a-sheet-to-color
	  convert-sheet-and-inferiors-to-color
	  convert-screen-to-color

	  ;; From SCREENS
	  make-a-screen
	  things-to-do-first-time
	  make-who-line-screen
	  reset-who-line-variables
	  make-color-sprite-window
	  update-global-window-variables
	  create-color-screen
	  move-sheet-to-another-screen
	  kill-screen
	  switch-to-dual
	  switch-to-single
	  kbd-screen-redisplay-some
	  return-matching-who-line-screen
	  dual-monitor-p
	  setup-previously-selected-screens-array
	  ADD-TO-PREVIOUSLY-SELECTED-SCREENS
	  REMOVE-FROM-PREVIOUSLY-SELECTED-SCREENS
	  kbd-switch-screens
;;

	  ;; Flavors
	  ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN
          ALIAS-FOR-INFERIORS-MIXIN
          ANY-TYI-MIXIN
          AUTOEXPOSING-MORE-MIXIN
          BASIC-CHOOSE-VARIABLE-VALUES
          BASIC-FRAME
          BASIC-MENU
          BASIC-MOMENTARY-MENU
          BASIC-MOUSE-SENSITIVE-ITEMS
          BASIC-MULTIPLE-CHOICE
          BASIC-SCROLL-BAR
          BASIC-SCROLL-WINDOW
          BASIC-TYPEOUT-WINDOW
          BITBLT-BLINKER
          BLINKER
          BORDERED-CONSTRAINT-FRAME
          BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER
          BORDERS-MIXIN
          BOTTOM-BOX-LABEL-MIXIN
          BOX-BLINKER
          BOX-LABEL-MIXIN
          CENTERED-LABEL-MIXIN
          CHARACTER-BLINKER
          CHOOSE-VARIABLE-VALUES-PANE
          CHOOSE-VARIABLE-VALUES-WINDOW
          COMMAND-MENU
          COMMAND-MENU-ABORT-ON-DEEXPOSE-MIXIN
          COMMAND-MENU-MIXIN
          COMMAND-MENU-PANE
          CONSTRAINT-FRAME
          CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER
          CURRENT-ITEM-MIXIN
          DELAY-NOTIFICATION-MIXIN
          DELAYED-REDISPLAY-LABEL-MIXIN
          DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW
          DONT-SELECT-WITH-MOUSE-MIXIN
          DYNAMIC-ITEM-LIST-MIXIN
          DYNAMIC-MOMENTARY-MENU
          DYNAMIC-MOMENTARY-WINDOW-HACKING-MENU
          DYNAMIC-MULTICOLUMN-MIXIN
          DYNAMIC-MULTICOLUMN-MOMENTARY-MENU
          DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU
          DYNAMIC-POP-UP-ABORT-ON-DEEXPOSE-COMMAND-MENU
          DYNAMIC-POP-UP-COMMAND-MENU
          DYNAMIC-POP-UP-MENU
          DYNAMIC-TEMPORARY-ABORT-ON-DEEXPOSE-COMMAND-MENU
          DYNAMIC-TEMPORARY-COMMAND-MENU
          DYNAMIC-TEMPORARY-MENU
          ESSENTIAL-ACTIVATE
          ESSENTIAL-EXPOSE
          ESSENTIAL-MOUSE
          ESSENTIAL-SCROLL-MOUSE-MIXIN
          ESSENTIAL-SET-EDGES
          ESSENTIAL-WINDOW
          ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN
          FLASHY-MARGIN-SCROLLING-MIXIN
          FLASHY-SCROLLING-MIXIN
          FRAME-DONT-SELECT-INFERIORS-WITH-MOUSE-MIXIN
          FRAME-FORWARDING-MIXIN
          FULL-SCREEN-HACK-MIXIN
          FUNCTION-TEXT-SCROLL-WINDOW
          GRAPHICS-MIXIN
          GRAY-DEEXPOSED-RIGHT-MIXIN
          GRAY-DEEXPOSED-WRONG-MIXIN
          HOLLOW-RECTANGULAR-BLINKER
          HYSTERETIC-WINDOW-MIXIN
          IBEAM-BLINKER
          INFERIORS-NOT-IN-SELECT-MENU-MIXIN
          INITIALLY-INVISIBLE-MIXIN
	  INTERACTION-PANE
          INTRINSIC-NO-MORE-MIXIN
          KBD-MOUSE-BUTTONS-MIXIN
          LABEL-MIXIN
          LINE-AREA-MOUSE-SENSITIVE-TEXT-SCROLL-MIXIN
          LINE-AREA-TEXT-SCROLL-MIXIN
          LINE-TRUNCATING-MIXIN
          LISP-INTERACTOR
          LIST-MOUSE-BUTTONS-MIXIN
          LISTENER-MIXIN
          LISTENER-MIXIN-INTERNAL
          MAGNIFYING-BLINKER
          MARGIN-CHOICE-MENU
          MARGIN-CHOICE-MIXIN
          MARGIN-MULTIPLE-MENU-MIXIN
          MARGIN-REGION-MIXIN
          MARGIN-SCROLL-MIXIN
          MARGIN-SCROLL-REGION-ON-AND-OFF-WITH-SCROLL-BAR-MIXIN
          MENU
          MENU-EXECUTE-MIXIN
          MENU-HIGHLIGHTING-MIXIN
          MENU-MARGIN-CHOICE-MIXIN
          MINIMUM-WINDOW
          MOMENTARY-MARGIN-CHOICE-MENU
          MOMENTARY-MENU
          MOMENTARY-MULTIPLE-MENU
          MOMENTARY-WINDOW-HACKING-MENU
          MOUSE-BLINKER-MIXIN
          MOUSE-BOX-BLINKER
          MOUSE-BOX-STAY-INSIDE-BLINKER
          MOUSE-CHARACTER-BLINKER
          MOUSE-HOLLOW-RECTANGULAR-BLINKER
          MOUSE-RECTANGULAR-BLINKER
          MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
          MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK
          MULTIPLE-CHOICE
          MULTIPLE-MENU
          NO-SCREEN-MANAGING-MIXIN
          NOT-EXTERNALLY-SELECTABLE-MIXIN
          NOTIFICATION-MIXIN
          PANE-MIXIN
          PANE-NO-MOUSE-SELECT-MIXIN
          POP-UP-TEXT-WINDOW
          PREEMPTABLE-READ-ANY-TYI-MIXIN
          PROCESS-MIXIN
          RECTANGULAR-BLINKER
          RESET-ON-OUTPUT-HOLD-FLAG-MIXIN
          REVERSE-CHARACTER-BLINKER
          SCREEN
	  SCROLL-BAR-MIXIN
          SCROLL-MOUSE-MIXIN
          SCROLL-PIXEL-INCREMENT
          SCROLL-STUFF-ON-OFF-MIXIN
          SCROLL-WINDOW
          SCROLL-WINDOW-WITH-TYPEOUT
          SCROLL-WINDOW-WITH-TYPEOUT-MIXIN
          SELECT-MIXIN
          SHADOW-BORDERS-MIXIN
          SHEET
          SHOW-PARTIALLY-VISIBLE-MIXIN
          STAY-INSIDE-BLINKER-MIXIN
          STREAM-MIXIN
          TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW
          TEMPORARY-MENU
          TEMPORARY-MULTIPLE-CHOICE-WINDOW
	  TEMPORARY-SHADOW-BORDERS-WINDOW-MIXIN
          TEMPORARY-WINDOW-MIXIN
          TEXT-SCROLL-WINDOW
          TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK
          TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN
          TOP-BOX-LABEL-MIXIN
          TOP-LABEL-MIXIN
          TRUNCATING-POP-UP-TEXT-WINDOW
          TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET
          TRUNCATING-WINDOW
          TYPEOUT-WINDOW
          TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS
          WINDOW
          WINDOW-HACKING-MENU-MIXIN
          WINDOW-PANE
          WINDOW-WITH-TYPEOUT-MIXIN
          ;; FUNCTIONS
          %DRAW-RECTANGLE-CLIPPED
          12-HOUR-CLOCK-SETUP
          24-HOUR-CLOCK-SETUP
          ADD-ESCAPE-KEY
          ADD-SYSTEM-KEY
          ADD-TERMINAL-KEY
          ADD-TO-SYSTEM-MENU-COLUMN
          ADD-TO-SYSTEM-MENU-PROGRAMS-COLUMN
          ADD-TYPEOUT-ITEM-TYPE
	  ADD-WINDOW-TYPE
          ADJUST-BY-INTERVAL
          AWAIT-USER-TYPEAHEAD
          AWAIT-WINDOW-EXPOSURE
          BASIC-TYPEOUT-WINDOW-BOTTOM-REACHED
          BASIC-TYPEOUT-WINDOW-INCOMPLETE-P
          BEEP
          BLACK-ON-WHITE
	  BLINK
          BLINKER-DESELECTED-VISIBILITY
          BLINKER-READ-CURSORPOS
          BLINKER-SET-CHARACTER
          BLINKER-SET-CURSORPOS
          BLINKER-SET-SHEET
          BLINKER-SET-SIZE
          BLINKER-SET-VISIBILITY
          BLINKER-SHEET
          BLINKER-VISIBILITY
          CAREFUL-NOTIFY
          CHOOSE-PROCESS-IN-ERROR
          CHOOSE-VARIABLE-VALUES
          CHOOSE-VARIABLE-VALUES-FIND-CONTINUATION-ITEMS
          CHOOSE-VARIABLE-VALUES-PRINT-FUNCTION
          CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE
          CLOSE-ALL-SERVERS
          COLOR-SYSTEM-P
          COMPLEMENT-BOW-MODE
	  CONVERT-TO-COLOR
	  COPY-COLOR-MAP
          COPY-SPEECH
	  CREATE-COLOR-MAP
	  CURRENT-COLOR-LUT-BUFFER
          DEF-BEEP-FUNCTION
          DEF-BEEP-TYPE
          DEFAULT-BEEP
          DEFSOUND
          DEFWINDOW-RESOURCE
          DELAYING-SCREEN-MANAGEMENT
          DELETE-FROM-SYSTEM-MENU-COLUMN
          DESCRIBE-SERVERS
          DESELECT-AND-MAYBE-BURY-WINDOW
          DO-SOUND
          DOORBELL
          DOPLIST
	  DOWNLOAD-COLOR-LUT-BUFFER
          DRAW-CHAR
          DRAW-CHAR-DOWN
          DRAW-CHAR-UP
          DRAW-RECTANGLE-INSIDE-CLIPPED
          DRAW-RECTANGULAR-BORDER
          EDIT-SCREEN
          ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN-TYPEOUT-WINDOW
          EXPOSE-WINDOW-NEAR
          FIND-PROCESS-IN-ERROR
          FIND-WINDOW-OF-FLAVOR
          FLUSH-FULL-SCREEN-BORDERS
          FONT-BASELINE
          FONT-BLINKER-HEIGHT
          FONT-BLINKER-WIDTH
          FONT-CHAR-HEIGHT
	  FONT-CHAR-MIN-RASTER-WIDTH
          FONT-CHAR-WIDTH
          FONT-CHAR-WIDTH-TABLE
          FONT-CHARS-EXIST-TABLE
          FONT-EVALUATE
          FONT-FILL-POINTER
          FONT-INDEXING-TABLE
          FONT-LEFT-KERN-TABLE
          FONT-NAME
          FONT-OBJECT-P
          FONT-RASTER-HEIGHT
          FONT-RASTER-WIDTH
          FONT-RASTERS-PER-WORD
          FONT-WORDS-PER-CHAR
	  GET-COLOR-LUT-BUFFER
	  GET-DISPLAY-TYPE
          GET-LINE-FROM-KEYBOARD
          GET-VISIBILITY-OF-ALL-SHEETS-BLINKERS
	  HIGHLIGHTED-ITEMS
          IDLE-LISP-LISTENER
          IO-BUFFER
          IO-BUFFER-CLEAR
          IO-BUFFER-EMPTY-P
          IO-BUFFER-FULL-P
          IO-BUFFER-GET
          IO-BUFFER-INPUT-FUNCTION
          IO-BUFFER-INPUT-POINTER
          IO-BUFFER-LAST-INPUT-PROCESS
          IO-BUFFER-LAST-OUTPUT-PROCESS
          IO-BUFFER-OUTPUT-FUNCTION
          IO-BUFFER-OUTPUT-POINTER
          IO-BUFFER-PLIST
          IO-BUFFER-PUSH
          IO-BUFFER-PUT
          IO-BUFFER-RECORD
          IO-BUFFER-RECORD-POINTER
          IO-BUFFER-SIZE
          IO-BUFFER-STATE
          IO-BUFFER-UNGET
          KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER
          KBD-CHAR-TYPED-P
          KBD-DEFAULT-OUTPUT-FUNCTION
          KBD-INTERCEPT-ABORT
          KBD-INTERCEPT-ABORT-ALL
          KBD-INTERCEPT-BREAK
          KBD-INTERCEPT-ERROR-BREAK
          KBD-IO-BUFFER-GET
          KBD-SNARF-INPUT
          KBD-WAIT-FOR-INPUT-OR-DEEXPOSURE
          KBD-WAIT-FOR-INPUT-WITH-TIMEOUT
          KEY-STATE
          LABEL-BOTTOM
          LABEL-CENTERED
          LABEL-FONT
          LABEL-LEFT
          LABEL-NEEDS-UPDATING
          LABEL-RIGHT
          LABEL-STRING
          LABEL-TOP
          LABEL-VSP
          LOCK-SHEET
          MAIN-SCREEN-AND-WHO-LINE
          MAKE-BLINKER
	  MAKE-COLOR-MAP
          MAKE-DEFAULT-IO-BUFFER
          MAKE-FONT
	  MAKE-GRAY
          MAKE-IO-BUFFER
          MAKE-FONT-PURPOSE
          MAKE-SHEET-BIT-ARRAY
          MAKE-WINDOW
          MAP-OVER-EXPOSED-SHEET
          MAP-OVER-EXPOSED-SHEETS
          MAP-OVER-SHEET
          MAP-OVER-SHEETS
          MARGIN-REGION-AREA
          MARGIN-REGION-BOTTOM
          MARGIN-REGION-FUNCTION
          MARGIN-REGION-LEFT
          MARGIN-REGION-MARGIN
          MARGIN-REGION-RIGHT
          MARGIN-REGION-SIZE
          MARGIN-REGION-TOP
          MENU-CHOOSE
          MENU-COMPUTE-GEOMETRY
          MENU-ITEM-STRING
          MERGE-SHIFT-KEYS
          MOUSE-BUTTON-ENCODE
          MOUSE-BUTTONS
          MOUSE-CALL-SYSTEM-MENU
	  MOUSE-CHARACTER-BUTTON-ENCODE
          MOUSE-CONFIRM
          MOUSE-DEFAULT-HANDLER
          MOUSE-DEFINE-BLINKER-TYPE
          MOUSE-DISCARD-CLICKAHEAD
          MOUSE-GET-BLINKER
          MOUSE-INPUT
          MOUSE-SAVE-IMAGE
          MOUSE-SELECT
	  MOUSE-SENSITIVE-ITEM
          MOUSE-SET-BLINKER
          MOUSE-SET-BLINKER-CURSORPOS
          MOUSE-SET-BLINKER-DEFINITION
          MOUSE-SET-SHEET
          MOUSE-SET-SHEET-THEN-CALL
          MOUSE-SET-WINDOW-POSITION
          MOUSE-SET-WINDOW-SIZE
          MOUSE-SPECIFY-RECTANGLE
          MOUSE-STANDARD-BLINKER
          MOUSE-WAIT
          MOUSE-WAKEUP
          MOUSE-WARP
          MOUSE-Y-OR-N-P
          MULTICOLUMN-MENU-CHOOSE
          MULTIPLE-CHOOSE
          MULTIPLE-MENU-CHOOSE
          NOISE
          NOTIFY
          OPEN-ALL-SHEETS-BLINKERS
          OPEN-BLINKER
          PLAY
          POP-UP-NOTIFY
          PREPARE-SHEET
          PRESERVE-SUBSTITUTE-STATUS
          PRINT-NOTIFICATIONS
          PRINT-STRING-WITH-INDENTATION
          PRINT-UNIVERSAL-TIME-OR-NEVER
          PROCESS-TYPEAHEAD
          PROCESS-WHO-LINE-DOCUMENTATION-LIST
	  READ-ANY
	  READ-ANY-NO-HANG
	  READ-COLOR-LUT-BUFFER
	  READ-COLOR-MAP
	  READ-LIST
	  READ-MOUSE-OR-KBD
	  READ-MOUSE-OR-KBD-NO-HANG
          READ-SPEECH
          REC
          REMOVE-BEEP-TYPE
          REMOVE-ESCAPE-KEY
          REMOVE-SYSTEM-KEY
          REMOVE-TERMINAL-KEY
          RENAME-SPEECH
          RESET-SOUND
          SAVE-SPEECH
          SCREEN-DEFAULT-FONT
          SCREEN-REDISPLAY
          SCROLL-INTERPRET-ENTRY
          SCROLL-ITEM-COMPONENT-ITEMS
          SCROLL-ITEM-LINE-SENSITIVITY
          SCROLL-ITEM-MOUSE-ITEMS
          SCROLL-ITEM-PLIST
          SCROLL-ITEM-SIZE
          SCROLL-MAINTAIN-LIST
          SCROLL-MAINTAIN-LIST-UNORDERED
          SCROLL-MAINTAIN-LIST-UPDATE-STATES
          SCROLL-PARSE-ITEM
          SCROLL-STRING-ITEM-WITH-EMBEDDED-NEWLINES
          SELECT-OR-CREATE-WINDOW-OF-FLAVOR
          SET-DEFAULT-FONT
          SET-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES
          SET-SCREEN-STANDARD-FONT
          SET-STANDARD-FONT
          SET-VISIBILITY-OF-ALL-SHEETS-BLINKERS
          SETUP-APPLICATION-MODE
          SETUP-KEYBOARD-KEYCLICK
          SETUP-KEYPAD-MODE
          SETUP-MOUSE-LEFT-HANDED
          SETUP-MOUSE-RIGHT-HANDED
          SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG
          SHEET-BASELINE
          SHEET-BIT-ARRAY
          SHEET-BLINKER-LIST
          SHEET-BOTTOM-MARGIN-SIZE
          SHEET-BOUNDS-WITHIN-SHEET-P
          SHEET-CALCULATE-OFFSETS
          SHEET-CAN-GET-LOCK
          SHEET-CHAR-ALUF
          SHEET-CHAR-WIDTH
          SHEET-CHARACTER-WIDTH
          SHEET-CLEAR
          SHEET-CLEAR-CHAR
          SHEET-CLEAR-EOF
          SHEET-CLEAR-EOL
          SHEET-CLEAR-LOCKS
          SHEET-CLEAR-STRING
          SHEET-COMPUTE-MOTION
          SHEET-CONTAINS-SHEET-POINT-P
          SHEET-CR-NOT-NEWLINE-FLAG
          SHEET-CRLF
          SHEET-CURRENT-FONT
          SHEET-CURSOR-X
          SHEET-CURSOR-Y
          SHEET-DEDUCE-MORE-VPOS
	  SHEET-DEEXPOSED-TYPEIN-NOTIFY
          SHEET-DEEXPOSED-TYPEOUT-ACTION
          SHEET-DELETE-CHAR
          SHEET-DELETE-LINE
          SHEET-END-OF-PAGE-FLAG
          SHEET-END-PAGE-FLAG
          SHEET-ERASE-ALUF
          SHEET-EXPOSED-INFERIORS
          SHEET-EXPOSED-P
          SHEET-FOLLOWING-BLINKER
          SHEET-FONT-MAP
          SHEET-FORCE-ACCESS
          SHEET-GET-SCREEN
          SHEET-HEIGHT
          SHEET-HOME
          SHEET-INCREMENT-BITPOS
          SHEET-INFERIORS
          SHEET-INSERT-CHAR
          SHEET-INSERT-LINE
          SHEET-INSIDE-BOTTOM
          SHEET-INSIDE-HEIGHT
          SHEET-INSIDE-LEFT
          SHEET-INSIDE-RIGHT
          SHEET-INSIDE-TOP
          SHEET-INSIDE-WIDTH
          SHEET-LEFT-MARGIN-SIZE
          SHEET-LINE-HEIGHT
          SHEET-LINE-NO
          SHEET-LINE-OUT
          SHEET-LOCK
          SHEET-ME-OR-MY-KID-P
	  SHEET-MENU-ABORT-ON-DEEXPOSE
	  SHEET-MENU-COMMAND-MENU
	  SHEET-MENU-DYNAMIC
	  SHEET-MENU-HIGHLIGHTING
	  SHEET-MENU-MULTICOLUMN
	  SHEET-MENU-POP-UP
	  SHEET-MENU-PERMANENT
          SHEET-MORE-FLAG
          SHEET-MORE-HANDLER
          SHEET-MORE-VPOS
          SHEET-NUMBER-OF-INSIDE-LINES
          SHEET-OUTPUT-HOLD-FLAG
          SHEET-OVERLAPS-EDGES-P
          SHEET-OVERLAPS-P
          SHEET-OVERLAPS-SHEET-P
          SHEET-READ-CURSORPOS
          SHEET-RIGHT-MARGIN-CHARACTER-FLAG
          SHEET-RIGHT-MARGIN-SIZE
          SHEET-SCREEN-ARRAY
          SHEET-SET-CURSORPOS
          SHEET-SET-FONT
          SHEET-SPACE
          SHEET-STRING-LENGTH
          SHEET-STRING-OUT
          SHEET-SUPERIOR
          SHEET-TAB-NCHARS
          SHEET-TAB-WIDTH
          SHEET-TOP-MARGIN-SIZE
          SHEET-TRUNCATE-LINE-OUT-FLAG
	  SHEET-TYO
          SHEET-WIDTH
          SHEET-WITHIN-P
          SHEET-WITHIN-SHEET-P
          SHEET-X-OFFSET
          SHEET-Y-OFFSET
	  SIMPLE-BEEP
          SPLINE
          SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW
          SYSTEM-MENU-CREATE-WINDOW
          TONE
          TONE-FREQUENCY
	  TRANSFER-COLOR-LUT-BUFFER
          TURN-OFF-ALL-SHEETS-BLINKERS
          TURN-OFF-SHEET-BLINKERS
          TURN-ON-SHEET-BLINKERS
          VOLUME
          WHITE-ON-BLACK
          WHO-LINE-CLOBBERED
          WHO-LINE-UPDATE
          WINDOW-CALL
          WINDOW-CREATE
          WINDOW-MOUSE-CALL
          WINDOW-OWNING-MOUSE
          WINDOW-UNDER-MOUSE
          WITH-BLINKER-READY
	  WITH-CLIPPING-RECTANGLE
          WITH-MOUSE-GRABBED
          WITH-MOUSE-USURPED
          WITH-SELECTION-SUBSTITUTE
          WITH-SHEET-DEEXPOSED
          WITH-SOUND-ENABLED
          WITHOUT-SCREEN-MANAGEMENT
	  WRITE-COLOR-LUT-BUFFER
	  WRITE-COLOR-MAP
          WRITE-KEYBOARD
          ;; Resources
          POP-UP-FINGER-WINDOW
          POP-UP-TEXT-WINDOW-WITHOUT-MORE
          SPLIT-SCREEN-LAYOUT-WINDOW
          SYSTEM-MENU
          ;; Variables and constants
	  *BEEP-TYPES*
	  *BEEPING-FUNCTIONS*
          *BIDIRECTIONAL-MORE-STANDARD-MESSAGE* 
          *CHOOSE-VARIABLE-VALUES-NAME-STANDARD-FONT*
          *CHOOSE-VARIABLE-VALUES-SELECTED-STANDARD-FONT*
          *CHOOSE-VARIABLE-VALUES-STRING-STANDARD-FONT*
          *CHOOSE-VARIABLE-VALUES-UNSELECTED-CHOICE-STANDARD-FONT*
          *CHOOSE-VARIABLE-VALUES-VALUE-STANDARD-FONT*
	  *CONTROL-REGISTER*
	  *CURRENT-BACKGROUND*  ;; Added for dual monitor/multiple screen support. 02/11/88 KJF
	  *CURRENT-FOREGROUND*  ;; Added for dual monitor/multiple screen support. 02/11/88 KJF
          *DEFAULT-FONT*
	  *DEFAULT-BACKGROUND*
	  *DEFAULT-BLINKER-OFFSET*
          *DEFAULT-BORDER-COLOR*
	  *DEFAULT-COLOR-PLANE-MASK*   ;; Added for dual monitor/multiple screen support. 02/11/88 KJF
	  *DEFAULT-COLOR-TABLE*
	  *DEFAULT-DOCUMENTATION-FOREGROUND*
	  *DEFAULT-DOCUMENTATION-BACKGROUND*
	  *DEFAULT-FOREGROUND*
	  *DEFAULT-LABEL-FOREGROUND*
	  *DEFAULT-LABEL-BACKGROUND*
	  *DEFAULT-MENU-FOREGROUND*
	  *DEFAULT-MENU-BACKGROUND*
	  *DEFAULT-MENU-LABEL-FOREGROUND*
	  *DEFAULT-MENU-LABEL-BACKGROUND*
	  *DEFAULT-MONOCHROME-PLANE*  ;; Added for dual monitor/multiple screen support. 02/11/88 KJF
	  *DEFAULT-MONOCHROME-PLANE-MASK*  ;; Added for dual monitor/multiple screen support. 02/11/88 KJF
          *DEFAULT-SCROLL-BAR-COLOR*
	  *DEFAULT-STATUS-BACKGROUND*
	  *DEFAULT-STATUS-FOREGROUND*
          *DESELECTED-PROCESS-PRIORITY*
          *ENABLE-TYPEOUT-WINDOW-BORDERS*
          *FRAME-LABEL-STANDARD-FONT*
          *HOLLOW-M-CHOICE-BOX-P*
          *MENU-ITEM-STANDARD-FONT*
          *MORE-BACKWARD-STANDARD-MESSAGE*
          *MORE-FORWARD-STANDARD-MESSAGE*
          *MORE-PROCESSING-STANDARD-BEEP*
          *MORE-PROCESSING-STANDARD-FONT*
          *MOUSE-DOCUMENTATION-LINE-BUTTONS-STANDARD-FONT*
          *MOUSE-DOCUMENTATION-LINE-STANDARD-FONT*
          *MOUSE-INCREMENTING-KEYSTATES*
          *NO-MORE-BACKWARD-STANDARD-MESSAGE*
          *NO-MORE-FORWARD-STANDARD-MESSAGE*
          *REMOVE-TYPEOUT-STANDARD-BEEP*
          *REMOVE-TYPEOUT-STANDARD-FONT*
          *REMOVE-TYPEOUT-STANDARD-MESSAGE*
	  *SCROLL-BAR-CHAR-INDEX*
	  *SCROLL-BAR-CHAR-X-OFFSET*
	  *SCROLL-BAR-CHAR-Y-OFFSET*
	  *SCROLL-BAR-DEFAULT-CLICKS*
	  *SCROLL-BAR-DEFAULT-DELAY-TIME*
	  *SCROLL-BAR-DEFAULT-ICON-HEIGHT*
	  *SCROLL-BAR-DEFAULT-ICON-WIDTH*
	  *SCROLL-BAR-DEFAULT-LINES*
	  *SCROLL-BAR-DEFAULT-MODE*
	  *SCROLL-BAR-DEFAULT-SIDE*
	  *SCROLL-BAR-SHADE*
	  *SCROLL-BAR-WHO-LINE-DOCUMENTATION*
	  *SELECTED-PROCESS-PRIORITY*
          *SYSTEM-KEYS*
	  *SYSTEM-MENU-DEBUG-TOOLS-COLUMN*
	  *SYSTEM-MENU-EDIT-WINDOWS-COLUMN*
	  *SYSTEM-MENU-PROGRAMS-COLUMN*
	  *SYSTEM-MENU-USER-AIDS-COLUMN*
          *TERMINAL-KEYS*
	  *UNEXPECTED-SELECT-DELAY*
          *UNIDIRECTIONAL-MORE-STANDARD-MESSAGE*
          *WINDOW-PANE-LABEL-STANDARD-FONT*
          100%-black
          100%-white
          12%-gray
          25%-gray
          33%-gray
          50%-gray
          66%-gray
          75%-gray
          88%-gray
          12%-gray-color			;next several lines are new for color
          25%-gray-color
          33%-gray-color
          50%-gray-color
          66%-gray-color
          75%-gray-color
          88%-gray-color			;previous lines were new for color
          ALL-THE-SCREENS
          ALU-AND
          ALU-ANDCA
          ALU-IOR
          ALU-SETA
          ALU-SETZ
          ALU-XOR
	  ALU-TRANSP				;next several lines are new for color
	  ALU-MAX
	  ALU-MIN
	  ALU-AVG
	  ALU-ADD
	  ALU-SUB
	  ALU-ADDS
	  ALU-SUBC
	  ALU-BACK
	  BLACK
	  WHITE
	  RED
	  GREEN
	  BLUE
	  YELLOW
	  MAGENTA
	  CYAN
	  ORANGE
	  BLUE-GREEN
	  PINK
	  PURPLE
	  RED-PURPLE
	  LIGHT-BROWN
	  DARK-GREEN
	  DARK-BLUE
	  DARK-BROWN				;this line and earlier are new for color
          BACKGROUND-COLOR
          BASELINE
          BASELINE-ADJ
          BIT-ARRAY
	  BITMAP-MOUSE-PATHNAME
          BLINKER-LIST
          BORDERS
	  BOTTOM-MARGIN-SIZE
	  CHAR-ALUF
          CHAR-WIDTH
          CHOOSE-VARIABLE-VALUES-DEFAULT-COMMAND-CHARACTERS
          CHOOSE-VARIABLE-VALUES-KEYWORD
          CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION
          CLOCK-TYPE
          COMMAND-CHARACTERS
          CONSTRAINTS
          CONTINUOUS-REPEAT-DELAY 
          CURRENT-FONT
          CURRENT-ITEM
          CURSOR-X
          CURSOR-Y
          DEEXPOSED-TYPEOUT-ACTION
          DEFAULT-BORDER-SIZE
          DEFAULT-SCREEN
          DEFAULT-WINDOW-TYPES-ITEM-LIST
	  DEFERRED-NOTIFICATIONS
          DEFINE-MOUSE-CHAR-MAPPING
          DESELECTED-VISIBILITY
          DISPLAY-FONT
	  ERASE-ALUF
          EXPAND-WINDOW
          EXPLODING-MOMENTARY-WINDOWS
          EXPOSED-INFERIORS
          EXPOSED-P
          FLAGS
          FLASH-DURATION
          FOLLOW-P
          FONT-MAP
          FOREGROUND-COLOR
          GET-WINDOW-EDGE-ALIST
          HALF-PERIOD
          HEIGHT
          HYSTERESIS
          INFERIORS
          INITIAL-LISP-LISTENER
          INITIAL-REPEAT-DELAY 
          INITIALIZE-MULTIPLE-MOVE-BLINKER
          INVISIBLE-TO-MOUSE-P
          ITEM-BLINKER
          ITEM-TYPE-ALIST
          ITEMS
          KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS
          KBD-INTERCEPTED-CHARACTERS
          KBD-IO-BUFFER
          KBD-LAST-ACTIVITY-TIME
          KBD-PROCESS
          KBD-STANDARD-ASYNCHRONOUS-CHARACTERS
          KBD-STANDARD-INTERCEPTED-CHARACTERS
          KBD-SYS-1
          KBD-TYI-HOOK
          KEYPAD-IN-APPLICATION-MODE-P
          LAST-WHO-LINE-PROCESS
	  LEFT-MARGIN-SIZE
          LINE-HEIGHT
          LOCATIONS-PER-LINE
          LOCK
          LOCK-COUNT
          MAIN-SCREEN
          MARGIN-CHOICES
          MENU-FILL-BREAKAGE
          MENU-GOLDEN-RATIO
          MENU-INTERCOLUMN-SPACING
          MENU-INTERWORD-SPACING
          MORE-PROCESSING-GLOBAL-ENABLE
          MORE-VPOS
          MOUSE-BLINKER
          MOUSE-BOUNCE-TIME
          MOUSE-DOUBLE-CLICK-TIME
	  MOUSE-FAST-MOTION-BITMAP-TIME
	  MOUSE-FAST-MOTION-CROSS-SIZE
	  MOUSE-FAST-MOTION-CROSS-TIME
	  MOUSE-FAST-MOTION-SPEED
          MOUSE-FAST-TRACK-BITMAP-MOUSE-P
	  MOUSE-GLYPH-THIN-UP-ARROW
	  MOUSE-GLYPH-THIN-RIGHT-ARROW
	  MOUSE-GLYPH-THIN-DOWN-ARROW
	  MOUSE-GLYPH-THIN-LEFT-ARROW
	  MOUSE-GLYPH-THIN-UP-DOWN-ARROW
	  MOUSE-GLYPH-THIN-LEFT-RIGHT-ARROW
	  MOUSE-GLYPH-NORTH-WEST-ARROW
	  MOUSE-GLYPH-THIN-CROSS
	  MOUSE-GLYPH-THICK-UP-ARROW
	  MOUSE-GLYPH-THICK-RIGHT-ARROW
	  MOUSE-GLYPH-THICK-DOWN-ARROW
	  MOUSE-GLYPH-THICK-LEFT-ARROW
	  MOUSE-GLYPH-THICK-UP-DOWN-ARROW
	  MOUSE-GLYPH-PARAGRAPH
	  MOUSE-GLYPH-UPPER-LEFT-CORNER
	  MOUSE-GLYPH-LOWER-RIGHT-CORNER
	  MOUSE-GLYPH-HOURGLASS
	  MOUSE-GLYPH-CIRCLE-PLUS
	  MOUSE-GLYPH-PAINT-BRUSH
	  MOUSE-GLYPH-SCISSOR
	  MOUSE-GLYPH-TRIDENT
	  MOUSE-GLYPH-NORTH-EAST-ARROW
	  MOUSE-GLYPH-CIRCLE-X
	  MOUSE-GLYPH-LARGE-RIGHT-TRIANGLE-POINTER
	  MOUSE-GLYPH-MEDIUM-RIGHT-TRIANGLE-POINTER
	  MOUSE-GLYPH-SMALL-RIGHT-TRIANGLE-POINTER
	  MOUSE-GLYPH-BLOCK-UP-ARROW
	  MOUSE-GLYPH-SMALL-DIAMOND
	  MOUSE-GLYPH-BLOCK-DOWN-ARROW
	  MOUSE-GLYPH-HOLLOW-BOX-POINTER
	  MOUSE-GLYPH-SOLID-BOX-POINTER
	  MOUSE-GLYPH-HOLLOW-CIRCLE-POINTER
	  MOUSE-GLYPH-SOLID-CIRCLE-POINTER
	  MOUSE-GLYPH-THICK-HOLLOW-CROSS
	  MOUSE-GLYPH-BLOCK-LETTER-T
	  MOUSE-GLYPH-HAND-POINTING-LEFT
	  MOUSE-GLYPH-DOUBLE-UP-ARROW
	  MOUSE-GLYPH-HOLLOW-ARC-POINTER
	  MOUSE-GLYPH-SOLID-ARC-POINTER
	  MOUSE-GLYPH-SPLINE-POINTER
	  MOUSE-GLYPH-MEDIUM-DIAMOND
	  MOUSE-GLYPH-HOLLOW-TRIANGLE-POINTER
	  MOUSE-GLYPH-SOLID-TRIANGLE-POINTER
	  MOUSE-GLYPH-CURTAIN
	  MOUSE-GLYPH-SCALE
	  MOUSE-GLYPH-6-3-ARC-POINTER
	  MOUSE-GLYPH-9-6-ARC-POINTER
	  MOUSE-GLYPH-3-12-ARC-POINTER
	  MOUSE-GLYPH-12-9-ARC-POINTER
	  MOUSE-GLYPH-RULER
	  MOUSE-GLYPH-POLYLINE
	  MOUSE-GLYPH-DOUBLE-UP-ARROW-LETTERED-D
	  MOUSE-GLYPH-THICK-UP-ARROW-LETTERED-D
	  MOUSE-GLYPH-THICK-LINE-POINTER
	  MOUSE-GLYPH-QUESTION-MARK
	  MOUSE-GLYPH-THIN-HOLLOW-CROSS
	  MOUSE-GLYPH-EYE-GLASSES
	  MOUSE-GLYPH-THIN-HOLLOW-PLUS
	  MOUSE-GLYPH-RECTANGLE-DOTS
	  MOUSE-GLYPH-WEST-RAT-ON-BOTTOM
	  MOUSE-GLYPH-WEST-RAT-ON-TOP
	  MOUSE-GLYPH-NORTH-RAT-ON-LEFT
	  MOUSE-GLYPH-NORTH-RAT-ON-RIGHT
	  MOUSE-GLYPH-EAST-RAT-ON-BOTTOM
	  MOUSE-GLYPH-EAST-RAT-ON-TOP
	  MOUSE-GLYPH-SOUTH-RAT-ON-LEFT
	  MOUSE-GLYPH-SOUTH-RAT-ON-RIGHT
	  MOUSE-GLYPH-WEST-CURLY-RAT-ON-BOTTOM
	  MOUSE-GLYPH-WEST-CURLY-RAT-ON-TOP
	  MOUSE-GLYPH-NORTH-CURLY-RAT-ON-LEFT
	  MOUSE-GLYPH-NORTH-CURLY-RAT-ON-RIGHT
	  MOUSE-GLYPH-EAST-CURLY-RAT-ON-BOTTOM
	  MOUSE-GLYPH-EAST-CURLY-RAT-ON-TOP
	  MOUSE-GLYPH-SOUTH-CURLY-RAT-ON-LEFT
	  MOUSE-GLYPH-SOUTH-CURLY-RAT-ON-RIGHT
	  MOUSE-GLYPH-WEST-MOUSE-ON-BOTTOM
	  MOUSE-GLYPH-WEST-MOUSE-ON-TOP
	  MOUSE-GLYPH-NORTH-MOUSE-ON-LEFT
	  MOUSE-GLYPH-NORTH-MOUSE-ON-RIGHT
	  MOUSE-GLYPH-EAST-MOUSE-ON-BOTTOM
	  MOUSE-GLYPH-EAST-MOUSE-ON-TOP
	  MOUSE-GLYPH-SOUTH-MOUSE-ON-LEFT
	  MOUSE-GLYPH-SOUTH-MOUSE-ON-RIGHT
	  MOUSE-GLYPH-SMALL-DOT
	  MOUSE-GLYPH-THICK-CROSS
	  MOUSE-GLYPH-SMALL-SOLID-CIRCLE
	  MOUSE-GLYPH-MEDIUM-SOLID-CIRCLE
	  MOUSE-GLYPH-HOLLOW-CIRCLE
	  MOUSE-GLYPH-HOLLOW-CIRCLE-MINUS
	  MOUSE-GLYPH-HOLLOW-CIRCLE-PLUS
	  MOUSE-GLYPH-SHORT-THIN-DOWN-BORDER-ARROW
	  MOUSE-GLYPH-SHORT-THIN-DOWN-ARROW
	  MOUSE-GLYPH-SHORT-THIN-UP-BORDER-ARROW
	  MOUSE-GLYPH-SHORT-THIN-UP-ARROW
	  MOUSE-GLYPH-SMALL-UP-TRIANGLE
	  MOUSE-GLYPH-SMALL-DOWN-TRIANGLE
          MOUSE-HANDEDNESS
          MOUSE-LAST-BUTTONS
          MOUSE-LAST-BUTTONS-TIME
          MOUSE-LAST-X
          MOUSE-LAST-Y
          MOUSE-PROCESS
          MOUSE-SHEET
          MOUSE-SPEED
          MOUSE-WINDOW
          SYS:MOUSE-X
          SYS:MOUSE-Y
          MULTIPLE-MOVE-BLINKER
          NAME
          NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES
          OLD-SCREEN-ARRAY
          ON-VOLUME
          PENDING-NOTIFICATIONS
          PHASE
          PREVIOUSLY-SELECTED-WINDOWS
          PRIORITY
          PROCESS-IS-IN-ERROR
          READ-BIT-ARRAY-FILE
          REMAP-MOUSE
          RESTORE-MOUSE-MAP
          RESTORED-BITS-P
	  RIGHT-MARGIN-SIZE
          ROTATE-180
          ROTATE-270
          ROTATE-90
	  RUBOUT-HANDLER
	  RUBOUT-HANDLER-ALPHA-CHAR-P
          SCREEN-ARRAY
          SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS
          SCREEN-MANAGER-SCREEN-IMAGE
          SCROLL-BAR-MAX-EXIT-SPEED
          SCROLL-BAR-MAX-SPEED
          SCROLL-BAR-MODE
          SCROLL-BAR-RELUCTANCE
          SCROLL-ITEM-LEADER-OFFSET
          SELECTED-IO-BUFFER
          SELECTED-WINDOW
          SELECTION-SUBSTITUTE
          SERVER-DESC-HOST-NAME
          SHEET-AREA
          SHEET-BASELINE-ADJ
          SHOW-BIT-ARRAY
          SUPERIOR
          TEMPORARY-BIT-ARRAY
          TEMPORARY-WINDOWS-LOCKED
          TIME-STAMP
          TIME-UNTIL-BLINK
          TOP-ITEM
	  TOP-MARGIN-SIZE
	  TYPEOUT-ITEM-BOTTOM
	  TYPEOUT-ITEM-ITEM
	  TYPEOUT-ITEM-LEFT
	  TYPEOUT-ITEM-RIGHT
	  TYPEOUT-ITEM-TOP
	  TYPEOUT-ITEM-TYPE
	  UNREAD-ANY
	  USE-KBD-BUTTONS
          VALUE-TAB
          VISIBILITY
          WHO-LINE-FILE-STATE-SHEET
          WHO-LINE-MOUSE-GRABBED-DOCUMENTATION
          WHO-LINE-PROCESS
          WHO-LINE-SCREEN
          WHO-LINE-RUN-STATE
          WIDTH
          WINDOW-RESOURCE-NAMES
          WRITE-BIT-ARRAY-FILE
          X-OFFSET
          Y-OFFSET
          X-POS
          Y-POS)
	'TV)




(UNLESS (FIND-PACKAGE 'W)
  (MAKE-PACKAGE "W" :SIZE 1000. :USE '("LISP" "TV" "TICL" "SYSTEM")))


;;; The following symbols are those which are intended to be documented.
;;; They are suitable for exporting from the W package.

(DEFPARAMETER ALL-TV-DOCUMENTED-SYMBOLS
	'(*BIDIRECTIONAL-MORE-STANDARD-MESSAGE* 
          *DEFAULT-BACKGROUND*
	  *DEFAULT-FOREGROUND*
          *DEFAULT-BORDER-COLOR*		;new for color
          *DEFAULT-SCROLL-BAR-COLOR*
	  *DESELECTED-PROCESS-PRIORITY*
	  *ENABLE-TYPEOUT-WINDOW-BORDERS*
          *HOLLOW-M-CHOICE-BOX-P*
	  *MOUSE-INCREMENTING-KEYSTATES*
	  *SCROLL-BAR-CHAR-INDEX*
	  *SCROLL-BAR-CHAR-X-OFFSET*
	  *SCROLL-BAR-CHAR-Y-OFFSET*
	  *SCROLL-BAR-DEFAULT-CLICKS*
	  *SCROLL-BAR-DEFAULT-DELAY-TIME*
	  *SCROLL-BAR-DEFAULT-ICON-HEIGHT*
	  *SCROLL-BAR-DEFAULT-ICON-WIDTH*
	  *SCROLL-BAR-DEFAULT-LINES*
	  *SCROLL-BAR-DEFAULT-MODE*
	  *SCROLL-BAR-DEFAULT-SIDE*
	  *SCROLL-BAR-SHADE*
	  *SCROLL-BAR-WHO-LINE-DOCUMENTATION*
	  *SELECTED-PROCESS-PRIORITY*
          *SYSTEM-KEYS*
          *TERMINAL-KEYS*
	  *UNEXPECTED-SELECT-DELAY*
          100%-black
          100%-white
          12%-gray
          12-HOUR-CLOCK-SETUP
          24-HOUR-CLOCK-SETUP
          25%-gray
          33%-gray
          50%-gray
          66%-gray
          75%-gray
          88%-gray
          12%-gray-color			;next several lines are new for color
          25%-gray-color
          33%-gray-color
          50%-gray-color
          66%-gray-color
          75%-gray-color
          88%-gray-color
          ALU-TRANSP
	  ALU-MAX
	  ALU-MIN
	  ALU-AVG
	  ALU-ADD
	  ALU-SUB
	  ALU-ADDS
	  ALU-SUBC
	  ALU-BACK
	  BLACK
	  WHITE
	  RED
	  GREEN
	  BLUE
	  YELLOW
	  MAGENTA
	  CYAN
	  ORANGE
	  BLUE-GREEN
	  PINK
	  PURPLE
	  RED-PURPLE
	  LIGHT-BROWN
	  DARK-GREEN
	  DARK-BLUE
	  DARK-BROWN				;previous lines new for color
          ADD-SYSTEM-KEY
          ADD-TERMINAL-KEY
          ADD-TO-SYSTEM-MENU-COLUMN
          ADD-TYPEOUT-ITEM-TYPE
	  ADD-WINDOW-TYPE
          ADJUST-BY-INTERVAL
          ALIAS-FOR-INFERIORS-MIXIN
          ALL-THE-SCREENS
          ALU-AND
          ALU-ANDCA
          ALU-IOR
          ALU-SETA
          ALU-SETZ
          ALU-XOR
          AUTOEXPOSING-MORE-MIXIN
          AWAIT-USER-TYPEAHEAD
	  BACKGROUND-COLOR
          BASIC-CHOOSE-VARIABLE-VALUES
          BASIC-FRAME
          BASIC-MOUSE-SENSITIVE-ITEMS
          BASIC-MULTIPLE-CHOICE
          BASIC-TYPEOUT-WINDOW
          BASELINE
          BASELINE-ADJ
          BIT-ARRAY
          BITBLT-BLINKER
	  BITMAP-MOUSE-PATHNAME
          BLACK-ON-WHITE
	  BLINK
          BLINKER
          BLINKER-DESELECTED-VISIBILITY
          BLINKER-LIST
          BLINKER-SET-VISIBILITY
          BLINKER-VISIBILITY
          BORDERED-CONSTRAINT-FRAME
          BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER
	  BORDERS
          BORDERS-MIXIN
          BOTTOM-BOX-LABEL-MIXIN
	  BOTTOM-MARGIN-SIZE
          BOX-BLINKER
          BOX-LABEL-MIXIN
          CAREFUL-NOTIFY
          CENTERED-LABEL-MIXIN
	  CHAR-ALUF
          CHAR-WIDTH
          CHARACTER-BLINKER
          CHOOSE-PROCESS-IN-ERROR
          CHOOSE-VARIABLE-VALUES
          CHOOSE-VARIABLE-VALUES-DEFAULT-COMMAND-CHARACTERS
          CHOOSE-VARIABLE-VALUES-FIND-CONTINUATION-ITEMS
          CHOOSE-VARIABLE-VALUES-KEYWORD
          CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION
          CHOOSE-VARIABLE-VALUES-PANE
          CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE
          CHOOSE-VARIABLE-VALUES-WINDOW
          CLOSE-ALL-SERVERS
          COMMAND-CHARACTERS
          COMPLEMENT-BOW-MODE
          CONSTRAINT-FRAME
          CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER
          CONSTRAINTS
          CONTINUOUS-REPEAT-DELAY
          CURRENT-FONT
          CURRENT-ITEM
          CURRENT-ITEM-MIXIN
          CURSOR-X
          CURSOR-Y
          DEEXPOSED-TYPEOUT-ACTION
          DEF-BEEP-TYPE
          DEFAULT-BEEP
          DEFAULT-BORDER-SIZE
          DEFAULT-SCREEN
          DEFAULT-WINDOW-TYPES-ITEM-LIST
	  DEFERRED-NOTIFICATIONS
          DEFINE-MOUSE-CHAR-MAPPING
          DEFWINDOW-RESOURCE
          DELAY-NOTIFICATION-MIXIN
          DELAYED-REDISPLAY-LABEL-MIXIN
          DELAYING-SCREEN-MANAGEMENT
          DELETE-FROM-SYSTEM-MENU-COLUMN
          DESCRIBE-SERVERS
          DESELECT-AND-MAYBE-BURY-WINDOW
          DESELECTED-VISIBILITY
          DISPLAY-FONT
          DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW
          DO-SOUND
          DOORBELL
          DOPLIST
          DRAW-CHAR
          DRAW-CHAR-DOWN
          DRAW-CHAR-UP
          DRAW-RECTANGLE-INSIDE-CLIPPED
          DRAW-RECTANGULAR-BORDER
          DYNAMIC-ITEM-LIST-MIXIN
          EDIT-SCREEN
	  ERASE-ALUF
          ESSENTIAL-ACTIVATE
          ESSENTIAL-EXPOSE
          ESSENTIAL-MOUSE
          ESSENTIAL-SCROLL-MOUSE-MIXIN
          ESSENTIAL-SET-EDGES
          ESSENTIAL-WINDOW
          ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN
          ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN-TYPEOUT-WINDOW
          EXPAND-WINDOW
          EXPOSE-WINDOW-NEAR
          EXPOSED-INFERIORS
          EXPOSED-P
          FIND-PROCESS-IN-ERROR
          FIND-WINDOW-OF-FLAVOR
          FLAGS
          FLASHY-MARGIN-SCROLLING-MIXIN
          FLASHY-SCROLLING-MIXIN
          FLUSH-FULL-SCREEN-BORDERS
          FOLLOW-P
          FONT-BASELINE
          FONT-BLINKER-HEIGHT
          FONT-BLINKER-WIDTH
          FONT-CHAR-HEIGHT
	  FONT-CHAR-MIN-RASTER-WIDTH
          FONT-CHAR-WIDTH
          FONT-CHAR-WIDTH-TABLE
          FONT-CHARS-EXIST-TABLE
          FONT-EVALUATE
          FONT-FILL-POINTER
          FONT-INDEXING-TABLE
          FONT-LEFT-KERN-TABLE
          FONT-MAP
          FONT-NAME
          FONT-RASTER-HEIGHT
          FONT-RASTER-WIDTH
          FONT-RASTERS-PER-WORD
          FONT-WORDS-PER-CHAR
	  FOREGROUND-COLOR
          FRAME-FORWARDING-MIXIN
          FULL-SCREEN-HACK-MIXIN
          FUNCTION-TEXT-SCROLL-WINDOW
          GET-VISIBILITY-OF-ALL-SHEETS-BLINKERS
          GET-WINDOW-EDGE-ALIST
          GRAY-DEEXPOSED-RIGHT-MIXIN
          GRAY-DEEXPOSED-WRONG-MIXIN
          HALF-PERIOD 
          HEIGHT
	  HIGHLIGHTED-ITEMS
          HOLLOW-RECTANGULAR-BLINKER
          HYSTERESIS
          HYSTERETIC-WINDOW-MIXIN
          IBEAM-BLINKER
          IDLE-LISP-LISTENER
          INFERIORS
          INFERIORS-NOT-IN-SELECT-MENU-MIXIN
          INITIAL-LISP-LISTENER
          INITIAL-REPEAT-DELAY 
          INITIALIZE-MULTIPLE-MOVE-BLINKER
          INITIALLY-INVISIBLE-MIXIN
          INTRINSIC-NO-MORE-MIXIN
          INVISIBLE-TO-MOUSE-P
          IO-BUFFER
          IO-BUFFER-CLEAR
          IO-BUFFER-EMPTY-P
          IO-BUFFER-FULL-P
          IO-BUFFER-GET
          IO-BUFFER-INPUT-FUNCTION
          IO-BUFFER-INPUT-POINTER
          IO-BUFFER-LAST-INPUT-PROCESS
          IO-BUFFER-LAST-OUTPUT-PROCESS
          IO-BUFFER-OUTPUT-FUNCTION
          IO-BUFFER-OUTPUT-POINTER
          IO-BUFFER-PLIST
          IO-BUFFER-PUSH
          IO-BUFFER-PUT
          IO-BUFFER-RECORD
          IO-BUFFER-RECORD-POINTER
          IO-BUFFER-SIZE
          IO-BUFFER-STATE
          IO-BUFFER-UNGET
          ITEM-BLINKER
          ITEMS
          KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER
          KBD-CHAR-TYPED-P
          KBD-DEFAULT-OUTPUT-FUNCTION
          KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS
          KBD-INTERCEPT-ABORT
          KBD-INTERCEPT-ABORT-ALL
          KBD-INTERCEPT-BREAK
          KBD-INTERCEPT-ERROR-BREAK
          KBD-INTERCEPTED-CHARACTERS
          KBD-IO-BUFFER-GET
          KBD-MOUSE-BUTTONS-MIXIN
          KBD-SNARF-INPUT
          KBD-STANDARD-ASYNCHRONOUS-CHARACTERS
          KBD-STANDARD-INTERCEPTED-CHARACTERS
          KBD-SYS-1
          KBD-TYI-HOOK
          KBD-WAIT-FOR-INPUT-OR-DEEXPOSURE
          KBD-WAIT-FOR-INPUT-WITH-TIMEOUT
          KEY-STATE
          KEYPAD-IN-APPLICATION-MODE-P
          LABEL-MIXIN
          LABEL-NEEDS-UPDATING
          LAST-WHO-LINE-PROCESS
	  LEFT-MARGIN-SIZE
          LINE-AREA-MOUSE-SENSITIVE-TEXT-SCROLL-MIXIN
          LINE-AREA-TEXT-SCROLL-MIXIN
          LINE-HEIGHT
          LINE-TRUNCATING-MIXIN
          LISP-INTERACTOR
          LISTENER-MIXIN
          LOCATIONS-PER-LINE
          LOCK
          LOCK-COUNT
          LOCK-SHEET
          MAGNIFYING-BLINKER
          MAIN-SCREEN
          MAKE-BLINKER
          MAKE-DEFAULT-IO-BUFFER
          MAKE-IO-BUFFER
          MAKE-SHEET-BIT-ARRAY
          MAP-OVER-EXPOSED-SHEET
          MAP-OVER-EXPOSED-SHEETS
          MAP-OVER-SHEET
          MAP-OVER-SHEETS
          MARGIN-CHOICE-MIXIN
	  MARGIN-CHOICES
          MARGIN-REGION-AREA
          MARGIN-REGION-BOTTOM
          MARGIN-REGION-FUNCTION
          MARGIN-REGION-LEFT
          MARGIN-REGION-MARGIN
          MARGIN-REGION-MIXIN
          MARGIN-REGION-RIGHT
          MARGIN-REGION-SIZE
          MARGIN-REGION-TOP
          MARGIN-SCROLL-MIXIN
	  MENU-GOLDEN-RATIO
          MENU-ITEM-STRING
          MERGE-SHIFT-KEYS
          MINIMUM-WINDOW
          MORE-PROCESSING-GLOBAL-ENABLE
          MORE-VPOS
          MOUSE-BLINKER
          MOUSE-BLINKER-MIXIN
          MOUSE-BOUNCE-TIME
          MOUSE-BOX-BLINKER
          MOUSE-BOX-STAY-INSIDE-BLINKER
          MOUSE-BUTTON-ENCODE
          MOUSE-BUTTONS
          MOUSE-CALL-SYSTEM-MENU
          MOUSE-CHARACTER-BLINKER
	  MOUSE-CHARACTER-BUTTON-ENCODE
          MOUSE-CONFIRM
          MOUSE-DEFAULT-HANDLER
          MOUSE-DEFINE-BLINKER-TYPE
          MOUSE-DISCARD-CLICKAHEAD
          MOUSE-DOUBLE-CLICK-TIME
	  MOUSE-FAST-MOTION-BITMAP-TIME
	  MOUSE-FAST-MOTION-CROSS-SIZE
	  MOUSE-FAST-MOTION-CROSS-TIME
	  MOUSE-FAST-MOTION-SPEED
          MOUSE-FAST-TRACK-BITMAP-MOUSE-P
          MOUSE-GET-BLINKER
	  MOUSE-GLYPH-THIN-UP-ARROW
	  MOUSE-GLYPH-THIN-RIGHT-ARROW
	  MOUSE-GLYPH-THIN-DOWN-ARROW
	  MOUSE-GLYPH-THIN-LEFT-ARROW
	  MOUSE-GLYPH-THIN-UP-DOWN-ARROW
	  MOUSE-GLYPH-THIN-LEFT-RIGHT-ARROW
	  MOUSE-GLYPH-NORTH-WEST-ARROW
	  MOUSE-GLYPH-THIN-CROSS
	  MOUSE-GLYPH-THICK-UP-ARROW
	  MOUSE-GLYPH-THICK-RIGHT-ARROW
	  MOUSE-GLYPH-THICK-DOWN-ARROW
	  MOUSE-GLYPH-THICK-LEFT-ARROW
	  MOUSE-GLYPH-THICK-UP-DOWN-ARROW
	  MOUSE-GLYPH-PARAGRAPH
	  MOUSE-GLYPH-UPPER-LEFT-CORNER
	  MOUSE-GLYPH-LOWER-RIGHT-CORNER
	  MOUSE-GLYPH-HOURGLASS
	  MOUSE-GLYPH-CIRCLE-PLUS
	  MOUSE-GLYPH-PAINT-BRUSH
	  MOUSE-GLYPH-SCISSOR
	  MOUSE-GLYPH-TRIDENT
	  MOUSE-GLYPH-NORTH-EAST-ARROW
	  MOUSE-GLYPH-CIRCLE-X
	  MOUSE-GLYPH-LARGE-RIGHT-TRIANGLE-POINTER
	  MOUSE-GLYPH-MEDIUM-RIGHT-TRIANGLE-POINTER
	  MOUSE-GLYPH-SMALL-RIGHT-TRIANGLE-POINTER
	  MOUSE-GLYPH-BLOCK-UP-ARROW
	  MOUSE-GLYPH-SMALL-DIAMOND
	  MOUSE-GLYPH-BLOCK-DOWN-ARROW
	  MOUSE-GLYPH-HOLLOW-BOX-POINTER
	  MOUSE-GLYPH-SOLID-BOX-POINTER
	  MOUSE-GLYPH-HOLLOW-CIRCLE-POINTER
	  MOUSE-GLYPH-SOLID-CIRCLE-POINTER
	  MOUSE-GLYPH-THICK-HOLLOW-CROSS
	  MOUSE-GLYPH-BLOCK-LETTER-T
	  MOUSE-GLYPH-HAND-POINTING-LEFT
	  MOUSE-GLYPH-DOUBLE-UP-ARROW
	  MOUSE-GLYPH-HOLLOW-ARC-POINTER
	  MOUSE-GLYPH-SOLID-ARC-POINTER
	  MOUSE-GLYPH-SPLINE-POINTER
	  MOUSE-GLYPH-MEDIUM-DIAMOND
	  MOUSE-GLYPH-HOLLOW-TRIANGLE-POINTER
	  MOUSE-GLYPH-SOLID-TRIANGLE-POINTER
	  MOUSE-GLYPH-CURTAIN
	  MOUSE-GLYPH-SCALE
	  MOUSE-GLYPH-6-3-ARC-POINTER
	  MOUSE-GLYPH-9-6-ARC-POINTER
	  MOUSE-GLYPH-3-12-ARC-POINTER
	  MOUSE-GLYPH-12-9-ARC-POINTER
	  MOUSE-GLYPH-RULER
	  MOUSE-GLYPH-POLYLINE
	  MOUSE-GLYPH-DOUBLE-UP-ARROW-LETTERED-D
	  MOUSE-GLYPH-THICK-UP-ARROW-LETTERED-D
	  MOUSE-GLYPH-THICK-LINE-POINTER
	  MOUSE-GLYPH-QUESTION-MARK
	  MOUSE-GLYPH-THIN-HOLLOW-CROSS
	  MOUSE-GLYPH-EYE-GLASSES
	  MOUSE-GLYPH-THIN-HOLLOW-PLUS
	  MOUSE-GLYPH-RECTANGLE-DOTS
	  MOUSE-GLYPH-WEST-RAT-ON-BOTTOM
	  MOUSE-GLYPH-WEST-RAT-ON-TOP
	  MOUSE-GLYPH-NORTH-RAT-ON-LEFT
	  MOUSE-GLYPH-NORTH-RAT-ON-RIGHT
	  MOUSE-GLYPH-EAST-RAT-ON-BOTTOM
	  MOUSE-GLYPH-EAST-RAT-ON-TOP
	  MOUSE-GLYPH-SOUTH-RAT-ON-LEFT
	  MOUSE-GLYPH-SOUTH-RAT-ON-RIGHT
	  MOUSE-GLYPH-WEST-CURLY-RAT-ON-BOTTOM
	  MOUSE-GLYPH-WEST-CURLY-RAT-ON-TOP
	  MOUSE-GLYPH-NORTH-CURLY-RAT-ON-LEFT
	  MOUSE-GLYPH-NORTH-CURLY-RAT-ON-RIGHT
	  MOUSE-GLYPH-EAST-CURLY-RAT-ON-BOTTOM
	  MOUSE-GLYPH-EAST-CURLY-RAT-ON-TOP
	  MOUSE-GLYPH-SOUTH-CURLY-RAT-ON-LEFT
	  MOUSE-GLYPH-SOUTH-CURLY-RAT-ON-RIGHT
	  MOUSE-GLYPH-WEST-MOUSE-ON-BOTTOM
	  MOUSE-GLYPH-WEST-MOUSE-ON-TOP
	  MOUSE-GLYPH-NORTH-MOUSE-ON-LEFT
	  MOUSE-GLYPH-NORTH-MOUSE-ON-RIGHT
	  MOUSE-GLYPH-EAST-MOUSE-ON-BOTTOM
	  MOUSE-GLYPH-EAST-MOUSE-ON-TOP
	  MOUSE-GLYPH-SOUTH-MOUSE-ON-LEFT
	  MOUSE-GLYPH-SOUTH-MOUSE-ON-RIGHT
	  MOUSE-GLYPH-SMALL-DOT
	  MOUSE-GLYPH-THICK-CROSS
	  MOUSE-GLYPH-SMALL-SOLID-CIRCLE
	  MOUSE-GLYPH-MEDIUM-SOLID-CIRCLE
	  MOUSE-GLYPH-HOLLOW-CIRCLE
	  MOUSE-GLYPH-HOLLOW-CIRCLE-MINUS
	  MOUSE-GLYPH-HOLLOW-CIRCLE-PLUS
	  MOUSE-GLYPH-SHORT-THIN-DOWN-BORDER-ARROW
	  MOUSE-GLYPH-SHORT-THIN-DOWN-ARROW
	  MOUSE-GLYPH-SHORT-THIN-UP-BORDER-ARROW
	  MOUSE-GLYPH-SHORT-THIN-UP-ARROW
	  MOUSE-GLYPH-SMALL-UP-TRIANGLE
	  MOUSE-GLYPH-SMALL-DOWN-TRIANGLE
          MOUSE-HANDEDNESS
          MOUSE-HOLLOW-RECTANGULAR-BLINKER
          MOUSE-INPUT
          MOUSE-LAST-BUTTONS
          MOUSE-LAST-BUTTONS-TIME
          MOUSE-LAST-X
          MOUSE-LAST-Y
          MOUSE-PROCESS
          MOUSE-RECTANGULAR-BLINKER
          MOUSE-SAVE-IMAGE
          MOUSE-SELECT
	  MOUSE-SENSITIVE-ITEM
          MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
          MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK
          MOUSE-SET-BLINKER
          MOUSE-SET-BLINKER-CURSORPOS
          MOUSE-SET-BLINKER-DEFINITION
          MOUSE-SET-SHEET
          MOUSE-SET-SHEET-THEN-CALL
          MOUSE-SET-WINDOW-POSITION
          MOUSE-SET-WINDOW-SIZE
          MOUSE-SHEET
          MOUSE-SPECIFY-RECTANGLE
          MOUSE-SPEED
          MOUSE-STANDARD-BLINKER
          MOUSE-WAIT
          MOUSE-WAKEUP
          MOUSE-WARP
          MOUSE-WINDOW
          SYS:MOUSE-X
          SYS:MOUSE-Y
          MOUSE-Y-OR-N-P
          MULTIPLE-CHOICE
          MULTIPLE-CHOOSE
          MULTIPLE-MOVE-BLINKER
          NAME
          NO-SCREEN-MANAGING-MIXIN
          NOISE
          NOT-EXTERNALLY-SELECTABLE-MIXIN
          NOTIFICATION-MIXIN
          NOTIFY
          NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES
          OLD-SCREEN-ARRAY
          OPEN-ALL-SHEETS-BLINKERS
          OPEN-BLINKER
          PANE-NO-MOUSE-SELECT-MIXIN
          PENDING-NOTIFICATIONS
          PHASE
          POP-UP-FINGER-WINDOW
          POP-UP-TEXT-WINDOW
          PREEMPTABLE-READ-ANY-TYI-MIXIN
          PREPARE-SHEET
          PRESERVE-SUBSTITUTE-STATUS
          PREVIOUSLY-SELECTED-WINDOWS
          PRINT-NOTIFICATIONS
          PRIORITY
          PROCESS-MIXIN
          PROCESS-TYPEAHEAD
	  READ-ANY
	  READ-ANY-NO-HANG
          READ-BIT-ARRAY-FILE
	  READ-LIST
	  READ-MOUSE-OR-KBD
	  READ-MOUSE-OR-KBD-NO-HANG
          RECTANGULAR-BLINKER
          REMAP-MOUSE
          REMOVE-BEEP-TYPE
          REMOVE-SYSTEM-KEY
          REMOVE-TERMINAL-KEY
          RESET-ON-OUTPUT-HOLD-FLAG-MIXIN
          RESET-SOUND
          RESTORE-MOUSE-MAP 
          RESTORED-BITS-P
          REVERSE-CHARACTER-BLINKER
	  RIGHT-MARGIN-SIZE
          ROTATE-180
          ROTATE-270
          ROTATE-90
	  RUBOUT-HANDLER
	  RUBOUT-HANDLER-ALPHA-CHAR-P
          SCREEN
          SCREEN-ARRAY
          SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS
          SCREEN-MANAGER-SCREEN-IMAGE
	  SCROLL-BAR-MIXIN
          SCROLL-BAR-MODE
          SCROLL-INTERPRET-ENTRY
          SCROLL-ITEM-COMPONENT-ITEMS
          SCROLL-ITEM-LEADER-OFFSET
          SCROLL-ITEM-LINE-SENSITIVITY
          SCROLL-ITEM-MOUSE-ITEMS
          SCROLL-ITEM-PLIST
          SCROLL-ITEM-SIZE
          SCROLL-MAINTAIN-LIST
          SCROLL-MAINTAIN-LIST-UNORDERED
          SCROLL-MAINTAIN-LIST-UPDATE-STATES
          SCROLL-MOUSE-MIXIN
          SCROLL-PARSE-ITEM
          SCROLL-PIXEL-INCREMENT
          SCROLL-STRING-ITEM-WITH-EMBEDDED-NEWLINES
          SCROLL-STUFF-ON-OFF-MIXIN
          SCROLL-WINDOW
          SCROLL-WINDOW-WITH-TYPEOUT-MIXIN
          SELECT-MIXIN
          SELECT-OR-CREATE-WINDOW-OF-FLAVOR
          SELECTED-IO-BUFFER
          SELECTED-WINDOW
          SELECTION-SUBSTITUTE
          SERVER-DESC-HOST-NAME
          SET-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES
          SET-SCREEN-STANDARD-FONT
          SET-STANDARD-FONT
          SET-VISIBILITY-OF-ALL-SHEETS-BLINKERS
          SETUP-APPLICATION-MODE
          SETUP-KEYBOARD-KEYCLICK
          SETUP-KEYPAD-MODE
          SETUP-MOUSE-LEFT-HANDED
          SETUP-MOUSE-RIGHT-HANDED
          SHADOW-BORDERS-MIXIN
          SHEET
          SHEET-AREA
          SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG
          SHEET-BASELINE
          SHEET-BASELINE-ADJ
          SHEET-BIT-ARRAY
          SHEET-BLINKER-LIST
          SHEET-BOTTOM-MARGIN-SIZE
          SHEET-BOUNDS-WITHIN-SHEET-P
          SHEET-CALCULATE-OFFSETS
          SHEET-CAN-GET-LOCK
          SHEET-CHAR-ALUF
          SHEET-CHAR-WIDTH
          SHEET-CLEAR
          SHEET-CLEAR-EOF
          SHEET-CLEAR-EOL
          SHEET-COMPUTE-MOTION
          SHEET-CONTAINS-SHEET-POINT-P
          SHEET-CR-NOT-NEWLINE-FLAG
          SHEET-CURRENT-FONT
          SHEET-CURSOR-X
          SHEET-CURSOR-Y
	  SHEET-DEEXPOSED-TYPEIN-NOTIFY
          SHEET-END-OF-PAGE-FLAG
          SHEET-END-PAGE-FLAG
          SHEET-ERASE-ALUF
          SHEET-EXPOSED-INFERIORS
          SHEET-EXPOSED-P
          SHEET-FOLLOWING-BLINKER
          SHEET-FONT-MAP
          SHEET-FORCE-ACCESS
          SHEET-GET-SCREEN
          SHEET-HEIGHT
          SHEET-INFERIORS
          SHEET-INSIDE-BOTTOM
          SHEET-INSIDE-HEIGHT
          SHEET-INSIDE-LEFT
          SHEET-INSIDE-RIGHT
          SHEET-INSIDE-TOP
          SHEET-INSIDE-WIDTH
          SHEET-LEFT-MARGIN-SIZE
          SHEET-LINE-HEIGHT
          SHEET-LINE-NO
          SHEET-LINE-OUT
          SHEET-ME-OR-MY-KID-P
	  SHEET-MENU-ABORT-ON-DEEXPOSE
	  SHEET-MENU-COMMAND-MENU
	  SHEET-MENU-DYNAMIC
	  SHEET-MENU-HIGHLIGHTING
	  SHEET-MENU-MULTICOLUMN
	  SHEET-MENU-POP-UP
	  SHEET-MENU-PERMANENT
          SHEET-MORE-FLAG
          SHEET-MORE-HANDLER
          SHEET-NUMBER-OF-INSIDE-LINES
          SHEET-OUTPUT-HOLD-FLAG
          SHEET-OVERLAPS-EDGES-P
          SHEET-OVERLAPS-P
          SHEET-OVERLAPS-SHEET-P
          SHEET-READ-CURSORPOS
          SHEET-RIGHT-MARGIN-CHARACTER-FLAG
          SHEET-RIGHT-MARGIN-SIZE
          SHEET-SCREEN-ARRAY
          SHEET-SET-CURSORPOS
          SHEET-SET-FONT
          SHEET-STRING-LENGTH
          SHEET-STRING-OUT
          SHEET-SUPERIOR
          SHEET-TAB-NCHARS
          SHEET-TAB-WIDTH
          SHEET-TOP-MARGIN-SIZE
          SHEET-TRUNCATE-LINE-OUT-FLAG
	  SHEET-TYO
          SHEET-WIDTH
          SHEET-WITHIN-P
          SHEET-WITHIN-SHEET-P
          SHEET-X-OFFSET
          SHEET-Y-OFFSET
          SHOW-BIT-ARRAY
          SHOW-PARTIALLY-VISIBLE-MIXIN
	  SIMPLE-BEEP
          SPLINE
          SPLIT-SCREEN-VIA-MENUS-SETUP-WINDOW
          STAY-INSIDE-BLINKER-MIXIN
          STREAM-MIXIN
          SUPERIOR
          TEMPORARY-BIT-ARRAY
          TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW
          TEMPORARY-MULTIPLE-CHOICE-WINDOW
          TEMPORARY-SHADOW-BORDERS-WINDOW-MIXIN
          TEMPORARY-WINDOW-MIXIN
          TEMPORARY-WINDOWS-LOCKED
          TEXT-SCROLL-WINDOW
          TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK
          TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN
          TIME-STAMP
          TIME-UNTIL-BLINK
          TONE
          TONE-FREQUENCY
          TOP-BOX-LABEL-MIXIN
          TOP-ITEM
          TOP-LABEL-MIXIN
	  TOP-MARGIN-SIZE
          TRUNCATING-POP-UP-TEXT-WINDOW
          TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET
          TRUNCATING-WINDOW
          TURN-OFF-ALL-SHEETS-BLINKERS
          TURN-OFF-SHEET-BLINKERS
          TURN-ON-SHEET-BLINKERS
	  TYPEOUT-ITEM-BOTTOM
	  TYPEOUT-ITEM-ITEM
	  TYPEOUT-ITEM-LEFT
	  TYPEOUT-ITEM-RIGHT
	  TYPEOUT-ITEM-TOP
	  TYPEOUT-ITEM-TYPE
          TYPEOUT-WINDOW
          TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS
	  UNREAD-ANY
          VALUE-TAB
          VISIBILITY
          WHITE-ON-BLACK
          WHO-LINE-CLOBBERED
          WHO-LINE-FILE-STATE-SHEET
          WHO-LINE-MOUSE-GRABBED-DOCUMENTATION
          WHO-LINE-PROCESS
          WHO-LINE-RUN-STATE
          WHO-LINE-SCREEN
          WIDTH
          WINDOW-CALL
          WINDOW-HACKING-MENU-MIXIN
          WINDOW-MOUSE-CALL
          WINDOW-OWNING-MOUSE
          WINDOW-RESOURCE-NAMES
          WINDOW-UNDER-MOUSE
          WINDOW-WITH-TYPEOUT-MIXIN
          WITH-BLINKER-READY
	  WITH-CLIPPING-RECTANGLE
          WITH-MOUSE-GRABBED
          WITH-MOUSE-USURPED
          WITH-SELECTION-SUBSTITUTE
          WITH-SHEET-DEEXPOSED
          WITHOUT-SCREEN-MANAGEMENT
          WRITE-BIT-ARRAY-FILE
          WRITE-KEYBOARD
          X-OFFSET
          Y-OFFSET
          X-POS
          Y-POS))

