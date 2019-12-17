;;; -*- Mode:Common-Lisp; Package:W; Fonts:(CPTFONT HL12B HL12BI); Base:10 -*-

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
;;; Copyright (C) 1983- 1989 Texas Instruments Incorporated. All rights reserved.
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains code to draw the keyboard to aid the user in entering characters
;;; whose mapping to keystrokes is not obvious.

;;;
;;; Change history:
;;;
;;;   Date	Author	Description
;;; -------------------------------------------------------------------------------------
;; 06/15/87      PMH     Changed draw-keyboard function to create a new process to display
;;                          the window in. Also added the :mouse-moves method here to detect
;;                          when the mouse moves out of the window
;; 01/07/87      TWE	Changed :ANY to :MOUSE-ANY in the who-line-documentation-string method since :ANY
;;			is incorrect.
;; 01/02/87      HW	Changed some more fonts for aesthetic purposes.
;; 11/26/86      HW	Changed font of symbol and symbol-shift characters, swapped positions
;;                          of symbol and symbol-shift characters.
;; 11/25/86	TWE	Initial creation.

;;; Define the dimensions of the keyboard drawing window.  These were found empirically.
(DEFCONSTANT KEYBOARD-DRAWING-WINDOW-WIDTH  985.)
(DEFCONSTANT KEYBOARD-DRAWING-WINDOW-HEIGHT 490.)

(DEFFLAVOR KEYBOARD-DRAWING-WINDOW-FLAVOR
   ((mouse-within-borders t))
   (TEMPORARY-SHADOW-BORDERS-WINDOW-MIXIN WINDOW)
  (:DEFAULT-INIT-PLIST
    :BLINKER-P NIL
    :WIDTH KEYBOARD-DRAWING-WINDOW-WIDTH
    :INSIDE-HEIGHT KEYBOARD-DRAWING-WINDOW-HEIGHT
    :LABEL '(:STRING "Explorer Keyboard Map" 
 		     :TOP :CENTERED
 		     :FONT FONTS:TR12B)
	      :FONT-MAP (LIST FONTS:hl6 FONTS:TVFONT FONTS:MEDFNB FONTS:CPTFONT))
  :settable-instance-variables			;; PMH Made the instance variable settable
  :outside-accessible-instance-variables		;; PHM generate an accessor macro
  (:DOCUMENTATION "Window on which to display the keyboard."))

(DEFMETHOD (KEYBOARD-DRAWING-WINDOW-FLAVOR :MOUSE-MOVES)(X Y)
  "This methods tracks if the mouse moves out of the keyboard window."
  (IF (OR (MINUSP X)
	  (MINUSP Y)
	  (> Y HEIGHT)
	  (> X WIDTH))
      (SETQ MOUSE-WITHIN-BORDERS :OUTSIDE)
      (SETQ MOUSE-WITHIN-BORDERS :INSIDE)))

(DEFMETHOD (KEYBOARD-DRAWING-WINDOW-FLAVOR :WHO-LINE-DOCUMENTATION-STRING) ()
  "Returns who-line documentation for the Symbol-Help window."
  '(:MOUSE-ANY "Remove keyboard map."
    :mouse-r-2 "System Menu"
    :DOCUMENTATION "Press any mouse button or any key on the keyboard to remove the drawing of the keyboard"))

(DEFWINDOW-RESOURCE KEYBOARD-DRAWING-WINDOW-FLAVOR ()
  :MAKE-WINDOW (KEYBOARD-DRAWING-WINDOW-FLAVOR)
  :REUSABLE-WHEN :DEACTIVATED
  :INITIAL-COPIES 0)


(DEFVAR KEYBOARD-KEY-DIMENSIONS
        `((big-box        70. 40.)
          (small-box      33. 40.)
          (locking-box    45. 40.)
          (top-row-box    42. 40.)
	  (return-box-up  21. 51.)
          (return-box     55. 40.)
	  (rubout-box     69. 40.)
	  (enter-up-box   33. 51.)
          (enter-box      33. 40.)
          (space-bar-box 299. 40.)
	  (mouse-box      33. 40.)
	  (escape-box     43. 40.)
	  (tab-box        65. 40.)
	  (shift-box      51. 40.)
	  (symbol-box     55. 40.)
	  (m-c-box        62. 40.)
	  (zero-box       71. 40.))
  "Dimensions of the keycaps for the various keycap types.
The format of each item is: (box-type width height).")

(DEFVAR KEYCAP-SPACING 5.
  "Number of pixels between each keycap.")

(DEFCONSTANT BAD-KEYBOARD-CHARACTER #o140000
  "Value in the keyboard table for non-existant characters.")

(DEFVAR KEYBOARD-KEYS 
        `(("HELP"         ,SI:SCAN-CODE-HELP              #\help          big-box       1 10.)
          ("SYSTEM"       ,SI:SCAN-CODE-SYSTEM            #\system        top-row-box   1 (10.))
          ("NETWORK"      ,SI:SCAN-CODE-NETWORK           #\network       top-row-box   1 (0))
          ("STATUS"       ,SI:SCAN-CODE-STATUS            #\status        top-row-box   1 (0))
          ("TERM"         ,SI:SCAN-CODE-TERMINAL          #\terminal      top-row-box   1 (0))
          ("CAPS LOCK"    ,SI:SCAN-CODE-CAPS-LOCK         nil             locking-box   1 (7))
          ("BOLD LOCK"    ,SI:SCAN-CODE-BOLD-LOCK         nil             locking-box   1 (0))
          ("ITALIC LOCK"  ,SI:SCAN-CODE-ITAL-LOCK         nil             locking-box   1 (0))
          ("MODE LOCK"    ,SI:SCAN-CODE-MODE-LOCK         nil             locking-box   1 (0))
          ("CLEAR SCREEN" ,SI:SCAN-CODE-CLEAR-SCREEN      #\clear-screen  top-row-box   1 (7))
          ("CLEAR INPUT"  ,SI:SCAN-CODE-CLEAR-INPUT       #\clear-input   top-row-box   1 (0))
          ("UNDO"         ,SI:SCAN-CODE-UNDO              nil             top-row-box   1 (0))
          ("END"          ,SI:SCAN-CODE-END               #\end           top-row-box   1 (0))
          ("LEFT"         ,SI:SCAN-CODE-LEFT              #\roman-i       mouse-box     1 (10.))
          ("MIDDLE"       ,SI:SCAN-CODE-MIDDLE            #\roman-ii      mouse-box     1 (0))
          ("RIGHT"        ,SI:SCAN-CODE-RIGHT             #\roman-iii     mouse-box     1 (0))
          ("F1"           ,SI:SCAN-CODE-F1                nil             small-box     1 (10.))
          ("F2"           ,SI:SCAN-CODE-F2                nil             small-box     1 (0))
          ("F3"           ,SI:SCAN-CODE-F3                nil             small-box     1 (0))
          ("F4"           ,SI:SCAN-CODE-F4                nil             small-box     1 (0))
          
          ("RESUME"       ,SI:SCAN-CODE-RESUME            #\resume        big-box       2 10.)
          ("ESCAPE"       ,SI:SCAN-CODE-ALT               #\altmode       escape-box    2 (10.))
          ("! 1"          ,SI:SCAN-CODE-1                 #\1             small-box     2 (0))
          ("@ 2"          ,SI:SCAN-CODE-2                 #\2             small-box     2 (0))
          ("# 3"          ,SI:SCAN-CODE-3                 #\3             small-box     2 (0))
          ("$ 4"          ,SI:SCAN-CODE-4                 #\4             small-box     2 (0))
          ("% 5"          ,SI:SCAN-CODE-5                 #\5             small-box     2 (0))
          ("^ 6"          ,SI:SCAN-CODE-6                 #\6             small-box     2 (0))
          ("& 7"          ,SI:SCAN-CODE-7                 #\7             small-box     2 (0))
          ("* 8"          ,SI:SCAN-CODE-8                 #\8             small-box     2 (0))
          ("( 9"          ,SI:SCAN-CODE-9                 #\9             small-box     2 (0))
          (") 0"          ,SI:SCAN-CODE-0                 #\0             small-box     2 (0))
          ("_ -"          ,SI:SCAN-CODE-MINUS             #\-             top-row-box   2 (0))
          ("+ ="          ,SI:SCAN-CODE-EQUALS            #\=             small-box     2 (0))
          ("{ `"          ,SI:SCAN-CODE-BACK-QUOTE        #\`             small-box     2 (0))
          ("} ~"          ,SI:SCAN-CODE-TILDE             #\~             small-box     2 (0))
          ("="            ,SI:SCAN-CODE-KEYPAD-EQUAL      #\=             small-box     2 (135.))
          ("+"            ,SI:SCAN-CODE-KEYPAD-PLUS       #\+             small-box     2 (0))
          ("SPACE"        ,SI:SCAN-CODE-KEYPAD-SPACE      #\space         small-box     2 (0))
          ("TAB"          ,SI:SCAN-CODE-KEYPAD-TAB        #\tab           small-box     2 (0))
          
          ("BREAK"        ,SI:SCAN-CODE-BREAK             #\break         big-box       3 10.)
          ("TAB"          ,SI:SCAN-CODE-TAB               #\tab           tab-box       3 (10.))
          ("Q"            ,SI:SCAN-CODE-Q                 #\q             small-box     3 (0))
          ("W"            ,SI:SCAN-CODE-W                 #\w             small-box     3 (0))
          ("E"            ,SI:SCAN-CODE-E                 #\e             small-box     3 (0))
          ("R"            ,SI:SCAN-CODE-R                 #\r             small-box     3 (0))
          ("T"            ,SI:SCAN-CODE-T                 #\t             small-box     3 (0))
          ("Y"            ,SI:SCAN-CODE-Y                 #\y             small-box     3 (0))
          ("U"            ,SI:SCAN-CODE-U                 #\u             small-box     3 (0))
          ("I"            ,SI:SCAN-CODE-I                 #\i             small-box     3 (0))
          ("O"            ,SI:SCAN-CODE-O                 #\o             small-box     3 (0))
          ("P"            ,SI:SCAN-CODE-P                 #\p             small-box     3 (0))
          ("[ ("          ,SI:SCAN-CODE-OPEN-PARENTHESIS  #\(             small-box     3 (0))
          ("] )"          ,SI:SCAN-CODE-CLOSE-PARENTHESIS #\)             small-box     3 (0))
          (""             ,SI:SCAN-CODE-RETURN            nil             return-box-up 3 (0))
          ("| \\"         ,SI:SCAN-CODE-BACKSLASH         #\\             small-box     3 (0))
          (,(STRING #\UP-ARROW)  ,SI:SCAN-CODE-UP-ARROW   #\hand-up       small-box     3 (48.))
          ("7"            ,SI:SCAN-CODE-KEYPAD-7          #\7             small-box     3 (48.))
          ("8"            ,SI:SCAN-CODE-KEYPAD-8          #\8             small-box     3 (0))
          ("9"            ,SI:SCAN-CODE-KEYPAD-9          #\9             small-box     3 (0))
          ("-"            ,SI:SCAN-CODE-KEYPAD-MINUS      #\-             small-box     3 (0))
          
          ("ABORT"        ,SI:SCAN-CODE-ABORT             #\abort         big-box       4 10.)
          ("RUBOUT"       ,SI:SCAN-CODE-RUBOUT            #\rubout        rubout-box    4 (10.))
          ("A"            ,SI:SCAN-CODE-A                 #\a             small-box     4 (0))
          ("S"            ,SI:SCAN-CODE-S                 #\s             small-box     4 (0))
          ("D"            ,SI:SCAN-CODE-D                 #\d             small-box     4 (0))
          ("F"            ,SI:SCAN-CODE-F                 #\f             small-box     4 (0))
          ("G"            ,SI:SCAN-CODE-G                 #\g             small-box     4 (0))
          ("H"            ,SI:SCAN-CODE-H                 #\h             small-box     4 (0))
          ("J"            ,SI:SCAN-CODE-J                 #\j             small-box     4 (0))
          ("K"            ,SI:SCAN-CODE-K                 #\k             small-box     4 (0))
          ("L"            ,SI:SCAN-CODE-L                 #\l             small-box     4 (0))
          (": ;"          ,SI:SCAN-CODE-SEMICOLON         #\;             small-box     4 (0))
          ("\" '"         ,SI:SCAN-CODE-APOSTROPHE        #\'             small-box     4 (0))
          ("RETURN"       ,SI:SCAN-CODE-RETURN            #\return        return-box    4 (0))
          ("LINE FEED"    ,SI:SCAN-CODE-LINE              #\line          small-box     4 (0))
          (,(STRING #\LEFT-ARROW) ,SI:SCAN-CODE-LEFT-ARROW #\hand-left    small-box     4 (10.))
          (""             ,SI:SCAN-CODE-HOME              nil             small-box     4 (0))
          (,(STRING #\RIGHT-ARROW) ,SI:SCAN-CODE-RIGHT-ARROW #\hand-right small-box     4 (0))
          ("4"            ,SI:SCAN-CODE-KEYPAD-4          #\4             small-box     4 (10.))
          ("5"            ,SI:SCAN-CODE-KEYPAD-5          #\5             small-box     4 (0))
          ("6"            ,SI:SCAN-CODE-KEYPAD-6          #\6             small-box     4 (0))
          (","            ,SI:SCAN-CODE-KEYPAD-COMMA      #\,             small-box     4 (0))
          
          ("SYMBOL"       ,SI:SCAN-CODE-LEFT-SYMBOL       nil             small-box     5 95.)
          (,(STRING-APPEND (STRING #\UP-ARROW) "SHIFT") ,SI:SCAN-CODE-LEFT-SHIFT nil shift-box 5 (0))
          ("Z"            ,SI:SCAN-CODE-Z                 #\z             small-box     5 (0))
          ("X"            ,SI:SCAN-CODE-X                 #\x             small-box     5 (0))
          ("C"            ,SI:SCAN-CODE-C                 #\c             small-box     5 (0))
          ("V"            ,SI:SCAN-CODE-V                 #\v             small-box     5 (0))
          ("B"            ,SI:SCAN-CODE-B                 #\b             small-box     5 (0))
          ("N"            ,SI:SCAN-CODE-N                 #\n             small-box     5 (0))
          ("M"            ,SI:SCAN-CODE-M                 #\m             small-box     5 (0))
          ("< ,"          ,SI:SCAN-CODE-COMMA             #\,             small-box     5 (0))
          ("> ."          ,SI:SCAN-CODE-PERIOD            #\.             small-box     5 (0))
          ("? /"          ,SI:SCAN-CODE-QUESTION          #\/             small-box     5 (0))
          (,(STRING-APPEND "SHIFT" (STRING #\UP-ARROW)) ,SI:SCAN-CODE-RIGHT-SHIFT nil shift-box 5 (0))
          ("SYMBOL"       ,SI:SCAN-CODE-RIGHT-SYMBOL      nil             symbol-box    5 (0))
          (,(STRING #\DOWN-ARROW) ,SI:SCAN-CODE-DOWN-ARROW #\hand-down    small-box     5 (48.))
          ("1"            ,SI:SCAN-CODE-KEYPAD-1          #\1             small-box     5 (48.))
          ("2"            ,SI:SCAN-CODE-KEYPAD-2          #\2             small-box     5 (0))
          ("3"            ,SI:SCAN-CODE-KEYPAD-3          #\3             small-box     5 (0))
	  (""             ,SI:SCAN-CODE-KEYPAD-ENTER      nil             enter-up-box  5 (0))
          
          ("HYPER"        ,SI:SCAN-CODE-LEFT-HYPER        nil             small-box     6 10.)
          ("SUPER"        ,SI:SCAN-CODE-LEFT-SUPER        nil             small-box     6 (0))
          ("META"         ,SI:SCAN-CODE-LEFT-META         nil             m-c-box       6 (9.))
          ("CTRL"         ,SI:SCAN-CODE-LEFT-CONTROL      nil             m-c-box       6 (0))
          (""             ,SI:SCAN-CODE-SPACE             #\space         space-bar-box 6 (0))
          ("CTRL"         ,SI:SCAN-CODE-RIGHT-CONTROL     nil             small-box     6 (0))
          ("META"         ,SI:SCAN-CODE-RIGHT-META        nil             small-box     6 (0))
          ("SUPER"        ,SI:SCAN-CODE-RIGHT-SUPER       nil             small-box     6 (0))
          ("HYPER"        ,SI:SCAN-CODE-RIGHT-HYPER       nil             small-box     6 (0))
          ("0"            ,SI:SCAN-CODE-KEYPAD-0          #\0             zero-box      6 (134.))
          ("."            ,SI:SCAN-CODE-KEYPAD-PERIOD     #\.             small-box     6 (0))
          ("ENTER"        ,SI:SCAN-CODE-KEYPAD-ENTER      nil             enter-box     6 (0)))
  "List of all keys on the Explorer.  The format of this list is
  (keycap-string scan-code character-code box-type row-number x-position).
The keycap-string is what is placed into the box on the screen for
that key.  The character-code is used to index into the scan code
table.  In this way we can keep this code somewhat independent of the
scan code table organization.  Box-type can be one of BIG-BOX or
SMALL-BOX.  The row number is the the row at which the upper part
of the key is on (the return key is handled special).
The x position is a pixel coordinate for the upper corner of the box.
If the x position is a list then it is the extra amount of spacing between
this key and the previous key.")

(DEFVAR KEYBOARD-CHARACTER-INFORMATION
        (MAKE-ARRAY '(#o400 5) :ELEMENT-TYPE 'INTEGER)
  "Mapping table to map a character code into one of several pieces
of information.  The table is organized to have the character code
be the row index.  The first column contains the scan code in the 
SI:KBD-TI-TABLE for the character.  The second and subsequent
columns contain the left, top, right and bottom pixel cordinates
of the box for that character in the keyboard-drawing-window.")

(DEFUN WAIT-FOR-SYMBOL-HELP-RESPONSE (WINDOW)
  (PROCESS-WAIT TV:*DEFAULT-READ-WHOSTATE*
		      #'(LAMBDA (WINDOW)
			  (IF (and (NOT (EQ (WINDOW-OWNING-MOUSE) WINDOW))
				   (eq :outside
				       (w:KEYBOARD-DRAWING-WINDOW-FLAVOR-MOUSE-WITHIN-BORDERS WINDOW)))
			      t
			      (SEND WINDOW :MOUSE-OR-KBD-TYI-NO-HANG)))
                      WINDOW))

(defun draw-keyboard (&rest ignore)
  "Spawn a process to draw the keyboard layout."
  (if (si:addin-p)
      (tv:notify tv:selected-window "Symbol-Help not currently available")
      (process-run-function "Show Keyboard Map" 'draw-keyboard-1)))

(DEFUN DRAW-KEYBOARD-1 (&REST IGNORE &AUX LAST-X-POSITION )
  "Draw the initial keyboard layout as a window."
  (MOUSE-WARP  (TRUNCATE TV:MAIN-SCREEN-WIDTH 2) (TRUNCATE TV:MAIN-SCREEN-HEIGHT 2))
  (USING-RESOURCE (KEYBOARD-DRAWING-WINDOW KEYBOARD-DRAWING-WINDOW-FLAVOR)
    (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG KEYBOARD-DRAWING-WINDOW) 0)
    (LET ((OSW SELECTED-WINDOW))
      (UNWIND-PROTECT
          (PROGN
            (EXPOSE-WINDOW-NEAR KEYBOARD-DRAWING-WINDOW '(:MOUSE))
	    (SETQ MOUSE-RECONSIDER T)
            (SEND KEYBOARD-DRAWING-WINDOW :SELECT)
            (SEND KEYBOARD-DRAWING-WINDOW :SET-CURSORPOS 0 0)
            (SEND KEYBOARD-DRAWING-WINDOW :CLEAR-EOF)
            (DOLIST (KEY KEYBOARD-KEYS)
                    (SETQ LAST-X-POSITION 
                          (DRAW-KEYBOARD-BOX (FIRST KEY) (SECOND KEY) (THIRD KEY) (FOURTH KEY) (FIFTH KEY)
                                    (IF (LISTP (SIXTH KEY))
                                        (+ LAST-X-POSITION KEYCAP-SPACING (CAR (SIXTH KEY)))
                                        ;;ELSE
                                        (SIXTH KEY))
                                    KEYBOARD-DRAWING-WINDOW)))
            (DRAW-LEGEND KEYBOARD-DRAWING-WINDOW)
        (WAIT-FOR-SYMBOL-HELP-RESPONSE KEYBOARD-DRAWING-WINDOW))
        (DELAYING-SCREEN-MANAGEMENT
          (SEND KEYBOARD-DRAWING-WINDOW :DEACTIVATE)
          (WHEN OSW (SEND OSW :SELECT NIL)))))))


(DEFUN DRAW-KEYBOARD-BOX (KEYCAP CHARACTER-SCAN-CODE CHARACTER BOX-TYPE ROW-NUMBER X-POSITION WINDOW 
                        &AUX SPACE-POSITION)
  "Draw a box representing a key and put the keycap label in the box.
Returns the X position of the right edge of the keycap."
  (LET* ((CURRENT-FONT      (SEND WINDOW :CURRENT-FONT))
	 (LOCAL-FONT)
	 (OFF-CENTER-X)
	 (FONT-HEIGHT       (FONT-CHAR-HEIGHT CURRENT-FONT))
         (KEY-DIMENSION     (ASSOC BOX-TYPE KEYBOARD-KEY-DIMENSIONS :TEST #'EQ))
         (BOX-WIDTH         (SECOND  KEY-DIMENSION))
         (BOX-HEIGHT        (THIRD   KEY-DIMENSION))
         (SHIFTED-CHAR      (AREF SI:KBD-TI-TABLE 1  CHARACTER-SCAN-CODE))
         (SYMBOL-CHAR       (AREF SI:KBD-TI-TABLE 2  CHARACTER-SCAN-CODE))
         (SYMBOL-SHIFT-CHAR (AREF SI:KBD-TI-TABLE 3  CHARACTER-SCAN-CODE))
         (LAST-X-POSITION   (+ X-POSITION BOX-WIDTH))
         (WINDOW-WIDTH      (SEND WINDOW :WIDTH))
         (Y-POSITION        (- (* (+ ROW-NUMBER (IF (> ROW-NUMBER 1) 1 0)) 50.)
                               (IF (> ROW-NUMBER 1) 20. 0)))
         (Y-CENTERED        (+ Y-POSITION (TRUNCATE (- BOX-HEIGHT
                                                       FONT-HEIGHT)
                                                    2))))
    ;; Correction factor for a font with no decenders.
    (WHEN (< LAST-X-POSITION WINDOW-WIDTH)
      (SEND WINDOW :DRAW-FILLED-RECTANGLE
            X-POSITION Y-POSITION BOX-WIDTH BOX-HEIGHT BLACK ALU-IOR NIL)
      (IF (SETQ SPACE-POSITION (POSITION #\SPACE KEYCAP :TEST #'CHAR=))
          (PROGN
            ;; We have 2 lines in the keycap.  Recompute y-centered to take 2
            ;; lines into account.
            (SETQ Y-CENTERED (+ Y-POSITION -2
                                (TRUNCATE (- BOX-HEIGHT
                                             (+ FONT-HEIGHT
						(FONT-BASELINE CURRENT-FONT)))
                                          2)))
	    (WHEN (EQ BOX-TYPE 'LOCKING-BOX)
	      ;; Move down a little to make these keys slightly off-center
	      ;; in the Y direction.
	      (SETQ Y-CENTERED (+ Y-CENTERED 5)))
	    
	    (IF (> (LENGTH (SUBSEQ KEYCAP 0 SPACE-POSITION)) 1)	;on QWERTY part of keyboard?
		(SETF OFF-CENTER-X 3                               ;no
		      LOCAL-FONT CURRENT-FONT)
		(SETF OFF-CENTER-X 12                              ;yes
	              LOCAL-FONT FONTS:TVFONT))
	    
            (SEND WINDOW :STRING-OUT-CENTERED-EXPLICIT
                  (SUBSEQ KEYCAP 0 SPACE-POSITION)
                  (+ OFF-CENTER-X X-POSITION)
		  Y-CENTERED
		   LAST-X-POSITION 
		  (+ Y-CENTERED 999.)
                  LOCAL-FONT TV:ALU-XOR)
            (SETQ Y-CENTERED (+ Y-CENTERED FONT-HEIGHT 2))
	   			
	    (SEND WINDOW :STRING-OUT-CENTERED-EXPLICIT     
		  (SUBSEQ KEYCAP (1+ SPACE-POSITION))
		  (+ OFF-CENTER-X X-POSITION) 
		  Y-CENTERED
		  LAST-X-POSITION 
		  (+ Y-CENTERED 999.)
		  LOCAL-FONT tv:ALU-XOR)
	    )
        ;;ELSE
	    (IF (AND (= (LENGTH KEYCAP) 1)		;single char on keycap?
		     (NOT (ARROW-KEY-P KEYCAP)))
		(SETF LOCAL-FONT FONTS:CPTFONT     ;yes
		      OFF-CENTER-X 12)
		(SETF LOCAL-FONT CURRENT-FONT      ;no
		      OFF-CENTER-X 3))
	    
	    (SEND WINDOW :STRING-OUT-CENTERED-EXPLICIT KEYCAP
              (+ OFF-CENTER-X X-POSITION) Y-CENTERED LAST-X-POSITION (+ Y-CENTERED 999.)
              LOCAL-FONT TV:ALU-XOR))
	  
            ;; Draw the symbol char for this key.
            (WHEN (AND SYMBOL-CHAR
                       (NOT (= SYMBOL-CHAR BAD-KEYBOARD-CHARACTER))
		       (NOT (= SYMBOL-CHAR #\CALL))	
                       (OR (NULL CHARACTER) (NOT (CHAR= SYMBOL-CHAR CHARACTER)))
                       (NOT (CHAR= SYMBOL-CHAR SHIFTED-CHAR)))
                  (SEND WINDOW :STRING-OUT-EXPLICIT
                        (FORMAT NIL "~C" SYMBOL-CHAR)
                        (+ X-POSITION 4)
                        (+ 23. Y-POSITION)
                        LAST-X-POSITION 
                        (+ Y-CENTERED 999.)
                        FONTS:MEDFNB TV:ALU-XOR))
            ;; Check for shifted symbol character.
            (WHEN (AND SYMBOL-SHIFT-CHAR
                       (NOT (= SYMBOL-SHIFT-CHAR BAD-KEYBOARD-CHARACTER))
                       (OR (NULL CHARACTER) (NOT (CHAR= SYMBOL-SHIFT-CHAR CHARACTER)))
                       (NOT (CHAR= SYMBOL-SHIFT-CHAR SHIFTED-CHAR)))
                  ;; Draw shift-symbol char for this key.
                  (SEND WINDOW :STRING-OUT-EXPLICIT
                        (FORMAT NIL "~C" SYMBOL-SHIFT-CHAR)
                        (+ X-POSITION 4)
                        (+ 3. Y-POSITION)
                        LAST-X-POSITION 
                        (+ Y-CENTERED 999.)
                        FONTS:MEDFNB TV:ALU-XOR)))
    
    (IF (= FONT-HEIGHT (FONT-BASELINE CURRENT-FONT))
	(SETQ FONT-HEIGHT (+ 2 FONT-HEIGHT)))
    (WHEN (EQ BOX-TYPE 'LOCKING-BOX)
      ;; Draw a circle in the upper right corner to represent the LED.
      (SEND WINDOW :DRAW-FILLED-CIRCLE
	    (- LAST-X-POSITION 9) (+ Y-POSITION 5) 3. BLACK TV:ALU-XOR 10 nil))
    ;;Record the information about the dimensions for this character.
    (WHEN CHARACTER
          (SETF (AREF KEYBOARD-CHARACTER-INFORMATION CHARACTER 1)      X-POSITION)       ; Left
          (SETF (AREF KEYBOARD-CHARACTER-INFORMATION CHARACTER 2)      Y-POSITION)       ; Top
          (SETF (AREF KEYBOARD-CHARACTER-INFORMATION CHARACTER 3) LAST-X-POSITION)       ; Right
          (SETF (AREF KEYBOARD-CHARACTER-INFORMATION CHARACTER 4) (+ Y-POSITION BOX-HEIGHT))     ; Bottom
          )
    LAST-X-POSITION))

(DEFUN ARROW-KEY-P (KEY)
  "Returns T if KEY is an arrow key."
  (OR (EQUAL KEY "")
      (EQUAL KEY "")
      (EQUAL KEY "")
      (EQUAL KEY "")))

(DEFUN DRAW-LEGEND (WINDOW)
   "Draw the legend to explain what the layout is for the characters."
   (LET* ((MAX-Y (+ (LOOP FOR INDEX FROM 0 BELOW (ARRAY-DIMENSION KEYBOARD-CHARACTER-INFORMATION 0)
                              MAXIMIZE (OR (AREF KEYBOARD-CHARACTER-INFORMATION INDEX 4) 0))
                       50))
           (OLD-CURRENT-FONT     (SEND WINDOW :CURRENT-FONT))
           (X-CENTERED           (TRUNCATE (SHEET-WIDTH WINDOW) 2))
           (CHARACTER-SEPARATION 2)
           (LEGEND-ENTRY         (ASSOC "P" KEYBOARD-KEYS :TEST #'EQUAL))
           (BOX-TYPE             (FOURTH LEGEND-ENTRY))
           (KEY-DIMENSION        (ASSOC BOX-TYPE KEYBOARD-KEY-DIMENSIONS :TEST #'EQ))
           (BOX-WIDTH            (SECOND  KEY-DIMENSION))
           (BOX-HEIGHT           (THIRD   KEY-DIMENSION)))
      (DRAW-KEYBOARD-BOX (FIRST LEGEND-ENTRY) (SECOND LEGEND-ENTRY) 
                  NIL
                  BOX-TYPE (TRUNCATE MAX-Y 50)
                  X-CENTERED
                  WINDOW)
      (SEND WINDOW :SET-CURRENT-FONT 2)
      (LET ((ENHANCED-FONT (SEND WINDOW :CURRENT-FONT)))
         (SEND WINDOW :STRING-OUT-EXPLICIT "Key"
                (+ X-CENTERED BOX-WIDTH CHARACTER-SEPARATION 2)
                (+ MAX-Y BOX-HEIGHT (- (FONT-CHAR-HEIGHT ENHANCED-FONT)) -2)
                MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM
                ENHANCED-FONT TV:ALU-XOR)
         
         (SEND WINDOW :STRING-OUT-EXPLICIT "SYMBOL character"
                (- X-CENTERED (SEND WINDOW :STRING-LENGTH "SYMBOL character") CHARACTER-SEPARATION)
                (+ MAX-Y BOX-HEIGHT (- (FONT-CHAR-HEIGHT ENHANCED-FONT)) 6)
                MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM
                ENHANCED-FONT TV:ALU-XOR)
      
      (SEND WINDOW :STRING-OUT-EXPLICIT "SYMBOL-SHIFT character"
                (- X-CENTERED (SEND WINDOW :STRING-LENGTH "SYMBOL-SHIFT character") CHARACTER-SEPARATION)
                (+ MAX-Y (FONT-CHAR-HEIGHT ENHANCED-FONT) -2)
                MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM
                ENHANCED-FONT TV:ALU-XOR))
      (SEND WINDOW :SET-CURRENT-FONT OLD-CURRENT-FONT)))

;;; Make this an asynchronous character like the SYSTEM and TERMINAL keys are.
(SETF KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS (NCONC
                                           '((#\CALL DRAW-KEYBOARD))
                                           KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS))
