;;; -*- Mode:Common-Lisp; Package:FED; Base:10; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B) -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;;   UCL Command definitions and Menus

;;;  CHANGE HISTORY

;;;  6/22/87  DKM  - Change char-io-doc-cmd to refer to SAVE X instead of SAVE CHAR X

;; ===============
;; Font io commands.
;; ===============
(DEFCOMMAND list-fonts-cmd ()
  '(:DESCRIPTION "Display a menu of loaded fonts to select from."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Select" :typein-name? nil) "Select Font")
    :KEYS (#\super-a))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-list-fonts))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND display-font-cmd ()
  '(:DESCRIPTION "Display the currently selected font."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Display" :typein-name? nil) "Display Current Font")
    :KEYS (#\super-d))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-display-font))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND copy-font-cmd ()
  '(:DESCRIPTION "Copy selected font to a new font."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Copy" :typein-name? nil) "Copy Whole Font")
    :KEYS (#\super-c))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-copy-font)
  (SEND fed-frame :handle-prompt))

;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND read-file-cmd ()
  '(:DESCRIPTION "Specify a font file to be loaded into the editor and selected."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Load" :typein-name? nil) "Load Font File")
    :KEYS (#\super-l))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-read-file)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND write-file-cmd ()
  '(:DESCRIPTION "Write the current selected font to a specified file."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Write" :typein-name? nil) "Write Font File")
    :KEYS (#\super-w))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-write-file)
  (SEND fed-frame :handle-prompt))

;;;; rb 1/10/86 - make sure command issues prompt character
;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND create-font-cmd ()
  '(:DESCRIPTION "Create a new font."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Create" :typein-name? nil) "Create New Font")
    :KEYS ((#\super-m)))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-create-font)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND remove-font-cmd ()
  '(:DESCRIPTION "Remove the specified font from the fonts package."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Remove" :typein-name? nil) "Remove Font")
    :KEYS ((#\super-k)))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-remove-font)
  (SEND fed-frame :handle-prompt))

(DEFCOMMAND directory-fonts-cmd ()
  '(:DESCRIPTION "Display a menu of fonts in a FONT directory to view/edit."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Directory" :typein-name? nil) "Font Directory")
    :KEYS (#\super-f))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-list-fc-fonts)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND rotate-font-left-cmd ()
  '(:DESCRIPTION "Rotate all characters in the specified font left (270 degree orientation)."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Rotate Left" :typein-name? nil) "Rotate Font Left")
    :KEYS ((#\super-left-arrow)))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-rotate-font-left)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND rotate-font-right-cmd ()
  '(:DESCRIPTION "Rotate all characters in the specified font right (90 degree orientation)."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Rotate Right" :typein-name? nil) "Rotate Font Right")
    :KEYS ((#\super-right-arrow)))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-rotate-font-right)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND rotate-font-180-cmd ()
  '(:DESCRIPTION "Rotate all characters in the specified font upside-down (180 degree orientation)."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Rotate 180" :typein-name? nil) "Rotate Font 180")
    :KEYS ((#\super-down-arrow)
	   (#\super-up-arrow)))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-rotate-font-180)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND italicize-font-cmd ()
  '(:DESCRIPTION "Italicize all characters in the specified font."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Italicize" :typein-name? nil) "Italicize Font")
    :KEYS ((#\super-i)))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-italicize-font)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND scale-font-cmd ()
  '(:DESCRIPTION "Expand or compress each character in the specified font in either/both X and Y directions."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Stretch" :typein-name? nil) "Stretch Font")
    :KEYS (#\super-s))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-scale-font)
  (SEND fed-frame :handle-prompt))

(DEFCOMMAND thicken-font-cmd ()
  '(:DESCRIPTION "Make all characters in the current font appear more BOLD."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Thicken" :typein-name? nil) "Thicken Font")
    :KEYS (#\super-t))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-thicken-font)
  (SEND fed-frame :handle-prompt))

;;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND unthicken-font-cmd ()
  '(:DESCRIPTION "Reduce the width of all characters in the specified font."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Unthicken" :typein-name? nil) "Unthicken Font")
    :KEYS ((#\super-u)))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-unthicken-font)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND reverse-font-cmd ()
  '(:DESCRIPTION "Reverse-video all characters in this font."
    :MENUS ((ucl-fed-font-io-menu))
    :NAMES (("Reverse" :typein-name? nil) "Reverse Video Font")
    :KEYS ((#\super-v)))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-reverse-video-font)
  (SEND fed-frame :handle-prompt))

;; ===================
;; Character io commands.
;; ===================
;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND specify-char-cmd ()
  '(:DESCRIPTION "Get a character from the selected font into the black plane."
    :MENUS ((ucl-fed-char-io-menu))
    :NAMES (("Get Char" :typein-name? nil) "Get Char")
    :KEYS (#\control-g))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-specify-character)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND read-gray-char-cmd ()
  '(:DESCRIPTION "Get a character from the selected font into the gray plane."
    :MENUS ((ucl-fed-char-io-menu))
    :NAMES (("Get Gray Char" :typein-name? nil) "Get Gray Char")
    :KEYS (#\meta-g))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-read-gray-character)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND spec-char-num-cmd ()
  '(:DESCRIPTION "Get a character specified by number from the selected font into the black plane."
    :MENUS ((ucl-fed-char-io-menu))
    :NAMES (("Get Char Num" :typein-name? nil) "Get Char Num")
    :KEYS (#\meta-control-g))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-specify-character-by-number)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND save-char-cmd ()
  '(:DESCRIPTION "Save the black plane into the selected character in the selected font."
    :MENUS ((ucl-fed-char-io-menu))
    :NAMES (("Save Char" :typein-name? nil) "Save Char")
    :KEYS (#\control-p))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-save-character)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND store-char-explicit-cmd ()
  '(:DESCRIPTION "Save the black plane into an explicitly specified font and character."
    :MENUS ((ucl-fed-char-io-menu))
    :NAMES (("Save X" :typein-name? nil) "Save Explicit Char")
    :KEYS (#\meta-control-p))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-store-character-explicit)
  (SEND fed-frame :handle-prompt))

;(DEFCOMMAND remove-char-cmd ()
;  '(:DESCRIPTION "Remove the specified character from the current font."
;    :MENUS ((ucl-fed-char-io-menu))
;    :NAMES (("Remove" :typein-name? nil) "Remove Char")
;    :KEYS (#\control-r))
;  (DECLARE (SPECIAL fed-pane fed-frame))
;  (SEND FED-PANE :fed-exec-cmd 'com-remove-character)
;  (SEND fed-frame :handle-prompt))

;; ===============
;;  Editing commands.
;; ===============
(DEFCOMMAND reflect-char-cmd ()
  '(:DESCRIPTION "Reflect character about an axis."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Reflect" :typein-name? nil) "Reflect Char")
    :KEYS (#\meta-r))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-reflect-character))

(DEFCOMMAND rotate-char-left-cmd ()
  '(:DESCRIPTION "Rotate character 90 degrees counter-clockwise."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Left Rotate" :typein-name? nil) "Rotate Char Left")
    :KEYS (#\meta-left-arrow))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-rotate-character-left))

(DEFCOMMAND rotate-char-right-cmd ()
  '(:DESCRIPTION "Rotate character 90 degrees clockwise."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Right Rotate" :typein-name? nil) "Rotate Char Right")
    :KEYS (#\meta-right-arrow))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-rotate-character-right))

(DEFCOMMAND rotate-char-180-cmd ()
  '(:DESCRIPTION "Rotate character 180 degrees clockwise."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("180 Rotate" :typein-name? nil) "Rotate Char 180")
    :KEYS ((#\meta-down-arrow)
           (#\meta-up-arrow)))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-rotate-character-180))

(DEFCOMMAND italicize-char-cmd ()
  '(:DESCRIPTION "Form an italic version of the current character."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Char Italicize" :typein-name? nil) "Italicize Char")
    :KEYS (#\meta-i))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-italicize-character))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND scale-char-cmd ()
  '(:DESCRIPTION "Expand or compress the character in either/both X and Y directions."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Stretch Char" :typein-name? nil) "Stretch Char")
    :KEYS (#\meta-s))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-scale-character)
  (SEND fed-frame :handle-prompt))

(DEFCOMMAND thicken-char-cmd ()
  '(:DESCRIPTION "Make the character appear more BOLD by adding one pixel in width to all elements."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Char Thicken" :typein-name? nil) "Thicken Char")
    :KEYS (#\meta-t))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-thicken-character))

(DEFCOMMAND unthicken-char-cmd ()
  '(:DESCRIPTION "Make the character appear less BOLD by deleting one pixel in width to all wide elements."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Char Unthicken" :typein-name? nil) "Unthicken Char")
    :KEYS (#\meta-u))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd'com-unthicken-character))

(DEFCOMMAND reverse-char-cmd ()
  '(:DESCRIPTION "Reverse-video this character"
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Char Reverse" :typein-name? nil) "Reverse Video Char")
    :KEYS ((#\meta-v)))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-reverse-video-character))



(DEFCOMMAND mouse-draw-line-cmd ()
  '(:DESCRIPTION "Create a line specified by two endpoints."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Line" :typein-name? nil) "Draw Line")
    :KEYS (#\meta-control-l))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-mouse-draw-line))

(DEFCOMMAND Mouse-draw-spline-cmd ()
  '(:DESCRIPTION "Create a curved line (a spline) through the specified points."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Spline" :typein-name? nil) "Draw Spline")
    :KEYS (#\meta-control-s))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-mouse-draw-spline))

(DEFCOMMAND operate-on-rect-cmd ()
  '(:DESCRIPTION "Create a rectangle (a box) specified by two opposite corners."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Box" :typein-name? nil) "Draw Box")
    :KEYS (#\meta-control-b))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-operate-on-rectangle))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND erase-all-cmd ()
  '(:DESCRIPTION "Erase both the black and gray planes without changing the size of the character box."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Erase Both" :typein-name? nil) "Erase Both Planes")
    :KEYS (#\meta-control-e))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-erase-all)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND erase-black-cmd ()
  '(:DESCRIPTION "Erase only the black plane."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Erase Black" :typein-name? nil) "Erase Black")
    :KEYS (#\control-e))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-erase-black)
  (SEND fed-frame :handle-prompt))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND erase-gray-cmd ()
  '(:DESCRIPTION "Erase only the gray plane."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Erase Gray" :typein-name? nil) "Erase Gray")
    :KEYS (#\meta-e))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-erase-gray)
  (SEND fed-frame :handle-prompt))



(DEFCOMMAND merge-gray-cmd ()
  '(:DESCRIPTION "Merge the gray plane into the black plane."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Merge Gray" :typein-name? nil) "Merge Gray")
    :KEYS (#\meta-x))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-merge-gray))

(DEFCOMMAND merge-gray-menu-cmd ()
  '(:DESCRIPTION "Merge the gray plane into the black plane with menu for copy, set, clear, or flip."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Merge Menu" :typein-name? nil) "Merge Menu")
    :KEYS (#\meta-control-x))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-merge-gray-menu))

(DEFCOMMAND exchange-planes-cmd ()
  '(:DESCRIPTION "Exchange the gray and black planes."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Swap Planes" :typein-name? nil) "Swap Planes")
    :KEYS (#\control-x))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-exchange-planes))

(DEFCOMMAND mouse-shift-win-cmd ()
  '(:DESCRIPTION "Move both the black and gray planes from one specified reference point to another specified reference point."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Move Both" :typein-name? nil) "Move Window")
    :KEYS (#\meta-control-m))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-mouse-shift-window))

(DEFCOMMAND mouse-shift-gray-cmd ()
  '(:DESCRIPTION "Move the gray plane from one specified reference point to another specified reference point."
    :MENUS ((ucl-fed-editing-menu))
    :NAMES (("Move Gray" :typein-name? nil) "Move Gray")
    :KEYS (#\meta-m))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-mouse-shift-gray))



;; ==============
;; Screen commands.
;; ==============
(DEFCOMMAND home-cmd ()
  '(:DESCRIPTION "Move the character and box to the center of the grid window."
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("Home" :typein-name? nil) "Home")
    :KEYS (#\control-h))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-home))

(DEFCOMMAND shift-win-right-cmd ()
  '(:DESCRIPTION "Move the character and box to the right in the grid window."
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("Right" :typein-name? nil) "Window Right")
    :KEYS (#\control-meta-right-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\control-right-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-window))

(DEFCOMMAND shift-win-left-cmd ()
  '(:DESCRIPTION "Move the character and box to the left in the grid window."
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("Left" :typein-name? nil) "Window Left")
    :KEYS (#\control-meta-left-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\control-left-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-window))

(DEFCOMMAND shift-win-down-cmd ()
  '(:DESCRIPTION "Move the character and box down in the grid window."
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("Down" :typein-name? nil) "Window Down")
    :KEYS (#\control-meta-down-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\control-down-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-window))

(DEFCOMMAND shift-win-up-cmd ()
  '(:DESCRIPTION "Move the character and box up in the grid window."
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("Up" :typein-name? nil) "Window Up")
    :KEYS (#\control-meta-up-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\control-up-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-window))

;;; rb 1/10/86 - make sure command issues prompt character
(DEFCOMMAND scale-cmd ()
  '(:DESCRIPTION "Change the grid scale."
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("Set Scale" :typein-name? nil) "Set Scale")
    :KEYS (#\control-@))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-scale)
  (SEND fed-frame :handle-prompt))

(DEFCOMMAND screen-redisplay-cmd ()
  '(:DESCRIPTION "Redisplay the entire window."
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("Redisplay" :typein-name? nil) "Redisplay")
    :KEYS (#\clear-screen))
  (DECLARE (SPECIAL fed-pane fed-frame prompt-pane))
  (tv:delaying-screen-management
    (SEND FED-PANE :fed-exec-cmd 'tv:screen-redisplay))
  (SEND prompt-pane :clear-screen)
  (SEND fed-frame :handle-prompt))

(DEFCOMMAND modify-display-variables-cmd ()
  '(:DESCRIPTION "Change the variables that control how a font is displayed."
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("Change Vars" :typein-name? nil) "Change Display Variables")
    :KEYS (#\hyper-v))
  (DECLARE (SPECIAL fed-pane fed-frame))
  (SEND FED-PANE :fed-exec-cmd 'com-modify-display-variables)
  (SEND fed-frame :handle-prompt))

(DEFCOMMAND mouse-change-draw-mode-cmd ()
  '(:DESCRIPTION "Toggle the mode to draw, flip or clear. (The current mode is displayed in the label pane.)" 
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("Change Mode" :typein-name? nil) "Change Mouse Draw Mode")
    :KEYS (#\control-m))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-mouse-change-draw-mode))

; We are going to merge this functionality under the changes variables command.
(DEFCOMMAND select-new-mouse-character-cmd ()
  '(:DESCRIPTION "Select a new mouse character for drawing in the grid plane."
    :MENUS ((ucl-fed-screen-menu))
    :NAMES (("New Mouse" :typein-name? nil) "New Mouse")
    :KEYS (#\hyper-n))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-select-new-mouse))

(DEFCOMMAND clear-all-registers-cmd ()
  '(:DESCRIPTION "Clear the contents of all eight registers."
    :MENUS ((ucl-fed-all-registers-menu))
    :NAMES (("Clear Registers" :typein-name? nil) "Clear Registers")
    :KEYS (#\super-e))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-pane :fed-exec-cmd 'com-clear-all-registers))

;; Non mouse cursor commands.
(DEFCOMMAND click-square-cmd () 
  '(:DESCRIPTION "Flip, set, or clear the mouse cursor point."
    :NAMES ("Flip Cursor")
    :KEYS (#\control-space))
  (DECLARE (SPECIAL fed-pane))
  (SEND FED-PANE :fed-exec-cmd 'com-shift-cursor T))

(DEFCOMMAND shift-cursor-down-cmd ()
  '(:DESCRIPTION "Move the non-mouse cursor down."
    :NAMES ("Cursor Down")
    :KEYS (#\down-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\down-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-cursor))

(DEFCOMMAND shift-cursor-down-click-cmd ()
  '(:DESCRIPTION "Move the non-mouse cursor down after setting the square."
    :NAMES ("Cursor Down Set")
    :KEYS (#\control-down-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\control-down-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-cursor T))

(DEFCOMMAND shift-cursor-left-cmd ()
  '(:DESCRIPTION "Move the non-mouse cursor left."
    :NAMES ("Cursor Left")
    :KEYS (#\left-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\left-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-cursor))

(DEFCOMMAND shift-cursor-left-click-cmd ()
  '(:DESCRIPTION "Move the non-mouse cursor left after setting the square."
    :NAMES ("Cursor Left Set")
    :KEYS (#\control-left-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\control-left-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-cursor T))

(DEFCOMMAND shift-cursor-right-cmd ()
  '(:DESCRIPTION "Move the non-mouse cursor right."
    :NAMES ("Cursor Right")
    :KEYS (#\right-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\right-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-cursor))

(DEFCOMMAND shift-cursor-right-click-cmd ()
  '(:DESCRIPTION "Move the non-mouse cursor right after setting the square."
    :NAMES ("Cursor Right Set")
    :KEYS (#\control-right-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\control-right-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-cursor T))

(DEFCOMMAND shift-cursor-up-cmd ()
  '(:DESCRIPTION "Move the non-mouse cursor up."
    :NAMES ("Cursor Up")
    :KEYS (#\up-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\up-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-cursor))

(DEFCOMMAND shift-cursor-up-click-cmd ()
  '(:DESCRIPTION "Move the non-mouse cursor up after setting the square."
    :NAMES ("Cursor Up Set")
    :KEYS (#\control-up-arrow))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\control-up-arrow)
  (SEND FED-PANE :fed-exec-cmd 'com-shift-cursor T))


;; ====================
;; Documentation commands.
;; ====================
(DEFCOMMAND general-doc-cmd ()
  '(:DESCRIPTION "Display information about each Fed pane."
    :MENUS ((ucl-fed-help-menu))
    :NAMES (("Basic" :typein-name? nil) "Help")
    :KEYS (#\meta-help))
  (DECLARE (SPECIAL typeout-pane))
  (LET ((*terminal-io* typeout-pane))
    (SEND typeout-pane ':set-current-font 1)
    (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~35T-BASIC-~%~%")
    (FORMAT typeout-pane
"The font editor has several panes.  The following is a description
of the location and function of each of them.

 MENUS - These appear on your right and present the font editor's
         many commands in meaningful groups.  Clicking on the titles
         above each of these menus presents additional documentation
         on those entries in the following menu.

 GRID PANE - This is the main character editing window.  To edit a 
         character, first select a font, then select a character from
         that font.  

 REGISTER PANES - These are useful for temporarily storing edits. Mouse 
         clicks in these registers store or retrieve character edits.
         Additional documentation can be obtained by clicking on the
         registers title above these panes.

 LABEL PANE - The label pane lists various editor parameters which
         can be adjusted by clicking on their values.

 INTERACTOR PANE - This pane is used for prompting for values and can
         also be used for command or lisp typein.

Press the ~:@C key for UCL help.~% " #\help
    ))
  (TERPRI))

(DEFCOMMAND register-doc-cmd ()
  '(:DESCRIPTION "Display information about the Fed registers."
    :MENUS ((ucl-fed-help-menu)
	    (ucl-register-title))
    :NAMES (("Registers" :typein-name? nil) "Register")
    :KEYS (#\hyper-r))
  (DECLARE (SPECIAL typeout-pane))
  (LET ((*terminal-io* typeout-pane))
    (SEND typeout-pane ':set-current-font 1)
    (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~33T-REGISTERS-~%~%")
    (FORMAT typeout-pane
"Registers are used to temporarily store edited characters.  They
are located on the left-hand side of this screen.

To save a character in a register, move the mouse-cursor into
one of the registers, then click Mouse-Left.  The current black
plane will be saved.

To read a character from a register into the grid pane, move the 
mouse-cursor into the register, then click Mouse-Middle.

A menu listing various register operations will appear by first
moving the mouse-cursor into one of the registers, and then 
clicking Mouse-Right.~%"
    )
    (TERPRI)))

(DEFCOMMAND fed-char-doc-cmd ()
  '(:DESCRIPTION "Display information about the character box."
    :MENUS ((ucl-fed-help-menu))
    :NAMES (("Character" :typein-name? nil) "Character")
    :KEYS (#\hyper-b))
  (DECLARE (SPECIAL typeout-pane))
  (LET ((*terminal-io* typeout-pane))
    (SEND typeout-pane ':set-current-font 1)
    (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~33T-CHARACTER-~%~%")
    (FORMAT typeout-pane
"To adjust the size of the character box, click on the mouse-sensitive
values in the Label Pane.  

To move the box without moving the planes, hold down Mouse-right while
moving the mouse. 

To move the box and the planes together, use the Home command or the 
Move Both commands.~%"
      ) 
  (TERPRI)))

(DEFCOMMAND fed-cursor-doc-cmd ()
  '(:DESCRIPTION "Display information about non-mouse cursor commands."
    :MENUS ((ucl-fed-help-menu))
    :NAMES (("Cursor" :typein-name? nil) "Cursor")
    :KEYS (#\hyper-c))
  (DECLARE (SPECIAL typeout-pane))
  (LET ((*terminal-io* typeout-pane))
    (SEND typeout-pane ':set-current-font 1)
    (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~34T-CURSOR-~%~%")
    (FORMAT typeout-pane
"If you don't want to use the mouse to set points, you can use
the arrow keys to control cursor motion.  

Pressing the control key and arrow key will set the point and 
move the cursor. 

To set a single point at the current cursor location, use ~:@C .~%" #\control-space
      ) 
    (TERPRI)))

(DEFCOMMAND fed-planes-doc-cmd ()
  '(:DESCRIPTION "Display information about the black and gray planes."
    :MENUS ((ucl-fed-help-menu))
    :NAMES (("Planes" :typein-name? nil) "Planes")
    :KEYS (#\hyper-p))
  (DECLARE (SPECIAL typeout-pane))
  (LET ((*terminal-io* typeout-pane))
    (SEND typeout-pane ':set-current-font 1)
    (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~34T-PLANES-~%~%")
    (FORMAT typeout-pane
"There are two planes, black and gray.  

When you save a character, the pixels in the black plane are used
to define the character.  

The gray plane is provided to allow you to experiment or use 
another character as reference.  

Edits in the gray plane can be transferred into the black plane 
at any time using the merge commands.~%"
      )
    (TERPRI)))

(DEFCOMMAND fed-modes-doc-cmd ()
  '(:DESCRIPTION "To switch modes, click middle while over grid window."
    :MENUS ((ucl-fed-help-menu))
    :NAMES (("Modes" :typein-name? nil) "Modes")
    :KEYS (#\hyper-m))
  (DECLARE (SPECIAL typeout-pane))
  (LET ((*terminal-io* typeout-pane))
    (SEND typeout-pane ':set-current-font 1)
    (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~35T-MODES-~%~%")
    (FORMAT typeout-pane
"This field is displayed in the Label Pane and gives an indication
of the current mode of the drawing mouse, i.e., the action that
will take place when one clicks Mouse-Left while the mouse-cursor
is in the drawing pane.

Available modes are:

     FLIP-The current state of the pixel under the mouse is inverted.
          (i.e., if on, it is turned off; if off, it is turned on)

     DRAW-The pixel under the mouse is turned on.

     ERASE-The pixel under the mouse is turned off.

The state of the current mode of the drawing mouse is changed by clicking 
Mouse-middle, or by clicking over the current-value field which follows the
Mode title in the Label Pane, or by executing the Change Mode command.~%"
      )
    (TERPRI)))

(DEFCOMMAND font-io-doc-cmd ()
   '(:DESCRIPTION "Display information about font-io functions."
		  :MENUS ((ucl-fed-font-io-doc)
			  (ucl-fed-help-menu))
		  :NAMES (("Font-io" :typein-name? nil) "Font-io")
		  :KEYS (#\hyper-f))
   (DECLARE (SPECIAL typeout-pane))
   (LET ((*terminal-io* typeout-pane))
     (SEND typeout-pane ':set-current-font 3)
     (FORMAT typeout-pane " ***  Mouse a FONT-IO command name for additional documentation.  ***~%")
     (SEND typeout-pane ':set-current-font 1)
     (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~34T-FONT-IO-~%~%")
     (FORMAT typeout-pane 
"The following is a brief description of the commands relating to font-io that
can be activated.  These are located in the top-portion of the the Menus Pane 
and each of these commands are mouse-sensitive. The key assignments for each of
these commands are displayed in the Mouse Documentation Line when the commands 
are highlighted with the mouse-cursor.~%~%")
     (SEND typeout-pane ':item 'document-details-font-io " SELECT ")
     (FORMAT typeout-pane " - displays a menu of the currently loaded fonts. You can choose
    one of these fonts for editing with the mouse.~%~%")
     (SEND typeout-pane ':item 'document-details-font-io " DISPLAY ")
     (FORMAT typeout-pane " - displays all characters in the current font.~%~%")
     (SEND typeout-pane ':item 'document-details-font-io " COPY ")
     (FORMAT typeout-pane " - copies the current font to a new font name.~%~%")
     (SEND typeout-pane ':item 'document-details-font-io " LOAD ")
     (FORMAT typeout-pane " - prompts for a filename of a font to be loaded.~%~%")
     (SEND typeout-pane ':item 'document-details-font-io " WRITE ")
     (FORMAT typeout-pane " - prompts for a filename to which it will write the current font.~%~%")
     (SEND typeout-pane ':item 'document-details-font-io " CREATE ")
     (FORMAT typeout-pane " - prompts for a name for a new font to be created.~%~%")
     (SEND typeout-pane ':item 'document-details-font-io " REMOVE ")
     (FORMAT typeout-pane " - removes the specified font from the FONTS package.~%~%")
     (SEND typeout-pane ':item 'document-details-font-io " DIRECTORY ")
     (FORMAT typeout-pane " - lists the fonts on a directory you specify.  You can select 
   a font from this list to be loaded.~%~%")
     (SEND typeout-pane ':item 'document-details-font-io
	   " ROTATE, ITALICIZE, STRETCH, THICKEN, UNTHICKEN, REVERSE ")
     (FORMAT typeout-pane " - modifies all
   characters of the current according to the specific action implied by the command
   name and saves the altered characters in a new font."))
   (TERPRI))

(DEFCOMMAND char-io-doc-cmd ()
   '(:DESCRIPTION "Display information about char-io functions."
		  :MENUS ((ucl-fed-char-io-doc)
			  (ucl-fed-help-menu))
		  :NAMES (("Character-io" :typein-name? nil) "Character-io")
		  :KEYS (#\hyper-a))
   (DECLARE (SPECIAL typeout-pane))
   (LET ((*terminal-io* typeout-pane))
     (SEND typeout-pane ':set-current-font 3)
     (FORMAT typeout-pane "***  Mouse a CHARACTER-IO command name for additional documentation. ***~%")
     (SEND typeout-pane ':set-current-font 1)
     (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~31T-CHARACTER-IO-~%~%")
     (FORMAT typeout-pane 
"The following is a brief description of the commands relating to character-io 
that can be activated.  These are located in the upper-middle portion of the 
Menus Pane and each of these commands are mouse-sensitive.  The key assignments 
for each of these commands are displayed in the Mouse Documentation Line when the 
commands are highlighted with the mouse-cursor.~%~%")
     (SEND typeout-pane ':item 'document-details-character-io " GET CHAR ")  
     (FORMAT typeout-pane " -  gets the specified character from the current font for editing.~%~%")
     (SEND typeout-pane ':item 'document-details-character-io " GET CHAR NUM ")  
     (FORMAT typeout-pane " -  gets the specified character, indicated by its numerical
   position within the font, from the current font for editing.~%~%")
     (SEND typeout-pane ':item 'document-details-character-io " GET GRAY CHAR ")  
     (FORMAT typeout-pane " -  gets the indicated character from a specified font for editing.~%~%")
     (SEND typeout-pane ':item 'document-details-character-io " SAVE CHAR")
     (FORMAT typeout-pane " - stores the current character into the current font in its appropriate
   position.~%~%")
     (SEND typeout-pane ':item 'document-details-character-io " SAVE X ")  ;save x instead of save char x  dkm 5/12/87
     (FORMAT typeout-pane " -  stores the current character into a font and character position
   for which the editor prompts you.~%~%"))
   (TERPRI))

(DEFCOMMAND editing-doc-cmd ()
  '(:DESCRIPTION "Display information about character editing functions."
    :MENUS ((ucl-fed-editing-doc)
	    (ucl-fed-help-menu))
    :NAMES (("Editing" :typein-name? nil) "Editing")
    :KEYS (#\hyper-e))
  (DECLARE (SPECIAL typeout-pane))
  (LET ((*terminal-io* typeout-pane))
    (SEND typeout-pane ':set-current-font 3)
    (FORMAT typeout-pane " ***  Mouse an EDITING command name for additional documentation.  ***~%")
    (SEND typeout-pane ':set-current-font 1)
    (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~34T-EDITING-~%~%")
    (FORMAT typeout-pane 
"The following is a brief description of the commands relating to character editing 
that can be activated.  These are located in the middle-portion of the Menus Pane 
and each of these commands are mouse-sensitive.  The key assignments for each of 
these commands is displayed in the Mouse Documentation Line when the commands are 
highlighted with the mouse-cursor.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " REFLECT ")
    (FORMAT typeout-pane " -  reflects the current character around a specified axis.
   A menu of the available axis is provided.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " ROTATE, ITALICIZE, STRETCH, THICKEN, UNTHICKEN, REVERSE ")
    (FORMAT typeout-pane " -  performs
   the indicated operation on the current character in the drawing pane.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " LINE ")
    (FORMAT typeout-pane " -  creates a line on the drawing pane between two points. 
   You specify each point by clicking-left.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " SPLINE ")
    (FORMAT typeout-pane " -  creates a spline (a curved line) on the drawing pane through a 
   series of points. You specify a point by clicking left; you connect the points by 
   clicking right.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " BOX ")
    (FORMAT typeout-pane " -  creates a rectangular area on the drawing pane.  You specify two 
   opposite corners of the rectangle by clicking left.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " ERASE BOTH, ERASE BLACK, ERASE GRAY ")
    (FORMAT typeout-pane " -  clears the drawing pane
   of all pixels, of black pixels, or of gray pixels respectively.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " MERGE GRAY ")
    (FORMAT typeout-pane " -  merges the gray plane into the black plane.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " MERGE MENU ")
    (FORMAT typeout-pane " -  merges the gray plane into the black plane using a menu to
   indicate whether the gray bits are to copy, set, clear, or flip corresponding bits in 
   the black plane.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " SWAP PLANES ")
    (FORMAT typeout-pane " -  swaps the contents of the gray and black planes.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " MOVE BOTH ")
    (FORMAT typeout-pane " -  moves the character relative to the drawing pane.  You specify 
   start and end points for the relative motion by clicking left.~%~%")
    (SEND typeout-pane ':item 'document-details-editing " MOVE GRAY ")
    (FORMAT typeout-pane " -  moves the gray area relative to the drawing pane by indicating
   start and end points for the relative motion.~%~%"))
  (TERPRI))

(DEFCOMMAND screen-doc-cmd ()
   '(:DESCRIPTION "Display information about screen manipulation functions."
		  :MENUS ((ucl-fed-screen-doc)
			  (ucl-fed-help-menu))
		  :NAMES (("Screen" :typein-name? nil) "Screen")
		  :KEYS (#\hyper-s))
   (DECLARE (SPECIAL typeout-pane))
   (LET ((*terminal-io* typeout-pane))
     (SEND typeout-pane ':set-current-font 3)
     (FORMAT typeout-pane " ***  Mouse a SCREEN command name for additional documentation.  ***~%~%")
     (SEND typeout-pane ':set-current-font 1)
     (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~34T-SCREEN-~%~%")
     (FORMAT typeout-pane 
"The following is a brief description of the commands relating to
screen operations that can be activated.  These are located in the 
lower-middle portion of the Menus Pane and each of these commands
are mouse-sensitive.  The key assignments for each of these commands
are displayed in the Mouse Documentation Line when the commands are
highlighted with the mouse-cursor.~%~%")
     (SEND typeout-pane ':item 'document-details-screen " HOME ")
     (FORMAT typeout-pane " -  moves the display to the center of the drawing pane.~%~%")
     (SEND typeout-pane ':item 'document-details-screen " REDISPLAY ")
     (FORMAT typeout-pane " -  redisplays the entire screen.~%~%")
     (SEND typeout-pane ':item 'document-details-screen " SET SCALE ")
     (FORMAT typeout-pane " -  sets the scale of the drawing pane by changing the
   size of the grid represented on screen. Scale values below 6 do not provide a 
   grid on the drawing pane. ~%~%")
     (SEND typeout-pane ':item 'document-details-screen " CHANGE MODE ")
     (FORMAT typeout-pane " -  toggles the current mode of the drawing mouse between 
   flip/draw/erase.~%~%")  
     (SEND typeout-pane ':item 'document-details-screen " CHANGE VARS ")
     (FORMAT typeout-pane " -  selects variable values for the font display.~%~%")  
     (SEND typeout-pane ':item 'document-details-screen " LEFT, UP, RIGHT, DOWN ") 
     (FORMAT typeout-pane " -  move the display in the indicated direction.~%~%")
     (SEND typeout-pane ':item 'document-details-screen " NEW MOUSE ")
     (FORMAT typeout-pane " -  selects a new drawing mouse character for use on the
   drawing pane from a menu of available mouse characters.~%~%")   
     )
   (TERPRI))

(DEFCOMMAND help-doc-cmd ()
  '(:DESCRIPTION "Display information about general help functions."
    :MENUS ((ucl-fed-help-doc))
    :NAMES (("Help" :typein-name? nil) "Help")
    :KEYS (#\hyper-h))
  (DECLARE (SPECIAL typeout-pane))
  (LET ((*terminal-io* typeout-pane))
    (SEND typeout-pane ':set-current-font 1)
    (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%~35T-HELP-~%~%")
    (FORMAT typeout-pane " The help commands provide some general font editor documentation.~%~%")
    (TERPRI)))

;;; A few commands created to clean up some existing fonts.
;;; These will not be included in any menu and will not be documented
;;; as part of the font editor - but, since they appeared to have worked,
;;; the commands and their associated functions will be retained here in the code.
(DEFCOMMAND remove-top-row-pixels-cmd ()
  '(:DESCRIPTION "Remove the top row of pixels from all characters in the current font."
    :NAMES ("Remove Top Row")
    :KEYS (#\super-hyper-r))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\super-hyper-r)
  (SEND FED-PANE :fed-exec-cmd 'com-remove-top-row-pixels-font))

(DEFCOMMAND remove-bottom-row-pixels-cmd ()
  '(:DESCRIPTION "Remove the bottom row of pixels from all characters in the current font."
    :NAMES ("Remove Bottom Row")
    :KEYS (#\super-hyper-meta-r))
  (DECLARE (SPECIAL ucl:kbd-input fed-pane))
  (SETQ ucl:kbd-input #\super-hyper-meta-r)
  (SEND FED-PANE :fed-exec-cmd 'com-remove-bottom-row-pixels-font))

(DEFCOMMAND end-cmd nil
  '(:description "Exit the Font Editor."
    :names ("Exit Font Editor")
    :Keys (#\end))
  (DECLARE (SPECIAL fed-frame))
  (SEND fed-frame :deselect))



;Make the UCL command table.
(BUILD-COMMAND-TABLE 'fed-cmd-table 'fed-frame
  '(end-cmd
    shift-win-right-cmd 
    shift-win-left-cmd 
    shift-win-down-cmd 
    shift-win-up-cmd 
    scale-cmd 
    operate-on-rect-cmd
    specify-char-cmd 
    read-gray-char-cmd 
    spec-char-num-cmd 
    display-font-cmd
    modify-display-variables-cmd
    erase-all-cmd
    erase-black-cmd
    erase-gray-cmd 
    screen-redisplay-cmd 
    click-square-cmd  
    shift-cursor-up-cmd 
    shift-cursor-down-cmd 
    shift-cursor-left-cmd 
    shift-cursor-right-cmd 
    shift-cursor-up-click-cmd 
    shift-cursor-down-click-cmd 
    shift-cursor-left-click-cmd 
    shift-cursor-right-click-cmd 
    home-cmd 
    mouse-draw-line-cmd
    mouse-change-draw-mode-cmd
    mouse-draw-spline-cmd 
    merge-gray-cmd 
    merge-gray-menu-cmd 
    read-file-cmd 
    reflect-char-cmd 
    save-char-cmd
    store-char-explicit-cmd
;    remove-char-cmd
    scale-char-cmd
    scale-font-cmd
    mouse-shift-win-cmd 
    mouse-shift-gray-cmd 
    write-file-cmd 
    exchange-planes-cmd
    list-fonts-cmd
    directory-fonts-cmd
    copy-font-cmd
    remove-font-cmd
    create-font-cmd
    thicken-font-cmd
    unthicken-font-cmd
    thicken-char-cmd
    unthicken-char-cmd
    italicize-font-cmd
    italicize-char-cmd
    rotate-char-left-cmd
    rotate-char-right-cmd
    rotate-char-180-cmd
    rotate-font-left-cmd
    rotate-font-right-cmd
    rotate-font-180-cmd
    reverse-char-cmd
    reverse-font-cmd
    select-new-mouse-character-cmd
    clear-all-registers-cmd
    remove-top-row-pixels-cmd
    remove-bottom-row-pixels-cmd
    general-doc-cmd
    font-io-doc-cmd
    char-io-doc-cmd
    editing-doc-cmd
    screen-doc-cmd
    help-doc-cmd
    register-doc-cmd
    fed-char-doc-cmd
    fed-modes-doc-cmd
    fed-planes-doc-cmd
    fed-cursor-doc-cmd)
  :INIT-OPTIONS
  '(:NAME "FED Command Table"
    :DOCUMENTATION "The FED commands."))

;;;New menu which contains an entry to exit the font editor
(BUILD-MENU 'ucl-fed-exit 'fed-frame
  :default-item-options '(:font fonts:hl12b)
  :item-list-order '(end-cmd))

;;; New menu containing the title over the registers that is also used as register documentation.
(BUILD-MENU 'ucl-register-title 'fed-frame
	    :default-item-options '(:font fonts:hl12b)
	    :item-list-order '(register-doc-cmd))
;;; New menu below the registers used for command to clear all registers at once.
(BUILD-MENU 'ucl-fed-all-registers-menu 'fed-frame
	    :default-item-options '(:font fonts:TR8B)
	    :item-list-order '(clear-all-registers-cmd))

;;; Re-ordered the commands to both
;;;    put them in some form of logical sequence (which they already were) PLUS
;;;    place most of the heavily-used commands closest to the editing window
;;;    leaving the least-frequently-used commands at the right margin.
;;; Add new menu to provide a title for the font-io commands and documentation on same. 
(BUILD-MENU 'ucl-fed-font-io-doc 'fed-frame
	    :default-item-options '(:font fonts:hl12b)
	    :item-list-order '(font-io-doc-cmd))

(BUILD-MENU 'ucl-fed-font-io-menu 'fed-frame
            :default-item-options '(:font fonts:cptfont)
	    :item-list-order '(list-fonts-cmd       rotate-font-left-cmd
			       display-font-cmd     rotate-font-right-cmd
			       copy-font-cmd        rotate-font-180-cmd
			       read-file-cmd        italicize-font-cmd
			       write-file-cmd       scale-font-cmd
			       create-font-cmd      thicken-font-cmd         
			       remove-font-cmd      unthicken-font-cmd
			       directory-fonts-cmd  reverse-font-cmd))

;;; Add new menu to act as title for the character io commands and provide documentation on same.
(BUILD-MENU 'ucl-fed-char-io-doc 'fed-frame
	    :default-item-options '(:font fonts:hl12b)
	    :item-list-order '(char-io-doc-cmd))

(BUILD-MENU 'ucl-fed-char-io-menu 'fed-frame
            :default-item-options '(:font fonts:cptfont)
	    :item-list-order '(specify-char-cmd     save-char-cmd     
	    		       spec-char-num-cmd    store-char-explicit-cmd 	
			       read-gray-char-cmd   "   "))                         ;remove-char-cmd

;;; Add new menu to act as title for the editing commands and provide documentation on same.
(BUILD-MENU 'ucl-fed-editing-doc 'fed-frame
	    :default-item-options '(:font fonts:hl12b)
	    :item-list-order '(editing-doc-cmd))
(BUILD-MENU 'ucl-fed-editing-menu 'fed-frame
            :default-item-options '(:font fonts:cptfont)
	    :item-list-order '(reflect-char-cmd        scale-char-cmd
			       rotate-char-left-cmd    thicken-char-cmd 
			       rotate-char-right-cmd   unthicken-char-cmd
			       rotate-char-180-cmd     reverse-char-cmd      
			       italicize-char-cmd      " "  
			       " "                     " "
			       mouse-draw-line-cmd     erase-all-cmd
			       mouse-draw-spline-cmd   erase-black-cmd 
			       operate-on-rect-cmd     erase-gray-cmd
			       " "                     " "
			       merge-gray-cmd          mouse-shift-win-cmd
			       merge-gray-menu-cmd     mouse-shift-gray-cmd  	        
			       exchange-planes-cmd     "          "))


;;; Add new menu to act as title for the screen-control commands and provide documentation on same.
(BUILD-MENU 'ucl-fed-screen-doc 'fed-frame
	    :default-item-options '(:font fonts:hl12b)
	    :item-list-order '(screen-doc-cmd))
(BUILD-MENU 'ucl-fed-screen-menu 'fed-frame
            :default-item-options '(:font fonts:cptfont)
	    :item-list-order '(home-cmd                        shift-win-left-cmd
			       screen-redisplay-cmd            shift-win-up-cmd
			       scale-cmd                       shift-win-right-cmd
			       mouse-change-draw-mode-cmd      shift-win-down-cmd
			       modify-display-variables-cmd    select-new-mouse-character-cmd))        

;;; Add new menu to act as title for the help commands and provide documentation on same.
(BUILD-MENU 'ucl-fed-help-doc 'fed-frame
	    :default-item-options '(:font fonts:hl12b)
	    :item-list-order '(help-doc-cmd))
(BUILD-MENU 'ucl-fed-help-menu 'fed-frame
            :default-item-options '(:font fonts:cptfont)
	    :item-list-order '(general-doc-cmd        fed-planes-doc-cmd
			       fed-char-doc-cmd       fed-modes-doc-cmd
			       fed-cursor-doc-cmd     register-doc-cmd
			       "  "                   "  "
			       font-io-doc-cmd        char-io-doc-cmd
			       editing-doc-cmd        screen-doc-cmd))



