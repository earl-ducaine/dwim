;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(medfnt hl12b hl12bi) -*-

;                           RESTRICTED RIGHTS LEGEND

;Use, duplication, or disclosure by the Government is subject to
;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;Technical Data and Computer Software clause at 52.227-7013.
; Copyright (C) 1985- Texas Instruments Incorporated. All rights reserved.

;
;                     TEXAS INSTRUMENTS INCORPORATED.
;                              P.O. BOX 2909
;                           AUSTIN, TEXAS 78769
;
; Copyright (C) 1984- 1989 Texas Instruments Incorporated. All rights reserved.

;;; Change History
;;;
;;;  Date      Author  Description
;;; -------------------------------------------------------------------------------------
;;; 02/22/88  KJF           Updated reverse-alu-translation-table with color alu's.
;;;  6/22/87   KWW          changed definition of some colors
;;;  5/20/87   KWW          Changes for color
;;;  7/6/87     PMH          Added get-graphic-font macro
;;;   4/12/87  TWE	Made DEFVARs for cptfont-font and medfnb-font so that the build process for
;;;			GWIN goes smoother.
;;; 12/04/86   TWE,JEB	Added identity-structured-list to make initialization of transform matrices an easy task.
;;; 10/21/86   JEB		Fixed CHECK-FOR-COLOR-SCREEN and COLOR-EXISTS-P to eliminate "bound but not used" warnings.
;;; 10/20/86   TWE	Changed the defvar for B&W-TABLE to use make-array instead of fillarray (for Common Lisp).
;;; 09/15/86   JEB		Moved functions from GWIN: to W:.  Deactivated all the old CADR color support.
;;;

;;
;;                           GLOBALS
;;
;; OVERVIEW
;;
;; These are the global variable and constant definitions which are used by the window
;; system.
;;


(defvar 0%-gray (make-gray 1 1 0)
   "A gray pattern array which is all white.  That means it is all zeros.") 


; 66%-gray is defined by window; scrman
;(defvar 66%-gray  (make-gray 3. 3. #o110 #o011 #o101)
;  "a gray pattern array which is 66% black.
;that means two of every three pixels are ones.")


(defvar 100%-gray (make-gray 1 1 1)
   "a gray pattern array which is all black.  that means it is all ones.") 

(DEFCONSTANT b&w-color-alist
	'(("white" . 0)    ("12% gray" . 1)  ("25% gray" . 2)  ("33% gray" . 3)
	  ("50% gray" . 4) ("66% gray" . 5)  ("75% gray" . 6)  ("88% gray" . 7)
	  ("black" . 8)    ("none"))
  "An alist of color names and their associated color table indices.
can be used as an :assoc option value in tv:multiple-choice-menus for color
selection.")

(DEFCONSTANT color-color-alist
	`(("white" . ,white) ("12% gray" . ,12%-gray-color) ("25% gray" . ,25%-gray-color)
	  ("33% gray" . ,33%-gray-color) ("50% gray" . ,50%-gray-color) ("66% gray" . ,66%-gray-color)
	  ("75% gray" . ,75%-gray-color) ("88% gray" . ,88%-gray-color) ("black" . ,black)
	  ("dark-green" . ,dark-green) ("blue" . ,blue) ("red" . ,red) ("orange"  . ,orange)
	  ("purple" . ,purple) ("pink" . ,pink) ("cyan" . ,cyan) ("magenta" . ,magenta)
	  ("yellow" . ,yellow) ("dark-blue" . ,dark-blue) ("green" . ,green)
	  ("dark-brown" . ,dark-brown) ("blue-green" . ,blue-green)
	  ("light-brown" . ,light-brown) ("red-purple" . ,red-purple) ("none"))
  "An alist of color names and their associated color table indices.
can be used as an :assoc option value in tv:multiple-choice-menus for color
selection.")


(defvar color-alist b&w-color-alist) ;;; this is the default for a fresh band - when color-convert function
                                     ;;; is run, its changed to color-color-alist.


(defvar b&w-table
        (let ((b&w-list (list 0%-gray 12%-gray 25%-gray
                              33%-gray 50%-gray 66%-gray
                              75%-gray 88%-gray 100%-gray)))
          (make-array (length b&w-list) :initial-contents b&w-list))
   "The table of gray pattern arrays for the black and white color map.") 

(defparameter b&w-black (1- (length b&w-table))
   "The color table index for the solid black color.") 

(DEFCONSTANT b&w-alu-alist
   `(("opposite" . ,alu-xor)
     ("combine"  . ,alu-transp)
     ("normal"   . ,alu-seta)
     ("erase"    . ,alu-back))
   "An alist of draw alu names and their associated values.  can be used as an
:assoc option value in tv:choose-variable-values menus")

(DEFCONSTANT color-alu-alist
   `(("opposite" . ,alu-xor)
     ("combine"  . ,alu-transp)
     ("normal"   . ,tv:alu-seta)
     ("erase"    . ,tv:alu-back)
     ("maximum"  . ,tv:alu-max)
     ("minimum"  . ,tv:alu-min)
     ("average"  . ,tv:alu-avg)
     ("add"      . ,tv:alu-add)
     ("subtract" . ,tv:alu-sub)
     ("add with saturation" . ,tv:alu-adds)
     ("subtract with clamping" . ,tv:alu-subc)
     )
   "An alist of draw alu names and their associated values.  can be used as an
:assoc option value in tv:choose-variable-values menus")

(DEFVAR alu-alist b&w-alu-alist) ;;; default to b&w mode

(defparameter opposite alu-xor
   "Exclusive-or alu function.  Bits in the object drawn are complemented and 
other bits are left alone.") 


(defparameter combine alu-transp
   "Bits in the object drawn are turned on and other
bits are left alone.") 


(defparameter normal alu-seta
   "Alu fucntion to copy the input bits to the output bits, ignoring the previous
contents.") 


(defparameter erase alu-back ;;; in monochrome acts like andca
   "Sets destination to the background color. ") 


;;; the rest of these are for color only ....


(DEFPARAMETER maximum tv:alu-max
  "Color alu operation - the larger of the source or destination is the drawn")

(DEFPARAMETER minimum tv:alu-min
  "Color alu operation - the smaller of the source or destination is drawn")

(DEFPARAMETER average tv:alu-avg
  "Color alu operation - the average of the source or destination is drawn")

(DEFPARAMETER add tv:alu-add
  "Color alu operation - adds the source and destination, and wraps around if > 255.")

(DEFPARAMETER sub tv:alu-sub
  "Color alu operation - subtracts the source from the destination, and wraps around if < 0.")

(DEFPARAMETER add-with-saturate tv:alu-adds
  "Color alu operation - the source and destination are added, and if the result exceeds the saturation
value, the result is the saturated color value.")

(DEFPARAMETER sub-with-clamp tv:alu-subc
  "Color alu operation - the source is subtracted from the destination, and if the result is less
than the clamp value, the result is the clamp value.")

(DEFPARAMETER set-background tv:alu-back
  "Color alu operation - the destination is set to the background color")


;;;;;;;;;;;;
;;; the following two forms are obsolete ways of checking for color systems
(defmacro check-for-color-screen (window)
  window                                           ; the argument is no longer used  JEB
  ;;Do the checking on window to be a color screen.
  'nil)                                            ; disable color by forcing nil return  JEB

(defun color-exists-p (&optional (screen))         ; eliminate default for "screen"  color::color-screen))
  (declare (ignore screen))                        ; which is no longer used JEB
  nil)                                             ; disable color by forcing nil return  JEB
;;;;;;;;;;;;;;;


(defvar all-sprites ()
   "A list of all sprite cursors which have been created to allow easy update.") 


(defconstant identity-structured-list '((1 0 0)
                                        (0 1 0)
                                        (0 0 1))
   "A list of the elements in an identity transformation matrix for a graphics window.")

(defparameter identity-array #2a((1 0 0)
                                          (0 1 0)
                                          (0 0 1))
   "An array of the elements in an identity transformation matrix for a graphics window.") 


(defvar picture () "A global variable for reading and writing picture file data.") 


(defparameter radians-per-degree  (quotient pi 180)
   "The conversion factor for translating degrees to radians and vice versa.") 

;; Added color alu's.  Note alu-transp and alu-back are opposites and alu-avg has no opposite. 02/22/88 KJF.
(defconstant reverse-alu-translation-table
	     #(15 11 7 3 14 10 6 2 13 9 5 1 12 8 4 0 22 18 17 19 21 20 16 24 23)
  "Reverse alu translation table.")

(defvar standard-font ()
  "The default font initialization of the current font status.") 


(defparameter standard 'fonts:cptfont
   "The font used to show standard-font in the font menu display") 



(defvar *font-list* `(("standard font"
		       :value standard-font
		       :font ,standard))
   "Fonts add themselves to this list when they are loaded.")

(defmacro get-graphics-font (s-font)
  "Given a structure font, return a graphic font of the same style according to W:*FONT-LIST*."
  ;; Could make this macro smarter in case s-font is a symbol, then
  ;; return the symbol of the new font.
  `(symbol-value
     (second
       (member :value
	       (the list (find ,s-font
			       (the list w:*font-list*)
			       :Key #'(lambda (l)
					(symbol-value (second (member :font (the list l) :test #'eq))))
			       :test #'eq)))))) 

;;; Make the W fonts that are defined by the window system also be defvars so that
;;; other functions can reference them and not get a `used free assumed special'
;;; warning message from the compiler.
(defvar cptfont-font :unbound)
(defvar medfnb-font :unbound)

(defparameter special-chars '(#\backspace #\linefeed #\newline #\tab)
   "The list of special characters in text strings which do format control.") 


(defparameter two-pi (* 2 pi) "Two times the constant pi.") 

(defvar *application-window* ()
  "The main window (or frame) of the application using the graphics window 
system.  This information is used to ensure that certain menus do not disappear
when the application is exited and reentered while the menu is exposed.")



