;;; -*- Mode:Common-Lisp; Package:MACINTOSH; Base:10; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB) -*-


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
;;; Copyright (C) 1988-1989 Texas Instruments Incorporated. All rights reserved.

;;; These functions have been lifted from the FED: package and placed in the
;;; MACINTOSH: package in support of custom mouse blinkers.  Since the FED:
;;; package will never exist in any microExplorer band, the duplication of
;;; these functions and the use of the FED:xxx properties as MAC:xxx properties
;;; should not cause any confusion.

(DEFVAR *min-font-length* 128.)

(DEFSTRUCT (font-descriptor (:type :array-leader) :named (:callable-constructors nil) (:conc-name nil))
	   (fd-fill-pointer 0)
	   fd-name
	   FD-LINE-SPACING		;; ;  Vertical distance between baselines.
	   FD-BASELINE			;; ;  Vertical distance from top of characters in this font.  
					;; ;  The baseline is what is aligned for different fonts.
	   FD-BLINKER-HEIGHT		;; ;  Height of a "blinker" in this font.
	   FD-BLINKER-WIDTH		;; ;  Width of a "blinker" in this font.
	   FD-SPACE-WIDTH		;; ;  Width of a space.
	   FD-DOUBLE-WIDTH-P		;; ;  T means this font is intended for display
					;; ;  with twice as many pixels per unit distance
					;; ;  in the horizontal direction. (ESCAPE 7)

					;; ;  The rest is for saving info that
					;; ;  comes in Xerox fonts, so we don't lose it.
	   (fd-vert-resolution 3840.)	;; ;  Dots per inch, times ten.
	   (fd-horiz-resolution 3840.)	;; ;  " "
					;; ;  Default is right for the Dover.
	   (fd-rotation 0)		;; ;  Rotation in degrees.
	   )


;; ;  A CHAR-DESCRIPTOR is a two dimensional array (with leader).
;; ;  The first dimension is the height of the character and the second is the width

(DEFSTRUCT (char-descriptor (:type :array-leader) :named (:callable-constructors nil) (:conc-name nil))
	   cd-fill-pointer
	   cd-name
	   cd-char-width		;; ;  The horizontal distance taken by this character
	   cd-char-vert-width		;; ;  Vertical spacing caused by this character
					;; ;    (always 0 for the usual horizontal font).
					;; ;  For the sake of Xerox fonts.
	   cd-char-left-kern)		;; ;  The distance to the left to move before placing the
					;; ;  character.  A left kern of -5 means the array is to
					;; ;  be placed 5 units to the right of the current position.


(DEFUN font-name-font-descriptor (fontname &aux fd)
  "Return a font-descriptor whose data is equivalent to font FONTNAME.
If we computed one previously, we return it; otherwise, we create one.
This assumes that anyone who alters the contents of the font
also alters the corresponding font descriptor."
  (OR (SYMBOLP fontname) (SETQ fontname (tv:font-name fontname)))
  (SETQ fd (GET fontname 'font-descriptor))
  (COND
    ((AND fd (EQ (GET fontname 'font-described) (SYMBOL-VALUE fontname))))
    (t (SETQ fd (font-into-font-descriptor (SYMBOL-VALUE fontname)))
     (SETF (GET fontname 'font-described) (SYMBOL-VALUE fontname))
     (SETF (GET fontname 'font-descriptor) fd)))
  fd)


(DEFUN font-into-font-descriptor (font &optional (dbl-width-p nil))
  (LET ((font-length (tv:font-fill-pointer font))
	(line-spacing (tv:font-char-height font))
	(raster-height (tv:font-raster-height font))
	(baseline (tv:font-baseline font))
	(blinker-height (tv:font-blinker-height font))
	(blinker-width (tv:font-blinker-width font))
	(space-width (tv:font-char-width font))
	(fcwt (tv:font-char-width-table font))
	(fcw (tv:font-char-width font))
	(ck (tv:font-left-kern-table font))
	font-descriptor font-chars-exist-table temp
	raster-width character-width left-kern pixel)
    "Create an return a font-descriptor containing the data from font."
    
    (ERRSET (SETQ font-chars-exist-table (tv:font-chars-exist-table font)) nil)
    ;; Correct for old fonts that may not have valid fill pointers.
    (WHEN (< (tv:font-fill-pointer font) *min-font-length*)
      (SETQ font-length *min-font-length*))
    (SETQ font-descriptor (make-font-descriptor :make-array (:length font-length)
						:fd-fill-pointer font-length))
    (SETF (fd-name font-descriptor) (tv:font-name font))
    (SETF (fd-line-spacing font-descriptor) line-spacing)
    (SETF (fd-baseline font-descriptor) baseline)
    (SETF (fd-blinker-height font-descriptor) blinker-height)
    (SETF (fd-blinker-width font-descriptor) blinker-width)
    (SETF (fd-space-width font-descriptor) space-width)
    (SETF (fd-double-width-p font-descriptor) dbl-width-p)
    (DO ((CHAR-CODE 0 (1+ char-code)))
	((>= char-code font-length))
      (COND ((AND font-chars-exist-table
		  (ZEROP (AREF font-chars-exist-table char-code))))
	    (t
	     ;; Characters 128.-160. are reserved for LISPM control characters.  They should not
	     ;; have a character stored in that position, and the character width must be NIL for
	     ;; the window system to handle these chars correctly.
	     (SETQ character-width (COND (fcwt (OR (AREF fcwt char-code) fcw)) ;don't put nil in CD.
					 (t fcw)))
	     (SETQ raster-width (tv:font-char-min-raster-width font char-code)) 
	     ;; If we don't know for sure which chars exist, 
	     ;; discard chars containing no information.
	     (COND ((AND (NULL font-chars-exist-table)
			 (ZEROP raster-width)
			 (/= char-code (CHAR-INT #\space))
			 (IF character-width
			     (= character-width (tv:font-char-width font))
			     t)))
		   (t (SETQ left-kern (COND (ck (AREF ck char-code))
					    (t 0)))
		      (SETQ temp
			    (make-char-descriptor
			      :make-array (:type 'art-4b :length (LIST raster-height raster-width))
			      :cd-char-width character-width
			      :cd-char-left-kern left-kern))
		      (SETF (AREF font-descriptor char-code) temp)
		      (COND (dbl-width-p
			     (DO ((row 0 (1+ row)))
				 ((>= row raster-height))
			       (DO ((coli 0 (1+ coli))
				    (col 0 (+ 2 col)))
				   ((>= col raster-width))
				 (SETQ pixel (font-get-pixel font char-code row coli))
				 (SETF (AREF temp row col) pixel)
				 (SETF (AREF temp row (1+ col)) (LSH pixel -1)))))
			    (t
			     (DO ((row 0 (1+ row)))
				 ((>= row raster-height))
			       (DO ((col 0 (1+ col)))
				   ((>= col raster-width))
				 (SETF (AREF temp row col) (font-get-pixel font char-code row col)) )))))))))
    font-descriptor))


(DEFUN FONT-GET-PIXEL (FONT CHAR ROW COL &AUX TEM (NEXTCHAR (1+ CHAR)))
  "Get the pixel at position ROW, COL in character CHAR of FONT.
FONT should be a font array, not a name."
  (COND
    ((OR (< ROW 0) (>= ROW (TV:FONT-RASTER-HEIGHT FONT)) (< COL 0)
	(COND
	  ((SETQ TEM (TV:FONT-INDEXING-TABLE FONT))
	   (SETQ CHAR (+ (AREF TEM CHAR) (TRUNCATE COL (TV:FONT-RASTER-WIDTH FONT))))
	   (SETQ COL (REM COL (TV:FONT-RASTER-WIDTH FONT))) (>= CHAR (AREF TEM NEXTCHAR)))
	  ((>= COL (TV:FONT-RASTER-WIDTH FONT)))))
     0);out of bounds, return 0
    (T
     (DO ((FONT FONT (TV:FONT-NEXT-PLANE FONT))
	  (PIXEL 0)
	  (PLANENUM 0 (1+ PLANENUM)))
	 ((NULL FONT)
	  PIXEL)
       (SETQ PIXEL
	     (+ PIXEL
		(LSH
		 (AREF FONT
		       (+
			(* 32.
			   (+ (* (TV:FONT-WORDS-PER-CHAR FONT) CHAR)
			      (TRUNCATE ROW (TV:FONT-RASTERS-PER-WORD FONT))))
			(+
			 (* (TV:FONT-RASTER-WIDTH FONT)
			    (REM ROW (TV:FONT-RASTERS-PER-WORD FONT)))
			 COL)))
		 PLANENUM)))))))