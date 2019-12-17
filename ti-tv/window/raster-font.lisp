;;;-*- Mode:Common-Lisp; Package:W; Fonts:(MEDFNT HL12B HL12BI); Base:10 -*-

;;;			      RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769

;;; Copyright (C) 1984-1989 Texas Instruments Incorporated.  All rights reserved.

;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 02/04/87 JEB		Eliminated definition of cptfont, which is defined earlier as standard font.
;;; 10/22/86 TWE		Changed the function define-raster-font-internal so that it doesn't use fillarray.
;;;			Changed references to make-pixel-array to use make-array instead.
;;; 10/02/86 JEB		Copied functions from GWIN: to W: to generate the standard raster fonts.
;;;


;;
;;                RASTER-FONT
;;



(defmacro define-raster-font (font-symbol w-font font-description)
  "This macro allows the definitions of raster fonts.  It creates
 the font, creates the raster-character objects in the font,
 and updates the font list used to choose fonts."
  
  `(define-raster-font-internal ',font-symbol ,font-symbol ',w-font
     ,font-description))

(defun define-raster-font-internal
       (font-symbol system-font w-font font-description)
  "This is used by the define-raster-font macro to define a raster font.
It creates the raster font object, creates the raster character objects in the
raster font, and updates the list of raster fonts."
  
  (set w-font
       (make-instance 'font
		      :horz-spacing    (font-char-width         system-font)
		      :vert-spacing    (font-char-height        system-font)
		      :blinker-height  (font-blinker-height     system-font)
		      :blinker-width   (font-blinker-width      system-font)))
  (loop with chars-exist-table = (if (< (array-leader-length    system-font) 17)
                                        (font-chars-exist-table system-font))
     with all-chars-exist         = (if chars-exist-table () t)
     with default-char-width      = (font-char-width               system-font)
     with char-height             = (font-char-height            system-font)
     with char-width-table        = (font-char-width-table       system-font)
     with left-kern-table         = (font-left-kern-table        system-font)
     with fill-pointer            = (font-fill-pointer           system-font)
     with number-chars-in-font    = (if (and fill-pointer (> fill-pointer 0))
                                        fill-pointer
                                      128)
     with file-name               = (make-pathname
				      :host "sys"
				      :directory "Window"
				      :name (string w-font))
     with new-array
     for char-index from 0 below number-chars-in-font
     for char-width = (if char-width-table
                          (aref char-width-table char-index)
                        default-char-width)
     for char-left-kern = (if left-kern-table
			       (aref left-kern-table char-index)
			    0)
     if (or all-chars-exist (= (aref chars-exist-table char-index) 1))
        do (setq new-array
		  (make-array `(,w:cache-size ,w:cache-size)
			      :element-type 'bit :initial-element 0))
           (fill-font-array new-array char-index system-font)
           (send (eval w-font) :set-character char-index
	      (make-instance 'raster-character
			     :identity-cache new-array
			     :identity-width char-width
		             :identity-height char-height
			     :left-kern char-left-kern))
     finally (dump-forms-to-file file-name
		    `((setq ,w-font ',(eval w-font))
			   (setq *font-list*
				 (loop for list in *font-list*
				       for font = (fifth list)
				       unless (eq font ',font-symbol)
				       collect list))
			   (push '(,font-description
				   :value ,w-font
				   :font ,font-symbol)
			         *font-list*))
			 '(:mode :Common-lisp :package w :base 10))))


(defun fill-font-array (new-array char-index system-font)
  "Fills a 2-dimensional array with the character char-index in the system-font
specified"
  
  (loop with raster-width       = (font-raster-width     system-font)
	with rows-per-word      = (font-rasters-per-word system-font)
	with words-per-char     = (font-words-per-char   system-font)
        with word-length        = 32
	with font-raster-length = (* words-per-char word-length)
	with indexing-table     = (font-indexing-table   system-font)
	with start-index        = (if indexing-table
                                      (aref indexing-table char-index)
                                     char-index)
        with end-index          = (if indexing-table
			              (aref indexing-table (1+ char-index))
		                    (1+ char-index))
        for x from start-index below end-index
	for char-adj = (* (- x start-index) word-length)
	for char-array-ptr = (* x font-raster-length)
	do (loop for word from 0 below words-per-char
		 do (loop for temp-row from 0 below rows-per-word
			  for row from (+ (* word rows-per-word) temp-row)
			  for start-ptr from (+ char-array-ptr (* word word-length))
			                by raster-width
			  do (loop for i from 0 below raster-width
				   for ptr from start-ptr
				   do (setf (aref new-array row (+ i char-adj))
					    (aref system-font ptr)))))))


(defun define-w-fonts ()
  "Creates the fonts used by the graphics window system.  These fonts
are in order by size to help in creating a momentary menu using them."
  
  (define-raster-font fonts:medfnb medfnb-font "Bold Medium Font"))


(defparameter *default-w-fonts*
   '((fonts:cptfont cptfont-font "standard font")
     (fonts:medfnb medfnb-font "Medium Font Bold"))
   "W fonts are made by converting regular LISP system fonts.  This is a list of fonts
   that will be converted by the CREATE-W-FONTS function")


(defun create-w-fonts (&optional (fonts-to-convert *default-w-fonts*))
  "This function will convert a normal LISP system font to a raster font, suitable for the
   graphics system.  The argument can be a single font description or a list of font
   descriptions.  A font description is a list of the form
     ( FONTS:<symbol name> W:<new symbol name> <printable name string>)"
  
  (if (symbolp (car fonts-to-convert))
    (setq fonts-to-convert (list fonts-to-convert)))
  (dolist (next fonts-to-convert)
    (define-raster-font-internal (car next) (eval (car next))
				 (cadr next) (caddr next))))



#|
;;                Debugging routines
 
(defun pc (char font)
  "Display a w font character using 1's and spaces"
  
  (let* ((array (send (aref (send font :characters) char) :current-cache))
	 (width (array-dimension-n 1 array))
	 (height (array-dimension-n 2 array)))
    (terpri)
    (loop for i from 0 below height
	  do (terpri)
          (loop for j from 0 below width
		for x = (aref array j i)
		do (if (= x 0)
	           (princ " ")
	           (princ "1")))))) 


(defun pc2 (array)
  "Display the contents of a two diensional array with 1's and spaces"
  
  (let* ((width (array-dimension-n 1 array))
	 (height (array-dimension-n 2 array)))
    (terpri)
    (loop for i from 0 below height
	  do (terpri)
          (loop for j from 0 below width
		for x = (aref array j i)
		do (if (= x 0)
	               (princ " ")
	             (princ "1")))))) 


|#
