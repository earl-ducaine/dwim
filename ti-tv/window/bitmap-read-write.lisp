;;; -*- Mode:Common-Lisp; Package:TV; Fonts:(MEDFNT HL12B HL12BI); Base:10 -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change History
;;;
;;;  Date	Author	Description
;;; -------------------------------------------------------------------------------------
;;;   6/3/87       KWW     Changed 'bit to (sheet-array-type-cl default-screen)
;;;   3/03/87  TWE	Undommented out a function which should not have been commented out.  Also
;;;			removed the TV package prefix from symbols in the bitmap display function.
;;; 12/12/86	LGO	Fixed read/write bit-array-file to handle color bitmaps.  Added the x/y parameter.
;;; 12/11/86	TWE	Fixed read-bit-array-file to use its file variable properly.
;;; 12/08/86	LGO	Add id-phrase and pathname defaulting to read/write-bit-array-file
;;; 12/08/86	LGO	Deleted rotate-90, and renamed fast-rotate-90 to rotate-90
;;; 12/01/86	TWE	Changed array referencing to correctly take into account the to-array width.
;;; 11/26/86	TWE	Changed DEFCONSTs to DEFCONSTANTs to conform to Common Lisp.
;;; 11/25/86	TWE	Added the code for bitmap rotations.
;;; 07/29/86	TWE	Changed to use Common Lisp functions.
;;; 07/28/86	TWE	Modified references to the pixel functions to use ARRAY-DIMENSION instead.

#|
This file contains functions which allow one to read/write a bitmap from/to a
file, and to display a bitmap on a window.
|#

(DEFUN mouse-save-image (PATHNAME)
  "Save a rectangular image specified by the mouse to PATHNAME"
  (MULTIPLE-VALUE-BIND (left top right bottom)
      (mouse-specify-rectangle)
    (write-bit-array-file pathname (sheet-screen-array mouse-sheet)
			  (- right left) (- bottom top) left top)))

(DEFCONSTANT bitmap-id-phrase "This is a BitMap file")

(DEFUN WRITE-BIT-ARRAY-FILE (PATHNAME ARRAY &optional
			     (WIDTH (array-dimension array 1))
			     (HEIGHT (array-dimension array 0))
			     (x 0) (y 0))
  "Write a bitmap out to disk so we can read it back fast.
PATHNAME	where to write the bitmap,
ARRAY		the bitmap to be written,
WIDTH,HEIGHT	the dimensions of the bitmap,
X, Y		the upper left hand corner of the image to save."
  (SETQ pathname (fs:merge-and-set-pathname-defaults pathname *default-pathname-defaults* :bitmap))
  (LET (8B-ARRAY
	(real-width width)
	(bits-per-pixel (ARRAY-ELEMENT-SIZE array))
        (AUX-INFO (MAKE-ARRAY 16 :element-type '(unsigned-byte 8)))) ;; Leave room for future info
    ;; Create a byte array holding all the pixels
    (IF (AND (ZEROP x) (= width (array-dimension array 1)))
	 (SETQ 8b-array (MAKE-ARRAY (TRUNCATE (* WIDTH HEIGHT bits-per-pixel) 8)
				    :ELEMENT-TYPE '(UNSIGNED-BYTE 8)
				    :DISPLACED-TO ARRAY
				    :DISPLACED-INDEX-OFFSET (* y width)))
      ;; Real-width is a multiple of 32 bits to ensure rows are word-aligned
      (setq real-width (TRUNCATE (DPB 0 5 (+ (* WIDTH bits-per-pixel) 31.))
				 bits-per-pixel))
      (SETQ 8b-array (MAKE-ARRAY (CEILING (* real-width HEIGHT bits-per-pixel) 8)
				 :ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
      (BITBLT alu-seta width height
	      array x y
	      (MAKE-ARRAY `(,HEIGHT ,real-width)
			  :element-type (sheet-array-type-cl default-screen)
			  :displaced-to 8b-array)
	      0 0))
    (WITH-OPEN-FILE (FSTREAM PATHNAME :DIRECTION :OUTPUT :CHARACTERS NIL :BYTE-SIZE 8.)
      ;; Write an identifying phrase
      (SEND fstream :string-out bitmap-id-phrase)
      ;; Write out the length information in the first elements, then the data.
      (SETF (AREF AUX-INFO 0) (LDB #o0010 WIDTH))
      (SETF (AREF AUX-INFO 1) (LDB #o1010 WIDTH))
      (SETF (AREF AUX-INFO 2) (LDB #o0010 HEIGHT))
      (SETF (AREF AUX-INFO 3) (LDB #o1010 HEIGHT))
      (SETF (AREF AUX-INFO 4) (LDB #o0010 real-width))
      (SETF (AREF AUX-INFO 5) (LDB #o1010 real-width))
      (SETF (AREF aux-info 6) bits-per-pixel)
      (SEND FSTREAM :STRING-OUT AUX-INFO)
      (SEND FSTREAM :STRING-OUT 8B-ARRAY))))

(DEFUN READ-BIT-ARRAY-FILE (PATHNAME)
  "Read in a bit array which is stored on disk.  Three values are returned: bitmap, width and height.
PATHNAME	where the bitmap is located on disk."
  (SETQ pathname (fs:merge-and-set-pathname-defaults pathname *default-pathname-defaults* :bitmap))
  (LET ((AUX-INFO (MAKE-ARRAY 16 :element-type '(unsigned-byte 8)))
        WIDTH
        HEIGHT
	real-width
	ARRAY
	8B-ARRAY
	bits-per-pixel)
    (WITH-OPEN-FILE (FSTREAM PATHNAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 8.)
      ;; Verify that this is a bitmap file.
      (UNLESS (LOOP FOR CHAR BEING THE ARRAY-ELEMENTS OF BITMAP-ID-PHRASE
		    ALWAYS (CHAR= (TYI FSTREAM) CHAR))
	(FERROR "~A is not a BitMap file" (SEND FSTREAM :TRUENAME)))
      ;; Obtain the dimensions of the bitmap from the first 4 bytes of the file.
      (SEND FSTREAM :STRING-IN T AUX-INFO)
      (SETQ WIDTH  (DPB (AREF AUX-INFO 1) (byte 8 8) (AREF AUX-INFO 0)))
      (SETQ height (DPB (AREF AUX-INFO 3) (byte 8 8) (AREF AUX-INFO 2)))
      (SETQ real-width (DPB (AREF AUX-INFO 5) (byte 8 8) (AREF AUX-INFO 4)))
      (SETQ bits-per-pixel (AREF aux-info 6))
      (SETQ ARRAY (MAKE-ARRAY `(,HEIGHT ,real-width) :element-type `(unsigned-byte ,bits-per-pixel))
	    8B-ARRAY (MAKE-ARRAY (CEILING (* real-width HEIGHT bits-per-pixel) 8)
				 :ELEMENT-TYPE '(UNSIGNED-BYTE 8) :DISPLACED-TO ARRAY))
      ;; Read in the contents of the array in one swipe.
      (SEND FSTREAM :STRING-IN T 8B-ARRAY))
    (VALUES ARRAY WIDTH HEIGHT)))

(DEFUN SHOW-BIT-ARRAY (BITMAP &KEY &OPTIONAL (WINDOW NIL)(INCREMENT 100.)
		       &aux alu )
  "Display a bitmap on a window and allow the caller to move the image via the arrow keys.
Returns the (X Y) location of the upper left corner of the image.
BITMAP		the bitmap to be displayed in the window,
WINDOW		where to draw the bitmap
INCREMENT	pixel increment of bitmap movement when the user presses an arrow key."
  ;; Special care is taken to allow one to show a bitmap in the cold-load-stream environment.
  (LET ((IN-COLD-LOAD-STREAM (NOT (BOUNDP 'MAIN-SCREEN))))
    (UNLESS WINDOW
      (IF IN-COLD-LOAD-STREAM
	  (SETQ WINDOW SI:COLD-LOAD-STREAM)
	  ;;ELSE
	  (SETQ WINDOW SELECTED-WINDOW)))
    (setf alu (if (color-system-p window) alu-transp alu-xor))
    (SEND WINDOW :SET-CURSORPOS 0 0)
    (LET* ((ARRAY-WIDTH  (ARRAY-DIMENSION  BITMAP 1))
	   (ARRAY-HEIGHT (ARRAY-DIMENSION  BITMAP 0))
	   (WIDTH        (MIN ARRAY-WIDTH  (IF IN-COLD-LOAD-STREAM
					       (SI:%INSTANCE-REF WINDOW 14)
					       ;;ELSE
					       (SHEET-INSIDE-WIDTH WINDOW))))
	   (HEIGHT       (MIN ARRAY-HEIGHT (IF IN-COLD-LOAD-STREAM
					       808.
					       ;;ELSE
					       (SHEET-INSIDE-HEIGHT WINDOW))))
	   (WINDOW-SCREEN-ARRAY (IF IN-COLD-LOAD-STREAM
				    (SI:%INSTANCE-REF WINDOW 1)
				    ;;ELSE
				    (SHEET-SCREEN-ARRAY WINDOW)))
	   (WINDOW-X-OFFSET (IF IN-COLD-LOAD-STREAM
				0
				;;ELSE
				(SHEET-X-OFFSET WINDOW)))
	   (WINDOW-Y-OFFSET (IF IN-COLD-LOAD-STREAM
				0
				;;ELSE
				(SHEET-Y-OFFSET WINDOW)))
	   (XLOC 0)
	   (YLOC 0)
	   CHAR)
      (LOOP ALWAYS T
	    DO (PROGN (SEND WINDOW :SET-CURSORPOS 0 0)
		      (SEND WINDOW :CLEAR-EOF)
		      (BITBLT ALU WIDTH HEIGHT BITMAP XLOC YLOC WINDOW-SCREEN-ARRAY
			      WINDOW-X-OFFSET WINDOW-Y-OFFSET)
		      (SETQ CHAR (SEND WINDOW :TYI))
		      (COND ((EQUAL CHAR (CHAR-CODE #\UP-ARROW   ))
			     (SETQ YLOC (MIN (- ARRAY-HEIGHT HEIGHT) (+ YLOC INCREMENT))))
			    ((EQUAL CHAR (CHAR-CODE #\DOWN-ARROW ))
			     (SETQ YLOC (MAX 0                       (- YLOC INCREMENT))))
			    ((EQUAL CHAR (CHAR-CODE #\LEFT-ARROW ))
			     (SETQ XLOC (MIN (- ARRAY-WIDTH WIDTH)   (+ XLOC INCREMENT))))
			    ((EQUAL CHAR (CHAR-CODE #\RIGHT-ARROW))
			     (SETQ XLOC (MAX 0                       (- XLOC INCREMENT))))
			    (T
			     (RETURN NIL)))))
      (SEND WINDOW :REFRESH)
      (LIST XLOC YLOC))))

;;; Defined here are three rotation functions which will allow one to rotate
;;; an array in 90 degree increments.  The 270 degree rotation
;;; function is quite slow, and could be improved by about an order of
;;; magnitude with a lot of lisp hacking.  The 180 degree rotation function is
;;; about 2 orders of magnitude faster since it uses BITBLT.  For example, on
;;; a release 2 band, a 754x1024 bit array took the following times to rotate:
;;;
;;;   90 degrees => 12.6  seconds
;;;  180 degrees => 0.321 seconds
;;;  270 degrees => 46.414 seconds
;;;
;;; With this in mind, the 270 degree rotation should be used
;;; sparingly until it is improved.
;;;

(defun validate-rotation-arrays (from-array to-array &optional (check-between-arrays t))
  ;; Perform error checking to make sure that we have good arrays that are compatible for
  ;; a transpose operation.
  (when (or (not (arrayp from-array))
            (not (= (array-rank from-array) 2))
            ;(not (eq (array-element-type from-array) (sheet-array-type-cl default-screen)))
	    )
    (ferror nil "From-array argument, ~d, must be a 2 dimensional array" from-array))
  (when (or (not (arrayp to-array))
            (not (= (array-rank to-array  ) 2))
            ;(not (eq (array-element-type to-array) (sheet-array-type-cl default-screen)))
	    )
    (ferror nil "To-array argument, ~d, must be a 2 dimensional array" to-array))
  (when (and check-between-arrays
             (not (equal (array-dimensions from-array) (nreverse (array-dimensions to-array))))
	     (not (subtypep (array-element-type from-array)(array-element-type to-array))))
    ;; Allow the case where the width of the to-array is a multiple of 32 appropriately
    ;; near the height of the from-array.
    (when (not (= (array-dimension to-array 1)
                  (* 32 (ceiling (array-dimension from-array 0) 32))))
      (ferror nil "Dimensions of ~d and ~d must agree" from-array to-array))))

             ;1 - WIDTH, 0 HEIGHT
(defun rotate-270 (from-array to-array)
  "Rotates a bitmap 90 degrees in the clockwise direction."
  (declare (optimize (speed 3) (safety 0)))
  (validate-rotation-arrays from-array to-array)
  (let ((from-height (array-dimension from-array 0))
        (from-width  (array-dimension from-array 1)))
    (DOTIMES (I from-height)
      (DOTIMES (J from-width)
        (setf (AREF to-array J I) (aref from-array I (- from-width J 1))))))
  to-array)

(defun rotate-180 (from-array to-array)
  "Rotates a bitmap 180 degrees."
  (declare (optimize (speed 3) (safety 0)))
  (validate-rotation-arrays from-array to-array nil)
  (when (not (equal (array-dimensions from-array) (array-dimensions to-array)))
    (ferror nil "Dimensions of ~d and ~d must agree" from-array to-array))
  (let ((from-height (array-dimension from-array 0))
        (from-width  (array-dimension from-array 1)))
    (loop for index from 0 below from-height
          for reverse-index from (1- from-height) downto 0
          do (bitblt alu-seta from-width 1 from-array 0 index to-array 0 reverse-index))))

;;; This rotate function was translated from Smalltalk.  It appeared in the August 1981
;;; issue of Byte magazine.  The array must be square and a power of two bits on a side.
;;; The direction of rotation will be clockwise.  To rotate a 512x512 bit array takes
;;; about 4.5 seconds of solid bitblt time.  Rotate takes 2 + 15*log(N) bitblts.

(defconstant .STOR  alu-seta)
(defconstant .IOR   alu-ior)
(defconstant .AND   alu-and)
(defconstant .XOR   alu-xor)
(defconstant .CLEAR alu-setz)
(defconstant .SET   15)

(defmacro copy-all-to (from xoffset yoffset to alu)
  `(bitblt ,alu (- width ,xoffset) (- width ,yoffset)
           ,from x-base y-base ,to (+ x-base ,xoffset) (+ y-base ,yoffset)))

(defmacro copy-all-from (to xoffset yoffset from alu)
  `(bitblt ,alu (- width ,xoffset) (- width ,yoffset)
           ,from (+ x-base ,xoffset) (+ y-base ,yoffset) ,to x-base y-base))


(defun rotate-square-power-of-two (bitmap width x-base y-base)
  "Rotate BITMAP by 90 degrees clockwise using WIDTH to define a square inside BITMAP.
X-BASE and Y-BASE define the upper left corner of the square."
  (let* ((half-width (truncate width 2))
         (bitmap-width  (array-dimension bitmap 1))
         (bitmap-height (array-dimension bitmap 0))
	 (mask (make-array (list bitmap-height bitmap-width) :element-type (array-element-type bitmap)))
	 (temp (make-array (list bitmap-height bitmap-width) :element-type (array-element-type bitmap))))
    (copy-all-to mask 0 0 mask .CLEAR)
    (copy-all-from mask half-width half-width mask .SET)
    (loop for quadrant = half-width then (truncate quadrant 2)
          when (< quadrant 1)
          return nil
          do
          ;; First exchange left and right halves.
          (copy-all-to   mask   0         0        temp  .STOR)    ; 1
          (copy-all-to   mask   0         quadrant temp   .IOR)    ; 2
          (copy-all-to   bitmap 0         0        temp   .AND)    ; 3
          (copy-all-to   temp   0         0        bitmap .XOR)    ; 4
          (copy-all-from temp   quadrant  0        bitmap .XOR)    ; 5
          (copy-all-from bitmap quadrant  0        bitmap .IOR)    ; 6
          (copy-all-to   temp   quadrant  0        bitmap .XOR)    ; 7
          ;; Then flip the diagonals.
          (copy-all-to   bitmap 0         0        temp  .STOR)    ; 8
          (copy-all-from temp   quadrant  quadrant bitmap .XOR)    ; 9
          (copy-all-to   mask   0         0        temp   .AND)    ; 10
          (copy-all-to   temp   0         0        bitmap .XOR)    ; 11
          (copy-all-to   temp   quadrant  quadrant bitmap .XOR)    ; 12
          ;; Compute the next fine mask.
          (copy-all-from mask   (truncate quadrant 2) (truncate quadrant 2) mask .AND)     ; 13
          (copy-all-to   mask   quadrant  0        mask   .IOR)    ; 14
          (copy-all-to   mask   0         quadrant mask   .IOR))   ; 15
    ;; Tell GC that the temporary arrays can be reclaimed.
    (return-array (prog1 mask (setq mask nil)))
    (return-array (prog1 temp (setq temp nil))))
  bitmap)

(defsubst log2 (number) (truncate (log number) (log 2)))

(defsubst copy-rotate (width from-array from-x from-y to-array to-x to-y)
          (bitblt alu-seta width width from-array from-x from-y to-array to-x to-y)
          (rotate-square-power-of-two to-array width to-x to-y))

(defun rotate-90 (from-array to-array &optional
                       (width  (array-dimension from-array 1))
                       (height (array-dimension from-array 0))
                       (from-x 0) (from-y 0) (to-x 0) (to-y 0))
  "Rotates a bitmap 90 degrees in the clockwise direction."
  (declare (optimize (speed 3) (safety 0)))
  (when (= 0 from-x from-y to-x to-y)
    ;; Only validate upon initial call.  The user shouldn't call this with the from/to x/y's
    ;; set to non-zero values.
    (validate-rotation-arrays from-array to-array))
  (let (
        ;; Biggest power of two which can be used to scan through the from-array.
        (increment (expt 2 (log2 (min width height)))))
    (IF (= width height increment)
        ;; Simple case.  Just use the rotate primitive directly and we are done.
        (copy-rotate width from-array from-x from-y to-array to-x to-y)
        ;;ELSE
          (IF (> width height)
              ;; The increment may go several times down the width.
              (let (last-offset delta-x
                    (delta-y (- height increment)))
                (LOOP FOR offset FROM 0 TO (- width increment) BY increment
                      DO (copy-rotate increment from-array (+ offset from-x) from-y
                                      to-array (+ to-x delta-y) (+ to-y offset))
                      FINALLY (setq last-offset offset))
                (when (not (zerop (setq delta-x (- width last-offset))))
                  ;; There was some part on the right that we couldn't get.  Call us again to
                  ;; rotate it.
                  (rotate-90 from-array to-array delta-x increment
			     (+ from-x last-offset) from-y
			     (+ to-x delta-y) (+ to-y last-offset)))
                (when (not (= height increment))
                  ;; There was some part on the bottom that we couldn't get.  Call
                  ;; us on this too to rotate it.
                  (rotate-90 from-array to-array width delta-y
			     from-x (+ from-y increment)
			     to-x to-y)))
              ;;ELSE
              ;; Do the same thing, only go down the height.
              (let (last-offset
                    (delta-y (mod height increment))
                    (delta-x (- width increment)))
                (LOOP FOR offset FROM 0 TO (- height increment) BY increment
                      FOR new-to-x-offset FROM (- height increment) by (- increment)
                      DO (copy-rotate increment from-array from-x (+ from-y offset)
                                      to-array (+ new-to-x-offset to-x) to-y)
                      FINALLY (setq last-offset offset))
                (when (not (zerop delta-y))
                  ;; There was some part on the bottom that we couldn't get.  Call us again
                  ;; to rotate it.
                  (rotate-90 from-array to-array increment delta-y
			     from-x (+ last-offset from-y)
			     to-x to-y))
                (when (not (= width increment))
                  ;; There was some part on the right that we couldn't get.  Call us on this
                  ;; too to rotate it.
                  (rotate-90 from-array to-array delta-x height
			     (+ from-x increment) from-y
			     to-x (+ to-y increment)))))))
  to-array)
