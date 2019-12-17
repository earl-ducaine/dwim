;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;                           RESTRICTED RIGHTS LEGEND

;Use, duplication, or disclosure by the Government is subject to
;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;Technical Data and Computer Software clause at 52.227-7013.

;
;                     TEXAS INSTRUMENTS INCORPORATED.
;                              P.O. BOX 2909
;                           AUSTIN, TEXAS 78769
;
; Copyright (C) 1984-1989 Texas Instruments Incorporated.  All rights reserved.


;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 04/23/88 KJF            Removed add-initialization due to multiple screen support, and not using
;;;                            convert-to-color anymore.
;;; 01/20/88 KJF            Fixed add-initialization for color to fixup cache-window properly.  See patch-3-26.
;;; 9/15/87  PMH            Added erase-alu fixup to after init method to sprite-window and cache-window
;;; 7/8/87  KWW            Incorporated Ken Fischer's Changes based on his work with GWIN and GED.
;;; 6/16/87 KWW            Changed sheet-array-type-cl since cold load hates it.
;;; 6/3/87   KWW           Changed 'bit to (sheet-array-type-cl default-screen)
;;; 5/20/87  KWW           Added support for color drawing operations
;;; 1/29/87  DLS		Fixed scaled character drawing to work properly.
;;; 11/03/86 TWE		Removed the compile-flavor-methods forms and the load-time initialization of
;;; 10/23/86 TWE		Changed uses of make-pixel-array into make-array.
;;; 10/22/86 TWE		Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 09/17/86 JEB		Moved functions from GWIN: to W:.  Changed cache-window to use the new graphics-mixin,
;;;			rather than the obsolete GWIN:draw-mixin.
;;;

;;;
;;;                FONT
;;
;; This file contains the flavors and methods for fonts used in gwin
;; windows.  The key feature of these fonts is that the characters in
;; them may be drawn at any scale.  They are drawn first to an unexposed
;; window and then bitblted onto the graphics window.  Each character
;; has its own cache array that saves the bitmap of its current
;; scaled version unless the bitmap is too big to fit.  In that case the
;; cache array is left untouched.
;;


(DEFPARAMETER cache-size (* 2 32)
   "The x and y dimensions of the cache array for a character.") 


(DEFVAR cache-height cache-size
   "This is the current character height that will fit in a character cache.
This must be changed whenever the transform for the cache window is changed.
It is kept separately to speed character drawing.") 


(DEFVAR cache-width cache-size
   "This is the current character width that will fit in a character cache.
This must be changed whenever the transform for the cache window is changed.
It is kept separately to speed character drawing.") 


(DEFVAR cache-window-x-scale 1
   "This is the current horizontal scale value for the cache window.
This must be changed whenever the transform for the cache window is changed.
It is kept separately to speed character drawing.") 


(DEFVAR cache-window-y-scale 1
   "This is the current vertical scale value for the cache window.
This must be changed whenever the transform for the cache window is changed.
It is kept separately to speed character drawing.") 


(DEFFLAVOR cache-window ()
           (no-screen-managing-mixin graphics-mixin transform-mixin minimum-window)
  (:default-init-plist
    :bit-array		      (MAKE-ARRAY '(1024 1024) :element-type 'bit)
    :deexposed-typeout-action :permit
    :min-dot-delta            1
    :min-nil-delta            0
    :save-bits                t
    :height                   1024
    :width                    1024)
  (:gettable-instance-variables)
  (:documentation :special-purpose
		  "A flavor which is used by GWIN fonts to create a deexposed window for
temporarily drawing character images in their scaled form.")) 

(DEFMETHOD (cache-window :after :init) (IGNORE)
  "Sets the screen array to match the bit array.  This is just in case
the array is larger than the normal screen array."
 (let* ((array-type (array-type bit-array))
        (pixels-per-word (cond ((eql array-type  'art-1b) 32)
                                ((eql array-type  'art-8b) 4)
                         ))
       )
  (SETQ screen-array	   bit-array
	old-screen-array   bit-array
        erase-aluf (IF (color-system-p self) w:alu-setz w:alu-andca)
	locations-per-line (TRUNCATE (ARRAY-DIMENSION bit-array 1) pixels-per-word))))


(DEFMETHOD (cache-window :after :set-scales) (x-scale &optional (y-scale x-scale))
  "This sets the scale values in the transformation for the cache window.
It also makes sure that any global data that is derived from the transform is
updated."
  (SETQ cache-window-x-scale x-scale
	cache-window-y-scale y-scale)
  (MULTIPLE-VALUE-SETQ (cache-width cache-height)
    (SEND self :untransform-deltas cache-size cache-size)))

(DEFFLAVOR sprite-window ()
           (cache-window)
  (:default-init-plist
    :bit-array (MAKE-ARRAY '(1024 1024) :element-type (tv:sheet-array-type-cl default-screen)))
  (:documentation :special-purpose
		  "A flavor which is used by GWIN to create a deexposed window for
temporarily drawing GWIN objects in raster format."))

(DEFMETHOD (sprite-window :after :init) (ignore)
  (SETF erase-aluf w:alu-back ))


(DEFVAR sprite-window nil
   "A cache window for drawing sprites before bitblting.") 


(DEFVAR cache-window nil
   "The deexposed window which is used by gwin fonts for temporarily drawing
character images in their scaled form.") 


;;; The idea of this function is to delay the creation of anything until the last possible moment.
(DEFUN initialize-sprite-window ()
  (SETQ sprite-window (MAKE-INSTANCE 'sprite-window)   ;;; changed this to 'sprite-window from 'cache-window
        cache-window  (MAKE-INSTANCE 'cache-window)))

;; This is no longer used because we don't do convert-to-color anymore.
;; Now that multiple screens exist, we have to handle these things differently.  04/23/88 KJF
(comment
(add-initialization "Generate 8-bit sprite-window and fixup cache-window."
		    '(PROGN (SETQ sprite-window (MAKE-INSTANCE 'sprite-window))
			    (setf (tv:sheet-locations-per-line cache-window) 256)
                            ;; this cache window is ALWAYS 1 bit, and never exposed, so we
                            ;; can change locations per line to refelect the fact the this is
                            ;; a color screen
                            (SEND cache-window :set-erase-aluf w:alu-setz))
		    :convert-to-color)
 )

(DEFFLAVOR basic-character-mixin
	   ((current-cache	(MAKE-ARRAY `(,cache-size ,cache-size)
                                            :element-type 'BIT))
	    (height             0)
	    (horizontal-spacing nil)
	    (thickness          0)
	    (vertical-spacing   nil)
	    (width              0)
	    (x-scale            0)
	    (y-scale            0)
	    max-x
	    max-y)
	   ()
  :gettable-instance-variables
  (:initable-instance-variables horizontal-spacing thickness vertical-spacing)
  (:documentation :mixin
		  "This contains the common data needed by all types of characters.")) 

(DEFFLAVOR vector-character (x-points y-points)
	   (basic-character-mixin)
   :initable-instance-variables
   :gettable-instance-variables
   (:documentation :combination
    "This is a character which is defined by a set of thick lines.
The representation of a vector character includes two arrays:  one with the x
coordinates, and one with the y coordinates. The origin of the coordinate
system in which the vectors are defined is the upper left corner of the
character box.")) 


(DEFMETHOD (vector-character :draw) (x y window &optional
				     (color (if (color-system-p window)
						(tv:sheet-foreground-color window)
						black))
				     (alu (tv:sheet-char-aluf window)))
  "This draws a vector character at a specified location and scale.
The character cache is first checked to see if it already has a bitmap image
of the correct size character.  If so that image is bitblted into the window.
Otherwise the character is first drawn in the cache and then it is
bitblted into the window.  It is assumed that the scales for the cache window
have been set to the desired drawing scale for this character."
  (UNLESS (NULL color)
    (IF (AND (< (ABS (- cache-window-x-scale x-scale)) 0.01)
	     (< (ABS (- cache-window-y-scale y-scale)) 0.01))
	;;
	;; This is the good case - no transformation is necessary.
	;;
	(SEND window :draw-raster
	      x y current-cache (sheet-inside-left cache-window)
	      (sheet-inside-top cache-window) width height color alu)
	;;
	;; Transformation has changed - redraw the character and save the
	;; new transformation if it fits in the cache.
	;;
	(PROGN
	 (SEND cache-window :clear-screen)
	 (LOOP for xp in x-points
	       for yp in y-points
	       do (SEND cache-window :draw-polyline xp yp thickness color))
	 ;;
	 ;; Check if the transformed version of the character is small
	 ;; enough to fit into the cache.
	 ;;
	 (MULTIPLE-VALUE-BIND (new-width new-height)
	     (SEND cache-window :transform-point max-x max-y)
	   (COND ((AND (< max-x cache-width) (< max-y cache-height))
		  ;;
		  ;; The cache is big enough to hold the new version so set
		  ;; the width and height values for the character which is
		  ;; to be stored in the cache.
		  ;;


		  (SETQ height (1+ new-height)
			width  (1+ new-width))
		  ;;
		  ;; Copy the drawn image of the character into the cache
		  ;; from the cache window array.
		  ;;
;;; 8/27/87 removed install and restore color, used prepare-color-register macro
               (prepare-color-register (window color)
		  (BITBLT alu-seta width height
			  (sheet-screen-array cache-window) 0 0
			  current-cache 0 0)
		  ;;
		  ;; Save the transform for the cached version and copy the
		  ;; character image to the screen.
		  ;;
	       )
		  (SETQ x-scale cache-window-x-scale
			y-scale cache-window-y-scale)
		  (SEND window :draw-raster
			x y current-cache
			(sheet-inside-left cache-window)
			(sheet-inside-top cache-window)
			width height color alu))
		 ;;
		 ;; The cache is too small to hold a copy of the character
		 ;; at the current transformation so just copy it directly
		 ;; from the cache window to the desired window.
		 ;;
		 (t (SEND window :draw-raster
			  x y (sheet-screen-array cache-window)
			  (sheet-inside-left cache-window) (sheet-inside-top cache-window)
			  new-width new-height color alu)))))))) 



(DEFMETHOD (vector-character :fasd-form) ()
  "This returns a form to recreate this vector character when evaluated.
This is the generic message that is sent to an object when it is being written
in compiled form to a file."
  `(MAKE-INSTANCE 'vector-character
		  :horizontal-spacing  ,horizontal-spacing
		  :thickness	        ,thickness
		  :vertical-spacing    ,vertical-spacing
		  :x-points           ',x-points
		  :y-points           ',y-points)) 


(DEFMETHOD (vector-character :after :init) (IGNORE)
  "This initializes all of the internal data for the vector character."
  (MULTIPLE-VALUE-SETQ (max-x max-y)
    (LOOP with (x y)
	  for x-point in x-points
	  for y-point in y-points
	  do (MULTIPLE-VALUE-SETQ (nil nil x y)
	       (polyline-min-max x-point y-point thickness))
       maximize x into maximum-x
       maximize y into maximum-y
       finally (RETURN (VALUES maximum-x maximum-y))))) 


(DEFFLAVOR raster-character
   ((identity-cache  (MAKE-ARRAY `(,cache-size ,cache-size)
				 :element-type 'BIT))
    (identity-width  0)
    (identity-height 0)
    (left-kern       0))
   (basic-character-mixin)
  :initable-instance-variables
  :gettable-instance-variables
   (:documentation :combination
    "This is a character which is defined by a raster bit pattern.
A raster character includes a two-dimensional array of the points which define
the character.")) 


(DEFMETHOD (raster-character :draw) (x y window &optional
				     (color (if (color-system-p window)
						(tv:sheet-foreground-color window)
						black))
				     (alu (tv:sheet-char-aluf window)))
  "This draws a raster character at a specified location and scale.  The
character cache is first checked to see if it already has a bitmap image of
the correct size character.  If so that image is bitblted into the window.
Otherwise the character is first drawn in the cache and then it is bitblted
into the window.  It is assumed that the scales for the cache window have been
set to the desired drawing scale for this character."
  ;; Left-kerns must be adjusted according to the window the text is being *drawn*
  ;; in, not just according to the cache-window scale.
  (LET ((scaled-left-kern (/ (/ left-kern (FLOAT cache-window-x-scale))
			     (SEND window :get-scaling-factors))))
    ;;
    ;; If the color is nil, don't draw anything
    ;;
    (COND ((NULL color))
	  ;;
	  ;; If the desired scale is the identity scale then just draw from
	  ;; the identity cache version.
	  ;;
	  ((AND (< (ABS (- cache-window-x-scale 1)) 0.01)
		(< (ABS (- cache-window-y-scale 1)) 0.01))
	   (SEND window :draw-raster (- x scaled-left-kern) y
		 identity-cache 0 0 identity-width
		 identity-height color alu))
	  ;;
	  ;; If the cache has a copy of the character at the correct scale
	  ;; then just draw from the cache version.
	  ;;
	  ((AND (< (ABS (- cache-window-x-scale x-scale)) 0.01)
		(< (ABS (- cache-window-y-scale y-scale)) 0.01))
	   (SEND window :draw-raster (- x scaled-left-kern)
		 y current-cache 0 0 width height color alu))
	  ;;
	  ;; Transformation has changed - redraw the character and save the
	  ;; new transformation if it fits in the cache.
	  ;;
	  (t (LET ((cache-window-array (sheet-screen-array cache-window)))
	       (MULTIPLE-VALUE-BIND (s-height s-width)
		   (scale-pixel-array-into-window
		    identity-cache identity-height identity-width
		    cache-window-x-scale cache-window-y-scale
		    cache-window cache-window-array)
		 (SEND window :draw-raster
		       (- x scaled-left-kern) y
		       cache-window-array 0 0 s-width
		       s-height color alu)
		 ;;
		 ;; Copy the drawn version of the character to the cache and
		 ;; update the current width, height, max-x and max-x, but
		 ;; only if the character fits in the normal cache size.
		 ;;
		 (COND ((AND (< s-height cache-size) (< s-width cache-size))
			(SETQ width   s-width
			      height  s-height
			      x-scale cache-window-x-scale
			      y-scale cache-window-y-scale)
                        (prepare-color-register (window color)
			  (BITBLT normal width height
				cache-window-array 0 0
				current-cache 0 0)
			)
)))))))) 


(DEFMETHOD (raster-character :fasd-form) ()
  "This returns a form to recreate this raster character when evaluated.
This is the generic message that is sent to an object when it is being written
in compiled form to a file."
  `(MAKE-INSTANCE 'raster-character
		  :thickness         ,thickness
		  :vertical-spacing  ,vertical-spacing
		  :identity-cache   ',identity-cache
		  :identity-width    ,identity-width
		  :identity-height   ,identity-height
		  :left-kern         ,left-kern)) 


(DEFMETHOD (raster-character :after :init) (IGNORE)
  "This initializes all of the internal data for the raster character."
  (SETQ horizontal-spacing identity-width
	max-x              identity-width
	max-y              identity-height)) 


(DEFFLAVOR font   ((characters     (MAKE-ARRAY 256))
		   (horz-spacing   10)
                   (vert-spacing   12)
		   (blinker-width  nil)
		   (blinker-height nil))
	   ()
  :gettable-instance-variables
  :initable-instance-variables
  :settable-instance-variables
  (:documentation :combination
		  "This is the basic font for use in the graphic window system.
The font may be comprised of any type of character objects that provides
support for displaying text at any scale.")) 


(DEFMETHOD (font :character-size) (char-index &optional (create-scale 1))
  "This returns the horizontal and vertical motion for the specified character."
  (DECLARE (VALUES horizontal-step vertical-step))
  (SETQ create-scale (FLOAT create-scale))
  (LET ((CHAR (AREF characters char-index)))
    (COND (CHAR (VALUES (/ (OR (SEND char :horizontal-spacing) horz-spacing)
			   create-scale)
			(/ (OR (SEND char :vertical-spacing) 0)
			   create-scale)))
	  (t	(VALUES 0 0))))) 


(DEFMETHOD (font :draw-character) (char-index x y window
				   &optional (color (if (color-system-p window)
						(tv:sheet-foreground-color window)
						black))
					     (create-scale 1)
					     (alu (tv:sheet-char-aluf window)))
  "This method draws a character from this font in the specifed window.
The returned value is a list of the delta-x and delta-y increments to move the
cursor after drawing this character."

  (DECLARE (VALUES horizontal-increment vertical-increment))
  (SETQ create-scale (FLOAT create-scale))
  (LET ((CHAR (AREF characters char-index))
	bottom h-space left right top sx sy v-space scaled-v-space)
    (COND (CHAR (SETQ h-space	     (/ (OR (SEND char :horizontal-spacing)
					    horz-spacing)
					create-scale)
		      v-space	     (/ (OR (SEND char :vertical-spacing) 0)
					create-scale)
		      scaled-v-space (/ (OR (SEND char :vertical-spacing)
					    vert-spacing)
					create-scale))
		;;
		;; Only draw the character if it is in the window bounds.
		;;
		(MULTIPLE-VALUE-SETQ (sx sy)
		  (SEND window :get-scaling-factors))
		(SEND cache-window :set-scales
		      (/ sx create-scale) (/ sy create-scale))
		(MULTIPLE-VALUE-SETQ (left top right bottom)
		  (SEND window :world-edges))
		(UNLESS (OR (< (+ x h-space) left)	 (> x right)
			    (< (+ y scaled-v-space) top) (> y bottom))
		  (SEND char :draw x y window color alu))
		(VALUES h-space v-space))
	  (t (VALUES 0 0)))))


(DEFMETHOD (font :draw-string) (text x y window
				&optional (color (if (color-system-p window)
						(tv:sheet-foreground-color window)
						black)) (start 0)
					  (end (LENGTH text))
					  (create-scale 1)
					  (alu (tv:sheet-char-aluf window))) 
  "This draws a string of characters from this font in the specifed window.
The returned value is a list of the x and y coordinates to move the cursor
after drawing this character string."
  (DECLARE (VALUES x-position y-position))
  (SETQ create-scale (FLOAT create-scale))
  (LET (bottom char h-space left right sx sy top v-space scaled-v-space)
    (MULTIPLE-VALUE-SETQ (left top right bottom)
      (SEND window :world-edges))
    (MULTIPLE-VALUE-SETQ (sx sy)
      (SEND window :get-scaling-factors))
    (SEND cache-window :set-scales (/ sx create-scale) (/ sy create-scale))
    (DO ((i start (1+ i)))
	((>= i end))
      (WHEN (SETQ char (AREF characters (AREF text i)))
	(SETQ h-space (/ (OR (SEND char :horizontal-spacing) horz-spacing)
			 create-scale)
	      v-space (/ (OR (SEND char :vertical-spacing) 0)
			 create-scale)
	      scaled-v-space (/ (OR (SEND char :vertical-spacing) vert-spacing)
				create-scale))
	(UNLESS (OR (< (+ x h-space) left) (> x right)
		    (< (+ y scaled-v-space) top) (> y bottom))
	  (SEND char :draw x y window color alu))
	(SETQ x (+ x h-space)
	      y (+ y v-space)))))
  (VALUES x y))

		 
(DEFMETHOD (font :fasd-form) ()
  "This returns a form to recreate this font when evaluated.
This is the generic message that is sent to an object when it is being written
in compiled form to a file."
  `(MAKE-INSTANCE 'font
		  :blinker-height  ,blinker-height
		  :blinker-width   ,blinker-width
		  :characters     ',characters
		  :horz-spacing    ,horz-spacing
		  :vert-spacing    ,vert-spacing)) 


(DEFMETHOD (font :after :init) (IGNORE)
  "This initializes all of the internal data for the font."
  (OR blinker-height (SETQ blinker-height vert-spacing))
  (OR blinker-width  (SETQ blinker-width horz-spacing))) 


(DEFMETHOD (font :set-character) (char-index object)
  "This changes the definition of a character within this font."
  (SETF (AREF characters char-index) object)) 
