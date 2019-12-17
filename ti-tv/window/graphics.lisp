;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(medfnt hl12b hl12bi) -*-

;                           RESTRICTED RIGHTS LEGEND

;Use, duplication, or disclosure by the Government is subject to
;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;Technical Data and Computer Software clause at 52.227-7013.
;
;                     TEXAS INSTRUMENTS INCORPORATED.
;                              P.O. BOX 2909
;                           AUSTIN, TEXAS 78769
;
; Copyright (C) 1984- 1989 Texas Instruments Incorporated. All rights reserved.


;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  05/08/89  MAY	Changed (graphics-mixin :draw-filled-circle) to correctly draw center point. SPR 9675.
;;;  			Changed (graphics-mixin :draw-polyline) to correct window patch 4-112
;;;  			which did not consider world coordinates. Also corrected centering of thick line.
;;;  			Also fixed MAC side, at considerable slowdown, for SPR 9447, 8911.
;;;  			Changed (graphics-mixin :draw-filled-rectangle) to correct patch 4-112
;;;  			for world coordinates. Also changed (graphics-mixin :draw-rectangle).
;;;  			Changed (graphics-mixin :draw-filled-polygon) and
;;;  			draw-clipped-solid-triangle to allow w:draw-color-triangle to draw if ucode
;;;  			does not work correctly. Currently the user must provide extra arg in order to
;;;  			take advantage of this. This corrects bad window patch 4.121 which did not consider
;;;  			polygons with inward pointing vertices (even though they are not supported in
;;;  			documentation, they are used by arrow panes in color-map-editor, etc.).
;;;  01/27/89  KJF        [may] Changes to :point for Dual and Multiple Monitor (MMON) users.
;;;  01-09-89  MAY       Rewrote (graphics-mixin :draw-filled-polygon). Fixes spr 8834.
;;;  12-15-88  LG          Added end-point correction to :draw-polyline segments.
;;;  11-15-88  LG         Removed width/height correction in :draw-rectangle now that we are asking the Mac
;;;  		        to draw hollow rectangles correctly.
;;;  10-20-88  LG          Changed all references to mX send-... functions to check for reverse-video and
;;;  		         use (reverse-alu alu) instead of alu if true.
;;;  10-20 88  LG          Used send-draw-shaded-polygon in :draw-regular-polygon when on the mX.
;;;  10-19-88  LG          Rounded DX and DY before zero-tests for line width adjustment in :draw-polyline to
;;;  		         handle floating point values correctly.  Skipped line width adjustments if a Mac
;;;  		         window; they are not needed due to the Mac's graphics model.
;;;  10-16-88  LG          Draw a single filled polygon for a wide line in :draw-polyline.
;;;  08-30-88  MAY       Changed :draw-polyline, :draw-rectangle, and :draw-filled-rectange which drew lines
;;;                         1-pixel too wide if width >1. Fixes SPR 8529 and 8204.
;;;  04/07/88  LG	       Handle :draw-line's and :draw-polyline's COLOR argument 
;;;                                 correctly for the Mac using send-draw-line's new optional COLOR
;;;                                 argument.  Draw thick lines on the Mac using the Explorer's
;;;                                 rotated-rectangle logic rather than the Mac's pen.  Fixup the
;;;                                 end-point correctly on the Mac for a thin line.
;;;  3/15/88 KED           Make line weigth of 0 draw 1 pixel wide line on the Mac. Also adjust
;;;                                 :draw-rectangle on Mac to be a little more like Explorer's. This still
;;;                                 needs work.
;;; 12/1/87  KJF            Do not return error if nil specified for value (color) in :draw-point.  Just do
;;;                            nothing.  Fixes bug # 4246.
;;; 8/26/87  KWW          Changes resulting from code reading:
;;;                            1. Point returns two values, the first a 0 or 1 for compatability with old code, the
;;;                               second value a nil if monochrome, an 8 bit color if working in color
;;;                            2. Replaced install and restore color in draw-raster with prepare-color macro
;;;                            3. made default value for color get the self foreground color in the parameter list.
;;;                            4. Some IFs to WHENs or UNLESSs as appropriate.

;  8/10/87 KWW         Temporary fix of center point of filled circle and filled arc.
;;;                             Its a kludge - just set the center point with a :draw-point. Maybe we need a better
;;;                             Fill algorithm than the triangles???

;;; 7/10/87  KWW          Changed some draw edge arguments so draw-filled-arc and draw-filled-circle work with XOR
;;; 7/7/87   KWW           Modified to take advantage of %draw-rectangle on :draw-filled-rectangle with nil array
;;; 6/25/87  KWW           Fixed boo-boo in draw-polyline concerning line-alu
;;; 6/9/87   KWW           Changed draw-raster to utilize bitblt fully rather than expanding manually
;;; 6/5/87   KWW           25% speed improvement in draw-circle and draw-arc by pulling sin & cos from inside loop
;;; 5/20/87  KWW           Modified for color - added optional fill texture on interior fill type methods
;;;                            Range checking of color is done in the functions provided by primitives.lisp
;;;                            If no color argument is passed as a parameter, then the color is set to the
;;;                             window's current foreground color. DRAW-CLIPPLED-LINE and SOLID-TRIANGLE
;;;                             primitives worry about having the right color in the hardware.
;;;                            Assumes that normal has been defined as using transparency, otherwise the bits left
;;;                             unset by microcode get drawn in the background, making things ugly.
;;; overlapping explanations happened during color merging
;;;  7/9/87   PMH           Added optimizations to :draw-arc :draw-filled-arc :draw-circle draw-filled-circle
;;;  7/6/87   PMH           Fixed :draw-regular-polygon to draw edges correctly
;;; 7/6/87    PMH           Change :draw-string to attempt to translate structure fonts to graphics fonts
;;;  7/6/87   PMH           Fixed draw-filled-arc to take care of some boundary conditions
;;; 7/6/87    PMH           Fixed draw-dashed-line's call to :draw-line
;;;  4/01/87  DAN	          Fixed off-by-one error in :DRAW-FILLED-ARC.
;;;  3/30/87  DAN	          Fixed :draw-regular-polygon to not call DRAW-CLIPPED-SOLID-TRIANGLE if color is NIL.
;;; 01/27/87 JEB		Fixed :draw-raster, which occasionally moved start point out of bounds of the source array.
;;; 01/13/87 JEB		Fixed :point and :draw-point to not adjust positions from outside coordinates to
;;;			inside coordinates, to match the other functions.
;;; 01/08/87 JEB		Fixed :draw-filled-circle to fill the center pixel.
;;; 12/08/86 TWE		Fixed :undraw-picture-list to correct a clipping problem.
;;; 11/10/86 TWE		Removed SPLINE and CURGEN since they are also in STREAM.
;;; 10/30/86 TWE		Changed references to REMAINDER to use the Common Lisp function REM instead.
;;; 10/22/86 TWE		Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 09/10/86 JEB		Changed names of methods from :DRAW-SOLID-POLYGON to :DRAW-FILLED-POLYGON and
;;;			from :DRAW-RECT to :DRAW-RECTANGLE for consistency with the other method names
;;; 09/05/86 JEB		Changed name of mixin from DRAW-MIXIN to GRAPHICS-MIXIN and moved it from GWIN: to W:
;;;

;;;                GRAPHICS
;;
;; OVERVIEW:
;;
;; The flavor GRAPHICS-MIXIN collects together the methods used to draw graphics
;; objects on a window.  Graphics objects are represented in a world-coordinate system
;; which extends infinitely in two dimensions.

;; When a new graphics object is added which is not drawable via a combination of these
;; methods, the method for drawing that object should be added to GRAPHICS-MIXIN.
;;
;;	flavor:	GRAPHICS-MIXIN
;;
;;      methods: :draw-arc
;;               :draw-circle
;;               :draw-line
;;               :draw-polyline
;;               :draw-rectangle
;;               :draw-triangle
;;               :draw-filled-arc
;;               :draw-filled-circle
;;               :draw-filled-polygon
;;               :draw-filled-rectangle
;;               :draw-filled-triangle
;;               :draw-filled-triangle-list
;;               :draw-raster
;;               :draw-string
;;               :point
;;               :draw-point
;;               :draw-dashed-line
;;               :draw-regular-polygon
;;               :draw-cubic-spline
;;               :draw-picture-list
;;               :undraw-picture-list

;;
;; Definition of GRAPHICS-MIXIN and methods for drawing objects with edges
;;
;; The GRAPHICS-MIXIN  flavor collects together functions and methods which actually draw on
;; a window;  MIN-DOT-DELTA and MIN-NIL-DELTA are used in determining whether the
;; drawn object will be big enough to actually be worth drawing.  ALLOW-INTERRUPTS?
;; is a flag that allows the drawing of a picture list to be interruptable.
;;
;; Objects with edges draw themselves by constructing a polyline representation of themselves,
;; then calling DRAW-POLYLINE with this representation. 
;;



(defflavor graphics-mixin ((allow-interrupts? nil)
		       (min-dot-delta 6)
		       (min-nil-delta 2))
      	   (transform-mixin)
   :settable-instance-variables
   (:required-flavors minimum-window)
   (:documentation :mixin
    "Collects together the methods used to draw graphics objects in a window."))

(defmethod (graphics-mixin :draw-arc)
	   (x-center y-center x-start y-start
	    &optional (arc-angle  360)
	              (thickness  1)
	              (color      (if (color-system-p self) (tv:sheet-foreground-color self) black))
                      (alu        normal)
		      (num-points 29)
		      (texture *default-texture*))
  "This method draws a hollow arc.  This is a portion of a circle which has a thickness."
  
  (unless (or (null color)
	      (zerop arc-angle))
    ;;the following calculation was changed to fix up some boundary conditions PMH
    (setq arc-angle (* (signum arc-angle)  
		       (let ((arc-angle (rem (abs arc-angle) 360)))
			 (if (zerop arc-angle)
			     360
			     arc-angle))))
    (let* ((dx          (- x-start x-center))
	   (dy          (- y-start y-center))
	   (radius      (dist x-center y-center x-start y-start))
	   (start-angle (atan dy dx))
	   (delta-angle (quotient (* -1 arc-angle radians-per-degree) num-points))
	   (x-points    (make-array (1+ num-points)))
	   (y-points    (make-array (1+ num-points)))
	   (cos-delta (coerce (cos delta-angle) 'short-float))
	   (sin-delta (coerce (sin delta-angle) 'short-float))
	   (x (coerce (* radius (cos start-angle)) 'short-float))
	   (y (coerce (* radius (sin start-angle)) 'short-float))
	    )
	(do ((i 0 (1+ i)))
	    ((> i num-points))
	  (setf (aref x-points i) (+ x-center x))
	  (setf (aref y-points i) (+ y-center y))
	  (psetq x (- (* x cos-delta) (* y sin-delta))
		 y (+ (* x sin-delta) (* y cos-delta))))
	(when (= (abs arc-angle) 360)
	    (setf (aref x-points num-points) (aref x-points 0))
	    (setf (aref y-points num-points) (aref y-points 0)))
	(send self :draw-polyline
	      x-points y-points thickness color (1+ num-points) alu t texture))))


(defmethod (graphics-mixin :draw-circle)
	   (x-center y-center radius &optional (thickness  1)
	                                       (color      (if (color-system-p self) (tv:sheet-foreground-color self) black))
					       (alu        normal)
					       (num-points 29)
					       (texture *default-texture*))
  "This method draws a hollow circle.  This is a circle which has a thickness."
  
  (unless (null color)
    (IF (mac-window-p self)
	(MULTIPLE-VALUE-BIND (x-radius)
	    (SEND self :transform-point (+ x-center radius) 0)
	  (MULTIPLE-VALUE-BIND (x-center y-center)
	      (send self :transform-point x-center y-center)
	    (MULTIPLE-VALUE-BIND (thickness)
		(send self :transform-deltas thickness thickness)
	      (send-DrawHollowCircle
		(ROUND x-center) (ROUND y-center)
		(ROUND (- x-radius x-center))
		(MAX 1 (ROUND thickness))	       ;KED 3/14/88, Line weight=0 means thickness=1
		color
		(IF (SEND self :reverse-video-p) (reverse-alu alu) alu)
		num-points self))))
      ;; else...
      (let* ((delta-angle	(quotient two-pi num-points))
	     (x-points    (make-array (1+ num-points)))
	     (y-points    (make-array (1+ num-points)))
	     (cos-delta   (coerce(cos delta-angle) 'short-float))      ;FLOATED these two locals PMH 3/1/88
	     (sin-delta   (coerce(sin delta-angle) 'short-float))
	     (x radius)
	     (y 0.0))
	;; set the first and last point for the x and y vectors
	(setf (aref x-points num-points)
	      (setf (aref x-points 0) (+ x-center x)))
	(setf (aref y-points num-points)
	      (setf (aref y-points 0) (+ y-center y)))
	(do ((i 1 (1+ i)))
	    ((>= i num-points))
	  (psetq x (coerce (- (* x cos-delta) (* y sin-delta)) 'short-float)
		 y (coerce (+ (* x sin-delta) (* y cos-delta)) 'short-float))
	  (setf (aref x-points i) (+ x-center x))
	  (setf (aref y-points i) (+ y-center y)))
	(send self :draw-polyline
	      x-points y-points thickness color (1+ num-points) alu t texture)))))



(defmethod (graphics-mixin :draw-line)
	  (from-x from-y to-x to-y &optional (thickness 1)
	   (color     (if (color-system-p self) (tv:sheet-foreground-color self) black))
	   (alu       normal)
	   (draw-end-point t)
	   (texture *default-texture*))
  "this method draws a line with a thickness.
this is actually a rectangle which is rotated and centered along the line."
  
  (unless (null color)
    (IF (AND (mac-window-p self)
	    (> 2 (SEND self :transform-deltas thickness thickness)))
        (MULTIPLE-VALUE-BIND (from-x from-y)
	   (send self :transform-point from-x from-y)
	 (MULTIPLE-VALUE-BIND (to-x to-y)
	     (send self :transform-point to-x to-y)
	   (send-draw-line
	     (ROUND from-x) (ROUND from-y)
	     (ROUND to-x) (ROUND to-y)
	     (IF (SEND self :reverse-video-p) (reverse-alu alu) alu)
	     draw-end-point self t 1 color)
	   (UNLESS draw-end-point			; Turn off end point.
	     (send-draw-point to-x to-y w:alu-xor w:black self))))
      ;; else...
      (let ((x (make-array 2))
	    (y (make-array 2)))
        ;; using initial-contents rather than this more than doubles speed of 140 pixel line !!! KED 3/14/88
        (setf (aref x 0) from-x)   
        (setf (aref y 0) from-y)
        (setf (aref x 1) to-x)
        (setf (aref y 1) to-y)
        (send self :draw-polyline x y thickness color 2 alu draw-end-point texture)))))



(DEFMETHOD (graphics-mixin :draw-polyline)
	   (x-points y-points &optional (thickness  1)
	    (color      (if (color-system-p self) (tv:sheet-foreground-color self) black))
	    (num-points (MIN (LENGTH x-points)
			     (LENGTH y-points)))
	    (alu        normal)
	    (draw-end-point t)                               ;;  pc
	    (texture *default-texture*)
	    &aux  half-thickness ;; may 05/08/89 
	    (closed                                        ;; pc
	      (AND (> num-points 0)
		   (= (AREF x-points 0) (AREF x-points (1- num-points)))
		   (= (AREF y-points 0) (AREF y-points (1- num-points))))))
  "this method draws a polyline with a thickness.
this is actually a sequence of lines which are connected together."
  
  (UNLESS (NULL color)
    (MULTIPLE-VALUE-BIND (dx dy)
	(SEND self :transform-deltas thickness thickness)
      (IF (AND (>= 1.3 dx)
	       (>= 1.3 dy))	  
	  (prepare-sheet (self)
	    (DO ((i 0 (1+ i))
		 (n (  1- num-points))
		 (line-alu (IF (color-system-p self) alu
			       ;;;else
			       (IF (SEND self :reverse-video-p)
				   (reverse-alu alu)
				   alu)))
		 x1
		 y1
		 x2
		 y2)
		((>= i num-points))
	      (SETQ x2 (AREF x-points i)
		    y2 (AREF y-points i))
	      (MULTIPLE-VALUE-SETQ (x2 y2)
		(SEND self :transform-point x2 y2))
	      (UNLESS (NULL x1)
		(IF (mac-window-p self)
		    (PROGN
		      (send-draw-line
			(ROUND x1) (ROUND y1)
			(ROUND x2) (ROUND y2)
			line-alu draw-end-point self t 1 color)
		      (UNLESS (AND (= i n) draw-end-point) ; Turn off end point.
			(send-draw-point (ROUND x2) (ROUND y2) w:alu-xor color self)))
		    ;; else...
		    (draw-clipped-line x1 y1 x2 y2 line-alu (IF (AND (NOT closed) ;; pc
								     (= i (1-  num-points)))
								draw-end-point
								nil)
				       self color texture)))
	      (SETQ x1 x2
		    y1 y2))
	    )
	  (PROGN
	    (SETQ half-thickness (/ (MAX 0 thickness) 2.0)) 	;; may 05/08/89 
	    (multiple-value-bind (pixel-width-x pixel-width-y) 	;; may 05/06/89 
		(send self :get-scaling-factors)		;; may 05/06/89 
	      (prepare-sheet (self)
		(DO ((i    1 (1+ i))
		   (i-1  0 (1+ i-1))
		   (n (  1- num-points))
		   (n-1  (- num-points 2))
		   (dx)  (dy)
		   (x1)  (y1)  (x2)  (y2)  (x3)  (y3)
		   (px1) (py1) (px2) (py2) (px3) (py3) (px4) (py4))
		  ((>= i num-points))
		(SETQ x1 (AREF x-points i-1)
		      y1 (AREF y-points i-1)
		      x2 (AREF x-points i)
		      y2 (AREF y-points i))
		(OR x1 y1 x2 y2 (RETURN ()))
		(IF (AND (mac-window-p self) (> (ABS (- x1 x2)) 1500))
		    (CERROR "continue"
			    "cannot draw a ~d pixel thick line from (~d,~d) to (~d,~d) -- too long."
			    thickness ;;(FLOOR (* 2 thickness)) ;; may 04/13/89 
			    x1 y1 x2 y2)
		    ;; else...
		    (COND
		      ((= i 1)
		       (COND (closed (SETQ x3 (AREF x-points n-1)
					   y3 (AREF y-points n-1)))
			     (t          (SETQ x3 x1
					       y3 y1)))
		       (MULTIPLE-VALUE-SETQ (dx dy)
			 (line-deltas x3 y3 x1 y1 x2 y2 half-thickness)) ;; may 04/13/89
		       (SETQ px1 (- x1 dx)
			     px2 (+ x1 dx))
		       (SETQ py1 (- y1 dy)
			     py2 (+ y1 dy))
;;;		       (UNLESS (mac-window-p self)
;;;			 (UNLESS (ZEROP (ROUND dx)) (IF (< px1 px2) (INCF px1) (INCF px2)));; may 8-30-88
;;;			 (UNLESS (ZEROP (ROUND dy)) (IF (< py1 py2) (INCF py1) (INCF py2)));; may 8-30-88
		       ;; may 05/08/89 
		       ;; Replaced above which subtracted 1 from bottom (largest y or x) 
		       ;; coord with code just below which subtracts .5 pixel from largest and adds .5 to smallest
		       ;; coord. When the line width is computed and the coord for the top
		       ;; and bottom pixel is calculated as : top = x - 1/2 thickness
		       ;; and bottom = x + 1/2 thickness - the number of pixels will include
		       ;; the pixel at x also. For example: line width = 3, half-thickness = 1.5,
		       ;; x = 10, top = (g-round (- 10 1.5)) = 9 and bottom = (g-round (+ 10 1.5)) = 12.
		       ;; which causes the line to be pixels at (9,10,11,12) - 4 pixels instead of 3 !
		       ;;  The solution of 8-30-88 subtracted the extra pixel out but left the
		       ;; line centering incorrect. Subtracting 1/2 pixel from each coord (top & bottom)
		       ;; corrects this problem.
		       ;;  An even line width will have a larger pixel width below
		       ;; the baseline (denoted by "->") than above the baseline. The drawing below
		       ;; shows horizontal pixel line placement for (N=91,2,3,4,5 ...) line widths drawn
		       ;; at the baseline coord. NOTE: g-round will round up any number that is (N + .5)
		       ;; where N is a whole number. ROUND will do the same ONLY if N is ODD !
		       ;; Also included world coordinate conversion of the (.5).
		       ;;      .................. drawn for line width of         5 etc.
		       ;;    .....................drawn for line width of     3 4 5 etc.
		       ;;->.......................drawn for line width of 1 2 3 4 5 etc.
		       ;;   ......................drawn for line width of   2 3 4 5 etc.
		       ;;     ....................drawn for line width of       4 5 etc.	       
;		       (UNLESS (ZEROP (ROUND dx))
;			 (IF (< px1 px2)
;			     (PROGN (INCF px1 .5) (DECF px2 .5))
;			     (PROGN (INCF px2 .5) (DECF px1 .5))))
;		       (UNLESS (ZEROP (ROUND dy))
;			 (IF (< py1 py2)
;			     (PROGN (INCF py1 .5) (DECF py2 .5))
;			     (PROGN (INCF py2 .5) (DECF py1 .5))))
		       (UNLESS (ZEROP (g-ROUND dx))
			 (IF (< px1 px2)
			     (progn (INCF px1 (quotient .5 pixel-width-x)) 	;; may 05/06/89
				    (DECF px2 (quotient .5 pixel-width-x))) 	;; may 05/06/89
			     (progn (INCF px2 (quotient .5 pixel-width-x)) 	;; may 05/06/89 
				    (DECF px1 (quotient .5 pixel-width-x))))) 	;; may 05/06/89 
		       (UNLESS (ZEROP (g-ROUND dy))
			 (IF (< py1 py2)
			     (progn (INCF py1 (quotient .5 pixel-width-y)) 	;; may 05/06/89
				    (DECF py2 (quotient .5 pixel-width-y))) 	;; may 05/06/89
			     (progn (INCF py2 (quotient .5 pixel-width-y)) 	;; may 05/06/89 
				    (DECF py1 (quotient .5 pixel-width-y))))) 	;; may 05/06/89 
		       )
		      (t (SETQ px1 px3
			       py1 py3
			       px2 px4
			       py2 py4)))
		    (COND ((= i n) (COND (closed (SETQ x3 (AREF x-points 1)
						       y3 (AREF y-points 1)))
					 (t      (SETQ x3 x2
						       y3 y2))))
			  (t           (SETQ x3 (AREF x-points (1+ i))
					     y3 (AREF y-points (1+ i)))))
		    (OR x3 y3 (RETURN ()))
		    (MULTIPLE-VALUE-SETQ (dx dy)
		      (line-deltas x1 y1 x2 y2 x3 y3 half-thickness)) ;; may 04/13/89 
		    (SETQ px3 (- x2 dx)
			  px4 (+ x2 dx))
		    (SETQ py3 (- y2 dy)
			  py4 (+ y2 dy))
		    
;		      (UNLESS (mac-window-p self)
;		        (UNLESS (ZEROP (ROUND dx)) (IF (< px3 px4) (INCF px3) (INCF px4)));; may 8-30-88
;		        (UNLESS (ZEROP (ROUND dy)) (IF (< py3 py4) (INCF py3) (INCF py4)));; may 8-30-88
		    ;; may 05/08/89 
		    ;; Replaced above which subtracted 1 from bottom coord with
		    ;; code just below which adjusts BOTH coord's by .5 pixel. Also included world coordinate conversion
		    ;; Used g-round instead of round - made a difference on (wl 1) case for y-scaling of 2. s.b.2 got 3 w/ round
;		    (UNLESS (ZEROP (ROUND dx))
;		      (IF (< px3 px4)
;			  (PROGN (INCF px3 .5) (DECF px4 .5))
;			  (PROGN (INCF px4 .5) (DECF px3 .5))))
;		    (UNLESS (ZEROP (ROUND dy))
;		      (IF (< py3 py4)
;			  (PROGN (INCF py3 .5) (DECF py4 .5))
;			  (PROGN (INCF py4 .5) (DECF py3 .5))))
		    (UNLESS (ZEROP (g-ROUND dx))
		      (IF (< px3 px4)
			  (progn (INCF px3 (quotient .5 pixel-width-x)) 	;; may 05/06/89
				 (DECF px4 (quotient .5 pixel-width-x))) 	;; may 05/06/89
			  (progn (INCF px4 (quotient .5 pixel-width-x)) 	;; may 05/06/89 
				 (DECF px3 (quotient .5 pixel-width-x))))) 	;; may 05/06/89 
		    (UNLESS (ZEROP (g-ROUND dy))
		      (IF (< py3 py4)
			  (progn (INCF py3 (quotient .5 pixel-width-y)) 	;; may 05/06/89
				 (DECF py4 (quotient .5 pixel-width-y))) 	;; may 05/06/89
			  (progn (INCF py4 (quotient .5 pixel-width-y)) 	;; may 05/06/89 
				 (DECF py3 (quotient .5 pixel-width-y))))) 	;; may 05/06/89
		    ;; may 05/08/89 Replaced below takes about 23ms instead of 49-to-129ms
		    ;; but does not do correct line widths
		    ;; and requires the incf and decf stuff with pxn and pyn to be surrounded by
		    ;; (unless (mac-window-p self) ... ) for star polygon tests to look good.
		    ;;  Endpoints are NOT drawn and line widths are wrong by 1 for odd widths.
		    ;; Textures are also not supported.
;;;;		      (IF (mac-window-p self)
;;;;			  (WITH-STACK-LIST (x-coords PX1 PX2 PX4 PX3)
;;;;			    (WITH-STACK-LIST (y-coords PY1 PY2 PY4 PY3)
;;;;			      (send-draw-filled-polygon x-coords y-coords
;;;;							(IF (SEND self :reverse-video-p)
;;;;							    (reverse-alu alu) alu) color self t)))
;;;;		      ;; else... 5 times slower on mac ... but works!
		    (IF (AND  (NOT draw-end-point) (= i n))
			(draw-clipped-solid-triangle px1 py1 px3 py3 px4 py4 self color alu nil nil t texture)
			(draw-clipped-solid-triangle px1 py1 px3 py3 px4 py4 self color alu nil t t texture))
		    (IF (AND (NOT closed) (ZEROP i-1))
			(draw-clipped-solid-triangle px1 py1 px4 py4 px2 py2 self color
						     alu t t t texture)	    
			(draw-clipped-solid-triangle px1 py1 px4 py4 px2 py2 self color
						     alu nil t t texture)))))
	    ))))))

(defmethod (graphics-mixin :draw-rectangle)
	   (left top r-width r-height &optional (thickness 1)
	                                        (color     (if (color-system-p self) (tv:sheet-foreground-color self) black))
						(alu       normal)
						(texture *default-texture*))
  "this method draws a hollow rectangle whose lines have a thickness.
the sides of the rectangle are always orthogonal to the window edges."
  
  (unless (null color)
    (IF (mac-window-p self)
	(MULTIPLE-VALUE-BIND (right bottom)
	    (SEND self :transform-point (+ left r-width) (+ top r-height))
	  (MULTIPLE-VALUE-BIND (left top)
	      (SEND self :transform-point left top)
	    (LET ((thickness (MAX 1 (ROUND (send self :transform-deltas thickness thickness)))))       ;KED 3/14/88, Line weight=0 means thickness=1
	      (send-DrawHollowRectangle
		(ROUND left) (ROUND top)
		(ROUND (- right left))
		(ROUND  (- bottom top))
		thickness
		color
		(IF (SEND self :reverse-video-p) (reverse-alu alu) alu)
		self))))
	;;; else...
	(multiple-value-bind (pixel-width-x pixel-width-y) 	;; may 05/06/89 
	    (send self :get-scaling-factors)			;; may 05/06/89 
	  (let* (;(right  (+ left (- r-width 1)))	;; may 8-30-88
		 ;(bottom (+ top (- r-height 1)))	;; may 8-30-88
		 ;; subtract 1 in world coordinates (not screen) to get width/height correct
		 (right  (+ left (- r-width (quotient 1.0 pixel-width-x))))
		 (bottom  (+ top (- r-height (quotient 1.0 pixel-width-y))))
		 (x      (make-array 5))
		 (y      (make-array 5)))
	    (setf (aref x 0) left)
	    (setf (aref y 0) top)
	    (setf (aref x 1) right)
	    (setf (aref y 1) top)
	    (setf (aref x 2) right)
	    (setf (aref y 2) bottom)
	    (setf (aref x 3) left)
	    (setf (aref y 3) bottom)
	    (setf (aref x 4) left)
	    (setf (aref y 4) top)
	    (send self :draw-polyline x y thickness color 5 alu t
		  texture)))) ))


(defmethod (graphics-mixin :draw-triangle)
	   (x1 y1 x2 y2 x3 y3 &optional (thickness 1)
	                                (color     (if (color-system-p self) (tv:sheet-foreground-color self) black))
					(alu       normal)
					(texture *default-texture*))
    "this method draws a hollow triangle whose lines have a thickness."
  
  (unless (null color)

    (let ((x (make-array 4))
	  (y (make-array 4)))
      (setf (aref x 0) x1)
      (setf (aref y 0) y1)
      (setf (aref x 1) x2)
      (setf (aref y 1) y2)
      (setf (aref x 2) x3)
      (setf (aref y 2) y3)
      (setf (aref x 3) x1)
      (setf (aref y 3) y1)
      (send self :draw-polyline x y thickness color 4 alu t texture))))

;;
;; clipped-solid-triangle objects
;;
;; these are the methods for drawing objects which have no thickness; they are drawn as
;; a series of filled triangles.
;;


(defmethod (graphics-mixin :draw-filled-arc)
	   (x-center y-center x-start y-start
	    &optional (arc-angle  360)
	              (color      (if (color-system-p self) (tv:sheet-foreground-color self) black))
		      (alu        normal)
                      (num-points 29)
		      (draw-edge  t)
		      (texture *default-texture*))
  "this method draws a solid, filled in arc.
this is a sector of a circle which is filled in between the center and circumference."
  
  (unless (or (null color)
	      (zerop arc-angle))	   ;exit if no angle PMH
    ;;the following calculation was changed to fix up some boundary conditions PMH
    (setq arc-angle (* (signum arc-angle)  
		       (let ((arc-angle (rem (abs arc-angle) 360)))
			 (if (zerop arc-angle)
			     360
			     arc-angle))))
      (prepare-sheet (self)
	 (let* ((delta-angle (quotient (* -1 arc-angle radians-per-degree) num-points))
		(cos-delta (coerce (cos delta-angle) 'short-float))  ;coerced these guys PMH
		(sin-delta (coerce (sin delta-angle) 'short-float))
		(dx (- x-start x-center))
		(dy (- y-start y-center))
		x2 y2)
	   (do ((i 1 (1+ i))
		(x1 x-start x2)
		(y1 y-start y2)
		)
	       ((> i num-points))
	     (psetq dx (- (* dx cos-delta) (* dy sin-delta))
		    dy (+ (* dx sin-delta) (* dy cos-delta)))
	     (setq x2 (+ x-center dx)
		   y2 (+ y-center dy))
	     (draw-clipped-solid-triangle
	       x-center y-center x1 y1 x2 y2 self color alu
	       (if (and (= i num-points) (/= 360 (abs arc-angle))) t nil)
	       draw-edge t texture)))
	 ;; draw the center point
	 (SEND self :draw-filled-rectangle x-center y-center 0 0 color alu t texture)
	 )))

(defmethod (graphics-mixin :draw-filled-circle)
	   (x-center y-center radius &optional (color (if (color-system-p self)
							  (tv:sheet-foreground-color self)
							  black))
	                                       (alu         normal)
					       (num-points  29)
					       (draw-edge   t)
					       (texture *default-texture*))
  "This method draws a solid, filled in circle."
  (unless (null color)
     (IF (mac-window-p self)
           (MULTIPLE-VALUE-BIND (x-radius)
                (SEND self :transform-point (+ x-center radius) 0)
              (MULTIPLE-VALUE-BIND (x-center y-center)
                   (SEND self :transform-point x-center y-center)
                 (send-DrawCircle
                    (ROUND x-center) (ROUND y-center)
                    (ROUND (- x-radius x-center)) color
		    (IF (SEND self :reverse-video-p) (reverse-alu alu) alu)
		    num-points draw-edge self)))
      ;; else...
      (prepare-sheet (self)
	(let* ((delta-angle (quotient two-pi num-points))
	       (cos-delta (coerce (cos delta-angle) 'short-float))
	       (sin-delta (coerce (sin delta-angle) 'short-float))
	       (dx radius)
	       (dy 0.0)
	       (x-start (+ dx x-center))
	       (y-start (+ dy y-center))
	       x2 y2)
	  (do ((i 1 (1+ i))
	       (x1 x-start x2)
	       (y1 y-start y2))
	      ((>= i num-points)
	       (draw-clipped-solid-triangle
		 x-center y-center x-start y-start x1 y1 self color alu
		 nil draw-edge t texture))
	    (psetq dx (- (* dx cos-delta) (* dy sin-delta))
		   dy (+ (* dx sin-delta) (* dy cos-delta)))
	    (setq x2 (+ x-center dx)
		  y2 (+ y-center dy))
	    (draw-clipped-solid-triangle
	      x-center y-center x2 y2 x1 y1 self color alu
	      nil draw-edge t texture)))
	;; draw the center point
;	(SEND self :draw-filled-rectangle x-center y-center 0 0 color alu t texture) ;; may 05/08/89 
	;; may 05/08/89 A :draw-filled-rectangle is not correct since the point not yet drawn is ALWAYS a single point
	;; even if the scaling factors are not both 1. Below is the equaivalent of draw-point with scaling factors
	;; bound temporarily to 1:1 AND provides a texture arg to :draw-point which has none.
	(multiple-value-bind (x-trans y-trans) 				;; may 05/08/89 
	    (send self :transform-point x-center y-center)		;; may 05/08/89 
	  (bind (locf identity?) t)					;; may 05/08/89 
	  (send self :draw-point x-trans y-trans alu			;; may 05/08/89 
		;; texture arg is not supported in :draw-point so kludge it here ..	;; may 05/08/89 
		(aref texture (mod y-trans 32.) (mod x-trans 32.))))	;; may 05/08/89
	))))


;; may 01-9-89. 
;; Rewrote algorithm to ignore center points which were unnecessary.
;; If the center points were outside the polygon, of course the old
;; method then did use certer points but it drew using them even if
;; the center was OUTSIDE the polygon. 
;; Old algorithm drew triangles from x & y-points to center points
;; New algorithm draws triangles from x & y-points only -- does NOT require
;; that the x & y-point arrays have the first and last values indentical,
;; but ignores them if they are. Fixes SPR 8834.
;;
(defmethod (graphics-mixin :draw-filled-polygon)
	   (x-center y-center x-points y-points
	    &optional (color      (if (color-system-p self) (tv:sheet-foreground-color self) black))
                      (num-points (MIN (length x-points) (length y-points)))
		      (alu        normal)
		      (texture *default-texture*))
  "This method draws a solid, filled in convex polygon.
this is a solid textured polygon which has no inward pointing vertices.
THE X-CENTER AND Y-CENTER POINTS ARE IGNORED. The polygon is created
by drawing triangles using the set of x & y-points in the order given
therefore at least 3 x & y-points must be supplied. The last edge drawn
from the first point to the last point is drawn automatically, therefore
if the last set of x & y-points are the same as the first set, they are
ignored."
  (DECLARE (IGNORE x-center y-center))
  (unless (null color)
    (prepare-sheet (self)
      (PROG ((x1) (y1)
	     (x2) (y2)
	     (x3) (y3))
	    (OR (>= num-points 3) (RETURN nil))
	    (SETQ x1 (AREF x-points 0)
		  y1 (AREF y-points 0)
		  x3 (AREF x-points 1)
		  y3 (AREF y-points 1))	    
	    ;; ignore last points in arrays if identical to first points
	    (WHEN (AND (= x1 (AREF x-points (1- (length x-points))))
		       (= y1 (AREF y-points (1- (length y-points)))))
	      (DECF num-points))
	    (do ((i 2 (1+ i)))
		((>= i num-points))
	      (setq x2 x3
		    y2 y3)
	      (setq x3 (aref x-points i)
		    y3 (aref y-points i))
	      (or x3 y3 x2 y2 x1 y1 (return ()))
	        ;;; added texture argument
	      (draw-clipped-solid-triangle x1 y1 x2 y2 x3 y3 self
					   color alu (IF (= i (1- num-points)) t nil) t t texture))))))
;;; old algorithm :
;(defmethod (graphics-mixin :draw-filled-polygon)
;	   (x-center y-center x-points y-points
;	    &optional (color      (if (color-system-p self) (tv:sheet-foreground-color self) black))
;                      (num-points (length x-points))
;		      (alu        normal)
;		      (texture *default-texture*))
;  "this method draws a solid, filled in convex polygon.
;this is a polygon which has no inward pointing vertices and which is solid from the
;center point to the edges between the vertices."
  
;  (unless (null color)

;    (prepare-sheet (self)
;       (do ((i 1 (1+ i))
;	    (x1)
;	    (y1)
;	    (x2)
;	    (y2))
;	   ((>= i num-points))
;	 (cond
;	   ((= i 1) (setq x1 (aref x-points 0)
;			  y1 (aref y-points 0))
;		    (or x1 y1 (return ())))
;	   (t (setq x1 x2
;		    y1 y2)))
;	 (setq x2 (aref x-points i)
;	       y2 (aref y-points i))
;	 (or x2 y2 (return ()))
;	 ;;; added texture argument
;	 (draw-clipped-solid-triangle x1 y1 x2 y2 x-center y-center self
;				      color alu () t t texture)))))


;;; modified to take advantage of %draw-rectangle when source array is nil
(defmethod (graphics-mixin :draw-filled-rectangle)
	   (left top r-width r-height &optional (color     (if (color-system-p self)
							       (tv:sheet-foreground-color self) black))
	                                    (alu       normal)
					    (draw-edge t)
					    (texture *default-texture*))
   "this method draws a solid, filled in rectangle.
the sides of the rectangle are always orthogonal to the coordinate axes."
  
  (unless (null color)
    (IF (mac-window-p self)
	(MULTIPLE-VALUE-BIND (right bottom)
	    (SEND self :transform-point (+ left r-width) (+ top r-height))
	  (MULTIPLE-VALUE-BIND (left top)
	      (SEND self :transform-point left top)
	    (send-draw-rectangle
	      (ROUND (- right left)) (ROUND (- bottom top))
	      (ROUND left) (ROUND top) color
	      (IF (SEND self :reverse-video-p) (reverse-alu alu) alu) self t)))
	;; else...
	(multiple-value-bind (pixel-width-x pixel-width-y) 	;; may 05/06/89 
	    (send self :get-scaling-factors)			;; may 05/06/89 
	  (let (;(bottom (+ top (1- r-height))) 	;; may 8-30-88
		;(right  (+ left (1- r-width)))) 	;; may 8-30-88
		;; subtract 1 in world coordinates (not screen) to get width/height correct
		(bottom  (+ top (- r-height (quotient 1.0 pixel-width-y)))) ;; may 05/06/89 
		(right  (+ left (- r-width (quotient 1.0 pixel-width-x))))) ;; may 05/06/89 
	    (prepare-sheet (self)
	      (draw-clipped-solid-triangle left top left bottom right bottom self color alu
					   () draw-edge draw-edge texture)
	      (draw-clipped-solid-triangle left top right top right bottom self color alu
					   t draw-edge draw-edge texture))
	    )))))

(defmethod (graphics-mixin :draw-filled-triangle)
	   (x1 y1 x2 y2 x3 y3 &optional (color   (if (color-system-p self)
						     (tv:sheet-foreground-color self) black))
	                                (alu              normal)
	                                (draw-third-edge  nil)
                                        (draw-second-edge t)
					(draw-first-edge  t)
					(texture *default-texture*))
  "this function draws a solid, filled in, triangle."
  (unless (null color)
    (prepare-sheet (self)
;;; added texture argument
       (draw-clipped-solid-triangle x1 y1 x2 y2 x3 y3 self color alu
				    draw-third-edge
				    draw-second-edge
				    draw-first-edge
				    texture))))



(defmethod (graphics-mixin :draw-filled-triangle-list)
	   (triangle-list &optional (color (if (color-system-p self)
						     (tv:sheet-foreground-color self) black))
	                            (alu   normal)  ;;; change alu-ior to normal
				    (texture *default-texture*))
  "this method draw a set of solid, filled in, triangles.
the verticies are specified as six element lists which are the elements of the
triangle-list parameter."


;;; added texture parameter to solid-triangle...
  (prepare-sheet (self)
     (dolist (vertices (butlast triangle-list))
       (draw-clipped-solid-triangle (first vertices) (second vertices)
				    (third vertices) (fourth vertices)
				    (fifth vertices) (sixth vertices)
				    self color alu () () t texture))
     (let ((vertices (car (last triangle-list))))
       (draw-clipped-solid-triangle (first vertices) (second vertices)
				    (third vertices) (fourth vertices)
				    (fifth vertices) (sixth vertices)
				    self color alu () () () texture))))

;;
;; the rest of the methods.
;;
;; raster and text objects are handled specially, draw-picture-list and
;; undraw-picture-list tell objects in a list to draw/undraw themselves
;; unless the object's screen size is too small to waste time on, in which
;; case a blob is drawn.
;;


(defmethod (graphics-mixin :draw-raster) (x y raster start-x start-y r-width r-height
					  &optional (color (if (color-system-p self)
						     (tv:sheet-foreground-color self) black))
					  (alu   normal))
  "this method draws a raster image in the window by copying the pixel data.
this is just like a bitblt operation except that the starting location is in world
coordinates and the image is clipped to the window edges."
  
  (unless (null color)
    (multiple-value-setq (x y)
      (send self :transform-point x y))
    
    (let ((left (sheet-inside-left))
	  (top (sheet-inside-top)))
      (if (< x left)
	  (setq start-x (- left x)
		r-width   (- r-width start-x)
		x       left))
      (if (< y top)
	  (setq start-y (- top y)
		r-height  (- r-height start-y)
		y       top)))
    (setq start-x (mod start-x (array-dimension raster 1)) ; Be sure start point is in bounds.
	  start-y (mod start-y (array-dimension raster 0)))	   ; 1/27/87  JEB
    (setq r-width (min r-width   (max 0 (- (sheet-inside-right) x)))
	  r-height (min r-height (max 0 (- (sheet-inside-bottom) y))))
;;; got rid of install color and restore color, used prepare-color macro
    (tv:prepare-color (self color)
    (prepare-sheet (self)
      (if (and (plusp r-width) (plusp r-height))
	    (bitblt alu r-width r-height raster start-x start-y screen-array x y)
	    ))))
  )

      
(defmethod (graphics-mixin :draw-string)
	   (font text x y
	    &optional (color (if (color-system-p self)
						     (tv:sheet-foreground-color self) black) )
	    (left 0) (tab 8) (scale 1) (alu char-aluf))
  "this method draws a string of text in the specified font."


  (declare (values x-position y-position))

  (setq scale (float scale))
  (let ((h-spacing (quotient (send font :horz-spacing) scale))
	(limit     (length text))
	(v-spacing (quotient (send font :vert-spacing) scale)))
    (cond
      ((= limit 1)
       (let ((char (character text)))
	 (case char
	   (#\backspace (setq x (- x h-spacing)))
	   (#\linefeed  (setq y (+ y v-spacing)))
	   (#\newline   (setq x left
			      y (+ y v-spacing)))
	   (#\tab	(let ((tb (* h-spacing tab)))
	                  (setq x (* (quotient (g-round (+ x tb))
					       (g-round tb)) tb))))
	   (otherwise	(multiple-value-bind (dx dy)
	                    (send font :draw-character
				  char x y self color scale alu)
	                  (setq x (+ x dx)
		                y (+ y dy)))))))
      (t (setq text (string text))
       (do ((from 0 to)
	    (to   (or (string-search-set special-chars text) limit)
	          (or (string-search-set special-chars text to) limit)))
	   ((>= from limit))
	 (multiple-value-setq (x y)
	   (send font :draw-string text x y self color from to scale alu))
	 (do ()
	     ((or (>= to limit)
		  (not (member (aref text to) special-chars :test #'eq))))
	   (case (aref text to)
	     (#\backspace (setq x (- x h-spacing)))
	     (#\linefeed  (setq y (+ y v-spacing)))
	     (#\newline   (setq x left
			        y (+ y v-spacing)))
	     (#\tab	  (let ((tb (* h-spacing tab)))
		            (setq x (* (quotient (g-round (+ x tb))
						 (g-round tb)) tb)))))
	   (setq to (1+ to))))))
    (values x y)))

(DEFMETHOD (graphics-mixin :point)
	   (x y)
  "This method returns the value of the pixel at the window location
that corresponds to the world coordinates specified by X and Y.  Three
values are returned.  Values 0, NIL, NIL are returned for locations not
inside the window.  Otherwise, the first value is a 0 or 1.  For
monochrome screens, this is the value in the SCREEN-ARRAY.  For color
screens, this is 0 if the pixel value is the background color, and 1
otherwise.  The second value is the color of the pixel.  This is NIL for
monochrome screens.  For 8-bit color screens, this the value in the
SCREEN-ARRAY.  For color screens in dual-monitor mode, this is the value
in the SCREEN-ARRAY ANDed with the screen's PLANE-MASK.  The third value
is always the value in the SCREEN-ARRAY."
  (MULTIPLE-VALUE-SETQ (x y)
    (SEND self :transform-point x y))
  (IF (OR (< x (sheet-inside-left))
	  (>= x (sheet-inside-right))
	  (< y (sheet-inside-top))
	  (>= y (sheet-inside-bottom)))
      ;; we are off the screen - 0 for
      ;; compatability, nil for 8 bit system
      (VALUES 0 nil nil)
      ;; ELSE
      (LET (bit-value color-value screen-array-value)
	(SETF screen-array-value (prepare-sheet (self) (AREF screen-array y x)))   ; grab the screen contents
	(IF (tv:color-sheet-p self)	   ; was (= 8 (tv:screen-bits-per-pixel tv:default-screen)) KJF 09/27/88
	    (PROGN
	      ;; If Dual Monitors and we're working with a color sheet, return 7-bit value and 8-bit value.
	      ;; For Dual and Multiple Monitor (MMON) users.  09/27/88 KJF.
	      (SETF color-value
		    (LET ((screens-plane-mask (tv:sheet-plane-mask (tv:get-screen self))))
		      (IF (EQ screens-plane-mask tv:*default-plane-mask*)
			  screen-array-value
			  ;; ELSE, in dual-monitor mode, so remove the monochrome-screen bit from the color-value
			  (LOGAND screen-array-value screens-plane-mask)))
		    bit-value
		    (IF (EQL color-value (tv:sheet-background-color self))
			0
			1))
	      (VALUES bit-value color-value screen-array-value))
	    ;; else its a 1 bit/pixel system, so we just return the value read from the array, and nil for the color value
	    (VALUES screen-array-value nil screen-array-value)))))


(defmethod (graphics-mixin :draw-point)
	   (x y &optional (alu char-aluf)
	    (value (IF (color-system-p self) (tv:sheet-foreground-color self) -1 )))
  "this method draws a pixel at the window location that corresponds to the world coordinates
specified by x and y.  The new value is combined with the existing value, based on the alu.  The
monochrome default -1 is a pixel with its bits set to 1. The color default is the window's foreground color."
  
;;; note - new alus are added for color. Since these alus are not understood by the boole function, they must be
;;; seperated out and handled properly.
  
  (UNLESS (NULL value) ;; do not error off if nil for value (color); do like other primitives 12/1/87 KJF
    (multiple-value-setq (x y)
      (send self :transform-point x y))
    (IF (mac-window-p self)
	(send-draw-point x y (IF (SEND self :reverse-video-p) (reverse-alu alu) alu) value self)
      ;; else...
      (or (< x (sheet-inside-left)) (>= x (sheet-inside-right)) (< y (sheet-inside-top))
	  (>= y (sheet-inside-bottom))
	  (prepare-sheet (self)
	    (LET* ((current (AREF screen-array y x))
		   (color-map (tv:sheet-color-map self))
		   (background (tv:sheet-background-color self))
		   )
	      ;; a little cosmetit fix up here.  PMH 4/6/88
	      (SETF (AREF screen-array y x)
		(COND
		  ((= alu tv:alu-back)   background)
		  ((= alu tv:alu-transp) value)
		  ((= alu tv:alu-add)    (MOD (+ value current) 256))
		  ((= alu tv:alu-sub)    (MOD (- current value) 256))
		  ((= alu tv:alu-max)    (MAX value current))
		  ((= alu tv:alu-min)    (MIN value current))
		  ((= alu tv:alu-avg)    (TRUNCATE (+ value current) 2))
		  ((= alu tv:alu-adds)   (MIN (+ value current) (tv:color-map-saturate color-map)))
		  ((= alu tv:alu-subc)   (MAX (- current value) (tv:color-map-clamp color-map)))
		  (t                     (BOOLE alu value current))
		  ))
	      ))))))

(defmethod (graphics-mixin :draw-dashed-line)
	   (x0 y0 x1 y1 &optional (thickness 1)
	                          (color (if (color-system-p self)
					  (tv:sheet-foreground-color self) black) )
     				  (alu normal)
                        	  (dash-spacing 20)
				  space-literally-p
				  (offset 0)
                                  (dash-length (floor dash-spacing 2))
				  (texture *default-texture*))
  "this method uses :draw-line repeatedly to create a dashed line.  Dash-spacing is the number of
pixels from the start of one dash to the start of the next.  Space-literally-p set to nil allows
dash-spacing to be adjusted to fit evenly.  Offset is the number of pixels at the endds of the line
before the first dash starts."

  (let (n-dashes distance
	(real-dash-spacing dash-spacing)
	(real-dash-length dash-length)
	(meth (get-handler-for self :draw-line)))
    (setq distance (sqrt (small-float (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2)))))
    (if (zerop distance)
	;; take care of the case where the two (x,y) points specify the
	;; same point.
      	(send self :draw-point x0 y0 alu)
       (progn
	  (if space-literally-p
	      ;; get number of dashes of specified size that will fit.
       	      (setq n-dashes (floor (+ distance (- dash-spacing dash-length)) dash-spacing))
	      ;; get approximate number of dashes that will fit,
	      ;; then change spacing to make them fit exactly.
              (progn             
	        (setq n-dashes (round (+ distance (- dash-spacing dash-length)) dash-spacing))
	        (if (= n-dashes 1)
		    (setq real-dash-spacing distance real-dash-length (- distance offset offset))
		    (setq real-dash-spacing
			  (quotient (- distance offset offset dash-length) (1- n-dashes))))))
	  (let ((x (+ x0 (* offset (quotient (- x1 x0) distance))))
		(y (+ y0 (* offset (quotient (- y1 y0) distance))))
		(dx (* real-dash-length (quotient (- x1 x0) distance)))
		(dy (* real-dash-length (quotient (- y1 y0) distance)))
		(dx2 (* real-dash-spacing (quotient (- x1 x0) distance)))
		(dy2 (* real-dash-spacing (quotient (- y1 y0) distance))))
	    (dotimes (i n-dashes)
	      (funcall meth :draw-line (values (round x)) (values (round y))
		       (values (round (+ x dx))) (values (round (+ y dy))) thickness color 
		       alu
		       t texture)
	      (incf x dx2)
	      (incf y dy2)))))))
	

(defmethod (graphics-mixin :draw-regular-polygon) (x1 y1 x2 y2 n
					       &optional (color (if (color-system-p self)
								    (tv:sheet-foreground-color self) black))
							 (alu normal)
						   	 (draw-edge t)
							 (texture *default-texture*))
  "Given an edge and a number of sides, draw something.
the sign of n determines which side of the line the figure is drawn on.
if the line is horizontal, the rest of the polygon is in the positive
direction when n is positive."

  (if (<= 3 (abs n))			   ;must be a sensible polygon PMH
      (progn
	(prepare-sheet (self)
	(let* ((theta (* 3.14159 (1- (/ 2.0 n))))
	       (sin-theta (coerce (sin theta) 'short-float))  ; coerce these guys  PHM 3/10/88
	       (cos-theta (coerce (cos theta) 'short-float))
	       (n (abs n))
	       (first-time 0)
	       (last-time (- n 3))
	       (x0 x1)
	       (y0 y1)
	       (x3)
	       (y3)
	       (x-coords (LIST x2 x1))
	       (y-coords (LIST y2 y1)))
	  ;; this code looks ungly since there are so many exceptions
	  ;; I tried to clean it up some  PMH
	  (dotimes (i (- n 2))
	    (setf x3
		  (+
		    (- (- (* x1 cos-theta) (* y1 sin-theta)) (* x2 (1- cos-theta)))
		    (* y2 sin-theta))
		  y3
		  (- (- (+ (* x1 sin-theta) (* y1 cos-theta)) (* x2 sin-theta))
		     (* y2 (1- cos-theta))))
	    (IF (mac-window-p self)
		(PROGN
		  (PUSH x3 x-coords)
		  (PUSH y3 y-coords))
	      ;; else...
	      (WHEN color
		(select i
		  ;;draw the two outer edges for the first triangle
		  (first-time (draw-clipped-solid-triangle x0 y0 x2 y2 x3 y3 self color
							   alu
							   ;; special check for triangles PMH
							   (if (= n 3) draw-edge)
							   draw-edge draw-edge texture))
		  ;;Draw the two outer edges for the final triangle
		  (last-time (draw-clipped-solid-triangle x0 y0 x2 y2 x3 y3 self color
							  alu draw-edge draw-edge t texture))
		  ;;only draw the outer edge for the inner triangles
		  (otherwise (draw-clipped-solid-triangle x0 y0 x2 y2 x3 y3 self color
							  alu () draw-edge t texture)))))
	    (setq x1 x2 y1 y2 x2 x3 y2 y3))
	  (WHEN (mac-window-p self)
	    (send-draw-filled-polygon
	      (NREVERSE x-coords) (NREVERSE y-coords)
	      (IF (SEND self :reverse-video-p) (reverse-alu alu) alu) color self t))
	  )))))

(defmethod (graphics-mixin :draw-cubic-spline)
	   (px py z &optional (thickness 1)
			      (color (if (color-system-p self)
					 (tv:sheet-foreground-color self) black))
			      (alu normal)
			      (c1 ':relaxed)
			      (c2 c1)
			      p1-prime-x p1-prime-y pn-prime-x  pn-prime-y
			      (texture *default-texture*))
  "this method draws a cubic spline curve (a smooth curve) through the sequence of points."

  (multiple-value-bind (cx cy i)
      (spline px py z nil nil c1 c2
	      p1-prime-x p1-prime-y pn-prime-x pn-prime-y)
      (send self :draw-polyline cx cy thickness color i alu t texture)))



(defmethod (graphics-mixin :draw-picture-list) (items &optional (world nil))
  "This method draws a list of graphic entities in the window.
clipping is performed on each item and a check is done to see if the item is too small to
bother drawing in detail.  T is returned if the drawing is not interrupted,
otherwise nil."
  
  (declare (values drawn-without-interruption?))
  (let (botm
	delta
	left
	right
	top
	x
	x1
	y
	y1)
    (multiple-value-setq (left top right botm)
      (send self :inside-edges))
    (multiple-value-setq (left top)
      (send self :untransform-point left top))
    (multiple-value-setq (right botm)
      (send self :untransform-point right botm))
    (loop for item in items
	  when (and allow-interrupts? (send self :listen)) return ()
	  unless (send item :outside-p left top right botm)
	  collect item into window-items and
	  do (multiple-value-setq (x y x1 y1)
				  (send item :extents))
	  (multiple-value-bind (deltax deltay)
	      (send self :transform-deltas (- x1 x) (- y1 y))
	    (setq delta (+ deltax deltay)))
	  (cond ((> delta min-dot-delta)
		 (send item :draw self))
		((> delta min-nil-delta)
		 (let ((color (or (send item :edge-color)
				  
				  (send item :fill-color)))
		       (alu (send item :alu)))
		   (when color
		     (prepare-sheet (self)
		       (draw-clipped-solid-triangle x y x y1 x1 y1 self color alu ()))))))
	  finally
	     (when world
	        (send world :set-objects-in-window window-items))
	     (return t))))



(defmethod (graphics-mixin :undraw-picture-list) (items)
  "This method undraws a list of graphic entities in the window.
clipping is performed on each item and a check is done to see if the item is too small to
bother undrawing.  T is returned if the undrawing is not interrupted, otherwise
nil."
  
  (declare (values undrawn-without-interruption))
  (let (botm
	delta
	left
	right
	top
	x
	x1
	y
	y1)
    (multiple-value-setq (left top right botm)
      (send self :inside-edges))
    (multiple-value-setq (left top)
      (send self :untransform-point left top))
    (multiple-value-setq (right botm)
      (send self :untransform-point right botm))
    (loop for item in items
	  when (and allow-interrupts? (send self :listen)) return ()
	  unless (send item :outside-p left top right botm) do
	  (multiple-value-setq (x y x1 y1)
			       (send item :extents))
	  (multiple-value-bind (deltax deltay)
	      (send self :transform-deltas (- x1 x) (- y1 y))
	    (setq delta (+ deltax deltay)))
	  (cond
	    ((> delta min-dot-delta) (send item :undraw self))
	    ((> delta min-nil-delta)
	     (prepare-sheet (self)
	       (draw-clipped-solid-triangle x y x y1 x1 y1 self
					    (or (send item :edge-color) (send item :fill-color))
					    erase))))
	  finally (return t))))
;;;; this is a new drawing method added in case anyone prefers bezier over splines.

(DEFUN bezier (u n x-points y-points)
  (LET ((x 0.0)
	(y 0.0)
	a
	(v 0.00)
	(uprime (- 1.0 u))
       )
    (loop for i from 0 below (+ n 1) do
	 (SETF a 1)
	 (loop for k from (+ i 1) below (+ n 1) do
	      (SETF a (* a k)))
	 (loop for k from 1 below (+ (- n i) 1) do
	      (SETF a (TRUNCATE a k)))
	 (SETQ v a)
	 (loop for j from 1 below (+ i 1) do
	      (SETQ v (* v u)))
	 (loop for j from 1 below (+ (- n i) 1) do
	      (SETQ v (* v uprime)))
	 (SETQ x (+ x (* (AREF x-points i) v)))
	 (SETQ y (+ y (* (AREF y-points i) v)))
    )
    (VALUES x y)
  )
)

(DEFMETHOD (graphics-mixin :draw-bezier)
	   (x-points y-points num-points &optional
	    (thickness 1)
	    (color tv:*default-foreground*)
	    (alu normal)
	    (draw-end-point t)
	    (texture *default-texture*)
	   )
  (LET ( (x0 (AREF x-points 0))
	 (y0 (AREF y-points 0))
	 (xn 0.0)
	 (yn 0.0)
	 (xt 0)
	 (yt 0)
	 (sub-div 43.0)
       )
    (LOOP for j from 1 below (+ 1 sub-div) do
	 (MULTIPLE-VALUE-setq (xn yn) (bezier (/ j sub-div) num-points x-points y-points))
	 (SETQ xt (ROUND xn))
	 (SETQ yt (ROUND yn))
	 (SEND self :draw-line x0 y0 xt yt thickness color alu draw-end-point texture)
	 (SETQ x0 xt)
	 (SETQ y0 yt)
    )
  )
)