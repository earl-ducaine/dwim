;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(medfnt hl12b hl12bi) -*-

;                       RESTRICTED RIGHTS LEGEND

;Use, duplication, or disclosure by the Government is subject to
;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;Technical Data and Computer Software clause at 52.227-7013.

;
;                     TEXAS INSTRUMENTS INCORPORATED
;                              P.O. BOX 2909
;                           AUSTIN, TEXAS 78769

; Copyright (C) 1984-1989 Texas Instruments Incorporated.  All rights reserved.


;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 05/08/89 MAY		Changed draw-clipped-solid-triangle & (graphics-mixin :draw-filled-polygon) 
;;;  			to allow w:draw-color-triangle to draw if ucode
;;;  			does not work correctly. Currently the user must provide extra arg in order to
;;;  			take advantage of this. This corrects bad window patch 4.121 which did not consider
;;;  			polygons with inward pointing vertices (even though they are not supported in
;;;  			documentation, they are used by arrow panes in color-map-editor, etc.).
;;; 08-30-88 MAY	          Fixed draw-clipped-line to be more accurate when trying to adjust end points
;;;                            to clip lines for the clipping rectangle. 
;;; 03/18/88 KJF, KED       Fix to scale-pixel-array-into-window because you cannot AREF a window array
;;;                            when on a microExplorer.
;;; 10/07/87 KJF for KWW  Fix to draw-clipped-solid-triangle.  If color-array is nil we want to blt 100%-black
;;;                            when triangle covers entire window.
;;; 8/26/87 KWW           Changes based on code reading:
;;;                             1. got rid of install color and restore color, used a macro prepare-color-register instead.
;;;                             2. proclaimed range check in line
;;; 7/7/87   KWW           Added specialized draw-rectangle-clipped for use by :draw-filled-rectangle
;;; 7/7/87   KWW           Changed default texture to NIL, since microcode is faster that way
;;; 6/25/87  KWW           Fixed oversight in draw-clipped-line : alu was restricted to just 3 types!
;;; 5/28/87  KWW           Added support for texture in clipped line
;;; 5/20/87  KWW           Added INSTALL-COLOR, RESTORE-COLOR, and RANGE-CHECK for color window system.
;;;                            Modified functions to draw in color.                       
;;;   7/20/87  PMH          Fixed draw-clipped-line to understand alu-seta
;;;   3/11/87  DAN	          Added color argument back to DRAW-CLIPPED-LINE to correct lines with thickness 1
;;;                             and color 0.
;;; 01/05/87 JEB		Added WITH-CLIPPING-RECTANGLE around calls to %draw-shaded-triangle.
;;; 12/23/86 JEB		Corrected parameter list to %draw-shaded-triangle for one pixel wide lines.
;;; 11/25/86 TWE		Changed references to %draw-line with %draw-shaded-triangle.
;;; 10/29/86 TWE		Changed a call to BIT-TEST to use the Common Lisp function LOGTEST instead.
;;;			Likewise for REMAINDER and the Common Lisp function REM.
;;; 10/23/86 TWE		Changed draw-clipped-line to ignore its color argument.
;;; 10/22/86 TWE		Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 10/21/86 JEB		Replaced obsolete %draw-filled-triangle with %draw-shaded-triangle
;;; 10/20/86 TWE		Changed references to pixel-array-width and pixel-array-height to use
;;;			array-dimension to be in conformance with Common Lisp.
;;; 09/16/86 JEB		Moved functions from GWIN: to W:.  Deactivated all the old CADR color support.
;;;

;;;
;;;                PRIMITIVES
;;
;; OVERVIEW:
;;
;; These primitive functions are the basic drawing functions for all types of objects.  In addition to
;; these fuctions there are several standard system functions that are also used as primitives.
;;
;;	STENGER'S CONJECTURE:  Any graphics object can be drawn with filled triangles
;;	At the lowest level all drawing is done by these routines, so if anything can be done to
;;	improve the operation of these primitive funtions you will see an improvement in overall
;;	performance.
;;
;;
;;	Conversion between world and screen coordinates (i.e. point transformation) is localized in
;;	DRAW-CLIPPED-SOLID-TRIANGLE.
;;
;;	functions:	draw-clipped-solid-triangle
;;		draw-color-triangle
;;


(DEFPARAMETER *default-texture* nil) ; initialize to a solid fill pattern (microcode optimized for nil source)


;;; Utility functions for the color version

(PROCLAIM '(inline range-check))
		     
(DEFUN range-check (value)
  (MIN (MAX value 0) 255)
) ; end of defun

;;; prepare-sheet normally makes the sheet instance variables track into the hardware registers.
;;; the normal approach to color programming is to then manipulate the sheet instance variables
;;; before doing the I/O. Then, when the I/O occurs, the prepare-sheet updates the hardware.
;;;
;;; BUT!!!! some of the graphics stuff here gets called from within a prepare sheet, so we
;;; have to deal directly with the hardware register ourselves. This prepare-color-register
;;; macro does the work for us.

(DEFMACRO prepare-color-register ((sheet color) body)
  `(LET ((save-foreground (tv:sheet-foreground-color ,sheet))
	 (prepare-color-register-p (AND (color-system-p ,sheet) (NUMBERP ,color))))
     (UNWIND-PROTECT
	 (progn
	   (if prepare-color-register-p  ;;; make sure color is active and valid
	       (tv:set-foreground-color-register ,color))
	   ,body
	   ) ; end of progn
      (if prepare-color-register-p
	  (tv:set-foreground-color-register save-foreground))
      )))


;;; 8/26/87 changed install and remove color to prepare-color-register macro
(DEFUN DRAW-RECTANGLE-CLIPPED
       (WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION draw-edge color SHEET
        &AUX (INSIDE-LEFT (SHEET-INSIDE-LEFT SHEET))
        (INSIDE-TOP (SHEET-INSIDE-TOP SHEET)))
  "Draw rectangle in SHEET, coordinates relative to inside of SHEET,
clipping to inside.  Recall that the inside of SHEET is what is not part
of the margins."
  (prepare-color-register (sheet color)
   (progn
    (IF draw-edge
      (SETQ height (+ height 1)
 	    width (+ width 1))
    )
    (AND (< X-BITPOS INSIDE-LEFT)
         (SETQ WIDTH (- WIDTH (- INSIDE-LEFT X-BITPOS))
               X-BITPOS INSIDE-LEFT))
    (AND (< Y-BITPOS INSIDE-TOP)
         (SETQ HEIGHT (- HEIGHT (- INSIDE-TOP Y-BITPOS))
               Y-BITPOS INSIDE-TOP))
    (SETQ WIDTH  (MIN WIDTH  (MAX 0 (- (SHEET-INSIDE-RIGHT SHEET)  X-BITPOS))))
    (SETQ HEIGHT (MIN HEIGHT (MAX 0 (- (SHEET-INSIDE-BOTTOM SHEET) Y-BITPOS))))
    (AND (> WIDTH 0) (> HEIGHT 0)
       (%DRAW-RECTANGLE (ROUND WIDTH) (ROUND HEIGHT) (ROUND X-BITPOS) (ROUND Y-BITPOS) ALU-FUNCTION SHEET)
    )
   )
  )
)


(defun draw-clipped-solid-triangle (x1 y1 x2 y2 x3 y3 window
				    &optional (color (tv:sheet-foreground-color window))
				              (alu normal)
					      (draw-third-edge t)
                                              (draw-second-edge t)
					      (draw-first-edge t)
					      (color-array *default-texture*)
					      (use-lisp nil) ;; may 05/08/89 
					      )
  "This function draws a colored, filled in, triangle.
The triangle vertices are converted from world to screen coordinates."
  ;;
  ;; Convert endpoints from world to screen coordinates if the window handles transformation.
  ;;
  (cond
    ((send window :operation-handled-p :transform-point)
     (multiple-value-setq (x1 y1)
       (send window :transform-point x1 y1))
     (multiple-value-setq (x2 y2)
       (send window :transform-point x2 y2))
     (multiple-value-setq (x3 y3)
       (send window :transform-point x3 y3)))
    (t
     (setq x1 (g-round x1)
	   y1 (g-round y1)
	   x2 (g-round x2)
	   y2 (g-round y2)
	   x3 (g-round x3)
	   y3 (g-round y3))))
  ;;
  ;; Make sure that the specified color is in the range of available colors.
  ;;
  
  (IF (color-system-p window)
      (setq color (range-check color)) ;;; >>> changed to range-check
      ;;; else
      (progn
	(setq color (rem color (array-active-length b&w-table))) ; sure color is in range for gray shades   JEB
	(unless color-array (SETQ color-array (AREF b&w-table color)))
        )
        ) ; if color-system


  (WHEN (AND (send window :reverse-video-p) (NULL (color-system-p window))) 
    (setq alu (reverse-alu alu))  
    )
  
  (let* ((array         (sheet-screen-array window))
	 (bottom        (sheet-inside-bottom window))
	 (left          (sheet-inside-left window))
	 (right         (sheet-inside-right window))
	 (top           (sheet-inside-top window)))
     ;;
    ;; If the triangle is gigantic and covers the whole window, then don't bother drawing it.  Instead we just fill
    ;; the entire window with the appropriate color. 
    ;;
    (prepare-color-register (window color)
    (cond
      ((and (< (min x1 x2 x3) -5000)  (> (max x1 x2 x3) 5000)
	    (< (min y1 y2 y3) -5000)  (> (max y1 y2 y3) 5000))
         ;;;; <<< here is the fix - if color-array is nil we want to blt 100%-black >>>>
         (bitblt alu (- right left) (- bottom top) (OR color-array 100%-black) 0 0 array left top))
      ;;
      ;; Now draw the triangle but limit the coordinates to reasonable values to keep the drawing speed in
      ;; the acceptable range. 
      ;;
      (t
       (setq x1 (min (max x1 -5000) 5000)
	     y1 (min (max y1 -5000) 5000)
	     x2 (min (max x2 -5000) 5000)
	     y2 (min (max y2 -5000) 5000)
	     x3 (min (max x3 -5000) 5000)
	     y3 (min (max y3 -5000) 5000))
       (IF use-lisp					;; may 05/08/89 
	   (draw-color-triangle x1 y1 x2 y2 x3 y3	;; may 05/08/89 
				color-array window	;; may 05/08/89 
				alu			;; may 05/08/89 
				draw-third-edge		;; may 05/08/89 
				draw-second-edge	;; may 05/08/89 
				draw-first-edge)	;; may 05/08/89 
	   (tv:with-clipping-rectangle
	     (left top right bottom)
	     ;; sys:%draw-shaded-triangle  ALWAYS returns t since ucode began to handle lines! may 8-30-88
	     ;; the UNLESS body is no longer necessary.
	     (unless (or
		       (sys:%draw-shaded-triangle x1 y1 x2 y2 x3 y3 alu
						  draw-first-edge draw-second-edge draw-third-edge
						  color-array window)
		       (not draw-third-edge))
	       (if (and (= x1 x2) (= y1 y2) (neq x1 x3) (neq y1 y3))
		   (draw-clipped-line x1 y1 x3 y3 alu t window color color-array)
		   (if (and (neq x1 x2) (neq y1 y2))
		       (draw-clipped-line x1 y1 x2 y2 alu t window color color-array)))
	       ))))))))

; Don't bother looking for a color screen any more and replace obsolete %draw-filled-triangle with %draw-shaded-triangle

;        (unless (or
; 		 
; 		 (if color-screen?
; 		     (draw-color-triangle x1 y1 x2 y2 x3 y3 color-array window alu draw-third-edge
; 					  draw-second-edge draw-first-edge)
; 		     
; 		     (system:%draw-filled-triangle x1 y1 x2 y2 x3 y3
; 						   left top right bottom alu
; 						   
; 						   ;;; draw-third-edge draw-second-edge draw-first-edge
; 						   
; 						   ;;; draw all edges until bug in %draw-filled-triangle
; 						   ;;  is fixed
; 					       
; 						   t t t color-array array))
; 		     (not draw-third-edge))
; 		 (if (and (= x1 x2) (= y1 y2) (neq x1 x3) (neq y1 y3))
; 		     (draw-clipped-line x1 y1 x3 y3 alu t window color)
; 		     (if (and (neq x1 x2) (neq y1 y2))
; 			 (draw-clipped-line x1 y1 x2 y2 alu t window color)))))))) 



(defun draw-color-triangle (x1 y1 x2 y2 x3 y3 color window
			    &optional (alu normal)
			    (draw-third-edge nil)
                            (draw-second-edge t)
			    (draw-first-edge t))
  "Draw a filled triangle of the specified color.
Color is an array ready to be bitblted onto the b&w or color screen, either
of gray shades or of indexes into the color table."
  
  (declare (values triangle-drawn?))
  (let ((first-edge 2)
	(second-edge 1)
	(third-edge 0))
	;;
	;; The first thing to do is to sort the points by increasing y coordinates.
	;;
    (cond
      ((< y1 y2) (psetq x1 x2 x2 x1) (psetq y1 y2 y2 y1) (setq third-edge 1
							       second-edge 0)))
    (cond
      ((< y1 y3) (psetq x1 x3 x3 x1) (psetq y1 y3 y3 y1) (setq first-edge 1)
       (if (= 1 third-edge)
	 (setq third-edge 2)
	 (setq second-edge 2))))
    (cond
      ((< y2 y3) (psetq x2 x3 x3 x2) (psetq y2 y3 y3 y2)
       (case third-edge
	 (0 (setq third-edge 2))
	 (2 (setq third-edge 0)))
       (case second-edge
	 (0 (setq second-edge 2))
	 (2 (setq second-edge 0)))
       (if (= 2 first-edge)
	 (setq first-edge 0))))
    (let (left
	  error-left
	  inc-left
	  neg-inc-left
	  pos-inc-left
	  right
	  error-right
	  inc-right
	  neg-inc-right
	  pos-inc-right
	  (bottom-limit (sheet-inside-bottom window))
	  (determinant (determinant x3 y3 x1 y1 x2 y2))
	  (left-limit (sheet-inside-left window))
	  (right-limit (sheet-inside-right window))
	  (screen-array (sheet-screen-array window))
	  (top-limit (sheet-inside-top window))
	  (x-dim (array-dimension color 1))        ; Width
	  (y-dim (array-dimension color 0))        ; Height
	  (drew-something? nil)
	  (draw-line 1))
      (cond
	((not (zerop determinant))
	 ;;
	 ;; Calculate the initial error and increment values for the long side
	 ;; of, which is shared between both halves of the triangle, (x3,y3)
	 ;; to (x1,y1).
	 ;;
	 (cond
	   ((minusp determinant)
	    ;;
	    ;; The long side is the left side so set the left error and increment values.
	    ;;
	    (multiple-value-setq (error-left inc-left neg-inc-left pos-inc-left)
	      (triangle-increments x3 y3 x1 y1))
	    (setq left x3)
	    (when (or (and (not draw-third-edge) (zerop third-edge))
		(and (not draw-second-edge) (zerop second-edge))
		(and (not draw-first-edge) (zerop first-edge)))
	      (incf left)))
	   (t
	    ;;
	    ;; The long side is the right side so set the right error and increment values.
	    ;;
	    (multiple-value-setq (error-right inc-right neg-inc-right pos-inc-right)
	      (triangle-increments x3 y3 x1 y1))
	    (setq right x3)
	    (when (or (and (not draw-third-edge) (zerop third-edge))
		(and (not draw-second-edge) (zerop second-edge))
		(and (not draw-first-edge) (zerop first-edge)))
	      (decf right))))
	 (do ((x x3 x2)
	      (y y3)
	      (x-end x2 x1)
	      (y-end y2 y1)
	      (y-limit y2 (1+ y1)))
	     ((> y y1))
	      ;;
	      ;; Calculate the initial error and increment values for the short
	      ;; side of the triangle, (x3,y3) to (x2,y2) and then (x2,y2) to
	      ;; (x1,y1).
	      ;;
	   (decf third-edge)
	   (decf second-edge)
	   (decf first-edge)
	   (cond
	     ((minusp determinant)
	      ;;
	      ;; The short side is the right side so set the right error and increment values.
	      ;;
	      (multiple-value-setq (error-right inc-right neg-inc-right pos-inc-right)
		                   (triangle-increments x y x-end y-end))
	      (setq right x)
	      (when (or (and (not draw-third-edge) (zerop third-edge))
		  (and (not draw-second-edge) (zerop second-edge))
		  (and (not draw-first-edge) (zerop first-edge)))
		(decf right)
		(if (< (- y-limit y) 2)
		  (setq draw-line 0))))
	     (t
	      ;;
	      ;; The short side is the left side so set the left error and increment values.
	      ;;
	      (multiple-value-setq (error-left inc-left neg-inc-left pos-inc-left)
		(triangle-increments x y x-end y-end))
	      (setq left x)
	      (when (or (and (not draw-third-edge) (zerop third-edge))
		  (and (not draw-second-edge) (zerop second-edge))
		  (and (not draw-first-edge) (zerop first-edge)))
		(incf left)
		(if (< (- y-limit y) 2)
		  (setq draw-line 0)))))
	   (do ((xl)
		(xr))
	       ((>= y y-limit) t)
		;;
		;; Clip this horizontal line of the triangle by first checking to
		;; be sure that the y value is between the top and bottom of the
		;; window, and then trimming the left and right ends to the sides
		;; of the window.
		;;
	     (and (>= y bottom-limit) (setq y (1+ y1)) (return t))
	     (cond
	       ((>= y top-limit)
		(setq xl (min (max left left-limit) (1- right-limit))
		      xr (min (max right left-limit) (1- right-limit)))
		(when (or (> xr xl) (and (> xl left-limit) (< xr (1- right-limit))))
		  (setq drew-something? t)
		  (bitblt alu (max (- xr xl -1) 0) draw-line color (rem xl x-dim)
			  (rem y y-dim) screen-array xl y))))
	     (cond
	       ((minusp error-left) (setq error-left (+ error-left neg-inc-left)))
	       (t (setq error-left (+ error-left pos-inc-left)
			left (1+ left))))
	     (cond
	       ((minusp error-right) (setq error-right (+ error-right neg-inc-right)))
	       (t (setq error-right (+ error-right pos-inc-right)
			right (1+ right))))
	     (setq left (+ left inc-left)
		   right (+ right inc-right)
		   y (1+ y)
		   draw-line 1)))))
      (values drew-something?)))) 



(defun draw-clipped-line (x1 y1 x2 y2 alu draw-end-point window color &optional
			  (color-array *default-texture*))
  "This function draws a single pixel thick line which is clipped to the insides of the window.
The coordinates which are input are inside coordinates for the window.  The algorithm used here is
the Cohen-Sutherland algorithm as described in Principles of Interactive Computer Graphics, by
Newman and Sproull."
  (IF (mac-window-p window)
      (send-draw-line x1 y1 x2 y2 alu draw-end-point window t)
    ;; else...
    ;;
    ;; Make sure that the specified color is in the range of available colors.
    ;;
    (if (color-system-p window)
	(setq color (range-check color)) ;;; >>> changed to range-check
      ;;; else do what's right for black and white
      (progn
	(setq color (rem color (array-active-length b&w-table))) ; sure color is in range for gray shades   JEB
	(unless color-array (SETQ color-array (AREF b&w-table color)))
	)
      ) ;;; end of if
    (unless (rassoc alu color-alu-alist)	;de-CONSified this test PMH 3/1/88
	(setq alu (send window :char-aluf)))
    (let ((bottom (sheet-inside-bottom window))
	  (left (sheet-inside-left window))
	  (right (sheet-inside-right window))
	  (top (sheet-inside-top window))
	  )
;;      (x-offset (send window :x-offset))    ; no longer used
;;      (y-offset (send window :y-offset)))
      (do ((code1 (sector-code x1 y1 left top right bottom))
	   (code2 (sector-code x2 y2 left top right bottom))
	   x
	   y)
	  ((and (zerop code1) (zerop code2))
	   (prepare-color-register (window color)
				   (tv:with-clipping-rectangle
				     (left top right bottom)
				     (SYS:%DRAW-SHADED-TRIANGLE
				       x1 y1 x2 y2 x2 y2 alu t draw-end-point t color-array window)
				     )
				   )) 
	(or (zerop (logand code1 code2)) (return))
	(selector (if (zerop code1)
		      code2
		    code1)
		  logtest
	   ;; may 8-29-88 changed "(quotient" to "(round (/" in 4 setq's below. SPR 4536
	  (1 (setq x left
		   y (+ y1 (round (/ (* (- y2 y1) (- left x1)) (- x2 x1))))))
	  (2 (setq x right
		   y (+ y1 (round (/ (* (- y2 y1) (- right x1)) (- x2 x1))))))
	  (4 (setq x (+ x1 (round (/ (* (- x2 x1) (- bottom y1)) (- y2 y1))))
		   y bottom))
	  (8 (setq x (+ x1 (round (/ (* (- x2 x1) (- top y1)) (- y2 y1))))
		   y top)))
	(if (zerop code1)
	    (setq x2 x
		  y2 y
		  code2 (sector-code x y left top right bottom))
	    (setq x1 x
		  y1 y
		  code1 (sector-code x y left top right bottom))))
      )					   ; end of let
    ))


; 
; (defun color-draw-line (x1 y1 x2 y2
; 			&optional (color 17) (alu alu-seta)
; 			          (screen color::color-screen))
;     "Replaces color:color-draw-line..  color:color-draw-line does not use the alu for 
; drawing non-rectilinear lines.."
;   
;   (and (> x1 x2) (rotatef x1 x2) (rotatef y1 y2))
;   (prepare-sheet (screen)
;      (let ((dx (- x2 x1))
; 	   (dy (- y2 y1))
; 	   (pixel-array (sheet-screen-array color::color-screen)))
;        (let ((dir-y (if (minusp dy)
; 		      -1
; 		      1))
; 	     (dy (abs dy)))
; 	 (cond
; 	   ((zerop dy)
; 	    (bitblt alu (abs dx) 1 (aref color-table color) 0 0
; 		    pixel-array (min x1 x2) y1))
; 	   ((zerop dx) (bitblt alu 1 dy (aref color-table color) 0 0
; 			       pixel-array x1 (min y1 y2)))
; 	   ((> dx dy)                                                       ;X IS LARGER STEP
; 	    (do ((x1 x1 (1+ x1))
; 		 (rem (quotient dy 2) (+ rem dy)))
; 		((> x1 x2))
; 	      (if (>= rem dx)
; 		(setq y1 (+ y1 dir-y)
; 		      rem (- rem dx)))
; 	      (setf (aref pixel-array y1 x1) (boole alu color
; 						    (aref pixel-array y1 x1)))))
; 	   (t	                                                                 ;Y IS LARGER STEP
; 	    (do ((i 0 (1+ i))
; 		 (y y1 (+ y dir-y))
; 		 (rem (quotient dx 2) (+ rem dx)))
; 		((> i dy))
; 	      (if (>= rem dy)
; 		(setq x1 (1+ x1)
; 		      rem (- rem dy)))
; 	      (setf (aref pixel-array y x1) (boole alu color
; 						   (aref pixel-array y x1)))))))))) 
; 


;; On a microExplorer, drawing should NOT be done using (SETF (AREF window-array ...)).
;; So we create a temp-array to use instead.  We can't use :draw-point or :draw-rectangle
;; becaue they do transforms.  The other choice would be to use the mac (send-draw-point)
;; function, but since we would be sending so many commands over to the mac, the performance
;; would be very bad!!  We tried it, it's slow!  KJF, KED.  03/18/88
(defun scale-pixel-array-into-window (pixel-array height width
				      x-scale y-scale window window-array)
  "Scales a 2D pixel array into window-array of window.
Height and width are the amounts of pixel-array containing valid data to be
scaled.  The scaled height and scaled width are returned."
  
  (declare (values scaled-height scaled-width))
  (send window :clear-screen)
  (LET* ((s-height (min (g-round (* y-scale height))
			(array-dimension window-array 0)))
	 (s-width  (min (g-round (* x-scale width))
			(array-dimension window-array 1)))
	 ;; When on microExplorer, the window-array is cache'd on the MAC side, thus in the code below,
	 ;; AREF'ing will not work.  So, a temp array is set up which will hold the scaled pattern.  03/18/88 KJF.
	 (temp-array (IF (mac-window-p window)
			 ;; Remember, second dimension must be a multiple of 32.
			 (make-array (list s-height (* 32 (ceiling s-width 32)))
				     :element-type
				     (array-element-type window-array))
			 window-array)))
    ;;
    ;; Scale the character by pixel replication.
    ;;
    (loop for i from 0 below s-width
	  for previous-x = -1 then old-x
	  for old-x = (values (floor (quotient i x-scale)))
	  when (< old-x width)
	  do (if (= old-x previous-x)
		 (bitblt normal 1 s-height temp-array (1- i) 0 temp-array i 0)
		 (loop for j from 0 below s-height
		       for old-y = (values (floor (quotient j y-scale)))
		       when (< old-y height)
		       do (setf (aref temp-array j i) (aref pixel-array old-y old-x))))
	  ;; Only on microExplorer do we have to blt the array.
	  finally (WHEN (mac-window-p window)
		    (bitblt normal s-width s-height temp-array 0 0 window-array 0 0)))
    (values s-height s-width)))