;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(medfnt hl12b hl12bi) -*-

;Use, duplication, or disclosure by the Government is subject to
;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;Technical Data and Computer Software clause at 52.227-7013.

;                     TEXAS INSTRUMENTS INCORPORATED
;                              P.O. BOX 2909
;                           AUSTIN, TEXAS 78769

; Copyright (C) 1984-1989 Texas Instruments Incorporated.  All rights reserved.


;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 10/22/86 TWE		Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 09/11/86 JEB		Moved functions from GWIN: to W:




(defsubst line-deltas (ax ay bx by cx cy half-thickness)
  "This calculates the delta x and y values for the edges of two connected wide lines.
These two values are returned in a list.  The line segments go from (ax,ay) to (bx,by)
to (cx,cy)."
  (declare (values delta-x delta-y))
  (let ((dx1 (- ax bx))
	(dy1 (- ay by))
	(dx2 (- cx bx))
	(dy2 (- cy by)))
    (let ((dt (- (* cx dy1) (* bx (- ay cy)) (* ax dy2)))
	  (length1 (sqrt (+ (* dx1 dx1) (* dy1 dy1))))
	  (length2 (sqrt (+ (* dx2 dx2) (* dy2 dy2)))))
      (cond
	((> (abs dt) 0.5)
	 (setq dt (quotient (float half-thickness) dt))
	 (values (* dt (+ (* length2 dx1) (* length1 dx2)))
		 (* dt (+ (* length2 dy1) (* length1 dy2)))))
	((not (zerop length1))
	 (values (quotient (* half-thickness dy1) length1)
                 (quotient (* half-thickness (- dx1)) length1)))
	((not (zerop length2))
	 (values (quotient (* half-thickness (- dy2)) length2)
	  	 (quotient (* half-thickness dx2) length2)))
	(t (values 0 0))))))



(defun polyline-min-max (x-points y-points
			 &optional (thickness 1)
                                   (num-points (min (array-active-length x-points)
						    (array-active-length y-points))))
  "This calculates the minimum and maximum coordinates of a polyline with a thickness.
This is basically the extents of the polyline."
  
  (declare (arglist x-points y-points
		    &optional (thickness 1)
	                      (num-points (min (array-active-length x-points)
					       (array-active-length y-points))))
           (values min-x min-y max-x max-y))
  (setq thickness (/ (max 0 thickness) 2.0))
  (do ((i 1 (1+ i))
       (i-1 0 (1+ i-1))
       (n (1- num-points))
       (n-1 (- num-points 2))
       (closed
	(and (= (aref x-points 0) (aref x-points (1- num-points)))
	     (= (aref y-points 0) (aref y-points (1- num-points)))))
       (max-x (aref x-points 0))
       (max-y (aref y-points 0))
       (min-x (aref x-points 0))
       (min-y (aref y-points 0))
       (dx)
       (dy)
       (x1)
       (y1)
       (x2)
       (y2)
       (x3)
       (y3))
      ((>= i num-points)
       (values min-x min-y max-x max-y))
    (setq x1 (aref x-points i-1)
	  y1 (aref y-points i-1)
	  x2 (aref x-points i)
	  y2 (aref y-points i))
    (or x1 y1 x2 y2 (return (values min-x min-y max-x max-y)))
    (cond
      ((= i 1)
       (cond
	 (closed (setq x3 (aref x-points n-1)
		       y3 (aref y-points n-1)))
	 (t (setq x3 x1
		  y3 y1)))
       (multiple-value-setq (dx dy)
	 (line-deltas x3 y3 x1 y1 x2 y2 thickness))
       (setq max-x (max max-x (- x1 dx) (+ x1 dx))
	     max-y (max max-y (- y1 dy) (+ y1 dy))
	     min-x (min min-x (- x1 dx) (+ x1 dx))
	     min-y (min min-y (- y1 dy) (+ y1 dy)))))
    (cond
      ((= i n)
       (cond
	 (closed (setq x3 (aref x-points 1)
		       y3 (aref y-points 1)))
	 (t (setq x3 x2
		  y3 y2))))
      (t (setq x3 (aref x-points (1+ i))
	       y3 (aref y-points (1+ i)))))
    (or x3 y3 (return (values min-x min-y max-x max-y)))
    
    (multiple-value-setq (dx dy)
      (line-deltas x1 y1 x2 y2 x3 y3 thickness))
    (setq max-x (max max-x (- x2 dx) (+ x2 dx))
	  max-y (max max-y (- y2 dy) (+ y2 dy))
	  min-x (min min-x (- x2 dx) (+ x2 dx))
	  min-y (min min-y (- y2 dy) (+ y2 dy)))))



