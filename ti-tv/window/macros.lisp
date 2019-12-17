;;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(medfnt hl12b hl12bi) -*-

;;;			      RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769

;;; Copyright (C) 1984- 1989 Texas Instruments Incorporated.  All rights reserved.


;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 10/20/88 LG         	Replaced reference to ARRAY-LENGTH in reverse-alu to ARRAY-TOTAL-LENGTH.
;;; 02/22/88 KJF             Added some error checking to reverse-alu function.
;;; 10/22/86 TWE		Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 09/15/86 JEB		Moved functions from GWIN: to W:.


(defmacro g-round (x &optional (y 1 y-p))
  "Round to the nearest integer of x divided by y."
  
  (if y-p
    `(floor (+ (quotient ,x (float ,y)) 0.5))
    `(floor (+ ,x 0.5))))


(defmacro scalef (ref amount)
  
  `(setf ,ref (* ,ref ,amount)))


(defsubst determinant (x y x1 y1 x2 y2)
  "This calculates the determinant of the matrix formed from three points.
The matrix is as follows:

	--         --
	| x  x1  x2 |
	| y  y1  y2 |
	| 1   1   1 |
	--         --


This determinant also is the value of the equation of the line through the
points (x1,y1) and (x2,y2) with the point (x,y) substituted in."

  
  (declare (values determinant))
  (- (* (- x1 x2) (- y1 y)) (* (- y1 y2) (- x1 x))))


(defsubst dist (x1 y1 x2 y2)
  "This calculates the distance between two points (x1,y1) and (x2,y2)."
  
  (declare (values distance))
  (let ((dx (- x2 x1))
	(dy (- y2 y1)))
    (sqrt (+ (* dx dx) (* dy dy)))))



(defsubst transform-point (x y transform)
  "This transforms a two dimensional point with a three by three matrix."
  
  (declare (values transformed-x transformed-y))
  (values
    (g-round (+ (* x (aref transform 0 0)) (* y (aref transform 1 0)) (aref transform 2 0)))
    (g-round (+ (* x (aref transform 0 1)) (* y (aref transform 1 1)) (aref transform 2 1)))))



(defsubst untransform-deltas (dx dy transform)
  "This changes two distances from window to world coordinates."
  
  (let ((denom
	 (float
	  (- (* (aref transform 0 0) (aref transform 1 1))
	     (* (aref transform 1 0) (aref transform 0 1))))))
    (cond
      ((not (zerop denom))
       (values  (quotient (- (* dx (aref transform 1 1))
			     (* dy (aref transform 1 0)))
			  denom)
                (quotient (- (* dy (aref transform 0 0))
			     (* dx (aref transform 0 1)))
			  denom)))
      (t (values 0 0)))))


(defsubst triangle-increments (from-x from-y to-x to-y)
  "This calculates inital error values and the increments for a triangle side.
It returns these four values."
  
  (declare
   (values initial-error x-increment negative-error-increment positive-error-increment))
  (let* ((dx (- to-x from-x))
	 (dy (- to-y from-y))
	 (slope (cond ((plusp dy) (values (floor (quotient (float dx) dy))))
	              (t dx)))
	 (value (* 2 (- dx (* dy slope)))))
    (values (- value dy) slope value (- value (* 2 dy)))))


(defsubst untransform-point (x y transform)
  "This reverses the transformation process that transform point performs.
It untransforms a two dimensional point using a general three by three matrix."
  
  (declare (values untransformed-x untransformed-y))
  (let ((denom
	 (float
	  (- (* (aref transform 0 0) (aref transform 1 1))
	     (* (aref transform 0 1) (aref transform 1 0))))))
    (cond ((not (zerop denom))
           (setq x (- x (aref transform 2 0))
				 y (- y (aref transform 2 1)))
           (values (quotient (- (* x (aref transform 1 1))
			    (* y (aref transform 1 0)))
		      denom)
       	           (quotient (- (* y (aref transform 0 0))
			    (* x (aref transform 0 1)))
		      denom)))
           (t (values 0 0)))))


(defsubst sector-code (x y left top right bottom)
  "This returns a bit encoded value for the location of a point in relation to an
orthogonal rectangle.  The rectangle is the center box of the following diagram and the
returned value is indicated for each sector.

                    |        |
              1001  |  1000  |  1010		The first number is the
                9   |    8   |   10		binary value of the code 
                    |        |			if the point lies in that

            --------------------------		area.  The second number 
                    |        |			is the decimal value of
              0001  |  0000  |  0010		the code.
                1   |    0   |    2
                    |        |

            --------------------------
                    |        |
              0101  |  0100  |  0110
                5   |    4   |    6
                    |        |
                                 "
  
  (declare (values location-code))
  (let ((code 0))
    (cond-every ((< x left) (setq code (+ code 1)))
		((> x right) (setq code (+ code 2)))
                ((> y bottom) (setq code (+ code 4)))
		((< y top) (setq code (+ code 8))))
    code))


;; Do some error checking.  Added when color alu's were added to reverse-alu-translation-table.
;; 02/22/88 KJF.
(defsubst reverse-alu (alu)
  "Return the reverse alu from the reverse-alu-translation-table.  Alu
should be an integer within the range of reverse-alu-translation-table."
  (LET ((high (ARRAY-TOTAL-SIZE reverse-alu-translation-table)))
    (ASSERT (TYPEP alu `(integer 0 (,high)))  ;; high - exclusive
	    nil
	    "Alu specified is not in the range of reverse-alu-translation-table
or is not an integer.")
    (SVREF reverse-alu-translation-table alu)))


