;;;;-*-Mode:Common-LISP;Package:TV;Base:10;Fonts:(MEDFNT HL12BI MEDFNB)-*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1989 Texas Instruments Incorporated. All rights reserved.

;;;; This file contains definitions and methods for accessing the register block.
;;;; The block is where the foreground and background colors are set, for example.
;;;; See the hardware reference for what these registers are used for.
;;;;
;;;; HARDWARE REFERENCES (P/N 2534334-0001 DATED 31 MARCH 1987)
;;;;
;;;;   4.2.1.1 THROUGH 4.2.4
;;;;   4.2.9.4
;;;;


;;;
;;;   Original code developed for CSIB-Inspector.
;;;   Adapted for use in CSIB-mixin by Ken Wood 4/8/87
;;;   Change history:
;;;    08/25/87   KWW   Made changes recommended at the code reading:
;;;                              mostly minor cleanup, for example :type => :element-type
;;;
;;;    08/17/87   KWW   Fixed so that it could re-initialize after booting
;;;
;;;

(DEFFLAVOR block-flavor
	   (nubus-address
	    foreground-color-register
	    background-color-register
	    transparency-register
	    plane-mask-register
	    pixel-mask-register
	    (pixel-mask-vector (make-array 1024. :element-type 'bit :fill-pointer 0)))
	   () 
  (:inittable-instance-variables nubus-address)
  (:gettable-instance-variables nubus-address pixel-mask-vector))


;;; 
;;; define a function that makes an instance displaced to the appropriate block
;;;

(DEFUN make-register-block (which-block)
  (MAKE-INSTANCE 'block-flavor
		 :nubus-address
		 (physical-address
		   block-base
		   (* which-block #x100))))
;;;
;;; make all of the blocks by looping over the possible legal values of block number
;;;

;;; removed (if already exists test) 8/17/87
(defun make-blocks ()
  (dotimes (i (1+ number-of-blocks) nil)
      (progn
        (setf (aref *blocks* i) (make-register-block i))
      ) ; end of progn
  ) ; end of dotime
) ; end of function

;;;;
;;;;  REGISTER METHODS 
;;;;

;;;
;;; this method is invoked by CSIB-mixin :before :init methods.
;;; had to seperate out some functionality for ease of update.
;;;

(DEFMETHOD (block-flavor :after :init) ;;; changed from :finish-init to :after :init	   
	   (&rest ignore)

  (SETF foreground-color-register 
	(make-physical-array 1. :element-type '(unsigned-byte 32) ;;; art-32b array in common lisp form
			     :displaced-to-physical-address
			     (when nubus-address (+ nubus-address foreground-offset))))
  (SETF background-color-register 
	(make-physical-array 1. :element-type '(unsigned-byte 32)
			     :displaced-to-physical-address
			     (when nubus-address (+ nubus-address background-offset))))
  (SETF transparency-register 
	(make-physical-array 1. :element-type '(unsigned-byte 32)
			     :displaced-to-physical-address
			     (when nubus-address (+ nubus-address transparency-offset))))
  (SETF plane-mask-register 
	(make-physical-array 1. :element-type '(unsigned-byte 32)
			     :initial-element #xFFFFFFFF
			     :displaced-to-physical-address
			     (when nubus-address (+ nubus-address plane-mask-offset))))
  (SETF pixel-mask-register 
	(make-physical-array 32. :element-type '(unsigned-byte 32)
			     :initial-element #xFFFFFFFF
			     :displaced-to-physical-address
			     (when nubus-address (+ nubus-address pixel-mask-offset))))

)


;;;
;;; The following methods provide means to read or change the contents of each register within the block.
;;;

(DEFMETHOD (block-flavor :foreground-color-register)
	   ()
"Return the contents of the foreground color register"
  (logand #xFF (AREF foreground-color-register 0)))

;;;
;;; ======================================================
;;;

(DEFMETHOD (block-flavor :set-foreground-color-register)
	   (value)
"Set the foreground color register"
  (setf (AREF foreground-color-register 0) value)
)

;;;
;;; ======================================================
;;;

(DEFMETHOD (block-flavor :background-color-register)
	   ()
"Return the contents of the background color register"
  (LOGAND #xFF (AREF background-color-register 0)))

;;;
;;; ======================================================
;;;

(DEFMETHOD (block-flavor :set-background-color-register)
	   (value)
"Set the background color register"
  (SETF (AREF background-color-register 0) value)
)

;;;
;;; ======================================================
;;;

(DEFMETHOD (block-flavor :transparency-register)
	   ()
"Return the contents of the transparency register"
  (LOGAND #xFF (AREF transparency-register 0)))

;;;
;;; ======================================================
;;;

(DEFMETHOD (block-flavor :set-transparency-register)
	   (value)
"Set the transparency register"
  (SETF (AREF transparency-register 0) value)
)

;;;
;;; ======================================================
;;;

(DEFMETHOD (block-flavor :plane-mask-register)
	   ()
"Return the contents of the plane-mask-register"
  (LOGAND #xFF (AREF plane-mask-register 0)))

;;;
;;; ======================================================
;;;

(DEFMETHOD (block-flavor :set-plane-mask-register)
	   (value)
"Set the plane-mask-register"
  (SETF (AREF plane-mask-register 0) value)
)



(DEFMETHOD (block-flavor :pixel-mask-register)
	   ()
"Return the contents of the pixel-mask-register" 
  (reduce-vector-descriptor (read-mask pixel-mask-register)))

(defun read-mask (register)
  (let ((length (* 32. (car (array-dimensions register))))
	(element 0)
	descriptor)
    (dotimes (j length descriptor)
      (setf descriptor
	    (cons (list (dotimes (i (- length element -1.))
			  (setf element (1+ element))
			  (if (>= element length) (return (1+ i)))
			  (if (= (bit-in-array (+ element -1.) register)
				 (bit-in-array element register))
			      nil
			      (return (1+ i))))
			(bit-in-array (1- element) register))
		  descriptor))
      (if (>= element length) (return descriptor)))))

;;;
;;; ======================================================
;;;

(DEFUN bit-in-array (bit-index array)
  (multiple-value-bind (word bit)
      (TRUNCATE bit-index 32.) 
    (LDB (BYTE 1. bit) (AREF array word))))

;;;
;;; ======================================================
;;;

(DEFUN reduce-vector-descriptor (vector-descriptor)
  (let ((descriptor vector-descriptor)
	reduced-descriptor)
    (dotimes (j (length descriptor) reduced-descriptor)
      (dotimes (i (length descriptor))
	(if (and (equal (first descriptor) (third descriptor))
		 (equal (second descriptor) (fourth descriptor)))
	    (setf descriptor (cddr descriptor))
	    (if (zerop i)
		(progn (setf reduced-descriptor
			     (append reduced-descriptor (list (car descriptor))))
		       (setf descriptor (cdr descriptor))
		       (return i))
		(progn (setf reduced-descriptor
			     (append reduced-descriptor
				     (list (list (1+ i)
						 (car descriptor)
						 (cadr descriptor)))))
		       (setf descriptor (cddr descriptor))
		       (return i))))))))

;;;
;;; ======================================================
;;;

(DEFMETHOD (block-flavor :set-pixel-mask-register)
	   (descriptor)
"Set the pixel-mask-register."
  (fill pixel-mask-vector '1) 
  (setf (fill-pointer pixel-mask-vector) 0)
  (make-mask-vector pixel-mask-vector descriptor)
  (dotimes (i 1024.) (set-bit-in-array (bit pixel-mask-vector (- 1023. i))
				      i
				      pixel-mask-register))
  (SEND self :pixel-mask-register))

(defun set-bit-in-array (value bit-index array)
  (multiple-value-bind (word bit)
      (truncate bit-index 32.) 
    (setf (aref array word) (dpb value (byte 1. bit) (aref array word)))))

(DEFUN make-mask-vector (vector descriptor) 
  (cond ((null descriptor) vector)
	((atom (cadar descriptor))
	 (let ((val (cadar descriptor)))
	   (dotimes (i (caar descriptor))
	     (vector-push val vector))
	   (make-mask-vector vector (cdr descriptor))))
	(t (dotimes (i (caar descriptor))
	     (make-mask-vector
	       vector
	       (list (cadar descriptor)
		     (caddar descriptor))))
	   (make-mask-vector vector (cdr descriptor)))))








