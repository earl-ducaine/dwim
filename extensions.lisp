;;; -*- Syntax: Common-lisp; Package: DWIM -*-

;; Copyright (c) 1987-1993 by BBN Systems and Technologies,
;; A Division of Bolt, Beranek and Newman Inc.
;; All rights reserved.
;;
;; Permission to use, copy, modify and distribute this software and
;; its documentation is hereby granted without fee, provided that the
;; above copyright notice of BBN Systems and Technologies, this
;; paragraph and the one following appear in all copies and in
;; supporting documentation, and that the name Bolt Beranek and Newman
;; Inc. not be used in advertising or publicity pertaining to
;; distribution of the software without specific, written prior
;; permission. Any distribution of this software or derivative works
;; must comply with all applicable United States export control laws.
;;
;; BBN makes no representation about the suitability of this software
;; for any purposes.  It is provided "AS IS", without express or
;; implied warranties including (but not limited to) all implied
;; warranties of merchantability and fitness for a particular purpose,
;; and notwithstanding any other provision contained herein.  In no
;; event shall BBN be liable for any special, indirect or
;; consequential damages whatsoever resulting from loss of use, data
;; or profits, whether in an action of contract, negligence or other
;; tortuous action, arising out of or in connection with the use or
;; performance of this software, even if BBN Systems and Technologies
;; is advised of the possiblity of such damages.


(in-package :dwim)


;;; Lisp Extensions


;; (unless (fboundp 'ignore)
;;   ;; Define IGNORE to be like our old friend from Genera.
;;   ;; This practice is frowned upon because IGNORE is in the
;;   ;; common lisp package (it is a declaration) and changing
;;   ;; anything about those symbols is frowned upon.  So we
;;   ;; should learn to live without this old friend some day.
;;   #FEATURE-CASE
;;   ((:allegro
;;     (excl:without-package-locks
;;      (setf (symbol-function 'ignore)
;;        #'(lambda (&rest args)
;; 	   (declare (ignore args) (dynamic-extent args))
;; 	   nil))))
;;    ((not allegro)
;;     (unless (fboundp 'ignore)
;;       (setf (symbol-function 'ignore)
;; 	#'(lambda (&rest args)
;; 	    (declare (ignore args) (dynamic-extent args))
;; 	    nil))))))

(defmacro with-rem-keywords ((new-list list keywords-to-remove) &body body)
  `(let ((,new-list (with-rem-keywords-internal ,list ,keywords-to-remove)))
    ,@body))

(defun with-rem-keywords-internal (keyword-list keywords-to-remove)
  ;; Remove leading keywords.
  (loop (unless (member (car keyword-list) keywords-to-remove)
	  (return))
	(setf keyword-list (cddr keyword-list)))
  (when keyword-list
    (do* ((kwl keyword-list cons2)
	  (cons1 (cdr kwl) (cdr kwl))
	  (cons2 (cdr cons1) (cdr cons1)))
	 ((null cons2) keyword-list)
      (when (member (car cons2) keywords-to-remove)
	(setf (cdr cons1) (cddr cons2))
	(setf cons2 kwl)))))

(defun rem-keywords (list keywords-to-remove)
  (with-rem-keywords (new-list list keywords-to-remove)
    (copy-list new-list)))

;;;Still need Genera
(eval-when (compile eval load)
  #+lucid (import '(#-clim-1.0 lcl:*load-pathname* lcl:*source-pathname*))
  #+allegro (import 'excl:*source-pathname*))

#1feature-case
((:lucid
  (eval-when (compile eval load)
    ;;Just use the existing Lucid definition
    (import 'lcl:working-directory)))
 (:allegro
  (defun working-directory ()
    (excl:current-directory))
  (defsetf working-directory excl:chdir)))


;;; UNIX Environmental Support


(defun process-run-function (name-or-keywords function &rest args)
  (let* ((new-args (copy-list args)) ; in case of stack-allocation
	 (predicate
	  (if args #'(lambda () (apply function new-args)) function)))
    (CLIM-SYS:make-process predicate :name name-or-keywords)))

(defun activate-process (p)
  (clim-sys:enable-process p))

(defun deactivate-process (p)
  (clim-sys:disable-process p))

(defun kill-process (p)
  (clim-sys:destroy-process p))

(defmacro with-process-lock ((lock) &body body)
  "Grant current process exclusive access to some resource.  Wait for
   access if necessary."
  #+allegro
  `(progn
     (or ,lock (setf ,lock (mp:make-process-lock)))
     (mp:with-process-lock (,lock) ,@body))
  #+lucid
  `(lucid::with-process-lock (,lock) ,@body)
  #+genera
  (let ((me (gensym)))
    `(let ((,me scl:*current-process*))
       (if (eq ,lock ,me) (error "Lock already locked by this process."))
       (unwind-protect
	   (if (or (si:store-conditional (scl:locf ,lock) nil ,me)
		   (and (process::safe-to-process-wait-p scl:*current-process*)
			(scl:process-wait "Lock"
			  #'(lambda (locative)
			      (declare (sys:downward-function))
			      (si:store-conditional locative nil ,me))
			  (scl:locf ,lock))))
	       (when (eq ,lock ,me) ,@body))
	 (si:store-conditional (scl:locf ,lock) ,me nil)))))

;; Loop unrolling can increase the performance of big loops by 10 to
;; 30% if the body is fast relative to the price of an iteration.
;; Here is a portable version of DOTIMES that unrolls its body. The
;; argument BLOCKING must be an integer; the compiler unrolls the loop
;; BLOCKING number of times. A good number to use is 8. Avoid choosing
;; a really big integer because your compiled code will be huge.

(defmacro dotimes-unrolled
    ((var init countform blocking &optional resultform) &body body)
  (unless (integerp blocking)
    (error "To unroll this loop, ~S must be an integer." blocking))
  `(let ((,var ,init))
     (dotimes (ignore (floor ,countform ,blocking))
       ,@(let ((result nil))
	   (setq body (append body `((incf ,var))))
	   (dotimes (ignore blocking)
	     (setq result (nconc (copy-list body) result)))
	   result))
     (dotimes (ignore (mod ,countform ,blocking) ,resultform)
       ,@body)))

;; (defun roll-test (n)
;;   (let ((number 2.1))
;;     (multiple-value-bind (val time)
;; 	(the-time
;; 	  (dotimes (i n)
;; 	    (* number number)))
;;       (print val)
;;       (print time))
;;     (multiple-value-bind (val time)
;; 	(the-time
;; 	  (dotimes-unrolled (i 0 n 20)
;; 	    (* number number)))
;;       (print val)
;;       (print time))
;;     ))

;;; Zetalisp function.
(defmethod instancep ((object t)) nil)
(defmethod instancep ((object standard-object)) t)

(defun type-specifier-p (object)
  "Determine if OBJECT is a valid type specifier"
  ;; A somewhat consful implementation, but entirely portable.
  (let ((test #'(lambda (x) (typep 't x))))
    (when (or (symbolp object) (listp object))
      (multiple-value-bind (v errorp) (ignore-errors (funcall test object))
	(declare (ignore v))
	(not errorp)))))
