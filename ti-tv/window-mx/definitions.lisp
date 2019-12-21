
;;; Restricted rights legend

;;; Use, duplication, or disclosure by the Government is subject to
;;; restrictions as set forth in subdivision (c)(1)(ii) of the Rights
;;; in Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights
;;; reserved.


;;; This file contains structure definitions, macros, and functions
;;; that either are now or might at some future time be proclaimed to
;;; be INLINE.  The defsystem forces this file to be compiled and
;;; loaded before any other files.  A change in this file forces the
;;; recompilation of all other files, so expect to wait awhile if you
;;; change this file.

;;;                     Patch
;;;  Date  Author       Number Description
;;;
;;;  02-14-89   LG      5-36   Added TIME-RCVD slot to CBUFFER structure.
;;;  10-16-88   LG      5-18   Redefined remember-call to maintain a circular
;;;                            list of the calls to it.
;;;  10-16-88   LG	5-17   Added definition for CBUFFER structure.
;;;  09-13-88   LG      5-10   Changed setup-this-window-on-the-mac to locally
;;;  			       bind the list of undisplaced arrays for
;;;  			       debugging purposes.

;; ab 1/6/88

(in-package :mac-windows)

(proclaim '(function report-all-frames))

(defmacro tv::mac-explorer-window-id (window)
  `(tv:sheet-window-id ,window))

(defsetf tv::mac-explorer-window-id (window) (x)
  `(setf (tv:sheet-window-id ,window) ,x))

(defstruct (cbuffer (:conc-name cbuffer-)
		    (:type (vector art-32b))
		    ;; (:make-array '(:dimensions 8192 :type art-32b))
		    )
  n-segments-rcvd
  n-segments-total
  n-words-per-segment
  start-fchain
  start-bchain
  time-rcvd)

(defun syz2 (arg-names-and-values to-a-depth-of)
  (rplaca *calls* (cons (time:get-universal-time)
			(cons arg-names-and-values
			      (report-all-frames nil to-a-depth-of))))
  (setf *calls* (rest *calls*)))

(defmacro remember-call (&rest args)
  "Maintains the list *calls* as a circular history of the stack and
   the values of ARGS as of its calls.  ARGS must all be symbols;
   their names and values will be remembered.  If the first of ARGS is
   an integer, then it is used as a limit on the stack depth
   remembered, otherwise all levels are remembered."
  (let ((to-a-depth-of *default-stack-depth-to-record*)
	(debugging-class t)
	 arg-names)
    ;;  If 1st arg is a number use it as the depth to which the stack
    ;;  is to be reported...
    (when (and (> (length args) 0) (integerp (first args)))
      (setf to-a-depth-of (first args))
      (setf args (rest args)))
    ;; If 1st arg is a keyword then it specifies this call's debugging
    ;; class.  Add it to the list of legal debugging classes.
    (when (and (> (length args) 0)
	       (symbolp (first args))
	       (keywordp (first args)))
      (setf debugging-class (first args))
      (setf args (rest args))
      (unless (member debugging-class *all-debugging-classes*)
	(push debugging-class *all-debugging-classes*)))
    (if args
	(progn
	  (setf arg-names (loop for arg in args
				collecting (format nil "~s=" arg)
				collecting arg))
	  `(without-interrupts
	     (unless (or *tracing-off* (suppress-tracing-p ,debugging-class))
	       (let ((%v% (list ,@arg-names)))
		 (loop for arg-tail first (cdr %v%)
		       then (cddr arg-tail)
		       while arg-tail
		       do (setf (first arg-tail)
				(typecase (first arg-tail)
				  (symbol (symbol-value (first arg-tail)))
				  (cons (if (functionp (first (first arg-tail)))
					    (eval (first arg-tail))
					  (first arg-tail)))
				  (t (first arg-tail)))))
		 (syz2 %v% ,to-a-depth-of)))))
      ;; else...
      `(without-interrupts
	 (unless (or *tracing-off* (suppress-tracing-p ,debugging-class))
	   (syz2 nil ,to-a-depth-of))))))

(DEFVAR remember-call-regardless nil)
(DEFMACRO remember-call-regardless (&rest args)
  `(LET ((remember-call-regardless t))
     (remember-call ,@args)))

(DEFMACRO save-an-image-of (&rest args)
  `(UNLESS (suppress-tracing-p)
     (LET (image image-list)
       (DOLIST (arg (LIST ,@args))
	 (TYPECASE arg
	   (ARRAY (SETF image (MAKE-ARRAY (ARRAY-DIMENSIONS arg)
					  :element-type (ARRAY-ELEMENT-TYPE arg)
					  :leader-length (ARRAY-LEADER-LENGTH arg)))
		  (COPY-ARRAY-CONTENTS-AND-LEADER arg image))
	   (LIST (SETF image (COPY-TREE arg)))
	   (t (SETF image arg)))
	 (PUSH image image-list))
       (RPLACA *calls* (CONS (time:get-universal-time)
			(CONS nil (FORMAT nil "Saved images of~{ ~s~} in slot ~d."
					',args (LENGTH *images*)))))
       (SETF *calls* (REST *calls*))
       (VECTOR-PUSH-EXTEND (NREVERSE image-list) *images*))))


;;;   Use the right definition for the Mac stuff at least...
(DEFMACRO with-clipping-rectangle ((left top right bottom) &body body)
  "Execute body with the clipping rectangle bound to the intersection
   of the current clipping rectangle and the clipping rectangle
   specified by left, top, right, and bottom, which should all be
   integers. Microcode will not draw outside of the edges specified
   by the clipping rectangle."
  `(let
     ((sys:*clipping-rectangle-left-edge*
	(max sys:*clipping-rectangle-left-edge* ,left))
      (sys:*clipping-rectangle-top-edge*
	(max sys:*clipping-rectangle-top-edge* ,top))
      (sys:*clipping-rectangle-right-edge*
	(min sys:*clipping-rectangle-right-edge* ,right))
      (sys:*clipping-rectangle-bottom-edge*
	(min sys:*clipping-rectangle-bottom-edge* ,bottom)))
     . ,body))

(DEFUN make-bit-array-Mac-resident (window &optional contents-matter-p)
  ;;  Note that the following two statements are equivalent:
  ;;     1. The bit array being drawn to by window W1's is not Mac resident.
  ;;     2. Window W1's redirection is not known on the Mac.
  (LET ((window-id (tv:Mac-explorer-window-id window)))

    ;;  Handle a deactivated window...
    (WHEN (EQ t window-id)
      (SETF window-id (give-a-window-an-id window)))

    ;;  Iff the Mac has no cognizance of this window's redirection, tell it, in
    ;;  the process using send-redirect-drawing to get the bit-array
    ;;  Mac resident if necessary...
    (WHEN (AREF *has-the-Mac-forgotten-my-redirection?* window-id)
      (send-redirect-drawing window contents-matter-p))
    window-id))


;;;  For debugging purposes, make and use a local copy of the pointer to the list of
;;;  undisplaced Mac-window arrays so we can see from the debugger whether or not it is
;;;  momentarily sick.
(DEFUN setup-this-window-on-the-Mac (ARRAY &optional but-dont-copy)

  (LET (array-displaced-to offset array-and-window window window-id
	(list-of-undisplaced-Mac-window-arrays *undisplaced-Mac-window-arrays*))

    ;;  Get the ultimately-displaced-to-array first...
    (MULTIPLE-VALUE-SETQ (array-displaced-to offset)
      (chase-displaced-pointers array))

    ;;  See it if is the buffer of a Mac-resident screen or
    ;;  the bit-array of a window possessing a Mac-resident bit array...
    (SETF array-and-window
	  (ASSOC array-displaced-to list-of-undisplaced-Mac-window-arrays :test 'EQ))

    ;;
    (IF array-and-window
      (MULTIPLE-VALUE-BIND (y-offset x-offset)
	  (FLOOR offset (ARRAY-DIMENSION array-displaced-to 1))
	(SETF window (REST array-and-window)
	      window-id (tv:Mac-explorer-window-id window))
	(WHEN (EQ window-id t)
	  (SETF window-id (give-a-window-an-id window)))
	(UNLESS but-dont-copy
	  (move-a-windows-bit-array-to-the-mac window-id))
	(VALUES window-id x-offset y-offset))
      ;; else...
      (VALUES array 0 0))))




(defflavor mac-screen
	   ()
	   (tv:standard-screen)
  :settable-instance-variables)

(DEFFLAVOR Mac-window
	   ()
	   (tv:window)
  :settable-instance-variables)


(DEFSTRUCT (draw-char-cache-state (:type list)
				     (:callable-constructors nil))
  (next-x 0)
  (window nil)
  (start-y 0)
  (alu w:alu-ior)
  (font nil)
  (top-clip 0) (bottom-clip 2000) (left-clip 0) (right-clip 2000)
  (dont-clip-at-margins nil))

;;; (make-draw-char-cache-state :window w :start-x x :start-y y)

(DEFSTRUCT (draw-char-cache
	      (:type list)
	      (:default-pointer *draw-char-cache*))
  (state nil)				   ; NIL or a draw-char-cache-state structure.
  (start-x 0)
  (string (MAKE-ARRAY 256 :element-type 'STRING-CHAR :fill-pointer 0)))


(DEFSTRUCT (explorer-screen
	      (:type list)
	      (:conc-name nil))
  (the-screen nil)
  (the-last-selected-window nil))


;;; ab 1/6/88
;;; These used to be in obsolete REDEFINE-%-PRIMITIVES file.


(DEFCONSTANT white-array (MAKE-ARRAY '(4 32) :element-type 'BIT
				:initial-element 0))

(DEFCONSTANT black-array (MAKE-ARRAY '(4 32) :element-type 'BIT
				:initial-element 1))


(DEFUN handle-forcing-of-destination-bits (alu from-array from-x from-y)
  (CASE alu
    (00 (VALUES w:alu-seta white-array 0 0))
    (03 (VALUES w:alu-xor white-array 0 0))
    (12 (VALUES w:alu-xor black-array 0 0))
    (15 (VALUES w:alu-seta black-array 0 0))
    (t
     (VALUES alu from-array from-x from-y))))

;;;08-19-88 DAB Create a special mac-window that will be used to print
;;; file to a local printer. The ID will always be 255.

(DEFFLAVOR Mac-screen-for-printer
	   ()
	   (mac:mac-screen)
  :settable-instance-variables)

(defmethod (mac-screen-for-printer :printer-screen-p) () T)

(defmethod (mac-screen-for-printer :close) (&rest ignore)
   )
