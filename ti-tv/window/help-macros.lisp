;; -*- Mode:Common-Lisp; Package:SYSTEM-INTERNALS; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 07/24/86   TWE	Moved this code out of HELP-STREAM to get correct compilation.


;;; PROCESS-DEFAULTS is for use in macros which handle keyword arguments
;;; (via KEYWORD-EXTRACT or whatever).
;;;
;;; Usage:
;;;   (PROCESS-DEFAULTS DEFAULT-LIST)
;;;
;;; DEFAULT-LIST is a list of two-element lists.  Each element of the list should
;;; contain the name of a variable to default as its CAR, and the default value
;;; as its CADR.  If the variable is NIL, its value will be set to the default.
;;;
;;; This macro-expands into a series of IF statements (with a SETQ statement as the
;;; consequent).  The variable and default-value names are copied directly into the
;;; SETQ's, so you probably want to quote the values.
;;;
;;; The DEFAULT-LIST is evaluated, so you have to quote it as well if it is constant.

(defmacro process-defaults (default-list)
  `(progn . ,(mapcar #'(lambda (default)
			 (let ((var (car default))
			       (val (cadr default)))
			   `(if (null ,var) (setq ,var ,val))))
		     (eval default-list))))

;;; WITH-HELP-STREAM sets up a stream for printing a long help message.
;;; This is a pop-up window (like FINGER windows) if the parent stream is a window,
;;; otherwise the stream is simply the parent stream (this avoids lossage if the
;;; stream is a file stream or the cold-load stream.).
;;;
;;; Usage:
;;;   (WITH-HELP-STREAM (STREAM . OPTIONS) &BODY BOD)
;;;
;;; STREAM is a symbol to assign the new stream to.  Output operations in the body
;;; (PRINT, FORMAT, etc.) should use this symbol as the stream argument.
;;;
;;; OPTIONS is a keyword argument list.  The following options are recognized:
;;;   :LABEL     Label to give the help window.  Default is a null string.
;;;   :WIDTH     Symbol to take on the value of the width in characters of the stream.
;;;              An internal symbol is used (and ignored) if none is specified.
;;;   :HEIGHT    Symbol to take on the value of the height in characters of the stream.
;;;              An internal symbol is used (and ignored) if none is specified.
;;;   :SUPERIOR  The superior stream.  Defaults to *TERMINAL-IO*.
(defmacro with-help-stream ((stream . options) &body bod
				     &aux label width height superior)
  "Execute the BODY with STREAM bound to a stream for printing help text on.
If *TERMINAL-IO* or the specified superior is a window, a special \"help window\"
is popped up and used as STREAM. 
If *TERMINAL-IO* or the specified superior is not a window, it is used directly.
OPTIONS is a list of alternating keywords and values.
 :LABEL's value is a string to use as the label of the help window if one is used.
 :WIDTH's value is a symbol to bind to the width in characters of STREAM.
  It is bound while BODY is executed.
 :HEIGHT's value is a symbol to bind to the height in characters.
 :SUPERIOR's value is a window to use as the superior of the help window."
  (keyword-extract options *with-help-iter* (label width height superior) nil)
  (process-defaults '((label "") (width '*with-help-width*)
		      (height '*with-help-height*) (superior '*terminal-io*)))
  `(let ((body-function
	   #'(LAMBDA (,stream &aux ,width ,height)
	       (if (member :size-in-characters (send ,stream :which-operations) :test #'eq)
		   (multiple-value-setq (,width ,height) (send ,stream :size-in-characters))
		 (setq ,width 85. ,height 66.))
	       . ,bod)))
     (with-help-stream-1 ,label ,superior body-function)))
