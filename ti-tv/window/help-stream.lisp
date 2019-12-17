;; -*- Mode:Common-Lisp; Package:SYSTEM-INTERNALS; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1984-1989 Texas Instruments Incorporated.  All rights reserved.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 07/24/86   TWE	Moved the macros code out of here into HELP-MACROS to get correct compilation.
;;; 07/11/86   TWE	Converted to Common Lisp and moved this from the kernel.  Also fixed up the `press
;;;			any key to continue' message to use the window system variable which contains the
;;;			default string.

(defun with-help-stream-1 (label superior body-function &aux input-p)
  (cond ((typep superior 'tv:sheet)
	 (using-resource (stream tv:pop-up-finger-window)
	   (lexpr-send stream :set-edges
		       (multiple-value-list (send superior :edges)))
	   (send stream :set-label label)
	   (tv:window-call (stream :deactivate)
	     (send stream :clear-screen)
	     (funcall body-function stream)
	     (terpri stream)
	     (terpri stream)
	     (princ tv:*remove-typeout-standard-message* stream)
	     (send stream :wait-for-input-or-deexposure)
	     ;; This hair is so that if we woke up due to deexposure
	     ;; we do not try to read anything;
	     ;; if we woke up due to input, we do not read until after
	     ;; we deactivate the help window, so that if the input is Break
	     ;; the break-loop is not entered until our normal window is usable again.
	     (if (and (send stream :exposed-p)
		      (send stream :listen))
		 (setq input-p t)))
	   (if input-p
	       (send (if (send superior :operation-handled-p :tyi-no-hang)
			 superior *terminal-io*)
		     :tyi-no-hang))))
	(t (funcall body-function superior))))
