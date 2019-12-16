;;; -*- Syntax: Common-lisp; Package: User -*-

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

(defpackage :dwim
  (:shadow *default-server-path*
	   find-restart
	   handler-bind
	   handler-case
	   ignore-errors
	   interactive-stream-p
	   invoke-restart
	   parse-error
	   restart-case
	   with-simple-restart
   )
  (:use cl))

(defpackage :dwim-clim-util
  (:use cl))


;; (:import clim:present-to-string clim:presentation-type clim:present)
;; (:import dw:present-to-string dw:presentation-type dw:present scl:send)
;; (clim:boolean clim:alist-member clim:expression clim:command)
;; (dw:boolean dw:alist-member sys:expression cp:command)
;;  (sys:downward-funarg sys:downward-function sys:array-register) 'dwim)

;;  (proclaim '(declaration
;; March 1989, X3J13 votes to subsume downward-funarg & downward-function
;; by dynamic-extent.  Get rid of the next two eventually.  jpm.
;; dwim::downward-funarg dwim::downward-function
;; dwim::dynamic-extent
;; dwim::array-register)))
