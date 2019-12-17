;;; -*- Mode:Common-Lisp; Package:TV; Fonts:(MEDFNT HL12B HL12BI); Base:10 -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;   1/19/87  TWE	Put the DEFUN inside of a let to turn off the compiler function definition warning.
;;;			The version of this function in the kernel is more primitive, but both need to exist.
;;;  11/13/86  TWE	Changed a call to symeval to use symbol-value instead.
;;;  09/26/86  TWE	Changed ASSQ to ASSOC to be Common Lisp.
;;;  08/14/86  TWE	Finished conversion to Common Lisp and added comments.

#|
From OREN%zermatt.lcs.mit.edu@CSNET-RELAY.ARPA Tue Aug  5 12:43:46 1986
To: Craig Thompson <THOMPSON@TI-CSL.ti>
Cc: Ekberg@TI-CSL.ti, oren@TI-CSL.ti
Subject:        Re: mouse sensitive formats
Date:           6-Jul-86 08:59:57
From: OREN%zermatt.lcs.mit.edu@CSNET-RELAY.ARPA

>  LaMott, long ago you pointed me at some code that implemented mouse
>  sensitive formats (~M).  Do you still know where such code resides?

Its in SI:OREN.SYM;AUTOSCROLL-MOUSE.LISP.  It was written for Symbolics release
5.2 as part of the work I did on spread-sheet windows (for Norm McCain and
Harry Tennant).  It doesn't work on the Explorer, but I think its such a good
idea, that I converted it for you.  The code follows.

LaMott




Define a new FORMAT directive ~M

	The argument is printed with PRINC and when printed on a stream that
	includes TV:BASIC-MOUSE-SENSITIVE-ITEMS, the item printed will be made
	mouse-sensitive.  The item type defaults to the first item type in the
	window's ITEM-TYPE-ALIST.  ~nM can be used to set the item type to the
	nTH item in ITEM-TYPE-ALIST, or you can specify the item-type with a
	symbol passed through ~VM.

	~:M is a recursive call where the argument is a list containing the
	item value format-string and arguments.  This can be used to make a
	sentence mouse sensitive as well as individual words within the
	sentence.

EXAMPLE:

(defflavor mouse-sensitive-items-window ()
  (basic-mouse-sensitive-items window))

(SETQ TV:X (TV:MAKE-INSTANCE 'MOUSE-SENSITIVE-ITEMS-WINDOW
			     :item-type-alist '((default bar "Click on me!"))))

(TV:window-call (TV:X :deactivate)
  (FORMAT TV:X "~&Hi, ~M is mouse sensitive" "foo")
  
  (SEND TV:X :any-tyi)				   ;left click on "foo"
  )
=> (:TYPEOUT-EXECUTE BAR "foo")

Here's another example:

(TV:window-call (TV:X :deactivate)
  
  (FORMAT TV:X "~%~:M"
	  '(whole-sentence "This whole ~M is ~M and so are some of its ~M."
			sentence mouse-sensitive words))
  
  (SEND TV:X :any-TYI)		   ;middle click on "This whole ... its words"
  )
=> (:TYPEOUT-EXECUTE BAR WHOLE-SENTENCE)

The format command this sets up is ~M.  It works on non-mouse-sensitive
windows, but doesn't make its output mouse-sensitive.

NOTE: Currently, ~M items must not extend over more than one line.

|#

(LET ((INHIBIT-FDEFINE-WARNINGS T))
  (DEFUN (:property format:m format:format-ctl-one-arg) (ARG params)
    (LET* ((alist (SEND *standard-output* :send-if-handles :item-type-alist))
           (type (AND alist (COND ((NULL params) (CAAR alist))
                                  ((FIXNUMP (FIRST params))
                                   (FIRST (NTH (FIRST params) alist)))
                                  (t (FIRST (ASSOC (FIRST params) alist :test #'equal))))))
           (item (IF (ATOM arg) arg (FIRST arg)))
           item-list)
      (WHEN (AND alist (NULL type))
        (ERROR "Format parameter ~s does not correspond to an item-type"
               params))
      (COND (type
             (MULTIPLE-VALUE-BIND (start-x start-y)
                 (SEND *standard-output* :read-cursorpos)
               ;; The right and bottom edges are 0 because we are going to fill them in later.
               (SEND *standard-output* :primitive-item type arg
                     start-x start-y 0 0))
             (SETQ item-list (CAR (SEND *standard-output*
                                        :funcall-inside-yourself
                                        'symbol-value 'tv:item-list)))
             (COND (format:colon-flag
                    (APPLY #'FORMAT *standard-output* (CDR arg))
                    (SETF (typeout-item-item item-list) (CAR (typeout-item-item
                                                               item-list))))
                   (t (PRINC item)))
             ;; Calculate the right and bottom edges.
             (MULTIPLE-VALUE-BIND (end-x end-y)
                 (SEND *standard-output* :read-cursorpos)
               (SETF (typeout-item-right item-list) (+ end-x (typeout-item-right
                                                               item-list)))
               (SETF (typeout-item-bottom item-list)
                     (+ (+ end-y (SEND *standard-output* :line-height))
                        (typeout-item-bottom item-list)))))
            (t (IF format:colon-flag
                   (APPLY #'FORMAT *standard-output* (CDR arg))
                   (PRINC item))))))
  )
