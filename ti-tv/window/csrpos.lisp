;;; -*- Mode: Common-Lisp; Package: TV; Base: 10.; Fonts: CPTFONT,HL12B,HL12BI -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; The CURSORPOS function is present for Maclisp compatibility.
;;;Usually it is preferable to send the appropriate messages instead
;;;of using this function.
;;; No arguments returns (line . column)
;;;
;;; One argument does a variety of operations.  This is a quoted symbol
;;; which can be one of the following.
;;;   F - move one position forward.
;;;   B - move one position backward.
;;;   D - move one line down.
;;;   U - move one line up
;;;   C - clears the window
;;;   T - moves to the top left corner.  Note that if T is specified as
;;;       the last argument that a stream must be specified too.
;;;   Z - move the bottom left corner.
;;;   A - Advance to a fresh line.  See :FRESH-LINE for more details.
;;;   E - Clear from the cursor to the end of the window.
;;;   L - Clear from the cursor to the end of the line.
;;;   K - Clear the character at the current cursor position.
;;;   X - Clear the character before the current cursor position.
;;; Returns T if the operation succeeded, NIL otherwise.
;;;   
;;; Two arguments sets cursorpos to there (args there of NIL mean don't
;;;   change).  Returns T if it succeeded, NIL if it didn't.
;;; Hmm, NEWIO seems to blow out rather than returning NIL now.  Change this?

;;; If the last argument is T (meaning *TERMINAL-IO*) or a stream, then it
;;; is applied to that stream.  Otherwise it is applied to
;;; *STANDARD-OUTPUT*.  Anything other than a number or a 1-character long
;;; symbol or NIL is assumed to be a stream.

(DEFUN CURSORPOS (&REST ARGS)
  (LET ((NARGS (LENGTH ARGS))
	(STREAM *STANDARD-OUTPUT*)
	ARG1 WHICH-OPERATIONS)
    (COND ((NULL ARGS))
          ;; If any args, look for stream as 1st arg
	  ((EQ (SETQ ARG1 (CAR (LAST ARGS))) T)
           (SETQ STREAM *TERMINAL-IO*
                 NARGS (1- NARGS)))
	  ((OR (NUMBERP ARG1) (NULL ARG1)))
	  ((OR (NOT (SYMBOLP ARG1)) (> (ARRAY-TOTAL-SIZE (SYMBOL-NAME ARG1)) 1))
	   (SETQ STREAM ARG1
                 NARGS (1- NARGS))))
    (SETQ ARG1 (CAR ARGS)
	  WHICH-OPERATIONS (FUNCALL STREAM :WHICH-OPERATIONS))
    (COND ((ZEROP NARGS)
	   (IF (MEMBER :READ-CURSORPOS WHICH-OPERATIONS :TEST #'EQ)
	       (MULTIPLE-VALUE-BIND (X Y)
                   (FUNCALL STREAM :READ-CURSORPOS :CHARACTER)
		 (CONS Y X))
	       (FERROR NIL "~S stream does not support cursor positioning" STREAM)))
	  ((> NARGS 2)
           ;; Why bother signalling the correct condition?
	   (FERROR NIL "Too many arguments"))
          ;; 2 arguments or one numeric argument.
	  ((OR (> NARGS 1) (NUMBERP ARG1))
	   (IF (MEMBER :SET-CURSORPOS WHICH-OPERATIONS :TEST #'EQ)
	       (MULTIPLE-VALUE-BIND (X Y)
                   (FUNCALL STREAM :READ-CURSORPOS :CHARACTER)
		 (FUNCALL STREAM :SET-CURSORPOS
			  (OR (SECOND ARGS) X) (OR (FIRST ARGS) Y) :CHARACTER))
	       (FERROR NIL "~S stream does not support cursor positioning" STREAM)))
	  ((CHAR= (SETQ ARG1 (CHAR-UPCASE (AREF (SYMBOL-NAME ARG1) 0)))
              #\F)				;F forward space
	   (CURSORPOS-INTERNAL STREAM +1 0 #\SPACE WHICH-OPERATIONS))
	  ((CHAR= ARG1 #\B)				;B backspace
	   (CURSORPOS-INTERNAL STREAM -1 0 #\BACKSPACE WHICH-OPERATIONS))
	  ((CHAR= ARG1 #\D)				;D down a line
	   (CURSORPOS-INTERNAL STREAM 0 +1 NIL WHICH-OPERATIONS))
	  ((CHAR= ARG1 #\U)				;U up a line
	   (CURSORPOS-INTERNAL STREAM 0 -1 NIL WHICH-OPERATIONS))
	  ((CHAR= ARG1 #\C)				;C clear screen
	   (IF (MEMBER :CLEAR-SCREEN WHICH-OPERATIONS :TEST #'EQ)
               (FUNCALL STREAM :CLEAR-SCREEN)
             ;ELSE
             (FUNCALL STREAM :TYO #\FORM))
	   T)
	  ((CHAR= ARG1 #\T)				;T top of screen
	   (IF (MEMBER :HOME-CURSOR WHICH-OPERATIONS :TEST #'EQ)
               (FUNCALL STREAM :HOME-CURSOR)
             ;ELSE
             (FUNCALL STREAM :TYO #\FORM))
	   T)
	  ((CHAR= ARG1 #\E)				;E erase to end of screen
	   (COND ((MEMBER :CLEAR-EOF WHICH-OPERATIONS :TEST #'EQ)
                  (FUNCALL STREAM :CLEAR-EOF) T)))
	  ((CHAR= ARG1 #\L)				;L erase to end of line
	   (COND ((MEMBER :CLEAR-EOL WHICH-OPERATIONS :TEST #'EQ)
                  (FUNCALL STREAM :CLEAR-EOL) T)))
	  ((CHAR= ARG1 #\K)				;K erase character
	   (COND ((MEMBER :CLEAR-CHAR WHICH-OPERATIONS :TEST #'EQ)
                  (FUNCALL STREAM :CLEAR-CHAR) T)))
	  ((CHAR= ARG1 #\X)				;X erase character backward
	   (CURSORPOS 'B STREAM)
	   (CURSORPOS 'K STREAM))
	  ((CHAR= ARG1 #\Z)				;Z home down
	   (IF (MEMBER :HOME-DOWN WHICH-OPERATIONS :TEST #'EQ)
               (FUNCALL STREAM :HOME-DOWN)
             ;ELSE
             (FUNCALL STREAM :FRESH-LINE))
	   T)
	  ((CHAR= ARG1 #\A)				;A fresh line
	   (FUNCALL STREAM :FRESH-LINE)
	   T)
	  ((FERROR NIL "~C not a recognized CURSORPOS option" ARG1)))))  

(DEFUN CURSORPOS-INTERNAL
       (STREAM DX DY ALTERNATE-CHARACTER WHICH-OPERATIONS)
  (COND ((MEMBER :SET-CURSORPOS WHICH-OPERATIONS :TEST #'EQ)
	 (MULTIPLE-VALUE-BIND (X Y)
             (FUNCALL STREAM :READ-CURSORPOS :CHARACTER)
	   (FUNCALL STREAM :SET-CURSORPOS (+ X DX) (+ Y DY) :CHARACTER)
	   T))
	((NOT (NULL ALTERNATE-CHARACTER))
	 (FUNCALL STREAM :TYO ALTERNATE-CHARACTER)
	 T)
	(T NIL)))  	;Or should it give an error?
