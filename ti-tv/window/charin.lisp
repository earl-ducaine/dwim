;;; -*- Mode:Common-Lisp; Package:TV; Fonts:(CPTFONT HL12B HL12BI); Base:10 -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 10/21/86   LGO	Replace calls to LISTP with CONSP.
;;; 10/01/86   TWE	Fixed up the blip character input (read-any and friends) functions to make sure that
;;;			mouse-button blips contain character objects.  This goes along with a change done
;;;			in corresponding methods in the stream-mixin flavor.  These functions were simplified
;;;			a bit too.


#| This file contains Window System functions which support the reading of Common
Lisp character objects from windows.  The  Common Lisp manual only describes  one
function, READ-ANY, which will return a character object when read from a stream.
The naming conventions  used are  similar to  the current  Window System  methods
which return  fixnums  instead  of  character  objects.   The  function  READ-ANY
corresponds to the method :ANY-TYI. 

Note: methods which  perform the  equivalent operation  are located  in the  file
CHARIN-STREAM.  |#

(DEFUN read-any (&optional (stream *standard-input*) (eof-errorp t) eof-value recursive-p)
  "Read one character or blip from STREAM.
If EOF-ERRORP is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE.
RECURSIVE-P is not used; it is a confusion in Common Lisp."
  (DECLARE (IGNORE recursive-p))
  (LET ((CHAR-INT-MOUSE-BUTTON-BLIP-P NIL))
    ;; Make sure that the mouse-button blips contain characters instead of fixnums.
    (DECLARE (SPECIAL CHAR-INT-MOUSE-BUTTON-BLIP-P))
    (LET ((value (SEND (IF (EQ stream t)
                           *terminal-io*
                           (IF (EQ stream nil)
                               *standard-input*
                               stream))
                       :any-tyi eof-errorp)))
      (IF (NULL value)
          eof-value
          ;;ELSE
          (IF (INTEGERP value)
              (%MAKE-POINTER dtp-character value)
              ;;ELSE
              value)))))

(DEFUN read-list (&optional (stream *standard-input*) (eof-errorp t) eof-value recursive-p)
  "Read one blip from STREAM.
If EOF-ERRORP is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE.
RECURSIVE-P is not used; it is a confusion in Common Lisp."
  (DECLARE (IGNORE recursive-p))
  (LOOP FOR VALUE = (read-any stream eof-errorp eof-value)
        WHEN (NULL VALUE)
        DO (RETURN EOF-VALUE)
        WHEN (CONSP VALUE)
        DO (RETURN VALUE)))

(DEFUN read-mouse-or-kbd (&optional (stream *standard-input*) (eof-errorp t) eof-value recursive-p)
  "Read one character or :mouse-button blip from STREAM.
If EOF-ERRORP is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE.
RECURSIVE-P is not used; it is a confusion in Common Lisp."
  (DECLARE (IGNORE recursive-p))
  (LOOP FOR value = (read-any stream eof-errorp eof-value)
        WHEN (CHARACTERP value)
        DO (RETURN value)
        WHEN (INTEGERP value)
        DO (RETURN (%MAKE-POINTER dtp-character value))
        WHEN (NULL value)
        DO (RETURN EOF-VALUE)
        WHEN (AND (CONSP value) (EQ (CAR value) :MOUSE-BUTTON))
        DO (RETURN (values (INT-CHAR (SECOND value)) value))))

(DEFUN read-mouse-or-kbd-no-hang (&optional (stream *standard-input*) (eof-errorp t) eof-value recursive-p)
  "Read one character or :mouse-button blip from STREAM.
If EOF-ERRORP is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE.
RECURSIVE-P is not used; it is a confusion in Common Lisp."
  (DECLARE (IGNORE recursive-p))
  (CONDITION-CASE-IF (NOT eof-errorp) ()
      (LET ((CHAR-INT-MOUSE-BUTTON-BLIP-P NIL))
        ;; Make sure that the mouse-button blips contain characters instead of fixnums.
        (DECLARE (SPECIAL CHAR-INT-MOUSE-BUTTON-BLIP-P))
        (LOOP FOR value = (SEND (IF (EQ stream t)
                                    *terminal-io*
                                    (IF (EQ stream nil)
                                        *standard-input*
                                        stream))
                                :any-tyi-no-hang t)
              WHEN (NULL value)
              DO (RETURN nil)
              WHEN (CHARACTERP value)
              DO (RETURN value)
              WHEN (INTEGERP value)
              DO (RETURN (%MAKE-POINTER dtp-character value))
              WHEN (AND (CONSP value) (EQ (CAR value) :MOUSE-BUTTON))
              DO (RETURN (values (INT-CHAR (SECOND value)) value))))
    (end-of-file eof-value)))

(DEFUN read-any-no-hang (&optional (stream *standard-input*) (eof-errorp t) eof-value recursive-p)
  "Read one character or blip from STREAM but don't wait.
On an interactive stream, if no input is currently buffered, NIL is returned.
If EOF-ERRORP is T (the default), EOF is an error.
Otherwise, at EOF we return EOF-VALUE.
RECURSIVE-P is not used; it is a confusion in Common Lisp."
  (DECLARE (IGNORE recursive-p))
  (CONDITION-CASE-IF (NOT eof-errorp) ()
      (LET ((CHAR-INT-MOUSE-BUTTON-BLIP-P NIL))
        ;; Make sure that the mouse-button blips contain characters instead of fixnums.
        (DECLARE (SPECIAL CHAR-INT-MOUSE-BUTTON-BLIP-P))
        (LET ((value (SEND (IF (EQ stream t)
                               *TERMINAL-IO*
                               (IF (EQ stream nil)
                                   *standard-input*
                                   stream))
                           :any-tyi-no-hang t)))
          (IF (NULL value)
              nil
              (IF (INTEGERP value)
                  (%MAKE-POINTER dtp-character value)
                  value))))
    (end-of-file eof-value)))

(DEFUN unread-any (char &optional (stream *standard-input*))
  "Put CHAR back in STREAM to be read out again as the next character or blip.
CHAR must be the same character or blip last read from STREAM,
or this may not work or might even signal an error."
  (SEND (IF (EQ stream t)
	    *terminal-io*
	    (IF (EQ stream nil)
		*standard-input*
		stream))
	:untyi (IF (CHARACTERP char)
		   (CHAR-INT char)
		   char))
  char)
