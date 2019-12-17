;;; -*- Mode:Common-Lisp; Package:TV; Fonts:(CPTFONT HL12B HL12BI); Base:10 -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


#| This file contains Window System  methods which support the reading  of Common
Lisp character objects from windows.  The  Common Lisp manual only describes  one
function, READ-ANY, which will return a character object when read from a stream.
The naming conventions  used are  similar to  the current  Window System  methods
which  return  fixnums  instead  of  character  objects.   The  method  :READ-ANY
corresponds to the method :ANY-TYI.

When deciding whether to use a function or the equivalent method, the  preference
should be the  function, since  it will  be faster.   The method  is supplied for
those cases where  it is  difficult to  call the  function, and  will be somewhat
slower then its functional equivalent.  |#

(DEFMETHOD (STREAM-MIXIN :READ-ANY) (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERRORP T) EOF-VALUE RECURSIVE-P)
  (READ-ANY STREAM EOF-ERRORP EOF-VALUE RECURSIVE-P))

(DEFMETHOD (STREAM-MIXIN :READ-LIST) (&OPTIONAL (*STANDARD-INPUT*) (EOF-ERRORP T) EOF-VALUE RECURSIVE-P)
  (READ-LIST *STANDARD-INPUT* EOF-ERRORP EOF-VALUE RECURSIVE-P))

(DEFMETHOD (STREAM-MIXIN :READ-MOUSE-OR-KBD)
           (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERRORP T) EOF-VALUE RECURSIVE-P)
  (READ-MOUSE-OR-KBD STREAM EOF-ERRORP EOF-VALUE RECURSIVE-P))

(DEFMETHOD (STREAM-MIXIN :READ-MOUSE-OR-KBD-NO-HANG)
           (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERRORP T) EOF-VALUE RECURSIVE-P)
  (READ-MOUSE-OR-KBD-NO-HANG STREAM EOF-ERRORP EOF-VALUE RECURSIVE-P))

(DEFMETHOD (STREAM-MIXIN :READ-ANY-NO-HANG)
           (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERRORP T) EOF-VALUE RECURSIVE-P)
  (READ-ANY-NO-HANG STREAM EOF-ERRORP EOF-VALUE RECURSIVE-P))

(DEFMETHOD (STREAM-MIXIN :UNREAD-ANY) (CHAR &OPTIONAL (STREAM *STANDARD-INPUT*))
  (UNREAD-ANY CHAR STREAM))
