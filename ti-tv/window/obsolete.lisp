;;; -*- Mode:Common-lisp;  Package: TV; Base:10.; Fonts: (CPTFONT CPTFONTB HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1985- 1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

#|
This file contains old obsolete window system code.
Versatec from BASSTR 2/18/86.
User options from CHOICE 2/20/86.
|#

;;				Change History
;;
;;  Date	Author	Description
;; -------------------------------------------------------------------------------------
;;;  7/2/87   PMH    Modified choose-user-options to get rid of compiler warnings
;;  3/12/87	GRH	Added compiler:make-obsolete forms for all functions in this file.
;; 11/25/86	TWE	Removed the Versatec code since it is being moved in with the
;;			rest of the printer code.
;; 10/31/86	TWE	Changed DELQ to DELETE.
;;  6/14/86	TWE	Commented out the GLOBALIZE form for USER-OPTIONS.
;;  6/11/86	TWE	Changed the OPEN form in versa-write-bit-file to conform to the
;;			Common Lisp standard.

;; from file MOUSE.

(DEFUN MOUSE-BUTTON-ENCODE (BD)
  "This is obsolete.  This returns fixnums instead of characters.
 See MOUSE-CHARACTER-BUTTON-ENCODE."
  (CHAR-INT (MOUSE-CHARACTER-BUTTON-ENCODE (int-char bd))))

;; user options from CHOICE

;;; User program macro interface

(DEFVAR *ALL-USER-OPTION-ALISTS* () "A list of all user option alists")

(DEFMACRO DEFINE-USER-OPTION-ALIST (ALIST &OPTIONAL CONSTRUCTOR DOCUMENTATION)
  "Define ALIST as a user option alist for CHOOSE-USER-OPTIONS.
DOCUMENTATION is documentation of the variable.
CONSTRUCTOR is an unrecommended feature."
  `(PROGN 'COMPILE
    ,(AND CONSTRUCTOR
       `(DEFMACRO ,CONSTRUCTOR (OPTION DEFAULT &OPTIONAL NAME TYPE &REST ARGS)
	  `(DEFVAR-USER-OPTION ,OPTION ,DEFAULT () ,',ALIST ,NAME ,TYPE ,@ARGS)))
    (DEFVAR ,ALIST NIL ,DOCUMENTATION)
    (SETQ *ALL-USER-OPTION-ALISTS* (CONS ',ALIST (DELETE ',ALIST *ALL-USER-OPTION-ALISTS* :TEST #'EQUAL)))))

(DEFINE-USER-OPTION-ALIST USER-OPTIONS NIL
  "Input and display user options")
;;;(GLOBALIZE 'USER-OPTIONS)  ;;User options don't just happen in the TV package

(DEFMACRO DEFVAR-USER-OPTION (OPTION DEFAULT DOCUMENTATION ALIST
                              &OPTIONAL NAME TYPE &REST ARGS
                              &AUX REALNAME REALTYPE)
  "Define variable OPTION and put it on ALIST, for CHOOSE-USER-OPTIONS.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST.
DEFAULT and DOCUMENTATION are used to DEFVAR OPTION.
NAME is a string to use to label the line for OPTION in
 the choose-variable-values window, instead of OPTION's actual name.
TYPE is a keyword for choose-variable-values windows, and
 ARGS are arguments (evaluated at definition time) for that keyword."
  (IF (STRINGP NAME)
      (SETQ REALNAME NAME REALTYPE TYPE)
      (SETQ REALTYPE NAME REALNAME TYPE))
  `(PROGN 'COMPILE
    (DEFINE-USER-OPTION-1 ',OPTION ',ALIST ,DEFAULT ',(OR REALTYPE :SEXP)
			  ',(OR REALNAME (MAKE-OPTION-NAME OPTION)) ,@ARGS)
    (DEFVAR ,OPTION ,DEFAULT ,DOCUMENTATION)))

(DEFUN MAKE-OPTION-NAME (STRING)
  (STRING-CAPITALIZE-WORDS (STRING-TRIM #\* STRING)))

(DEFMACRO DEFINE-USER-OPTION ((OPTION ALIST) DEFAULT &OPTIONAL TYPE NAME &REST ARGS)
  `(PROGN 'COMPILE
    (DEFINE-USER-OPTION-1 ',OPTION ',ALIST ,DEFAULT ',(OR TYPE :SEXP)
			  ',(OR NAME (MAKE-OPTION-NAME OPTION)) ,@ARGS)
    (DEFVAR ,OPTION ,DEFAULT)))

(DEFUN DEFINE-USER-OPTION-1 (OPTION ALIST DEFAULT TYPE NAME &REST ARGS)
  (SETF (GET OPTION 'DEFAULT-VALUE) DEFAULT)
  (LET ((ELEM (ASSOC OPTION (SYMBOL-VALUE ALIST) :TEST #'EQ)))
    (AND ELEM (SET ALIST (DELETE ELEM (THE LIST (SYMBOL-VALUE ALIST)) :TEST #'EQ))))
  (PUSH (LIST* OPTION NAME TYPE (COPY-LIST ARGS))
        (SYMBOL-VALUE ALIST)))

(DEFUN RESET-USER-OPTIONS (ALIST)
  "Reset all variables specified by ALIST to their default values.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST
and variables put on it with DEFVAR-USER-OPTION."
  (DO ((X ALIST (CDR X))
       (SYM))
      ((NULL X))
    (SETQ SYM (CAAR X))
    (SET SYM (GET SYM 'DEFAULT-VALUE))))

(DEFUN CHOOSE-USER-OPTIONS (&OPTIONAL ALIST &REST ARGS &AUX LABEL)
  "Invoke a pop-up choose-variable-values window on variables specified by ALIST.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST
and variables put on it with DEFVAR-USER-OPTION.
If ALIST is not specified, the user may select a
user-option-alist from a pop up menu"
  (UNLESS ALIST (SETQ ALIST
		      ;; TV:menu-choose is obsloete but is appropriate in this case
		      ;; 7/2/87  PMH
		      (inhibit-style-warnings
			      (MENU-CHOOSE *ALL-USER-OPTION-ALISTS*))))
  (WHEN (SYMBOLP ALIST)
    (SETQ LABEL (LIST :LABEL (DOCUMENTATION ALIST))
          ALIST (SYMBOL-VALUE ALIST)))
  (UNLESS ARGS
    (SETQ ARGS (APPEND LABEL
                       `(:MARGIN-CHOICES
                          ("Exit" ("Revert to default"
                                   (PROGN (RESET-USER-OPTIONS ',ALIST)
                                          (WHEN (VARIABLE-BOUNDP WINDOW)
                                            (SEND WINDOW ':REFRESH)))))))))
  (WHEN ALIST (APPLY #'CHOOSE-VARIABLE-VALUES ALIST ARGS)))


(DEFUN WRITE-USER-OPTIONS (ALIST STREAM &AUX (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T)
                           (SI:PRINT-READABLY T))
  "Write forms on STREAM to set all non-default variables on ALIST to their current values.
That is, for each variable on ALIST whose current value is not its default,
a LOGIN-SETQ form is output to STREAM which records the variable's current value.
ALIST may be one list, or a list of alists.
If ALIST is NIL, it defaults to *all-user-option-alists*"
  (UNLESS ALIST (SETQ ALIST *ALL-USER-OPTION-ALISTS*))
  (WHEN (ATOM (CAAR ALIST)) (SETQ ALIST (LIST ALIST)))
  (DOLIST (ALIST ALIST)
    (WHEN (SYMBOLP ALIST) (SETQ ALIST (SYMBOL-VALUE ALIST)))
    (DO ((ALIST ALIST (CDR ALIST))
	 (OPTION) (DEFAULT) (VALUE))
	((NULL ALIST))
      (SETQ OPTION (CAAR ALIST)
            DEFAULT (GET OPTION 'DEFAULT-VALUE)
            VALUE (SYMBOL-VALUE OPTION))
      (OR (EQUAL VALUE DEFAULT)
	  (GRIND-TOP-LEVEL `(LOGIN-SETQ ,OPTION
                                        ,(IF (OR (NUMBERP VALUE) (MEMBER VALUE '(T NIL) :TEST #'EQ)
                                                 ) VALUE `',VALUE))
                           95. STREAM)))))

;;; Site dependent versions
(DEFMACRO DEFVAR-SITE-USER-OPTION (OPTION KEYWORD DOCUMENTATION ALIST
                                   &OPTIONAL NAME TYPE &REST ARGS
                                   &AUX REALNAME REALTYPE)
  "Define site-dependent variable OPTION and put it on ALIST, for CHOOSE-USER-OPTIONS.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST.
KEYWORD is a site option name whose value,
 according to current site configuration, will be the default for OPTION.
DOCUMENTATION is documentation for OPTION as a variable (a la DEFVAR).
NAME is a string to use to label the line for OPTION in
 the choose-variable-values window, instead of OPTION's actual name.
TYPE is a keyword for choose-variable-values windows, and
 ARGS are arguments (evaluated at definition time) for that keyword."
  (IF (STRINGP NAME)
      (SETQ REALNAME NAME REALTYPE TYPE)
      ;;ELSE
      (SETQ REALTYPE NAME REALNAME TYPE))
  `(PROGN 'COMPILE
    (DEFINE-USER-OPTION-1 ',OPTION ',ALIST () ',(OR REALTYPE :SEXP)
			  ',(OR REALNAME (MAKE-OPTION-NAME OPTION)) ,@ARGS)
    (DEFVAR ,OPTION :UNBOUND ,DOCUMENTATION)
    (ADD-INITIALIZATION ,(FORMAT () "SITE:~A" OPTION)
			'(RESET-USER-OPTION ',OPTION (GET-SITE-OPTION ',KEYWORD))
                        '(SITE-OPTION))))

(DEFMACRO DEFINE-SITE-USER-OPTION ((OPTION ALIST) KEYWORD &OPTIONAL TYPE NAME &REST ARGS)
  `(PROGN 'COMPILE
    (DEFINE-USER-OPTION-1 ',OPTION ',ALIST () ',(OR TYPE :SEXP)
			  ',(OR NAME (MAKE-OPTION-NAME OPTION)) ,@ARGS)
    (DEFVAR ,OPTION)
    (ADD-INITIALIZATION ,(FORMAT () "SITE:~A" OPTION)
			'(RESET-USER-OPTION ',OPTION (GET-SITE-OPTION ',KEYWORD))
                        '(SITE-OPTION))))

;;; Change the default value of an option
(DEFUN RESET-USER-OPTION (OPTION VALUE)
  "Set user-option variable OPTION to VALUE, and make VALUE the new default."
  (SET OPTION VALUE)
  (SETF (GET OPTION 'DEFAULT-VALUE) VALUE))

;;; A :MENU-ALIST type variable whose alist changes
(DEFMACRO DEFVAR-SITE-ALIST-USER-OPTION (OPTION DEFAULT DOCUMENTATION ALIST NAME MENU-ALIST)
  "Define variable OPTION and put it on ALIST, for CHOOSE-USER-OPTIONS.
ALIST may have been defined by DEFINE-USER-OPTION-ALIST.
MENU-ALIST is a menu item list of possible values of OPTION;
 but each item is actually valid only if a corresponding site option's
 value (in the current site configuration) is non-NIL.
 The site option name is by default the same as the menu item value;
  but the item may contain :SITE-KEYWORD followed by a site option name.
DEFAULT is the name of a site keyword whose value, in the current
 site configuration, is the default value of OPTION.
DOCUMENTATION is documentation for OPTION as a variable (a la DEFVAR).
NAME is a string to use to label the line for OPTION in
 the choose-variable-values window, instead of OPTION's actual name."
  `(PROGN 'COMPILE
    (DEFINE-USER-OPTION-1 ',OPTION ',ALIST () ':MENU-ALIST
                          ',(OR NAME (MAKE-OPTION-NAME OPTION))
			  ,MENU-ALIST)
    (DEFVAR ,OPTION :UNBOUND ,DOCUMENTATION)
    (ADD-INITIALIZATION ,(FORMAT () "SITE:~A" OPTION)
			'(RESET-ALIST-USER-OPTION ',OPTION ,ALIST ,MENU-ALIST
                                                  ',DEFAULT)
			'(SITE-OPTION))))

;;; A :MENU-ALIST type variable whose alist changes
(DEFMACRO DEFINE-SITE-ALIST-USER-OPTION ((OPTION ALIST) NAME MENU-ALIST &OPTIONAL DEFAULT)
  `(PROGN 'COMPILE
    (DEFINE-USER-OPTION-1 ',OPTION ',ALIST NIL ':MENU-ALIST
                          ',(OR NAME (MAKE-OPTION-NAME OPTION))
			  ,MENU-ALIST)
    (DEFVAR ,OPTION)
    (ADD-INITIALIZATION ,(FORMAT () "SITE:~A" OPTION)
			'(RESET-ALIST-USER-OPTION ',OPTION ,ALIST ,MENU-ALIST
                                                  ',DEFAULT)
			'(SITE-OPTION))))

(DEFUN RESET-ALIST-USER-OPTION (OPTION ALIST MENU-ALIST DEFAULT)
  (AND DEFAULT
       (SETQ DEFAULT (GET-SITE-OPTION DEFAULT)))
  (LOOP FOR ELEM IN MENU-ALIST
        AS SITE-KEYWORD = (OR (AND (CONSP (CDR ELEM)) (GET ELEM :SITE-KEYWORD))
                              (MENU-EXECUTE-NO-SIDE-EFFECTS ELEM))
	AS DEFAULT-SITE-KEYWORD = (OR (AND (CONSP (CDR ELEM))
                                           (GET ELEM :DEFAULT-SITE-KEYWORD))
                                      SITE-KEYWORD)
        WHEN (NOT (NULL (GET-SITE-OPTION SITE-KEYWORD)))
        COLLECT ELEM INTO NEW-ALIST
        WITH DEFAULT-ELEM
        WHEN (AND (EQ DEFAULT-SITE-KEYWORD DEFAULT) (NULL DEFAULT-ELEM))
        DO (SETQ DEFAULT-ELEM ELEM)
        FINALLY (AND DEFAULT-ELEM (SETQ NEW-ALIST (CONS DEFAULT-ELEM
                                                        (DELETE DEFAULT-ELEM (THE LIST NEW-ALIST) :TEST #'EQ))))
	        (SETQ MENU-ALIST NEW-ALIST))
  (LET ((ELEM (ASSOC OPTION ALIST :TEST #'EQ)))
    (SETF (FOURTH ELEM) MENU-ALIST))
  (RESET-USER-OPTION OPTION (AND MENU-ALIST
                                 (MENU-EXECUTE-NO-SIDE-EFFECTS (CAR MENU-ALIST)))))

(DEFMACRO RESTRICT-USER-OPTION (OPTION RESTRICTION-TYPE &REST SITE-KEYWORDS &AUX IF IF-NOT)
  "Allow site tables to control whether OPTION is significant.
OPTION should be a user-option variable (see DEFVAR-USER-OPTION).
RESTRICTION-TYPE should be :IF, :UNLESS or :NEVER.
 :IF means offer OPTION to the user only if one of
  SITE-KEYWORDS is non-NIL in the current site configuration.
 :UNLESS means offer OPTION only if none of SITE-KEYWORDS
  are non-NIL in the current site configuration.
 :NEVER means never offer OPTION to the user.
Restrictions are actually implemented by TV:PRUNE-USER-OPTION-ALIST;
to avoid presenting suppressed options to the user, you must
call that function."
  (SETQ SITE-KEYWORDS (COPY-LIST SITE-KEYWORDS))
  (CASE RESTRICTION-TYPE
    (:IF (SETQ IF SITE-KEYWORDS))
    (:UNLESS (SETQ IF-NOT SITE-KEYWORDS))
    (:NEVER (SETQ IF-NOT T)))
  `(DEFPROP ,OPTION ,(OR IF IF-NOT)
     ,(IF IF 'SITE-KEYWORDS-RESTRICTION 'NOT-SITE-KEYWORDS-RESTRICTION)))

;;; This removes all user options that are restricted or choices with less than two
;;; possibilities.
(DEFUN PRUNE-USER-OPTION-ALIST (ALIST)
  "Return a subset of ALIST eliminating trivial choices.
Options on ALIST that are restricted, or that have only one valid
alternative, are omitted from the value returned.  Usage:
 (CHOOSE-USER-OPTIONS (PRUNE-USER-OPTION-ALIST ALIST))."
  (LOOP FOR ELEM IN ALIST
        AS OPTION = (CAR ELEM)
        WITH TEM
        UNLESS (OR (AND (NOT (NULL (SETQ TEM (GET OPTION 'NOT-SITE-KEYWORDS-RESTRICTION))))
                        (OR (EQ TEM T)
                            (LOOP FOR KEY IN TEM
                                  THEREIS (GET-SITE-OPTION KEY))))
                   (AND (NOT (NULL (SETQ TEM (GET OPTION 'SITE-KEYWORDS-RESTRICTION))))
                        (NOT (LOOP FOR KEY IN TEM
                                   ALWAYS (GET-SITE-OPTION KEY))))
                   (AND (MEMBER (THIRD ELEM) '(:ASSOC :MENU-ALIST) :TEST #'EQ)
                        (NULL (CDR (FOURTH ELEM)))))
	COLLECT ELEM)) 


;; Define these functions as obsolete.

(COMPILER:MAKE-OBSOLETE PRUNE-USER-OPTION-ALIST "user options are no longer supported")
(COMPILER:MAKE-OBSOLETE RESET-ALIST-USER-OPTION "user options are no longer supported")
(COMPILER:MAKE-OBSOLETE RESET-USER-OPTION "user options are no longer supported")
(COMPILER:MAKE-OBSOLETE WRITE-USER-OPTIONS "user options are no longer supported")
(COMPILER:MAKE-OBSOLETE CHOOSE-USER-OPTIONS "user options are no longer supported")
(COMPILER:MAKE-OBSOLETE RESET-USER-OPTIONS "user options are no longer supported")
(COMPILER:MAKE-OBSOLETE DEFINE-USER-OPTION-1 "user options are no longer supported")
(COMPILER:MAKE-OBSOLETE MAKE-OPTION-NAME "user options are no longer supported")

(COMPILER:MAKE-OBSOLETE MOUSE-BUTTON-ENCODE
			"use MOUSE-CHARACTER-BUTTON-ENCODE which returns characters not fixnums")

