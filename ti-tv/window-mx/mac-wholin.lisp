;;-*- Mode: COMMON-LISP; Package: TV; Base: 10.; Fonts: CPTFONT,HL12B,HL12BI -*-
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
;;; Copyright (C) 1985-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1981 Massachusetts Institute of Technology **

; Copyright (c) 1983 Texas Instruments Incorporated  All Rights Reserved 
; Copyright (c) 1989 Texas Instruments Incorporated  All Rights Reserved 

;;; 02-23-89 DAB Changed display-mac-who-line to use the new format of who-line-file-sheet.

;;; This file contains the mac support for who line  excluding run bars and who state

(PROCLAIM '(SPECIAL si:%MX-Who-File-String-Max si:%mx-mouse-doc-max))	;ab 8/18/88

(DEFPARAMETER mac-who-file-string
	      (MAKE-ARRAY (+ 1 si:%MX-Who-File-String-Max) :element-type '(unsigned-byte 8)
			  :displaced-to-physical-address
			  (dpb si:*addin-memory-slot* si:%%Nubus-F-And-Slot-Bits
			       (+ si:%Driver-Data-Start si:%DD-file-state))))

(DEFVAR last-mac-who-line-file-state "")

(DEFPARAMETER mac-who-mouse-doc-string-1
	      (MAKE-ARRAY (+ 1 si:%mx-mouse-doc-max) :element-type '(unsigned-byte 8)
			  :displaced-to-physical-address
			  (dpb si:*addin-memory-slot* si:%%Nubus-F-And-Slot-Bits
			       (+ si:%Driver-Data-Start si:%DD-mouse-doc-1))))

(DEFPARAMETER mac-who-mouse-doc-string-2
	      (MAKE-ARRAY (+ 1 si:%mx-mouse-doc-max) :element-type '(unsigned-byte 8)
			  :displaced-to-physical-address
			  (dpb si:*addin-memory-slot* si:%%Nubus-F-And-Slot-Bits
			       (+ si:%Driver-Data-Start si:%DD-mouse-doc-2))))

(DEFVAR last-mac-mouse-doc-1 "")
(DEFVAR mac-line 0)  ; current mac-mouse-line
(DEFPARAMETER current-doc-line "")

(DEFUN mw-get-pkg ()
  (LET* ((SG nil)
	 (val nil)
	 (PKG (COND
		((SETQ LAST-WHO-LINE-PROCESS (OR WHO-LINE-PROCESS
						 (AND
						   SELECTED-IO-BUFFER
						   (IO-BUFFER-LAST-OUTPUT-PROCESS
						     SELECTED-IO-BUFFER))))
		 (SETf SG (PROCESS-STACK-GROUP LAST-WHO-LINE-PROCESS))
		 (COND ((EQ SG %CURRENT-STACK-GROUP) *PACKAGE*)
		       ((TYPEP SG 'STACK-GROUP)
			(SYMEVAL-IN-STACK-GROUP '*PACKAGE* SG))
		       (T *PACKAGE*))))))
    (COND ((AND PKG (ARRAYP PKG))
	   ;	(NEQ WHO-LINE-ITEM-STATE PKG))
	   (SETQ VAL (SI:PKG-SHORTEST-NAME PKG))
	   (WHEN (ZEROP (LENGTH VAL))
	     ;; Handle the keyword package.
	     (SETQ VAL (PACKAGE-NAME PKG))))
	  (t " "))
    val ))


(DEFUN display-mac-mouse-documentation (only-one-line
					&optional (mouse-doc-string current-doc-line))
  ;Display the current mouse documentation on line mac-line of the mouse
  ;documentation display
  (COND
    ((< mac-line 2)
     (copy-array-portion mouse-doc-string 0 (LENGTH mouse-doc-string)
			 mac-who-mouse-doc-string-1 0 (- si:%MX-Mouse-Doc-Max 1))
       (SETF (AREF  mac-who-mouse-doc-string-1  si:%MX-Mouse-Doc-Max ) 1))
    ((= mac-line 2)
     (copy-array-portion mouse-doc-string 0 (LENGTH mouse-doc-string)
			 mac-who-mouse-doc-string-2 0 (- si:%MX-Mouse-Doc-Max 1))
      (SETF (AREF  mac-who-mouse-doc-string-2  si:%MX-Mouse-Doc-Max ) 1))
    (t nil))
  (when (and (> (length mouse-doc-string) si:%MX-Mouse-Doc-Max)
	     (< mac-line si:%MX-Max-mouse-lines))
    (setf current-doc-line (nsubstring mouse-doc-string si:%MX-Mouse-Doc-Max))
    (incf mac-line)
    (display-mac-mouse-documentation only-one-line)
    )
  (setf current-doc-line "")
  (IF only-one-line
      (SETF mac-line 1)  ; also blank out remaining lines.
      (INCF mac-line)))
(defvar newline-seq (format nil "~c" #\newline))
(DEFUN WHO-LINE-DOCUMENTATION-FUNCTION-MAC ()
  "This displays the who line documentation for the currently selected
window.  If the selected window is locked an error message is blinked
in the who line area.  Who line documentation may be either a string
of text which is to be displayed or a list of keyword value pairs.  To
see what keywords are accepted see the documentation for the
tv:process-who-line-documentation-list function."
  (LET* ((W MOUSE-WINDOW)
	 (MSG  "*** Error with window locked; try TERMINAL C-CLEAR-INPUT or TERMINAL C-C ***")
	 (MSG1 "    Error with window locked; try TERMINAL C-CLEAR-INPUT or TERMINAL C-C")
	 (NEW-STATE (COND (LOCKED-ERROR-WINDOWS
			    ;; To attract attention, make this message blink.
			    ;; The following EQ test should remain as EQ in spite of what the
			    ;; compiler says.  In this case EQ is both faster and correct.
			    (IF (compiler-let ((inhibit-style-warnings-switch t))
				  (EQ last-mac-mouse-doc-1 MSG))	;Compiler-let added by PMH 7/2/87
				MSG1 MSG))
                          ((SYMBOLP W)
                           (AND W WHO-LINE-MOUSE-GRABBED-DOCUMENTATION))
                          (T (MULTIPLE-VALUE-BIND (DOC ERROR)
                                 (CATCH-ERROR
                                   (FUNCALL W :WHO-LINE-DOCUMENTATION-STRING)
                                   NIL)
                               (IF ERROR
                                   "Error getting documentation string" DOC))))))
    (COND ((AND (NEQ last-mac-mouse-doc-1  NEW-STATE)
                (NOT (EQUAL last-mac-mouse-doc-1  NEW-STATE)))
           (SETQ last-mac-mouse-doc-1  NEW-STATE)
	   (COND ((TYPEP NEW-STATE 'STRING)
		  (let ((newline-search-loc -1))
		    (loop
		      (if (setf newline-search-loc (search newline-seq (the string new-state) :start2 (incf newline-search-loc))) 
			  (setf (aref new-state newline-search-loc) #\space)
			  (return))))
		  (display-mac-mouse-documentation t new-state))
		 ((CONSP NEW-STATE)

		  (PROCESS-WHO-LINE-DOCUMENTATION-LIST-MAC NEW-STATE)
		  )
		 (T (CATCH 'PAGE-OVERFLOW
		      (PROCESS-WHO-LINE-DOCUMENTATION-LIST-MAC
			'(:MOUSE-R-1 "Bring up the System Menu."))
		      ))))))
  (unless (ZEROP (LENGTH current-doc-line))
    (display-mac-mouse-documentation t))
  (SETF mac-line 1)
  )


(DEFUN PROCESS-WHO-LINE-DOCUMENTATION-LIST-MAC (NEW-STATE) 
  "This function is similar to PROCESS-WHO-LINE-DOCUMENTATION-LIST
except it displays who line mouse documentation on the MAC"
  (LET ((HAVE-DOCUMENTATION (OR (MEMBER  :DOCUMENTATION NEW-STATE :TEST #'EQ)
                                (MEMBER :KEYSTROKE     NEW-STATE :TEST #'EQ)))
			
	;; when comma is nil, we will NOT output a comma in documentation line. 
	(COMMA (MEMBER :NO-COMMA NEW-STATE :TEST #'EQ)))
	;; We string the documentation components together in certain cases.  If there
	;; is only 1 or 2 lines then there isn't much choice.  If there are 3 lines,
	;; then we also have to have a :DOCUMENTATION component too.  If there are 4 or
	;; more lines then we can display the documentation in a 3 column format.
    (SETQ STRING-TOGETHER (OR (= NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES 1)
                              (= NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES 2)
                              (AND HAVE-DOCUMENTATION
                                   (= NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES 3))))

    ;; Initialize constants for this function execution.
    (SETQ LEFT-CLICK-LOC 2
	  MIDDLE-CLICK-LOC 200.
	  RIGHT-CLICK-LOC (IF (NOT STRING-TOGETHER)
                              400.)
	  MOUSE-SINGLE-LOC 2
	  MOUSE-DOUBLE-LOC (AND (NOT STRING-TOGETHER)
                                (IF (OR (MEMBER :MOUSE-L-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-M-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-R-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-1-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-2-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-3-1 NEW-STATE :TEST #'EQ))
                                    ;; We have single click info, put this on the second line.
                                    (+ MOUSE-SINGLE-LOC WHO-LINE-DOCUMENTATION-LINE-HEIGHT)
                                    ;;ELSE Don't have single click info, put this on the first line.
                                    MOUSE-SINGLE-LOC))
	  MOUSE-HOLD-LOC (AND
                           (NOT STRING-TOGETHER)
                           (IF (OR (MEMBER :MOUSE-L-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-M-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-R-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-1-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-2-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-3-2 NEW-STATE :TEST #'EQ))
                               ;; We have both double click info.  The hold info must on the line after that.
                               (+ MOUSE-DOUBLE-LOC WHO-LINE-DOCUMENTATION-LINE-HEIGHT)
                               ;;ELSE
                               (IF (NOT (= MOUSE-SINGLE-LOC MOUSE-DOUBLE-LOC))
                                   ;; There was single click info. put this after that.
                                   (+ MOUSE-SINGLE-LOC WHO-LINE-DOCUMENTATION-LINE-HEIGHT)
                                   ;; ELSE This is the only mouse documentation. put on the first line.
                                   MOUSE-SINGLE-LOC))))

    (SETQ OLD-WHO-LINE-FONT        nil ;(GET-DEFAULT-FONT WHO-SHEET)
	  NEW-WHO-LINE-FONT        nil ;OLD-WHO-LINE-FONT
	  NOT-FIRST?               NIL
	  MAXIMUM-WHO-LINE-MOUSE-X 0
	  MAXIMUM-WHO-LINE-MOUSE-Y 0)


    ;; This loops through all of the non-documentation keywords.  We process them first so we can put the
    ;; documentation strings towards the bottom of the window.  If we didn't then we might intersperse them.
    (LOOP FOR DOC-SPEC = NEW-STATE THEN (CDDR DOC-SPEC)
          WHILE DOC-SPEC
          FOR OLD-KEY = NIL THEN KEY
          FOR KEY     = (FIRST  DOC-SPEC)
          FOR VALUE   = (SECOND DOC-SPEC)
          FINALLY  t ;(UNLESS (EQ (SHEET-CURRENT-FONT WHO-SHEET) OLD-WHO-LINE-FONT)
                  ;  (SEND WHO-SHEET :SET-CURRENT-FONT OLD-WHO-LINE-FONT))
          DO
          (PROGN
            (WHEN (AND (NOT NOT-FIRST?) OLD-KEY)
              (SETQ NOT-FIRST? (AND (NOT (EQ OLD-KEY :FONT))
                                    (NOT (EQ OLD-KEY :KEYSTROKE))
				   ;; (NOT (EQ OLD-KEY :NO-COMMA))
                                    (NOT (EQ OLD-KEY :DOCUMENTATION)))))
            (unless (EQ KEY :FONT)
                (IF (AND (NOT (EQ KEY :KEYSTROKE))
                         (NOT (EQ KEY :DOCUMENTATION))
			 (not (eq key :no-comma)))
 		    (progn
		      (DISPLAY-WHO-LINE-MOUSE-INFO-MAC  KEY VALUE COMMA)
		      (WHEN (> (LENGTH current-doc-line) 120.)
			(display-mac-mouse-documentation nil))))
		    )))

    (WHEN HAVE-DOCUMENTATION
      ;; If the mouse info wraps onto the last line available then we start the :DOCUMENTATION info
      ;; there.  Otherwise we put the :DOCUMENTATION on the next line.
      (SETQ NOT-FIRST? (AND STRING-TOGETHER
                            (= (1+ (TRUNCATE MAXIMUM-WHO-LINE-MOUSE-Y WHO-LINE-DOCUMENTATION-LINE-HEIGHT))
                               NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES)
                            (NOT (ZEROP MAXIMUM-WHO-LINE-MOUSE-X)))) 

	(WHEN (NOT NOT-FIRST?)
	  (display-mac-mouse-documentation nil)
     )
	;; Now we loop through again to get all of the :DOCUMENTATION info.
	(LOOP FOR DOCUMENTATION-KEYWORD IN '(:DOCUMENTATION :KEYSTROKE) ;;;:no-comma)
	      DO
	      (LOOP FOR DOC-SPEC = NEW-STATE THEN (CDDR DOC-SPEC)
		    WHILE DOC-SPEC
		    WITH OLD-KEY = NIL
		    FOR KEY      = (FIRST  DOC-SPEC)
		    FOR VALUE    = (SECOND DOC-SPEC)
		    FINALLY t
		    WHEN
		       (EQ KEY DOCUMENTATION-KEYWORD)
		    DO
		    (PROGN
		      (IF (EQ KEY :FONT)
			  t
			  ;;ELSE
			  (WHEN (NOT (EQ KEY :FONT))
				(when (and NOT-FIRST? (not comma))
				  (SETF current-doc-line (STRING-APPEND  current-doc-line ", "))
				       ;;else
				    (SETQ NOT-FIRST? T)))
			    (WHEN (EQ KEY :KEYSTROKE)
			      (SETF current-doc-line (STRING-APPEND current-doc-line "Keystroke: "))

			      ;; Make sure the value is a string.
			      (WHEN (OR (CHARACTERP VALUE) (INTEGERP VALUE))
				(SETQ VALUE (FORMAT NIL "~:C" VALUE)))
			      )
			    (SETF current-doc-line (STRING-APPEND current-doc-line value))
			    (setf mac-line NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES)
			    (display-mac-mouse-documentation nil)
   			    )
		      (SETQ OLD-KEY KEY))
		    )))))

(DEFUN DISPLAY-WHO-LINE-MOUSE-INFO-MAC ( MOUSE-KEYWORD DOCUMENTATION COMMA)
  "Display mouse information in the mac who line documentation window."
  ;; Do the things which need to be done before writing out the documentation string.
  (LET ((PAGE-OVERFLOW-ENCOUNTERED T)) 
    (COND (STRING-TOGETHER
           ;; If we are stringing everything together and this is not the first line
           ;; then we need to output a comma to separate this string from the previous string.
           (when (and NOT-FIRST? (not comma))            ;; if not-first? is T and the value of  :no-comma is nil, 
	     (SETF current-doc-line (STRING-APPEND current-doc-line ", "))
	     ))   ;; then we output a comma.
          (T t))
    (CATCH 'PAGE-OVERFLOW
      (SETF current-doc-line (STRING-APPEND current-doc-line 
			(OR (CADR (IF (EQ MOUSE-HANDEDNESS :LEFT)
                                      (ASSOC MOUSE-KEYWORD
                                             '((:MOUSE-ANY "L,M,R") (:ANY "L,M,R")
                                               (:MOUSE-R-1 "L")   (:MOUSE-R-2 "L2")   (:MOUSE-R-HOLD "LH")
                                               (:MOUSE-3-1 "L")   (:MOUSE-3-2 "L2")
                                               (:MOUSE-M-1 "M")   (:MOUSE-M-2 "M2")   (:MOUSE-M-HOLD "MH")
                                               (:MOUSE-2-1 "M")   (:MOUSE-2-2 "M2")
                                               (:MOUSE-L-1 "R")   (:MOUSE-L-2 "R2")   (:MOUSE-L-HOLD "RH")
                                               (:MOUSE-1-1 "R")   (:MOUSE-1-2 "R2")) :TEST #'EQ)
                                      ;;ELSE
				       (ASSOC MOUSE-KEYWORD
                                             '((:MOUSE-ANY "L,M,R") (:ANY "L,M,R")
                                               (:MOUSE-R-1 "R")   (:MOUSE-R-2 "R2")   (:MOUSE-R-HOLD "RH")
                                               (:MOUSE-3-1 "R")   (:MOUSE-3-2 "R2")
                                               (:MOUSE-M-1 "M")   (:MOUSE-M-2 "M2")   (:MOUSE-M-HOLD "MH")
                                               (:MOUSE-2-1 "M")   (:MOUSE-2-2 "M2")
                                               (:MOUSE-L-1 "L")   (:MOUSE-L-2 "L2")   (:MOUSE-L-HOLD "LH")
                                               (:MOUSE-1-1 "L")   (:MOUSE-1-2 "L2")) :TEST #'EQ)
				       ))
                            ;; If the caller specified an illegal mouse button
                            ;; then use the following string as the mouse prefix.
                            "Bad doc keyword")))
;;;       (SHEET-STRING-OUT WHO-SHEET ": " 0
;;;			(IF (STRING-EQUAL "" DOCUMENTATION)
;;;                            ;; If the documentation for this button is empty then we do
;;;                            ;; not want to have the space after the mouse prefix.  In
;;;                            ;; this case there are two mouse buttons which do the same
;;;                            ;; thing.  The next mouse button will have the documentation
;;;                            ;; for this mouse button.  See the EDIT SCREEN menu item of
;;;                            ;; the System Menu for an example of this.
;;;                            1
;;;                            ;;ELSE
;;;                            NIL))
;;;      (SEND WHO-SHEET :SET-CURRENT-FONT NEW-WHO-LINE-FONT T)
;;;      (SHEET-STRING-OUT WHO-SHEET DOCUMENTATION)
      (SETF current-doc-line (STRING-APPEND current-doc-line ":" documentation))
      (WHEN (> (LENGTH current-doc-line) 120.)
	(display-mac-mouse-documentation nil) )
      (SETQ PAGE-OVERFLOW-ENCOUNTERED NIL))
    )
  )


(DEFUN display-mac-who-line ()
  (WHO-LINE-DOCUMENTATION-FUNCTION-mac)
  (LET* ((file-screen who-line-file-state-sheet)
	 (current-stream (if (boundp 'tv:*current-stream*)  ;dab 02-23-89
			     (send file-screen :current-stream) ;new version of who-line-file-state-sheet
			     (si:symeval-in-instance file-screen 'tv:current-stream)))
	 (pathname nil) (direction) (COUNT) (percent)
	 (mac-who-string))

    (SETF mac-who-string
	  (COND  (current-stream
		  (MULTIPLE-VALUE-SETQ
		    (pathname direction count percent)
		    (SEND current-stream :who-line-information))
		  (FIXNUM-INTO-STRING COUNT DISPLAY-FILE-TRANSFER-COUNT-STRING)
		  (STRING-APPEND 
		    (CASE DIRECTION
		      (:INPUT         "< ") ;#\LEFT-ARROW)
		      (:OUTPUT        "> ") ;#\RIGHT-ARROW)
		      (T              ""))
		    (if percent
			(progn
			  (fixnum-into-string percent display-file-transfer-percent-string)
			  (STRING-APPEND  (SEND pathname :string-for-printing)
					  "   "	 display-file-transfer-percent-string
					  "% " display-file-transfer-count-string))
			(STRING-APPEND  (SEND pathname :string-for-printing)
					"   "	 display-file-transfer-count-string))))
		 (t (STRING-APPEND user-id "      " (mw-get-pkg) ":"))))
    (UNLESS (string= mac-who-string last-mac-who-line-file-state)
      (copy-array-portion mac-who-string 0 (LENGTH mac-who-string)
			  mac-who-file-string 0 (- si:%MX-Who-File-String-Max 1))
      (SETF (AREF mac-who-file-string si:%MX-Who-File-String-Max) 1)
      (SETF last-mac-who-line-file-state  mac-who-string) )
    mac-who-string))