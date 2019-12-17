;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10b); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; AST Support

(DEFUN READ-AST-INTO-FONT (FILENAME &OPTIONAL FONTNAME)
  (MULTIPLE-VALUE-SETQ (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AST"))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME (READ-AST-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
  FONTNAME) 


(DEFUN READ-AST-INTO-FONT-DESCRIPTOR (FILENAME &OPTIONAL FONTNAME &AUX FD)
  (MULTIPLE-VALUE-SETQ (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "AST"))
  (WITH-OPEN-FILE (STREAM FILENAME :DIRECTION :INPUT)
    (SETQ FD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH 200)
				   :FD-NAME FONTNAME))
    (READ-AST-NUMBER STREAM)				  ;DISCARD KSTID
    (SETF (FD-LINE-SPACING FD) (READ-AST-NUMBER STREAM))  ;HEIGHT
    (SETF (FD-BASELINE FD) (READ-AST-NUMBER STREAM))	  ;BASELINE
    (READ-AST-NUMBER STREAM)				  ;DISCARD COLUMN POSITION ADJUSTMENT
    (SETF (FD-SPACE-WIDTH FD) 0)			  ;Just in case no space character.
    (SETF (FD-BLINKER-HEIGHT FD) (FD-LINE-SPACING FD))
    (SETF (FD-NAME FD) FONTNAME)
    (LET ((LINE-HEIGHT (FD-LINE-SPACING FD))
	  KERN CHAR-CODE RASTER-WIDTH INPUT-RASTER-WIDTH CHAR-WIDTH CD CH)
      (DO () ((NULL (READ-AST-NEXT-PAGE STREAM)))
	(SETQ CHAR-CODE (READ-AST-NUMBER STREAM 8.))	  ;Read char-code in base 8.
	(SETQ INPUT-RASTER-WIDTH (READ-AST-NUMBER STREAM) ;RASTER WIDTH
	      RASTER-WIDTH INPUT-RASTER-WIDTH)		  
	(SETQ CHAR-WIDTH (READ-AST-NUMBER STREAM))	  ;CHARACTER WIDTH
	(SETQ KERN (READ-AST-NUMBER STREAM))		  ;LEFT KERN
	(WHEN (< KERN 0)				  ;FED compact raster lossage
	  (SETQ RASTER-WIDTH (+ RASTER-WIDTH (ABS KERN)))
	  (SETQ KERN 0))
	
	(IF (<= 128. CHAR-CODE 160.)
	    NIL					;Don't even look at LISPM control characters.
	    ;; else, make a char-descriptor and load it up.
	    (SETQ CD (MAKE-CHAR-DESCRIPTOR
		       :MAKE-ARRAY (:TYPE ART-1B :LENGTH (LIST LINE-HEIGHT RASTER-WIDTH))
		       :CD-CHAR-WIDTH CHAR-WIDTH
		       :CD-CHAR-LEFT-KERN KERN))
	    (FD-STORE-CD FD CD CHAR-CODE)
	    (AND (= CHAR-CODE (CHAR-INT #\SPACE))
		 (SETF (FD-SPACE-WIDTH FD) CHAR-WIDTH))
	    (BLOCK TOP
	      (DO ((VPOS 0 (1+ VPOS)))
		  ((= VPOS LINE-HEIGHT))
		(DO ((HCNT 0 (1+ HCNT)))
		    ((= HCNT INPUT-RASTER-WIDTH)
		     (DO ((CH)) (())
		       (COND ((OR (NULL (SETQ CH (READ-CHAR STREAM NIL)))
				  (EQL CH #\NEWLINE))
			      (RETURN nil))
			     ((EQL CH #\PAGE)
			      (UNREAD-CHAR CH STREAM)
			      (RETURN-FROM TOP nil))
			     ((NOT (EQL CH #\SPACE))
			      (FERROR nil "non space seen past raster width")))))
		  (SETQ CH (READ-CHAR STREAM NIL))
		  (COND
		    ((NULL CH)
		     (RETURN-FROM TOP nil))
		    ((EQL CH #\PAGE)
		     (UNREAD-CHAR CH STREAM)
		     (RETURN-FROM TOP nil))
		    ((EQL CH #\NEWLINE)
		     (RETURN NIL))
		    ((NOT (STANDARD-CHAR-P CH))
		     (DO () ((EQL CH #\NEWLINE))	;spin to next newline character
		       (SETQ CH (READ-CHAR STREAM NIL)))
		     (RETURN nil))
		    ((NOT (EQL ch #\space))
		     (SETF (AREF CD VPOS (+ HCNT (- RASTER-WIDTH INPUT-RASTER-WIDTH))) 1))))))))
      ;; Truncate fd to discard unused elements at the end.
      (DO ((I (1- (ARRAY-TOTAL-SIZE FD)) (1- I)))
	  ((OR (MINUSP I) (AREF FD I))
	   (ADJUST-ARRAY FD (1+ I))))
      (SETF (FD-FILL-POINTER FD) (ARRAY-TOTAL-SIZE FD))
      ;; Set width of blinker and space fields from the space character.
      (SETF (FD-BLINKER-WIDTH FD) (FD-SPACE-WIDTH FD))
      FD))) 

(DEFUN READ-AST-NUMBER (STREAM &optional (the-base 10.))
  ;;Read next number in specified base.  This does not error check to insure
  ;;digits are in specified base.  So, if you specify base 8, then the input
  ;;better be in base 8 or this will lose.  After seeing any non-digit character, this
  ;;will skip to the next newline character and return the built number in base.
  (DO ((CH (READ-CHAR STREAM nil) (READ-CHAR STREAM nil))
       (SIGN 1)
       (FOUND-P NIL)
       (N 0)
       (STATE :START))
      ((NULL CH)
       (AND FOUND-P (* N SIGN)))		;Return  char-code at eof. Don't forget the sign
    (COND
      ;;look for a sign the first time thru
      ((AND (EQ STATE :START)
	    (SETQ STATE :BUILD-CHAR-CODE)
	    (EQL CH #\-)
	    (SETQ SIGN -1)))
      ;;build up the character code one digit at a time until we see a non-digit character
      ((IF (AND (EQ STATE :BUILD-CHAR-CODE)
		(>= CH #\0) (<= CH #\9))
	   (PROGN (SETQ N (+ (* N the-base)
			     (- (CHAR-CODE CH) (CHAR-CODE #\0)) ))
		  (SETQ FOUND-P T))
	   (PROGN (SETQ STATE :SKIP-TO-NEWLINE)
		  NIL)))
      ;;Once we see a non-digit character, ignor EVERYTHING until the next newline, and return char-code.
      (T					;state is :skip-to-newline
       (WHEN (EQL CH #\NEWLINE)
	 (RETURN (* N SIGN)) )))))		;Return char-code. Don't forget to apply the sign
		   

(DEFUN READ-AST-NEXT-PAGE (STREAM)
  ;;Read until the page character found.  Read ahead one char (and put it back)
  ;;to return an indication if more info available.
  (DO ((CH (READ-CHAR STREAM NIL) (READ-CHAR STREAM NIL)))
      ((NULL CH))
    (WHEN (EQL CH #\PAGE)
      (SETQ CH (READ-CHAR STREAM NIL))
      (UNREAD-CHAR CH STREAM)
      (IF (NULL CH)
	  (RETURN nil)
	  (RETURN T)))))

(DEFUN WRITE-FONT-INTO-AST (FONTNAME &OPTIONAL FILENAME)
  (MULTIPLE-VALUE-SETQ (FONTNAME FILENAME)
    (GET-OUTPUT-FILENAME-AND-FONTNAME FONTNAME FILENAME "AST"))
  (LET ((FD (FONT-NAME-FONT-DESCRIPTOR FONTNAME)))
    (WRITE-FONT-DESCRIPTOR-INTO-AST FD FILENAME))) 


(DEFUN WRITE-FONT-DESCRIPTOR-INTO-AST (FD &OPTIONAL FILENAME &AUX (FONT-LENGTH (ARRAY-ACTIVE-LENGTH FD)))
  (SETQ FILENAME (GET-OUTPUT-FILENAME-FROM-FONT-DESCRIPTOR FD FILENAME "AST"))
  (WITH-OPEN-FILE (STREAM FILENAME :DIRECTION :OUTPUT :ASCII)
    (FORMAT STREAM "0 KSTID ~A" FILENAME)
    (FORMAT STREAM "~%~D HEIGHT" (FD-LINE-SPACING FD))
    (FORMAT STREAM "~%~D BASE LINE" (FD-BASELINE FD))
    (FORMAT STREAM "~%0 COLUMN POSITION ADJUSTMENT~%")
    ;; Then write out all the characters.
    (LET (CD CHAR-HEIGHT LOSING-KERN KERN)
      (DOTIMES (CHAR-CODE FONT-LENGTH)
	(COND
	  ((<= 128. CHAR-CODE 160.))  ;Skip control-characters
	  ((AND (SETQ CD (AREF FD CHAR-CODE))
		;; Wide fonts without chars-exist-tables can have 0-width chars.
		(OR (NOT (ZEROP (ARRAY-DIMENSION CD 1))) (NOT (ZEROP (CD-CHAR-WIDTH CD)))))
	   (WRITE-CHAR #\PAGE STREAM)
	   (SETQ KERN (CD-CHAR-LEFT-KERN CD)
		 LOSING-KERN (AND (MINUSP KERN)
				  (>= (CD-CHAR-WIDTH CD)
				      (- (ARRAY-DIMENSION CD 1) KERN))
				  (- KERN)))
	   (FORMAT STREAM "~O CHARACTER CODE ~A" CHAR-CODE FILENAME)
	   (FORMAT STREAM "~%~D RASTER WIDTH"
		   (IF LOSING-KERN
		       (CD-CHAR-WIDTH CD)
		       (ARRAY-DIMENSION CD 1)))
	   (FORMAT STREAM "~%~D CHARACTER WIDTH" (CD-CHAR-WIDTH CD))
	   (FORMAT STREAM "~%~D LEFT KERN~%" (IF LOSING-KERN
						 0
						 KERN))
	   (SETQ CHAR-HEIGHT (ARRAY-DIMENSION CD 0))
	   (DOTIMES (VPOS (MIN (FD-LINE-SPACING FD) CHAR-HEIGHT))
	     (IF LOSING-KERN
		 (DOTIMES (HPOS LOSING-KERN)
		   (WRITE-CHAR #\SPACE STREAM)))
	     (DOTIMES (HPOS (ARRAY-DIMENSION CD 1))
	       (WRITE-CHAR (IF (ZEROP (AREF CD VPOS HPOS))
			       #\SPACE
			       #\*)
			   STREAM))
	     (WRITE-CHAR #\NEWLINE STREAM))))))
    (CLOSE STREAM)
    (SEND STREAM :TRUENAME))) 
