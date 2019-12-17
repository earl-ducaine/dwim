;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10b); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; THIS FILE CONTAINS ALL THE FONT AND CHARACTER LEVEL MODIFIERS, LIKE
;;; ITALICIZE, ROTATE, REVERSE-VIDEO, ETC.

;;; Change History
;;;  12/31/86 MRR - Changed com-scale-character to work around LET problem.
;;;                     Scaling still needs repair.
;;;  6/22/87  DKM - changed com-scale-character and com-scale-font to edit inputs to be an integer

;;; FONT LEVEL MODIFICATION OPERATIONS

;;; Rotate all characters of specified font left (270 degrees).
(DEFUN COM-ROTATE-FONT-LEFT ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No current font")
      (LET (newfont fname good LEN FD NFD)
	(WHEN (MULTIPLE-VALUE-SETQ (good newfont fname)
		(get-new-font-name #\L "rotate-left"))
	   (SETQ fd (font-get-fd current-font)
		 len (array-active-length fd))
	   (SETQ nfd (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH len)
					   :FD-NAME fname
					   :fd-fill-pointer (fd-fill-pointer fd)
					   :FD-LINE-SPACING (fd-space-width fd)
					   :FD-BASELINE (fd-space-width fd)
					   :FD-BLINKER-HEIGHT (fd-blinker-width fd)
					   :FD-BLINKER-WIDTH (fd-blinker-height fd)
					   :FD-SPACE-WIDTH (fd-line-spacing fd)
					   :fd-rotation (IF (< (- (fd-rotation fd) 90.) 0)
							    270.
							    (- (fd-rotation fd) 90.))))
						;  now transfer characters from old font into new one rotating as we go
	    (LOOP FOR CH FROM 0 BELOW LEN
		  AS CD = (AREF FD CH)
		  WHEN CD
		  DO (SETF (AREF NFD CH) (ROTATE-CHAR-DESCRIPTOR-LEFT CD )) )
	      (ACKNOWLEDGE-NEW-FONT FNAME NFD)))))

(DEFUN ROTATE-CHAR-DESCRIPTOR-LEFT (cd)
  (LET* ((hite (ARRAY-DIMENSION cd 0))
	 (wide (ARRAY-DIMENSION cd 1))
	 (ncd (make-char-descriptor
		:make-array (:length (LIST wide hite) :type (ARRAY-TYPE cd))
		:cd-char-left-kern 0
		:cd-char-width hite)))
    (LOOP for x from 0 below hite
	  DO (LOOP for y from 0 below wide
		   DO (SETF (AREF ncd (- wide y 1) x) (AREF cd x y)) ))
    ncd))

;;; Rotate all characters of specified font right (90 degrees).
(DEFUN COM-ROTATE-FONT-RIGHT ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No current font")
      (LET (newfont fname good len fd nfd)
	(WHEN (MULTIPLE-VALUE-SETQ (good newfont fname)
		(get-new-font-name #\R "rotate-right"))
	   (SETQ fd (font-get-fd current-font)
		 len (array-active-length fd))
	   (SETQ nfd (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH len)
					   :FD-NAME fname
					   :fd-fill-pointer (fd-fill-pointer fd)
					   :FD-LINE-SPACING (fd-space-width fd)
					   :FD-BASELINE (fd-space-width fd)
					   :FD-BLINKER-HEIGHT (fd-blinker-width fd)
					   :FD-BLINKER-WIDTH (fd-blinker-height fd)
					   :FD-SPACE-WIDTH (fd-line-spacing fd)
					   :fd-rotation (IF (< (- (fd-rotation fd) 270.) 0)
							    90.
							    (- (fd-rotation fd) 270.))))
					   ;  now transfer characters from old font into new one rotating as we go
	    (LOOP for ch from 0 below len
		  as cd = (AREF fd ch)
		  WHEN cd
		  DO (SETF (AREF nfd ch) (rotate-char-descriptor-right cd)) )
	    (acknowledge-new-font fname nfd)))))

(DEFUN ROTATE-CHAR-DESCRIPTOR-RIGHT (cd)
   (LET* ((hite (ARRAY-DIMENSION cd 0))
	  (wide (ARRAY-DIMENSION cd 1))
	  (ncd (make-char-descriptor
		 :make-array (:length (LIST wide hite) :type (ARRAY-TYPE cd))
		 :cd-char-left-kern 0
		 :cd-char-width hite)))
     (LOOP for x from 0 below hite
	   DO (LOOP for y from 0 below wide
		    DO (SETF (AREF ncd y (- hite x 1)) (AREF cd x y)) ))
     ncd))

;;; Rotate all characters of specified font upside-down (180 degrees).
(DEFUN COM-ROTATE-FONT-180 ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No current font")
      (LET (NEWFONT FNAME GOOD LEN FD NFD)
	(WHEN (MULTIPLE-VALUE-SETQ (good newfont fname) (get-new-font-name #\U "rotate-180"))
	      (SETQ fd (font-get-fd current-font)
		    len (array-active-length fd))
	      (SETQ nfd (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH len)
					      :FD-NAME fname
					      :fd-fill-pointer (fd-fill-pointer fd)
					      :FD-LINE-SPACING (fd-line-spacing fd)
					      :FD-BASELINE (fd-baseline fd)
					      :FD-BLINKER-HEIGHT (fd-blinker-height fd)
					      :FD-BLINKER-WIDTH (fd-blinker-width fd)
					      :FD-SPACE-WIDTH (fd-space-width fd)
					      :fd-rotation (IF (< (- (fd-rotation fd) 180.) 0)
							       180.
							       (- (fd-rotation fd) 180.))))
						;  now transfer characters from old font into new one rotating as we go
	      (LOOP for ch from 0 below len
		    as cd = (AREF fd ch)
		    WHEN cd
		    DO (SETF (AREF nfd ch) (rotate-char-descriptor-180 cd)) )
	      (acknowledge-new-font fname nfd)))))

(DEFUN ROTATE-CHAR-DESCRIPTOR-180 (cd)
   (LET* ((hite (ARRAY-DIMENSION cd 0))
	  (wide (ARRAY-DIMENSION cd 1))
	  (ncd (make-char-descriptor
		 :make-array (:length (LIST hite wide) :type (ARRAY-TYPE cd))
		 :cd-char-left-kern 0
		 :cd-char-width wide)))
     (LOOP for x from 0 below hite
	   DO (LOOP for y from 0 below wide
		    DO (SETF (AREF ncd (- hite x 1) (- wide y 1)) (AREF cd x y)) ))
     ncd))

(DEFUN COM-REVERSE-VIDEO-FONT ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No current font")
      (LET (newfont fname good LEN FD NFD)
	(WHEN (MULTIPLE-VALUE-SETQ (good newfont fname)
		(get-new-font-name #\V "reverse-video"))
	   (SETQ fd (font-get-fd current-font)
		 len (array-active-length fd))
	   (SETQ nfd (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH len)
					   :FD-NAME fname
					   :FD-FILL-POINTER (fd-fill-pointer fd)
					   :FD-LINE-SPACING (fd-line-spacing fd)
					   :FD-BASELINE (fd-baseline fd)
					   :FD-BLINKER-HEIGHT (fd-blinker-height fd)
					   :FD-BLINKER-WIDTH (fd-blinker-width fd)
					   :FD-SPACE-WIDTH (fd-space-width fd)
					   :FD-ROTATION (fd-rotation fd)))
					   ;  now transfer characters from old font into new one reversing pixels as we go
	   (LOOP for ch from 0 below len
		 as cd = (AREF fd ch)
		 WHEN cd
		 DO (SETF (AREF nfd ch) (reverse-video-char-descriptor cd fd)) )
	   (acknowledge-new-font fname nfd)))))

(DEFUN REVERSE-VIDEO-CHAR-DESCRIPTOR  (cd fd)
   (LET* ((kern (cd-char-left-kern cd))
	  (nrek (IF (MINUSP kern) (- 0 kern) 0))
	  (hite (fd-line-spacing fd))
	  (wide (IF (PLUSP kern) (MAX (+ (cd-char-width cd) kern) (ARRAY-DIMENSION cd 1))
		                 (MAX (cd-char-width cd) (- (ARRAY-DIMENSION cd 1) kern))))
	  (ncd (make-char-descriptor
		 :make-array (:length (LIST hite wide) :type (ARRAY-TYPE cd) :initial-value 1)
		 :cd-char-left-kern (IF (PLUSP kern) kern 0)
		 :cd-char-width (cd-char-width cd))))
     (LOOP for y from 0 below (ARRAY-DIMENSION cd 0)
	   DO (LOOP for x from 0 below (ARRAY-DIMENSION cd 1)
		    DO (IF (PLUSP (AREF cd y x))
			   (SETF (AREF ncd y (+ x nrek)) 0)  )))
     ncd))

; Italicize all characters of specified font.

(DEFUN COM-ITALICIZE-FONT ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No current font")
      (LET (NEWFONT FNAME GOOD LEN FD NFD NCD BITS-PER-SHIFT NUMBER-OF-SHIFTS)
	(WHEN (MULTIPLE-VALUE-SETQ (GOOD NEWFONT FNAME)
		(GET-NEW-FONT-NAME #\I "italic"))
	   (SETQ FD (FONT-GET-FD CURRENT-FONT)
		 LEN (ARRAY-ACTIVE-LENGTH FD)
		 
		  ;; Calculate the number of vertical bits we will have in a group.
		  ;; For example, if it is 3, then we will shift 3 rows of bits as a
		  ;; group.  The number of rows is determined by the height.
		  BITS-PER-SHIFT (MIN 3 (TRUNCATE (FD-BLINKER-HEIGHT FD) 3))
		  
		  ;; Calculate the number of right-word shifts we will need to make to
		  ;; turn characters of this font into italic characters.
		  NUMBER-OF-SHIFTS (CEILING (FD-LINE-SPACING FD) BITS-PER-SHIFT))

	   (SETQ NFD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH LEN)
					   :FD-NAME FNAME
					   :FD-FILL-POINTER (FD-FILL-POINTER FD)
					   :FD-LINE-SPACING (FD-LINE-SPACING FD)
					   :FD-BASELINE (FD-BASELINE FD)
					   :FD-BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)
					   :FD-BLINKER-WIDTH (+ NUMBER-OF-SHIFTS (FD-BLINKER-WIDTH FD))
					   :FD-SPACE-WIDTH (+ NUMBER-OF-SHIFTS (FD-SPACE-WIDTH FD))
					   :FD-ROTATION 264))
	    ;;  now transfer characters from old font into new one italicizing as we go
	    (LOOP FOR CH FROM 0 BELOW LEN
		  AS CD = (AREF FD CH)
		  WHEN CD
		  DO (LET* ((WIDTH (ARRAY-DIMENSION CD 1))
			    (HEIGHT (ARRAY-DIMENSION CD 0))
			    ;; Actual width of the new italic character.  The
			    ;; difference between the width as specified in the
			    ;; character descriptor and this width is the white
			    ;; space at the end of this character.
			    (NEW-WIDTH 0)
			    ;; Actual width of this character.  The difference
			    ;; between the width as specified in the character
			    ;; descriptor and this width is the white space at the
			    ;; end of this character.
			    (THIS-WIDTH 0)
			    ;; Actual height of the new italic character.  This is
			    ;; used to calculate the width of the character.
			    (NEW-HEIGHT 0)
			    ;; The smallest number of empty pixels at the left
			    ;; side of the new italic character.  We want to
			    ;; left-justify all characters so that spacing comes
			    ;; out proper.
			    (NEW-OFFSET WIDTH)
			    ;; The smallest number of empty pixels at the left
			    ;; side of this character.  We want to justify the new
			    ;; italic character like it is in this character that
			    ;; spacing comes out proper.
			    (THIS-OFFSET WIDTH)
			    ;; T if we have only seen zeros so far on this row.
			    ;; NIL otherwise.
			    LEADING-ZEROP (SHIFT-AMOUNT NUMBER-OF-SHIFTS) (BIT-NUMBER BITS-PER-SHIFT))
		       ;; Calculate the width and starting offset for this italic character.
		       (DOTIMES (J HEIGHT)
			 (SETQ LEADING-ZEROP T)
			 (DOTIMES (I WIDTH)
			   (WHEN (NOT (ZEROP (AREF CD J I)))
			     (WHEN LEADING-ZEROP
			       (SETQ LEADING-ZEROP nil)
			       (IF (< I THIS-OFFSET)
				   (SETQ THIS-OFFSET I))
			       (IF (< (+ SHIFT-AMOUNT I) NEW-OFFSET)
				   (SETQ NEW-OFFSET (+ SHIFT-AMOUNT I))))
			     (IF (> I THIS-WIDTH)
				 (SETQ THIS-WIDTH I))
			     (IF (> (+ SHIFT-AMOUNT I) NEW-WIDTH)
				 (SETQ NEW-WIDTH (+ SHIFT-AMOUNT I)))
			     (IF (> J NEW-HEIGHT)
				 (SETQ NEW-HEIGHT J))))
			 (SETQ BIT-NUMBER (1- BIT-NUMBER))
			 (IF (ZEROP BIT-NUMBER)
			     (SETQ BIT-NUMBER BITS-PER-SHIFT
				   SHIFT-AMOUNT (1- SHIFT-AMOUNT))))
		       ;; Convert from 0-based numbers to 1-based numbers.
		       (IF (NOT (ZEROP THIS-WIDTH))
			   (SETQ THIS-WIDTH (1+ THIS-WIDTH)))
		       (IF (NOT (ZEROP NEW-WIDTH))
			   (SETQ NEW-WIDTH (- (1+ NEW-WIDTH) NEW-OFFSET)))
		       ;; Make the new italic character justified at the same
		       ;; place as this character.
		       (SETQ NEW-OFFSET (- NEW-OFFSET THIS-OFFSET))
		       (SETQ NEW-WIDTH (+ NEW-WIDTH THIS-OFFSET))
		       (WHEN (AND (NOT (ZEROP (CD-CHAR-WIDTH CD))) (ZEROP THIS-WIDTH))
			 ;; We have a blank character.  Leave its width alone.
			 (SETQ NEW-HEIGHT (FD-BASELINE FD)
			       NEW-OFFSET 0))
		      ;; All of the previous hairiness was so that the width of
		       ;; the new italic character will come out right.  It needs
		       ;; to be left justified and the right side needs to have
		       ;; the same number of empty pixels as the original
		       ;; character had.
		       (IF (= NEW-WIDTH 1)
			   (SETQ NEW-WIDTH (1+ NEW-WIDTH)))
		       (SETQ NCD (MAKE-CHAR-DESCRIPTOR
				   :MAKE-ARRAY (:TYPE ART-4B :LENGTH (LIST HEIGHT NEW-WIDTH))
				   :CD-CHAR-WIDTH (- (CD-CHAR-WIDTH CD) (TRUNCATE NEW-OFFSET (* 1.3 BITS-PER-SHIFT)))
				   :CD-CHAR-LEFT-KERN (+ (CD-CHAR-LEFT-KERN CD)
							;; Give some left kerning
							;; to those characters
							;; that have descenders on
							;; the left side.
							(MAKE-POSITIVE
							  (TRUNCATE (- NEW-HEIGHT (FD-BASELINE FD)) BITS-PER-SHIFT)))))
		       (LET ((SHIFT-AMOUNT (- NUMBER-OF-SHIFTS NEW-OFFSET))
			     (BIT-NUMBER BITS-PER-SHIFT))
			 (DOTIMES (J HEIGHT)
			   (DOTIMES (I WIDTH)
			     (SETF (AREF NCD J (+ SHIFT-AMOUNT I)) (AREF CD J I)))
			   (SETQ BIT-NUMBER (1- BIT-NUMBER))
			   (IF (ZEROP BIT-NUMBER)
			       (SETQ BIT-NUMBER BITS-PER-SHIFT
				     SHIFT-AMOUNT (1- SHIFT-AMOUNT)))))
		       (SETF (AREF NFD CH) NCD)))
	    (ACKNOWLEDGE-NEW-FONT FNAME NFD)))))

(DEFUN MAKE-POSITIVE (NUMBER)
  "Force the number to be positive."
  (IF (MINUSP NUMBER)      0    NUMBER))

;;; Stretch all characters of a given font.
;;; Rewrote to edit the values for the scale inputs to be non-zero integers.  
;;; It turns out there is an undocumented hack in scale-font that is activated when
;;; the x numerator is input as a negative number, which then uses a different function to
;;; do the scaling.  You will only see a difference when you scale the font UP, and then you
;;; will get a honeycomb effect on the new stretched characters.  It looks useless and I almost
;;; got rid of it ... but left it in.  dkm 5/15/87

(DEFUN COM-SCALE-FONT ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No current font")
      (LET (SCALE-LIST)
	(DOLIST (PROMPT '("Numerator of X scale factor (default 1): "
			  "Denominator of X scale factor (default 1): "
			  "Numerator of Y scale factor (default 1): "
			  "Denominator of Y scale factor (default 1): "))
	  (PUSH-END (DO (INPUT)  ((NOT (NULL INPUT)) INPUT)
		      (SETQ INPUT (PROMPT-LINE-DEFAULTED-READLINE 1 'READ-FROM-STRING PROMPT))
		      (IF (AND (INTEGERP INPUT) (NOT (ZEROP INPUT)))
			  T
			  (SETQ INPUT NIL)
			  (barf "~&Input must be an integer")))
		    SCALE-LIST))
	(MULTIPLE-VALUE-BIND (XNUM XDENOM YNUM YDENOM)
	    (VALUES-LIST SCALE-LIST)
	  (SCALE-FONT (ABS XNUM) (ABS XDENOM) (ABS YNUM) (ABS YDENOM)
		      (MINUSP XNUM))))))


(DEFUN SCALE-FONT (XNUM XDENOM YNUM YDENOM &optional (SCALE-TWO NIL) &aux LEN FD NFD)
   (DECLARE (:SELF-FLAVOR FED))
   (LET (newfont fname good)
     (WHEN (MULTIPLE-VALUE-SETQ (good newfont fname) (get-new-font-name #\S "stretched"))
	(SETQ fd (font-get-fd current-font)
		 len (array-active-length fd))
	(SETQ nfd (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH len)
					:FD-NAME fname
					:fd-fill-pointer (fd-fill-pointer fd)
					:FD-LINE-SPACING (CEILING (* (fd-line-spacing fd) ynum) ydenom)
					:FD-BASELINE (CEILING (* (fd-baseline fd) ynum) ydenom)
					:FD-BLINKER-HEIGHT (CEILING (* (fd-blinker-height fd) ynum) ydenom)
					:FD-BLINKER-WIDTH (CEILING (* (fd-blinker-width fd) xnum) xdenom)
					:FD-SPACE-WIDTH (CEILING (* (fd-space-width fd) xnum) xdenom)
					:fd-rotation (fd-rotation fd)))
	;;now transfer characters from old font into new one scaling as we go.
	(LOOP for ch from 0 below len
	      as cd = (AREF fd ch)
	      WHEN cd
	      DO (SETF (AREF nfd ch)
		       (IF scale-two
			   (scale-character-descriptor-two cd xnum xdenom ynum ydenom)
			   (scale-character-descriptor cd xnum xdenom ynum ydenom)) ))
	(acknowledge-new-font fname nfd))))

(DEFUN SCALE-CHARACTER-DESCRIPTOR (CD XNUM XDENOM YNUM YDENOM)
   "Scale each character descriptor according to both X and Y scale factors.
First, the character is expnaded by multiplying by X and Y numerators.
Each pixel in the input character is replicated as necessary to satisfy the
numerator values.  This result is stored in an intermediate array.
Now, the character is compressed according to the denominator values.
For each rectangular area of the temporary array that will be reduced
to a single pixel in the output the number of pixels in the intermediate
array are counted.  If half or more are on the resultant pixel is turned
on in the output."
   (LET* ((hite (ARRAY-DIMENSION cd 0))		 ;; height of input character
	  (wide (ARRAY-DIMENSION cd 1))		 ;; width of input character
	  (new-hite (CEILING (* hite ynum) ydenom))	 ;; height of output character
	  (new-wide (CEILING (* wide xnum) xdenom))	 ;; width of output character
	  (int-hite (* (1+ hite) ynum))		 ;; height of intermediate storage character
	  (int-wide (* (1+ wide) xnum))		 ;; width of intermediate storage character
	  (ncd (make-char-descriptor
		 :make-array (:length (LIST new-hite new-wide) :type (ARRAY-TYPE cd))
		 :cd-char-left-kern (FLOOR (* (cd-char-left-kern cd) xnum) xdenom) 
		 :cd-char-width (CEILING (* (cd-char-width cd) xnum) xdenom)))
	  (int (make-char-descriptor
		 :make-array (:length (LIST int-hite int-wide) :type (ARRAY-TYPE cd))  ; intermediate storage
		 :cd-char-left-kern (* (cd-char-left-kern cd) xnum)
		 :cd-char-width (* (cd-char-width cd) xnum))))
     ;; expand input according to numerators given
     (LOOP for y from 0 below hite
	   DO (LOOP for x from 0 below wide
		    DO (IF (PLUSP (AREF cd y x))
			   (LOOP for yy from 0 below ynum
				 DO (LOOP for xx from 0 below xnum
					  DO (SETF (AREF int (+ (* y ynum) yy) (+ (* x xnum) xx)) 1) )))))
     ;; compress intermediate according to denominators given
     (LOOP for y from 0 below new-hite
	   DO (LOOP for x from 0 below new-wide
		    DO (IF (> (LOOP for yy from (* y ydenom) below (* (1+ y) ydenom)
				    summing (LOOP for xx from (* x xdenom) below (* (1+ x) xdenom)
						  COUNT (PLUSP (AREF int yy xx))))
			      (TRUNCATE (* xdenom ydenom) 2))
			   (SETF (AREF ncd y x) 1))))
		    
     ncd))

(DEFUN SCALE-CHARACTER-DESCRIPTOR-TWO (cd xnum xdenom ynum ydenom)
  (LET* ((hite (ARRAY-DIMENSION cd 0))
	  (wide (ARRAY-DIMENSION cd 1))
	  (new-hite (CEILING (* hite ynum) ydenom))
	  (new-wide (CEILING (* wide xnum) xdenom))
	  (ncd (make-char-descriptor
		 :make-array (:length (LIST new-hite new-wide) :type (ARRAY-TYPE cd))
		 :cd-char-left-kern (FLOOR (* (cd-char-left-kern cd) xnum) xdenom) 
		 :cd-char-width (CEILING (* (cd-char-width cd) xnum) xdenom))))
     (LOOP for y from 0 below hite
	   DO (LOOP for x from 0 below wide
		    DO (SETF (AREF ncd (FLOOR (* y ynum) ydenom) (FLOOR (* x xnum) xdenom))
			     (AREF cd y x))  ))
     ncd))


(DEFUN COM-THICKEN-FONT ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
      (BARF "No current font")
      (LET (FONT FNAME GOOD LEN FD NFD)
	(WHEN (MULTIPLE-VALUE-SETQ (GOOD FONT FNAME)
		(GET-NEW-FONT-NAME #\B "bold"))
	   (SETQ FD (FONT-GET-FD CURRENT-FONT)
		 LEN (ARRAY-ACTIVE-LENGTH FD))
	   (SETQ NFD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH LEN)
					   :FD-NAME FNAME
					   :FD-FILL-POINTER (FD-FILL-POINTER FD)
					   :FD-LINE-SPACING (FD-LINE-SPACING FD)
					   :FD-BASELINE (FD-BASELINE FD)
					   :FD-BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)
					   :FD-BLINKER-WIDTH (1+ (FD-BLINKER-WIDTH FD))
					   :FD-SPACE-WIDTH (1+ (FD-SPACE-WIDTH FD))))
	     ;;  now transfer characters from old font into new one thickening as we go
	   (DO ((I 0 (1+ I))
		(CD)
		(NCD))
	       ((>= I LEN))
	     (AND (SETQ CD (AREF FD I))
		  (LET ((WIDTH (ARRAY-DIMENSION CD 1))
			(HEIGHT (ARRAY-DIMENSION CD 0)))
		    (SETQ NCD (MAKE-CHAR-DESCRIPTOR
				:MAKE-ARRAY (:TYPE ART-4B :LENGTH (LIST HEIGHT (IF (ZEROP WIDTH)
										   0
										   (1+ WIDTH))))
				:CD-CHAR-WIDTH (1+ (CD-CHAR-WIDTH CD))
				:CD-CHAR-LEFT-KERN (CD-CHAR-LEFT-KERN CD)))
		    (DOTIMES (J HEIGHT)
			     (DOTIMES (I WIDTH)
				      (SETF (AREF NCD J I) (LOGIOR (AREF CD J I) (AREF NCD J I)))
				      (SETF (AREF NCD J (1+ I)) (AREF CD J I))))
		    (SETF (AREF NFD I) NCD))))
	   (ACKNOWLEDGE-NEW-FONT FNAME NFD)))))


;;; Unthicken all characters of specified font.
(DEFUN COM-UNTHICKEN-FONT ()
 "Given font-descriptor FD, make a new one whose characters are less thick. "
 (DECLARE (:SELF-FLAVOR FED))
 (IF (NULL CURRENT-FONT)
     (BARF "No current font.")
     (LET (NEWFONT FNAME GOOD  LEN FD NFD)
       (WHEN (MULTIPLE-VALUE-SETQ (good newfont fname)
	       (get-new-font-name #\N "unthickened (narrower)"))
	 (SETQ fd (font-get-fd current-font)
	       len (array-active-length fd))
	 (SETQ nfd (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH len)
						 :FD-NAME fname
						 :fd-fill-pointer (fd-fill-pointer fd)
						 :FD-LINE-SPACING (fd-line-spacing fd)
						 :FD-BASELINE (fd-baseline fd)
						 :FD-BLINKER-HEIGHT (fd-blinker-height fd)
						 :FD-BLINKER-WIDTH (MAX (1- (fd-blinker-width fd)) 1)
						 :FD-SPACE-WIDTH (MAX (1- (fd-space-width fd)) 1)))
	  ;;  now transfer characters from old font into new one unthickening as we go.
	 (LOOP for ch from 0 below len
		       as cd = (AREF fd ch)
		       WHEN cd
		       DO (SETF (AREF nfd ch) (unthicken-char-descriptor cd)) )
	 (acknowledge-new-font fname nfd)))))

(DEFUN UNTHICKEN-CHAR-DESCRIPTOR (cd)
   (LET* ((WIDTH  (ARRAY-DIMENSION CD 1))
	  (HEIGHT (ARRAY-DIMENSION CD 0))
	  (NCD (MAKE-CHAR-DESCRIPTOR
		 :MAKE-ARRAY (:TYPE ART-4B :LENGTH (LIST HEIGHT WIDTH))
		 :CD-CHAR-WIDTH     (MAX (1- (CD-CHAR-WIDTH CD)) 1)
		 :CD-CHAR-LEFT-KERN (CD-CHAR-LEFT-KERN CD))))
     (DOTIMES (J HEIGHT)
       (DO* ((I (1- WIDTH) (1- I))
	     (RIGHT 0 THIS)
	     (THIS (AREF CD J I) LEFT)
	     LEFT)
	    ((= I 0))
	 (SETQ LEFT (AREF CD J (1- I)))
	 (SETF (AREF NCD J I)
	       (IF (AND (/= LEFT 0)
			(/= THIS 0)
			(= RIGHT 0))
		   0
		   (AREF CD J I)))))
     ncd))

;;; This isn't used????
(DEFUN COM-REGENERATE-FONT ()
  (DECLARE (:SELF-FLAVOR FED))
  (AND CURRENT-CHARACTER
       (FONT-STORE-CD CURRENT-FONT CURRENT-CHARACTER nil))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR CURRENT-FONT (FONT-GET-FD CURRENT-FONT)))
 

;;; CHARACTER LEVEL MODIFICATION OPERATIONS

(DEFUN COM-REFLECT-CHARACTER (&AUX AXIS)
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (AND (SETQ AXIS (FED-CHOOSE '(("X") ("Y") ("XY") ("-XY")) "Axis to reflect in"))
	   (REFLECT-CHARACTER AXIS)))) 


(DEFUN REFLECT-CHARACTER (AXIS &AUX NEW-CHAR ORIGINS EXTENTS)
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (PROGN
	(SETQ NEW-CHAR (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
	(SETQ ORIGINS (PLANE-ORIGIN PLANE))
	(SETQ EXTENTS (ARRAY-DIMENSIONS PLANE))
	(DO ((HPOS (FIRST ORIGINS) (1+ HPOS))
	     (HEND (+ (FIRST ORIGINS) (FIRST EXTENTS))))
	    ((>= HPOS HEND))
	  (DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
	       (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
	      ((>= VPOS VEND))
	    (LET ((NEWVPOS VPOS)
		  (NEWHPOS HPOS))
	      (COND
		((EQUALP AXIS "X") (SETQ NEWVPOS (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) VPOS)))
		((EQUALP AXIS "Y") (SETQ NEWHPOS (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) HPOS)))
		((EQUALP AXIS "-XY")
		 (SETQ NEWHPOS (+ CHAR-BOX-X1 (- VPOS CHAR-BOX-Y1))
		       NEWVPOS (+ CHAR-BOX-Y1 (- HPOS CHAR-BOX-X1))))
		((EQUALP AXIS "XY")
		 ;; Invert in the origin, then reflect in X-Y.
		 (SETQ NEWVPOS (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) VPOS))
		 (SETQ NEWHPOS (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) HPOS))
		 (PSETQ NEWHPOS (+ CHAR-BOX-X1 (- NEWVPOS CHAR-BOX-Y1)) NEWVPOS
			(+ CHAR-BOX-Y1 (- NEWHPOS CHAR-BOX-X1)))))
	      (PLANE-ASET (PLANE-AREF PLANE HPOS VPOS) NEW-CHAR NEWHPOS NEWVPOS))))
	(SETQ PLANE NEW-CHAR)
	(SETQ UNSAVED-CHANGES T)
	(SETQ REDISPLAY-DEGREE REDISPLAY-ALL)))) 

(DEFUN COM-THICKEN-CHARACTER (&AUX NEW-CHAR ORIGINS EXTENTS)
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (PROGN
	(SETQ NEW-CHAR (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
	(SETQ ORIGINS (PLANE-ORIGIN PLANE))
	(SETQ EXTENTS (ARRAY-DIMENSIONS PLANE))
	(DO ((HPOS (FIRST ORIGINS) (1+ HPOS))
	     (HEND (+ (FIRST ORIGINS) (FIRST EXTENTS))))
	    ((>= HPOS HEND))
	  (DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
	       (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
	      ((>= VPOS VEND))
	    (PLANE-ASET (LOGIOR (PLANE-AREF NEW-CHAR HPOS VPOS) (PLANE-AREF PLANE HPOS VPOS)) NEW-CHAR
			HPOS VPOS)
	    (PLANE-ASET (PLANE-AREF PLANE HPOS VPOS) NEW-CHAR (1+ HPOS) VPOS)))
	(SETQ CHAR-BOX-X2 (1+ CHAR-BOX-X2))
	(SETQ PLANE NEW-CHAR)
	(SETQ UNSAVED-CHANGES T)
	(SETQ REDISPLAY-DEGREE REDISPLAY-ALL)))) 

;;; Unthicken the current character being edited.
(DEFUN COM-UNTHICKEN-CHARACTER (&AUX NEW-CHAR ORIGINS EXTENTS)
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (PROGN
	(SETQ NEW-CHAR (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
	(SETQ ORIGINS (PLANE-ORIGIN PLANE))
	(SETQ EXTENTS (ARRAY-DIMENSIONS PLANE))
	(DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
	     (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
	    ((>= VPOS VEND))
	  (DO* ((HPOS (1- (+ (FIRST ORIGINS) (FIRST EXTENTS))) (1- HPOS))
		(HEND (FIRST ORIGINS))
		(right 0 this)
		(this (PLANE-AREF plane hpos vpos) left)
		left)
	       ((<= HPOS HEND))
	    (SETQ left (PLANE-AREF plane (1- hpos) vpos))
	    (IF (AND (/= left 0)
		     (/= this 0)
		     (= right 0))
		(PLANE-ASET 0 new-char hpos vpos)
		(PLANE-ASET (PLANE-AREF plane hpos vpos) new-char hpos vpos))))
	(SETQ CHAR-BOX-X2 (MAX (1- CHAR-BOX-X2) (1+ CHAR-BOX-X1)))
	(SETQ PLANE NEW-CHAR)
	(SETQ UNSAVED-CHANGES T)
	(SETQ REDISPLAY-DEGREE REDISPLAY-ALL))))


(DEFUN COM-ROTATE-CHARACTER-RIGHT (&AUX NEW-CHAR ORIGINS EXTENTS)
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (PROGN
	(SETQ NEW-CHAR (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
	(SETQ ORIGINS (PLANE-ORIGIN PLANE))
	(SETQ EXTENTS (ARRAY-DIMENSIONS PLANE))
	(DO ((HPOS (FIRST ORIGINS) (1+ HPOS))
	     (HEND (+ (FIRST ORIGINS) (FIRST EXTENTS))))
	    ((>= HPOS HEND))
	  (DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
	       (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
	      ((>= VPOS VEND))
	    (LET ((NEWVPOS (+ CHAR-BOX-Y1 (- HPOS CHAR-BOX-X1)))
		  (NEWHPOS (- CHAR-BOX-X2 1 (- VPOS CHAR-BOX-Y1))))
	      (PLANE-ASET (PLANE-AREF PLANE HPOS VPOS) NEW-CHAR NEWHPOS NEWVPOS))))
	(SETQ PLANE NEW-CHAR)
	(SETQ UNSAVED-CHANGES T)
	(SETQ REDISPLAY-DEGREE REDISPLAY-ALL)))) 


(DEFUN COM-ROTATE-CHARACTER-LEFT (&AUX NEW-CHAR ORIGINS EXTENTS)
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (PROGN
	(SETQ NEW-CHAR (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
	(SETQ ORIGINS (PLANE-ORIGIN PLANE))
	(SETQ EXTENTS (ARRAY-DIMENSIONS PLANE))
	(DO ((HPOS (FIRST ORIGINS) (1+ HPOS))
	     (HEND (+ (FIRST ORIGINS) (FIRST EXTENTS))))
	    ((>= HPOS HEND))
	  (DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
	       (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
	      ((>= VPOS VEND))
	    (LET ((NEWVPOS (+ CHAR-BOX-Y1 (- CHAR-BOX-X2 HPOS 1)))
		  (NEWHPOS (+ CHAR-BOX-X1 1 (- VPOS CHAR-BOX-Y1 1))))
	      (PLANE-ASET (PLANE-AREF PLANE HPOS VPOS) NEW-CHAR NEWHPOS NEWVPOS))))
	(SETQ PLANE NEW-CHAR)
	(SETQ UNSAVED-CHANGES T)
	(SETQ REDISPLAY-DEGREE REDISPLAY-ALL)))) 


(DEFUN COM-ROTATE-CHARACTER-180 (&AUX NEW-CHAR ORIGINS EXTENTS)
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (PROGN
	(SETQ NEW-CHAR (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
	(SETQ ORIGINS (PLANE-ORIGIN PLANE))
	(SETQ EXTENTS (ARRAY-DIMENSIONS PLANE))
	(DO ((HPOS (FIRST ORIGINS) (1+ HPOS))
	     (HEND (+ (FIRST ORIGINS) (FIRST EXTENTS))))
	    ((>= HPOS HEND))
	  (DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
	       (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
	      ((>= VPOS VEND))
	    (LET ((NEWVPOS (+ CHAR-BOX-Y1 (- CHAR-BOX-X2 CHAR-BOX-X1 VPOS 1)))
		  (NEWHPOS (- CHAR-BOX-X2 1 (- HPOS CHAR-BOX-X1))))
	      (PLANE-ASET (PLANE-AREF PLANE HPOS VPOS) NEW-CHAR NEWHPOS NEWVPOS))))
	(SETQ PLANE NEW-CHAR)
	(SETQ UNSAVED-CHANGES T)
	(SETQ REDISPLAY-DEGREE REDISPLAY-ALL)))) 

(DEFUN COM-REVERSE-VIDEO-CHARACTER ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (PROGN
	(DO ((HPOS char-box-x1 (1+ HPOS))
	     (HEND char-box-x2))
	    ((>= HPOS HEND))
	  (DO ((VPOS char-box-y1 (1+ VPOS))
	       (VEND char-box-y3))
	      ((>= VPOS VEND))
	    (PLANE-ASET (ABS (1- (PLANE-AREF PLANE HPOS VPOS))) plane HPOS VPOS)))
	(SETQ UNSAVED-CHANGES T)
	(SETQ REDISPLAY-DEGREE REDISPLAY-ALL))))

;;; Italicize the current character being edited
(DEFUN COM-ITALICIZE-CHARACTER (&AUX NEW-CHAR ORIGINS EXTENTS FD BITS-PER-SHIFT NUMBER-OF-SHIFTS)
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (PROGN
	(SETQ NEW-CHAR (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10)
	      ORIGINS (PLANE-ORIGIN PLANE)
	      EXTENTS (ARRAY-DIMENSIONS PLANE)
	      FD (font-get-fd current-font)
	      BITS-PER-SHIFT (MIN 3 (TRUNCATE (fd-blinker-height FD) 3))
	      NUMBER-OF-SHIFTS (CEILING (fd-line-spacing FD) BITS-PER-SHIFT))
	(LET* ((WIDTH (1+ (- char-box-x2 char-box-x1)))
	       (HEIGHT (1+ (- char-box-y3 char-box-y1)))
	       (NEW-WIDTH 0)
	       (THIS-WIDTH 0)
	       (NEW-HEIGHT 0)
	       (NEW-OFFSET WIDTH)
	       (THIS-OFFSET WIDTH)
	       LEADING-ZEROP
	       (SHIFT-AMOUNT NUMBER-OF-SHIFTS)
	       (BIT-NUMBER BITS-PER-SHIFT))
	  (DOTIMES (J HEIGHT)
		   (SETQ LEADING-ZEROP T)
		   (DOTIMES (I WIDTH)
			    (WHEN LEADING-ZEROP
				  (SETQ LEADING-ZEROP NIL)
				  (IF (< I THIS-OFFSET)
				      (SETQ THIS-OFFSET I))
				  (IF (< (+ SHIFT-AMOUNT I) NEW-OFFSET)
				      (SETQ NEW-OFFSET (+ SHIFT-AMOUNT I))))
			    (IF (> I THIS-WIDTH)
				(SETQ THIS-WIDTH I))
			    (IF (> (+ SHIFT-AMOUNT I) NEW-WIDTH)
				(SETQ NEW-WIDTH (+ SHIFT-AMOUNT I)))
			    (IF (> J NEW-HEIGHT)
				(SETQ NEW-HEIGHT J)))
		   (SETQ BIT-NUMBER (1- BIT-NUMBER))
		   (IF (ZEROP BIT-NUMBER)
		       (SETQ BIT-NUMBER BITS-PER-SHIFT
			     SHIFT-AMOUNT (1- SHIFT-AMOUNT))))
	  (IF (NOT (ZEROP THIS-WIDTH))
	      (SETQ THIS-WIDTH (1+ THIS-WIDTH)))
	  (IF (NOT (ZEROP NEW-WIDTH))
	      (SETQ NEW-WIDTH  (- (1+ NEW-WIDTH) NEW-OFFSET)))
	  (SETQ NEW-OFFSET (- NEW-OFFSET THIS-OFFSET))
	  (SETQ NEW-WIDTH  (+ NEW-WIDTH  THIS-OFFSET))
	  (SETQ NEW-HEIGHT (1+ (- char-box-y2 char-box-y1)))
	  (LET ((SHIFT-AMOUNT (- NUMBER-OF-SHIFTS NEW-OFFSET))
		(BIT-NUMBER BITS-PER-SHIFT))
	    (DOTIMES (J HEIGHT)
		     (DOTIMES (I WIDTH)
			      (PLANE-ASET (PLANE-AREF plane i j) new-char (1- (+ shift-amount i)) j))
		     (SETQ BIT-NUMBER (1- BIT-NUMBER))
		     (IF (ZEROP BIT-NUMBER)
			 (SETQ BIT-NUMBER BITS-PER-SHIFT
			       SHIFT-AMOUNT (1- SHIFT-AMOUNT))))))
	(SETQ PLANE NEW-CHAR)
	(SETQ UNSAVED-CHANGES T)
	(SETQ REDISPLAY-DEGREE REDISPLAY-ALL))))

;;; Rewrote to edit the values for the scale inputs to be positive integers.  dkm 5/15/87

(DEFUN COM-SCALE-CHARACTER ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-CHARACTER)
      (BARF "No current character")
      (LET (SCALE-LIST)
	(DOLIST (PROMPT '("Numerator of X scale factor (default 1): "
			  "Denominator of X scale factor (default 1): "
			  "Numerator of Y scale factor (default 1): "
			  "Denominator of Y scale factor (default 1): "))
	  (PUSH-END (DO (INPUT)  ((NOT (NULL INPUT)) INPUT)
		      (SETQ INPUT (PROMPT-LINE-DEFAULTED-READLINE 1 'READ-FROM-STRING PROMPT))
		      (IF (AND (INTEGERP INPUT) (PLUSP INPUT))
			  T
			  (SETQ INPUT NIL)
			  (barf "~&Input must be an integer")))
		    SCALE-LIST))
	(MULTIPLE-VALUE-BIND (XNUM XDENOM YNUM YDENOM)
	    (VALUES-LIST SCALE-LIST)
	  (SCALE-CHARACTER XNUM XDENOM YNUM YDENOM)))))
	

(DEFUN SCALE-CHARACTER (XNUM XDENOM YNUM YDENOM)
  (DECLARE (:SELF-FLAVOR FED))
  (SEND SELF :MUST-REDISPLAY-CURRENT-PLANE-AREA)
  (SETQ UNSAVED-CHANGES T)
  (LET* ((OPLANE PLANE)
	 (XOFFS (+ (TRUNCATE (* (- (FIRST (PLANE-ORIGIN OPLANE))
				   (TRUNCATE (+ CHAR-BOX-X1 CHAR-BOX-X2) 2)) XNUM)
			     XDENOM)
		   (TRUNCATE (+ CHAR-BOX-X1 CHAR-BOX-X2) 2)))
	 (YOFFS (+ (TRUNCATE (* (- (SECOND (PLANE-ORIGIN OPLANE)) CHAR-BOX-Y2) YNUM) YDENOM) CHAR-BOX-Y2))
	 (XEND (+ XOFFS (TRUNCATE (* (ARRAY-DIMENSION OPLANE 0) XNUM) XDENOM)))
	 (YEND (+ YOFFS (TRUNCATE (* (ARRAY-DIMENSION OPLANE 1) YNUM) YDENOM))))
    (SETQ PLANE (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
    (PLANE-ENSURE-EXISTS PLANE XOFFS YOFFS)
    (PLANE-ENSURE-EXISTS PLANE (1- XEND) (1- YEND))
    (LET ((XORG (FIRST (PLANE-ORIGIN PLANE)))
	  (YORG (SECOND (PLANE-ORIGIN PLANE)))
	  (BIG (MAKE-ARRAY (LIST (* (ARRAY-DIMENSION OPLANE 0) XNUM) (* (ARRAY-DIMENSION OPLANE 1) YNUM))
			   :TYPE ART-1B)))		;
      (DO ((I (1- (ARRAY-DIMENSION OPLANE 0)) (1- I)))
	  ((MINUSP I) NIL)
	(DO ((J (1- (ARRAY-DIMENSION OPLANE 1)) (1- J)))
	    ((MINUSP J) NIL)
	  (IF (NOT (ZEROP (AREF OPLANE I J)))
	    (DO ((M 0 (1+ M)))
		((= M XNUM) NIL)
	      (DO ((N 0 (1+ N)))
		  ((= N YNUM)
		   (SETF (AREF BIG (+ (* I XNUM) M) (+ (* J YNUM) N)) 1)))))))
      (DO ((I XOFFS (1+ I)))
	  ((= I XEND) NIL)
	(DO ((J YOFFS (1+ J)))
	    ((= J YEND) NIL)
	  (IF (>
	    (LOOP FOR X FROM (* (- I XOFFS) XDENOM) BELOW (* (- I XOFFS -1) XDENOM)
		  SUMMING (LOOP FOR Y FROM (* (- J YOFFS) YDENOM) BELOW (* (- J YOFFS -1) YDENOM)
				COUNT (NOT (ZEROP (AREF BIG X Y)))))
	    (TRUNCATE (* XDENOM YDENOM) 2))
	    (SETF (AREF PLANE (- I XORG) (- J YORG)) 1))))
      (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE)))) 


