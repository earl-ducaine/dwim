;;; -*- Mode:Common-Lisp; Package:FED; Base:8; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B) -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.
 
;	FD (or Font Descriptor) Format is used as a machine resident format
;		which is easily manipulated.  The format consists of a 200
;		or more element array with a leader.  The elements of this array are
;		themselves two dimensional arrays which contain the actual
;		pixel values for the character.
;	FONT (or internal) Format is the format actually used by the tv display
;		routines.  The format is fairly complicated and its direct
;		use is not recommended when a conversion to FD format would
;		be better.
;	KST format is used for communication with the PDP-10.
;	AL format is used for ALTO fonts.
;	AC another xerox format.
;	KS kerned strike xerox format.
;	AST stars and spaces.
 
;   Functions to convert between FONT and FONT-DESCRIPTOR are:
;      FONT-INTO-FONT-DESCRIPTOR FONTNAME => FONT-DESCRIPTOR
;         (you might really want FONT-NAME-FONT-DESCRIPTOR, which remembers the FONT-DESCRIPTOR)
;      FONT-DESCRIPTOR-INTO-FONT FONT-DESCRIPTOR => FONTNAME

;   Functions exist which will convert various formats of fonts back and forth from some
;host's font format to the Lisp Machine's format.  The best way to do this is to
;go from HOST format <---> AST format <---> Lisp Machine format, as AST is an ASCII format
;that can be edited in a text editor, can be easily transmitted over a network, and is 
;supported by lots of other hosts.  If AST is not supported, direct conversion to several
;other formats are supported.  

;   The generic functions names for converting between fonts is:
;       READ-X-INTO-FONT FILENAME &OPTIONAL FONTNAME => FONTNAME
;       READ-X-INTO-FONT-DESCRIPTOR FILENAME &OPTIONAL FONTNAME => FONT-DESCRIPTOR
;       WRITE-FONT-INTO-X FONTNAME &OPTIONAL FILENAME => OUTPUT-TRUENAME
;       WRITE-FONT-DESCRIPTOR-INTO-X FONT-DESCRIPTOR &OPTIONAL FILENAME => OUTPUT-TRUENAME

;Other useful functions in here:
; THICKEN-FONT[-DESCRIPTOR], makes B(old) fonts.
; ROTATE-FONT[-DESCRIPTOR], makes R(otated) fonts.  I.e. landscape from portrait.

; CD-RASTER-RANGE CHAR-DESCRIPTOR => MINX MINY MAXX MAXY
; CD-UNUSED-RASTER-HEIGHT CHAR-DESCRIPTOR => BOTTOM TOP


(DEFUN INTERN-FONT-NAME (STRING)
  "Given a STRING, return the font-name symbol of that name."
  (INTERN (STRING-UPCASE STRING) "FONTS")) 

(DEFUN FONT-GET-FD (FONT-SYMBOL &AUX FD)
  "Return the font descriptor corresponding to the font named FONT-SYMBOL.
This is an object of type FONT-DESCRIPTOR which contains the same
data as the font itself, but in a more convenient format.
If FONT-SYMBOL is not an existing font, create an empty FD for it."
  (IF (BOUNDP FONT-SYMBOL)
    (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL)
    (PROGN
      (SETQ FD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH *MAX-FONT-LENGTH*)
				     :FD-NAME FONT-SYMBOL
				     :FD-LINE-SPACING 14
				     :FD-BASELINE 11
				     :FD-BLINKER-HEIGHT 14
				     :FD-BLINKER-WIDTH 7
				     :FD-SPACE-WIDTH 7))
      (SETF (AREF FD (CHAR-INT #\SPACE))
	    (MAKE-CHAR-DESCRIPTOR :MAKE-ARRAY (:TYPE ART-4B :DIMENSIONS '(11 7))
				  :CD-CHAR-WIDTH 7
				  :CD-CHAR-LEFT-KERN 0))
      (SETF (GET FONT-SYMBOL 'FONT-DESCRIPTOR) FD)
      (SET FONT-SYMBOL nil)
      (SETF (GET FONT-SYMBOL 'FONT-DESCRIBED) nil)
      FD))) 

(DEFUN FED-DISPLAY-FONT-CHAR-WIDTH (FD DF CH)
  "Return the width of char CH in font DF or font descriptor FD, whichever is larger."
  (MAX (FED-CHAR-DISPLAY-WIDTH FD CH) (FONT-CHARACTER-WIDTH DF CH))) 


(DEFUN FED-CHAR-DISPLAY-WIDTH (FD CHAR)
  (COND
    ((AND (< CHAR (ARRAY-TOTAL-SIZE FD)) (AREF FD CHAR))
     (+ 3 (ARRAY-DIMENSION (AREF FD CHAR) 1) (MAX 0 (- (CD-CHAR-LEFT-KERN (AREF FD CHAR))))))
    (T 0))) 

;;; Return the width of a given char in a given font.
(DEFUN FONT-CHARACTER-WIDTH (FONT CHAR)
  "Return the width of char CHAR in font FONT."
  (LET ((CWT (TV:FONT-CHAR-WIDTH-TABLE FONT)))
    (IF CWT
      (AREF CWT CHAR)
      (TV:FONT-CHAR-WIDTH FONT))))
 
(DEFUN CD-TRUE-RASTER-WIDTH (CD)
  "Returns two values, the raster width of the character, and a flag set 
true if the CD can be saved inside the boundaries of the character box.
'Raster width' is defined here as the minimum width required to store
the character in TV format.  It could take more since storage is based
on the font's raster width rather than any individual character's width."
  ;;; For characters (not CD's) with nonzero kerns, the raster width is
  ;;; the CD's array width (ARRAY-DIMENSION CD 1), since that is all that's
  ;;; stored in TV format.  But characters whose pixels all fit in their
  ;;; character box are stored *starting from the box's left edge* and 
  ;;; going to the rightmost pixel column.  Any empty columns
  ;;; between the box's left edge and the leftmost column of pixels
  ;;; would be ignored if we didn't compensate.  The kern of the CD
  ;;; tells us this distance.
  (DECLARE (VALUES (RASTER-WIDTH FLAG)))
  (LET ((CD-WIDTH (ARRAY-DIMENSION CD 1))
	(CD-KERN (CD-CHAR-LEFT-KERN CD)))
    (COND
      ((AND (<= CD-KERN 0);L edge of char to R of L edge of box
	    (< (- CD-WIDTH CD-KERN) (CD-CHAR-WIDTH CD)))
       (VALUES (- CD-WIDTH CD-KERN) T))
      (T (VALUES CD-WIDTH nil))))) 



(DEFUN MAX-RASTER-WIDTH (FONT-DESCRIPTOR &AUX (GUESS 0) CD)
  "Return the maximum raster width of all characters in FONT-DESCRIPTOR."
  (DO ((CHAR-CODE 0 (1+ CHAR-CODE))
       (FONT-LENGTH (ARRAY-ACTIVE-LENGTH FONT-DESCRIPTOR)))
      ((>= CHAR-CODE FONT-LENGTH)
       GUESS)
    (COND
      ((SETQ CD (AREF FONT-DESCRIPTOR CHAR-CODE))
       (SETQ GUESS (MAX GUESS (CD-TRUE-RASTER-WIDTH CD))))))) 



(DEFUN MAX-RASTER-HEIGHT (FONT-DESCRIPTOR &AUX (GUESS 0) TEMP)
  "Return the maximum raster height of all characters in FONT-DESCRIPTOR."
  (DO ((CHAR-CODE 0 (1+ CHAR-CODE))
       (FONT-LENGTH (ARRAY-ACTIVE-LENGTH FONT-DESCRIPTOR)))
      ((>= CHAR-CODE FONT-LENGTH)
       GUESS)
    (COND
      ((SETQ TEMP (AREF FONT-DESCRIPTOR CHAR-CODE))
       (SETQ GUESS (MAX GUESS (ARRAY-DIMENSION TEMP 0))))))) 

;;; Memoizing version of FONT-INTO-FONT-DESCRIPTOR
;;; that wants a font name (symbol in FONTS:) rather than the font itself.
;;; The FONT-DESCRIPTOR property of the symbol holds the descriptor.
;;; The FONT-DESCRIBED property holds the font itself which the descriptor matches.
;;; If anyone changes the font, we can see that the old descriptor is no good.

(DEFUN FONT-NAME-FONT-DESCRIPTOR (FONTNAME &AUX FD)
  "Return a font-descriptor whose data is equivalent to font FONTNAME.
If we computed one previously, we return it; otherwise, we create one.
This assumes that anyone who alters the contents of the font
also alters the corresponding font descriptor."
  (OR (SYMBOLP FONTNAME) (SETQ FONTNAME (TV:FONT-NAME FONTNAME)))
  (SETQ FD (GET FONTNAME 'FONT-DESCRIPTOR))
  (COND
    ((AND FD (EQ (GET FONTNAME 'FONT-DESCRIBED) (SYMBOL-VALUE FONTNAME))))
    (T (SETQ FD (FONT-INTO-FONT-DESCRIPTOR (SYMBOL-VALUE FONTNAME)))
     (SETF (GET FONTNAME 'FONT-DESCRIBED) (SYMBOL-VALUE FONTNAME))
     (SETF (GET FONTNAME 'FONT-DESCRIPTOR) FD)))
  FD) 

;;; Set a font given a font descriptor.  Keep the descriptor around.
;;; Forward the old definition of the font to the new one.

(DEFUN FONT-NAME-SET-FONT-AND-DESCRIPTOR (FONTNAME FONT-DESCRIPTOR)
  "Define or redefine a font named FONTNAME from data in FONT-DESCRIPTOR."
  (LET ((OLDFONT (AND (BOUNDP FONTNAME) (SYMBOL-VALUE FONTNAME))))
    (SET FONTNAME (FONT-DESCRIPTOR-INTO-FONT FONT-DESCRIPTOR))
    (AND OLDFONT (STRUCTURE-FORWARD OLDFONT (SYMBOL-VALUE FONTNAME)))
    (SETF (GET FONTNAME 'FONT-DESCRIPTOR) FONT-DESCRIPTOR)
    (SETF (GET FONTNAME 'FONT-DESCRIBED) (SYMBOL-VALUE FONTNAME))
    FONT-DESCRIPTOR)) 

;;; Given FONTNAME and FILENAME from FONTNAME &OPTIONAL FILENAME, canonicalize them.

(DEFUN GET-OUTPUT-FILENAME-AND-FONTNAME (FONTNAME FILENAME FILE-TYPE)
  (DECLARE (VALUES FONTNAME FILENAME))
  (AND (STRINGP FONTNAME) (SETQ FONTNAME (INTERN (STRING-UPCASE FONTNAME) "FONTS")))
  (OR FILENAME (SETQ FILENAME (STRING FONTNAME)))
  (SETQ FILENAME (FS:MERGE-PATHNAME-DEFAULTS FILENAME (PATHNAME-DEFAULTS) FILE-TYPE))
  (VALUES FONTNAME FILENAME)) 


(DEFUN GET-OUTPUT-FILENAME-FROM-FONT-DESCRIPTOR (FONT-DESCRIPTOR FILENAME FILE-TYPE)
  (OR FILENAME (SETQ FILENAME (STRING (FD-NAME FONT-DESCRIPTOR))))
  (FS:MERGE-PATHNAME-DEFAULTS FILENAME (PATHNAME-DEFAULTS) FILE-TYPE)) 

(DEFUN FONT-NAME-STORE-CD (FONTNAME CD CHAR-CODE)
  "Store character CHAR-CODE in font named FONTNAME from data in CD.
CD should be a CHARACTER-DESCRIPTOR.  Both the font itself
and any remembered font-descriptor are modified.
A new font array is constructed if the new character's raster size
is too big for the old one."
  (LET ((WIDTH (CD-TRUE-RASTER-WIDTH CD))
	(HEIGHT (ARRAY-DIMENSION CD 0))
	TEM FD FONT)
    (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONTNAME))
    (FD-STORE-CD FD CD CHAR-CODE)
    (WHEN (= CHAR-CODE (CHAR-INT #\SPACE))
	 (SETF (FD-SPACE-WIDTH FD) (CD-CHAR-WIDTH CD)))
    (COND
      ((OR (NOT (BOUNDP FONTNAME))
	   (NULL (SETQ FONT (SYMBOL-VALUE FONTNAME)))
	   (>= CHAR-CODE (TV:FONT-FILL-POINTER FONT)) ;font array size determined by *max-font-length*
	   (> WIDTH
	      (COND
		((SETQ TEM (TV:FONT-INDEXING-TABLE FONT))	
		 (* (TV:FONT-RASTER-WIDTH FONT) (- (AREF TEM (1+ CHAR-CODE)) (AREF TEM CHAR-CODE))))
		(T (TV:FONT-RASTER-WIDTH FONT))))
	  (> HEIGHT (TV:FONT-RASTER-HEIGHT FONT)))
       ;; ... then, redefine the font and font-descriptor
       (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME FD))
      ;; otherwise, just store the new char into the font
      (T (STORE-CD-IN-FONT CD FONT CHAR-CODE nil))))) 


(DEFUN FD-STORE-CD (FD CD CH)
  "Store character-descriptor CD as character CH in font descriptor FD."
  (AND (>= CH (ARRAY-TOTAL-SIZE FD)) (ADJUST-ARRAY FD (+ CH 100)))
  (AND (>= CH (FD-FILL-POINTER FD)) (SETF (FD-FILL-POINTER FD) (1+ CH)))
  (SETF (AREF FD CH) CD)) 

;Functions for referring to specified pixels of characters in an internal format font.

;ROW and COL are measured from top/left as usual.  An alternative would be:
;	COL is measured from the left, with Kerning hacked.
;	ROW is positive above the baseline and negative below.
;  (SETQ ROW (- (TV:FONT-BASELINE FONT) ROW))
;  (AND (SETQ TEM (TV:FONT-LEFT-KERN-TABLE FONT))
;       (SETQ COL (+ COL (AREF TEM CHAR))))
;However it looks like this would cause more trouble than it would save.
;Attempts to reference outside of the raster return 0, or barf if storing.
;Conceivably it might be good to not barf at attempts to store 0 out of bounds?


(DEFUN FONT-GET-PIXEL (FONT CHAR ROW COL &AUX TEM (NEXTCHAR (1+ CHAR)))
  "Get the pixel at position ROW, COL in character CHAR of FONT.
FONT should be a font array, not a name."
  (COND
    ((OR (< ROW 0) (>= ROW (TV:FONT-RASTER-HEIGHT FONT)) (< COL 0)
	(COND
	  ((SETQ TEM (TV:FONT-INDEXING-TABLE FONT))
	   (SETQ CHAR (+ (AREF TEM CHAR) (TRUNCATE COL (TV:FONT-RASTER-WIDTH FONT))))
	   (SETQ COL (REM COL (TV:FONT-RASTER-WIDTH FONT))) (>= CHAR (AREF TEM NEXTCHAR)))
	  ((>= COL (TV:FONT-RASTER-WIDTH FONT)))))
     0);out of bounds, return 0
    (T
     (DO ((FONT FONT (TV:FONT-NEXT-PLANE FONT))
	  (PIXEL 0)
	  (PLANENUM 0 (1+ PLANENUM)))
	 ((NULL FONT)
	  PIXEL)
       (SETQ PIXEL
	     (+ PIXEL
		(LSH
		 (AREF FONT
		       (+
			(* 32.
			   (+ (* (TV:FONT-WORDS-PER-CHAR FONT) CHAR)
			      (TRUNCATE ROW (TV:FONT-RASTERS-PER-WORD FONT))))
			(+
			 (* (TV:FONT-RASTER-WIDTH FONT)
			    (REM ROW (TV:FONT-RASTERS-PER-WORD FONT)))
			 COL)))
		 PLANENUM))))))) 


(DEFUN FONT-SET-PIXEL (PIXEL FONT CHAR ROW COL &AUX TEM (NEXTCHAR (1+ CHAR)))
  "Set the pixel at position ROW, COL in character CHAR of font FONT to PIXEL.
FONT should be a font array, not a name."
  (COND
    ((OR (< ROW 0) (>= ROW (TV:FONT-RASTER-HEIGHT FONT)) (< COL 0)
	(COND
	  ((SETQ TEM (TV:FONT-INDEXING-TABLE FONT))
	   (SETQ CHAR (+ (AREF TEM CHAR) (TRUNCATE COL (TV:FONT-RASTER-WIDTH FONT))))
	   (SETQ COL (REM COL (TV:FONT-RASTER-WIDTH FONT))) (>= CHAR (AREF TEM NEXTCHAR)))
	  ((>= COL (TV:FONT-RASTER-WIDTH FONT)))))
     (FERROR nil "Store of ~C in ~S at ~O,~O out of character bounds" CHAR FONT ROW COL))
    (T
     (DO ((FONT FONT (TV:FONT-NEXT-PLANE FONT))
	  (BIT PIXEL (LSH BIT -1)))
	 ((NULL FONT)
	  PIXEL)
       (SETF
	(AREF FONT
	      (+
	       (* 32.
		  (+ (* (TV:FONT-WORDS-PER-CHAR FONT) CHAR)
		     (TRUNCATE ROW (TV:FONT-RASTERS-PER-WORD FONT))))
	       (+ (* (TV:FONT-RASTER-WIDTH FONT) (REM ROW (TV:FONT-RASTERS-PER-WORD FONT))) COL)))
	BIT))))) 

;This function takes an FD format font and creates an internal format
;	font from it.  All of the hairy formats of the stored font
;	are taken care of by this function so the user doesn't have
;	to worry about them.

(DEFUN FONT-DESCRIPTOR-INTO-FONT (FONT-DESCRIPTOR &OPTIONAL (NBR-PLANES-OUT NIL))
  (LET* ((FONT-OUT NIL)
	 (FONT-DESCRIPTOR-LENGTH (ARRAY-ACTIVE-LENGTH FONT-DESCRIPTOR))
	 (FONT-LENGTH (MAX FONT-DESCRIPTOR-LENGTH *MIN-FONT-LENGTH*))
	 (COL-INCR (COND ((FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR) 2)
			 (T 1)))
	 (SPACE-WIDTH (OR (VALUES (FLOOR (+ (FD-SPACE-WIDTH FONT-DESCRIPTOR) 0.5))) 0))
	 (WIDTH (TRUNCATE SPACE-WIDTH COL-INCR))
	 (HEIGHT (FD-LINE-SPACING FONT-DESCRIPTOR))
	 (BASELINE (FD-BASELINE FONT-DESCRIPTOR))
	 (RASTER-WIDTH (MAX 1 (CEILING (MAX-RASTER-WIDTH FONT-DESCRIPTOR) COL-INCR)))
	 (RASTER-HEIGHT (MAX-RASTER-HEIGHT FONT-DESCRIPTOR))
	 (RASTERS-PER-WORD (TRUNCATE 32. (MIN 32. RASTER-WIDTH)))
	 (WORDS-PER-RASTER-ELEMENT (CEILING RASTER-HEIGHT RASTERS-PER-WORD))
	 (TOTAL-RASTER-ELEMENTS FONT-LENGTH)
	 (BLINKER-WIDTH (FLOOR (FD-BLINKER-WIDTH FONT-DESCRIPTOR) COL-INCR))
	 (BLINKER-HEIGHT (FD-BLINKER-HEIGHT FONT-DESCRIPTOR))
	 (INDEXING-TABLE NIL)
	 (CHARS-EXIST-TABLE (MAKE-ARRAY FONT-LENGTH :ELEMENT-TYPE 'BIT))
	 TEMP							      ;General temporary
	 )
    "Create a font-array from font-descriptor FONT-DESCRIPTOR.
Its name is set from that in FONT-DESCRIPTOR."
    ;;Set up NBR-PLANES-OUT if defaulted
    (COND
      ((NULL NBR-PLANES-OUT) (SETQ NBR-PLANES-OUT COL-INCR)))
    ;;Create INDEXING-TABLE if needed
    (COND
      ((> RASTER-WIDTH 32.)
       (SETQ INDEXING-TABLE (MAKE-ARRAY (1+ FONT-LENGTH) :ELEMENT-TYPE '(MOD 257.)))  ;art-16b array
       (SETF (AREF INDEXING-TABLE 0) 0)
       (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
	   ((>= CHAR-CODE FONT-LENGTH)
	    (SETQ TOTAL-RASTER-ELEMENTS (AREF INDEXING-TABLE FONT-LENGTH)))
	 (SETQ TEMP (AND (< CHAR-CODE FONT-DESCRIPTOR-LENGTH)	      ;TEMP gets char's CD
			 (AREF FONT-DESCRIPTOR CHAR-CODE)))
	 (SETF (AREF INDEXING-TABLE (1+ CHAR-CODE))
	       (+ (AREF INDEXING-TABLE CHAR-CODE)
		  (COND
		    ((NULL TEMP) 0)
		    (T (CEILING (CD-TRUE-RASTER-WIDTH TEMP) 32.))))))
       (SETQ RASTER-WIDTH 32.)))
    ;;set up all the planes of the font
    (DO ((I NBR-PLANES-OUT (1- I)))
	((ZEROP I))
      ;;Make up a (one-plane) font and make it's next plane be the last one we made
      (SETQ TEMP (TV:MAKE-FONT
		   :MAKE-ARRAY (:TYPE 'ART-1B :length (* TOTAL-RASTER-ELEMENTS WORDS-PER-RASTER-ELEMENT 32.))))
      (SETF (TV:FONT-NEXT-PLANE TEMP) FONT-OUT)
      (SETQ FONT-OUT TEMP)
      ;;Now set all the other fields in the leader
      (SETF (TV:FONT-NAME FONT-OUT) (FD-NAME FONT-DESCRIPTOR))
      (SETF (TV:FONT-CHAR-WIDTH FONT-OUT) WIDTH)
      (SETF (TV:FONT-CHAR-HEIGHT FONT-OUT) HEIGHT)
      (SETF (TV:FONT-RASTER-WIDTH FONT-OUT) RASTER-WIDTH)
      (SETF (TV:FONT-RASTER-HEIGHT FONT-OUT) RASTER-HEIGHT)
      (SETF (TV:FONT-RASTERS-PER-WORD FONT-OUT) RASTERS-PER-WORD)
      (SETF (TV:FONT-WORDS-PER-CHAR FONT-OUT) WORDS-PER-RASTER-ELEMENT)
      (SETF (TV:FONT-BASELINE FONT-OUT) BASELINE)
      (SETF (TV:FONT-BLINKER-WIDTH FONT-OUT) BLINKER-WIDTH)
      (SETF (TV:FONT-BLINKER-HEIGHT FONT-OUT) BLINKER-HEIGHT)
      (SETF (TV:FONT-NAME FONT-OUT) (FD-NAME FONT-DESCRIPTOR))
      (SETF (TV:FONT-CHARS-EXIST-TABLE FONT-OUT) CHARS-EXIST-TABLE)
      (SETF (TV:FONT-INDEXING-TABLE FONT-OUT) INDEXING-TABLE)
      (SETF (TV:FONT-FILL-POINTER FONT-OUT) FONT-LENGTH))
    (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
	((>= CHAR-CODE FONT-LENGTH))
      (SETQ TEMP (AND (< CHAR-CODE FONT-DESCRIPTOR-LENGTH) (AREF FONT-DESCRIPTOR CHAR-CODE)))
      (COND
	(TEMP (STORE-CD-IN-FONT TEMP FONT-OUT CHAR-CODE (FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR)))))
    FONT-OUT)) 

;Store the data in CD into character number CHAR-CODE of FONT (the compressed TV format).
;It is assumed that the dimensions of the CD fit within the raster dimensions of the font.
;(If this isn't true, we'll need a new font data structure, refer to FONT-DESCRIPTOR-INTO-FONT.)
;This is not recommended for users to call.

(DEFUN STORE-CD-IN-FONT (CD FONT CHAR-CODE &OPTIONAL (DOUBLE-WIDTH-P NIL))
  (LET ((FONT-LENGTH (TV:FONT-FILL-POINTER FONT))
	(WIDTH (ARRAY-DIMENSION CD 1))
	(HEIGHT (ARRAY-DIMENSION CD 0))
	(FONT-HEIGHT (TV:FONT-RASTER-HEIGHT FONT))
	(FONT-WIDTH (TV:FONT-RASTER-WIDTH FONT))
	(LOSING-KERN 0)
	(COL-INCR (COND (DOUBLE-WIDTH-P 2) (T 1)))
	STORAGE-WIDTH CHAR-FITS-IN-BOX PIXEL)
 ;;; Note:  The CD width is the distance between the leftmost and rightmost pixels
 ;;; in the character.  Its height is always the font height.  Therefore the
 ;;; left and right edges of the CD are flush against the character, but the top and
 ;;; bottom edges are not necessarily so.  The CD kern is the distance between the
 ;;; left edge of the box and the leftmost column of pixels in the character.  It is
 ;;; a signed value:  positive for pixels left of the box, zero if the two are aligned,
 ;;; and negative otherwise.  However, if the character's pixels all fit in the box,
 ;;; the value stored in the font's kern table is zero and the CD's kern value is left
 ;;; alone.
    (WHEN (>= CHAR-CODE (TV:FONT-FILL-POINTER FONT))     ;;; THIS IS WRONG.  WHERE DOES THE FONT ARRAY EXTENDED. ?
      (SETQ FONT-LENGTH *MAX-FONT-LENGTH*))            ;;; WHERE ARE THE CHAR-WIDTH, LEFT-KERN, ETC TABLES EXTENDED?

  ;; Update the font's char-width-table, creating one if necessary.  Note that positions 128.-160. inclusive are
  ;; control characters that will likely be NIL, and typically should not have a character glyph in there.
  (LET ((CW (IF (<= 128. CHAR-CODE 160.)                ;force control chars to have nil char widths.
		NIL
		(CEILING (ROUND (CD-CHAR-WIDTH CD)) COL-INCR)))
	(FCW (TV:FONT-CHAR-WIDTH FONT))
	(FCWT (TV:FONT-CHAR-WIDTH-TABLE FONT)))
    (COND
      (FCWT
       (SETF (AREF FCWT CHAR-CODE) CW))
      ((AND CW
	    (NOT (= CW FCW)))
       (SETF (TV:FONT-CHAR-WIDTH-TABLE FONT)
	     (SETQ FCWT (MAKE-ARRAY FONT-LENGTH)))
       (AND DOUBLE-WIDTH-P
	    (SETF (TV:FONT-CHAR-WIDTH-TABLE (TV:FONT-NEXT-PLANE FONT))
		  FCWT))
       (DO ((I 0 (1+ I)))
	   ((= I FONT-LENGTH))
	 (SETF (AREF FCWT I) (IF (<= 128. I 160.) NIL FCW))) ;;; initialize reserved control-char slots to NIL
       (SETF (AREF FCWT CHAR-CODE) CW)))
    (AND (= CHAR-CODE (CHAR-INT #\SPACE))
	 (SETF (TV:FONT-CHAR-WIDTH FONT) CW)))
  ;; Update the font's left-kern table, creating one if necessary.
  (LET ((CK (CD-CHAR-LEFT-KERN CD)) (FCKT (TV:FONT-LEFT-KERN-TABLE FONT)))
    ;; Try to suppress a negative CD kern should all a character's pixels fit in the
    ;; character box.  In this case, everything starting at the left edge of the box
    ;; in stored into TV format, so its leftmost columns could be empty.
    ;; Otherwise, the leftmost column of pixels will be the leftmost column of TV format.
    ;; (An implication is that a very wide character whose pixels have a narrow horizontal
    ;; spread and don't fit in the box could be stored in one subcharacter.  If the same
    ;; narrow pattern is translated so it fits entirely in the box, then it could occupy 
    ;; occupy 2 subcharacters since the leading blank columns aren't suppressed.)
    (MULTIPLE-VALUE-SETQ (STORAGE-WIDTH CHAR-FITS-IN-BOX)
      (CD-TRUE-RASTER-WIDTH CD))
    (IF CHAR-FITS-IN-BOX
      (SETQ LOSING-KERN (- CK)
	    CK 0))
    (COND
      (FCKT (SETF (AREF FCKT CHAR-CODE) CK))
      ((NOT (ZEROP CK))
       (SETF (TV:FONT-LEFT-KERN-TABLE FONT)        ;Must be ART-FIX since kern can be negative.
	     (SETQ FCKT (MAKE-ARRAY FONT-LENGTH :ELEMENT-TYPE 'FIXNUM :INITIAL-VALUE 0)))
       (AND DOUBLE-WIDTH-P (SETF (TV:FONT-LEFT-KERN-TABLE (TV:FONT-NEXT-PLANE FONT)) FCKT))
       (SETF (AREF FCKT CHAR-CODE) CK))))
  ;; Tell the font this char exists.
  (IF (TV:FONT-CHARS-EXIST-TABLE FONT)		;if no table, char must exist
    (SETF (AREF (TV:FONT-CHARS-EXIST-TABLE FONT) CHAR-CODE) 1))
;  (ERRSET (SETF (AREF (TV:FONT-CHARS-EXIST-TABLE FONT) CHAR-CODE) 1) NIL)
  ;; In wide fonts, the raster width depends on the character, and is a multiple of 32.
  (COND
    ((TV:FONT-INDEXING-TABLE FONT) (SETQ FONT-WIDTH (* (CEILING STORAGE-WIDTH 32.) 32.))))
  ;; Now copy the data.
  (DO ((ROW 0 (1+ ROW))
       (ONE-BIT-FONT (NULL (TV:FONT-NEXT-PLANE FONT)))
       (RASTER-WIDTH (TV:FONT-RASTER-WIDTH FONT)))
      ((>= ROW FONT-HEIGHT))
    (DO ((COL 0 (IF (< PIXEL-COL LOSING-KERN)
		  COL
		  (+ COL COL-INCR)))
	 ;; Count columns in font.
	 (PIXEL-COL 0 (1+ PIXEL-COL))
	 ;; for one-bit fonts this is index in font itself of start of row.
	 ;; For multi-bit fonts it is not used.
	 (NEXT-BIT-FONT-INDEX
	  (+ (* 32. (+ (* (TV:FONT-WORDS-PER-CHAR FONT)
			  (IF (TV:FONT-INDEXING-TABLE FONT)
			      (AREF (TV:FONT-INDEXING-TABLE FONT) CHAR-CODE)
			      CHAR-CODE))
		       (FLOOR ROW (TV:FONT-RASTERS-PER-WORD FONT))))
	     (* RASTER-WIDTH (REM ROW (TV:FONT-RASTERS-PER-WORD FONT))))
	  (1+ NEXT-BIT-FONT-INDEX)))
	((>= PIXEL-COL FONT-WIDTH))
	 ;; Get pixel out of font descriptor.
	 ;; If font is "double width", two pixels of font descriptor
	 ;; are combined into one pixel for the font itself.
      (SETQ PIXEL
	    (COND
	      ((< PIXEL-COL LOSING-KERN) 0)
	      ((OR (>= COL WIDTH) (>= ROW HEIGHT)) 0)
	      (DOUBLE-WIDTH-P
	       (+ (COND
		    ((>= (1+ COL) WIDTH) 0)
		    (T (AREF CD ROW (1+ COL))))
		  (* 2 (AREF CD ROW COL))))
	      (T (AREF CD ROW COL))))
      ;; Store pixel into font.
      ;; If have one-bit pixels, use a short cut.
      (COND
	(ONE-BIT-FONT
	 ;; In wide font, notice when our horizontal advance carries us into
	 ;; the "next character" of the many characters in the font which
	 ;; actually represent vertical 32-bit-wide strips of one character.
	 (AND (ZEROP (REM PIXEL-COL RASTER-WIDTH)) (NOT (ZEROP PIXEL-COL))
	    (SETQ NEXT-BIT-FONT-INDEX
		  (+ NEXT-BIT-FONT-INDEX (* 32. (TV:FONT-WORDS-PER-CHAR FONT)) (- RASTER-WIDTH))))
	 (SETF (AREF FONT NEXT-BIT-FONT-INDEX) PIXEL))
	(T (FONT-SET-PIXEL PIXEL FONT CHAR-CODE ROW PIXEL-COL))))))) 


;Create an FD format font from an internal format font


(DEFUN FONT-INTO-FONT-DESCRIPTOR (FONT &OPTIONAL (DBL-WIDTH-P NIL))
  (LET ((FONT-LENGTH (TV:FONT-FILL-POINTER FONT))
	(LINE-SPACING (TV:FONT-CHAR-HEIGHT FONT))
	(RASTER-HEIGHT (TV:FONT-RASTER-HEIGHT FONT))
	(BASELINE (TV:FONT-BASELINE FONT))
	(BLINKER-HEIGHT (TV:FONT-BLINKER-HEIGHT FONT))
	(BLINKER-WIDTH (TV:FONT-BLINKER-WIDTH FONT))
	(SPACE-WIDTH (TV:FONT-CHAR-WIDTH FONT))
	(FCWT (TV:FONT-CHAR-WIDTH-TABLE FONT))
	(FCW (TV:FONT-CHAR-WIDTH FONT))
	(CK (TV:FONT-LEFT-KERN-TABLE FONT))
	FONT-DESCRIPTOR FONT-CHARS-EXIST-TABLE TEMP
	RASTER-WIDTH CHARACTER-WIDTH LEFT-KERN PIXEL)
    "Create an return a font-descriptor containing the data from FONT."
    
    (ERRSET (SETQ FONT-CHARS-EXIST-TABLE (TV:FONT-CHARS-EXIST-TABLE FONT)) nil)
    ;; Correct for old fonts that may not have valid fill pointers.
    (WHEN (< (TV:FONT-FILL-POINTER FONT) *MIN-FONT-LENGTH*)
      (SETQ FONT-LENGTH *MIN-FONT-LENGTH*))
    (SETQ FONT-DESCRIPTOR (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH FONT-LENGTH)
						:FD-FILL-POINTER FONT-LENGTH))
    (SETF (FD-NAME FONT-DESCRIPTOR) (TV:FONT-NAME FONT))
    (SETF (FD-LINE-SPACING FONT-DESCRIPTOR) LINE-SPACING)
    (SETF (FD-BASELINE FONT-DESCRIPTOR) BASELINE)
    (SETF (FD-BLINKER-HEIGHT FONT-DESCRIPTOR) BLINKER-HEIGHT)
    (SETF (FD-BLINKER-WIDTH FONT-DESCRIPTOR) BLINKER-WIDTH)
    (SETF (FD-SPACE-WIDTH FONT-DESCRIPTOR) SPACE-WIDTH)
    (SETF (FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR) DBL-WIDTH-P)
    (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
	((>= CHAR-CODE FONT-LENGTH))
      (COND ((AND FONT-CHARS-EXIST-TABLE
		  (ZEROP (AREF FONT-CHARS-EXIST-TABLE CHAR-CODE))))
	    (T
	     ;; Characters 128.-160. are reserved for LISPM control characters.  They should not
	     ;; have a character stored in that position, and the character width must be NIL for
	     ;; the window system to handle these chars correctly.
	     (SETQ CHARACTER-WIDTH (COND (FCWT (OR (AREF FCWT CHAR-CODE) FCW)) ;don't put nil in CD.
					 (T FCW)))
	     (SETQ RASTER-WIDTH (TV:FONT-CHAR-MIN-RASTER-WIDTH FONT CHAR-CODE)) 
	     ;; If we don't know for sure which chars exist, 
	     ;; discard chars containing no information.
	     (COND ((AND (NULL FONT-CHARS-EXIST-TABLE)
			 (ZEROP RASTER-WIDTH)
			 (/= CHAR-CODE (CHAR-INT #\SPACE))
			 (IF CHARACTER-WIDTH
			     (= CHARACTER-WIDTH (TV:FONT-CHAR-WIDTH FONT))
			     T)))
		   (T (SETQ LEFT-KERN (COND (CK (AREF CK CHAR-CODE))
					    (T 0)))
		      (SETQ TEMP
			    (MAKE-CHAR-DESCRIPTOR
			      :MAKE-ARRAY (:TYPE 'ART-4B :LENGTH (LIST RASTER-HEIGHT RASTER-WIDTH))
			      :CD-CHAR-WIDTH CHARACTER-WIDTH
			      :CD-CHAR-LEFT-KERN LEFT-KERN))
		      (SETF (AREF FONT-DESCRIPTOR CHAR-CODE) TEMP)
		      (COND (DBL-WIDTH-P
			     (DO ((ROW 0 (1+ ROW)))
				 ((>= ROW RASTER-HEIGHT))
			       (DO ((COLI 0 (1+ COLI))
				    (COL 0 (+ 2 COL)))
				   ((>= COL RASTER-WIDTH))
				 (SETQ PIXEL (FONT-GET-PIXEL FONT CHAR-CODE ROW COLI))
				 (SETF (AREF TEMP ROW COL) PIXEL)
				 (SETF (AREF TEMP ROW (1+ COL)) (LSH PIXEL -1)))))
			    (T
			     (DO ((ROW 0 (1+ ROW)))
				 ((>= ROW RASTER-HEIGHT))
			       (DO ((COL 0 (1+ COL)))
				   ((>= COL RASTER-WIDTH))
				 (SETF (AREF TEMP ROW COL) (FONT-GET-PIXEL FONT CHAR-CODE ROW COL)) )))))))))
    FONT-DESCRIPTOR)) 

;;; Some useful font munging functions

(DEFUN THICKEN-FONT-DESCRIPTOR (FD &OPTIONAL NEW-NAME &AUX LEN NFD)
  "Given font-descriptor FD, make a new one whose characters are \"thicker\".
NEW-NAME specifies the name to give the new font-descriptor;
by default, a \"B\" is appended to the old name.
The new font descriptor is returned, and the new name is not actually defined."
  (OR NEW-NAME
      (SETQ NEW-NAME (INTERN (STRING-APPEND (FD-NAME FD) "B") 'FONTS)))
  (SETQ LEN (ARRAY-ACTIVE-LENGTH FD)
	NFD (MAKE-FONT-DESCRIPTOR
	      :MAKE-ARRAY (:LENGTH LEN) ;art-q
	      :FD-FILL-POINTER (FD-FILL-POINTER FD)
	      :FD-NAME NEW-NAME
	      :FD-LINE-SPACING (FD-LINE-SPACING FD)
	      :FD-BASELINE (FD-BASELINE FD)
	      :FD-BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)
	      :FD-BLINKER-WIDTH (FD-BLINKER-WIDTH FD)
	      :FD-SPACE-WIDTH (FD-SPACE-WIDTH FD)))
  (DO ((I 0 (1+ I))
       (CD)
       (NCD))
      ((>= I LEN))
    (AND (SETQ CD (AREF FD I))
       (LET ((WIDTH (ARRAY-DIMENSION CD 1))
	     (HEIGHT (ARRAY-DIMENSION CD 0)))
	 (SETQ NCD
	       (MAKE-CHAR-DESCRIPTOR
		 :MAKE-ARRAY (:TYPE 'ART-4B :LENGTH (LIST HEIGHT (1+ WIDTH)))
		 :CD-CHAR-WIDTH (1+ (CD-CHAR-WIDTH CD))
		 :CD-CHAR-LEFT-KERN (CD-CHAR-LEFT-KERN CD)))
	 (COPY-ARRAY-CONTENTS CD NCD)
	 (DOTIMES (J HEIGHT)
	   (DOTIMES (I WIDTH)
	     (SETF (AREF NCD J (1+ I)) (LOGIOR (AREF CD J I) (AREF NCD J (1+ I))))))
	 (SETF (AREF NFD I) NCD))))
  NFD) 


(DEFUN THICKEN-FONT (FONT-SYMBOL &AUX FD NFD NFS)
  "Create a thicker (bolder) version of font named FONT-SYMBOL.
The new font is given a name which is \"B\" appended to the old name."
  (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL)
	NFD (THICKEN-FONT-DESCRIPTOR FD)
	NFS (FD-NAME NFD))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR NFS NFD)
  NFS) 


(DEFUN UNTHICKEN-FONT-DESCRIPTOR (FD NEW-NAME &AUX LEN NFD)
  "Given font-descriptor FD, make a new one whose characters are less thick.
NEW-NAME specifies the name to give the new font-descriptor.
The new font descriptor is returned, and the new name is not actually defined."
  (SETQ LEN (ARRAY-ACTIVE-LENGTH FD)
	NFD (MAKE-FONT-DESCRIPTOR
	      :MAKE-ARRAY (:LENGTH LEN) ;art-q
	      :FD-FILL-POINTER (FD-FILL-POINTER FD)
	      :FD-NAME NEW-NAME
	      :FD-LINE-SPACING (FD-LINE-SPACING FD)
	      :FD-BASELINE (FD-BASELINE FD)
	      :FD-BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)
	      :FD-BLINKER-WIDTH (FD-BLINKER-WIDTH FD)
	      :FD-SPACE-WIDTH (FD-SPACE-WIDTH FD)))
  (DO ((I 0 (1+ I))
       (CD)
       (NCD))
      ((>= I LEN))
    (AND (SETQ CD (AREF FD I))
       (LET ((WIDTH (ARRAY-DIMENSION CD 1))
	     (HEIGHT (ARRAY-DIMENSION CD 0)))
	 (SETQ NCD
	       (MAKE-CHAR-DESCRIPTOR
		 :MAKE-ARRAY (:TYPE 'ART-4B :LENGTH (LIST HEIGHT WIDTH)) 
		 :CD-CHAR-WIDTH (CD-CHAR-WIDTH CD)
		 :CD-CHAR-LEFT-KERN (CD-CHAR-LEFT-KERN CD)))
	 (COPY-ARRAY-CONTENTS CD NCD);110  100
	 (DOTIMES (J HEIGHT)
	   (LOOP FOR I FROM (1- WIDTH) ABOVE 0
		 AS RIGHT = 0 THEN THIS
		 AS THIS FIRST (AREF CD J I) THEN LEFT
		 AS LEFT = (AREF CD J (1- I))
		 WHEN (AND (= LEFT 1) (= THIS 1) (= RIGHT 0))
		 DO (SETF (AREF NCD J I) 0)))
	 (SETF (AREF NFD I) NCD))))
  NFD) 


(DEFUN UNTHICKEN-FONT (FONT-SYMBOL NFS &AUX FD NFD)
  "Create a less thick (bold) version of font named FONT-SYMBOL.
NFS is the name for the new font."
  (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL)
	NFD (UNTHICKEN-FONT-DESCRIPTOR FD NFS))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR NFS NFD)
  NFS) 


(DEFUN ROTATE-FONT-DESCRIPTOR (FD &AUX LENGTH NFD)
  (SETQ LENGTH (ARRAY-ACTIVE-LENGTH FD)
	NFD (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH LENGTH)
				  :FD-FILL-POINTER (FD-FILL-POINTER FD)
				  :FD-NAME (INTERN (STRING-APPEND (FD-NAME FD) "R") 'FONTS)
				  :FD-BASELINE (FD-SPACE-WIDTH FD)
				  :FD-LINE-SPACING (FD-SPACE-WIDTH FD)
				  :FD-BLINKER-HEIGHT (FD-BLINKER-WIDTH FD)
				  :FD-BLINKER-WIDTH (FD-BLINKER-HEIGHT FD)
				  :FD-SPACE-WIDTH (FD-LINE-SPACING FD)
				  :FD-ROTATION 90.))
  (LOOP FOR CH FROM 0 BELOW LENGTH
	AS CD = (AREF FD CH)
	WHEN CD
	DO (SETF (AREF NFD CH) (ROTATE-CHAR-DESCRIPTOR CD)))
  NFD) 


(DEFUN ROTATE-CHAR-DESCRIPTOR (CD)
  (LET* ((HEI (ARRAY-DIMENSION CD 0))
	 (WID (ARRAY-DIMENSION CD 1))
	 (NCD (MAKE-CHAR-DESCRIPTOR
		:MAKE-ARRAY (:LENGTH (LIST WID HEI) :TYPE (ARRAY-TYPE CD))
		:CD-CHAR-LEFT-KERN 0
		:CD-CHAR-WIDTH HEI)))
    (LOOP FOR X FROM 0 BELOW WID
	  DO (LOOP FOR Y FROM 0 BELOW HEI
		   DO (SETF (AREF NCD (- WID X 1) Y) (AREF CD Y X))))
    NCD)) 


(DEFUN ROTATE-FONT (FONT-SYMBOL &AUX FD NFD NFS)
  "Create a rotated version of font named FONT-SYMBOL.
The new font is given a name which is \"R\" appended to the old name."
  (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL)
	NFD (ROTATE-FONT-DESCRIPTOR FD)
	NFS (FD-NAME NFD))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR NFS NFD)
  NFS)


;;; This returns in array units, which are from the upper-left corner

(DEFUN CD-RASTER-RANGE (CD)
  (DECLARE (VALUES MINX MINY MAXX MAXY))
  (LOOP WITH HEIGHT = (ARRAY-DIMENSION CD 0)
	AND WIDTH = (ARRAY-DIMENSION CD 1)
	WITH MINX = WIDTH
	AND MINY = HEIGHT AND MAXX = 0 AND MAXY = 0
	FOR X FROM 0 BELOW WIDTH
	DO (LOOP FOR Y FROM 0 BELOW HEIGHT
		 WHEN (NOT (ZEROP (AREF CD Y X)))
		 DO (SETQ MINX (MIN MINX X)
			  MAXX (MAX MAXX (1+ X))
			  MINY (MIN MINY Y)
			  MAXY (MAX MAXY (1+ Y))))
	FINALLY (RETURN (VALUES (MIN MINX MAXX)
				(MIN MINY MAXY)
				MAXX
				MAXY)))) 

;Return how many rasters are empty (all 0)
;at the bottom of the character and at the top.

(DEFUN CD-UNUSED-RASTER-HEIGHT (CD)
  (DECLARE (VALUES BOTTOM TOP))
  (LET* ((DIMS (ARRAY-DIMENSIONS CD))
	 BOTTOM
	 TOP
	 (HEIGHT (CAR DIMS))
	 (WIDTH (CADR DIMS)))
    (DOTIMES (I HEIGHT)
      (AND (DOTIMES (J WIDTH)
	     (AND (NOT (ZEROP (AREF CD I J))) (RETURN T)))
	 (RETURN (SETQ TOP I))))
    (DOTIMES (I HEIGHT)
      (AND (DOTIMES (J WIDTH)
	     (AND (NOT (ZEROP (AREF CD (- HEIGHT I 1) J))) (RETURN T)))
	 (RETURN (SETQ BOTTOM I))))
    (COND
      (TOP (VALUES BOTTOM TOP))
      ;; Handle case where all data is empty.
      (T (VALUES HEIGHT 0))))) 

;; Given filename and fontname from filename &optional fontname, canonicalize them

(DEFUN GET-INPUT-FILENAME-AND-FONTNAME (FILENAME FONTNAME FILE-TYPE)
  (DECLARE (VALUES FILENAME FONTNAME))
  (SETQ FILENAME (FS:MERGE-PATHNAME-DEFAULTS FILENAME (PATHNAME-DEFAULTS) FILE-TYPE))
  (OR FONTNAME (SETQ FONTNAME (FUNCALL FILENAME :NAME)))
  (AND (STRINGP FONTNAME) (SETQ FONTNAME (INTERN (STRING-UPCASE FONTNAME) "FONTS")))
  (VALUES FILENAME FONTNAME)) 

