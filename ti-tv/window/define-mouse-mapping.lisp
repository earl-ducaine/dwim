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


#|

This file defines code which allows  one to easily and quickly  change some
mouse characters to contain other glyphs.  The idea came from having people
define new mouse fonts which  replaced the oft-used mouse  characters (such
as the north-east  arrow) with  other glyphs.   A problem  arose when other
people complained that they didn't like the new mouse glyphs.  They  either
wanted their own or  wanted the original  glyphs.  The only  general way to
remove the modified mouse  characters was to  load the original  mouse font
file from the remote file  server.  This took enough  time to make this  an
inconvenience.

In an attempt to  solve this problem,  a mouse character  mapping mechanism
was defined to  make it  possible for  the user  to define  a set of glyphs
which are to replace existing characters  in the mouse font.  In  addition,
one can quickly restore the original glyphs of the mouse characters with  a
simple function call.

The  function  DEFINE-MOUSE-CHAR-MAPPING   defines  a   mapping  of   mouse
characters to  glyphs  in  either  the  mouse  font  or  a  specified font.
Examples of the use of  this function are present  at the end of  this file
where a  :MIGHTY-MOUSE  and  :RAT-MOUSE  mapping  are  defined.   The first
argument to  the  DEFINE-MOUSE-CHAR-MAPPING  function  is  a user-specified
keyword which is used later to  refer to the mapping.  The  second argument
is the list of the mappings.  A mapping is of the form:

	(mouse-character new-glyph-character optional-font)

`Mouse-character' is either a character object or a fixnum which  indicates
the mouse character that is being  mapped.  Constants have been defined  to
allow the  user  to  specify  a  name  instead  of  a  number.  For example
MOUSE-GLYPH-NORTH-WEST-ARROW `New-glyph-character'  is  the  same thing for
the glyph for to  replace the mouse  character.  The `optional-font'  is an
optional element that indicates the font for `new-glyph-character'.  It  is
more preferable to use the constant  name for characters in the  mouse font
than to use its corresponding  character object since the  character object
has no relation to the mouse character.  For example, the  north-west-arrow
glyph in the mouse font is the #\epsilon character object.

After defining a mapping, or deciding to use an existing mapping, one needs
to have the mouse characters  remapped.  The function REMAP-MOUSE  actually
remaps the mouse characters according to specified mappings.  The arguments
are keywords which define  a mouse mapping.   The mouse mapping  defined by
each of these keywords is  performed in the left-to-right  order specified.
One may call REMAP-MOUSE multiple times with additional mappings; the state
of the mouse  mapping is  saved for  all of  the calls.   This has the same
effect as calling REMAP-MOUSE once with all of the mappings.  Normally  one
only gives one argument to REMAP-MOUSE to define a single mapping.

To restore the mapping which existed  before the first call to  REMAP-MOUSE
simply invoke REMAP-MOUSE with no arguments.  |#


;;; Make sure the extended mouse font is loaded.
;;;(load "sys:fonts;mouse")


#| The reason  for the  MOUSE-MAPPING-STACK is  so that  we can restore the
mouse to its original  value.  This variable  remembers the order  in which
the user remapped  the mouse.   Restoring the  order amounts  to doing  the
inverse of what the user did to map it earlier.  When we remapped the mouse
originally, we saved  information about  each character  that was  changed.
Restoring amounts to  copying this  saved information  back into  the mouse
font.  

The value of  this variable  is a  list of  character data.  Each character
datum is a list of the form:

	(character width fixnum...)

where `character' is the fixnum which  is the position of the  character in
the mouse font.  The `width' is  the width of the original  character.  The
`fixnum...'  is a sequence of numbers which are the raster bits which, when
drawn, comprise the character.  |#
(defvar mouse-mapping-stack nil)

#| The following constants name all of the glyphs in the mouse font so that
hard-coded numbers are not  necessary.  The numbers  are in hex  so that we
can look at a character chart and locate the glyph easily.  |#

(defconstant mouse-glyph-thin-up-arrow		          0)
(defconstant mouse-glyph-thin-right-arrow	          1)
(defconstant mouse-glyph-thin-down-arrow	          2)
(defconstant mouse-glyph-thin-left-arrow	          3)
(defconstant mouse-glyph-thin-up-down-arrow	          4)
(defconstant mouse-glyph-thin-left-right-arrow	          5)
(defconstant mouse-glyph-north-west-arrow	          6)
(defconstant mouse-glyph-thin-cross		          7)
(defconstant mouse-glyph-thick-up-arrow		          8)
(defconstant mouse-glyph-thick-right-arrow	          9)
(defconstant mouse-glyph-thick-down-arrow	       #x0A)
(defconstant mouse-glyph-thick-left-arrow	       #x0B)
(defconstant mouse-glyph-thick-up-down-arrow	       #x0C)
(defconstant mouse-glyph-thick-left-right-arrow	       #x0E)
(defconstant mouse-glyph-paragraph		       #x10)
(defconstant mouse-glyph-upper-left-corner	       #x11)
(defconstant mouse-glyph-lower-right-corner	       #x12)
(defconstant mouse-glyph-hourglass		       #x13)
(defconstant mouse-glyph-circle-plus		       #x14)
(defconstant mouse-glyph-paint-brush		       #x15)
(defconstant mouse-glyph-scissor		       #x16)
(defconstant mouse-glyph-trident		       #x17)
(defconstant mouse-glyph-north-east-arrow	       #x19)
(defconstant mouse-glyph-circle-x		       #x1A)
(defconstant mouse-glyph-large-right-triangle-pointer  #x1C)
(defconstant mouse-glyph-medium-right-triangle-pointer #x1D)
(defconstant mouse-glyph-small-right-triangle-pointer  #x1E)
(defconstant mouse-glyph-block-up-arrow		       #x1F)
(defconstant mouse-glyph-small-diamond		       #x20)
(defconstant mouse-glyph-block-down-arrow	       #x21)
(defconstant mouse-glyph-hollow-box-pointer	       #x22)
(defconstant mouse-glyph-solid-box-pointer	       #x23)
(defconstant mouse-glyph-hollow-circle-pointer	       #x24)
(defconstant mouse-glyph-solid-circle-pointer	       #x25)
(defconstant mouse-glyph-thick-hollow-cross	       #x26)
(defconstant mouse-glyph-block-letter-t		       #x27)
(defconstant mouse-glyph-hand-pointing-left	       #x28)
(defconstant mouse-glyph-double-up-arrow	       #x29)
(defconstant mouse-glyph-hollow-arc-pointer	       #x2A)
(defconstant mouse-glyph-solid-arc-pointer	       #x2B)
(defconstant mouse-glyph-spline-pointer		       #x2C)
(defconstant mouse-glyph-medium-diamond		       #x2D)
(defconstant mouse-glyph-hollow-triangle-pointer       #x2E)
(defconstant mouse-glyph-solid-triangle-pointer	       #x2F)
(defconstant mouse-glyph-curtain		       #x30)
(defconstant mouse-glyph-scale			       #x31)

;;; The numbers indicate the hour positions on a clock.
(defconstant mouse-glyph-6-3-arc-pointer	       #x32)
(defconstant mouse-glyph-9-6-arc-pointer	       #x33)
(defconstant mouse-glyph-3-12-arc-pointer	       #x34)
(defconstant mouse-glyph-12-9-arc-pointer	       #x35)

(defconstant mouse-glyph-ruler			       #x36)
(defconstant mouse-glyph-polyline		       #x37)
(defconstant mouse-glyph-double-up-arrow-lettered-d    #x38)
(defconstant mouse-glyph-thick-up-arrow-lettered-d     #x39)
(defconstant mouse-glyph-thick-line-pointer	       #x3A)
(defconstant mouse-glyph-question-mark		       #x3B)
(defconstant mouse-glyph-thin-hollow-cross	       #x3C)
(defconstant mouse-glyph-eye-glasses		       #x3D)
(defconstant mouse-glyph-thin-hollow-plus	       #x3E)
(defconstant mouse-glyph-rectangle-dots		       #x3F)

(defconstant mouse-glyph-west-rat-on-bottom	       #x41)
(defconstant mouse-glyph-west-rat-on-top	       #x42)
(defconstant mouse-glyph-north-rat-on-left	       #x43)
(defconstant mouse-glyph-north-rat-on-right	       #x44)
(defconstant mouse-glyph-east-rat-on-bottom	       #x45)
(defconstant mouse-glyph-east-rat-on-top	       #x46)
(defconstant mouse-glyph-south-rat-on-left	       #x47)
(defconstant mouse-glyph-south-rat-on-right	       #x48)
(defconstant mouse-glyph-west-curly-rat-on-bottom      #x49)
(defconstant mouse-glyph-west-curly-rat-on-top	       #x4A)
(defconstant mouse-glyph-north-curly-rat-on-left       #x4B)
(defconstant mouse-glyph-north-curly-rat-on-right      #x4C)
(defconstant mouse-glyph-east-curly-rat-on-bottom      #x4D)
(defconstant mouse-glyph-east-curly-rat-on-top	       #x4E)
(defconstant mouse-glyph-south-curly-rat-on-left       #x4F)
(defconstant mouse-glyph-south-curly-rat-on-right      #x50)

(defconstant mouse-glyph-west-mouse-on-bottom	       #x51)
(defconstant mouse-glyph-west-mouse-on-top	       #x52)
(defconstant mouse-glyph-north-mouse-on-left	       #x53)
(defconstant mouse-glyph-north-mouse-on-right	       #x54)
(defconstant mouse-glyph-east-mouse-on-bottom	       #x55)
(defconstant mouse-glyph-east-mouse-on-top	       #x56)
(defconstant mouse-glyph-south-mouse-on-left	       #x57)
(defconstant mouse-glyph-south-mouse-on-right	       #x58)

(defconstant mouse-glyph-small-dot		       #x60)
(defconstant mouse-glyph-thick-cross		       #x61)
(defconstant mouse-glyph-small-solid-circle	       #x62)
(defconstant mouse-glyph-medium-solid-circle	       #x63)
(defconstant mouse-glyph-hollow-circle		       #x64)
(defconstant mouse-glyph-hollow-circle-minus	       #x65)
(defconstant mouse-glyph-hollow-circle-plus	       #x66)
(defconstant mouse-glyph-short-thin-down-border-arrow  #x6A)
(defconstant mouse-glyph-short-thin-down-arrow	       #x6B)
(defconstant mouse-glyph-short-thin-up-border-arrow    #x6C)
(defconstant mouse-glyph-short-thin-up-arrow	       #x6D)
(defconstant mouse-glyph-small-up-triangle	       #x6E)
(defconstant mouse-glyph-small-down-triangle	       #x75)



#| Define  a  mapping  of  mouse  characters  so  that  one  can change the
appearance of the mouse.  Error checking is performed so that one does  not
get into trouble when using a font which is not compatible with the  mouse.
The mapping is stored  as the :MOUSE-CHAR-MAPPING  property on the  keyword
specified by the caller.  The mappings are processed so that the characters
are converted into fixnums and the font is converted into a symbol.  |#
(defun define-mouse-char-mapping (map-keyword new-mapping)
  "Define a mapping of characters to mouse characters.
MAP-KEYWORD	user-specified name of the mapping.
NEW-MAPPING......	 list of mappings.  Each mapping is of the form 
			(old-mouse new-mouse new-font) where old-mouse
			specifies an existing mouse character and new-mouse
			new-font specify the replacement for old-mouse.  The
			old or new mouse characters can be specified as
			constants such as mouse-glyph-north-west-arrow. The
			new-font is optional and defaults to fonts:mouse if not
			specified."
   (putprop map-keyword 
	     (LOOP FOR mapping IN new-mapping
		   FOR raw-position = (first mapping)
		   FOR position     = (char-int raw-position)
		   FOR new-raw-position = (second mapping)
		   FOR new-position = (char-int new-raw-position)
		   ;; The font is a symbol so we don't get into trouble if the font is a different object.
		   FOR font = (if (<= (length mapping) 2)
				  'fonts:mouse
				  ;;ELSE
				  `(send tv:default-screen :parse-font-specifier ,(third mapping)))
		   UNLESS (reasonable-mouse-font-p (eval font))
		   DO (ferror nil "~a is not a compatible mouse font; it is of the wrong ~a." (eval font)
			       (diagnose-unreasonable-mouse-font (eval font)))
		   COLLECTING `(,position ,new-position ,font))
	     :mouse-char-mapping))

#| Checks to make sure that the font specified is compatible with the mouse
font.  |#
(defun reasonable-mouse-font-p (font)
  (and (<= (font-raster-width   font) (font-raster-width   fonts:mouse))
       (=  (font-raster-height  font) (font-raster-height  fonts:mouse))
       (=  (font-words-per-char font) (font-words-per-char fonts:mouse))))

#| Returns a string which indicates  which attribute of the specified  font
is not compatible with the mouse font.  Used for error reporting  purposes.
|#
(defun diagnose-unreasonable-mouse-font (font)
  (or  (and (>       (font-raster-width   font) (font-raster-width   fonts:mouse))  "width")
       (and (not (=  (font-raster-height  font) (font-raster-height  fonts:mouse))) "height")
       (and (not (=  (font-words-per-char font) (font-words-per-char fonts:mouse))) "size")))

(defun copy-mouse-chars (char-source font-source char-dest font-dest)
  "Copy the bits for char-source in font-source to char-dest in font-dest.
Returns a list containing the destination character, its width and 16-bit values
which are the bits of that character (for later recovery purposes).
It is assumed that the two fonts are compatable."
  (let ((saved-mouse-info nil)
	(width-table-source (font-char-width-table font-source))
	(width-table-dest   (font-char-width-table font-dest)))
    (setq char-source (char-int char-source)
	  char-dest   (char-int char-dest))
    (push char-dest saved-mouse-info)
    ;; Copy the width of char-source to char-dest font.  We need to do this
    ;; because the widths of the two characters may not be the same.
    (push (if width-table-dest
	      (aref width-table-dest char-dest)
	      ;;ELSE
	      (font-char-width font-dest))
	  saved-mouse-info)
    (when width-table-dest
      (setf (aref width-table-dest char-dest) (if width-table-source
						  (aref width-table-source char-source)
						  ;;ELSE
						  (font-char-width font-source))))
    ;; Loop through all of the bits of each character, copying bits.
    (LOOP WITH font-words-per-char = (font-words-per-char font-source)
	  WITH char-source-offset  = (* 32. font-words-per-char char-source)
	  WITH char-dest-offset    = (* 32. font-words-per-char char-dest)
	  FOR bit-index from 0 below (* 32. font-words-per-char)
	  FOR source-bit           = (aref font-source (+ char-source-offset bit-index))
	  FOR dest-bit             = (aref font-dest (+ char-dest-offset bit-index))
	  FOR dest-bits            FIRST dest-bit THEN (+ (* dest-bits 2) dest-bit)
	  DO (progn
	       (when (zerop (mod (1+ bit-index) 16.))
		 (push dest-bits saved-mouse-info)
		 (setq dest-bits 0))
	       (setf (aref font-dest (+ char-dest-offset bit-index)) source-bit)))
    ;; Reverse the list so that the car of the list is the mouse character
    ;; and the cadr is the width.
    (nreverse saved-mouse-info)))

(defun copy-mouse-bits (mouse-char-info)
  "Restore mouse bits from a previously saved state.
MOUSE-CHAR-INFO is an element of the MOUSE-MAPPING-STACK."
  (let* ((font-dest (eval 'fonts:mouse))
	 (char-width-table (font-char-width-table font-dest))
	 (char-dest       (car  mouse-char-info))
	 (char-width      (cadr mouse-char-info))
	 (char-bits-list  (cddr mouse-char-info)))
    (if char-width-table
	;; Set the width back to what it was.
	(setf (aref char-width-table char-dest) char-width))
    ;; Loop through each element in char-bits-list to restore the raster
    ;; bits of the mouse character.
    (LOOP WITH font-words-per-char = (font-words-per-char font-dest)
	  WITH char-dest-offset    = (* 32. font-words-per-char char-dest)
	  FOR char-bits IN char-bits-list
	  FOR bit-index FROM 0 BELOW (* 32. font-words-per-char) BY 16.
	  ;; Loop through all 16 bits in the saved raster fixnum.
	  DO (LOOP FOR bit-number       FROM 15. DOWNTO 0
		   FOR bit-index-offset FROM 0 BELOW 16.
		   FOR char-bit = (ldb (byte 1 bit-number) char-bits)
		   DO (setf (aref font-dest (+ char-dest-offset
					       bit-index
					       bit-index-offset))
			    char-bit)))))

(defun remap-mouse (&rest map-keywords)
  "Remap mouse characters to contain other glyphs.
MAP-KEYWORDS	user-defined keywords which define the replacement glyphs.
		This can also be NIL which indicates to restore the mouse
		mapping to its original state."
  (if (null map-keywords)
      (restore-mouse-map)
      ;;ELSE
      (let ((mouse-font (eval 'fonts:mouse)))
	;; Loop through the map performing the specified transformation.
	(dolist (map-keyword map-keywords)
	  (when (null (get map-keyword :mouse-char-mapping))
	    (ferror nil "Do not have a mouse mapping defined for ~A." map-keyword)
	    (restore-mouse-map))
	  (LOOP FOR (position new-position font) IN (get map-keyword :mouse-char-mapping)
		DO (push (copy-mouse-chars new-position (eval font) position mouse-font)
			 mouse-mapping-stack))))))

(defun restore-mouse-map ()
  "Restore the mouse to its original mapping."
  (LOOP FOR mouse-char-info IN mouse-mapping-stack
	DO (copy-mouse-bits mouse-char-info))
  (setq mouse-mapping-stack nil))


;;; Define the mapping for the elephant-eared fat mouse.
(define-mouse-char-mapping :mighty-mouse
			   (list (list mouse-glyph-north-west-arrow    mouse-glyph-east-mouse-on-bottom)
				 (list mouse-glyph-thick-up-arrow      mouse-glyph-north-mouse-on-right)
				 (list mouse-glyph-thick-down-arrow    mouse-glyph-south-mouse-on-right)
				 (list mouse-glyph-thick-up-down-arrow mouse-glyph-north-mouse-on-left)
				 (list mouse-glyph-north-east-arrow    mouse-glyph-west-mouse-on-bottom)))


;;; Define the mapping for the scrawny rat mouse.
(define-mouse-char-mapping :rat-mouse
			   (list (list mouse-glyph-thin-up-arrow       mouse-glyph-north-rat-on-right)
				 (list mouse-glyph-thin-right-arrow    mouse-glyph-east-rat-on-top)
				 (list mouse-glyph-thin-down-arrow     mouse-glyph-south-rat-on-right)
				 (list mouse-glyph-thin-left-arrow     mouse-glyph-west-rat-on-top)
				 (list mouse-glyph-north-west-arrow    mouse-glyph-east-curly-rat-on-bottom)
				 (list mouse-glyph-thick-up-arrow      mouse-glyph-north-rat-on-left)
				 (list mouse-glyph-thick-right-arrow   mouse-glyph-east-rat-on-bottom)
				 (list mouse-glyph-thick-down-arrow    mouse-glyph-south-rat-on-left)
				 (list mouse-glyph-thick-left-arrow    mouse-glyph-west-rat-on-bottom)
				 (list mouse-glyph-thick-up-down-arrow mouse-glyph-north-curly-rat-on-left)
				 (list mouse-glyph-north-east-arrow    mouse-glyph-west-curly-rat-on-bottom)))
