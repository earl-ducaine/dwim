;;; -*- Mode:Common-Lisp; Package:MAC-WINDOWS; Base:10; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB) -*-



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
;;; Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.

;;;   Date    Author  Patch   Comment
;----------------------------------------------------------------------------------------
;;; 10/24/88  AB  5-24      Deleted Userxxx font specifiers  and all non-installed Mac fonts
;;;                               from *mac-font-list*.
;;;                               Also added new undo-mac-font-translations function that will
;;;                               restore the original font translation table if the user screws up.
;;; 09-26-88   LG    5-13    Added ,boldBit to *mac-font-translation-table* entries for
;;; 			medfntb and higher-medfnb.  medfnb and medfntb now describe
;;; 			the same Mac font.
;;; 09-20-88   CRW 5-11  Add mappings for VT100 and CTERM. 
;;; 09-06-88   LG    5-8  	Fix "mac-cptfont unbound" entry to EH when disk-saving after a build.
;;; 06-08-88   LG    4-57	Clean up a window's font references at activation time.  Clean
;;; 			up all packages' font-referencing variables as a :before-cold
;;; 			initialization.  
;;; 05-25-88   ab    4-53 	Add mappings for MEDFNTB and BIGFONT.  
;;;                           	Support Mouse font properly
;;; 04-26-88   LG    4-43    Added translation for fonts:tr10bi.
;;; 03-17-88   KED     ?      Reworked to allow dynamic mapping of new fonts.

;;; Pen Patterns and LISPM "colors".

;;; This should be in the variables file!!!
(DEFUN mac-pattern (LISPM-color)
  "Translates a LISPM color (w:white,w:black,w:0%-gray,12%,25%,33%,50%,66%,75%,88%, and w:100%-gray)
to a Mac pattern (white,black,gray,ltGray,dkGray)."
  (WHEN (INTEGERP LISPM-color)
    (SETF LISPM-color (AREF w:b&w-table (MOD LISPM-color 9))))
  (SELECT LISPM-color
    ;; (w:white (VALUES #x0000 #x0000 #x0000 #x0000))
    ;; (w:black (VALUES #xFFFF #xFFFF #xFFFF #xFFFF))
    (w:0%-gray (VALUES #x0000 #x0000 #x0000 #x0000))
    (w:12%-gray (VALUES #x8800 #x2200 #x8800 #x2200))
    (w:25%-gray (VALUES #x8822 #x8822 #x8822 #x8822))
    (w:33%-gray (VALUES #x8A51 #xA254 #xA815 #x2A45))
    (w:50%-gray (VALUES #x55AA #x55AA #x55AA #x55AA))
    (w:66%-gray (VALUES #x75AE #x5DAB #x57EA #xD5BA))	;Changed to be exact compliment of 33%-gray KED 3/3/87
    (w:75%-gray (VALUES #x77DD #x77DD #x77DD #x77DD))	;Changed to be exact compliment of 25%-gray KED 3/3/87
    (w:88%-gray (VALUES #x77FF #xDDFF #x77FF #xDDFF))	;Changed to be exact compliment of 12%-gray KED 3/3/87
    (w:100%-gray (VALUES #xFFFF #xFFFF #xFFFF #xFFFF))
    (w:100%-black (VALUES #xFFFF #xFFFF #xFFFF #xFFFF))	;Added KED 3/3/87
    (w:100%-white (VALUES #x0000 #x0000 #x0000 #x0000))	;Added KED 3/3/87
    (:otherwise (VALUES #xAA55 #xAA55 #xAA55 #xAA55))))

;----------------------------------------------------------------------------------------

;;; LISPM fonts to Macintosh fonts

(DEFCONSTANT *lispm-font-leading* 0
  "The interline spacing used on LISPM windows. Mac fonts include this in the font size.")

;;; The Mac fonts:
(DEFCONSTANT systemFont 0)
(DEFCONSTANT chicagoFont 0)
(DEFCONSTANT applFont 1)
(DEFCONSTANT newYorkFont 2)
(DEFCONSTANT genevaFont 3)
(DEFCONSTANT monacoFont 4)
(DEFCONSTANT veniceFont 5)
(DEFCONSTANT londonFont 6)
(DEFCONSTANT athensFont 7)
(DEFCONSTANT sanFranFont 8)
(DEFCONSTANT torontoFont 9)
(DEFCONSTANT cairoFont 11)
(DEFCONSTANT losAngelesFont 12)
(DEFCONSTANT timesFont 20)
(DEFCONSTANT helveticaFont 21)
(DEFCONSTANT courierFont 22)
(DEFCONSTANT symbolFont 23)
(DEFCONSTANT taliesinFont 24)
(DEFCONSTANT cptFontFont 200)
(DEFCONSTANT mouseFont 201.)		   ;ab 5-25-88
(DEFCONSTANT bigFontFont 202.)		   ;ab 10-14-88

;;(DEFCONSTANT user300 300)
;;(DEFCONSTANT user301 301)
;;(DEFCONSTANT user302 302)
;;(DEFCONSTANT user303 303)
;;(DEFCONSTANT user304 304)
;;(DEFCONSTANT user305 305)

(DEFVAR *mac-font-list*
        '("System" "Application" "Chicago" "Courier" "Geneva"
	"Helvetica" "Monaco" "Symbol" "Times" "Venice" 
	"CptFont" "Mouse" "BigFont")) 	                  ;ab 5/25/88  ab 10/14/88  ab 10/24/88    

(PUTPROP '*mac-font-list*
         '(systemFont applFont chicagoFont courierFont genevaFont
	  helveticaFont monacoFont symbolFont timesFont veniceFont
	  mouseFont bigFontFont)	   ;ab 5/25/88  ab 10/14/88 ab 10/24/88
         :mac-font-names)

;;; The Mac font styles:
(DEFCONSTANT boldBit 1)
(DEFCONSTANT italicBit 2)
(DEFCONSTANT underlineBit 4)
(DEFCONSTANT outlineBit 8)
(DEFCONSTANT shadowBit 16)
(DEFCONSTANT condensedBit 32)
(DEFCONSTANT extendedBit 64)

(DEFVAR *mac-style-list*
        '("Bold " "Italic " "Underline " "Outline "
          "Shadow " "Condensed " "Extended"))
(PUTPROP '*mac-style-list*
        (LIST boldBit italicBit underlineBit outlineBit
	    shadowBit condensedBit extendedBit)
        :mac-font-style-bits)

;;; Initial font translations
;;;
;;; ab 10/24/88.
;;; WARNING:  As a microExplorer developer, if you change this in a WINDOW-MX patch you must now also make sure the
;;; new variable *original-mac-font-translations* also records the change.  This can be done by:
;;; (SETF *original-mac-font-translations* (COPY-ALIST *mac-font-translation-table*))
(DEFVAR *mac-font-translation-table*
        `((fonts:bigfnt bigFontFont 20)	   ;ab 10/14/88
	  (fonts:bottom cptFontFont 12)    ;crw 9/20/88
	  (fonts:top cptFontFont 12)       ;crw 9/20/88 
	  (fonts:cptfont cptFontFont 12)
          (fonts:cptfontb cptFontFont 12 ,boldBit)
          (fonts:cptfonti cptFontFont 12 ,italicBit)
          (fonts:cptfontbi cptFontFont 12 ,boldBit ,italicBit)
          (fonts:wider-font genevaFont 14 ,extendedBit)
          (fonts:courier courierFont 12)
          (fonts:medfnt monacoFont 12)
	  (fonts:medfntb monacoFont 12 ,boldBit)   ;ab 5/25/88
          (fonts:medfnb monacoFont 12 ,boldBit)
	  (fonts:higher-medfnb cptFontFont 12 ,boldBit)     ;crw 9/20/88 
          (fonts:wider-medfnt monacoFont 12 ,extendedBit)
          (fonts:tvfont monacoFont 9)
          (fonts:tiny monacoFont 6)
          (fonts:hl6 helveticaFont 9)
          (fonts:hl7 helveticaFont 9)
          (fonts:hl10 helveticaFont 10)
          (fonts:hl10b helveticaFont 10 ,boldBit)
          (fonts:hl12 helveticaFont 12)
          (fonts:hl12b helveticaFont 12 ,boldBit)
          (fonts:hl12i helveticaFont 12 ,italicBit)
          (fonts:hl12bi helveticaFont 12 ,boldBit ,italicBit)
          (fonts:mets timesFont 18)
          (fonts:metsb timesFont 18 ,boldBit)
          (fonts:metsi timesFont 18 ,italicBit)
          (fonts:metsbi timesFont 18 ,boldBit ,italicBit)
	  (fonts:tr8 timesFont 9)
          (fonts:tr8b timesFont 9 ,boldBit)
          (fonts:tr8i timesFont 9 ,italicBit)
	  (fonts:higher-tr8 cptFontFont 12)    ;crw 9/20/88 
          (fonts:tr10 timesFont 10)
          (fonts:tr10b timesFont 10 ,boldBit)
          (fonts:tr10i timesFont 10 ,italicBit)
          (fonts:tr10bi timesFont 10 ,boldBit ,italicBit)
	  (fonts:tr12i timesFont 12 ,boldBit ,italicBit)
          (fonts:tr12 timesFont 12)
          (fonts:tr12b timesFont 12 ,boldBit)         
          (fonts:tr12i timesFont 12 ,italicBit)
          (fonts:tr12bi timesFont 12 ,boldBit ,italicBit)
          (fonts:tr18 timesFont 18)
          (fonts:tr18b timesFont 18 ,boldBit)
	  (fonts:vt-graphics cptFontFont 12)               ;crw 9/20/88	 
	  (fonts:vt-graphics-bottom cptFontFont 12)        ;crw 9/20/88
	  (fonts:vt-graphics-top cptFontFont 12)           ;crw 9/20/88 
	  (fonts:vt-graphics-wider-font cptFontFont 12)    ;crw 9/20/88
          (fonts:mouse mouseFont 16)
          (fonts:search cptFontFont 12) ;Search = cptFont for ZMACs minibuffer!
          (fonts:5x5 monacoFont 6)))

(DEFPARAMETER *original-mac-font-translations* nil)    ;ab 10/24/88. 
(DEFPARAMETER *saved-mac-font-translations* nil)       ;ab 10/24/88. 

(ADD-INITIALIZATION "Setup saved translations"	       ;ab 10/24/88. 
		    '(SETF *original-mac-font-translations* (COPY-ALIST *mac-font-translation-table*))
		    '(:once))

;;ab 10/24/88.  New.
(DEFUN undo-mac-font-translations (&optional (remap-fonts t) (fixup-data-structures nil)
				   &aux original need-remap)
  "Call this to restore the original microExplorer Explorer-to-Mac font translations.  Repeated
calls will undo the undo.
  When REMAP-FONTS it true (the default), verifies current font object information with the Macintosh.
  When FIXUP-DATA-STRUCTURES is true, also tries to fix up font references in existing windows and 
symbols.  This process takes a while, so the default is NIL.
  Returns two values:  the translation table (after the undo) and a flag indicating whether it
is the original version or a user-modified version (one of the keywords :original or :user-modified)."
  (DECLARE (VALUES translation-table original-flag)) 
  (WITHOUT-INTERRUPTS
    (COND ((NULL *saved-mac-font-translations*)
	   ;; Undo case (restore the original system mappings)
	   (COND ((EQUAL *mac-font-translation-table* *original-mac-font-translations*)
		  ;; Mappings haven't changed.  No work needed.
		  (SETF original t need-remap nil))
		 (t
		  ;; Mappings have changed from original set. 
		  (SETF *saved-mac-font-translations* *mac-font-translation-table*
			*mac-font-translation-table* (COPY-ALIST *original-mac-font-translations*)
			original t
			need-remap t))))
	  (t
	   ;; Undo-the-undo case (restore user-modified mappings).
	   (SETF *mac-font-translation-table* *saved-mac-font-translations*
		 *saved-mac-font-translations* nil
		 original nil
		 need-remap t))))
  (WHEN need-remap				       ;don't need remap if current set = original set.
    (WHEN remap-fonts
      (remap-all-fonts))
    (WHEN fixup-data-structures
      (fixup-font-refs-in-screens-and-windows)
      (clean-up-existing-font-refs)))
  (VALUES *mac-font-translation-table* (IF original :original :user-modified))
  )

(DEFUN map-lispm-font (lispm-font-name mac-font size &rest mac-font-style)
  "This stashes the mac-font and style on the property list of the LISPM font."
  (LET ((style (APPLY 'BOOLE boole-ior 0 mac-font-style)))
    (UNLESS (BOUNDP lispm-font-name)
      (SET lispm-font-name (tv:make-font))
      (SETF (tv:font-name (SYMBOL-VALUE lispm-font-name)) lispm-font-name))
    (PUTPROP lispm-font-name mac-font :mac-font)
    (PUTPROP lispm-font-name style :mac-font-style)
    (PUTPROP lispm-font-name size :mac-font-size)
    (UNLESS (OR (NULL w:default-screen))
       (SET lispm-font-name
            (SEND w:default-screen :parse-font-descriptor lispm-font-name)))))

(DEFUN map-LISPM-fonts ()
  "Maps LISPM fonts to Mac fonts by putting Mac fonts on property lists of the LISPM fonts."
  (DOLIST (fsp *mac-font-translation-table*) 
    (APPLY #'map-lispm-font 
           (CONS (CAR fsp) (CONS (SYMBOL-VALUE (CADR fsp)) (CDDR fsp))))))

(DEFUN demap-mac-font (font-symbol)
  "Marks a font as not mac-translated so that the next parse-font-descriptor will
reaquire font parameters from the Mac."
  (WHEN (AND (SYMBOLP font-symbol)
	     (BOUNDP font-symbol)
	     (TYPEP (SYMBOL-VALUE font-symbol) 'tv:font))
    (DOLIST
      (font-name
        (LIST font-symbol			       ; Do both Explorer name
	      (tv:font-name (SYMBOL-VALUE font-symbol))))      ;and MAC-
      (WHEN (GET font-name :mac-translated)
        (SETF (GET font-name :mac-translated) nil))
      (WHEN (GET font-name :mac-fd)
        (SETF (GET font-name :mac-fd) nil)))))

(DEFUN demap-mac-fonts ()
  "Marks all fonts as not Mac-translated."
  (DO-LOCAL-SYMBOLS (symbol 'FONTS)
    (demap-mac-font symbol)))

;;ab new 5-25-88.
(DEFUN remap-all-fonts ()
  (mac:demap-mac-fonts)
  (mac:map-lispm-fonts)
  (mac:fixup-fonts t)
  )


;;ab 5/25/88.  
(DEFUN get-ultimate-font-descriptor (font)
  (SETF font (TYPECASE font
	       (symbol (SYMBOL-VALUE font))
	       (STRING (SYMBOL-VALUE (INTERN font 'fonts)))
	       (t font)))
  (LOOP FOR last-font-descriptor FIRST font THEN next-font-descriptor
	FOR next-font-descriptor = (SYMBOL-VALUE
				     (tv:font-name last-font-descriptor))
	WHEN (EQ last-font-descriptor next-font-descriptor)
	RETURN last-font-descriptor))

;;;  06-08-88  LG		Patch 4-57.
(DEFUN fixup-font-map (font-map)
  (WHEN font-map
    (LOOP for i from 0 to (1- (LENGTH font-map))
	  do
	  (SETF (AREF font-map i) (get-ultimate-font-descriptor (AREF font-map i))))))

;;;  06-08-88  LG		Patch 4-57.
(DEFUN fixup-font-refs-in-window (window &aux item1)
  ;;  Clean up the window's label spec if it contains a font instance rather than  a font
    ;;  name...
  (LET ((tv:item-list :no-value) item-list)
    (DECLARE (SPECIAL tv:item-list))
    (DOLIST (element (SEND window :send-if-handles :label))
      (WHEN (TYPEP element 'font)
	(SETF element (get-ultimate-font-descriptor element))))
       ;;  Clean up the window's item list if it is a menu...
    (SETF item-list (sys:symeval-maybe-in-instance window 'tv:item-list))
    (WHEN (CONSP item-list)
      (DOLIST (item item-list)
	(WHEN (AND (CONSP item) (SYMBOLP (REST item))
		   (SETF item1 (GETF (REST item) :font))
		   (ARRAYP item1) (TYPEP item1 'font))
	  (SETF (GETF (REST item) :font)
		(get-ultimate-font-descriptor item)))))
       ;;  Clean up this window's inferiors, if any...
    (DOLIST (inferior (tv:sheet-inferiors window))
      (fixup-font-refs-in-window inferior))))
  
  
;;;  06-08-88  LG		Patch 4-57.
(DEFUN fixup-font-refs-in-screens-and-windows ()
  (DOLIST (screen tv:all-the-screens)
    (WHEN (mac-screen-p screen)
      (fixup-font-map (tv:sheet-font-map screen))
      (DOLIST (window (tv:sheet-inferiors screen))
	(fixup-font-map (tv:sheet-font-map window))
	(fixup-font-refs-in-window window))
      (SEND screen :change-of-default-font 
	    (tv:screen-default-font screen)
	    (get-ultimate-font-descriptor (tv:screen-default-font screen)))
      (SEND screen :set-current-font (tv:screen-default-font screen)))))


;;;   06-06-88  LG		Clean up the activated window's (and its inferiors') font
;;;   			references if its timestamp says it may need it.
tv:
(DEFMETHOD (essential-activate :before :activate) ()
  (UNLESS (OR (NULL superior)
	      (EQ time-stamp (SEND superior :time-stamp)))
    (mac:fixup-font-refs-in-window self)))


;;;  05/25/88  ab		Make this general and user-callable.
;;;  06-06-88  LG		Make sure the old-value of a symbol was a font structure;
;;;  			ignore W's objects that are instances of their FONT flavor.
;;;  			Return the number of symbols fixed-up.

(DEFUN fixup-fonts-in-package (pack &optional dont-print-p)
  (COND ((FIND-PACKAGE pack)
	 (LOOP with n-symbols-fixedup = 0
	       for symbol being the local-interned-symbols
	       in (SETQ pack (FIND-PACKAGE pack))
	       for old-value = (AND (BOUNDP symbol)
				    (SYMBOL-VALUE symbol))
	       for new-value = (AND (ARRAYP old-value)
				    (TYPEP old-value 'font)
				    (get-ultimate-font-descriptor symbol))
	       do
	       (UNLESS (OR (NULL new-value) (EQ old-value new-value))
		 (UNLESS dont-print-p
		   (FORMAT t "~%Font symbol ~a changed from ~a to ~a."
			   symbol old-value new-value))
		 (INCF n-symbols-fixedup)
		 (SET symbol new-value))
	       finally (RETURN n-symbols-fixedup)))
	(t (FERROR nil "Package ~s does not exist" pack)))
  )

(DEFVAR *font-changes-by-package* nil)

(DEFUN clean-up-existing-font-refs ()
  (SETF *font-changes-by-package* nil)  
  (DO-ALL-PACKAGES (p)
    (LET ((n-fixups (fixup-fonts-in-package p t)))
      (UNLESS (ZEROP n-fixups)
	(PUSH (LIST p n-fixups) *font-changes-by-package*)))))

(ADD-INITIALIZATION "Fixup Font Refs"
		    '(clean-up-existing-font-refs)
		    :before-cold)


;;; CVV to get a new font mapping

(DEFPARAMETER fonts:*use-default-mac-font* nil)       ; So that others can use it without the Mac package being loaded put this in the fonts package.    
(DEFPARAMETER *mac-default-font* 'cptFontFont)
(DEFPARAMETER *mac-default-font-style* 0)
(DEFPARAMETER *mac-default-font-size* 12)

(DEFVAR *chosen-mac-font* "CptFont")
(DEFVAR *chosen-mac-style* nil)
(DEFVAR *chosen-mac-size* 12)
(DEFUN make-mac-font-translation (LISPM-font)
  (IF (CATCH 'if-default
        (w:choose-variable-values
          `((*chosen-mac-font* "Font"
		      :documentation
		      (:mouse-L-1 " To select from a menu of Mac fonts."
		       :documentation "Click on 'Do it' to use this font or on 'Default' to use the default font instead.")
		      :menu *mac-font-list*)
            (*chosen-mac-style* "Style"
		       :documentation
		       (:mouse-L-1 " To select any comination of stylistic variations."
		        :documentation
		        "Click on 'Do it' to use this font or on 'Default' to use the default font instead.")
		       :set
		       ,*mac-style-list*)
            (*chosen-mac-size* "Size"
		      :documentation
		      (:mouse-L-1 " To type in a new font size."
		        :documentation
		        "Click on 'Do it' to use this font or on 'Default' to use the default font instead.")
		      :typep (integer 0)))
          :margin-choices '("Do it" ("Default"
		            (THROW 'if-default t) :documentation
		            "Use the default font instead"))
          :label (FORMAT nil "Specify a Mac font to associate with ~a"
	             (w:font-name LISPM-font))
	  :extra-width "userealcharactersheresowegetrealcharwidths"))
      ;; Then .. return the default
      (LIST *mac-default-font* *mac-default-font-size* *mac-default-font-style*)
    ;; else..use the selected items
    (LET ((font-name (NTH (OR (POSITION *chosen-mac-font* *mac-font-list*
			   :test #'STRING-EQUAL) 0)
		 (GET '*mac-font-list* :mac-font-names)))
          (font-style (LET (style
		   (style-bits (GET '*mac-style-list*
			        :mac-font-style-bits)))
	            (DOLIST (a-style *chosen-mac-style* style)
		  (WHEN a-style
		    (SETF style
		          (CONS (NTH (POSITION a-style *mac-style-list*
				      :test #'STRING-EQUAL) 
			         style-bits)
			    style)))))))
      (CONS font-name (CONS *chosen-mac-size* font-style)))))


(DEFUN map-new-LISPM-font (LISPM-font &optional (font-spec nil))
  "Maps a LISPM font to a Mac font. Font-spec is a list of Mac-font-name,
size, and styles...e.g. '(cairoFont 12 4 16)"
  (LET ((mac-font-spec (OR font-spec
		  (make-mac-font-translation LISPM-font)))
        (LISPM-font-name (w:font-name LISPM-font)))
    (APPLY #'map-lispm-font
           LISPM-font-name
           (CONS (SYMBOL-VALUE (CAR mac-font-spec)) (CDR mac-font-spec)))
    (PUSH LISPM-font-name mac-font-spec)
    (SETF *mac-font-translation-table*
          (REMOVE-IF #'(lambda (spec) (EQ (CAR spec) LISPM-font-name))
	         *mac-font-translation-table*)) 
    (PUSH mac-font-spec *mac-font-translation-table*)))

(DEFUN Mac-font (LISPM-font &optional (enable-remap-p nil))
  "Given a LISPM font returns the Mac font."
  (COND ((GET (tv:font-name LISPM-font) :Mac-font))
        (fonts:*use-default-mac-font* (SYMBOL-VALUE *mac-default-font*))
        (enable-remap-p (map-new-LISPM-font LISPM-font)
	            (OR (GET (tv:font-name LISPM-font) :Mac-font)
		    (SYMBOL-VALUE *mac-default-font*)))
        (t (SYMBOL-VALUE *mac-default-font*))))

(DEFUN Mac-font-style (LISPM-font)
  "Given a LISPM font returns the Mac font style."
  (OR (GET (tv:font-name LISPM-font) :Mac-font-style)
       *mac-default-font-style*))

(DEFUN Mac-font-size (LISPM-font)
  "Given a LISPM font returns the Mac font size."
  (OR (GET (tv:font-name LISPM-font) :Mac-font-size)
       *mac-default-font-size*))
