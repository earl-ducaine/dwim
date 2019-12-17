;;; -*- Mode: Common-Lisp; Package: TV; Base: 10.; Fonts: CPTFONT,HL12B,HL12BI -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.

;;; Change History
;;;
;;;  Date      Author  Description
;;; ----------------------------------------------------------------------
;;; 03/23/89  MAY   Changes to quiet cwarn errors.
;;; 03/07/89  MAY   Changed convert-screen-to-color to use :set-screens-who-line-screen in/of (.set-screens-who-line-screen)
;;; 01/27/89  KJF    [may] Changes to convert-screen-to-color for Multiple Monitor (MMON) support.
;;; 05/23/88  KJF    Changed references to screen-descriptor to screens-who-line-screen due to source build
;;;                     being done which allowed instance variable to take on a meaningful name.
;;; 04/23/88  KJF    Added new version of convert-to-color and also made it obsolete.
;;;                     Also added convert-screen-to-color function.  Also :sheet-legal-for-superior
;;;                     and yet another change to :before :activate
;;; 04/10/88  KJF    Many changes due to multiple screen/dual monitor work.  Best way to
;;;                     find changes are to search for KJF.
;;; 01/20/88  KJF    Fix in convert-to-color; don't change w:normal to alu-transp (16).
;;;                     See patch-3-27.
;;;

;;; this file contains the code for massaging an existing band into color. The functions assume that the system has been either
;;; patched to a color capability, or source built. To carry out the conversion, do (convert-to-color t) if the system was
;;; made via patching, or (convert-to-color nil) if the system was source built with color sources.

;;; The only thing left to add to this program is to load and make the color system, i.e. the 
;;; color-map editor, and to load a modified the Sreen Editor Edit Attributes function 
;;; that relies on the stuff loaded by color. this later I'll get to you as part of the code read modifications
;;; process.

;;; the following parameter hangs around so that :before :activate knows how the system was banged together

(DEFPARAMETER *system-was-patched* nil "A parameter used in transistion from one release to the next")

;;; Let there be a color feature after the system has been converted 
(add-initialization "Declare the :COLOR feature"
		    '(pushnew :color *features*) :convert-to-color)

(DEFRESOURCE monochrome-color-conversion-arrays
	     (&optional convert-to-monochrome)
  "Arrays used by SHEET :CHANGE-OF-ARRAY method."
  :constructor (MAKE-ARRAY `(1024 1024)
			   ;; This may appear backwards, but the type we want is the type we're converting FROM...
			   :element-type (IF convert-to-monochrome '(MOD 256) 'bit))
  :initial-copies 1)

;;; this method is derived from :change-of-size-or-margin. In this case, however, its job is to transform arrays into
;;; 8 bit per pixel arrays.

(DEFMETHOD  (sheet :change-of-array)
	    ;; Add optional argument to allow conversion both ways.  CJJ 04/20/88.
	    ;; Made TEMPORARY a resource instead of an &aux variable.  CJJ 04/20/88.
	    ;;(&AUX (temporary (MAKE-ARRAY `(1024 1024) :element-type 'bit)))
	    (&optional convert-to-monochrome)
  "Grow the screen and/or bit array from 1 bit per pixel to 8 bits per pixel or vice versa."
  (OR superior
      ;; (NOT exposed-p) Code below will blow up if no superior whether exposed or not.  CJJ 04/20/80.
      (FERROR
	nil
	"Cannot change array of an window with no superior"))
  
  ;; Add check to make sure superior is already converted.  CJJ 04/20/80.
  (IF convert-to-monochrome
      (WHEN (color-sheet-p superior)
	(FERROR nil "Cannot convert to monochrome when superior is not monochrome"))
      ;; ELSE...
      (UNLESS (color-sheet-p superior)
	(FERROR nil "Cannot convert to color when superior is not color")))
  
  ;;; TEMPORARY has a window's contents BITBLT into it before the new array is created,
  ;;; then the contents are BITBLT'ed or copied with expansion or compression back into the new array.
  (USING-RESOURCE (temporary monochrome-color-conversion-arrays convert-to-monochrome)
    (WITHOUT-INTERRUPTS
      (sheet-force-access (self t)
	(MAPC #'open-blinker blinker-list))
      
      ;; If we have a bit array, save it into TEMPORARY,
      ;; then make new bit array, then copy TEMPORARY into the new bit array...
      (AND bit-array
	    ;; Check first to see if conversion has already occurred.  CJJ 04/20/88.
	   (EQUAL (ARRAY-ELEMENT-TYPE bit-array) (IF convert-to-monochrome '(MOD 256) 'BIT))
	   (PROGN
	     (BITBLT tv:alu-seta width height bit-array 0 0 temporary 0 0)
	     (SETQ bit-array
		   (MAKE-ARRAY `(,height 1024)
			       ;; Make element-type depend on conversion direction.  CJJ 04/20/88.
			       ;;:type 'art-8b
			       :element-type (IF convert-to-monochrome 'BIT '(MOD 256))))
	     ;; Use COMPRESS-ARRAY for conversion to monochrome.  CJJ 04/20/88.
	     (IF convert-to-monochrome
		 (w:compress-array background-color width height temporary 0 0 bit-array 0 0)
		 ;; ELSE...
		 (BITBLT tv:alu-seta width height temporary 0 0 bit-array 0 0))))
      
      ;; If we have a temporary bit array, save it into TEMPORARY,
      ;; then make the new temporary bit array, then copy TEMPORARY into the new temporary bit array...
      ;; This was added by KJF on 10/07/87.
      (AND (EQ (TYPEP temporary-bit-array) :array)
	   ;; Check first to see if conversion has already occurred...
	   ;; Make comparison element-type depend on conversion direction.  CJJ 04/20/88.
	   ;;(NOT (EQUAL (ARRAY-ELEMENT-TYPE temporary-bit-array) '(MOD 256)))
	   (EQUAL (ARRAY-ELEMENT-TYPE temporary-bit-array)
		  (IF convert-to-monochrome '(MOD 256) 'BIT))
	   (PROGN
	     (BITBLT tv:alu-seta width height temporary-bit-array 0 0 temporary 0 0)
	     (SETQ temporary-bit-array
		   (MAKE-ARRAY `(,height 1024)
			       ;; Make element-type depend on conversion direction.  CJJ 04/20/88.
			       ;;:type 'art-8b
			       :element-type (IF convert-to-monochrome 'BIT '(MOD 256))))
	     ;; Use COMPRESS-ARRAY for conversion to monochrome.  CJJ 04/20/88.
	     (IF convert-to-monochrome
		 (w:compress-array background-color width height temporary 0 0 temporary-bit-array 0 0)
		 ;; ELSE...
		 (BITBLT tv:alu-seta width height temporary 0 0 temporary-bit-array 0 0))))
      
      ;; If the screen array exists, and there is a bit array, create the screen array displaced into the bit array.
      ;; Otherwise, make the screen array displaced into the superior...
      (AND screen-array
	   (SETQ screen-array
		 (IF bit-array
		     (MAKE-ARRAY `(,height ,width)
				 ;; Make element-type depend on conversion direction.  CJJ 04/20/88.
				 ;;:type 'art-8b
				 :element-type (IF convert-to-monochrome 'BIT '(MOD 256))
				 :displaced-to bit-array
				 :displaced-index-offset 0)
		     ;; ELSE...
		     (MAKE-ARRAY `(,height ,width)
				 ;; Make element-type depend on conversion direction.  CJJ 04/20/88.
				 ;;:type 'art-8b
				 :element-type (IF convert-to-monochrome 'BIT '(MOD 256))
				 :displaced-to (OR (sheet-screen-array superior)
						   (sheet-old-screen-array superior))
				 :displaced-index-offset (+ x-offset
							    (* y-offset
							       (ARRAY-DIMENSION 
								 (OR (sheet-screen-array superior)
								     (sheet-old-screen-array superior))
								 1)))))))
      
        ;; If there is no screen array or bit array, then we have to make a screen array.
        ;; This case usually only exists for a deactivated window about to be activated...
      (WHEN (AND (NULL screen-array) (NULL bit-array))
	(SETQ screen-array
	      (MAKE-ARRAY `(,height ,width)
			  ;; Make element-type depend on conversion direction.  CJJ 04/20/88.
			  ;;:type 'art-8b
			  :element-type (IF convert-to-monochrome 'BIT '(MOD 256))
			  :displaced-to (OR (sheet-screen-array superior)
					    (sheet-bit-array superior)
					    (sheet-old-screen-array superior))
			  :displaced-index-offset (+ x-offset
						     (* y-offset
							(ARRAY-DIMENSION 
							  (OR (sheet-screen-array superior)
							      (sheet-bit-array superior)
							      (sheet-old-screen-array superior))
							  1))))))
      
      ;; Now that the screen and bit arrays are set up, redirect them as is done in :change-of-size-or-margins,
      ;; i.e. the heirarchy management operations...
      (COND (superior
	     ;; If we have a bit-array, SCREEN-ARRAY indirects to it, else
	     ;; OLD-SCREEN-ARRAY indirects into our superior...
	     (LET ((array (OR screen-array old-screen-array))
		   (indirect-to (OR (AND (NOT exposed-p)
					 bit-array)
				    (sheet-superior-screen-array))))
	       (redirect-array
		 array
		 (ARRAY-ELEMENT-TYPE indirect-to)
		 (ARRAY-DIMENSION indirect-to 1)
		 height
		 indirect-to
		 (IF (AND bit-array
			  (NOT exposed-p))
		     0
		     ;; ELSE...
		     (+ x-offset (* y-offset (ARRAY-DIMENSION indirect-to 1)))))
	       (IF (OR bit-array exposed-p)
		   (SETQ screen-array array
			 old-screen-array nil)
		   ;;ELSE...
		   (SETQ old-screen-array array
			 screen-array nil)))))))
  (SEND self :update-time-stamp))

(DEFUN convert-a-sheet-to-monochrome
       (sheet &optional (monitor :monochrome))
  "Convert a sheet from color to monochrome.  SHEET can also be a screen."
  (SETF (sheet-locations-per-line sheet) 32.)
  (SETF (sheet-plane-mask sheet)
	(IF (AND (TYPEP sheet 'screen)
		 (dual-monitor-p sheet))
	    (IF (EQ monitor :monochrome)
		*default-dual-monitor-monochrome-plane-mask*
		*default-dual-monitor-color-plane-mask*)
	    ;; ELSE...
	    *default-plane-mask*))
  (SEND self :set-reverse-video-p (sheet-color-reverse-video-state sheet))
  ;; We should try to make sure sheets get a reasonable color map...
  (WHEN (download-color-map-p sheet)
    (LET* ((superior (sheet-superior sheet)))
      ;; If color-map is not a type we know works well for monochrome, then figure where/how to
      ;; get/make a color-map which will work better.  Actually, in MOST cases, the color-map
      ;; will probably be of the "DEFAULT" type, so none of this may ever happen.  That is, in
      ;; Release 4.1, even monochrome sheets have a "DEFAULT" color map.  This is a color map
      ;; which works well for monochrome, color, or dual monitors.
      (UNLESS (OR (STRING-EQUAL (color-map-name (sheet-color-map sheet)) "DEFAULT")
		  (STRING-EQUAL (color-map-name (sheet-color-map sheet)) "B&W"))
	;; If we're a screen, we have our own copy (create a new one).
	;; If no superior, which may never be the case??, create a new one.
	(IF (OR (TYPEP sheet 'screen) (NULL superior))  ;; If we're a screen, we have our own copy.
	    (SETF (sheet-color-map sheet) (create-color-map)) ;; or (copy-color-map *default-color-map*))
	    ;; If our superior is a screen, make a copy of its map for us to use.  This is the case for TOP
	    ;; level windows (like the ZMACS frame or the Listener) .
	    (IF (TYPEP superior 'screen)
		(SETF (sheet-color-map sheet) (copy-color-map (sheet-color-map superior)))
		;; Otherwise, we always want to get a pointer to our superior's map.
		(SETF (sheet-color-map sheet) (sheet-color-map superior)))))))
  (IF (TYPEP sheet 'screen)
      (PROGN
	(SETF (sheet-screen-array sheet)
	      (MAKE-ARRAY (LIST (sheet-height sheet) (sheet-width sheet))  ;; width must always be 1024?
			  :type 'art-1b :displaced-to (IF sib-is-csib
							  csib-expans-no-transp-va
							  io-space-virtual-address)))
	(SETF (sheet-old-screen-array sheet) (sheet-screen-array sheet))
	(SETF (screen-bits-per-pixel sheet) 1.)
	(SETF (screen-buffer sheet) (IF sib-is-csib
					csib-expans-no-transp-va
					io-space-virtual-address))
	(SEND sheet :setup-property-list 1. monitor *default-sib*))
      ;; ELSE, we're not doing a screen...
      (SEND sheet :change-of-array t)))

(DEFUN convert-sheet-and-inferiors-to-monochrome
       (sheet)
  "Convert sheet and its inferiors to monochrome."
  (ASSERT sheet (sheet) "SHEET should not be NIL.")
  (convert-a-sheet-to-monochrome sheet)
  (DOLIST (inf (sheet-inferiors sheet))
    (convert-sheet-and-inferiors-to-monochrome inf)))

;;;; this next function creates an 8 bit screen displaced to the proper frame buffer for color, and adjusts instance variables.
;;; THIS IS NOT USED ANYMORE, see convert-screen-to-color, below.  04/23/88 KJF
;;; Also see create-color-screen and make-a-screen !!! in file SCREENS.LISP
;(comment
;(Defun fixup-screen ()
; (SETF (sheet-screen-array tv:main-screen) (MAKE-ARRAY '(754 1024) :type 'art-8b :displaced-to tv:csib-color-no-transp-va))
; (setf (sheet-old-screen-array tv:main-screen) (sheet-screen-array tv:main-screen))
; (SETF (screen-bits-per-pixel tv:main-screen) 8)
; (SETF (sheet-locations-per-line tv:main-screen) 256)
; (SETF (screen-buffer tv:main-screen) tv:csib-color-no-transp-va)
; ;;; reset the parameter for use by any later functions, such as who-line-setup, that allocate a screen
; (SETQ main-screen-buffer-address (IF sib-is-csib
;				       (IF tv:*color-system*
;				         csib-color-no-transp-va
;					 csib-expans-no-transp-va)
;				       IO-SPACE-VIRTUAL-ADDRESS))
;))

;;; this function changes the sheet instance variables, then applies :change-of-array
;;; THIS IS NOT USED ANYMORE, see convert-a-sheet-to-color below.  04/23/88 KJF
(comment
(Defun fixup-sheet (the-sheet)
  ;;; add code to handle blinkers if patched...
  (unless (typep (sheet-foreground-color the-sheet) '(integer 0 255))
    (SETF (sheet-foreground-color the-sheet) *default-foreground*))
  (unless (typep (sheet-background-color the-sheet) '(integer 0 255))
    (SETF (sheet-background-color the-sheet) *default-background*))
  (setf (sheet-color-reverse-video-state the-sheet) nil)
  (setf (sheet-locations-per-line the-sheet) 256)
  (setf (sheet-plane-mask the-sheet) #xff)
  (if (or (eq the-sheet w:main-screen) (eq w:main-screen (sheet-superior the-sheet)))
    (SETF (sheet-color-map the-sheet) (create-color-map))
    (setf (sheet-color-map the-sheet) (sheet-color-map (sheet-superior the-sheet))))
  
  (SETF (sheet-char-aluf the-sheet) tv:alu-transp)
  (SETF (sheet-erase-aluf the-sheet) tv:alu-back)
  (when (neq the-sheet w:main-screen) ;;; not of the following matters if we're doing the screen
    (send the-sheet :change-of-array)
     (LET ((temp (send the-sheet :send-if-handles :label))) ;;; not all windows have labels!
       (WHEN temp
         (IF *system-was-patched*
	   ;;; append the background to the existing label structure
           (SETF (send the-sheet :label) (push-end *default-label-background* temp))
	   ;;; else
	   ;;; change the label background from B&W value to color value
	   (SEND the-sheet :send-if-handles :set-label-background  *default-label-background*))
	 ;;; in either case update the existing foreground color of the label
	 (SEND the-sheet :set-label-color *default-label-foreground*)
        ))))
 )

;; >>> Modified version of fixup-sheet.  04/06/88 KJF.
;; This has some slight enhancements over fixup-sheet.  It also does some things,
;; (setup-property-list ...), useful for multiple screens/dual monitors.
;; WIth Rel 4.1, this may get used much more than fixup-sheet ever did.
(DEFUN convert-a-sheet-to-color (sheet)
  "Convert a sheet from monochrome to color.  SHEET can also be a screen."
  (UNLESS sib-is-csib
    (ERROR "Cannot convert sheets to color without a CSIB."))
  (UNLESS (TYPEP (sheet-foreground-color sheet) '(integer 0 255))
    (SETF (sheet-foreground-color sheet) *default-foreground*))
  (UNLESS (TYPEP (sheet-background-color sheet) '(integer 0 255))
    (SETF (sheet-background-color sheet) *default-background*))
  ;; Reverse video for color sheets is done by swapping char-aluf and erase-aluf i-vars.
  (SETF (sheet-color-reverse-video-state sheet) nil)
  (SETF (sheet-locations-per-line sheet) 256.)
  ;; If the system is in dual monitor mode, giving a screen a plane-mask of 255, and exposing
  ;; that screen, will break dual monitor mode.
  (SETF (sheet-plane-mask sheet) *default-plane-mask*)
  (SETF (sheet-char-aluf sheet) alu-transp)
  (SETF (sheet-erase-aluf sheet) alu-back)
  ;; We should try to make sure sheets get a reasonable color map.
  (LET* ((superior (sheet-superior sheet)))
    ;; If color-map is not a type we know works well for color, then figure where/how to
    ;; get/make a color-map which will work better.  Actually, in MOST cases, the color-map
    ;; will probably be of the "DEFAULT" type, so none of this may ever happen.  That is, in
    ;; Release 4.1, even monochrome sheets have a "DEFAULT" color map.  This is a color map
    ;; which works well for monochrome, color, or dual monitors.
    (UNLESS (OR (STRING-EQUAL (color-map-name (sheet-color-map sheet)) "DEFAULT")
		(STRING-EQUAL (color-map-name (sheet-color-map sheet)) "COLOR"))
      ;; If we're a screen, we have our own copy (create a new one).
      ;; If no superior, which may never be the case??, create a new one.
      (IF (OR (TYPEP sheet 'screen) (NULL superior))  ;; If we're a screen, we have our own copy.
	  (SETF (sheet-color-map sheet) (create-color-map)) ;; or (copy-color-map *default-color-map*))
	  ;; If our superior is a screen, make a copy of its map for us to use.  This is the case for TOP
	  ;; level windows (like the ZMACS frame or the Listener) .
	  (IF (TYPEP superior 'screen)
	      (SETF (sheet-color-map sheet) (copy-color-map (sheet-color-map superior)))
	      ;; Otherwise, we always want to get a pointer to our superior's map.
	      (SETF (sheet-color-map sheet) (sheet-color-map superior))))))
  (IF (TYPEP sheet 'screen)
      (PROGN
	(SETF (sheet-screen-array sheet)
	      (MAKE-ARRAY (LIST (sheet-height sheet) (sheet-width sheet))  ;; width must always be 1024?
			  :type 'art-8b :displaced-to csib-color-no-transp-va))
	(SETF (sheet-old-screen-array sheet) (sheet-screen-array sheet))
	(SETF (screen-bits-per-pixel sheet) 8.)
	(SETF (screen-buffer sheet) csib-color-no-transp-va)
	(SEND sheet :setup-property-list 8. :color *default-sib*))
      ;;else formS, when we're not doing a screen
      (SEND sheet :change-of-array)
      (LET ((temp (SEND sheet :send-if-handles :label))) ;;; not all windows have labels!
	(WHEN temp
	  (SEND sheet :set-label-background *default-label-background*)
	  (SEND sheet :set-label-color *default-label-foreground*)))))


;; >>> The following method has been changed.  New code is below.
;; Leaving this here for history's sake.  04/10/88 KJF.
;;
;; Comments by KJF.
;; The following method should only exist on a converted band.
;; For now, I'll just check for *color-system*.
;; If any of the tests pass, fixup-sheet does things regardless of whether
;;  it really needs to or not.  (e.g. if, for some reason, one of the arrays is
;;  a 1 bit array and another is an 8 bit array, the 8 bit array will get changed
;;  to another 8 bit array.  See window;convert.lisp
;; Must check for arrays being bound or nil first.  Will arrays of default-screen ever be unbound??
;; We are assuming all arrays of default-screen are always bound.
;; Temporary-bit-arrays in GED menus weren't always getting made into 8 bit arrays. 10/07/87 KJF
;;; here is our fail safe test for windows that were not in the heirarchy, but get activated. KWW
;;(defmethod (sheet :before :activate) (&rest ignore)
;;  (WHEN (AND *color-system* (or (NOT (EQUAL (sheet-locations-per-line self) (sheet-locations-per-line default-screen)))
;;				(AND (VARIABLE-BOUNDP bit-array) bit-array
;;				     (NOT (EQUAL (ARRAY-ELEMENT-TYPE bit-array) '(MOD 256))))
;;				(AND (VARIABLE-BOUNDP screen-array) screen-array
;;				     (NOT (EQUAL (ARRAY-ELEMENT-TYPE screen-array) '(MOD 256))))
;;				(AND (VARIABLE-BOUNDP old-screen-array) old-screen-array
;;				     (NOT (EQUAL (ARRAY-ELEMENT-TYPE old-screen-array) '(MOD 256))))
;;				(AND (VARIABLE-BOUNDP temporary-bit-array)
;;				     (EQ (typep temporary-bit-array) :array)
;;				     (NOT (EQUAL (ARRAY-ELEMENT-TYPE temporary-bit-array) '(MOD 256))))))
;;    (fixup-sheet self)
;;    )
;;  )

;; >>> New method.  Goes with modified :before :activate method below.  04/10/88 KJF.
(DEFMETHOD (sheet :sheet-legal-for-superior)
	   (&optional (convert-color-sheet *convert-color-sheet-to-monochrome*) &rest ignore)
  "Check if type, monochrome or color, of sheet matches type of its screen.
A monochrome sheet will be converted to color automatically, but
CONVERT-COLOR-SHEET must be t for color sheets to get converted
to monochrome.  The global *convert-color-sheet-to-monochrome* can be
set to always convert color sheets to monochrome.
This looks only at locations-per-line."
  (LET ((color-superior? (color-sheet-p superior))
	(color-sheet? (color-sheet-p self)))
    ;; For the good case, they match, just return.
    (UNLESS (EQ color-superior? color-sheet?)
;; Note: This comment is not valid anymore, but it's here for history's sake.
;; The :set-superior and :change-of-size-or-margins methods now will change arrays as
;; well as locations-per-line if it needs to.  For testing locations-per-line AND all the arrays
;; of a sheet for inconsistencies, see method :sheet-legal-for-superior-full-test.
      ;; Cannot rely only on locations-per-line since there are other places in the window
      ;; system which change locations-per-line.  For example: SHEET-SET-SUPERIOR-PARAMS
      ;; and (DEFMETHOD (SHEET :SET-SUPERIOR).  Thus we must check all arrays of sheet
      ;; to see that they will work for the screen.  This was proven by GED.  GED's drop
      ;; down menus get a :set-superior method to the menu-pane instead of the GED frame.
      ;; I believe so that they end up in the right place (below the menu pane).
      (IF (AND color-superior? (NOT color-sheet?))  ;; locations-per-line don't match.
;;		 ;; Check for arrays that aren't right.
;;		 (AND (VARIABLE-BOUNDP bit-array) bit-array
;;		      (NOT (EQUAL (ARRAY-ELEMENT-TYPE bit-array) '(MOD 256))))
;;		 (AND (VARIABLE-BOUNDP screen-array) screen-array
;;		      (NOT (EQUAL (ARRAY-ELEMENT-TYPE screen-array) '(MOD 256))))
;;		 (AND (VARIABLE-BOUNDP old-screen-array) old-screen-array
;;		      (NOT (EQUAL (ARRAY-ELEMENT-TYPE old-screen-array) '(MOD 256))))
;;		 (AND (VARIABLE-BOUNDP temporary-bit-array)
;;		      (EQ (typep temporary-bit-array) :array)
;;		      (NOT (EQUAL
;;			     (ARRAY-ELEMENT-TYPE temporary-bit-array) '(MOD 256))))))
	  (convert-sheet-and-inferiors-to-color self) ;;:convert
	  ;; else
	  (WHEN (AND (NOT color-superior?) color-sheet?)  ;; locations-per-line don't match.
;;		     ;; Check for arrays that aren't right.
;;		     (AND (VARIABLE-BOUNDP bit-array) bit-array
;;			  (NOT (EQUAL (ARRAY-ELEMENT-TYPE bit-array) 'bit)))
;;		     (AND (VARIABLE-BOUNDP screen-array) screen-array
;;			  (NOT (EQUAL (ARRAY-ELEMENT-TYPE screen-array) 'bit)))
;;		     (AND (VARIABLE-BOUNDP old-screen-array) old-screen-array
;;			  (NOT (EQUAL (ARRAY-ELEMENT-TYPE old-screen-array) 'bit)))
;;		     (AND (VARIABLE-BOUNDP temporary-bit-array)
;;			  (EQ (typep temporary-bit-array) :array)
;;			  (NOT (EQUAL (ARRAY-ELEMENT-TYPE temporary-bit-array) 'bit)))))
	    (IF convert-color-sheet
		(convert-sheet-and-inferiors-to-monochrome self)
		(ERROR "Type of ~s does not match its superior's, ~s, type."
		       self (get-screen self)))) ;;:error
	  ))))

(DEFMETHOD (sheet :sheet-legal-for-superior-full-test)
	   (&optional (convert-color-sheet *convert-color-sheet-to-monochrome*) &rest ignore)
  "Check if type, monochrome or color, of sheet matches type of its screen.
A monochrome sheet will be converted to color automatically, but
CONVERT-COLOR-SHEET must be t for color sheets to get converted
to monochrome.  The global *convert-color-sheet-to-monochrome* can be
set to always convert color sheets to monochrome.
This looks at more than just locations-per-line."
  (LET ((color-screen? (color-sheet-p (get-screen self)))
	(color-sheet? (color-sheet-p self)))
;;    ;; For the good case, they match, just return.
;;    (UNLESS (EQ color-screen? color-sheet?)
    ;; Cannot rely only on locations-per-line since there are other places in the window
    ;; system which change locations-per-line.  For example: SHEET-SET-SUPERIOR-PARAMS
    ;; and (DEFMETHOD (SHEET :SET-SUPERIOR).  Thus we must check all arrays of sheet
    ;; to see that they will work for the screen.  This was proven by GED.  GED's drop
    ;; down menus get a :set-superior method to the menu-pane instead of the GED frame.
    ;; I believe so that they end up in the right place (below the menu pane).
    (IF (AND color-screen?
	     (OR (NOT color-sheet?)  ;; locations-per-line don't match.
		 ;; Check for arrays that aren't right.
		 (AND (VARIABLE-BOUNDP bit-array) bit-array
		      (NOT (EQUAL (ARRAY-ELEMENT-TYPE bit-array) '(MOD 256))))
		 (AND (VARIABLE-BOUNDP screen-array) screen-array
		      (NOT (EQUAL (ARRAY-ELEMENT-TYPE screen-array) '(MOD 256))))
		 (AND (VARIABLE-BOUNDP old-screen-array) old-screen-array
		      (NOT (EQUAL (ARRAY-ELEMENT-TYPE old-screen-array) '(MOD 256))))
		 (AND (VARIABLE-BOUNDP temporary-bit-array)
		      (EQ (typep temporary-bit-array) :array)
		      (NOT (EQUAL
			     (ARRAY-ELEMENT-TYPE temporary-bit-array) '(MOD 256))))))
	(convert-sheet-and-inferiors-to-color self) ;;:convert
	;; else
	(IF (AND (NOT color-screen?)
		 (OR color-sheet?  ;; locations-per-line don't match.
		     ;; Check for arrays that aren't right.
		     (AND (VARIABLE-BOUNDP bit-array) bit-array
			  (NOT (EQUAL (ARRAY-ELEMENT-TYPE bit-array) 'bit)))
		     (AND (VARIABLE-BOUNDP screen-array) screen-array
			  (NOT (EQUAL (ARRAY-ELEMENT-TYPE screen-array) 'bit)))
		     (AND (VARIABLE-BOUNDP old-screen-array) old-screen-array
			  (NOT (EQUAL (ARRAY-ELEMENT-TYPE old-screen-array) 'bit)))
		     (AND (VARIABLE-BOUNDP temporary-bit-array)
			  (EQ (typep temporary-bit-array) :array)
			  (NOT (EQUAL (ARRAY-ELEMENT-TYPE temporary-bit-array) 'bit)))))
	    (IF convert-color-sheet
		(convert-sheet-and-inferiors-to-monochrome self)
		(ERROR "Type of ~s does not match its screens', ~s, type."
		       self (get-screen self)))) ;;:error
	)))

;;; >>> Slightly different version of old :before :activate.  04/10/88 KJF
(DEFMETHOD (sheet :before :activate) (&rest ignore)
  "This method tries to make sure things are O.K. before a sheet is used.
Basically, it checks to see if sheet matches its superior, by comparing
their locations-per-line."
  
  (SEND self :sheet-legal-for-superior)
  
;; >>> Not sure if the following is really necessary, or desirable.  KJF 04/06/88
;;  (IF (color-sheet-p self) ;; load up the registers in preparation for blinking in color
;;      (PROGN
;;	;;; >>>
;;	;;;; !!!! NOTE *default-blinker-offset* may not be what we want
;;	;;; Because we don't know about any blinkers in this particular window.
;;	;; What should be used is the blinker-offset of some blinker in this window.
;;	(send (aref *blocks* 0)  ;; add this amount to make a mouse
;;	      :set-foreground-color-register *default-blinker-offset*)
;;	  ;; we don't want to affect the background
;;	(send (aref *blocks* 0) :set-background-color-register 0))
;;      ;; else formS
;;      (send (aref *blocks* 0)
;;	    :set-foreground-color-register *current-foreground*)  ;; set for B&W operation
;;      (send (aref *blocks* 0)
;;	    :set-background-color-register *current-background*)))
  )

;; >>> Replaced by following function.  04/10/88 KJF
(DEFUN top-down (window) ;;; this works from the top of the heirarchy down
  (fixup-sheet window)
  (LET ((infs (sheet-inferiors window))
       )
    (UNLESS (NULL infs)
      (MAPC #'top-down infs)
    )
  )
  nil
)

;; >> Replaces top-down.  04/10/88 KJF
(DEFUN convert-sheet-and-inferiors-to-color (sheet)
  "Convert sheet and its inferiors to color."
  (ASSERT sheet (sheet) "SHEET should not be NIL.")
  (convert-a-sheet-to-color sheet)
  (DOLIST (inf (sheet-inferiors sheet))
    (convert-sheet-and-inferiors-to-color inf)))


(DEFUN refresh-all (window) ;;; this works from the top of the heirarchy down
  (send window :expose)
  (send window :refresh :complete-redisplay)
  (LET ((infs (sheet-inferiors window))
       )
    (UNLESS (NULL infs)
      (MAPC #'refresh-all infs)
    )
  )
  nil
)

;;; collect together some calls, so we can process-run-function on the lot...

;;; >>> call this function when converting b&w band to color
(DEFUN reset-default-colors ()
  (setq *default-foreground*          black)
  (setq *DEFAULT-BACKGROUND*          12%-gray-color)
  (setq *DEFAULT-BORDER-COLOR*        black)
  (setq *DEFAULT-SCROLL-BAR-COLOR*    75%-gray-color)

  (setq *default-label-foreground* black)
  (setq *default-label-background* 25%-gray-color)
  
;;; menu values

  (setq *default-menu-foreground* black)
  (setq *default-menu-background* 25%-gray-color)
  (setq *default-menu-label-foreground* black)
  (setq *default-menu-label-background* 50%-gray-color)

;;; who line screen valies

  (setq *default-status-foreground*   12%-gray-color)
  (setq *default-status-background*   black)
  (setq *default-documentation-foreground* black)
  (setq *default-documentation-background* 33%-gray-color)
  
)

(defun color-conversion-refresh ()
  (DECLARE (SPECIAL INITIAL-LISP-LISTENER)) ;; may 03/23/89 
  (refresh-all tv:main-screen)
  (send initial-lisp-listener :select)
  (SEND initial-lisp-listener :clear-window)
  (screen-redisplay)
)

;; This old version has been replaced.  See below.  04/23/88 KJF
(COMMENT
(defun convert-to-color ()	;PMH
  "Convert the current running system for color display.  You must be running
with a Color System Interface Board, CSIB, to do the conversion."

  (unless sib-is-csib (ferror "You must be running with a CSIB to do the conversion to color."))

  (SETQ *system-was-patched* T)
    ;; next line makes prepare-sheet macro update hardware registers in color environment
  (setf (symbol-function 'shift-into-graphics-mode) #'sheet-load-registers)
  (unless (aref *blocks* 0) (init-csib-registers))				;PMH
  (SETQ tv:*color-system* t)
  (tv:reset-default-colors)
  (tv:reset-color-map)
;;  (SETF w:normal tv:alu-transp)  ;; Should stay as w:alu-seta (5).  See patch-3-27 for color.
  (SETF w:erase  tv:alu-back)
  (SETF w:color-alist w:color-color-alist)
  (SETF w:alu-alist w:color-alu-alist)

  (fixup-screen)
  (SETQ tv:who-line-screen nil) ; set the old who line to nil, so who-line-setup can create a new, color version
  (who-line-setup)
  (top-down tv:main-screen)
  (clean-up-resources)
  ;;; now, to make it all look right, we refresh everyone....
  (color-conversion-refresh)
  ; now clean up resources by clearing all of them - then, the next time needed, they will be allocated as color versions
  (initializations '*CONVERT-TO-COLOR-INITIALIZATION-LIST*)
))

(DEFUN convert-to-color (&optional suppress-message)
  "Convert the current running system for color display.  You must be running
with a Color System Interface Board, CSIB, to do the conversion.  What this
actually does is convert W:MAIN-SCREEN to color.  Multiple screens of both
monochrome and color are now supported by the system and can coexist at the
same time.  The recommended way to make a color screen is to use
(W:CREATE-COLOR-SCREEN) or (W:MAKE-A-SCREEN ...).
SUPPRESS-MESSAGE means to not print message and prompt user about
MAIN-SCREEN being converted to color."
  (convert-screen-to-color main-screen suppress-message))

(compiler:make-obsolete convert-to-color
			"this function converts W:MAIN-SCREEN to color.  It is recommended
to use (W:CREATE-COLOR-SCREEN) or (W:MAKE-A-SCREEN ...) instead.")

;; may 01/27/89 
(DEFUN convert-screen-to-color (screen &optional suppress-message)
  "Convert a screen from monochrome to color.  All inferiors of screen
are also converted.  SCREEN will be exposed and selected after being
converted.  SUPPRESS-MESSAGE means to not print message and prompt
user if trying to convert MAIN-SCREEN to color."
  (DECLARE (SPECIAL previously-selected-windows)) ;; may 03/23/89 
  (WHEN (TYPEP screen 'who-line-screen)
    (ERROR "Cannot convert Who Line Screens to color."))
  (UNLESS (color-sheet-p screen)  ;; Already a color screen!
    (IF (mmon-p)
	;; Check monitor-controller in question instead of SIB-IS-CSIB.  CJJ 06/10/88.
	;;; Added by KJF on 08/20/88 for CJJ during addition of Multiple Monitor (MMON) support.
	(UNLESS (TYPEP (AREF *all-the-monitor-controllers*
			     (OR (SEND screen :sib-number) 0))
		       'csib)
	  (ERROR "Cannot convert sheets to color without a CSIB."))
	(UNLESS sib-is-csib
	  (ERROR "Cannot convert sheets to color without a CSIB.")))
    (WHEN (OR (NOT (EQ screen main-screen))
	      suppress-message
	      ;; Could use (cerror ...) instead??
	      (YES-OR-NO-P "An attempt is being made to convert W:MAIN-SCREEN to color.
Multiple screens of both monochrome and color are now supported by the system and
can coexist at the same time.  It is recommended to use (W:CREATE-COLOR-SCREEN)
or (W:MAKE-A-SCREEN ...) instead.  Are you sure you want to convert W:MAIN-SCREEN to color?"))
      (convert-sheet-and-inferiors-to-color screen)
      ;; If converting MAIN-SCREEN, must set up its screens-previously-selected-windows
      ;; instance variable.  Any other screens which exist would have already had it set up
      ;; by the MAKE-A-SCREEN function.
      (WHEN (AND (EQ screen main-screen)
		 ;; Check to make sure this hasn't already been done.  CJJ 06/10/88.
		 (NOT (screen-screens-previously-selected-windows screen)))
	(SEND screen :set-screens-previously-selected-windows
	      previously-selected-windows)
	(UNLESS (OR *mono-who-line* (color-sheet-p who-line-screen))
	  (SETQ *mono-who-line* who-line-screen))
	(UNLESS *color-who-line*
	  (SETQ *color-who-line* (make-who-line-screen "Color Who Line" 8 :color)))
	;;This forces SCREEN to have a who-line.
	(SEND screen :set-screens-who-line-screen *color-who-line*)) ;; may 03/07/89 
      (refresh-all screen) ;; This will do an :expose of screen.
      ;; We must do this :expose to get back to where we were and to :select a window.
      (SEND screen :expose))))

(DEFUN clean-up-resources ()
  (DOLIST (resource-symbol window-resource-names)
    (LET ((resource (GET resource-symbol 'DEFRESOURCE)))
      (when ( > (si:resource-n-objects resource) 0)
        (clear-resource resource-symbol)))))

