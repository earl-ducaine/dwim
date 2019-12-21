;;; -*- Mode: Common-LISP; Package: TV; Base: 10; Fonts: CPTFONT,HL12B,HL12BI -*-

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
;;; Copyright (C) 1983- 1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  04/14/89  MAY	Corrected (STREAM-MIXIN :BITBLT-WITHIN-SHEET) which was missing margin
;;;                         correction code that was dropped in 08/04/86 change below, apparently.
;;;			SPR's : 8235, 7155, 7282.
;;;  03/07/89  MAY	Change to stream-mixin to add sys:character-stream sub-flavor.
;;;                         This makes CLOS recognize windows as belonging to STREAM class.
;;;			Before CLOS existed, the function STREAMP just kludged-in (type 'tv:sheet).
;;;  01/26/89  MAY       Change to (stream-mixin :after :kill) for build process.
;;;  01/09/89  MAY	Corrected alu-transp cond clause in DRAW-CLIPPED-POINT. Spr 9064.
;;;  04/25/88  LG        Handle COLOR argument of the :draw-point, :draw-circle, and
;;; 		       :draw-filled-in-circle methods of the tv:graphics-mixin as a 0/1
;;;		       pixel value rather than a w:color value.
;;;  04/07/88  LG	    Handle :draw-line's COLOR argument correctly for the Mac.
;;;		    Fix incorrect b&w-system default value for :draw-line's color arg
;;;		    introduced by a previous patch.
;;;   3/07/88  LG          Made tv:'s graphics-mixin methods work on the Mac.
;;;   8/26/87  KWW      Changes from code reading:
;;;                          1. got rid of install color and restore color, used prepare-color-register or prepare-color macro
;;;                               as was appropriate
;;;                          2. got rid of (color *default-foreground* new-color?), making it (color (sheet-foreground-color self))
;;;   8/17/87  KWW      Fixed missing color argument in draw-curve
;;;                      Also fixed draw-clipped-point macro to correctly handle color alus
;;;                          This change required changes to draw-circle, draw-octant, and draw-circular-arc
;;;   5/22/87  KWW      Added color arguments to graphics-mixin, plus added optional color to
;;;                           stream-mixin draw-rectangle
;;;   7/2/87   PMH        Modified stream-mixin :tyi and :tyi-no-hang to get rid of compiler warnings
;;;   3/10/87  DAN	Added WITH-CLIPPING-RECTANGLE to :DRAW-CHAR.
;;;   3/10/87  DAN	Fixed :RESTORE-RUBOUT-HANDLER-BUFFER to correctly replace the old input.
;;;   3/09/87  DAN	Added (PLUSP around (CHAR-KEYPAD ch) in :ANY-TYI and :ANY-TYI-NO-HANG to
;;;                         correctly check for keypad character.
;;;  2/02/87   TWE	Fixed up :ANY-TYI and :ANY-TYI-NO-HANG to so that the keypad code would work
;;;			with the rubout handler.
;;;  1/22/87   JEB	Added WITH-CLIPPING-RECTANGLE to GRAPHICS-MIXIN :DRAW-REGULAR-POLYGON.
;;;  1/21/87   TWE	Changed the check-type in :add-asynchronous-character to allow characters in
;;;			addition to fixnums.
;;;  1/15/87   JEB	Fixed the SPLINE function to not modify input arrays unless needed.
;;;  1/09/87   TWE	Fixed up :ANY-TYI-NO-HANGs to handle keypad characters properly.
;;; 12/23/86   JEB	Put in the permanent fix for the 12/17 fix (the bug was really here).
;;; 12/23/86   TWE	Made several changes to make the build process cleaner.
;;;			Moved TRUNCATING-WINDOW from here to BASWIN near truncating-pop-up-text-window.
;;; 12/18/86   TWE	Fixed :DRAW-LINE so that the argument list was specified correctly (the parentheses
;;;			didn't match.  Also commented it so that the next person understands what was done,
;;;			why it was done and when it will be appropriate to back out the hack.
;;; 12/17/86   KDB	Made workaround fix for :DRAW-LINE method. (DRAW-END-POINT T) as a default
;;;			is now temporarily hard coded in SYS:DRAW-SHADED-TRIANGLE, where bug really is.
;;; 12/11/86   TWE	Fixed up what was done on 12/01 to use char-keypad correctly.
;;; 12/10/86   TWE	Fixed the references to the suggestions symbol function-arglist to have
;;;			the sys: package prefix.
;;; 12/04/86   TWE	Changed references to copy-array-contents to use replace.
;;; 12/01/86   TWE	Put in the support for keypad characters in :ANY-TYI and :ANY-TYI-NOHANG.
;;; 11/25/86   TWE	Changed references to %draw-filled-triangle to use %draw-shaded-triangle instead.
;;;			Also changed calls to %draw-line with %draw-shaded-triangle.
;;; 11/20/86   SLM	Added calls to Suggestions macros for (:METHOD TV:STREAM-MIXIN :RUBOUT-HANDLER).
;;; 10/28/86   KDB        Fixed (GRAPHICS-MIXIN :DRAW-REGULAR-POLYGON) to use System:%draw-filled-triangle.
;;; 10/01/86   TWE	Fixed up the blip character input (:any-tyi and friends) methods to use a common
;;;			function to convert mouse-button blips to contain fixnums instead of character objects.
;;;			Also added a hook into this function to allow a caller of a blip character input method
;;;			to make sure that mouse-button blips contain character objects.  This was done for
;;;			the read-any function and friends.
;;; 09/08/86   TWE	Changed misnamed defstruct slots rh-mumble to rhb-mumble
;;; 09/08/86   GRH	Fixed :draw-wide-curve to accept floating point arrays.
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 08/07/86   GRH	Changed :draw-regular-polygon to use the new primitive %draw-shaded-triangle.
;;; 08/04/86   GRH	Rewrote :bitblt, :bitblt-from-sheet :bitblt-within-sheet to fix some clipping bugs.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 07/21/86   TWE	Changed several functions to use SAFE-CHAR= instead of CHAR=.
;;; 07/01/86   TWE	Added an efficiency hack to DEFAULT-RUBOUT-HANDLER so that it would do a MEMBER
;;;			test on characters faster.
;;; 06/24/86   TWE	Fixed :UNTYI to do a CHAR= comparison instead of EQ to compare the a character in
;;;			the rubout handler's buffer with the character being UNTYIed.

;;; This is overridden by loading SYS:WINDOW;RH, which is now standardly
;;; loaded.
(DEFVAR STREAM-MIXIN-RUBOUT-HANDLER 'DEFAULT-RUBOUT-HANDLER)

;;; Io stream stuff
(DEFFLAVOR STREAM-MIXIN
	   ((IO-BUFFER NIL)
	    (RUBOUT-HANDLER-BUFFER NIL)
	    (OLD-TYPEAHEAD NIL))  ;Used for :PREEMPTABLE-READ.
	   (sys:character-stream) ;; may 03/07/89 Added (per dng) for CLOS - make window an instance of STREAM class
  ;; Explicit presence of SHEET helps init flavor-unmapped-instance-variables.
  (:REQUIRED-FLAVORS SHEET ESSENTIAL-WINDOW)
  (:SELECT-METHOD-ORDER :TYO :STRING-OUT :LINE-OUT :TYI :TYI-NO-HANG :LISTEN)
  (:GETTABLE-INSTANCE-VARIABLES IO-BUFFER)
  (:INITABLE-INSTANCE-VARIABLES IO-BUFFER RUBOUT-HANDLER-BUFFER)
  (:SETTABLE-INSTANCE-VARIABLES OLD-TYPEAHEAD)
  (:INIT-KEYWORDS :ASYNCHRONOUS-CHARACTERS)
  (:DOCUMENTATION :MIXIN "Ordinary tv stream operations.
Gives all the meaningful stream operations for a display, such as :TYO,
:TYI, :RUBOUT-HANDLER, :STRING-OUT, etc.  Include this flavor someplace
so that the window can be passed to functions that take streams as
arguments, and especially if TERMINAL-IO is going to be bound to the
window."))

;;; The third, fourth and fifth components of this structure are used
;;; only by LMIO;RH, and not by the default rubout handler.

(DEFSTRUCT (RUBOUT-HANDLER-BUFFER (:TYPE :ARRAY-LEADER)
				  (:MAKE-ARRAY (:LENGTH 512 ))
				  (:DEFAULT-POINTER RUBOUT-HANDLER-BUFFER)
				  (:CONC-NAME RHB-)
				  (:ALTERANT NIL)

				  (:PREDICATE NIL)
				  (:COPIER NIL))
  (FILL-POINTER 0)      ;Index into buffer for the next character.
  (SCAN-POINTER 0)
  (TYPEIN-POINTER 0)
  (DONT-SAVE-FLAG NIL)  ;T means this input should not go on the input ring.
  (INPUT-RING NIL)
  (INITIAL-ENTRY NIL))  ;Set to T on entry to :RUBOUT-HANDLER stream op.

(DEFMETHOD (STREAM-MIXIN :BEFORE :INIT) (INIT-PLIST)
 ;; Pre-create :WHICH-OPERATIONS, certainly going to be used.
  (SEND SELF :WHICH-OPERATIONS)
  (OR (TYPEP IO-BUFFER 'IO-BUFFER)
      ;; IO-BUFFER isn't an IO buffer.  Convert it from what it
      ;; is to an IO buffer.
       (LET (SIZE
	    INPUT-FUNCTION
	    OUTPUT-FUNCTION)
	(IF (NUMBERP IO-BUFFER)
	    (SETQ SIZE IO-BUFFER
		  INPUT-FUNCTION NIL
		  OUTPUT-FUNCTION 'KBD-DEFAULT-OUTPUT-FUNCTION)
	    (SETQ SIZE (OR (FIRST IO-BUFFER) 64)
		  INPUT-FUNCTION (SECOND IO-BUFFER)
		  OUTPUT-FUNCTION (OR (THIRD IO-BUFFER) 'KBD-DEFAULT-OUTPUT-FUNCTION)))
	(SETQ IO-BUFFER (MAKE-IO-BUFFER SIZE INPUT-FUNCTION OUTPUT-FUNCTION))))
  (IF (GETL INIT-PLIST '(:ASYNCHRONOUS-CHARACTERS))
      (SETF (GETF (IO-BUFFER-PLIST IO-BUFFER) :ASYNCHRONOUS-CHARACTERS)
	    (GET INIT-PLIST :ASYNCHRONOUS-CHARACTERS)))
  (OR RUBOUT-HANDLER-BUFFER
      (SETQ RUBOUT-HANDLER-BUFFER (MAKE-RUBOUT-HANDLER-BUFFER))))

;;; Added to get rid of non-garbage garbage from the input kill ring  PMH 12/8/87
(defmethod (stream-mixin :after :kill) (&rest ignore &aux input-ring)
  (DECLARE (SPECIAL zwei:*histories-to-clear*)) ;; may 01/26/89 make build process cleaner
  (when (and ;;most of the following test are need for the bootstraping build  PMH
	     (fboundp 'rh-input-ring)
	     (boundp 'rubout-handler)
	     (boundp 'rubout-handler-buffer)
	     (fboundp 'zwei:clear-history)
	     (boundp 'zwei:*histories-to-clear*)
	     (setf input-ring (rh-input-ring)))
    (zwei:clear-history input-ring)
    (setf zwei:*histories-to-clear*
	  (remove input-ring (the list zwei:*histories-to-clear*) :test #'eq ))))

;;; The :ASYNCHRONOUS-CHARACTERS property list contains:
;;;   (the-character a-function-to-call optional-arguments)
(DEFMETHOD (STREAM-MIXIN :ADD-ASYNCHRONOUS-CHARACTER) (CHARACTER FUNCTION &REST ARGS)
  (CHECK-TYPE CHARACTER (OR FIXNUM CHARACTER))                      ;; character ??
  (CHECK-ARG FUNCTION FUNCTIONP "a function")
  (LET ((PLIST (LOCF (IO-BUFFER-PLIST IO-BUFFER))))
    (PUSH (LIST* CHARACTER FUNCTION (COPY-LIST ARGS))
	  (GET PLIST :ASYNCHRONOUS-CHARACTERS))))

(DEFMETHOD (STREAM-MIXIN :ASYNCHRONOUS-CHARACTER-P) (CHARACTER)
  (LET* ((PLIST (LOCF (IO-BUFFER-PLIST IO-BUFFER)))
	 (ALIST (GET PLIST :ASYNCHRONOUS-CHARACTERS)))
    (ASSOC CHARACTER ALIST :TEST #'safe-char=)))    ;;;;;;;;;;;;;;;;;;;;;;;;;should be #'char=

(DEFMETHOD (STREAM-MIXIN :HANDLE-ASYNCHRONOUS-CHARACTER) (CHARACTER)
  (LET* ((PLIST (LOCF (IO-BUFFER-PLIST IO-BUFFER)))
	 (ALIST (GET PLIST :ASYNCHRONOUS-CHARACTERS))
	 TEM)
    (AND (SETQ TEM (ASSOC CHARACTER ALIST :TEST #'EQ))
	 (APPLY (CADR TEM) (CAR TEM) SELF (CDDR TEM)))))

(DEFMETHOD (STREAM-MIXIN :REMOVE-ASYNCHRONOUS-CHARACTER) (CHARACTER)
  (LET* ((PLIST (LOCF (IO-BUFFER-PLIST IO-BUFFER)))
	 (ALIST (GET PLIST :ASYNCHRONOUS-CHARACTERS)))
    (SETF (GET PLIST :ASYNCHRONOUS-CHARACTERS)
	  (DELETE (ASSOC CHARACTER ALIST :TEST #'EQ) (THE LIST ALIST) :TEST #'EQ))))

(DEFMETHOD (STREAM-MIXIN :DIRECTION) ()
  :BIDIRECTIONAL)

(DEFMETHOD (STREAM-MIXIN :BEFORE :SELECT) (&REST IGNORE)
  (KBD-CLEAR-SELECTED-IO-BUFFER))

(DEFMETHOD (STREAM-MIXIN :BEFORE :DESELECT) (&REST IGNORE)
  (KBD-CLEAR-SELECTED-IO-BUFFER))

(DEFMETHOD (STREAM-MIXIN :SET-IO-BUFFER) (NEW-BUFFER)
  (WITHOUT-INTERRUPTS
    (KBD-CLEAR-SELECTED-IO-BUFFER)
    (SETQ IO-BUFFER NEW-BUFFER)))

(DEFMETHOD (STREAM-MIXIN :PUSH-INPUT) (INPUT)
  (IF (STRINGP INPUT)
      (DO ((I (1- (LENGTH INPUT)) (1- I)))
	  ((MINUSP I))
	(IO-BUFFER-PUSH IO-BUFFER (AREF INPUT I)))
      (IO-BUFFER-PUSH IO-BUFFER INPUT)))

(DEFMETHOD (STREAM-MIXIN :DRAW-RECTANGLE)
           (RECTANGLE-WIDTH RECTANGLE-HEIGHT X Y
            &OPTIONAL (ALU CHAR-ALUF) (color (sheet-foreground-color self)))
 (prepare-color (self color)
  (PREPARE-SHEET (SELF)
     (DRAW-RECTANGLE-INSIDE-CLIPPED RECTANGLE-WIDTH RECTANGLE-HEIGHT X Y ALU SELF)
  )
 )
)

(DEFMETHOD (STREAM-MIXIN :BITBLT)				; - Rewrote 8/4/86 -GRH
           (ALU WID HEI FROM-ARRAY FROM-X FROM-Y TO-X TO-Y)
  "Copy rectangle from FROM-ARRAY to self's screen-array."
  (PREPARE-SHEET (SELF)
    (BITBLT-array ALU wid hei
		    FROM-ARRAY from-x from-y
		    SCREEN-ARRAY (+ to-x (sheet-inside-left)) (+ to-y (sheet-inside-top))
		    (SHEET-INSIDE-LEFT)
		    (SHEET-INSIDE-TOP)
		    (SHEET-INSIDE-RIGHT)
		    (SHEET-INSIDE-BOTTOM))))

(DEFMETHOD (STREAM-MIXIN :BITBLT-FROM-SHEET)
           (ALU WID HEI FROM-X FROM-Y TO-ARRAY TO-X TO-Y
            &AUX (IL (SHEET-INSIDE-LEFT))
            (IR (SHEET-INSIDE-RIGHT))
            (IT (SHEET-INSIDE-TOP))
            (IB (SHEET-INSIDE-BOTTOM))
	    (abs-wid (abs wid))
	    (abs-hei (abs hei)))
  "Copy rectangle from self's screen-array at inside coordinates
 FROM-X, FROM-Y to a given array, clipping as necessary."
  (when (< from-x il)
    (setq abs-wid (- abs-wid (- il from-x))
	  from-x il))
  (when (< from-y it)
    (setq abs-hei (- abs-hei (- it from-y))
	  from-y it))
  (when (> (+ from-x abs-wid) ir)
    (setq abs-wid (- ir from-x)))
  (when (> (+ from-y abs-hei) ib)
    (setq abs-hei (- ib from-y)))
  (when (and (plusp abs-hei) (plusp abs-wid))
    (prepare-sheet (self)
      (bitblt-array alu (if (minusp wid) (- abs-wid) abs-wid)
		    (if (minusp hei) (- abs-hei) abs-hei)
		    screen-array from-x from-y to-array to-x to-y))))

(DEFMETHOD (STREAM-MIXIN :BITBLT-WITHIN-SHEET)
           (ALU WID HEI FROM-X FROM-Y TO-X TO-Y
            &AUX (IL (SHEET-INSIDE-LEFT))
            (IR (SHEET-INSIDE-RIGHT))
            (IT (SHEET-INSIDE-TOP))
            (IB (SHEET-INSIDE-BOTTOM))
	    (abs-wid (abs wid))
	    (abs-hei (abs hei)))
  "Move rectangle on self's screen-array at inside coordinates
 FROM-X, FROM-Y to TO-X, TO-Y, clipping as necessary."
  (SETQ FROM-X (+ IL FROM-X) FROM-Y (+ IT FROM-Y) ; may 04/14/89 Added in code lost in release 3
	TO-X (+ IL TO-X) TO-Y (+ IT TO-Y))	  ; may 04/14/89
  (when (< from-x il)
    (setq abs-wid (- abs-wid (- il from-x))
	  from-x il))
  (when (< from-y it)
    (setq abs-hei (- abs-hei (- it from-y))
	  from-y it))
  (when (> (+ from-x abs-wid) ir)
    (setq abs-wid (- ir from-x)))
  (when (> (+ from-y abs-hei) ib)
    (setq abs-hei (- ib from-y)))
  (when (and (plusp abs-hei) (plusp abs-wid))
    (prepare-sheet (self)
      (bitblt-array alu (if (minusp wid) (- abs-wid) abs-wid)
		    (if (minusp hei) (- abs-hei) abs-hei)
		    screen-array from-x from-y screen-array to-x to-y
		    il it ir ib))))

(defun bitblt-array (alu width height from-array from-x from-y to-array to-x to-y
		     &optional (to-x-min 0) (to-y-min 0)
		     (to-x-max (array-dimension to-array 1))
		     (to-y-max (array-dimension to-array 0))
            &AUX
	    (abs-width (abs width))
	    (abs-height (abs height))
	    (max-from-height (ARRAY-DIMENSION from-array 0))
	    delta)
  "Copy rectangle of WIDTH and HEIGHT from FROM-ARRAY to TO-ARRAY with ALU.
  Arguments are clipped as appropriate.  Destination is assumed prepared."

  (when (< to-x to-x-min)
    (setq delta (- to-x-min to-x)
	  abs-width (- abs-width delta)
	  from-x (+ from-x delta)
	  to-x to-x-min))
  (when (> (+ to-x abs-width) to-x-max)
    (setq abs-width (- to-x-max to-x)))
  (when (< to-y to-y-min)
    (setq delta (- to-y-min to-y)
	  abs-height (- abs-height delta)
	  from-y (+ from-y delta)
	  to-y to-y-min))
  (when (> (+ to-y abs-height) to-y-max)
    (setq abs-height (- to-y-max to-y)))
  (when (or (minusp from-y)
	    (>= from-y max-from-height))
    ; this assumes we want to wrap around, not clip
    (setq from-y (abs (rem from-y max-from-height))))

  (cond ((not (plusp abs-width)))		;bitblt errs when w=h=0
	((not (plusp abs-height)))		;and dims are out of bounds
	(t
	 (BITBLT ALU (if (minusp width) (- abs-width) abs-width)
		 (if (minusp height) (- abs-height) abs-height)
		 from-array from-x from-y
		 to-array to-x to-y))))

(DEFMETHOD (STREAM-MIXIN :DRAW-CHAR)
           (FONT CHAR X-BITPOS Y-BITPOS &OPTIONAL (ALU CHAR-ALUF) color)
  (LET ((left 0) (right (sheet-width self)) (top 0) (bottom (sheet-height self)))
    (prepare-color (self color)
    (PREPARE-SHEET (SELF)
      (with-clipping-rectangle (left top right bottom)
        (DRAW-CHAR FONT CHAR
                   (+ X-BITPOS LEFT-MARGIN-SIZE)
                   (+ Y-BITPOS TOP-MARGIN-SIZE)
                   ALU SELF))))))

(DEFMETHOD (STREAM-MIXIN :UNTYI) (CH)
  (IF (AND RUBOUT-HANDLER
	   ;; RUBOUT-HANDLER added as conjunct 6/1/83
	   ;; to avoid lossage entering editor rubout handler
	   ;; by typing (= 1 2) then stray ) while inside BREAK.
	   (<= 1 (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
	   (SAFE-CHAR= CH (AREF RUBOUT-HANDLER-BUFFER (1- (RHB-SCAN-POINTER)))))
      (DECF (RHB-SCAN-POINTER))
      ;;ELSE
      (IO-BUFFER-UNGET IO-BUFFER CH)))

(DEFMETHOD (STREAM-MIXIN :LISTEN) ()
;;; 08/24/84 - Removed the direct call to kbd-process-main-loop-internal.
;;; The keyboard process handles this anyway.

  (NOT (AND (<= (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
	    (IO-BUFFER-EMPTY-P IO-BUFFER)
	    (WITHOUT-INTERRUPTS
	      (IF (NEQ IO-BUFFER (KBD-GET-IO-BUFFER)) T
;;;;;;;;;;;;;;;;;;(AND (KBD-HARDWARE-CHAR-AVAILABLE)
;;;;;;;;;;;;;;;;;;;;;;;(KBD-PROCESS-MAIN-LOOP-INTERNAL))
		  (IO-BUFFER-EMPTY-P KBD-IO-BUFFER))))))


(DEFMETHOD (STREAM-MIXIN :WAIT-FOR-INPUT-WITH-TIMEOUT) (TIMEOUT)
  (KBD-WAIT-FOR-INPUT-WITH-TIMEOUT IO-BUFFER TIMEOUT))

(DEFMETHOD (STREAM-MIXIN :CLEAR-INPUT) ()
  (SETF (RHB-FILL-POINTER) 0)
  (SETF (RHB-SCAN-POINTER) 0)
  (IO-BUFFER-CLEAR IO-BUFFER)
  (AND (EQ IO-BUFFER (KBD-GET-IO-BUFFER))
       (KBD-CLEAR-IO-BUFFER)))

(DEFMETHOD (STREAM-MIXIN :TYI) (&OPTIONAL IGNORE)
  (DO ((CH)) (NIL)
    (AND (NUMBERP (SETQ CH (SEND SELF :ANY-TYI)))
	 (RETURN CH))
    (AND (CHARACTERP CH)
	 (RETURN (CHAR-INT CH)))
    (AND (CONSP CH)
	 (EQ (CAR CH) :MOUSE-BUTTON)
	 ;; The following EQ test is correct in spite of what the compiler says.
	 (EQL (INT-CHAR (CADR CH)) #\MOUSE-3-1)	;used to be EQ PMH 7/1/87
	 (MOUSE-CALL-SYSTEM-MENU))))

(DEFMETHOD (STREAM-MIXIN :TYI-NO-HANG) (&OPTIONAL IGNORE)
  (DO ((CH)) (NIL)
    (AND (OR (NULL (SETQ CH (SEND SELF :ANY-TYI-NO-HANG)))
	     (NUMBERP CH))
	 (RETURN CH))
    (AND (CHARACTERP CH)
	 (RETURN (CHAR-INT CH)))
    (AND (CONSP CH)
	 (EQ (CAR CH) :MOUSE-BUTTON)
	 ;; The following EQ test is correct in spite of what the compiler says.
	 (EQL (INT-CHAR (CADR CH)) #\MOUSE-3-1);used to be EQ PMH 7/1/87
	 (MOUSE-CALL-SYSTEM-MENU))))

;;
;;Modified by Hogge to drop code related to the handling of :PREEMPTABLE-READ.
;;The proper mechanism is now installed in :PREEMPTABLE-READ and TV:ALTERNATE-RUBOUT-HANDLER.
;;
(DEFMETHOD (STREAM-MIXIN :ANY-TYI) (&OPTIONAL IGNORE &AUX ch IDX)
  ;; If there are characters to be read located in the rubout handler's buffer
  ;; then get the input from there.  Otherwise get input from the window's IO buffer.
  (COND ((> (RHB-FILL-POINTER) (SETQ IDX (RHB-SCAN-POINTER)))
	 (SETF (RHB-SCAN-POINTER) (1+ IDX))
	 (OR (SETQ CH (AREF RUBOUT-HANDLER-BUFFER IDX))
	     (FERROR NIL "EOF on input from a window.")))
	((NOT RUBOUT-HANDLER)
	 (WHEN (NULL (SETQ CH (KBD-IO-BUFFER-GET IO-BUFFER T)))
	   ;; No character was waiting for us.  We need to wait until there is one.
	   (SEND SELF :NOTICE :INPUT-WAIT)
	   (SETQ CH (KBD-IO-BUFFER-GET IO-BUFFER)))
	 ;; When we have a keypad character and the keypad bit for the window is
	 ;; off then make it a non-keypad character.  By default, all characters
	 ;; generated from keys on the keypad have their keypad bit turned on.
	 (WHEN (AND (OR (CHARACTERP CH) (INTEGERP CH))
		    (ZEROP (SHEET-KEYPAD-ENABLE))
		    (PLUSP (CHAR-KEYPAD CH)))
	   (SETQ CH (SET-CHAR-BIT CH :KEYPAD NIL))
	   ;; The following trick is to make sure that if someone does an untyi
	   ;; there won't be an error.  One can only untyi something from a buffer
	   ;; if it the same as was gotten from the buffer.
	   (IO-BUFFER-PUT IO-BUFFER CH)
	   (IO-BUFFER-GET IO-BUFFER CH)))
	(T (OR (SETQ ch (FUNCALL STREAM-MIXIN-RUBOUT-HANDLER))
	       (FERROR nil "EOF on input from a window."))))
  (CHAR-INT-MOUSE-BUTTON-BLIP CH)
  CH)

;;; If this is bound to NIL then character part of a mouse-button blip is converted to a
;;; character object, otherwise it is converted to a fixnum.
(DEFVAR CHAR-INT-MOUSE-BUTTON-BLIP-P T)

;;; Convert a :mouse-button blip so that it contains a fixnum in the character component.
;;; Note that this function destructively modifies the blip to accomplish this.
(DEFUN CHAR-INT-MOUSE-BUTTON-BLIP (OBJECT)
  (IF CHAR-INT-MOUSE-BUTTON-BLIP-P
     (WHEN (AND (CONSP OBJECT) (EQ (FIRST OBJECT) :MOUSE-BUTTON) (CHARACTERP (SECOND OBJECT)))
       (SETF (SECOND OBJECT) (CHAR-INT (SECOND OBJECT))))
     ;;ELSE
     (WHEN (AND (CONSP OBJECT) (EQ (FIRST OBJECT) :MOUSE-BUTTON) (INTEGERP (SECOND OBJECT)))
       (SETF (SECOND OBJECT) (INT-CHAR (SECOND OBJECT)))))
  OBJECT)

(DEFMETHOD (STREAM-MIXIN :ANY-TYI-NO-HANG) (&OPTIONAL IGNORE &AUX ch)
  (COND ((NOT RUBOUT-HANDLER)
	 (SETQ CH (KBD-IO-BUFFER-GET IO-BUFFER T))
	 ;; When we have a keypad character and the keypad bit for the window is
	 ;; off then make it a non-keypad character.  By default, all characters
	 ;; generated from keys on the keypad have their keypad bit turned on.
	 (WHEN (AND (OR (CHARACTERP CH) (INTEGERP CH))
		    (ZEROP (SHEET-KEYPAD-ENABLE))
		    (PLUSP (CHAR-KEYPAD CH)))
	   (SETQ CH (SET-CHAR-BIT CH :KEYPAD NIL))
	   ;; The following trick is to make sure that if someone does an untyi
	   ;; there won't be an error.  One can only untyi something from a buffer
	   ;; if it the same as was gotten from the buffer.
	   (IO-BUFFER-PUT IO-BUFFER CH)
	   (IO-BUFFER-GET IO-BUFFER CH)))
	(T (FERROR NIL ":ANY-TYI-NO-HANG from inside a rubout handler.")))
  (CHAR-INT-MOUSE-BUTTON-BLIP CH)
  CH)

;;;For things only prepared to deal with fixnums
(DEFMETHOD (STREAM-MIXIN :MOUSE-OR-KBD-TYI) ()
  (DO ((CH)) (NIL)
    (COND ((NUMBERP (SETQ CH (SEND SELF :ANY-TYI)))
	   (RETURN CH CH))
	  ((CHARACTERP CH)
	   (RETURN (CHAR-INT CH) (CHAR-INT CH)))
	  ((AND (CONSP CH) (EQ (CAR CH) :MOUSE-BUTTON))
             (CHAR-INT-MOUSE-BUTTON-BLIP CH)
             (RETURN (VALUES (SECOND CH) CH))))))

(DEFMETHOD (STREAM-MIXIN :MOUSE-OR-KBD-TYI-NO-HANG) ()
  (DO ((CH)) (NIL)
    (COND ((OR (NULL (SETQ CH (SEND SELF :ANY-TYI-NO-HANG)))
	       (NUMBERP ch))
	   (RETURN CH CH))
	  ((CHARACTERP CH)
	   (RETURN (CHAR-INT CH) (CHAR-INT CH)))
	  ((AND (CONSP CH) (EQ (CAR CH) :MOUSE-BUTTON))
             (CHAR-INT-MOUSE-BUTTON-BLIP CH)
	   (RETURN (VALUES (SECOND CH) CH))))))

(DEFMETHOD (STREAM-MIXIN :LIST-TYI) ()
  "Only return lists."
  (LOOP FOR CH = (SEND SELF :ANY-TYI)
        UNTIL (CONSP CH)
        FINALLY (RETURN (CHAR-INT-MOUSE-BUTTON-BLIP CH))))

(DEFMETHOD (STREAM-MIXIN :PLAYBACK) ()
  "Return a circular buffer array describing the last however many
input characters.  The (array-leader array 1) points at the last slot
stored into."
  (IO-BUFFER-RECORD IO-BUFFER))

(DEFMETHOD (STREAM-MIXIN :FORCE-KBD-INPUT) (CH-OR-STRING)
  (COND  ((STRINGP CH-OR-STRING)
	  (DOTIMES (N (LENGTH CH-OR-STRING))
	    (IO-BUFFER-PUT IO-BUFFER (AREF CH-OR-STRING N))))
	 (T (IO-BUFFER-PUT IO-BUFFER CH-OR-STRING))))	;all of the old cases.

(DEFFLAVOR LIST-TYI-MIXIN () ()
  (:DOCUMENTATION :MIXIN "Makes :TYI discard non-keyboard input."))

(DEFFLAVOR ANY-TYI-MIXIN () ()
  (:DOCUMENTATION :MIXIN "Makes :TYI discard non-keyboard input."))

(DEFFLAVOR PREEMPTABLE-READ-ANY-TYI-MIXIN () ())

;;; The RUBOUT-HANDLER-OPTIONS are described in more detail in the
;;; Window System documentation on the :RUBOUT-HANDLER method.
;;; Moved to SYS:WINDOW;COLD since used there first.
;(DEFVAR RUBOUT-HANDLER-OPTIONS NIL
;  "Within rubout handler, the options supplied as first arg to
;   :RUBOUT-HANDLER operation.")
(DEFVAR RUBOUT-HANDLER-STARTING-X :UNBOUND
  "Within rubout handler, X position of beginning of input.")
(DEFVAR RUBOUT-HANDLER-STARTING-Y :UNBOUND
  "Within rubout handler, Y position of beginning of input.")
(DEFVAR RUBOUT-HANDLER-RE-ECHO-FLAG :UNBOUND
  "Within rubout handler, T when there are error messages in the
middle of the input.  Set to NIL when the input is reprinted and they
are gone.")
(DEFVAR RUBOUT-HANDLER-INSIDE NIL
  "Non-NIL while inside the rubout handler.")
(DEFVAR RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL
  "The character or blip that is activating this invocation of
:RUBOUT-HANDLER.")

(DEFVAR PROMPT-STARTING-X :UNBOUND
  "Within rubout handler, X position of beginning of printed prompt string.")
(DEFVAR PROMPT-STARTING-Y :UNBOUND
  "Within rubout handler, Y position of beginning of printed prompt string.")

(sys:declare-suggestions-for '(:method TV:stream-mixin :rubout-handler)
			     :around (sys:make-default-around-form
				       (:method TV:stream-mixin :rubout-handler)
				       recursive-input-editor-top-menu
				       W:lisp-listener-bottom-menu
				       W:basic-menu
				       :predicate
				       (locally (declare (special sys:function-arglist))
						(sys:sugg-rubout-handler-predicate
						  (second sys:function-arglist))))
			     :use-arglist t)

;;; WHen this flag is true begin rescanning when any change is made to the input.
;;; THis is mainly needed after an error message is printed while we are waiting for
;;; the input buffer to change; once it does we need to re-enter the users read function.
;;;  PMH 4/6/88
(defvar rh-rescan-any-change nil)

(DEFMETHOD (STREAM-MIXIN :RUBOUT-HANDLER)
	   (RUBOUT-HANDLER-OPTIONS FUNCTION &REST ARGS)
  (sys:with-suggestions-menus-for (:method tv:stream-mixin :rubout-handler)
  (COND ((> (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
     (COPY-ARRAY-PORTION RUBOUT-HANDLER-BUFFER
			 (RHB-SCAN-POINTER)
			 (RHB-FILL-POINTER)
			 RUBOUT-HANDLER-BUFFER
			 0
			 (ARRAY-TOTAL-SIZE RUBOUT-HANDLER-BUFFER))
     (IF (NUMBERP (RHB-TYPEIN-POINTER))
	 (DECF (RHB-TYPEIN-POINTER) (RHB-SCAN-POINTER)))
     (SETF (RHB-FILL-POINTER) (- (RHB-FILL-POINTER) (RHB-SCAN-POINTER))))
    (T (SETF (RHB-FILL-POINTER) 0)))
  (SETF (RHB-SCAN-POINTER) 0)
  (SETF (RHB-INITIAL-ENTRY) T)
  (CATCH 'RETURN-FROM-RUBOUT-HANDLER
    (MULTIPLE-VALUE-BIND (PROMPT-STARTING-X PROMPT-STARTING-Y)
	(SEND SELF :READ-CURSORPOS)
      (LET ((PROMPT-OPTION (ASSOC :PROMPT RUBOUT-HANDLER-OPTIONS :TEST #'EQ)))
	(AND PROMPT-OPTION                     ;Prompt if desired
	     (RUBOUT-HANDLER-PROMPT (CADR PROMPT-OPTION) SELF NIL)))

      (MULTIPLE-VALUE-BIND
	(RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
	  (SEND SELF :READ-CURSORPOS)
	;; In case we inherit some typein from a recursive call set cursor position
	;; this assumes the cursor is at the typein position PMH 3/24/88
	(when (and rubout-handler (plusp (RHB-fill-POINTER)))
	  (setf RUBOUT-HANDLER-STARTING-X (- RUBOUT-HANDLER-STARTING-X  (* char-width (RHB-fill-POINTER))))
	  (sheet-set-cursorpos self RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
	  )
       ;; Output any "typeahead"
	(AND (PLUSP (RHB-FILL-POINTER))
	     (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER)
	     ;; make sure typein cursor is correctly positioned
	     ;; for recursive calls to rubout handler PMH 3/24/88
	     (when (and rubout-handler
			(NUMBERP (RHB-TYPEIN-POINTER))
			(< 0 (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER)))
	       (sheet-set-cursorpos self
				    (+ RUBOUT-HANDLER-STARTING-X (* char-width (RHB-typein-POINTER)))
				    RUBOUT-HANDLER-STARTING-Y))
	     )
	(DO ((RUBOUT-HANDLER T)                 ;Establish rubout handler
	     (RUBOUT-HANDLER-INSIDE T)
	     (RUBOUT-HANDLER-RE-ECHO-FLAG NIL NIL)
	     (RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL))
	    (NIL)
	  (CATCH 'RUBOUT-HANDLER                           ;Throw here when rubbing out
       	    (CONDITION-CASE-IF
	     (NULL (ASSOC :DONT-HANDLE-ERRORS RUBOUT-HANDLER-OPTIONS :TEST #'EQ)) (ERROR)
			       (RETURN
				(LET (LIST)
				  (SETQ LIST (MULTIPLE-VALUE-LIST
					      ;; Call READ or whatever.
					      (APPLY FUNCTION ARGS)))
				  (SETF (RHB-FILL-POINTER) (RHB-SCAN-POINTER))
				  (AND (RHB-TYPEIN-POINTER)
				       (> (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER))
				       (SETF (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER)))
				  (VALUES-LIST LIST)))
			       (PARSE-ERROR
				  (TERPRI SELF)
				  (PRINC ">>ERROR: " SELF)
				  (SEND ERROR :REPORT SELF)
				  (TERPRI SELF)
				  (SETQ RUBOUT-HANDLER-RE-ECHO-FLAG T)
				  ;; If error, force user to rub out.
				  ;; add rh-rescan-any-change  flag PMH 3/25
				  (DO ((rh-rescan-any-change t))
				      (NIL)
;;;;;				  (SEND SELF :TYI)))))
				    (READ-CHAR self))

				)))
	  ;;Maybe return when user rubs all the way back
	  (AND (ZEROP (RHB-FILL-POINTER))
	       ;; FULL-RUBOUT says we return if there isn't any input.
	       (LET ((FULL-RUBOUT-OPTION
		       (ASSOC :FULL-RUBOUT RUBOUT-HANDLER-OPTIONS :TEST #'EQ)))
		 (WHEN FULL-RUBOUT-OPTION
		  ;; Get rid of the prompt, if any.
		   (SHEET-CLEAR-BETWEEN-CURSORPOSES
		     SELF PROMPT-STARTING-X PROMPT-STARTING-Y
						    (- CURSOR-X LEFT-MARGIN-SIZE)
						    (- CURSOR-Y TOP-MARGIN-SIZE))
		   (SHEET-SET-CURSORPOS SELF
					PROMPT-STARTING-X PROMPT-STARTING-Y)
		   (RETURN NIL (CADR FULL-RUBOUT-OPTION)))))))))))

(DEFMETHOD (STREAM-MIXIN :RESCANNING-P) ()
  (OR (< (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
      RUBOUT-HANDLER-ACTIVATION-CHARACTER))

(DEFMETHOD (STREAM-MIXIN :FORCE-RESCAN) ()
  (SETF (RHB-SCAN-POINTER) 0)
  (THROW 'T
	 'RUBOUT-HANDLER))

(DEFMETHOD (STREAM-MIXIN :READ-BP) ()
  (RHB-SCAN-POINTER))

;(DEFMETHOD (STREAM-MIXIN :REPLACE-INPUT) (N STRING &OPTIONAL (START 0) END)
;  )

;;
;;Modified by Hogge to use keyword :PREEMPTABLE, the originally intended mechanisms,
;;instead of :FULL-RUBOUT and a kludge which encompassed :ANY-TYI and TV:ALTERNATE-RUBOUT-HANDLER.
;;
(DEFMETHOD (STREAM-MIXIN :PREEMPTABLE-READ) (OPTIONS FUN &REST ARGS)
  (DO ((TYPEAHEAD OLD-TYPEAHEAD NIL)
       (RESULT)
       (FLAG))
      (NIL)
    (SETQ OLD-TYPEAHEAD T)
    (UNWIND-PROTECT
	(MULTIPLE-VALUE-SETQ
		     (RESULT FLAG)
		     (APPLY SELF :RUBOUT-HANDLER
			    (APPEND '((:PREEMPTABLE :PREEMPTABLE))               ;Hogge 2-02-85
				    (AND (CONSP TYPEAHEAD)
					 `((:INITIAL-INPUT ,(CAR TYPEAHEAD))
					  (:INITIAL-INPUT-POINTER ,(CADR TYPEAHEAD))))
				    OPTIONS)
			    FUN ARGS))
      (AND (EQ OLD-TYPEAHEAD T)
	   (SETQ OLD-TYPEAHEAD NIL)))
    (AND (NOT (MEMBER FLAG '(:PREEMPTABLE :FULL-RUBOUT) :TEST #'EQ ))             ;Hogge 2-02-85

	 (RETURN (VALUES RESULT FLAG)))                                   ;FORD  9-26-84
    ;; Determine whether a mouse character caused the full-rubout
;;;    (SETQ RESULT (SEND SELF :ANY-TYI-NO-HANG))
    (SETQ RESULT (READ-ANY-NO-HANG self))
    (COND (RESULT
;;;	   (OR (NUMBERP RESULT)
	   (OR (CHARACTERP RESULT)                                                  ;;;check needed  ??
	       (RETURN (VALUES RESULT :MOUSE-CHAR)))
;;;	   (SEND SELF :UNTYI RESULT)))
           (UNREAD-ANY RESULT self)))
    (AND (SETQ FLAG (CADR (ASSOC :FULL-RUBOUT OPTIONS :TEST #'EQ )))
	 (RETURN (VALUES NIL FLAG)))
    ;; Presumably after this point, the user didn't call us with
    ;; :FULL-RUBOUT option, so we should retry. We have to fix up the
    ;; notion of :PROMPT and :REPROMPT first though.
    (LET ((PROMPT (ASSOC :PROMPT OPTIONS :TEST #'EQ )))
      (COND (PROMPT
	 (SETQ OPTIONS (REMOVE PROMPT (THE LIST OPTIONS) :TEST #'EQ))
	 ;; This next condition may be unnecessary, but just in
	 ;; case. --kmp
       	 (COND ((NOT (ASSOC :REPROMPT OPTIONS :TEST #'EQ))
	    ;; make fake reprompt info. our old prompt should
	    ;; still be there --kmp
       	    (PUSH `(:REPROMPT . ,(CDR PROMPT)) OPTIONS))))))))

;;; The following function is an efficiency hack for the compiler.  The idea is
;;; to use member in a manner in which the compiler can generate efficient code.
;;; It is preferable to use this approach when searching through a small list
;;; because the search will be done in microcode instead of macrocode.
(DEFUN MEMBER-CHAR (ITEM LIST)
  (OR (MEMBER ITEM LIST :TEST #'EQ)
      (MEMBER (CHAR-INT ITEM) LIST :TEST #'EQ)))
(PROCLAIM '(INLINE MEMBER-CHAR))

(DEFUN DEFAULT-RUBOUT-HANDLER ()
  "Give a single character, or do rubout processing, throws to
RUBOUT-HANDLER on editing."
  (DECLARE (:SELF-FLAVOR STREAM-MIXIN))
  ;; Mark that old rubout handler is in use.
  (SETF (RHB-TYPEIN-POINTER) NIL)
  (WHEN (EQ (RHB-INITIAL-ENTRY) T)
    ;; No point looking for :initial-input-pointer since this rh can't
    ;; do anything with it.
    (LET ((INITIAL-INPUT (CADR (ASSOC :INITIAL-INPUT RUBOUT-HANDLER-OPTIONS :TEST #'EQ))))
      (WHEN INITIAL-INPUT
	(STRING-NCONC RUBOUT-HANDLER-BUFFER INITIAL-INPUT))))
  (SETF (RHB-INITIAL-ENTRY) NIL)
  (WHEN (= (RHB-SCAN-POINTER) MOST-POSITIVE-FIXNUM)
    (SETF (RHB-SCAN-POINTER) 0)
    (THROW 'RUBOUT-HANDLER
	   T))
  (OR (PROG1 RUBOUT-HANDLER-ACTIVATION-CHARACTER
	(SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL))
      (DO ((RUBOUT-HANDLER NIL)
	   (RUBBED-OUT-SOME NIL)
	   (EDITING-COMMAND (CDR (ASSOC :EDITING-COMMAND RUBOUT-HANDLER-OPTIONS :TEST #'EQ)))
	   (DO-NOT-ECHO     (CDR (ASSOC :DO-NOT-ECHO     RUBOUT-HANDLER-OPTIONS :TEST #'EQ)))
	   (PASS-THROUGH    (CDR (ASSOC :PASS-THROUGH    RUBOUT-HANDLER-OPTIONS :TEST #'EQ)))
	   (COMMAND-HANDLER      (ASSOC :COMMAND         RUBOUT-HANDLER-OPTIONS :TEST #'EQ))
	   (PREEMPTABLE          (ASSOC :PREEMPTABLE     RUBOUT-HANDLER-OPTIONS :TEST #'EQ))
	   (ACTIVATION-HANDLER   (ASSOC :ACTIVATION      RUBOUT-HANDLER-OPTIONS :TEST #'EQ))
	   CH LEN)
	  (NIL)
;;;	(SETQ CH (INT-CHAR (SEND SELF :ANY-TYI)))
	(SETQ CH (READ-ANY self))
	(COND ((AND (CONSP CH) (EQ (CAR CH) 'REDISPLAY-RUBOUT-HANDLER))
	       (SEND SELF :SET-CURSORPOS
		     PROMPT-STARTING-X
		     PROMPT-STARTING-Y)
	       (SEND SELF :CLEAR-EOL)
	       (AND	(SETQ LEN
			      (OR (ASSOC :REPROMPT RUBOUT-HANDLER-OPTIONS :TEST #'EQ)
				  (ASSOC :PROMPT   RUBOUT-HANDLER-OPTIONS :TEST #'EQ)))
			(RUBOUT-HANDLER-PROMPT (CADR LEN) SELF CH))
	       (MULTIPLE-VALUE-SETQ
		 (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
		 (SEND SELF :READ-CURSORPOS))
	       (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER))
	      ((CONSP CH)
	       (WHEN PREEMPTABLE
		 (SETF (RHB-SCAN-POINTER) 0)
		 (THROW 'RETURN-FROM-RUBOUT-HANDLER
			(VALUES CH (CADR PREEMPTABLE)))))
	      ((AND COMMAND-HANDLER
		    (APPLY (CADR COMMAND-HANDLER) CH (CDDR COMMAND-HANDLER)))
	       (SETF (RHB-SCAN-POINTER) 0)
	       (THROW 'RETURN-FROM-RUBOUT-HANDLER
		      (VALUES
			`(:COMMAND ,CH 1)
			:COMMAND)))
	      ;; Don't touch this character, just return it to caller.
	      ((OR (MEMBER-CHAR CH EDITING-COMMAND)
		   (SI:ASSQ-CAREFUL CH EDITING-COMMAND))
	       ;; Cause rubout handler rescan next time the user does :TYI.
	       (IF RUBBED-OUT-SOME
		   (SETF (RHB-SCAN-POINTER) MOST-POSITIVE-FIXNUM))
	       (RETURN CH))
	      ;; Is it an editing character?
	      ((AND
		 (NOT (OR (MEMBER-CHAR CH DO-NOT-ECHO)
			  (AND ACTIVATION-HANDLER
			       (APPLY (CADR ACTIVATION-HANDLER) CH (CDDR ACTIVATION-HANDLER)))))
;;;	    (OR (LDB-TEST %%KBD-CONTROL-META CH)
		 (OR (NOT (ZEROP (CHAR-BITS CH)))
		     (AND (MEMBER CH '(#\RUBOUT #\CLEAR-INPUT #\PAGE #\DELETE) :TEST #'EQ)    ;;WHAT IS THIS DOING?
			  (NOT (MEMBER-CHAR CH PASS-THROUGH)))))
	       (COND ((MEMBER CH '(#\PAGE #\DELETE) :TEST #'EQ)	;Retype buffered input
		      (SEND SELF :TYO CH)	;Echo it
		      (IF (SAFE-CHAR= CH #\PAGE) (SEND SELF :CLEAR-SCREEN)            ;;CHAR=
			  (SEND SELF :TYO #\NEWLINE))
		      (MULTIPLE-VALUE-SETQ (PROMPT-STARTING-X PROMPT-STARTING-Y)
					   (SEND SELF :READ-CURSORPOS))
		      (AND
			(SETQ LEN
			      (OR (ASSOC :REPROMPT RUBOUT-HANDLER-OPTIONS :TEST #'EQ)
				  (ASSOC :PROMPT   RUBOUT-HANDLER-OPTIONS :TEST #'EQ)))
			(RUBOUT-HANDLER-PROMPT (CADR LEN) SELF CH))
		      (MULTIPLE-VALUE-SETQ
			(RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
			(SEND SELF :READ-CURSORPOS))
		      (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER))
		     ((MEMBER CH '(#\RUBOUT #\M-RUBOUT #\CLEAR-INPUT) :TEST #'EQ)
		      ;; Delete some characters.
		      (COND ((NOT (ZEROP (SETQ LEN (RHB-FILL-POINTER))))
			     (SETF (RHB-FILL-POINTER)
				   (SETQ LEN
					 (CASE CH
					       (#\RUBOUT (1- LEN))
					       (#\m-RUBOUT (STRING-BACKWARD-WORD RUBOUT-HANDLER-BUFFER LEN))
					       (#\CLEAR-INPUT 0))))
			     (SETQ RUBBED-OUT-SOME T)
			     (MULTIPLE-VALUE-BIND (X Y)
				 (SEND SELF :COMPUTE-MOTION
				       RUBOUT-HANDLER-BUFFER
				       0
				       LEN
				       RUBOUT-HANDLER-STARTING-X
				       RUBOUT-HANDLER-STARTING-Y)
			       (IF RUBOUT-HANDLER-RE-ECHO-FLAG
				   (SETQ X RUBOUT-HANDLER-STARTING-X
					 Y RUBOUT-HANDLER-STARTING-Y))
			       (MULTIPLE-VALUE-BIND (CX CY)
				   (SEND SELF :READ-CURSORPOS)
				 (SEND SELF :CLEAR-BETWEEN-CURSORPOSES X Y CX CY))
			       (SEND SELF :SET-CURSORPOS X Y)
			       (AND RUBOUT-HANDLER-RE-ECHO-FLAG
				    (SEND SELF :STRING-OUT
					  RUBOUT-HANDLER-BUFFER))))))
		     (T
		      ;; Undefined editing character.
		      (BEEP)))
	       (COND ((AND (ZEROP (RHB-FILL-POINTER))
			   (ASSOC :FULL-RUBOUT RUBOUT-HANDLER-OPTIONS :TEST #'EQ))
		      (SETF (RHB-SCAN-POINTER) 0)
		      (THROW 'RUBOUT-HANDLER T))))
	      (T
	       ;; It's a self-inserting character.
	       ;; If this is first character typed in, re-get starting
	       ;; cursorpos since while waiting for input a notification
	       ;; may have been typed out.
	       (AND (ZEROP (RHB-FILL-POINTER))
		    (MULTIPLE-VALUE-SETQ
		      (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
		      (SEND SELF :READ-CURSORPOS)))
	       (COND ((MEMBER-CHAR CH DO-NOT-ECHO)
		      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER CH))
		     ((AND ACTIVATION-HANDLER
			   (APPLY (CADR ACTIVATION-HANDLER)
				  CH
				  (CDDR ACTIVATION-HANDLER)))
		      (SETQ CH `(:ACTIVATION ,CH 1))
		      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER CH))
		     (T
		      (SEND SELF :TYO CH)                             ;; ch alone?
		      (VECTOR-PUSH-EXTEND CH RUBOUT-HANDLER-BUFFER)))
	       (COND (RUBBED-OUT-SOME
		      (SETF (RHB-SCAN-POINTER) 0)
		      (THROW 'RUBOUT-HANDLER T))
		     (T
		      (SETF (RHB-SCAN-POINTER) (RHB-FILL-POINTER))
		      (SETQ RUBOUT-HANDLER-ACTIVATION-CHARACTER NIL)
		      (RETURN CH))))))))

;;; The following variable exists so that Zmacs can change it to use
;;; Zmac's word syntax table.  For those cases where Zmacs is not loaded
;;; into the system, the value of this variable will not change.
(DEFVAR RUBOUT-HANDLER-ALPHA-CHAR-P #'ALPHA-CHAR-P
  "Function to call within the rubout handler to test for alphabetic characters.")

;; Use ZWEI's syntax table if ZWEI is around...
(DEFUN STRING-BACKWARD-WORD (STRING INDEX)
  (DO ((I (1- INDEX) (1- I))
       (INSIDE-WORD NIL))
      ((MINUSP I)
       0)
    (IF (FUNCALL RUBOUT-HANDLER-ALPHA-CHAR-P (AREF STRING I))
	(SETQ INSIDE-WORD T)
	(AND INSIDE-WORD (RETURN (1+ I))))))

(DEFUN RUBOUT-HANDLER-PROMPT (PROMPT-OPTION STREAM CH)
  (IF (STRINGP PROMPT-OPTION)
      (FUNCALL STREAM :STRING-OUT PROMPT-OPTION)
      (FUNCALL PROMPT-OPTION STREAM CH)))

(DEFMETHOD (STREAM-MIXIN :SAVE-RUBOUT-HANDLER-BUFFER) ()
  (WHEN RUBOUT-HANDLER-INSIDE
    ;; Give rubout handler function a chance to put its internal data
    ;; into RUBOUT-HANDLER-BUFFER where we look for it.
    (LET ((PROP (GET STREAM-MIXIN-RUBOUT-HANDLER
                     'SAVE-RUBOUT-HANDLER-BUFFER)))
      (WHEN PROP (FUNCALL PROP SELF)))
    (VALUES (COPY-SEQ RUBOUT-HANDLER-BUFFER) (RHB-TYPEIN-POINTER))))

(DEFMETHOD (STREAM-MIXIN :RESTORE-RUBOUT-HANDLER-BUFFER)
	   (STRING &OPTIONAL POINTER)
  (LET ((LENGTH (ARRAY-ACTIVE-LENGTH STRING)))
    (OR (>= (ARRAY-TOTAL-SIZE RUBOUT-HANDLER-BUFFER) LENGTH)
	(ADJUST-ARRAY RUBOUT-HANDLER-BUFFER LENGTH))
    (SETF (RHB-FILL-POINTER) LENGTH)
    (REPLACE RUBOUT-HANDLER-BUFFER STRING))
  (SETF (RHB-TYPEIN-POINTER) POINTER)
  (SEND SELF :REFRESH-RUBOUT-HANDLER)
  (SETF (RHB-SCAN-POINTER) 0)
  (SETF (RHB-INITIAL-ENTRY) :RESTORED)
  (THROW 'RUBOUT-HANDLER
	 T))

(DEFMETHOD (STREAM-MIXIN :REFRESH-RUBOUT-HANDLER)
	   (&OPTIONAL DISCARD-LAST-CHARACTER)
  (IF DISCARD-LAST-CHARACTER
      (SETF (RHB-FILL-POINTER) (MAX 0 (1- (RHB-FILL-POINTER)))))
  (IF (RHB-TYPEIN-POINTER)
      (SETF (RHB-TYPEIN-POINTER)
	    (MIN (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER))))
  (SEND SELF :FRESH-LINE)
  (LET ((PROMPT
	 (OR (ASSOC :REPROMPT RUBOUT-HANDLER-OPTIONS :TEST #'EQ)
	     (ASSOC :PROMPT   RUBOUT-HANDLER-OPTIONS :TEST #'EQ))))
    (AND PROMPT (RUBOUT-HANDLER-PROMPT (CADR PROMPT) SELF #\DELETE)))
  (MULTIPLE-VALUE-SETQ (RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
    (SEND SELF :READ-CURSORPOS))
  (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER))

(DEFFLAVOR LINE-TRUNCATING-MIXIN () ()
  (:REQUIRED-FLAVORS STREAM-MIXIN)
  (:DOCUMENTATION :MIXIN "Causes stream output functions to truncate
if the SHEET-TRUNCATE-LINE-OUT-FLAG in the window is set."))

(DEFWRAPPER (LINE-TRUNCATING-MIXIN :TYO) (IGNORE . BODY)
  `(CATCH 'LINE-OVERFLOW
     . ,BODY))

(DEFMETHOD (LINE-TRUNCATING-MIXIN :BEFORE :END-OF-LINE-EXCEPTION) ()
  (OR (ZEROP (SHEET-TRUNCATE-LINE-OUT-FLAG))
      (THROW 'LINE-OVERFLOW T)))

(DEFMETHOD (LINE-TRUNCATING-MIXIN :STRING-OUT)
	   (STRING &OPTIONAL (START 0) END color)
  (OR END (SETQ END (LENGTH STRING)))
  (DO ((I START (1+ CR-IDX))
       (CR-IDX))
      ((>= I END))
    (SETQ CR-IDX
	  (POSITION #\NEWLINE (THE STRING STRING) :TEST #'CHAR= :START I :END END))
    (CATCH 'LINE-OVERFLOW
      (prepare-color (self color)
      (SHEET-STRING-OUT SELF STRING I (OR CR-IDX END))))
    (OR CR-IDX (RETURN NIL))
    (SHEET-CRLF SELF)))

(DEFFLAVOR AUTOEXPOSING-MORE-MIXIN () ()
  (:REQUIRED-FLAVORS WINDOW)
  (:DOCUMENTATION :MIXIN
   "Makes a window expose itself if output on it stops at a **MORE**."))

(DEFMETHOD (AUTOEXPOSING-MORE-MIXIN :BEFORE :MORE-EXCEPTION) ()
  (SEND SELF :EXPOSE))

;;; Stream operations which all streams are required to support or
;;; ignore.  I'm afraid these will appear in the :WHICH-OPERATIONS even
;;; though they aren't "really supported".

;These 3 are ignored since we don't have buffered output
(DEFMETHOD (STREAM-MIXIN :CLEAR-OUTPUT) ()
  NIL)

(DEFMETHOD (STREAM-MIXIN :FORCE-OUTPUT) ()
  NIL)

(DEFMETHOD (STREAM-MIXIN :FINISH) ()
  NIL)

(DEFMETHOD (STREAM-MIXIN :CLOSE) (&OPTIONAL IGNORE)
  NIL)

(DEFMETHOD (STREAM-MIXIN :LINE-IN) (&OPTIONAL LEADER)
  (STREAM-DEFAULT-HANDLER SELF :LINE-IN LEADER NIL))

(DEFMETHOD (STREAM-MIXIN :STRING-IN) (EOF &REST REST)
  (DECLARE (ARGLIST EOF STRING &OPTIONAL START END))
  (STREAM-DEFAULT-HANDLER SELF :STRING-IN EOF REST))

(DEFMETHOD (STREAM-MIXIN :STRING-LINE-IN) (EOF &REST REST)
  (DECLARE (ARGLIST EOF STRING &OPTIONAL START END))
  (STREAM-DEFAULT-HANDLER SELF :STRING-LINE-IN EOF REST))

(DEFFLAVOR GRAPHICS-MIXIN () ()
  ;; Explicit presence of SHEET helps init the
  ;; flavor-unmapped-instance-variables.
  (:REQUIRED-FLAVORS SHEET ESSENTIAL-WINDOW)
  (:DOCUMENTATION :MIXIN
   "Provides graphics output operations for windows."))

;;; this now returns an 8-bit quantity, since the screen is an 8 bit screen

;;; 8/26/1987 modified to return a 1 bit value for compatability with monochrome systems, then an 8 bit value
;;; for use in color systems. This second value is NIL when the window is monochrome.
;;; To get a 0 or 1 for the point in color, a 0 is returned if the screen color == background color, otherwise a 1 is returned

(DEFMETHOD (GRAPHICS-MIXIN :POINT) (X Y)
  (LET ((value nil)
	(reduced nil)
       )
    (SETQ X (+ X (SHEET-INSIDE-LEFT))
	Y (+ Y (SHEET-INSIDE-TOP)))
    (IF  (OR (< X (SHEET-INSIDE-LEFT)) (>= X (SHEET-INSIDE-RIGHT)) (< Y (SHEET-INSIDE-TOP))
            (>= Y (SHEET-INSIDE-BOTTOM)))
     (VALUES 0 nil) ;;; we are off the screen - 0 for compatability, nil for 8 bit system
     ;;; else
     (progn
       (SETQ value (PREPARE-SHEET (SELF) (AREF SCREEN-ARRAY Y X))) ;;; grab the screen contents
       (IF (= 8 (screen-bits-per-pixel default-screen))
	   (SETQ reduced (IF (= value (sheet-background-color self))
			     0
			     1
			  ))
	   ;;; else its a 1 bit/pixel system, so we just return the value read from the array, and nil for the color value
	   (SETQ reduced value
		 value nil)
       )
       (VALUES reduced value)
     )
    )
  )
)

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-POINT) (X Y &OPTIONAL (ALU CHAR-ALUF)
					 (color (sheet-foreground-color self)))
  ;;; -1 was the old optional value before color, preserve it for b&w operations.
  ;;; NOTE: new ALUs were added for color, but are not understood by the BOOLE function, so we
  ;;; have to handle them properly.
  ;;; This looks slow compared to the old, but its actually quite fast...
    (SETQ X (+ X (SHEET-INSIDE-LEFT)) Y (+ Y (SHEET-INSIDE-TOP)))
    (UNLESS (OR (< X (SHEET-INSIDE-LEFT)) (>= X (SHEET-INSIDE-RIGHT))
		(< Y (SHEET-INSIDE-TOP)) (>= Y (SHEET-INSIDE-BOTTOM)))
        (IF (mac-window-p self)
	   (with-clipping-rectangle (0 0 width height)
	     (send-draw-point x y alu (IF (color-system-p self) color
					(IF (ZEROP color) w:white w:black)) self))
	   ;; else
	  (PREPARE-SHEET (SELF)
	    (LET ((current (AREF screen-array y x))
		  (the-color-map (sheet-color-map self))
		  (background (sheet-background-color self)))
	      (unless (color-system-p self)
		(cond						; have to translate to monochrome equivalents
		  ((= alu alu-back) (setf alu alu-andca))
		  ((= alu alu-transp) (setf alu alu-ior))
		  ((= alu alu-add) (setf alu alu-xor))
		  ((= alu alu-sub) (setf alu alu-xor))
		  ((= alu alu-max) (setf alu alu-ior))
		  ((= alu alu-min) (setf alu alu-and))
		  ((= alu alu-avg) (setf alu alu-ior))
		  ((= alu alu-adds) (setf alu alu-ior))
		  ((= alu alu-subc) (setf alu alu-andca))
		  )
		)
	      (COND
		((= alu alu-back)  (SETF (AREF screen-array y x) background))
		((= alu alu-transp)  (SETF (AREF screen-array y x) color))
		((= alu alu-add)   (SETF (AREF screen-array y x) (mod (+ color current) 256)))
		((= alu alu-sub)   (SETF (AREF screen-array y x) (mod (- current color) 256)))
		((= alu alu-max)   (SETF (AREF screen-array y x) (max color current)))
		((= alu alu-min)   (SETF (AREF screen-array y x) (min color current)))
		((= alu alu-avg)   (SETF (AREF screen-array y x) (truncate (+ color current) 2)))
		((= alu alu-adds)  (SETF (AREF screen-array y x) (min (+ color current)
								      (color-map-saturate the-color-map))))
		((= alu alu-subc)  (SETF (AREF screen-array y x) (max (- current color)
								      (color-map-clamp the-color-map))))
		(t    	              (setf (aref screen-array y x) (boole alu color current)))
		))))))



;;; The following function returns a mask which indicates where the (X,Y)
;;; point is located relative to the edges of the window.  The following
;;; diagram shows the binary mask value returned if the (X,Y) point were
;;; located where the number is located.  For example, if the (X,Y)
;;; point were located to the left and down from the window then the
;;; mask returned would be 1001.
;;;
;;;    0101                          0110
;;;                  0100
;;;
;;;           .-----------------.
;;;           |                 |
;;;           |                 |
;;;           |                 |
;;;    0001   |      0000       |    0010
;;;           |                 |
;;;           |                 |
;;;           |                 |
;;;           `-----------------'
;;;
;;;                  1000
;;;    1001                          1010
;;;
(DEFUN DRAW-LINE-CLIP-VISIBILITY (POINT-X POINT-Y &AUX VISIBILITY)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (SETQ VISIBILITY (COND ((<  POINT-X (SHEET-INSIDE-LEFT)) 1)
			 ((>= POINT-X (SHEET-INSIDE-RIGHT)) 2)
			 (T 0)))
  (COND ((<  POINT-Y (SHEET-INSIDE-TOP))    (LOGIOR 4 VISIBILITY))
	((>= POINT-Y (SHEET-INSIDE-BOTTOM)) (LOGIOR 8 VISIBILITY))
	(T VISIBILITY)))


(DEFMETHOD (GRAPHICS-MIXIN :DRAW-LINE)
	   (FROM-X FROM-Y TO-X TO-Y
	    &OPTIONAL (ALU CHAR-ALUF) (DRAW-END-POINT T)
	    (color (IF (color-system-p self)
		       (sheet-foreground-color self) -1)))
  (SETQ FROM-X (+ FROM-X (SHEET-INSIDE-LEFT))
	FROM-Y (+ FROM-Y (SHEET-INSIDE-TOP))
	TO-X (+ TO-X (SHEET-INSIDE-LEFT))
	TO-Y (+ TO-Y (SHEET-INSIDE-TOP)))
  (IF (mac-window-p self)
      (with-clipping-rectangle (0 0 width height)
        (send-draw-line from-x from-y to-x to-y alu draw-end-point self t 1
			(IF (color-system-p self) color
			  (IF (ZEROP color) w:white w:black)))
	(UNLESS draw-end-point			; Turn off end point.
	  (send-draw-point to-x to-y w:alu-xor w:black self)))
    ;; else...
      (DO ((FROM-VISIBILITY (DRAW-LINE-CLIP-VISIBILITY FROM-X FROM-Y)
			    (DRAW-LINE-CLIP-VISIBILITY FROM-X FROM-Y))
	   (TO-VISIBILITY (DRAW-LINE-CLIP-VISIBILITY TO-X TO-Y))
	   (EXCHANGED NIL))
	  ;;When completely visible, draw the line
	  ((AND (ZEROP FROM-VISIBILITY) (ZEROP TO-VISIBILITY))
	   (AND EXCHANGED (PSETQ FROM-X TO-X TO-X FROM-X FROM-Y TO-Y TO-Y FROM-Y))
	   (prepare-color (self color)
			  (PREPARE-SHEET (SELF)
			    (SYS:%DRAW-SHADED-TRIANGLE FROM-X FROM-Y TO-X TO-Y TO-X TO-Y ALU T DRAW-END-POINT T NIL SELF)
			    )
			  )
	   )
	;; If all off the screen, dont draw anything.
	(OR (ZEROP (LOGAND FROM-VISIBILITY TO-VISIBILITY)) (RETURN NIL))
	;; Exchange points to try to make to point visible.
	(AND (ZEROP FROM-VISIBILITY)
	     (PSETQ FROM-X TO-X TO-X FROM-X FROM-Y TO-Y TO-Y FROM-Y
		    FROM-VISIBILITY TO-VISIBILITY TO-VISIBILITY FROM-VISIBILITY
		    EXCHANGED (NOT EXCHANGED)))
	;; If TO-X = FROM-X then FROM-VISIBILITY = 0, 4 or 8 so there is no
	;; danger of divide by zero in the next "Push".
	(COND ((LDB-TEST (BYTE 1 0) FROM-VISIBILITY)	;Push toward left edge
	       (SETQ FROM-Y	(+ FROM-Y
				   (TRUNCATE (* (- TO-Y FROM-Y)
						(- (SHEET-INSIDE-LEFT) FROM-X))
					     (- TO-X FROM-X)))
		     FROM-X (SHEET-INSIDE-LEFT)))
	      ((LDB-TEST (BYTE 1 1) FROM-VISIBILITY)	;Push toward right edge
	       (SETQ FROM-Y	(+ FROM-Y
				   (TRUNCATE (* (- TO-Y FROM-Y)
						(- (SHEET-INSIDE-RIGHT) FROM-X 1))
					     (- TO-X FROM-X)))
		     FROM-X (1- (SHEET-INSIDE-RIGHT)))))
	(COND ((LDB-TEST (BYTE 1 2) FROM-VISIBILITY)	;Push toward top
	       ;; It is possible that TO-Y = FROM-Y at this point because of
	       ;; the effects of the last "Push", but in that case TO-X is
	       ;; probably equal to FROM-X as well (or at least close to it)
	       ;; so we needn't draw anything:
	       (AND (= TO-Y FROM-Y) (RETURN NIL))
	       (SETQ FROM-X (+ FROM-X (TRUNCATE (* (- TO-X FROM-X)
						   (- (SHEET-INSIDE-TOP) FROM-Y))
						(- TO-Y FROM-Y)))
		     FROM-Y (SHEET-INSIDE-TOP)))
	      ((LDB-TEST (BYTE 1 3) FROM-VISIBILITY)	;Push toward bottom
	       ;; Same:
	       (AND (= TO-Y FROM-Y) (RETURN NIL))
	       (SETQ FROM-X (+ FROM-X
			       (TRUNCATE (* (- TO-X FROM-X)
					    (- (SHEET-INSIDE-BOTTOM) FROM-Y 1))
					 (- TO-Y FROM-Y)))
		     FROM-Y (1- (SHEET-INSIDE-BOTTOM))))))))

;;; This never draws any end points, thus it is good for making closed
;;; polygons.  Calls the :DRAW-LINE method to do the clipping.
;;; Hack alert: note that we will allow a color argument in a "keyword-like"
;;; syntax; lets just say the :color keyword argument must follow the X and Y pairs.
(DEFMETHOD (GRAPHICS-MIXIN :DRAW-LINES) (ALU X1 Y1 &REST END-XS-AND-YS
					 &aux (color (sheet-foreground-color self)))
  (when (member :color END-XS-AND-YS)
    (setf color (second (member :color END-XS-AND-YS)))
    ;;cant modify the stack list structure  PMH
    (setq  END-XS-AND-YS (copy-list END-XS-AND-YS))
    (remf END-XS-AND-YS :color))
  (DO ((X2)
       (Y2)
       (METH (GET-HANDLER-FOR SELF :DRAW-LINE)))
      ((NULL END-XS-AND-YS))
    (SETQ X2 (CAR END-XS-AND-YS)
	  Y2 (CADR END-XS-AND-YS)
	  END-XS-AND-YS (CDDR END-XS-AND-YS))
    (FUNCALL METH NIL X1 Y1 X2 Y2 ALU NIL color)
    (SETQ X1 X2
	  Y1 Y2)))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-DASHED-LINE)
	   (X0 Y0 X1 Y1 &OPTIONAL (ALU CHAR-ALUF)
	    (DASH-SPACING 20) SPACE-LITERALLY-P (OFFSET 0)
            (DASH-LENGTH (FLOOR DASH-SPACING 2)) (color (sheet-foreground-color self)))
  (LET (N-DASHES DISTANCE
	(REAL-DASH-SPACING DASH-SPACING)
	(REAL-DASH-LENGTH DASH-LENGTH)
	(METH (GET-HANDLER-FOR SELF :DRAW-LINE)))
    (SETQ DISTANCE (SQRT (SMALL-FLOAT (+ (EXPT (- X1 X0) 2) (EXPT (- Y1 Y0) 2)))))
    (IF (ZEROP DISTANCE)
	;; Take care of the case where the two (x,y) points specify the
	;; same point.
       (SEND SELF :DRAW-POINT X0 Y0 ALU color)
       (PROGN
	  (IF SPACE-LITERALLY-P
	      ;; Get number of dashes of specified size that will fit.
       	      (SETQ N-DASHES (FLOOR (+ DISTANCE (- DASH-SPACING DASH-LENGTH)) DASH-SPACING))
	      ;; Get approximate number of dashes that will fit,
	      ;; then change spacing to make them fit exactly.
              (PROGN
	        (SETQ N-DASHES (ROUND (+ DISTANCE (- DASH-SPACING DASH-LENGTH)) DASH-SPACING))
	        (IF (= N-DASHES 1)
		    (SETQ REAL-DASH-SPACING DISTANCE REAL-DASH-LENGTH (- DISTANCE OFFSET OFFSET))
		    (SETQ REAL-DASH-SPACING
			  (QUOTIENT (- DISTANCE OFFSET OFFSET DASH-LENGTH) (1- N-DASHES))))))
	  (LET ((X (+ X0 (* OFFSET (QUOTIENT (- X1 X0) DISTANCE))))
		(Y (+ Y0 (* OFFSET (QUOTIENT (- Y1 Y0) DISTANCE))))
		(DX (* REAL-DASH-LENGTH (QUOTIENT (- X1 X0) DISTANCE)))
		(DY (* REAL-DASH-LENGTH (QUOTIENT (- Y1 Y0) DISTANCE)))
		(DX2 (* REAL-DASH-SPACING (QUOTIENT (- X1 X0) DISTANCE)))
		(DY2 (* REAL-DASH-SPACING (QUOTIENT (- Y1 Y0) DISTANCE))))
	    (DOTIMES (I N-DASHES)
	      (FUNCALL METH :DRAW-LINE (VALUES (ROUND X)) (VALUES (ROUND Y))
		       (VALUES (ROUND (+ X DX))) (VALUES (ROUND (+ Y DY))) ALU
		       (< (1+ I) N-DASHES) color)
	      (INCF X DX2)
	      (INCF Y DY2))))))
)

;This clips in microcode
;;;patched on 17 Dec 85 for GRH by GSM
(DEFMETHOD (GRAPHICS-MIXIN :DRAW-TRIANGLE)
           (X1 Y1 X2 Y2 X3 Y3 &OPTIONAL (ALU CHAR-ALUF) (COLOR (sheet-foreground-color self)))
 (prepare-color (self color)
   (PREPARE-SHEET (SELF)
     (with-clipping-rectangle ((sheet-inside-left) (sheet-inside-top) (sheet-inside-right) (sheet-inside-bottom))
	(%DRAW-SHADED-TRIANGLE (+ X1 (sheet-inside-left)) (+ Y1 (sheet-inside-top))
			       (+ X2 (sheet-inside-left)) (+ Y2 (sheet-inside-top))
			       (+ X3 (sheet-inside-left)) (+ Y3 (sheet-inside-top))
			       ALU t t t 100%-black self
        )
     )
   )
 )
)

;;; Very special kludgey macro for :DRAW-CIRCLE. In color, its even kludeier- do we really need this, i.e. why not
;;;                                                      use :draw-point method?? Speed?

;;; added color argument
;;; fixed to comprehend color alu operations

;;; note - this macro is always called from within a prepare-sheet

(DEFMACRO DRAW-CLIPPED-POINT (X-FORM Y-FORM color-val &optional (alu-val (IF (color-system-p self) alu-transp alu-ior)))
  `(PROGN
     (SETQ X-VAL ,X-FORM
	   Y-VAL ,Y-FORM)
     (OR (< X-VAL IL) (>= X-VAL IR)
	 (< Y-VAL IT) (>= Y-VAL IB)
       (LET ((current (AREF screen-array y-val x-val))
	     (the-color-map (sheet-color-map self))
	     (background (sheet-background-color self))
	     (value ,color-val)
	     (malu ,alu-val))
          (unless (color-system-p self)
            (cond
              ((= malu alu-back) (setf malu alu-andca))
              ((= malu alu-transp) (setf malu alu-ior))
              ((= malu alu-add) (setf malu alu-xor))
              ((= malu alu-sub) (setf malu alu-xor))
              ((= malu alu-max) (setf malu alu-ior))
              ((= malu alu-min) (setf malu alu-and))
              ((= malu alu-avg) (setf malu alu-ior))
              ((= malu alu-adds) (setf malu alu-ior))
              ((= malu alu-subc) (setf malu alu-andca))
            )
          )
          (COND
	    ((= malu alu-back)  (SETF (AREF screen-array y-val x-val) background))
	    ((= malu alu-transp) (SETF (AREF screen-array y-val x-val) value)) ;; may 01-09-89
	    ((= malu alu-add)   (SETF (AREF screen-array y-val x-val) (mod (+ value current) 256)))
	    ((= malu alu-sub)   (SETF (AREF screen-array y-val x-val) (mod (- current value) 256)))
	    ((= malu alu-max)   (SETF (AREF screen-array y-val x-val) (max value current)))
	    ((= malu alu-min)   (SETF (AREF screen-array y-val x-val) (min value current)))
	    ((= malu alu-avg)   (SETF (AREF screen-array y-val x-val) (truncate (+ value current) 2)))
            ((= malu alu-adds)  (SETF (AREF screen-array y-val x-val) (min (+ value current)
									   (color-map-saturate the-color-map))))
            ((= malu alu-subc)  (SETF (AREF screen-array y-val x-val) (max (- current value)
									   (color-map-clamp the-color-map))))
            (t    	              (setf (aref screen-array y-val x-val) (boole malu value current)))
          )))))


(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CIRCLE)
	   (CENTER-X CENTER-Y RADIUS &OPTIONAL (ALU CHAR-ALUF)
	   (color (if (color-system-p self)(sheet-foreground-color self) 1)))
  (LET* ((IL (SHEET-INSIDE-LEFT))
	 (IT (SHEET-INSIDE-TOP))
	 (IR (SHEET-INSIDE-RIGHT))
	 (IB (SHEET-INSIDE-BOTTOM))
	 (CENTER-X (+ CENTER-X IL))
	 (CENTER-Y (+ CENTER-Y IT)))
    (IF (mac-window-p self)
      (with-clipping-rectangle (0 0 width height)
	(send-DrawHollowCircle center-x center-y radius 1
			       (IF (color-system-p self) color
				 (IF (ZEROP color) w:white w:black))
			       alu 29 self))
      ;; else...
      (PREPARE-SHEET (SELF)
	(DO ((Y 0)
	     (X-VAL) (Y-VAL)
	     (F 0)			      	; F is just Y squared without any multiplies
	     (X RADIUS))
	    (NIL)
	  (DRAW-CLIPPED-POINT (+ CENTER-X X) (- CENTER-Y Y) color alu)
	  (DRAW-CLIPPED-POINT (- CENTER-X X) (+ CENTER-Y Y) color alu)
	  (DRAW-CLIPPED-POINT (+ CENTER-X Y) (+ CENTER-Y X) color alu)
	  (DRAW-CLIPPED-POINT (- CENTER-X Y) (- CENTER-Y X) color alu)
	  (SETQ F (+ F Y Y 1) Y (1+ Y))
	  (COND ((>= F X) (SETQ F (- F X X -1) X (- X 1))))
	  (COND ((> Y X) (RETURN)))
	  (DRAW-CLIPPED-POINT (+ CENTER-X X) (+ CENTER-Y Y) color alu)
	  (DRAW-CLIPPED-POINT (- CENTER-X X) (- CENTER-Y Y) color alu)
	  (DRAW-CLIPPED-POINT (+ CENTER-X Y) (- CENTER-Y X) color alu)
	  (DRAW-CLIPPED-POINT (- CENTER-X Y) (+ CENTER-Y X) color alu)
	  (COND ((= Y X) (RETURN))))))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-FILLED-IN-CIRCLE)
	   (CENTER-X CENTER-Y RADIUS &OPTIONAL (ALU CHAR-ALUF) (color (sheet-foreground-color self)))
;;; note - no texture can be provided since it calls %draw-rectangle, which takes no source array

  (IF (mac-window-p self)
      (LET ((center-x (+ center-x (sheet-inside-left self)))
	   (center-y (+ center-y (sheet-inside-top self))))
        (with-clipping-rectangle (0 0 width height)
	 (send-DrawCircle center-x center-y radius
			  (IF (color-system-p self) color
				 (IF (ZEROP color) w:white w:black))
			  alu 29 t self)))
    ;; else...
    (prepare-color (self color)
      (PREPARE-SHEET (SELF)

	(DO ((X 0)
	     (F 0)						; F is just x^2. Don't use multiplication!
	     (Y RADIUS))
	    ((> X Y))
	  (UNLESS (= X Y)
	    (DRAW-RECTANGLE-INSIDE-CLIPPED (+ Y Y 1) 1
					   (- CENTER-X Y)
					   (+ CENTER-Y X)
					   ALU SELF)
	    (UNLESS (ZEROP X)
	      (DRAW-RECTANGLE-INSIDE-CLIPPED (+ Y Y 1) 1
					     (- CENTER-X Y)
					     (- CENTER-Y X)
					     ALU SELF)))
	  (SETQ F (+ F X X 1) X (1+ X))
	  (WHEN (>= F Y)
	    (SETQ F (- F Y Y -1) Y (- Y 1))
	    (DRAW-RECTANGLE-INSIDE-CLIPPED (+ X X -1) 1
					   (- CENTER-X X -1)
					   (+ CENTER-Y Y 1)
					   ALU SELF)
	    (DRAW-RECTANGLE-INSIDE-CLIPPED (+ X X -1) 1
					   (- CENTER-X X -1)
					   (- CENTER-Y Y 1)
					   ALU SELF))))
      ))							; end prepare-color
)

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CIRCLE-OCTANT-ARC)
	   (CENTER-X CENTER-Y RADIUS
	    &OPTIONAL (ALU CHAR-ALUF)
	    RIGHT-UP-START
	    RIGHT-UP-END
	    TOP-RIGHT-START
            TOP-RIGHT-END
	    TOP-LEFT-START
	    TOP-LEFT-END
	    LEFT-UP-START
	    LEFT-UP-END
	    LEFT-DOWN-START
            LEFT-DOWN-END
	    BOTTOM-LEFT-START
	    BOTTOM-LEFT-END
	    BOTTOM-RIGHT-START
	    BOTTOM-RIGHT-END
            RIGHT-DOWN-START
	    RIGHT-DOWN-END
	    )
  "Draw a portion of each octant of a circle.  There is one pair of a
-START and a -END argument for each octant, which controls the
portion of that octant which is actually drawn."

  ;;; hack alert- the function that called this function must make sure that the desired color is set on the
  ;;; window foreground-colore instance variable. Otherwise, we have no way to know what color to draw.
  ;;; Add a parameter, you say - look at that list already! - adding color at the end is a nightmare....

  (LET* ((color (if (color-system-p self) (sheet-foreground-color self) 1))
         (IL (SHEET-INSIDE-LEFT))
	 (IT (SHEET-INSIDE-TOP))
	 (IR (SHEET-INSIDE-RIGHT))
	 (IB (SHEET-INSIDE-BOTTOM))
	 (MAX-END (MAX RIGHT-UP-END
		       TOP-LEFT-END
		       LEFT-DOWN-END
		       BOTTOM-RIGHT-END
	               (- (/ 3.1416s0 4)
		          (MIN RIGHT-DOWN-END BOTTOM-LEFT-END
			       LEFT-UP-END TOP-RIGHT-END))))
	 (CENTER-X (+ CENTER-X IL))
	 (CENTER-Y (+ CENTER-Y IT)))
    (IF (NOT (ZEROP RADIUS))
	(PREPARE-SHEET (SELF)
		       (DO ((Y 0)
			    (X-VAL)
			    (Y-VAL)
			    ANGLE
			    (F 0)                         ; F is just R squared without any multiplies
       			    (X RADIUS))
			   (NIL)
			 (SETQ ANGLE (ATAN (SMALL-FLOAT Y) (SMALL-FLOAT X)))
			 ;; Octants counter clockwise from an axis
       			 (IF (AND (< ANGLE RIGHT-UP-END) (>= ANGLE RIGHT-UP-START))
			     (DRAW-CLIPPED-POINT (+ CENTER-X X) (- CENTER-Y Y) color alu))
			 (IF (AND (< ANGLE LEFT-DOWN-END) (>= ANGLE LEFT-DOWN-START))
			     (DRAW-CLIPPED-POINT (- CENTER-X X) (+ CENTER-Y Y) color alu))
			 (IF (AND (< ANGLE BOTTOM-RIGHT-END) (>= ANGLE BOTTOM-RIGHT-START))
			     (DRAW-CLIPPED-POINT (+ CENTER-X Y) (+ CENTER-Y X) color alu))
			 (IF (AND (< ANGLE TOP-LEFT-END) (>= ANGLE TOP-LEFT-START))
			     (DRAW-CLIPPED-POINT (- CENTER-X Y) (- CENTER-Y X) color alu))
			 (IF (> ANGLE MAX-END) (RETURN))
			 (SETQ F (+ F Y Y 1) Y (1+ Y))
			 (COND ((>= F X) (SETQ F (- F X X -1) X (- X 1))))
			 (COND ((> Y X) (RETURN)))
			 ;; Clockwise
       			 (SETQ ANGLE (- (/ 3.1416s0 4) ANGLE))
			 (IF (AND (< ANGLE RIGHT-DOWN-END) (>= ANGLE RIGHT-DOWN-START))
			     (DRAW-CLIPPED-POINT (+ CENTER-X X) (+ CENTER-Y Y) color alu))
			 (IF (AND (< ANGLE LEFT-UP-END) (>= ANGLE LEFT-UP-START))
			     (DRAW-CLIPPED-POINT (- CENTER-X X) (- CENTER-Y Y) color alu))
			 (IF (AND (< ANGLE TOP-RIGHT-END) (>= ANGLE TOP-RIGHT-START))
			     (DRAW-CLIPPED-POINT (+ CENTER-X Y) (- CENTER-Y X) color alu))
			 (IF (AND (< ANGLE BOTTOM-LEFT-END) (>= ANGLE BOTTOM-LEFT-START))
			     (DRAW-CLIPPED-POINT (- CENTER-X Y) (+ CENTER-Y X) color alu))
			 (COND ((= Y X) (RETURN))))))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CIRCULAR-ARC)
	   (CENTER-X CENTER-Y RADIUS START-THETA END-THETA
	    &OPTIONAL (ALU CHAR-ALUF)
	    (color (if (color-system-p self)(sheet-foreground-color self) 1))
	    &AUX (PI/4 (/ 3.1416s0 4)))
  (LET* ( (save-fore (sheet-foreground-color self))
          (START-OCTANT
	    (MULTIPLE-VALUE-BIND (NIL TEM)
	            (FLOOR (VALUES (FLOOR (FLOOR START-THETA PI/4))) 8)
	           TEM))
	 (END-OCTANT
	  (MULTIPLE-VALUE-BIND (NIL TEM)
	      (FLOOR (VALUES (FLOOR (FLOOR END-THETA PI/4))) 8)
	    TEM))
	 N-OCTANTS)
    ;;; hack alert - put the desired color on the sheet, for communication wioth the octant drawing function

    (SETF (sheet-foreground-color self) color)

    (IF (= START-OCTANT END-OCTANT)
	(SETQ N-OCTANTS
	      (IF (<= START-THETA END-THETA (+ START-THETA PI/4)) 1 9))
	(MULTIPLE-VALUE-BIND (NIL TEM)
	    (FLOOR (- END-OCTANT START-OCTANT) 8)
	  (SETQ N-OCTANTS (1+ TEM))))
    ;; STARTS-AND-ENDS is a list of two angles for each octant,
    ;; a start and an end.  Both are between 0 and pi/4.
    (DO ((STARTS-AND-ENDS (MAKE-LIST 16 :INITIAL-VALUE 0))
	 (OCTANT*2 (* START-OCTANT 2) (REM (+ 2 OCTANT*2) 16))
	 (COUNT 0 (1+ COUNT)))
	((= COUNT N-OCTANTS)
	 (LEXPR-SEND SELF :DRAW-CIRCLE-OCTANT-ARC CENTER-X CENTER-Y RADIUS ALU STARTS-AND-ENDS))
      (WHEN (= COUNT 8)
       ;; if the arc is more than 7/4 pi and ends in same quadrant it
       ;; starts in...  Output all but the tail that ends in that octant,
	(LEXPR-SEND SELF :DRAW-CIRCLE-OCTANT-ARC CENTER-X CENTER-Y RADIUS ALU STARTS-AND-ENDS)
	;; Then zero the starts and ends so we can output that one last piece.
	(DO ((X STARTS-AND-ENDS (CDR X)))
	    ((NULL X))
	  (RPLACA X 0)))
      (SETF (NTH OCTANT*2 STARTS-AND-ENDS)
	    (IF (ZEROP COUNT)
		(MULTIPLE-VALUE-BIND (NIL TEM)
		    (FLOOR START-THETA PI/4)
				TEM)
		0))
      (SETF (NTH (1+ OCTANT*2) STARTS-AND-ENDS)
	    (IF (= (1+ COUNT) N-OCTANTS)
		(MULTIPLE-VALUE-BIND (NIL TEM)
		    (FLOOR END-THETA PI/4)
		  TEM)
		PI/4)))
    (setf (sheet-foreground-color self) save-fore)))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-FILLED-IN-SECTOR) (CENTER-X CENTER-Y RADIUS THETA-1 THETA-2 &OPTIONAL (ALU CHAR-ALUF)
		   (color (sheet-foreground-color self)))
  (PREPARE-SHEET (SELF)
		 (DO ((Y (- RADIUS) (1+ Y))
		      (X 0)
		      (U0 0)
		      (U1 0)                           ;Clipped plane 1
       		      (V0 0)
		      (V1 0)                           ;Clipped plane 2
      		      (CO-X0 (VALUES (FLOOR (* -1000.0 (SIN THETA-1)))))
		      (CO-Y0 (VALUES (FLOOR (* 1000.0 (COS THETA-1)))))
		      (CO-X1 (VALUES (FLOOR (* -1000.0 (SIN THETA-2)))))
		      (CO-Y1 (VALUES (FLOOR (* 1000.0 (COS THETA-2)))))
		      (FLAG (> (ABS (- THETA-1 THETA-2)) 3.14159))
		      (R2 (* RADIUS RADIUS)))
		     ((> Y RADIUS))

		   (SETQ X (ISQRT (- R2 (* Y Y))))      ;Unclipped line
       		   (SETQ U0 (- X) U1 X V0 (- X) V1 X)   ;Init clipped lines

		   (AND (PLUSP (- (* CO-Y0 Y) (* CO-X0 U1)))         ;Clip with first plane
       			(SETQ U1 (IF (= 0 CO-X0) 0 (TRUNCATE (* CO-Y0 Y) CO-X0))))
		   (AND (PLUSP (- (* CO-Y0 Y) (* CO-X0 U0)))
			(SETQ U0 (IF (= 0 CO-X0) 0 (TRUNCATE (* CO-Y0 Y) CO-X0))))

		   (AND (MINUSP (- (* CO-Y1 Y) (* CO-X1 V1)))        ;Clip with second plane
       			(SETQ V1 (IF (= 0 CO-X1) 0 (TRUNCATE (* CO-Y1 Y) CO-X1))))
		   (AND (MINUSP (- (* CO-Y1 Y) (* CO-X1 V0)))
			(SETQ V0 (IF (= 0 CO-X1) 0 (TRUNCATE (* CO-Y1 Y) CO-X1))))

		   ;; Ok, we have two lines, [U0 U1] and [V0 V1].
		   ;; If the angle was greater than pi, then draw both of them,
		   ;; otherwise draw their intersection

		   (COND (FLAG
			  (AND (> U1 U0)
			       (SEND SELF :DRAW-LINE (+ CENTER-X U0) (+ CENTER-Y Y) (+ CENTER-X U1)
				     (+ CENTER-Y Y) ALU T color))
			  (AND (> V1 V0)
			       (SEND SELF :DRAW-LINE (+ CENTER-X V0) (+ CENTER-Y Y) (+ CENTER-X V1)
				     (+ CENTER-Y Y) ALU T color)))
			 (T			;Compute intersection
			  (LET ((LEFT (MAX U0 V0))
				(RIGHT (MIN U1 V1)))
			    (AND (> RIGHT LEFT)
				 (SEND SELF :DRAW-LINE (+ CENTER-X LEFT) (+ CENTER-Y Y)
				       (+ CENTER-X RIGHT) (+ CENTER-Y Y) ALU T color))))))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-REGULAR-POLYGON) (X1 Y1 X2 Y2 N &OPTIONAL (ALU CHAR-ALUF)
		(color (sheet-foreground-color self))  &AUX THETA)
  "Given an edge and a number of sides, draw something.
The sign of N determines which side of the line the figure is drawn on.
If the line is horizontal, the rest of the polygon is in the positive
direction when N is positive."
  (IF (NOT (ZEROP N))
      (PROGN
	(SETQ THETA (* 3.14159 (1- (/ 2.0 N))) N (ABS N))
	(prepare-color (self color)
	(PREPARE-SHEET (SELF)

		       (DO ((I 2 (1+ I))
			    (SIN-THETA (SIN THETA))
			    (COS-THETA (COS THETA))
			    (X0 X1)
			    (Y0 Y1)
			    (X3)
			    (Y3))
			   ((>= I N))
			 (SETQ X3
			       (+
				(- (- (* X1 COS-THETA) (* Y1 SIN-THETA)) (* X2 (1- COS-THETA)))
				(* Y2 SIN-THETA))
			       Y3
			       (- (- (+ (* X1 SIN-THETA) (* Y1 COS-THETA)) (* X2 SIN-THETA))
				  (* Y2 (1- COS-THETA))))
			 (with-clipping-rectangle
			   ((sheet-inside-left) (sheet-inside-top) (sheet-inside-right) (sheet-inside-bottom))
			   (system:%DRAW-shaded-TRIANGLE (+ (SHEET-INSIDE-LEFT) (VALUES (FLOOR X0)))
							 (+ (SHEET-INSIDE-TOP) (VALUES (FLOOR Y0)))
							 (+ (SHEET-INSIDE-LEFT) (VALUES (FLOOR X2)))
							 (+ (SHEET-INSIDE-TOP) (VALUES (FLOOR Y2)))
							 (+ (SHEET-INSIDE-LEFT) (VALUES (FLOOR X3)))
							 (+ (SHEET-INSIDE-TOP) (VALUES (FLOOR Y3))) ALU
							 t t t nil self))
			 (SETQ X1 X2 Y1 Y2 X2 X3 Y2 Y3)))
                         ))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CURVE) (PX PY &OPTIONAL END (ALU CHAR-ALUF) CLOSED-CURVE-P
					 (color (sheet-foreground-color self)))
  "Display vectors of points."

  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH PX)))
  (LET ((X0)
	(X1 (VALUES (FLOOR (AREF PX 0))))
	(Y0)
	(Y1 (VALUES (FLOOR (AREF PY 0))))
	(METH (GET-HANDLER-FOR SELF :DRAW-LINE)))
    (DO ((I 1 (1+ I)))
	((>= I END))
      (SETQ X0 X1)
      (OR (SETQ X1 (AREF PX I)) (RETURN NIL))
      (SETQ X1 (VALUES (FLOOR X1)))
      (SETQ Y0 Y1)
      (OR (SETQ Y1 (AREF PY I)) (RETURN NIL))
      (SETQ Y1 (VALUES (FLOOR Y1)))
      (FUNCALL METH NIL X0 Y0 X1 Y1 ALU NIL color))
    (WHEN CLOSED-CURVE-P
      (FUNCALL METH NIL X1 Y1 (VALUES (FLOOR (AREF PX 0))) (VALUES (FLOOR (AREF PY 0))) ALU NIL color))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CLOSED-CURVE)
           (PX PY &OPTIONAL END (ALU CHAR-ALUF) (color (sheet-foreground-color self)))
  "Display vectors of points."
  (SEND SELF :DRAW-CURVE PX PY END ALU T color))

;;;added the following defsubst on 18 Dec 85 for GRH by GSM
(DEFSUBST LINE-DELTAS (AX AY BX BY CX CY HALF-THICKNESS)
  "This calculates the delta x and y values for the edges of two connected wide lines.
These two values are returned in a list.  The line segments go from (ax,ay) to (bx,by)
to (cx,cy)."
  (DECLARE (VALUES DELTA-X DELTA-Y))
  (LET* ((DX1 (- AX BX))
	 (DY1 (- AY BY))
	 (DX2 (- CX BX))
	 (DY2 (- CY BY))
	 (DT (- (* CX DY1) (* BX (- AY CY)) (* AX DY2)))
	 (LENGTH1 (SQRT (+ (* DX1 DX1) (* DY1 DY1))))
	 (LENGTH2 (SQRT (+ (* DX2 DX2) (* DY2 DY2)))))
    (COND ((> (ABS DT) 0.5) (SETQ DT (QUOTIENT (FLOAT HALF-THICKNESS) DT))
			    (VALUES (* DT (+ (* LENGTH2 DX1) (* LENGTH1 DX2)))
				    (* DT (+ (* LENGTH2 DY1) (* LENGTH1 DY2)))))
	  ((NOT (ZEROP LENGTH1))
	   (VALUES (QUOTIENT (* HALF-THICKNESS DY1) LENGTH1)
		   (QUOTIENT (* HALF-THICKNESS (- DX1)) LENGTH1)))
	  ((NOT (ZEROP LENGTH2))
	   (VALUES (QUOTIENT (* HALF-THICKNESS (- DY2)) LENGTH2)
		   (QUOTIENT (* HALF-THICKNESS DX2) LENGTH2)))
	  (T (VALUES 0 0)))))

;;;replaced the following defmethod on 18 Dec 85 for GRH by GSM
(DEFMETHOD (GRAPHICS-MIXIN :DRAW-WIDE-CURVE)
	   (X-POINTS Y-POINTS THICKNESS
	    &OPTIONAL NUM-POINTS (ALU CHAR-ALUF) CLOSED-P (COLOR (sheet-foreground-color self)))
  "Draw a curve specified by arrays X-POINTS and Y-POINTS with specified THICKNESS,
 ALU and COLOR.  CLOSED-P means to draw a line between the first and last points.
 NUM-POINTS is how many of the points within the array to use."

  (IF (< THICKNESS 2)                                        ; this loses any specified gray-shade
      (SEND SELF :DRAW-CURVE X-POINTS Y-POINTS NUM-POINTS ALU CLOSED-P color)
      (LET ((X1) (Y1) (X2) (Y2) (X3) (Y3)

	    ; these contain four coords to draw two triangles representing the
	    ; wide line segements for points 1 & 2 above.
	    (PX1) (PY1) (PX2) (PY2) (PX3) (PY3) (PX4) (PY4)

	    ; these are used to obtain the triangle points from the line points.
       	    (DX) (DY)

	    ; these define the clipping rectangle for %draw-filled-triangle
            (MAX-LEFT   (SHEET-INSIDE-LEFT))
 	    (MAX-TOP    (SHEET-INSIDE-TOP))
 	    (MAX-RIGHT  (SHEET-INSIDE-RIGHT))
 	    (MAX-BOTTOM (SHEET-INSIDE-BOTTOM))

	    (NUM-SEGMENTS
	         (1- (MIN (ARRAY-ACTIVE-LENGTH X-POINTS) (ARRAY-ACTIVE-LENGTH Y-POINTS))))
	    (I 2)
	    (TWO 2))

	(SETQ THICKNESS (/ THICKNESS 2.0)
	      NUM-POINTS
	         (OR (AND NUM-POINTS (MIN NUM-POINTS (1+ NUM-SEGMENTS))) (1+ NUM-SEGMENTS))
	      NUM-SEGMENTS (1- NUM-POINTS))
	(WHEN (OR (> NUM-SEGMENTS 1)                                   ; only draw lines of length >= 1
                  (AND (= NUM-SEGMENTS 1)                              ; where 1 is a special case.
                	  (SETQ CLOSED-P NIL TWO 0)))
	  (SETQ X1 (AREF X-POINTS 0)
		Y1 (AREF Y-POINTS 0)
		X2 (AREF X-POINTS 1)
		Y2 (AREF Y-POINTS 1))
	  (IF CLOSED-P
	      (SETQ X3 (AREF X-POINTS NUM-SEGMENTS)
		    Y3 (AREF Y-POINTS NUM-SEGMENTS))
	      (SETQ X3 X1 Y3 Y1))
	  (MULTIPLE-VALUE-SETQ (DX DY)
	    (LINE-DELTAS X3 Y3 X1 Y1 X2 Y2 THICKNESS))
	  (SETQ PX1 (round (- X1 DX))
		PY1 (round (- Y1 DY))
		PX2 (round (+ X1 DX))
		PY2 (round (+ Y1 DY))
		X3 (AREF X-POINTS TWO)
		Y3 (AREF Y-POINTS TWO))
        (prepare-color (self color)
	  (PREPARE-SHEET (SELF)
	    (LOOP
	      (MULTIPLE-VALUE-SETQ (DX DY)
				   (LINE-DELTAS X1 Y1 X2 Y2 X3 Y3 THICKNESS))
	      (SETQ PX3 (round (- X2 DX))
		    PY3 (round (- Y2 DY))
		    PX4 (round (+ X2 DX))
		    PY4 (round (+ Y2 DY)))	; draw a line segment via two triangles

              (with-clipping-rectangle (MAX-LEFT MAX-TOP MAX-RIGHT MAX-BOTTOM)
                                       (%DRAW-SHADED-TRIANGLE PX1 PY1 PX4 PY4 PX2 PY2
                                                              ALU t T T 100%-black
							      self)
                                       (%DRAW-SHADED-TRIANGLE PX1 PY1 PX3 PY3 PX4 PY4
                                                              ALU t T T 100%-black
							      self))
	      (SETQ PX1 PX3 PY1 PY3 PX2 PX4 PY2 PY4 X1 X2 Y1 Y2 X2 X3 Y2 Y3 I (1+ I))
	      (IF (< I NUM-POINTS) (SETQ X3 (AREF X-POINTS I) Y3 (AREF Y-POINTS I))
		  (COND ((= I NUM-POINTS)
			 (IF CLOSED-P (SETQ X3 (AREF X-POINTS 0) Y3 (AREF Y-POINTS 0))
			     (SETQ X3 X2 Y3 Y2)))
			((AND CLOSED-P (= I (1+ NUM-POINTS)))
			 (SETQ X3 (AREF X-POINTS 1) Y3 (AREF Y-POINTS 1)))
			((RETURN NIL)))))
	    ))))))

;;; Cubic splines from Rogers and Adams, "Mathematical Elements for
;;; Computer Graphics".  This began as a translation from a BASIC
;;; program, but has been changed a bit.  The original program uses a
;;; full matrix inversion when the boundary conditions are cyclic or
;;; anti-cyclic, which is inefficient; in this version the special-case
;;; tridiagonal solver is extended to handle the cyclic and anti-cyclic
;;; end conditions.  (Also, the original program has a bug wherein it
;;; neglects to initialize one diagonal of the M matrix.)

(DEFUN SPLINE (PX PY Z &OPTIONAL CX CY (C1 :RELAXED) (C2 C1)
	       P1-PRIME-X P1-PRIME-Y PN-PRIME-X PN-PRIME-Y
	       &AUX  N N-1 N-2 N-3 BX BY L UX UY N1 N2 N3 N4 SIGN
	       (ZUNDERFLOW T) CLEN)
  "Compute cubic splines.  PX and PY are arrays of X-coords and
Y-coords.  They describe a sequeuce of points through which a smooth
curve should be drawn.  This program generates Z intermediate points
between each pair of points, returning a sequence of points in CX and
CY that includes the original points with the intermediate points
inserted.  The caller can then plot lines between successive pairs of
points of CX and CY to draw the curve.

The caller may pass in arrays to be filled in with the answers (used as
CX and CY); they should be (+ N (* Z (- N 1))) long.  If NIL is passed,
this function creates the arrays itself.  If they are not long enough,
they are adjusted with ADJUST-ARRAY.

The optional argument C1 is the initial end condition, one of
:RELAXED, :CLAMPED, :CYCLIC, or :ANTI-CYCLIC; C2 is the final end
condition, one of :RELAXED or :CLAMPED.  The first defaults to
:RELAXED, and the second defaults to the first.  The second must be
the same as the first if the first is :CYCLIC or :ANTI-CYCLIC.  The last
four arguments are the X and Y values to which the endpoints are
being clamped if the corresponding boundary condition is :CLAMPED.
For cyclic splines that join themselves, the caller must pass the same
point twice, as both the first point and the last point.

P1-PRIME-X, etc., specify the slopes at the two endpoints, for the
sake of :CLAMPED constraints.

Three values are returned: The two arrays CX and CY, and the number
of active elements those arrays."
  (DECLARE (VALUES CX CY NUMBER-OF-POINTS))
  (SETQ N (ARRAY-ACTIVE-LENGTH PX)                            ;The number of points
	N-1 (1- N)
	N-2 (1- N-1)
	N-3 (1- N-2))
  (SETQ CLEN (+ N (* N-1 Z)))

  ;; Create the arrays if they were not given them, or redimension them
  ;; if needed.

  (COND ((NULL CX) (SETQ CX (MAKE-ARRAY CLEN)))
	((< (ARRAY-TOTAL-SIZE CX) CLEN)
	 (SETQ CX (ADJUST-ARRAY CX CLEN))))
  (COND ((NULL CY) (SETQ CY (MAKE-ARRAY CLEN)))
	((< (ARRAY-TOTAL-SIZE CY) CLEN)
	 (SETQ CY (ADJUST-ARRAY CY CLEN))))
  ;; Set up L to hold the approximate spline segment lengths.  The Nth
  ;; element of L holds the distance between the Nth and N+1st points of
  ;; PX,PY.  The last element of L is not used.

  (SETQ L (MAKE-ARRAY N))
  (LOOP FOR J FROM 0 TO N-2
	DO
	 (SETF (AREF L J)
	       (SMALL-FLOAT
		(SQRT
		 (+ (EXPT (- (AREF PX (1+ J)) (AREF PX J)) 2)
		    (EXPT (- (AREF PY (1+ J)) (AREF PY J)) 2))))))
  ;; Fill in the last element of L so that the next check will work
  ;; properly.

  (SETF (AREF L (1- N)) -1.0s0)
  ;; Check L to make sure that none of the lengths are zero.  This will
  ;; happen when the caller has passed in two adjacent X,Y points which
  ;; are the same.  Normally, this wouldn't even make sense, but we will
  ;; simply ignore the repetitious point(s) to avoid getting a
  ;; zero-divide error later on.
  (LET ((NEW-INDEX 0))                                                 ; Place to store elements during copy operation
    (DOTIMES (INDEX N)
      (WHEN (/= NEW-INDEX INDEX)                                       ; Don't move values to same location.  JEB
	(SETF (AREF L NEW-INDEX) (AREF L INDEX))
	(SETF (AREF PX NEW-INDEX) (AREF PX INDEX))
	(SETF (AREF PY NEW-INDEX) (AREF PY INDEX)))
      (UNLESS (ZEROP (AREF L INDEX))
	(SETQ NEW-INDEX (1+ NEW-INDEX))))
    ;; Update the limits that have changed, since we have removed the
    ;; equal adjacent points.
    (SETQ N NEW-INDEX
	  N-1 (1- N)
	  N-2 (1- N-1)
	  N-3 (1- N-2)
	  CLEN (+ N (* N-1 Z))))
  ;; The bulk of the code here is concerned with solving a set of
  ;; simultaneous linear equations, expressed by the matrix equation
  ;; M * U = B.  M is an N by N square matrix, and B and U are N by 1
  ;; column matricies.  U will hold the values of the slope of the curve
  ;; at each point PX, PY.
  ;; The M matrix is tridiagonal for :RELAXED and :CLAMPED end conditions.
  ;; We represent it by storing M(I,I-1) in N1(I), M(I,I) in N2(I), and
  ;; M(I,I+1) in N3(I).  This means N1(0) and N3(N-1) are unused.

  (SETQ N1 (MAKE-ARRAY N)
	N2 (MAKE-ARRAY N)
	N3 (MAKE-ARRAY N))
  ;; These quantities are meaningless, but they get referred to as part
  ;; of array bound conditions; these values just prevent errors from
  ;; happening.

  (SETF (AREF N1 0) 0.0s0)
  (SETF (AREF N3 N-1) 0.0s0)
  (COND ((MEMBER C1 '(:CYCLIC :ANTI-CYCLIC) :TEST #'EQ)
	 ;; With these conditions, the M matrix is not quite tri-diagonal;
	 ;; it is initialize with a 1 in the upper-right hand corner, and
	 ;; during the solution of the equations the whole right column
	 ;; gets non-zero values.  Also, it is only N-1 by N-1!  So the upper
	 ;; right corner is M(0, N-2).  N4 represents the N-2 column; element
	 ;; M(I,N-2) is stored in N4(I).  The last two elements are not
	 ;; used, because N4(N-2) = N2(N-2) and N4(N-3) = N3(N-3).  We also
	 ;; set up this handy SIGN variable.
	 (SETQ N4 (MAKE-ARRAY (1- N)))
	 (SETQ SIGN (IF (EQ C1 :CYCLIC) 1.0s0 -1.0s0)))
	((NOT (MEMBER C1 '(:RELAXED :CLAMPED) :TEST #'EQ))
	 (FERROR NIL "~S is not known spline type" C1)))
  ;; B is just a column vector, represented normally.

  (SETQ BX (MAKE-ARRAY N)
	BY (MAKE-ARRAY N))
  ;; Set up the boundary conditions.  The 0th row of M and B are
  ;; determined by the initial boundary conditions, and the N-1st row is
  ;; determined by the final boundary condition.  Note that the 0th row
  ;; of M is implemented as the 0th element of N2, N3, and sometimes N4;
  ;; N1(0) is not used.  A similar thing is true of the N-1st row.

  (CASE C1
    (:CLAMPED
     (SETF (AREF N2 0) 1.0s0)
     (SETF (AREF N3 0) 0.0s0)
     (SETF (AREF BX 0) P1-PRIME-X)
     (SETF (AREF BY 0) P1-PRIME-Y))
    (:RELAXED
     (SETF (AREF N2 0) 1.0s0)
     (SETF (AREF N3 0) 0.5s0)
     (LET ((TEM (/ 3.0s0 (* 2.0s0 (AREF L 0)))))
      	(SETF (AREF BX 0) (* TEM (- (AREF PX 1) (AREF PX 0))))
       	(SETF (AREF BY 0) (* TEM (- (AREF PY 1) (AREF PY 0))))))
    ((:CYCLIC :ANTI-CYCLIC)
     (LET ((S3 (QUOTIENT (AREF L N-2) (AREF L 0))))
       (SETF (AREF N2 0) (+ 2.0s0 (* S3 2.0s0)))
       (SETF (AREF N3 0) S3)
       (SETF (AREF N4 0) SIGN)
       (LET ((TEM (/ 3.0s0 (AREF L 0))))
	  (SETF (AREF BX 0)
		(* TEM
		   (+ (* S3 (- (AREF PX 1) (AREF PX 0)))
		      (* SIGN (QUOTIENT (- (AREF PX N-1) (AREF PX N-2)) S3)))))
	  (SETF (AREF BY 0)
		(* TEM
		   (+ (* S3 (- (AREF PY 1) (AREF PY 0)))
		      (* SIGN (QUOTIENT (- (AREF PY N-1) (AREF PY N-2)) S3)))))))))
  (CASE C2
    (:CLAMPED
     (SETF (AREF N1 N-1) 0.0s0)
     (SETF (AREF N2 N-1) 1.0s0)
     (SETF (AREF BX N-1) PN-PRIME-X)
     (SETF (AREF BY N-1) PN-PRIME-Y))
    (:RELAXED
     (SETF (AREF N1 N-1) 2.0s0)
     (SETF (AREF N2 N-1) 4.0s0)
     (LET ((TEM (/ 6.0s0 (AREF L N-2))))
       	(SETF (AREF BX N-1) (* TEM (- (AREF PX N-1) (AREF PX N-2))))
       	(SETF (AREF BY N-1) (* TEM (- (AREF PY N-1) (AREF PY N-2))))))
    ;; Note: there are no final end conditions for :CYCLIC and
    ;; :ANTI-CYCLIC, since they are the same at each end.  The M matrix
    ;; has no N-1st row, either, as it is smaller by one row and one
    ;; column.
    )
  ;; Now fill in the insides of M and B arrays.

  (LOOP FOR J FROM 1 TO N-2
	AS L0  := (AREF L  0) THEN L1
	AS L1  := (AREF L  1) THEN (AREF L J)
	AS PX0 := (AREF PX 0) THEN PX1
	AS PX1 := (AREF PX 1) THEN PX2
	AS PX2 := (AREF PX (1+ J))
	AS PY0 := (AREF PY 0) THEN PY1
	AS PY1 := (AREF PY 1) THEN PY2
	AS PY2 := (AREF PY (1+ J))
	DO (SETF (AREF N1 J) L1)
	   (SETF (AREF N2 J) (* 2 (+ L0 L1)))
           (SETF (AREF N3 J) L0)
	   (IF N4 (SETF (AREF N4 J) 0.0s0))
           (SETF (AREF BX J)
	      (/ (* 3.0s0 (+ (* (EXPT L0 2) (- PX2 PX1)) (* (EXPT L1 2) (- PX1 PX0))))
			(* L0 L1)))
	   (SETF (AREF BY J)
	      (/ (* 3.0s0 (+ (* (EXPT L0 2) (- PY2 PY1)) (* (EXPT L1 2) (- PY1 PY0))))
			(* L0 L1))))
  ;; Now that we have the matricies filled in, we solve the equations.
  ;; We use Gaussian elimination, with a special version that takes
  ;; advantage of the sparsity of this tridiagonal or almost-tridiagonal
  ;; matrix to run in time O(n) instead of O(n**3).  No pivoting is
  ;; used, because for any real dat (not all zeroes, for example) the
  ;; matrix is both irreducible and diagonally-dominant, and therefore
  ;; pivoting is not needed (Forsythe and Moler, p. 117, exercise
  ;; 23.10).  The first step is to make the matrix upper-triangular, by
  ;; making all of N1 be zero.

  (LET ((Q (AREF N2 0)))                                            ;Normalize row 0.
    (SETF (AREF N3 0) (QUOTIENT (AREF N3 0) Q))
    (IF N4
     	 (SETF (AREF N4 0) (QUOTIENT (AREF N4 0) Q)))
    (SETF (AREF BX 0) (QUOTIENT (AREF BX 0) Q))
    (SETF (AREF BY 0) (QUOTIENT (AREF BY 0) Q)))
  (LOOP FOR I FROM 1 TO (IF (NULL N4) N-1 N-2)
	AS N1I := (AREF N1 I)
	WHEN (NOT (ZEROP N1I))                                      ;If it is zero already, OK.
       	DO (LET ((D (/ 1.0s0 N1I)))
	      ;; D = M(I-1, I-1) / M(I, I-1)  so multiply row I
	      ;;   by D and subtract row I-1 from row I.
	     (SETF (AREF N2 I) (- (* D (AREF N2 I)) (AREF N3 (1- I))))
             (SETF (AREF N3 I) (* D (AREF N3 I)))                   ; Uses N3(N-1), a garbage element.
             (COND (N4
        	   (SETF (AREF N4 I) (- (* D (AREF N4 I)) (AREF N4 (1- I))))
	           (IF (= I N-3)
		       ;; In this case, N4(N-4) is above N3(N-3), so
	               ;; it must be subtracted out.
		       (SETF (AREF N3 I) (- (AREF N3 I) (AREF N4 (1- I)))))))

       	     (SETF (AREF BX I) (- (* D (AREF BX I)) (AREF BX (1- I))))
	     (SETF (AREF BY I) (- (* D (AREF BY I)) (AREF BY (1- I)))))
	;; Next normalize, by dividing row I through by M(I,I).
	;; This leaves the center diagonal all 1.0s0, which the
	;; back-solver in R&A doesn't take advantage of.

	(LET ((Q (AREF N2 I)))
	   (SETF (AREF N3 I) (QUOTIENT (AREF N3 I) Q))
	   (IF N4
	       (SETF (AREF N4 I) (QUOTIENT (AREF N4 I) Q)))
	   (SETF (AREF BX I) (QUOTIENT (AREF BX I) Q))
	   (SETF (AREF BY I) (QUOTIENT (AREF BY I) Q))))

  ;; Create the arrays to hold the answers.
  (SETQ UX (MAKE-ARRAY N)                                   ;Tangent vector matrix
	UY (MAKE-ARRAY N))

  ;; Backsolve the upper-triangular matrix.
  (COND ((NOT N4)
	 ;; Simpler version if there is no N4.
	 (SETF (AREF UX N-1) (AREF BX N-1))
	 (SETF (AREF UY N-1) (AREF BY N-1))
	 (LOOP FOR J FROM N-2 DOWNTO 0 DO
	       (LET ((N3J (AREF N3 J)))
		 (SETF (AREF UX J) (- (AREF BX J) (* N3J (AREF UX (1+ J)))))
		 (SETF (AREF UY J) (- (AREF BY J) (* N3J (AREF UY (1+ J))))))))
	(T
	 ;; Hairier version with N4.
	 (LET ((UXN-2 (AREF BX N-2))
	       (UYN-2 (AREF BY N-2)))
	   (SETF (AREF UX N-2) UXN-2)
	   (SETF (AREF UY N-2) UYN-2)
	   (SETF (AREF UX N-3) (- (AREF BX N-3) (* (AREF N3 N-3) UXN-2)))
	   (SETF (AREF UY N-3) (- (AREF BY N-3) (* (AREF N3 N-3) UYN-2)))
	   (LOOP FOR J FROM (1- N-3) DOWNTO 0 DO
		 (LET ((N3J (AREF N3 J))
		       (N4J (AREF N4 J)))
		   (SETF (AREF UX J) (- (AREF BX J) (* N3J (AREF UX (1+ J))) (* N4J UXN-2)))
		   (SETF (AREF UY J) (- (AREF BY J) (* N3J (AREF UY (1+ J))) (* N4J UYN-2))))))
	 (SETF (AREF UX N-1) (* SIGN (AREF UX 0)))
	 (SETF (AREF UY N-1) (* SIGN (AREF UY 0)))))

  (MULTIPLE-VALUE-SETQ (CX CY)
    (CURGEN N PX PY Z CX CY L UX UY))                        ; Generate it

  (RETURN-ARRAY UY)
  (RETURN-ARRAY UX)
  (RETURN-ARRAY BY)
  (RETURN-ARRAY BX)
  (IF N4 (RETURN-ARRAY N4))
  (RETURN-ARRAY N3)
  (RETURN-ARRAY N2)
  (RETURN-ARRAY N1)
  (RETURN-ARRAY L)
  (VALUES CX CY CLEN))

;;; Generate the spline curve points.
;;; This is a separate function because if it got merged, there would be
;;; too many local variables.
(DEFUN CURGEN (N PX PY Z CX CY L UX UY)
  (LOOP WITH I := 0
	FOR J FROM 0 TO (- N 2)
	FOR LEN := (AREF L J)
	FOR LEN^2 := (EXPT LEN 2)
	FOR LEN^3 := (* LEN^2 LEN)
	FOR FX1 := (AREF PX J)
	FOR FX2 := (AREF UX J)
	FOR TEMX := (- (AREF PX (1+ J)) FX1)
	FOR TEMX1 := (+ (AREF UX (1+ J)) FX2)
	FOR FX3 := (- (* (/ 3.0s0 LEN^2) TEMX) (/ (+ TEMX1 FX2) LEN))
	FOR FX4 := (+ (* (/ -2.0s0 LEN^3) TEMX) (/ TEMX1 LEN^2))
	FOR FY1 := (AREF PY J)
     	FOR FY2 := (AREF UY J)
	FOR TEMY := (- (AREF PY (1+ J)) FY1)
	FOR TEMY1 := (+ (AREF UY (1+ J)) FY2)
	FOR FY3 := (- (* (/ 3.0s0 LEN^2) TEMY) (/ (+ TEMY1 FY2) LEN))
	FOR FY4 := (+ (* (/ -2.0s0 LEN^3) TEMY) (/ TEMY1 LEN^2))
	DO (LOOP FOR K FROM 0 TO Z
		 FOR X FROM 0 BY (/ LEN (1+ Z))
		 DO (SETF (AREF CX I) (+ FX1 (* FX2 X) (* FX3 (EXPT X 2)) (* FX4 (EXPT X 3))))
	            (SETF (AREF CY I) (+ FY1 (* FY2 X) (* FY3 (EXPT X 2)) (* FY4 (EXPT X 3))))
	            (SETQ I (1+ I)))
	FINALLY	(PROGN (SETF (AREF CX I) (SMALL-FLOAT (AREF PX (1- N))))
               (SETF (AREF CY I) (SMALL-FLOAT (AREF PY (1- N))))
	       (RETURN CX CY))))

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-CUBIC-SPLINE)
	   (PX PY Z &OPTIONAL CURVE-WIDTH ALU (C1 :RELAXED) (C2 C1)
                	    P1-PRIME-X P1-PRIME-Y PN-PRIME-X  PN-PRIME-Y
			    (color (sheet-foreground-color self)))

  (IF (NULL CURVE-WIDTH)
      (SETQ CURVE-WIDTH 1))
  (IF (NULL ALU)
      (SETQ ALU CHAR-ALUF))
  (MULTIPLE-VALUE-BIND (CX CY I)
      (SPLINE PX PY Z NIL NIL C1 C2
	      P1-PRIME-X P1-PRIME-Y PN-PRIME-X PN-PRIME-Y)
    (IF (= CURVE-WIDTH 1)
	(SEND SELF :DRAW-CURVE CX CY I ALU nil	;NIL here means open curve PMH 3/9
	      color)
	(SEND SELF :DRAW-WIDE-CURVE CX CY CURVE-WIDTH I ALU nil ;NIL here means open curve PMH 3/9
	      color))))
