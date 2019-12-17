;;;-*- Mode:Common-Lisp; Package:TV; Base:10.; Fonts:CPTFONT,HL12B,HL12BI -*-

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
;;;	** (c) Copyright 1980, 1981 Massachusetts Institute of Technology **
;;;
;;; Change history:
;;;
;;;  Date        Author  Description
;;; -------------------------------------------------------------------------------------
;;;  04/25/89  MAY    Added associated-who-line-screen-is-compatible-p to *initial-screen-tests*
;;;                       to make sure who-line-screen is visible at boot on CSIB.
;;;  04/10/89  JLM     Moved defvar of INITIAL-LISP-LISTENER to top of file to quiet more warnings.
;;;  03/23/89  MAY    Changes to quiet cwarn errors.
;;;  03/20/89  MAY      More changes to define-screen for build on MX. 
;;;  03/09/89  MAY/JLM Changes to clean up code and fix build problems.
;;;		       o Added associated-who-line-is-who-line-screen-p to *initial-screen-tests*
;;;		       o Cleanup of create-acceptable-initial-who-line-screen 
;;;		       o added :screens-previously-selected-windows init in create-acceptable-initial-screen 
;;;		       o Moved setf of who-line-screen from  find-or-create-acceptable-initial-screen
;;;                          to initial-screen-setup - since create-acceptable-initial-who-line-screen USES it.
;;;		       o Simplified expose-initial-screens knowing that *intial-screen* MUST point to who-line-screen
;;;		        o LOCKED main-screen to *initial-screen* in INITIALIZE during boot and moved
;;;		         code to invoke THINGS-TO-DO-FIRST-TIME AFTER (initial-screen-setup) when
;;;			 things are initialized in system build.
;;;  03/07/89  JLM    Added DEFVARs for mp functions to be filled in by mp installation 
;;;  02/27/89  JLM    Added setup-screens-for-mp and changed window-initialize for the MP.
;;;  02/24/89  MAY   Changes to define-screen to build mac load band - who-line-screen exposing/refresh 
;;;		     with bit-array mapped to non-existant explorer! ALSO change to
;;;                       create-acceptable-initial-who-line-screen to get plane mask right for build.
;;;  02/15/89  MAY   Changed  find-or-create-acceptable-initial-screen to ALWAYS set tv:who-line-screen.
;;;  02/14/89  MAY   Changed EXPOSE-INITIAL-SCREENS to use SCREEN-SCREENS-WHO-LINE-SCREEN in/of SCREEN-SCREEN-DESCRIPTOR 
;;;  01/30/89  KJF     o [may] Change to expose-initial-screens for Multiple Monitors (MMON).
;;;                      o Fix define-screen to remove screens from all-the-screens as it did before.  SPR 8649.
;;;  		      o Change to main-screen-and-who-line for MMON systems.
;;;                      o Added virtual-address-of-frame-buffer-p and added new version of
;;;                      screen-exposable-p.  Also changed COMPLEMENT-BOW-MODE
;;;                      o Added during mods for Multiple Monitor (MMON) support.  Search for MMON
;;;                      o Removed :screens-previously-selected-windows from create-acceptable-initial-screen.
;;;  01/18/89 may      Rewrote TAB cond in SHEET-CHARACTER-WIDTH since the left margin was NOT accounted
;;;                       for and it returned incorrect values when called by set-blinker-size in zmacs :redisplay
;;;                       when zwei:*tab-blinker-flag* was nil.
;;;  10/17/88 LG	       Fix sheet-delete-char to bitblt to the left just the area to the right of the cursor.
;;;  07/11/88 KJF       Fix to DRAW-CHAR to not do prepare-color if WINDOW arg is an array.  Fixes
;;;                       SPR #8202.
;;; 05/23/88  KJF    Changed references to screen-descriptor to screens-who-line-screen due to source build
;;;                     being done which allowed instance variable to take on a meaningful name.
;;; 04/26/88  KJF         Change to screen-exposable-p to return t always when on microExplorer.
;;; 04/23/88  KJF         Changes to MAIN-SCREEN-AND-WHO-LINE and DEFINE-SCREEN
;;; 04/23/88  CJJ, PMH, KJF  Many changes for multiple screen support.  New functions added to help in
;;;                          booting bands containing multiple screens (color and monochrome).
;;;  04/10/88  KJF        Change to define-screen for multiple screen support.
;;;  02/21/88  KJF        Added some info to the property list of main-screen.
;;;  01/22/88  LG         Made (SHEET :KILL) work differently for microExplorer than for Explorer since deactivation of
;;;			a window's inferiors must precede deactivation of a window in microExplorer environment.
;;;   8/27/87  KWW      Made changes that resulted from code reading, mostly just simple clean up.
;;;   7/13/87  KWW      Added optional color argument to text drawing methods and functions.
;;;    8/28/87  PMH      Modified SHEET-STRING-OUT to correctly clip right edge
;;;    7/2/87   PMH       Changed SHEET-DISPLAY-LOZENGED-STRING-INTERNAL to better center the 
;;;                         text in the lozenge.
;;;                         Also allowed lozenges to be in top and bottom labels
;;;   4/24/87  TWE	Updated the io-buffer cleanout to happen on only a :kill or on returning
;;;			a window instance to a resource.
;;;  4/06/87  GRH	Rewrote draw-string-internal to be faster and use %draw-string.
;;;   4/2/87   SLM        Added SYS:DECLARE-SUGGESTIONS-FOR and the call to 
;;;                         SYS:WITH-SUGGESTIONS-MENUS-FOR to the :kill method for SHEET.
;;;   3/27/87  TWE	Fixed up work done on 3/26/87 so that it would compile for the test build.
;;;   3/26/87  KK	Fixed problems with lozenged strings reported in SPR #3259. 
;;;                         LOZENGE-STRING-WIDTH eliminated and replaced by
;;;                         LOZENGED-STRING-GEOMETRY. SHEET-DISPLAYED-LOZENGED-STRING,
;;;                         SHEET-DISPLAYED-LOZENGED-STRING-INTERNAL, SHEET-CHARACTER-WIDTH, and
;;;                         SHEET-STRING-OUT-EXPLICIT-1 modified.
;;;   3/23/87  GRH	Changed nearly all of uses of %draw-character to use %draw-char
;;;			since the uses of %draw-character were incorrect.
;;;   3/18/87  KK	Fixed SHEET-TYO-OUT to handle chars with font bits. Fixes SPR #3744.
;;;   3/13/87  KK	Fixed SHEET-LINE-OUT to properly display chars whose bits extend outside char
;;;                         height/width box. Fixes SPR #'s 3506, 3521.
;;;   3/11/87  KK	Fix SHEET-DISPLAY-CENTERED-STRING for Rel 3 SPR # 3076: prepare sheet 
;;;                         before SHEET-INCREMENT-BITPOS.
;;;   3/09/87  KK	Corrected typo in :INCREMENT-CURSORPOS to fix Rel 3 SPR #3001. 
;;;                         Historical note: the typo had been lurking for 15 months!
;;;   2/05/87  TWE	Fixed DRAW-CHAR again so that italicized fonts will be drawn properly.
;;;   1/30/87  KDB	Fixed SHEET-STRING-OUT-EXPLICIT-1. INDEX was not incremented after
;;;                         seeing #\NEWLINE. Multi-line labels, etc. now work.
;;;   1/29/87  KDB	Made fix to DRAW-CHAR suggested by MR to correcly draw fonts with 
;;;                         index tables. Added another fix for those with variable widths
;;;   1/19/87  KK	New implementation of SHEET-DISPLAY-CENTERED-STRING. This was
;;;			motivated by inability of previous version to center a text block given by
;;;			a string with embedded #\newlines. More efficient implementation might be
;;;			possible by modifying SHEET-STRING-OUT to handle such a string.
;;; 12/12/86   TWE	Fixed DRAW-CHAR to work properly with ISO fonts like HL12B.
;;; 11/25/86   TWE	Changed all occurances of %draw-line with %draw-shaded-triangle.
;;;			Also added hooks for color support.
;;; 11/21/86   LGO	Replaced all occurances of %DRAW-CHAR with %DRAW-CHARACTER.
;;; 11/21/86   LGO	Re-wrote SHEET-STRING-OUT and SHEET-STRING-OUT-EXPLICIT-1 to use
;;;			the new DRAW-STRING-INTERNAL function, which uses %DRAW-STRING.
;;; 11/06/86   TWE	Fixed sheet-string-out to work with fat strings.  It ignores the font info.
;;;			If the user wants to have the font info used, then sheet-fat-string-out
;;;			should be used.
;;; 10/30/86   TWE	Removed some lozenge debug code.
;;; 10/29/86   KDB	Fixed  :DISPLAY-LOZENGED-STRING so lozenge stops at right inside
;;;			edge of sheet. Closed bug report number 1807.
;;; 10/14/86   TWE	Changed PRINTING-CHARACTER-TRANSLATE-TABLE to handle ISO characters.
;;; 10/07/86   TWE	Fixed up sheet-string-out to properly handle normal strings (by not handling
;;;			fat strings, specifically the font part).  This makes it more like release 78,
;;;			and makes menus work when the font is 43VXMS.
;;; 09/17/86   TWE	Fixed a bug when trying to do a :string-out-explicit on a string which
;;;			contains the END character by fixing up SHEET-STRING-OUT-EXPLICIT-1
;;;			to look into xr-special-character-names properly.  Also fixed up
;;;			SHEET-CHARACTER-WIDTH which had the same problem.
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 08/04/86   TWE	Changed type checks for font to use the FONT-OBJECT-P function.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 07/28/86   TWE	Modified references to the pixel functions to use ARRAY-DIMENSION
;;;			and MAKE-ARRAY instead.
;;; 07/14/86   TWE	Fixed MAIN-SCREEN-AND-WHO-LINE to use element-array-type instead of type as
;;                      the argument
;;;			to adjust-array.
;;; 06/21/86   TWE	Changed window-initialize to instantiate the initial lisp listener as a simple-lisp-listener if such a flavor
;;;			exists.  This was done to make use of a lisp listener even when the UCLized lisp-listener isn't there.
;;; 06/18/86   TWE	Changed MAIN-SCREEN-AND-WHO-LINE to use ADJUST-ARRAY instead of SI:CHANGE-INDIRECT-ARRAY.
;;; 06/04/86   TWE	Modified SHEET-TYO to handle CONTROL-Z instead of erroring out.
;;; 02/24/86   LGO	Moved the beep functions into a new file - sys:window;SOUND

(DEFVAR INITIAL-LISP-LISTENER nil ;; may 03/23/89 was missing nil
        "Window instance for the first Lisp Listener that is created.")

(DEFMACRO COERCE-FONT (FONT-VARIABLE SHEET)
  `(UNLESS (FONT-OBJECT-P ,FONT-VARIABLE)
     (SETQ ,FONT-VARIABLE (SEND (SHEET-GET-SCREEN ,SHEET) :PARSE-FONT-SPECIFIER
				,FONT-VARIABLE))))

(DEFUN SCREEN-REDISPLAY
       (&OPTIONAL (TYPE :COMPLETE-REDISPLAY) (SCREEN DEFAULT-SCREEN))
  "Redisplay the entire contents of SCREEN."
  (SEND SCREEN :REFRESH TYPE)
  (WHO-LINE-CLOBBERED))

;;; --------------------------------------------------
;;; BEEP methods
;;; --------------------------------------------------

(DEFMETHOD (SCREEN :BEEP) (&OPTIONAL BEEP-TYPE)
  "Beep the beeper."
  ;; This method presumes nothing about the current state of beeping.
  (WHEN BEEP
    (BEEP BEEP-TYPE 'IGNORE)))

(DEFMETHOD (SHEET :BEEP) (&OPTIONAL BEEP-TYPE)
  (AND SUPERIOR (SEND SUPERIOR :BEEP BEEP-TYPE)))


(DEFVAR *CURRENT-SCREEN-COLOR* NIL
   "The initial shading or color of the monitor's SCREEN.")

(DEFUN SET-UP-CURRENT-SCREEN-COLOR ()
  "Reset the hardware in case of a WARM-BOOT."
  (IF *CURRENT-SCREEN-COLOR*
      (BLACK-ON-WHITE)   ;;; Dept. of Redundency Dept.   
      (WHITE-ON-BLACK))  ;;; Dept. of Redundency Dept. 
  )

(DEFUN BLACK-ON-WHITE (&OPTIONAL (ignore DEFAULT-SCREEN))
  "Set SCREEN to display one bits as black and zeros as white."
  (SETQ *CURRENT-SCREEN-COLOR* T)
  (or (si:screen-black-on-white-p)
      (si:complement-screen)))
 
(DEFUN WHITE-ON-BLACK (&OPTIONAL (ignore DEFAULT-SCREEN))
  "Set SCREEN to display one bits as white and zeros as black."
  (SETQ *CURRENT-SCREEN-COLOR* NIL)
  (and (si:screen-black-on-white-p)
       (si:complement-screen)))

;;; note - this is a FUNCTION, which only affects the monochrome monitor
;;; when reverse videoing the color stuff, send the MESSAGE :complement-bow-mode to the window

(DEFUN COMPLEMENT-BOW-MODE (&OPTIONAL ignore)
  "Complement whether SCREEN displays one bits as white or as black."
  ;; COMPLEMENT-BOW-MODE stands for COMPLEMENT Black On White MODE.
  (SETQ *CURRENT-SCREEN-COLOR* (NOT *CURRENT-SCREEN-COLOR*))
  ;; for multiple CSIBs complement
  ;;; Modified by KJF on 08/20/88 for GRH during addition of Multiple Monitor (MMON) support.
  (IF (AND mouse-sheet (mmon-p))
      (si:complement-screen (csib-sheet-slot mouse-sheet))
      (si:complement-screen)))


(DEFMETHOD (SHEET :INCREMENT-BITPOS) (DX DY)
  (SHEET-INCREMENT-BITPOS SELF DX DY))

(DEFUN SHEET-INCREMENT-BITPOS (SHEET DX DY &AUX X Y MORE-VPOS)
  "Increment cursor X and cursor Y, keeping within sheet.
Sets exception flags according to new positions."
  (SETF (SHEET-CURSOR-X SHEET)
	(SETQ X (MAX (+ DX (SHEET-CURSOR-X SHEET)) (SHEET-INSIDE-LEFT SHEET))))
  (SETF (SHEET-CURSOR-Y SHEET)
	(SETQ Y (MAX (+ DY (SHEET-CURSOR-Y SHEET)) (SHEET-INSIDE-TOP  SHEET))))
  (AND (> (+ Y (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
       (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
  (AND (SETQ MORE-VPOS (SHEET-MORE-VPOS SHEET))
       (>= Y MORE-VPOS)
       (SETF (SHEET-MORE-FLAG SHEET) 1))
  NIL)

(DEFUN SHEET-SET-FONT (SHEET FONT)
  "Set the current font of SHEET to FONT.
The current font is what ordinary output is printed in.
FONT may be a font object, a name of one, a name of a name, etc."
  (SEND SHEET :SET-CURRENT-FONT FONT T))

(DEFMETHOD (SHEET :SIZE-IN-CHARACTERS) ()
  (VALUES (TRUNCATE (SHEET-INSIDE-WIDTH) CHAR-WIDTH)
          (SHEET-NUMBER-OF-INSIDE-LINES)))

(DEFMETHOD (SHEET :SET-SIZE-IN-CHARACTERS) (WIDTH-IN-CHARS HEIGHT-IN-CHARS
							   &OPTIONAL OPTION)
   (SEND SELF :SET-SIZE
		 (DECODE-CHARACTER-WIDTH-SPEC WIDTH-IN-CHARS)
		 (DECODE-CHARACTER-HEIGHT-SPEC HEIGHT-IN-CHARS)
		 OPTION))

(DEFMETHOD (SHEET :SET-CURSORPOS) (X Y &OPTIONAL (UNIT :PIXEL))
  (CASE UNIT
    (:PIXEL)
    (:CHARACTER (AND X
		     (SETQ X (* X CHAR-WIDTH)))
		(AND Y
		     (SETQ Y (* Y LINE-HEIGHT))))
    (OTHERWISE (FERROR NIL "~S is not a known unit." UNIT)))
  (SHEET-SET-CURSORPOS SELF X Y))

(DEFUN SHEET-SET-CURSORPOS (SHEET X Y)
  "Set 'cursor' position of SHEET in terms of raster units.
The cursor is where ordinary output will appear (the top left corner
of the next character).  Cursorposes are relative to the left and top
margins.  The arguments are `clipped' to stay inside the sheet's
margins."
  (DO ((INHIBIT-SCHEDULING-FLAG T T)		;Keep trying until we get the lock
       (LOCK)
       (BL))
      ((AND (SETQ LOCK (SHEET-CAN-GET-LOCK SHEET))
	    (NOT (SHEET-OUTPUT-HELD-P SHEET)))
       (SETQ X
	     (IF X
		 (MIN (+ (MAX (VALUES (FLOOR X)) 0) (SHEET-INSIDE-LEFT SHEET))
		      (SHEET-INSIDE-RIGHT SHEET))
		 (SHEET-CURSOR-X SHEET)))
       (SETQ Y
	     (IF Y
		 (MIN (+ (MAX (VALUES (FLOOR Y)) 0) (SHEET-INSIDE-TOP SHEET))
		      (SHEET-INSIDE-BOTTOM SHEET))
		 (SHEET-CURSOR-Y SHEET)))
       (AND (= (SHEET-CURSOR-X SHEET) X)
	    (= (SHEET-CURSOR-Y SHEET) Y)
	    (RETURN NIL))			;Not moving, don't open the blinker
       (AND (SETQ BL (SHEET-FOLLOWING-BLINKER SHEET)) (OPEN-BLINKER BL))
       (AND (SHEET-MORE-VPOS SHEET)		;If more processing enabled, delay until
						; bottom of sheet
	    (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
       (SETF (SHEET-CURSOR-X SHEET) X)
       (SETF (SHEET-CURSOR-Y SHEET) Y)
       (SETF (SHEET-EXCEPTIONS SHEET) 0)
       (AND (> (+ Y (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
	  (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
       T)
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (IF LOCK
      (SEND SHEET :OUTPUT-HOLD-EXCEPTION)
      (PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SHEET))))

;;;patched on 11 Dec 85 for PDC by GSM
(DEFMETHOD (SHEET :INCREMENT-CURSORPOS) (DX DY &OPTIONAL (UNIT :PIXEL))
  (CASE UNIT
	(:PIXEL)
	(:CHARACTER
	 (AND DX (NOT (ZEROP DX))
	      (SETQ DX
		    (- (* CHAR-WIDTH DX)
		       (NTH-VALUE 1 (CEILING (- CURSOR-X LEFT-MARGIN-SIZE) CHAR-WIDTH)))))
	 (AND DY (NOT (ZEROP DY))
	      (SETQ DY
		    (- (* LINE-HEIGHT DY)
		       (NTH-VALUE 1 (CEILING (- CURSOR-Y TOP-MARGIN-SIZE) LINE-HEIGHT))))))
	(OTHERWISE (FERROR NIL "~S is not a known unit." UNIT)))
  (PREPARE-SHEET (SELF) (OR (ZEROP (SHEET-EXCEPTIONS)) (SHEET-HANDLE-EXCEPTIONS SELF))
		 (SHEET-INCREMENT-BITPOS SELF DX DY)))

(DEFMETHOD (SHEET :READ-CURSORPOS) (&OPTIONAL (UNIT :PIXEL))
  (CASE UNIT
    (:PIXEL (VALUES (- CURSOR-X LEFT-MARGIN-SIZE) (- CURSOR-Y TOP-MARGIN-SIZE)))
    (:CHARACTER
     (VALUES (CEILING (- CURSOR-X LEFT-MARGIN-SIZE) CHAR-WIDTH)
	     (CEILING (- CURSOR-Y TOP-MARGIN-SIZE) LINE-HEIGHT)))
    (OTHERWISE (FERROR NIL "~S is not a known unit." UNIT))))

(DEFUN SHEET-READ-CURSORPOS (SHEET)
  "Return the cursor position in raster units relative to margins"
  (DECLARE (VALUES CURSOR-X CURSOR-Y))
  (VALUES (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
	  (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))))

(DEFMETHOD (SHEET :HOME-CURSOR) ()
  (SHEET-HOME SELF))

(DEFUN SHEET-HOME (SHEET)
  "Position SHEET's cursor to upper left corner (inside the margins)."
  (PREPARE-SHEET (SHEET)
    (AND (SHEET-MORE-VPOS SHEET)    ;If MORE processing, put it off 'til last line
	 (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
    (SETF (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
    (SETF (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP  SHEET))
    (SETF (SHEET-EXCEPTIONS SHEET) 0)))

(DEFMETHOD (SHEET :HOME-DOWN) ()
  (SHEET-SET-CURSORPOS SELF 0
		       (* (TRUNCATE (- (SHEET-INSIDE-HEIGHT) LINE-HEIGHT)
				     LINE-HEIGHT)
				 LINE-HEIGHT))
  (AND MORE-VPOS
       (SETQ MORE-VPOS (LOGIOR #o100000 MORE-VPOS))))	;Delay until next time

(DEFMETHOD (SHEET :TERPRI) ()
  (SHEET-CRLF SELF))

(DEFUN SHEET-CRLF (SHEET)
  "Move SHEET's cursor to beginning of next line, and clear the line."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))	;Handle exceptions first
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SETF (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
    (SHEET-INCREMENT-BITPOS SHEET 0 (SHEET-LINE-HEIGHT SHEET))
    (SHEET-CLEAR-EOL SHEET)))

(DEFMETHOD (SHEET :LINE-OUT) (STRING &OPTIONAL (START 0) END color)
  (SEND SELF :STRING-OUT STRING START END color)
  (SEND SELF :TERPRI))

(DEFMETHOD (SHEET :FRESH-LINE) ()
 ;; Return NIL if line cleared, T if CRLF was done.
  (IF (= CURSOR-X (SHEET-INSIDE-LEFT))
    (PROGN
      (SHEET-CLEAR-EOL SELF)
      NIL)
    (PROGN
      (SHEET-CRLF SELF)
      T)))

(DEFMETHOD (SHEET :CLEAR-CHAR) (&OPTIONAL CHAR)
  (SHEET-CLEAR-CHAR SELF CHAR))

(DEFUN SHEET-CLEAR-CHAR (SHEET &OPTIONAL CHAR)
  "Clear the character position SHEET's cursor points at.  CHAR may
be a character whose width controls how wide an area to clear."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (%DRAW-RECTANGLE (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
						     (SHEET-CURRENT-FONT SHEET))
			      (SHEET-CHAR-WIDTH SHEET))
		     (SHEET-LINE-HEIGHT SHEET)
		     (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
		     (SHEET-ERASE-ALUF SHEET) SHEET)))

(DEFMETHOD (SHEET :CLEAR-EOL) ()
  (SHEET-CLEAR-EOL SELF))

(DEFUN SHEET-CLEAR-EOL (SHEET)
  "Clear from SHEET's cursor to the right margin."
  (PREPARE-SHEET (SHEET)
    ;; Note that this need not handle **MORE** exception, because the **more**
    ;; would bash the line this is clearing anyway.  We don't want to **more**
    ;; if the next operation is going to be tyi.
    (OR (ZEROP (SHEET-END-PAGE-FLAG SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (%DRAW-RECTANGLE (MAX (- (SHEET-INSIDE-RIGHT  SHEET) (SHEET-CURSOR-X SHEET))
			  0)
		     (MIN (- (SHEET-INSIDE-BOTTOM SHEET) (SHEET-CURSOR-Y SHEET))
			  (SHEET-LINE-HEIGHT SHEET))
		     (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
		     (SHEET-ERASE-ALUF SHEET) SHEET)))

(DEFMETHOD (SHEET :CLEAR-STRING) (STRING &OPTIONAL START END)
  (SHEET-CLEAR-STRING SELF STRING START END))

(DEFUN SHEET-CLEAR-STRING (SHEET STRING &OPTIONAL START END)
  "Clear enough space after SHEET's cursor to hold STRING, or part of
it.  If STRING contains Return characters, we clear space on each line
to hold the characters of STRING on that line."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
     (DO ((LINE-START (OR START 0))
	  (WHOLE-END (OR END (LENGTH STRING)))
	  (PSEUDO-CURSOR-X (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
	  (PSEUDO-CURSOR-Y (SHEET-CURSOR-Y SHEET) (+ PSEUDO-CURSOR-Y LINE-HEIGHT))
	  MAXIMUM-X
	  FINAL-INDEX
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  (LINE-END))
	 (NIL)
	  ;; Do vertical wrap around at bottom of sheet.
       (IF (>= (+ PSEUDO-CURSOR-Y LINE-HEIGHT)
	       (SHEET-INSIDE-BOTTOM SHEET))
	 (SETQ PSEUDO-CURSOR-Y (SHEET-INSIDE-TOP SHEET)))
       ;; Find end of this line of the string.
       (SETQ LINE-END
	     (OR (POSITION #\NEWLINE (THE STRING (STRING STRING)) :START LINE-START :END WHOLE-END
			:TEST #'CHAR-EQUAL)
		 WHOLE-END))
       ;; Does it fit in one screen-line?  If not, how much does?
       (MULTIPLE-VALUE-SETQ (NIL FINAL-INDEX MAXIMUM-X)
	 (SHEET-STRING-LENGTH SHEET STRING LINE-START LINE-END (SHEET-INSIDE-RIGHT SHEET)
			      (SHEET-CURRENT-FONT SHEET) PSEUDO-CURSOR-X))
       ;; We only handle what fits in this screen-line.
       (IF (= FINAL-INDEX LINE-END)
	 (SETQ LINE-START (1+ LINE-END))
	 (SETQ LINE-START FINAL-INDEX
	       MAXIMUM-X (SHEET-INSIDE-RIGHT SHEET)))
       ;; Clear that much.
       (%DRAW-RECTANGLE (- MAXIMUM-X PSEUDO-CURSOR-X) LINE-HEIGHT
			PSEUDO-CURSOR-X PSEUDO-CURSOR-Y
			(SHEET-ERASE-ALUF SHEET) SHEET)
       ;; If entire specified portion of string is done, exit.
       (IF (>= LINE-START WHOLE-END)
	 (RETURN NIL)))))

(DEFMETHOD (SHEET :CLEAR-BETWEEN-CURSORPOSES) (START-X START-Y END-X END-Y)
  (SHEET-CLEAR-BETWEEN-CURSORPOSES SELF START-X START-Y END-X END-Y))

;;;patched on 11 Dec 85 for PDC by GSM
;;;patched on 16 Dec 85 by GSM
(DEFUN SHEET-CLEAR-BETWEEN-CURSORPOSES (SHEET START-X START-Y END-X END-Y &AUX (ALUF (SHEET-ERASE-ALUF SHEET)) MID-Y)
  "Erase on SHEET from START-X, START-Y to END-X, END-Y.  All
positions are relative to SHEET's margins.  Does nothing if start is
after end on the same line, but if on different lines, assumes screen
wrap-around."
  (SETQ START-X (MIN (+ START-X (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT  SHEET))
	START-Y (MIN (+ START-Y (SHEET-INSIDE-TOP  SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
	END-X   (MIN (+ END-X   (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT  SHEET))
	END-Y   (MIN (+ END-Y   (SHEET-INSIDE-TOP  SHEET)) (SHEET-INSIDE-BOTTOM SHEET)))
  (PREPARE-SHEET (SHEET)
     (COND
       ((= START-Y END-Y)
	(COND
	  ((< START-X END-X)
	   (%DRAW-RECTANGLE (- END-X START-X)
			    (MIN (- (SHEET-INSIDE-BOTTOM SHEET) START-Y)
				 (SHEET-LINE-HEIGHT SHEET))
			    START-X START-Y ALUF SHEET))))
       (T
	(%DRAW-RECTANGLE (- (SHEET-INSIDE-RIGHT SHEET) START-X)
			 (MIN (- (SHEET-INSIDE-BOTTOM SHEET) START-Y) (SHEET-LINE-HEIGHT SHEET))
			 START-X START-Y ALUF SHEET)
	(SETQ MID-Y (+ START-Y (SHEET-LINE-HEIGHT SHEET)))
	      (%DRAW-RECTANGLE (- END-X (SHEET-INSIDE-LEFT SHEET))	;pdc 12/4/85 and GSM 16 Dec 85
			 (MIN (- (SHEET-INSIDE-BOTTOM SHEET) END-Y) (SHEET-LINE-HEIGHT SHEET))
			 (SHEET-INSIDE-LEFT SHEET) END-Y ALUF SHEET)
	(IF (< START-Y END-Y)
	  (AND (< MID-Y END-Y)
	     (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) (- END-Y MID-Y)
			      (SHEET-INSIDE-LEFT SHEET) MID-Y ALUF SHEET))
	  (PROGN
	    (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) (- (SHEET-INSIDE-BOTTOM SHEET) MID-Y)
			     (SHEET-INSIDE-LEFT SHEET) MID-Y ALUF SHEET)
	    (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) (- END-Y (SHEET-INSIDE-TOP SHEET))
			     (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET) ALUF SHEET)))))))

(DEFMETHOD (SHEET :CLEAR-SCREEN) ()
  (SHEET-CLEAR SELF))

;;;Provide compatibility with Symbolics
;;; :CLEAR-WINDOW is a synonym (not an alternative) to :CLEAR-SCREEN
(DEFMETHOD (SHEET :CLEAR-WINDOW) ()
  (SEND SELF :CLEAR-SCREEN))

(DEFUN SHEET-CLEAR (SHEET &OPTIONAL (MARGINS-P NIL))
  "Erase all of SHEET.  If MARGINS-P, erase its margins too."
  (PREPARE-SHEET (SHEET)
    (SHEET-HOME SHEET)				;Handles any exceptions
    (IF MARGINS-P
	(%DRAW-RECTANGLE (SHEET-WIDTH SHEET) (SHEET-HEIGHT SHEET)
			 0 0
			 (SHEET-ERASE-ALUF SHEET) SHEET)
	(%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH  SHEET)
                         (SHEET-INSIDE-HEIGHT SHEET)
			 (SHEET-INSIDE-LEFT   SHEET)
                         (SHEET-INSIDE-TOP    SHEET)
			 (SHEET-ERASE-ALUF    SHEET) SHEET))
    (SCREEN-MANAGE-FLUSH-KNOWLEDGE SHEET)))

(DEFMETHOD (SHEET :CLEAR-EOF) ()
  (SHEET-CLEAR-EOF SELF))

(DEFUN SHEET-CLEAR-EOF (SHEET &AUX HT TEM)
  "Clear from SHEET's cursor to right margin, and all area below."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-CLEAR-EOL SHEET)
    (AND (PLUSP (SETQ HT (- (SHEET-INSIDE-BOTTOM SHEET)
			    (SETQ TEM (+ (SHEET-CURSOR-Y SHEET)
                                         (SHEET-LINE-HEIGHT SHEET))))))
	 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) HT
			  (SHEET-INSIDE-LEFT SHEET) TEM
			  (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFMETHOD (SHEET :INSERT-LINE) (&OPTIONAL (LINE-COUNT 1) (UNIT :CHARACTER))
  (SHEET-INSERT-LINE SELF LINE-COUNT UNIT))

(DEFUN SHEET-INSERT-LINE (SHEET &OPTIONAL (LINE-COUNT 1) (UNIT :CHARACTER))
  "Make room for some line before the line the cursor is currently on.
The data on this line and below is moved downward on the screen, and
that near the bottom of SHEET is discarded.

LINE-COUNT	how many lines to insert; default 1.
UNIT		:CHARACTER (the default) or :PIXEL, and says
		what unit LINE-COUNT is expressed in.
		:CHARACTER means it is multiplied by the
		window's line-height."
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (WIDTH (SHEET-INSIDE-WIDTH SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  HEIGHT
	  DELTA-HEIGHT)
      (SETQ HEIGHT (IF (EQ UNIT :CHARACTER)
                           (* LINE-COUNT LINE-HEIGHT)
                        LINE-COUNT))
      ;; Compute minus height of block to BLT
      (SETQ DELTA-HEIGHT
	    (- HEIGHT (- (* LINE-HEIGHT (SHEET-NUMBER-OF-INSIDE-LINES SHEET))
			 (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET)))))
      (OR (>= DELTA-HEIGHT 0)			;If some bits to move, move them
	  (if (color-system-p sheet)
	      (si:%scroll WIDTH (abs DELTA-HEIGHT)
			  (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)
			  (+ (SHEET-CURSOR-Y SHEET) HEIGHT) array)
	      (BITBLT ALU-SETA
		      WIDTH DELTA-HEIGHT
		      ARRAY (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)
		      ARRAY (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-CURSOR-Y SHEET)
							 HEIGHT))))
      (%DRAW-RECTANGLE WIDTH HEIGHT
		       (SHEET-INSIDE-LEFT SHEET)
                       (SHEET-CURSOR-Y    SHEET)
		       (SHEET-ERASE-ALUF  SHEET) SHEET))))

(DEFMETHOD (SHEET :DELETE-LINE) (&OPTIONAL (LINE-COUNT 1) (UNIT :CHARACTER))
  (SHEET-DELETE-LINE SELF LINE-COUNT UNIT))

(DEFUN SHEET-DELETE-LINE (SHEET &OPTIONAL (LINE-COUNT 1) (UNIT :CHARACTER))
  "Discard one or more lines starting at the cursor vpos, moving data
below up.  Blank lines appear at the bottom of SHEET.
LINE-COUNT	how many lines to delete; default 1.
UNIT		:CHARACTER (the default) or :PIXEL, and says what
		unit LINE-COUNT is expressed in.  :CHARACTER means
		it is multiplied by the window's line-height."
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (WIDTH (SHEET-INSIDE-WIDTH sheet))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  HEIGHT
	  DELTA-HEIGHT)
      (SETQ HEIGHT (IF (EQ UNIT :CHARACTER)
                           (* LINE-COUNT LINE-HEIGHT)
                        LINE-COUNT))
      (AND (PLUSP (SETQ DELTA-HEIGHT
			(- (+ (- (SHEET-CURSOR-Y   SHEET)
                                 (SHEET-INSIDE-TOP SHEET))
                              HEIGHT)
			   (* LINE-HEIGHT
                              (SHEET-NUMBER-OF-INSIDE-LINES SHEET)))))
	   (FERROR NIL "Illegal line-count ~S for ~S" LINE-COUNT SHEET))
      (if (color-system-p sheet)
	  (si:%scroll WIDTH (- DELTA-HEIGHT)
		      (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-CURSOR-Y SHEET) HEIGHT)
		      (SHEET-CURSOR-Y SHEET) array)
	  (BITBLT ALU-SETA WIDTH (- DELTA-HEIGHT)
		  ARRAY (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-CURSOR-Y SHEET) HEIGHT)
		  ARRAY (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)))
      (%DRAW-RECTANGLE WIDTH HEIGHT
		       (SHEET-INSIDE-LEFT SHEET)
                       (- (SHEET-CURSOR-Y SHEET) DELTA-HEIGHT)
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFMETHOD (SHEET :INSERT-CHAR) (&OPTIONAL (-WIDTH- 1) (UNIT :CHARACTER))
  (SHEET-INSERT-CHAR SELF -WIDTH- UNIT))

(DEFUN SHEET-INSERT-CHAR (SHEET &OPTIONAL (WIDTH 1) (UNIT :CHARACTER))
  "Make room for characters at SHEET's cursor, moving rest of line
right.  The last part of the line is discarded.  The cursorpos does not
change.
If UNIT is :CHARACTER, WIDTH is a number of characters.  This is
accurate only for fixed-width fonts.  Alternatively, WIDTH may be a
number of pixels if UNIT is :PIXEL."
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  (WIDTH (IF (EQ UNIT :PIXEL) WIDTH
		     (* WIDTH (SHEET-CHAR-WIDTH SHEET)))))
      ;; This when was added to avoid the BITBLT when possible PMH
      (when (< (+ (SHEET-CURSOR-X SHEET) WIDTH)
	       (SHEET-INSIDE-RIGHT SHEET))
      (BITBLT ALU-SETA
	      ;; the follow calculation is the negitive of
	      ;; (- (SHEET-INSIDE-RIGHT SHEET) (+ WIDTH (SHEET-CURSOR-X     SHEET)))
	      ;; this makes sure we copy from right to left  PMH
	      (- WIDTH (- (SHEET-INSIDE-RIGHT SHEET)
                          (SHEET-CURSOR-X     SHEET)))
	      LINE-HEIGHT
	      ARRAY    (SHEET-CURSOR-X SHEET)        (SHEET-CURSOR-Y SHEET)
	      ARRAY (+ (SHEET-CURSOR-X SHEET) WIDTH) (SHEET-CURSOR-Y SHEET)))
      ;; This used to be %DRAW-RECTANGLE
      (DRAW-RECTANGLE-INSIDE-CLIPPED WIDTH LINE-HEIGHT
		       (- (SHEET-CURSOR-X   SHEET) (sheet-left-margin-size sheet))
                       (- (SHEET-CURSOR-Y   SHEET) (sheet-top-margin-size sheet))
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFMETHOD (SHEET :DELETE-CHAR) (&OPTIONAL (-WIDTH- 1) (UNIT :CHARACTER))
  (SHEET-DELETE-CHAR SELF -WIDTH- UNIT))

(DEFUN SHEET-DELETE-CHAR (SHEET &OPTIONAL (WIDTH 1) (UNIT :CHARACTER))
  "Discard characters before SHEET's cursor, moving rest of line left.
Blank space is created near the right margin.  The cursor position does
not change.
If UNIT is :CHARACTER, WIDTH is a number of characters.  This is
accurate only for fixed-width fonts.  Alternatively, WIDTH may be a
number of pixels if UNIT is :PIXEL."
  (PREPARE-SHEET (SHEET)
    (LET* ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	   (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	   (WIDTH (IF (EQ UNIT :PIXEL) WIDTH
		      (* WIDTH (SHEET-CHAR-WIDTH SHEET))))
	   (from-x (MIN (+ (SHEET-CURSOR-X SHEET) WIDTH) (SHEET-INSIDE-RIGHT SHEET))))
     
      (BITBLT ALU-SETA
	      (- (SHEET-INSIDE-RIGHT SHEET) from-x)	;GSM  5 Jan 86 ;; LG 10/17/88 
	      LINE-HEIGHT			
	      ARRAY
	      from-x	
	      (SHEET-CURSOR-Y SHEET)		
	      ARRAY				
	      (SHEET-CURSOR-X SHEET)		
	      (SHEET-CURSOR-Y SHEET))
      (DRAW-RECTANGLE-inside-clipped WIDTH	;GSM  5 Jan 86
				     LINE-HEIGHT
				     (- (MAX (SHEET-CURSOR-X SHEET)
					     (- (SHEET-INSIDE-RIGHT SHEET) WIDTH))
					(SHEET-INSIDE-LEFT SHEET))	;subtract margin off because d-r-i-c is relative to inside
				     (- (SHEET-CURSOR-Y SHEET)
					(SHEET-INSIDE-TOP SHEET))	;ditto
				     (SHEET-ERASE-ALUF      SHEET)
				     SHEET))))

(DEFMETHOD (SHEET :INSERT-STRING) (STRING &OPTIONAL (START 0) END (TYPE-TOO T) color)
  (SHEET-INSERT-STRING SELF STRING START END TYPE-TOO color))

(DEFUN SHEET-INSERT-STRING (SHEET STRING
			    &OPTIONAL (START 0) END (TYPE-TOO T) color)
  "Make room for STRING after SHEET's cursor, moving rest of line
right.  The last part of the line is discarded.  The cursorpos does not
change.
If TYPE-TOO is non-NIL, STRING is output into the space and the
cursor is left after it."
  ;; Handle the number/character case separately
  ;; PMH 12/10/87 SPR#7018
  (if (numberp string)
      (progn
	(SHEET-INSERT-CHAR SHEET (SHEET-CHARACTER-WIDTH SHEET STRING (SHEET-CURRENT-FONT SHEET)) :pixel)
	(and type-too (sheet-tyo sheet string (sheet-current-font sheet) color)))
      (progn
	(SHEET-INSERT-CHAR SHEET (SHEET-STRING-LENGTH SHEET STRING START END) :PIXEL)
	(AND TYPE-TOO
	     (SHEET-STRING-OUT SHEET STRING START END color)))))

(DEFMETHOD (SHEET :DELETE-STRING) (STRING &OPTIONAL (START 0) END)
  (SHEET-DELETE-STRING SELF STRING START END))

(DEFUN SHEET-DELETE-STRING (SHEET STRING &OPTIONAL (START 0) END &AUX LEN)
  "Delete enough space for STRING, after SHEET's cursor.  The
following part of the line moves left, creating blank space at the end.
The cursor position does not change."
  (SETQ LEN
	(IF (NUMBERP STRING)
	  (SHEET-CHARACTER-WIDTH SHEET STRING (SHEET-CURRENT-FONT SHEET))
	  (SHEET-STRING-LENGTH SHEET STRING START END)))
  (SHEET-DELETE-CHAR SHEET LEN :PIXEL))

;;; Lozenge output.

(DEFCONSTANT *lozenge-corner-width*  4 "Width of rectangle bounding a triangular lozenge corner")
(DEFCONSTANT *lozenge-corner-height* 8 "Height of rectangle bounding a triangular lozenge corner")

;; Use font data for FONTS:|5X5|
(DEFCONSTANT *LOZENGE-FONT* FONTS:5X5)

(DEFCONSTANT *lozenge-char-height* (font-char-height *LOZENGE-FONT*)
  "Height of chars displayed inside a lozenge.")
(DEFCONSTANT *lozenge-char-width*  (font-char-width  *LOZENGE-FONT*)
  "Width of chars displayed inside a lozenge.")
(DEFCONSTANT *lozenge-char-base*   (font-baseline    *LOZENGE-FONT*)
  "Base line of chars displayed inside a lozenge.")

;;; Lozenge code really needs rewritten to handle an arbitrary font.  But for now....
(DEFPARAMETER *mac-lozenge-char-width* 3.5
  "Character width to use for drawing a Mac lozenge. May be fractional!")

(DEFMETHOD (SHEET :DISPLAY-LOZENGED-STRING) (STRING &optional color)
  (SHEET-DISPLAY-LOZENGED-STRING SELF STRING color))

(DEFUN SHEET-DISPLAY-LOZENGED-STRING (sheet string &optional color)
  "Display STRING on SHEET inside a lozenge.  This is how special
characters with no graphic or formatting meaning are output."  
  (MULTIPLE-VALUE-BIND (width corner-width corner-height)
      (LOZENGED-STRING-GEOMETRY string)
    
    ;; Make sure there is enough room on the line, if not CRLF and hope
    ;; the sheet isn't too narrow.  Relies on the fact that handling of
    ;; all exceptions leaves you no further to the right than you were
    ;; (usually at the left margin).
    (PREPARE-SHEET (sheet)
      (OR (ZEROP (SHEET-EXCEPTIONS sheet))
	  (SHEET-HANDLE-EXCEPTIONS sheet))
      (COND
	((> (+ (SHEET-CURSOR-X sheet) width)
	    (IF (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG sheet))
		(SHEET-INSIDE-RIGHT sheet)
		(- (SHEET-INSIDE-RIGHT sheet) (SHEET-CHAR-WIDTH sheet))))
	 (SEND sheet :END-OF-LINE-EXCEPTION)))
      (SETF (SHEET-CURSOR-X sheet)
	    (SHEET-DISPLAY-LOZENGED-STRING-INTERNAL
	      sheet string
	      (SHEET-CURSOR-X sheet)
	      (SHEET-CURSOR-Y sheet)
	      (SHEET-INSIDE-RIGHT sheet)
	      (SHEET-CHAR-ALUF sheet)
	      width
	      corner-width
	      corner-height
	      color)))))

(DEFUN SHEET-DISPLAY-LOZENGED-STRING-INTERNAL (sheet string x0 y0 xlim aluf
					       &optional width corner-width corner-height color)
  "The lozenge is actually displayed here.  First the characters 
inside the lozenge are displayed, then the outline of the 
lozenge is displayed. The final x position is returned."

  ;; Compute lozenge geometry if not already given.
  (UNLESS width
    (MULTIPLE-VALUE-SETQ (width corner-width corner-height)
      (LOZENGED-STRING-GEOMETRY string)))

  ;; Display the string  
  (SHEET-STRING-OUT-EXPLICIT-1
    sheet string
    (IF corner-width (+ x0 corner-width) x0)
    (IF corner-width
	(+ y0
	   (IF (mac-window-p sheet)
	       ;; Use top-y for Mac. Depend on whitespace above Monaco 6 characters
	       ;; to position character properly.
	       (TRUNCATE (- (SHEET-BASELINE sheet) corner-height) 2)
	        ;; For Explorer 5x5
	     (CEILING (- (SHEET-BASELINE sheet)       ;CEILING was previously a call to ROUND, PMH
			  *lozenge-char-height*)
		       2)))
      y0)
    nil nil
    (SEND (SHEET-GET-SCREEN sheet) :PARSE-FONT-DESCRIPTOR *LOZENGE-FONT*)
    aluf
    0 nil nil
    y0 color)

  ;; Draw the lozenge outline if necessary

  (WHEN corner-width
    (prepare-color (sheet color)
    (LET* ((line-start-x (+ x0 corner-width))
	   (line-end-x   (+ line-start-x (- width corner-width corner-width)))
	   (end-x        (+ x0 width))
	   (top-y        (+ y0 (TRUNCATE (- (SHEET-BASELINE sheet) corner-height) 2)))
	   (mid-y        (+ top-y (TRUNCATE corner-height 2)))
	   (bottom-y     (+ top-y corner-height))
	   (xlim         (OR xlim (SHEET-INSIDE-RIGHT sheet))))

      ;; lozenges that were displayed in the label were not getting the lines drawn
      ;; change this to use the outside edges of sheet.  PMH for KK
      (WITH-CLIPPING-RECTANGLE ((SHEET-INSIDE-LEFT sheet) 0 ;; 0 for top allows labels in top margin
				xlim (sheet-height sheet)   ;; full height allows label on bottom
				)
	(%DRAW-LINE (1+ line-start-x) top-y    line-end-x   top-y    aluf t   sheet)	;Top line
	(%DRAW-LINE (1+ line-start-x) bottom-y line-end-x   bottom-y aluf t   sheet)	;Bottom line
	(%DRAW-LINE line-start-x      top-y    x0           mid-y    aluf t   sheet)	;Left corner
	(%DRAW-LINE x0                mid-y    line-start-x bottom-y aluf t   sheet)
	(%DRAW-LINE line-end-x        top-y    end-x        mid-y    aluf t   sheet)	;Right corner
	(%DRAW-LINE end-x             mid-y    line-end-x   bottom-y aluf t   sheet)))))

  ;; Return final x position
  (+ x0 width))

(DEFUN lozenged-special-char-name (char)
  "Return the string used to represent a special char inside a lozenge."
  (STRING
    (OR (CAR (RASSOC char si:xr-special-character-names :test #'CHAR=))
      ;; If there is no name for the character
      ;; then print out its octal character
      ;; number.
      (FORMAT nil "~3o" char))))

(DEFUN lozenged-string-geometry (string)
  "Return geometry used to draw the given string as a lozenged string:
total width (including lozenge), lozenge corner width,  and lozenge 
corner height. 

The lozenge corner dimensions define the size of the triangular corner 
drawn on each end of the lozenge and describe the size of a rectangle 
enclosing a corner.

STRING may be any object convertible to a string via the STRING function.

In the case where STRING contains only special chars, no lozenge needs to
be drawn, and NIL is returned for the lozenge corner width."
  (LET (c
	(contains-graphic-chars nil)
	(string-width 0)
	(lozenge-char-width (IF (mac-screen-p default-screen)
				*mac-lozenge-char-width*
			      *lozenge-char-width*)))
    (VALUES
      ;; Total width (string plus lozenge corners)
      (CEILING	    ;Handle possible fractional character widths for Mac
	(+ (DOTIMES (i (LENGTH (SETQ string (STRING string))) string-width)      
	     (INCF string-width
		   (IF (AND (GRAPHIC-CHAR-P (SETQ c (ELT string i)))
			    (SETQ contains-graphic-chars t))
		       
		       ;; Increment by ordinary char width
		       lozenge-char-width
		     
		     ;; Else special character. Increment by width of lozenged character name.
		     (lozenged-string-geometry (lozenged-special-char-name c)))))
	   
	   (IF contains-graphic-chars		       ;Add in corners unless all special chars
	       (+ *lozenge-corner-width* *lozenge-corner-width*)
	     0)))

      ;; Lozenge corner width (unless all special chars)
      (AND contains-graphic-chars *lozenge-corner-width*)

      ;; Lozenge corner height
      *lozenge-corner-height*)))

(DEFMETHOD (SHEET :TYO) (CH &OPTIONAL FONT color)
  (SHEET-TYO SELF CH FONT color))

(DEFUN SHEET-TYO (SHEET CHAR &OPTIONAL FONT color &AUX BASE-ADJ)
  "Output printing or formatting character CHAR on SHEET in FONT.
FONT defaults to the current font of SHEET.  Non-printing characters
are printed in lozenges."
  (declare (inline graphic-char-p))
  ;; Coerce to one data type.
  (SETQ CHAR (CHAR-INT CHAR))
;;;  (CHECK-ARG CHAR (AND (INTEGERP CHAR) (>= CHAR 0) (< CHAR 256)) "a character") ; Commented out by TWE on 4-JUNE-86
  (IF (OR (MINUSP CHAR) (NOT (GRAPHIC-CHAR-P CHAR)))
    (COND
      ((AND (= CHAR #\NEWLINE)
	    (ZEROP (SHEET-CR-NOT-NEWLINE-FLAG SHEET))) (SHEET-CRLF SHEET))
      ((= CHAR #\TAB)
       (SHEET-TAB-1 SHEET))
      ((AND (= CHAR #\BACKSPACE)
	    (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
       (SHEET-BACKSPACE-1 SHEET))
      ((PLUSP (CHAR-BITS CHAR))
       ;; Print out the control-meta etc. characters as a lozenge.
       (SHEET-DISPLAY-LOZENGED-STRING
         SHEET
         (STRING-APPEND (NTH (CHAR-BITS CHAR)
                             '("" "CONTROL-" "META-" "CONTROL-META-"
                               "SUPER-" "CONTROL-SUPER-" "META-SUPER-" "CONTROL-META-SUPER-"
                               "HYPER-" "CONTROL-HYPER-" "META-HYPER-" "CONTROL-META-HYPER-"
                               "SUPER-HYPER-" "CONTROL-SUPER-HYPER-" "META-SUPER-HYPER-" "CONTROL-META-SUPER-HYPER-"))
                        (STRING (CHARACTER (CHAR-CODE CHAR)))) color ))
      (T
       ;; We can't decide how to print the character; print out a lozenge of something.
       ;; If it has a print-name use that, otherwise just use the octal number.
       (SHEET-DISPLAY-LOZENGED-STRING
	 SHEET (STRING (OR (CAR (RASSOC CHAR SI:XR-SPECIAL-CHARACTER-NAMES :TEST #'EQUAL))
			   (FORMAT NIL "~o" CHAR))) color )))
    ;;ELSE
    (prepare-color (sheet color)  ;;; this sets up the color once, so inside here we need not pass color as an optional argument...
    (PREPARE-SHEET (SHEET)
      (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	  (SHEET-HANDLE-EXCEPTIONS SHEET))

      ;; If no font argument and char is "fat", use font specified by font bits.
      ;; Caveat: if (ZEROP (CHAR-FONT CHAR)), use current font, not font 0!!
      (UNLESS (OR FONT (ZEROP (CHAR-FONT CHAR)))
	(SETQ FONT (ELT (SHEET-FONT-MAP SHEET) (CHAR-FONT CHAR))))

       (IF FONT
	 (PROGN
	   (COERCE-FONT FONT SHEET)
	   (SETQ BASE-ADJ (- (SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
	 (SETQ FONT (SHEET-CURRENT-FONT SHEET)
	       BASE-ADJ (SHEET-BASELINE-ADJ SHEET)))
       (LET* ((CHAR (CHAR-CODE CHAR))
	      (CHAR-WIDTHS (FONT-CHAR-WIDTH-TABLE FONT))
	      (FIT (FONT-INDEXING-TABLE FONT))
	      (WIDTH)
	      (KERN 0)
	      (KERN-TABLE)
	      (XPOS (SHEET-CURSOR-X SHEET))
	      (RIGHT-LIM (SHEET-INSIDE-RIGHT SHEET)))
	 (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
	     (SETQ RIGHT-LIM (- RIGHT-LIM (SHEET-CHAR-WIDTH SHEET))))
	 (SETQ WIDTH (IF CHAR-WIDTHS
		      ;; Handle variable width fonts.
		       (AREF CHAR-WIDTHS CHAR)
		       (FONT-CHAR-WIDTH FONT)))
	 (COND
	   ((> (+ XPOS WIDTH) RIGHT-LIM)
	    (SEND SHEET :END-OF-LINE-EXCEPTION)
	    (SHEET-TYO SHEET CHAR FONT)); Call us again.
	   (T
	    (AND (SETQ KERN-TABLE (FONT-LEFT-KERN-TABLE FONT))
		 (SETQ KERN (AREF KERN-TABLE CHAR)))
	    (COND
	      ((NULL FIT)
	       (%DRAW-CHAR FONT CHAR (- XPOS KERN) (+ (SHEET-CURSOR-Y SHEET) BASE-ADJ)
			   (SHEET-CHAR-ALUF SHEET) SHEET))
	      ;; Wide character, draw several columns.
	      (T
	       (DRAW-CHAR FONT CHAR (- XPOS KERN) (+ (SHEET-CURSOR-Y SHEET) BASE-ADJ)
			  (SHEET-CHAR-ALUF SHEET) SHEET)))
	    (SETF (SHEET-CURSOR-X SHEET) (+ XPOS WIDTH))))))))
  CHAR)

(DEFMETHOD (SHEET :BACKWARD-CHAR) (&OPTIONAL CHAR)
  (SHEET-BACKSPACE-1 SELF CHAR))

(DEFUN SHEET-BACKSPACE-1 (SHEET &OPTIONAL CHAR)
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-INCREMENT-BITPOS
      SHEET
      (- (IF CHAR
             (SHEET-CHARACTER-WIDTH SHEET CHAR (SHEET-CURRENT-FONT SHEET))
           (SHEET-CHAR-WIDTH SHEET)))
      0)))

(DEFUN SHEET-TAB-1 (SHEET)
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (LET ((TAB-WIDTH (SHEET-TAB-WIDTH SHEET)))
      (IF (NOT (ZEROP TAB-WIDTH))		; fix for zero tab width problem (dls).
	  (SHEET-INCREMENT-BITPOS SHEET		; fix for zero tab width problem (dls).
	    (- TAB-WIDTH			; fix for zero tab width problem (dls).
	       (REM (- (SHEET-CURSOR-X SHEET)	; fix for zero tab width problem (dls).
			     (SHEET-INSIDE-LEFT SHEET))	; fix for zero tab width problem (dls).
			  TAB-WIDTH))		; fix for zero tab width problem (dls).
	    0)))))				; fix for zero tab width problem (dls).

(DEFMETHOD (SHEET :FORWARD-CHAR) (&OPTIONAL CHAR)
  (SHEET-SPACE SELF CHAR))

(DEFUN SHEET-SPACE (SHEET &OPTIONAL CHAR)
  "Move SHEET's cursor forward one character position.  Will move to
a new line if necessary.
If CHAR is specified, that character's width is the distance to move."
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-INCREMENT-BITPOS
      SHEET
      (IF CHAR
          (SHEET-CHARACTER-WIDTH SHEET CHAR (SHEET-CURRENT-FONT SHEET))
        (SHEET-CHAR-WIDTH SHEET))
      0)))

(DEFMETHOD (SHEET :STRING-OUT) (STRING &OPTIONAL (START 0) END color)
  (SHEET-STRING-OUT SELF STRING START END color))


(DEFUN SHEET-STRING-OUT (SHEET STRING &OPTIONAL START END color)
  "Print STRING on SHEET
   Optional starting and ending indicies may be supplied.
   The default is to output the whole string."
  (declare (inline graphic-char-p))
  (prepare-color (sheet color)
  (PREPARE-SHEET (SHEET)
    (WHEN (SYMBOLP STRING)		  ;Convert symbols to strings for output
      (SETQ STRING (SYMBOL-NAME STRING)))
    (LET* ((INDEX (OR START 0))
	   (END (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	   (FONT (SHEET-CURRENT-FONT SHEET))
	   (BASELINE-ADJUST (SHEET-BASELINE-ADJ SHEET))
	   (ALUF (SHEET-CHAR-ALUF SHEET))
	   (XLIM (SHEET-inside-right SHEET))	;used to be (sheet-width...
	   CH XPOS)
      (UNLESS (ZEROP (SHEET-EXCEPTIONS SHEET))	;End of page, MORE
	  (SHEET-HANDLE-EXCEPTIONS SHEET))
      (WHEN (PLUSP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
        ;; Ensure enough room for ! at right of screen.
	(DECF XLIM (FONT-CHAR-WIDTH FONT)))
      (LOOP WHILE (< INDEX END) DOING
	    (MULTIPLE-VALUE-SETQ (XPOS INDEX)
	      
	      (DRAW-STRING-INTERNAL SHEET STRING INDEX END
			    (SHEET-CURSOR-X SHEET) (+ (SHEET-CURSOR-Y SHEET)
						      BASELINE-ADJUST)
			    XLIM FONT ALUF))
	    (SETF (SHEET-CURSOR-X SHEET) XPOS)
	    (unless index (return nil))		;Done
	    (COND ((NOT (GRAPHIC-CHAR-P (SETQ CH (AREF STRING INDEX))))	;Special character?
		   (SHEET-TYO SHEET ch)
		   (INCF INDEX))
		  (T (SETF (SHEET-CURSOR-X SHEET) XPOS)			;Else end of line
		     (SEND SHEET :END-OF-LINE-EXCEPTION))))))))


(DEFUN DRAW-CHAR (FONT CH X Y ALU WINDOW &optional color)
  "Similar to %draw-char but handles fonts with index tables
 for characters wider than 32 pixels."
  (COERCE-FONT FONT WINDOW)
  (IF (mac-window-p window)
      (send-draw-char font ch x y alu window)
      (progn
	;; Only do prepare-color if a color is specified (for performance reasons) and if WINDOW
	;; arg is a sheet and not an array.  Fixes SPR #8202.  07/11/88 KJF.
	(IF (AND color (TYPEP window 'sheet))
	    (prepare-color (window color)
			   (draw-char-internal FONT CH X Y ALU WINDOW (FONT-INDEXING-TABLE FONT)))
	    (draw-char-internal FONT CH X Y ALU WINDOW (FONT-INDEXING-TABLE FONT))))))

(DEFUN draw-string-internal (window string index end xpos ypos xlim font alu &optional color)
  "Draw characters indexed in STRING from INDEX up to but not including END on WINDOW 
 at XPOS YPOS using FONT and ALU.  Returns the x-position for the next character to be 
 drawn.  A second value is returned if the end index is not reached.  It is the index of the
 first character not drawn, which can occur if the character overlaps xlim or is a non
 graphic character."
  (declare (inline graphic-char-p))
  (DECLARE (values new-xpos ending-index))
  
  (prepare-color (window color)
    (LET* ((sys:clipping-rectangle-right-edge xlim)	; bind now for use in NUM
	   (mac-window (mac-window-p window))
	   (num (IF mac-window
		    (send-draw-string window string index end xpos ypos font alu)
		  (si:%draw-string font string index end xpos ypos alu window))))	;If not implemented -> NIL
      (IF num
	  ;; Microcoded draw-string worked...just return results
	  (VALUES (LDB (BYTE 12. 0) num)	; new xpos
		  ;; Fixed to handle %draw-string returning index relative to start...ab 1/7/88
		  (AND (< (INCF index (LDB (byte 12. 12.) num)) end)	;ab 1/7/88
		       index))
	;; ...else must do it in LISP
	(LET ((fwt (FONT-CHAR-WIDTH-TABLE font))
	      (lkt (FONT-LEFT-KERN-TABLE font))
	      (fit (IF (and mac-window (EQ (font-indexing-table font) t))
		       nil
		     (FONT-INDEXING-TABLE font)))
	      (wid (FONT-CHAR-WIDTH font))
	      ch)
	  (IF (OR fwt lkt fit)
	      ;; ...then hard case of large, variable-width, or kerned font
	      (DO ()
		  (())
		(OR (< index end)
		    (RETURN xpos))		; reached the end
		(OR (graphic-char-p
		      (SETQ ch (CHAR-CODE (AREF string index))))	; use char code for art-q strings.
		    (RETURN xpos index))	; non printing char
		(OR (draw-char-internal
		      font ch (IF lkt (- xpos (AREF lkt ch)) xpos) ypos alu window fit)
		    (RETURN xpos index))	; clipped 
		(INCF xpos (IF fwt (AREF fwt ch) wid))
		(INCF index))
	    ;; ...else easy case of small, fixed-width font.  Speed hack KED 2/25/87
	    (DO ()
		(())					
	      (OR (< index end)
		  (RETURN xpos))		; reached the end
	      (OR (graphic-char-p (SETQ ch (CHAR-CODE (AREF string index)))) 	
		  (RETURN xpos index))		; non printing char
	      (OR (si:%draw-char font ch xpos ypos alu window)
		  (RETURN xpos index))		; clipped 
	      (INCF xpos wid)
	      (INCF index))))))))

(DEFMETHOD (SHEET :FAT-STRING-OUT) (STRING &OPTIONAL (START 0) END color)
  (SHEET-FAT-STRING-OUT SELF STRING START END color))

(DEFUN SHEET-FAT-STRING-OUT (SHEET STRING &OPTIONAL (START 0) END color)
  (DO ((INDEX START)
       (STOP (OR END (LENGTH STRING)))
       (XPOS))
      ((>= INDEX STOP))
    ;; Need to hold interrupts until the new cursor position is set.
    ;; SPR #7008    PMH 12/14/87
    (without-interrupts
      (prepare-color (sheet color)
      (SETF (VALUES INDEX XPOS) (SHEET-LINE-OUT SHEET STRING INDEX END))) ;;; do it this way rather than call with color option
    (SETF (SHEET-CURSOR-X SHEET) XPOS))
    (WHEN (> INDEX STOP)
      (SHEET-INCREMENT-BITPOS SHEET 0 0)
      (RETURN NIL))
    (SHEET-CRLF SHEET)))

;;; Editor's line redisplay primitive, output STRING from START to END,
;;; first setting position to (SET-XPOS,SET-YPOS) and doing a clear-eol
;;; DWIDTH is a special hack for DIS-LINE redisplay of italic fonts, it
;;; means draw an extra character starting one character back, since the
;;; clear-eol will have erased part of the last character where it
;;; sticks out past its width.  (If this can really happen, it's going
;;; to mean trouble with the margins, too!) This function never does
;;; more than one line; it stops rather than wrapping around.  If you
;;; put a carriage return in the string, above may not be true.  Where
;;; this leaves the sheet's actual cursorpos is undefined (somewhere on
;;; the line).
;;; This is NOT related to the :LINE-OUT operation!

;;; The following mod made 12/28/87  PMH for LG
;;; This is a new version of the old tv:sheet-line-out. The old version broke the string up into
;;; individual characters and did %draw-char. This produces unacceptable command interface overheads
;;; when going to the Mac. This new version breaks the string up into chunks (of the same font) and
;;; does draw-string-internal. Since we found the average string length passed to sheet-line-out
;;; was 42 and font changes to be very rare this new version produces a very significant speedup on
;;; the Mac (and probably would on the Explorer as well if the microcoded draw-string-internal were turned on).

(DEFUN sheet-line-out (sheet string &optional (start 0) (end nil) set-xpos set-ypos dwidth color)
  "New version of sheet line out which uses draw-string-internal.
Returns the index of the next character to be printed and where the cursor got to.
The first value may be incremented indicating that the line was completed."
  (declare (inline graphic-char-p))
  
  (LET ((x (IF set-xpos
	       (MIN (+ set-xpos (sheet-inside-left sheet)) (sheet-inside-right sheet))
	     (sheet-cursor-x sheet)))
	(y (IF set-ypos
	       (MIN (+ set-ypos (sheet-inside-top sheet)) (sheet-inside-bottom sheet))
	     (sheet-cursor-y sheet)))
	(margin-flag (NOT (ZEROP (sheet-right-margin-character-flag sheet)))))

    (prepare-color (sheet color)    
    (prepare-sheet (sheet)
      
      ;; Setting cursor position
      (SETF (sheet-cursor-x sheet) x)
      (IF (sheet-more-vpos sheet)
	  (SETF (sheet-more-vpos sheet) (sheet-deduce-more-vpos sheet)))
      (SETF (sheet-cursor-y sheet) y)
      (WHEN set-ypos
	(SETF (sheet-exceptions sheet) 0)
	(WHEN (> (+ y (sheet-line-height sheet)) (sheet-inside-bottom sheet))
	  (SETF (sheet-end-page-flag sheet) 1)))
      (OR (ZEROP (sheet-exceptions sheet))
	  (sheet-handle-exceptions sheet))
      ;; If we set the cursor then do a clear to end of line.
      (AND (OR set-xpos set-ypos)
	   (%draw-rectangle
	     (- (sheet-inside-right sheet) x)
	     (sheet-line-height sheet)
	     (sheet-cursor-x sheet) (sheet-cursor-y sheet) 
	     (sheet-erase-aluf sheet) sheet))
       ;; If special case of italic line, move back and decrement
       ;; starting index.
       (WHEN (AND DWIDTH (NOT (ZEROP start)))
	 (SETF (sheet-cursor-x sheet) (DECF x dwidth))
	 (DECF start))
       
      ;; Drawing the string      
      (LOOP with index = start
	    with end = (OR end (LENGTH string))
	    with right-edge = (- (sheet-inside-right sheet)
				 (IF margin-flag (sheet-char-width sheet) 0))
	    with font-map = (sheet-font-map sheet)
	    with alu = (sheet-char-aluf sheet)
	    until (>= index end) 
	    for current-font-number = (CHAR-FONT (AREF string index))
	    for current-font = (AREF font-map
				     (IF (>= current-font-number (ARRAY-ACTIVE-LENGTH font-map))
					 0
				       current-font-number))
	    for baseline-adjust = (- (sheet-baseline sheet) (font-baseline current-font))
	    for font-change-index = (LOOP for i from index below end
					  when (neq (char-font (aref string i))
						    current-font-number)
					  return i
					  finally (return end))
	    do
	    (MULTIPLE-VALUE-BIND (new-x new-end)
		(draw-string-internal sheet
				      string
				      index
				      font-change-index
				      x (+ y baseline-adjust)
				      right-edge
				      current-font
				      alu)
	      (IF new-end
		  ;;Then all characters didn't get printed 
		  (IF (GRAPHIC-CHAR-P (AREF string new-end))
		      ;; Then last character must have overlapped XLIM, so print margin character and get out.
		      (PROGN
			(WHEN margin-flag
			  (SETF (sheet-cursor-x sheet) new-x)
			  (SEND sheet :tyo-right-margin-character new-x y #\!))
			(RETURN (IF (ZEROP (sheet-truncate-line-out-flag sheet))
				    new-end
				  (1+ end))
				(- (sheet-cursor-x sheet) (sheet-inside-left sheet))))	
		    ;; Last character was non-graphic, let TYO handle it and go try to do the remainder of the string
		    (SETF (sheet-cursor-x sheet) new-x)
		    (sheet-tyo sheet (AREF string new-end) current-font) 
		    (SETF index (1+ new-end))
		    (SETF x (sheet-cursor-x sheet)
			  y (sheet-cursor-y sheet)))
		;; Else they were all printed, go do rest of string
		(SETF index font-change-index)
		(SETF x new-x)))
	    finally
	    (SETF (sheet-cursor-x sheet) (- x (sheet-inside-left sheet)))
	    (RETURN (1+ index) (- x (sheet-inside-left sheet))))))))



(DEFMETHOD (SHEET :COMPUTE-MOTION) (STRING &OPTIONAL (START 0) END X Y &REST ARGS)
  (APPLY 'SHEET-COMPUTE-MOTION SELF X Y STRING START END ARGS))

;;; The following variable is read by the string-width microcode function to make fixed-width fonts work like
;;; variable width fonts.  This table is passed as a font-char-width-table and then the result (the number of
;;; characters processed) is multiplied by the width of the fixed with font to yield the width of the string.
;;; The gap in the middle is for the special characters which string-width isn't supposed to handle, and are
;;; handled elsewhere.  If a font is variable width then this table isn't used at all.
(DEFVAR PRINTING-CHARACTER-TRANSLATE-TABLE
	(MAKE-ARRAY 256. :INITIAL-CONTENTS (LOOP FOR CHAR FROM 0 BELOW 256.
						 COLLECTING (IF (<= 128. CHAR 159.) NIL 1))))

;;; Change to T to fix the bug that we stop after a character that goes
;;; across STOP-X though we ought to stop before it.  Should not turn on
;;; the fix until the callers are changed to match.  If indeed the
;;; change really should be made.
(DEFVAR COMPUTE-MOTION-ROUND-DOWN NIL)

(DEFUN SHEET-COMPUTE-MOTION (SHEET X Y STRING &OPTIONAL (START 0) (END NIL)
			     (CR-AT-END-P NIL) (STOP-X 0) (STOP-Y NIL)
			     BOTTOM-LIMIT RIGHT-LIMIT FONT
			     (LINE-HT (IF FONT
					  (FONT-CHAR-HEIGHT FONT)
					  (SHEET-LINE-HEIGHT SHEET)))
			     (TAB-WIDTH
			       (IF FONT
				   (* (FONT-CHAR-WIDTH FONT) (SHEET-TAB-NCHARS SHEET))
				   (SHEET-TAB-WIDTH SHEET))))
  "Compute the motion that would be caused by outputing a string.
This is used by the editor and by TV:STREAM-MIXIN.  In computing the
motion, it will chose the font in one of two ways:
 IF given an ART-FAT-STRING array (16 bit string) like the editor
	uses, it will take the font from the CHAR-FONT field (high 8
	bits) of the character, and look in SHEET's font-map.
 IF given an ART-STRING array (8 bit string), it will take the font
	from FONT, or the SHEET-CURRENT-FONT of the sheet.

SHEET is used to supply information such as the font map, and for
	defaulting such things as BOTTOM-LIMIT, RIGHT-LIMIT and
	LINE-HT.
STRING, with START and END, specifies what characters to process.
CR-AT-END-P if non-NIL says OUTPUT a Return after STRING or the
	portion of STRING, and count that in the cursor motion.
STOP-X and STOP-Y specify a cursor position at which to stop.
	Processing stops when both coordinates are >= the stop points.
	The stop points default to the bottom left corner of SHEET.
	Specify a very large value for STOP-Y if you do not want
	processing to stop before the end of STRING.
BOTTOM-LIMIT and RIGHT-LIMIT are a cursor position at which to
	wrap around; these default to the inside-size of SHEET.  Use
	SHEET-STRING-LENGTH if you do not want wrap-around.
FONT specifies the font to use, if STRING is not a fat string.
LINE-HT is the line height to use for Return characters, defaulting to
	SHEET's line height.
TAB-WIDTH is the width to use for Tab characters, defaulting to
	SHEET's SHEET-TAB-WIDTH.

Processing stops either because the string or portion has been
processed or because the stopping-point has been reached.

Returns 4 values:
FINAL-X, FINAL-Y are the cursor position at which processing stopped.
FINAL-STRING-INDEX is the index in the string at which processing
	stopped (could be the length of the string, if the stop point
	was passed then), T if stopped due to reaching the stop point
	after the additional Return, or NIL if stopped due to finishing.
MAXIMUM-X was the largest X-position ever encountered during
	processing."
  ;;; *** The interface to this should be redesigned.  Also note
  ;;; *** that the exact treatment of STOP-X and STOP-Y does not agree
  ;;; *** with SHEET-STRING-LENGTH.  This is what turning on
  ;;; *** COMPUTE-MOTION-ROUND-DOWN is going to fix.
  (declare (inline graphic-char-p))
  (DECLARE (VALUES FINAL-X FINAL-Y FINAL-STRING-INDEX MAXIMUM-X))
  (IF FONT
    (COERCE-FONT FONT SHEET)
    (SETQ FONT (SHEET-CURRENT-FONT SHEET)))
  (PROG (CWA CW CH FONTX TEM I N NN II MARGIN-FLAG MAXIMUM-X OLD-X)
    (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
	(SETQ MARGIN-FLAG T))
    (AND (NULL X)
	 (SETQ X (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))))
    (AND (NULL Y)
	 (SETQ Y (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))))
    (AND (NULL STOP-Y)
	 (SETQ STOP-Y (1+ (SHEET-INSIDE-HEIGHT SHEET))))
                   ;   ^-- This 1+ is so can use >= rather than >
    (OR RIGHT-LIMIT
	(SETQ RIGHT-LIMIT (SHEET-INSIDE-WIDTH SHEET)))
    (AND MARGIN-FLAG
	 (SETQ RIGHT-LIMIT (- RIGHT-LIMIT (SHEET-CHAR-WIDTH SHEET))))
    (AND (NULL BOTTOM-LIMIT)
	 (SETQ BOTTOM-LIMIT (- (SHEET-INSIDE-HEIGHT SHEET) LINE-HT)))
    (SETQ MAXIMUM-X X
	  I START
	  N (OR END (ARRAY-ACTIVE-LENGTH STRING))
	  CW (FONT-CHAR-WIDTH FONT))
    ;; At this point, decide whether we can use the fast version.
    (COND
     ;; If FONTX is non-NIL, then we have a string with font changes.
     ;;      ((NEQ 8 (CDR (ASSQ (ARRAY-TYPE STRING) ARRAY-BITS-PER-ELEMENT)))
     ((EQ (ARRAY-TYPE STRING) 'ART-FAT-STRING) (SETQ FONTX T))
     ;; The current font is variable width.
     ((SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)))
     ;; No font changes and the current font is fixed width.  We can
     ;; use the fast version.
     (T (GO FAST)))
    ;;This is the slow version.
 SLOW
    (SETQ MAXIMUM-X (MAX X MAXIMUM-X))
    (COND
      ((AND (>= Y STOP-Y)
	    (>= X STOP-X))	; Reached sticking-point.
       (RETURN X Y I MAXIMUM-X))
      ((NOT (< I N))	; If string exhausted.
       (COND
	 (CR-AT-END-P (SETQ X 0
			    Y (+ Y LINE-HT)); CRLF if told to.
	  (AND (> Y BOTTOM-LIMIT)
	       (SETQ Y 0))))
       (RETURN X Y (AND (>= X STOP-X)
			(>= Y STOP-Y))
	       MAXIMUM-X)))
    ;; Move quickly over the remaining characters until we reach an
    ;; x-position at which something must be done.
    (UNLESS (EQ FONTX T)
      (LET (WIDTH-INCR
	    (LIMIT (MIN RIGHT-LIMIT (IF (>= Y STOP-Y)
				      STOP-X
				      RIGHT-LIMIT))))
	(SETQ WIDTH-INCR
	      (%STRING-WIDTH (OR CWA PRINTING-CHARACTER-TRANSLATE-TABLE)
	       (IF FONTX 
                   (CODE-CHAR 0 0 FONTX)
                   ;;(DPB FONTX %%CH-FONT 0)
                   ;;ELSE
                   0)
	       STRING I N (IF CWA
			    (- LIMIT X)
			    (FLOOR (- LIMIT X) CW))))
	(UNLESS CWA
	  (SETQ WIDTH-INCR (* WIDTH-INCR CW)))
	(SETQ I (%POP))
	;; increment positions.
	(SETQ X (+ WIDTH-INCR X))
	;; At end of string, loop back, to exit.
	(IF (= I N)
	  (GO SLOW)))) 
	;; Otherwise we stopped due to funny char or font change or
	;; reaching the X limit.
    (SETQ MAXIMUM-X (MAX X MAXIMUM-X))
    (COND
      ((AND (>= Y STOP-Y)
	    (>= X STOP-X))	;If reached sticking-point, done.
       (RETURN X Y I MAXIMUM-X)))
    (SETQ CH (CHAR-CODE (SETQ TEM (AREF STRING I))))
    (COND
      ((AND FONTX
	    (NEQ (SETQ TEM (CHAR-FONT TEM)) FONTX))	;Changing fonts
       (SETQ FONTX TEM
	     FONT
	     (LET ((FONT-MAP (SHEET-FONT-MAP SHEET)))
	       (AREF FONT-MAP (IF (>= FONTX (ARRAY-ACTIVE-LENGTH FONT-MAP))
				0
				FONTX)))
	     CWA (FONT-CHAR-WIDTH-TABLE FONT)
	     CW (FONT-CHAR-WIDTH FONT))))
    (SETQ OLD-X X)
    ;; Try to do this one char.
    (COND
      ((= CH #\NEWLINE) (SETQ X 0
			      Y (+ Y LINE-HT))
                        (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
      ((GRAPHIC-CHAR-P CH)				; Printing character.
       (SETQ X (+ (COND
		    (CWA (AREF CWA CH))
		    (T CW))
		  X)))				; Do char width.
      ;; Tab (have to do here since X-dependent).
      ((= CH #\TAB)
       (IF (NOT (ZEROP (SETQ TEM TAB-WIDTH)))			; fix for zero tab width problem (dls).
	   (SETQ X (* (TRUNCATE (+ X TEM) TEM) TEM))))		; fix for zero tab width problem (dls).
      (T				      ;Format effector
       (SETQ X (MAX (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)) 0))))
    ;; If this character went past the stop-point, pretend we stopped
    ;; before it.
    (WHEN (AND COMPUTE-MOTION-ROUND-DOWN (> X STOP-X) (>= Y STOP-Y))
      (RETURN OLD-X Y I MAXIMUM-X))
    ;; If this character went past the right margin, do a CR, then redo
    ;; this character.
    (COND
      ((> X RIGHT-LIMIT)
       (SETQ X 0
	     Y (+ Y LINE-HT)) (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
      (T (SETQ I (1+ I))))
    (GO SLOW)
    ;; Here is the fast loop.  The basic idea is to scan as fast as
    ;; possible over printing characters, with all checking outside the
    ;; loop.
 FAST
    (SETQ MAXIMUM-X (MAX X MAXIMUM-X))
    ;; First, decide the most characters we want to scan over in a whack.
    (SETQ NN
	  ;; Stop-point is in this line.
	  (MIN (+ (TRUNCATE (- (COND
				 ((>= Y STOP-Y) STOP-X)
				 ;; Stop for this line is the margin.
				 (T RIGHT-LIMIT))
			       X) CW) I)
	       ;; NN is limiting value of I.
	       N))				;Now, scan over printing characters.
    (AND (>= (SETQ II I) NN)			;Save initial I, and check for null loop
	 (GO SCX))
 SCN
    (%STRING-WIDTH PRINTING-CHARACTER-TRANSLATE-TABLE 0 STRING II NN NIL)
    (SETQ I (%POP))
    (SETQ X (+ (* (- I II) CW) X))		;Account for the motion of those chars
 SCX
    (SETQ NN X)
    (SETQ MAXIMUM-X (MAX X MAXIMUM-X))
    (COND
      ((AND (>= Y STOP-Y)
	    (>= X STOP-X))			;If reached sticking-point, done.
       (RETURN X Y I MAXIMUM-X))
      ((>= I N)					;If string exhausted THEN
       (COND
	 (CR-AT-END-P				; Do return X off end of line
	  (SETQ X 0				; crlf if told to.
		Y (+ Y LINE-HT)) (AND (> Y BOTTOM-LIMIT) (SETQ Y 0))))
       (RETURN X Y (AND (>= X STOP-X)
			(>= Y STOP-Y))
	       MAXIMUM-X)))
    (SETQ OLD-X X)
    ;; Try to do this one char.
    (COND
      ((= (SETQ CH (AREF STRING I)) #\NEWLINE)
       (SETQ X 0
	     Y (+ Y LINE-HT))
       (AND (> Y BOTTOM-LIMIT)
	    (SETQ Y 0)))
      ((GRAPHIC-CHAR-P CH)				;Printing character
       (SETQ X (+ CW X)))
      ((= CH #\TAB)					;Tab (have to do here since X-dependent)
       (IF (NOT (ZEROP (SETQ TEM TAB-WIDTH)))		; fix for zero tab width problem (dls).
	   (SETQ X (* (TRUNCATE (+ X TEM) TEM) TEM))))	; fix for zero tab width problem (dls).
      (T			    ;Format effector
       (SETQ X (MAX (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)) 0))))
    ;; If this character went past the stop-point, pretend we stopped
    ;; before it.
    (WHEN (AND COMPUTE-MOTION-ROUND-DOWN
	       (> X STOP-X)
	       (>= Y STOP-Y))
      (RETURN OLD-X Y I MAXIMUM-X))
    ;; If this character went past the right margin, do a CR and then
    ;; redo this character.
    (COND
      ((> X RIGHT-LIMIT)
       (SETQ X 0
	     Y (+ Y LINE-HT))
       (AND (> Y BOTTOM-LIMIT)
	    (SETQ Y 0)))
      (T (SETQ I (1+ I))))
    (GO FAST)))

(DEFMETHOD (SHEET :CHARACTER-WIDTH) (CHAR &OPTIONAL (FONT CURRENT-FONT))
  (SHEET-CHARACTER-WIDTH SELF CHAR
			 (IF (FONT-OBJECT-P FONT)
			     FONT
			   (SEND (SHEET-GET-SCREEN SELF)
				    :PARSE-FONT-DESCRIPTOR FONT))))

(DEFUN SHEET-CHARACTER-WIDTH (SHEET CH FONT &AUX TEM)
  "Returns the width of a character, in raster units.
For backspace, it can return a negative number.
For tab, the number returned depends on the current cursor position.
For return, the result is zero.
CH can be NIL; then you get the font's character-width."
  (COERCE-FONT FONT SHEET)
  (COND
    ((NULL CH) (FONT-CHAR-WIDTH FONT))
    ((GRAPHIC-CHAR-P CH)			;Ordinary printing character
     (COND
       ((SETQ TEM (FONT-CHAR-WIDTH-TABLE FONT))
	(AREF TEM CH))
       (T (FONT-CHAR-WIDTH FONT))))
    ((= CH #\NEWLINE) 0)			;Return
    ((= CH #\TAB)				;TAB
     (LET ((relative-x-offset (- (SHEET-CURSOR-X SHEET) (sheet-inside-left sheet))))  ; may 01/18/89 
       (IF (ZEROP (SETQ TEM (SHEET-TAB-WIDTH SHEET)))	; fix for zero tab width problem (dls).
	   0	;; prevent divide by 0
	   (- tem (rem relative-x-offset tem))))) ; may 01/18/89 
    ((AND (= CH #\BACKSPACE)
	  (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
     (- (SHEET-CHAR-WIDTH SHEET)))		;Backspace
    (T						;Misc lozenge character
     (LOZENGED-STRING-GEOMETRY (CODE-CHAR CH)))))

(DEFMETHOD (SHEET :STRING-LENGTH) (&REST ARGS)
  (APPLY 'SHEET-STRING-LENGTH SELF ARGS))

;;;replaced previous definition for DLS by PDC on 10 Jan 86
(DEFUN SHEET-STRING-LENGTH (SHEET STRING &OPTIONAL (START 0) (END NIL) (STOP-X NIL) FONT (START-X 0)
			    (TAB-WIDTH
			      (IF FONT
				  (* (FONT-CHAR-WIDTH FONT) (SHEET-TAB-NCHARS SHEET))
				  (SHEET-TAB-WIDTH SHEET)))
			    &AUX (MAX-X START-X))
  "Return the length in X-position of STRING or a portion.

START and END specify the portion (default is all).
START-X is an X-position to begin computation at.
STOP-X is an X-position at which to stop processing and return.
	FINAL-INDEX will indicate where in the string this was
	reached.
FONT is the font to use (default is SHEET's current font); but if
	STRING is an ART-FAT-STRING, each character's font is looked
	up in SHEET's font-map.
TAB-WIDTH is the width to use for tab characters, defaulting to
	SHEET's SHEET-TAB-WIDTH.

The cursor position does not wrap around during processing; arbitrarily
large values can be returned.  Use TV:SHEET-COMPUTE-MOTION if you
want wrap-around.

Three values are returned:
FINAL-X is the X-position when processing stopped (due to end of
	string or portion, or reaching STOP-X).
FINAL-INDEX is the index in the string at which processing stopped.
MAXIMUM-X is the largest X-position reached during processing.  This
	can be larger than FINAL-X if the string contains Backspaces
	or Returns."
  (DECLARE (VALUES FINAL-X FINAL-INDEX MAXIMUM-X))
  (IF FONT
    (COERCE-FONT FONT SHEET)
    (SETQ FONT (SHEET-CURRENT-FONT SHEET)))
  (PROG (CWA CW CH FONTX TEM I N NN II STRINGP (X START-X))
    (SETQ I START
	  N (OR END (ARRAY-ACTIVE-LENGTH STRING))
	  CW (FONT-CHAR-WIDTH FONT))	;At this point, decide whether we can use the fast version
 SLOW
    (AND (SETQ STRINGP (= (%P-MASK-FIELD-OFFSET %%ARRAY-TYPE-FIELD STRING 0) ART-STRING))	;i.e. no font changes
	 (NULL (SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)));and fixed width
	 (GO FAST))
 SLOW0
    (OR (< I N)
	(RETURN X I MAX-X))	;If string exhausted
    ;; Move quickly over 20 characters, or all the remaining characters,
    ;; if that does not go past STOP-X.
    (WHEN (OR STRINGP FONTX)
      (LET ((WIDTH-INCR
	     (%STRING-WIDTH (OR CWA PRINTING-CHARACTER-TRANSLATE-TABLE)
	      (IF FONTX
		(CODE-CHAR 0 0 FONTX)
                ;;(DPB FONTX %%CH-FONT 0)
                ;;ELSE
		0)
	      STRING I N (AND STOP-X (IF CWA
				       (- STOP-X X)
				       (FLOOR (- STOP-X X) CW))))))
	(UNLESS CWA
	  (SETQ WIDTH-INCR (* WIDTH-INCR CW)))
	(SETQ I (%POP))
	(SETQ X (+ WIDTH-INCR X))
	(SETQ MAX-X (MAX X MAX-X))
	;; Loop back if reached end of string.
	(IF (= I N)
	  (GO SLOW0))))
	;; Otherwise we stopped due to funny character or font change or
	;;  reaching STOP-X.
    (SETQ CH (CHAR-CODE (SETQ TEM (AREF STRING I))))
    (COND
      ((AND (NOT STRINGP)	;Changing fonts
	    (NEQ (SETQ TEM (CHAR-FONT TEM)) FONTX))
       (SETQ FONTX TEM
	     FONT
	     (AREF (SHEET-FONT-MAP SHEET)
		   (IF (>= FONTX (ARRAY-ACTIVE-LENGTH (SHEET-FONT-MAP SHEET)))	; -GRH
		       0
		       FONTX))
	     CWA (FONT-CHAR-WIDTH-TABLE FONT)
	     CW (FONT-CHAR-WIDTH FONT))))
    (COND
      ((GRAPHIC-CHAR-P CH)	;Printing character
       (SETQ NN (IF CWA
		  (AREF CWA CH)
		  CW)))
      ((= CH #\TAB)
       (SETQ NN (IF (ZEROP (SETQ TEM TAB-WIDTH))	; fix for zero tab width problem (dls).
		    0					; fix for zero tab width problem (dls).
		    (- (* (TRUNCATE (+ X TEM) TEM) TEM) X))))	; fix for zero tab width problem (dls).
      ((AND (= CH #\BACKSPACE)
	    (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
       (SETQ NN (- (MAX 0 (- X (SHEET-CHAR-WIDTH SHEET))) X)))
      ((= CH #\NEWLINE)
       (SETQ NN 0
	     X 0))
      (T	;Lozenged character
       (SETQ NN (SHEET-CHARACTER-WIDTH SHEET CH FONT))))
    (SETQ X (+ X NN))
    (IF (> X MAX-X)
      (SETQ MAX-X X))
    (AND STOP-X
	 (> X STOP-X)	;If char doesn't fit, stop before it
	 (RETURN (- X NN) I MAX-X))
    (SETQ I (1+ I))
    (GO SLOW)
    ;; Here is the fast loop.  The basic idea is to scan as fast as
    ;; possible over printing characters, with all checking outside the
    ;; loop.
 FAST
    ;; First, decide the most characters we want to scan over in a whack.
    (SETQ NN (COND
	       ((NULL STOP-X) N)	;NN is limiting value of I
	       ((MIN (+ (TRUNCATE (- STOP-X X) CW) I) N))))
    ;; Now, scan over printing characters.
    ;; Save initial I, and check for null loop.
    (AND (>= (SETQ II I) NN)
	 (GO SLOW0))
 SCN
    (%STRING-WIDTH PRINTING-CHARACTER-TRANSLATE-TABLE 0 STRING II NN NIL)
    (SETQ I (%POP))
    (SETQ X (+ (* (- I II) CW) X))	;Account for the motion of those chars
    (IF (> X MAX-X)
      (SETQ MAX-X X))
    ;Either string exhausted, non-printing,
    ; or reached stop-x
    (GO SLOW0)))


(DEFMETHOD (SHEET :STRING-OUT-EXPLICIT)
	   (STRING START-X START-Y X-LIMIT Y-LIMIT FONT ALU &OPTIONAL (START 0) END MULTI-LINE-LINE-HEIGHT color)
  (SHEET-STRING-OUT-EXPLICIT-1 SELF STRING START-X START-Y X-LIMIT Y-LIMIT FONT ALU START END
			       MULTI-LINE-LINE-HEIGHT nil color))

(COMPILER:MAKE-OBSOLETE SHEET-STRING-OUT-EXPLICIT
  "use the :STRING-OUT-EXPLICIT operation with rearranged args")
(DEFUN SHEET-STRING-OUT-EXPLICIT
       (SHEET STRING START-X Y XLIM FONT ALU &OPTIONAL (START 0) (END NIL) MULTI-LINE-LINE-HEIGHT YLIM color
	&AUX NEW-X NEW-Y NEW-INDEX)
  (prepare-color (sheet color) ;;; do it this way so that color does not have to be passed to explicit-1
  (MULTIPLE-VALUE-SETQ (NEW-X NEW-Y NEW-INDEX)
    (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING START-X Y XLIM YLIM FONT ALU START END
				 MULTI-LINE-LINE-HEIGHT)))
  (VALUES NEW-X NEW-INDEX NEW-Y))


(DEFUN SHEET-STRING-OUT-EXPLICIT-1
       (SHEET STRING START-X Ypos XLIM YLIM FONT ALUF
	&OPTIONAL (START 0) (END NIL) MULTI-LINE-LINE-HEIGHT (ALIGN-FOR-LOZENGE NIL) color)
  "Output STRING on SHEET without using SHEET's cursor, font, etc.
Output starts at cursor position START-X, Y but SHEET's cursor is not
moved.  Output stops if x-position XLIM or y-position YLIM is reached.

Font FONT is used, and alu-function ALU.
START and END specify a portion of STRING to be used.
MULTI-LINE-LINE-HEIGHT is how far to move down for Return
	characters; Return also moves back to x-position START-X.
	NIL means output <Return> with a lozenge.

All position arguments are relative to SHEET's outside edges."
  (DECLARE (VALUES FINAL-X FINAL-Y FINAL-INDEX))
  (COERCE-FONT FONT SHEET)
  (prepare-color (sheet color)
  (PREPARE-SHEET (SHEET)
  (OR XLIM (SETQ XLIM (SHEET-WIDTH SHEET)))
    (LET* ((INDEX (OR START 0))
	   (END (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	   (XPOS start-x)
	   CH)
      (WHEN (PLUSP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
        ;; Ensure enough room for ! at right of screen.
	(DECF XLIM (FONT-CHAR-WIDTH FONT)))
      (LOOP WHILE (< INDEX END) DOING
	    (MULTIPLE-VALUE-SETQ (XPOS INDEX)
	      (DRAW-STRING-INTERNAL SHEET STRING INDEX END
			    xpos ypos XLIM FONT ALUF))
	    (UNLESS index			;Done?
	       (RETURN xpos ypos end))
	    (SETQ CH (AREF STRING INDEX))
	    (COND ((AND MULTI-LINE-LINE-HEIGHT (= CH #\NEWLINE))
		   (SETQ Xpos START-X
			 Ypos (+ Ypos MULTI-LINE-LINE-HEIGHT))
		   (INCF index)
		   (IF (AND YLIM (> (+ Ypos MULTI-LINE-LINE-HEIGHT) YLIM))
		       (RETURN Xpos Ypos Index)))
		  ((NOT (GRAPHIC-CHAR-P ch))		;Special character?
		   (LET ((STRING (LOZENGED-SPECIAL-CHAR-NAME CH))
			 NX)
		     (MULTIPLE-VALUE-BIND (LOZENGE-WIDTH CORNER-WIDTH CORNER-HEIGHT)
			 (LOZENGED-STRING-GEOMETRY STRING)
		       (IF (> (SETQ NX (+ XPOS LOZENGE-WIDTH)) XLIM)
			   (RETURN XLIM YLIM INDEX))
		       (SHEET-DISPLAY-LOZENGED-STRING-INTERNAL
			 SHEET STRING
			 XPOS (OR ALIGN-FOR-LOZENGE YPOS)
			 XLIM ALUF
			 LOZENGE-WIDTH CORNER-WIDTH CORNER-HEIGHT)
		       (SETQ XPOS NX)))
		   (INCF INDEX))
		  (T (RETURN XPOS YPOS END)))	; Else end of line
	    FINALLY (RETURN xpos ypos end)	; KED 3/16/88
	    )))))



(DEFMETHOD (SHEET :STRING-OUT-UP) (STRING &OPTIONAL (START 0) (END NIL) 
   (color (if (color-system-p self) (sheet-foreground-color self) -1)))
  "Display STRING going up the window using the current font in
SHEET and the current cursor position."
  (SHEET-STRING-OUT-UP SELF STRING START END color))

(DEFMETHOD (SHEET :STRING-OUT-DOWN) (STRING &OPTIONAL (START 0) (END NIL) 
  (color (if (color-system-p self) (sheet-foreground-color self) -1)))
  "Display STRING going down the window using the current font in
SHEET and the current cursor position."
  (SHEET-STRING-OUT-DOWN SELF STRING START END color))

;;; The following variations on STRING-OUT allow one to draw characters
;;; up and down the screen.  The primitives used are not written in
;;; microcode which means that they are much slower than DRAW-CHAR.
(DEFUN SHEET-STRING-OUT-UP (SHEET STRING &OPTIONAL (START 0) (END NIL) 
  (color (if (color-system-p sheet) (sheet-foreground-color sheet) -1)))
  "Display STRING going up the window using the current font in
SHEET and the current cursor position."
  (UNLESS END
    (SETQ END (1- (LENGTH STRING))))
  (LET* ((FONT (SEND SHEET :CURRENT-FONT))
	 (CHAR-ALUF (SEND SHEET :CHAR-ALUF))
	 CHARACTER
	 (WIDTH-TABLE (FONT-CHAR-WIDTH-TABLE FONT)))
    (LOOP FOR STRING-INDEX FROM START TO END DO
       (PROGN
	 (SETQ CHARACTER (AREF STRING STRING-INDEX))
	 (DRAW-CHAR-UP FONT CHARACTER NIL NIL; X,Y
		       CHAR-ALUF SHEET color)
	 ;; Update the cursor position to reflect the character
	 ;; just drawn.
	 (SEND SHEET :INCREMENT-CURSORPOS 0
	    (- (IF WIDTH-TABLE
		 (AREF WIDTH-TABLE CHARACTER)
		 (FONT-CHAR-WIDTH FONT))))))))

(DEFUN SHEET-STRING-OUT-DOWN (SHEET STRING &OPTIONAL (START 0) (END NIL) 
  (color (if (color-system-p sheet) (sheet-foreground-color sheet) -1)))
  "Display STRING going down the window using the current font in
SHEET and the current cursor position."
  (UNLESS END
    (SETQ END (1- (LENGTH STRING))))
  (LET* ((FONT (SEND SHEET :CURRENT-FONT))
	 (CHAR-ALUF (SEND SHEET :CHAR-ALUF))
	 CHARACTER
	 (WIDTH-TABLE (FONT-CHAR-WIDTH-TABLE FONT)))
    (LOOP FOR STRING-INDEX FROM START TO END DO
       (PROGN
	 (SETQ CHARACTER (AREF STRING STRING-INDEX))
	 (DRAW-CHAR-DOWN FONT CHARACTER
			 NIL NIL	; X,Y
			 CHAR-ALUF SHEET color)
	 ;; Update the cursor position to reflect the character
	 ;; just drawn.
	 (SEND SHEET :INCREMENT-CURSORPOS 0
	    (IF WIDTH-TABLE
	      (AREF WIDTH-TABLE CHARACTER)
	      (FONT-CHAR-WIDTH FONT)))))))

(DEFUN DRAW-CHAR-UP (FONT CHARACTER
		     &OPTIONAL (X NIL) (Y NIL) (ALU NIL) (SHEET SELECTED-WINDOW) 
   (color (if (color-system-p sheet) (sheet-foreground-color sheet) -1)))
  "Draw character CHARACTER in FONT at X, Y in SHEET using
alu-function ALU.  X and Y are relative to SHEET's outside edges."
  (UNLESS ALU
    (SETQ ALU ALU-XOR))
  (COERCE-FONT FONT SHEET)
  (LET ((FIT (FONT-INDEXING-TABLE FONT)))
	;; Start at the current cursor position.
    (MULTIPLE-VALUE-BIND (X Y)
      (IF (OR (NULL X) (NULL Y))
	(SEND SHEET :READ-CURSORPOS)
	(VALUES X Y))
      (IF FIT
	(DO ((CHARACTER (AREF FIT CHARACTER) (1+ CHARACTER))
	     (LIM (AREF FIT (1+ CHARACTER)))
	     (BPP (IF (ARRAYP SHEET)
		    (ARRAY-BITS-PER-PIXEL SHEET)
		    (SHEET-BITS-PER-PIXEL SHEET)))
	     (Y Y (- Y (TRUNCATE (FONT-RASTER-WIDTH FONT) BPP))))
	    ((>= CHARACTER LIM))
	  (SIMPLE-DRAW-CHAR-UP FONT CHARACTER X Y ALU SHEET color))
	(SIMPLE-DRAW-CHAR-UP FONT CHARACTER X Y ALU SHEET color)))))

(DEFUN DRAW-CHAR-DOWN (FONT CHARACTER
		       &OPTIONAL (X NIL) (Y NIL) (ALU NIL) (SHEET SELECTED-WINDOW) 
  (color (if (color-system-p sheet) (sheet-foreground-color sheet) -1))) 
  "Draw character CHARACTER in FONT at X, Y in SHEET using
alu-function ALU.  X and Y are relative to SHEET's outside edges."
  (UNLESS ALU
    (SETQ ALU ALU-XOR))
  (COERCE-FONT FONT SHEET)
  (LET ((FIT (FONT-INDEXING-TABLE FONT)))
	;; Start at the current cursor position.
    (MULTIPLE-VALUE-BIND (X Y)
      (IF (OR (NULL X) (NULL Y))
	(SEND SHEET :READ-CURSORPOS)
	(VALUES X Y))
      (IF FIT
	(DO ((CHARACTER (AREF FIT CHARACTER) (1+ CHARACTER))
	     (LIM (AREF FIT (1+ CHARACTER)))
	     (BPP (IF (ARRAYP SHEET)
		    (ARRAY-BITS-PER-PIXEL SHEET)
		    (SHEET-BITS-PER-PIXEL SHEET)))
	     (Y Y (+ Y (TRUNCATE (FONT-RASTER-WIDTH FONT) BPP))))
	    ((>= CHARACTER LIM))
	  (SIMPLE-DRAW-CHAR-DOWN FONT CHARACTER X Y ALU SHEET color))
	(SIMPLE-DRAW-CHAR-DOWN FONT CHARACTER X Y ALU SHEET color)))))

(DEFUN SIMPLE-DRAW-CHAR-UP (FONT CHARACTER X Y ALU SHEET &optional 
  (color (if (color-system-p sheet) (sheet-foreground-color sheet) -1)))
  "Draws a single character going up the window."
  ;; Note that this is a simple version since it does not handle wide
  ;; fonts.  We start drawing the characters at the position specified
  ;; by (X,Y).
  ;; 
  ;; This function deals directly with the font array.  Before trying to
  ;; understand what is going on here, it is important to read about
  ;; the font layout in the Window System manual.  Only after that is
  ;; done, will the comments make sense.
  (LET* ((setq RASTERS-PER-WORD (FONT-RASTERS-PER-WORD FONT))
	 (HEIGHT (FONT-RASTER-HEIGHT FONT))
	 (setq WIDTH (FONT-RASTER-WIDTH FONT))
	 ;; Amount of unused space in a word.
	 (setq WASTE (- 32 (* RASTERS-PER-WORD WIDTH)))
	 (WORD-BASE
	  (-
	   ;; Compute the base index within the font of
	   ;; where the character starts.  Subtract from
	   ;; the base index of the font, the waste and
	   ;; width.  They are subtracted because we are
	   ;; going to add them back within the loop.
	   (* 32 CHARACTER (FONT-WORDS-PER-CHAR FONT)) (+ WASTE WIDTH)))
	 ;; This is a counter we are going to use to tell us when we
	 ;; are going on to another 32 bit word.  When this counter
	 ;; gets up to RASTERS-PER-WORD then we are to start the next
	 ;; word. 
	 (NEXT-WORD-COUNTER RASTERS-PER-WORD))
    (DOTIMES (HEIGHT-INDEX HEIGHT)
      (SETQ WORD-BASE
	    (+ WORD-BASE WIDTH
	       (IF (= NEXT-WORD-COUNTER RASTERS-PER-WORD)
		;; Starting on the next word.  Adjust WORD-BASE so
		;; that it starts on the index for the next word.
		 (PROGN
		   (SETQ NEXT-WORD-COUNTER 1)
		   WASTE)
		 (PROGN
		   (INCF NEXT-WORD-COUNTER)
		   0))))
      (DOTIMES (WIDTH-INDEX WIDTH)
       ;; Use DRAW-POINT because it performs clipping.
	(if (color-system-p sheet)
	    (when (eql 1 (AREF FONT (+ WORD-BASE WIDTH-INDEX)))
	      (SEND SHEET :DRAW-POINT (+ X HEIGHT-INDEX) (- Y WIDTH-INDEX); We are going up, so subtract.
		    ALU color))
	    (SEND SHEET :DRAW-POINT (+ X HEIGHT-INDEX) (- Y WIDTH-INDEX); We are going up, so subtract.
		  ALU (AREF FONT (+ WORD-BASE WIDTH-INDEX))))))))

(DEFUN SIMPLE-DRAW-CHAR-DOWN (FONT CHARACTER X Y ALU SHEET &optional 
  (color (if (color-system-p sheet) (sheet-foreground-color sheet) -1)))
  "Draws a single character going down the window."
  ;; Note that this is a simple version since it does not handle wide
  ;; fonts.  We start drawing the characters at the position specified
  ;; by (X,Y).
  ;; 
  ;; This function deals directly with the font array.  Before trying to
  ;; understand what is going on here, it is important to read about the
  ;; font layout in the Window System manual.  Only after that is done,
  ;; will the comments make sense.
  (LET* ((RASTERS-PER-WORD (FONT-RASTERS-PER-WORD FONT))
	 (HEIGHT (FONT-RASTER-HEIGHT FONT))
	 (WIDTH (FONT-RASTER-WIDTH FONT))
	 ;; amount of unused space in a word
	 (WASTE (- 32 (* RASTERS-PER-WORD WIDTH)))
	 (WORD-BASE
	  (-
	   ;; Compute the base index within the font of
	   ;; where the character starts.  Subtract from
	   ;; the base index of the font, the waste and
	   ;; width.  They are subtracted because we are
	   ;; going to add them back within the loop.
	   (* 32 CHARACTER (FONT-WORDS-PER-CHAR FONT)) (+ WASTE WIDTH)))
	 ;; This is a counter we are going to use to tell us when we
	 ;; are going on to another 32 bit word.  When this counter
	 ;; gets up to RASTERS-PER-WORD then we are to start the next
	 ;; word. 
	 (NEXT-WORD-COUNTER RASTERS-PER-WORD))
    (DOTIMES (HEIGHT-INDEX HEIGHT)
      (SETQ WORD-BASE
	    (+ WORD-BASE WIDTH
	       (IF (= NEXT-WORD-COUNTER RASTERS-PER-WORD)
		;; Starting on the next word.  Adjust WORD-BASE so
		;; that it starts on the index for the next word.
		 (PROGN
		   (SETQ NEXT-WORD-COUNTER 1)
		   WASTE)
		 (PROGN
		   (INCF NEXT-WORD-COUNTER)
		   0))))
      (DOTIMES (WIDTH-INDEX WIDTH)
       ;; Use DRAW-POINT because it performs clipping.
       (if (color-system-p sheet)
	   (when (eql 1 (AREF FONT (+ WORD-BASE WIDTH-INDEX)))
	     (SEND SHEET :DRAW-POINT (+ X (- HEIGHT HEIGHT-INDEX)) (+ Y WIDTH-INDEX)
		   ALU color))
	   (SEND SHEET :DRAW-POINT (+ X (- HEIGHT HEIGHT-INDEX)) (+ Y WIDTH-INDEX) ALU
		 (AREF FONT (+ WORD-BASE WIDTH-INDEX))))))))

(DEFMETHOD (SHEET :DISPLAY-CENTERED-STRING-EXPLICIT) (&REST ARGS)
  (APPLY 'SHEET-DISPLAY-CENTERED-STRING-EXPLICIT SELF ARGS))

(COMPILER:MAKE-OBSOLETE
  SHEET-DISPLAY-CENTERED-STRING-EXPLICIT
  "use the :STRING-OUT-CENTERED-EXPLICIT operation with rearranged args")
(DEFUN SHEET-DISPLAY-CENTERED-STRING-EXPLICIT
       (SHEET STRING
	&OPTIONAL (LEFT (SHEET-INSIDE-LEFT SHEET)) Y-POS
	(RIGHT (SHEET-INSIDE-RIGHT SHEET)) (FONT (SHEET-CURRENT-FONT SHEET))
	(ALU (SHEET-CHAR-ALUF SHEET)) (START 0) END (MULTI-LINE-LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	Y-LIMIT color)
  (prepare-color (sheet color)
  (SHEET-STRING-OUT-CENTERED-EXPLICIT SELF STRING LEFT Y-POS RIGHT Y-LIMIT FONT ALU START END
				      MULTI-LINE-LINE-HEIGHT)))

(DEFMETHOD (SHEET :STRING-OUT-CENTERED-EXPLICIT)
	   (STRING
	    &OPTIONAL (LEFT (SHEET-INSIDE-LEFT)) Y-POS (RIGHT (SHEET-INSIDE-RIGHT)) Y-LIMIT
	    (FONT CURRENT-FONT) (ALU CHAR-ALUF) (START 0) END (MULTI-LINE-LINE-HEIGHT LINE-HEIGHT) color)
  (prepare-color (self color)
  (SHEET-STRING-OUT-CENTERED-EXPLICIT SELF STRING LEFT Y-POS RIGHT Y-LIMIT FONT ALU START END
				      MULTI-LINE-LINE-HEIGHT)))

(DEFUN SHEET-STRING-OUT-CENTERED-EXPLICIT
       (SHEET STRING &OPTIONAL (LEFT (SHEET-INSIDE-LEFT SHEET)) Y-POS
	(RIGHT (SHEET-INSIDE-RIGHT SHEET)) YLIM (FONT (SHEET-CURRENT-FONT SHEET))
	(ALU (SHEET-CHAR-ALUF SHEET)) (START 0) END (MULTI-LINE-LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET)) color
	&AUX WID SWID)
  "Output STRING or portion on SHEET centered between LEFT and
RIGHT.  The string is truncated on the right if it is too wide.
LEFT and RIGHT are relative to SHEET's inside edge.

Y-POS specifies the vertical position of the top of the output, relative
to SHEET's top margin.  This defaults to the SHEET's inside top edge.

Y-LIM specifies a Y limit beyond which output is not to be displayed.
This defaults to the SHEET's inside bottom edge.

FONT and ALU function are used, defaulting to SHEET's font and char-aluf.

START and END specify a portion of STRING to output; default is all.

MULTI-LINE-LINE-HEIGHT is the distance for Return to move down;
	then each line is centered individually.  It can be NIL meaning
	output Return as <Return>.  Default is sheet's line-height.

SHEET's cursor is not used or moved."
   (IF FONT
    (COERCE-FONT FONT SHEET)
    (SETQ FONT (SHEET-CURRENT-FONT SHEET)))
  (OR ALU (SETQ ALU ALU-IOR))
  (OR END (SETQ END (LENGTH STRING)))
  (OR Y-POS (SETQ Y-POS (SHEET-INSIDE-TOP SHEET)))
  (OR YLIM (SETQ YLIM (SHEET-INSIDE-BOTTOM SHEET)))
  (SETQ WID (- RIGHT LEFT)
	STRING (STRING STRING))
  (DO ((BEG START))
      ((>= BEG END))
    (LET ((STOP
	   (OR
	    (POSITION #\NEWLINE (THE STRING (STRING STRING)) :START BEG :END END :TEST
		      #'CHAR-EQUAL)
	    END)))
	  ;; Compute how wide the string is, and whether to truncate.
      (MULTIPLE-VALUE-SETQ (NIL NIL NIL SWID)
	(SHEET-COMPUTE-MOTION SHEET 0 0 STRING BEG STOP NIL 1.0s10 1.0s10 1.0s10 1.0s10 FONT))
      (prepare-color (sheet color)
      (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING (+ LEFT (MAX (TRUNCATE (- WID SWID) 2) 0)) Y-POS
				   RIGHT NIL FONT ALU BEG STOP))
      (IF (OR (NULL MULTI-LINE-LINE-HEIGHT)
	      (>= (+ Y-POS MULTI-LINE-LINE-HEIGHT) YLIM))
	  (RETURN))
      (INCF Y-POS MULTI-LINE-LINE-HEIGHT)
      (SETQ BEG (1+ STOP)))))

(DEFMETHOD (SHEET :STRING-OUT-CENTERED) (&REST ARGS)
  (APPLY 'SHEET-DISPLAY-CENTERED-STRING SELF ARGS))

(DEFMETHOD (SHEET :DISPLAY-CENTERED-STRING) (&REST ARGS)
  (APPLY 'SHEET-DISPLAY-CENTERED-STRING SELF ARGS))

;;; This function displays a string centered between two X coordinates,
;;; truncated if necessary.  The arguments are relative to the margins,
;;; as usual.
(DEFUN SHEET-DISPLAY-CENTERED-STRING
       (SHEET STRING
	&OPTIONAL (LEFT 0) (RIGHT (SHEET-INSIDE-WIDTH SHEET))
	(TOP (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))) color)
  "Output STRING on SHEET centered between LEFT and RIGHT.
LEFT and RIGHT are relative to SHEET's margin.
TOP specifies the vertical position of the top of the output, relative
to SHEET's top margin.  The output may be multiple lines.
Centering is based on the length of the longest line substring. All lines 
are then left-justified to the same horizontal position.

SHEET's current font, alu function and line height are used.
SHEET's cursor is left at the end of the string."
 (prepare-color (sheet color)  
  (LET* ((EOS         (ARRAY-ACTIVE-LENGTH STRING))	 
	 (LEFT-MARGIN (DO* ((LONGEST 0)	;; Pixel length of longest line substring
			    (SOL     0 (IF EOL (1+ EOL)))
			    EOL) 
			   ((NULL SOL) (MAX 0 (+ LEFT (TRUNCATE (- RIGHT LEFT LONGEST) 2)))) 
			(SETF LONGEST (MAX LONGEST (SHEET-STRING-LENGTH
						     SHEET
						     STRING
						     SOL
						     (SETF EOL (%STRING-SEARCH-CHAR
								 #\NEWLINE
								 STRING
								 SOL
								 EOS
								 )))))				
			)))
    
    ;; Output string, positioning each substring, line-at-a-time, to the (centered) left margin .
    (SHEET-SET-CURSORPOS SHEET 0 TOP)
    (DO ((SOL 0 (IF EOL (1+ EOL)))
	 EOL) 
	((NULL SOL)) 
      (PREPARE-SHEET (SHEET)
	(SHEET-INCREMENT-BITPOS SHEET LEFT-MARGIN 0))	                ;Position to (centered) start position      
      (SHEET-STRING-OUT SHEET STRING SOL (SETF EOL (%STRING-SEARCH-CHAR	;Output string upto next newline
						     #\NEWLINE
						     STRING
						     SOL
						     EOS)))      
      (IF EOL (SHEET-CRLF SHEET))))))		                        ;Output next newline (if any)

(DEFMETHOD (SHEET :STRING-OUT-X-Y-CENTERED-EXPLICIT) (&REST ARGS)
  (APPLY 'SHEET-DISPLAY-X-Y-CENTERED-STRING SELF ARGS))

(DEFMETHOD (SHEET :DISPLAY-X-Y-CENTERED-STRING) (&REST ARGS)
  (APPLY 'SHEET-DISPLAY-X-Y-CENTERED-STRING SELF ARGS))

(DEFUN SHEET-DISPLAY-X-Y-CENTERED-STRING
       (SHEET STRING
	&OPTIONAL
	(LEFT (SHEET-INSIDE-LEFT SHEET))
	(TOP (SHEET-INSIDE-TOP SHEET))
	(RIGHT (SHEET-INSIDE-RIGHT SHEET))
	(BOTTOM (SHEET-INSIDE-BOTTOM SHEET))
	(FNT (SHEET-CURRENT-FONT SHEET))
	(ALU (SHEET-CHAR-ALUF SHEET))
	(START 0)
	END
	(MULTI-LINE-LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET)) color)
  "Display STRING on SHEET centered in both X and Y, in font FNT.
It is centered horizontally between LEFT and RIGHT, vertically
between TOP and BOTTOM.
All four coordinates are relative to SHEET's outside edges.  SHEET's
cursor is not used or moved."
 (prepare-color (sheet color)
  (LET ((WID (- RIGHT LEFT)))
    (MULTIPLE-VALUE-BIND (NIL SHEI SLEN SWID)
      (SHEET-COMPUTE-MOTION SHEET 0 0 STRING START END T 0
			    (+ TOP (SHEET-INSIDE-HEIGHT SHEET)) #o30000000 NIL FNT
			    MULTI-LINE-LINE-HEIGHT)
      (UNLESS (NUMBERP SLEN)
	(SETQ SLEN NIL))
      (SHEET-STRING-OUT-EXPLICIT-1 SHEET STRING (+ LEFT (MAX (TRUNCATE (- WID SWID) 2) 0))
				   (MAX (- (TRUNCATE (+ TOP BOTTOM) 2) (TRUNCATE SHEI 2)) TOP)
				   RIGHT BOTTOM FNT ALU START SLEN MULTI-LINE-LINE-HEIGHT)))))

;;; Most screens can be seen by the "user".
(DEFMETHOD (SCREEN :USER-VISIBLE) NIL T)

;;; A mixin that causes inferiors to be scaled when the size of the
;;; window changes and propagates changes in the default font.
;;; TIME-STAMP is (as for any sheet), the time-stamp for comparison with
;;; this sheet's superior.
;;; CURRENT-TIME-STAMP is the stamp which propagates down into our
;;; inferiors.  If an inferior's TIME-STAMP is EQ to our
;;; CURRENT-TIME-STAMP, then the inferior is up to date.  Otherwise we
;;; compare the two stamps and resolve the differences.  This comparison
;;; happens to the active inferiors when our stamp changes and to any
;;; newly-activated inferior.
;;; This mixin is the only thing which knows the format of time stamps
;;; (other than that they are compared with EQ).  A time stamp is a list
;;; which represents the state of a window that has this mixin:
;;;   (serial-number our-inside-width our-inside-height default-font)
;;; serial-number is incremented every time a new time stamp is
;;; generated, and is only there for human beings looking at the stamps.
;;; Other elements may be added as needed.
(DEFFLAVOR SCALE-INFERIORS-MIXIN (CURRENT-TIME-STAMP) ()
  (:REQUIRED-FLAVORS SHEET)
  (:GETTABLE-INSTANCE-VARIABLES CURRENT-TIME-STAMP))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :AFTER :INIT) (IGNORE)
  (SETQ CURRENT-TIME-STAMP
	(LIST 0 (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
	      (LOOP FOR ELT IN (SCREEN-FONT-ALIST (SHEET-GET-SCREEN SELF))
		    APPEND (LIST (CAR ELT) (FONT-EVALUATE (CDR ELT)))))))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :INFERIOR-TIME-STAMP) (INF)
  INF					;Inferiors all have same time stamp
  CURRENT-TIME-STAMP)

(DEFUN SCALE-INFERIORS-MIXIN-UPDATE-INFERIOR (INFERIOR)
  (DECLARE (:SELF-FLAVOR SCALE-INFERIORS-MIXIN))
  (LET ((INF-TIME-STAMP (SHEET-TIME-STAMP INFERIOR)))
    (COND ((NEQ INF-TIME-STAMP CURRENT-TIME-STAMP)
	   (LET ((OLD-FONT (FOURTH (SHEET-TIME-STAMP INFERIOR)))
		 (NEW-FONT (FOURTH CURRENT-TIME-STAMP)))
	     (OR (EQUAL OLD-FONT NEW-FONT)
		 (SEND INFERIOR :CHANGE-OF-DEFAULT-FONT NIL NIL)))
	   (SCALE-INFERIORS-MIXIN-SCALE-INFERIOR INFERIOR NIL
                                                 INF-TIME-STAMP)))))

(DEFUN SCALE-INFERIORS-MIXIN-SCALE-INFERIOR
       (INFERIOR EXPOSE
        &OPTIONAL (INF-TIME-STAMP (SHEET-TIME-STAMP INFERIOR)))
  (DECLARE (:SELF-FLAVOR SCALE-INFERIORS-MIXIN))
  (OR (EQ CURRENT-TIME-STAMP INF-TIME-STAMP)
      ;; Hasn't had edges set in the current time slice, so set them
      (LET* ((SIZE-LAST-TIME (CDR INF-TIME-STAMP))
	     (NEW-LEFT   (TRUNCATE (* (SHEET-X-OFFSET INFERIOR) (SHEET-INSIDE-WIDTH))
                                   (FIRST  SIZE-LAST-TIME)))
	     (NEW-TOP    (TRUNCATE (* (SHEET-Y-OFFSET INFERIOR) (SHEET-INSIDE-HEIGHT))
                                   (SECOND SIZE-LAST-TIME)))
	     (NEW-WIDTH  (TRUNCATE (* (SHEET-WIDTH    INFERIOR) (SHEET-INSIDE-WIDTH))
                                   (FIRST  SIZE-LAST-TIME)))
	     (NEW-HEIGHT (TRUNCATE (* (SHEET-HEIGHT   INFERIOR) (SHEET-INSIDE-HEIGHT))
				   (SECOND SIZE-LAST-TIME))))
	(COND ((AND (= (SHEET-X-OFFSET INFERIOR) NEW-LEFT)
		    (= (SHEET-Y-OFFSET INFERIOR) NEW-TOP)
		    (= (SHEET-WIDTH    INFERIOR) NEW-WIDTH)
		    (= (SHEET-HEIGHT   INFERIOR) NEW-HEIGHT)
		    (= (sheet-locations-per-line inferior)
		       (sheet-locations-per-line
			 (sheet-superior inferior))))
	       (SETQ NEW-LEFT NIL))
	      ((NOT (SEND INFERIOR :SET-EDGES NEW-LEFT NEW-TOP
			  (+ NEW-LEFT NEW-WIDTH)
			  (+ NEW-TOP  NEW-HEIGHT) :VERIFY))
	       ;; Won't go, try not to change size.
	       (SETQ NEW-WIDTH  (SHEET-WIDTH  INFERIOR)
		     NEW-HEIGHT (SHEET-HEIGHT INFERIOR))
	       (AND (> (+ NEW-WIDTH  NEW-LEFT) (SHEET-INSIDE-RIGHT))
		    (SETQ NEW-LEFT (- (SHEET-INSIDE-RIGHT)  NEW-WIDTH)))
	       (AND (> (+ NEW-HEIGHT NEW-TOP)  (SHEET-INSIDE-BOTTOM))
		    (SETQ NEW-TOP  (- (SHEET-INSIDE-BOTTOM) NEW-HEIGHT)))
	       (OR (SEND INFERIOR :SET-EDGES NEW-LEFT NEW-TOP
			 (+ NEW-LEFT NEW-WIDTH)
			 (+ NEW-TOP  NEW-HEIGHT) :VERIFY)
		   ;; Won't go, don't change size at all.
		   (SETQ NEW-LEFT NIL))))
	(COND (NEW-LEFT
	       (SEND INFERIOR :SET-EDGES
		     NEW-LEFT NEW-TOP
		     (+ NEW-LEFT NEW-WIDTH)
		     (+ NEW-TOP  NEW-HEIGHT))
	       (AND EXPOSE (SEND INFERIOR :EXPOSE)))
	      (T (SEND INFERIOR :UPDATE-TIME-STAMP))))))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :BEFORE :INFERIOR-ACTIVATE) (INFERIOR)
  "Catch up with any changes that happened while we were inactive."
  (SCALE-INFERIORS-MIXIN-UPDATE-INFERIOR INFERIOR)
  INFERIOR)

(DEFWRAPPER (SCALE-INFERIORS-MIXIN :CHANGE-OF-SIZE-OR-MARGINS) (IGNORE . BODY)
  `(DELAYING-SCREEN-MANAGEMENT
     (LET ((OLD-EXP-INFS (REVERSE EXPOSED-INFERIORS)))
       (DOLIST (I EXPOSED-INFERIORS)
	 (SEND I :DEEXPOSE))
       ,@BODY
       (SETQ CURRENT-TIME-STAMP
	     (LIST (1+ (CAR CURRENT-TIME-STAMP))
		   (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
		   (FOURTH CURRENT-TIME-STAMP)))
       (DOLIST (I OLD-EXP-INFS)
	 (SCALE-INFERIORS-MIXIN-SCALE-INFERIOR I T))
       (DOLIST (I INFERIORS)
	 (OR (MEMBER I EXPOSED-INFERIORS :TEST #'EQ) 
	     (SCALE-INFERIORS-MIXIN-SCALE-INFERIOR I NIL))))))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :BEFORE :CHANGE-OF-DEFAULT-FONT) (IGNORE IGNORE)
  (SETQ CURRENT-TIME-STAMP
	(LIST (1+ (CAR CURRENT-TIME-STAMP)) (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
	      (LOOP FOR ELT IN (SCREEN-FONT-ALIST (SHEET-GET-SCREEN SELF))
		    APPEND (LIST (CAR ELT) (FONT-EVALUATE (CDR ELT)))))))

(DEFVAR KILL-RECURSION NIL
  "Non-NIL if killing the inferiors of another window being killed.")

;;; Kill the processes only after any :after :kill daemons have run.
(DEFWRAPPER (SHEET :KILL) (IGNORE . BODY)
  `(LET ((PROCESSES (UNLESS KILL-RECURSION (SEND SELF :PROCESSES)))
	 (KILL-RECURSION T))
     ,@BODY
     (KILL-PROCESSES PROCESSES)))

(sys:declare-suggestions-for
  '(:method tv:sheet :kill)
  :before '(sys:sugg-before-kill-method))

(DEFMETHOD (SHEET :KILL) ()
  (sys:with-suggestions-menus-for (:method tv:sheet :kill)
    (COND
      ((mac-window-p self)
       (delaying-screen-management
	 (WHEN superior
	   (SEND self :bury))
	 (MAPC 'SEND (COPY-LIST INFERIORS) (CIRCULAR-LIST :KILL)))
       (SEND SELF :DEACTIVATE)
       (CLEAN-OUT-IO-BUFFER NIL SELF))
      (t
       (SEND SELF :DEACTIVATE)
       (CLEAN-OUT-IO-BUFFER NIL SELF)
       (MAPC 'SEND (COPY-LIST INFERIORS) (CIRCULAR-LIST :KILL))))))

(DEFUN KILL-PROCESSES (PROCESSES)
  (DOLIST (P PROCESSES)
    (UNLESS (EQ P CURRENT-PROCESS)
      (SEND P :KILL)))
  (IF (MEMBER CURRENT-PROCESS PROCESSES :TEST #'EQ)
    (SEND CURRENT-PROCESS :KILL)))

;;; Uses :APPEND method combination and returns a list of processes to
;;; be killed.
(DEFMETHOD (SHEET :PROCESSES) ()
  (MAPCAN 'SEND (COPY-LIST INFERIORS) (CIRCULAR-LIST :PROCESSES)))

(DEFFLAVOR STANDARD-SCREEN () (SCALE-INFERIORS-MIXIN SCREEN))

;;; This height may get hacked by the who-line making code if the
;;; wholine ends up at the bottom of the main screen (which it usually
;;; does).
(DEFVAR MAIN-SCREEN-WIDTH 1024.)

(DEFVAR MAIN-SCREEN-HEIGHT 808.)

(DEFVAR ORIG-MAIN-SCREEN-HEIGHT 808.)				; jlm 2/28/89
(DEFVAR MIN-MAIN-SCREEN-HEIGHT 202.)				; .
(DEFVAR MAIN-SCREEN-OFFSET-FOR-PROC 0.)				; .
(defvar *mp-blank-window*)					; jlm 3/7/89
(defvar *mp-full-screen-mode*)					; .
(defvar mp-setup-gsb-window-early)				; .
(defvar mp-count-displayed-screens)				; .
(defvar mp-displayp)						; .
(defvar MP-disable-run-light)					; .
(defvar mp-get-number-of-active-processors)			; .
(defvar mp-grab-processor-internal)				; .
(defvar mp-get-logical-processor-id)				; .
(defvar mp-GET-ACTIVE-PROCESSOR-SLOT-NUMBERS)			; .
(defvar mp-sib-owner-p)						; .
(defvar mp-get-relative-position-of-screen)			; jlm 3/7/89


(DEFVAR MAIN-SCREEN-BUFFER-ADDRESS         nil) ; wait until initialization, check to see which board we're on
(DEFVAR MAIN-SCREEN-CONTROL-ADDRESS 131056)
(DEFVAR MAIN-SCREEN-BUFFER-LENGTH 32768)

;;; New method.  Allow GET to work on screens.  CJJ 04/13/88.
(DEFMETHOD (screen :get)
	   (property &optional default)
  (GETF property-list property default))

;;; Added by KJF for CJJ on 08/16/88 for Multiple Monitor (MMON) support.
(DEFUN virtual-address-of-frame-buffer-p
       (thing)
  "Returns THING if THING is the virtual address of a frame buffer.  Otherwise, returns NIL."
  (WHEN (MULTIPLE-VALUE-BIND (obj address flag)
	    (si:fsh-safe thing)
	  (OR obj address flag))
    (LET* ((physical-address (%physical-address thing))
	   (f-slot (LDB %%NuBus-F-And-Slot-Bits physical-address))
	   (frame-buffer-pointer (LDB si:%%NuBus-slot-relative-bits physical-address)))
      (AND (EQL *slot-space* (DEPOSIT-FIELD 0 (BYTE 4 0) f-slot))
	   (SELECTOR (sys:board-type f-slot) STRING=
	     (tv:*CSIB-board-type*
	      (OR (EQL frame-buffer-pointer
		       %CSIB-Color-Xinc-No-transp-byte-offset)
		  (EQL frame-buffer-pointer
		       %CSIB-Expans-Xinc-No-transp-byte-offset)))
	     (tv:*SIB-board-type*
	      (EQL frame-buffer-pointer
		   %TV-Screen-Memory-Start-Byte-Offset)))
	   thing))))

;;; Modified by KJF for CJJ on 08/16/88 for Multiple Monitor (MMON) support.
(DEFUN screen-exposable-p (screen)
  "Determines if SCREEN can be exposed on display hardware available on this system.
Meant for use on Explorer systems only, not microExplorer.  Always returns t on
microExplorer."
  ;; If MX, just return t.  04/26/88 KJF
  (OR (mac-system-p)
      (IF (mmon-p)
	  (AND
	    ;; Added for multiple-monitor support.  05/25/88 CJJ
	    ;; Make sure screen will expose on a configured monitor...
	    (SEND screen :visible-on-monitors)
	    ;; Put remaining code into a separate function for use elsewhere.  05/26/88 CJJ
	    (virtual-address-of-frame-buffer-p (tv:screen-buffer screen)))
	  (virtual-address-of-frame-buffer-p (tv:screen-buffer screen)))))

(DEFUN screen-p (thing)
  (TYPEP thing 'screen))

(DEFUN who-line-screen-p (thing)
  "Determines if THING is a who-line-screen."
  (OR (TYPEP thing 'who-line-screen)
      (AND (TYPEP thing 'screen)
	   (GET thing :who-line nil))))

(DEFUN not-a-who-line-screen-p (thing)
  (NOT (who-line-screen-p thing)))

(DEFUN associated-who-line-screen (screen)
  "Returns who-line screen associated with SCREEN.
If SCREEN has no associated who-line screen,
or if the associated who-line screen is not really a who-line screen, returns NIL."
  (LET ((who (tv:screen-screens-who-line-screen screen)))
    (WHEN (who-line-screen-p who)
      who)))

(DEFUN associated-who-line-screen-is-exposable-p (screen)
  (screen-exposable-p (tv:screen-screens-who-line-screen screen)))

;;; ********* Attention ! *********
;;; This function is redefined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;
(DEFUN same-monitor-p (screen1 screen2)
  "Determines if both screens will expose on the same monitor(s)."
  (AND (tv:compare-properties screen1 screen2)
       (EQL (tv:screen-buffer screen1)
	    (tv:screen-buffer screen2))))

(DEFUN associated-who-line-screen-is-on-same-monitor-p
       (screen)
  (same-monitor-p screen (tv:screen-screens-who-line-screen screen)))


(DEFUN associated-who-line-screen-does-not-overlap-p
       (screen)
  (MULTIPLE-VALUE-BIND
    (screen-left screen-top screen-right screen-bottom)
      (SEND screen :edges)
    (MULTIPLE-VALUE-BIND
      (who-left who-top who-right who-bottom)
	(SEND (tv:screen-screens-who-line-screen screen) :edges)
      (OR (>= who-top screen-bottom)	   ;Check most likely case first...
	  (<= who-bottom screen-top)
	  (>= who-left screen-right)
	  (<= who-right screen-left)))))

;; may 03/09/89 Added to *initial-screen-tests* - *initial-screen* MUST point to tv:who-line-screen 
(DEFUN associated-who-line-is-who-line-screen-p (screen)
  (EQ (screen-screens-who-line-screen screen)
      tv:who-line-screen))

;; may 04/25/89 Added to *initial-screen-tests* 
;; Must insure that initial who-line-screen is visible - its plane-mask is same for screen and who-line-screen.
(defun associated-who-line-screen-is-compatible-p (screen)
  "Returns T if associated who-line-screen of SCREEN has the same
   :display-type and :bits-per-pixel properties of SCREEN."
  (let ((wholin (screen-screens-who-line-screen screen)))
    (and wholin
	 (eql (get screen :logical-bits-per-pixel)
	      (get wholin :logical-bits-per-pixel))
	 (eql (get screen :display-type)
	      (get wholin :display-type))
	 (eql (sheet-plane-mask screen)
	      (sheet-plane-mask wholin)))))

(DEFPARAMETER *initial-screen-tests*
	      (LIST 'screen-p
		    'not-a-who-line-screen-p
		    'screen-exposable-p
		    'associated-who-line-screen	   ;Also ensures it really is a who-line-screen.
		    'associated-who-line-is-who-line-screen-p 	;; may 03/09/89 Added
		    'associated-who-line-screen-is-compatible-p ;; may 04/25/89 Added
		    'associated-who-line-screen-is-exposable-p
		    'associated-who-line-screen-is-on-same-monitor-p
		    'associated-who-line-screen-does-not-overlap-p)
  "Functions applied to a screen to determine if it can be exposed as the initial screen.")


(DEFUN acceptable-initial-screen-p (screen)
  (LOOP FOR test IN *initial-screen-tests*
	ALWAYS (FUNCALL test screen)))


(DEFPARAMETER *initial-who-line-screen-tests*
	      (LIST 'who-line-screen-p 'screen-exposable-p)
  "Functions applied to a screen to determine if it can be exposed as the initial who-line screen.")


(DEFUN acceptable-initial-who-line-screen-p
       (screen)
  "Applies the tests in *INITIAL-WHO-LINE-SCREEN-TESTS* to SCREEN."
  (LOOP FOR test IN *initial-who-line-screen-tests*
	ALWAYS (FUNCALL test screen)))


;; may 03/09/89 
(DEFUN create-acceptable-initial-who-line-screen ()
  ;; First force buffer-address to appropriate, non-color value.
  (SETQ main-screen-buffer-address (IF sib-is-csib
				       CSIB-EXPANS-NO-TRANSP-VA
				       IO-SPACE-VIRTUAL-ADDRESS))
  ;; Make who-line-screen NIL so a new version gets created in #'who-line-setup
  (SETQ who-line-screen nil)
  ;; Don't use make-who-line-screen because appropriate who-line variables
  ;; don't get set (they end up NIL).
  (who-line-setup   ;;tv:make-who-line-screen
    nil  ;; Force unique name to be generated.
    nil
    *initial-who-line-screen-bits-per-pixel* 
    (if sib-is-csib :both *initial-who-line-screen-display-type*) ;; may 02/24/89 Need :both to get plane-mask to 255.
    )
  ;; Return who-line-screen just created
  who-line-screen)

;; may 03/09/89 
(DEFUN find-or-create-acceptable-initial-who-line-screen ()
  "Returns a screen which passes ACCEPTABLE-INITIAL-WHO-LINE-SCREEN-P.  If none found, creates one."
  (OR (LOOP FOR screen IN (REMOVE-DUPLICATES
			    (APPEND
			      (WHEN who-line-screen
				(LIST who-line-screen))
			      *screens-exposed-at-disk-save*
			      all-the-screens)
			    :test #'EQ
			    ;; Must remove from the back end, since the ones we probably want are at the front end...
			    :from-end t)
	    WHEN (acceptable-initial-who-line-screen-p screen)
	    RETURN screen)
      (create-acceptable-initial-who-line-screen)))

;;; ********* Attention ! *********
;;; This function is redefined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;
(DEFUN create-acceptable-initial-screen () ;; may 03/09/89 
  "Creates an acceptable initial screen which is compatible with the current value of WHO-LINE-SCREEN.
 WHO-LINE-SCREEN MUST be setup FIRST!"
  (define-screen
    'tv:standard-screen
    nil				   ;Force unique name to be generated.
    :buffer (tv:screen-buffer who-line-screen)
    :control-address tv:main-screen-control-address
    :property-list `(		   ;Skip a line so auto-indent lines up the properties...
		     :video ,(GET who-line-screen :video)
		     ;; Not sure what :video is used for.
		     ;; Maybe could be used in place of :display-type below. KJF
		     :controller ,(GET who-line-screen :controller)
		     ;; These next 2 are needed for multiple screen/dual monitor support.
		     ;; The combination of these 2 determines what plane mask the
		     ;; screen shall have, and how things behave in general.
		     :display-type ,(GET who-line-screen :display-type)
		     ;; logical-bits-per-pixel for a screen would be 7 if it were a color
		     ;; screen being used in dual-monitor mode.  02/21/88 KJF.
		     :logical-bits-per-pixel ,(GET who-line-screen :logical-bits-per-pixel)
		     :sib-number ,(GET who-line-screen :sib-number tv:*default-sib*))
;;    :screens-previously-selected-windows (MAKE-ARRAY 20.) ;; may 01/30/89 this is done in (initialize)
    :screens-who-line-screen who-line-screen
    :bits-per-pixel (tv:screen-bits-per-pixel who-line-screen)
    :locations-per-line (tv:sheet-locations-per-line who-line-screen)
    :height (- tv:main-screen-height (sheet-height who-line-screen))
    :width tv:main-screen-width)
  )

;;; ********* Attention ! *********
;;; This function is redefined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;

(DEFUN find-or-create-acceptable-initial-screen ()
  "Finds or creates a screen which passes ACCEPTABLE-INITIAL-SCREEN-P.
the screen is returned."
  (OR (LOOP FOR screen IN (REMOVE-DUPLICATES
			    (APPEND
			      (WHEN initial-lisp-listener
				(LIST (sheet-get-screen initial-lisp-listener)))
			      (WHEN main-screen
				(LIST main-screen))
			      *screens-exposed-at-disk-save*
			      all-the-screens)
			    :test #'EQ
			    ;; Must remove from the back end, since the ones we probably want are at the front end...
			    :from-end t)
	    WHEN (acceptable-initial-screen-p screen)
	    RETURN screen)
      (create-acceptable-initial-screen)))


(DEFUN remove-from-screens-previously-selected-windows (window)
  "This works when moving WINDOW from a screen, whether or not the screen is DEFAULT-SCREEN.
In the case of DEFAULT-SCREEN, WINDOW is also removed from the global
PREVIOUSLY-SELECTED-WINDOWS because DEFAULT-SCREEN's SCREENS-PREVIOUSLY-SELECTED-WINDOWS
instance variable points to the same array pointed to by the global variable."
  (WHEN window
    (LET ((psw (tv:screen-screens-previously-selected-windows (sheet-get-screen window))))
      ;; After source-build where it is assured all screens have an array in this slot, this let-if can be removed...
      (LET-IF psw ((tv:previously-selected-windows psw))
	(tv:remove-from-previously-selected-windows window)))))


(DEFUN initial-screen-setup ()
  "Finds or creates an acceptable initial screen and assigns it to *INITIAL-SCREEN*.
If INITIAL-LISP-LISTENER exists, forces it to be a legal inferior of *INITIAL-SCREEN*.
ALSO finds or creates the who-line-screen for *initial-screen*."
  (SETF who-line-screen (find-or-create-acceptable-initial-who-line-screen))
  (SETF *initial-screen* (find-or-create-acceptable-initial-screen)) ;; NEEDS who-line-screen !
  ;; Make INITIAL-LISP-LISTENER a valid inferior of *INITIAL-SCREEN*...
  (WHEN (AND initial-lisp-listener
	     (NOT (MEMBER initial-lisp-listener (sheet-inferiors *initial-screen*) :test #'EQ)))
    ;; May want to move associated suggestions frames at this point also...
    ;; Set-superior may remove INITIAL-LISP-LISTENER from the INFERIORS list of *INITIAL-SCREEN*,
    ;;  but it doesn't invoke the DEACTIVATE daemons...
    (SEND initial-lisp-listener :deactivate)
;;    ;; Remove next line when this logic gets put in :set-superior...
;;    (remove-from-screens-previously-selected-windows initial-lisp-listener)
    ;; Binding this global to T causes conversion to monochrome if necessary, instead or error signalled...
    (LET ((tv:*convert-color-sheet-to-monochrome* t))
      (SEND initial-lisp-listener :set-superior *initial-screen*))
    ;; Set-superior may put INITIAL-LISP-LISTENER on the INFERIORS list of *INITIAL-SCREEN*,
    ;;  but it doesn't invoke the ACTIVATE daemons...
    (SEND initial-lisp-listener :activate)))

;; Redefined for Multiple Monitors on 09-27-88 by KJF and CJJ.
;; With MMON installed, the global variable tv:WHO-LINE-SCREEN gets set to
;; NIL by a deexpose daemon when the who-line gets deexposed.  Therefore we
;; don't need to set it to NIL here.  Without MMON, it does need to be set
;; to NIL here so that the right stuff will happen.
(DEFUN expose-initial-screens ()
  "Determines which screens should be exposed on available display hardware, and exposes them."
  (unless (mmon-p)
    ;; Screen expose method also exposes the associated who-line and makes it the "active" who-line
    ;; Since we're NILing out WHO-LINE-SCREEN, *INITIAL-SCREEN* exposure won't be able to deexpose it,
    ;; so we have to do it here.
    (WHEN (SEND who-line-screen :exposed-p)
      (SEND who-line-screen :deexpose))
    
    ;; NIL it out to force initialization in (standard-screen :around :expose)
    (SETF who-line-screen nil)
    
    ;; Prevent deexposure of DEFAULT-SCREEN when it hasn't been exposed yet...
    (WHEN (AND default-screen (NOT (SEND default-screen :exposed-p)))
      (SETF default-screen NIL)))
  ;; ALWAYS do this
  (SEND *initial-screen* :expose))


(DEFUN compress-array
       (background-color width height from-array from-x from-y to-array to-x to-y)
  "Compresses FROM-ARRAY into TO-ARRAY.  If an element of FROM-ARRAY is EQUAL to
BACKGROUND-COLOR, places a zero into the corresponding element of TO-ARRAY.
Otherwise, places a one into the corresponding element of TO-ARRAY.
Similar to BITBLT, but much slower."
  (LOOP FOR x FROM 0 BELOW (ABS width)
	FOR source-x = (MOD (IF (MINUSP width)
				(- from-x width x 1)
				(+ from-x x))
			    (ARRAY-DIMENSION from-array 1))
	FOR destination-x = (IF (MINUSP width)
				(- to-x width x 1)
				(+ to-x x))
	DOING
	(LOOP FOR y FROM 0 BELOW (ABS height)
	      FOR source-y = (MOD (IF (MINUSP height)
				      (- from-y height y 1)
				      (+ from-y y))
				  (ARRAY-DIMENSION from-array 0))
	      FOR destination-y = (IF (MINUSP height)
				      (- to-y height y 1)
				      (+ to-y y))
	      DOING
	      (SETF (AREF to-array destination-y destination-x)
		    (IF (EQUAL (AREF from-array source-y source-x)
			       background-color)
			0.
			1.)))))

;;; ********* Attention ! *********
;;; This function is redefined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;
;;; NOTE: This is not called from WINDOW-INITIALIZE when (MAC-SYSTEM-P).
(DEFUN initialize () ;&optional (color-screen? tv:*color-system*)) ;; may 03/09/89 
  ;; MAIN-SCREEN isn't created here anymore, so optional argument isn't needed.  CJJ 04/14/88
  (sheet-clear-locks)
  ;; Initialize the user-input activity time to the current time.  That
  ;; is, make it look like the user just hit (i.e. pressed) a key on the
  ;; keyboard (or mouse).
  (SETQ kbd-last-activity-time (TIME))
  ;;; >>> now see which board we are on
  ;;; Leave MAIN-SCREEN-BUFFER-ADDRESS as is, unless NIL.  This eliminates one more reference to *COLOR-SYSTEM*.  CJJ 04/14/88
  (UNLESS main-screen-buffer-address
    (SETQ main-screen-buffer-address
	  (IF sib-is-csib
	      ;; Always create a monochrome screen unless color explicitly requested.  CJJ 04/14/88
	      csib-expans-no-transp-va
	      ;;(IF tv:*color-system*
	      ;;    csib-color-no-transp-va
	      ;;    csib-expans-no-transp-va)
	      io-space-virtual-address)))
  (IF sib-is-csib
      (PROGN
	(SETF time:keyboard-base #xf30000
	      time:configuration-rom-base #xff0000)
	(init-csib-registers)) ;;; one time initialization of csib control registers
      (SETF time:keyboard-base #xfc0000
	    time:configuration-rom-base #xfe0000))
  ;; may 03/09/89 Moved this below since all AND conditions were never
  ;; true in system build.
;  ;; Ensure setup for multiple-screens has been done.  CJJ 04/20/88
;  (AND main-screen
;       who-line-screen
;       (NOT (screen-screens-previously-selected-windows main-screen))
;       (things-to-do-first-time))  
  ;; WHO-LINE-SCREEN is set up below in conjuction with *INITIAL-SCREEN*.  CJJ 04/14/88
  (initial-screen-setup)   ;; was (who-line-setup)

  ;; Use *INITIAL-SCREEN* for MAIN-SCREEN when MAIN-SCREEN doesn't exist - at build time.
  ;;(OR main-screen (SETQ main-screen *initial-screen*))
  ;; may 03/09/89. MAIN-SCREEN and *initial-screen* MUST be same at boot and FOREVER.
  ;; The variable main-screen has been renamed to default-screen. A new variable *initial-screen* 
  ;; is now needed for boot purposes when more than one screen became possible.
  ;; Now main-screen is a misnomer since more that one screen can exist.
  ;; Any code referencing main-screen should probably use default-screen instead.
  (SETQ main-screen *initial-screen*) ;; may 03/09/89 

  ;; Ensure setup for multiple-screens has been done.
  ;; may 03/09/89 Moved and modified call to things-to-do-first-time from above for system build.
  (unless (screen-screens-previously-selected-windows *initial-screen*)
    (things-to-do-first-time))

  (SETQ *landscape-monitor* (> (sheet-inside-width  *initial-screen*)
			       (sheet-inside-height *initial-screen*)))
  ;; Use *INITIAL-SCREEN* instead of MAIN-SCREEN for MOUSE-SHEET and DEFAULT-SCREEN.  CJJ 04/14/88
  (SETQ mouse-sheet *initial-screen*)
  (SETQ default-screen *initial-screen*
	inhibit-screen-management nil
	screen-manager-top-level t
	screen-manager-queue nil))


;; >>> Added 04/20/88 KJF.
(DEFVAR *do-extra-things-on-screen-expose* t)

(DEFCONSTANT *printer-id* 255.)			;ab 9/15/88

(DEFUN printer-screen-p (screen)		;ab 9/15/88
  (AND (mac-window-p screen)
       (NUMBERP (sheet-window-id screen))	       ;ab 11/22/88
       (= *printer-id* (sheet-window-id screen))))

;; >> Change to handle screens with same names.  04/10/88 KJF.
;; >> If name passed in as NIL, create a name to use.  04/19/88 KJF.
;; >> Set do-extra-things-... to nil for :expose)
;; may 01/30/89 MMON integration changes.
(DEFUN DEFINE-SCREEN (FLAVOR NAME &REST ARGS)
  "Define a screen which will be completely visible to the user.  NAME should be
unique for each screen created.  If name is passed in as NIL, an attempt will be
made to create an unique name to use."
  ;; If name passed in as NIL, create an unique name to use.
  (UNLESS name
    (SETQ name (FORMAT nil "~a~d" "Screen-" (GET-UNIVERSAL-TIME)))
    ;; To ensure unique time (name), let's sleep.  This is really needed for cases of
    ;; creating a standard-screen followed immediately by creating a who-line-screen.
    (SLEEP 1))
  ;; Do not allow 2 different screens to have equal names.  See comment(s) below.  04/09/88 KJF.
  ;; Not anymore.  Bug 8649 came in, so we're going back to old way.  08/23/88 KJF.
  ;; But this should be O.K. now since tv:make-a-screen and tv:create-a-screen check for
  ;; duplicate names, along with generating an unique name if name passed in as NIL.
  ;; MMON functions tv:make-dual and tv:make-cmc call their own version of tv:make-a-screen
  ;; which also handles the problem of duplicate names being specified.
  ;;  (DOLIST (screen all-the-screens)
  ;;    (ASSERT (NOT (STRING-EQUAL name (SEND screen :name))) (name)
  ;;	    "Screen ~s is already using the name ~s.
  ;;SCREENS must have unique names.
  ;;Press RESUME to specify a different name for the screen being created.
  ;;NAME should be specified as a string." screen name))
  (LET ((SCREEN (APPLY #'MAKE-WINDOW FLAVOR :NAME NAME ARGS)))
    ;; Get rid of any screens which have the same name.  This is a fix
    ;; to a bug which occurred when a new who line screen was created.
    ;; Note that this should be a safe thing to do since every screen
    ;; should have a unique name.
    ;; The above (ASSERT ...) change should eliminate having to do this anymore.
    ;; Just removing screens from all-the-screens with no warnings or indications is NOT a good
    ;; thing to do.  04/09/88 KJF.
    ;; Going back to old way. 08/23/88 KJF
    (SETQ ALL-THE-SCREENS
	  (REMOVE SCREEN (THE LIST ALL-THE-SCREENS) :TEST
		  #'(LAMBDA (FOO BAR)
		      (STRING-EQUAL (SEND FOO :NAME) (SEND BAR :NAME)))))
    (PUSH SCREEN ALL-THE-SCREENS)
    (WHEN (mac-screen-p screen)
      (define-mac-resident-exp-screen screen)
      (WHEN (printer-screen-p screen)			;;ab 9/15/88.  Force alu for printer screen for correct
	(SETF (sheet-erase-aluf screen) alu-setz)))	;;lasar printer support on microExplorer.

    (when (AND (mac-system-p) 			;; may 03/20/89 
	       (typep screen 'who-line-screen)) ;; may 02/24/89
      ;; Guarantee any access to screen-array invokes debugger rather than crashing
      (SETF (sheet-screen-array screen) nil) 	;; may 02/24/89 
      ;; Exit. We can't expose on mac with explorer screen array
      (return-from DEFINE-SCREEN screen))	;; may 02/24/89 

    ;; Bind global special so certain things don't happen during this expose.
    ;; See method (standard-screen :around :expose).  04/20/88 KJF.
    (LET ((*do-extra-things-on-screen-expose* nil))
      (SEND SCREEN :EXPOSE))
    SCREEN))

;; Changed function to be sure arrays match.  Also, with the introduction of multiple screen
;; support, must use default-screen instead of main-screen.  04/23/88 KJF.
(DEFVAR MAIN-SCREEN-AND-WHO-LINE NIL)
;; Make this smarter for Multiple Monitor/CSIB (MMON) systems.  08/20/88 KJF
;; Note old version is commented out below.
(DEFUN main-screen-and-who-line ()
  (LET ((element-type (IF sib-is-csib '(MOD 256) 'BIT))
	(frame-buffer
	  (IF (mmon-p)
	      (SEND (AREF *all-the-monitor-controllers* 0)
		    :frame-buffer-for (IF sib-is-csib 8. 1.))
	      (IF sib-is-csib
		  tv:csib-color-no-transp-va
		  tv:io-space-virtual-address))))
    (IF (AND (TYPEP main-screen-and-who-line 'ARRAY)
	     (EQUAL (ARRAY-ELEMENT-TYPE main-screen-and-who-line)
		    element-type))
	(ADJUST-ARRAY main-screen-and-who-line
		      `(,main-screen-height ,main-screen-width)
		      :element-type element-type
		      :displaced-to frame-buffer)
	(SETQ main-screen-and-who-line
	      (MAKE-ARRAY `(,main-screen-height ,main-screen-width)
			  ;; Cannot use :element-type element-type because of strange error
			  ;; generated by MAKE-ARRAY function.  Don't ask me why.  KJF.
			  :type (IF (EQ element-type 'BIT) 'art-1b 'art-8b)
			  :displaced-to frame-buffer)))
    main-screen-and-who-line))

;;(DEFUN MAIN-SCREEN-AND-WHO-LINE ()
;;  (IF (AND MAIN-SCREEN-AND-WHO-LINE
;;	   (TYPEP MAIN-SCREEN-AND-WHO-LINE 'ARRAY)
;;	   (EQ (ARRAY-ELEMENT-TYPE MAIN-SCREEN-AND-WHO-LINE)
;;	       (ARRAY-ELEMENT-TYPE (SHEET-SCREEN-ARRAY DEFAULT-SCREEN))))
;;      (ADJUST-ARRAY MAIN-SCREEN-AND-WHO-LINE (LIST MAIN-SCREEN-HEIGHT MAIN-SCREEN-WIDTH)
;;		    :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE (SHEET-SCREEN-ARRAY DEFAULT-SCREEN))
;;		    :DISPLACED-TO (SCREEN-BUFFER DEFAULT-SCREEN))
;;      (SETQ MAIN-SCREEN-AND-WHO-LINE
;;	    (MAKE-ARRAY `(,MAIN-SCREEN-HEIGHT ,MAIN-SCREEN-WIDTH)
;;			:TYPE (SHEET-ARRAY-TYPE DEFAULT-SCREEN)
;;			:DISPLACED-TO (SCREEN-BUFFER DEFAULT-SCREEN))))
;;  MAIN-SCREEN-AND-WHO-LINE)

;;; moved the following to the top  of the file jlm 4/10/89
;(DEFVAR INITIAL-LISP-LISTENER nil ;; may 03/23/89 was missing nil
;        "Window instance for the first Lisp Listener that is created.")

;;; This function is called from the :once and :system initialization lists in INITIALIZATIONS.LISP.
(DEFUN window-initialize (&aux first-time)
  (DECLARE (SPECIAL INITIAL-LISP-LISTENER)) ;; may 03/23/89 quiet cwarns
  ;; done to setup the MP:*GLOBAL-STATE-BLOCK* var
  (and (si:mp-system-p)
       (si:cool-boot-p)
       mp-setup-gsb-window-early
       (funcall mp-setup-gsb-window-early))
  (IF (mac-system-p)
      (PROGN
        (sheet-clear-locks)
	;; Initialize the user-input activity time to the current time.  That
	;; is, make it look like the user just hit (i.e. pressed) a key on the
	;; keyboard (or mouse).
	(SETQ kbd-last-activity-time (TIME))
	(who-line-setup)
	(SETQ *landscape-monitor* t))
      ;; ELSE...
      (PROGN
	;; Don't allow *COLOR-SYSTEM* when we don't have a CSIB.  CJJ 04/13/88
	(UNLESS sib-is-csib
	  (SETF *color-system* nil))
	(initialize)
	(when (si:mp-system-p)
	  (unless *mp-blank-window*
	    (setf *mp-blank-window* (make-instance 'w:window))
	    (send (car (send *mp-blank-window* :blinker-list)) :set-visibility nil)
	    (send (car (send *mp-blank-window* :blinker-list)) :set-deselected-visibility nil)
	    (send *mp-blank-window* :set-label nil))
	  (setup-screens-for-mp)
	  (when (and (si:cool-boot-p)
		      mp-count-displayed-screens
	    (setf *mp-full-screen-mode*
		  (= (funcall mp-count-displayed-screens) 1)))
	    ))
	;; Now that we have multiple screens not intended for concurrent exposure, don't expose them all.  CJJ 04/13/88.
	;;(DOLIST (s all-the-screens)
	;;  (SEND s :expose))
	(w:expose-initial-screens)))
  (SETQ kbd-tyi-hook nil
	process-is-in-error nil)
   ;; So it stays latched here during loading.
  (OR (EQ who-line-process si:initial-process)
      (SETQ who-line-process nil))
  (WHEN (mac-system-p)
    (SETQ mouse-sheet nil
	  default-screen nil
	  inhibit-screen-management nil
	  screen-manager-top-level t
	  screen-manager-queue nil)
    (SETF first-time t)
    (after-tv-initialized)			;sets up initial Lisp Listenr for MX.
    (SETQ mouse-sheet (w:sheet-get-screen initial-lisp-listener)
	  default-screen mouse-sheet
	  ;; Assign a value for *INITIAL-SCREEN* on MX, also.  CJJ 04/14/88.
	  w:*initial-screen* mouse-sheet)
    (SETQ *window-system-mouse-on-the-mac* (mac-window-p default-screen)))
  (UNLESS initial-lisp-listener
    (SETQ initial-lisp-listener (make-window 
				  ;; If we have a simple lisp listener flavor then instantiate
				  ;; that, otherwise, get the UCL version.
				  (IF (GET 'simple-lisp-listener 'si:flavor)
				      'simple-lisp-listener
				      ;;ELSE
				      'lisp-listener)
				  :process si:initial-process)
	  first-time t))
  (if (and (si:mp-system-p)
	   (si:cool-boot-p)
	   mp-displayp
	   (not (funcall mp-displayp (logand #xf si:processor-slot-number))))
      (progn (setf default-screen main-screen)
	     (setf who-line-screen (screen-screens-who-line-screen main-screen)) ;; may 03/07/89 
	     (when MP-disable-run-light (funcall MP-disable-run-light)))
      (SEND initial-lisp-listener :select))
  (WHEN first-time
    (SETQ *terminal-io* initial-lisp-listener))
  (OR (MEMBER 'blinker-clock clock-function-list :test #'EQ)
      (PUSH 'blinker-clock clock-function-list)))

(defun setup-screens-for-mp ()
  (let* ((number-of-screens (if (and (si:cool-boot-p) mp-count-displayed-screens)
				(funcall mp-count-displayed-screens)
				(length (si:find-explorer-II-procs))))
	 (rel-pos (if (si:cool-boot-p)
		      (if (or (= number-of-screens 1)
			      (not mp-get-relative-position-of-screen )) 
			  0
			  (funcall mp-get-relative-position-of-screen (logand #xf si:processor-slot-number)))
		      (or (position (logand #xf si:processor-slot-number) (si:find-explorer-II-procs)) 0)))
	 
	 (WHO-LINE-DOC-LINES DEFAULT-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES)
	 (WHO-LINE-DOC-FONT (LIST *MOUSE-DOCUMENTATION-LINE-STANDARD-FONT*))
	 (WHO-VSP WHO-LINE-VSP)
	 (WHO-LINE-DOC-FONT-HEIGHT
	       (FONT-CHAR-HEIGHT *MOUSE-DOCUMENTATION-LINE-STANDARD-FONT*))
	 (WHO-LINE-STATUS-FONT-HEIGHT
	       (FONT-CHAR-HEIGHT *STATUS-LINE-STANDARD-FONT* ))
	 )
    (setf tv:main-screen-height (max (floor tv:orig-main-screen-height number-of-screens) tv:min-main-screen-height))
    (setf tv:main-screen-offset-for-proc (* tv:main-screen-height rel-pos))
    (FUNCALL WHO-LINE-SCREEN
	     :CHANGE-OF-SIZE-OR-MARGINS
	     :BOTTOM  (+ MAIN-SCREEN-HEIGHT main-screen-offset-for-proc)
	     :HEIGHT (+ WHO-VSP
			(* WHO-LINE-DOC-LINES
			   (+ WHO-LINE-DOC-FONT-HEIGHT WHO-VSP))
			WHO-VSP
			WHO-LINE-STATUS-FONT-HEIGHT
			WHO-VSP))
    
    (DOLIST (WIND (SEND WHO-LINE-SCREEN :INFERIORS))
      (COND ((EQ WIND WHO-LINE-DOCUMENTATION-WINDOW)
	     (SEND WIND :SET-FONT-MAP WHO-LINE-DOC-FONT)
	     (SEND WIND :SET-VSP WHO-VSP)
	     (Send WIND :CHANGE-OF-SIZE-OR-MARGINS
		   :HEIGHT (+ (* WHO-LINE-DOC-LINES
				 (+ WHO-LINE-DOC-FONT-HEIGHT WHO-VSP)) WHO-VSP)
		   :LEFT 0
		   :RIGHT (SHEET-WIDTH          WHO-LINE-SCREEN)
		   :TOP 0))
	    (T (SEND WIND :CHANGE-OF-SIZE-OR-MARGINS
		     :BOTTOM (SHEET-HEIGHT      WHO-LINE-SCREEN)
		     :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)))))
    (FUNCALL *initial-screen*
	     :CHANGE-OF-SIZE-OR-MARGINS
	     :HEIGHT (- MAIN-SCREEN-HEIGHT
			(SHEET-HEIGHT WHO-LINE-SCREEN))
	     :BOTTOM (- (+ MAIN-SCREEN-HEIGHT main-screen-offset-for-proc) (SHEET-HEIGHT WHO-LINE-SCREEN)))
    ))


(DEFVAR SYNC-RAM-CONTENTS :UNBOUND
  "Data to load into the TV board's sync memory.")

(DEFUN SET-DEFAULT-FONT (FONT-DESCRIPTOR)
  "Make FONT-DESCRIPTOR be the default font.  All windows that
were set up to use the default font will presently be using
FONT-DESCRIPTOR.

FONT-DESCRIPTOR should be a font descriptor, such as the name of a
	font."
  (SET-STANDARD-FONT :DEFAULT FONT-DESCRIPTOR))

(DEFUN SET-STANDARD-FONT (PURPOSE FONT-DESCRIPTOR)
  "Make FONT-DESCRIPTOR be the standard font for purpose
PURPOSE.  All windows that were set up to use the font for PURPOSE
will presently be using FONT-DESCRIPTOR.

FONT-DESCRIPTOR should be a font descriptor, such as the name of a
	font."
  (DOLIST (SCREEN ALL-THE-SCREENS)
    (SET-SCREEN-STANDARD-FONT SCREEN PURPOSE FONT-DESCRIPTOR)))

(DEFUN GET-STANDARD-FONT (FONT-PURPOSE &OPTIONAL THE-SCREEN)
  "Returns the font object assigned for this font-purpose.
Font-Purpose is a keyword indicating the kind of use.
The-screen is the screen which is associated with this font purpose.
	This defaults to default-screen."
  (EVAL (CDR (ASSOC FONT-PURPOSE (SCREEN-FONT-ALIST (OR THE-SCREEN
							DEFAULT-SCREEN))))))

(DEFUN MAKE-FONT-PURPOSE (FONT-SPECIFIER FONT-PURPOSE)
  "Adds or alters the FONT-PURPOSE to be associated with a different FONT.
This affects all of the screens."
  (LET (FONT-ALIST)
    (DOLIST (A-SCREEN ALL-THE-SCREENS)
      (SETQ FONT-ALIST (SCREEN-FONT-ALIST A-SCREEN))
      ;; If the purpose is already there, smash it with the new font.
      (IF (GET-STANDARD-FONT FONT-PURPOSE A-SCREEN)
	  (RPLACD (ASSOC FONT-PURPOSE FONT-ALIST) FONT-SPECIFIER)
	  ;;ELSE
	  (SETF (SCREEN-FONT-ALIST A-SCREEN)
		;; Append the new font purpose to the
		;; beginning of the font-alist.
		(PUSH `(,FONT-PURPOSE . ,(IF (SYMBOLP FONT-SPECIFIER)
					     FONT-SPECIFIER
					     (FONT-NAME FONT-SPECIFIER)))
		      FONT-ALIST))))))

(DEFUN SET-SCREEN-STANDARD-FONT (SCREEN PURPOSE FONT-DESCRIPTOR)
  "Make FONT-DESCRIPTOR be the standard font for purpose PURPOSE
on SCREEN.  All windows on SCREEN that were set up to use the font
for PURPOSE will presently be using FONT-DESCRIPTOR."
  ;; Make absolutely sure we really have a font,
  ;; since if we don't this will wedge everything.
  (LET ((FONT (SEND SCREEN :PARSE-FONT-DESCRIPTOR FONT-DESCRIPTOR))
	FONT-NAME)
    (DO NIL
	((FONT-OBJECT-P FONT))
      (SETQ FONT
	    (FONT-EVALUATE (CERROR T NIL :WRONG-TYPE-ARG "~S is not a font name" FONT-DESCRIPTOR))))
    (SETQ FONT-NAME (FONT-NAME FONT))
    (LET* ((STANDARD-NAME (SEND SCREEN :FONT-NAME-FOR PURPOSE T))
	   (OLD-FONT (SYMBOL-VALUE STANDARD-NAME)))
	   ;; If the screen has no entry for PURPOSE on its font alist,
	   ;; :FONT-NAME-FOR returns T because we supplied that as the default.
	   ;; We do that so we can avoid clobbering the font names on the
	   ;; DEFAULT-FONT-ALIST.
      (COND
	((NEQ STANDARD-NAME T)
	 (SET STANDARD-NAME FONT-NAME)
	 (IF (EQ PURPOSE :DEFAULT)
	   (SEND SCREEN :CHANGE-OF-DEFAULT-FONT (FONT-EVALUATE OLD-FONT)
	      (FONT-EVALUATE FONT-NAME))
	   (SEND SCREEN :CHANGE-OF-DEFAULT-FONT NIL NIL)))))))
