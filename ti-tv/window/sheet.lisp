;;; -*- Mode:Common-Lisp; Package:TV; Base:10; Fonts:(CPTFONT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983- 1989 Texas Instruments Incorporated.  All rights reserved.
;;; ** (c) Copyright 1980 by Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 05/08/89   LG     Modified sheet-get-lock, sheet-release-lock, sheet-get-temporary-lock, and
;;; 		   sheet-release-temporary-lock to comprehend Mac-based fast rubberbanding.
;;; 04/04/89   MAY   Added defmacro WITH-FONT-MAP & defun show-font-map.
;;; 02/09/89   LG     Replaced a (mac-system-p) with a (mac-screen-p ...) in (sheet :init) so an mX lashup would not treat its
;;; 		   Explorer windows as mX windows.
;;; 01/30/89   KJF    [may] Change to (sheet :inverse-around :init) for MMON.
;;;      		     Changes to :expose and :deexpose methods of sheet for Multiple Monitor (MMON) support.
;;;   		     Other changes for Multiple Monitor (MMON) support, search for MMON
;;; 11-17-88   LG     Allow :change-of-size-or-margins to make a window wider than its superior.
;;; 11-17-88   LG     When an mX window is created don't allocate a window-id or Mac-side data structures.
;;; 		    Just set its window-id to T to mark it as a deactivated mX window and remember its
;;; 		    bit-array/buffer if it has one.
;;; 10-24-88   MAY    Changed (SHEET :END-OF-PAGE-EXCEPTION) to restore (sheet-more-flag)
;;;                      clobbered by (sheet-home). Fixes SPR addin-bug #'s 188 201 205
;;; 09-07-88   MAY    Changed (SHEET :CHANGE-OF-DEFAULT-FONT) to search font-map more
;;;                      efficiently, and more consistently. Fixes SPR 5377.
;;; 06-07-88   LG       Passed old height & width to send-adjust-bit-array-maybe when
;;; 		     called from within grow-bit-array when on a Mac.
;;; 05/27/88   KJF      Check for default-screen being NIL in FONT-EVALUATE.  Needed during Window System
;;;                        build.  Caused by change by LG on 3/08/88, I think??
;;; 05-25-88   ab      Remove LG fix to SHEET-NEW-FONT-MAP of 5-17. 
;;;  05/25/88  LG      Fix the 5/10 patch to work right!!! (Mea culpa, mea culpa!)
;;; 05/23/88   KJF     Changed references to screen-descriptor to screens-who-line-screen due to source build
;;;                        being done which allowed instance variable to take on a meaningful name.
;;;  05/17/88  LG      In sheet-new-font-map, calculate a sheet's line-height as the sum
;;;  		        of the largest font-baseline and the largest (- font-char-height
;;;  		        font-baseline), each maximized independently over all the sheet's
;;;  		        fonts. 
;;;   05/10/88  LG         In :change-of-size-or-margins tell the Mac when a window with or
;;;                                 without a bit array changes size.  Don't try to redirect a window's
;;;                                 screen-array if there's nothing to indirect from or to.
;;;   04/26/88  KJF       Change to :set-superior to skip some code if sheet's screen was not created with
;;;                          one of the multiple screen functions.
;;;   04/23/88  KJF       Changes to :set-superior and :change-of-size-or-margins for multiple screen support.
;;;                          Changes are to ensure sheet's arrays change if locations-per-line changes, if
;;;                          appropriate.
;;;   04/23/88  KJF       Added compare-properties, to go with other screen property-list code.
;;;   04/10/88  KJF       Changes to (SCREEN :BEFORE :INIT), install-color-map.  Also, new code added for
;;;                          multiple screen/dual monitor support.  Search for KJF
;;;   3/18/88   KED       Changed sheet-parse-font-descriptor to return a new font rather
;;;                                than going into error handler when font not found on Mac screens.
;;;   03/08/88  LG	Made FONT-EVALUATE always parse the font descriptor it is about to return for 
;;;		graceful handling of Mac font descriptors. 
;;;   03/07/88  LG       Made sure :change-of-size-or-margins and :set-superior both
;;; 		      told the Mac whenever they changed a window's bit-array.
;;;   02/25/88  LG       (:method sheet :deexpose) was not telling the Mac about having created a bit array
;;;   			for a window.  Now it does.
;;;   02/21/88 KJF       Fixed the way color maps are set up for windows.
;;;   02/15/88  KJF        Make sure we have a superior in :change-of-size-or-margins.
;;;   2/12/88   LG          Made draw-rectangle-inside-clipped work on a Mac.
;;;   9/26/87  PMH       Added corrections for grow-bit-array to initialize new background area
;;;   6/10/87  KWW      Added initialization functions for color sheet, also :after :expose
;;;   4/24/87  TWE	Restored SHEET :DEACTIVATE to its original form.
;;;   4/16/87  TWE,KDB	Hacked up SHEET :DEACTIVATE to clean up non-garbage garbage in the io buffer record.
;;;   4/10/87  TWE	Fixed up set-current-font to check the font being in the font map differently.  Instead
;;;			of comparing font objects for EQness, it now checks font names for EQness.  This fixes
;;;			the problem of loading in a new font object for a font in the font map and it still just
;;;			as fast a check.
;;;   3/24,30/87  GRH	Speed up and simplify %draw-rectangle-clipped taking advantage of the new microcode.
;;;   3/23/87  TWE	Made TV patch 1.7 to fix DISPLAY-FONT again.  Now it works for both TV and W
;;;			GRAPHICS-MIXINs.
;;;   3/22/87  GRH	Back out all the changes to make narrower bit-save arrays until further notice.
;;;   3/20/87  KDB	Patched DISPLAY-FONT to use %draw-rectangle instead of :DRAW-LINE
;;;   3/20/87  KK	Modify FONT-EVALUATE to handle font purposes. Fixes SPR #3738.
;;;   3/16/87  GRH	Fix width of indirected screen arrays for windows without bit arrays broken
;;;			in the previous fix.
;;;   3/12/87  GRH	Fix bit-arrays so that they are only as wide as necessary, not as wide as the screen.
;;;   3/09/87  TWE	Defined 2 methods to allow one to set or get the keypad bit.  This is needed for the
;;;			VT100 emulator.
;;;   3/04/87  KDB	Fixed DISPLAY-FONT line-width problem. Closed SPR 3764. 
;;;  1/09/87   TWE	Fixed up the initialization for the keypad-enable initialization option to set the flag to
;;;			the proper value.
;;; 12/22/86   TWE	Made several changes to make the build process cleaner.
;;;			Put a DECLARE SPECIAL for PROCESS-IS-IN-ERROR inside the SHEET :NOTICE method.
;;;			Also fixed a mouse sensitivity problem in DISPLAY-FONT for the FED.
;;;			Moved the following from here to TVDEFS: SHEET-ME-OR-MY-KID-P, SHEET-CAN-GET-LOCK
;;;			and SHEET-CAN-GET-LOCK-INTERNAL.
;;; 12/08/86   TWE	Fixed (SHEET :MORE-EXCEPTION) by having it check the more-processing-global-enable
;;;			flag.
;;; 12/01/86   TWE	Added code to support the option :KEYPAD-ENABLE to the SHEET :INIT method.
;;; 11/20/86   SLM	Added calls to Suggestions macros for TV:MAKE-WINDOW, and (:METHOD TV:SHEET :MORE-EXCEPTION)
;;; 11/14/86   TWE	Fixed DISPLAY-FONT to handle non-existant fonts better, and to handle fonts with a
;;;			nil fill-pointer.
;;; 11/13/86   TWE	Fixed DISPLAY-FONT to add another option to :COLUMNS for the FED.
;;; 11/10/86   TWE	Fixed DISPLAY-FONT to work properly for variable width ISO fonts.
;;; 11/05/86   TWE	Declared COLD-LOAD-STREAM-OWNS-KEYBOARD to be declared special in
;;;			the functions where it is referenced.
;;; 10/29/86   TWE	Put the SYS package prefix in for COLD-LOAD-STREAM-OWNS-KEYBOARD.
;;;			Likewise for COLD-LOAD-STREAM
;;; 10/21/86   LGO	Replace calls to LISTP with CONSP.
;;; 10/17/86   TWE	Added the function DISPLAY-FONT which used to be in the FED package.
;;; 10/14/86   TWE	Added the function FONT-CHAR-MIN-RASTER-WIDTH which used to be in
;;;			the FED package.  It is used by Zmacs and the FED, so it is a bit
;;;			better to put it here.
;;; 09/05/86   TWE	Removed the defsubst for FONT-MAP-FONT-LIST since it is already
;;;			defined in the defstruct for FONT-MAP.
;;; 08/04/86   TWE	Changed type checks for font to use the FONT-OBJECT-P function.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 07/28/86   TWE	Modified references to the pixel functions to use ARRAY-DIMENSION
;;;			and MAKE-ARRAY instead.
;;; 06/20/86   TWE	Changed calls to REDIRECT-ARRAY to use array-element-type instead of
;;;			array-type.  This macro was changed to use the adjust-array function
;;;			which needs the element type of the array instead of just its type.
;;; 06/18/86   TWE	Changed SHEET-NEW-FONT-MAP to use COERCE instead of the obsolete
;;;			function LISTARRAY.  Changed the method (SCREEN :BEFORE :EXPOSE) to use
;;;			ADJUST-ARRAY instead of SI:CHANGE-INDIRECT-ARRAY.
;;; 04/11/86   LGO	Optimize SHEET-CAN-GET-LOCK and make it INLINE. Code read by TWE.
;;; 04/09/86   LGO	Move SHEET-OPEN-ALL-BLINKERS and SHEET-OPEN-BLINKERS to BLINKERS.
;;;			Removed obsolete blinker code from SHEET-PREPARE-SHEET-INTERNAL.
;;;			Code read by TWE.
;;; 04/08/86   LGO	Rewrite SHEET-NEW-FONT-MAP to be faster. Code read by TWE.
;;; 04/08/86   TWE	Altered SHEET-DEDUCE-AND-SET-SIZES and the before/after methods
;;;			for SCREEN to use *SCREEN-STANDARD-FONT-MAP* instead of
;;;			*WINDOW-STANDARD-FONT-MAP* when dealing with a font map for a SCREEN.
;;; 04/08/86   TWE	Changed the before method for SCREEN to use make-array instead of fillarray.
;;; 07-01-88   MAY	Added prepare-sheet around %draw-rectangle in (sheet :deexpose) to :clean. SPR 8283.

(DEFUN %DRAW-RECTANGLE-CLIPPED
       (WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET)
  "Draw rectangle in SHEET, coords relative to SHEET, clipping to SHEET."
  ;; An alternate approach would be to bind the clipping rectangle right edge,
  ;; but this might interfere with someone else's bindings.
  (when (> (+ width x-bitpos) (sheet-width sheet))
    (setq width (- (sheet-width sheet) x-bitpos)))
  ;; clipping top, left and bottom is always done by microcode.
  ;; clipping right is only sometimes done.
  (%DRAW-RECTANGLE WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET))

;;;This takes arguments relative to the inside and clips inside
(DEFUN DRAW-RECTANGLE-INSIDE-CLIPPED
       (WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET
        &AUX (INSIDE-LEFT (SHEET-INSIDE-LEFT SHEET))
        (INSIDE-TOP (SHEET-INSIDE-TOP SHEET)))
  "Draw rectangle in SHEET, coordinates relative to inside of SHEET,
clipping to inside.  Recall that the inside of SHEET is what is not part
of the margins."
  (SETQ X-BITPOS (+ X-BITPOS INSIDE-LEFT)
	Y-BITPOS (+ Y-BITPOS INSIDE-TOP))
  (IF (mac-window-p sheet)
      (send-draw-rectangle width height x-bitpos y-bitpos
			   black alu-function sheet t)
    ;; else...
    (AND (< X-BITPOS INSIDE-LEFT)
	 (SETQ WIDTH (- WIDTH (- INSIDE-LEFT X-BITPOS))
	       X-BITPOS INSIDE-LEFT))
    (AND (< Y-BITPOS INSIDE-TOP)
	 (SETQ HEIGHT (- HEIGHT (- INSIDE-TOP Y-BITPOS))
	       Y-BITPOS INSIDE-TOP))
    (SETQ WIDTH  (MIN WIDTH  (MAX 0 (- (SHEET-INSIDE-RIGHT SHEET)  X-BITPOS))))
    (SETQ HEIGHT (MIN HEIGHT (MAX 0 (- (SHEET-INSIDE-BOTTOM SHEET) Y-BITPOS))))
    (AND (> WIDTH 0) (> HEIGHT 0)
	 (%DRAW-RECTANGLE WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET))))


(DEFUN BITBLT-CLIPPED
       (ALU WIDTH HEIGHT FROM-ARRAY FROM-X FROM-Y TO-ARRAY TO-X TO-Y)
  "Like BITBLT except clips to the actual area of FROM-ARRAY and
TO-ARRAY.  Negative WIDTH and HEIGHT are not allowed."
  (LET* ((LEFT-OMIT (MAX (- (MAX 0 TO-X)   TO-X)
			 (- (MAX 0 FROM-X) FROM-X)))
	 (TOP-OMIT (MAX  (- (MAX 0 TO-Y)   TO-Y)
                         (- (MAX 0 FROM-Y) FROM-Y)))
	 (FROM-XEND (+ FROM-X WIDTH))
	 (FROM-YEND (+ FROM-Y HEIGHT))
	 (TO-XEND (+ TO-X WIDTH))
	 (TO-YEND (+ TO-Y HEIGHT))
	 (RIGHT-OMIT  (MAX (- TO-XEND
                              (MIN TO-XEND   (ARRAY-DIMENSION TO-ARRAY 1)))
                           (- FROM-XEND
                              (MIN FROM-XEND (ARRAY-DIMENSION FROM-ARRAY 1)))))
	 (BOTTOM-OMIT (MAX (- TO-YEND
                              (MIN TO-YEND   (ARRAY-DIMENSION TO-ARRAY 0)))
			   (- FROM-YEND
                              (MIN FROM-YEND (ARRAY-DIMENSION FROM-ARRAY 0)))))
	 (CLIPPED-WIDTH
	   (MAX 0 (- WIDTH  LEFT-OMIT RIGHT-OMIT)))
	 (CLIPPED-HEIGHT
	   (MAX 0 (- HEIGHT TOP-OMIT  BOTTOM-OMIT))))
    (AND (NOT (ZEROP CLIPPED-WIDTH))		;bitblt errs when w=h=0
	 (NOT (ZEROP CLIPPED-HEIGHT))		;and dims are out of bounds
	 (BITBLT ALU
		 CLIPPED-WIDTH CLIPPED-HEIGHT
		 FROM-ARRAY (+ FROM-X LEFT-OMIT) (+ FROM-Y TOP-OMIT)
		 TO-ARRAY   (+ TO-X   LEFT-OMIT) (+ TO-Y   TOP-OMIT)))))

;;;Primitives
(DEFMETHOD (SHEET :PRINT-SELF) (STREAM IGNORE SLASHIFY-P)
  (IF SLASHIFY-P
      ;; Do %POINTER explicitly.
      (SI:PRINTING-RANDOM-OBJECT (SELF STREAM :NO-POINTER)
	(FORMAT STREAM "~A ~A ~O ~A"
		(TYPE-OF SELF) NAME (%POINTER SELF)
		(IF EXPOSED-P "exposed"
		    (IF (OR (NULL SUPERIOR)
			    (MEMBER SELF (SHEET-INFERIORS SUPERIOR) :TEST #'EQ))
			"deexposed"
			"deactivated"))))
      (FUNCALL STREAM :STRING-OUT
               (STRING (OR (SEND SELF :NAME-FOR-SELECTION) NAME)))))

(DEFUN SHEET-CALCULATE-OFFSETS (WINDOW TOP)
  "Return the X and Y offset of the top left corner of WINDOW
relative to of TOP.  TOP should be a superior of WINDOW, to one or
more levels (or WINDOW itself).  TOP = NIL is equivalent to supplying
WINDOW's screen, assuming that a screen's offsets are always 0."
  (DO ((W WINDOW (SHEET-SUPERIOR W))
       (X-OFFSET 0)
       (Y-OFFSET 0))
      ((EQ W TOP)
       (VALUES X-OFFSET Y-OFFSET))
      (SETQ X-OFFSET (+ X-OFFSET (SHEET-X W))
	    Y-OFFSET (+ Y-OFFSET (SHEET-Y W)))))

(DEFUN SHEET-GET-SCREEN (SHEET &OPTIONAL HIGHEST)
  "Return the SCREEN that SHEET is on.
Actually, returns the highest level superior of SHEET that is either a
screen or not active.  If HIGHEST is non-NIL, inferiors of HIGHEST are
treated like screens."
  (DO ((SHEET SHEET SUPERIOR)
       (SUPERIOR SHEET (SHEET-SUPERIOR SUPERIOR)))
      ((OR (NULL SUPERIOR)
	   (EQ SUPERIOR HIGHEST))
       SHEET)))

(DEFUN MAP-OVER-EXPOSED-SHEETS (FUNCTION)
  "Call FUNCTION on every exposed sheet, on all screens."
  (DOLIST (SCREEN ALL-THE-SCREENS)
    (AND (SHEET-EXPOSED-P SCREEN)
	 (MAP-OVER-EXPOSED-SHEET FUNCTION SCREEN))))

(DEFUN MAP-OVER-EXPOSED-SHEET (FUNCTION SHEET)
  "Call FUNCTION on SHEET and all exposed inferiors to all levels."
  (DOLIST (SHEET (SHEET-EXPOSED-INFERIORS SHEET))
    (MAP-OVER-EXPOSED-SHEET FUNCTION SHEET))
  (FUNCALL FUNCTION SHEET))

(DEFUN MAP-OVER-SHEETS (FUNCTION)
  "Call FUNCTION on every active sheet, on all screens."
  (DOLIST (SCREEN ALL-THE-SCREENS)
    (MAP-OVER-SHEET FUNCTION SCREEN)))

(DEFUN MAP-OVER-SHEET (FUNCTION SHEET)
  "Call FUNCTION on SHEET and all active inferiors to all levels."
  (DOLIST (SHEET (SHEET-INFERIORS SHEET))
    (MAP-OVER-SHEET FUNCTION SHEET))
  (FUNCALL FUNCTION SHEET))

;;; This page implements locking for the window system.  The lock of a
;;; SHEET can be in one of the following states:
;;; Lock cell is NIL -- no lock, LOCK-COUNT must be zero.
;;; Lock cell is an atom and the lock count equals the lock count of the
;;;   superior then the sheet is locked, but can be temp locked by any
;;;   inferior of the lowest superior that is actually locked (lock-plus
;;;   state).  the lock count is greater than the lock count of the
;;;   superior then the sheet is really locked, and can only be locked by
;;;   the same unique ID.
;;; Lock cell is a list then the sheet is temp locked by the windows in
;;;   that list and if the lock count is non-zero then the window is also
;;;   lock-plus.

;;; What all this says, essentially, is that you can get the lock on the
;;; sheet and the sheet can be temp locked if all the temp lockers are
;;; being locked by the same operation that is locking the original
;;; sheet (these locks can happen in either order).

(DEFUN SHEET-GET-LOCK (SHEET &OPTIONAL (UNIQUE-ID CURRENT-PROCESS))
  "Lock SHEET's lock, waiting if necessary.
The locks of SHEET's inferiors are locked also."
  (DO ((INHIBIT-SCHEDULING-FLAG T T))
      (())
    (COND ((SHEET-CAN-GET-LOCK SHEET UNIQUE-ID)
	   (RETURN (PROG1 (SHEET-GET-LOCK-INTERNAL SHEET UNIQUE-ID)
			  (WHEN (mac-system-p)
			    (suppress-mac-based-rubberbanding sheet)))))
	  (T
	   (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	   (PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SHEET UNIQUE-ID)))))


(DEFUN SHEET-GET-LOCK-INTERNAL (SHEET UNIQUE-ID)
  "Really get the lock on a sheet and its inferiors, assuming lock is
available to us now.  INHIBIT-SCHEDULING-FLAG must be non-NIL."
  (OR INHIBIT-SCHEDULING-FLAG
      (FERROR NIL "SHEET-GET-LOCK-INTERNAL called with interrupts enabled."))
  (OR (SHEET-LOCK SHEET)
      ;; If lock is currently non-NIL, then initialize it to the unique-id
      (SETF (SHEET-LOCK SHEET) UNIQUE-ID))
  ;; Always bump the lock count here
  (SETF (SHEET-LOCK-COUNT SHEET) (1+ (SHEET-LOCK-COUNT SHEET)))
  (DOLIST (INFERIOR (SHEET-INFERIORS SHEET))
    (SHEET-GET-LOCK-INTERNAL INFERIOR UNIQUE-ID)))

(DEFUN SHEET-RELEASE-LOCK
       (SHEET &OPTIONAL (UNIQUE-ID CURRENT-PROCESS)
        &AUX (INHIBIT-SCHEDULING-FLAG T) LOCK)
  "Release a lock on a sheet and its inferiors"
  (COND ((OR (EQ UNIQUE-ID (SETQ LOCK (SHEET-LOCK SHEET)))
	     (AND LOCK (NOT (ZEROP (SHEET-LOCK-COUNT SHEET)))))
	 ;; If we own the lock, or if temp locked and the lock count is
         ;; non-zero, then we must decrement the lock count.
	 (SETF (SHEET-LOCK-COUNT SHEET) (1- (SHEET-LOCK-COUNT SHEET)))	 
	 (WHEN (AND (ZEROP (SHEET-LOCK-COUNT SHEET))
		    (NOT (CONSP LOCK)))
	   ;; If the count is currently zero, and the sheet is not
	   ;; temp-locked, then clear out the lock cell.
	   (PROG1 (SETF (SHEET-LOCK SHEET) NIL)
		  (WHEN (mac-system-p)
		    (unsuppress-mac-based-rubberbanding sheet))))
	 (DOLIST (INFERIOR (SHEET-INFERIORS SHEET))
	   (SHEET-RELEASE-LOCK INFERIOR UNIQUE-ID)))))

(DEFUN SHEET-CAN-GET-TEMPORARY-LOCK (SHEET REQUESTOR &AUX LOCK)
  "Returns T if the lock can be grabbed.  Should be called with
interrupts inhibited.  REQUESTOR is the temporary sheet that is going
to cover SHEET."
  (COND ((NULL (SETQ LOCK (SHEET-LOCK SHEET)))
	 ;; Can always get temporary lock if no previous locker
	 T)
	(T
	 ;; Determine if sheet is in Lock, Temp-Lock, Lock-Plus, or
         ;; Temp-Lock-Plus.
	 (LET* ((LC (SHEET-LOCK-COUNT SHEET))
		(SUP (SHEET-SUPERIOR SHEET))
		;; In plus state if sheet's lock count is the same as
                ;; that of its superior, and the lock count is non-zero
                ;; (this is for the case of a window being in temp-lock
                ;; state, but not being plussified).
		(PLUS (AND (NOT (ZEROP LC)) (= LC (SHEET-LOCK-COUNT SUP)))))
	   (COND (PLUS
		  ;; In plus state, determine if we are a valid temp
                  ;; locker (we must be an inferior (direct or indirect)
                  ;; of the lowest superior that is not in the plus
                  ;; state).
		  (SHEET-ME-OR-MY-KID-P
                    REQUESTOR
                    (DO ((OSUP SUP SUP))
                        (())
                      (SETQ SUP (SHEET-SUPERIOR OSUP))
                      (AND (OR (NULL SUP)
                               (> LC (SHEET-LOCK-COUNT SUP)))
                           ;; Found where the buck stops, return the sheet
                           (RETURN OSUP)))))
		 (T
		  ;; Otherwise, only ok to lock if already temp locked
		  (CONSP LOCK)))))))

(DEFUN SHEET-GET-TEMPORARY-LOCK (SHEET REQUESTOR)
  "Get a temporary lock on SHEET.
REQUESTOR is the temporary sheet that is going to cover SHEET."
  (DO ((INHIBIT-SCHEDULING-FLAG T T))
      ((SHEET-CAN-GET-TEMPORARY-LOCK SHEET REQUESTOR)
       ;; Make sure we lock in appropriate fashion (i.e. if the window
       ;; is already temp locked add another locker, else start the
       ;; list).  We don't have to worry about plus states, since
       ;; SHEET-CAN-GET-TEMPORARY-LOCK already worried for us.
       (LET ((LOCK (SHEET-LOCK SHEET)))
	 (PROG1 (SETF (SHEET-LOCK SHEET)
		      (IF (CONSP LOCK)
			  (CONS REQUESTOR LOCK)
			(CONS REQUESTOR nil)))
		(WHEN (mac-system-p)
		  (suppress-mac-based-rubberbanding sheet)))))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (PROCESS-WAIT "Window Lock"
                  #'SHEET-CAN-GET-TEMPORARY-LOCK SHEET REQUESTOR)))


(DEFUN SHEET-FIND-LOCKER (SHEET)
  (DO ((SUP SHEET) (LOCK)) (())
    (SETQ SUP (SHEET-SUPERIOR SUP))
    (OR SUP
        (FERROR NIL
                "Internal error - Lock count non-zero, but nobody is locked!"))
    (AND (ATOM (SETQ LOCK (SHEET-LOCK SUP)))
	 (RETURN LOCK))))

(DEFUN SHEET-RELEASE-TEMPORARY-LOCK
       (SHEET REQUESTOR &AUX (INHIBIT-SCHEDULING-FLAG T))
  "Release a temporary lock on a sheet."
  (LET ((LOCK (DELETE REQUESTOR (THE LIST (SHEET-LOCK SHEET)) :TEST #'EQ)))
    (PROG1 (SETF (SHEET-LOCK SHEET)
		 (OR LOCK (IF (ZEROP (SHEET-LOCK-COUNT SHEET))
			      NIL
			    (SHEET-FIND-LOCKER SHEET))))
	   (WHEN (mac-system-p)
	     (unsuppress-mac-based-rubberbanding sheet)))))


(DEFUN SHEET-FREE-TEMPORARY-LOCKS (SHEET)
  "Free all temporary locks on a sheet by deexposing the sheets that
  own the lock."
  (DO ((LOCK (SHEET-LOCK SHEET) (SHEET-LOCK SHEET)))
      ((NULL LOCK) T)
    (OR (CONSP LOCK)
        ;; Not temporary locked, can't do anything.
	(RETURN NIL))
    (OR (= DTP-INSTANCE (%DATA-TYPE (SETQ LOCK (CAR LOCK))))
        ;; The lock isn't an instance, can't do anything.
	(RETURN NIL))
    (OR (GET-HANDLER-FOR LOCK :DEEXPOSE)
        ;; An instance, but maybe not a window -- punt.
	(RETURN NIL))
    (COND ((CONSP (SHEET-LOCK LOCK))	;Is the locker also temp locked?
           ;; Yes, free it up first.  If ok, keep going.
	   (OR (SHEET-FREE-TEMPORARY-LOCKS LOCK)
	       (RETURN NIL))))
    (FUNCALL LOCK :DEEXPOSE)))

(DEFUN SHEET-CLEAR-LOCKS ()
  "Called in an emergency to reset all locks."
  (DOLIST (SHEET ALL-THE-SCREENS)
    (SHEET-CLEAR-LOCKS-INTERNAL SHEET)))

(DEFUN SHEET-CLEAR-LOCKS-INTERNAL (SHEET)
  (SETF (SHEET-LOCK SHEET) NIL)
  (SETF (SHEET-LOCK-COUNT SHEET) 0)
  (SETF (SHEET-TEMPORARY-WINDOWS-LOCKED SHEET) NIL)
  (SETF (SHEET-INVISIBLE-TO-MOUSE-P SHEET) NIL)
  (DOLIST (SHEET (SHEET-INFERIORS SHEET))
    (SHEET-CLEAR-LOCKS-INTERNAL SHEET)))

(DEFUN SHEET-ASSURE-LOCK-AVAILABLE (SHEET)
  "Wait till SHEET's lock is available, then return with interrupts off.
Must be called with INHIBIT-SCHEDULING-FLAG bound to T.  However,
other processes can run when this function is called."
  (DO () ((SHEET-CAN-GET-LOCK SHEET))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SHEET)
    (SETQ INHIBIT-SCHEDULING-FLAG T)))

;;;patched on 11 Dec 85 for DLS by GSM
(DEFUN SHEET-MORE-LOCK-KLUDGE (FUN &REST ARGS)
  (DECLARE (:SELF-FLAVOR SHEET))
;;;  ;; **********************************************************************		;patch dls
;;;  ;; ** The following is a total kludge and should not even be looked at **		;patch dls
;;;  ;; **********************************************************************		;patch dls
;;;  (IF (EQUAL USER-ID "RMS")								;patch dls
;;;      ;; Let's see what this is accomplishing, the hard way.				;patch dls
;;;      (APPLY FUN ARGS)								;patch dls
  ;; It seems we 1) set the lock state of this window and all inferiors
  ;; to be the same as this window's superior's lock state,
  ;; 2) do the operation,
  ;; 3) set the lock states back to what they were.
  ;; However, when this is called, normally the sheet is prepared
  ;; but not locked.  So it ought to do nothing, and that's what
  ;; appears to happen.
  (LET ((INHIBIT-SCHEDULING-FLAG T)
	(OLD-LOCK-STATE) (CHAR))
    (UNWIND-PROTECT
	(PROGN
	  (AND LOCK
	       (NEQ LOCK CURRENT-PROCESS)
	       (FERROR
                 NIL
		 "Attempt to **MORE** when sheet was not locked by current process."))
	  (SETQ OLD-LOCK-STATE
		(AND LOCK
                     (SHEET-MORE-LOCK-KLUDGE-LOCK-STATE
                       SELF
                       (SHEET-LOCK-COUNT SUPERIOR))))
	  (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	  (SETQ CHAR (APPLY FUN ARGS)))
      (AND OLD-LOCK-STATE (SHEET-GET-LOCK SELF))
      (SETQ INHIBIT-SCHEDULING-FLAG T)
      (AND OLD-LOCK-STATE
	   (SHEET-MORE-LOCK-KLUDGE-RESTORE-LOCK-STATE SELF OLD-LOCK-STATE))
      (PREPARE-SHEET (SELF)))			;Open blinkers.
    CHAR))
;;;  ;; ************** End of total, complete, and utter kludge **************		;patch dls
;;;  )											;patch dls


(DEFUN SHEET-MORE-LOCK-KLUDGE-LOCK-STATE
       (SHEET SUPERIOR-LC &OPTIONAL (STATE NIL))
  (DOLIST (I (SHEET-INFERIORS SHEET))
    (SETQ STATE (SHEET-MORE-LOCK-KLUDGE-LOCK-STATE I SUPERIOR-LC STATE)))
  (PUSH (CONS SHEET (- (SHEET-LOCK-COUNT SHEET) SUPERIOR-LC)) STATE)
  (OR (CONSP (SHEET-LOCK SHEET)) (SETF (SHEET-LOCK SHEET) NIL))
  (SETF (SHEET-LOCK-COUNT SHEET) SUPERIOR-LC)
  STATE)

(DEFUN SHEET-MORE-LOCK-KLUDGE-RESTORE-LOCK-STATE
       (SHEET STATE
        &OPTIONAL (SUPERIOR-LOCK-COUNT 0)
        &AUX LOCK-COUNT)
  ;; This code assumes that the caller has locked the sheet once already.
  (SETF (SHEET-LOCK-COUNT SHEET)
	(SETQ LOCK-COUNT 
	      (+ SUPERIOR-LOCK-COUNT (SHEET-LOCK-COUNT SHEET)
		 (OR (CDR (ASSOC SHEET STATE :TEST #'EQ)) 0)
		 -1)))
  (DOLIST (I (SHEET-INFERIORS SHEET))
    (SHEET-MORE-LOCK-KLUDGE-RESTORE-LOCK-STATE SHEET STATE LOCK-COUNT)))

(DEFUN SHEET-CAN-ACTIVATE-INFERIOR (SUPERIOR &AUX SUP-LOCK)
  "T if SUPERIOR's lock state permits this process to activate
inferiors of SUPERIOR."
  (OR (NULL (SETQ SUP-LOCK (SHEET-LOCK SUPERIOR)))
      (EQ SUP-LOCK CURRENT-PROCESS)
      (AND (CONSP SUP-LOCK) (OR (ZEROP (SHEET-LOCK-COUNT SUPERIOR))
                                (EQ CURRENT-PROCESS (SHEET-FIND-LOCKER SUPERIOR))))))

(DEFMETHOD (SHEET :INFERIOR-ACTIVATE)   (INFERIOR) INFERIOR)
(DEFMETHOD (SHEET :INFERIOR-DEACTIVATE) (INFERIOR) INFERIOR)
(DEFMETHOD (SHEET :INFERIOR-TIME-STAMP) (INFERIOR)
  INFERIOR				;Inferior getting stamped -- unused here
  TIME-STAMP)

(DEFMETHOD (SHEET :UPDATE-TIME-STAMP) ()
  (AND SUPERIOR
       (SETQ TIME-STAMP (FUNCALL SUPERIOR :INFERIOR-TIME-STAMP SELF))))

;Other flavors which provide the ability to do input will override this.
(DEFMETHOD (SHEET :DIRECTION) () :OUTPUT)

(DEFMETHOD (SHEET :CHARACTERS) () T)

(DEFMETHOD (SHEET :ELEMENT-TYPE) () 'CHARACTER)

;;; Activation and deactivation (these go with locking)
(DEFMETHOD (SHEET :ACTIVATE) (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Activates a sheet."
  (COND
    ((NOT (MEMBER (sheet-get-screen self) all-the-screens :test #'EQ))
     (SEND self :kill))
    ((NOT (FUNCALL SUPERIOR :INFERIOR-ACTIVATE SELF)))
    ((DO () ((MEMBER SELF (SHEET-INFERIORS SUPERIOR) :TEST #'EQ)  NIL)
       (COND
         ((NOT (SHEET-CAN-GET-LOCK SELF))
          (SETQ INHIBIT-SCHEDULING-FLAG NIL)
          (PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SELF)
          (SETQ INHIBIT-SCHEDULING-FLAG T))
         ((SHEET-CAN-ACTIVATE-INFERIOR SUPERIOR)
          (OR (ZEROP (SHEET-LOCK-COUNT SUPERIOR))
              ;; Superior is locked by us, must merge lock counts.
              (LOCK-SHEET (SELF)
		(LET ((**ACTIVATE-LOCK-COUNT** (SHEET-LOCK-COUNT SUPERIOR)))
		  (DECLARE (SPECIAL **ACTIVATE-LOCK-COUNT**))
		  (MAP-OVER-SHEET #'(LAMBDA (SHEET)
				      (SETF (SHEET-LOCK-COUNT SHEET)
					    (+ (SHEET-LOCK-COUNT SHEET)
					       **ACTIVATE-LOCK-COUNT**)))
				  SELF))))
          (RETURN T))
         (T
          (SETQ INHIBIT-SCHEDULING-FLAG NIL)
          ;; Wait for sheet to become activatable or to become activated.
          (PROCESS-WAIT "Activate" #'(LAMBDA (SHEET SUP)
                                       (OR (SHEET-CAN-ACTIVATE-INFERIOR SUP)
                                           (MEMBER SHEET (SHEET-INFERIORS SUP) :TEST #'EQ)))
                        SELF SUPERIOR)
          ;; Loop back to prevent timing screws.
          (SETQ INHIBIT-SCHEDULING-FLAG T))))
     ;; Executed if we are not active already.
     (when (mac-window-p self)
       (activate-Mac-window self))
     (SHEET-SET-SUPERIOR-PARAMS SELF (SHEET-LOCATIONS-PER-LINE SUPERIOR))
     (SHEET-CONSING
       (SETF (SHEET-INFERIORS SUPERIOR)
             (COPY-LIST (CONS SELF (SHEET-INFERIORS SUPERIOR))))))))

(DEFWRAPPER (SHEET :DEACTIVATE) (IGNORE . BODY)
  `(LOCK-SHEET (SELF)
     (DELAYING-SCREEN-MANAGEMENT . ,BODY)))

(DEFMETHOD (SHEET :DEACTIVATE) (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Deactivates a sheet.  Should be called by all deactivate methods to
do the actual work."
  (DECLARE (INLINE CLEAR-IO-BUFFER-RECORD))
  (COND
    ((NULL superior)			   ; Screen or already-deactivated window.
     (WHEN (TYPEP self 'screen)
       (SETF all-the-screens
	     (DELETE self w:all-the-screens))
       (WHEN (mac-window-p self)	   ; Tell the Mac to destroy the screen, too.
	 (deactivate-mac-screen self))))
    ((FUNCALL SUPERIOR :INFERIOR-DEACTIVATE SELF)
     (DO () ((NOT (MEMBER SELF (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ)))
       (SETQ INHIBIT-SCHEDULING-FLAG NIL)
       (SEND SELF :DEEXPOSE)
       (SETQ INHIBIT-SCHEDULING-FLAG T))
     (COND ((MEMBER SELF (SHEET-INFERIORS SUPERIOR) :TEST #'EQ) 
	    (OR (ZEROP (SHEET-LOCK-COUNT SUPERIOR))
		;; Superior is locked by us, must subtract his lock
		;; count from ours because he isn't going to do it
		;; for us when he gets unlocked.  (Note: the
		;; superior can't be locked by someone else as in
		;; the deactivate case because we own the lock on
		;; one of his inferiors (namely, us) preventing this
		;; situation from arising) That lock also prevents
		;; the lock count from going to zero in here.
		(LET ((**ACTIVATE-LOCK-COUNT** (SHEET-LOCK-COUNT SUPERIOR)))
		  (DECLARE (SPECIAL **ACTIVATE-LOCK-COUNT**))
		  (MAP-OVER-SHEET
		    #'(LAMBDA (SHEET)
			(SETF (SHEET-LOCK-COUNT SHEET)
			      (- (SHEET-LOCK-COUNT SHEET)
				 **ACTIVATE-LOCK-COUNT**)))
		    SELF)))
	    (SETF (SHEET-INFERIORS SUPERIOR)
		  (DELETE SELF (THE LIST (SHEET-INFERIORS SUPERIOR)) :TEST #'EQ))
	    (when (mac-window-p self)
	      (deactivate-Mac-window  self)))))))

(DEFUN SHEET-OVERLAPS-P
       (SHEET LEFT TOP WIDTH HEIGHT
        &AUX
        (W-X         (SHEET-X      SHEET))
        (W-Y         (SHEET-Y      SHEET))
        (W-X1 (+ W-X (SHEET-WIDTH  SHEET)))
        (W-Y1 (+ W-Y (SHEET-HEIGHT SHEET))))
  "True if a sheet overlaps the given area.
The specified coordinates are relative to SHEET's superior."
  (NOT (OR (>= LEFT W-X1)
	   (>= W-X (+ LEFT WIDTH))
	   (>= TOP W-Y1)
	   (>= W-Y (+ TOP  HEIGHT)))))

(DEFUN SHEET-OVERLAPS-EDGES-P
       (SHEET LEFT TOP RIGHT BOTTOM
        &AUX
        (W-X         (SHEET-X      SHEET))
        (W-Y         (SHEET-Y      SHEET))
        (W-X1 (+ W-X (SHEET-WIDTH  SHEET)))
        (W-Y1 (+ W-Y (SHEET-HEIGHT SHEET))))
  "True if a sheet overlaps the given four coordinates.
The specified coordinates are relative to SHEET's superior."
  (NOT (OR (>= LEFT W-X1)
	   (>= W-X  RIGHT)
	   (>= TOP  W-Y1)
	   (>= W-Y  BOTTOM))))

(DEFUN SHEET-OVERLAPS-SHEET-P
       (SHEET-A SHEET-B &AUX X-OFF-A X-OFF-B Y-OFF-A Y-OFF-B)
  "True if two sheets overlap.  They need not have the same superior."
  (COND ((EQ (SHEET-SUPERIOR SHEET-A) (SHEET-SUPERIOR SHEET-B))
	 ;; If superiors are the same, simple comparison
	 (SHEET-OVERLAPS-P SHEET-A
                           (SHEET-X      SHEET-B)
                           (SHEET-Y      SHEET-B)
			   (SHEET-WIDTH  SHEET-B)
                           (SHEET-HEIGHT SHEET-B)))
	(T
	 (MULTIPLE-VALUE-SETQ (X-OFF-A Y-OFF-A)
	   (SHEET-CALCULATE-OFFSETS SHEET-A NIL))
	 (MULTIPLE-VALUE-SETQ (X-OFF-B Y-OFF-B)
	   (SHEET-CALCULATE-OFFSETS SHEET-B NIL))
	 (NOT (OR (>= X-OFF-A (+ X-OFF-B (SHEET-WIDTH  SHEET-B)))
		  (>= X-OFF-B (+ X-OFF-A (SHEET-WIDTH  SHEET-A)))
		  (>= Y-OFF-A (+ Y-OFF-B (SHEET-HEIGHT SHEET-B)))
		  (>= Y-OFF-B (+ Y-OFF-A (SHEET-HEIGHT SHEET-A))))))))

(DEFUN SHEET-WITHIN-P
       (SHEET OUTER-LEFT OUTER-TOP OUTER-WIDTH OUTER-HEIGHT
        &AUX
        (W-X         (SHEET-X      SHEET))
        (W-Y         (SHEET-Y      SHEET))
        (W-X1 (+ W-X (SHEET-WIDTH  SHEET)))
        (W-Y1 (+ W-Y (SHEET-HEIGHT SHEET))))
  "True if the sheet is fully within the specified rectangle.
The specified coordinates are relative to SHEET's superior."
  (AND (<= OUTER-LEFT W-X)
       (<= W-X1 (+ OUTER-LEFT OUTER-WIDTH))
       (<= OUTER-TOP W-Y)
       (<= W-Y1 (+ OUTER-TOP  OUTER-HEIGHT))))

(DEFUN SHEET-BOUNDS-WITHIN-SHEET-P
       (W-X W-Y WIDTH HEIGHT OUTER-SHEET
        &AUX
        (OUTER-LEFT   (SHEET-INSIDE-LEFT   OUTER-SHEET))
        (OUTER-TOP    (SHEET-INSIDE-TOP    OUTER-SHEET))
        (OUTER-WIDTH  (SHEET-INSIDE-WIDTH  OUTER-SHEET))
        (OUTER-HEIGHT (SHEET-INSIDE-HEIGHT OUTER-SHEET)))
  "True if the specified rectangle is fully within the non-margin part
of the sheet.  The specified coordinates are relative to
OUTER-SHEET's superior."
  (AND (<= OUTER-LEFT W-X)
       (<= (+ W-X WIDTH)  (+ OUTER-LEFT OUTER-WIDTH))
       (<= OUTER-TOP W-Y)
       (<= (+ W-Y HEIGHT) (+ OUTER-TOP  OUTER-HEIGHT))))

(DEFUN SHEET-WITHIN-SHEET-P (SHEET OUTER-SHEET)
  "True if SHEET is fully within the non-margin area of OUTER-SHEET."
  (if (mac-window-p sheet)
      (SHEET-WITHIN-P SHEET 0 0 (sheet-width outer-sheet) (sheet-height outer-sheet))
      (SHEET-WITHIN-P SHEET
		      (SHEET-INSIDE-LEFT   OUTER-SHEET)
		      (SHEET-INSIDE-TOP    OUTER-SHEET)
		      (SHEET-INSIDE-WIDTH  OUTER-SHEET)
		      (SHEET-INSIDE-HEIGHT OUTER-SHEET))))

(DEFUN SHEET-CONTAINS-SHEET-POINT-P (SHEET TOP-SHEET X Y)
  "T if (X,Y) lies in SHEET.  X and Y are co-ordinates in TOP-SHEET."
  (DO ((S SHEET (SHEET-SUPERIOR S))
       (X X (- X (SHEET-X S)))
       (Y Y (- Y (SHEET-Y S))))
      ((NULL S))			;Not in the same hierarchy, return nil
    (AND (EQ S TOP-SHEET)
	 (RETURN (AND (>= X 0) (>= Y 0)
		      (< X (SHEET-WIDTH SHEET)) (< Y (SHEET-HEIGHT SHEET)))))))

(DEFUN DESELECT-SHEET-BLINKERS (SHEET)
  "Set visibility of blinkers of SHEET to their DESELECTED values."
  (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
    (AND (EQ (BLINKER-VISIBILITY BLINKER) :BLINK)
	 (SETF (BLINKER-VISIBILITY BLINKER)
	       (BLINKER-DESELECTED-VISIBILITY BLINKER)))))

(DEFUN TURN-OFF-SHEET-BLINKERS (SHEET)
  "Turn visibility of blinkers of SHEET off."
  (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
    (AND (MEMBER (BLINKER-VISIBILITY BLINKER) '(:BLINK :ON) :TEST #'EQ) 
	 (SETF (BLINKER-VISIBILITY BLINKER) :OFF))))

(DEFUN TURN-ON-SHEET-BLINKERS (WINDOW)
  "Turns on the blinkers in the specified window"
  (DOLIST (ONE-BLINKER (SEND WINDOW :SEND-IF-HANDLES :BLINKER-LIST))
    (SEND ONE-BLINKER :SET-VISIBILITY :BLINK))) 

(DEFUN GET-VISIBILITY-OF-ALL-SHEETS-BLINKERS (SHEET)
  "Returns visibility of all of SHEET's blinkers, including inferiors."
  (LET (BLINKER-LIST)
    (WHEN SHEET
      (DOLIST (INFERIOR (SEND SHEET :INFERIORS))
	(DOLIST (BLINKER (SHEET-BLINKER-LIST INFERIOR))
	  (SETQ BLINKER-LIST
		(APPEND BLINKER-LIST (LIST (BLINKER-VISIBILITY BLINKER))))))
      (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET) BLINKER-LIST)
	(SETQ BLINKER-LIST
	      (APPEND BLINKER-LIST (LIST (BLINKER-VISIBILITY BLINKER))))))))

(DEFUN SET-VISIBILITY-OF-ALL-SHEETS-BLINKERS (SHEET BLINKER-LIST-VALUES)
  "Sets visibility of all of sheet's blinkers, including inferiors.
used in conjunction with get-visibility-of-all-sheets-blinkers."
  (WHEN SHEET
    (DOLIST (INFERIOR (SEND SHEET :INFERIORS))
      (DOLIST (BLINKER (SHEET-BLINKER-LIST INFERIOR))
	(SETF (BLINKER-VISIBILITY BLINKER) (CAR BLINKER-LIST-VALUES))
	(POP BLINKER-LIST-VALUES)))
    (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
      (SETF (BLINKER-VISIBILITY BLINKER) (CAR BLINKER-LIST-VALUES))
      (POP BLINKER-LIST-VALUES))))

(DEFUN TURN-OFF-ALL-SHEETS-BLINKERS (SHEET)
  "Turns off all of sheet's blinkers, including inferiors."
  ;; Note that even though we say that we are turning off the blinkers,
  ;; they may not actually be turned off immediatedly.  This is because
  ;; the scheduler actually is the thing that turns them off.  If a
  ;; blinker is currently on, one will have to wait about 1/60th of a
  ;; second before the blinker is really turned off.  This is important
  ;; only for those applications which require tight control of the
  ;; state of the blinkers.  Use OPEN-ALL-SHEETS-BLINKERS to actually
  ;; turn off the blinkers.  That function, however, doesn't keep them
  ;; off permanently.  They are only off until the scheduler sees that
  ;; they are off, and it turns them back on.
  (WHEN SHEET
    (DOLIST (INFERIOR (SEND SHEET :INFERIORS))
      (DOLIST (BLINKER (SHEET-BLINKER-LIST INFERIOR))
	(SETF (BLINKER-VISIBILITY BLINKER) NIL)))
    (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
      (SETF (BLINKER-VISIBILITY BLINKER) NIL))))

;; Only used by MAKE-THE-ENTIRE-SCREEN-BLACK
(DEFUN OPEN-ALL-SHEETS-BLINKERS (SHEET)
  "Does an OPEN-BLINKER on all of sheet's blinkers, including inferiors.
Should be done within a WITHOUT-INTERRUPTS form.  This will insure that
all of the blinkers are turned off when we exit from here."
  ;; Note that this function doesn't actually keep the blinkers off,
  ;; they are only off until the scheduler sees them again.
  (WHEN SHEET
    (DOLIST (INFERIOR (SEND SHEET :INFERIORS))
      (DOLIST (BLINKER (SHEET-BLINKER-LIST INFERIOR))
	(OPEN-BLINKER BLINKER)))
    (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
      (OPEN-BLINKER BLINKER))))

(DEFUN SELECT-SHEET-BLINKERS (SHEET)
  "Set visibility of blinkers of SHEET to their SELECTED values."
  (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
    (AND (MEMBER (BLINKER-VISIBILITY BLINKER) '(:ON :OFF) :TEST #'EQ) 
	 (SETF (BLINKER-VISIBILITY BLINKER) :BLINK))))

(DEFUN SHEET-FOLLOWING-BLINKER (SHEET)
  "Return the blinker which follows SHEET's cursor, if SHEET has one,
else NIL.  If there is more than one, which would be strange, only one
is returned."
  (DOLIST (B (SHEET-BLINKER-LIST SHEET))
    (AND (BLINKER-FOLLOW-P B) (RETURN B))))

(DEFUN SHEET-PREPARE-SHEET-INTERNAL (SHEET &AUX LOCK)
  "This is an internal function for PREPARE-SHEET, and must be called
with INHIBIT-SCHEDULING-FLAG bound."
  (DECLARE (SPECIAL SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD))
  (WHEN SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD
    (UNLESS (SHEET-ME-OR-MY-KID-P SHEET WHO-LINE-SCREEN)
      (PROCESS-WAIT "Screen Lock"
                    #'(LAMBDA () (NOT SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD)))))
  (DO () ((AND (SETQ LOCK (SHEET-CAN-GET-LOCK SHEET))
	       (NOT (SHEET-OUTPUT-HELD-P SHEET))))
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (IF LOCK
	(FUNCALL SHEET :OUTPUT-HOLD-EXCEPTION)
	(PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SHEET))
    (SETQ INHIBIT-SCHEDULING-FLAG T))
  (SHEET-OPEN-ALL-BLINKERS sheet))

(DEFMETHOD (SHEET :EDGES) ()
  (VALUES X-OFFSET Y-OFFSET (+ X-OFFSET WIDTH) (+ Y-OFFSET HEIGHT)))

(DEFMETHOD (SHEET :SIZE) ()
  (VALUES WIDTH HEIGHT))

(DEFMETHOD (SHEET :INSIDE-SIZE) ()
  (VALUES (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)))

(DEFMETHOD (SHEET :INSIDE-WIDTH) ()
  (SHEET-INSIDE-WIDTH))

(DEFMETHOD (SHEET :INSIDE-HEIGHT) ()
  (SHEET-INSIDE-HEIGHT))

(DEFMETHOD (SHEET :SQUARE-PANE-SIZE)
           (MAX-WIDTH MAX-HEIGHT IGNORE IGNORE STACKING)
  (CASE STACKING
    (:VERTICAL MAX-WIDTH)
    (:HORIZONTAL MAX-HEIGHT)))

(DEFMETHOD (SHEET :SQUARE-PANE-INSIDE-SIZE)
           (MAX-WIDTH MAX-HEIGHT IGNORE IGNORE STACKING)
  (CASE STACKING
    (:VERTICAL
     (+ TOP-MARGIN-SIZE
        (- MAX-WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE) BOTTOM-MARGIN-SIZE))
    (:HORIZONTAL
     (+ LEFT-MARGIN-SIZE
        (- MAX-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE) RIGHT-MARGIN-SIZE))))

(DEFMETHOD (SHEET :INSIDE-EDGES) ()
  (VALUES (SHEET-INSIDE-LEFT)
          (SHEET-INSIDE-TOP)
          (SHEET-INSIDE-RIGHT)
	  (SHEET-INSIDE-BOTTOM)))

(DEFMETHOD (SHEET :POSITION) ()
  (VALUES X-OFFSET Y-OFFSET))

(DEFMETHOD (SHEET :MARGINS) ()
  (VALUES LEFT-MARGIN-SIZE
          TOP-MARGIN-SIZE
          RIGHT-MARGIN-SIZE
          BOTTOM-MARGIN-SIZE))
 ;;; Screen management issues 
(DEFMETHOD (SHEET :NAME-FOR-SELECTION) () NIL)

(DEFMETHOD (SHEET :ORDER-INFERIORS) ()
  (WITHOUT-INTERRUPTS
    (SETQ INFERIORS (STABLE-SORT INFERIORS #'SHEET-PRIORITY-LESSP))))

(DEFMETHOD (SHEET :SET-PRIORITY) (NEW-PRIORITY)
  (CHECK-ARG NEW-PRIORITY (OR (NUMBERP NEW-PRIORITY) (NULL NEW-PRIORITY))
	     "a number or NIL" NUMBER-OR-NIL)
  (SETQ PRIORITY NEW-PRIORITY)
  (SCREEN-CONFIGURATION-HAS-CHANGED SELF))

(DEFMETHOD (SHEET :BEFORE :REFRESH) (&OPTIONAL IGNORE)
  (SCREEN-MANAGE-FLUSH-KNOWLEDGE SELF))

(DEFUN SHEET-PRIORITY-LESSP
       (S1 S2 &AUX (EI (SHEET-EXPOSED-INFERIORS (SHEET-SUPERIOR S1)))
        (PRI-S1 (SHEET-PRIORITY S1))
        (PRI-S2 (SHEET-PRIORITY S2))
        (EX1 (MEMBER S1 EI :TEST #'EQ))
        (EX2 (MEMBER S2 EI :TEST #'EQ)))
  (COND ((AND EX1 (NOT EX2))
	 ;; First exposed, second not -- S1 on top.
	 T)
	((AND (NOT EX1) EX2)
	 ;; Second exposed, first not -- S1 underneath.
	 NIL)
	((OR (EQ PRI-S1 PRI-S2)
	     (AND EX1 EX2))
	 ;; Both exposed, or equal priority -- S2 remains on bottom.
	 NIL)
	((AND (NULL PRI-S1) PRI-S2)
	 ;; S2 has explicit priority, and S1 doesn't -- S1 on bottom.
	 NIL)
	((AND PRI-S1 (NULL PRI-S2))
	 ;; S1 has explicit priority, and S2 doesn't -- S1 on top.
	 T)
	(T
	 ;; Both have explicit priority -- S2 on bottom if it's priority
         ;; is less, stable if equal.
	 (<= PRI-S2 PRI-S1))))


(sys:declare-suggestions-for
  'TV:make-window
  :around
  ;;This code should run whether Suggestions is on or not so that windows will not be resized inappropriately
  ;;when Suggestions is turned on.
  '(sys:sugg-around-make-window :do-it)
  :use-arglist t)

(DEFUN MAKE-WINDOW (FLAVOR-NAME &REST OPTIONS &AUX (PLIST (LOCF OPTIONS)))
  "Obsolete function for creating windows.  Now you can simply use
MAKE-INSTANCE."
  (if (mac-system-p)
      (INSTANTIATE-FLAVOR FLAVOR-NAME PLIST T)
      (sys:with-suggestions-menus-for tv:make-window
	(INSTANTIATE-FLAVOR FLAVOR-NAME PLIST T))))

(DEFF WINDOW-CREATE 'MAKE-WINDOW)
(COMPILER:MAKE-OBSOLETE WINDOW-CREATE "it has been renamed to TV:MAKE-WINDOW")

(DEFMETHOD (sheet :inverse-around :init)
	   (cont mt args init-plist)
  ;; Make sure the BIT-ARRAY's type is correct.
  (WHEN (AND bit-array (sheet-superior self))	   ; if there is no bit-array or superior, skip all this
    ;; Revised to avoid dependency on SCREEN-ARRAY of superior screen.
    ;;  With multiple screens, the superior screen may be deexposed and not have a SCREEN-ARRAY.
    ;;  CJJ 09/29/88.
    ;; For Multiple Monitor (MMON) support by KJF for CJJ on 10/03/88.
    (LET ((should-be-element-type (sheet-array-type-cl self)))
      ;; Must use EQUAL to compare element-types, since some are lists.
      (UNLESS (EQUAL should-be-element-type (ARRAY-ELEMENT-TYPE bit-array))
	(SETF bit-array (MAKE-ARRAY
			  (LIST (ARRAY-DIMENSION bit-array 0) (ARRAY-DIMENSION bit-array 1))
			  :element-type should-be-element-type)))))
  ;; Make sure no problem if we PUTPROP the init-plist.
  (SETF (CDR INIT-PLIST) (COPY-LIST (CDR INIT-PLIST)))
  (DELAYING-SCREEN-MANAGEMENT
    (LOCK-SHEET (SELF)
      (AROUND-METHOD-CONTINUE CONT MT ARGS))
    (AND (SHEET-BIT-ARRAY SELF)
	 (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
	   (SEND SELF :REFRESH :COMPLETE-REDISPLAY)))
    (IF (GET INIT-PLIST :ACTIVATE-P)
	(SEND SELF :ACTIVATE))
    (LET ((EXPOSE-P (GET INIT-PLIST :EXPOSE-P)))
      (IF EXPOSE-P
	  (SEND SELF :EXPOSE (UNLESS (EQ EXPOSE-P T) EXPOSE-P))))))

(DEFUN SHEET-ARRAY-TYPE (SHEET)
  "Return the proper array type to use for bit arrays for SHEET.
This depends on the type of screen SHEET is on."
  (CASE (SCREEN-BITS-PER-PIXEL (SHEET-GET-SCREEN SHEET))
    (1 'ART-1B)
    (2 'ART-2B)
    (4 'ART-4B)
    (8 'ART-8B)
    (T 'ART-1B)))


;;; >>> same thing, but returns form used by Common Lisp :element-type rather than for used by :type
(DEFUN SHEET-ARRAY-TYPE-CL (SHEET)
  "Return the proper array type to use for bit arrays for SHEET.
This depends on the type of screen SHEET is on."
  (CASE (SCREEN-BITS-PER-PIXEL (SHEET-GET-SCREEN SHEET))
    (1 'BIT)
    (2 '(MOD 4))
    (4 '(MOD 16))
    (8 '(MOD 256))
    (T 'bit)))


;;;patched on 11 Dec 85 for PDC by GSM
(DEFUN DECODE-CHARACTER-WIDTH-SPEC (SPEC)
  "Decode the value of the :CHARACTER-WIDTH init keyword, when
creating a window.  Returns a number of pixels."
  (DECLARE (:SELF-FLAVOR SHEET))
  (MIN (COND ((NUMBERP SPEC)
	      (+ (* SPEC CHAR-WIDTH) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE))
	     ((STRINGP SPEC)
	      (MULTIPLE-VALUE-BIND (NIL NIL MAX-X)
		  (SHEET-STRING-LENGTH SELF SPEC)
		(+ MAX-X LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))
	     (T
              (FERROR
                NIL
                "~S illegal as :CHARACTER-WIDTH; use NIL, number, or string" spec)))	;PDC 11/22/85
       (IF SUPERIOR (SHEET-INSIDE-WIDTH SUPERIOR) #o1000000)))

(DEFUN DECODE-CHARACTER-HEIGHT-SPEC (SPEC &OPTIONAL WIDTH-ALSO &AUX WID)
  "Decode the value of the :CHARACTER-HEIGHT init keyword, when
creating a window.  Returns a number of pixels."
  (DECLARE (:SELF-FLAVOR SHEET))
  (AND WIDTH-ALSO (STRINGP SPEC)
       (SETQ WID (- (DECODE-CHARACTER-WIDTH-SPEC SPEC)
                    LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))
  (MIN (COND ((NUMBERP SPEC)
	      (+ (* SPEC LINE-HEIGHT)
		 TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
	     ((STRINGP SPEC)
	      (MULTIPLE-VALUE-BIND (IGNORE HT)
		  (SHEET-COMPUTE-MOTION SELF 0 0 SPEC 0 NIL T
                                        0 #o1000000 #o1000000 WID)
		(+ HT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)))
	     (T
              (FERROR
                NIL
                "~S illegal as :CHARACTER-HEIGHT; use NIL, number, or string" spec)))	;PDC 11/22/86
       (IF SUPERIOR (SHEET-INSIDE-HEIGHT SUPERIOR) #o1000000)))

(DEFUN SHEET-DEDUCE-AND-SET-SIZES
       (RIGHT BOTTOM VSP INTEGRAL-P
        &OPTIONAL CHARACTER-WIDTH CHARACTER-HEIGHT)
  "Given the values (possibly NIL) of six init keywords, set SELF's
edges.  If instance variables such as X-OFFSET or WIDTH were
specified, the variables already have their values, and we use those
values."
  (DECLARE (:SELF-FLAVOR SHEET))
   ;;Standardize the font map
   (OR (AND (VARIABLE-BOUNDP FONT-MAP) FONT-MAP)
       (SETQ FONT-MAP (IF (TYPEP SELF 'SCREEN)
			  ;; We have a different font map for screens.
			  *SCREEN-STANDARD-FONT-MAP*
			  ;;ELSE
			  *WINDOW-STANDARD-FONT-MAP*)))
   (SHEET-NEW-FONT-MAP FONT-MAP VSP)

   ;; If height and/or width given in terms of characters in font 0,
   ;; convert to pixels.
   (IF (NOT (NULL CHARACTER-WIDTH))
       (SETQ WIDTH  (DECODE-CHARACTER-WIDTH-SPEC  CHARACTER-WIDTH)))
   (IF (NOT (NULL CHARACTER-HEIGHT))
       (SETQ HEIGHT (DECODE-CHARACTER-HEIGHT-SPEC CHARACTER-HEIGHT)))

   ;; Need to have X-OFFSET, Y-OFFSET, WIDTH, HEIGHT
   (OR X-OFFSET
       (SETQ X-OFFSET (IF (AND RIGHT WIDTH)
                          (- RIGHT WIDTH)
                        ;;ELSE
                        (SHEET-INSIDE-LEFT SUPERIOR))))
   (OR Y-OFFSET
       (SETQ Y-OFFSET (IF (AND BOTTOM HEIGHT)
                          (- BOTTOM HEIGHT)
                        ;;ELSE
                        (SHEET-INSIDE-TOP SUPERIOR))))
   (OR WIDTH
       (SETQ WIDTH  (- (OR RIGHT  (SHEET-INSIDE-RIGHT  SUPERIOR)) X-OFFSET)))
   (OR HEIGHT
       (SETQ HEIGHT (- (OR BOTTOM (SHEET-INSIDE-BOTTOM SUPERIOR)) Y-OFFSET)))

   (AND INTEGRAL-P
	(SETQ BOTTOM-MARGIN-SIZE (- HEIGHT TOP-MARGIN-SIZE
				    (* LINE-HEIGHT
                                       (SHEET-NUMBER-OF-INSIDE-LINES)))))

   (SETQ CURSOR-X (SHEET-INSIDE-LEFT))
   (SETQ CURSOR-Y (SHEET-INSIDE-TOP))

   SELF)

(DEFMETHOD (SHEET :INIT)
           (INIT-PLIST
            &AUX BOTTOM RIGHT SAVE-BITS (VSP 2) (MORE-P T)
            (CHARACTER-WIDTH NIL) (CHARACTER-HEIGHT NIL)
            (REVERSE-VIDEO-P NIL) (INTEGRAL-P NIL)
            (BLINKER-P T) (BLINK-FL 'RECTANGULAR-BLINKER)
            (DESELECTED-VISIBILITY :ON))
  ;; Process options
  (DOPLIST ((CAR INIT-PLIST) VAL OP)
    (CASE OP
      ((:LEFT :X) (SETQ X-OFFSET VAL))
      ((:TOP  :Y) (SETQ Y-OFFSET VAL))
      (:POSITION  (SETQ X-OFFSET (FIRST  VAL)
			Y-OFFSET (SECOND VAL)))
      (:RIGHT     (SETQ RIGHT    VAL))
      (:BOTTOM    (SETQ BOTTOM   VAL))
      (:SIZE  (AND VAL (SETQ WIDTH    (FIRST  VAL)
			     HEIGHT   (SECOND VAL))))
      (:EDGES (AND VAL (SETQ X-OFFSET (FIRST  VAL)
			     Y-OFFSET (SECOND VAL)
			     RIGHT    (THIRD  VAL)
			     BOTTOM   (FOURTH VAL)
			     ;; Override any specified height,
			     ;; probably from default plist.
			     HEIGHT NIL WIDTH NIL))
	      (UNLESS (> RIGHT X-OFFSET)
		(FERROR
		  NIL
		  "Specified edges give width ~S"  (- RIGHT  X-OFFSET)))
	      (UNLESS (> BOTTOM Y-OFFSET)
		(FERROR
		  NIL
		  "Specified edges give height ~S" (- BOTTOM Y-OFFSET))))
      (:CHARACTER-WIDTH               (SETQ CHARACTER-WIDTH       VAL))
      (:CHARACTER-HEIGHT              (SETQ CHARACTER-HEIGHT      VAL))
      (:BLINKER-P                     (SETQ BLINKER-P             VAL))
      (:REVERSE-VIDEO-P               (SETQ REVERSE-VIDEO-P       VAL))
      (:MORE-P                        (SETQ MORE-P                VAL))
      (:VSP                           (SETQ VSP                   VAL))
      (:BLINKER-FLAVOR                (SETQ BLINK-FL              VAL))
      (:BLINKER-DESELECTED-VISIBILITY (SETQ DESELECTED-VISIBILITY VAL))
      (:INTEGRAL-P                    (SETQ INTEGRAL-P            VAL))
      (:SAVE-BITS                     (SETQ SAVE-BITS             VAL))
      (:RIGHT-MARGIN-CHARACTER-FLAG     (SETF (SHEET-RIGHT-MARGIN-CHARACTER-FLAG)     VAL))
      (:BACKSPACE-NOT-OVERPRINTING-FLAG (SETF (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG) VAL))
      (:CR-NOT-NEWLINE-FLAG             (SETF (SHEET-CR-NOT-NEWLINE-FLAG)             VAL))
      (:TRUNCATE-LINE-OUT-FLAG          (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG)          VAL))
      ;; Set keypad-enable to 0 if val is either NIL or is 0.  Otherwise set to 1.
      (:KEYPAD-ENABLE (SETF (SHEET-KEYPAD-ENABLE) (IF (FIXNUMP VAL)
						      (IF (= VAL 0) 0 1)
						      ;;ELSE
						      (IF VAL 1 0))))
      (:TAB-NCHARS                      (SETF (SHEET-TAB-NCHARS)                      VAL))
      (:DEEXPOSED-TYPEIN-ACTION       (SEND SELF :SET-DEEXPOSED-TYPEIN-ACTION VAL))
      ))
  (SHEET-DEDUCE-AND-SET-SIZES
    RIGHT BOTTOM VSP INTEGRAL-P CHARACTER-WIDTH CHARACTER-HEIGHT)
  (COND ((OR (EQ SAVE-BITS 'T) BIT-ARRAY)
	 (LET ((DIMS (LIST (TRUNCATE
                             (* 32.
                                (SETQ LOCATIONS-PER-LINE
                                      (SHEET-LOCATIONS-PER-LINE SUPERIOR)))
                             (SCREEN-BITS-PER-PIXEL (SHEET-GET-SCREEN SELF)))
			   HEIGHT))
	       (ARRAY-TYPE (SHEET-ARRAY-TYPE (OR SUPERIOR SELF))))
	   (SETQ BIT-ARRAY
		 (IF BIT-ARRAY
		     (GROW-BIT-ARRAY BIT-ARRAY (CAR DIMS) (CADR DIMS) WIDTH)
		     ;;ELSE
		     (MAKE-ARRAY `(,(CADR DIMS) ,(CAR DIMS)) :TYPE ARRAY-TYPE)))
           ;; Use a portion of the superior's screen array.
	   (SETQ SCREEN-ARRAY (MAKE-ARRAY `(,(CADR DIMS) ,(CAR DIMS))
					  :TYPE ARRAY-TYPE
					  :DISPLACED-TO BIT-ARRAY
					  :DISPLACED-INDEX-OFFSET 0))))
	((EQ SAVE-BITS :DELAYED)
	 (SETF (SHEET-FORCE-SAVE-BITS) 1)))
  (SETQ MORE-VPOS (AND MORE-P (SHEET-DEDUCE-MORE-VPOS SELF)))
  (COND (SUPERIOR
	 (OR BIT-ARRAY
	     (LET ((ARRAY (SHEET-SUPERIOR-SCREEN-ARRAY)))
	       (SETQ OLD-SCREEN-ARRAY
		     (MAKE-ARRAY
		       `(,HEIGHT ,(ARRAY-DIMENSION ARRAY 1))
		       :TYPE (ARRAY-TYPE ARRAY)
		       :DISPLACED-TO ARRAY
		       :DISPLACED-INDEX-OFFSET
		       (+ X-OFFSET (* Y-OFFSET (ARRAY-DIMENSION ARRAY 1)))))
	       (SETQ LOCATIONS-PER-LINE (SHEET-LOCATIONS-PER-LINE SUPERIOR))))
	 (AND BLINKER-P
	      (APPLY #'MAKE-BLINKER SELF BLINK-FL
		     :FOLLOW-P T
		     :DESELECTED-VISIBILITY DESELECTED-VISIBILITY
		     (AND (CONSP BLINKER-P) BLINKER-P)))))
  (when (mac-screen-p (sheet-get-screen self))
    ;;  Just mark the window as being a deactivated Mac window.  Window id and such
       ;;  will get allocated when it gets activated.  Must also add its bit array to the mX's
       ;;  *undisplaced-Mac-window-arrays* list.
    (SETF window-id t)
    (remember-bit-array self))
  (SETF (SHEET-OUTPUT-HOLD-FLAG) 1)
  
;;;>>> changed char and erase aluf
  (OR (VARIABLE-BOUNDP CHAR-ALUF)
      (if (color-system-p self)
	  (SETQ CHAR-ALUF  ALU-TRANSP)
	  (setq char-aluf  (IF reverse-video-p alu-back alu-transp))
	  ))
  (OR (VARIABLE-BOUNDP ERASE-ALUF)
      (if (color-system-p self)
	  (SETQ ERASE-ALUF ALU-BACK)
	  (setq erase-aluf (IF reverse-video-p alu-transp alu-back))
	  ))
;;; new code added to support color reverse video:
  (SETQ color-reverse-video-state reverse-video-p)
;;; now flip the colors if reverse-video is true. NOTE - check the instance variable, not the AUX variable, since the
;;; instance variable is inittable.
  
  (WHEN (AND color-reverse-video-state (color-system-p self))
    (SEND self :complement-bow-mode)
    )
  
;; Setup the color map based on who and what we are.
  (UNLESS color-map  ;; If one already specified, don't change it.
    (IF (TYPEP self 'screen)  ;; If we're a screen, we have our own copy.
	;; Note: Doing a create-color-map here will give screens a color map with the Window System
	;; version number, not the System version number (as is for *default-color-map*).  See MAP.LISP
	(SETQ color-map (create-color-map)) ;; or (copy-color-map *default-color-map*))
	(IF (NULL superior)  ;; If no superior, which may never be the case??, get a copy from somewhere.
	    (SETQ color-map (copy-color-map (OR (AND default-screen (sheet-color-map default-screen))
						*default-color-map*)))
	    ;; If our superior is a screen, make a copy of its map for us to use.  This is the case for TOP
	    ;; level windows (like the ZMACS frame or the Listener) .
	    (IF (TYPEP (sheet-superior self) 'screen)
		(SETQ color-map (copy-color-map (sheet-color-map superior)))
		;; Otherwise, we always want to get a pointer to our superior's map.
		(SETQ color-map (sheet-color-map superior))))))
;; This is how it was being done for Release 3.2.  The problem was each TOP level window (like the ZMACS
;; frame or the Listener) was getting a pointer to the screen's (mouse-sheet's) color map instead of a COPY
;; of it.  02/21/88 KJF
;;;;; >>> set up to make a color map on the superior screen, and copy it down to everyone else
;;  (UNLESS color-map
;;        (if (null superior)
;;          (SETQ color-map (copy-color-map (OR (AND tv:mouse-sheet (sheet-color-map tv:mouse-sheet))
;;						*default-color-map*)))
;;          ; else
;;          (setq color-map (sheet-color-map superior))
;;        )
;;  )
  (SEND SELF :UPDATE-TIME-STAMP)
  SELF)


(DEFUN on-screen (window)
  "Check to see if the window is actually on the screen"
  (when (sheet-exposed-p window)
                                     ; not exposed so not on screen
      (IF (sheet-superior window)       ; exposed, but must check to see if superior exists and is also exposed
        (on-screen (sheet-superior window))
	t                               ; no superior existed, so if exposed must be on-screen
      )
  )
)

;;; >>> The following few methods were added for multiple screen/dual monitor support.  04/10/88  KJF.

(DEFMETHOD (screen :setup-property-list) (logical-bits-per-pixel display-type sib-number)
  "Setup screen's property-list ivar based on values specified.  This information
indicates how a screen is being used.  That is, if it is being used in a dual monitor
mode or not."
  (LET ((save-properties (COPY-LIST property-list)))
    (IF (GETF property-list :logical-bits-per-pixel)
	(SETF (GETF property-list :logical-bits-per-pixel) logical-bits-per-pixel)
	;; else formS
	(PUSH logical-bits-per-pixel property-list)
	(PUSH :logical-bits-per-pixel property-list))
    (IF (GETF property-list :display-type)
	(SETF (GETF property-list :display-type) display-type)
	;; else formS
	(PUSH display-type property-list)
	(PUSH :display-type property-list))
    (IF (GETF property-list :sib-number)
	(SETF (GETF property-list :sib-number) sib-number)
	;; else formS
	(PUSH sib-number property-list)
	(PUSH :sib-number property-list))
    (UNLESS (check-properties property-list)
      (SETF property-list save-properties)
      (FERROR "Illegal properties.  Property list not changed.")))
  property-list)

(DEFMETHOD (screen :logical-bits-per-pixel) ()
  "Return screen's logical-bits-per-pixel property from property-list ivar."
  (GETF property-list :logical-bits-per-pixel))

(DEFMETHOD (screen :display-type) ()
  "Return screen's display-type property from property-list ivar."
  (GETF property-list :display-type))

(DEFMETHOD (screen :sib-number) ()
  "Return screen's sib-number property from property-list ivar."
  (GETF property-list :sib-number))

(DEFUN check-properties (properties)
  "Check to see if properties are valid, meaning, a legal combination.
Return value of NIL means not valid."
  (LET ((display-type (GETF properties :display-type))
	(logical-bits-per-pixel (GETF properties :logical-bits-per-pixel)))
    (OR (AND (EQ display-type :monochrome)
	     (NUMBERP logical-bits-per-pixel)
	     (= logical-bits-per-pixel 1))
	(AND (EQ display-type :color)
	     (NUMBERP logical-bits-per-pixel)
	     (TYPEP logical-bits-per-pixel '(integer 1 8))))))

(DEFMETHOD (screen :determine-plane-mask-from-property-list) ()
  "Based on properties, return appropriate plane-mask."
  (UNLESS (check-properties property-list)
    (FERROR "Illegal properties for screen.  Either display-type or logical-bits-per-pixel are wrong."))
  (LET* ((display-type (GETF property-list :display-type))
	 (logical-bits-per-pixel (GETF property-list :logical-bits-per-pixel))
	 (new-plane-mask *default-plane-mask*))
    (IF (AND (EQ display-type :color)
	     (EQUAL logical-bits-per-pixel 7.))
	(SETQ new-plane-mask *default-dual-monitor-color-plane-mask*)
	(WHEN (AND (EQ display-type :monochrome)
		   (EQUAL logical-bits-per-pixel 1.))
	  (SETQ new-plane-mask *default-dual-monitor-monochrome-plane-mask*)))
    new-plane-mask))

;; Added 04/23/88 KJF.
;; >> Used to see if a standard-screen's who-line-screen (:screens-who-line-screen) has same
;; properties as the standard screen.  This is used in determining whether who-line-screen
;; should be displayed on same monitor as the standard-screen (default-screen).
(DEFUN compare-properties (screen-1 screen-2)
  "Return t if properties of screens match.  Used to see if a standard-screen's
who-line-screen (:screens-who-line-screen) has same properties as the standard screen.  This
is used in determining whether who-line-screen should be displayed on same monitor as
the standard-screen (default-screen)."
  (AND (EQ (SEND screen-1 :logical-bits-per-pixel)
	   (SEND screen-2 :logical-bits-per-pixel))
       (EQ (SEND screen-1 :display-type)
	   (SEND screen-2 :display-type))))

;; >> Added for dual monitor support.  04/10/88 KJF.
;; Return nil if:
;;  Window's screen is on monochrome monitor.
(DEFUN download-color-map-p (window)
  "If window is on a monochrome monitor, don't need to download its color map."
  (NEQ (SEND (get-screen window) :display-type) :monochrome))

;;; >>> modified 7/7/1987 to download the window's plane mask when the color map is installed KWW
;; >> Changes: Only download when necessary.
;; Also change to LOGAND plane mask.  04/10/88 KJF.
(DEFUN install-color-map (window)
  (WHEN (AND sib-is-csib (download-color-map-p window))  ;; Will this hurt performance??
    (LET ((MAP (sheet-color-map window)))
      (WHEN (AND map (on-screen window))	; only load if window is really on the screen and the map is non-nil
	(download-color-lut-buffer map (current-color-lut-buffer))
	;; LOGAND'ing prevents overridding the screens' plane-mask.
	(SEND window :write-plane-mask (LOGAND (sheet-plane-mask (get-screen window)) (sheet-plane-mask window)))
	(send (aref *blocks* kludge-block) :set-foreground-color-register (color-map-saturate map))
	(send (aref *blocks* kludge-block) :set-background-color-register (color-map-clamp map))))))

(DEFMETHOD (sheet :after :expose) (&rest ignore)
    (install-color-map self) ; if window is already exposed but just now selected, we load it's color map
)

;;; >>> updated to initialize based on color or mono system
(DEFMETHOD (SCREEN :BEFORE :INIT) (IGNORE)
  ;; Get rid of using tv:*color-system*.  bits-per-pixel should be specified when asking
  ;; to create a screen.  If it's a color screen they want, they should specifiy 8 as an
  ;; init option.  That's what :initable-instance-variables are for.  Otherwise, default will
  ;; be 1.  See SCREEN defflavor in WINDOW;SHEET.LISP.  This was done to make code for
  ;; multiple-screen support cleaner.  04/06/88 KJF.
;;  (SETQ bits-per-pixel (IF tv:*color-system* 8. 1.))
  ;; Make sure bits-per-pixel is something we can really use.  04/06/88 KJF.
  (ASSERT (AND (NUMBERP bits-per-pixel)
	       (OR (EQL bits-per-pixel 1) (EQL bits-per-pixel 8)))
	  (bits-per-pixel)
	  "bits-per-pixel must be 1 or 8")
  ;;  Calculate LOCATIONS-PER-LINE based on caller-specified screen width always!!!   LG  1/12/88
  (SETQ LOCATIONS-PER-LINE (TRUNCATE (* WIDTH BITS-PER-PIXEL) 32.))
;;  (OR (VARIABLE-BOUNDP LOCATIONS-PER-LINE)
;;      (SETQ LOCATIONS-PER-LINE (TRUNCATE (* WIDTH BITS-PER-PIXEL) 32.)))
  (SETQ FONT-ALIST NIL)   ;Overridden by the :AFTER method.
			  ;Makes :PARSE-FONT-DESCRIPTOR not lose.
  ;; These two must get values here, but they will be replaced with the
  ;; right ones later.
  (SETQ DEFAULT-FONT *DEFAULT-FONT*
	FONT-MAP (FILL (MAKE-ARRAY 26. :LEADER-LENGTH 1) DEFAULT-FONT)
	;; No one uses this anyway...
	BUFFER-HALFWORD-ARRAY (MAKE-ARRAY (TRUNCATE
                                            (* WIDTH
                                               (OR HEIGHT 1)
                                               BITS-PER-PIXEL)
                                            16.)
					  :TYPE ART-16B :DISPLACED-TO BUFFER))
  (OR BIT-ARRAY
      (SETQ OLD-SCREEN-ARRAY (MAKE-ARRAY `(,(OR HEIGHT 1) ,WIDTH)
					       :TYPE (SHEET-ARRAY-TYPE SELF)
					       :DISPLACED-TO BUFFER))))

(DEFMETHOD (SCREEN :AFTER :INIT) (IGNORE)
  (SETQ FONT-ALIST
	(LOOP for (purpose . font) in  DEFAULT-FONT-ALIST
	      for screen-local-font = (SEND self :parse-font-descriptor font)
	      collect (CONS purpose screen-local-font)))
  (DOLIST (ELT FONT-ALIST)
    (LET ((SYMBOL (INTERN (FORMAT NIL "~A-~O-~A-FONT"
				  (TYPE-OF SELF) (%POINTER SELF) (CAR ELT))
			  "TV")))
      (SET SYMBOL (CDR ELT))
      (SETF (CDR ELT) SYMBOL)))
  (SETQ DEFAULT-FONT (SEND SELF :PARSE-FONT-SPECIFIER
                           (CAR *SCREEN-STANDARD-FONT-MAP*)))
  (SEND SELF :SET-FONT-MAP *SCREEN-STANDARD-FONT-MAP*))

(DEFMETHOD (SCREEN :BEFORE :EXPOSE) (&REST IGNORE)
  (COND ((NOT EXPOSED-P)
	 (SETQ BUFFER-HALFWORD-ARRAY (MAKE-ARRAY
                                       (TRUNCATE
                                         (* WIDTH
                                            (OR HEIGHT 1)
                                            BITS-PER-PIXEL)
                                         16.)
                                       :TYPE ART-16B :DISPLACED-TO BUFFER))
	 (if (mac-screen-p self)
	     ;; Handle the old-screen-array of the Macintosh Screen...
	     (ADJUST-ARRAY old-screen-array
			   `(,height ,width)
;			   :element-type (ARRAY-ELEMENT-TYPE old-screen-array)
			   :displaced-to buffer
			   :displaced-index-offset (+ (* y-offset width) x-offset))
	     (ADJUST-ARRAY OLD-SCREEN-ARRAY
			   (LIST HEIGHT WIDTH)
			   :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE OLD-SCREEN-ARRAY)
			   :DISPLACED-TO (+ BUFFER (TRUNCATE (* Y-OFFSET WIDTH) (COND ( (= bits-per-pixel 1) 32)
										      ( (= bits-per-pixel 8) 4)))))
	     (WHEN (mmon-p)
	       ;; Deexpose any exposed overlapping screens, so this screen will have a clean spot to put itself...
	       ;; Added for multiple-monitor support.  CJJ 06/03/88.
	       ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
	       (LOOP FOR screen IN (SEND self :overlapping-exposed-screens)
		     DOING
		     ;; Also erase the spot left by the screen being deexposed...
		     (SEND screen :deexpose :default :clean)))))))

(DEFMETHOD (SCREEN :SELECTABLE-WINDOWS) ()
  (MAPCAN #'(LAMBDA (I) (FUNCALL I :SELECTABLE-WINDOWS)) INFERIORS))

(DEFMETHOD (SHEET :IDLE-LISP-LISTENER) () 
  (IF SUPERIOR
      (FUNCALL SUPERIOR :IDLE-LISP-LISTENER)
      (IDLE-LISP-LISTENER SELF)))

(DEFMETHOD (SHEET :ALIAS-FOR-SELECTED-WINDOWS) ()
  SELF)

(DEFMETHOD (SHEET :ALIAS-FOR-INFERIORS) () NIL)

;;; Font stuff.


(DEFMETHOD (SCREEN :PARSE-FONT-SPECIFIER) (FD)
  (SCREEN-PARSE-FONT-DESCRIPTOR FD 'FONTS:CPT-FONT))

(DEFMETHOD (SCREEN :PARSE-FONT-DESCRIPTOR) (FD)
  (SCREEN-PARSE-FONT-DESCRIPTOR FD 'FONTS:CPT-FONT))

(DEFUN SCREEN-PARSE-FONT-DESCRIPTOR (FD TYPE &OPTIONAL DONT-LOAD-P)
  (DECLARE (:SELF-FLAVOR SCREEN))
  (AND (FONT-OBJECT-P FD) (BOUNDP (FONT-NAME FD))
       (SETQ FD (FONT-NAME FD)))
  (COND ((SYMBOLP FD)
	 ;; Name of font -- find appropriate font.
	 (PROG ((FONT (GET FD TYPE)))
	   ;; First try a property of the symbol.
	   (COND ((NULL FONT))
		 ((SYMBOLP FONT)
		   (RETURN (SCREEN-PARSE-FONT-DESCRIPTOR
                             FONT TYPE DONT-LOAD-P)))
		 ((font-object-p FONT) (RETURN FONT)))
	   ;; Then see if it is a known "purpose" of a standard font.
	   (IF (ASSOC FD FONT-ALIST :TEST #'EQ)
	       (RETURN (FONT-EVALUATE
                         (CDR (ASSOC FD FONT-ALIST :TEST #'EQ)))
                       (CDR (ASSOC FD FONT-ALIST :TEST #'EQ))))
	   ;; Then try its value.
	   (IF (BOUNDP FD) (SETQ FONT (SYMBOL-VALUE FD)))
	   (COND ((NULL FONT))
		 ((EQ FONT FD))  ;In case arg is in KEYWORD.
		 ((SYMBOLP FONT)
		   (RETURN (SCREEN-PARSE-FONT-DESCRIPTOR
                             FONT TYPE DONT-LOAD-P)
                           FD))
		 ((font-object-p FONT) (RETURN FONT FD)))
	   ;; Then try re-interning in FONTS if not already there.
	   (SETQ FONT (FIND-SYMBOL FD "FONTS"))
	   (AND FONT (NEQ FONT FD)
		(RETURN (SCREEN-PARSE-FONT-DESCRIPTOR FONT TYPE DONT-LOAD-P)))
	   ;; Then maybe try loading a file.
	   (OR DONT-LOAD-P
	       (PROGN
		 ;; Specifying FONTS package is to inhibit loading message.
		 (multiple-value-bind (ignore had-an-error)
		     (CATCH-ERROR
                       (LOAD (FORMAT NIL "SYS: FONTS; ~A" FD) "FONTS" T T) NIL)
		   (unless had-an-error
		     ;; See if we got a value for the symbol from the file.
		     (IF (BOUNDP FD) (SETQ FONT (SYMBOL-VALUE FD)))
		     (COND ((NULL FONT))
			   ((EQ FONT FD))
			   ((SYMBOLP FONT)
			    (RETURN (SCREEN-PARSE-FONT-DESCRIPTOR
                                      FONT TYPE DONT-LOAD-P)))
			   ((font-object-p FONT) (RETURN FONT FD)))))))
	   ;; If Mac system make up a font..
	   (WHEN (AND (mac-screen-p self)
		      (IF (SYMBOLP FD) (SETF FONT FD)))
	     (LET* ((new-font-name (STRING-UPCASE (SYMBOL-NAME FONT)))
		    (new-font-symbol (INTERN new-font-name 'fonts))
		    (new-font (tv:make-font)))
	       (SET new-font-symbol new-font)
	       (COPY-ARRAY-CONTENTS-AND-LEADER fonts:cptfont new-font)
	       (SETF (w:font-name new-font) new-font-symbol)
	       (RETURN new-font new-font-symbol)))
	   ;; Ask the user to specify some other font descriptor.
	   (RETURN (SCREEN-PARSE-FONT-DESCRIPTOR
		     (CERROR T NIL NIL "Font ~D not found" FD)
		     TYPE))))
	((font-object-p FD) FD)
	(T (FERROR NIL "Illegal font descriptor ~A" FD))))

(DEFMETHOD (SCREEN :PARSE-FONT-NAME) (FD)
  (MULTIPLE-VALUE-BIND (FONT -NAME-)
      (SEND SELF :PARSE-FONT-SPECIFIER FD)
    (OR -NAME- 
	(IF (AND (SYMBOLP FD)
	     (EQ (FONT-EVALUATE FD) FONT))
	    FD (FONT-NAME FONT)))))

(DEFUN FONT-EVALUATE (FONTNAME &OPTIONAL (SCREEN DEFAULT-SCREEN))
  "Evaluate FONTNAME repeatedly until the result is not a symbol or is
an unbound symbol or is NIL. SCREEN is needed only in the case where
FONTNAME is a font purpose keyword. In this case, SCREEN is
either a sheet or screen object, which is used to determine the
screen for which the font purpose is evaluated."
  (WHEN screen  ;; default-screen is nil when building Window System.  05/27/88 KJF.
    (SETQ SCREEN (SHEET-GET-SCREEN SCREEN))
    (WHEN (KEYWORDP fontname)
      ;; Find font name belonging to font purpose
      (CHECK-TYPE SCREEN SCREEN "a SHEET or a SCREEN object.")
      (SETQ FONTNAME (SEND SCREEN :FONT-NAME-FOR FONTNAME)))
    
    ;; Evaluate font name symbol
    (send screen :parse-font-descriptor
	  (DO ((F FONTNAME (SYMBOL-VALUE F)))
	      ((OR (NULL F) (NOT (SYMBOLP F)) (NOT (BOUNDP F)))
	       F)))))

(DEFUN GET-DEFAULT-FONT (WINDOW)
  "Return the default font for a window."
  (AREF (SEND WINDOW :FONT-MAP) 0))

(DEFMETHOD (SCREEN :DEFAULT-FONT-NAME) ()
  (SEND SELF :FONT-NAME-FOR :DEFAULT))

(DEFMETHOD (SCREEN :FONT-NAME-FOR) (PURPOSE &OPTIONAL OVERALL-DEFAULT)
  (OR (CDR (ASSOC PURPOSE FONT-ALIST :TEST #'EQ))
      OVERALL-DEFAULT
      (CDR (ASSOC PURPOSE DEFAULT-FONT-ALIST :TEST #'EQ))))

(DEFSUBST FONT-MAP-CURRENT-FONT-NAME (FONT-MAP)
  "Return the number specified in FONT-MAP for the current font.
This is actually the array index into the font map."
  (ARRAY-LEADER FONT-MAP 2))

(DEFSTRUCT (FONT-MAP (:ALTERANT NIL) (:MAKE-ARRAY (:LENGTH 26.)) (:CONC-NAME FONT-MAP-)
  (:CALLABLE-CONSTRUCTORS NIL) (:PREDICATE NIL) (:COPIER NIL) (:TYPE :ARRAY-LEADER))
  (FILL-POINTER 26.)
  (FONT-LIST NIL :DOCUMENTATION
	     "List of fonts or font names from this font-map was constructed.")
  (CURRENT-FONT 0 :DOCUMENTATION
		"What was supplied to the last call to :SET-CURRENT-FONT."))

(DEFUN SHEET-NEW-FONT-MAP (NEW-fonts VSP &AUX (SCREEN (SHEET-GET-SCREEN SELF)) (new-map new-fonts))
  "Set SELF's FONT-MAP and VSP according to NEW-MAP and VSP.
NEW-MAP is either a legitimate font-map array, or a list or ordinary
array of fonts or font descriptors.  VSP is a number of pixels."
  (DECLARE (:SELF-FLAVOR SHEET))
  ;; Reduce all Mac-resident windows' VSPs by 2.   2/6/88 LG
  (WHEN (mac-screen-p screen)			; Can't use (mac-window-p self) because :init
    (SETF vsp (MAX 0 (- vsp 2))))		;    calls us too early for that too work.
  (UNLESS (AND (ARRAYP NEW-FONTS)
	       (>= (ARRAY-TOTAL-SIZE NEW-FONTS) 26.)
	       (EQ (ARRAY-LEADER-LENGTH NEW-FONTS) 3))
    (WHEN (ARRAYP NEW-FONTS) ;; this should never happen
      (SETQ NEW-FONTS (COERCE NEW-FONTS 'LIST)))
    (LET ((LENGTH (MAX (LENGTH NEW-FONTS) 26.))
	  font)
      (SETQ new-map (MAKE-FONT-MAP :FONT-LIST NEW-FONTS
				   :FILL-POINTER LENGTH
				   :MAKE-ARRAY (:LENGTH LENGTH)))
      (DOTIMES (X length)
	(WHEN new-fonts
	  (SETQ font (SEND SCREEN :PARSE-FONT-SPECIFIER (POP new-fonts))))
	(SETF (AREF new-map X) font))))
  (WITHOUT-INTERRUPTS
    (SETQ FONT-MAP NEW-MAP)
    ;; Now, find out the character dimensions of this set of fonts.
    (LET ((FONT (AREF NEW-MAP 0)))
      (SETQ CURRENT-FONT FONT)
      (SETF (FONT-MAP-CURRENT-FONT FONT-MAP) 0)
      (SETQ CHAR-WIDTH (FONT-CHAR-WIDTH FONT))
      (LET ((BL (SHEET-FOLLOWING-BLINKER SELF)))
	(IF BL
            (FUNCALL BL :SET-SIZE
                     (FONT-CHAR-WIDTH  FONT)
                     (FONT-CHAR-HEIGHT FONT)))))
    (DO* ((I 0 (1+ I))
	  (LENGTH (ARRAY-ACTIVE-LENGTH NEW-MAP))
;	 ;;(MAXWIDTH 0)
	  (MAXHEIGHT 0)
	  (MAXBASE 0)
	  (previous-font nil font)
	  (FONT))
	((>= I LENGTH)
	 (SETQ BASELINE MAXBASE
	       LINE-HEIGHT (+ VSP MAXHEIGHT)))
      (SETQ FONT (AREF NEW-MAP I))
      (WHEN (NOT (EQ font previous-font))
	(SETQ MAXHEIGHT (MAX MAXHEIGHT (FONT-CHAR-HEIGHT FONT))
	      MAXBASE   (MAX MAXBASE   (FONT-BASELINE    FONT)))
;     (LET ((CWT (FONT-CHAR-WIDTH-TABLE FONT)))
;       (IF CWT
;	   (DOTIMES (J #o200)
;	       (SETQ MAXWIDTH (MAX MAXWIDTH (AREF TEM J))))
;	   (SETQ MAXWIDTH (MAX MAXWIDTH (FONT-CHAR-WIDTH (AREF NEW-MAP I))))))
	))
    (SETQ BASELINE-ADJ (- BASELINE (FONT-BASELINE CURRENT-FONT)))))

;;;ab 5.25.88  This is LG's version that should be made eventually.
;;;(DEFUN SHEET-NEW-FONT-MAP (NEW-fonts VSP
;;;			   &AUX (SCREEN (SHEET-GET-SCREEN SELF)) (new-map new-fonts))
;;;  "Set SELF's FONT-MAP and VSP according to NEW-MAP and VSP.
;;;NEW-MAP is either a legitimate font-map array, or a list or ordinary
;;;array of fonts or font descriptors.  VSP is a number of pixels."
;;;  (DECLARE (:SELF-FLAVOR SHEET))
;;;  ;; Reduce all Mac-resident windows' VSPs by 2.   2/6/88 LG
;;;  (WHEN (mac-screen-p screen)			; Can't use (mac-window-p self) because :init
;;;    (SETF vsp (MAX 0 (- vsp 2))))		;    calls us too early for that too work.
;;;  (UNLESS (AND (ARRAYP NEW-FONTS)
;;;	       (>= (ARRAY-TOTAL-SIZE NEW-FONTS) 26.)
;;;	       (EQ (ARRAY-LEADER-LENGTH NEW-FONTS) 3))
;;;    (WHEN (ARRAYP NEW-FONTS) ;; this should never happen
;;;      (SETQ NEW-FONTS (COERCE NEW-FONTS 'LIST)))
;;;    (LET ((LENGTH (MAX (LENGTH NEW-FONTS) 26.))
;;;	  font)
;;;      (SETQ new-map (MAKE-FONT-MAP :FONT-LIST NEW-FONTS
;;;				   :FILL-POINTER LENGTH
;;;				   :MAKE-ARRAY (:LENGTH LENGTH)))
;;;      (DOTIMES (X length)
;;;	(WHEN new-fonts
;;;	  (SETQ font (SEND SCREEN :PARSE-FONT-SPECIFIER (POP new-fonts))))
;;;	(SETF (AREF new-map X) font))))
;;;  (WITHOUT-INTERRUPTS
;;;    (SETQ FONT-MAP NEW-MAP)
;;;    ;; Now, find out the character dimensions of this set of fonts.
;;;    (LET ((FONT (AREF NEW-MAP 0)))
;;;      (SETQ CURRENT-FONT FONT)
;;;      (SETF (FONT-MAP-CURRENT-FONT FONT-MAP) 0)
;;;      (SETQ CHAR-WIDTH (FONT-CHAR-WIDTH FONT))
;;;      (LET ((BL (SHEET-FOLLOWING-BLINKER SELF)))
;;;	(IF BL
;;;            (FUNCALL BL :SET-SIZE
;;;                     (FONT-CHAR-WIDTH  FONT)
;;;                     (FONT-CHAR-HEIGHT FONT)))))
;;;    (DO* ((I 0 (1+ I))
;;;	  (LENGTH (ARRAY-ACTIVE-LENGTH NEW-MAP))
;;;;	 ;;(MAXWIDTH 0)
;;;	  (maxdescender 0)
;;;	  (MAXBASE 0)
;;;	  (previous-font nil font)
;;;	  (FONT))
;;;	((>= I LENGTH)
;;;	 (SETQ BASELINE MAXBASE
;;;	       LINE-HEIGHT (+ VSP maxbase maxdescender)))
;;;      (SETQ FONT (AREF NEW-MAP I))
;;;      (WHEN (NOT (EQ font previous-font))
;;;	(SETQ maxdescender (MAX maxdescender (- (FONT-CHAR-HEIGHT FONT)
;;;						(font-baseline font)))
;;;	      MAXBASE   (MAX MAXBASE   (FONT-BASELINE    FONT)))
;;;;     (LET ((CWT (FONT-CHAR-WIDTH-TABLE FONT)))
;;;;       (IF CWT
;;;;	   (DOTIMES (J #o200)
;;;;	       (SETQ MAXWIDTH (MAX MAXWIDTH (AREF TEM J))))
;;;;	   (SETQ MAXWIDTH (MAX MAXWIDTH (FONT-CHAR-WIDTH (AREF NEW-MAP I))))))
;;;	))
;;;    (SETQ BASELINE-ADJ (- BASELINE (FONT-BASELINE CURRENT-FONT)))))

(DEFMETHOD (SCREEN :BEFORE :CHANGE-OF-DEFAULT-FONT) (OLD-FONT NEW-FONT)
  (IF (EQ DEFAULT-FONT OLD-FONT)
      (PROGN (SETQ DEFAULT-FONT NEW-FONT)
	     (SET (SEND SELF :DEFAULT-FONT-NAME)
		  (FONT-NAME NEW-FONT)))))

(DEFMETHOD (SCREEN :AFTER :CHANGE-OF-DEFAULT-FONT) (OLD-FONT NEW-FONT)
  (DECLARE (SPECIAL OLD-FONT NEW-FONT))
  (DOLIST (RESOURCE-NAME WINDOW-RESOURCE-NAMES)
    (SI:MAP-RESOURCE
      (FUNCTION
        (LAMBDA (WINDOW IGNORE IGNORE)
          (AND (EQ (FUNCALL WINDOW :STATUS) :DEACTIVATED)
               (EQ SELF (SHEET-GET-SCREEN WINDOW))
               (FUNCALL WINDOW :CHANGE-OF-DEFAULT-FONT OLD-FONT NEW-FONT))))
      RESOURCE-NAME)))

(DEFMETHOD (SHEET :CHANGE-OF-DEFAULT-FONT) (OLD-FONT NEW-FONT &AUX CURRENT)
  (OR OLD-FONT (SETQ OLD-FONT CURRENT-FONT))

   ;; may 9-7-88. This search is more efficient. Also returns FIRST index of font not LAST.
   (SETQ current (SEARCH current-font font-map :test #'eq))	;; may 9-7-88
;;  (DOTIMES (I (ARRAY-TOTAL-SIZE FONT-MAP))  			;; may 9-7-88
;;    (IF (EQ (AREF FONT-MAP I) CURRENT-FONT)  			;; may 9-7-88
;;      (SETQ CURRENT I)))		    			;; may 9-7-88

  (SEND SELF :SET-FONT-MAP (FONT-MAP-FONT-LIST FONT-MAP));; recreate font-map from original creation list
  (IF CURRENT
      (SEND SELF :SET-CURRENT-FONT CURRENT)) 	;; if current-font was set to font in map - restore it.
  (SEND SELF :UPDATE-TIME-STAMP)
  (DOLIST (I INFERIORS)				;; do inferiors, too
    (FUNCALL I :CHANGE-OF-DEFAULT-FONT OLD-FONT NEW-FONT)))

(DEFMETHOD (SHEET :MORE-P) ()
  (NOT (NULL MORE-VPOS)))

(DEFMETHOD (SHEET :SET-MORE-P) (MORE-P)
  (SETQ MORE-VPOS (AND MORE-P (SHEET-DEDUCE-MORE-VPOS SELF))))

(DEFUN SHEET-DEDUCE-MORE-VPOS (SHEET &AUX (LH (SHEET-LINE-HEIGHT SHEET)))
  "Return the vpos of the line just above the bottom margin."
  (+ (SHEET-TOP-MARGIN-SIZE SHEET)
     (1- (* (1- (FLOOR (SHEET-INSIDE-HEIGHT SHEET) LH)) LH))))

(DEFMETHOD (SHEET :VSP) ()
  (- LINE-HEIGHT
     (LET ((H 0))
       (DOTIMES (I (ARRAY-TOTAL-SIZE FONT-MAP))
	 (SETQ H (MAX H (FONT-CHAR-HEIGHT (AREF FONT-MAP I)))))
       H)))

(DEFMETHOD (SHEET :SET-VSP) (NEW-VSP)
  (SHEET-NEW-FONT-MAP FONT-MAP NEW-VSP)
  NEW-VSP)

(DEFMETHOD (SHEET :SET-FONT-MAP) (NEW-MAP)
  (OR NEW-MAP
      (SETQ NEW-MAP *WINDOW-STANDARD-FONT-MAP*))
  (SHEET-NEW-FONT-MAP NEW-MAP (SEND SELF :VSP))
  FONT-MAP)

(DEFMETHOD (SHEET :SET-CURRENT-FONT)
           (NEW-FONT &OPTIONAL OK-IF-NOT-IN-FONT-MAP &aux font tem)
  (COND ((NUMBERP new-font)
	 (CHECK-ARG new-font
		    (< -1 new-font (LENGTH font-map))
		    "is not a valid index into the font map of this window")
	 (SETQ font (AREF font-map new-font)))
	(t
	 (SETQ font (SEND (sheet-get-screen self)
			  :parse-font-specifier new-font)
	       tem (POSITION font font-map :TEST #'(LAMBDA (FONT-A FONT-B) (EQ (FONT-NAME FONT-A) (FONT-NAME FONT-B)))))
	 (UNLESS (OR tem ok-if-not-in-font-map)
	   (FERROR nil "~a is not in the font map of ~s." font self))))
  (WITHOUT-INTERRUPTS
    (SETF (FONT-MAP-CURRENT-FONT FONT-MAP)
	  (COND ((NUMBERP NEW-FONT) NEW-FONT)
		(T
                 (IF (SYMBOLP NEW-FONT) NEW-FONT (OR TEM FONT)))))
    (LET ((BL (SHEET-FOLLOWING-BLINKER SELF)))
      (IF BL
          (FUNCALL BL :SET-SIZE
                   (FONT-CHAR-WIDTH  FONT)
                   (FONT-CHAR-HEIGHT FONT))))
    (SETQ CURRENT-FONT FONT
	  BASELINE-ADJ (- BASELINE (FONT-BASELINE FONT))
	  CHAR-WIDTH (FONT-CHAR-WIDTH FONT))))

;;; Compute the smallest raster width needed to store the specified char
;;; as defined by the specified font.  Low-level means we are looking at
;;; one sub-character in a wide font.
(DEFUN FONT-CHAR-MIN-RASTER-WIDTH (FONT CHAR-CODE &OPTIONAL LOW-LEVEL
				   &AUX BIT-POS WORD-POS TEM MIN-RASTER-WIDTH F-RASTER-WIDTH
				   RASTER-HEIGHT)
  
  (IF (GET (font-name font) :mac-fd)
      (AREF (font-char-width-table font) char-code)
    ;; else...
    (IF (AND (NOT LOW-LEVEL) (SETQ TEM (FONT-INDEXING-TABLE FONT)))
	(PROGN
	  ;; If it's a wide font, go by the number of vertical stripes,
	  ;; but also see how wide the rightmost stripe really needs to be.
	  (LET ((START-IDX (AREF TEM     CHAR-CODE))
		(END-IDX   (AREF TEM (1+ CHAR-CODE))))
	    (IF (= START-IDX END-IDX)
		0
	      (MAX 0
		   (+ (* 32. (- END-IDX START-IDX 1))
		      (FONT-CHAR-MIN-RASTER-WIDTH FONT (1- END-IDX) T))))))
      ;;ELSE
      (PROGN
	(SETQ WORD-POS         (* CHAR-CODE (FONT-WORDS-PER-CHAR FONT))
	      BIT-POS          0
	      MIN-RASTER-WIDTH 0
	      F-RASTER-WIDTH   (FONT-RASTER-WIDTH  FONT)
	      RASTER-HEIGHT    (FONT-RASTER-HEIGHT FONT))
	(DOTIMES (VPOS RASTER-HEIGHT)
	  (WHEN (> (+ BIT-POS F-RASTER-WIDTH) 32.)
	    (SETQ BIT-POS 0
		  WORD-POS (1+ WORD-POS)))
	  (DO ((HPOS 0 (1+ HPOS))
	       (INDEX (+ BIT-POS (LSH WORD-POS 5)) (1+ INDEX)))
	      ((= HPOS F-RASTER-WIDTH))
	    (WHEN (NOT (ZEROP (AREF FONT INDEX)))
	      (SETQ MIN-RASTER-WIDTH (MAX (1+ HPOS) MIN-RASTER-WIDTH))))
	  (SETQ BIT-POS (+ F-RASTER-WIDTH BIT-POS)))
	MIN-RASTER-WIDTH))))

;; may 04/04/89 ADDED new macro ( in here because most of the font stuff is here, too)
;; This must replace the entire map and the current-font - together they make up ALL the font info
;; that is common to ALL windows. Needed initially by 'si:edit-disk-label.
(DEFMACRO WITH-FONT-MAP ((font-map-or-list window) &BODY BODY)
  "Execute BODY with the WINDOW's font map temporarily replaced by FONT-MAP-OR-LIST.
The first font in FONT-MAP-OR-LIST will be the current font prior to entering the BODY,"
  `(LET* ((OLD-FONT-MAP (SEND ,window :FONT-MAP))
	  (OLD-CURRENT-FONT (SEND ,window :CURRENT-FONT)))
     (UNWIND-PROTECT
	 (PROGN
	   ;; :set-font-map sets the current font as the *FIRST* font in font-map-or-list.
	   ;; Although the current-font could be non-zero in FONT-MAP-OR-LIST - this is unlikely
	   ;; and should be handled in BODY if desired.
	   (SEND ,window :SET-FONT-MAP ,font-map-or-list)
	   ,@BODY)
       (SEND ,window :SET-FONT-MAP OLD-FONT-MAP)
       ;; Do this because the :current-font was NOT necessarily the 0'th font in old font map array 
       (SEND ,window :SET-CURRENT-FONT OLD-CURRENT-FONT T))))

;; may 04/04/89 ADDED new function.
(DEFUN show-font-map (&optional (window *terminal-io*))
  "Show contents and other info for the font-map of sheet or screen WINDOW."
  (LET* ((font-map (SEND window :font-map))
	 (current-font-descriptor (FONT-MAP-CURRENT-FONT-NAME font-map))
	 (current-font-name (IF (NUMBERP current-font-descriptor)
				(aref font-map (FONT-MAP-CURRENT-FONT-NAME font-map))
				(font-evaluate current-font-descriptor))))
    (FORMAT t "~% Fill pointer       = ~2d." (array-leader font-map 0) )
    (FORMAT t "~% Font-map-font-list = ~s"   (array-leader font-map 1) )
    ;; describe current purpose fonts - if any
    (DOLIST (purpose (ARRAY-LEADER font-map 1))
      (WHEN (KEYWORDP purpose) (FORMAT t "~%~8@t Purpose ~s is currently ~s for w:default-screen"
				       Purpose
				       (tv:get-standard-font Purpose w:default-screen))))
    ;; Current-font is usually an index into font map. *ONLY*? is a symbol if
    ;; :set-current-font is passed in a T to OK-IF-NOT-IN-FONT-MAP arg. I don't think
    ;; this ever happens ?
    (IF (NUMBERP current-font-descriptor)
	(FORMAT t "~% Current-font :  Index  = ~2d.  Font = ~s" current-font-descriptor current-font-name)
	(FORMAT t "~% Current-font :  Font not in map = ~s" current-font-name))
    (dotimes (i (array-leader font-map 0)) (FORMAT t "~%~2d : ~s" i (zwei:current-font window i)))))

(DEFUN DISPLAY-FONT (FONT &KEY (WINDOW SELECTED-WINDOW) COLUMNS (LABEL-BASE 8.)
		               (LABEL-FONT FONTS:CPTFONT) (SAMPLE-FONT FONTS:CPTFONT)
                     (HEADER-FONT FONTS:HL12B)
		     MOUSE-SENSITIVE-ITEM-TYPE RESET-CURSOR-P)
  "Displays a table showing FONT compared to SAMPLE-FONT on WINDOW.
FONT - the font to be displayed.
WINDOW - the window to put the display on.
COLUMNS - number of columns to use for the display. If NIL, default to largest power of 2 that fits.
          a negative or zero value will use as many columns as will fit.
LABEL-BASE - Base to use for printing the label numbers.
LABEL-FONT - The font to print the label number in.
SAMPLE-FONT - The font to print first as a comparison to FONT.
HEADER-FONT - The font to use for the heading.
MOUSE-SENSITIVE-ITEM-TYPE - When non-nil, makes each character mouse sensitive with this item type.
RESET-CURSOR-P - IF non-nil, puts the cursor at 0 0 instead of at the end of the displayed table."
  (check-type font font "a font object")
  (when (mac-window-p window)
    (SETF label-font (SEND (sheet-get-screen window) :parse-font-descriptor label-font)
	sample-font (SEND (sheet-get-screen window) :parse-font-descriptor sample-font)
	header-font (SEND (sheet-get-screen window) :parse-font-descriptor header-font)
	font (SEND (sheet-get-screen window) :parse-font-descriptor font)))
  (LET* ((FONT-SIZE (IF (and (font-fill-pointer font) (NOT (ZEROP (FONT-FILL-POINTER FONT))))
                        (FONT-FILL-POINTER FONT)
                        ;;ELSE
                        (LENGTH (FONT-CHAR-WIDTH-TABLE FONT))))
	 (SAMPLE-FONT-MAX-RASTER-WIDTH (OR (LET ((CHAR-WIDTH-TABLE (FONT-CHAR-WIDTH-TABLE SAMPLE-FONT))
                                                 (MAX-CHAR-WIDTH 0))
                                             (AND CHAR-WIDTH-TABLE
                                                  (DOTIMES (CHAR (LENGTH CHAR-WIDTH-TABLE) MAX-CHAR-WIDTH)
                                                    (SETQ MAX-CHAR-WIDTH (MAX MAX-CHAR-WIDTH
                                                                              (OR (AREF CHAR-WIDTH-TABLE CHAR)
                                                                                  0))))))
                                           (FONT-RASTER-WIDTH SAMPLE-FONT)))
	 (FONT-MAX-RASTER-WIDTH (OR (LET ((CHAR-WIDTH-TABLE (FONT-CHAR-WIDTH-TABLE FONT))
					  (MAX-CHAR-WIDTH 0))
				      (AND CHAR-WIDTH-TABLE
					   (DOTIMES (CHAR (LENGTH CHAR-WIDTH-TABLE) MAX-CHAR-WIDTH)
					     (SETQ MAX-CHAR-WIDTH (MAX MAX-CHAR-WIDTH
								       (OR (AREF CHAR-WIDTH-TABLE CHAR)
                                                                           0))))))
				    (FONT-RASTER-WIDTH FONT)))
         (START-X-OFFSET  50.)
         (START-Y-OFFSET 100.)
	 (end-x-fudge  7)
         (X-OFFSET-LINE-FUDGE -6.)
         (Y-OFFSET-LINE-FUDGE -6.)
	 (SPACING-BETWEEN-FONTS 3.)
	 (COLUMN-INCREMENT (+ (* -2 X-OFFSET-LINE-FUDGE)
			      (MAX SAMPLE-FONT-MAX-RASTER-WIDTH FONT-MAX-RASTER-WIDTH)))
         (ROW-INCREMENT    (+ (FONT-RASTER-HEIGHT SAMPLE-FONT)
			      SPACING-BETWEEN-FONTS
			      (FONT-RASTER-HEIGHT FONT)
			      (* -2 Y-OFFSET-LINE-FUDGE)))
	 (COLUMNS (LET ((COLUMNS-THAT-FIT (FLOOR (- (SHEET-WIDTH WINDOW) START-X-OFFSET)
                                                 COLUMN-INCREMENT)))
                    (OR (WHEN (AND COLUMNS (PLUSP COLUMNS))
			   (IF (<= COLUMNS COLUMNS-THAT-FIT)
			       (IF (PLUSP COLUMNS) COLUMNS
				   ;;ELSE
				   (CHECK-ARG COLUMNS (PLUSP COLUMNS) "a positive number" :FIXNUM)
				   (CERROR T NIL NIL "There is only room for ~d columns - ~d specified~%"
					   COLUMNS-THAT-FIT COLUMNS))))
                        (IF (PLUSP COLUMNS-THAT-FIT)
			    (IF COLUMNS
				COLUMNS-THAT-FIT
				(EXPT 2 (FLOOR (LOG COLUMNS-THAT-FIT 2))))   ;find power of 2 that fits
                            ;;ELSE
                            (PROGN
                              (FORMAT WINDOW "~%This font is too large to fit on this window")
                              (RETURN-FROM DISPLAY-FONT))))))
         (ROWS (CEILING FONT-SIZE COLUMNS))
	 (ROWS-THAT-FIT (MIN ROWS
			     (FLOOR (- (SHEET-HEIGHT WINDOW) START-Y-OFFSET Y-OFFSET-LINE-FUDGE 40.)
				    ROW-INCREMENT)))
	 (ROW-INDEX 0)
         (CHARACTER 0)
         CURSOR-X CURSOR-Y
	 (aluf (sheet-char-aluf window)))
    ;; Loop thru the font, doing one screen's worth at a time.
    (PREPARE-SHEET (WINDOW)
      (DO (ROWS-TO-DO)
	  ((= ROW-INDEX ROWS))
	(SETQ ROWS-TO-DO (MIN ROWS-THAT-FIT (- ROWS ROW-INDEX)))
	(SEND WINDOW :SET-CURSORPOS 0 0)
      (SEND WINDOW :CLEAR-EOF)
      (WHEN MOUSE-SENSITIVE-ITEM-TYPE
        ;; Hack alert!! The following is a fix for the font editor when there is more than one
        ;; screenful of font information and the last screen is only partially filled.
        ;; Previously there were areas on the screen following the last character which were
        ;; mouse sensitive.  This hack clears out all mouse sensitivity at the beginning of
        ;; each page so that the additions made are the only ones in effect.  A better fix
        ;; would be to add a daemon for :CLEAR-EOF in BASIC-MOUSE-SENSITIVE-ITEMS to use the
        ;; current cursor position to clear out mouse sensitivity of certain items.  A check
        ;; could be made to see if the current cursorpos is (0,0) to see if it can just smash
        ;; the item list (like we are doing here).
	(SET-IN-INSTANCE WINDOW 'ITEM-LIST NIL)
 	(SEND WINDOW :STRING-OUT-EXPLICIT 
	      "Mouse on any character to select it"
	      START-X-OFFSET
	      (MAX (- START-Y-OFFSET 75.) (TRUNCATE START-Y-OFFSET 4))
	      999. 999.
	      HEADER-FONT
	      aluf)) ;;; ior to char-aluf

      ;; Write out the name of this font centered
      (SEND WINDOW :STRING-OUT-CENTERED-EXPLICIT 
	    (FORMAT NIL "~A ~:[(Continued)~]" (FONT-NAME FONT) (ZEROP ROW-INDEX))
	    START-X-OFFSET
	    (MAX (- START-Y-OFFSET 50.)
		 (TRUNCATE START-Y-OFFSET 2))
	    (MIN (SHEET-INSIDE-RIGHT WINDOW)
		 (+ START-X-OFFSET (* COLUMNS COLUMN-INCREMENT)))
	    999.
	    HEADER-FONT)
      
      ;; Do the Vertical lines and put the number labels above them.
      (DOTIMES (COLUMN-INDEX COLUMNS)
	;; Draw the vertical lines.
	(SYS:%DRAW-RECTANGLE 1  ; Width
                             (* ROWS-TO-DO ROW-INCREMENT)       ; Height
                             (+ START-X-OFFSET X-OFFSET-LINE-FUDGE (* COLUMN-INDEX COLUMN-INCREMENT))   ; X
                             (+ START-Y-OFFSET Y-OFFSET-LINE-FUDGE)     ; Y
                             aluf WINDOW) ;;; >>> ior to char-aluf

	;; Draw the number labels at the top.
	(LET* ((LABEL-PIXEL-LENGTH (* SAMPLE-FONT-MAX-RASTER-WIDTH      ; Width of the label
				      (IF (ZEROP COLUMN-INDEX)
					  1
                                          ;;ELSE
					  (1+ (FLOOR (LOG COLUMN-INDEX 8.))))))
	       (X-LABEL-OFFSET                                  ; X distance to move to write label.
		 (- (FLOOR (MAX SAMPLE-FONT-MAX-RASTER-WIDTH    ; Half the width of the character.
				FONT-MAX-RASTER-WIDTH) 2)
                    (FLOOR LABEL-PIXEL-LENGTH 2))))             ; Half the length of the label.
	  (SEND WINDOW :STRING-OUT-EXPLICIT
		(FORMAT NIL "~VR" LABEL-BASE COLUMN-INDEX)
		(+ START-X-OFFSET (* COLUMN-INDEX COLUMN-INCREMENT) X-LABEL-OFFSET)
		(+ START-Y-OFFSET Y-OFFSET-LINE-FUDGE  (- (FONT-RASTER-HEIGHT LABEL-FONT)))
		9999. 9999. LABEL-FONT ALUF)))  ;;; >>> ior to char-aluf

      ;; Draw the rightmost vertical line.
      (SYS:%DRAW-RECTANGLE 1                    ; Width
                           (* ROWS-TO-DO ROW-INCREMENT) ; Height
			   (+ START-X-OFFSET X-OFFSET-LINE-FUDGE (* COLUMNS COLUMN-INCREMENT))  ; X
			   (+ START-Y-OFFSET Y-OFFSET-LINE-FUDGE)       ; Y
                           ALUF WINDOW) ;;; >>> ior to char-aluf
	


      ;; Draw the topmost horizontal line.
      (SYS:%DRAW-RECTANGLE (+ END-X-FUDGE X-OFFSET-LINE-FUDGE (* COLUMNS COLUMN-INCREMENT))     ; Width
			    1                   ; Height
			    (+ START-X-OFFSET X-OFFSET-LINE-FUDGE)      ; X
			    (+ START-Y-OFFSET Y-OFFSET-LINE-FUDGE)      ; Y
			     ALUF WINDOW) ;;; ior to char-aluf
			       
 

      ;; Loop through all of the rows in the font that can fit on one screen.
      (BLOCK OUTER
	(DOTIMES (ROW ROWS-TO-DO)
	  (INCF ROW-INDEX)
	  (PROGN
	    (SETQ CURSOR-Y (+ START-Y-OFFSET (* ROW ROW-INCREMENT)))
	    ;; Draw a horizontal line at the bottom of this row.
	     (SYS:%DRAW-RECTANGLE (+ END-X-FUDGE  X-OFFSET-LINE-FUDGE (* COLUMNS COLUMN-INCREMENT))     ; Width
				   1            ; Height
				   (+ X-OFFSET-LINE-FUDGE START-X-OFFSET)	        ; X
				   (+ Y-OFFSET-LINE-FUDGE ROW-INCREMENT CURSOR-Y)       ; Y
				    ALUF WINDOW) ;;; >>> ior to char-aluf

	    ;; Put out the row heading.
	    (SEND WINDOW :STRING-OUT-EXPLICIT
		  (FORMAT NIL "~V,4R" LABEL-BASE CHARACTER)
		  0 CURSOR-Y 9999. 9999. LABEL-FONT ALUF) ;;; >>> ior to char-aluf

	    ;; Loop through all of the columns.
	    (LOOP FOR COLUMN-INDEX FROM 0 BELOW COLUMNS
		  DO
		  (PROGN
		    (SETQ CURSOR-X (+ START-X-OFFSET (* COLUMN-INDEX COLUMN-INCREMENT)))
		    ;; Here is where we actually draw the character.
		    ;; Draw a character in the sample font above the other character.
		   
		      (DRAW-CHAR SAMPLE-FONT
				 (IF (< CHARACTER (FONT-FILL-POINTER SAMPLE-FONT))
				     CHARACTER
                                     ;;ELSE
				     #\SPACE)   ; Draw a space character if no sample font chars there.
				 CURSOR-X
                                 CURSOR-Y
				 ALUF WINDOW) ;;; ior to char-aluf
		      (DRAW-CHAR FONT
				 CHARACTER
				 CURSOR-X
				 (+ CURSOR-Y SPACING-BETWEEN-FONTS
				    (FONT-RASTER-HEIGHT SAMPLE-FONT))
				 ALUF WINDOW) ;;; ior to char-aluf
		    ;; Give this character a mouse sensitive box.
		    (WHEN MOUSE-SENSITIVE-ITEM-TYPE
		      (SEND WINDOW :SEND-IF-HANDLES
			    :PRIMITIVE-ITEM
			    MOUSE-SENSITIVE-ITEM-TYPE
			    (INT-CHAR CHARACTER)                           ; Mouse click will return char object.
			    (- CURSOR-X 1)                                 ; Left
			    (- CURSOR-Y 1)                                 ; Top
			    (+ CURSOR-X (MAX SAMPLE-FONT-MAX-RASTER-WIDTH  ; Right
					     FONT-MAX-RASTER-WIDTH) 1)	      
			    (+ CURSOR-Y SPACING-BETWEEN-FONTS              ; Bottom
			       (FONT-RASTER-HEIGHT SAMPLE-FONT)
			       (FONT-RASTER-HEIGHT FONT)
			       1)))
		    (INCF CHARACTER)
		    ;; Get out when we don't have any more characters.
		    (WHEN (>= CHARACTER FONT-SIZE)
		      (RETURN-FROM OUTER NIL)))))))

      ;; See if we need to put out a MORE message.
      (WHEN (< ROW-INDEX ROWS)
        (LET ((MORE-STATUS (SEND WINDOW :MORE-P)))
          ;; Put cursor at bottom of screen.
          (SEND WINDOW :SET-CURSORPOS (+ START-X-OFFSET X-OFFSET-LINE-FUDGE)
                ;; Try to fit the cursor just below the lowest horizontal line.  If it
                ;; doesn't fit then put the cursor very near the bottom of the window.
        	(MIN (+ START-Y-OFFSET Y-OFFSET-LINE-FUDGE (* ROWS-TO-DO ROW-INCREMENT)
                        (FONT-RASTER-HEIGHT (SHEET-CURRENT-FONT WINDOW)))
		     ;; The following calculation is (* (1- number of lines in window) height of each line)
		     ;; which means the Y pixel value for the second to the last line.
		     (* (1- (FLOOR (SHEET-HEIGHT WINDOW) (SHEET-LINE-HEIGHT WINDOW))) (SHEET-LINE-HEIGHT WINDOW))))
          (SEND WINDOW :SET-MORE-P T)
          (LET* ((SELF WINDOW)
                 ;; Put out a more message and wait for user to do something.
                 (READ-ANY (SHEET-MORE-HANDLER :READ-ANY)))
            (WHEN (NULL MORE-STATUS)
              ;; Set the `more' status back to what it was before we changed it.
              (SEND WINDOW :SET-MORE-P MORE-STATUS))
            (WHEN (AND (CONSP READ-ANY) MOUSE-SENSITIVE-ITEM-TYPE)
              ;; If this was a mouse click, put it back in the io buffer and get out.
              (SEND WINDOW :UNREAD-ANY READ-ANY)
              (RETURN-FROM DISPLAY-FONT)))))))
    ;; Move the cursor back to 0 0 if requested.  Otherwise, put it after the table.
    (SEND WINDOW :SET-CURSORPOS 0 (IF RESET-CURSOR-P
				      0
                                      ;;ELSE
				      (+ CURSOR-Y ROW-INCREMENT)))))

(DEFMETHOD (SHEET :REVERSE-VIDEO-P) ()
  (IF (color-system-p self)
    color-reverse-video-state
    ;;; else
    (EQ CHAR-ALUF ALU-BACK)
  )
)

(DEFMETHOD (SHEET :SET-REVERSE-VIDEO-P) (REVERSE-VIDEO-P)
  (IF (color-system-p self)  
     (WHEN (NEQ reverse-video-p color-reverse-video-state) ; requested state differs from current state
       (SETQ color-reverse-video-state reverse-video-p)    ; set state to new state (which must be the opposite of what it was!)
       (SEND self :complement-bow-mode)                    ; now reverse the meaning of foreground and background color
   )
   ; else we must be monochrome system, do the appropriate thing.
   (progn
    (AND (NOT (= CHAR-ALUF (IF REVERSE-VIDEO-P ALU-BACK ALU-TRANSP)))
       (SHEET-FORCE-ACCESS (SELF)
	 (TV:PREPARE-SHEET (SELF)
	   (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 ALU-XOR SELF))))
    (IF REVERSE-VIDEO-P
      (SETQ CHAR-ALUF ALU-BACK ERASE-ALUF ALU-TRANSP)
      (SETQ CHAR-ALUF ALU-TRANSP ERASE-ALUF ALU-BACK))
   )
  )
)

(DEFMETHOD (SHEET :DEEXPOSED-TYPEIN-ACTION) ()
  (IF (ZEROP (SHEET-DEEXPOSED-TYPEIN-NOTIFY)) :NORMAL :NOTIFY))

(DEFMETHOD (SHEET :SET-DEEXPOSED-TYPEIN-ACTION) (VALUE)
  (SETF (SHEET-DEEXPOSED-TYPEIN-NOTIFY)
	(CASE VALUE
	  (:NORMAL 0)
	  (:NOTIFY 1)
	  (OTHERWISE
	   (FERROR
             NIL
             "~S illegal deexposed-typein-action; use :NORMAL or :NOTIFY")))))

(DEFMETHOD (SHEET :SAVE-BITS) ()
  (IF BIT-ARRAY T (IF (ZEROP (SHEET-FORCE-SAVE-BITS)) NIL :DELAYED)))

(DEFMETHOD (SHEET :SET-SAVE-BITS) (SAVE-BITS &optional dont-refresh &AUX (INHIBIT-SCHEDULING-FLAG T))
  (OR SUPERIOR (FERROR NIL "Cannot :SET-SAVE-BITS on a top-level sheet"))
  (LOCK-SHEET (SELF)
    (COND ((EQ SAVE-BITS 'T)
	   (LET ((INHIBIT-SCHEDULING-FLAG T))
	     (UNLESS BIT-ARRAY
		 (SETQ BIT-ARRAY
		       (MAKE-ARRAY
                         `(,HEIGHT
			   ,(TRUNCATE
			      (* 32. LOCATIONS-PER-LINE)
			      (SCREEN-BITS-PER-PIXEL (SHEET-GET-SCREEN SELF))))
                         :TYPE (SHEET-ARRAY-TYPE SELF)))
		 (when (mac-system-p)
		   (send-adjust-bit-array-maybe self)))
	     (COND ((NULL SCREEN-ARRAY)
		    (REDIRECT-ARRAY (SETQ SCREEN-ARRAY OLD-SCREEN-ARRAY)
				    (ARRAY-ELEMENT-TYPE BIT-ARRAY)
				    (ARRAY-DIMENSION BIT-ARRAY 1)
				    (ARRAY-DIMENSION BIT-ARRAY 0)
				    BIT-ARRAY 0)
		    (SETQ OLD-SCREEN-ARRAY NIL)
		    (when (mac-window-p self)
		      (redirect-drawing-of-window-and-inferiors self)))))
	   (COND ((NOT (or EXPOSED-P dont-refresh))	;added test for dont-refresh
		  ;; We are not exposed, first refresh ourself.
		  (SHEET-FORCE-ACCESS
                    (SELF :NO-PREPARE) (SEND SELF :REFRESH))
		  ;; Expose in reverse order for the sake of temporary windows.
		  (DOLIST (I (REVERSE EXPOSED-INFERIORS))
		    ;; Then actually expose all of our virtually exposed
                    ;; inferiors.  Note that we already own the lock on
                    ;; all of them, and the mouse can't be in them since
                    ;; we are deexposed.
		    (FUNCALL I :EXPOSE)))))
	  ((NULL BIT-ARRAY))
	  (T
	   (SETQ BIT-ARRAY NIL)
	   ;; Note that SCREEN-ARRAY still points to the old value of
           ;; BIT-ARRAY.  This is important for the following deexposes
           ;; to work.
	   (COND ((NOT EXPOSED-P)
		  ;; The mouse can't possibly be in any of these
                  ;; windows, so it's alright to just go ahead and
                  ;; deexpose them with us locked.
		  (DOLIST (I EXPOSED-INFERIORS)
		    (FUNCALL I :DEEXPOSE :DEFAULT :NOOP NIL))
		  (WITHOUT-INTERRUPTS		    
		    (SETQ OLD-SCREEN-ARRAY SCREEN-ARRAY)
		    (LET ((ARRAY (SHEET-SUPERIOR-SCREEN-ARRAY)))
		      (REDIRECT-ARRAY OLD-SCREEN-ARRAY
                                      (ARRAY-ELEMENT-TYPE OLD-SCREEN-ARRAY)
				      (ARRAY-DIMENSION ARRAY 1)
				      (ARRAY-DIMENSION OLD-SCREEN-ARRAY 0)
				      ARRAY
				      (+ X-OFFSET
                                         (* Y-OFFSET
                                            (ARRAY-DIMENSION ARRAY 1)))))
		    (SETQ SCREEN-ARRAY NIL))))
	   (when (mac-window-p self)
	     (redirect-drawing-of-window-and-inferiors self))))
    (SETF (SHEET-FORCE-SAVE-BITS) (IF (EQ SAVE-BITS :DELAYED) 1 0)))
  SAVE-BITS)

(DEFMETHOD (SHEET :AFTER :SET-SAVE-BITS) (&rest IGNORE)
  (SCREEN-MANAGE-WINDOW-AREA SELF))

;;;patched for GRH by GSM on 5 Jan 86
(DEFUN ERASE-MARGINS ()
  ;; This function erases the margins of a window.  This function must
  ;; be called from within a method in order to work properly.  The
  ;; order of erasing is as follows.
  ;;      
  ;;      .--------C--------.
  ;;      |                 |
  ;;      |                 |
  ;;      |                 |
  ;;      A                 B
  ;;      |                 |
  ;;      |                 |
  ;;      |                 |
  ;;      `--------D--------'
  ;;      
  ;; The letters on the sides of the labels correspond to the comment on
  ;; the call to %DRAW-RECTANGLE which does the actual erasing of the
  ;; margin.
  (DECLARE (:SELF-FLAVOR SHEET))
  (COND (SCREEN-ARRAY
	 (PREPARE-SHEET (SELF)
	   (%DRAW-RECTANGLE LEFT-MARGIN-SIZE HEIGHT     ; A
			    0 0 ERASE-ALUF SELF)
	   (%DRAW-RECTANGLE RIGHT-MARGIN-SIZE HEIGHT    ; B
			    (SHEET-INSIDE-RIGHT) 0 ERASE-ALUF SELF)
	   (%DRAW-RECTANGLE (sheet-inside-WIDTH) TOP-MARGIN-SIZE       ; C  GRH
			    (sheet-inside-left) 0 ERASE-ALUF SELF)	;GRH
	   (%DRAW-RECTANGLE (sheet-inside-WIDTH) BOTTOM-MARGIN-SIZE    ; D  GRH
			    (sheet-inside-left) (SHEET-INSIDE-BOTTOM) ERASE-ALUF SELF)))))	;GRH

(DEFMETHOD (SHEET :CHANGE-OF-SIZE-OR-MARGINS)
           (&REST OPTIONS
            &AUX TOP BOTTOM LEFT RIGHT
            NEW-HEIGHT NEW-WIDTH OLD-X OLD-Y
            (OLD-TOP-MARGIN-SIZE  TOP-MARGIN-SIZE)
            (OLD-LEFT-MARGIN-SIZE LEFT-MARGIN-SIZE)
            DELTA-TOP-MARGIN
            DELTA-LEFT-MARGIN
            (INTEGRAL-P NIL)
            (OLD-INSIDE-WIDTH  (SHEET-INSIDE-WIDTH))
            (OLD-INSIDE-HEIGHT (SHEET-INSIDE-HEIGHT))
            (OLD-WIDTH WIDTH)
            (OLD-HEIGHT HEIGHT))
  "Change some sheet parameters."
  (OR SUPERIOR (NOT EXPOSED-P)
      (FERROR
         NIL
         "Cannot change size or margins of an exposed window with no superior"))
  (SHEET-FORCE-ACCESS (SELF)
    (ERASE-MARGINS))
  (SETQ OLD-X (- CURSOR-X LEFT-MARGIN-SIZE)
	OLD-Y (- CURSOR-Y TOP-MARGIN-SIZE))
  ;; Process options
  (DOPLIST (OPTIONS VAL OP)
    (CASE OP
      ((:TOP :Y)           (SETQ TOP                VAL))
      (:BOTTOM             (SETQ BOTTOM             VAL))
      ((:LEFT :X)          (SETQ LEFT               VAL))
      (:RIGHT              (SETQ RIGHT              VAL))
      (:WIDTH              (SETQ NEW-WIDTH          VAL))
      (:HEIGHT             (SETQ NEW-HEIGHT         VAL))
      (:TOP-MARGIN-SIZE    (SETQ TOP-MARGIN-SIZE    VAL))
      (:BOTTOM-MARGIN-SIZE (SETQ BOTTOM-MARGIN-SIZE VAL))
      (:LEFT-MARGIN-SIZE   (SETQ LEFT-MARGIN-SIZE   VAL))
      (:RIGHT-MARGIN-SIZE  (SETQ RIGHT-MARGIN-SIZE  VAL))
      (:INTEGRAL-P         (SETQ INTEGRAL-P         VAL))
      (OTHERWISE (FERROR NIL "~S is not a recognized option" OP))))
  (SETQ X-OFFSET (OR LEFT (IF RIGHT
                              (- RIGHT (OR NEW-WIDTH WIDTH))
                            ;;ELSE
                            X-OFFSET)))
  (SETQ Y-OFFSET (OR TOP (IF BOTTOM
                             (- BOTTOM (OR NEW-HEIGHT HEIGHT))
                           ;;ELSE
                           Y-OFFSET)))
  (SETQ NEW-WIDTH  (OR NEW-WIDTH  (IF RIGHT  (- RIGHT  LEFT) WIDTH)))
  (SETQ NEW-HEIGHT (OR NEW-HEIGHT (IF BOTTOM (- BOTTOM TOP)  HEIGHT)))
  (SETQ WIDTH NEW-WIDTH HEIGHT NEW-HEIGHT)

  ;; We need to deexpose all of our inferiors that won't fit anymore.
  (DOLIST (I EXPOSED-INFERIORS)
    (OR (SHEET-WITHIN-P I (SHEET-INSIDE-LEFT) (SHEET-INSIDE-TOP)
			(SHEET-INSIDE-RIGHT) (SHEET-INSIDE-BOTTOM))
	(FUNCALL I :DEEXPOSE)))

  (WITHOUT-INTERRUPTS
    (SHEET-FORCE-ACCESS (SELF T)
      (MAPC #'OPEN-BLINKER BLINKER-LIST))
    (SHEET-DEDUCE-AND-SET-SIZES RIGHT BOTTOM (SEND SELF :VSP) INTEGRAL-P)
    (SETQ CURSOR-X
	  (MIN (+ LEFT-MARGIN-SIZE OLD-X)
               (- WIDTH RIGHT-MARGIN-SIZE CHAR-WIDTH)))
    (SETQ CURSOR-Y
	  (MIN (+ TOP-MARGIN-SIZE OLD-Y)
               (- HEIGHT BOTTOM-MARGIN-SIZE LINE-HEIGHT)))
    (DOLIST (BL BLINKER-LIST)
      (COND ((NULL (BLINKER-X-POS BL)))
	    ((>= (BLINKER-X-POS BL)   (SHEET-INSIDE-RIGHT))
	     (SETF (BLINKER-X-POS BL) (SHEET-INSIDE-LEFT)))) ; Wrap blinker around
      (COND ((NULL (BLINKER-Y-POS BL)))
	    ((>= (BLINKER-Y-POS BL)   (SHEET-INSIDE-BOTTOM))
	     (SETF (BLINKER-Y-POS BL) (SHEET-INSIDE-TOP)))))
    (WHEN superior  ;; A screen will have NIL for superior.  02/15/88 KJF
      ;; Don't let locations-per-line change without changing sheet's arrays also, if needed.
      ;; This handles cases of color/monochrome inconsistencies.
      ;; On microExplorer, locations-per-line may be arbitrary, but arrays will always be
      ;; the same (1 bit, until color on mExp. is available).  So for now, on mExp., we only
      ;; need to make sure locations-per-line matches superior.  04/22/88 KJF.
      (IF (mac-window-p self)
	  (SETF locations-per-line (sheet-locations-per-line superior)) ;; If superior is nil, we break.  KJF
	  ;; :sheet-legal-for-superior determines if any conversion needs to take place.
	  ;; That is, if one is color and one is monochrome, a conversion may occur.
	  ;; :sheet-legal-for-superior can convert in both directions.  It looks at:
	  ;; *convert-color-sheet-to-monochrome*  04/22/88 KJF
	  (SEND self :sheet-legal-for-superior)))
    (WHEN BIT-ARRAY
      ;; Handle the possibility of growing the window wider than its screen.  So
      ;; long as the window is never activated & exposed, this causes no problems.
      ;; Visidoc requires this capability. 11/11/88 LG
      (LET* ((our-screen (sheet-get-screen self))
	     (pixels-per-location (floor 32. (screen-bits-per-pixel our-screen)))
	     (screen-width (* pixels-per-location (sheet-locations-per-line our-screen)))
	     (width-to-use (MAX (* pixels-per-location (CEILING width pixels-per-location)) screen-width))
	     (locations-per-line-to-use (FLOOR width-to-use pixels-per-location)))
	(WHEN (> width-to-use screen-width)
	  (SETF locations-per-line locations-per-line-to-use))
	(SETQ BIT-ARRAY
	      (GROW-BIT-ARRAY
		BIT-ARRAY
		width-to-use
		HEIGHT width
		OLD-HEIGHT OLD-WIDTH))))

    (cond (superior
	   ;;if we have a bit-array, SCREEN-ARRAY indirects to it, else
           ;; OLD-SCREEN-ARRAY indirects into our superior.
	   (LET ((ARRAY (OR SCREEN-ARRAY OLD-SCREEN-ARRAY))
		 (INDIRECT-TO (OR (AND (NOT EXPOSED-P) BIT-ARRAY)
				  (SHEET-SUPERIOR-SCREEN-ARRAY))))
	     (WHEN (AND array indirect-to)
	     (REDIRECT-ARRAY
	       ARRAY (ARRAY-ELEMENT-TYPE INDIRECT-TO)
	       (ARRAY-DIMENSION INDIRECT-TO 1) HEIGHT
	       INDIRECT-TO
	       (IF (AND BIT-ARRAY (NOT EXPOSED-P)) 0
		   (+ X-OFFSET (* Y-OFFSET (ARRAY-DIMENSION INDIRECT-TO 1)))))
	     (IF (OR BIT-ARRAY EXPOSED-P)
		 (SETQ SCREEN-ARRAY ARRAY
		       OLD-SCREEN-ARRAY NIL)
               ;;ELSE
               (SETQ OLD-SCREEN-ARRAY ARRAY
                     SCREEN-ARRAY NIL))
	     (when (mac-window-p self)
	       (send-adjust-bit-array-maybe self t)
	       (redirect-drawing-of-window-and-inferiors self))
	     ;; If the size of the top and/or left margin changed, move
             ;; the inside bits around.
	     (SETQ DELTA-TOP-MARGIN (- TOP-MARGIN-SIZE OLD-TOP-MARGIN-SIZE)
		   DELTA-LEFT-MARGIN (- LEFT-MARGIN-SIZE OLD-LEFT-MARGIN-SIZE))
	     (COND ((AND (ZEROP DELTA-TOP-MARGIN)
                         (ZEROP DELTA-LEFT-MARGIN)))
                   ;; Don't BITBLT some other guy's bits!!
		   ((NULL SCREEN-ARRAY))
		   (T
                    ;; This should be BITBLT-WITH-FAST-PAGING, sometimes
                    ;; it is not paged in.
		    (OR EXPOSED-P
                        (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT)))
		    (BITBLT ALU-SETA
                            (IF (PLUSP DELTA-LEFT-MARGIN)
                                (- (SHEET-INSIDE-WIDTH))
                              ;;ELSE
                              (SHEET-INSIDE-WIDTH))
			    (IF (PLUSP DELTA-TOP-MARGIN)
                                (- (SHEET-INSIDE-HEIGHT))
                              ;;ELSE
                              (SHEET-INSIDE-HEIGHT))
			    ARRAY OLD-LEFT-MARGIN-SIZE OLD-TOP-MARGIN-SIZE
			    ARRAY LEFT-MARGIN-SIZE TOP-MARGIN-SIZE)
		    ;; If margins got smaller, may be space to clear out
                    ;; on bottom and right.
		    (AND (MINUSP DELTA-LEFT-MARGIN)
			 (BITBLT ERASE-ALUF
                                 (- DELTA-LEFT-MARGIN)
                                 (SHEET-INSIDE-HEIGHT)
				 ARRAY
                                 (+ (SHEET-INSIDE-RIGHT) DELTA-LEFT-MARGIN)
				 (SHEET-INSIDE-TOP)
				 ARRAY
                                 (+ (SHEET-INSIDE-RIGHT) DELTA-LEFT-MARGIN)
				 (SHEET-INSIDE-TOP)))
		    (AND (MINUSP DELTA-TOP-MARGIN)
			 (BITBLT ERASE-ALUF
                                 (SHEET-INSIDE-WIDTH)
                                 (- DELTA-TOP-MARGIN)
				 ARRAY
                                 (SHEET-INSIDE-LEFT)
				 (+ (SHEET-INSIDE-BOTTOM) DELTA-TOP-MARGIN)
				 ARRAY
                                 (SHEET-INSIDE-LEFT)
				 (+ (SHEET-INSIDE-BOTTOM) DELTA-TOP-MARGIN)))))
	     (SHEET-FORCE-ACCESS (SELF)
	       (ERASE-MARGINS)))
	     (AND TEMPORARY-BIT-ARRAY (NEQ TEMPORARY-BIT-ARRAY T)
		  (SETQ TEMPORARY-BIT-ARRAY (GROW-BIT-ARRAY
					      TEMPORARY-BIT-ARRAY WIDTH
					      HEIGHT WIDTH NIL NIL NIL))))))
    (SEND SELF :UPDATE-TIME-STAMP)
    (OR (NOT (= OLD-INSIDE-WIDTH  (SHEET-INSIDE-WIDTH)))
	(NOT (= OLD-INSIDE-HEIGHT (SHEET-INSIDE-HEIGHT))))))

;;The following used to be used by the method (SHEET :AFTER :CHANGE-OF-SIZE-OR-MARGINS)
;;See the commented-out method right below this function. 
;(DEFUN CLEAR-HUGE-RESOURCES (SUPERIOR)
;  "Scan through all window resources that have SUPERIOR as their
;superior to see if they will fit within SUPERIOR."

;  ;; Essentially, this function goes through the resource inferiors of
;  ;; SUPERIOR to make sure that they still fit inside of it.  If they
;  ;; don't fit inside their superior then the resource is cleared.  When
;  ;; the resource is needed, another object of that resource type will
;  ;; be created from scratch.
;  (LET (SUPERIOR-WIDTH
;	SUPERIOR-HEIGHT)
;    (MULTIPLE-VALUE-SETQ (SUPERIOR-WIDTH SUPERIOR-HEIGHT)
;      (SEND SUPERIOR :SEND-IF-HANDLES :SIZE))
    
;    ; Go through the list of window resource names maintained by
;    ; the window system.
;    (DOLIST (RESOURCE-NAME TV:WINDOW-RESOURCE-NAMES)
;      (LET (
;            ;; Flag -> t when we need to clear the resource.
;            (CLEAR-THE-RESOURCE NIL)	
;	    (RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))
;            ;; height, width of window resource.
;	    WINDOW-HEIGHT
;	    WINDOW-WIDTH)
;	(WHEN RESOURCE
;	  ;; There may be more than one object created for this
;          ;; resource.  We need to check each of them.
;	  (DOTIMES (RESOURCE-INDEX (SI:RESOURCE-N-OBJECTS RESOURCE))
;	    (LET ((RESOURCE-OBJECT (SI:RESOURCE-OBJECT
;                                     RESOURCE RESOURCE-INDEX)))
;	      (WHEN (TYPEP RESOURCE-OBJECT :INSTANCE)
;		;; Is this the resource we are interested in;
;		(WHEN (EQ SUPERIOR
;                          (SEND RESOURCE-OBJECT :SEND-IF-HANDLES :SUPERIOR))
;		  ;; Yes, get the dimensions and see if they are within
;                  ;; the superior's.
;		  (MULTIPLE-VALUE-SETQ (WINDOW-WIDTH WINDOW-HEIGHT)
;		    (SEND RESOURCE-OBJECT :SEND-IF-HANDLES :SIZE))
;		  (IF (OR (> WINDOW-HEIGHT SUPERIOR-HEIGHT)
;			  (> WINDOW-WIDTH  SUPERIOR-WIDTH))
;		      (SETQ CLEAR-THE-RESOURCE T))))))

;	  (WHEN CLEAR-THE-RESOURCE
;	    (CLEAR-RESOURCE RESOURCE-NAME)
;	    ))))
;    ))

;;;This apparently used to be necessary, but now is too expensive
;;;of an operation to perform for NO benefit.
;(DEFMETHOD (SHEET :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
;  "Clear resources which are no longer within SELF."
;  (CLEAR-HUGE-RESOURCES SELF))



(DEFUN MAKE-SHEET-BIT-ARRAY (SHEET X Y &REST MAKE-ARRAY-OPTIONS)
  "Create a bit-array for SHEET, of size X by Y."
  (LET* ((TYPE (ARRAY-TYPE
                 (WITHOUT-INTERRUPTS
                   (OR (SHEET-SCREEN-ARRAY (SHEET-GET-SCREEN SHEET))
                       (SHEET-OLD-SCREEN-ARRAY (SHEET-GET-SCREEN SHEET))))))
	 (ROUND-TO (TRUNCATE 32. (OR (CDR (ASSOC TYPE ARRAY-BITS-PER-ELEMENT :TEST #'EQ)) 32.))))
    (apply #'MAKE-ARRAY `(,Y ,(* (CEILING X ROUND-TO) ROUND-TO))
	   :TYPE TYPE
	   MAKE-ARRAY-OPTIONS)))

(DEFUN GROW-BIT-ARRAY (ARRAY WIDTH HEIGHT &OPTIONAL (REAL-WIDTH WIDTH)
		       OLD-HEIGHT OLD-REAL-WIDTH
		       (CONTENTS-MATTER T)
		       &AUX
                       (color-system (unless (typep array '(array bit)) t))
		       (alu-initialize (if color-system alu-back alu-setz))
		       (AWIDTH  (ARRAY-DIMENSION ARRAY 1))
		       (AHEIGHT (ARRAY-DIMENSION ARRAY 0)))
  "Adjust size of ARRAY for a window of size WIDTH, HEIGHT.
REAL-WIDTH		is the actual width to make the array.
OLD-HEIGHT and OLD-REAL-WIDTH are the previous size of the array,
CONTENTS-MATTER 	says preserve the old contents if possible."
  (LET ((WWIDTH (LOGAND #o-40 (+ WIDTH #o37)))	;Width as even number of words.
	(REAL-ARRAY ARRAY))
    (COND ((NOT (IF CONTENTS-MATTER
		    (AND (EQ HEIGHT OLD-HEIGHT) (EQ REAL-WIDTH OLD-REAL-WIDTH))
		  (AND (<= WWIDTH AWIDTH) (<= HEIGHT AHEIGHT))))
	   (WHEN (AND (mac-system-p)
		      (TYPEP self 'sheet)
		      (EQ array (tv:sheet-bit-array self)))
	       (send-adjust-bit-array-maybe self contents-matter
					    old-real-width old-height))
	   (IF (OR (> WWIDTH AWIDTH) (> HEIGHT AHEIGHT))
	       ;; Need bigger array, make it and copy in the old one
	       (LET ((NARRAY (MAKE-ARRAY `(,HEIGHT ,WWIDTH)
					 :TYPE (ARRAY-TYPE ARRAY))))
		 (COND (CONTENTS-MATTER
			(SI:PAGE-IN-ARRAY ARRAY)
			(if (mac-window-p self)
			    (si-bitblt alu-seta
					   (MIN real-width awidth)
					   (MIN height     aheight)
					   array 0 0 narray 0 0)
			    (BITBLT ALU-SETA
				    (MIN REAL-WIDTH AWIDTH)
				    (MIN HEIGHT     AHEIGHT)
				    ARRAY 0 0 NARRAY 0 0))
			;;note we cannot just initialize the background in the make-array
			;;at this point we don't know the background color; also recall
			;;that the previous BITBLT extends to the right past the real screen
			;;width.  PMH
			(when (and color-system  OLD-REAL-WIDTH  OLD-HEIGHT)
			  (when (plusp (- REAL-WIDTH OLD-REAL-WIDTH))
			    (if (mac-window-p self)
				(si-bitblt alu-initialize (- real-width old-real-width) height
					       narray old-real-width 0
					       narray old-real-width 0)
				(BITBLT ALU-initialize (- REAL-WIDTH OLD-REAL-WIDTH) HEIGHT
					NARRAY OLD-REAL-WIDTH 0
					NARRAY OLD-REAL-WIDTH 0)))
			  (when (plusp (- HEIGHT OLD-HEIGHT))
			    (if (mac-window-p self)
				(si-bitblt alu-initialize real-width (- height old-height)
					       narray 0 old-height
					       narray 0 old-height)
				(BITBLT ALU-initialize REAL-WIDTH (- HEIGHT OLD-HEIGHT)
					NARRAY 0 OLD-HEIGHT
					NARRAY 0 OLD-HEIGHT))))))
		 (STRUCTURE-FORWARD ARRAY NARRAY)
		 (SI:PAGE-OUT-ARRAY ARRAY)
		 (SETQ REAL-ARRAY NARRAY))
	     ;; Array is big enough, but used size is changing and we
             ;; want to initialize the contents that are about to be used.
	     (COND ((AND OLD-REAL-WIDTH (> REAL-WIDTH OLD-REAL-WIDTH))
		    (PAGE-IN-PIXEL-ARRAY ARRAY NIL (LIST REAL-WIDTH HEIGHT))
		    (if (mac-window-p self)
			(si-bitblt alu-initialize (- real-width old-real-width) height
				       array old-real-width 0
				       array old-real-width 0)
			(BITBLT ALU-initialize (- REAL-WIDTH OLD-REAL-WIDTH) HEIGHT
				ARRAY OLD-REAL-WIDTH 0
				ARRAY OLD-REAL-WIDTH 0))))
	     (COND ((AND OLD-HEIGHT (> HEIGHT OLD-HEIGHT))
		    (PAGE-IN-PIXEL-ARRAY ARRAY (LIST REAL-WIDTH HEIGHT))
		    (if (mac-window-p self)
			(si-bitblt alu-initialize real-width (- height old-height)
			      array 0 old-height
			      array 0 old-height)
			(BITBLT ALU-initialize REAL-WIDTH (- HEIGHT OLD-HEIGHT)
				ARRAY 0 OLD-HEIGHT
				ARRAY 0 OLD-HEIGHT)))))))
    REAL-ARRAY))





;;; Called to set the position of SELF, which must be deexposed and
;;; locked.  Can be called on sheets on deexposed screens.  NEW-X and
;;; NEW-Y are rel to SELF's superior.  This is not used.
(DEFUN SHEET-SET-DEEXPOSED-POSITION (NEW-X NEW-Y)
  (DECLARE (:SELF-FLAVOR SHEET))
  (AND EXPOSED-P
       (FERROR
         NIL
         "Wrong function called to set position of exposed sheet ~A" SELF))
  (SETQ X-OFFSET NEW-X
	Y-OFFSET NEW-Y)
  (OR BIT-ARRAY (NULL SUPERIOR)
      ;; No bit array, but we have a superior.
     (LET ((SUP-ARRAY (SHEET-SUPERIOR-SCREEN-ARRAY)))
	(REDIRECT-ARRAY OLD-SCREEN-ARRAY (ARRAY-ELEMENT-TYPE OLD-SCREEN-ARRAY)
			(ARRAY-DIMENSION SUP-ARRAY 1)
			(ARRAY-DIMENSION OLD-SCREEN-ARRAY 0)
			SUP-ARRAY
			(+ NEW-X (* NEW-Y
				    (ARRAY-DIMENSION SUP-ARRAY 1))))))
  (SEND SELF :UPDATE-TIME-STAMP))

(DEFUN SHEET-SET-EXPOSED-POSITION (NEW-X NEW-Y &AUX OX OY)
  "Called to set the position of SELF, which must be exposed and
locked.  NEW-X and NEW-Y are rel to SELF's superior.  A subroutine
of :SET-EDGES."
  (DECLARE (:SELF-FLAVOR SHEET))
  (PREPARE-SHEET (SELF)
    (SETQ OX X-OFFSET
	  OY Y-OFFSET
	  X-OFFSET NEW-X
	  Y-OFFSET NEW-Y)
    (LET ((SUP-ARRAY (SHEET-SUPERIOR-SCREEN-ARRAY)))
      (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-ELEMENT-TYPE SCREEN-ARRAY)
		      (ARRAY-DIMENSION  SUP-ARRAY 1)
		      (ARRAY-DIMENSION SCREEN-ARRAY 0)
		      SUP-ARRAY
		      (+ NEW-X (* NEW-Y (ARRAY-DIMENSION SUP-ARRAY 1))))
      (when (mac-window-p self)
	(redirect-drawing-of-window-and-inferiors self))
      (BITBLT ALU-SETA
	      (IF (> OX NEW-X) WIDTH (- WIDTH))
	      (IF (> OY NEW-Y) HEIGHT (- HEIGHT))
	      SUP-ARRAY OX OY
	      SUP-ARRAY NEW-X NEW-Y))
    (SETQ MOUSE-RECONSIDER T))
  (SEND SELF :UPDATE-TIME-STAMP))

;;; This may need some work to really work right if locations-per-line changes
(DEFMETHOD (SHEET :SET-SUPERIOR) (NEW-SUPERIOR &AUX ACTIVE-P)
  "Make NEW-SUPERIOR the superior of self.  Type of sheets are compared to
see if they match.  That is, if one is color and one is monochrome, then some
conversion may take place."
  (OR (EQ NEW-SUPERIOR SUPERIOR)
      (DELAYING-SCREEN-MANAGEMENT
	(AND EXPOSED-P (SEND SELF :DEEXPOSE))
	(WITHOUT-INTERRUPTS
	  (COND ((SETQ ACTIVE-P (MEMBER SELF (SHEET-INFERIORS SUPERIOR) :TEST #'EQ))
                 ;; Remove us from our old superior's list of inferiors.
		 (SETF (SHEET-INFERIORS SUPERIOR)
                       (DELETE SELF (THE LIST (SHEET-INFERIORS SUPERIOR)) :TEST #'EQ))
		 (FUNCALL SUPERIOR :ORDER-INFERIORS)
		 (SCREEN-AREA-HAS-CHANGED SELF)))
	  ;; If moving window from one screen to another, remove window from the screens
	  ;; copy of previously-selected-windows.  04/23/88 KJF
	  ;; Only bother doing this if screen was created in normal way.  That is, using
	  ;; tv:make-a-screen or tv:create-color-screen.  04/26/88 KJF
	  (WHEN (explorer-screen-p (sheet-get-screen self))
	    (UNLESS (EQ (sheet-get-screen new-superior) (sheet-get-screen self))
	      (remove-from-screens-previously-selected-windows self)))
	  (SETQ SUPERIOR NEW-SUPERIOR)
	  ;; Don't let locations-per-line change without changing sheet's arrays also, if needed.
	  ;; This handles cases of color/monochrome inconsistencies.
	  ;; On microExplorer, locations-per-line may be arbitrary, but arrays will always be
	  ;; the same (1 bit, until color on mExp. is available).  So for now, on mExp., we only
	  ;; need to make sure locations-per-line matches new superior.  04/22/88 KJF.
	  (IF (mac-window-p self)
	      (SETF locations-per-line (sheet-locations-per-line new-superior))
	      ;; :sheet-legal-for-superior determines if any conversion needs to take place.
	      ;; That is, if one is color and one is monochrome, a conversion may occur.
	      ;; :sheet-legal-for-superior can convert in both directions.  It looks at:
	      ;; *convert-color-sheet-to-monochrome*  04/22/88 KJF
	      (SEND self :sheet-legal-for-superior))
	  ;; Old code.  :sheet-legal-for-superior will set locations-per-line.
	  ;; If any conversion takes place, all inferiors will be fixed also, thus sometimes,
	  ;; (SHEET-SET-SUPERIOR-PARAMS SELF LOCATIONS-PER-LINE) will be redundant.
	  ;; 04/22/88 KJF
;;		LOCATIONS-PER-LINE (SHEET-LOCATIONS-PER-LINE NEW-SUPERIOR))
	  (SHEET-SET-SUPERIOR-PARAMS SELF LOCATIONS-PER-LINE)
	  (COND (BIT-ARRAY
		 (SETQ BIT-ARRAY
		       (GROW-BIT-ARRAY
                         BIT-ARRAY
                         (TRUNCATE (* LOCATIONS-PER-LINE 32.)
                                   (SCREEN-BITS-PER-PIXEL
                                     (SHEET-GET-SCREEN SELF)))
			 HEIGHT WIDTH))
		 (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-ELEMENT-TYPE SCREEN-ARRAY)
				 (TRUNCATE (* LOCATIONS-PER-LINE 32.)
					   (SCREEN-BITS-PER-PIXEL
                                             (SHEET-GET-SCREEN SELF)))
				 HEIGHT
				 BIT-ARRAY 0)
		 (when (mac-window-p self)
		   (send-adjust-bit-array-maybe self)
		   (redirect-drawing-of-window-and-inferiors self)))		 
		(T
		 (REDIRECT-ARRAY OLD-SCREEN-ARRAY (ARRAY-ELEMENT-TYPE OLD-SCREEN-ARRAY)
				 (TRUNCATE (* LOCATIONS-PER-LINE 32.)
					   (SCREEN-BITS-PER-PIXEL
                                             (SHEET-GET-SCREEN SELF)))
				 HEIGHT
				 (SHEET-SUPERIOR-SCREEN-ARRAY)
				 (+ X-OFFSET
                                    (TRUNCATE
                                      (* LOCATIONS-PER-LINE 32. Y-OFFSET)
                                      (SCREEN-BITS-PER-PIXEL
                                        (SHEET-GET-SCREEN SELF)))))))
	  (COND (ACTIVE-P
		 (SHEET-CONSING
		   (SETF (SHEET-INFERIORS NEW-SUPERIOR)
			 (CONS SELF (COPY-LIST (SHEET-INFERIORS NEW-SUPERIOR)))))
		 (FUNCALL NEW-SUPERIOR :ORDER-INFERIORS)
		 (SCREEN-AREA-HAS-CHANGED SELF)))
	  (SEND SELF :UPDATE-TIME-STAMP)))))

(DEFUN SHEET-SET-SUPERIOR-PARAMS (SHEET LOC-PER-LINE)
  (SETF (SHEET-LOCATIONS-PER-LINE SHEET) LOC-PER-LINE)
  (DOLIST (I (SHEET-INFERIORS SHEET))
    (SHEET-SET-SUPERIOR-PARAMS I LOC-PER-LINE)))

;;; Sheet exposure/deexposure

;;; Normal sheets ignore notification about exposure/deexposure/change-of-edges/selection
(DEFMETHOD (SHEET :INFERIOR-EXPOSE)    (SHEET)              SHEET)
(DEFMETHOD (SHEET :INFERIOR-DEEXPOSE)  (SHEET)              SHEET)
(DEFMETHOD (SHEET :INFERIOR-SET-EDGES) (SHEET &REST IGNORE) SHEET)
(DEFMETHOD (SHEET :INFERIOR-SELECT)    (SHEET &REST IGNORE) SHEET)
(DEFMETHOD (SHEET :INFERIOR-BURY)      (SHEET)              SHEET)

(DEFVAR *SHEETS-MADE-INVISIBLE-TO-MOUSE*)

(DEFMETHOD (SHEET :EXPOSABLE-P) ()
  (NOT (NULL (MEMBER SELF (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ))))

(DEFUN SHEET-PREPARE-FOR-EXPOSE (SHEET INSIDE-EXPOSE-METHOD
				 &OPTIONAL TURN-ON-BLINKERS BITS-ACTION
				 	   (X X-OFFSET) (Y Y-OFFSET))
  ;; The INSIDE-EXPOSE-METHOD argument is a kludge to tell us whether or
  ;; not we are being called from inside the :EXPOSE method.
  (DECLARE (:SELF-FLAVOR SHEET))
  TURN-ON-BLINKERS
  (BLOCK abort
    (PROG ((OLD-INHIBIT-SCHEDULING-FLAG INHIBIT-SCHEDULING-FLAG)
	   (INHIBIT-SCHEDULING-FLAG T)
	   RESULT)
       MAIN-LOOP
	  (SETQ INHIBIT-SCHEDULING-FLAG T)
	  (COND ((NOT (SHEET-CAN-GET-LOCK SHEET))
		 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		 (PROCESS-WAIT "Window Lock" #'SHEET-CAN-GET-LOCK SHEET)
		 (GO MAIN-LOOP)))
	  (WHEN EXPOSED-P
	    (RETURN-FROM ABORT T BITS-ACTION NIL))
	  (OR (NOT INSIDE-EXPOSE-METHOD)
	      (NULL SUPERIOR)
	      (MEMBER SELF (SHEET-INFERIORS SUPERIOR) :TEST #'EQ) 
	      ;; We can only be exposed if we are activated
	      (RETURN-FROM
		ABORT NIL BITS-ACTION
		(LIST NIL "Attempt to expose deactivated sheet ~S" SELF)))
	  (COND ((OR (NOT (= X-OFFSET X)) (NOT (= Y-OFFSET Y)))
		 (AND INSIDE-EXPOSE-METHOD
		      (RETURN-FROM ABORT NIL BITS-ACTION NIL))
		 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		 (SHEET-SET-DEEXPOSED-POSITION X Y)
		 (GO MAIN-LOOP)))
	  (OR (NULL SUPERIOR)
	      (NOT INSIDE-EXPOSE-METHOD)
	      (SHEET-WITHIN-SHEET-P SELF SUPERIOR)
	      (RETURN-FROM
		ABORT NIL BITS-ACTION
		(LIST NIL "Attempt to expose ~S outside of its superior" SELF)))
	  ;; If our superior is temp locked, see if we will overlap any of
	  ;; the temp windows.  If we will, then wait until the temp
	  ;; window is deexposed then try again.
	  (COND ((AND SUPERIOR
		      ;; A temporary window has a LOCK instance which is a
		      ;; list of windows it is partially covering up.
		      (CONSP (SHEET-LOCK SUPERIOR))
		      (SETQ RESULT
			    (DOLIST (TEMP-SHEET (SHEET-LOCK SUPERIOR))
			      (AND (SHEET-OVERLAPS-SHEET-P TEMP-SHEET SELF)
				   (RETURN TEMP-SHEET)))))
		 (AND INSIDE-EXPOSE-METHOD
		      (RETURN-FROM ABORT NIL BITS-ACTION NIL))
		 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		 (PROCESS-WAIT "Sheet Deexpose"
			       #'(LAMBDA (TEMP-SHEET SUP)
				   (OR (NOT (CONSP (SHEET-LOCK SUP)))
				       (NOT (MEMBER TEMP-SHEET (SHEET-LOCK SUP) :TEST #'EQ))))
			       RESULT SUPERIOR)
		 (GO MAIN-LOOP)))
	  (COND
	    ((SHEET-TEMPORARY-P)
	     (SETQ RESULT
		   (CATCH 'SHEET-EXPOSE-CANT-GET-LOCK
		     (LET ((*REQUESTOR* SELF))
		       (DECLARE (SPECIAL *REQUESTOR*))
		       ;; Check to make sure we can get all the locks at once.
		       (MAP-OVER-EXPOSED-SHEET
			 #'(LAMBDA (TARGET)
			     (AND ;; Can't be us, we aren't exposed yet
			       (NEQ TARGET (SHEET-SUPERIOR *REQUESTOR*))
			       ;; Sheet may be on EXPOSED-INFERIORS, but not
			       ;; in actuality exposed.
			       (SHEET-EXPOSED-P TARGET)
			       (SHEET-OVERLAPS-SHEET-P *REQUESTOR* TARGET)
			       (OR (SHEET-CAN-GET-TEMPORARY-LOCK
				     TARGET *REQUESTOR*)
				   (THROW 'SHEET-EXPOSE-CANT-GET-LOCK TARGET))
			       ;; If this window owns the mouse, must force
			       ;; mouse out of it.
			       (EQ TARGET MOUSE-WINDOW)
			       (THROW 'SHEET-EXPOSE-CANT-GET-LOCK TARGET)))
			 SUPERIOR)
		       ;; We can, get them all and win totally, but
		       ;; only do this if we are inside the expose
		       ;; method proper.
		       (AND INSIDE-EXPOSE-METHOD
			    (LET ((*REQUESTOR* SELF))
			      (DECLARE (SPECIAL *REQUESTOR*))
			      (MAP-OVER-EXPOSED-SHEET
				#'(LAMBDA
				    (TARGET)
				    (COND
				      ((AND
					 ;; Can't be us, we aren't
					 ;; exposed yet.
					 (NEQ TARGET (SHEET-SUPERIOR *REQUESTOR*))
					 ;; Sheet may be on
					 ;; EXPOSED-INFERIORS, but
					 ;; not in actuality exposed.
					 (SHEET-EXPOSED-P TARGET)
					 (SHEET-OVERLAPS-SHEET-P
					   *REQUESTOR* TARGET))
				       ;; All blinkers must get turned off
				       ;; on this sheet.
				       (SHEET-OPEN-BLINKERS TARGET)
				       (OR (SHEET-GET-TEMPORARY-LOCK TARGET *REQUESTOR*)
					   (FERROR NIL
						   "Internal error, can't get lock on ~A, but we already verified we could get lock"
						   TARGET))
				       (PUSH TARGET TEMPORARY-WINDOWS-LOCKED))))
				SUPERIOR)))
		       ;; Return NIL indicating that we are winning
		       NIL)))
	     (COND ((NULL RESULT)
		    (AND INSIDE-EXPOSE-METHOD
			 ;; For temporary windows, we must open the
			 ;; blinkers of our superiors to all levels.
			 (SHEET-OPEN-ALL-BLINKERS SUPERIOR)))
		   (INSIDE-EXPOSE-METHOD (RETURN-FROM ABORT NIL BITS-ACTION NIL))
		   ((EQ RESULT MOUSE-WINDOW)
		    (SETQ MOUSE-RECONSIDER T)
		    (PUSH RESULT *SHEETS-MADE-INVISIBLE-TO-MOUSE*)
		    (SETF (SHEET-INVISIBLE-TO-MOUSE-P RESULT) T)
		    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		    (PROCESS-WAIT "Mouse Out"
				  #'(LAMBDA (SHEET) (NEQ MOUSE-WINDOW SHEET))
				  RESULT)
		    (GO MAIN-LOOP))
		   (T
		    ;; One we couldn't get: wait for it
		    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		    (PROCESS-WAIT
		      "Temp Lock"
		      #'(LAMBDA (TARGET SHEET)
			  (OR (NOT (SHEET-EXPOSED-P TARGET))
			      (NOT (SHEET-OVERLAPS-SHEET-P SHEET TARGET))
			      (SHEET-CAN-GET-TEMPORARY-LOCK TARGET SHEET)))
		      RESULT SELF)
		    (GO MAIN-LOOP))))
	    (SUPERIOR
	     ;; Deexpose all we will overlap, then loop again as the world
	     ;; may have changed out from under us.
	     (LET ((FLAG NIL))
	       (DOLIST (SIBLING (SHEET-EXPOSED-INFERIORS SUPERIOR))
		 (COND ((AND (NEQ SELF SIBLING)
			     (SHEET-OVERLAPS-SHEET-P SELF SIBLING))
			(AND INSIDE-EXPOSE-METHOD
			     (RETURN-FROM ABORT NIL BITS-ACTION NIL))
			(SETQ INHIBIT-SCHEDULING-FLAG OLD-INHIBIT-SCHEDULING-FLAG
			      FLAG T)
			(FUNCALL SIBLING :DEEXPOSE))))
	       (AND FLAG
		    ;; If had to deexpose someone, world may have changed.
		    (GO MAIN-LOOP)))))
	  ;; We have successfully met all of the requirements, be successful.
	  (RETURN T BITS-ACTION))))

(DEFUN SHEET-EXPOSE
       (DAEMON-ARGS INTERNALS &AUX (*SHEETS-MADE-INVISIBLE-TO-MOUSE* NIL)
        VAL1 VAL2 VAL3)
  (DECLARE (:SELF-FLAVOR SHEET))
  (IF (OR (NULL SUPERIOR)
	  (TYPEP SUPERIOR 'SCREEN)
	  (FUNCALL SUPERIOR :INFERIOR-EXPOSE SELF))
      (DELAYING-SCREEN-MANAGEMENT
	(UNWIND-PROTECT
	  (DO ((DONE NIL) ERROR) (DONE)
	    (APPLY #'SHEET-PREPARE-FOR-EXPOSE SELF NIL
		   (CDR DAEMON-ARGS))
	    (SETQ ERROR
		  (CATCH 'SHEET-ABORT-EXPOSE
		    (LOCK-SHEET (SELF)
		      (MULTIPLE-VALUE-SETQ (VAL1 VAL2 VAL3)
                        (FUNCALL INTERNALS DAEMON-ARGS))
		      (SETQ DONE T)
		      NIL)))
	    (AND (NOT DONE) ERROR
		 (APPLY #'FERROR ERROR)))
	  (DOLIST (SHEET *SHEETS-MADE-INVISIBLE-TO-MOUSE*)
	    (SETF (SHEET-INVISIBLE-TO-MOUSE-P SHEET) NIL))
	  (MOUSE-WAKEUP))
	(VALUES VAL1 VAL2 VAL3))))

(DEFWRAPPER (SHEET :EXPOSE) (IGNORE . BODY)
  `(SHEET-EXPOSE
     SI:.DAEMON-CALLER-ARGS.
     #'(LAMBDA (SI:.DAEMON-CALLER-ARGS.
                &AUX FOO (SI:.DAEMON-MAPPING-TABLE. SYS:SELF-MAPPING-TABLE))
         ;; Local slot 1 must contain our mapping table
         ;; in something that contains combined method code.
         FOO
         SI:.DAEMON-MAPPING-TABLE.
         . ,BODY)))

;;; TURN-ON-BLINKERS means that this window will soon become the
;;; SELECTED-WINDOW, so it is not necessary to change blinkers from
;;; :BLINK to their DESELECTED-BLINKER-VISIBILITY.
(DEFMETHOD (SHEET :EXPOSE)
           (&OPTIONAL INHIBIT-BLINKERS BITS-ACTION (X X-OFFSET) (Y Y-OFFSET)
            &AUX (OLD-INHIBIT-SCHEDULING-FLAG INHIBIT-SCHEDULING-FLAG)
            (INHIBIT-SCHEDULING-FLAG T) SUPERIOR-HAS-SCREEN-ARRAY
            OK ERROR)
  "Expose a sheet (place it on the physical screen)."
  (BLOCK nil
    (SETQ RESTORED-BITS-P T)
    (OR BITS-ACTION (SETQ BITS-ACTION (IF BIT-ARRAY :RESTORE :CLEAN)))
    (AND EXPOSED-P (RETURN NIL))
    (SETQ RESTORED-BITS-P NIL)
    (SETQ SUPERIOR-HAS-SCREEN-ARRAY (OR (NULL SUPERIOR)
					(SHEET-SCREEN-ARRAY SUPERIOR)))
    (MULTIPLE-VALUE-SETQ (OK BITS-ACTION ERROR)
			 (SHEET-PREPARE-FOR-EXPOSE SELF T INHIBIT-BLINKERS BITS-ACTION X Y))
    (OR OK (THROW 'SHEET-ABORT-EXPOSE ERROR))
    ;; Have made our area of the screen safe for us.  We'll now call
    ;; ourselves "exposed", even though we haven't put our bits on
    ;; the screen at all.  This will win, because we have ourself
    ;; locked, and if someone wants to cover us he'll have to go
    ;; blocked until we are done -- it's a cretinous thing to have
    ;; happen, but the system shouldn't come crashing to the ground
    ;; because of it.
    ;; *** INHIBIT-SCHEDULING-FLAG had better still be T ***
    (OR INHIBIT-SCHEDULING-FLAG
	(FERROR
	  NIL
	  "Hairy part of expose finished with INHIBIT-SCHEDULING-FLAG off"))
    ;; Lie by saying that we are exposed, because we aren't really,
    ;; but we are locked so it doesn't matter.
    (AND SUPERIOR-HAS-SCREEN-ARRAY (SETQ EXPOSED-P T PREPARED-SHEET NIL))
    (AND SUPERIOR
	 (OR (NOT (MEMBER SELF (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ))
	     ;; Must always reorder in the case of temporary windows
	     ;; since they are the only type of window that can be
	     ;; exposed and overlapping some other exposed window.
	     (SHEET-TEMPORARY-P))
	 (SHEET-CONSING
	   ;; Put us at the top.
	   (SETF (SHEET-EXPOSED-INFERIORS SUPERIOR)
		 (CONS SELF (COPY-LIST
			      (DELETE SELF (THE LIST (SHEET-EXPOSED-INFERIORS SUPERIOR)) :TEST #'EQ))))))
    (COND ((AND SUPERIOR-HAS-SCREEN-ARRAY BIT-ARRAY)
	   (SETF (SHEET-OUTPUT-HOLD-FLAG) 0)
	   ;; Open all our blinkers, etc, but don't think this sheet
	   ;; is prepared.
	   (PREPARE-SHEET (SELF))
	   (SETQ PREPARED-SHEET NIL)
	   (LET ((ARRAY (IF SUPERIOR
			    (SHEET-SUPERIOR-SCREEN-ARRAY)
			  (SCREEN-BUFFER SELF))))
	     (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-ELEMENT-TYPE SCREEN-ARRAY)
			     (ARRAY-DIMENSION ARRAY 1)
			     (ARRAY-DIMENSION SCREEN-ARRAY 0)
			     ARRAY
			     (+ X-OFFSET (* Y-OFFSET
					    (ARRAY-DIMENSION ARRAY 1))))))
	  (SUPERIOR-HAS-SCREEN-ARRAY
	   (SETQ SCREEN-ARRAY OLD-SCREEN-ARRAY)
	   (SETF (SHEET-OUTPUT-HOLD-FLAG) 0)))
    (when (and SUPERIOR-HAS-SCREEN-ARRAY (mac-window-p self))
      (redirect-drawing-of-window-and-inferiors self))
    (COND ((AND SUPERIOR-HAS-SCREEN-ARRAY (SHEET-TEMPORARY-P))
	   (IF (EQ TEMPORARY-BIT-ARRAY T)
	       (SETQ TEMPORARY-BIT-ARRAY
		     (MAKE-ARRAY `(,HEIGHT ,(LOGAND #o-40 (+ #o37 WIDTH)))
				 :TYPE (SHEET-ARRAY-TYPE SELF)))
	     (PAGE-IN-PIXEL-ARRAY
	       TEMPORARY-BIT-ARRAY NIL (LIST WIDTH HEIGHT)))
	   (BITBLT ALU-SETA WIDTH HEIGHT
		   SCREEN-ARRAY 0 0
		   TEMPORARY-BIT-ARRAY 0 0)
	   (PAGE-OUT-PIXEL-ARRAY
	     TEMPORARY-BIT-ARRAY NIL (LIST WIDTH HEIGHT))))
    (DOLIST (SHEET *SHEETS-MADE-INVISIBLE-TO-MOUSE*)
      (SETF (SHEET-INVISIBLE-TO-MOUSE-P SHEET) NIL))
    (SETQ *SHEETS-MADE-INVISIBLE-TO-MOUSE* NIL)
    (MOUSE-DISCARD-CLICKAHEAD)
    (MOUSE-WAKEUP)
    ;; This goes after preceeding code so that blinkers won't accidentally
    ;; turn on before the bits get BITBLT'ed into the temporary array.
    (SETQ INHIBIT-SCHEDULING-FLAG OLD-INHIBIT-SCHEDULING-FLAG)
    (COND (superior-has-screen-array
	    ;; CJJ 09/20/88.  Make sure all relevant bits are affected...
	   ;; Added for Multiple Monitor (MMON) support. 09/28/88 KJF
	   (LET* ((original-plane-mask plane-mask)
		  (modify-plane-mask-p (AND (NOT (EQL original-plane-mask *default-plane-mask*))
					    (color-sheet-p self))))
	     ;; Added unwind-protect - MMON 09/28/88 KJF
	     (UNWIND-PROTECT
		 (PROGN
		   ;; Added for Multiple Monitor (MMON) support. 09/28/88 KJF
		   (WHEN modify-plane-mask-p
		     (SEND self :set-plane-mask (sheet-plane-mask (sheet-get-screen self))))
		   (CASE bits-action
		     (:noop nil)
		     (:restore
		      (SEND self :refresh :use-old-bits))
		     (:clean
		      (sheet-home self)
		      (SEND self :refresh :complete-redisplay))
		     (otherwise
		      (FERROR nil "Unknown BITS-ACTION ~S" bits-action))))
	       ;; but always restore the plane-mask...
	       ;; Clean-up form for unwind-protect - MMON 09/28/88 KJF
	       (WHEN modify-plane-mask-p
		 (SEND self :set-plane-mask original-plane-mask))))
	   (OR inhibit-blinkers
	       (deselect-sheet-blinkers self))
	   (OR bit-array
	       ;; Expose in opposite order for the sake of temporary windows
	       (DOLIST (inferior (REVERSE exposed-inferiors))
		 (FUNCALL inferior :expose nil)))
	   (RETURN t)))))

(DEFUN SHEET-DEEXPOSE (DAEMON-ARGS INTERNALS)
  (DECLARE (:SELF-FLAVOR SHEET))
  (IF (OR (NULL SUPERIOR)
	  (TYPEP SUPERIOR 'SCREEN)
	  (FUNCALL SUPERIOR :INFERIOR-DEEXPOSE SELF))
      (UNWIND-PROTECT
	(PROGN
	  ;; Always make ourselves invisible to the mouse.
	  (SETF (SHEET-INVISIBLE-TO-MOUSE-P SELF) T)
	  (LET ((INHIBIT-SCHEDULING-FLAG T))
	    (COND ((SHEET-ME-OR-MY-KID-P MOUSE-SHEET SELF)
		   ;; The mouse is currently on me or one of my
                   ;; inferiors, get it out of there.
		   (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		   (IF SUPERIOR
		       (MOUSE-SET-SHEET SUPERIOR)
		       (IF (NEQ SELF DEFAULT-SCREEN)
			   (MOUSE-SET-SHEET DEFAULT-SCREEN)
			   (FERROR NIL
	"Attempt to deexpose sheet ~S, which is top level sheet that owns mouse"
				   SELF)))
		   (SETQ INHIBIT-SCHEDULING-FLAG T)))
	    (COND ((AND (TYPEP MOUSE-WINDOW 'SHEET)
                        (SHEET-ME-OR-MY-KID-P MOUSE-WINDOW SELF))
		   ;; Me or my inferior is the current mouse sheet, so
                   ;; force it out.
		   (SETQ MOUSE-RECONSIDER T)
		   (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		   (PROCESS-WAIT
                     "Mouse Out"
                     #'(LAMBDA (SHEET)
                         (OR (NOT (TYPEP MOUSE-WINDOW 'SHEET))
                             (NOT (SHEET-ME-OR-MY-KID-P MOUSE-WINDOW SHEET))))
                     SELF))))
	  (LOCK-SHEET (SELF)
	    (FUNCALL INTERNALS DAEMON-ARGS)))
	(SETF (SHEET-INVISIBLE-TO-MOUSE-P SELF) NIL))))

(DEFWRAPPER (SHEET :DEEXPOSE) (IGNORE . BODY)
  `(SHEET-DEEXPOSE
     SI:.DAEMON-CALLER-ARGS.
     #'(LAMBDA (SI:.DAEMON-CALLER-ARGS.
                &AUX FOO (SI:.DAEMON-MAPPING-TABLE. SYS:SELF-MAPPING-TABLE))
         ;; Local slot 1 must contain our mapping table
         ;; in something that contains combined method code.
         FOO
         ;; Make sure we are marked as requiring SELF-MAPPING-TABLE
         ;; to be recomputed, because SHEET-DEEXPOSE set it to SHEET's.
         SCREEN-ARRAY
         SI:.DAEMON-MAPPING-TABLE.
         . ,BODY)))

(DEFMETHOD (SHEET :DEEXPOSE)
           (&OPTIONAL (SAVE-BITS-P :DEFAULT) SCREEN-BITS-ACTION
            (REMOVE-FROM-SUPERIOR T))
  "Deexpose a sheet (removing it virtually from the physical screen,
some bits may remain)"
  (DELAYING-SCREEN-MANAGEMENT
    (COND ((AND (EQ SAVE-BITS-P :DEFAULT)
                (NOT (ZEROP (SHEET-FORCE-SAVE-BITS))) EXPOSED-P)
	   (SETQ SAVE-BITS-P :FORCE)
	   (SETF (SHEET-FORCE-SAVE-BITS) 0)))
    (LET ((SW SELECTED-WINDOW))
      (AND SW (SHEET-ME-OR-MY-KID-P SW SELF)
	   (FUNCALL SW :DESELECT NIL)))
    (OR SCREEN-BITS-ACTION (SETQ SCREEN-BITS-ACTION :NOOP))
    (COND (EXPOSED-P
	   (OR BIT-ARRAY
               ;; We do not have a bit-array, take our inferiors off screen.
	       (EQ SAVE-BITS-P :FORCE)	;but leave them in EXPOSED-INFERIORS
	       (DOLIST (INFERIOR EXPOSED-INFERIORS)
		 (FUNCALL INFERIOR :DEEXPOSE SAVE-BITS-P :NOOP NIL)))
	   (WITHOUT-INTERRUPTS
	     (WHEN (AND (EQ SAVE-BITS-P :FORCE)
			(NULL BIT-ARRAY))
	         ;; We are to force a saving of the SCREEN-ARRAY and
	         ;; there isn't a BIT-ARRAY.  We must create a BIT-ARRAY.
	       (SETF BIT-ARRAY (MAKE-ARRAY
				 `(,HEIGHT
				   ,(LOGAND (+ (TRUNCATE (* LOCATIONS-PER-LINE 32.)
							 (SCREEN-BITS-PER-PIXEL
							   (SHEET-GET-SCREEN SELF)))
					       #o37)
					    #o-40))
				 :TYPE (SHEET-ARRAY-TYPE SELF)))
	       (SETQ OLD-SCREEN-ARRAY NIL)
	       (when (mac-system-p)
		    (send-adjust-bit-array-maybe self)))
	     (PREPARE-SHEET (SELF)
	       (AND SAVE-BITS-P BIT-ARRAY
		    (PROGN
                      (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT))
                      (BITBLT ALU-SETA WIDTH HEIGHT
                              SCREEN-ARRAY 0 0
                              BIT-ARRAY    0 0)
                      (PAGE-OUT-PIXEL-ARRAY BIT-ARRAY NIL
                                            (LIST WIDTH HEIGHT)))))
	     (COND ((sheet-temporary-p)
		    (page-in-pixel-array temporary-bit-array nil
                                         (LIST width height))
		    ;; CJJ 09/20/88.  Make sure all relevant bits are affected...
		    ;; Added for Multiple Monitor (MMON) support. 09/28/88 KJF
		    (LET* ((original-plane-mask plane-mask)
			   (modify-plane-mask-p (AND (NOT (EQL original-plane-mask *default-plane-mask*))
						     (color-sheet-p self))))
		      ;; Added unwind-protect - MMON 09/28/88 KJF
		      (UNWIND-PROTECT
			  (PROGN
			    ;; Added for Multiple Monitor (MMON) support. 09/28/88 KJF
			    (WHEN modify-plane-mask-p
			      (SEND self :set-plane-mask (sheet-plane-mask (sheet-get-screen self))))
			    (BITBLT alu-seta width height
				    temporary-bit-array 0 0
				    screen-array        0 0))
			;; but always restore the plane-mask...
			;; Clean-up form for unwind-protect - MMON 09/28/88 KJF
			(WHEN modify-plane-mask-p
			  (SEND self :set-plane-mask original-plane-mask))))
		    (page-out-pixel-array temporary-bit-array nil
                                          (LIST width height))
		    (DOLIST (sheet temporary-windows-locked)
		      (sheet-release-temporary-lock sheet self))
		    (SETQ temporary-windows-locked nil))
		   (t
		    (CASE screen-bits-action
		      (:noop)
		      (:clean
		       (prepare-sheet (self) ;; may 7-1-88 added prepare-sheet
			 ;; CJJ 09/20/88.  Make sure all relevant bits are affected...
			 ;; Added for Multiple Monitor (MMON) support. 09/28/88 KJF
			 (LET* ((original-plane-mask plane-mask)
				(modify-plane-mask-p (AND (NOT (EQL original-plane-mask *default-plane-mask*))
							  (color-sheet-p self))))
			   ;; Added unwind-protect - MMON 09/28/88 KJF
			   (UNWIND-PROTECT
			       (PROGN
				 ;; Added for Multiple Monitor (MMON) support. 09/28/88 KJF
				 (WHEN modify-plane-mask-p
				   (SEND self :set-plane-mask (sheet-plane-mask (sheet-get-screen self))))
				 ;;>>> alu-andca changed to erase-aluf
				 (%draw-rectangle width height 0 0 (sheet-erase-aluf self) self))
			     ;; but always restore the plane-mask...
			     ;; Clean-up form for unwind-protect - MMON 09/28/88 KJF
			     (WHEN modify-plane-mask-p
			       (SEND self :set-plane-mask original-plane-mask))))))
		      (otherwise
		       (FERROR
                         nil
                         "~S is not a valid bit action" screen-bits-action)))))
	     (SETQ EXPOSED-P NIL)
	     (AND REMOVE-FROM-SUPERIOR SUPERIOR
		  (SETF (SHEET-EXPOSED-INFERIORS SUPERIOR)
			(DELETE SELF (THE LIST (SHEET-EXPOSED-INFERIORS SUPERIOR)) :TEST #'EQ)))
	     (IF (NULL BIT-ARRAY)
		 (SETQ OLD-SCREEN-ARRAY SCREEN-ARRAY SCREEN-ARRAY NIL)
		 (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-ELEMENT-TYPE BIT-ARRAY)
				 (ARRAY-DIMENSION BIT-ARRAY 1)
				 (ARRAY-DIMENSION BIT-ARRAY 0)
				 BIT-ARRAY 0))
	     (when (mac-window-p self)
	       (redirect-drawing-of-window-and-inferiors self))
	     (SETF (SHEET-OUTPUT-HOLD-FLAG) 1)))
	  (REMOVE-FROM-SUPERIOR
	   (AND SUPERIOR
		(SETF (SHEET-EXPOSED-INFERIORS SUPERIOR)
		      (DELETE SELF (THE LIST (SHEET-EXPOSED-INFERIORS SUPERIOR)) :TEST #'EQ)))))))

(DEFMETHOD (SHEET :REFRESH) (&OPTIONAL (TYPE :COMPLETE-REDISPLAY) &AUX PAGE-IN)
  (SETQ RESTORED-BITS-P (AND BIT-ARRAY (NEQ TYPE :COMPLETE-REDISPLAY)))
  (SETQ PAGE-IN (AND BIT-ARRAY (IF EXPOSED-P RESTORED-BITS-P
				 (OR (NEQ TYPE :USE-OLD-BITS)
				     (NOT RESTORED-BITS-P)))))
  (IF PAGE-IN (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT)))
  (COND (RESTORED-BITS-P
	  (AND EXPOSED-P	;If we are deexposed, this is a big no-op!
	       (PREPARE-SHEET (SELF)
	         (BITBLT ALU-SETA WIDTH HEIGHT BIT-ARRAY 0 0 SCREEN-ARRAY 0 0)))
	  (COND ((NEQ TYPE :USE-OLD-BITS)
		 (ERASE-MARGINS)
		 (SEND SELF :REFRESH-MARGINS))))
	(T
	 (PREPARE-SHEET (SELF)
	   (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 ERASE-ALUF SELF))
	 (SEND SELF :REFRESH-MARGINS)
	 (DOLIST (INFERIOR INFERIORS)
	   (AND 
             ;; EXPOSED-INFERIORS may not all be on screen.
             (SHEET-EXPOSED-P INFERIOR)
		(FUNCALL INFERIOR :REFRESH :COMPLETE-REDISPLAY)))
;	 ;;(SEND SELF :SCREEN-MANAGE)
	 (SCREEN-MANAGE-QUEUE SELF 0 0 WIDTH HEIGHT)))
  (AND PAGE-IN (PAGE-OUT-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT))))

;;; We have this method so we can tack on BEFORE/AFTER methods.
(DEFMETHOD (SHEET :REFRESH-MARGINS) () )


(DEFMETHOD (SHEET :HANDLE-EXCEPTIONS) ()
  (OR (ZEROP (SHEET-EXCEPTIONS)) (SHEET-HANDLE-EXCEPTIONS SELF)))

;;; Exceptions
(DEFUN SHEET-HANDLE-EXCEPTIONS (SHEET)
  "Handle any exception flags set in SHEET, including output-hold, **more**, end of page, etc.
This is usually called after testing that (SHEET-EXCEPTIONS SHEET) is nonzero
to save time in the normal case."
  (OR (ZEROP (SHEET-OUTPUT-HOLD-FLAG SHEET))
      (FUNCALL SHEET :OUTPUT-HOLD-EXCEPTION))
  (OR (ZEROP (SHEET-END-PAGE-FLAG SHEET))
      (FUNCALL SHEET :END-OF-PAGE-EXCEPTION))
  (OR (ZEROP (SHEET-MORE-FLAG SHEET))
      (COND (MORE-PROCESSING-GLOBAL-ENABLE
	     (FUNCALL SHEET :MORE-EXCEPTION)
	     (OR (ZEROP (SHEET-END-PAGE-FLAG SHEET))
		 (FUNCALL SHEET :END-OF-PAGE-EXCEPTION)))
            ;; Ignore MORE processing if it wasn't enabled.
	    (T (SETF (SHEET-MORE-FLAG SHEET) 0))))
  (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
      (FERROR NIL "Exceptions (~O) on sheet ~S won't go away"
	      (SHEET-EXCEPTIONS SHEET)
	      SHEET))
  NIL)

;;; Called by typeout routines when they discover there is not enough
;;; space to output another character.  Sheet has already been prepared
;;; when this is called.
(DEFMETHOD (SHEET :END-OF-LINE-EXCEPTION) ()
  "Put an ! in the right margin if called for."
  (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG))
      (SEND SELF :TYO-RIGHT-MARGIN-CHARACTER CURSOR-X CURSOR-Y #\!))
  ;; Move to left margin, next line, and clear it
  (SHEET-INCREMENT-BITPOS SELF (- CURSOR-X) LINE-HEIGHT)
  (SHEET-CLEAR-EOL SELF)	     ;If at end of page, this will home up first
  (OR (ZEROP (SHEET-EXCEPTIONS))     ;Take care of any residual **more**
      (SHEET-HANDLE-EXCEPTIONS SELF)))	;since caller is about to type out

(DEFMETHOD (SHEET :TYO-RIGHT-MARGIN-CHARACTER)
	   (XPOS YPOS CH
	    &AUX (FONT (AREF FONT-MAP 0))
	    (WID (SHEET-CHARACTER-WIDTH SELF CH FONT)))
  "This used to put continuation-line marks in the margin.  Note that
when using variable-width fonts, the mark is placed relative to the
right margin rather than relative to the text that is already there.
Hope this is right."
  ;; XPOS is ignored now, but supplied in case I decide to change where
  ;; the character goes.
  XPOS
  (PREPARE-SHEET (SELF)
    (DRAW-CHAR FONT CH (- (SHEET-INSIDE-RIGHT) WID) YPOS CHAR-ALUF SELF)))

(DEFMETHOD (SHEET :END-OF-PAGE-EXCEPTION) ()
  (COND ((NOT (ZEROP (SHEET-END-PAGE-FLAG)))
	 (LET ((M-VP MORE-VPOS)	;SHEET-HOME smashes this,
	       (m-flg (sheet-more-flag)))	;;SHEET-HOME smashes this, may 10-18-88
	   ;;                                         ; since it moves the cursor.
	   ;; Wrap around to top of sheet
	   (SHEET-HOME SELF)
	   (SHEET-CLEAR-EOL SELF)
	   ;; Arrange for more processing next time around
	   (setf (sheet-more-flag) m-flg)	;;restore clobbered flag. may 10-18-88
	   (COND ((NULL M-VP))			;;No more processing at all
		 ((>= M-VP #o100000)		;;More processing delayed?
		  (SETQ MORE-VPOS (- M-VP #o100000)))	;Cause to happen next time around.
		 (T (SETQ MORE-VPOS (SHEET-DEDUCE-MORE-VPOS SELF))))))))


(DEFUN SHEET-MORE-HANDLER
       (&OPTIONAL (OPERATION :TYI)
        (MORE-STRING *UNIDIRECTIONAL-MORE-STANDARD-MESSAGE*)
        &AUX (CURRENT-X CURSOR-X) CHAR)
  "This function is the default way of handing **MORE** exceptions.
It outputs **MORE**, performs OPERATION (by default :TYI) to read a
character, then erases the **MORE** and returns the character read.
Untyi'ing the character or flushing future output and typing
**FLUSHED** is the caller's responsibility.

On return, the cursor points at the now-blank line which once held
**MORE**.  Note that, if this line is the last line in the window, the
end-of-page exception will be set, so ordinarily the next output will
happen on the top line.

Output is done with the SHEET- functions, not by sending messages.
It is sometimes useful that the output does not do any special things
that the normal output operations on SELF would do."
  (DECLARE (:SELF-FLAVOR SHEET))
  (SETF (SHEET-MORE-FLAG) 0)		;"Won't need MORE processing no more"
  (SETQ MORE-VPOS (+ #o100000 MORE-VPOS))	;Defer more's while typing **MORE**
  (SHEET-CLEAR-EOL SELF)
  (LET ((OLD-FONT CURRENT-FONT)
	(OLD-CHAR-WIDTH CHAR-WIDTH)
	(old-foreground (sheet-foreground-color self)))
    (AND MORE-STRING
	 (UNWIND-PROTECT
	   (PROGN
	     (SETQ CURRENT-FONT (AREF FONT-MAP 0))
	     (SETQ CHAR-WIDTH (FONT-CHAR-WIDTH CURRENT-FONT))
	     (SEND self :set-foreground-color red)
	     (send self :STRING-OUT-explicit MORE-STRING cursor-x cursor-y
                                        9999. (sheet-inside-width self)
                                        *MORE-PROCESSING-STANDARD-FONT*
                                        char-aluf))
	   (SETQ CURRENT-FONT OLD-FONT)
	   (SETQ CHAR-WIDTH OLD-CHAR-WIDTH)
	   (SEND self :set-foreground-color old-foreground))))

  (AND (GET-HANDLER-FOR SELF OPERATION)
       (SETQ CHAR (SHEET-MORE-LOCK-KLUDGE SELF OPERATION)))

  (COND (MORE-STRING
	 (SETQ CURSOR-X CURRENT-X)		;Wipe out the **MORE**
	 (SHEET-CLEAR-EOL SELF)))
  (COND ((>= (+ CURSOR-Y LINE-HEIGHT)
	    (+ TOP-MARGIN-SIZE
               (1- (* (1- (SHEET-NUMBER-OF-INSIDE-LINES)) LINE-HEIGHT))))
	 (IF (NOT (NULL MORE-VPOS))    ;Might have been disabled while waiting!!
	     (SETQ MORE-VPOS 0))
	 (SETF (SHEET-END-PAGE-FLAG) 1))	;Wrap around unless flushed
	;At bottom, wrap around (or scroll)
	;Next MORE will happen at same place
	(T
         ;; Otherwise, MORE one line up next time.
         (SEND SELF :NOTICE :INPUT-WAIT)))
  CHAR)

(sys:declare-suggestions-for '(:METHOD tv:SHEET :MORE-EXCEPTION)
			 :around (sys:make-default-around-form
				   (:METHOD TV:SHEET :MORE-EXCEPTION)
				   nil nil default-more-menu
				   :pop-menus-form
				   (sys:special-pop-menus '(:METHOD TV:SHEET :MORE-EXCEPTION)
						      nil nil 'default-more-menu)))

(DEFMETHOD (SHEET :MORE-EXCEPTION) ()
  (sys:with-suggestions-menus-for (:method tv:sheet :more-exception)
  (WHEN (OR (ZEROP (SHEET-MORE-FLAG)) MORE-PROCESSING-GLOBAL-ENABLE)
    (SHEET-MORE-HANDLER))))

(DEFMETHOD (SHEET :OUTPUT-HOLD-EXCEPTION) ()
  (OR (ZEROP (SHEET-OUTPUT-HOLD-FLAG))
      EXPOSED-P				;Output held due to deexposure
      (CASE DEEXPOSED-TYPEOUT-ACTION
	(:NORMAL)
	(:ERROR				;Give error if attempting typeout?
	  (FERROR 'OUTPUT-ON-DEEXPOSED-SHEET
		  "Attempt to typeout on ~S, which is deexposed"
		  SELF))
	(:PERMIT
	 ;; OUTPUT-HOLD gets cleared at this level, rather than never
         ;; getting set when deexposing, so that software knows if a
         ;; sheet actually did typeout, as opposed to it being
         ;; permitted.  This allows software to know if it needs to
         ;; update a partially exposed window's bits, for example.  It
         ;; is similar to a page-fault handler's setting the
         ;; write-protect bit on write enabled pages to detect when a
         ;; page is actually modified (READ-WRITE-FIRST).
	 (AND SCREEN-ARRAY (SETF (SHEET-OUTPUT-HOLD-FLAG) 0)))
	(:EXPOSE
	 (SEND SELF :EXPOSE))
	(:NOTIFY
	 (SEND SELF :NOTICE :OUTPUT))	;Consider notifying the user
	(OTHERWISE
	 (IF (CONSP DEEXPOSED-TYPEOUT-ACTION)
	     (LEXPR-SEND SELF DEEXPOSED-TYPEOUT-ACTION)
	     (FERROR NIL "~S is not a recognized DEEXPOSED-TYPEOUT-ACTION"
		     DEEXPOSED-TYPEOUT-ACTION)))))
  ;; Wait until no output hold.
  (PROCESS-WAIT "Output Hold"
		#'(LAMBDA (SHEET)
		    (NOT (SHEET-OUTPUT-HELD-P SHEET)))
		SELF))

;;; This is the default method for :NOTICE, which is always called last
;;; if all other methods have returned NIL.  It provides the default
;;; handling for deexposed input and output in notify mode, handles
;;; :INPUT-WAIT, and provides the special handling for errors vis a vis
;;; the window system.  Other events are completely ignored; presumably
;;; they shouldn't be noticed by windows which don't have flavors to
;;; handle them.  No currently-defined events use the ARGS argument, but
;;; it is there for future extensibility.
(DEFMETHOD (SHEET :NOTICE) (EVENT &REST ARGS)
  ARGS ;ignored
  (CASE EVENT
    ((:INPUT :OUTPUT)		;Deexposed window needs some attention
     ;; Wait for there to be a place to notify
     (PROCESS-WAIT "A Selected Window" #'(LAMBDA () SELECTED-WINDOW))
     ;; Now, if this window is visible we don't need to bother notifying
     (OR (LOOP FOR W = SELF THEN (SHEET-SUPERIOR W) UNTIL (NULL W)
	       ALWAYS (SHEET-EXPOSED-P W))
	 (NOTIFY SELF "Process ~A wants ~A" (PROCESS-NAME CURRENT-PROCESS)
		 (IF (EQ EVENT :OUTPUT) "to type out" "typein")))
     T)
    (:INPUT-WAIT			;Hanging up waiting for input.
     (SETF (SHEET-MORE-FLAG) 0)		;Decide when we need to **more** next
     (COND ((NULL MORE-VPOS))		;Unless MORE inhibited entirely
           ;; More than 3/4 way down window?
	   ((< (* (- (SHEET-INSIDE-BOTTOM) CURSOR-Y) 4)
	       (SHEET-INSIDE-HEIGHT))
	    ;; Wrap around and more just before the current line.
	    (SETQ MORE-VPOS (+ #o100000 (- CURSOR-Y LINE-HEIGHT))))
	   (T ;; More at bottom.
	    (SETQ MORE-VPOS (SHEET-DEDUCE-MORE-VPOS SELF))))
     (AND (NOT EXPOSED-P)			;Send a notification if desired.
	  (NOT (ZEROP (SHEET-DEEXPOSED-TYPEIN-NOTIFY)))
	  (SEND SELF :NOTICE :INPUT))
     T)
    (:ERROR		;Error in process using this window as its *TERMINAL-IO*.
			;Notify if possible, and decide whether to use this
			;window or the cold-load stream.
     ;; Value is COLD-LOAD-STREAM to tell debugger to use that, or
     ;; TOO-SMALL meaning we altered *TERMINAL-IO* because we didn't think
     ;; the user would want to use it, or NIL.  If NIL and *TERMINAL-IO*
     ;; has changed, the debugger should propagate the change to the
     ;; Stack Group that bombed.
     (IF (OR (< (SHEET-INSIDE-WIDTH)  (* CHAR-WIDTH 35.))
	     (< (SHEET-INSIDE-HEIGHT) (* LINE-HEIGHT 5)))
	 ;; If window absurdly small, don't use it.
	 ;; Get a background window instead.
	 (IF (TYPEP SELF 'BACKGROUND-LISP-INTERACTOR)
	     'SYS:COLD-LOAD-STREAM
	   (SETQ *TERMINAL-IO* DEFAULT-BACKGROUND-STREAM)
	   (OR (APPLY *TERMINAL-IO* :NOTICE EVENT ARGS)
	       'TOO-SMALL))
       (IF (LOOP FOR W = SELF THEN (SHEET-SUPERIOR W) UNTIL (NULL W)
		 ALWAYS (SHEET-EXPOSED-P W))
	   ;; If window is visible, go ahead and use it.
	   (WAIT-TILL-SAFE-FOR-ERROR SELF 'SHEET-CAN-GET-LOCK SELF)
	 ;; Otherwise must notify.
	 (OR (LET ((PROCESS-IS-IN-ERROR SELF))
               (DECLARE (SPECIAL PROCESS-IS-IN-ERROR))
	       (WAIT-TILL-SAFE-FOR-ERROR SELF 'NOTIFY-POSSIBLE-P SELF))
	     (PROGN
	       (NOTIFY SELF "Process ~A got an error"
                       (PROCESS-NAME CURRENT-PROCESS))
	       ;; If notifying for an error, remain "in error" until selected
	       (LET ((PROCESS-IS-IN-ERROR SELF))
		 (PROCESS-WAIT
                   "Selected" #'(LAMBDA (W) (EQ SELECTED-WINDOW W)) SELF)
		 NIL))))))
    (OTHERWISE NIL)))	;Ignore unknown events (could signal error instead?)

;;; List of windows waiting for window system locks because they want to
;;; enter the error handler.  Looked at by mouse process.
(DEFVAR LOCKED-ERROR-WINDOWS NIL)

(DEFVAR *WINDOWS-LOCKED-ERROR-QUERY* T
  "T means ask (in cold load stream) what to do about background
error with windows locked.
NIL means just wait for user to type Terminal Call or Terminal
Ctl-Clear-Input.")

(DEFUN WAIT-TILL-SAFE-FOR-ERROR (WINDOW FUNCTION &REST ARGS)
  "Wait until either (APPLY FUNCTION ARGS) is non-NIL or user does
Terminal Call.  If user does Terminal Call (and picks WINDOW therein),
returns the symbol COLD-LOAD-STREAM; if FUNCTION returns non-NIL,
we return NIL.  If *WINDOWS-LOCKED-ERROR-QUERY* is non-NIL, we
immediately ask user to choose to use cold load stream, unlock window
locks, or do nothing.  Do nothing means we wait, as described above."
  (DECLARE (SPECIAL SYS:COLD-LOAD-STREAM SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD))
  (COND ((NOT (APPLY FUNCTION ARGS))
	 (UNWIND-PROTECT
	   (PROGN
	     (WITHOUT-INTERRUPTS (PUSH WINDOW LOCKED-ERROR-WINDOWS))
	     (SHEET-FREE-TEMPORARY-LOCKS WINDOW)
	     (IF *WINDOWS-LOCKED-ERROR-QUERY*
		 ;; Situation has not been resolved yet; query user in
                 ;; cold-load stream.
		 (LET (ANSWER)
		   (LET-GLOBALLY ((SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD T))
		     (LET ((*QUERY-IO* SYS:COLD-LOAD-STREAM))
		       (FUNCALL SYS:COLD-LOAD-STREAM :CLEAR-INPUT)
		       (EH:SAVE-SCREEN-FOR-COLD-LOAD-STREAM)
		       (SETQ ANSWER
			     (FQUERY
                               '(:CHOICES (((:C "Cold load stream") #\C)
                                           ((:U "Clear all locks") #\U)
                                           ((:N "Nothing now") #\N)))
                               "How do you want to handle error in process ~A?
You can handle it in the error handler by typing
        C  to use the cold-load stream (like Terminal Call),
        U  to forcibly unlock all windows so a notification can come out 
           (like Terminal Control-Clear-input)
     or N  to tell it to wait until you do some other thing. "
				     (PROCESS-NAME CURRENT-PROCESS))))
		     (EH:RESTORE-SCREEN-FOR-COLD-LOAD-STREAM T))
		   (IF (EQ ANSWER :C)
		       ;; Answer C means use cold load stream now.
		       (WITHOUT-INTERRUPTS
			 (SETQ LOCKED-ERROR-WINDOWS
			       (DELETE WINDOW (THE LIST LOCKED-ERROR-WINDOWS) :COUNT 1 :TEST #'EQ))))
		   ;; Answer U means unlock locks, allowing notification now.
		   (IF (EQ ANSWER :U) (SHEET-CLEAR-LOCKS))))
	     ;; Wait until either the function supplied to us returns T
	     ;; or someone removes this window from the locked list
	     ;; (which means, telling us to use the cold load stream).
	     (PROCESS-WAIT
	       "Error notify"
	       #'(LAMBDA (FUNCTION ARGS WINDOW)
		   (OR (APPLY FUNCTION ARGS)
		       (NOT (MEMBER WINDOW LOCKED-ERROR-WINDOWS :TEST #'EQ))))
	       FUNCTION ARGS WINDOW)
	     (IF (NOT (MEMBER WINDOW LOCKED-ERROR-WINDOWS :TEST #'EQ))
		 'SYS:COLD-LOAD-STREAM))
	   (WITHOUT-INTERRUPTS (SETQ LOCKED-ERROR-WINDOWS
				     (DELETE WINDOW (THE LIST LOCKED-ERROR-WINDOWS) :COUNT 1 :TEST #'EQ)))))))

(DEFUN NOTIFY-POSSIBLE-P (WINDOW)
  "T if it is possible at this instant to print a notification about WINDOW."
  (LET ((SW SELECTED-WINDOW))
    (AND (SHEET-CAN-GET-LOCK WINDOW)
	 SW				;No one in charge
	 (NOT (SHEET-OUTPUT-HELD-P SW))	;Guy in charge locked or broken
	 (SHEET-CAN-GET-LOCK		;Anything locked, even by this process,
	   (SHEET-GET-SCREEN SW) T))))	;would hang Terminal 0 S.


;;; Code to check or set the keypad bit.
(DEFMETHOD (SHEET :KEYPAD-ENABLE) ()
  "Returns T when keypad is in application mode, NIL otherwise."
  (PLUSP (SHEET-KEYPAD-ENABLE)))

(DEFMETHOD (SHEET :SET-KEYPAD-ENABLE) (NEW-VALUE)
  "Set keypad for window to either application or normal mode.
NEW-VALUE can be either Non-NIL or 1 for application mode, or NIL or 0 for normal mode."
  (SETF (SHEET-KEYPAD-ENABLE) (IF (FIXNUMP NEW-VALUE)
                                  (IF (= NEW-VALUE 0) 0 1)
                                  ;;ELSE
                                  (IF NEW-VALUE 1 0))))

(defmethod (sheet :set-color-map) (new-map &optional (inherit nil))
  (when inherit
    (check-maps self new-map (sheet-color-map self))
  )
    (setq color-map new-map)
)

(defmethod (sheet :restore-default-colors)()
  (when color-map
    (copy-array-contents (color-map-table *default-color-map*)
			 (color-map-table color-map))
    (setf (color-map-clamp color-map) *first-color-location* )
    (setf (color-map-saturate color-map) *last-color-location*))
  (setf foreground-color *default-foreground*)
  (setf background-color *default-background*)
  (dolist (blinker blinker-list)
    (send blinker :send-if-handles :restore-default-colors))
  (install-color-map self))

(DEFUN check-maps (window new-map old-map) ;;; this works from the top of the heirarchy down
  (when (eq old-map (sheet-color-map window))
    (setf (sheet-color-map window) new-map) ;;; use this construct rather than :set-color-map so that we
                                            ;;; don't fire the daemon over and over
  )
  (dolist (inferior (sheet-inferiors window))  ;;; now do it recursively
    (check-maps inferior new-map old-map)
  )
)
