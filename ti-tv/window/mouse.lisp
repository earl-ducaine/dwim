;; -*- Mode:Common-Lisp; Package:TV; Fonts:(CPTFONT HL12B HL12BI); Base:10. -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; ------------------------------------------------------------------------------------
;;; 04/20/89  MAY	Disabled BITMAP-MOUSE and DRAW-BITMAP-MOUSE-CURSOR on microExplorer.
;;; 02/28/89  JLM       	[may]  Force mouse to be non-displayed if it is not owned.
;; 			This seems to be especially necessary when a cool-booted processor
;; 			has its keyboard reset AND does NOT go through tv:WARM-WINDOW-INITIALIZATIONS 
;; 			which changes the sib-owner which also tickles the mp:KBD-SWITCH-PROCESS process
;; 			into tuning off the mouse. Since a cool-booted processor SHOULD NOT grab the keyboard
;; 			then the mouse does not get turned off so we will INIT it to be off using 'stop.
;;; 01/30/89   KJF        [may] Set some global variables used for mouse transport in mouse-input.  For Multiple
;;;                          Monitor (MMON) support.  Also changes to MOUSE-DEFAULT-HANDLER,
;;;                           MOUSE-OVERSEER, (HYSTERETIC-WINDOW-MIXIN :HANDLE-MOUSE), MOUSE-SET-SHEET,
;;;                           and MOUSE-WARP. 
;;; 01/30/89   MAY      Added (eq scroll-bar-flag t) check in mouse-default-handler.  
;;; 09-13-88   MAY      Changed mouse-default-handler to fix case where mouse is on extreme left edge
;;;		         AND scroll-bar is :left AND scroll bar is being added dynamically.
;;; 09-07-88   MAY	Changed mouse-default-handler to fix case where mouse is on extreme left edge
;;;                         and keyboard gets starved since mouse-input never waits in a (sleep) to allow
;;;                         other processes in. Also fixed bug when scroll-bar on right side of menu.
;;;		         Fixed bug with mouse seized case. SPR 2850.
;;; 09/01/88   MAY       Moved defun of mouse-wakeup to tvdefs.lisp	
;;; 04/23/88   KJF        Change to :track-mouse of MOUSE-BLINKER-FAST-TRACKING-MIXIN to not allow
;;;                          microcode drawing for dual monitor situations, as well as for color sheets.
;;;  01/22/88   LG         Added support for Explorer mouse blinkers on the microExplorer.
;;;  09/01/87   PMH       Added color support
;;;  08/03/87   PMH       Changed merge-shift-keys to allow merging with mouse characters that are
;;;                         not in *mouse-incrementing-keystates*.
;;;   3/23/87  TWE	Changed the value of MOUSE-HANDEDNESS to be a symbol in the keyword package
;;;			to get around TV/W package problems.  This should have been done in the first place.
;;;   3/06/87  TWE	Removed references to %% symbols and replaced them with the appropriate
;;;			calls to kernel functions.
;;; 01/06/87   KDB        Adjusted default SCROLL-BAR-MAX-SPEED up to 10. Reduced probability of occurance
;;;                          of "Rebump, only slower" bug. Users can still move through scroll bar.
;;; 01/06/87   KDB        Adjusted the default SCROLL-BAR-RELUCTANCE value to 1. Fixed "prolonged bumping"
;;;                          bug. Scroll bar can now be more reliably invoked, per original design.
;;;                          Also reduced needless computation in MOUSE-DEFAULT-HANDLER.
;;; 01/06/87   KDB        Corrected documentation string for MOUSE-FAST-MOTION-CROSS-SIZE. It is the "radius"
;;;                          and not the "diameter" of the cross.
;;; 12/31/86   KDB        Changed EQUAL test of MOUSE-HANDEDNESS to EQ test in MOUSE-BUTTONS.
;;; 12/22/86   TWE	Made several changes to make the build process cleaner.
;;;			Moved the DEFVARs for MOUSE-BLINKER, MOUSE-HANDEDNESS, MOUSE-PROCESS and
;;;			MOUSE-CURSOR-CLOSED-STATE from here to TVDEFS.
;;; 12/18/86   TWE	Changed the extension in the BITMAP-MOUSE-PATHNAME to .BITMAP instead of .XLD.
;;;			This makes bitmap files more visible and less confusing.
;;; 12/05/86   TWE	Fixed load-bitmap-mouse to work correctly.
;;; 12/01/86   TWE	Removed the hack from the code which loads the mouse bitmap file.
;;; 11/26/86   TWE	Moved the mouse-process DEFVAR to the front to get rid of a compiler warning.  Also
;;;			put in a check to not to try to use the value of who-line-documentation-window when
;;;			it is not bound, or is NIL.
;;; 11/05/86   GRH	Retweaked Scroll-bar-max-speed, reluctance values.
;;; 10/30/86   TWE	Changed calls to DEFINE-BLINKER to use MAKE-BLINKER instead.
;;; 10/20/86   TWE	Changed MOUSE-SET-BLINKER to use TYPEP properly.
;;; 10/14/86   TWE	Changed MERGE-SHIFT-KEYS to use the Common Lisp CHAR-BIT function instead of
;;;			the %%KBD-xxx constants.
;;; 10/03/86   GRH	Adjusted the scroll-bar speed and reluctance variables.
;;; 09/12/86   GRH	Small change to Mouse-initialize for JO so temporal gc will work.
;;; 08/28/86   TWE	Fixed the hack done on 7/30/86 to use the mouse glyph constants instead of keywords.
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 08/11/86   TWE	Changed names which contained `godzilla' instead contain `bitmap,' which is more descriptive.
;;; 08/07/86   TWE	Fixed DRAW-MOUSE-FAST-MOTION-CURSOR to save the values of the mouse (x y)
;;;			registers before using them.  Not doing so causes their values to change sometimes.
;;; 08/05/86   TWE	Fixed MOUSE-DISCARD-CLICKAHEAD to only call MOUSE-BUTTONS only when the mouse
;;;			process is running.  This fixes a hard run loop problem.
;;; 07/30/86   TWE	Changed the uses of fixnums for mouse characters to use keywords instead.  This is
;;;			possible because of the implementation of keyword support for mouse characters.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.

#|
This file contains mouse blinkers, mouse tracking, and the mouse process.
Basic-scroll-bar and flashy-scrolling is now in obsolete.  -GRH 2-19-86
|#

#|
		Edit History

7/17/6	TWE	Added the Bitmap mouse.

4/7/6	GRH	Modified mouse-default-handler to handle scroll-bar on
		left OR right.  It previously assumed it was on the left.
		Also added a call to update the who-line doc window when
		entering a new handler.

4/16/86	DLS	Modified MOUSE-OVERSEER, MOUSE-DISCARD-CLICKAHEAD, and
		MOUSE-BUTTONS to fix some bugs related to losing mouse
		button transitions.

|#

;;; These variables are low-level and more or less private to this file
;;; The higher-level variables that have to do with the mouse are declared
;;; in TVDEFS.

(DEFVAR MOUSE-LAST-X 0
  "Previous value of MOUSE-X.  For measuring change in each step.")
(DEFVAR MOUSE-LAST-Y 0
  "Previous value of MOUSE-Y.  For measuring change in each step.")
(DEFVAR MOUSE-LAST-BUTTONS 0
  "Previous value of MOUSE-BUTTONS.  For measuring change in each step.")
(DEFVAR MOUSE-LAST-BUTTONS-TIME NIL
  "Value of FIXNUM-MICROSECOND-TIME when buttons last changed.")
(DEFVAR MOUSE-LAST-BUTTONS-X NIL
  "Value of MOUSE-X when last a button changed.")
(DEFVAR MOUSE-LAST-BUTTONS-Y NIL
  "Value of MOUSE-X when last a button changed.")
(DEFVAR MOUSE-BUTTONS-IN-PROGRESS NIL
  "If non-NIL, is next state for buttons to enter.")

(DEFVAR USE-KBD-BUTTONS T
  "T says interpret the roman keys (I, II, III) as mouse clicks.")
(DEFVAR KBD-BUTTONS 0) 			;buttons input via roman numerials on new keyboard.
					;These get IORed with buttons from the mouse.
(DEFPARAMETER MOUSE-BOUNCE-TIME 2000.
  "Delay in usec after button raised or lifted before we check for
another change.")
(DEFPARAMETER MOUSE-DOUBLE-CLICK-TIME 200000.
  "Delay in usec before deciding there has been no double click.")

;;;This is defined in SYS:WINDOW;TVDEFS.
;(DEFVAR MOUSE-RECONSIDER)		;T => mouse process should return to overseer
					;and decide anew which window should get the mouse.
					;For use by :MOUSE-MOVES methods, etc.
					;Declared in TVDEFS

;;; At the lowest level encountered by the software are two hardware
;;; registers whose values can be read via the %UNIBUS-READ function.  These
;;; two registers are MOUSE-REG1 and MOUSE-REG2.  The formatting of the
;;; MOUSE-REG1 register is:
;;;
;;;	  17 16 15 14 13 12 11 10  7  6  5  4  3  2  1  0
;;;	 .-----------------------------------------------.
;;;	 |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
;;;	 |  |        |        |        |        |        |
;;;	 |           |............Y position.............|
;;;	 |  |R |M |L |                                   |
;;;	 `-----------------------------------------------'
;;;
;;; Where the Y position value increases by 1 when the mouse is moved down
;;; and decreases by 1 when the mouse is moved up.  Two's complement
;;; arithmetic is used to handle the wrap-around.
;;;
;;; The letters R, M and L stand for Right Middle and Left, respectively.
;;; These bit positions indicate that a particular mouse button has been
;;; depressed.
;;;
;;; For the MOUSE-REG2 register, the encoding looks like:
;;;
;;;	  17 16 15 14 13 12 11 10  7  6  5  4  3  2  1  0
;;;	 .-----------------------------------------------.
;;;	 |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
;;;	 |  |        |        |        |        |        |
;;;	 |           |............X position.............|
;;;	 |  |  |  |  |                                   |
;;;	 `-----------------------------------------------'
;;;
;;; Where the X position value increases by 1 when the mouse is moved right,
;;; and decreases by 1 when the mouse is moved left.  Two's complement
;;; arithmetic is used to handle the wrap-around.
;;;
(DEFPARAMETER MOUSE-REG1 #o764104) 		;Unibus address of buttons and Y position
(DEFPARAMETER MOUSE-REG2 #o764106) 		;Unibus address of raw quadrature and X position

(PROCLAIM '(SPECIAL FONTS:MOUSE)) 		;New 1980's mouse characters

;;; Low-level routines

;;; MOUSE-INPUT blocks until the mouse status changes (it moves or a button
;;; is depressed or raised).  It then returns 6 values: delta-X, delta-Y,
;;; buttons-newly-pushed, buttons-newly-raised, and the relevant mouse X and Y
;;; (the current position normally, but if any buttons changed, the position then).
;;; There are 3 coordinate systems involved:
;;;  Table coordinates - physical motion of the mouse
;;;			 Speeds are expressed in terms of these.
;;;  Mouse coordinates - These are the scaled table coordinates.  Only deltas
;;;			 are valid here.
;;;  Screen coordinates - these are where the mouse-blinker appears on
;;;			 the TV screen; the same as mouse coordinates except
;;;			 for non-simple geometry caused by e.g. scroll bars.
;;;			 These appear in the variables MOUSE-X and MOUSE-Y
;;; Note that because of non-simple geometry, the deltas returned by MOUSE-INPUT
;;; are not necessarily equal to the change in MOUSE-X and MOUSE-Y.
(DEFPARAMETER MOUSE-LAST-BUTTONS-FROM-BUFFER 0)

(DEFUN MOUSE-INPUT (&OPTIONAL (WAIT-FLAG T))
  "Wait until mouse moves or a button goes up or down, then report what happened.
The values are the delta-x, the delta-y (amounts of mouse motion),
buttons-newly-pushed, buttons-newly-raised, and the relevant mouse X and Y (the
current position normally, but if any buttons changed, the position then)."
  ;; Await a change in hardware status from what it was last time
  (COND (WAIT-FLAG
         ;; This sleep makes it reasonable for the mouse process to be high priority.
         ;; It insures that moving the mouse does not lock out lower priority
         ;; processes for a long time.  This constant may want to be tweaked.  A
         ;; value of 1 (1/60 of a second) does not noticably affect mouse response.
         (PROCESS-SLEEP 1)
         (PROCESS-WAIT "MOUSE" #'(LAMBDA () (IF (mac-system-p)
						(lispm-or-mac-mouse-wakeup)
					      (OR MOUSE-WAKEUP MOUSE-RECONSIDER))))))
  ;; Set some global variables used for mouse transport.  CJJ 05/06/88.
  ;; Added by KJF for CJJ on 08/16/88 for Multiple Monitor (MMON) support.
  (SETF *unglitched-mouse-x-speed* mouse-x-speed
	*unglitched-mouse-y-speed* mouse-y-speed
	;; Remember previous mouse position for mouse transport along mouse trajectory.  CJJ 08/15/88.
	*previous-unglitched-mouse-x* *unglitched-mouse-x*
	*previous-unglitched-mouse-y* *unglitched-mouse-y*
	*unglitched-mouse-x* mouse-x
	*unglitched-mouse-y* mouse-y)
  ;; Clear wakeup flag unless there are buffered mouse button transitions, since we
  ;; might not read all of them before calling MOUSE-INPUT again.
  (SETQ MOUSE-WAKEUP (NOT (= MOUSE-BUTTONS-BUFFER-IN-INDEX MOUSE-BUTTONS-BUFFER-OUT-INDEX)))
  ;; Compute delta-X and delta-Y in screen coordinates
  (LET ((DELTA-X (- MOUSE-X MOUSE-LAST-X))
        (DELTA-Y (- MOUSE-Y MOUSE-LAST-Y))
        (GLITCH-X NIL) (GLITCH-Y NIL) NEW-BUTTONS CHANGED-BUTTONS)
    (INCF MOUSE-LAST-X DELTA-X)
    (INCF MOUSE-LAST-Y DELTA-Y)
    ;; Compute change in button status
    (MULTIPLE-VALUE-SETQ (NEW-BUTTONS MOUSE-LAST-BUTTONS-TIME
                          MOUSE-LAST-BUTTONS-X MOUSE-LAST-BUTTONS-Y)
      (MOUSE-BUTTONS))
    (SETQ CHANGED-BUTTONS (LOGXOR NEW-BUTTONS MOUSE-LAST-BUTTONS)
          MOUSE-LAST-BUTTONS NEW-BUTTONS)
    ;; Force blinker to stay within mouse-sheet.  If the mouse moves during this
    ;; computation, it will glitch back.  So we only SETQ the variables
    ;; if the mouse position actually needs to be changed, rather than using
    ;; MAX and MIN which would be more readable.
    (IF (> 0 MOUSE-X)
        (SETQ GLITCH-X 0))
    (IF (<= (SHEET-WIDTH MOUSE-SHEET) MOUSE-X)
        (SETQ GLITCH-X (1- (SHEET-WIDTH MOUSE-SHEET))))
    (IF (> 0 MOUSE-Y)
        (SETQ GLITCH-Y 0))
    (IF (<= (SHEET-HEIGHT MOUSE-SHEET) MOUSE-Y)
        (SETQ GLITCH-Y (1- (SHEET-HEIGHT MOUSE-SHEET))))
    ;; If mouse blinker needs to be glitched, do so
    (IF (OR GLITCH-X GLITCH-Y)
        (WITHOUT-INTERRUPTS
          (%OPEN-MOUSE-CURSOR)
          (IF GLITCH-X
              (SETQ MOUSE-LAST-X (SETQ MOUSE-X GLITCH-X)))
          (IF GLITCH-Y
              (SETQ MOUSE-LAST-Y (SETQ MOUSE-Y GLITCH-Y)))
          (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
                PREPARED-SHEET NIL)))
    (VALUES DELTA-X
            DELTA-Y
            (LOGAND NEW-BUTTONS CHANGED-BUTTONS)
            (BOOLE 2 NEW-BUTTONS CHANGED-BUTTONS) ;BOOLE 2 is ANDCA
            (IF (ZEROP CHANGED-BUTTONS) MOUSE-LAST-X MOUSE-LAST-BUTTONS-X)
            (IF (ZEROP CHANGED-BUTTONS) MOUSE-LAST-Y MOUSE-LAST-BUTTONS-Y))))


(DEFUN mouse-buttons (&optional (PEEK (NOT (EQ current-process mouse-process))))
  "Returns a fixnum with a 1 for each button currently held down, plus related info.
The remaining values are the time when that was the true state of the buttons,
and the X and Y coordinates of the mouse then.  PEEK means to look at the state
without pulling anything out of the buffer (processes other than the mouse
process use this)."
  (DECLARE (VALUES mouse-last-buttons mouse-last-buttons-time mouse-x mouse-y))
  (WHEN (mac-window-p mouse-sheet)
    (mac-consider-mouse))
  (LET ((tem mouse-buttons-buffer-out-index)
	hardware-buttons software-buttons
	mouse-button-key-state mouse-button-microsecond-time
	mouse-button-x-coordinate mouse-button-y-coordinate)
    (COND (mouse-buttons-in-progress
           (SETQ mouse-button-key-state	       (PROG1 mouse-buttons-in-progress
						      (SETQ mouse-buttons-in-progress nil))
                 mouse-button-microsecond-time mouse-last-buttons-time
                 mouse-button-x-coordinate     mouse-last-buttons-x
                 mouse-button-y-coordinate     mouse-last-buttons-y))
	  ((= tem mouse-buttons-buffer-in-index)
	   (SETQ mouse-button-key-state	       (LOGIOR (IF use-kbd-buttons kbd-buttons 0)
						       mouse-last-buttons-from-buffer)
		 mouse-button-microsecond-time (time:fixnum-microsecond-time)
		 mouse-button-x-coordinate     mouse-x
		 mouse-button-y-coordinate     mouse-y))
	  (t (WITHOUT-INTERRUPTS
	      (OR peek (SETQ mouse-buttons-buffer-out-index (REM (+ tem 4.) 32.)))
	      ;; Put the hardware bits in the software order ++++ NEED field specifiers here ++++
	      (SETQ hardware-buttons (AREF mouse-buttons-buffer (+ tem 3.))
		    software-buttons (DPB (LDB 1 hardware-buttons) #o0201
					  (DPB (LDB #o0201 hardware-buttons) 1.
					       hardware-buttons)))
	      (OR peek (SETQ mouse-last-buttons-from-buffer software-buttons))
	      (SETQ mouse-button-key-state	  (LOGIOR (IF use-kbd-buttons kbd-buttons 0)
							  software-buttons)
		    mouse-button-microsecond-time (AREF mouse-buttons-buffer tem)
		    mouse-button-x-coordinate     (AREF mouse-buttons-buffer (+ tem 1.))
		    mouse-button-y-coordinate     (AREF mouse-buttons-buffer (+ tem 2.))))))
    (VALUES (IF (EQ mouse-handedness :left)
		(change-mouse-to-left-hand mouse-button-key-state)
		mouse-button-key-state)
	    mouse-button-microsecond-time
	    mouse-button-x-coordinate
	    mouse-button-y-coordinate)))


(DEFUN CHAPARRAL-INSERT-MOUSE-BUTTONS
       (BUTTONS
        &OPTIONAL (X MOUSE-X) (Y MOUSE-Y) (BUTTON-TIME (TIME:FIXNUM-MICROSECOND-TIME))
        &AUX OLD-IN-INDEX)
  "Insert mouse buttons into the mouse-buttons-buffer.
BUTTONS	mouse buttons as from #\MOUSE-R-1.
X		x position of mouse.
Y		y position of mouse.
BUTTON-TIME	time buttons were sampled."
  (WITHOUT-INTERRUPTS
   ;; Translate 0-->4, 1-->2, 2-->1.  This is the form that the microcode
   ;; gets from the hardware.  For the software:  0 = left, 1 = middle, 2 = right;
   ;; for the hardware: 4 = left, 2 = middle, 1 = right.
   (SETQ BUTTONS (LSH 1 (- 2 (CHAR-MOUSE-BUTTON BUTTONS))))
   ;; Allocate some room in the buttons-buffer.
   (SETQ OLD-IN-INDEX MOUSE-BUTTONS-BUFFER-IN-INDEX
         MOUSE-BUTTONS-BUFFER-IN-INDEX
         (REM (+ MOUSE-BUTTONS-BUFFER-IN-INDEX 4) (LENGTH MOUSE-BUTTONS-BUFFER)))
   ;; Put the downstroke information into the MOUSE-BUTTONS-BUFFER.
   (SETF (AREF MOUSE-BUTTONS-BUFFER (+ OLD-IN-INDEX 3)) BUTTONS)
   (SETF (AREF MOUSE-BUTTONS-BUFFER (+ OLD-IN-INDEX 1)) X)
   (SETF (AREF MOUSE-BUTTONS-BUFFER (+ OLD-IN-INDEX 2)) Y)
   (SETF (AREF MOUSE-BUTTONS-BUFFER (+ OLD-IN-INDEX 0)) BUTTON-TIME)
   ;; Allocate some room in the buttons-buffer.
   (SETQ OLD-IN-INDEX MOUSE-BUTTONS-BUFFER-IN-INDEX
         MOUSE-BUTTONS-BUFFER-IN-INDEX
         (REM (+ MOUSE-BUTTONS-BUFFER-IN-INDEX 4) (LENGTH MOUSE-BUTTONS-BUFFER)))
   ;; Put the upstroke information into the MOUSE-BUTTONS-BUFFER.
   (SETF (AREF MOUSE-BUTTONS-BUFFER (+ OLD-IN-INDEX 3)) 0)
   (SETF (AREF MOUSE-BUTTONS-BUFFER (+ OLD-IN-INDEX 1)) X)
   (SETF (AREF MOUSE-BUTTONS-BUFFER (+ OLD-IN-INDEX 2)) Y)
   (SETF (AREF MOUSE-BUTTONS-BUFFER (+ OLD-IN-INDEX 0)) (1+ BUTTON-TIME))
   ;; Tell the mouse-input handler that there is some input ready.
    (SETQ MOUSE-WAKEUP T)))


(DEFUN MOUSE-DISCARD-CLICKAHEAD ()
  "Discard any complete mouse clicks already recorded by the microcode."
  (SETQ MOUSE-BUTTONS-IN-PROGRESS NIL)
  (DO () ((= MOUSE-BUTTONS-BUFFER-OUT-INDEX MOUSE-BUTTONS-BUFFER-IN-INDEX))
    (mouse-buttons nil)))


(DEFUN MOUSE-DEFER-BUTTONS (BU BD)
  "Defer a change in the mouse buttons, to be handled later.
It is added at the front of the buffer of clicks coming from the microcode.
BU is a mask of buttons that went up, and BD a mask of those that went down."
  (IF (OR (NOT (ZEROP BU)) (NOT (ZEROP BD)))
      (SETQ MOUSE-BUTTONS-IN-PROGRESS MOUSE-LAST-BUTTONS
            MOUSE-LAST-BUTTONS (LOGIOR BU
                                       (BOOLE 2 BD      ;BOOLE 2 is ANDCA
                                              MOUSE-LAST-BUTTONS)))))

(DEFPARAMETER *MOUSE-INCREMENTING-KEYSTATES* '(:CONTROL :SHIFT :HYPER)
  "If any key in this list is down, it turns a single click into a double.")

(compiler:make-obsolete make-mouse-char code-mouse-char)
(DEFUN MAKE-MOUSE-CHAR (BUTTON N-CLICKS)
  "Type is not currently changed."
  (DPB 1 %%KBD-MOUSE (DPB N-CLICKS %%KBD-MOUSE-N-CLICKS BUTTON)))

(DEFUN CHAR-MOUSE-P (CHAR)
  "Returns T for mouse characters, NIL otherwise."
  (CHAR-BIT CHAR :MOUSE))

(DEFUN MOUSE-CHARACTER-BUTTON-ENCODE (BD
				      &optional (PEEK (NOT (EQ current-process mouse-process)))
			    &AUX BUTTON MASK CH TIME
                            (NEW-BUTTONS MOUSE-LAST-BUTTONS)
                            (NEW-TIME MOUSE-LAST-BUTTONS-TIME))
  "Look at mouse button transitions and detect double clicks.
BD is a mask of buttons that went down on the initial transition;
it presumably came from MOUSE-INPUT.
The value is NIL if no button is pushed (BD is 0),
or 2000 + 8 N + B, where B is the bit number in the button word,
and N is one less than the number of clicks.
   Accepts a character or fixnum.  Returns a character."
  (SETQ CH
	(COND
	  ((>= (SETQ BUTTON (1- (HAULONG BD))) 0)  ; Pick a button that was just pushed
	   (SETQ MASK (LSH 1 BUTTON)
		 CH   (CODE-MOUSE-CHAR BUTTON)
		 TIME MOUSE-LAST-BUTTONS-TIME)
	   ;; Set non-incrementing bucky-bits
	   (DOLIST (shift '(:hyper :super :meta :control))
	     (WHEN (AND (KEY-STATE shift)
			(NOT (MEMBER shift *MOUSE-INCREMENTING-KEYSTATES*)))
	       (SETF (CHAR-BIT CH shift) 1)))
	   ;; Each incrementing key that is held down
	   ;; counts as an extra click in the number of clicks.
	   (DOLIST (KEY *MOUSE-INCREMENTING-KEYSTATES*)
	     (WHEN (KEY-STATE KEY)
               (INCF (CHAR-MOUSE-CLICKS CH))))
	   (IF (mac-window-p mouse-sheet)
	       (PROGN (CASE (analyze-last-button)
			(:double (mouse-buttons peek)
				 (mouse-buttons peek)
				 (MULTIPLE-VALUE-SETQ (mouse-last-buttons mouse-last-buttons-time)
				     (mouse-buttons peek))
				 (INCF (CHAR-MOUSE-CLICKS ch)))
			(:triple (DOTIMES (i 4) (mouse-buttons peek))
				 (MULTIPLE-VALUE-SETQ (mouse-last-buttons mouse-last-buttons-time)
				     (mouse-buttons peek))
				 (INCF (CHAR-MOUSE-CLICKS ch)) 2)
			(:otherwise (MULTIPLE-VALUE-SETQ (mouse-last-buttons mouse-last-buttons-time)
					(mouse-buttons peek))))
		      ch)
	   (PROG1
	     (LOOP NAMED MOUSE-CHARACTER-BUTTON-ENCODE	;Do forever (until guy's finger wears out)
		   UNLESS MOUSE-DOUBLE-CLICK-TIME
		   RETURN CH
		   DOING
		   ;; Ignore any clicking during the bounce delay
		   (LOOP DOING (MULTIPLE-VALUE-SETQ (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS peek))
			 UNTIL (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-BOUNCE-TIME)
			 FINALLY (SETQ TIME NEW-TIME))
		   ;; Look for button to be lifted, or for double-click timeout
		   (LOOP WHILE (LOGTEST MASK NEW-BUTTONS)
			 DO (MULTIPLE-VALUE-SETQ (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS peek))
			 WHEN (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-DOUBLE-CLICK-TIME)
			 ;; Timed-out with button still down
			 DO (RETURN-FROM MOUSE-CHARACTER-BUTTON-ENCODE ch)
			 FINALLY (SETQ TIME NEW-TIME))
		   ;; Button was lifted, do another bounce delay
		   (LOOP DOING (MULTIPLE-VALUE-SETQ (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS peek))
			 UNTIL (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-BOUNCE-TIME)
			 FINALLY (SETQ TIME NEW-TIME))
		   ;; Now watch for button to be pushed again
		   (LOOP UNTIL (LOGTEST MASK NEW-BUTTONS)
			 DO (MULTIPLE-VALUE-SETQ (NEW-BUTTONS NEW-TIME) (MOUSE-BUTTONS peek))
			 WHEN (> (TIME-DIFFERENCE NEW-TIME TIME) MOUSE-DOUBLE-CLICK-TIME)
			 ;; Timed-out with button still up
			 DO (RETURN-FROM MOUSE-CHARACTER-BUTTON-ENCODE CH)
			 FINALLY (PROGN
                                   ;; Count multiplicity of clicks.
                                   (SETF (CHAR-MOUSE-CLICKS CH) (INCF (CHAR-MOUSE-CLICKS CH)))
                                   (SETQ TIME NEW-TIME)))
		   ;; Continue scanning (for triple click)
		   )
	     (SETQ MOUSE-LAST-BUTTONS      NEW-BUTTONS
		   MOUSE-LAST-BUTTONS-TIME NEW-TIME))))))
  (IF (INTEGERP CH) (INT-CHAR CH) CH))

(DEFUN MOUSE-SPEED-HACK (&REST SPECS)
  "Specs are SCALE-FACTOR SPEED-BREAK SCALE-FACTOR SPEED-BREAK ... SCALE-FACTOR
Each SCALE-FACTOR applies to speeds up to the following SPEED-BREAK.
The last SCALE-FACTOR applies to all higher speeds.
Args of (.6 120 1 200 1.5 400 2.2 700 3.3) are standardly used.
These apply to both X and Y."
  (LOOP FOR (SCALE SPEED) ON SPECS BY 'CDDR
        FOR I FROM 0 BY 2
        DO (SETF (AREF MOUSE-X-SCALE-ARRAY I) (OR SPEED #o37777777))
           (SETF (AREF MOUSE-Y-SCALE-ARRAY I) (OR SPEED #o37777777))
           (SETF (AREF MOUSE-X-SCALE-ARRAY (1+ I)) (TRUNCATE (TRUNCATE (* 2 SCALE 1024.)) 3))
           (SETF (AREF MOUSE-Y-SCALE-ARRAY (1+ I)) (TRUNCATE (TRUNCATE (* 3 SCALE 1024.)) 5))))

;;; Code to handle the left handed mouse.
(DEFUN SETUP-MOUSE-LEFT-HANDED ()
  "Change the mouse handling to be normal for a left-handed person."
  (SETQ MOUSE-HANDEDNESS :LEFT))

(DEFUN SETUP-MOUSE-RIGHT-HANDED ()
  "Change the mouse handling to be normal for a right-handed person."
  (SETQ MOUSE-HANDEDNESS :RIGHT))

(DEFUN CHANGE-MOUSE-TO-LEFT-HAND (MOUSE-BUTTON)
  "Change the mouse handedness so that LEFT <--> RIGHT."
  ;; This function is translates left mouse clicks into right clicks,
  ;; and vice versa.
  (LOGIOR (LSH (LDB #o0001 MOUSE-BUTTON) 2)
          (LSH (LDB #o0101 MOUSE-BUTTON) 1)
          (LDB      #o0201 MOUSE-BUTTON)))

;;; Middle-level routines

;(DEFVAR MOUSE-WINDOW)			;Window controlling the mouse, NIL if none
;(DEFVAR WINDOW-OWNING-MOUSE)		;NIL, or window which has seized the mouse, or
;					;T if someone has seized the mouse and can't identify
;					;himself as any particular window,
;					;or STOP to make the mouse process do nothing.
;(DEFVAR MOUSE-X)			;X coordinate of MOUSE-BLINKER
;(DEFVAR MOUSE-Y)			;Y coordinate of MOUSE-BLINKER
;(DEFVAR MOUSE-SHEET NIL)		;Which sheet MOUSE-BLINKER lives on
(DEFVAR MOUSE-WARP NIL
  "Set to T whenever mouse is WARPED -- moved virtually by program control.")

(DEFUN MOUSE-STANDARD-BLINKER (&OPTIONAL (WINDOW (WINDOW-OWNING-MOUSE)))
  "Set the mouse blinker to the standard kind (with an up-left arrow.)"
  (AND (SYMBOLP WINDOW)
       (SETQ WINDOW MOUSE-SHEET))
  (SEND WINDOW :MOUSE-STANDARD-BLINKER))

(DEFMETHOD (SHEET :MOUSE-STANDARD-BLINKER) ()
  (SEND SUPERIOR :MOUSE-STANDARD-BLINKER))

(DEFMETHOD (SCREEN :MOUSE-STANDARD-BLINKER) ()
  "Change the mouse blinker to a NW arrow."
  (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 0 :ON
                                :SET-CHARACTER MOUSE-GLYPH-NORTH-WEST-ARROW
				'FONTS:MOUSE))

(DEFMETHOD (SCREEN :MOUSE-FONT) ()
  (SEND SELF :PARSE-FONT-DESCRIPTOR 'FONTS:MOUSE))

(DEFMETHOD (MOUSE-BLINKER-MIXIN :OFFSETS) ()
  (block ()
    (RETURN X-OFFSET Y-OFFSET)))

(DEFMETHOD (MOUSE-BLINKER-MIXIN :SET-OFFSETS) (X Y)
  (WITH-BLINKER-READY T
    (COND ((NULL VISIBILITY)                        ;Don't open if visibility NIL (especially the mouse cursor!)
           (SETQ X-OFFSET X
                 Y-OFFSET Y))
          ((OR (NEQ X-OFFSET X)                     ;Only blink if actually moving blinker
               (NEQ Y-OFFSET Y))
           (OPEN-BLINKER SELF)
           (SETQ X-OFFSET X
                 Y-OFFSET Y)
           (SETQ TIME-UNTIL-BLINK 0)))))

(DEFMETHOD (MOUSE-BLINKER-FAST-TRACKING-MIXIN :SET-OFFSETS) (X Y)
  (if (or (color-system-p sheet) (mac-window-p sheet))					; draw in lisp
      (WITH-BLINKER-READY T
	(COND ((NULL VISIBILITY)                        ;Don't open if visibility NIL (especially the mouse cursor!)
	       (SETQ X-OFFSET X
		     Y-OFFSET Y))
	      ((OR (NEQ X-OFFSET X)                     ;Only blink if actually moving blinker
		   (NEQ Y-OFFSET Y))
	       (OPEN-BLINKER SELF)
	       (SETQ X-OFFSET X
		     Y-OFFSET Y)
	       (SETQ TIME-UNTIL-BLINK 0))))
      (progn						; draw in ucode
	(SETQ X-OFFSET X
	      Y-OFFSET Y)
	(IF (EQ MOUSE-BLINKER SELF)
	    (WITHOUT-INTERRUPTS
	      (%OPEN-MOUSE-CURSOR)
	      (SETQ MOUSE-CURSOR-X-OFFSET X
		    MOUSE-CURSOR-Y-OFFSET Y
		    MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
		    PREPARED-SHEET NIL)))))
  )

(DEFMETHOD (MOUSE-BLINKER-MIXIN :TRACK-MOUSE) ()
  (WITHOUT-INTERRUPTS
    (%OPEN-MOUSE-CURSOR)
    (SETQ MOUSE-BLINKER SELF
          MOUSE-CURSOR-CLOSED-STATE 1           ; 1 -> not being drawn by microcode
          MOUSE-CURSOR-X-OFFSET X-OFFSET
          MOUSE-CURSOR-Y-OFFSET Y-OFFSET)))

;; >> If either on color-sheet or running in dual monitor mode, cannot draw with microcode.
;; Problem on dual monitors is that both screens are exposed and both are competing for H/W
;; registers.  When color sheet causes registers (plane-mask, foreground-color, etc.) to be loaded,
;; if microcode is drawing, it knows nothing about the registers and just draws.  Thus if a "special"
;; plane-mask (one other than 255), is set, LISP drawing must be used to enforce effect of plane-mask.
;; >> This fixes mouse turds problem when using dual monitors and mouse is on monochrome screen
;; and color screen is continuously being updated.
;; Previously just checked for (color-system-p sheet).
(DEFMETHOD (MOUSE-BLINKER-FAST-TRACKING-MIXIN :TRACK-MOUSE) ()
  (WITHOUT-INTERRUPTS
    (cond      (sib-is-csib
		;; Disable microcode mouse display for any CSIB.  Since it will leave
		;; turds if any of several csib registers ever change. - GRH 7/88
		;;(OR (color-sheet-p sheet) (dual-monitor-p (get-screen sheet)))
		(%OPEN-MOUSE-CURSOR)
		(SETQ MOUSE-BLINKER SELF
		      MOUSE-CURSOR-CLOSED-STATE 1	; 1 -> not being drawn by microcode
		      MOUSE-CURSOR-X-OFFSET X-OFFSET
		      MOUSE-CURSOR-Y-OFFSET Y-OFFSET))
	       ((Mac-window-p sheet)
		;; Turn LISPM mouse drawing off - Mac now doing all mouse tracking!! KED 7/13/87
		(SEND self :set-visibility nil)
		(%open-mouse-cursor) 
		(SETF mouse-blinker self 
		      mouse-cursor-closed-state 0
		      mouse-cursor-x-offset x-offset
		      mouse-cursor-y-offset y-offset))
	       (t
		(SEND SELF :SET-VISIBILITY NIL)	;We are not in charge of drawing anything
		(%OPEN-MOUSE-CURSOR)		;Get rid of old microcode cursor
		(SETQ MOUSE-BLINKER SELF
		      MOUSE-CURSOR-CLOSED-STATE 2	; 2 -> being drawn by microcode
		      MOUSE-CURSOR-X-OFFSET X-OFFSET
		      MOUSE-CURSOR-Y-OFFSET Y-OFFSET)
		(MULTIPLE-VALUE-SETQ (MOUSE-CURSOR-WIDTH MOUSE-CURSOR-HEIGHT) (SEND SELF :SIZE))
		(SETQ MOUSE-CURSOR-WIDTH (* MOUSE-CURSOR-WIDTH (SHEET-BITS-PER-PIXEL SHEET)))
		(COND ((OR (> MOUSE-CURSOR-HEIGHT *microcode-mouse-cursor-buffer-height*)
			   (> MOUSE-CURSOR-WIDTH  *microcode-mouse-cursor-buffer-width*))
		       (SETQ MOUSE-CURSOR-CLOSED-STATE 1)	;Oops, too big to use microcode tracking
		       (SEND SELF :SET-VISIBILITY T))
		      (T			;Draw self into microcode cursor
		       (if (mac-window-p sheet)
			   (si-bitblt alu-setz
					  *microcode-mouse-cursor-buffer-width*
					  *microcode-mouse-cursor-buffer-height*
					  mouse-cursor-pattern 0 0 mouse-cursor-pattern 0 0)
			   (BITBLT alu-setz *microcode-mouse-cursor-buffer-width* *microcode-mouse-cursor-buffer-height*
				   MOUSE-CURSOR-PATTERN 0 0 MOUSE-CURSOR-PATTERN 0 0))
		       (LET-GLOBALLY ((X-POS 0)
				      (Y-POS 0))
			 (BIND (LOCF (SHEET-SCREEN-ARRAY SHEET)) MOUSE-CURSOR-PATTERN)
			 (BIND (LOCF (SHEET-LOCATIONS-PER-LINE SHEET)) 1)
			 ;; Cause recalculation of sheet parameters, because SCREEN-ARRAY changed
			 (SETQ SYS:%CURRENT-SHEET NIL)
			 (SEND SELF :BLINK)
			 ;; SCREEN-ARRAY will change back when unbinding gets done
			 (SETQ %CURRENT-SHEET NIL)
			 (SETQ PREPARED-SHEET NIL)
			 (SETQ PHASE          NIL))
		       (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE)))))
	))

;All the standard mouse blinkers are character blinkers, so enable fast tracking for them.
(DEFFLAVOR MOUSE-CHARACTER-BLINKER () (MOUSE-BLINKER-FAST-TRACKING-MIXIN CHARACTER-BLINKER))
(DEFFLAVOR MOUSE-RECTANGULAR-BLINKER () (MOUSE-BLINKER-MIXIN RECTANGULAR-BLINKER))
(DEFFLAVOR MOUSE-HOLLOW-RECTANGULAR-BLINKER
           ()
           (MOUSE-BLINKER-MIXIN HOLLOW-RECTANGULAR-BLINKER))


(DEFVAR MOUSE-BLINKER-TYPES NIL)
(DEFUN MOUSE-DEFINE-BLINKER-TYPE (TYPE CREATION-FUN)
  (SETQ MOUSE-BLINKER-TYPES (DELETE (ASSOC TYPE MOUSE-BLINKER-TYPES :TEST #'EQ)
				    MOUSE-BLINKER-TYPES :TEST #'EQ))
  (PUSH (CONS TYPE CREATION-FUN) MOUSE-BLINKER-TYPES)
  (AND MOUSE-SHEET (MOUSE-GET-BLINKER TYPE)))

(DEFUN MOUSE-GET-BLINKER (TYPE &OPTIONAL (SHEET MOUSE-SHEET)
                          &AUX (SCREEN (SHEET-GET-SCREEN SHEET))
                          BLINKERS)
  "Get a mouse blinker of type TYPE that can appear on SHEET.
TYPE is a keyword on MOUSE-BLINKER-TYPES."
  (LET ((BE (ASSOC TYPE MOUSE-BLINKER-TYPES :TEST #'EQ)))
    (OR BE (FERROR NIL "~A is unknown mouse blinker type" TYPE))
    (LET ((BL (CDR (ASSOC TYPE (SETQ BLINKERS (SEND SCREEN :MOUSE-BLINKERS)) :TEST #'EQ))))
      (COND ((NULL BL)
             (SETQ BL (SEND (CDR BE) SCREEN))
             (PUSH (CONS TYPE BL) BLINKERS)
             (SEND SCREEN :SET-MOUSE-BLINKERS BLINKERS)))
      (BLINKER-SET-SHEET BL SHEET)
      BL)))

(DEFUN MOUSE-SET-BLINKER-DEFINITION (TYPE X-OFFSET Y-OFFSET VISIBILITY
                                     OPERATION &REST OPERATION-ARGS)
  "Switch to a mouse blinker of type TYPE.
Send it OPERATION with arguments OPERATION-ARGS before using it.
X-OFFSET and Y-OFFSET are offsets which we subtract from the mouse position
to get the position to display the blinker.
VISIBILITY is the visibility for the blinker (usually T)."
  (LET (old-blinker-font old-blinker-character bl)
    ;;  Capture the previous mouse blinker font & char while we can...
    (WHEN (AND mouse-blinker
	       (mac-system-p)
	       *window-system-mouse-on-the-mac*
	       (NOT *ignore-commands-for-the-Mac*))
      (SETF old-blinker-font (SEND mouse-blinker :send-if-handles :font)
	    old-blinker-character (SEND mouse-blinker :send-if-handles :character)))
    (SETF BL (MOUSE-GET-BLINKER TYPE))
    (AND MOUSE-BLINKER (NEQ BL MOUSE-BLINKER) (SEND MOUSE-BLINKER :SET-VISIBILITY NIL))
    (WHEN operation
      (APPLY bl operation operation-args))  ;Change LISPM cursor
    (AND X-OFFSET
         (SEND BL :SET-OFFSETS X-OFFSET Y-OFFSET))
    (SEND BL :SET-VISIBILITY (IF (EQ VISIBILITY :ON) T VISIBILITY))
    (WHEN (AND mouse-blinker
	       (mac-system-p)
	       *window-system-mouse-on-the-mac*
	       (NOT *ignore-commands-for-the-Mac*))  
      ;;Change Mac cursor too
      (SEND bl :send-if-handles :create-Mac-image-of-Explorer-mouse-cursor
	    old-blinker-font old-blinker-character))
    (SEND BL :TRACK-MOUSE)
    BL))

(DEFUN MOUSE-SET-BLINKER (TYPE-OR-BLINKER &OPTIONAL X-OFFSET Y-OFFSET)
  "Switch to mouse blinker BLINKER, or to a blinker of type TYPE.
X-OFFSET and Y-OFFSET are offsets which we subtract from the mouse position
to get the position to display the blinker."
  (LET (BL)
    (IF (TYPEP TYPE-OR-BLINKER 'INSTANCE)
        (SETQ BL TYPE-OR-BLINKER)
        ;;ELSE
        (PROGN
          (SETQ BL (MOUSE-GET-BLINKER TYPE-OR-BLINKER))
          (AND X-OFFSET (SEND BL :SET-OFFSETS X-OFFSET Y-OFFSET))))
    (AND (NEQ BL MOUSE-BLINKER) (SEND MOUSE-BLINKER :SET-VISIBILITY NIL))
    (SEND BL :SET-VISIBILITY T)
    (WHEN (AND (mac-system-p)
	       *window-system-mouse-on-the-mac*
	       (NOT *ignore-commands-for-the-Mac*))
      ;;Change Mac cursor too
      (SEND bl :send-if-handles :create-Mac-image-of-Explorer-mouse-cursor nil nil))
    (SEND BL :TRACK-MOUSE)
    BL))

;;; Define a mouse blinker for a NW pointing arrow.
(MOUSE-DEFINE-BLINKER-TYPE :CHARACTER-BLINKER
                           #'(LAMBDA (SCREEN)
                               (MAKE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
                                 :VISIBILITY nil
                                 :FONT (SEND SCREEN :MOUSE-FONT)
                                 :CHAR MOUSE-GLYPH-NORTH-WEST-ARROW)))

;; Old name, same as :CHARACTER-BLINKER.
(MOUSE-DEFINE-BLINKER-TYPE :CHARACTER
                           #'(LAMBDA (SCREEN)
                               (MAKE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
                                 :VISIBILITY nil
                                 :FONT (SEND SCREEN :MOUSE-FONT)
                                 :CHAR MOUSE-GLYPH-NORTH-WEST-ARROW)))

;;; Define a mouse blinker for the angle bracket for the upper left
;;; corner.
(MOUSE-DEFINE-BLINKER-TYPE :RECTANGLE-CORNER-BLINKER
                           #'(LAMBDA (SCREEN)
                               (MAKE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
                                 :VISIBILITY NIL
                                 :FONT (SEND SCREEN :MOUSE-FONT)
                                 :CHAR MOUSE-GLYPH-UPPER-LEFT-CORNER)))
				
;;; Define a mouse blinker for a solid rectangle.
(MOUSE-DEFINE-BLINKER-TYPE :RECTANGLE-BLINKER
                           #'(LAMBDA (SCREEN)
                               (MAKE-BLINKER SCREEN 'MOUSE-RECTANGULAR-BLINKER
                                 :VISIBILITY NIL
                                 :HEIGHT 8. :WIDTH 8.)))

(DEFUN MOUSE-SET-BLINKER-CURSORPOS (&REST IGNORE)
  "Move the mouse blinker to the current mouse position."
  (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
      (SEND MOUSE-BLINKER :OFFSETS)
    (BLINKER-SET-CURSORPOS MOUSE-BLINKER
                           (- MOUSE-X X-OFF (SHEET-INSIDE-LEFT MOUSE-SHEET))
                           (- MOUSE-Y Y-OFF (SHEET-INSIDE-TOP  MOUSE-SHEET)))))

(DEFUN MOUSE-CALL-SYSTEM-MENU (&OPTIONAL (SUP MOUSE-SHEET))
  (PROCESS-RUN-FUNCTION '(:NAME "System Menu" :PRIORITY 10.)
                        #'(LAMBDA (SUP)
                            (USING-RESOURCE (MENU SYSTEM-MENU SUP)
                              (SEND MENU :CHOOSE)))
                        SUP))

;;; This function as a warm initialization
;;; to initialize the mouse process and associated variable.
(DEFUN MOUSE-INITIALIZE (&OPTIONAL (SHEET DEFAULT-SCREEN))
  (when (or (not (si:mp-system-p))				;jlm 2/28/89
	    (funcall mp-sib-owner-p))				;jlm 3/07/89
    (OR (BOUNDP 'MOUSE-PROCESS)			;If first time loaded, initialize everything
	(SETQ MOUSE-PROCESS (MAKE-PROCESS "Mouse" :SPECIAL-PDL-SIZE 2000. :PRIORITY 30.
					  :WARM-BOOT-ACTION NIL)))
    ;; Above warm-boot-action prevents the process from starting up
    ;; until after these initializations have been completed.
    (SETQ MOUSE-WINDOW        NIL
	  WINDOW-OWNING-MOUSE NIL
	  MOUSE-X 0
	  MOUSE-Y 0
	  MOUSE-SHEET SHEET)
    (%SET-MOUSE-SCREEN SHEET)
    ;; Fill the mouse tracker's arrays with NIL instead of the garbage
    ;; that they contain initially.  At least interpreted ASET won't work otherwise.
    (LOOP FOR I FROM #o1640 BELOW #o1774
	  DO (%P-dpb nil %%q-all-but-cdr-code (+ A-MEMORY-VIRTUAL-ADDRESS I)))
    ;; Set scaling and speed dependence.
    (MOUSE-SPEED-HACK 0.6 80. 0.8 104. 1 128. 1.3 192. 1.5 256. 1.8 320. 2.2 448. 2.5)
    ;; Make sure at least one blinker of each type exists
    (MOUSE-GET-BLINKER :CHARACTER)
    (MOUSE-GET-BLINKER :RECTANGLE-CORNER-BLINKER)
    (MOUSE-GET-BLINKER :RECTANGLE-BLINKER)
    (AND MOUSE-BLINKER (BLINKER-SET-VISIBILITY MOUSE-BLINKER NIL))
    (MOUSE-STANDARD-BLINKER)
    (MOUSE-WARP (- (SHEET-INSIDE-WIDTH MOUSE-SHEET)   8.)
		(- (SHEET-INSIDE-HEIGHT MOUSE-SHEET) 16.))
    ;; Call MOUSE-INPUT once to flush any pending motion and update variables, but don't wait.
    (SETQ MOUSE-BUTTONS-BUFFER-OUT-INDEX MOUSE-BUTTONS-BUFFER-IN-INDEX)
    (MOUSE-INPUT NIL)
    (SETQ MOUSE-X-SPEED 0
	  MOUSE-Y-SPEED 0)
    ;; Start up the mouse process
    (SEND MOUSE-PROCESS :PRESET 'MOUSE-OVERSEER)
    (SEND MOUSE-PROCESS :RUN-REASON)))

(DEFUN MOUSE-SET-SHEET (NEW-SHEET)
  "Specify the sheet for the mouse to track on.  Sets MOUSE-SHEET."
  (LOOP FOR SH = NEW-SHEET THEN (SHEET-SUPERIOR SH) UNTIL (NULL SH)
        UNLESS (SHEET-EXPOSED-P SH)
        DO (FERROR NIL "Attempt to set MOUSE-SHEET to a non-visible sheet"))
  (WITH-MOUSE-USURPED
    (%OPEN-MOUSE-CURSOR)
    (SETQ MOUSE-SHEET NEW-SHEET)
    (%SET-MOUSE-SCREEN NEW-SHEET)
    (MOUSE-DISCARD-CLICKAHEAD)			; Since the coordinate system has changed
    (MOUSE-STANDARD-BLINKER)
    (MULTIPLE-VALUE-BIND (X Y) (SEND MOUSE-BLINKER :READ-CURSORPOS)
      (SETQ MOUSE-X (+ X (SHEET-INSIDE-LEFT MOUSE-SHEET))
            MOUSE-Y (+ Y (SHEET-INSIDE-TOP  MOUSE-SHEET)))))
  ;;; Added by KJF for CJJ on 08/16/88 for Multiple Monitor (MMON) support.
  (WHEN (mmon-p)
    (LET ((screen (sheet-get-screen new-sheet)))
      (SEND screen :maybe-make-default-screen :mouse)
      (SEND screen :maybe-make-associated-who-line-screen-active :mouse)
      (SEND screen :maybe-select-window-on :mouse))))

(DEFUN MOUSE-WARP (X Y &OPTIONAL (RELATIVE NIL))
  "Sets mouse position on MOUSE-SHEET to X and Y.  If RELATIVE is
T then the mouse moves relative to its current position.  If RELATIVE
is NIL then the mouse moves to the specified (X,Y) position."
  (COND ((OR (NOT (= MOUSE-X X)) (NOT (= MOUSE-Y Y)))
         (SETQ MOUSE-WARP T)
         (WITHOUT-INTERRUPTS
           (%OPEN-MOUSE-CURSOR)
	   ;; Also update the *unglitched-...* variables.
	   ;; This ensures mouse-transport stops in case mouse-overseer gets called before mouse-input after a mouse-transport.
	   ;; Added for mouse-transport support.  CJJ 06/06/88.
	   ;;; Added by KJF for CJJ on 08/16/88 for Multiple Monitor (MMON) support.
	   (SETQ *unglitched-mouse-x*
		 (SETQ MOUSE-LAST-X
		       (SETQ MOUSE-X (MAX 0
					  (MIN (1- (SHEET-WIDTH MOUSE-SHEET))
					       (IF RELATIVE (+ MOUSE-X X) X))))))
	   (SETQ *unglitched-mouse-y*
		 (SETQ MOUSE-LAST-Y
		       (SETQ MOUSE-Y (MAX 0
					  (MIN (1- (SHEET-HEIGHT MOUSE-SHEET))
					       (IF RELATIVE (+ MOUSE-Y Y) Y))))))
           (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
                 PREPARED-SHEET NIL
		 ;;; Added by KJF for CJJ on 08/16/88 for Multiple Monitor (MMON) support.
		 *unglitched-mouse-x-speed* 0
		 *unglitched-mouse-y-speed* 0))
         (MOUSE-SET-BLINKER-CURSORPOS)
         ;; Make sure the mouse tracker process notices.
         (SETQ MOUSE-WAKEUP T))))

;;; This returns the lowest window under the mouse prepared to handle the given operation.
(DEFUN WINDOW-UNDER-MOUSE (&OPTIONAL METHOD (ACTIVE-CONDITION :ACTIVE) X Y)
  (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET (OR X MOUSE-X) (OR Y MOUSE-Y)
                            METHOD ACTIVE-CONDITION))

;;; This is the window or special thing that owns the mouse
(DEFUN WINDOW-OWNING-MOUSE (&OPTIONAL X Y)
  "Return the window that is allowed to handle the mouse now.
If X and Y are specified, then return the WINDOW that would
be handling the mouse if it were at that position."
  (OR WINDOW-OWNING-MOUSE
      (WINDOW-UNDER-MOUSE :HANDLE-MOUSE :EXPOSED X Y)))

(DEFUN WINDOW-OWNS-MOUSE-P (WINDOW &OPTIONAL X Y)
  "T if WINDOW is the window that is allowed to handle the mouse now.
If one of WINDOW's inferiors is the one, we also return T.
If X and Y are specified, then T if WINDOW is the window that would
be handling the mouse if it were at that position."
  (COND ((EQ WINDOW T)
         (EQ WINDOW-OWNING-MOUSE T))
        (WINDOW-OWNING-MOUSE
         (DO ((W WINDOW (SHEET-SUPERIOR W))) ((NULL W) NIL)
           (AND (EQ W WINDOW-OWNING-MOUSE) (RETURN T))))
        (T
         (AND (SHEET-EXPOSED-P WINDOW)
              (SHEET-CONTAINS-SHEET-POINT-P WINDOW MOUSE-SHEET
                                            (OR X MOUSE-X) (OR Y MOUSE-Y))))))

(DEFUN MOUSE-SEIZE ()
  "Make the currently selected window able to handle the mouse wherever it goes.
Even if the mouse moves out of the window, this window will still handle it.
Set WINDOW-OWNING-MOUSE to NIL to let other windows have a chance again."
  (SETQ WINDOW-OWNING-MOUSE SELECTED-WINDOW))

;; may 9-1-88
;(DEFUN MOUSE-WAKEUP ()
;  "Inform mouse tracking that the screen layout has changed."
;; (SETQ MOUSE-RECONSIDER T)
;  (SETQ MOUSE-WAKEUP T))


(DEFUN MOUSE-OVERSEER ()
  "This is the top-level function of the mouse process.
It tracks the mouse and does default things with the buttons.  If the
mouse enters the region of the screen occupied by a window that has its
own mouse handler, that mouse handler is called.  It then has control of
the mouse until it returns.  Both this function and specialized mouse
handlers are to call the above low-level routines.  MOUSE-WINDOW is the
window which has control of the mouse, or NIL if there is none.  Note
that window need not be selected, nor exposed.  It does have to be
active.  Mouse handlers are free to mung the mouse blinker however they
like.  The mouse overseer is guaranteed to put it back.  Most mouse
handlers will return whenever WINDOW-OWNING-MOUSE says that the mouse
has moved outside of the visible part of that window.  Some however,
will not return until they feel like it.  The convention to be used is
up to the individual handler.  Frobbing with MOUSE-RECONSIDER is for
race-free interface with WITH-MOUSE-GRABBED."
  (ERROR-RESTART-LOOP ((SYS:ABORT ERROR) "Return to top level of MOUSE-PROCESS.")
    (mouse-discard-clickahead)
    (DO () (NIL)
      (SETQ MOUSE-RECONSIDER NIL)
      (IF MOUSE-SHEET
	  (MOUSE-SET-BLINKER-CURSORPOS))
      (COND ((EQ (WITHOUT-INTERRUPTS (SETQ MOUSE-WINDOW (WINDOW-OWNING-MOUSE))) 'STOP)
	     (PROCESS-WAIT "Usurped"
			   #'(LAMBDA () (OR MOUSE-RECONSIDER	; This can happen randomly
					    (NEQ WINDOW-OWNING-MOUSE 'STOP)))))
	    ((NOT (SYMBOLP MOUSE-WINDOW))
	     (FUNCALL MOUSE-WINDOW :HANDLE-MOUSE))
	    (T
	     ;; Standardize the blinker if no one else will.
	     (OR MOUSE-WINDOW (MOUSE-STANDARD-BLINKER MOUSE-SHEET))
	     (MOUSE-DEFAULT-HANDLER MOUSE-WINDOW)))
      ;; Added for mouse-transport.  This was put here so that everything involved with mouse leaving a window
      ;; would be performed before attempting to have mouse leave a screen.  CJJ 05/06/88.
      ;;; Added by KJF for CJJ on 08/16/88 for Multiple Monitor (MMON) support.
      (WHEN (mmon-p)
	(IF *transport-mouse-along-trajectory*
	    (maybe-change-mouse-sheet-along-trajectory)
	    (maybe-change-mouse-sheet))))))

;;; Magic adjustments

;; Magic top-level values for scroll bar parameters
;; These can be bound by :HANDLE-MOUSE methods.
(DEFVAR MOUSE-SPEED 0) 				;Speed mouse is travelling (inches per sec)

(DEFPARAMETER SCROLL-BAR-MAX-SPEED 10
  "Speed at which scroll bar gets ignored.")
(DEFPARAMETER SCROLL-BAR-MAX-EXIT-SPEED NIL
  "Speed at which you leave scroll bar.")
(DEFPARAMETER SCROLL-BAR-RELUCTANCE 1
  "Pixels before entering scroll bar.")

(DEFPARAMETER MOUSE-FAST-MOTION-SPEED 30.
  "Moving faster than this triggers a cross
MOUSE-FAST-MOTION-CROSS-SIZE in diameter.")
(DEFPARAMETER MOUSE-FAST-MOTION-CROSS-SIZE 40.
  "Moving the mouse fast than this triggers a cross with this as the radius")
(DEFPARAMETER MOUSE-FAST-MOTION-CROSS-TIME 2000.
  " which lasts this long (DO-loop units).")

(DEFPARAMETER MOUSE-FAST-MOTION-BITMAP-TIME 16000.
  " which lasts this long (DO-loop units).")
(DEFPARAMETER MOUSE-FAST-TRACK-BITMAP-MOUSE-P NIL
  "When the mouse is moved very rapidly, the mouse blinker gets big
like Godzilla in order to maintain visibility.  Small children should be
taken out of the room before demonstrating this frightening feature.
Non-NIL says to use the Godzilla mouse, NIL says to use the cross.
This is NOT supported on the microExplorer.") ;; may 04/20/89 

(DEFPARAMETER BITMAP-MOUSE-PATHNAME "SYS:WINDOW;GODZILLA-MOUSE.BITMAP"
  "A default pathname for the bitmap mouse.  This lets a user
load from somewhere else (perhaps even a different bitmap).")

(DEFVAR BITMAP-MOUSE-MAX-WIDTH  NIL)
(DEFVAR BITMAP-MOUSE-MAX-HEIGHT NIL)

(DEFUN LOAD-BITMAP-MOUSE ()
  "Load the bitmap mouse."
  (MULTIPLE-VALUE-BIND (ARRAY WIDTH HEIGHT)
      (COND ((PROBE-FILE BITMAP-MOUSE-PATHNAME)
	     (READ-BIT-ARRAY-FILE BITMAP-MOUSE-PATHNAME))
	    (T (VALUES 0 0)))
    (SETQ BITMAP-MOUSE-MAX-WIDTH  WIDTH
	  BITMAP-MOUSE-MAX-HEIGHT HEIGHT)
    ARRAY))

(DEFVAR BITMAP-MOUSE (and (not (si:addin-p)) ;; may 04/20/89 for build ...
			  (LOAD-BITMAP-MOUSE))
  "Bitmap for the Bitmap mouse.")

;; This mouse handler serves for windows which want to do things the simple way.
;; A second argument of T says that the window should have a scroll bar.
;; This function is also used to track the mouse when it isn't inside any window,
;; by calling it with an argument of NIL.
;; An arg of T is used when the mouse has been seized by a process not
;; for any specific window.
(DEFUN MOUSE-DEFAULT-HANDLER (WINDOW &OPTIONAL SCROLL-BAR-FLAG
                              &AUX
                              (WINDOW-X-OFFSET 0) (WINDOW-Y-OFFSET 0)
                              WINDOW-X WINDOW-Y)
  (unless (SYMBOLP WINDOW)
    (MULTIPLE-VALUE-SETQ (WINDOW-X-OFFSET WINDOW-Y-OFFSET)
			 (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)))

  ;; Be careful not to do the :update method when the who line documentation window
  ;; isn't there (which is the case during a window system build).
  (WHEN (AND (BOUNDP 'WHO-LINE-DOCUMENTATION-WINDOW) WHO-LINE-DOCUMENTATION-WINDOW)
    ;; Update who-line when entering new handlers.
    (send who-line-documentation-window :update))
  
  (DO ((DX) (DY) (BU) (BD) (HAND) (X) (Y)
       (OLD-OWNER WINDOW-OWNING-MOUSE WINDOW-OWNING-MOUSE)
       (LEFT-OFFSET 0)
       (right-offset 0)
       (WAIT-FLAG NIL T))
      (MOUSE-RECONSIDER)
    ;; Wait until the mouse moves
    (MULTIPLE-VALUE-SETQ (DX DY BD BU X Y) (MOUSE-INPUT WAIT-FLAG))
    ;; If asked to reconsider, do so immediately.
    ;; Don't bother updating blinker since it is likely to change soon, and
    ;; in any case we are going to be called back shortly.
    (when MOUSE-RECONSIDER (RETURN NIL))
    ;; Update console-idle time when buttons pushed
    (unless (ZEROP BD) (SETQ KBD-LAST-ACTIVITY-TIME (TIME)))
    ;; Approximate speed of the mouse in inches per second
    (SETQ MOUSE-SPEED (/ (ISQRT (+ (* MOUSE-X-SPEED MOUSE-X-SPEED)
                                   (* MOUSE-Y-SPEED MOUSE-Y-SPEED)))
                         100.0s0))
    ;; If the mouse is moving incredibly fast, flash up something to
    ;; help the user find it.  Thus if you can't find the mouse, you must whip it.
    (when (> MOUSE-SPEED MOUSE-FAST-MOTION-SPEED)
      (if mouse-fast-track-bitmap-mouse-p
	  (draw-bitmap-mouse-cursor mouse-speed)
	  ;;ELSE
	  (draw-mouse-fast-motion-cursor)))

    (SETQ WINDOW-X (- X WINDOW-X-OFFSET)        ; X offset of mouse within window
          WINDOW-Y (- Y WINDOW-Y-OFFSET))       ; Y offset of mouse within window

    ;; Consider entering the scroll bar.  [Perhaps this should be changed so that
    ;; it is in the move-handler rather than here.  Problem with that is LEFT-OFFSET.]
    ;; If there is a scroll bar and we are entering it, activate it.
    ;; However, the mouse must move at least a certain distance past the edge
    ;; of the window in order to qualify for scrolling (this is set by
    ;; the SCROLL-BAR-RELUCTANCE variable in the window).  Before entering
    ;; scroll bar, send a :MOUSE-MOVES message in order to let the window know
    ;; what's happening.

    ;; LEFT-OFFSET is how far out the left side of the window the mouse has moved,
    ;; or 0 if the mouse is inside the window.
    ;; If the window is at the left edge of the screen, MOUSE-X will not itself
    ;; move out the left edge of the window, but DX will.  When the mouse reaches
    ;; the left edge of the window, accumulate leftward motion into LEFT-OFFSET.  
    ;; RIGHT-OFFSET does the same thing for when the scroll-bar is on the right.
    (COND ((< WINDOW-X 0)	;; may 9-7-88 was <= see HACK below
	   (SETQ LEFT-OFFSET  (IF (PLUSP LEFT-OFFSET)
				  (MAX (- LEFT-OFFSET DX) 1)
				  1)))	   		;First time, don't use all of DX
	  ((and (typep window 'sheet)
		(>= window-x (sheet-width window))
		(send window :send-if-handles :scroll-bar-on-right))
	   (setq right-offset (if (plusp right-offset)
				  (max (+ right-offset dx) 1)
				  1)))
	  ;; [may] 9-7-88
	  ;; HACK : the left edge is a special case. Its possible to hang up the
	  ;;         the system when scroll bar on right and mouse on left edge
	  ;;         looping in :handle-mouse and here with wait-flag always nil
	  ;;         which allows #'mouse-input to be process greedy! We COULD always set
	  ;;         left-offset to 0 in/of 1 BUT left edge of scroll-bar would not make
	  ;;         double-arrow for scrolling sometimes.
	  ;; cases : 1. left-offset=0 AND no scroll-bar ( fixed lying :enable-scrolling-p )
	  ;;         2. left-offset=0 AND scroll-bar on right - force left-offset = 0 to check right-offset
	  ;;         3. left-offset=0 AND left scroll-bar - this case only happens SOMETIMES as scroll-bar is being
	  ;;            added. The other two cases happen EVERY time.
	  ((and (= window-x 0) (eq scroll-bar-flag t)) ;; may 01/30/89 ADDED (eq t scroll-bar-flag) check I left out.
	   ;; we are called with scroll-bar-flag indicating a scroll bar but if
	   ;; scroll bar is on RIGHT we really should check right-offset FIRST in COND below
	   (COND ((and (typep window 'sheet)
		       (send window :send-if-handles :scroll-bar-on-right)) ;; scroll bar is on RIGHT
		  (SETQ left-offset 0)) ;; force skipping of left-offset in COND below
		 (t
		  (SETQ LEFT-OFFSET  (IF (PLUSP LEFT-OFFSET)
				  (MAX (- LEFT-OFFSET DX) 1)
				  0))))) ;First time, don't use all of DX [may 9-13-88 was 1 - fix case 3]
	  (t
	   (SETQ LEFT-OFFSET 0
		 right-offset 0)))
    (COND ((or old-owner window-owning-mouse)) ; These disable scroll-bar.
	  ((EQ SCROLL-BAR-FLAG T)
	   (cond ((AND SCROLL-BAR-MAX-SPEED
		      (> MOUSE-SPEED SCROLL-BAR-MAX-SPEED)))
		 ((PLUSP LEFT-OFFSET)
		  (cond ((NOT (SEND WINDOW :send-if-handles :scroll-bar))
			 (return nil))
			((> LEFT-OFFSET SCROLL-BAR-RELUCTANCE)
			 (SEND (IF (SYMBOLP WINDOW)
				   'MOUSE-SET-BLINKER-CURSORPOS WINDOW)
			       :mouse-moves WINDOW-X WINDOW-Y)
			 (RETURN (SEND WINDOW :HANDLE-MOUSE-SCROLL)))
			(T (SETQ WINDOW-X 0))))	;Don't escape the window yet
		 ((plusp right-offset)
		  (cond ((NOT (SEND WINDOW :send-if-handles :scroll-bar-on-right))
			 (return nil))
			((> right-OFFSET SCROLL-BAR-RELUCTANCE)
			 (SEND (IF (SYMBOLP WINDOW)
				   'MOUSE-SET-BLINKER-CURSORPOS WINDOW)
			       :mouse-moves WINDOW-X WINDOW-Y)
			 (RETURN (SEND WINDOW :HANDLE-MOUSE-SCROLL)))
			(T (SETQ WINDOW-X (1- (sheet-width window))))))))	;Don't escape the window yet
	  
	  ((EQ SCROLL-BAR-FLAG :IN)
	   ;; We are in the scroll bar.  Moving the mouse faster than the exit
	   ;; speed, or moving it into the interior of the window more than 40. 
	   ;; will escape.  
	   (COND ((AND SCROLL-BAR-MAX-EXIT-SPEED
		       (> MOUSE-SPEED SCROLL-BAR-MAX-EXIT-SPEED))
		  ;; Moving like a bat, let the guy out of the scroll bar
		  (RETURN NIL))
		 ((MINUSP WINDOW-X)		;Trying to go out left, shove back in.
		  (WITHOUT-INTERRUPTS
		    (%OPEN-MOUSE-CURSOR)
		    (SETQ WINDOW-X 0)
		    (SETQ MOUSE-LAST-X (SETQ MOUSE-X WINDOW-X-OFFSET))
		    (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
			  PREPARED-SHEET NIL)))
		 ((>= window-x (sheet-width window))	;Trying to go out right, shove back in.
		  (WITHOUT-INTERRUPTS
		    (%OPEN-MOUSE-CURSOR)
		    (SETQ WINDOW-X (- (sheet-width window) 8.))  ; hack
		    (SETQ MOUSE-LAST-X (SETQ MOUSE-X (+ WINDOW-X-OFFSET window-x)))
		    (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
			  PREPARED-SHEET NIL)))
		 ((and (> WINDOW-X (+ 40. (sheet-inside-left window)))	; Escape scroll-bar
		       (< window-x (- (sheet-inside-right window) 40.)))
		  (RETURN NIL)))))
    ;; Update the position of the mouse before checking for button clicks, so
    ;; that button clicks get processed with knowledge of where the mouse
    ;; was when the button was first clicked.  The arguments to the move handler
    ;; may be where the mouse was when the button was clicked, whereas the
    ;; mouse cursor follows MOUSE-X and MOUSE-Y, which may be different.   
    (SETQ MOUSE-WARP NIL)
    (SEND (IF (SYMBOLP WINDOW)
                 'MOUSE-SET-BLINKER-CURSORPOS WINDOW)
             :mouse-moves WINDOW-X WINDOW-Y)
    ;; Check for all the ways of losing control of the mouse.
    (IF (COND ;; The move handler may have decided to warp the mouse so that it will not
              ;; move out of the window.  This test is a crock but should work.
              (MOUSE-WARP NIL)
              ;; Check for mouse ceasing to be grabbed.
              ((EQ WINDOW T)
               (NEQ WINDOW-OWNING-MOUSE T))
              ;; Check for window becoming grabbed.
              ((EQ WINDOW-OWNING-MOUSE T)
               (NEQ WINDOW T))
              ;; Check for some other window (not above this one) being greedy.
              (WINDOW-OWNING-MOUSE
               (NOT (SHEET-ME-OR-MY-KID-P WINDOW WINDOW-OWNING-MOUSE)))
              ;; Check for moving into a window when not in any	      
	      ((NULL window)
	       (OR (window-owning-mouse x y)
		   ;; Also check for mouse escaping MOUSE-SHEET.  CJJ 05/11/88.
		   ;;; For Mouse-Transport support.  Added by KJF for CJJ on 08/16/88 for
		   ;;; Multiple Monitor (MMON) support.
		   (AND (mmon-p) (NOT (mouse-in-mouse-sheet-p)))))
              ;; Check for leaving the boundaries of the current window
              ;; HYSTERETIC-WINDOW-MIXIN requires that we wait at least once before returning
              ((NOT (AND (SHEET-EXPOSED-P WINDOW)
                         (>= WINDOW-X 0)
                         (<  WINDOW-X (SHEET-WIDTH WINDOW))
                         (>= WINDOW-Y 0)
                         (<  WINDOW-Y (SHEET-HEIGHT WINDOW))
			 ;; Also check for mouse escaping MOUSE-SHEET.  CJJ 05/11/88.
			 ;;; For Mouse-Transport support.  Added by KJF for CJJ on 08/16/88 for
			 ;;; Multiple Monitor (MMON) support.
			 (IF (mmon-p)
			     (OR (EQ scroll-bar-flag :in) (mouse-in-mouse-sheet-p))
			     ;; else
			     t)))
               WAIT-FLAG)
              ;; Check for moving into an inferior of the current window
              ((NEQ (LOWEST-SHEET-UNDER-POINT WINDOW WINDOW-X WINDOW-Y
                                              :HANDLE-MOUSE :EXPOSED)
                    WINDOW)
               T))
        ;; Return to overseer, saving any pending button click.
        (RETURN (MOUSE-DEFER-BUTTONS BU BD)))
    ;; Now process button pushes if mouse is not seized.
    (COND ((OR (ZEROP BD) (EQ WINDOW T) (EQ OLD-OWNER T)))	;; may 9-7-88 
          ;; If over an exposed window, send it the button-push
          (WINDOW (SEND WINDOW :mouse-buttons BD WINDOW-X WINDOW-Y))
          ;; Default action for left button is to select what mouse is pointing at
          ((LOGTEST 1 BD)
           (AND (SETQ HAND (WINDOW-UNDER-MOUSE :MOUSE-SELECT :ACTIVE X Y))
                ;; Next line temporarily papers over a bug with :MOUSE-SELECT
                (GET-HANDLER-FOR HAND :SELECT)
                (MOUSE-SELECT HAND)))
          ;; Default action for middle button is to switch to the main screen
          ((LOGTEST 2 BD)
           (when (TYPEP MOUSE-SHEET 'SCREEN)
	     (if (mac-screen-p mouse-sheet)
		 (give-mouse-ownership-to-the-Explorer)
		 (PROCESS-RUN-FUNCTION "Set mouse sheet" #'MOUSE-SET-SHEET DEFAULT-SCREEN))))
          ;; Default action for right button is to call the system menu
          ((LOGTEST 4 BD)
           (MOUSE-CHARACTER-BUTTON-ENCODE BD)      ;Satisfy those who double-click out of habit
           (MOUSE-CALL-SYSTEM-MENU)))))

(DEFUN DRAW-MOUSE-FAST-MOTION-CURSOR ()
  "Draw a large mouse cursor."
  (LET ((MIN-X 0) (MIN-Y 0)
	(MAX-X (1- (SHEET-INSIDE-WIDTH  MOUSE-SHEET)))
	(MAX-Y (1- (SHEET-INSIDE-HEIGHT MOUSE-SHEET)))
	;; Save the mouse (x y) values away in case they change.
	;; Remember that the mouse is moving very fast in this function,
	;; making the probability that the mouse (x y) values changing
	;; be reasonably large.
	(CURRENT-MOUSE-X MOUSE-X)
	(CURRENT-MOUSE-Y MOUSE-Y))
    (LET ((XTOP    (MAX (- CURRENT-MOUSE-Y MOUSE-FAST-MOTION-CROSS-SIZE    ) MIN-Y))
	  (XBOTTOM (MIN (+ CURRENT-MOUSE-Y MOUSE-FAST-MOTION-CROSS-SIZE 16.) MAX-Y))
	  (XLEFT   (MAX (- CURRENT-MOUSE-X MOUSE-FAST-MOTION-CROSS-SIZE    ) MIN-X))
	  (XRIGHT  (MIN (+ CURRENT-MOUSE-X MOUSE-FAST-MOTION-CROSS-SIZE 16.) MAX-X))
	  (XX      (MAX MIN-X (MIN CURRENT-MOUSE-X (- MAX-X 16.))))
	  (YY      (MAX MIN-Y (MIN CURRENT-MOUSE-Y (- MAX-Y 16.)))))
      (SHEET-IS-PREPARED (MOUSE-SHEET)
	(WITHOUT-INTERRUPTS
	  (%DRAW-RECTANGLE 16. (MAX (- XBOTTOM XTOP) 0) XX XTOP  ALU-XOR MOUSE-SHEET)
	  (%DRAW-RECTANGLE (MAX (- XRIGHT XLEFT) 0) 16. XLEFT YY ALU-XOR MOUSE-SHEET)
	  (DOTIMES (I MOUSE-FAST-MOTION-CROSS-TIME))
	  (%DRAW-RECTANGLE 16. (MAX (- XBOTTOM XTOP) 0) XX XTOP  ALU-XOR MOUSE-SHEET)
	  (%DRAW-RECTANGLE (MAX (- XRIGHT XLEFT) 0) 16. XLEFT YY ALU-XOR MOUSE-SHEET))))))

(DEFUN DRAW-BITMAP-MOUSE-CURSOR (MOUSE-SPEED)
  "Draw a bitmap as a mouse cursor."
  ;; When we were not successful loading the bitmap mouse then use the default `big' mouse.
  (IF (or (si:addin-p) ;; may 04/20/89 
	  (ZEROP BITMAP-MOUSE-MAX-WIDTH))
      (DRAW-MOUSE-FAST-MOTION-CURSOR)
      ;;ELSE
      (LET* ((MIN-X 0) (MIN-Y 0)
	     (WIDTH-MAX   BITMAP-MOUSE-MAX-WIDTH )
	     (HEIGHT-MAX  BITMAP-MOUSE-MAX-HEIGHT)
	     (HALF-WIDTH  (TRUNCATE WIDTH-MAX  2))
	     (HALF-HEIGHT (TRUNCATE HEIGHT-MAX 2))
	     (MAX-X       (1- (SHEET-INSIDE-WIDTH  MOUSE-SHEET)))
	     (MAX-Y       (1- (SHEET-INSIDE-HEIGHT MOUSE-SHEET)))
	     ;; Center the image on the current mouse position.
	     (X           (MAX MIN-X (- MOUSE-X HALF-WIDTH)))
	     (Y           (MAX MIN-Y (- MOUSE-Y HALF-HEIGHT)))
	     ;; Make sure that the image stays within the mouse-sheet.
	     (WIDTH       (MIN WIDTH-MAX  (- MAX-X X)))
	     (HEIGHT      (MIN HEIGHT-MAX (- MAX-Y Y))))
	(SHEET-IS-PREPARED (MOUSE-SHEET)
	  (WITHOUT-INTERRUPTS
	    (BITBLT ALU-XOR WIDTH HEIGHT BITMAP-MOUSE 0 0 (SHEET-SCREEN-ARRAY MOUSE-SHEET) X Y)
	    (DOTIMES (I MOUSE-FAST-MOTION-BITMAP-TIME))
	    (BITBLT ALU-XOR WIDTH HEIGHT BITMAP-MOUSE 0 0 (SHEET-SCREEN-ARRAY MOUSE-SHEET) X Y))))))

(DEFUN MOUSE-WAIT (&OPTIONAL (OLD-X MOUSE-X) (OLD-Y MOUSE-Y) (OLD-BUTTONS MOUSE-LAST-BUTTONS)
                   (WHOSTATE "MOUSE"))
  "Wait for the mouse to move or a button transition.  For processes other than MOUSE-PROCESS.
If the arguments are supplied, we wait for the mouse state to be
different from them.  To avoid lossage, save the values of
MOUSE-X and MOUSE-Y and MOUSE-LAST-BUTTONS, use the saved values,
then pass the saved values to this function.
WHOSTATE is displayed in the who line while we wait."
  ; BUTTONS can be character or integer.  
  (PROCESS-WAIT WHOSTATE
                #'(LAMBDA (OLD-X OLD-Y OLD-BUTTONS)
                    (OR (NOT (= MOUSE-X OLD-X))
                        (NOT (= MOUSE-Y OLD-Y))
                        (NOT (= MOUSE-LAST-BUTTONS OLD-BUTTONS))))
                OLD-X OLD-Y OLD-BUTTONS))

(DEFFLAVOR ESSENTIAL-MOUSE () ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:METHOD-COMBINATION (:OR :BASE-FLAVOR-LAST :MOUSE-CLICK)))

(DEFMETHOD (ESSENTIAL-MOUSE :HANDLE-MOUSE) ()
  (MOUSE-STANDARD-BLINKER SELF)
  (MOUSE-DEFAULT-HANDLER SELF (SEND SELF :ENABLE-SCROLLING-P)))

(DEFMETHOD (ESSENTIAL-MOUSE :SET-MOUSE-CURSORPOS) (X Y)
  (SEND SELF :SET-MOUSE-POSITION (+ (SHEET-INSIDE-LEFT) X) (+ (SHEET-INSIDE-TOP) Y)))

(DEFMETHOD (ESSENTIAL-MOUSE :SET-MOUSE-POSITION) (X Y &AUX X-OFF Y-OFF)
  (COND ((SHEET-ME-OR-MY-KID-P SELF MOUSE-SHEET)
         (MULTIPLE-VALUE-SETQ (X-OFF Y-OFF)
           (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
         (SETQ X (IF X
                     (+ X X-OFF)
                     ;;ELSE
                     MOUSE-X)
               Y (IF Y
                     (+ Y Y-OFF)
                     ;;ELSE
                     MOUSE-Y))
         (MOUSE-WARP X Y))
    (T (FERROR NIL
               "Attempt to :SET-MOUSE-POSITION on a ~S, which is not related to MOUSE-SHEET."
               SELF))))

(DEFMETHOD (ESSENTIAL-MOUSE :before :MOUSE-MOVES) MOUSE-SET-BLINKER-CURSORPOS)
(DEFMETHOD (ESSENTIAL-MOUSE :MOUSE-MOVES) (&rest ignore) nil)

(DEFMETHOD (ESSENTIAL-MOUSE :MOUSE-BUTTONS) (BD X Y)
  ; BD can be character or integer.
  (LET ((BUTTONS (MOUSE-CHARACTER-BUTTON-ENCODE BD)))
    (IF (= BUTTONS #\MOUSE-R-2)
        (MOUSE-CALL-SYSTEM-MENU)
        ;;ELSE
        (SEND SELF :MOUSE-CLICK BUTTONS X Y))))

(DEFMETHOD (ESSENTIAL-MOUSE :MOUSE-CLICK) (BUTTONS x y)
  ; BUTTONS can be character or integer.
  (COND ((AND (= BUTTONS #\MOUSE-L-1)
	      (NOT (SEND (SEND SELF :ALIAS-FOR-SELECTED-WINDOWS)
			 :SELF-OR-SUBSTITUTE-SELECTED-P))
	      (GET-HANDLER-FOR SELF :SELECT))	;paper over a bug
	 (MOUSE-SELECT SELF)
	 T)
	(T
	 (OR (SEND SELF :SEND-IF-HANDLES :FORCE-KBD-INPUT
		   `(:MOUSE-BUTTON ,(MERGE-SHIFT-KEYS BUTTONS) ,SELF ,X ,Y))
	     (AND (= BUTTONS #\MOUSE-R-1)
		  (MOUSE-CALL-SYSTEM-MENU)
		  T)
	     (BEEP)))))				;click not handled

(DEFUN MERGE-SHIFT-KEYS (CHAR)
  "Return CHAR with the bits for various shift keys updated.
For example, (CHAR-BIT result :HYPER) will be T
if a Hyper key is depressed right now."
  ; Accepts character or integer and returns the same.
  (IF (CHAR-MOUSE-P CHAR)
      ;; We have already translated mouse characters, so no
      ;; need to do anything more.
       CHAR
       ;;ELSE
       (PROGN
	 (SETF (CHAR-BIT CHAR :HYPER  ) (KEY-STATE :HYPER  ))
	 (SETF (CHAR-BIT CHAR :SUPER  ) (KEY-STATE :SUPER  ))
	 (SETF (CHAR-BIT CHAR :META   ) (KEY-STATE :META   ))
	 (SETF (CHAR-BIT CHAR :CONTROL) (KEY-STATE :CONTROL))
	 CHAR)))

(DEFUN MOUSE-SELECT (WINDOW)
  (IF (EQ CURRENT-PROCESS MOUSE-PROCESS)
      (PROCESS-RUN-FUNCTION '(:NAME "Mouse Select" :PRIORITY 20.) #'MOUSE-SELECT WINDOW)
      (SEND WINDOW :MOUSE-SELECT NIL)))

;New name.
(DEFMETHOD (ESSENTIAL-MOUSE :ENABLE-SCROLLING-P) ()
  (SEND SELF :SCROLL-BAR-P))

;Old name.
(DEFMETHOD (ESSENTIAL-MOUSE :SCROLL-BAR-P) () NIL)

;;; Other flavors have :AFTER methods for this method which will actually do
;;; the work.
(DEFMETHOD (ESSENTIAL-MOUSE :NEW-SCROLL-POSITION) (&OPTIONAL IGNORE) NIL)

(DEFFLAVOR KBD-MOUSE-BUTTONS-MIXIN () ()
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE)
  (:DOCUMENTATION :MIXIN "Sticks clicks in input buffer as characters
Clicking on the window when it is not selected will select it; mouse-right-twice
calls the system menu; any other number of mouse clicks is sent as a character object
via :force-kdb-input, char-mouse-button is button clicked on, char-mouse-clicks
the number of click, less 1."))

(DEFMETHOD (KBD-MOUSE-BUTTONS-MIXIN :MOUSE-CLICK) (BUTTON ignore ignore)
  ; BUTTON can be character or integer.
  (AND (= BUTTON #\MOUSE-L-1) (NEQ SELF SELECTED-WINDOW)
       (MOUSE-SELECT SELF))
  (SEND SELF :FORCE-KBD-INPUT
           (MERGE-SHIFT-KEYS BUTTON))
  T)

(DEFFLAVOR LIST-MOUSE-BUTTONS-MIXIN () ())

(DEFFLAVOR HYSTERETIC-WINDOW-MIXIN ((HYSTERESIS 25.)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES HYSTERESIS)
  (:DOCUMENTATION :MIXIN "Controls mouse for small area outside of itself too.
The hysteresis instance variable is the number of pixels outside of its own
area within the :handle-mouse method still retain control."))

(DEFMETHOD (HYSTERETIC-WINDOW-MIXIN :HANDLE-MOUSE) ()
  (LET (LEFT-LIM  TOP-LIM
        RIGHT-LIM BOTTOM-LIM)
    (MULTIPLE-VALUE-SETQ (LEFT-LIM TOP-LIM)
      (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
    (SETQ RIGHT-LIM  (+ LEFT-LIM WIDTH  HYSTERESIS)
          BOTTOM-LIM (+ TOP-LIM  HEIGHT HYSTERESIS)
          LEFT-LIM   (- LEFT-LIM HYSTERESIS)
          TOP-LIM    (- TOP-LIM  HYSTERESIS))
    (DO (W) (())
      ;; Let the mouse out of the window only if it moves more than <hysteresis> away
      (AND (OR MOUSE-RECONSIDER
               ;; Also leave if mouse fell into inferior
               (AND (NEQ SELF (SETQ W (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y
                                                                NIL :EXPOSED)))
                    (SHEET-ME-OR-MY-KID-P W SELF))
	       ;; Use unglitched mouse position so that escape is possible when against mouse-sheet edge.
	       ;; CJJ 05/11/88.  Added by KJF for CJJ on 08/16/88 for Multiple Monitor (MMON) support.
               (< *unglitched-mouse-x* left-lim)
               (> *unglitched-mouse-x* right-lim)
               (< *unglitched-mouse-y* top-lim)
               (> *unglitched-mouse-y* bottom-lim))
	   ;;; as was before change for MMON.
;;               (< MOUSE-X LEFT-LIM)
;;               (> MOUSE-X RIGHT-LIM)
;;               (< MOUSE-Y TOP-LIM)
;;               (> MOUSE-Y BOTTOM-LIM))
           (RETURN T))
      (MOUSE-STANDARD-BLINKER SELF)
      (MOUSE-DEFAULT-HANDLER SELF (SEND SELF :ENABLE-SCROLLING-P)))))
