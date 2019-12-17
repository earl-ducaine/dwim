;;; -*- Mode:Common-Lisp; Package:TV; Base:10; Fonts:(CPTFONT CPTFONTB HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;; ** (c) Copyright 1980 by Massachusetts Institute of Technology **


;;;Blinkers

;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 04/18/89   MAY   Change to (MAGNIFYING-BLINKER :BEFORE :BLINK) for non-zero offsets.
;;; 02/23/89   MAY   Changes to (bitblt :blink) and magnifying-blinker code.
;;;                  See comments in code. Made MAC and color compatible. SPR 9266,9271
;;; 02-07-89   LG    Rationalize mX code calling send-drawhollowrectangle to fit new Mac code.
;;; 12/15/88   LG    Reduce width & height by line width in send-drawhollowrectangle calls in
;;; 			(HOLLOW-RECTANGULAR-BLINKER :BLINK), (MAGNIFYING-BLINKER :BEFORE :BLINK) and
;;;			(BOX-BLINKER :BLINK) when on the Mac.
;;; 11/11/88   LG    Use send-drawhollowrectangle in (MAGNIFYING-BLINKER :BEFORE :BLINK) and in
;;;			(BOX-BLINKER :BLINK) when on the Mac.
;;; 10/20/88   LG    Force character blinkers to the mX's screen by calling
;;; 			dump-draw-char-cache in (:method tv:character-blinker :blink) and
;;;			(:method tv:reverse-character-blinker :set-character).
;;;  06/28/88    BJ     Only blink blinkers on the MX when there window channel queue is relatively empty.    
;;; 04/23/88   KJF   Changed main-screen to default-screen in blinker-clock.
;;; 04/10/88   KJF   Change to :before :blink for multiple screen/dual monitor support.
;;; 01/22/88   LG    Allowed hollow rectangular blinkers to be drawn in the margins on a
;;;                  microExplorer, drew them on a microExplorer using the Mac's HollowRectangle
;;;                  facility.  Made FONT and CHARACTER gettablge instance variables in the
;;;                  character-blinker mixin.
;;;10/22/87     KWW   *color-system* -> color-system-p
;;; 08/26/87   KWW   Changes as a result of the code-reading:
;;;                     1. blinker-offset now an instance variable of the blinker rather than the sheet
;;;                     2. removed init code for save-array
;;;                     3. IF => WHEN
;;; 06/22/87   KWW   Solved mouse problem by using alu-add and alu-sub. See defun mouse-alu
;;; 06/08/87   KWW   Added to blinker :init to setup the bit save array for blinkers
;;; 03/23/87   GRH	Add without-interrupts to the mouse-turds fix.
;;; 03/12/87   GRH   Fixed the mouse turds bug: Bind mouse-cursor-state to 1 around :blink method
;;; 01/06/87   KDB    Defined HOLLOW-MARGIN-CHOICE-BLINKER and RECTANGULAR-MARGIN-CHOICE-BLINKER
;;;                     flavors to simplify handling of margin-choice boxes.
;;; 12/22/86   TWE	Made several changes to make the build process cleaner.
;;;			Moved the proclaim inline for open-blinker to be after its definition.
;;; 11/05/86   TWE	Made the reference to COLD-LOAD-STREAM-OWNS-KEYBOARD special in the
;;;			function which uses it.
;;; 10/29/86   TWE	Put the SYS package prefix in for COLD-LOAD-STREAM-OWNS-KEYBOARD.
;;; 10/21/86   LGO	Replace (or (null (sheet-lock w)) (listp (sheet-lock w))) with
;;;			(listp (sheet-lock w)) in BLINKER-CLOCK-INTERNAL.
;;; 08/28/86   TWE	Removed the work done on 7/30/86 since mouse glyphs are now named
;;;			via constants instead of keywords.  This makes life much simpler
;;;			(and a bit faster too).
;;; 08/04/86   TWE	Changed type checks for font to use the FONT-OBJECT-P function.
;;; 07/30/86   TWE	Added support for keywords as mouse characters for CHARACTER-BLINKERs.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 07/28/86   TWE	Modified references to the pixel functions to use ARRAY-DIMENSION
;;;			and MAKE-ARRAY instead.
;;; 07/24/86   TWE	Fixed 2 bugs in BITBLT blinkers.  One where the documentation said
;;;			that the :ARRAY instance variable was settable/gettable but it was
;;;			not.  The defflavor was changed to be correct.  The other where the
;;;			blinker's height was being calculated wrong in the :BLINK method.
;;;			This was fixed to use the height (instead of the width).
;;; 04/09/86   LGO	Move SHEET-OPEN-ALL-BLINKERS and SHEET-OPEN-BLINKERS here from SHEET,
;;;			and make OPEN-BLINKER expand inline for them.  Added code to
;;;			SHEET-OPEN-ALL-BLINKERS to open blinkers for sheet exposed inferiors.
;;; 04/08/86   LGO	Compile code for (:METHOD BLINKER :DEFER-REAPPEARANCE) inline for OPEN-BLINKER


(DEFUN mouse-alu (PHASE)
  (IF phase tv:alu-sub tv:alu-add)
)

(DEFUN MAKE-BLINKER
       (SHEET &OPTIONAL (TYPE 'RECTANGULAR-BLINKER) &REST OPTIONS
        &AUX PLIST BLINKER)
  "Return a new blinker of type TYPE that runs over SHEET.
OPTIONS are init keywords and values for MAKE-INSTANCE."
  (SETQ OPTIONS (COPY-LIST OPTIONS)
	PLIST (LOCF OPTIONS))
  (PUTPROP PLIST SHEET :SHEET)
  (SETQ BLINKER (INSTANTIATE-FLAVOR TYPE PLIST T NIL BLINKER-AREA))
  (WITHOUT-INTERRUPTS
    (PUSH BLINKER (SHEET-BLINKER-LIST SHEET)))
  BLINKER)

(DEFF DEFINE-BLINKER 'MAKE-BLINKER) ;Keep old name for compatibility.
(COMPILER:MAKE-OBSOLETE DEFINE-BLINKER "it has been renamed to TV:MAKE-BLINKER")

(DEFMETHOD (BLINKER :INIT) (IGNORE)
  (OR FOLLOW-P X-POS
      (SETQ X-POS (SHEET-CURSOR-X SHEET)
	    Y-POS (SHEET-CURSOR-Y SHEET))))

(DEFMETHOD (RECTANGULAR-BLINKER :BEFORE :INIT) (IGNORE &AUX FONT)
  (SETQ FONT (AREF (SHEET-FONT-MAP SHEET) 0))
  (OR WIDTH  (SETQ WIDTH  (FONT-BLINKER-WIDTH  FONT)))
  (OR HEIGHT (SETQ HEIGHT (FONT-BLINKER-HEIGHT FONT))))

(DEFMETHOD (RECTANGULAR-BLINKER :SIZE) ()
  (VALUES WIDTH HEIGHT))

(DEFMETHOD (BLINKER :DEFER-REAPPEARANCE) ()
  (SETQ TIME-UNTIL-BLINK 30.))

;;; Make a blinker temporarily disappear from the screen.  Anything that
;;; moves it or changes its parameters should call this.  When the next
;;; clock interrupt happens with INHIBIT-SCHEDULING-FLAG clear, the
;;; blinker will come back on.  This is independent of the time until
;;; next blink, in order to provide the appearance of fast response.
;;; Anyone who calls this should have lambda-bound
;;; INHIBIT-SCHEDULING-FLAG to T.  This is a noop if the sheet the
;;; blinker is on is output held.

(DEFUN OPEN-BLINKER (BLINKER)
  "Take BLINKER temporarily off the screen so it does not interfere
with output.  The blinker will be put back by the scheduler if
appropriate; the caller must turn off interrupts if he expects the
blinker to stay open during any specific piece of code."
  ;; (BLINKER-PHASE is a DEFSUBST in the BLINKER flavor.)
  (WHEN (AND (BLINKER-PHASE BLINKER) 		;If blinker on, turn it off.
	     (NOT (SHEET-OUTPUT-HELD-P (BLINKER-SHEET BLINKER))))
    ;; Make it blink once more before turning it off.
    (BLINK BLINKER)
    (SETF (BLINKER-TIME-UNTIL-BLINK BLINKER) 0))
  ;; Solid blinkers should stay off for a while so they don't flicker
  ;; while the user is typing.
  (WHEN (AND (NEQ (BLINKER-VISIBILITY BLINKER) :BLINK)
	     (< (BLINKER-TIME-UNTIL-BLINK BLINKER) 25.))
    (SETF (blinker-time-until-blink blinker) 30.))
  (WHEN (EQ BLINKER MOUSE-BLINKER)
    (%OPEN-MOUSE-CURSOR)))

(PROCLAIM '(inline open-blinker))  ;; Make OPEN-BLINKER inline for the next few functions


(DEFUN SHEET-OPEN-ALL-BLINKERS-INTERNAL (inferiors)
  ;;Open all blinkers on exposed INFERIORS to all levels.
  (DOLIST (inferior inferiors)
    (DOLIST (BLINKER (SHEET-BLINKER-LIST inferior))
      (OPEN-BLINKER BLINKER))
    (LET ((iinferiors (SHEET-EXPOSED-INFERIORS inferior))) 
      (WHEN iinferiors 
	(SHEET-OPEN-ALL-BLINKERS-INTERNAL iinferiors)))))

(DEFUN SHEET-OPEN-ALL-BLINKERS (FROM-SHEET)
  "Make sure all blinkers that might appear on FROM-SHEET are
temporarily off the screen."
  (DO ((SHEET FROM-SHEET (SHEET-SUPERIOR SHEET)))
      ((OR (NULL SHEET) (NOT (SHEET-EXPOSED-P SHEET))))
    (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
      (OPEN-BLINKER BLINKER)))
  (LET ((inferiors (SHEET-EXPOSED-INFERIORS FROM-SHEET))) 
    (WHEN inferiors 
      (SHEET-OPEN-ALL-BLINKERS-INTERNAL inferiors)))
  )

(DEFUN SHEET-OPEN-BLINKERS (SHEET)
  "Make sure all blinkers belonging to SHEET are temporarily off the screen."
  (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
    (OPEN-BLINKER BLINKER)))

(PROCLAIM '(notinline open-blinker))  ;; OPEN-BLINKER doesn't need to be inline for other functions

(defvar *blink-blinkers-threshold* 5 "Only blink blinkers if there is less than this many items in TV queue.")

;; Only blink the blinker if there is not very much in the tv queue on an mx. *BJ*
(DEFUN BLINKER-CLOCK (BLINKER-DELTA-TIME)
  "This function should get called by the clock about every 60th of a
second.  Any blinkers which are supposed to be on but are off are
turned on.  Any blinkers which are supposed to be flashed are flashed
if it is time.  Note: we depend on the fact that blinkers temporarily
turned off have their BLINKER-TIME-UNTIL-BLINK fields set to 0."
  (DECLARE (SPECIAL BLINKER-DELTA-TIME SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD))
  (let (channel)
    (when (or (not (addin-p))
	      (and (setf channel  (add:find-channel 'display-io))
		   (< (add:q-length (send channel :mx-queue)) *blink-blinkers-threshold*)))
      (DOLIST (S ALL-THE-SCREENS)
	(AND (SHEET-EXPOSED-P S)
	     (OR (NEQ S default-SCREEN)  ;; was main-screen 04/23/88 KJF
		 (NOT SYS:COLD-LOAD-STREAM-OWNS-KEYBOARD))
	     (BLINKER-CLOCK-INTERNAL S))))))

(DEFUN BLINKER-CLOCK-INTERNAL (SHEET)
  (DECLARE (SPECIAL BLINKER-DELTA-TIME))
  (COND ((AND (SHEET-EXPOSED-P SHEET)
	      (ZEROP (SHEET-DONT-BLINK-BLINKERS-FLAG SHEET)))
	 (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
	   (LET ((NEW-TIME (MAX 0 (- (OR (BLINKER-TIME-UNTIL-BLINK BLINKER) 0)
				     BLINKER-DELTA-TIME))))
	     (SETF (BLINKER-TIME-UNTIL-BLINK BLINKER) NEW-TIME)
	     (AND (ZEROP NEW-TIME)
		  (CASE (BLINKER-VISIBILITY BLINKER)
		    ((NIL :OFF)
		     (BLINKER-PHASE BLINKER))
		    ((T :ON)
		     (NULL (BLINKER-PHASE BLINKER)))
		    (:BLINK
		     T))
		  (NOT (SHEET-OUTPUT-HELD-P SHEET))
		  (LET ((LV (SHEET-LOCK SHEET)))
		    (OR (NULL LV)
                        ;; SHEET is a temporary window.
                        (CONSP LV)))
		  (CATCH-ERROR (BLINK BLINKER) NIL))))
	 (AND (EQ SHEET MOUSE-SHEET)
	      (= MOUSE-CURSOR-STATE 1)
	      (= MOUSE-CURSOR-CLOSED-STATE 2)
	      (NEQ WINDOW-OWNING-MOUSE 'STOP)
	      (NOT (SHEET-OUTPUT-HELD-P SHEET))
	      (<= (OR (BLINKER-TIME-UNTIL-BLINK MOUSE-BLINKER) 0) 0)
	      (LISTP (SHEET-LOCK SHEET)) ;; Yes, we really want CONSP or NULL
	      (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
		    PREPARED-SHEET NIL))
	 (DOLIST (S (SHEET-EXPOSED-INFERIORS SHEET))
	   (BLINKER-CLOCK-INTERNAL S)))))

(DEFWRAPPER (BLINKER :BLINK) (IGNORE . BODY)
  `(SHEET-IS-PREPARED (SHEET)
     ;; Don't let the microcode try to XOR the mouse when we're drawing blinkers.  - GRH 3/12/87
     (let-if (neq self mouse-blinker)
	     ((sys:inhibit-scheduling-flag t)
	      (sys:mouse-cursor-state 1))
       . ,BODY)))

;; >> Change to set plane-mask.
;; Also, must set up registers for either color or monochrome.
;; This is important now that we have added multiple screen/dual monitor support.  04/10/88 KJF.
;; may 02/23/89 
;; Using :before/after methods to load the plane mask, fg reg, bg reg
;; is probably the wrong place from a design standpoint - this should
;; have been done in and :around method so that code using the
;; blinker flavor has the option of having methods put INSIDE
;; mask/register handling using :before/:after methods, or, the
;; choice of having methods OUTSIDE of the mask/reg handling in
;; a :wrapper or another :around method. This kludge just makes a 
;; defsusbst so that chnges propagate to BOTH places.
(DEFSUBST before-blinker-blink ()
  (SETQ PREPARED-SHEET NIL)		    ;Blinking any blinker makes us forget
  (SETQ TIME-UNTIL-BLINK HALF-PERIOD)	    ;Schedule the next blink (wink??)
  (WHEN sib-is-csib
    (IF (color-system-p sheet)		    ; load up the registers in preparation for blinking in color
	(PROGN
	  ;; ** NOTE POSSIBLE IMPROVEMENT ** - Use ALU-XOR and *default-blinker-color*. - GRH 8/12/88
	  ;; code here could then be (send ... :set-foreground-color-register
	  ;;                               (logxor *default-blinker-color*
	  ;;                                       (send sheet :background-color)))
	  (send (aref *blocks* 0) :set-foreground-color-register blinker-offset)  ;  add this amount to make a mouse
	  (send (aref *blocks* 0) :set-background-color-register 0))	;  we don't want to affect the background
	(PROGN
	  (send (aref *blocks* 0) :set-foreground-color-register *current-foreground*)  ;; set for B&W operation
	  (send (aref *blocks* 0) :set-background-color-register *current-background*)))
    ;; set plane mask for the plane with blinkers on it.
    ;; LOGAND'ing prevents overridding the screens' plane-mask.  04/10/88 KJF.
    (SEND sheet :write-plane-mask (LOGAND (sheet-plane-mask (get-screen sheet)) (sheet-plane-mask sheet))))
  (AND FOLLOW-P (SETQ X-POS (SHEET-CURSOR-X SHEET)
		      Y-POS (SHEET-CURSOR-Y SHEET))))
;; may 02/23/89 
(DEFMETHOD (BLINKER :BEFORE :BLINK) ()
  (before-blinker-blink))

(DEFMETHOD (BLINKER :AFTER :BLINK) ()
  (SETQ PHASE (NOT PHASE))
  (when (AND sib-is-csib (color-system-p sheet)) ; now restore registers for after color blink
    (sheet-load-registers (blinker-sheet self)) ;; this puts the current sheets foreground and background
  ))                                            ;; back to what they were before the blink operation


(defmethod (blinker :restore-default-colors)()
  (setf blinker-offset *default-blinker-offset*))

(DEFMETHOD (BLINKER :SET-CURSORPOS) (X Y &AUX (OLD-PHASE PHASE))
  "Set the position of a blinker relative to the sheet it is on.  Args in
terms of raster units.  If blinker was following cursor, it will no longer
be doing so."
  (WITH-BLINKER-READY T
    ;; Handle range checking for X and Y.
    (SETQ X (MIN (+ (MAX (FLOOR X) 0) (SHEET-INSIDE-LEFT SHEET))
                 (SHEET-INSIDE-RIGHT SHEET))
	  Y (MIN (+ (MAX (FLOOR Y) 0) (SHEET-INSIDE-TOP SHEET))
                 (SHEET-INSIDE-BOTTOM SHEET)))
    (COND (
           ;; Don't open if visibility NIL (especially the mouse cursor!)
           (NULL VISIBILITY)
	   (SETQ X-POS X
                 Y-POS Y
                 FOLLOW-P NIL))
	  ((OR (NEQ X X-POS)		;Only blink if actually moving blinker
	       (NEQ Y Y-POS))
	   (OPEN-BLINKER SELF)
	   (SETQ X-POS X
                 Y-POS Y
                 FOLLOW-P NIL
                 TIME-UNTIL-BLINK 0)
	   ;; If this is the mouse blinker, and it is not being tracked
           ;; by microcode, then it is important to turn it back on
           ;; immediately.
	   (AND (NEQ VISIBILITY :BLINK)
		OLD-PHASE
		(BLINK SELF))))))

(DEFMETHOD (RECTANGULAR-BLINKER :SET-SIZE) (NWIDTH NHEIGHT)
  (COND ((OR (NOT (= WIDTH  NWIDTH))
	     (NOT (= HEIGHT NHEIGHT)))
	 (WITH-BLINKER-READY ()
	   (SETQ WIDTH NWIDTH HEIGHT NHEIGHT)))))

(DEFMETHOD (RECTANGULAR-BLINKER :SET-SIZE-AND-CURSORPOS) (NWIDTH NHEIGHT X Y)
  "This is like :SET-SIZE and :SET-CURSORPOS together, in order to
prevent the user from seeing the intermediate state.  This prevents
occasional spasticity in menu blinkers, which looks terrible."
  (WITH-BLINKER-READY T
    ;; Handle range checking for X and Y.
    (SETQ X (MIN (+ (MAX (FLOOR X) 0) (SHEET-INSIDE-LEFT SHEET))
                 (SHEET-INSIDE-RIGHT SHEET))
	  Y (MIN (+ (MAX (FLOOR Y) 0) (SHEET-INSIDE-TOP SHEET))
                 (SHEET-INSIDE-BOTTOM SHEET)))
    (COND (
           ;; Don't open if visibility NIL (especially the mouse cursor!)
           (NULL VISIBILITY)
	   (SETQ X-POS X
                 Y-POS Y
                 FOLLOW-P NIL
                 WIDTH NWIDTH
                 HEIGHT NHEIGHT))
	  ((OR (NEQ X X-POS)		;Only blink if actually moving blinker
	       (NEQ Y Y-POS)
	       (NEQ WIDTH  NWIDTH)
	       (NEQ HEIGHT NHEIGHT))
	   (OPEN-BLINKER SELF)
	   (SETQ X-POS X
                 Y-POS Y
                 FOLLOW-P NIL
                 WIDTH NWIDTH
                 HEIGHT NHEIGHT
		 TIME-UNTIL-BLINK 0)))))

(DEFMETHOD (BLINKER :SET-FOLLOW-P) (NEW-FOLLOW-P)
  "Turn on or off whether the blinker follows the sheet's typeout cursor."
  (COND ((NEQ FOLLOW-P NEW-FOLLOW-P)
	 (WITH-BLINKER-READY ()
	   (SETQ FOLLOW-P NEW-FOLLOW-P)))))

(DEFMETHOD (BLINKER :READ-CURSORPOS) ()
  "Returns the position of a blinker in raster units relative to the
margins of the sheet it is on."
  (VALUES (- (OR X-POS (SHEET-CURSOR-X SHEET))
	     (SHEET-INSIDE-LEFT SHEET))
	  (- (OR Y-POS (SHEET-CURSOR-Y SHEET))
	     (SHEET-INSIDE-TOP SHEET))))

(DEFMETHOD (BLINKER :SET-VISIBILITY)
           (NEW-VISIBILITY &AUX (INHIBIT-SCHEDULING-FLAG T))
  "Carefully alter the visibility of a blinker."
  (OR (MEMBER NEW-VISIBILITY '(T NIL :BLINK :ON :OFF) :TEST #'EQ)
      (FERROR NIL "Unknown visibility type ~S" NEW-VISIBILITY))
  (COND ((EQ VISIBILITY NEW-VISIBILITY))
	((EQ PHASE NEW-VISIBILITY)
	 (SETQ VISIBILITY NEW-VISIBILITY))
	(T
	 (DO () ((NOT (SHEET-OUTPUT-HELD-P SHEET)))
	     (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	     (FUNCALL SHEET :OUTPUT-HOLD-EXCEPTION)
	     (SETQ INHIBIT-SCHEDULING-FLAG T))
	 (OR NEW-VISIBILITY (OPEN-BLINKER SELF))
	 (SETQ VISIBILITY NEW-VISIBILITY)
	 ;; Blinker clock will fix the screen
	 (SETQ TIME-UNTIL-BLINK 0))))

(DEFMETHOD (BLINKER :SET-SHEET) (NEW-SHEET &AUX EXCH-FLAG S-SUP S-INF)
  (COND ((NEQ NEW-SHEET SHEET)
	 ;; Only need to turn off blinker if it is turned on
	 (WITH-BLINKER-READY ()
	   (SETF (SHEET-BLINKER-LIST SHEET) (DELETE SELF (THE LIST (SHEET-BLINKER-LIST SHEET)) :TEST #'EQ))
	   (PUSH SELF (SHEET-BLINKER-LIST NEW-SHEET))
	   (IF (SHEET-ME-OR-MY-KID-P SHEET NEW-SHEET)
	       (SETQ S-SUP NEW-SHEET
		     S-INF SHEET
		     EXCH-FLAG 1)
	       (SETQ S-SUP SHEET
		     S-INF NEW-SHEET
		     EXCH-FLAG -1))
	   (COND ((OR (= EXCH-FLAG 1)
		      (SHEET-ME-OR-MY-KID-P S-INF S-SUP))
		  (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
		      (SHEET-CALCULATE-OFFSETS S-INF S-SUP)
		    (SETQ X-POS
			  (MIN (MAX 0 (+ X-POS (* EXCH-FLAG X-OFF)))
			       (1- (SHEET-WIDTH NEW-SHEET))))
		    (SETQ Y-POS
			  (MIN (MAX 0 (+ Y-POS (* EXCH-FLAG Y-OFF)))
			       (1- (SHEET-HEIGHT NEW-SHEET))))))			
		 (T
		  ;; The sheets aren't related so directly, just put the
                  ;; blinker in the middle.
		  (SETQ X-POS (TRUNCATE (SHEET-WIDTH  NEW-SHEET) 2)
			Y-POS (TRUNCATE (SHEET-HEIGHT NEW-SHEET) 2))))
	   (SETQ SHEET NEW-SHEET)))))

(DEFMETHOD (RECTANGULAR-BLINKER :BLINK) ()
  "Standard style, rectangular blinker."
  ;; Should this insure blinker in range?
  (%DRAW-RECTANGLE-CLIPPED
    WIDTH HEIGHT
    (MAX
      ;; Insure that the blinker doesn't go too far to the left.
      (TRUNCATE WIDTH -2)
      ;; Insure that the blinker doesn't go too far to the right.
      (MIN (- (SHEET-WIDTH SHEET) (TRUNCATE WIDTH 2)) X-POS))
    (MAX
      ;; Insure that the blinker doesn't go up past the top.
      (TRUNCATE HEIGHT -2)
      ;; Insure that the blinker doesn't go down past the bottom.
      (MIN (- (SHEET-HEIGHT SHEET) (TRUNCATE HEIGHT 2)) Y-POS))
    (MOUSE-ALU PHASE)
    SHEET))

(DEFFLAVOR HOLLOW-RECTANGULAR-BLINKER () (RECTANGULAR-BLINKER)
  (:DOCUMENTATION
    :COMBINATION "A flavor of blinker that appears as a rectangular outline."))

;;; two new flavors to make margin choice box code cleaner
(DEFFLAVOR HOLLOW-MARGIN-CHOICE-BLINKER () (HOLLOW-RECTANGULAR-BLINKER )
  (:DOCUMENTATION
    :COMBINATION "A flavor of blinker that appears as a hollow rectangular outline in margin regions."))

(DEFFLAVOR RECTANGULAR-MARGIN-CHOICE-BLINKER () (RECTANGULAR-BLINKER )
  (:DOCUMENTATION
    :COMBINATION "A flavor of blinker that appears as a solid rectangular outline in margin regions."))

;;; This sticks out by 1 pixel on the top and left but not on the bottom
;;; and right since that seems to be the right thing for boxing text --
;;; this may be a crock.
(DEFMETHOD (HOLLOW-RECTANGULAR-BLINKER :BLINK) ()
  (LET ((-X-POS- (1- X-POS)) (-Y-POS- (1- Y-POS)) (-HEIGHT- (1+ HEIGHT)) (-WIDTH- (1+ WIDTH)))
    (IF (mac-window-p sheet)
	(LET ((*dont-clip-at-the-margins* t))
	  (send-DrawHollowRectangle -x-pos- -y-pos- -width- -height- 1
				    w:black (MOUSE-ALU PHASE) sheet))
      ;; else...
      (%DRAW-RECTANGLE-CLIPPED 1 -HEIGHT-
			       -X-POS-       -Y-POS- (MOUSE-ALU PHASE) SHEET)
      (%DRAW-RECTANGLE-CLIPPED (- -WIDTH- 1) 1
			       (+ -X-POS- 1) -Y-POS- (MOUSE-ALU PHASE)  SHEET)
      (%DRAW-RECTANGLE-CLIPPED 1 (- -HEIGHT- 1)
			       (+ -X-POS- -WIDTH- -1) (+ -Y-POS- 1)
			       (MOUSE-ALU PHASE) SHEET)
      (%DRAW-RECTANGLE-CLIPPED (- -WIDTH- 2) 1
			       (+ -X-POS- 1) (+ -Y-POS- -HEIGHT- -1)
			       (MOUSE-ALU PHASE) SHEET))))

(DEFFLAVOR BOX-BLINKER () (RECTANGULAR-BLINKER)
  (:DOCUMENTATION
    :COMBINATION
    "A flavor of blinker that appears as a thick rectangular outline."))

(DEFMETHOD (BOX-BLINKER :BLINK) ()
  (IF (mac-window-p sheet)
      (LET ((*dont-clip-at-the-margins* t))
	(send-drawhollowrectangle x-pos y-pos
				  width height 2
				  w:black (mouse-alu phase) sheet))
      ;; else...
      (%DRAW-RECTANGLE-CLIPPED
	2 HEIGHT X-POS                          Y-POS (MOUSE-ALU PHASE) SHEET)
      (%DRAW-RECTANGLE-CLIPPED
	(- WIDTH 2) 2 (+ X-POS 2)               Y-POS (MOUSE-ALU PHASE) SHEET)
      (%DRAW-RECTANGLE-CLIPPED
	2 (- HEIGHT 2) (+ X-POS WIDTH -2) (+ Y-POS 2) (MOUSE-ALU PHASE) SHEET)
      (%DRAW-RECTANGLE-CLIPPED
	(- WIDTH 4) 2 (+ X-POS 2) (+ Y-POS HEIGHT -2) (MOUSE-ALU PHASE) SHEET)))

;Mixin that causes a blinker to stay entirely inside its sheet
(DEFFLAVOR STAY-INSIDE-BLINKER-MIXIN () ()
  (:REQUIRED-FLAVORS RECTANGULAR-BLINKER)
  (:DOCUMENTATION
    :MIXIN
    "This mixin prevents lower right corner of blinker from being
outside the sheet."))

(DEFWRAPPER (STAY-INSIDE-BLINKER-MIXIN :SET-CURSORPOS) (XY . BODY)
  `(PROGN
     (SETF (FIRST XY) (MIN (FIRST XY) (- (SHEET-INSIDE-WIDTH SHEET) WIDTH)))
     (SETF (SECOND XY) (MIN (SECOND XY) (- (SHEET-INSIDE-HEIGHT SHEET) HEIGHT)))
     . ,BODY))

(DEFFLAVOR IBEAM-BLINKER
	((HEIGHT NIL))
	(BLINKER)
  (:INITABLE-INSTANCE-VARIABLES HEIGHT)
  (:DOCUMENTATION :COMBINATION "A blinker that appears as an I-beam."))

(DEFMETHOD (IBEAM-BLINKER :BEFORE :INIT) (IGNORE)
  (OR HEIGHT (SETQ HEIGHT (SHEET-LINE-HEIGHT SHEET))))

(DEFMETHOD (IBEAM-BLINKER :SIZE) ()
  (VALUES 9. HEIGHT))

(DEFMETHOD (IBEAM-BLINKER :BLINK) (&AUX X0)
  (%DRAW-RECTANGLE-CLIPPED 2 HEIGHT (MAX 0 (1- X-POS)) Y-POS (MOUSE-ALU PHASE) SHEET)
  (SETQ X0 (MAX 0 (- X-POS 4)))
  (%DRAW-RECTANGLE-CLIPPED (- (+ X-POS 5) X0) 2 X0
                           (MAX 0 (- Y-POS 2)) (MOUSE-ALU PHASE) SHEET)
  (%DRAW-RECTANGLE-CLIPPED (- (+ X-POS 5) X0) 2 X0
                           (+ Y-POS HEIGHT)    (MOUSE-ALU PHASE) SHEET))

;; Interchange of CHAR and CHARACTER not patched in 93.
(DEFFLAVOR CHARACTER-BLINKER
	(FONT
	 CHARACTER)
	(BLINKER)
  (:INITABLE-INSTANCE-VARIABLES FONT CHARACTER)
  (:GETTABLE-INSTANCE-VARIABLES FONT CHARACTER)
  (:INIT-KEYWORDS :CHAR)
  (:DOCUMENTATION
    :COMBINATION "A blinker whose appearance is a character from a font."))

(DEFMETHOD (CHARACTER-BLINKER :BEFORE :INIT) (PLIST)
  (UNLESS (VARIABLE-BOUNDP CHARACTER)
    (SETQ CHARACTER (GET PLIST :CHAR)))
  (SETQ FONT (FUNCALL (SHEET-GET-SCREEN SHEET) :PARSE-FONT-SPECIFIER FONT)))

(DEFMETHOD (CHARACTER-BLINKER :SIZE) ()
  (VALUES (SHEET-CHARACTER-WIDTH SHEET CHARACTER FONT)
          (FONT-BLINKER-HEIGHT FONT)))

(DEFMETHOD (CHARACTER-BLINKER :BLINK) ()
  "Use a character as a blinker.  Any font, any character."
  (DRAW-CHAR FONT CHARACTER X-POS Y-POS (MOUSE-ALU PHASE) SHEET)
  (WHEN (mac-window-p sheet)
    (dump-draw-char-cache)))

(DEFMETHOD (CHARACTER-BLINKER :SET-CHARACTER) (NCHAR &OPTIONAL (NFONT FONT))
  "Change the character or font of a blinker.  Use
tv:mouse-set-blinker-definition to change the mouse blinker."
  (SETQ NFONT (FUNCALL (SHEET-GET-SCREEN SHEET) :PARSE-FONT-SPECIFIER NFONT))
  (WHEN (OR (NEQ NCHAR CHARACTER)
	    (NEQ NFONT FONT))
    (WITH-BLINKER-READY NIL
      (SETQ CHARACTER NCHAR FONT NFONT))))

(DEFMETHOD (CHARACTER-BLINKER :CHARACTER) () (VALUES CHARACTER FONT))

(DEFFLAVOR BITBLT-BLINKER
	((WIDTH NIL)
	 (HEIGHT NIL)
	 (ARRAY NIL))
	(MOUSE-BLINKER-MIXIN BLINKER)
  :INITABLE-INSTANCE-VARIABLES
  (:SETTABLE-INSTANCE-VARIABLES ARRAY)
  (:GETTABLE-INSTANCE-VARIABLES ARRAY)
  (:DOCUMENTATION
    :COMBINATION
    "A blinker whose appearance is made by BITBLTing an array."))

(DEFMETHOD (BITBLT-BLINKER :BEFORE :INIT) (IGNORE)
  (UNLESS ARRAY
    (UNLESS (AND WIDTH HEIGHT)
      (FERROR
        NIL
 "Attempt to create a BITBLT-BLINKER without specifying an array or its size."))
    (SETQ ARRAY (MAKE-ARRAY `(,HEIGHT ,(* 32. (CEILING WIDTH 32.)))
				  :TYPE (SHEET-ARRAY-TYPE SHEET))))
  (IF (NULL WIDTH)
      (SETQ WIDTH  (ARRAY-DIMENSION ARRAY 1)))
  (IF (NULL HEIGHT)
      (SETQ HEIGHT (ARRAY-DIMENSION ARRAY 0))))

(DEFMETHOD (BITBLT-BLINKER :SIZE) ()
  (VALUES WIDTH HEIGHT))

(DEFMETHOD (BITBLT-BLINKER :SET-SIZE) (NWIDTH NHEIGHT)
  (UNLESS (AND (>= (ARRAY-DIMENSION ARRAY 1) NWIDTH) (>= HEIGHT NHEIGHT))
    (SETQ ARRAY (MAKE-ARRAY `(,NHEIGHT ,(* 32. (CEILING NWIDTH 32.)))
				  :TYPE (SHEET-ARRAY-TYPE SHEET))))
  (SETQ WIDTH NWIDTH HEIGHT NHEIGHT))

;; may 02/23/89 Rewrote to use sheet not screen for
;; width and simplified. SPR 9266-9271
(DEFMETHOD (BITBLT-BLINKER :BLINK) ()
  (LET* ((SCREEN-ARRAY (SHEET-SCREEN-ARRAY SHEET))
	 (SWIDTH  (tv:sheet-inside-right  sheet))
	 (SHEIGHT (tv:sheet-inside-bottom sheet)))
    (BITBLT (MOUSE-ALU PHASE)
	    width height
	    ARRAY
	    0 0	
	    SCREEN-ARRAY
	    (MIN x-pos (- swidth width))
	    (MIN y-pos (- sheight height)))))

(DEFFLAVOR MAGNIFYING-BLINKER
	   ((MAGNIFICATION 3.)
	    (local-copy nil)) ;; may 02/22/89 added
	   (BITBLT-BLINKER)
  (:SETTABLE-INSTANCE-VARIABLES))

;; may 02/23/89 NEW
;; The size of this array needs only be approximately
;; (/ nwidth magnification) by (/ nheight magnification)
;; but this is safer and does not require more code to
;; check when the magnification factor is changed.
(DEFSUBST check-local-copy-size (nwidth nheight)
  "Internal function for MAGNIFYING-BLINKER."
  (WHEN (OR (NULL local-copy)
	    (> nwidth (ARRAY-DIMENSION local-copy 1))
	    (> nheight (ARRAY-DIMENSION local-copy 0)))
    (SETF local-copy (w:make-sheet-bit-array sheet nwidth nheight))))

(DEFMETHOD (MAGNIFYING-BLINKER :AFTER :INIT) (IGNORE)
  (UNLESS (AND (ZEROP (REM HEIGHT MAGNIFICATION))
	       (ZEROP (REM WIDTH  MAGNIFICATION)))
    (FERROR
      NIL
      "Height (~D) and width (~D) are not multiples of magnification (~D)."
      HEIGHT WIDTH MAGNIFICATION))
  (check-local-copy-size height width)) ;; may 02/23/89 

;; may 02/22/89 NEW
(DEFMETHOD (MAGNIFYING-BLINKER :after :SET-SIZE) (NWIDTH NHEIGHT)
  ;; Make sure local-copy array is big enough, too
  (check-local-copy-size nheight nwidth))


;; may 02/23/89 NEW
;; TURN off mouse blinker so that it does not get magnified and 
;; make the magnified image harder to read.
(DEFWRAPPER (MAGNIFYING-BLINKER :BLINK) (IGNORE . BODY)
  `(LET ((INHIBIT-SCHEDULING-FLAG T))
     (open-blinker tv:mouse-blinker)     
     . ,BODY))

;; may 02/23/89 Rewrite.
;; Sometimes ... (moving this blinker in the tv:inspect-window-with-typeout while 
;; doing a ctr-r refresh in inspector) ...
;; rectangle turds are left on the screen. This is because the (BLINKER :BEFORE :BLINK) method has not
;; yet had a chance to load the fg/bg reg's and set the plane mask.
;; For this reason we must include the guts of (BLINKER :BEFORE :BLINK) in this method !
;;    may 04/18/89 ******    changed calc of from-x & from-y to use -x-pos- & -y-pos-
;; since using x-pos & y-pos caused cold load stream bitblt error *ONLY* when 
;; ofsets where non-zero. Also removed unnecessary fill of local-copy.
(DEFMETHOD (MAGNIFYING-BLINKER :BEFORE :BLINK) ()
  ;; (:method BLINKER :BEFORE :BLINK) is called AFTER (:method MAGNIFYING-BLINKER :BEFORE :BLINK)
  ;; But we need to change the order. This is impossible without major changes
  ;; so we will just call (BLINKER :BEFORE :BLINK) - or its guts (before-blinker-blink) - twice.
  ;; Once now and once in (BLINKER :BEFORE :BLINK) - only the first call is necessary
  ;; and the second call will not hurt.
  (before-blinker-blink) ;; may 02/22/89 
  (LET* ((SCREEN-ARRAY (SHEET-SCREEN-ARRAY SHEET))
	 (SWIDTH  (tv:sheet-inside-right sheet)) 	;; may 02/21/89 swidth is NOT a width but sheet-inside-right!
	 (SHEIGHT (tv:sheet-inside-bottom sheet))	;; may 02/21/89
	 (-x-pos- (MIN x-pos (- swidth width)))		;; max cursor x-pos inside window
	 (-y-pos- (MIN y-pos (- sheight height)))	;; max cursor y-pos inside window
	 ;; FROM-xxxx variables are used to filter out 0 width/height bitblts that
	 ;; mac complains about.
	 (from-x (+ (TRUNCATE (- x-offset) magnification) x-offset -x-pos-)) ;; may 04/18/89 
	 (from-y (+ (TRUNCATE (- y-offset) magnification) y-offset -y-pos-)) ;; may 04/18/89 
	 (from-width (MIN (CEILING width magnification) (- swidth from-x)))
	 (from-height (MIN (CEILING height magnification) (- sheight from-y)))
	 (background-color (IF (color-system-p sheet)
			       (tv:sheet-background-color sheet)
			     w:white)))
    ;; OK, what we are doing is this :
    ;; 1. The source-array can be the screen-array on the Explorer
    ;;    but it must be a copy of the screen for the MAC - it does
    ;;    not support direct access to its screen array.
    ;; 2. magnify the source-array into instance variable ARRAY
    ;; 3. BITBLT source-array to ARRAY using alu-sub. This will merge
    ;;    the magnified image with a reverse immage of the screen so
    ;;    that when the :blink method blt's ARRAY to the screen the
    ;;    former text will be wiped out and the magnified image will
    ;;    be all that remains. NOTE: on a b&w monitor either mouse-alu
    ;;    alu-add or alu-sub gets converted to alu-xor. On a color system
    ;;    we MUST get the alu-sub. Thus we use (mouse-alu (not phase)).
    (WHEN (AND (NULL phase) (NOT (ZEROP from-height)) (NOT (ZEROP from-width)))
      ;; Since aref's of the screen are too expensive on MAC - just
      ;; blt the screen array portion into local copy and then magnify from local-copy.
      (BITBLT tv:alu-seta from-width from-height
	      screen-array from-x from-y
	      local-copy 0 0)
      ;; Init array for speed to background color, then change any element /= to background
      (ARRAY-INITIALIZE ARRAY background-color) ;; may 02/21/89 
      (WHEN SCREEN-ARRAY
	(LOOP with tem
	      for i from 0 
	      for i1 from 0 below width by magnification
	      do
	      (LOOP for j from 0
		    for j1 from 0 below height by magnification
		    do
		    (WHEN (/= background-color (SETQ tem (AREF local-copy j i)))
		      (DOTIMES (i2 magnification)
			(DOTIMES (j2 magnification)
			  (SETF (AREF array (+ j1 j2) (+ i1 i2)) tem))))))
	;; may 02/22/89 Changed similarly to (:method BITBLT-BLINKER :BLINK)
	;; Do not go into margins with blinker - necessary for magnifing blinker
	;; since areas near margins cannot be magnified-viewed.
	;; Changed to arg to mouse-alu from PHASE to T - phase is nil here.
	;; This makes color work better.
	(BITBLT (MOUSE-ALU (NOT PHASE)) ;; may 02/23/89 
		width height	
		SCREEN-ARRAY
		-x-pos- 
		-y-pos- 
		ARRAY
		0 0	
		)))
    ;; Make a box AROUND the whole thing so it can be found if screen is
    ;; blank under it.
    ;; We ALWAYS draw the rectangle on the screen.
    (LET ((-X-POS-  (+ -X-POS- -1)) ; note -x-pos- 
	  (-Y-POS-  (+ -Y-POS- -1)) ; note -y-pos- 
	  (-HEIGHT- (+ 2 HEIGHT))
	  (-WIDTH-  (+ 2 WIDTH)))
      (IF (mac-window-p sheet)
	  ;; *dont-clip-at-the-margins* T is probably not needed
	  ;; since blinker does NOT go into margins anymore ?
	  (LET ((*dont-clip-at-the-margins* t)) 
	    (send-drawhollowrectangle -x-pos- -y-pos- -width- -height-
				      1 w:black (mouse-alu phase) sheet))
	  ;; else...
	  (%DRAW-RECTANGLE-CLIPPED 1 -HEIGHT-
				   -X-POS-       -Y-POS- (MOUSE-ALU PHASE) SHEET)
	  (%DRAW-RECTANGLE-CLIPPED (- -WIDTH- 1) 1
				   (+ -X-POS- 1) -Y-POS- (MOUSE-ALU PHASE) SHEET)
	  (%DRAW-RECTANGLE-CLIPPED 1 (- -HEIGHT- 1)
				   (+ -X-POS- -WIDTH- -1) (+ -Y-POS- 1)
				   (MOUSE-ALU PHASE) SHEET)
	  (%DRAW-RECTANGLE-CLIPPED (- -WIDTH- 2) 1
				   (+ -X-POS- 1) (+ -Y-POS- -HEIGHT- -1)
				   (MOUSE-ALU PHASE) SHEET)))))

(DEFMETHOD (MAGNIFYING-BLINKER :DEFER-REAPPEARANCE) () NIL)

(DEFFLAVOR REVERSE-CHARACTER-BLINKER
	((CHARACTER NIL)
	 (FONT T)
	 (CHARACTER-X-OFFSET 0)
	 (CHARACTER-Y-OFFSET 0))
	(BITBLT-BLINKER)
  :INITABLE-INSTANCE-VARIABLES
  (:GETTABLE-INSTANCE-VARIABLES
    ;; CHARACTER             ; We explicitly define a method for this operation
    FONT
    CHARACTER-X-OFFSET
    CHARACTER-Y-OFFSET )
  (:DOCUMENTATION
    :COMBINATION
    "A blinker whose appearance is a character from a font against a
solid rectangular background."))

(DEFMETHOD (REVERSE-CHARACTER-BLINKER :BEFORE :INIT) (IGNORE)
  (IF (NULL CHARACTER) (FERROR NIL "You must specify a character"))
  (SEND SELF :SET-CHARACTER NIL))

(DEFMETHOD (REVERSE-CHARACTER-BLINKER :SET-CHARACTER)
           (NEW-CHARACTER &OPTIONAL NEW-FONT)
  (IF NEW-CHARACTER (SETQ CHARACTER NEW-CHARACTER))
  (IF NEW-FONT (SETQ FONT NEW-FONT))
  (SETQ FONT (IF (EQ FONT T)
		 (SHEET-CURRENT-FONT SHEET)
               ;;ELSE
               (FUNCALL (SHEET-GET-SCREEN SHEET) :PARSE-FONT-SPECIFIER FONT)))
  (SETQ WIDTH (LET ((CWT (FONT-CHAR-WIDTH-TABLE FONT)))
		(IF CWT
                    (AREF CWT CHARACTER)
                  ;;ELSE
                  (FONT-CHAR-WIDTH FONT))))
  (SETQ HEIGHT (FONT-BLINKER-HEIGHT FONT))
  (SETQ ARRAY (MAKE-SHEET-BIT-ARRAY SHEET WIDTH HEIGHT))
  (SETQ X-OFFSET
	(- (LET ((LKT (FONT-LEFT-KERN-TABLE FONT)))
	     (IF LKT
                 (AREF LKT CHARACTER)
               ;;ELSE
               0))))
  (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 ALU-IOR ARRAY)
  (DRAW-CHAR FONT CHARACTER
             CHARACTER-X-OFFSET CHARACTER-Y-OFFSET ALU-ANDCA ARRAY)
  (WHEN (mac-window-p sheet)
    (dump-draw-char-cache)))

(DEFMETHOD (REVERSE-CHARACTER-BLINKER :CHARACTER) () (VALUES CHARACTER FONT))

(DEFMETHOD (REVERSE-CHARACTER-BLINKER :SET-SIZE) (NEW-WIDTH NEW-HEIGHT)
  NEW-WIDTH NEW-HEIGHT
  ;no can do
  NIL)

;;; Kludgiferous stuff for dealing with the kludges involved in the
;;; font-map system.  these functions will make a change in a font (e.g.
;;; by loading or fed) propagate to all the places that the font is in
;;; use.  The reason why the changes don't propagate normally is that
;;; the font maps are consist of the actual font structures rather than
;;; symbols like 'FONTS:CPTFONT.
(DEFUN UPDATE-FONT (FONT)
  (IF (font-object-p font)
      (FONT-EVALUATE (FONT-NAME FONT))
    FONT))

(DEFUN UFM (WINDOW)
  (LET* ((OLD-FONT-MAP       (SEND WINDOW :FONT-MAP))
	 (FONT-MAP-FONT-LIST (FONT-MAP-FONT-LIST         OLD-FONT-MAP))
	 (CURRENT-FONT       (FONT-MAP-CURRENT-FONT-NAME OLD-FONT-MAP))
	 TEM)
    (SETQ FONT-MAP-FONT-LIST
	  (MAPCAR 'UPDATE-FONT FONT-MAP-FONT-LIST))
    (UNLESS CURRENT-FONT
      (SETQ CURRENT-FONT (SEND WINDOW :CURRENT-FONT)))
    (SETQ CURRENT-FONT (COND ((NUMBERP CURRENT-FONT)
			      CURRENT-FONT)
			     ((MEMBER CURRENT-FONT FONT-MAP-FONT-LIST :TEST #'EQ)
			      CURRENT-FONT)
			     ((MEMBER (FONT-NAME CURRENT-FONT) FONT-MAP-FONT-LIST :TEST #'EQ)
			      (FONT-NAME CURRENT-FONT))
			     ((SETQ TEM (POSITION CURRENT-FONT OLD-FONT-MAP))
			      TEM)
			     (T (UPDATE-FONT CURRENT-FONT))))
    (SEND WINDOW :SET-FONT-MAP FONT-MAP-FONT-LIST)
    (SEND WINDOW :SEND-IF-HANDLES :SET-DEFAULT-FONT
		 (UPDATE-FONT (OR (SEND WINDOW :SEND-IF-HANDLES :DEFAULT-FONT)
				  (SEND (SHEET-GET-SCREEN WINDOW) :PARSE-FONT-SPECIFIER
								  :DEFAULT))))
    (SEND WINDOW :SET-CURRENT-FONT CURRENT-FONT T))
  (DOLIST (I (SEND WINDOW :INFERIORS))
    (UFM I)))

(DEFUN UPDATE-FONT-MAPS ()
  "Update all the font maps in the window system.  This is a good
thing to call after loading a loading a new version of an old font.  IT
makes sure that all windows use the new copy."
  (DOLIST (SCREEN ALL-THE-SCREENS)
    (LET ((INFERIORS (SEND SCREEN :INFERIORS))
	  (FONT-ALIST (SEND SCREEN :FONT-ALIST))
	  )
      (DOLIST (TEM FONT-ALIST)
	(SET (CDR TEM) (UPDATE-FONT (SYMBOL-VALUE (CDR TEM)))))
      (UFM SCREEN)
      (DOLIST (WINDOW INFERIORS)
	(UFM WINDOW)))))
