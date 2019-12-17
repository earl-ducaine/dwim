;;; -*- Mode:Common-Lisp; Package:TV; Base:10; Fonts:(CPTFONT CPTFONTB) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated.  All rights reserved.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

 
;;; --------------------------------------------------
;;; Routines to manipulate the Explorer's TI SN76496 sound-generator chip
;;; --------------------------------------------------

;; This file contains the BEEP and speech functions.


;;; Change history:
;;;
;;;  Date      Author Description
;;; -------------------------------------------------------------------------------------
;;; 06/01/88   BJ     Change the sound channel constant.
;;; 01.12.88   MBC	Conditionalize on si:resource-present-p and :SOUND
;;; 04/13/87   LGO	Workaround a network bug in READ-ARRAY.
;;; 01/21/87	 LGO	Modified WITH-REAL-TIME to increase the process priority
;;; 12/22/86	 LGO	Fixed a bug where the REC loopback option lost the sound-enabled bit.
;;; 12/04/86   TWE	Changed references to copy-array-contents to use replace.
;;; 11/24/86   LGO	Change SAVE-SPEECH to write binary files (don't loose on character files)
;;;			Change READ-SPEECH to use buffered streams (5x faster)
;;;			Change PLAY so a nil speech name forces a read every time (less memory)
;;; 11/21/86   LGO	Changed character comparison in READ-SPEECH from EQ to CHAR=
;;; 11/03/86   TWE	Deleted the following form which is for CONTROL-G.
;;;			(DEF-BEEP-TYPE TELNET:TERMINAL-BEEP :VT100-BEEP)
;;; 10/29/86   TWE	Changed GET-SOUND-ARRAY to use SYMBOL as an argument to CHECK-TYPE
;;;			instead of :SYMBOL.
;;; 10/23/86   DAN	Fixed keyword args to MENU-CHOOSE in SOUND-MENU.
;;; 10/21/86   LGO	Add WITH-REAL-TIME call around BEEP.
;;;			Added *sound-disable-interrupts* flag to WITH-REAL-TIME.
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 04/09/86   LGO	Allow the BEEP global variable to be used as the default beep-type.
;;; 03/24/86   LGO	Merged cold-load sound functions into this file.
;;;			INITIALIZE-SOUND moved from the SI to the TV package.
;;;			*MONITOR-SOUND-ENABLED* moved from the SI to the TV package.
;;; 03/07/86   LGO	Modify WITH-SOUND-ENABLED to call reset-sound to ensure the sound
;;;			is off in the headset (the sib-sound-bit doesn't turn off the headset)
;;; 03/07/86   LGO	 Add *monitor-sound-enabled* flag to allow disabling the speaker while using the headset.
;;;			 Combine si:initialize-sound and tv:reset-sound.
;;; 03/03/86   LGO	Minor changes done while code reading - code read by TWE.
;;; 02/24/86   LGO	Moved here from SHWARM with the following changes:
;;;			1. Added compiler optimizations for volume tone noise and wait
;;;			2. Added WITH-REAL-TIME macro from the old HACKS files, and
;;;			   use this instead of WITHOUT-INTERRUPTS for the beeping functions.
;;;			3. It used to be that BEEP looked up the function to do the
;;;			   beep on *BEEP-ALIST*.  This was changed to seperate the concept of
;;;			   why the beep is being done and the sound the beep makes.
;;;			   A new macro DEF-BEEP-FUNCTION defines a function and parameters
;;;			   to make a sound.  The macro DEF-BEEP-TYPE was changed to associate
;;;			   a reason for doing a beep (e.g. zwei:no-completion) with a sound (beep-function).
;;;			   This is done using the :beep-type property on the beep-type and beep-function
;;;			   symbols *BEEP-ALIST* was eliminated.
;;;			4. DEF-BEEP-TYPE was changed incompatably, and REMOVE-BEEP-TYPE was eliminated.
;;;			5. Added two new variables: *beeping-functions* and *beep-types*
;;;			6. Moved the FLASH-DURATION variable to SOUND from TVDEFS.
;;;			7. Added the DEFSOUND macro
;;;			8. Moved the beeping code out of beep into do-beep.
;;;			9. Added beep-stop-flash to stop flashing on long beeps.
;;;		       10. Renamed DEFAULT-BEEP to SIMPLE-BEEP, and added the capability
;;;			   to sound multiple tones and control loudness.
;;;		       11. Defined the following beep functions:
;;;			   :beep :pip :beep-high :beep-low :vt100-beep :bo :doorbell :whoop
;;;			   :shoop :random-beep :flying-saucer :flash
;;;		       12. Defined a new area for sound and speech data called SOUND-AREA
;;;		       13. Added functions for recording, playing saving and restoring speech
;;;		       14. Added the SOUND-MENU function.
;;;		       15. See the DEMO directory for additional beeping functions.
;;; 02/24/86   LGO	*Modified TONE to return a 16 bit value instead of a cons of 2 8 bit values.
;;;			 Added WAIT which causes do-sound to wait.  Modified do-sound to accept
;;;		  	 the results from TONE and WAIT.  These changes allow a byte stream fed to do-sound
;;;			 do perform all sound functions.
;;;			*The delay parameter to DO-SOUND is now milliseconds instead of microseconds.
;;;			 Real people can't hear microsecond timing changes.
;;;			*Modified DELAY to look at the clock instead of blindly looping.
;;;			*Modified VOLUME to make the default :on attuenation be 2 instead of 0.
;;;			 This makes it possable to do louder than normal sounds in unusual cases.
;;;			*TONE no-longer returns the internal frequency number as a second value.
;;;			*TONE and TONE-FREQUENCY no-longer take the clock-rate as an optional parameter,
;;;			 they get it from the constant tv:sound-clock-rate.



;;; --------------------------------------------------
;;; Description of the Texas Instruments SN76496 sound chip
;;; (implementation independent)
;;; --------------------------------------------------

#|
	The SN76496 sound chip has 3 tone generators (registers 0..2) with 1024 levels 
of frequency and 16 levels of volume.  The exact frequencies produced depend on the 
clock rate of the chip.

	There is also a noise generator (register 3), providing for periodic or white noise,
with 3 frequencies of hiss.  1024 frequency levels of hiss are possible by using register 2.

	The SN76496 sound chip accepts three types of commands: 

	- frequency:    1rr0ffff - byte1
			       ^
			       lsb of freq. value
			0xffffff - byte2
			  ^
			  msb of freq. value
		"rr" is the register field and selects which tone generator will be affected;
		values are 0..2 for the tones and 3 for noise

		The frequency field can take any of 1024 values.  The value gets split
		between two consecutive bytes, with the lower 4 bits going into byte1 and
		the most significant bits into byte2 as shown.  This value is related
		to frequency in hertz by the formula:

			internal-value = clock-rate/(32*freq)

		where "clock-rate" and "freq" are both in hertz.  High values
		correspond to low frequencies and vice versa.  For a 2.048 MHz chip, the
		lowest frequency that can be produced is 62.5 Hz.

		It is possible, after having once sent both frequency bytes to the chip,
		to continue sending "byte2"'s.  This provides one way to produce a frequency
		sweep at a slight cost in resolution.

	- volume:	1rr1aaaa - byte

		"rr" is same as for frequency

		The attenuator field takes any of 16 values.  0 is the loudest, 14 is the
		softest, and 15 is off.  Each step is 2 db softer than the one before it.

	- noise:	1rr0xfnn - byte

		"rr" = 11 (binary)

		The "f" bit is 0 for periodic noise and 1 for white noise.

		"nn" is the noise frequency field.  Its values range from 0 for
		the least coarse hiss (higher pitch) to 2 for most coarse (lower pitch).
		If the value is 3 then the pitch is controlled by register 2 (the third 
		tone generator). It may be desirable to turn off the volume to 
		register 3 in that case, although the combined tones are consonant.

		There is one annoyance that seems not to be documented anywhere.
		If a noise command is given to the chip, it turns off the
		noise generator's volume.  Attempting to set the volume bit
		in the same command doesn't work.

       - wait:		11101nnn - byte
			       ^
			       lsb of time value
			0fffffff - byte2
			  ^
			  msb of time value
		The hardware doesn't do time delays, but the function that sends bytes
		to the hardware (do-sound) catches these values and does a time delay.
|# 


;;; --------------------------------------------------
;;; Very low-level routines that touch the hardware
;;; --------------------------------------------------

;;; This also includes DELAY and WITH-SOUND-ENABLED so things compile in order.

(DEFVAR *monitor-sound-enabled* t "Set to NIL to turn off the monitor speaker, leaving sound enabled for the headset.")

(si:define-when :SOUND

;;; This is the only function that knows the location of the SIB sound bit. 
(DEFUN (:cond (NOT (si:resource-present-p :SOUND)) sib-sound-bit) (&rest ignore)
  t)

(DEFUN (:cond (si:resource-present-p :SOUND) sib-sound-bit) (BIT)
  "Sets or resets the sound bit which controls the speaker 
on the System Interface Board (SIB).
BIT =  0, :off     to turn off sound bit
    =  1, :on      to turn it on
    = -1, :toggle  to complement it
    =     :query   to get its current value only
Returns the previous value of the sound bit."
;;; The sound chip can be sent commands at any time.  The results 
;;; cannot be heard, though, unless the SIB's sound bit is on.
  (DECLARE (VALUES previous-value))
  (LET ((slot   sib-slot-number)		;nubus slot# of SIB
	(offset si:%sib-monitor-speaker-enable)	;byte offset to sound bit control word
	(sound-field si:%%monitor-speaker-enable))	 ;byte spec for sound bit
    (LET* ((previous-contents (%NUBUS-READ slot offset))
	   (previous-bit (LDB sound-field previous-contents)))
      (SETQ bit (CASE bit
		  ((0  :off) 0)
		  ((1  :on ) 1)
		  ((-1 :toggle) (- 1 previous-bit))
		  (t nil)))			;else treat as :query
      (AND *monitor-sound-enabled* bit (%NUBUS-WRITE slot offset (DPB bit sound-field previous-contents)))
      previous-bit)))
)

(DEFUN delay (time-in-us)
  "Delay for x microseconds.  Returns nothing."
  (DECLARE (VALUES ignore))
  (DECF time-in-us 110.) ;; Overhead fudge factor
  (LET ((TIME (%fixnum-microsecond-time)))
    ;; Hack for stopping flash in the middle of a long beep
    (WHEN (FBOUNDP 'beep-stop-flash) (FUNCALL 'beep-stop-flash t))
    (LOOP while (> time-in-us (TIME-DIFFERENCE (%fixnum-microsecond-time) time)))))

(DEFVAR *do-sound-wait-flag* nil
  "Holds the least significant 3 bits of the delay time on a 2 byte delay sequence.")

;;; This is the only function that knows the location of the sound chip.
(si:define-when :SOUND
(DEFUN (:cond (NOT (si:resource-present-p :SOUND)) do-sound) (&rest ignore)
  t)

(DEFUN (:cond (si:resource-present-p :SOUND) do-sound) (BYTE &optional delay)
  "Sends the command byte to the sound chip.
No error checking is done.
BYTE = a command byte or a 2-list of command bytes
DELAY = delay in milliseconds
Returns nothing."
  (DECLARE (VALUES ignore))
  (LET ((slot   sib-slot-number)		;nubus slot# of SIB
	(offset si:%sound-control-register-offset) tem)	 ;byte offset to sound chip control word
    (COND ((NULL byte))				;Do delay only
	  (*do-sound-wait-flag*
	   (SETQ *do-sound-wait-flag* nil)
	   (delay (* 1000. (DPB (LOGAND #x7f byte) #o0307 byte))))
	  ((PLUSP (SETQ tem (LDB #o1010 byte)))	;freq. and delay commands can be packed in 2 bytes
	   (do-sound tem)
	   (do-sound (LDB #o0010 byte) delay))
	  ((= #xe8 (LOGAND byte #xf8))
	   (SETQ *do-sound-wait-flag* (LDB #o1003 byte)))
	  (t (%NUBUS-WRITE slot offset byte)))	;noise or volume cmds are one number
    (DONT-OPTIMIZE (FUNCALL 'IGNORE))
    (WHEN delay (delay (* 1000. delay)))))
)


;;; --------------------------------------------------
;;; Routines that determine the command(s) to give to the sound chip
;;; --------------------------------------------------

;;; To execute any of the commands, just call DO-SOUND with
;;; the values returned by any of these functions.
;;; Remember that to hear anything, both the SIB sound bit must be on
;;; and volume commands given to the chip.

;;; Only the routines on this page know about the internal structure of the chip.
;;; They implement the model of the chip as described earlier.

(si:define-when :SOUND

(DEFCONSTANT sound-clock-rate 2056000. "Sound chip clock rate (in hz) for the explorer.")

(DEFUN tone (reg freq)
  "REG = 0..3, :noise (=3), FREQ = frequency in hertz
Returns the command bytes used as input to do-sound."
  (WHEN (EQ reg :noise) (SETQ reg 2.))		;reg 2 controls noise hiss frequency
  (SETQ freq (FLOOR sound-clock-rate (LSH (FLOOR freq) 5)))
  (WHEN (> freq #x3ff) (SETQ freq #x3ff))	; Can't do frequency's lower than about 62 hz
  (DPB (LOGIOR #x80 (LSH reg 5) (LDB #o0004 freq)) #o1010 (LDB #o0406 freq)))

(DEFPARAMETER on-volume 2. "The default sound :ON volume level - see tv:volume")

(DEFUN volume (reg volume)
  "REG = 0..3, :noise (=3)
VOLUME = 0..15, :on (=2), :off (=15)
Returns one command byte."
  (COND ((EQ volume :off) (SETQ volume 15.))
	((EQ volume :on)  (SETQ volume on-volume))) ;; Make the default volume 2, so we can be louder in unusual cases.
  (COND ((EQ reg :noise)  (SETQ reg 3.)))
  (LOGIOR #x90 (LSH reg 5) volume))

(DEFUN noise (noise-type hiss)
  "NOISE-TYPE = 0, :periodic
              1, :white
HISS = 0 - higher frequency, least coarse noise
       1 - medium frequency
       2 - lower frequency, most coarse noise
       3, :override - reg #2 (tone gen. 3) controls noise frequency
Returns one command byte.
Warning: This command turns the noise volume off." ;; Advertize hardware flakeyness
  (COND ((EQ noise-type :periodic) (SETQ noise-type 0))
	((EQ noise-type :white)    (SETQ noise-type 1)))
  (COND ((EQ hiss :override)       (SETQ hiss 3.)))
  (LOGIOR #xe0 (LSH noise-type 2) hiss))

(DEFUN wait (ms-time)
  "Returns a command byte that causes DO-SOUND to wait MS-TIME milliseconds."
  (SETQ ms-time (MIN ms-time #x3ff))
  (DPB (LOGIOR #xe8 (LDB #o0003 ms-time)) #o1010 (LDB #o0307 ms-time)))

(DEFUN tone-frequency (tone)
  "Given the results from TONE, return the frequency in hertz that it will produce.
Since the sound chip accepts a limited range of values, this can tell you
the exact frequencies that the chip generates."
  (DECLARE (VALUES frequency))
  (FLOOR sound-clock-rate (* 32. (DPB (LDB #o0010 tone) #o0406 (LDB #o1010 tone)))))

)

;;; --------------------------------------------------
;;; Initialization of sound
;;; --------------------------------------------------
;;;
;;;  Changes to avoid leaving annoying tone on if monitor loses sync over
;;;  fiber optic, either by disconnect & reconnect or by power cycle.
;;;  Strategy:  Leave monitor speaker (sound) disabled at all times except when in use.
;;;					8-7-85 MBC   9-15-85 RJB
;;;
(DEFUN (:cond (NOT (si:resource-present-p :SOUND)) initialize-sound) (&rest ignore)
  "Initializes sound.  Enables it when ENABLEP is non-nil."
 t)

(DEFUN (:cond (si:resource-present-p :SOUND) initialize-sound) (&optional enablep)
  "Initializes sound.  Enables it when ENABLEP is non-nil."
  (SETQ *do-sound-wait-flag* nil)
  (DOTIMES (i 4) (do-sound (volume i :off)))
  (sib-sound-bit (IF enablep :on :off)))

(DEFF reset-sound #'initialize-sound)

;; Tell the compiler to optimize the volume tone noise and wait functions
;; into constants when their parameters are constants.
(DEFUN optimize-constant-args (form)
  (IF (LOOP for arg in (CDR form)
	    always (CONSTANTP arg))
      (EVAL form)
    form))

(compiler:add-optimizer volume optimize-constant-args)
(compiler:add-optimizer tone   optimize-constant-args)
(compiler:add-optimizer noise  optimize-constant-args)
(compiler:add-optimizer wait   optimize-constant-args)

;;; Don't take chances leaving sound on.
(DEFMACRO with-sound-enabled (&body body)
  "Execute BODY with sound enabled."
  `(UNWIND-PROTECT
       (PROGN (reset-sound t)
	      . ,body)
     (reset-sound)))

(DEFVAR *sound-disable-interrupts* nil "When non-nil, disable interrupts while doing sound")

(defmacro with-real-time (&body body)
  "Execute BODY with all interupts except KEYBOARD disabled
This has an effect similar to WITHOUT-INTERRUPTS, except it can
be control-abort'ed out of.  When *sound-disable-interrupts* is
non-nil, then do the same thing as WITHOUT-INTERRUPTS."
  ;; Perhaps a better solution can be found when the new scheduler comes?
  `(let ((old-sb-state (si:sb-on))
	 (old-priority (SEND si:current-process :priority)))
     (UNWIND-PROTECT
       (LET-IF *sound-disable-interrupts*
	       ((inhibit-scheduling-flag t))
	 (si:sb-on '(:keyboard))
	 (SEND si:current-process :set-priority 27.) ;; Higher than NUBUS, lower than KEYBOARD
	 . ,body)
       (SEND si:current-process :set-priority old-priority)
       (si:sb-on old-sb-state))))

(DEFMACRO With-Wired-Array (ARRAY &body body)
  "`Wire' ARRAY, making it unswappable during the execution of BODY"
  `(UNWIND-PROTECT
       (PROGN (si:Wire-Array ,array)
	      ,@body)
     (si:Unwire-Array ,array)))


(DEFVAR FLASH-DURATION 100000.
  "The interval (in usec) used between a double complement of the screen.")

(DEFVAR *BEEPING-FUNCTIONS* nil "List of all of the available beeping functions.")

(DEFVAR *BEEP-TYPES* nil "All valid BEEP-TYPES")

(DEFMACRO def-beep-function (name function . args)
  "Define a new beeping function NAME.
When (beep NAME) is called, FUNCTION will be called with ARGS.
DEF-BEEP-TYPE will recognize NAME as a valid sound name."
  `(PROGN (PUTPROP ',name '(,FUNCTION . ,args) :beep-type)
	  (PUSHNEW ',name *beeping-functions*)))

(DEFMACRO DEF-BEEP-TYPE (BEEP-TYPE SOUND-NAME)
  "Alias BEEP-TYPE to SOUND-NAME.
When (beep BEEP-TYPE) is called, do (beep SOUND-NAME)
This is used so calls to BEEP can be made with parameters that describe why the beep is being done.
Users may then alter the beep type for a problem with this macro."
  `(PROGN (unless (MEMBER ',SOUND-NAME *BEEPING-FUNCTIONS* :test #'eq)
	    (FSIGNAL "~A is an unknown sound. - see *beeping-functions*" ',SOUND-NAME))
	  (PUTPROP ',BEEP-TYPE ',sound-name :BEEP-TYPE)
	  (PUSHNEW ',BEEP-TYPE *BEEP-TYPES*)))

(si:define-when :SOUND
;; Helper function for DEFSOUND
(DEFUN save-sound (function &rest args)
  "Save the sound made by calling FUNCTION with ARGS into an array that can be passed to PLAY-SOUND"
  (LET ((do-sound (SYMBOL-FUNCTION 'do-sound))
	result)
    (USING-RESOURCE (save-sound-array sound-array 100)
      (FLET ((do-sound (BYTE &optional delay &aux tem)
		       (WHEN byte
			 (WHEN (PLUSP (SETQ tem (LDB #o1010 byte)))
			   (VECTOR-PUSH-EXTEND tem save-sound-array))
			 (VECTOR-PUSH-EXTEND (LDB #o0010 byte) save-sound-array))
		       (WHEN (AND delay (PLUSP delay))
			 (do-sound (wait delay)))))
	(UNWIND-PROTECT
	    (PROGN
	      (SETF (SYMBOL-FUNCTION 'do-sound) #'do-sound)
	      (APPLY function args)
	      (SETQ result (make-array (ARRAY-ACTIVE-LENGTH save-sound-array) :element-type '(mod 256)))
	      (replace result save-sound-array))
	  (SETF (SYMBOL-FUNCTION 'do-sound) do-sound))))
    result))

(DEFMACRO defsound (name &body body)
  "Define a new sound so that (beep NAME) will reproduce the sounds done in BODY.
Note that BODY is executed at macro expand time so a file containing DEFSOUNDs
is loaded, the code to produce the sound doesn't have to be loaded."
  (LET ((sound-array (save-sound 'EVAL `(PROGN ,@body))))
     `(def-beep-function ,name play-sound ,sound-array)))

(DEFUN play-sound (ARRAY)
  "Play an array of sound commands."
  (with-wired-array array
    (with-sound-enabled
      (with-real-time 
	(LOOP for byte being the array-elements of array do (do-sound byte))))))

)


(DEFPARAMETER sound-channel si:%Chan-Type-Misc)

(DEFUN (:cond (NOT (si:resource-present-p :SOUND)) BEEP) (&OPTIONAL BEEP-TYPE (STREAM 'ignore))
  "Ring the bell and flash the screen.
BEEP-TYPE says why the beep is being done.  See *BEEP-TYPES*
BEEP-TYPE may also be a beep-function.  See *BEEPING-FUNCTIONS*
Works via the :BEEP operation on STREAM if STREAM supports it.
The value of the BEEP global variable controls what this function does:
   NIL     Always quiet
   :SILENT Always quiet
   :FLASH  Always flash
   :t      Flash and beep. Default beep-type is TV:DEFAULT-BEEP
Anything else is used as the default beep-type (no flash)."
  (IF (SEND stream :operation-handled-p :BEEP)
      (SEND STREAM ':BEEP BEEP-TYPE)
      (CASE beep
	((nil :silent) nil)
	(:flash nil)
	(otherwise
	 (let ((acb (add:get-acb 2 t))
	       (ch (add:find-channel sound-channel)))
	   
	   (setf (add:requestor-complete acb) t)
	   (setf (add:opcode acb) si:%MC-tvcalls)
	   (setf (add:subopcode acb) si:%TC-Beep)
	   (add:transmit-packet acb ch))))))

(DEFUN (:cond (si:resource-present-p :SOUND) BEEP) (&OPTIONAL BEEP-TYPE (STREAM 'ignore))
  "Ring the bell and flash the screen.
BEEP-TYPE says why the beep is being done.  See *BEEP-TYPES*
BEEP-TYPE may also be a beep-function.  See *BEEPING-FUNCTIONS*
Works via the :BEEP operation on STREAM if STREAM supports it.
The value of the BEEP global variable controls what this function does:
   NIL     Always quiet
   :SILENT Always quiet
   :FLASH  Always flash
   :t      Flash and beep. Default beep-type is TV:DEFAULT-BEEP
Anything else is used as the default beep-type (no flash)."
  (IF (SEND stream :operation-handled-p :BEEP)
      (SEND STREAM ':BEEP BEEP-TYPE)
    (with-real-time 
      (CASE beep
	    ((nil :silent) nil)
	    (:flash (flash))
	    (otherwise
	     (LET* ((beep-bow-mode (AND (EQ beep t) (%fixnum-microsecond-time))))
	       ;; When beeping AND flashing, beep-bow-mode is set to the time to stop flashing
	       ;; This allows some long beep functions to clear the flash at the proper time.
	       (DECLARE (SPECIAL beep-bow-mode))
	       (WHEN beep-bow-mode
		 (complement-bow-mode))
	       (UNWIND-PROTECT  ;;Ensure the bow-mode goes back to normal
		   (do-beep (OR beep-type beep))
		 (beep-stop-flash))))))))

(si:define-when :SOUND

(DEFUN beep-stop-flash (&optional check-p)
  "Finish the flash started by beep"
  (DECLARE (SPECIAL beep-bow-mode))
  (WHEN (AND (VARIABLE-BOUNDP beep-bow-mode)
	     beep-bow-mode
	     (OR (NOT check-p)
		 (< flash-duration (TIME-DIFFERENCE (%fixnum-microsecond-time) beep-bow-mode))))
    (SETQ beep-bow-mode nil)
    (complement-bow-mode)))

(DEFUN do-beep (type)
  "Make beeping sound TYPE, where TYPE is a symbol with a :beep-type property, or a fixnum.
TYPE defaults to TV:DEFAULT-BEEP.
See DEF-BEEP-FUNCTION and DEF-BEEP-TYPE."
  (LOOP for n below 10 ;; Prevent endless looping
	until (OR (CONSP type) (NUMBERP type))
	do (SETQ type (OR (GET type :BEEP-TYPE)
			  'DEFAULT-BEEP))
	finally
	(COND ((NUMBERP type)
	       (simple-beep type))
	      ((CONSP type)
	       (with-sound-enabled
		 (APPLY (CAR type) (CDR type))))
	      (t (simple-beep 100)))))


;-----------------------------------------------------------------------------

(DEFUN flash (&optional (TIME FLASH-DURATION))
  "Flash the screen.  Time is the length of the flash in 60ths of a second."
  (DECLARE (SPECIAL beep-bow-mode))
  (IF (AND (VARIABLE-BOUNDP beep-bow-mode) beep-bow-mode)
      (delay time)
    (COMPLEMENT-BOW-MODE)
    (UNWIND-PROTECT 
	(delay time)
      (COMPLEMENT-BOW-MODE))))


(defun simple-beep (&optional (wavelength 512.) (duration 100.) (loudness :on))
  "Single-tone beep.
WAVELENGTH is in hertz and DURATION in milliseconds."
  (IF (CONSP wavelength)
      (DOLIST (w wavelength) (simple-beep w duration loudness))
    (with-sound-enabled
      (do-sound (tone 0 wavelength))
      (do-sound (volume 0 loudness) duration))))

(defun doorbell (&optional (wavelength 512) (duration 100) &aux (twelfth-root-of-2 (expt 2.0 (/ 1.0 12.0))))
  "Two-tone doorbell sound.
WAVELENGTH is in hertz and DURATION in microseconds."
  (with-sound-enabled
    (do-sound (tone 0 wavelength))					;major chord
    (do-sound (tone 1 (* wavelength (expt twelfth-root-of-2 4))))  	;  "
    (do-sound (volume 0 :off))
    (do-sound (volume 1 :on) duration) ;(quotient duration 1.5))
    (do-sound (volume 0 :off))
    (do-sound (volume 1 :on) (* 2 duration))
    (LOOP for level from on-volume below 12. doing
      (do-sound (volume 0 level))
      (do-sound (volume 1 level) duration))))

(DEFUN whoop (&optional (wavelength 600.) (increment 70.) (interval-time 7.))
  (with-sound-enabled
    (do-sound (volume 0 :on))
    (DOTIMES (i 8)
      (do-sound (tone 0 wavelength) interval-time)
      (INCF wavelength increment))))

(DEFUN RANDOM-BEEP ()
  "Makes a trio of random beeps."
  (DOTIMES (TEM 3 t)
    (simple-beep (FLOOR (SI:RANDOM-IN-RANGE 65. 1000.)))))

(DEFUN FLYING-SAUCER (&optional (LENGTH 15) (wavelength 750.) (increment 95.) (interval-time 5000.))
  "Makes a rising beep sound."
  (DOTIMES (I LENGTH)
    (LET ((next-wavelength wavelength))
      (do-sound (tone 0 wavelength))
      (do-sound (volume 0 :on))
      ;; bump the pitch by WAVELENGTH-RISE-INCREMENT each WAVELENGTH-INTERVAL-TIME
      ;;  for the duration of the beep.
      (DOTIMES (i 8)
	(do-sound (tone 0 (INCF next-wavelength increment)))
	(delay interval-time)))))

)


(si:define-when :SOUND

; Nubus addresses
(DEFCONSTANT voice-register #xf2001C)
(DEFCONSTANT speech-register #xf20018)
(DEFCONSTANT monitor-control #xf2000c) 

(DEFVAR sound-area  working-storage-area
  ;dont use customized area  PMH 2/25/88 this was (MAKE-AREA :name 'sound-area :region-size #o200000))
  "This area contains sound arrays.")

(defresource sound-array (minimum-size)
  :constructor (make-array minimum-size :element-type '(mod 256) :area sound-area
			   :leader-length 2 :fill-pointer 0)
  :matcher (>= (ARRAY-TOTAL-SIZE OBJECT) minimum-size)
  :initializer (SETF (FILL-POINTER object) 0)
  :initial-copies 0)

(DEFUN get-sound-array (ref-symbol &optional size (seconds-p t) &aux array)
  "Get the sound-array named REF-SYMBOL, with size of at least SIZE measured in SECONDS,
if SECONDS-P is T otherwise measured in bytes.
The array is paged-in upon return."
  (COND ((null ref-symbol) (SETQ ref-symbol :sound-array))
	((AND (ARRAYP ref-symbol)
	      (EQL (array-leader-length ref-symbol) 2))
	 (SETQ ref-symbol (ARRAY-LEADER ref-symbol 1))))
  (CHECK-TYPE REF-SYMBOL SYMBOL)
  (CHECK-ARG size (OR (NULL size)
		      (and (NUMBERP size) (PLUSP size)))
	     "a positive number or nil")
  (CHECK-ARG ref-symbol (OR (setq array (GET ref-symbol 'sound-array)) size)
	     "A Sound Array")
  (WHEN (OR (NOT array) size)
    (WHEN seconds-p
      (SETQ size (* size 8000.)))
    (WHEN array
      (DEALLOCATE-RESOURCE 'sound-array array))
    (SETQ ARRAY (ALLOCATE-RESOURCE 'sound-array size))
    (PUTPROP ref-symbol array 'SOUND-ARRAY)
    (STORE-ARRAY-LEADER ref-symbol array 1))
  (SETQ size (OR size (ARRAY-ACTIVE-LENGTH array)))
  (SETF (FILL-POINTER array) size)
  (si:page-in-array array '(0) (LIST size))
  array)

;
; VOICE and SPEECH - functions to input and output a speech sample
;

(EVAL-WHEN (COMPILE load eval)
;;; 1.12.88 MBC these macros included only for :SOUND
(defmacro voice ()
  "Input a sound sample"
  ;; The speech generator runs at a 8-kilohertz rate.
  ;; The sample is ready when bit 8 of the voice register goes high.
  (LET ((val (GENSYM)))
    `(do ((,val (compiler::undefined-value))) ; Return the whole 9 bits - only the bottom 8 are used 
	 ((AND (PLUSP (LOGAND #x100 (SETQ ,val (si:%nubus-read-16b sib-slot-number voice-register))))
	       (PLUSP (LOGAND #xff ,val)))
	  ,val))))

(defmacro speech (val &optional already-in-sync-p)
  "Output a sound sample"
  (IF already-in-sync-p
      `(si:%nubus-write-8b sib-slot-number speech-register ,val)
    ;; The speech generator runs at a 8-kilohertz rate.
    ;; Wait until bit 8 of the voice register goes high to synchronize.
    `(si:%nubus-write-8b sib-slot-number speech-register
			 (do () ((PLUSP (logand #x100 (si:%nubus-read-16b sib-slot-number voice-register)))
				 ,val)))))
) ;end eval-when

; Test function
(DEFUN (:cond (NOT (si:resource-present-p :SOUND)) loopback) (&rest ignore)
  t)

(defun (:cond (si:resource-present-p :SOUND) loopback) (&optional (loud t))
  "Get a sound sample and play it back immediately."
  (with-real-time
    (with-sound-enabled 
      (LET ((bits #xF100))
	(if loud (setq bits (+ bits #x200)))
	(si:%nubus-write sib-slot-number monitor-control bits))
      (do () (nil)
	(speech (voice) t)))))
;
; RECORD and PLAY functions to create and play back sound sample arrays
;
(DEFUN (:cond (NOT (si:resource-present-p :SOUND)) record) (&rest ignore)
  t)

(defun (:cond (si:resource-present-p :SOUND) record) (&optional ref-symbol (sec 2) (loud t))
  "Record for SEC seconds into a sample array (* sec 8000) bytes long
If LOUD is T, increase microphone sensitivity."
  (with-sound-enabled 
    (LET* ((n (FLOOR (* sec 8000)))
	   (vec (get-sound-array ref-symbol sec))
	   (bits #xf100))
      (if loud (setq bits (+ bits #x200)))
      (si:%nubus-write sib-slot-number monitor-control bits)
      (with-real-time
	(DOTIMES (i n)
	  (SETF (AREF vec i) (voice)))
	vec))))

(DEFUN (:cond (NOT (si:resource-present-p :SOUND)) play) (&rest ignore)
  t)

(defun (:cond (si:resource-present-p :SOUND) play) (&optional ref-symbol pathname &aux it sample)
  "Play back the speech in REF-SYMBOL. 
If REF-SYMBOL has no speech defined for it, 
load the speech sample from PATHNAME into REF-SYMBOL."
  (beep-stop-flash)
  (WHEN (AND pathname (NOT (GET ref-symbol 'sound-array)))
    (unless ref-symbol (setq ref-symbol :sound-array))
    (read-speech pathname ref-symbol))
  (SETQ it (get-sound-array ref-symbol))
  (with-real-time
    (with-sound-enabled 
      (DOTIMES (i (ARRAY-ACTIVE-LENGTH it))
	(SETQ sample (AREF it i))
	(speech sample)
	))))

(DEFUN (:cond (NOT (si:resource-present-p :SOUND)) rec) (&rest ignore)
  t)

(DEFUN (:cond (si:resource-present-p :SOUND) rec) (&optional ref-symbol (loud t) (max-length-in-seconds 30))
  "Record into an array with options for playback"
  (LET ((rec-array (get-sound-array 'rec-array max-length-in-seconds)))
    (LET-GLOBALLY ((initial-repeat-delay #o1777777)) ;; In case this was zero, Fix a bug in the keyboard process
      (LET ((bits #xf100))
	(if loud (setq bits (+ bits #x200)))
	(si:%nubus-write sib-slot-number monitor-control bits))
      (SETF (FILL-POINTER rec-array) 0)
      (LOOP for response = (FQUERY '#!Z(:choices (((:record "Hold down to Record") #\R #\r)
						  ((:play "Play") #\P #\p)
						  ((:loopback "Hold down to Loopback") #\L #\l)
						  ((:show "Show") #\S #\s)
						  ((nil "Exit") #\E #\e #\end #\return)))
				   "Record Playback Loopback or Exit ")
	    while response do
	    (CASE response
		  (:play (play rec-array))
		  (:show (IF (FBOUNDP 'plots)
						; Use funcall to avoid warning of undefined function.
			     (funcall 'plots rec-array)	
			   (PRINC " The SOUND_ANALYIZE file hasn't been loaded" *query-io*)))
		  (:loopback
		   (with-sound-enabled 
		     (with-real-time
		       (do ()
			   ((ZEROP (AREF SI:KBD-KEY-STATE-ARRAY (CHAR-INT #\l))))	;(NOT (key-state #\l)))
			 (speech (voice) t)))))
		  (:record
		   (with-real-time
		     (DO ((i 0 (1+ i))
			  (n (ARRAY-TOTAL-SIZE rec-array)))
			 ((OR (>= i n)
			      (ZEROP (AREF SI:KBD-KEY-STATE-ARRAY (CHAR-INT #\r))))	;(NOT (key-state #\r)))
			  (SETF (FILL-POINTER rec-array) i)
			  (FORMAT *query-io* " ~2$ seconds (~d bytes)" (/ i 8000.0) i)
			  (LOOP for s being the array-elements of rec-array
				with max = 0
				do (WHEN (ZEROP (logand s #x80))
				     (SETQ max (max MAX s)))
				finally (FORMAT *query-io* " Peak Modulation ~0d%"
						(FLOOR (/ (- max 63) 0.64)))))
		       (SETF (AREF REC-ARRAY I) (VOICE))))))))
    (WHEN (PLUSP (FILL-POINTER rec-array))
      (LET ((val (get-sound-array ref-symbol (FILL-POINTER rec-array) nil)))
	(replace val rec-array)
	val))))

)
;
; SAVE-SPEECH and READ-SPEECH functions to save and restore sound sample arrays
;
(DEFCONSTANT sound-id-phrase "This is a sound file")

(DEFVAR *sound-pathname-defaults* (fs:make-pathname-defaults))

(si:define-when :SOUND
(DEFUN save-speech (pathname &optional ref-symbol)
  "Save SAMPLE-ARRAY into PATHNAME"
  (SETQ pathname (fs:merge-and-set-pathname-defaults pathname *sound-pathname-defaults* "sound"))
  (LET* ((name (OR ref-symbol :sound-array))
	 (sample-array (get-sound-array name))
	 (LENGTH (ARRAY-ACTIVE-LENGTH sample-array)))
    (WITH-OPEN-FILE (STREAM pathname :direction :output :characters nil :byte-size 8 :estimated-length length)
       (LOOP for char being the array-elements of sound-id-phrase
	     do (WRITE-CHAR char stream))
		(WRITE-CHAR (LDB #o0010 length) stream) ;; bottom 8 bits
		(WRITE-CHAR (LDB #o1010 length) stream) ;; Top 8 bits
		(DOTIMES (i length)
		  (SEND stream :tyo (AREF sample-array i))))))

(defun read-speech (PATHNAME &optional ref-symbol)
  "Restore a sample-array from PATHNAME, and save it in REF-SYMBOL.
REF-SYMBOL defaults to the name portion of pathname, interned in the current package."
  (SETQ pathname (fs:merge-and-set-pathname-defaults pathname *sound-pathname-defaults* "sound"))
  (WITH-OPEN-FILE (STREAM pathname :CHARACTERS NIL :BYTE-SIZE 8)
    (UNLESS (LOOP for char being the array-elements of sound-id-phrase
		  always (char= (TYI stream) char))
      (FERROR "~a is not a Sound file" (SEND stream :truename)))
    (LET* ((LENGTH (+ (TYI stream) (ASH (TYI stream) 8.)))
	   (name (OR ref-symbol :sound-array))
	   (ARRAY (get-sound-array name length nil)))
      (read-array stream array (+ 2 (length sound-id-phrase)) length)
;;      (dotimes (i length)
;;	(SETF (AREF array i) (SEND stream :tyi)))
      array)))

(defun read-array (stream array count length)
  ;; Read LENGTH bytes from STREAM into ARRAY starting at COUNT in STREAM.
  (loop with start = 0 and (bstart bend blen sbuf)
	while (< start length)
	(multiple-value-setq (sbuf bstart bend)
	  (send stream :read-input-buffer))
	when (null sbuf) return nil do
	(setq blen (- bend bstart))
	(copy-array-portion sbuf bstart bend array start (min length (+ start blen)))
	(incf count blen)
	(incf start blen)
	(when (>= start length) (return t))
	(send stream :advance-input-buffer nil)
	finally (return t)))

(DEFUN rename-speech (old-name new-name)
  (LET ((sound-array (get-sound-array old-name)))
    (REMPROP (or old-name (ARRAY-LEADER sound-array 1)) 'sound-array)
    (STORE-ARRAY-LEADER new-name sound-array 1)
    (PUTPROP new-name sound-array 'sound-array)
    sound-array))

(DEFUN copy-speech (from-name to-name)
  (LET* ((from-array (get-sound-array from-name))
	 (to-array (get-sound-array to-name (ARRAY-ACTIVE-LENGTH from-array) nil)))
    (replace to-array from-array)
    to-array))

)

;-----------------------------------------------------------------------------

#|

The following functions can be used by the BEEP function.  The idea is
to have several different kinds of BEEP functions, each trying to get
the user's attention.  This way when one gets a notification the sound
generated will be different from that of a completion error.  See the
calls to DEF-BEEP-TYPE to see what these sounds are.  To change them,
simply look at the BEEPING-FUNCTIONS for the sound name, and REDEFINE
the beep-type to generate that sound.  Define a new beep-type is just as
easy.  To change the default beep to the doorbell sound, just do a
(DEF-BEEP-TYPE DEFAULT-BEEP :DOORBELL).

|#

(def-beep-function :flash flash)
(def-beep-function :silent ignore)
(def-beep-function :beep simple-beep)
(def-beep-function :pip simple-beep 512 30) ;; very short beep
(def-beep-function :beep-high simple-beep 2000)
(def-beep-function :beep-low simple-beep 200)
(def-beep-function :vt100-beep simple-beep 800 80)
(def-beep-function :bo simple-beep (480 84 84) 200)
(def-beep-function :doorbell doorbell)
(def-beep-function :whoop whoop)
(def-beep-function :shoop whoop 1160 -70)
(def-beep-function :RANDOM-BEEP RANDOM-BEEP)
(def-beep-function :FLYING-SAUCER FLYING-SAUCER)

(DEF-BEEP-TYPE ZWEI:CONVERSE-PROBLEM          :WHOOP        ) ; Converse was unable to send a message.
(DEF-BEEP-TYPE ZWEI:CONVERSE-MESSAGE-RECEIVED :doorbell     ) ; A Converse message has come in.
(DEF-BEEP-TYPE ZWEI:NO-COMPLETION             :shoop        ) ; Completion in a minibuffer failed.
(DEF-BEEP-TYPE NOTIFY                         :whoop        ) ; A notification cannot be printed on the selected window.
(DEF-BEEP-TYPE DEFAULT-BEEP		      :BEEP         ) ; Anything else



#| Test code
(DOLIST (BEEPf *BEEPING-FUNCTIONS*)
  (PRINT beepf) (BEEP beepf))
|#

(defun sound-menu (&aux item (BEEP :beep))
  (LOOP ALWAYS T
	DO (PROGN (SETQ item (W:menu-choose *beeping-functions*
					    :label "           Select a sound to play         "
                                            :near-mode '(:mouse)
                                            :default-item item))
		  (UNLESS item (RETURN))
		  (BEEP item))))

;; (add-to-system-menu-column :programs "Sounds and music" '(sound-menu)
;;   "Display a menu of sound effects and music for your listening entertainment.")
