;;; -*- Mode:Common-Lisp; Package:TV; Fonts:(CPTFONT CPTFONTB); Base:10 -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 12/11/86	LGO	Remove unnecessary reset-sound calls
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 02/27/86   LGO	Added MUSIC-INTERPRETER-A and CONVERT-NOTES functions to enable
;;;			playing of music transcribed by Nichael Cramer

;;;
;;; Global variables
;;;

(DEFPARAMETER *notes* '((c 0) (c# 1) (d- 1) (d 2) (d# 3) (e- 3) (e 4) (f 5) (f# 6)
			(g- 6) (g 7) (g# 8) (a- 8) (a 9) (a# 10) (b- 10) (b 11)))

(DEFPARAMETER *notes-a* '((a 0) (a# 1) (b- 1) (b 2) (c 3) (c# 4) (d- 4) (d 5) (d# 6) (e- 6) (e 7)
			  (f 8) (f# 9) (g- 9) (g 10) (g# 11) (a- 11)))


(DEFPARAMETER *expression* '((fff . 0) (ff . 1) (f . 2) (mf . 3) (mp . 4) (p . 5) (pp . 6) (ppp . 7)))

(DEFPARAMETER *attack* '((none . ignore) (simple-attack . simple-attack)))


(DEFPARAMETER *music-base-frequency* 32.703)	;default to lowest C on piano
(DEFVAR *transpose* 0)				;number of half-steps to transpose by (plus or minus)

(DEFUN convert-to-frequency (note-number)
  (round (* *music-base-frequency* (EXPT (EXPT 2 (/ 12.0)) (+ note-number *transpose*)))))

;-----------------------------------------------------------------------------

(DEFVAR state-vector (MAKE-ARRAY 4))		;Used for keeping track of which voices are "on".
(DEFVAR volume-vector (MAKE-ARRAY 4))		;Used for keeping track of the current volume of each voice.
(DEFVAR attack-vector (MAKE-ARRAY 4))		;Used for keeping an attack function for each voice.

(defun music-interpreter (delay music &aux temp)
  "Plays the music specified by MUSIC.
MUSIC is a list of sync-lists.  A sync-list specifies notes that are played together.
a sync-list is a list of notes.  A note is a list of (note-symbol octive-number duration)
where the duration is the multiple of DELAY milliseconds the note will last.  Duration is
optional and defaults to 1. If a note is (:REST duration) The voice associated with the note
is turned off for duration.  If a note is NIL, the sound remains unchanged from its previous state.
A note or sync-list may be one of:
  (:expression loudness) ;; Where loudness is one of FFF FF F MF MP P PP or PPP
  (:attack function)     ;; Where function is used to control the note attack - one of none or simple-attack.
  (:metronome time)      ;; Where time is used to set the DELAY parameter (i.e. millliseconds per beat)
Example: 'tv:((:attack simple-attack) ((E 4 3)) ((E- 4 2)) ((C# 4 1)) ((B 3 3)) ((:REST)))"
  
  (with-sound-enabled
    (ARRAY-INITIALIZE state-vector nil)
    (ARRAY-INITIALIZE volume-vector :on)
    (ARRAY-INITIALIZE attack-vector 'ignore)
    (dolist (i music) ;(PRINT i)
      (IF (LISTP (CAR i))
	   (Play-sync i delay)
	 (CASE (CAR i)
	       (:expression (SETQ temp (CDR (ASSOC (SECOND i) *expression* :test #'EQ)))
			     (IF temp (ARRAY-INITIALIZE volume-vector temp)
			       (FSIGNAL "Unknown expression ~s - see ~s" (SECOND i) '*expression*)))
	       (:attack     (SETQ temp (CDR (ASSOC (SECOND i) *attack* :test #'EQ)))
			     (IF temp (ARRAY-INITIALIZE attack-vector temp)
			       (FSIGNAL "Unknown expression ~s - see ~s" (SECOND i) '*attack*)))
	       (:metronome (SETQ delay (SECOND i)))
	       (otherwise (FSIGNAL "Unknown item in music ~s" i)))))))

(DEFUN music-interpreter-a (delay music)
  "Like MUSIC-INTERPRETER, except octives start a A instead of C."
  (LET ((*notes* *notes-a*)
	(*music-base-frequency* 55.0))
    (music-interpreter delay music)))

(DEFUN play-sync (sync-list &optional (delay 150.) &aux temp)
  "Play all the notes in the sync simultaneously.  Returns nothing.
A sync is a list of 3 notes, one for each voice, to be played simultaneously: (note1 note2 note3)"
  (do-sound nil (FLOOR delay 10.)) ;; Pause a bit to simulate some kind of attack
  (LOOP for sync in sync-list
	with voice = 0 and time = 1 and note-time
	do
	(CASE (CAR sync)
	  (:expression (SETQ temp (CDR (ASSOC (SECOND sync) *expression* :test #'EQ)))
		       (IF temp (SETF (AREF volume-vector voice) temp)
			 (FSIGNAL "Unknown expression ~s - see ~s" (SECOND sync) '*expression*)))
	  (:attack     (SETQ temp (CDR (ASSOC (SECOND sync) *attack* :test #'EQ)))
		       (IF temp (SETF (AREF attack-vector voice) temp)
			 (FSIGNAL "Unknown expression ~s - see ~s" (SECOND sync) '*attack*)))
	  (otherwise 
	   (WHEN (AND (SETQ note-time (parse-note sync voice))
		      (NUMBERP note-time))
	     (setq time (MAX time note-time)))
	   (INCF voice)))
	finally
	(do-attack delay time)))

(defun parse-note (note voice)
  "Translate note descriptions to chip commands."
;;; notes are:
;;;  nil - nop
;;;  (:rest) - a rest, turn off voice
;;;  (:expression exp-symbol) - get volume corr. to exp-symbol
;;;  (note octave) - return the pitch's frequency
  (cond ((null note))				;nil is nop
	((eq (car note) :rest)			;have a rest (turn off sound)
	 (WHEN (AREF state-vector voice)
	   (do-sound (volume voice ':off))
	   (SETF (AREF state-vector voice) nil))
	 (SECOND note))
	(t					;have a note of definite pitch, set freq. and volume
	 (do-sound (tone voice (convert-to-frequency
				  (parse-nominal-note-and-octave (car note) (cadr note)))))
	 (UNLESS (AREF state-vector voice)
	   (SETF (AREF state-vector voice) t)
	   (do-sound (volume voice (AREF volume-vector voice))))
	 (THIRD note))))

;; This is very simple, because no need has been found for a more complicated setup.
;; However, the hooks are here for doing whatever you want to do.
(DEFUN do-attack (delay time)
  "Process note attack and decay.  DELAY is the total length of the note(s)"
  (SEND (AREF attack-vector 0) delay)
  (do-sound nil (* delay time)))

(DEFUN simple-attack (delay)
  ;; Turn off voices that are on
  (DOTIMES (voice 3)
    (WHEN (AREF state-vector voice)
      (do-sound (volume voice :off))))
  ;; Delay for 1/10 the total delay
  (do-sound nil (FLOOR delay 10.))
  ;; Then turn on the sound to previous levels
  (DOTIMES (voice 3)
    (WHEN (AREF state-vector voice)
      (do-sound (volume voice (AREF volume-vector voice))))))

(defun parse-nominal-note-and-octave (nominal-note octave &optional note-number)
  "Return the note number corr. to the note in the given octave."
  (SETQ note-number (SECOND (ASSOC NOMINAL-NOTE *NOTES* :TEST #'EQ)))
  (IF note-number (+ (* octave 12.) note-number)
    (FSIGNAL "~s isn't a valid note name" nominal-note) 0))


;-----------------------------------------------------------------------------

;;; This is the user interface to the sound list interpreter.
;;; The interpreter takes sound chip command bytes and sends
;;; them to the hardware.  These byte lists are hard to understand
;;; and their use is discouraged.

(DEFUN playf (sound-list)
  "Play a sound list in foreground."
  (with-sound-enabled 
    (sound-list-interpreter sound-list)))

(defun sound-list-interpreter (sound-list &optional (delay 0.))
  "Play a sound list.  Returns nil."

;;; A sound list can contain the following kinds of elements:
;;;   nil - nop
;;;   number - assumed to be a command to the chip which is played
;;;   :end - terminates playing the sound list (as does reaching the end of the list)
;;;   atom - a variable whose value is a number to be played
;;;   2-list of numbers - as "number" above (frequency commands can take this form)
;;;   (:comment -any-) - this is ignored
;;;   (:delay x) - delay for x milliseconds; x is evaluated
;;;   (:delay x -sound-list-seq-) - each elt of sound list that will be played
;;;      will be delayed by x milliseconds; x is evaluated
;;;   (:let (-var-) -sound-list-seq-) - play sound list with vars bound (initialized to nil)
;;;   (:loop x -sound-list-seq-) - repeat playing sound list x times; x is evaluated
;;;   other list - assumed to be a form to be evaluated
  (do* ((s-list sound-list (cdr s-list))
	(s-elt (car s-list) (car s-list)))
       ((or (null s-list) (eq s-elt ':end)))
    (cond ((null s-elt))			;nil is nop
	  ((numberp s-elt)			;number is immed value, go play it
	   (do-sound s-elt delay))
	  ((atom s-elt)				;atom is variable holding a number, play it
	   (do-sound (eval s-elt) delay))
	  ((CONSP (CAR s-elt))			;have a "sync" of up to 3 notes, play it
	   (play-sync s-elt delay))
	  ((and (= (length s-elt) 2)		;have 2-list of numbers, play it
		(numberp (car s-elt))
		(numberp (cadr s-elt)))
	   (do-sound (car s-elt))
	   (do-sound (cadr s-elt) delay))
	  ((eq (car s-elt) ':comment))		;skip over comments
	  ((eq (car s-elt) ':delay)		;if 1 arg, delay x milliseconds
						;if >1 arg, delay x ms. for all nested sounds
	   (if (> (length s-elt) 2)		
	       (sound-list-interpreter (cddr s-elt) (eval (cadr s-elt)))
	     (do-sound nil (eval (cadr s-elt)))))
	  ((eq (car s-elt) ':let)		;create some local variables
	   (progv (cadr s-elt) nil (sound-list-interpreter (cddr s-elt))))
	  ((eq (car s-elt) ':loop)		;loop x times
	   (dotimes (i (eval (cadr s-elt))) (sound-list-interpreter (cddr s-elt) delay)))
	  (t
	   (eval s-elt)))))			;eval an arbitrary form

(DEFUN music-lines (lines-list &optional (basic-duration 1))
  "Convert the music-lines notation into that used by music-interpreter"
  (SETQ lines-list (LOOP for line in lines-list
			 for i below 3
			 collecting (convert-notes line basic-duration)))
  (LOOP for sync = (LOOP for line on lines-list
			 with found = nil
			 for note = (CAAR line)
			 collect (OR note '(:rest)) into result
			 when note
			 do (SETF (CAR line) (CDAR line))
			 (SETQ found t)
			 finally (RETURN (AND found result)))
	while sync
	collect sync))

(DEFUN convert-notes (note-list basic-duration)
  "Convert the music-lines notation into that used by music-interpreter"
  (LOOP for note in note-list
	and default-duration = (ROUND (/ 1.0 BASIC-DURATION))
	for duration = default-duration
	do (when (CONSP (CAR note))
	     (SETQ duration (ROUND (/ (float (SECOND note)) BASIC-DURATION)))
	     (SETQ note (CAR note)))
	   (WHEN (< DURATION BASIC-DURATION)
	     (ferror nil "Duration too short in note ~A" NOTE))
	   (when (EQ (CAR note) 'x)
	     (SETQ note (LIST* :rest (CDDR note))))
	when (> duration 1)
	append (make-list duration :initial-value note)
	else collect note))

(DEFUN play-polyphonic (frequencies time)
  (ARRAY-INITIALIZE state-vector nil)
  (ARRAY-INITIALIZE volume-vector :on)
  (with-sound-enabled
    (with-real-time 
      (DOLIST (sync frequencies)
	(LOOP for freq-list on sync
	      for freq = (CAR freq-list)
	      with voice = 0 do
	      (when (> voice 2) (PRINT (RETURN freq-list)))
	      (WHEN (PLUSP freq)
		(do-sound (tone voice (ROUND freq)))
		(UNLESS (AREF state-vector voice)
		  (SETF (AREF state-vector voice) t)
		  (do-sound (volume voice (AREF volume-vector voice))))
		(INCF voice))
	      finally
	      (LOOP for v from voice below 3 do
		    (WHEN (AREF state-vector v)
		      (do-sound (volume v ':off))
		      (SETF (AREF state-vector v) nil))))
	(do-sound nil time)))))


