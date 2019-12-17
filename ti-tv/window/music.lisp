;;; -*- Mode:Common-Lisp; Package:TV; Base:10 -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


;; The music interpreter isn't loaded in the standard system.
;; Ensure that its loaded when compiling this file.
(EVAL-WHEN (COMPILE)
  (UNLESS (FBOUNDP 'music-interpreter)
    (LOAD "sys:window;music-interpreter")))

#|

Note lists look like:
( ((note octave length) (note octave length) (note octave length))  ;; three notes played together, one for each voice
  ((note octave length) (note octave length) (note octave length))  ;; three more notes played together
  ...) ;; more notes
;;      voice 0             voice 1              voice 2

note - a#, b-, etc.
octave - 0..7
length - multiples of delay milliseconds.

;; this is how it should be (maybe someday)
note = (note octave duration -modifiers-)
duration = (length n-tuple dotted) or length
    length = 1,2,4,8,16,32,64
    n-tuple >= no. (e.g. 1=standard, 3=triplet)
    dotted = 0,1,2 (no. of dots)

|#

;;; This is kludgey beyond belief.  
;;;
;;; "I have no time, I have no time."  
;;;          - E. Galois
;;;
;;; But at least he could scribble out 60 pages of notes on group theory on the evening
;;; before his death in a duel.  Could I possibly scribble out 60 pages of code in the
;;; week before AAAI?  But then again, most of his results had no proofs, so nothing
;;; says this code has to be working.

;;; c1 is lowest correct note (at 63 hz)				 


;;; 


(defsound :warm-up
  (music-interpreter 900 '(((C 3) (E 3) (G 3))
	       ((C 3) (F 3) (A 3))
	       ((C 3) (E 3) (G 3))
	       ((B 2) (F 3) (G 3))
	       ((C 3) (E 3) (G 3)))))

(defsound :swar
  (music-interpreter 200
	 '(((G 3) (D 3) (B 2)) ((G 3) (D 3) (B 2)) ((D 4) (B 3) (G 3)) ((D 4) (B 3) (G 3))
	   ((C 4) (G 3) (E 3)) ((B 3)) ((A 3)) ((G 4) (D 4) (B 3)) ((G 4) (D 4) (B 3)) ((D 4) (B 3) (G 3)))))


(defsound :nuts
  (music-interpreter 100
	 '((:attack simple-attack)
	   ((B- 3 2)) ((B- 4 3)) ((A 4 1)) ((G 4 1)) ((F 4 1)) ((E- 4 1)) ((D 4 1)) ((D 4 1)) ((D 4 4)) ((:REST))
            ((G 3 2)) ((G 4 3)) ((F 4 1)) ((E- 4 1)) ((D 4 1)) ((C 4 1)) ((B- 3 2)))))

(defsound :kiss
  (music-interpreter 100 '((:attack simple-attack)
	       ((E 3 2)) ((F# 3 1)) ((A- 3 1)) ((B 3 1)) ((C# 4 1)) ((E 4 1)) ((E- 4 2)) ((B 3 1)) ((A- 3 2)) ((:REST)))))

(defsound :joy
  (music-interpreter 100
	 '((:attack simple-attack)
	   ((E 4 3)) ((E- 4 2)) ((C# 4 1)) ((B 3 3)) ((:REST))
           ((A 3 1)) ((A- 3 2)) ((F# 3 2)) ((E 3 4)) ((:REST))
           ((B 3 2)) ((C# 4 4)) ((:REST))
           ((C# 4 2)) ((E- 4 4)) ((:REST))
	   ((E- 4 2)) ((E 4 4)))))

(defsound :tree
  (music-interpreter 100 '((:attack simple-attack)
	       ((E 3 2)) ((A 3 1)) ((A 3 1)) ((A 3 2)) ((:REST))
	       ((B 3 2)) ((C# 4 1)) ((C# 4 1)) ((C# 4 2)) ((:REST))
	       ((C# 4 2)) ((B 3 1)) ((C# 4 1)) ((D 4 2)) ((A- 3 2)) ((B 3 2)) ((A 3 1)) ((A 3 3)))))

(defsound :deck
  (music-interpreter 100 '((:attack simple-attack)
	       ((F# 4 2)) ((E 4 1)) ((E- 4 2)) ((C# 4 2)) ((B 3 2)) ((C# 4 2)) ((E- 4 2)) ((B 3 3))
	       ((C# 4 1)) ((E- 4 1)) ((E 4 1)) ((C# 4 1)) ((E- 4 3)) ((C# 4 1)) ((B 3 2)) ((B- 3 2)) ((B 3 4)))))


(defsound :jingle
  (music-interpreter 100 '((:attack simple-attack)
	       ((A- 3 2)) ((A- 3 2)) ((A- 3 2)) ((:REST))
	       ((A- 3 2)) ((A- 3 2)) ((A- 3 2)) ((:REST))
	       ((A- 3 2)) ((B 3 2)) ((E 3 3)) ((F# 3 1)) ((A- 3 4)) ((:REST))
	       ((A 3 2)) ((A 3 2)) ((A 3 3)) ((A 3 1)) ((A 3 2)) ((A- 3 2)) ((A- 3 3)) ((A- 3 1))
	       ((B 3 2)) ((B 3 2)) ((A 3 2)) ((F# 3 2)) ((E 3 4)))))

(defsound :zowie
  (music-interpreter 100 '((:attack simple-attack)
	       ((A 3 1)) ((A 3 1)) ((A 3 2)) ((A- 3 1)) ((A 3 1)) ((B 3 2)) ((A 3 1)) ((B 3 1))
	       ((C# 4 2)) ((B 3 1)) ((A 3 1)) ((A- 3 2)) ((F# 3 1)) ((A- 3 1)) ((A 3 2)))))

(defsound :races
  (music-interpreter 100 '((:attack simple-attack)
	       ((E 3 2)) ((A 3 2)) ((C# 4 2)) ((E 4 1)) ((E 4 1)) ((E 4 1)) ((:REST))
	       ((C# 4 1)) ((C# 4 1)) ((C# 4 1)) ((:REST))
	       ((A 3 1)) ((C# 4 1)) ((A 3 1)) ((E 3 3)) ((:REST))
	       ((E 3 2)) ((A 3 2)) ((C# 4 2)) ((E 4 1)) ((E 4 1)) ((E 4 1)) ((:REST))
	       ((C# 4 1)) ((C# 4 1)) ((C# 4 1)) ((:REST))
	       ((E 3 1)) ((E 3 1)) ((E 3 1)) ((A 3 5)))))

(defsound :et
  (music-interpreter 100 '((:attack simple-attack)
	       ((E 4 2)) ((B 4 6)) ((A 4 2)) ((A- 4 2)) ((F# 4 2)) ((A- 4 2)) ((E 4 4)) ((B 3 6)) ((:REST))
	       ((F# 4 2)) ((c# 5 6)) ((B 4 2)) ((B- 4 2)) ((A- 4 2)) ((B- 4 2)) ((F# 4 6)))))

(defsound :tzone
  (music-interpreter 100 '((:attack simple-attack)
	       ((A- 3 1)) ((A 3 1)) ((A- 3 1)) ((F 3 1)) ((A- 3 1)) ((A 3 1)) ((A- 3 1))
	       ((F 3 1)) ((A- 3 1)) ((A 3 1)) ((A- 3 1)) ((F 3 1)))))

(defsound :bat5
  (music-interpreter 100 '((:attack simple-attack)
	       ((B- 3 1)) ((B- 3 1)) ((B- 3 1)) ((G 3 8)) ((:REST))
	       ((A 3 1)) ((A 3 1)) ((A 3 1)) ((F# 3 8)))))


;;
;; The following sounds and music are programed using the byte codes that go to the sound chip! (yech)
;; They're hard to understand, but sound nice.  They must have been ripped out of TI990/4 code.
;;

;;; chimes (1 Mhz)
(defsound :chime
  (playf '(#x9f #xbf #xdf #xff #x84 #x0a #x90 #xa0 #x0a #xbb #x91
	   (:delay 100. #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9a #x9b) ; #x9c #x9d #x9e #x9f)
	   )))
		  
;;; missile (2 MHz)
(defsound :missile
  (playf '(159. 191. 223. 255. 231. 240. (setq j 0) 
		(:loop 16.
		       (:delay 3. (:loop 16. (setq i 192.) i j (incf i)))
		       (setq k (+ 240. j)) k (incf j)))))

;;; bomb drop (2 Mhz)
(defsound :bomb-drop
  (playf '(159. 191. 223. 255. 144. (setq j 5.)
	       (:loop 13.
		      (setq i 128.)
		      (:loop 16. (:delay 4.) i j (incf i))
		      (incf j))
	       159. 228. (setq i 240.)
	       (:loop 16. (:delay 100.) i (incf i)))))
