; -*- Mode:Common-Lisp; Package:W; Fonts:(MEDFNT HL12B); Base:10 -*-

;                           RESTRICTED RIGHTS LEGEND

;Use, duplication, or disclosure by the Government is subject to
;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;Technical Data and Computer Software clause at 52.227-7013.
;
;                     TEXAS INSTRUMENTS INCORPORATED.
;                              P.O. BOX 2909
;                           AUSTIN, TEXAS 78769
;
; Copyright (C) 1984- 1989 Texas Instruments Incorporated. All rights reserved.

;;; Change History
;;;
;;;  Date      Author  Description
;;; -------------------------------------------------------------------------------------
;;; 10/22/86 TWE	  Changed the macro fontify so that it doesn't use fillarray.
;;; 10/02/86 JEB	  Moved functions from GWIN: to W:.
;;;

;;;
;;;                STANDARD-FONT
;;;
;; Defines standard-font as a graphics-window vector font


(setq standard-font (make-instance 'font :horz-spacing 10 :vert-spacing 13)) 


(setq standard-font standard-font) 


(defmacro fontify (&rest lists)
  `(list . ,(loop for list in lists
		  collect `(make-array ,(length list) ':type 'art-4b
				       :initial-contents ',list)))) 


(defmacro deffont (font char font-info1 font-info2 thickness)
  `(funcall ,font ':set-character ,char
	    (make-instance 'vector-character
			   ':x-points (fontify . ,font-info1)
			   ':y-points (fontify . ,font-info2)
			   ':thickness ,thickness))) 



(deffont standard-font 0 ((4 5 6 6 5 4 3 3 4) (3 6) (3 6)) ((5 5 6 7 8 8 7 6 5) (6 6) (7 7))  1) 

(deffont standard-font 1 ((6 4 2) (4 4)) ((7 9 7) (2 9)) 1) 

(deffont standard-font 2 ((7 7 3 2 1 1 2 3 7 8)) ((4 5 10 10 8 7 5 5 10 10)) 1) 

(deffont standard-font 3 ((2 2 4 6 7 7 6 3) (6 7 7 6 4 2)) ((12 5 2 2 3 5 6 6) (6 7 9 10 10 9)) 1) 

(deffont standard-font 4 ((7 4 1)) ((8 5 8)) 1) 

(deffont standard-font 5 ((1 7 7)) ((7 7 9)) 1) 

(deffont standard-font 6 ((6 4 2 2 4 6) (2 6)) ((4 4 6 8 10 10) (7 7)) 1) 

(deffont standard-font 7 ((1 2 7 8) (3 3) (6 6)) ((6 5 5 4) (5 10) (5 10)) 1) 

(deffont standard-font 8 ((2 3 7 8) (2 5)) ((2 3 9 10) (10 6)) 1) 

(deffont standard-font 9 ((1 2 4 6 6 4 3 2 2 7)) ((5 4 4 6 8 10 10 9 7 2)) 1) 

(deffont standard-font 10 ((6 5 4 3 3 6 6 5 3 2 2 4)) ((3 2 2 3 4 7 9 10 10 9 7 5)) 1) 

(deffont standard-font 11 ((6 4 2) (4 4)) ((4 2 4) (2 9)) 1) 

(deffont standard-font 12 ((4 4) (1 7) (1 7)) ((3 9) (6 6) (9 9)) 1) 

(deffont standard-font 13 ((3 5 7 7 5 3 1 1 3) (4 4) (1 6)) ((3 3 5 7 9 9 7 5 3) (3 9) (6 6)) 1) 

(deffont standard-font 14 ((2 3 6 7 8 8 7 6 3 2 1 1 2)) ((3 3 9 9 7 5 3 3 9 9 7 5 3)) 1) 

(deffont standard-font 15 ((3 4 5 7 7 5 4 2 2 4 7)) ((3 2 2 4 8 10 10 8 7 5 5)) 1) 

(deffont standard-font 16 ((7 2 1 1 2 7)) ((5 5 6 8 9 9)) 1) 

(deffont standard-font 17 ((1 6 7 7 6 1)) ((5 5 6 8 9 9)) 1) 

(deffont standard-font 18 ((2 2 3 5 6 6)) ((9 4 3 3 4 9)) 1) 

(deffont standard-font 19 ((2 2 3 5 6 6)) ((3 8 9 9 8 3)) 1) 

(deffont standard-font 20 ((8 5 2) (3 7)) ((2 10 2) (5 5)) 1) 

(deffont standard-font 21 ((2 8 8 2) (4 8)) ((2 2 10 10) (6 6)) 1) 

(deffont standard-font 22 ((3 5 7 7 5 3 1 1 3) (2 6) (2 6)) ((3 3 5 7 9 9 7 5 3) (4 8) (8 4)) 1) 

(deffont standard-font 23 ((3 1 3) (6 8 6) (1 8)) ((5 7 9) (5 7 9) (7 7)) 1) 

(deffont standard-font 24 ((3 1 3) (1 8)) ((5 7 9) (7 7)) 1) 

(deffont standard-font 25 ((6 8 6) (1 8)) ((5 7 9) (7 7)) 1) 

(deffont standard-font 26 ((1 8) (1 8) (1 8)) ((5 5) (8 8) (10 3)) 1) 

(deffont standard-font 27 ((4 7 4 1 4)) ((4 7 10 7 4)) 1) 

(deffont standard-font 28 ((7 1 7) (1 7)) ((5 7 9) (10 10)) 1) 

(deffont standard-font 29 ((1 7 1) (1 7)) ((9 7 5) (10 10)) 1) 

(deffont standard-font 30 ((1 8) (1 8) (1 8)) ((5 5) (7 7) (9 9)) 1) 

(deffont standard-font 31 ((1 4 7)) ((5 8 5)) 1) 

(deffont standard-font 32 () () 0) 

(deffont standard-font 33 ((4 4) (4 4)) ((2 7) (9 10)) 1) 

(deffont standard-font 34 ((3 3) (6 6)) ((2 5) (2 5)) 1) 

(deffont standard-font 35 ((2 2) (6 6) (1 7) (1 7)) ((2 10) (2 10) (4 4) (8 8)) 1) 

(deffont standard-font 36 ((7 6 2 1 1 2 6 7 7 6 2 1) (4 4)) ((3 2 2 3 4 5 6 7 8 9 9 8) (1 10)) 1) 

(deffont standard-font 37 ((1 2 3 4 4 3 2 1 1) (4 5 6 7 7 6 5 4 4) (1 7))
   ((2 1 1 2 3 4 4 3 2) (7 6 6 7 8 9 9 8 7) (8 2)) 1) 

(deffont standard-font 38 ((9 4 3 2 2 3 4 5 5 4 3 1 1 2 5 8))
   ((10 5 5 4 2 1 1 2 4 5 5 7 9 10 10 7)) 1) 

(deffont standard-font 39 ((5 4 4 5 5 3)) ((2 2 1 1 2 4)) 1) 

(deffont standard-font 40 ((5 3 3 5)) ((1 3 8 10)) 1) 

(deffont standard-font 41 ((3 5 5 3)) ((1 3 8 10)) 1) 

(deffont standard-font 42 ((1 8) (2 7) (2 7)) ((6 6) (3 9) (9 3)) 1) 

(deffont standard-font 43 ((4 4) (1 7)) ((3 9) (6 6)) 1) 

(deffont standard-font 44 ((5 4 4 5 5 3)) ((10 10 9 9 10 12)) 1) 

(deffont standard-font 45 ((1 7)) ((6 6)) 1) 

(deffont standard-font 46 ((4 5 5 4 4)) ((9 9 10 10 9)) 1) 

(deffont standard-font 47 ((1 8)) ((10 1)) 1) 

(deffont standard-font 48 ((1 1 3 5 7 7 5 3 1) (1 7)) ((8 3 1 1 3 8 10 10 8) (8 3)) 1) 

(deffont standard-font 49 ((2 4 4) (2 6)) ((3 1 10) (10 10)) 1) 

(deffont standard-font 50 ((1 2 6 7 7 6 3 1 1 7)) ((2 1 1 2 5 6 6 8 10 10)) 1) 

(deffont standard-font 51 ((1 2 6 7 7 6 3) (6 7 7 6 2 1)) ((2 1 1 2 4 5 5) (5 6 9 10 10 9)) 1) 

(deffont standard-font 52 ((6 6 6 1 1 8)) ((10 1 1 6 7 7)) 1) 

(deffont standard-font 53 ((7 1 1 6 7 7 6 2 1)) ((1 1 5 5 6 9 10 10 9)) 1) 

(deffont standard-font 54 ((6 3 1 1 2 6 7 7 6 1)) ((1 1 3 9 10 10 9 6 5 5)) 1) 

(deffont standard-font 55 ((1 7 7 3 3)) ((1 1 3 7 10)) 1) 

(deffont standard-font 56 ((2 1 1 2 6 7 7 6 2 1 1 2 6 7 7 6))
   ((5 4 2 1 1 2 4 5 5 6 9 10 10 9 6 5)) 1) 

(deffont standard-font 57 ((7 2 1 1 2 6 7 7 5 3)) ((5 5 4 2 1 1 2 8 10 10)) 1) 

(deffont standard-font 58 ((4 5 5 4 4) (4 5 5 4 4)) ((5 5 6 6 5) (9 9 10 10 9)) 1) 

(deffont standard-font 59 ((4 5 5 4 4) (5 4 4 5 5 3)) ((5 5 6 6 5) (10 10 9 9 10 12)) 1) 

(deffont standard-font 60 ((7 1 7)) ((10 7 4)) 1) 

(deffont standard-font 61 ((1 8) (1 8)) ((5 5) (8 8)) 1) 

(deffont standard-font 62 ((1 7 1)) ((4 7 10)) 1) 

(deffont standard-font 63 ((1 1 2 6 7 7 4 4) (4 4)) ((3 2 1 1 2 4 6 7) (9 10)) 1) 

(deffont standard-font 64 ((7 3 1 1 3 6 8 8 4 3 3 4 5 6 6)) ((10 10 8 5 3 3 5 8 8 7 6 5 5 6 8))
   1) 

(deffont standard-font 65 ((1 1 2 6 7 7) (1 7)) ((10 2 1 1 2 10) (6 6)) 1) 

(deffont standard-font 66 ((1 6 7 7 6 1 1 6 7 7 6)) ((5 5 6 9 10 10 1 1 2 4 5)) 1) 

(deffont standard-font 67 ((7 6 2 1 1 2 6 7)) ((2 1 1 2 9 10 10 9)) 1) 

(deffont standard-font 68 ((1 5 7 7 5 1 1)) ((1 1 3 8 10 10 1)) 1) 

(deffont standard-font 69 ((7 1 1 7) (1 5)) ((1 1 10 10) (5 5)) 1) 

(deffont standard-font 70 ((7 1 1) (1 5)) ((1 1 10) (5 5)) 1) 

(deffont standard-font 71 ((7 6 2 1 1 2 6 7 7 5)) ((2 1 1 2 9 10 10 9 6 6)) 1) 

(deffont standard-font 72 ((1 1) (7 7) (1 7)) ((1 10) (1 10) (5 5)) 1) 

(deffont standard-font 73 ((2 6) (2 6) (4 4)) ((1 1) (10 10) (1 10)) 1) 

(deffont standard-font 74 ((6 6 5 2 1 1)) ((1 9 10 10 9 8)) 1) 

(deffont standard-font 75 ((2 7) (1 1) (1 7)) ((6 10) (1 10) (7 1)) 1) 

(deffont standard-font 76 ((1 1 7)) ((1 10 10)) 1) 

(deffont standard-font 77 ((1 1 4 7 7) (4 4)) ((10 1 4 1 10) (4 5)) 1) 

(deffont standard-font 78 ((7 7 1 1)) ((1 10 1 10)) 1) 

(deffont standard-font 79 ((1 2 6 7 7 6 2 1 1)) ((2 1 1 2 9 10 10 9 2)) 1) 

(deffont standard-font 80 ((1 1 6 7 7 6 1)) ((10 1 1 2 5 6 6)) 1) 

(deffont standard-font 81 ((1 2 6 7 7 5 2 1 1) (4 7)) ((2 1 1 2 8 10 10 9 2) (7 10)) 1) 

(deffont standard-font 82 ((1 1 6 7 7 6 1) (7 3)) ((10 1 1 2 5 6 6) (10 6)) 1) 

(deffont standard-font 83 ((7 6 2 1 1 2 6 7 7 6 2 1)) ((2 1 1 2 4 5 5 6 9 10 10 9)) 1) 

(deffont standard-font 84 ((1 7) (4 4)) ((1 1) (1 10)) 1) 

(deffont standard-font 85 ((1 1 2 6 7 7)) ((1 9 10 10 9 1)) 1) 

(deffont standard-font 86 ((7 7 4 1 1)) ((1 5 10 5 1)) 1) 

(deffont standard-font 87 ((7 7 4 1 1) (4 4)) ((1 10 7 10 1) (6 7)) 1) 

(deffont standard-font 88 ((1 7) (7 1)) ((1 10) (1 10)) 1) 

(deffont standard-font 89 ((7 7 4 1 1) (4 4)) ((1 4 6 4 1) (6 10)) 1) 

(deffont standard-font 90 ((0 7 7 1 1 8)) ((1 1 2 9 10 10)) 1) 

(deffont standard-font 91 ((5 3 3 5)) ((1 1 10 10)) 1) 

(deffont standard-font 92 ((1 8)) ((1 10)) 1) 

(deffont standard-font 93 ((3 5 5 3)) ((1 1 10 10)) 1) 

(deffont standard-font 94 ((7 4 1)) ((4 1 4)) 1) 

(deffont standard-font 95 ((1 8)) ((10 10)) 1) 

(deffont standard-font 96 ((3 4 4 3 3 5)) ((2 2 1 1 2 4)) 1) 

(deffont standard-font 97 ((2 6 7 7 6 2 1 1 2 7) (7 8)) ((5 5 6 9 10 10 9 8 7 7) (9 10)) 1) 

(deffont standard-font 98 ((1 1 6 7 7 6 1)) ((1 10 10 9 6 5 5)) 1) 

(deffont standard-font 99 ((7 6 2 1 1 2 6 7)) ((6 5 5 6 9 10 10 9)) 1) 

(deffont standard-font 100 ((7 7 2 1 1 2 7)) ((1 10 10 9 6 5 5)) 1) 

(deffont standard-font 101 ((1 6 7 6 2 1 1 2 6)) ((7 7 6 5 5 6 9 10 10)) 1) 

(deffont standard-font 102 ((7 6 4 3 3) (1 5)) ((2 1 1 2 10) (5 5)) 1) 

(deffont standard-font 103 ((7 2 1 1 2 6 7 7 6 2 1)) ((9 9 8 6 5 5 6 11 12 12 11)) 1) 

(deffont standard-font 104 ((1 2 6 7 7) (1 1)) ((6 5 5 6 10) (1 10)) 1) 

(deffont standard-font 105 ((4 4) (4 4)) ((5 10) (2 3)) 1) 

(deffont standard-font 106 ((6 6 5 3 2) (6 6)) ((5 11 12 12 11) (2 3)) 1) 

(deffont standard-font 107 ((6 3 5) (1 1) (1 3)) ((10 7 5) (1 10) (7 7)) 1) 

(deffont standard-font 108 ((3 4 4) (3 5)) ((1 1 10) (10 10)) 1) 

(deffont standard-font 109 ((1 2 3 4 5 6 7 7) (1 1) (4 4)) ((6 5 5 6 5 5 6 10) (5 10) (6 10)) 1) 

(deffont standard-font 110 ((1 3 6 7 7) (1 1)) ((7 5 5 6 10) (5 10)) 1) 

(deffont standard-font 111 ((1 2 6 7 7 6 2 1 1)) ((6 5 5 6 9 10 10 9 6)) 1) 

(deffont standard-font 112 ((1 1 6 7 7 6 1)) ((12 5 5 6 8 9 9)) 1) 

(deffont standard-font 113 ((7 2 1 1 2 7 7 8)) ((9 9 8 6 5 5 12 12)) 1) 

(deffont standard-font 114 ((1 3 6 7) (1 1)) ((7 5 5 6) (5 10)) 1) 

(deffont standard-font 115 ((7 2 1 2 6 7 7 6 2 1)) ((5 5 6 7 7 8 9 10 10 9)) 1) 

(deffont standard-font 116 ((3 3 4 5 6) (1 5)) ((1 9 10 10 9) (5 5)) 1) 

(deffont standard-font 117 ((1 1 2 6 7 7) (7 8)) ((5 9 10 10 9 5) (9 10)) 1) 

(deffont standard-font 118 ((7 4 1)) ((5 10 5)) 1) 

(deffont standard-font 119 ((7 7 4 1 1)) ((5 10 7 10 5)) 1) 

(deffont standard-font 120 ((1 7) (1 7)) ((5 10) (10 5)) 1) 

(deffont standard-font 121 ((1 1 4) (7 7 1)) ((5 6 9) (5 6 12)) 1) 

(deffont standard-font 122 ((7 1 7 1)) ((10 10 5 5)) 1) 

(deffont standard-font 123 ((7 5 4 4 3 4 4 5 7)) ((11 11 10 7 6 5 2 1 1)) 1) 

(deffont standard-font 124 ((5 5)) ((1 12)) 1) 

(deffont standard-font 125 ((3 5 6 6 7 6 6 5 3)) ((1 1 2 5 6 7 10 11 11)) 1) 

(deffont standard-font 126 ((1 2 3 5 6 7)) ((2 1 1 3 3 2)) 1) 

(deffont standard-font 127 ((1 2 3 4 4 5 6 7)) ((9 10 10 9 2 1 1 2)) 1) 
