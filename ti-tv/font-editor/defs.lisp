;;; -*- Mode:Common-Lisp; Package:FED; Base:10; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B) -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; Some DEFS for the EXPLORER Font Editor

(DEFVAR *min-font-length* 128.)         ; Add global variables for minimum and maximum font lengths (128 & 256)
(DEFVAR *max-font-length* 256.)
(DEFVAR NUMERIC-ARG)			; Numeric argument to a command.
(DEFVAR NUMERIC-ARG-P)			; Flag numeric argument tracking.
(DEFVAR COMMAND-CHAR)			; Character that invoked this command.
(DEFPARAMETER MIN-BOX-SIZE 6)		; If box size is smaller than this,  no grid is shown. 
(DEFPARAMETER DEFAULT-BOX-SIZE 14)	; How big to create box.
(DEFPARAMETER GRID-POINT-SIZE 2)	; Size of a point on the grid.
(DEFPARAMETER REDISPLAY-NONE 0)		; No redisplay needed.
(DEFPARAMETER REDISPLAY-ONE 1)		; Only one box wrong.
(DEFPARAMETER REDISPLAY-SOME 2)		; A few boxes wrong.
(DEFPARAMETER REDISPLAY-ALL 3)		; Everything you know is wrong.

;; ;  The elements of a FONT-DESCRIPTOR are either NIL or a CHAR-DESCRIPTOR.
;; ;  If NIL, then this character is not defined for this font.

(DEFSTRUCT (font-descriptor (:type :ARRAY-LEADER) :NAMED (:CALLABLE-CONSTRUCTORS NIL) (:conc-name nil))
	   (FD-FILL-POINTER 0)
	   FD-NAME
	   FD-LINE-SPACING		;; ;  Vertical distance between baselines.
	   FD-BASELINE			;; ;  Vertical distance from top of characters in this font.  
					;; ;  The baseline is what is aligned for different fonts.
	   FD-BLINKER-HEIGHT		;; ;  Height of a "blinker" in this font.
	   FD-BLINKER-WIDTH		;; ;  Width of a "blinker" in this font.
	   FD-SPACE-WIDTH		;; ;  Width of a space.
	   FD-DOUBLE-WIDTH-P		;; ;  T means this font is intended for display
					;; ;  with twice as many pixels per unit distance
					;; ;  in the horizontal direction. (ESCAPE 7)

					;; ;  The rest is for saving info that
					;; ;  comes in Xerox fonts, so we don't lose it.
	   (FD-VERT-RESOLUTION 3840.)	;; ;  Dots per inch, times ten.
	   (FD-HORIZ-RESOLUTION 3840.)	;; ;  " "
					;; ;  Default is right for the Dover.
	   (FD-ROTATION 0)		;; ;  Rotation in degrees.
	   )

;; ;  A CHAR-DESCRIPTOR is a two dimensional array (with leader).
;; ;  The first dimension is the height of the character and the second is the width

(DEFSTRUCT (CHAR-DESCRIPTOR (:type :ARRAY-LEADER) :NAMED (:CALLABLE-CONSTRUCTORS NIL) (:conc-name nil))
	   CD-FILL-POINTER
	   CD-NAME
	   CD-CHAR-WIDTH		;; ;  The horizontal distance taken by this character
	   CD-CHAR-VERT-WIDTH		;; ;  Vertical spacing caused by this character
					;; ;    (always 0 for the usual horizontal font).
					;; ;  For the sake of Xerox fonts.
	   CD-CHAR-LEFT-KERN)		;; ;  The distance to the left to move before placing the
					;; ;  character.  A left kern of -5 means the array is to
					;; ;  be placed 5 units to the right of the current position.

;; ;  NOTE: the CHAR-WIDTH is measured from the current position to the new position.
;; ;  the LEFT-KERN is not used in this computation therefore if the LEFT-KERN plus
;; ;  the width (second dimension) of the array is greater than the CHAR-WIDTH the
;; ;  character will overlap the next character on the line.

;; ;  A positive value of LEFT-KERN will cause the character to overlap the space of the
;; ;  last character.
;;;  ----------------------------

(DEFVAR PATHNAME-DEFAULTS) 

(DEFUN PATHNAME-DEFAULTS ()
  "Return the pathname defaults for file i/o in FED."
  (COND
    ((NOT (VARIABLE-BOUNDP PATHNAME-DEFAULTS))
     (SETQ PATHNAME-DEFAULTS (FS:MAKE-PATHNAME-DEFAULTS))
     (FS:SET-DEFAULT-PATHNAME (FS:MAKE-PATHNAME
				:HOST "SYS"
				:DIRECTORY "FONTS"
				:NAME :WILD
				:TYPE (SI:LOCAL-BINARY-FILE-TYPE)
				:VERSION :NEWEST)
			      PATHNAME-DEFAULTS)))
  PATHNAME-DEFAULTS) 

;;;  These additional Plane functions are used in a couple of places and don't have
;;;  a real home.  So they are getting stuck here. 

(DEFUN PLANE-ENSURE-EXISTS (PLANE X Y)
  "Make sure that location X, Y in PLANE is explicitly represented."
  (PLANE-ASET (PLANE-AREF PLANE X Y) PLANE X Y)) 


(DEFUN PLANE-END (PLANE)
  "Return a list whose elements are the ends of the explicitly allocated index regions of PLANE.
Each element corresponds to one dimension, and is one plus the highest
value in that dimension for which storage in PLANE is allocated."
  (MAPCAR '+ (PLANE-ORIGIN PLANE) (ARRAY-DIMENSIONS PLANE))) 


(DEFUN PLANE-EDGES (PLANE)
  "Return a list containing the origin of PLANE followed by the end.
The elements of the list completely describe what coordinate ranges have actual storage."
  (APPEND (PLANE-ORIGIN PLANE) (PLANE-END PLANE)))

(DEFUN DISPLAY-PLANE (PLANE)
  "Print out contents of two-dimensional plane PLANE as matrix of characters."
  (LET ((EDGES (PLANE-EDGES PLANE)))
    (DO ((I (FIRST EDGES) (1+ I)))
	((= I (THIRD EDGES)))
      (DO ((J (SECOND EDGES) (1+ J)))
	  ((= J (FOURTH EDGES)))
	(PRINC (PLANE-AREF PLANE I J)))
      (TERPRI))
    (TERPRI))) 
