;;;;-*-Mode:Common-LISP;Package:TV;Base:10;Fonts:(MEDFNT HL12BI MEDFNB)-*-

;                           RESTRICTED RIGHTS LEGEND

;Use, duplication, or disclosure by the Government is subject to
;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;Technical Data and Computer Software clause at 52.227-7013.
; Copyright (C) 1985- Texas Instruments Incorporated. All rights reserved.

;
;                     TEXAS INSTRUMENTS INCORPORATED.
;                              P.O. BOX 2909
;                           AUSTIN, TEXAS 78769
;
; Copyright (C) 1987- 1989 Texas Instruments Incorporated. All rights reserved.


;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  8/24/1987 Ken Wood   Changes from code reading session

;;;  5/06/87    Ken Wood   Change to make this a part of sheet flavor rather than a seperate flavor
;;;                                   1. Defined some constants & doc string for "magic numbers"
;;;                                   2. Defparameter => defvar where appropriate
;;;                                   3. :type => :element-type, i.e. Common Lisp clean up
;;; 4/07/87    Ken Wood   General Cleanup
;;; 3/20/87    Ken Wood   Creation

;;;
;;; This file contains a bunch of defvar, defparameter, and defconstant statements for data
;;; used by the CSIB supplement to sheet flavor. This file should be loaded first so that references are right.
;;;
;;; The CSIB software is a mix of the orginial DSEG conception with ideas adapted from the
;;; CSIB-Inspector (software used for engineering checkout) written in Austin.
;;; Access to most hardware is via arrays displaced to the hardware locations. The variables
;;; can than be read and written in order to effect the desired hardware result.
;;;
;;; The CSIB software access an array of instances of the block-flavor, one for each register block.
;;;  Right now, everything will be done with register block 0, but we're trying to keep the code
;;;  generalized for later expansion
;;;

;;; stuff borrowed from CSIB-inspector software from austin:

(DEFCONSTANT *slot-base* #xF)
(DEFCONSTANT *slot-space* (* *slot-base* #x10))
(DEFCONSTANT *slot-space-address* (* *slot-base* #x10000000))

(DEFCONSTANT *board-type-offset* #xFFFF84)
(DEFCONSTANT *CSIB-board-type* "CSI")

(DEFCONSTANT *NuBus-base-register-offset* #xD00048)

(DEFCONSTANT *control-register-offset* #xD00520)

(DEFCONSTANT number-of-blocks 15 "The number of register blocks on the CSIB")
(DEFCONSTANT number-of-slots  15 "The number of backplane slots we need to scan to find a board")

(DEFCONSTANT number-of-buffers 2 "The number of LUT buffers on the CSIB")

;;; the next two are defined for later expansion, for example supporting two screen,  thus limiting color to
;;; just a portion of the color map.

(DEFVAR *first-color-location* 0 "First index of color map, default for clamp value")
(DEFVAR *last-color-location* 255 "Last index of color map, default for saturate value")


(DEFCONSTANT number-of-color-locations 768. "The number of words in a color LUT.
Since there are 3 primary colors, each with 256 possible values, we have 3 X 256 = 768 words")

(DEFPARAMETER *reserved* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
			   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)
  "a list of locations in the color map reserved for system use")

(DEFSTRUCT (color-map (:copier nil))
  (name ""                        :type string)
  (system-version (make-version-name 'window)   :type string)
  (CME-version ""         :type string)
  (saturate *last-color-location*  :type integer)
  (clamp  *first-color-location* :type integer)
  (reserved-slots *reserved*     :type list)
  (table (MAKE-ARRAY number-of-color-locations :element-type '(unsigned-byte 32)) :type array)
)

;;;
;;; CONFIGURATION REGISTER
;;;

(DEFCONSTANT *config-register-offset* #xD00040)
(defconstant *reset-bit* 0)
(defconstant *master-enable-bit* 1)
(defconstant *led-bit* 2)
(defconstant *nubus-test* 3)

;;; change from parameter to var
(defvar lut-buffers (MAKE-ARRAY 2 :initial-element nil)) ;; this guy is later initialized by a function

(DEFCONSTANT *lut-buffers-offset* #xA20000
  "Slot independent physical address of the palette 0 buffer") 
(DEFCONSTANT *buffer-red-address* 0 "Offset from the palette to the red LUT" )
(DEFCONSTANT *buffer-green-address* #x400 "Offset from the palette to the green LUT")
(DEFCONSTANT *buffer-blue-address* #x800 "Offset from the palette to the blue LUT") 


;;;;
;;;; Registers Parameters
;;;;

(DEFCONSTANT block-base #xA10000 "Offset to register block 0")
(DEFCONSTANT foreground-offset #x80
  "Offset from block base to foreground color
register")
(DEFCONSTANT background-offset #x84
  "Offset from block base to background color
register")
(DEFCONSTANT transparency-offset #x88 "Offset from block base to transparency register")
(DEFCONSTANT plane-mask-offset #x8C "Offset from block base to plane mask register")
(DEFCONSTANT pixel-mask-offset 0 "Offset from block base to pixel mask register")

;;; our data declarations

;;; added * to front and back of name
(defvar   *csib-slot*             nil)

(defvar   *CURRENT-REG-BLOCK*   0) 		;DO NOT CHANGE THIS UNLESS 
						;YOU KNOW WHAT YOU ARE DOING!

(defvar *CURRENT-COLOR-LUT-BUFFER* 0) ; keep track of current buffer, 0 or 1.


(DEFVAR *control-register* nil)

(DEFCONSTANT low 0)
(DEFCONSTANT high 1)

(DEFVAR *NuBus-base* nil)

;;; changed from parameter to var
(DEFVAR *NuBus-base-register* :unbound
  "NuBus base is in 4 MSb's of this byte. The 4 LSb's are read only!")

(defvar *config-register* nil)

;;; changed parameter to var
(defvar *blocks* (make-array 16 :initial-contents nil)) ; holds one instance for each register block


;;; Acknowledgements:
;;;       the code below is either CSIB-Inspector code used as is, or adapted to meet the needs
;;;       of the csib-mixin software
;;;

;;;;
;;;;  FUNCTIONS FOR MAKING ARRAYS DISPLACED TO PHYSICAL ADDRESSES
;;;;

(DEFUN physical-address (&rest offsets)  ; take a base address and add offsets to get to the right place
  (WHEN *csib-slot*                        ; test if initialization has occurred.
       (+ *slot-space-address*
	  (* *csib-slot* #x1000000)        ; here is the base
	  (apply '+ offsets))))          ; now add in all the offsets.

;;;
;;; the following function makes the displaced array
;;;

;;; modified based on code reading

(DEFUN make-physical-array (&rest args)
 (LET ((temp nil)
      )
  (if *csib-slot*                          ; test if initialization has occurred.
      (apply 'make-array args)
      (apply 'make-array
        (IF (SETQ temp (MEMBER ':displaced-to-physical-address args :test #'EQ))
	    (APPEND (LDIFF args temp) (CDDR temp))
	    args)))))
;; old code for reference
;;	     (REMOVE ':displaced-to-physical-address
;;		     (REMOVE (CADR (MEMBER ':displaced-to-physical-address args :test #'eq))
;;			     args)))))

;;;
;;; the following function uses nubus reads to grab the 3 characters that make up the
;;; board name. It uses the slot parameter to create the right address to read the on board 
;;; ROM for that particular slot.
;;;

(defun get-board-type (slot)             
  (let (c1 c2 c3)
    (when (setf c1 (ignore-errors
		     (si:%nubus-read-8b (+ *slot-space* slot)
					*board-type-offset*)))
      (setf c2 (ignore-errors
		 (si:%nubus-read-8b (+ *slot-space* slot)
				    (+ *board-type-offset* 4))))
      (setf c3 (ignore-errors
		 (si:%nubus-read-8b (+ *slot-space* slot)
				    (+ *board-type-offset* 8))))
      (format nil "~a~a~a"
	      (character (cond (c1) (t " ")))
	      (character (cond (c2) (t " ")))
	      (character (cond (c3) (t " ")))))))

;;;
;;; the following function loops through all slots looking for "CSI", which it the
;;; CSIB board. It returns that slot.
;;;

(defun find-slot (id-string)
  (dotimes (i number-of-slots)
    (when (string-equal id-string (get-board-type i))
      (return i))))

;;;
;;; The following constant represents an array of two elements. Each element corresponds to one of
;;; the two on board color map buffers. Each of the buffers is in turn a 3 element array, one
;;; element each for the RED, the GREEN, and the BLUE portion of the color map buffer.
;;;
;;; Each of these three elements will be set to a 256 element array displaced to the hardware
;;; address. Then, by use of AREF one can directly read the contents.
;;;



;;;
;;; the following function loops through buffer 0 and 1 calling
;;; make-map to make an array displaced to the appropriate portion of the hardware.
;;;


(defun initialize-lut-buffers ()
  (dotimes (i number-of-buffers nil) (setf (aref lut-buffers i) (make-lut-buffer i))) 
)

;;;
;;; this function makes the displaced array to the appropriate part of hardware.
;;;


(defun make-lut-buffer (which-lut-buffer)
  (make-physical-array number-of-color-locations 
		       :element-type '(unsigned-byte 32) ;;; this is an art-32b array, only lowest byte is actually used
                                        ;;; ^this funny looking number is what you use if you want common lisp....
		       :displaced-to-physical-address 
		       (physical-address          
			  *lut-buffers-offset*    ; points to where buffers are located
			  (* which-lut-buffer #x1000)          ; 0 for buffer 0, 1000 for buffer 1.
			  *buffer-red-address*       ; this offset points to the proper part.
                       ) ; end of generate the address
  ) ; end of make-physical array call
) ; end function













