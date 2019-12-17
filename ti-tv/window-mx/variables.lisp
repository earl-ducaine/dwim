;;; -*- Mode:Common-Lisp; Package:MAC-WINDOWS; Base:10; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB) -*-



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
;;; Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.

(DEFVAR *dont-clip-at-the-margins* nil
  "Bound to T by any routines that do not want clipping enforced at
the margins of the window.")

(DEFVAR *loops* 0)

(DEFVAR *length-of-circular-list* 256)

(DEFVAR *calls* (MAKE-LIST *length-of-circular-list*))

(DEFVAR *keep-recording* 0)

(DEFVAR *bitblted-images* nil)

(DEFVAR *images* (MAKE-ARRAY 256 :fill-pointer 0)
  "Debugging history built by the SAVE-AN-IMAGE-OF macro.")

(DEFVAR *all-debugging-classes* nil)

(DEFVAR *enabled-debugging-classes* nil)

(DEFVAR *debug-dumps* nil
  "A list of debug dumps in inverse order of receipt.")

(DEFVAR *processing-a-debug-dump* nil
  "While processing a debug-dump this contains the array into which the dump is being
received.")

(DEFVAR *I-wanna-run-real-slow* t
  "Safe but slow way to run.")

(DEFPARAMETER *default-stack-depth-to-record* 20
  "If no stack depth is specified in an invocation of REMEMBER-CALL, this many stack
levels will be recorded.")

(DEFVAR *tracing-off* t
  "REPORT-CALLS and SAVE-AN-IMAGE-OF do nothing if this is T.")

(DEFVAR *ignore-commands-for-the-Mac* nil
  "The :command-initiate method and friends only pretend to process command when this is T.
Bound to T within before-disk-save so it may be executed whether the Mac's state agrees with
the Explorer's or not.")


(DEFVAR *last-selected-Exp-screens-ID* nil
  "Negative if last selected Explorer screen's M-window is not the Mac's front window.
NIL if the Explorer's physical screen.")

(DEFCONSTANT ages-array-size 256.)

(DEFVAR *Mac-resident-Explorer-screens* (MAKE-ARRAY (1+ ages-array-size))
  "Indexed by Explorer-screen-ID, Ith entry contains a list of (1) the Explorer screen instance
corresponding to Exp screen ID I., and (2) that screen's last-selected window.  The zeroth
entry describes the Explorer's physical screen.  The rest describe Mac-resident Explorer
screens.  The entries are really Explorer-Screen structures.")

(DEFVAR *window-system-mouse-on-the-Mac* nil
  "T whenever the mouse as controlled by the window system is active
on the Macintosh's screen.")

(DEFPARAMETER *draw-char-cache* (make-draw-char-cache))

;;;###############################################################
;;;
;;;			Control for allocation/deallocation of window ids
;;;
;;;###############################################################

(DEFVAR *list-of-free-window-ids* nil)

(DEFVAR *largest-window-id-ever-assigned* -1)

(DEFVAR *window-id-lock* nil)



;;;###############################################################
;;;
;;;			Control for allocation/deallocation of screen ids
;;;
;;;###############################################################

(DEFVAR *list-of-exposed-Mac-screens* nil)

(DEFVAR *list-of-free-screen-ids* nil)

(DEFVAR *largest-screen-id-ever-assigned* 0
  "Never use screen id 0!!!")

(DEFVAR *screen-id-lock* nil)

(DEFPARAMETER *maximum-legal-Mac-explorer-screen-id*
	(1- (ARRAY-TOTAL-SIZE *Mac-resident-Explorer-screens*)))

;;;###############################################################
;;;
;;;			Control for moving bit arrays to/from the Mac
;;;
;;;###############################################################

(DEFVAR *bitblt-intermediate-array*
	(MAKE-ARRAY '(32 32) :element-type 'BIT :displaced-to-physical-address 0)
  "Indirected to the copy-array-address in the ACB space by the :copybits method
of the *mac* flavor instance with the proper height/width for moving a bit array to/from the
Mac.")

(DEFVAR *total-Mac-bit-array-cache-size* 0
  "Number of bytes allocated in the mac's physical memory to hold bit arrays for Explorer
windows and screens.")

(DEFVAR *has-the-Mac-forgotten-my-redirection?*
	(MAKE-ARRAY 256)
  "If the Nth entry is not NULL, then the Mac has no record of the current
redirection of the window whose window-id = N and, as of the last bit-array
cache flush, window N's image was in bit-array of the window pointed
to by the Nth entry in this array.")

(DEFVAR *all-windows-and-screens* (MAKE-ARRAY 256)
  "The Nth entry is the Nth window or screen.")

(DEFVAR *is-the-bit-array-Mac-resident?*
	(MAKE-ARRAY 256 :element-type 'BIT)
  "If the Nth entry = 1 then the Nth window's bit array is Mac resident.")

(DEFPARAMETER *maximum-legal-Mac-explorer-window-id*
	(1- (ARRAY-TOTAL-SIZE *all-windows-and-screens*)))

(DEFVAR *undisplaced-Mac-window-arrays* nil
  "An alist of  (bit-array . window) or (buffer-array . screen) entries.  Any array that is
eligible for becoming Mac-resident has an entry on this list.")

(DEFVAR *gba-window* nil
  "If currently adjusting the size of a window due to being called from tv:grow-bit-array
then this variable contains a three-item list:
  (window-being-changed  window-width-before-change  window-height-before-change).
If a bit array cache flush finds itself retrieving the bit array of the window specified here, it
gets the height and width for its bitblt from here rather than from the window's height and
width instance variables.")

;;;###############################################################
;;;
;;;			Control for LRU bit-array cache management...
;;;			...These variables are set by get-modified-rectangles...
;;;
;;;###############################################################

(DEFVAR *modified-rectangles* nil
  "The ACB returned by the ast :GetModifiedRectangles command. ")

(DEFVAR *n-descriptors* 0
  "Number of bit-array descriptors in *modified-rectangles*.")

(DEFVAR *nbytes-free-space* 0
  "What it says, as of last get-modified-rectangles call.")

(DEFVAR *fast-rb-q-limit* 3
  "Maximum  from-host queue length before RB mouse cursor drawing gets inhibited.")

;;; LISPM ALU's to Mac translations.

(DEFCONSTANT srcCopy 0
  "Mac transfer mode equivalent to LISPM w:alu-seta")
(DEFCONSTANT srcOr 1
  "Mac transfer mode equivalent to LISPM w:alu-ior")
(DEFCONSTANT srcXOR 2
  "Mac transfer mode equivalent to LISPM w:alu-xor")
(DEFCONSTANT srcBIC 3
  "Mac transfer mode equivalent to LISPM w:alu-andca")
(DEFCONSTANT notSrcCopy 4
  "Mac transfer mode. No equivalent LISPM alu exists")
(DEFCONSTANT notSrcOr 5
  "Mac transfer mode. No equivalent LISPM alu exists")
(DEFCONSTANT notSrcXor 6
  "Mac transfer mode. No equivalent LISPM alu exists")
(DEFCONSTANT notSrcBic 7
  "Mac transfer mode equivalent to LISPM w:alu-and")

(DEFPARAMETER *alu/transfer-mode-table*
	      (MAKE-ARRAY 32
			  :initial-contents
			  `(,srcCopy	   ;w:alu-setz (requires white source).
			    ,notSrcBic	   ;w:alu-and
			    ,srcBic	   ;w:alu-andca - erase
			    ,srcXor	   ;3 - setm (requires white source)
			    nil		   ;4 - andcm - double copybits needed.
			    ,srcCopy	   ;w:alu-seta
			    ,srcXor	   ;w:alu-xor
			    ,srcOr	   ;w:alu-ior
			    ,nil	   ;8 - andcb - double copybits needed.
			    ,notSrcXor	   ;9 - EQV, XNOR
			    ,notSrcCopy	   ;10 - SETCA
			    ,notSrcOr	   ;11 - ORCA
			    ,srcXor	   ;12 - SETCM (requires black source).
			    nil		   ;13 - ORCM - double copybits needed.
			    nil		   ;14 - ORCB - double copybits needed.
			    ,SrcCopy	   ;15 - SETO (requires black source).
			    ,srcOR		   ;16 - Transparency
			    ,srcOr		   ;17 - Max
			    ,notSrcBic	   ;18 - Min
			    ,srcOr		   ;19 - Average
			    ,srcOr		   ;20 - Add w/saturate
			    ,srcBic	   ;21 - Sub w/saturate
			    ,srcBic	   ;22 - Background
			    ,srcXor	   ;23 - Add
			    ,srcXor	   ;24 - Subtract
			    ))
  "ALIST which translates LISPM alu's to Mac transfer modes.")


;;
;; Window Crash codes
;;

(DEFVAR *mac-window-allocation-failure* 50.)
(DEFVAR *mac-screen-allocation-failure* 51.)
(DEFVAR *mac-keyboard-p-is-nil* 52.)
(DEFVAR *attempt-to-call-reset-mac-explorer-connection* 53.)

(DEFPARAMETER *MX-Window-Crash-Codes*
	      '(
		*mac-window-allocation-failure*
		*mac-screen-allocation-failure*
		*mac-keyboard-p-is-nil* 
		*attempt-to-call-reset-mac-explorer-connection*
		))

;;;###############################################################
;;;
;;;			Control for bitblt-blinker handling
;;;
;;;###############################################################

;;;  
;;;  *known-bitblt-blinkers* :  A list of three-item lists of the form
;;;
;;;	(bitblt-blinker-instance   Mac-cursor-image-or-nil   Mac-mask-image)

;;;     Built by :create-Mac-image-of-Explorer-mouse-cursor.  If the :array slot of
;;;     the bitblt-blinker-instance is all zeros, the entry is built with the value of the
;;;     Mac-cursor-or-nil item as NIL.  An :around :blink method has been added for
;;;     bitblt-blinkers that does not try to draw the blinker on a Mac if the blinker's size
;;;     fits within the Mac's 16x16 pixel limit.  If the :blink method of a bitblt-blinker
;;;     instance finds itself on the *known-bitblt-blinkers* list with a NIL
;;;     Mac-cursor-image-or-nil, it invokes :create-Mac-image-of-Explorer-mouse-cursor
;;;     on itself to correctly create the Mac-cursor-image and Mac-mask-image on the
;;;     assumption that while someone might :set-mouse-cursor with the :array slot all
;;;     zeros, they'll be sure to get the right bit pattern into the array before using it.

(DEFVAR *known-bitblt-blinkers* nil)


;;;
;;;   A pair of arrays to be indirected into two bit-arrays to allow them to be compared
;;;   16 bits at a time.  compare-bitblt-blinker-with-Mac-image uses them.
;;;
(DEFPARAMETER *16bit-array1* (MAKE-ARRAY 16 :element-type '(unsigned-byte 16)
					    :displaced-to nil))

(DEFPARAMETER *16bit-array2* (MAKE-ARRAY 16 :element-type '(unsigned-byte 16)
					    :displaced-to nil))
