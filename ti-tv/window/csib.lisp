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
; Copyright (C) 1987-1989 Texas Instruments Incorporated. All rights reserved.


;;; Change History
;;;
;;;  Date      Author	Description
;;; -----------------------------------------------------------------------------------------
;;; 01/27/89  MAY        Changed (sheet :set-plane-mask) to add LOGAND. SPR 9093.
;;; 01/27/89   KJF        Change to init-csib-registers during addition of Multiple Monitor (MMON) support.
;;; 04/23/88  CJJ, PMH, KJF  Changes for multiple screen support.  Search for CJJ and KJF.
;;; 02/16/88  KJF           Added a function which simply returns the plane-mask H/W register value.
;;; 8/25/87   KWW         Changes as a result of the code reading:
;;;                              1. change methods that accessed the fore and back ground registers to functions
;;;                              2. :type -> :element-type
;;;                              3. IF -> WHEN where appropriate
;;;                              4. Use PSETF in complement-bow-mode
;;;                              5. Deleted stuff on frame buffer select since we are only using one frame buffer right now
;;; 8/17/87   Ken Wood    Changed complement-bow-mode to manipulate instance variables rather
;;;                                 than hardware registers
;;;                             Removed code that prevented re-initializing to a different slot after booting.
;;; 5/05/87    Ken Wood   Get rid of "CSIB-MIXIN concept, and make these methods of sheet instead.
;;;                             This makes integration much easier, because adding new methods to an existing
;;;                             flavor is far easier than attempting to redefine a very low level flavor, such
;;;                             as essential window, just to force a pre-concieved mental concept that this
;;;                             code is in some way "more fundamental".......
;;; 4/8/87     Ken Wood    Make work with real hardware, using block-flavor
;;;                             and control-register flavor software.
;;; 3/20/87    Ken Wood   General Cleanup
;;; 1/15/87    Rick Still    Original Code concepts


;;; The following function needs only to be called once, early in the window system initialization,
;;; but not super early. It just needs to be called before trying to do anything with color.

(DEFUN init-csib-registers ()
  ;; Do not set/reset this, it will be set by si:setup-sib-slots.
  ;; Changed when multiple monitor (MMON) support was added.  08/04/88 KJF.
  ;; -> (SETF *csib-slot* (find-slot *CSIB-board-type*))
  (SETQ *control-register* (MAKE-INSTANCE 'control-register-flavor))
  (make-blocks)				   ; this makes 16 register blocks, one for each frame buffer gateway
  (initialize-lut-buffers)		   ; this makes displaced arrays to the color map buffers
  ;;; setup foreground and background registers with default colors
  (SEND (AREF *blocks* 0) :set-foreground-color-register (IF *color-system*
							     *default-foreground*
							     255.))
  (SEND (AREF *blocks* 0) :set-background-color-register (IF *color-system*
							     *default-background*
							     0))
  ;;; MAJOR HACK ALERT:
  ;;;   Here follows the one true kludge in the color stuff. The microcode needs to know what saturate and
  ;;;   clamp values to use for add with saturate and subtract with clamp. The microcode looks in the
  ;;;   foreground and background registers of an as now unused control block.
  ;;; setup saturate and clamp values for micro-code access
  (SEND (AREF *blocks* kludge-block) :set-foreground-color-register *last-color-location*)
  (SEND (AREF *blocks* kludge-block) :set-background-color-register *first-color-location*))

;;; note - there is only one control register and one configuration register, so those
;;; messages are sent directly to the *control-register* instance.
;;;  See the file control-register.lisp for these methods.
;;; For the register block stuff, there are 16 possible register blocks, so we let the
;;; CSIB supplement to sheet select the block of interest, and send messages to the appropriate block.
;;;  The file block-register.lisp contains the methods that are referenced by these methods.
;;;

;;; CJJ 04/22/88.   Added for multiple screen support.
(DEFUN CSIB-in-slot-p
       (f-slot)
  (STRING-EQUAL (sys:board-type f-slot) tv:*CSIB-board-type*))

;;; CJJ 04/22/88.   Added for multiple screen support.
(DEFUN color-monitor-present-on-CSIB-p
       (&optional (CSIB-f-slot tv:sib-slot-number))
  (ZEROP (si:%nubus-read-8b CSIB-f-slot si:%CSIB-color-presence-test-result-byte-offset)))

;; For dual monitor support.  04/23/88 KJF.
(DEFUN setup-mono-plane (plane)
  (SEND *control-register* :set-monochrome-blanking 1.)
  (SEND *control-register* :set-monochrome-plane plane))


;;;;***********************************************************************
;;; here are the register block oriented methods that are mode independent:
;;;;***********************************************************************

;;; >>> modified 7/7/87 to be methods of SHEET as per Keith's suggestion.

(defmethod (sheet :WRITE-PLANE-MASK)  (value &optional (reg *current-reg-block*))
  "Sets hardware plane mask in the Register Block."
  (send (aref *blocks* reg) :set-plane-mask-register value))

(DEFMETHOD (sheet :set-plane-mask) (value)
  (SETF (sheet-plane-mask self) value)		; put value in instance variable
  (WHEN (on-screen self)			; update hardware if on screen
    ;; may 01/12/89 for CJJ. LOGAND'ing prevents overridding the screen's plane-mask. 
    (SEND self :write-plane-mask (LOGAND (sheet-plane-mask (get-screen self)) value))))

(DEFUN PLANE-MASK-REGISTER (&optional (reg *current-reg-block*))
  "Returns Plane Mask value from Register Block reg-block."
  (send (aref *blocks* reg) :plane-mask-register))

;;;
;;; ===========================================================
;;;

(defmethod (SHEET :COMPLEMENT-BOW-MODE) ()
  "Reverses foreground and background colors for any mode."
    (PSETF (sheet-foreground-color self) (sheet-background-color self)
	   (sheet-background-color self) (sheet-foreground-color self)))

;;;
;;; ===========================================================
;;;

;;; These next few were changed from methods to functions, since they really didn't pertain to a
;;; window by window basis.

(DEFUN SET-TRANSPARENCY-register (value &optional (reg *current-reg-block*))
  "Sets Transparency value in Register Block."
 (WHEN (FIXNUMP value) ;;; fixnump test is a fail safe, since old windows had NIL as the default.
  (send (aref *blocks* reg) :set-transparency-register value)))

;;;
;;; ===========================================================
;;;

(DEFUN TRANSPARENCY-register (&optional (reg *current-reg-block*))
  "Returns Transparency mask byte."
  (send (aref *blocks* reg) :transparency-register))

;;;
;;; NOTE: Since the sheet flavor alreay has instance variables named foreground-color and
;;; background-color, methods already exist for getting the value of the sheet instance
;;; variables. These functions get the information directly from the hardware, or load a value
;;; directly into hardware. 



(DEFUN FOREGROUND-COLOR-REGISTER (&optional (reg *current-reg-block*))
  "Returns value held in the Foreground Color slot of Register Block  reg-block."
  (send (aref *blocks* reg) :foreground-color-register))

;;;
;;; ===========================================================
;;;

(DEFUN SET-FOREGROUND-COLOR-register (value &optional (reg *current-reg-block*))
  "Sets Foreground Color in Register Block  to value specified."
 (WHEN (FIXNUMP value) ;;; fixnump test is a fail safe, since old windows had NIL as the default.
  (send (aref *blocks* reg) :set-foreground-color-register value)))

;;;
;;; ===========================================================
;;;

(DEFUN BACKGROUND-COLOR-REGISTER (&optional (reg *current-reg-block*))
  "Returns Background Color value from  Register Block reg-block."
  (send (aref *blocks* reg) :background-color-register))

;;;
;;; ===========================================================
;;;

(DEFUN SET-BACKGROUND-COLOR-register (value &optional (reg *current-reg-block*))
  "Sets Background Color in Register Block reg-block to value specified."
 (WHEN (FIXNUMP value)
  (send (aref *blocks* reg) :set-background-color-register value)))

;;;
;;; ===========================================================
;;;

(defmethod (sheet :SET-PIXEL-MASK)  (32x1_art-32b_array &optional (reg *current-reg-block*))
  "Sets hardware pixel mask in all Register blocks."
  (let ((pixel-mask 
          (make-array 32 :element-type '(unsigned-byte 32) ;; 'art-32b
		      :displaced-to (+ block-base (* reg #x100) pixel-mask-offset))))
    (copy-array-contents 32x1_art-32b_array pixel-mask)))

;;;
;;; ===========================================================
;;;

(defmethod (sheet :PIXEL-MASK) (&optional (reg *current-reg-block*))
  "Returns hardware pixel mask."
  (let ((pixel-mask 
          (make-array 32 :element-type '(unsigned-byte 32) ;; 'art-32b
		      :displaced-to (+ block-base (* reg #x100) pixel-mask-offset))) 
        (32x1_art-32b_array (make-array 32 :element-type '(unsigned-byte 32) ;; 'art-32b
					)))
    (copy-array-contents pixel-mask 32x1_art-32b_array)
    32x1_art-32b_array))



;;;;********************************************************
;;;;  Methods Pertinent to Color Expansion Mode
;;;;********************************************************

;;; if these are settable, we don't need this code here!!!

(defmethod (SHEET :SET-FOREGROUND-COLOR) (value)
  "Sets Foreground Color in SHEET to value specified."
  (SETF (sheet-foreground-color self) value)) ; keep instance variable of sheet up to date


(defmethod (SHEET :SET-BACKGROUND-COLOR) (value)
  "Sets Background Color in SHEET to value specified."
   (SETF (sheet-background-color self) value)) ; keep instance variable of sheet up to date



;;;;********************************************************
;;;;  Methods Pertinent to Monochrome Operations Only
;;;;********************************************************

(defmethod (SCREEN :SET-REVERSE-VIDEO)  ()    ; it seems logical that only the screen understands this message
                                              ; since individual sheets cannot be toggled.
  "Toggles Monochrome Video Polarity bit in Video Attribute Register"
  (send *control-register* :set-monochrome-polarity (- 1 (send self :monochrome-polarity))))


;; MISC functions

;; Added 04/23/88 KJF.
(DEFUN swap-default-f-b ()
  "Swaps foreground and background colors.  Like doing a TERM-C,
but this works for monochrome screens on color monitor."
  (LET ((temp-foreground *current-foreground*))
    (SETQ *current-foreground* *current-background*) 
    (SETQ *current-background* temp-foreground))
  (kbd-screen-redisplay))


