;;;;-*-Mode:Common-LISP;Package:TV;Base:10;Fonts:(MEDFNT HL12BI MEDFNB)-*-


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
;;; Copyright (C) 1989 Texas Instruments Incorporated. All rights reserved.

;;;
;;; The functions in this file define access to the video attributes control register.
;;; An array is created displaced to the location of the register.
;;;
;;;; See the hardware reference for what these registers are used for.
;;;;
;;;; HARDWARE REFERENCES (P/N 2534334-0001 DATED 31 MARCH 1987)
;;;;
;;;;   4.2.10, 4.2.10.1 THROUGH 4.2.10.5 
;;;;   


;;; This code adapted from code originally written for the CSIB-Inspector
;;; Adapted 4/8/87 by Ken Wood
;;;
;;; Change History:
;;;
;;;  02/01/89   MAY   Change type type from :array to 'array for Common Lisp compatibilty
;;;  08/20/88   KJF    Change to (control-register-flavor :before :init)
;;;                           during addition of Multiple Monitor (MMON) support.
;;;  08/24/87   KWW  Changes based on code reading
;;;                            1. Changed :type to :element-type for common lisp
;;;                            2. changed pallette to LUT buffer
;;;                            3. Changed control-register flavor to seperate constants from instance variables
;;;                            4. made bit-value function keyword based
;;;  08/17/87   KWW  Removed code that prevented proper re-initializationa after booting
;;;                           Fixed up some typographical errors in rarely used routines, such as set-monochrome-polarity
;;;                           Fixed error in  set-monochrome-plane
;;;  05/05/87   kww   Changed so that this can be used to supplement sheet rather than be a mixin to it.
;;;                        the primary change is this comment, since the code turned out to be fine as is.

;;;;
;;;; CONFIGURATION REGISTER stuff
;;;;

;;; the only thing this register is used for is to completely reset the CSIB board.
;;; I assume that calling :csib-reset once the window system is up and running on the
;;; color board would be a real big mistake...
;;;



;;; removed the (if already exists test) 8/17/87
(defun make-config-register ()
    (setf *config-register*
	  (make-physical-array 1
			     :element-type '(unsigned-byte 32) ;;; :type 'art-32b
			     :displaced-to-physical-address
			     (physical-address *config-register-offset*))))

;;;
;;; ==================================================================
;;;

;;; added doc-string
(defun csib-reset ()
  "This function forces the CSIB to reset, which fatally crashes the window system.
   However, if we need a software method to force a display of the CSIB self test,
   this function does it."
  
  (let ((config-state (aref *config-register* 0)))
    (if (logbitp *reset-bit* config-state)
	(setf (aref *config-register* 0) (logand config-state
						 (dpb 0 (byte 1 (+ 16. *reset-bit*))
						      (dpb 0 (byte 1 *reset-bit*) #xFFFFFFFF))))
	;;; else
	(setf (aref *config-register* 0) (logior config-state
						 (dpb 1 (byte 1 (+ 16. *reset-bit*))
						      (dpb 1 (byte 1 *reset-bit*) 0)))))
    (sleep 1) 
    (setf *current-color-lut-buffer* 0)))


;;; The following functions initialize some important variables used in determining the
;;; physical address of board hardware. SEE SPECIFICALLY PARAGRAPH 4.8 OF THE HARDWARE SPEC.
;;;


(DEFUN get-NuBus-base ()
  (if (typep *nubus-base-register* 'array)
      (ldb (BYTE 4 4) (aref *NuBus-base-register* 0))
      *slot-base*))

;;;
;;; ==================================================================
;;;


(defun make-NuBus-base ()
  (setf *NuBus-base* (get-NuBus-base)))

;;;
;;; ==================================================================
;;;

(DEFMACRO handling-nubus-errors (&body body)
  "Returns result if no error, t if error but proceed, nil if no proceed"
  `(CONDITION-CASE (value)
       (PROGN ,@body)
     (eh:User-NuBus-Error
      (LET ((address (+ (* #x1000000 (LOGAND #xFF (SEND value :hi-addr)))
			(LOGAND #xFFFFFF (SEND value :lo-addr))))
	    (click "Click here to proceed"))
	(SELECT (SEND value :nubus-tms) 
	  (si:%NuBus-tms-no-error
	   (tv:mouse-confirm (format nil "No Error") click))
	  (si:%NuBus-tms-error
	   (tv:mouse-confirm (format nil "NuBus Error at #x~8,48x" address) click))
	  (si:%NuBus-tms-timeout
	   (tv:mouse-confirm (format nil "NuBus Timeout at #x~8,48x" address) click))
	  (si:%NuBus-tms-GACBL
	   (tv:mouse-confirm (format nil "NuBus GACBL at #x~8,48x" address) click))
	  (OTHERWISE
	   (tv:mouse-confirm (FORMAT nil "Unknown NuBus TMS code") click)))))))

;;;
;;; ==================================================================
;;;

(DEFUN set-NuBus-base (NuBus-base) 
  (cond ((null (handling-nubus-errors
		 (setf (aref *NuBus-base-register* 0)
		       (dpb NuBus-base #o0404 0))))	   
	 (setf *NuBus-base* *slot-base*))
	((not (= NuBus-base (get-NuBus-base)))
	 (tv:mouse-confirm
	   "Unable to set the NuBus Base register"
	   "Click here to acknowledge")
	 (setf *NuBus-base* *slot-base*))
	((numberp (handling-nubus-errors
		    (aref (make-physical-array 1
				      :element-type '(unsigned-byte 32) ;;;  :type 'art-32b
				      :displaced-to-physical-address
				      (physical-address
					(- *slot-space-address*)
					(* nubus-base #x10000000)))
			  0)))
	 (setf *NuBus-base* NuBus-base))
	((tv:mouse-confirm
	   "WARNING - unable to verify addressability. Proceeding could be dangerous"
	   "Click here to proceed anyway. Move away to abort.")
	 (setf *NuBus-base* NuBus-base))
	(t (setf *NuBus-base* *slot-base*))))

(defun make-NuBus-base-register ()
  (setf *NuBus-base-register*
	(make-physical-array 1 :element-type '(MOD 256) 
			     :displaced-to-physical-address
			     (physical-address
			       *NuBus-base-register-offset*))))

;;;;;
;;;;;  The Video Attributes Control Register Defined.
;;;;;

(DEFCONSTANT monochrome-blanking-bit 3.)
(DEFCONSTANT color-blanking-bit 4.)
(DEFCONSTANT monochrome-polarity-bit 5.)
(DEFCONSTANT lut-buffer-select-bit 6.)
(DEFCONSTANT lut-buffer-load-bit 7.)

;;; striped out the constant instance variables and made them constants:
(DEFFLAVOR control-register-flavor
	   ((control-register nil) ; these guys are initialized by a function
	    (monochrome-plane nil)
	    (primary-lut-buffer 0))
	   ()
  (:inittable-instance-variables)
  (:settable-instance-variables primary-lut-buffer))

(DEFMETHOD (control-register-flavor :before :init) (&rest ignore)
  ;; Do not set/reset this, it will be set by si:setup-sib-slots.
  ;; Changed when multiple monitor (MMON) support was added.  08/04/88 KJF.
  ;; -> (SETF *csib-slot* (find-slot *CSIB-board-type*))
  (make-nubus-base-register)
  (make-nubus-base)
  (set-NuBus-base *slot-base*)
  (make-config-register))

;;;
;;; ==================================================================
;;;
;;; The following is an :after :init so that we can be sure the base addresses are already set up
;;;

(DEFMETHOD (control-register-flavor :after :init) (&rest ignore)
	   (SETF control-register (make-physical-array 8. :element-type 'bit 
						   :displaced-to-physical-address
						   (physical-address
						     *control-register-offset*)))
	    (SETF monochrome-plane (make-physical-array 1. :element-type '(MOD 16) 
			:displaced-to-physical-address
			(physical-address *control-register-offset*))))


;;;
;;; the following little utility transforms words meaningful to various types of users
;;; into the actual state value that will carry out the action named. For example, 
;;; the word :BLANKED is translated into the constant LOW. Various methods can
;;; receive smart looking messages, such as :set-color-blanking :unblanked.
;;;

;;;  changed to be keyword oriented
(defun bit-value (on-off)		   
     (CASE on-off
	      (:on high)
	      (:off low)
	      (:low low)
	      (:high high)
	      (:blanked low)
	      (:unblanked high)
	      (:load low)
	      (:reset high)
	      ('t high)			   
	      ('nil low)
              (0 0)
              (1 1)
	      (:otherwise 1))) ; if no match, return a 1 as a safe choice

;;;
;;; The following methods provide means to read or change various parts of the video attribute control register.
;;;

(DEFMETHOD (control-register-flavor :monochrome-blanking) ()
"Return the current value of the monochrome blanking bit"
  (AREF control-register monochrome-blanking-bit))

;;;
;;; ==================================================================
;;;

(DEFMETHOD (control-register-flavor :set-monochrome-blanking)
	   (on-off)
"Set the bit according to the state passed in"
  (SETF (AREF control-register monochrome-blanking-bit) (bit-value on-off) ))  

;;;
;;; ==================================================================
;;;

(DEFMETHOD (control-register-flavor :color-blanking)
	   ()
"Return the current state of the color blanking bit"
  (AREF control-register color-blanking-bit))

;;;
;;; ==================================================================
;;;

(DEFMETHOD (control-register-flavor :set-color-blanking)
	   (on-off)
"Set the color-blanking bit according to the value passed in"
  (SETF (AREF control-register color-blanking-bit) (bit-value on-off)))

;;;
;;; ==================================================================
;;;

(DEFMETHOD (control-register-flavor :monochrome-polarity)
	   ()
"Return the current state of the polarity bit"
  (AREF control-register monochrome-polarity-bit))

;;;
;;; ==================================================================
;;;

(DEFMETHOD (control-register-flavor :set-monochrome-polarity)
	   (on-off)
"Set the polarity bit to the specified value"
  (SETF (AREF control-register monochrome-polarity-bit) (bit-value on-off)))

;;;
;;; ==================================================================
;;;

(DEFMETHOD (control-register-flavor :lut-buffer-select)
	   ()
"Return the state of the lut buffer select bit in the register"
  (AREF control-register lut-buffer-select-bit))

;;;
;;; ==================================================================
;;;

(DEFMETHOD (control-register-flavor :set-lut-buffer-select)
	   (lut-buffer)
"Set the lut-buffer select bit in the register"
  (SETF (AREF control-register  lut-buffer-select-bit)
	(CASE lut-buffer
	      (:toggle (- 1 (send self :lut-buffer-select)))
	      (:primary primary-lut-buffer)
	      (:secondary (- 1 primary-lut-buffer))
	      (:otherwise lut-buffer))))

;;;
;;; ==================================================================
;;;

(DEFMETHOD (control-register-flavor :lut-buffer-load)
	   ()
"return the current state of the lut-buffer load bit"
  (AREF control-register lut-buffer-load-bit))

;;;
;;; ==================================================================
;;;

(DEFMETHOD (control-register-flavor :set-lut-buffer-load)
	   (load-reset)
"Set the lut-buffer load bit to the specified state."
  (SETF (AREF control-register lut-buffer-load-bit) (bit-value load-reset)))


;;; I decided to keep this as a macro, since I need it also in the MAP.LISP file.

(DEFMACRO wait-with-timeout (60ths-of-a-second &body function)
  "Delay until function returns non-nil or 60ths-of-a-second have elapsed.
Returns t if function returned non-nil or nil if timeout elapsed."
  `(LOOP with future = (time:time-increment (time:time) ,60ths-of-a-second)
	 when (FUNCALL ,@function)
	 do (RETURN t)
	 when (time:time-lessp future (time:time))
	 do (RETURN nil)))

(DEFVAR *csib-lut-buffer-load-error* nil)

;;; the following function will force one of the two on board color map buffers
;;; to be loaded into the actual color lookup tables.
;;;

(DEFMETHOD (control-register-flavor :load-LUT-from-lut-buffer)
	   (&optional lut-buffer )
  "Wait for the CSIB to finish the previous lut-buffer load then load the specified
hardware lut-buffer buffer into the hardware LUT."
  (without-interrupts			                   ;Prevent a process switch 
    (WHEN lut-buffer                      ; if value passed in, use it; otherwise go with value already in hardware   
      (SEND self :set-lut-buffer-select lut-buffer))	   ;Select the specified lut-buffer
    (COND ((plusp (send self :lut-buffer-load)))	   ;Most of the time it should be ready
	  ((wait-with-timeout 2 #'PLUSP (send self :lut-buffer-load)))	   ;If not then wait
	  (t (SETF *csib-lut-buffer-load-error* lut-buffer)))  
              ;If timed out, Something is wrong - CSIB should have responded by now
    (SEND self :set-lut-buffer-load :load)))  ;Okay now load the actual color map


;;;
;;; ==================================================================
;;;

;;; if the monochrome display is blanked, report back :OFF. Else, report back plane being used for mono display
(DEFMETHOD (control-register-flavor :monochrome-plane)
	   ()
"report which plane is being used for monocrhome, if any"
  (IF (EQL (SEND self :monochrome-blanking) low)
      :off
      (LOGAND #x7 (aref monochrome-plane 0))))

;;;
;;; ==================================================================
;;;

;;; corrected error - referenced wrong location 8/17/87
;;; 8/24/1987 changed if => when
(DEFMETHOD (control-register-flavor :set-monochrome-plane)
	   (plane)
"Set which plane is to be used for generating monochrome output"
  (WHEN (NUMBERP plane)
   (progn
     (if (aref control-register monochrome-blanking-bit)
         (setf plane (logior plane #x8)))   ;;; retain the fourth bit set to one
     (setf (aref monochrome-plane 0) plane))))







