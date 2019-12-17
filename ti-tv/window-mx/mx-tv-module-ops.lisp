;;; -*- Mode:Common-Lisp; Package:tv; Base:10; Patch-file:nil; -*-


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



;;
;; MODULE-OP support
;;
;; This file contains all of the functions called from the tv
;; module ops in microcode and the bitblt routine if the
;; si:module-op-override special a-memory variable is not zero.
;;

(PROCLAIM '(SPECIAL si:module-op-override))

(DEFMACRO with-real-microcode (body)
  `(WITHOUT-INTERRUPTS
     (setf si:module-op-override 0)
     ,body
     (setf si:module-op-override 1)))

(DEFUN %draw-shaded-triangle (x1 y1 x2 y2 x3 y3 alu draw-first-edge
				  draw-second-edge draw-third-edge pattern-or-nil
				  Mac-window)
  (IF (mac-window-p Mac-window)
      (mac:send-draw-shaded-triangle x1 y1 x2 y2 x3 y3 alu draw-first-edge
				 draw-second-edge draw-third-edge pattern-or-nil
				 Mac-window)
      ;; else...
      (with-real-microcode
	(%draw-shaded-triangle x1 y1 x2 y2 x3 y3 alu draw-first-edge
				  draw-second-edge draw-third-edge pattern-or-nil
				  Mac-window))))

;; Pick up the doc string from DEFOP.
(SETF (DOCUMENTATION '%draw-shaded-triangle) (DOCUMENTATION (GET '%draw-shaded-triangle :previous-definition)))


(DEFUN %draw-rectangle (width height x-bitpos y-bitpos alu-function sheet)
  (IF (mac-window-p sheet)
      (send-draw-rectangle width height x-bitpos y-bitpos w:black alu-function sheet)
    ;; else...
      (with-real-microcode
	(%draw-rectangle width height x-bitpos y-bitpos alu-function sheet))))

;; Pick up the doc string from DEFOP.
(SETF (DOCUMENTATION '%draw-rectangle) (DOCUMENTATION (GET '%draw-rectangle :previous-definition)))


(DEFUN %draw-character (font char char-width-or-nil x y alu sheet)
  (IF (mac-window-p sheet)
      (mac:send-draw-char font char x y alu sheet)
      ;; else...
      (with-real-microcode
	(%draw-character font char char-width-or-nil x y alu sheet))))

;; Pick up the doc string from DEFOP.
(SETF (DOCUMENTATION '%draw-character) (DOCUMENTATION (GET '%draw-character :previous-definition)))


(DEFUN %draw-shaded-raster-line (x1 x2 y alu draw-last-point pattern-or-nil sheet)
  (IF (mac-window-p sheet)
      (mac:send-draw-line x1 y x2 y alu draw-last-point sheet)
      ;; else...
      (with-real-microcode
	(%draw-shaded-raster-line x1 x2 y alu draw-last-point pattern-or-nil sheet))))

;; Pick up the doc string from DEFOP.
(SETF (DOCUMENTATION '%draw-shaded-raster-line) (DOCUMENTATION (GET '%draw-shaded-raster-line :previous-definition)))


;; this is for the few places that Have to call bitblt even though they may appear to 
;; need to call the one on the mac.
(PROCLAIM  '(inline si-bitblt))
(defun si-bitblt (alu width height from-array from-x from-y to-array to-x to-y)
  (with-real-microcode
    (si:bitblt
      alu width height from-array from-x from-y to-array to-x to-y)))


;;;
;;;   The right thing for Mac-bitblt to do is
;;;	1.  be called if and only if the bitblt involves a window's bit-array or its screen image.
;;;	2.  for a Mac-window figure out which array(s), the from- and/or the to-, is on the Mac
;;;	    and call the proper routine with the Explorer array/Mac window id(s).
;;;   Mac-bitblt is more conservative than this right now.

(DEFUN Mac-bitblt (alu width height from-array from-x from-y to-array to-x to-y)
  (LET* (from-w from-x-offset from-y-offset to-w to-x-offset to-y-offset
	(inhibit-scheduling-flag t) (add:*no-interrupt* inhibit-scheduling-flag))
    
    (MULTIPLE-VALUE-SETQ (alu from-array from-x from-y)
      (mac:handle-forcing-of-destination-bits alu from-array from-x from-y))
    
    (MULTIPLE-VALUE-SETQ (from-w from-x-offset from-y-offset)
      (mac:setup-this-window-on-the-Mac from-array))
    (MULTIPLE-VALUE-SETQ (to-w to-x-offset to-y-offset)
      (mac:setup-this-window-on-the-Mac to-array))
    ;;  In case this last call flushed the Mac's bit-array cache, make sure the bit-array of
       ;;  the from-w is on the Mac now, too...
    (WHEN (INTEGERP from-w)
      (mac:move-a-windows-bit-array-to-the-mac from-w))
    (mac:remember-call :bit-arrays self from-w from-x-offset from-y-offset
	           to-w to-x-offset to-y-offset)
    (COND
      ((AND (ARRAYP from-w) (ARRAYP to-w))
       (with-real-microcode
         (si:bitblt
           alu width height from-array from-x from-y to-array to-x to-y)))
      
      (t
       (mac:send-bitblt alu width height
	            from-w (+ from-x from-x-offset) (+ from-y from-y-offset)
	            to-w (+ to-x to-x-offset) (+ to-y to-y-offset) from-array from-x from-y)))))




