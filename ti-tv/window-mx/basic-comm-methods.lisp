;;;-*- Mode:Common-Lisp; Package:MAC; Base:10; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB) -*-


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

;;; This file defines the window system commands for the MicroExplorer.

;----------------------------------------------------------------------------------------

;;;                   Patch
;;;   Date    Author  Number   Description
;;;--------------------------------------------------------------------
;;;  02-07-89   LG      5-32 Added :GetModifiedRectangles command.  Added ICAD's rubberbanded mouse cursor support.
;;;			Added fast dashed line support.
;;;  12-13-88   LG      5-31 A fake mouse event now sends newly selected window a
;;;  			:select-screen message, is detected iff (plusp mouse-valid).
;;;  11-27-88   LG      5-29 Previous fix faked only the down transition of the left button.
;;;  			Fix this by forcing a single left-click at the lower-right corner
;;;  			of the active window whenever a fake mouse event is
;;;  			detected.
;;;  11-21-88   LG      5-28  :mouse-position now interprets a negative mouse
;;;  button
;;;  			number as a fake mouse click, calculates the mouse
;;;  			coordinates to be the origin of the screen's currently selected
;;;  			window.
;;;  10-16-88   LG      5-19	Added :DrawFilledPolygon.
;;;  08-28-88   LG	      5-6     Remove the remember-call from :drawline.
;;;  08-28-88   LG	      5-6     Added remember-call to :DeallocateBitArray to track these.
;;;  07-19-88  LG       4-61	Made :mouse-position return mouse coordinates relative to
;;; 			mouse-sheet, not just relative to default-screen.
;;; 05-25-88  ab    4-53  	For GG.  Fix to FontMetrics.
;;; 05-09-88  LG 	  4-51   	Return a third value from the GetScreenSize command, T if
;;; 			automatic screen resizing is to be performed at boot time, NIL if
;;; 			not.
;;;   4-07-88    LG	        5-41       Added new optional argument COLOR to :DrawLine to
;;;   			        allow callers who want to to pass in a color to be used.
;;;   3-28-88    LG     5-36       Handle all alu's in :draw-line like other microExplorer
;;;   		      drawing methods do.  This corrects KEE's problems with
;;;		          strange looking arrows caused by us drawing black
;;;		          lines when they specified w:alu-setz.
;;;   3-17-88       KED       5.22      Allow multiple character mapping tables.
;;; 3-16-88     BJ     5.20    Add without-interrupts to set-mouse-blinker and copybits.


(DEFMETHOD (mac-flavor :mouse-position) ()
  "Returns the X and Y values of the mouse position and a button-event
if one occured.  A button-event has the bit format #b0000 0000 00UT DLMR
where:
  U = 1 means a mouse button up excursion occured,
  T = 1 means a mouse tripple click occured,
  D = 1 means a mouse double click occured,
  L = 1 means it was a mouse left button,
  M = 1 means it was a mouse middle button,
  R = 1 means it was a mouse right button.
Negative X/Y values indicate special events on the Mac.  The
x-coord and screen id  are converted from the 16-bit positive value
an art-16b array returns to a 16-bit signed value."
  (DECLARE (SPECIAL si:%driver-data-start))
  (LET ((mouse-x (si:%nubus-read-16b
		   si:*addin-memory-slot*
		   (+ si:%driver-data-start si:%dd-mouse-x)))
	(mouse-y (si:%nubus-read-16b
		   si:*addin-memory-slot*
		   (+ si:%driver-data-start si:%dd-mouse-y)))
	(mouse-screen-id (si:%nubus-read-16b si:*addin-memory-slot*
					     (+ si:%driver-data-start
			                        si:%dd-mouse-screen)))
	(mouse-button (si:%nubus-read-16b
			si:*addin-memory-slot*
			(+ si:%driver-data-start si:%dd-mouse-buffer)))
	(mouse-valid (si:%nubus-read-8b
		       si:*addin-memory-slot*
		       (+ si:%driver-data-start si:%dd-mouse-valid)))
	mouse-screen-descriptor)

    (WHEN (> mouse-valid 0)
      (si:%nubus-write-8b si:*addin-memory-slot*
			  (+ si:%driver-data-start si:%dd-mouse-valid)
			  0))

    (WHEN (> mouse-x #x7fff)
      (SETF mouse-x (- -1 (LOGXOR mouse-x #xffff))))

    (WHEN (> mouse-y #x7fff)			;Added 3/3/87 KED
      (SETF mouse-y (- -1 (LOGXOR mouse-y #xffff))))

    (WHEN (> mouse-screen-id #x7fff)
      (SETF mouse-screen-id (- -1 (LOGXOR mouse-screen-id #xffff))))

    (SETF mouse-screen-descriptor
	  (AREF *mac-resident-explorer-screens* (ABS mouse-screen-id)))

    ;;  If mouse-button is negative, then it's a fake mouse click for the screen's selected
    ;;  window.  Calculate mouse coordinates equal to the lower-right corner of
    ;;  the screen's selected window, change screen and selected window if
    ;;  necessary, but suppress the mouse event by zeroing mouse-button...
    (WHEN (AND (PLUSP mouse-valid) (> mouse-button #x7fff))
      (MULTIPLE-VALUE-SETQ (mouse-x mouse-y)
	(tv:sheet-calculate-offsets
	  (the-last-selected-window mouse-screen-descriptor) nil))
      (INCF mouse-x (1- (tv:sheet-width (the-last-selected-window
					  mouse-screen-descriptor))))
      (INCF mouse-y (1- (tv:sheet-height (the-last-selected-window
					  mouse-screen-descriptor))))
      (UNLESS (EQ (the-last-selected-window mouse-screen-descriptor) w:selected-window)
	(PROCESS-RUN-FUNCTION "Select Screen" #'FUNCALL
			      (the-last-selected-window mouse-screen-descriptor) :select-screen))
      (SETF mouse-button 0))

    (WHEN (AND (EQ tv:default-screen (the-screen mouse-screen-descriptor))
	       (NEQ tv:default-screen tv:mouse-sheet))
      ;; then we are not changing screens but in order to return mouse coordinates relative to the
      ;; mouse-sheet we must adjust mouse-x and mouse-y for the cumulative offset of
        ;; tv:mouse-sheet relative to the default-screen...
      (MULTIPLE-VALUE-BIND (cumulative-x-offset cumulative-y-offset)
	  (tv:sheet-calculate-offsets tv:mouse-sheet nil)
	(DECF mouse-x cumulative-x-offset)
	(DECF mouse-y cumulative-y-offset)))

    (VALUES
      mouse-x
      mouse-y
      (IF (AND (PLUSP mouse-valid) (PLUSP mouse-button)) mouse-button)
      mouse-screen-id)))

(DEFCONSTANT mac-mouse-cursor-offset #x10
  "Mac cursor index values 0-4 are reserved - so we offset our cursor index by
this value before sending it to the Mac.")


;;;   Arrays to be displaced to the art-1b arrays defining the current mouse blinker's
;;;   Mac glyph and Mac mask images.
(DEFVAR *art-16b-glyph* (MAKE-ARRAY 16 :element-type '(unsigned-byte 16)
				    :displaced-to nil))
(DEFVAR *art-16b-mask* (MAKE-ARRAY 16 :element-type '(unsigned-byte 16)
				   :displaced-to nil))

(DEFMETHOD (mac-flavor :set-mouse-blinker)
	   (CHARACTER &optional glyph mask x-offset y-offset
	    &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Sends a message to the Mac to use a different cursor."
  (LET ((acb (add:get-acb-fast 144)))
    (COND ((NOT (ZEROP character))		       ; Send just the resource number if specified.
	   (add:load-parms acb 16 (+ mac-mouse-cursor-offset character) 0))

	  (t
	   (WITHOUT-INTERRUPTS
	     ;; else send a zero resource number, the glyph, the mask, and the hot spot...
	     (ADJUST-ARRAY *art-16b-glyph* 16 :displaced-to glyph)
	     (ADJUST-ARRAY *art-16b-mask* 16 :displaced-to mask)
	     (add:load-parms acb 16
			     0
			     0
			     (AREF *art-16b-glyph* 0)  ; The glyph...
			     (AREF *art-16b-glyph* 1)
			     (AREF *art-16b-glyph* 2)
			     (AREF *art-16b-glyph* 3)
			     (AREF *art-16b-glyph* 4)
			     (AREF *art-16b-glyph* 5)
			     (AREF *art-16b-glyph* 6)
			     (AREF *art-16b-glyph* 7)
			     (AREF *art-16b-glyph* 8)
			     (AREF *art-16b-glyph* 9)
			     (AREF *art-16b-glyph* 10)
			     (AREF *art-16b-glyph* 11)
			     (AREF *art-16b-glyph* 12)
			     (AREF *art-16b-glyph* 13)
			     (AREF *art-16b-glyph* 14)
			     (AREF *art-16b-glyph* 15)
			     (AREF *art-16b-mask* 0)   ; The mask...
			     (AREF *art-16b-mask* 1)
			     (AREF *art-16b-mask* 2)
			     (AREF *art-16b-mask* 3)
			     (AREF *art-16b-mask* 4)
			     (AREF *art-16b-mask* 5)
			     (AREF *art-16b-mask* 6)
			     (AREF *art-16b-mask* 7)
			     (AREF *art-16b-mask* 8)
			     (AREF *art-16b-mask* 9)
			     (AREF *art-16b-mask* 10)
			     (AREF *art-16b-mask* 11)
			     (AREF *art-16b-mask* 12)
			     (AREF *art-16b-mask* 13)
			     (AREF *art-16b-mask* 14)
			     (AREF *art-16b-mask* 15)
			     x-offset		       ; The hot spot...
			     y-offset
			     ))))

    (SETF (add:opcode acb) #x7)
    (SETF (add:requestor-complete acb) t)
    (add:transmit-packet acb channel)))

;----------------------------------------------------------------------------------------

;;;
;;; Window Commands
;;;


(DEFMETHOD (mac-flavor :GetScreenSize)
	   (&aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Sets the screen-width and screen-height instance variables to the current size
of the LISPM window on the Mac screen. Returns the width and height."
						       ;(BREAK " :GetScreenSize")
  (LET (screen-resizing-at-boot-p
	(acb (add:get-acb-fast 8)))
    (SETF (add:opcode acb) #x0f)
    (add:transmit-packet-and-wait acb channel)
    (SETF screen-width (MAX 0 (- (add:parm-16b acb 2)
				 (add:parm-16b acb 0)
				 ))
	  screen-height (MAX 0 (- (add:parm-16b acb 3)
				  (add:parm-16b acb 1)
				  ))
	  screen-resizing-at-boot-p (NOT (ZEROP (add:parm-16b acb 4))))
    (add:return-acb acb t)
    (VALUES screen-width screen-height screen-resizing-at-boot-p)))


(DEFMETHOD (mac-flavor :AdjustBitArray)
	   (window width height contents-matter &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Causes the Mac to create a graphport to hold the bit array of window.
If it already exists then the Mac will resize it to width,height. If width and
height are zero then the graphport will have no bit array."
						       ;(BREAK " :AdjustBitArray")
  (LET (return-value
	(acb (add:get-acb-fast 8)))

    (add:load-parms acb 16
		    (Mac-window-id window)
		    width
		    height
		    (IF contents-matter 1 0))

    (SETF (add:opcode acb) #x10)
    (add:transmit-packet-and-wait acb channel)

    (SETF return-value (NOT (ZEROP (ADD:PARM-16B acb 0))))
    (add:return-acb acb t)
    return-value))

(DEFMETHOD (MAC-flavor :RedirectDrawing)	       ;
	   (window-being-redirected bitmap-redirected-to x y &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Updates the drawing indirection table maintained on the Mac. Drawing
done to window-being-redirected will actually take place to bitmap-redirected-to.
If (Mac-window-id window-directed-to) is zero then drawing goes to the screen."
						       ;(BREAK " :RedirectDrawing")
  (LET ((acb (add:get-acb-fast 8)))
    (SETF (add:opcode acb) #x11)
    (add:load-parms
      acb
      16
      (Mac-window-id window-being-redirected)
      (Mac-window-id bitmap-redirected-to)
      x
      y)
    (SETF (add:requestor-complete acb) t)
    (add:transmit-packet acb channel)))

(DEFMETHOD (mac-flavor :DeallocateBitArray)
	   (window &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Allows the Mac to destroy the graphport and reclaim the memory space
associated with this window."
  (LET ((acb (add:get-acb-fast 2)))
    (remember-call :selection self)
    (SETF (add:opcode acb) #x12)
    (add:load-parms acb 16 (Mac-window-id window))
    (SETF (add:requestor-complete acb) t)
    (add:transmit-packet acb channel)))


(DEFMETHOD (mac-flavor :TotalCacheSize) ()
  "Returns the number of bytes in the Mac's bit-array cache.  Uses a
strange option of the AdjustScreenSize command to avoid allocating a new
ACB/command opcode for this minor command."
  (LET ((add:*no-interrupt* inhibit-scheduling-flag)
	(acb (add:get-acb-fast 10))
	return-value)

    (SETF (add:opcode acb) #x13)
    (add:load-parms acb 16 0 0 0 0 4)
    (add:transmit-packet-and-wait acb channel)
    (SETF return-value (ADD:PARM-32B acb 0))
    (add:return-acb acb t)
    return-value))


(DEFMETHOD (mac-flavor :AdjustScreenSize)
	   (Explorer-screen-ID Explorer-window-ID width height visibility)
  "Creates or changes the size of a Mac-resident Explorer screen."
  (LET ((add:*no-interrupt* inhibit-scheduling-flag)
	(acb (add:get-acb-fast 10))
	return-value)
    (SETF (add:opcode acb) #x13)
    ;;(BREAK " :AdjustScreenSize")
    (add:load-parms acb 16
		    Explorer-screen-ID Explorer-window-ID width height
		    (TYPECASE visibility
		      (symbol (IF visibility 1 0))
		      (fixnum visibility)))

    (add:transmit-packet-and-wait acb channel)
    (remember-call :adjscr (ADD:PARM-16B acb 0))
    (SETF return-value (NOT (= #xFFFF (ADD:PARM-16B acb 0))))
    (add:return-acb acb t)
    return-value))


(DEFMETHOD (mac-flavor :SelectWindow)
	   (window hide-instead-of-select-p)
  "Make the Mac window containing the specified Explorer window the
top-most Mac window so the user can see it."
  (LET ((add:*no-interrupt* inhibit-scheduling-flag)
	(acb (add:get-acb-fast 4)))

    (SETF (add:opcode acb) #x14)
    (SETF (add:requestor-complete acb) t)
    (add:load-parms acb 16 (Mac-window-id window) (IF hide-instead-of-select-p 1 0))
    (add:transmit-packet acb channel)))


(DEFMETHOD (mac-flavor :GetModifiedRectangles)
	   (&aux (add:*no-interrupt* inhibit-scheduling-flag))
  (LET (acb n-windows-with-mac-resident-bitmaps acb-image)
    (SETF n-windows-with-mac-resident-bitmaps
	  (LOOP for (ARRAY . window) in *undisplaced-mac-window-arrays*
		for wid = (tv:sheet-window-id window)
		count (AND (INTEGERP wid)
			   (NOT (AREF *has-the-Mac-forgotten-my-redirection?* wid)))))
    (SETF acb (add:get-acb-fast (+ 4 (* 16 n-windows-with-mac-resident-bitmaps))))
    (SETF (add:opcode acb) #x08)
    (add:transmit-packet-and-wait acb channel)
    (SETF acb-image (MAKE-ARRAY (ARRAY-TOTAL-SIZE acb) :element-type '(unsigned-byte 16)))
    (COPY-ARRAY-CONTENTS acb acb-image)
    (add:return-acb acb t)
    acb-image))
;----------------------------------------------------------------------------------------

;;;;
;;;; Mac commands
;;;;

(DEFMETHOD (mac-flavor :DrawLine)
	   (x1 y1 x2 y2 alu draw-end-point window
	    &optional (line-width 1) (color w:black)
	    &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Draws a line on the Mac screen from x1,y1 to x2,y2 with LISPM alu."
  (DECLARE (IGNORE draw-end-point))
						       ;(BREAK " :DrawLine")
  ;; If line-width <> 1 then a complex calculation of pen-width and pen-height needs to go here...
  ;;   It would calc pen width and height from the geometric relationship of line angle and line width.
  ;;   Line end point will also have to be adjusted if pen width and height are not 1
  ;;   For speed should look for special cases of vertical and horizontal line.
  (LET ((pattern (adjust-source-pattern alu color)))
    (MULTIPLE-VALUE-BIND (p1 p2 p3 p4)
        (mac-pattern (IF pattern pattern color))
      (LET ((acb (add:get-acb-fast 32)))
	(SETF (add:opcode acb) #x4)
	(SETF (add:requestor-complete acb) t)
	(add:load-parms acb 16
			p1 p2 p3 p4
			x1 y1 x2 y2
			line-width line-width	       ;simple way out for now
			(transfer-mode alu)
			(Mac-window-id window)
			w:*clipping-rectangle-left-edge*
			w:*clipping-rectangle-top-edge*
			(MIN #X3FFF W:*CLIPPING-RECTANGLE-RIGHT-EDGE*)
			(MIN #X3FFF W:*CLIPPING-RECTANGLE-BOTTOM-EDGE*))
	(add:transmit-packet acb channel)))))

(DEFMETHOD (mac-flavor :Nop)
	   (&aux (add:*no-interrupt* inhibit-scheduling-flag))
  "This sends a nil command to the Mac. Mostly for timing purposes. This requires
330us. It represents the fastest we will be able to deliver commands to the Mac."
  ;(BREAK " :Nop")
  (LET ((acb (add:get-acb-fast 2)))
    (SETF (add:opcode acb) #xff)
    (SETF (add:requestor-complete acb) t)
    (add:transmit-packet acb channel)))


;;; Text output routines

(DEFMACRO Character-Mapping-Table (font)
  "Returns the right character mapping table depending on the font."
  `(IF (FBOUNDP 'tv:Explorer-to-Mac-char-code-map)	    ;Test only necessary for patch.
       (tv:Explorer-to-Mac-char-code-map ,font)
     tv:*Explorer-to-Mac-char-code-map*))

(DEFMETHOD (mac-flavor :DrawCharacter)
	   (font char x y alu window &optional size)
  "Draw LISPM CHAR on the Mac screen at x,y in LISPM FONT with LISPM ALU.
If size is specified then it it used instead of the standard font size and the
font baseline is assumed to be size."
  (LET ((add:*no-interrupt* inhibit-scheduling-flag))
    (font-cache font)
    (LET ((acb (add:get-acb-fast 24)))
      (SETF (add:opcode acb) #x1)
      (SETF (add:requestor-complete acb) t)
      (add:load-parms acb 16
		      (AREF (Character-Mapping-Table font)
			    (CHAR-CODE char))
		      current-mac-font
		      current-mac-font-style
		      (OR size current-mac-font-size)
		      x (+ y (OR size current-mac-font-baseline))
		      (transfer-mode alu)
		      (Mac-window-id window)
		      w:*clipping-rectangle-left-edge*
		      w:*clipping-rectangle-top-edge*
		      (MIN #X3FFF W:*CLIPPING-RECTANGLE-RIGHT-EDGE*)
		      (MIN #X3FFF W:*CLIPPING-RECTANGLE-BOTTOM-EDGE*))
      (add:transmit-packet acb channel))))


(DEFMETHOD (mac-flavor :DrawString)
	   (font string index end xpos ypos xlim alu window &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Draws string from index to end on the Mac."
  (DECLARE (inline graphic-char-p))
  (LET ((char-mapping-table (Character-Mapping-Table font))
	(fwt (w:font-char-width-table font))
	ch
	(index-of-first-char-to-not-draw index)
	(xpos-to-stop-at xpos)
	(limit (OR xlim w:*clipping-rectangle-right-edge*))
	(acb (add:get-acb-fast (+ (* 2 DrawString-parms) DrawString-max-size))))

    (SETF (add:opcode acb) #x3)

    (font-cache font)

    ;;Calculate length of string that fits in the clipping rectangle and stuff translated characters into the ACB.
    (DOTIMES (j (1- drawstring-max-size))
      (UNLESS (< index-of-first-char-to-not-draw end)
	(RETURN))			   ; reached the end
      (UNLESS (GRAPHIC-CHAR-P
		(SETQ ch (CHAR-CODE (AREF string index-of-first-char-to-not-draw))))
	(RETURN))			   ; non printing char
      (IF (< limit (INCF xpos-to-stop-at (AREF fwt ch)))
	  (PROGN (DECF xpos-to-stop-at (AREF fwt ch))	   ; this character goes too far
		 (RETURN))
	  ;; else
	  (SETF (add:parm-8b acb (+ (* 2 Drawstring-Parms) j))
		(AREF char-mapping-table ch))
	  (INCF index-of-first-char-to-not-draw)))

    (remember-call :drawing window xpos-to-stop-at index-of-first-char-to-not-draw)

    (add:load-parms-16b
      acb
      current-mac-font
      current-mac-font-style
      current-mac-font-size
      (transfer-mode alu)
      0
      (- index-of-first-char-to-not-draw index)
      xpos (+ ypos current-mac-font-baseline)
      xpos-to-stop-at
      (Mac-window-id window)
      w:*clipping-rectangle-left-edge*
      w:*clipping-rectangle-top-edge*
      (MIN #x3FFF w:*clipping-rectangle-right-edge*)
      (MIN #x3FFF w:*clipping-rectangle-bottom-edge*))
    (SETF (add:requestor-complete acb) t)
    (add:transmit-packet acb channel)

    (DPB (- index-of-first-char-to-not-draw index)
	 (BYTE 12. 12.)
	 xpos-to-stop-at)))

(DEFMETHOD (mac-flavor :FontMetrics)
	   (font &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Gets the global width table the Macs version of LISPM-font. The returned
table is a 256 entry array indexed by the ASCII code. Although the Mac returns
fixed point values for the width the routine rounds off the fractional
part and returns an array of integers. Note that kerning information is
not returned."
  (font-cache font)

  (LET ((acb (add:get-acb-fast 1050))
	ascent
	descent
	leading
	widMax
	width-table-length
	vScale
	hScale
	width-table)

    (add:load-parms acb 16
		    current-mac-font
		    current-mac-font-style
		    current-mac-font-size)
    (SETF (add:opcode acb) #xd)
    (add:transmit-packet-and-wait acb channel)

    (SETF ascent (read-fix-32 (add:parm-16b acb 0)     ;Integer part
			      (add:parm-16b acb 1)))   ;Fractional part
    (SETF descent (read-fix-32 (add:parm-16b acb 2)
			       (add:parm-16b acb 3)))
    (SETF leading (read-fix-32 (add:parm-16b acb 4)
			       (add:parm-16b acb 5)))
    (SETF widMax (read-fix-32 (add:parm-16b acb 6)
			      (add:parm-16b acb 7)))
    (SETF width-table-length 256)
    (SETF vScale (* (read-fix-16 (add:parm-16b acb
					       (+ 8  (* 2 width-table-length))))
		    (read-fix-16 (add:parm-16b acb
					       (+ 10 (* 2 width-table-length))))))
    (SETF hScale (* (read-fix-16 (add:parm-16b acb
					       (+ 9  (* 2 width-table-length))))
		    (read-fix-16 (add:parm-16b acb
					       (+ 11 (* 2 width-table-length))))))

    (SETF vScale 1.0)				       ;Mac IV-41 says VFactor is not used. KED 3/1/88
    (SETF width-table (MAKE-ARRAY width-table-length
				  :fill-pointer #xFF
				  :leader-length 6
				  :leader-list
				  `(0		       ;Space for fill-pointer (unused)
				     ,(ROUND (* widMax hScale))	       ; See below.
				     ,(ROUND (* (+ ascent descent) vScale)) ;;height
				     ,(ROUND (* ascent vScale))
				     ,(ROUND (* descent vScale))
				     ,(ROUND (* leading vScale))
				     )))
    (LOOP with char-mapping-table = (Character-Mapping-Table font)
	  for char from 0 to 255
	  for translated-char = (AREF char-mapping-table char)
	  do (SETF (AREF width-table char)
		   (ROUND (* hScale
			     (read-fix-32 (add:parm-16b
					    acb
					    (+ 8 (* 2 translated-char)))
					  (add:parm-16b
					    acb
					    (+ 9 (* 2 translated-char))))))))
    (SETF (ARRAY-LEADER width-table 1)
	  (AREF width-table (CHAR-CODE #\m))); AB for GG  ;Use size of space char for tv:font-char-width
    (add:return-acb acb t)
    width-table))

(DEFUN read-fix-16 (number)
  "Converts a 16bit fixed point number II.FF to an Explorer flonum."
  (+ (LDB (BYTE 8 8) number)
     (/ (LDB (BYTE 8 0) number) 256.0)))

(DEFUN read-fix-32 (integer fractional)
  "Converts a 32bit fixed point number II.FF to an Explorer flonum."
  (+ integer
     (/ fractional 65536.0)))

(DEFUN adjust-source-pattern (alu source-pattern)
  (SELECTOR alu EQL
    ((12 15) w:black)
    ((w:alu-setz 3) w:white)
    (t source-pattern)))

(DEFMETHOD (mac-flavor :DrawTriangle)
	   (x0 y0 x1 y1 x2 y2
	    alu
	    draw-first-edge draw-second-edge draw-third-edge
	    pattern-or-nil
	    window &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Like si:%draw-shaded-triangle but draws it on the Mac screen.
For now edges are always drawn."
  (DECLARE (IGNORE draw-first-edge draw-second-edge draw-third-edge))
  (LET ((pattern (adjust-source-pattern alu pattern-or-nil)))
    (MULTIPLE-VALUE-BIND (p1 p2 p3 p4)
	(mac-pattern (IF pattern pattern w:black))
						       ;(BREAK " :DrawTriangle")
      (LET ((acb (add:get-acb-fast 32)))
	(SETF (add:opcode acb) #x5)
	(SETF (add:requestor-complete acb) t)
	(add:load-parms acb 16
			p1 p2 p3 p4
			x0 y0 x1 y1 x2 y2
			(transfer-mode alu)
			(Mac-window-id window)
			w:*clipping-rectangle-left-edge*
			w:*clipping-rectangle-top-edge*
			(MIN #x3FFF w:*clipping-rectangle-right-edge*)
			(MIN #x3FFF w:*clipping-rectangle-bottom-edge*))
	(add:transmit-packet acb channel)))))

(DEFMETHOD (mac-flavor :DrawFilledPolygon)
	   (vertices alu fill-pattern-or-nil window
	    &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Like si:%draw-shaded-triangle but draws an arbitrary filled polygon on the Mac
screen. For now edges are always drawn."
  (LET ((pattern (adjust-source-pattern alu fill-pattern-or-nil)))
    (MULTIPLE-VALUE-BIND (p1 p2 p3 p4)
	(mac-pattern (IF pattern pattern w:black))
      (LET* ((n-vertices (/ (LENGTH vertices) 2))
	     (acb (add:get-acb-fast (* 2 (+ 12 n-vertices n-vertices)))))
	(SETF (add:opcode acb) 14)
	(SETF (add:requestor-complete acb) t)
	(add:load-parms acb 16
			p1 p2 p3 p4
			(transfer-mode alu)
			(Mac-window-id window)
			n-vertices 0
			w:*clipping-rectangle-left-edge*
			w:*clipping-rectangle-top-edge*
			(MIN #x3FFF w:*clipping-rectangle-right-edge*)
			(MIN #x3FFF w:*clipping-rectangle-bottom-edge*))
	(LOOP for i from 32
	      for x-y-coord in vertices
	      do (SETF (AREF acb i) x-y-coord))
	(add:transmit-packet acb channel)))))

(DEFMETHOD (mac-flavor :DrawRectangle)
	   (x y width height color alu window &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Like si:%draw-rectangle but draws it on the Mac screen."
  (MULTIPLE-VALUE-BIND (p1 p2 p3 p4)
      (mac-pattern (adjust-source-pattern alu color))
						       ;(BREAK " :DrawRectangle")
    (LET ((acb (add:get-acb-fast 28)))
      (SETF (add:opcode acb) #x9)
      (SETF (add:requestor-complete acb) t)
      (add:load-parms acb 16
		      p1 p2 p3 p4
		      x y
		      (+ x width) (+ y height)
		      (transfer-mode alu)
		      (Mac-window-id window)
		      w:*clipping-rectangle-left-edge*
		      w:*clipping-rectangle-top-edge*
		      (MIN #x3FFF w:*clipping-rectangle-right-edge*)
		      (MIN #x3FFF w:*clipping-rectangle-bottom-edge*))
      (add:transmit-packet acb channel))))

(DEFMETHOD (mac-flavor :DrawHollowRectangle)
	   (x y width height thickness color alu window &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "Like si:%draw-rectangle but draws a hollow rectangle on the Mac screen."
  (MULTIPLE-VALUE-BIND (p1 p2 p3 p4)
      (mac-pattern (adjust-source-pattern alu color))
						       ;(BREAK " :DrawHollowRectangle")
    (LET ((acb (add:get-acb-fast 32)))
      (SETF (add:opcode acb) #xa)
      (SETF (add:requestor-complete acb) t)

      (add:load-parms acb 16
		      p1 p2 p3 p4
		      x y
		      (+ x width) (+ y height)
		      (transfer-mode alu)
		      thickness thickness
		      (Mac-window-id window)
		      w:*clipping-rectangle-left-edge*
		      w:*clipping-rectangle-top-edge*
		      (MIN #x3FFF w:*clipping-rectangle-right-edge*)
		      (MIN #x3FFF w:*clipping-rectangle-bottom-edge*))
      (add:transmit-packet acb channel))))

(DEFMETHOD (mac-flavor :DrawCircle)
	   (x y radius
	    &optional
	    (color w:black)
	    (alu w:alu-seta)
	    (num-points 29)
	    (draw-edge t) window &aux (add:*no-interrupt* inhibit-scheduling-flag))
  "This draws a filled oval just inside the rectangle specified by left, top, right, and bottom.
Like the graphics mixin :draw-filled-circle."
  (DECLARE (IGNORE num-points))
  (LET ((r (MAX 0 (- radius (IF draw-edge 0 1)))))
    (MULTIPLE-VALUE-BIND (p1 p2 p3 p4)
	(mac-pattern (adjust-source-pattern alu color))
						       ;(BREAK " :DrawCircle")
      (LET ((acb (add:get-acb-fast 28)))
	(SETF (add:opcode acb) #xb)
	(SETF (add:requestor-complete acb) t)

	(add:load-parms acb 16
			p1 p2 p3 p4
			(- x r) (- y r)
			(+ x r) (+ y r)
			(transfer-mode alu)
			(Mac-window-id window)
			w:*clipping-rectangle-left-edge*
			w:*clipping-rectangle-top-edge*
			(MIN #X3FFF W:*CLIPPING-RECTANGLE-RIGHT-EDGE*)
			(MIN #X3FFF W:*CLIPPING-RECTANGLE-BOTTOM-EDGE*))
	(add:transmit-packet acb channel)))))

(DEFMETHOD (mac-flavor :DrawHollowCircle)
	   (x y radius
	    &optional
	    (thickness 1)
	    (color w:black)
	    (alu w:alu-seta)
	    (num-points 29) window)
  "This draws a filled oval just inside the rectangle specified by left, top, right, and bottom.
Like the graphics mixin :draw-circle."
  (DECLARE (IGNORE num-points))
  (LET ((add:*no-interrupt* inhibit-scheduling-flag))
    (MULTIPLE-VALUE-BIND (p1 p2 p3 p4)
	(mac-pattern (adjust-source-pattern alu color))
      ;;(BREAK " :DrawHollowCircle")

      (LET ((acb (add:get-acb-fast 32)))
	(SETF (add:opcode acb) #xc)
	(SETF (add:requestor-complete acb) t)

	(add:load-parms acb 16
			p1 p2 p3 p4
			(- x radius) (- y radius)
			(+ x radius) (+ y radius)
			(transfer-mode alu)
			thickness thickness
			(Mac-window-id window)
			w:*clipping-rectangle-left-edge*
			w:*clipping-rectangle-top-edge*
			(MIN #X3FFF W:*CLIPPING-RECTANGLE-RIGHT-EDGE*)
			(MIN #X3FFF W:*CLIPPING-RECTANGLE-BOTTOM-EDGE*))

	(add:transmit-packet acb channel)))))



;;  This is rather dirty but since we know that *bitblt-indirect-array*
;;  is always a displaced array, a physical array, and has 2 dimensions,
;;  and all we ever want to do is change the dimensions of this array in a
;;  way that will never violate the physical storage underlying the array,
;;  this is the fastest way to do it...
(DEFUN quick-change-of-two-dimension-indirect-arrays-dimensions (ARRAY h w)
  (sys:%P-STORE-CONTENTS-OFFSET w array 1)
  (sys:%P-STORE-CONTENTS-OFFSET (* h w) array 3))

(DEFMETHOD (mac-flavor :CopyBits)
	   (alu
	    width height
	    from-array from-x from-y
	    to-array to-x to-y
	    &optional (source-width width) (source-height height) replication delay-updates
	    restoration-p
	    &aux (add:*no-interrupt* inhibit-scheduling-flag) (add:*wait-error* nil))
  "Copies the from array to an intermediate physical array and then to the to array.
The purpose of the intermediate array is to word align it (adjust for source bitwise misalignment) and to
linearize it in physical memory.
    The source and destination are controlled by the type of from-array and to-array:
    IF from-array is number and to-array is a number then the transfer takes place between
two Mac bit maps with the number being the bitmap ID. A zero ID indicates
the source or destination is the screen.
    IF from-array is an array and to array is a number then the transfer takes place
from the LISPM array to a bitmap on the Mac.
    If from-array is a number and to-array is an array then the transfer takes place
from a Mac bitmap to a LISPM array.
    If both from-array and to-array are arrays the transfer is between arrays on the
LISPM."
  (LET (word-width)
    (SETF width (ABS width)
          height (ABS height))

    (UNLESS source-width
      (SETF source-width width))

    (UNLESS source-height
      (SETF source-height height))

    (COND
      ((AND (NUMBERP from-array) (NUMBERP to-array))	   ;Mac to Mac case
       ;(BREAK " :CopyBits Mac-to-Mac")
       (LET ((acb (add:get-acb-fast (* 2 (force-even CopyBits-parms)))))
	 (SETF (add:opcode acb) #x0)
	 (SETF (add:requestor-complete acb) t)
	 (add:load-parms acb 16
			 1
			 (transfer-mode alu)	       ;Mac sets srcMode
			 width height
			 from-array
			 to-array
			 from-x from-y
			 to-x to-y
			 source-width
			 source-height
			 (IF delay-updates 1 0)
			 (IF restoration-p 1 0))
	 (add:transmit-packet acb channel)))

      ((AND (ARRAYP from-array)	(NUMBERP to-array))	   ;LISPM to Mac case
       (WHEN replication
         (IF (AND (<= source-width 32) (< (* source-width 4) width))
	 (SETF source-width (* source-width 4)))
         (IF (AND (<= source-height 32) (< (* source-height 4) height))
	 (SETF source-height (* source-height 4))))
       (SETF word-width
	     (TRUNCATE 32. (ARRAY-ELEMENT-SIZE from-array)))
       (quick-change-of-two-dimension-indirect-arrays-dimensions
	 *bitblt-intermediate-array*
	 source-height (* word-width (CEILING source-width word-width)))

       ;(BREAK " :CopyBits Exp-to-Mac")
       (LET ((acb (add:get-acb-fast (+ (* (CEILING source-width 32) 4 source-height)
				       (* 2 (force-even CopyBits-parms))))))

	 (si:change-physical-array *bitblt-intermediate-array*
				   (+ (add:address acb)
				      add:*acb-overhead-bytes*
				      (* 2 (force-even CopyBits-parms))))
	 (SETF (add:requestor-complete acb) t)
	 (SETF (add:opcode acb) #x0)
	 (si-BITBLT			   ;Move image to intermediate array
	      w:alu-seta
	      source-width source-height
	      from-array from-x from-y
	      *bitblt-intermediate-array* 0 0)

	 (add:load-parms acb 16
	   (ARRAY-ELEMENT-SIZE from-array)	       ;type of the array
	   (transfer-mode alu)			       ;Mac sets srcMode
	   width height
	   -1					       ;negative tells Mac blt is comming from LISPM
	   to-array				       ;zero here means Mac screen else deexposed graphport
	   0 0					       ;from offset is meaningless in this case
	   to-x to-y
	   source-width
	   source-height
	   (IF delay-updates 1 0)
	   (IF restoration-p 1 0))
	 (add:transmit-packet acb channel)))

      ((AND (NUMBERP from-array) (ARRAYP to-array))	   ;Mac to LISPM
       (SETF word-width
	     (TRUNCATE 32. (ARRAY-ELEMENT-SIZE to-array)))
       (quick-change-of-two-dimension-indirect-arrays-dimensions
	 *bitblt-intermediate-array*
	 source-height (* word-width (CEILING source-width word-width)))

       (LET ((acb (add:get-acb-fast (+ (* (CEILING source-width 32) 4 source-height)
				       (* 2 (force-even CopyBits-parms))))))
	 (si:change-physical-array *bitblt-intermediate-array*
				   (+ (add:address acb)
				      add:*acb-overhead-bytes*
				      (* 2 (force-even CopyBits-parms))))
	 (SETF (add:requestor-complete acb) nil)
	 (SETF (add:opcode acb) #x0)

	 (add:load-parms acb 16
	   1
	   (transfer-mode w:alu-seta)	   ;Mac sets srcMode
	   source-width source-height
	   from-array			   ;zero here means Mac screen else deexposed graphport
	   -1				   ;negative tells Mac blt should go to LISPM
	   (IF replication 0 from-x) (IF replication 0 from-y)
	   0 0				   ;to offset is meaningless in this case
	   source-width
	   source-height
	   (IF delay-updates 1 0)
	   (IF restoration-p 1 0))
	 (add:transmit-packet-and-wait acb channel)
					   ;Bit image from Mac should be ready now.
	 (si-BITBLT alu			   ;Move it from the intermediate array to LISPM virtual memory
		    width height
		    *bitblt-intermediate-array*
		    (IF replication from-x 0) (IF replication from-y 0)
		    to-array to-x to-y)
	 (SETF (add:requestor-complete acb) t)
	 (add:return-acb-fast acb)
	 ))

      (t (si-BITBLT alu			   ;LISPM to LISPM case - Mac not involved
		    width height
		    from-array from-x from-y
		    to-array to-x to-y)))))


;;; ----------------------------------------------------------------------------------------------------------------------
;;;
;;;	ICAD's fast rubberbanded mouse cursor support...
;;;

(DEFMACRO SWAP-16b (Word)
  (ONCE-ONLY (Word)
    `(DPB (LDB (BYTE 8 0) ,Word) (BYTE 8 8) (LDB (BYTE 8 8) ,Word))))

(DEFMACRO SWAP-32b (Word)
  (ONCE-ONLY (Word)
    `(LET* ((n (ETYPECASE ,word
		(integer ,word)
		(mac-pointer (SEND ,word :pointer))
		(mac-handle (SEND ,word :handle))))
	    (swapped-n (DPB (swap-16b (LDB (BYTE 16 0) n)) (BYTE 16 16) (swap-16b (LDB (BYTE 16 16) n)))))
       (TYPECASE ,word
	 (integer swapped-n)
	 (mac-pointer (MAKE-INSTANCE 'mac-pointer :pointer swapped-n))
	 (mac-handle (MAKE-INSTANCE 'mac-handle :handle swapped-n))))))




(DEFMETHOD (MAC-FLAVOR :OPEN-RUBBER-BAND) (anchor-x anchor-y &optional (direction :from) (pattern :solid) (window tv:selected-window))
  (LET ((acb (add:get-acb-fast 36))
	clip-x1 clip-x2 clip-y1 clip-y2)

    (SETF (VALUES clip-x1 clip-y1) (tv:sheet-calculate-offsets window nil))
    (SETF clip-x2 (+ clip-x1 (tv:sheet-inside-right window)))
    (SETF clip-y2 (+ clip-y1 (tv:sheet-inside-bottom window)))

    (SETF (add:opcode acb) 23)
    (SETF (add:requestor-complete acb) t)
    (add:load-parms acb 16
		    anchor-x
		    anchor-y
		    *fast-rb-q-limit*
		    (+ (* 256 (ECASE pattern (:solid 0) (:dashed 1)))
		       (ECASE direction (:none 0) (:from 1) (:to 2)))
		    clip-x1
		    clip-y1
		    clip-x2
		    clip-y2)
    (add:transmit-packet acb mac:channel)))

(DEFMETHOD (mac:MAC-FLAVOR :CLOSE-RUBBER-BAND) (&optional leave-line-drawn)
  "Turns off rubber-banding. If leave-line-drawn is non nil the final line is left drawn and its location returned.
   Otherwise the return value has no meaning."
  (LET ((acb (add:get-acb-fast 20)))
    (SETF (add:opcode acb) 24)

    (COND (leave-line-drawn
	   (add:load-parms acb 16 1)
	   (SETF (add:requestor-complete acb) 1)
	   (add:transmit-packet-and-wait acb mac:channel))
	  (t
	   (add:load-parms acb 16 0)
	   (SETF (add:requestor-complete acb) 0)
	   (add:transmit-packet acb mac:channel)))


    (VALUES (add:parm-16b acb 1) (add:parm-16b acb 2) (add:parm-16b acb 3) (add:parm-16b acb 4))

    ))

(DEFMETHOD (mac:mac-flavor :draw-rubber-band) (x1 y1 x2 y2 &optional  (direction :from) (pattern :solid))
  (LET ((acb (add:get-acb-fast 24)))
    (SETF (add:opcode acb) 25)
    (add:load-parms acb 16
		    x1 y1 x2 y2
		    (ECASE direction (:none 0) (:from 1) (:to 2))
		    (ECASE pattern (:solid 0) (:dashed 1)))
    (add:transmit-packet acb mac:channel)))



;;; ----------------------------------------------------------------------------------------------------------------------
;;;
;;;	Fast dashed-line support...
;;;
;;; ----------------------------------------------------------------------------------------------------------------------
;;;  Although all the argument of the :draw-dashed-line methods appear to be supported, in reality the Mac-side code
;;;  always draws a dashed line using a pattern of
;;;
;;;	00001111                        	00000000
;;;	00001111                        	00000000
;;;	00001111                        	00000000
;;;	00001111            or    	00000000
;;;	00001111            	11111111
;;;	00001111              	11111111
;;;	00001111              	11111111
;;;	00001111              	11111111
;;;
;;;  depending on whether the line is more horizontal or vertical.  This results in approximately 4-pixel dashes spaced at 8
;;;  pixels.  This is MUCH faster than drawing lots of short lines and looks pretty good.

(DEFMETHOD (mac-flavor :DrawDashedLine)
	   (window x0 y0 x1 y1 alu dash-spacing space-literally-p offset dash-length color
	    &optional (thickness 1))
  (LET ((pattern (adjust-source-pattern alu color)))
    (MULTIPLE-VALUE-BIND (p1 p2 p3 p4)
        (mac-pattern (IF pattern pattern color))
      (LET ((acb (add:get-acb-fast 40)))
	(SETF (add:opcode acb) #x6)
	(SETF (add:requestor-complete acb) t)
	(add:load-parms acb 16
			p1 p2 p3 p4
			x0 y0 x1 y1
			thickness thickness
			(transfer-mode alu)
			(Mac-window-id window)
			dash-spacing (IF space-literally-p 1 0)
			offset dash-length
			w:*clipping-rectangle-left-edge*
			w:*clipping-rectangle-top-edge*
			(MIN #X3FFF W:*CLIPPING-RECTANGLE-RIGHT-EDGE*)
			(MIN #X3FFF W:*CLIPPING-RECTANGLE-BOTTOM-EDGE*))
	(add:transmit-packet acb channel)))))
