;;; -*- Mode:Common-Lisp; Package:MACINTOSH; Base:10; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB) -*-

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
;;; Copyright (C) 1988-1989 Texas Instruments Incorporated. All rights reserved.


;;
;; This file contains code for handing window-command initiated by the MAC.
;;

;;;                   Patch
;;;   Date     Author  Number   Description
;;;--------------------------------------------------------------------
;;;  02-14-89   LG      5-36   Added support for cbuffer structure's time-rcvd slot in tv:misc-debug-dump.
;;;  11-29-88   LG      5-30   Display the Mac's "watch" cursor during screen resizing.
;;;  10-16-88   LG      5-17   Added handler for opcode 3, tv:misc-debug-dump.
;;;  08-28-88   LG	      5-6     Bound SELF to SCREEN in resize-a-mac-resident-explorer-screen
;;;			   for GROW-BIT-ARRAY call.
;;; 05/10/88  LG 	      4-51	  Use tv:grow-bit-array to adjust the size of the screen buffer
;;; 			  and tv:redirect-array to change the size of the screen-array
;;; 			  in resize-a-mac-resident-explorer-screen just like
;;; 			  :change-of-size-or-margins does for sheet-screen-arrays.
;;; 			  Then make sure locations-per-line agrees with the width of
;;; 			  the screen's buffer-array as set by tv:grow-bit-array.
;;;			  In resize-screen, refresh screen if resizing failed and keep
;;;			  blinkers from blinking during screen resizing.


(DEFUN tv:misc-kill-screen (cmd)
  (WHEN (AREF *mac-resident-explorer-screens* (add:parm-32b cmd 0))
    (SEND (CAR (AREF *mac-resident-explorer-screens* (add:parm-32b cmd 0))) :kill)))


(DEFUN tv:misc-Mac-restart (cmd)
  (DECLARE (IGNORE cmd))
  (restart nil t))


(DEFUN tv:misc-resize-screen (cmd)
  (resize-screen (add:parm-32b cmd 0)
		 (add:parm-32b cmd 2)
		 (add:parm-32b cmd 1)))


;;;  Two functions, one to inhibit blinking of all blinkers on a screen, remembering
;;;  for each sheet whether they were allowed to blink or not, then another function to
;;;  reestablish the original blinkability state for each inferior sheet of the screen...

(DEFUN collect-and-turn-off-sheets-blinker-states (sheet)
  (CONS (CONS sheet
	      (PROG1 (tv:sheet-dont-blink-blinkers-flag sheet)
		     (SETF (tv:sheet-dont-blink-blinkers-flag sheet) 1)))
	(LOOP FOR inferior IN (SEND sheet :inferiors)
	      APPEND (collect-and-turn-off-sheets-blinker-states inferior))))

(DEFUN reestablish-sheets-blinker-states (blinker-states-list)
  (LOOP FOR (sheet . state) IN blinker-states-list
	DO (SETF (tv:sheet-dont-blink-blinkers-flag sheet) state)))

(DEFUN resize-screen (screen-id new-screen-height new-screen-width
		    &optional debug-p old-screen-height old-screen-width)
  (LET* (screen inferiors blinkability-states true-mouse-cursor
	 old-right old-bottom left top right bottom windows-to-resize
	 h-expansion-factor w-expansion-factor error (errors 0))

    (WHEN (SETF screen (the-screen (AREF *mac-resident-explorer-screens* screen-id)))
      (SETF inferiors (SEND screen :inferiors))
      (UNLESS old-screen-height
	(SETF old-screen-height (SEND screen :height)))
      (UNLESS old-screen-width
	(SETF old-screen-width (SEND screen :width)))
      (SETF new-screen-width (* 32 (CEILING new-screen-width 32)))
      
      (SETF h-expansion-factor (/ new-screen-height old-screen-height)
	    w-expansion-factor (/ new-screen-width old-screen-width))

      ;;  Ask the Mac whether the new screen size will fit.  Don't try if it won't...
      (IF (NOT (can-this-screen-be-resized
		 screen-id new-screen-width new-screen-height))
	  (pop-up-format-at-origin "~
Insufficient space in Mac's screen/window image
cache to accomodate the requested resizing.")
	;; else...
              ;;  Change the Mac's mouse cursor to the watch while we thrash bitmaps...
	(SETF true-mouse-cursor tv:mouse-blinker)
	(SEND *mac* :set-mouse-blinker (- 4 mac-mouse-cursor-offset))

	(tv:without-screen-management
	  (SETF blinkability-states
		(collect-and-turn-off-sheets-blinker-states screen))
	  (resize-a-Mac-resident-Explorer-screen
	    screen-id screen new-screen-height new-screen-width nil)
	  (LOOP for window in inferiors
		do
		(MULTIPLE-VALUE-SETQ (left top right bottom)
		  (SEND window :edges))
		(SETF old-right right old-bottom bottom)
		
		;;  If the screen is growing, expand the window proportionally...
		(WHEN (> w-expansion-factor 1)
		  (SETF right
			(+ left (ROUND (* w-expansion-factor (- right left))))))
		(WHEN (> h-expansion-factor 1)
		  (SETF bottom
			(+ top (ROUND (* h-expansion-factor (- bottom top))))))
		
		;;  Shrink the window to fit if it won't after the screen changes size...
		(WHEN (> right new-screen-width)
		  (SETF right new-screen-width))
		(WHEN (> bottom new-screen-height)
		  (SETF bottom new-screen-height))
		
		;; If the window needs to be resized and can be resized to this new
		;; size, then add it to the list of windows to be resized.  If it needs
		;; to be resized but cannot be, stop trying and tell the Mac we failed...
		(WHEN (OR (/= right old-right) (/= bottom old-bottom))
		  (IF (MULTIPLE-VALUE-SETQ (nil error)
			(SEND window :set-edges left top right bottom :verify))
		      (PUSH (LIST window left top right bottom) windows-to-resize)
		      ;; else...
		      ;; just tell user, truck on, if unresized window fits on resized screen...
		      (IF (AND (> new-screen-width right) (> new-screen-height bottom))
			  (pop-up-format-at-origin "~
Window ~S
could not resize because ~A
but still can fit on the resized ~dx~d screen."
						   window error
						   new-screen-height new-screen-width)
			  ;; else...
			  ;; count as a serious resize error, window won't fit on resized screen..
			  (INCF errors)
			  (pop-up-format-at-origin "~
Window ~S
prevented resizing this screen from ~dx~d to ~dx~d
because ~A."
						   window
						   old-screen-height old-screen-width
						   new-screen-height new-screen-width
						   error)))))
	  
	  ;;  Really change the windows-to-resize of the windows that needed it...
	  (IF (NOT (OR (ZEROP errors) debug-p))
	      (PROGN
		(resize-a-Mac-resident-Explorer-screen
		  screen-id screen old-screen-height old-screen-width nil)
		(SEND screen :refresh))
	    ;; else...
	    (resize-a-Mac-resident-Explorer-screen
	      screen-id screen new-screen-height new-screen-width t)
	    (LOOP for (window left top right bottom) in
		  windows-to-resize
		  do
		  (SEND window :set-edges left top right bottom))
	    ;;  Make sure all  windows get rebuilt correctly...
	    (DOLIST (i (w:sheet-inferiors screen))
	      (PROCESS-RUN-FUNCTION "Refresh"
				    #'(lambda (sheet)
					(WITHOUT-INTERRUPTS 
					  (AND (w:sheet-can-get-lock sheet)
					       (SEND sheet :refresh)))) i))
	    ))
	(reestablish-sheets-blinker-states blinkability-states)
	;;  Having done everything with screen management disabled, fix things up...
	(SEND screen :screen-manage)
	(w:who-line-clobbered)

	;;  Restore the Mac's mouse cursor to what the Explorer thinks it is...
	(SEND true-mouse-cursor :create-Mac-image-of-Explorer-mouse-cursor nil nil)))))


;;  A little routine to pop up a notification as near the origin as possible...
(DEFUN pop-up-format-at-origin (message-format &rest message-args)
  (IF tv:mouse-sheet
      (tv:pop-up-message (APPLY #'FORMAT nil message-format message-args)
			 tv:mouse-sheet '(:point 0 0))
      ;; else no screen yet created
      (SEND tv:cold-load-stream :string-out
	   (APPLY #'FORMAT nil message-format message-args))))

;;  The following routine does the necessary things to change the size of a
;;  Mac-resident Explorer screen...
;;;  08-28-88   LG	      5-6     Bound SELF to SCREEN for GROW-BIT-ARRAY call.
(DEFUN resize-a-mac-resident-explorer-screen (screen-id screen
					      new-screen-height
					      new-screen-width
					      tell-the-mac-p)
  
  ;;  Give the Explorer-side data structures the new size...
  (LET ((self screen)		; For GROW-BIT-ARRAY, SHEET-DEDUCE-AND-SET-SIZES. 
	(buffer-array (tv:screen-buffer screen)))
    ;;  Maybe change the screen's buffer array's size (if it's growing)...
    (SETF (tv:sheet-locations-per-line screen)
	  (CEILING (* new-screen-width (tv:screen-bits-per-pixel screen)) 32)) 
    (SETF (tv:screen-buffer screen)
          (tv:grow-bit-array
	    buffer-array
            (TRUNCATE (* 32. (tv:sheet-locations-per-line screen))
		      (tv:screen-bits-per-pixel screen))
            new-screen-height new-screen-width
            (tv:sheet-height screen) (tv:sheet-width screen)))
    ;;  Make location-per-line match the true width of the screen-buffer array.  If we just
    ;;  shrank the screen, tv:grow-bit-array did not change the size of the screen buffer
    ;;  array as we requested...
    (SETF (tv:sheet-locations-per-line screen)
	  (CEILING (* (tv:screen-bits-per-pixel screen)
		       (ARRAY-DIMENSION (tv:screen-buffer screen) 1)) 32))
    ;;  Remember the new screen-buffer as an undisplaced array iff it's really new...
    (UNLESS (EQ buffer-array (tv:screen-buffer screen))
      (SETF *undisplaced-Mac-window-arrays*
	    (CONS (CONS (tv:screen-buffer screen) screen)
		  (DELETE screen *undisplaced-Mac-window-arrays*
			  :key 'REST :test 'EQ))))
    ;;   Really change the size of the screen's screen-array to match the size of the
    ;;   buffer array (whether it changed or not), but iff we're exposed...
    (WHEN (tv:sheet-screen-array screen)
      (SETF (tv:sheet-screen-array screen)
	    (tv:redirect-array
	      (tv:sheet-screen-array screen)
	      (ARRAY-ELEMENT-TYPE buffer-array)
	      (ARRAY-DIMENSION buffer-array 1)
	      new-screen-height
	      buffer-array 0))
      (SETF (tv:sheet-old-screen-array screen) (tv:sheet-screen-array screen)))
    (SETF (tv:sheet-width screen) nil
	  (tv:sheet-height screen) nil)
    (tv:sheet-deduce-and-set-sizes new-screen-width new-screen-height
				     0 nil))
  ;;  Finally, give the new size to the Mac...
  (WHEN tell-the-mac-p
    (send-adjust-screen-size
      screen-id
      (tv:mac-explorer-window-id screen)
      (SEND screen :width) (SEND screen :height)
      (NOT (TYPEP screen 'sys:cold-load-stream)))
    (send-adjust-bit-array screen (tv:mac-explorer-window-id screen)
			   (NOT (TYPEP screen 'sys:cold-load-stream)))))

;;;
;;;   Opcode 3: accept a circular debug buffer dump fro the Mac...
;;;

(DEFUN tv:misc-debug-dump (cmd &aux (disp 0))
  ;;  If this is the first segment of a debug dump then there's some set-up work to be done
  (UNLESS *processing-a-debug-dump*
    (UNLESS mac:*tracing-off*
      (SETF mac:*tracing-off* (mac:dr t t)))
    (LET (first-word segment-size number-of-segments
	  second-word displ-to-start-of-foreward-chain displ-to-start-of-back-chain)
      (SETF first-word (add:parm-32b cmd 0))
      (SETF number-of-segments (LOGAND first-word #xFFFF))
      (SETF segment-size (/ (LOGAND first-word #xFFFF0000) (EXPT 2 16)))
      (SETF second-word (add:parm-32b cmd 1))
      (SETF displ-to-start-of-back-chain (LOGAND second-word #xFFFF))
      (SETF displ-to-start-of-foreward-chain
	    (/ (LOGAND second-word #xFFFF0000) (EXPT 2 16)))
      (remember-call :misc (add:parm-32b cmd 0) (add:parm-32b cmd 1)
		     (add:parm-32b cmd 2))
      ;;  Don't copy the 2 header words from the first segment...
      (SETF disp 2)
      ;;  Create the array to hold the dump, placing the info from the 2 header words into
      ;;  the array's leader...
      (SETF *processing-a-debug-dump*
	    (make-cbuffer :n-segments-rcvd 0
			  :n-segments-total number-of-segments 
			  :n-words-per-segment segment-size
			  :start-fchain displ-to-start-of-foreward-chain
			  :start-bchain displ-to-start-of-back-chain
			  :time-rcvd (GET-UNIVERSAL-TIME)
			  :make-array '(:dimensions (* number-of-segments segment-size)
						    :type art-32b)))))
  ;;  Copy the circular buffer data from this segment into the array.  On the 1st segment,
  ;;  don't copy the 2 header words so displacements within the dump will correspond
  ;;  exactly to indices in the array.
  (add:copy-parms-32b cmd *processing-a-debug-dump* :to-array
		      (- (cbuffer-n-words-per-segment) disp) disp
		      (+ (* (cbuffer-n-segments-rcvd) (cbuffer-n-words-per-segment))
			 disp -2))
  (LET ((end-displ1 (+ (* (1+ (cbuffer-n-segments-rcvd))
			  (cbuffer-n-words-per-segment)) -2))
	(end-displ2 (- (cbuffer-n-words-per-segment) disp)))
    (remember-call :misc end-displ2
		   (add:parm-32b cmd (- end-displ2 7))
		   (add:parm-32b cmd (- end-displ2 6))
		   (add:parm-32b cmd (- end-displ2 5)))
    (remember-call :misc end-displ1
		   (AREF *processing-a-debug-dump* (- end-displ1 7))
		   (AREF *processing-a-debug-dump* (- end-displ1 6))
		   (AREF *processing-a-debug-dump* (- end-displ1 5))))
  ;;  When we've received all the segments, move the array onto the list of successfully
  ;;  received debug dumps and clear the *processing-a-debug-dump* flag.
  (WHEN (= (INCF (cbuffer-n-segments-rcvd)) (cbuffer-n-segments-total))
    (IF (AND *debug-dumps*
	     (NULL (SECOND (FIRST *debug-dumps*))))
	(SETF (SECOND (FIRST *debug-dumps*)) *processing-a-debug-dump*)
      ;; else...
      (PUSH (LIST *processing-a-debug-dump* nil) *debug-dumps*))
    (SETF *processing-a-debug-dump* nil)))