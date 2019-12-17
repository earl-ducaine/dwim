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



;;;   This file contains the Explorer-Mac communications routines.

;;;   Note that each routine mimics some Explorer microcoded primitve.  This mimicry
;;;   must extend to returning the same values as the corresponding primitive does.

;;;--------------------------------------------------------------------
;;;                 Patch
;;;   Date  Author  Number   Description
;;;--------------------------------------------------------------------
;;;  02-22-89   LG      5-37   Check for zero-width or zero-height bitblt, do nothing.
;;;  02-07-89   LG      5-32   Add LRU-based bit-array cache management code.  Add fast dashed line support.
;;;  11-18-88   LG	    5-27	  Switch to "watch" Mac cursor when thrashing the bitmap cache.
;;;  11-16-88   LG      5-25   Make default value for remember-bit-array's bit-array param correct depending
;;;  			  on  whether window param is a screen or a window.
;;;  10-19-88   LG	      5.22   Since :draw-polyline no longer performs line width adjustment for Mac windows,
;;;			  removed the compensation for it from Send-Draw-Filled-Polygon.
;;;  10-17-88   LG      5.20   Enter error handler if can't fit a bit array in the cache.
;;;  10-16-88   LG      5.19   Added send-draw-shaded-polygon.  Protected it and
;;;			  send-draw-shaded-triangle from trying to draw too big a triangle.
;;;  10-04-88   LG	      5.15	  When asked to handle a window with a null
;;;  			  screen-array,  and the Mac knows about this window, just
;;;  			  retrieve it from the Mac -- it is a deexposure of a window with
;;;  			  no bit-array.  Handle the possibility that WINDOW has a NULL
;;;  			  screen-array correctly in retrieve-a-bit-array-from-the-Mac.
;;;  09-26-88   LG      5-12   Tell the Mac about the effective redirection-to-self when a
;;;  			  window with no bit-array is deexposed (screen-array is set to
;;;  			  NIL).
;;;  09-13-88   LG      5-10	  Bound inhibit-scheduling-flag and add:*no-interrupt* to T in all
;;;  			  send-xxx routines to protect against inter-process interference.
;;;  			  Tried to ensure all bit-arrays needed are Mac-resident before a
;;;  			  RedirectDrawing command is sent.
;;;  08-28-88   LG	      5-6     Deallocate Mac-side data structures for any window.
;;;  			   Bind *gba-window* in send-adjust-bit-array-maybe instead of
;;;  			   setting it.  Use correct height/width when BLTing the
;;;  			   *gba-window* in send-adjust-bit-array.
;;;  07-17-88   LG      4-59   Changed send-draw-shaded-triangle to expand the triangle by
;;;  			  one pixel to the right and down to compensate for the use of
;;;  			  just PaintPoly on the Mac.
;;;  06-07-88   LG      4-56	  Added two optional parameters to
;;;  			  retrieve-a-bit-array-from-the-mac so the call in
;;;  			  tv:grow-bit-array can pass the old height/width in (before the
;;;  			  array grew).  Bit array cache flushing now gets its bitblt's
;;;  			  height/width from *gba-window* where retrieve-a-bit-array...
;;;  			  leaves it if retrieving the window tv:grow-bit-array was
;;;  			  growing.
;;;  05-10-88   LG      4-51   If AdjScrSize command fails, don't try to retrieve the image of
;;;  			  the screen for which it failed when flushing the Mac's bitmap
;;;  			  cache -- it has already been retrieved and deallocated before
;;;  			  the failure was detected.
;;;			   Never add an entry to the *undisplaced-Mac-window-arrays*
;;;			   list in remember-bit-array unless the window's bit-array in not
;;;			   NIL.
;;;                                         In send-deactivate-window, when a Mac window is KILLed,
;;;                                         take its bit array off the *undisplaced-Mac-window-arrays* list.
;;;			  Made all heights/widths of bit-array transfers between the
;;;			  Explorer and the Mac be the heights/widths of the
;;;			  corresponding windows, not the array-dimensions of the
;;;			  Explorer copy of the array.  This allowed shrunken screens to
;;;			  appear their proper size after a don't-resize-screens boot.

;;;  04/28/88    LG      5-45   Ensured that source-width of a Mac-resident source was
;;;                  GG	   the width of the window, not the width of the screen-array.
;;;   			   Use this value and the real source-width for Mac-Mac and
;;;   			   Exp-Exp copies.

;;;  04/07/88    LG      5-41   Added new optional argument COLOR to send-draw-line
;;;   			   to allow callers who want to to pass in a color to be used.
;;;   			   Passed it on to the Mac using the new COLOR argument of
;;;   			   DrawLine.  Changed send-draw-point to pass its COLOR
;;;   			   argument on to send-draw-line.
	
(DEFUN give-a-window-an-id (window)
  "Allocates a new window id, puts it in WINDOW's foreground-color, and returns it."
  (LET (window-id (inhibit-scheduling-flag t))
    ;;Check if this window is the special local print window. 08-19-88
    (if (send window :send-if-handles  :printer-screen-p)  ;08-19-88 DAB
	(progn 
	  (SETF window-id 255.)
	  (SETF (AREF *all-windows-and-screens* window-id) window)
	  (SETF (AREF *has-the-Mac-forgotten-my-redirection?* window-id) nil)
	  (SETF (AREF *is-the-bit-array-Mac-resident?* window-id) 1)
	  (SETF (tv:Mac-explorer-window-id window) window-id)
	  )
	;else
	(SETF window-id (allocate-Mac-Explorer-window-id))
	(SETF (AREF *all-windows-and-screens* window-id) window)
	(SETF (AREF *has-the-Mac-forgotten-my-redirection?* window-id) window)
	(SETF (AREF *is-the-bit-array-Mac-resident?* window-id) 0)
	(SETF (tv:Mac-explorer-window-id window) window-id)
	) ;if 
    ))

;;;##########################################################
;;;
;;;     			LRU-base bit-array cache management...
;;;
;;;##########################################################

(DEFUN get-modified-rectangles ()
  (SETF *modified-rectangles* (SEND *mac* :getmodifiedrectangles))
  (SETF *n-descriptors* (LSH (AREF *modified-rectangles* 21) -8))
  (SETF *nbytes-free-space* (LOGAND #xFFFFFF (+ (ASH (AREF *modified-rectangles* 21) 16)
						   (AREF *modified-rectangles* 20))))
  (VALUES *nbytes-free-space*))

(DEFUN get-a-modified-area-descriptor (wid window)
  (DECLARE (IGNORE wid))
  (COMMENT This code cannot work until the Mac correctly maintains the modified-area rectangles.
	   It has been determined to be too expensive to maintain them.  So this routine now just returns a
	   rectangle equal to the full since of the specified window.  The following code has been left here as
	   an example of how to extract a modified-area rectangle from the info returned by the
	   :GetModifiedRectangles command.
	   (LOOP with acb-image16 = *modified-rectangles*
		 for i from 1 to *n-descriptors*
		 for i16 from 22 by 8
		 when (= wid (LSH (AREF acb-image16 (+ i16 1)) -8))
		 return (LIST (AREF acb-image16 (+ i16 5)) (AREF acb-image16 (+ i16 4))
			      (AREF acb-image16 (+ i16 7)) (AREF acb-image16 (+ i16 6)))))
  (LIST 0 0 (tv:sheet-width window) (tv:sheet-height window)))
  
(DEFUN print-modified-rectangles (&optional dump-p (stream t))
  (LET* (nbytes-free-space acb-image16 acb-image32 start-of-free-space)
    (SETF nbytes-free-space (get-modified-rectangles))
    (SETF acb-image16 *modified-rectangles*)
    (SETF start-of-free-space (- *total-mac-bit-array-cache-size* nbytes-free-space))
    (WHEN dump-p
      (SETF acb-image32 (MAKE-ARRAY (FLOOR (LENGTH acb-image16) 2)
				  :element-type '(unsigned-byte 32)
				  :displaced-to acb-image16))
      (FORMAT stream "~% ~8,'0x" (AREF acb-image32 10))
      (LOOP for ii from 1 to *n-descriptors*
	    for i32 from 11 by 4
	    do
	    (FORMAT stream "~% ~8,'0x ~8,'0x ~8,'0x ~8,'0x "
		(AREF acb-image32 (+ i32 0)) (AREF acb-image32 (+ i32 1))
		(AREF acb-image32 (+ i32 2)) (AREF acb-image32 (+ i32 3)))
	    finally (FORMAT t "~%")))

    
    (FORMAT stream "~%(~d descriptors, ~d bytes of free space starting at ~d)"
	    *n-descriptors* nbytes-free-space start-of-free-space)
    (LOOP for ii from 1 to *n-descriptors*
	  for i32 from 11 by 4
	  for i16 = (LSH i32 1)
	  for wid = (LSH (AREF acb-image16 (+ i16 1)) -8)
	  for macs-displ = (LOGAND #xFFFFFF (+ (ASH (AREF acb-image16 (+ i16 1)) 16)
					  (AREF acb-image16 i16)))
	  for time-of-last-update = (+ (ASH (AREF acb-image16 (+ i16 3)) 16)
				       (AREF acb-image16 (+ i16 2)))
	  for top = (AREF acb-image16 (+ i16 5))
	  for left = (AREF acb-image16 (+ i16 4))
	  for bottom = (AREF acb-image16 (+ i16 7))
	  for right = (AREF acb-image16 (+ i16 6))
	  for window = (AREF *all-windows-and-screens* wid)
	  for size = (* (CEILING (tv:sheet-width window) 8) (tv:sheet-height window))
	  do
	  (FORMAT stream "~%~s
     WID= ~3d.  Updated at ~d.  Modified rect = (~3d,~3d) (~4d,~3d).  Cache locations ~6d to ~6d."
		  window WID time-of-last-update top left bottom right macs-displ (+ macs-displ size)))))

(DEFUN acquire-Nbytes-of-cache-space (nbytes-to-acquire)
  (LET* (nbytes-free-space acb-image16)
    (SETF nbytes-free-space (get-modified-rectangles))
    (SETF acb-image16 *modified-rectangles*)
    (LOOP with nbytes-acquired = nbytes-free-space
	  when (>= nbytes-acquired nbytes-to-acquire) return t
	  for oldest-dbase = (LOOP with oldest-age = #x10000000 and oldest-dbase
				   for i from 1 to *n-descriptors*
				   for dbase from 22 by 8
				   for age = (+ (ASH (AREF acb-image16 (+ dbase 3)) 16)
						(AREF acb-image16 (+ dbase 2)))
				   for wid = (LSH (AREF acb-image16 (+ dbase 1)) -8)
				   when (AND (< age oldest-age)
					     (NOT (TYPEP (AREF *all-windows-and-screens* wid) 'mac-screen)))
				   do
				   (SETF oldest-age age)
				   (SETF oldest-dbase dbase)
				   finally (RETURN oldest-dbase))
	  when (NULL oldest-dbase) return NIL
	  for oldest-wid = (LSH (AREF acb-image16 (+ oldest-dbase 1)) -8)
	  for oldest-window = (AREF *all-windows-and-screens* oldest-wid)
	  do
#|
	  (WITH-STACK-LIST (modified-area-descriptor (AREF acb-image16 (+ oldest-dbase 5))
						     (AREF acb-image16 (+ oldest-dbase 4))
						     (AREF acb-image16 (+ oldest-dbase 7))
						     (AREF acb-image16 (+ oldest-dbase 6)))
|#
          (WITH-STACK-LIST (modified-area-descriptor 0 0 (tv:sheet-width oldest-window) (tv:sheet-height oldest-window))
	    (retrieve-a-bit-array-from-the-Mac oldest-window modified-area-descriptor))
	  (INCF nbytes-acquired (* (CEILING (tv:sheet-width oldest-window) 8) (tv:sheet-height oldest-window)))
	  (SETF (AREF acb-image16 (+ oldest-dbase 2)) 0
		(AREF acb-image16 (+ oldest-dbase 3)) #x1000))))

(DEFUN reorganize-bit-array-cache (nbytes-of-cache-needed)
  (LET (true-mouse-cursor)
    ;;  Change the Mac's mouse cursor to the watch while we thrash bitmaps...
    (SETF true-mouse-cursor tv:mouse-blinker)
    (SEND *mac* :set-mouse-blinker (- 4 mac-mouse-cursor-offset))
    (clean-up-deactivated-windows)
    (IF (< (get-modified-rectangles) nbytes-of-cache-needed)
	(IF (acquire-nbytes-of-cache-space nbytes-of-cache-needed)
	    (get-modified-rectangles)
	   ;; else...
	  (retrieve-all-bit-arrays-from-the-mac)))
    ;;  Restore the Mac's mouse cursor to what the Explorer thinks it is...
    (WHEN (TYPEP (tv:blinker-sheet true-mouse-cursor) '(OR mac-window mac-screen))
      (SEND true-mouse-cursor :create-Mac-image-of-Explorer-mouse-cursor nil nil))))


;;;##########################################################
;;;
;;;     			Bit array movement to/from the Mac
;;;
;;;##########################################################

(DEFUN remember-bit-array (window
			      &optional (bit-array
					  (IF (TYPEP window 'tv:standard-screen)
					      (tv:screen-buffer window)
					    (tv:sheet-bit-array window))))
  ;;  Always remember an activated bit-array/window pair on the front of the
  ;;  *undisplaced-Mac-window-arrays* list, a deactivated bit-array/window pair
  ;;  on the tail of the *undisplaced-Mac-window-arrays*  list, first deleting
  ;;  any existing entry for this window...
  (WITHOUT-INTERRUPTS 
    (LET ((temp (DELETE window *undisplaced-Mac-window-arrays* :key 'REST :test 'EQ)))
      (SETF *undisplaced-Mac-window-arrays*
	    (IF bit-array
		(IF (OR (NULL (tv:sheet-superior window))
			(SEND window :active-p))
		    (CONS (CONS bit-array window) temp)
		  (NCONC temp (LIST (CONS bit-array window))))
	      temp))
      (remember-call :bit-arrays (OR (NULL (tv:sheet-superior window))
				     (SEND window :active-p))))))


(DEFUN resize-bit-array-and-handle-cache-overflow
       (window window-id contents-matter-p)
  (LET* ((width (tv:sheet-width window))
	 (inhibit-scheduling-flag t)
	 (add:*no-interrupt* inhibit-scheduling-flag))
    (UNLESS (tv:sheet-bit-array window)
      (SETF width (- width)))
    (LOOP with size-of-new-bit-array = (* (CEILING (tv:sheet-width window) 8) (tv:sheet-height window))
	  FOR i FROM 0
	  UNTIL (SEND *mac* :adjustbitarray window-id width
		      (tv:sheet-height window) contents-matter-p)
	  DOING
	  (CASE i
	    (0 (reorganize-bit-array-cache size-of-new-bit-array))
	    (:otherwise (FERROR nil "Cannot fit the ~dx~d bit-array of ~s on the Mac."
				(tv:sheet-height window) (tv:sheet-width window) window))))
    (remember-call :bit-arrays window window-id width (tv:sheet-height window))))

(DEFUN move-a-windows-bit-array-to-the-mac (w)
  ;;  If we were passed a window in W instead of a window-id, handle it as
  ;;  well as the possibility that the window is deactivated...
  (LET* (window bit-array (original-w w) (inhibit-scheduling-flag t)
	 (add:*no-interrupt* inhibit-scheduling-flag))
    (UNLESS (INTEGERP w)
      (SETF window w)
      (SETF w (tv:mac-explorer-window-id window))
      (WHEN (EQ t w)
        (SETF w (give-a-window-an-id window))))
    (remember-call :bit-arrays w original-w
		   (AREF *is-the-bit-array-mac-resident?* w)
		   (tv:sheet-bit-array (AREF *all-windows-and-screens* w)))
    (WHEN (ZEROP (AREF *is-the-bit-array-mac-resident?* w))
      (SETF window (AREF *all-windows-and-screens* w))
      (resize-bit-array-and-handle-cache-overflow window w t)
      (WHEN (SETF bit-array (tv:sheet-bit-array window))
        (send-bitblt
	    tv:alu-seta
	    (tv:sheet-width window)
	    (tv:sheet-height window)
	    bit-array 0 0
	    w 0 0
	    nil 0 0 t)
        (SETF (AREF *is-the-bit-array-mac-resident?* w) 1)))))

(DEFUN get-total-cache-size ()
  (LET (cache-size)
    (SETF cache-size (SEND *mac* :totalcachesize))
    (remember-call :bit-arrays cache-size)
    cache-size))

;;;  10-03-88  LG  5.13	Handle the possibility that WINDOW has a NULL screen-array
;;;  			correctly.
(DEFUN retrieve-a-bit-array-from-the-mac (window &optional modified-area-descriptor)
  (UNLESS (OR *ignore-commands-for-the-mac*
	      (NULL window)	      
	      (TYPEP window 'w:screen))
    (LET* ((window-id (tv:mac-explorer-window-id window))
	   (bit-array (tv:sheet-bit-array window))
	   (mac-does-not-know-about-this-window
	     (AND (AREF *has-the-mac-forgotten-my-redirection?* window-id)
		  (ZEROP (AREF *is-the-bit-array-mac-resident?* window-id)))))
      (UNLESS modified-area-descriptor
	(SETF modified-area-descriptor (get-a-modified-area-descriptor window-id window)))
      (UNLESS mac-does-not-know-about-this-window
	;;  Make sure this window's inferiors have all been processed before it is...
	(LOOP for inferior in (tv:sheet-inferiors window)
	      do (retrieve-a-bit-array-from-the-mac inferior))
	;;  Copy the selected window's bit array from the Mac back to the Explorer...
	;;  We may have already done this window -- do nothing if we have...
	(remember-call :bit-arrays window window-id
		       mac-does-not-know-about-this-window
		       (tv:sheet-screen-array window) bit-array
		       (AREF *is-the-bit-array-mac-resident?* window-id)
		       (AREF *has-the-mac-forgotten-my-redirection?* window-id)
		       (WHEN modified-area-descriptor (LISTARRAY modified-area-descriptor)))
	(UNLESS (ZEROP (AREF *is-the-bit-array-mac-resident?* window-id))
	  (WHEN bit-array
	    (LET ((top 0) (left 0) width-to-retrieve height-to-retrieve)
	      (IF modified-area-descriptor
		  (SETF top (FIRST modified-area-descriptor)
			left (SECOND modified-area-descriptor)
			width-to-retrieve (- (THIRD modified-area-descriptor) top)
			height-to-retrieve (- (FOURTH modified-area-descriptor) left))
		;; else...
		(IF (AND *gba-window* (EQ window (FIRST *gba-window*)))
		    (SETF width-to-retrieve (SECOND *gba-window*)
			  height-to-retrieve (THIRD *gba-window*))
		  ;; else...
		  (SETF width-to-retrieve (tv:sheet-width window)
			height-to-retrieve (tv:sheet-height window))))
	      (UNLESS (OR (ZEROP width-to-retrieve) (ZEROP height-to-retrieve))
		(send-bitblt
		  tv:alu-seta width-to-retrieve height-to-retrieve
		  window-id top left
		  bit-array top left
		  bit-array))))
	  (SETF (AREF *is-the-bit-array-mac-resident?* window-id) 0)
	  (remember-call :bit-arrays window window-id bit-array *gba-window*
			 (tv:sheet-height window) (tv:sheet-width window)
			 (AREF *has-the-mac-forgotten-my-redirection?* window-id)))
	;;; Tell the Mac we're killing this window, regardless of whether or not there
	;;; was a Mac-resident bit array.  Remember to whom the window was redirected
	;;; when it was retrieved from the Mac.
	(SEND *mac* :deallocatebitarray window-id)
	(SETF (AREF *has-the-mac-forgotten-my-redirection?* window-id)
	      (IF (tv:sheet-screen-array window)
		  (AREF *all-windows-and-screens*
			(setup-this-window-on-the-mac
			  (tv:sheet-screen-array window) t))
		;; else...
		window)))
      window-id)))


(DEFUN retrieve-a-screens-bit-array-from-the-Mac (screen)
  (UNLESS *ignore-commands-for-the-Mac*
    (LET ((window-id (mac-window-p screen))
          (buffer-array (SEND screen :buffer)))
      (send-bitblt
	    tv:alu-seta
	    (tv:sheet-width screen)
	    (tv:sheet-height screen)
	    window-id 0 0
	    buffer-array 0 0)
      (SEND *mac* :DeallocateBitArray window-id)
      (SETF (AREF *is-the-bit-array-Mac-resident?* window-id) 0)
      (SETF (AREF *has-the-Mac-forgotten-my-redirection?* window-id) screen)
      (remember-call :bit-arrays screen window-id)
      window-id)))

(DEFUN restore-A-screens-bit-array-to-the-Mac (screen screen-id 
					 &optional rebuild-p)
  (UNLESS *ignore-commands-for-the-Mac*
    (LET ((window-id (mac-window-p screen))
          (buffer-array (SEND screen :buffer)))
      (send-adjust-screen-size
	screen-id
	(tv:Mac-Explorer-window-id screen)
	(tv:sheet-width screen)
	(tv:sheet-height screen)
	(IF rebuild-p t 3))		  ; Tell Mac not to rebuild its Mac window.
      (send-bitblt
	    tv:alu-seta
	    (tv:sheet-width screen)
	    (tv:sheet-height screen)
	    buffer-array 0 0
	    window-id 0 0
	    nil 0 0 t)
      (SETF (AREF *is-the-bit-array-Mac-resident?* window-id) 1)
      (SETF (AREF *has-the-Mac-forgotten-my-redirection?* window-id) nil))))


;;;  03/08/88   LG  	Changed calling sequence so si:before-disk-save can use it to
;;;  			just make all bit-arrays and buffer arrays Explorer-resident.

(DEFUN retrieve-all-bit-arrays-from-the-Mac
       (&optional and-dont-restore-p window-id-of-screen-not-to-retrieve)
  (UNLESS *ignore-commands-for-the-Mac*
    (LET* (exposed-windows screens (inhibit-scheduling-flag t)
	   (add:*no-interrupt* inhibit-scheduling-flag))
      (remember-call :bit-arrays self)
      ;;  Find all Mac-resident screens and their exposed windows...
      (MULTIPLE-VALUE-SETQ (screens exposed-windows)
	(LOOP FOR i FROM 1 TO *largest-screen-id-ever-assigned*
	      FOR screen = (the-screen (AREF *Mac-resident-Explorer-screens* i))
	      COLLECT screen INTO screens
	      WHEN screen APPEND (w:sheet-exposed-inferiors
				   screen) INTO exposed-windows
	      FINALLY (RETURN screens exposed-windows)))
      (w:delaying-screen-management
	;;  Retrieve all Mac-resident bit arrays, deallocate all GrafPorts...
	(LOOP FOR wid FROM 0 TO *largest-window-id-ever-assigned*
	      FOR window = (AREF *all-windows-and-screens* wid)
	      DO
	      (retrieve-a-bit-array-from-the-Mac window))
	
	;;  We just flagged every window-id currently in use by setting the
	;;  corresponding entry in the array *has-the-Mac-forgotten-my-redirection?*.
	;;  This will cause make-bit-array-mac-resident to send the Mac a
	;;  RedirectDrawing message to reestablish cognizance of the window's
	;;  redirection by the Mac...
	
	;; Empty the Mac's bit-array cache by retrieving all screens' buffer-arrays, too...
	(LOOP FOR screen IN screens
	      WHEN (AND screen
			(NEQ (mac-window-p screen)
			     window-id-of-screen-not-to-retrieve))
	      DO
	      (retrieve-a-screens-bit-array-from-the-Mac screen))
        
	;;  Once the bit-array cache has been cleared out, the Mac loses all
	;;  knowledge of redirections.  Each time a window is defined to the
	;;  Mac by AdjustBitArray we must check whether or not the Mac knows
	;;  the window's current redirection, and, if not, tell it.  We
	;;  cannot tell the Mac all the redirections now because the Mac
	;;  knows redirections only in terms of one window's GrafPort using
	;;  another window's Mac-resident bitarray, and at this time there
	;;  are no Mac-resident bitarrays.  Redirections are meaningful only
        ;;  within the context of the cacheing scheme.
	
	
	;;  Now that bit array storage is cleaned out restore all the screens...
	(UNLESS and-dont-restore-p
	  (LOOP FOR i FROM 1 TO *largest-screen-id-ever-assigned*
	        FOR screen = (the-screen (AREF *Mac-resident-Explorer-screens* i))
	        WHEN (AND screen
			(NEQ (mac-window-p screen)
			     window-id-of-screen-not-to-retrieve))
	        DO (restore-a-screens-bit-array-to-the-Mac screen i))))      
      )))


(DEFUN restore-all-bit-arrays-to-the-mac ()  
  (UNLESS *ignore-commands-for-the-mac*
    (LET (exposed-windows)
      ;; Get a list of all previously-visible windows...
      (SETF exposed-windows
	    (LOOP for i from 1 to *largest-screen-id-ever-assigned*
		  for screen = (the-screen (AREF *mac-resident-explorer-screens* i))
		  when screen append (w:sheet-exposed-inferiors
				       screen) into exposed-windows
		  finally (RETURN exposed-windows)))
      ;; Make all screens appear to have no exposed windows...
      (LOOP for i from 1 to *largest-screen-id-ever-assigned*
	    for screen = (the-screen (AREF *mac-resident-explorer-screens* i))
	    when screen do
	    (SETF (tv:sheet-exposed-inferiors screen) nil))
      ;; Make visible all previously-visible windows...
      (LOOP for w in exposed-windows do (SEND w :expose))
      (LOOP for i from 1 to *largest-screen-id-ever-assigned*
	    for screen = (the-screen (AREF *mac-resident-explorer-screens* i))
	    when screen
	    do (SEND screen :screen-manage)))))


;;;##########################################################
;;;
;;;           		     Draw-char cacheing routines
;;;
;;;##########################################################

(DEFUN add-to-draw-char-cache (font char x y alu mac-window)
  (WHEN (GRAPHIC-CHAR-P char)
    (WITHOUT-INTERRUPTS 
      (LET* ((fwt (tv:font-char-width-table font))
	     (wid (tv:font-char-width font))
	     (char-width (IF fwt (AREF fwt (CHAR-CODE char)) wid)))
	(WITH-STACK-LIST (new-state x mac-window y alu font
				    sys:clipping-rectangle-top-edge
				    sys:clipping-rectangle-bottom-edge
				    sys:clipping-rectangle-left-edge
				    sys:clipping-rectangle-right-edge
				    *dont-clip-at-the-margins*)
	  (UNLESS (EQUAL new-state (draw-char-cache-state))
	    (dump-draw-char-cache)	   ; Cache may be empty
	    (SETF (draw-char-cache-state) new-state
		  (draw-char-cache-start-x) x))
	  ;; Always put this character into the cache...
	  (VECTOR-PUSH char (draw-char-cache-string))
	  (INCF (draw-char-cache-state-next-x (draw-char-cache-state))
		char-width)
	  (remember-call :drawing *draw-char-cache*) t)))))

(DEFUN dump-draw-char-cache ()
  (WHEN (draw-char-cache-state)
    (DESTRUCTURING-BIND (state start-x string) *draw-char-cache*
      (LET ((nchars (LENGTH string)))
	(SETF (draw-char-cache-state) nil
	      (FILL-POINTER string) 0)
	(DESTRUCTURING-BIND (w y alu font
			       sys:clipping-rectangle-top-edge
			       sys:clipping-rectangle-bottom-edge
			       sys:clipping-rectangle-left-edge
			       sys:clipping-rectangle-right-edge
			       *dont-clip-at-the-margins*) (REST state)
	  (send-draw-string-internal w
				     string 0 nchars
				     start-x y font alu)
	  )))))


;;;##########################################################
;;;
;;;   			Screen/window creation messages...
;;;
;;;##########################################################

(DEFUN send-create-screen ()
  "Called when creating the Mac screen to (1) tell the Mac to create a window to receive
our commands and (2) get the size of this Mac window -- our screen."
  (DECLARE (VALUES Mac-screen-height Mac-screen-width))
  (LET* ((inhibit-scheduling-flag t)
	 (add:*no-interrupt* inhibit-scheduling-flag))
    (UNLESS *ignore-commands-for-the-Mac*
      (dump-draw-char-cache)
      (SEND *mac* :are-you-there?)
      (MULTIPLE-VALUE-BIND (width height)
	  (SEND *mac* :GetScreenSize)
	(remember-call :selection height width)
	(VALUES height width)))))



;;;##########################################################
;;;
;;;   			    Window activation messages...
;;;
;;;##########################################################

(DEFUN send-activate-window (window)
  "Make sure WINDOW's image, currently in its Explorer-resident bit
array. is stored its the Mac-resident bit array."
  (LET* ((window-id (ABS (tv:Mac-explorer-window-id window)))
	 (inhibit-scheduling-flag t)
	 (add:*no-interrupt* inhibit-scheduling-flag))
    (dump-draw-char-cache)
    (make-bit-array-Mac-resident window window-id)))


(DEFUN send-deactivate-window (window)
  "Save WINDOW's image from its Mac-resident bit array into its
Explorer resident bit array.  WINDOW's window-id is now reuseable."
  (LET* ((window-id (tv:Mac-explorer-window-id window))
	 (inhibit-scheduling-flag t)
	 (add:*no-interrupt* inhibit-scheduling-flag))
    (dump-draw-char-cache)
    (WHEN (AND (FIXNUMP window-id)); (tv:sheet-bit-array window))
      (retrieve-a-bit-array-from-the-mac window)
      (WHEN (si:function-active-p '(:method tv:sheet :kill))
	(SETF *undisplaced-Mac-window-arrays*
	      (DELETE window *undisplaced-Mac-window-arrays* :key 'REST :test 'EQ))
	(SETF (tv:sheet-bit-array window) nil
	      (tv:sheet-screen-array window) nil))
      (remember-call :selection window window-id))))

;;;##########################################################
;;;
;;;                                Window exposure messages...
;;;
;;;##########################################################

(DEFUN artificially-activate-a-window (window)
  "WINDOW is a deactivated Mac window -- make it look activated."
  (LET (window-id)
    (SETF window-id (give-a-window-an-id window))
    (make-bit-array-mac-resident window)
    window-id))

			       
;;;  10-4-88  LG	5.15	When asked to handle a window with a null
;;;  			screen-array,  and the Mac knows about this window, just
;;;  			retrieve it from the Mac -- it is a deexposure of a window with
;;;  			no bit-array.
			       
(DEFUN send-redirect-drawing (window &optional contents-matter-p)
  (UNLESS *ignore-commands-for-the-mac*
    (LET* (w	  		       ; window-id to which we are redirected.
	   wid			       ; window-id of this window.
	   x-offset y-offset
	   (inhibit-scheduling-flag t)
	   (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      
      ;;  Get specified window's id, handle redirection of a deactivated window...
      (SETF wid (tv:mac-explorer-window-id window))
      (WHEN (EQ t wid)
	(SETF wid (give-a-window-an-id window)))
 
      (IF (NULL (tv:sheet-screen-array window))
	  (retrieve-a-bit-array-from-the-mac window)
	;; else...
	;; Figure out to whom we are redirected and where, make its bit-array
	;; Mac-resident....
	(MULTIPLE-VALUE-SETQ (w x-offset y-offset)
	  (setup-this-window-on-the-mac (tv:sheet-screen-array window)))
	
	;;  If the window is redirected to an array as yet not associated with any Mac
	;;  window,  handle this gracefully if the array is this window's bit array...
	(WHEN (ARRAYP w)
	  (mac:remember-call :fatal
			     (NEQ (chase-displaced-pointers
				    (tv:sheet-screen-array window))
				  (tv:sheet-bit-array window)))
	  (IF (NEQ (chase-displaced-pointers (tv:sheet-screen-array window))
		   (tv:sheet-bit-array window))
	      (CERROR "onward" "window indirected to unknown array")
	    ;; else...
	    (remember-bit-array window (tv:sheet-bit-array window))
	    (MULTIPLE-VALUE-SETQ (w x-offset y-offset)
	      (setup-this-window-on-the-mac (tv:sheet-screen-array window)))
	    (SETF (AREF *has-the-mac-forgotten-my-redirection?* w) nil)
	    (SETF (AREF *is-the-bit-array-mac-resident?* w) 0)))
	
	;;  Refresh the Mac's knowledge of WINDOW's redirection iff necessary...
	(LET ((window-containing-last-saved-image
		(AREF *has-the-mac-forgotten-my-redirection?* wid)))
	  (WHEN window-containing-last-saved-image
	    
	    ;;  Move the bit array containing this window as of the last cache flush...
	    (move-a-windows-bit-array-to-the-mac
	      window-containing-last-saved-image)
	    
	    ;;  Resize the graphport of the window being redirected if necessary...
	    (UNLESS (AND (NULL (AREF *has-the-mac-forgotten-my-redirection?* wid))
			 (OR (= wid w)
			     (= wid (tv:mac-explorer-window-id
				      window-containing-last-saved-image))))
	      (resize-bit-array-and-handle-cache-overflow
		window wid contents-matter-p))
	    
	    ;;  In case the resize-bit-array-... flushed the cache, make sure...
	    (move-a-windows-bit-array-to-the-mac window-containing-last-saved-image)
	    (move-a-windows-bit-array-to-the-mac window))
	  
	  ;;  Tell the Mac about the redirection...
	  (SEND *mac* :redirectdrawing wid w x-offset y-offset)
	  (SETF (AREF *has-the-mac-forgotten-my-redirection?* wid) nil)
	  (remember-call :bit-arrays window wid
			 window-containing-last-saved-image
			 (AREF *has-the-mac-forgotten-my-redirection?* wid)
			 w x-offset y-offset))
	))))

(DEFUN send-adjust-bit-array (window window-id contents-matter-p)
  "Change the size the the Mac-resident window to match the size of the
Explorer-resident window (it just changed).  Allocate a Mac-resident bit-array if
the Explorer-resident window has one and the Mac-resident does not.  Signal a lack
of an Explorer-resident bit array by passing a negative width."
  (UNLESS *ignore-commands-for-the-mac*
    (dump-draw-char-cache)
    (LET (bit-array)     
      (SETF bit-array (tv:sheet-bit-array window))
      ;;  If the specified window's bit-array is not Mac-resident then we need to
      ;;  make IT Mac-resident, not the bit-array being used as
      ;;  make-bit-array-Mac-resident now does.
      ;;  Handle a deactivated window...
      (WHEN (EQ t window-id)
	(SETF window-id (give-a-window-an-id window)))
      (resize-bit-array-and-handle-cache-overflow window window-id contents-matter-p)
      (WHEN bit-array
	(WHEN (ZEROP (AREF *is-the-bit-array-mac-resident?* window-id))
	  (send-bitblt
		tv:alu-seta
		(IF (AND *gba-window* (EQ window (FIRST *gba-window*)))
		    (SECOND *gba-window*)
		  (tv:sheet-width window))
		(IF (AND *gba-window* (EQ window (FIRST *gba-window*)))
		    (THIRD *gba-window*)
		  (tv:sheet-height window))
		bit-array 0 0
		window-id 0 0
		nil 0 0 t)
	  (SETF (AREF *is-the-bit-array-mac-resident?* window-id) 1))
	;; Make sure window is on the *undisplaced-Mac-window-arrays* list...
	(remember-bit-array window bit-array)))))

(DEFUN send-adjust-bit-array-maybe (window &optional contents-matter-p
				        old-width old-height)
  "Does nothing if WINDOW is an Explorer window.  If a deactivated or previously
unknown Mac window, first gives WINDOW a window-id.  Once the Mac window has an id,
adjusts the window's Mac-resident size (and its bit array size if it has a bit array).
OLD-WIDTH and OLD-HEIGHT are supplied only if called by tv:grow-bit-array."
  (remember-call :bit-arrays self *gba-window*)
  (UNLESS *ignore-commands-for-the-Mac*
    (LET* ((window-id (tv:Mac-explorer-window-id window))
	   (*gba-window* (IF old-width (LIST window old-width old-height)))
	   (inhibit-scheduling-flag t)
	   (add:*no-interrupt* inhibit-scheduling-flag))
      (COND
	((NULL window-id)
	 (WHEN (TYPEP (tv:sheet-get-screen window) 'Mac-screen)
	   (send-adjust-bit-array
	     window
	     (give-a-window-an-id window)
	     contents-matter-p)))
	(t
	 (WHEN (EQ t window-id)
	   (SETF window-id (give-a-window-an-id window)))
	 (send-adjust-bit-array window window-id contents-matter-p))))))

;;;  03/08/88   LG  	If the :AdjustScreenSize command fails, retrieve ALL bit-arrays
;;;  		from the MAC instead of excluding the screen whose size we are
;;;  		adjusting.  This allows the Mac's cache to be flushed and
;;;  		reorganized, which leaving a screen's buffer array on the Mac did
;;;  		not.
;;;  09-07-88   LG   	Since the Mac code reports bit-array-cache-full before
;;;  			deallocating the resizing screen's existing bit array, I changed
;;; 			the call to retrieve-all-bit-arrays to request a full cache flush.

(DEFUN send-adjust-screen-size (Explorer-screen-ID Explorer-window-ID width height
				   &optional (visibility t))
  (UNLESS *ignore-commands-for-the-Mac*
    (remember-call :selection self)
    (LET* ((inhibit-scheduling-flag t)
	   (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (LOOP with window = (AREF *all-windows-and-screens* explorer-window-id)
	    with size-of-new-bit-array = (* (CEILING (tv:sheet-width window) 8) (tv:sheet-height window))
	    FOR i FROM 0
	    UNTIL (SEND *mac* :AdjustScreenSize Explorer-screen-ID
			    Explorer-window-ID width height visibility)
	    DOING
	    (CASE i
	      (0 (reorganize-bit-array-cache size-of-new-bit-array))
	      (:otherwise (CERROR "onward" "AdjScr tried to flush cache twice")))))))
  

(DEFUN send-select-window (window &optional hide-instead-of-select-p)
  "Tell the Mac to make the Macintosh window containing the specified Explorer WINDOW
the top-most Macintosh window."
  (UNLESS *ignore-commands-for-the-Mac*
    (LET* ((inhibit-scheduling-flag t)
	   (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (UNLESS hide-instead-of-select-p
	(SETF *mac-keyboard-p* (mac-window-p window)))
      (WHEN (AND (NULL *mac-keyboard-p*) (si:mx-p))
	(si:%crash *mac-keyboard-p-is-nil* window t))
      (UNLESS *ignore-commands-for-the-Mac*
	(LET ((window-id (tv:Mac-explorer-window-id window)))
	  (WHEN window-id
	       ;;  Fake a MouseUp event if selected by a solitary MouseDown event...
	    (WHEN (EQ :holding (tv:analyze-last-button))
	      (mac-insert-mouse-buttons 32))
	    (make-bit-array-mac-resident window t)
	    (remember-call :selection window-id)
	    (SEND *mac* :SelectWindow window-id hide-instead-of-select-p)))))))


(DEFUN send-hide-window (window)
  (UNLESS *ignore-commands-for-the-Mac*
    (LET* ((inhibit-scheduling-flag t)
	   (add:*no-interrupt* inhibit-scheduling-flag))
      (send-select-window window t))))


;;;##########################################################
;;;
;;;   			 Graphics-drawing messages...
;;;
;;;##########################################################

(DEFUN send-draw-line (x1 y1 x2 y2 alu draw-end-point Mac-window
			 &optional clip-at-the-margins (line-width 1)
			 (color w:black))
  "Mimics sys:%draw-line.  See mac:si-%draw-line for documentation."
  (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
    (LET (window-id (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (SETF window-id (make-bit-array-Mac-resident Mac-window))
      (remember-call :drawing window-id)
      (IF (NOT clip-at-the-margins)
	  (SEND *mac* :DrawLine
		x1 y1 x2 y2 alu draw-end-point window-id line-width color)
	;; else...
	(with-clipping-rectangle
	  ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	   (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	  (SEND *mac* :DrawLine
		x1 y1 x2 y2 alu draw-end-point window-id line-width color)))
      t
      )))


(DEFUN send-draw-rectangle (width height x y color alu Mac-window
			       &optional clip-at-the-margins)
  "Mimics sys:%draw-rectangle.  See mac:si-%draw-rectangle for documentation."
  (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
    (LET (window-id (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (SETF window-id (make-bit-array-Mac-resident Mac-window))
      (remember-call :drawing window-id)
      (IF (NOT clip-at-the-margins)
	  (SEND *mac* :DrawRectangle
		x y width height color alu window-id)
	;; else...
	(with-clipping-rectangle
	  ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	   (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	  (SEND *mac* :DrawRectangle
		x y width height
		color alu window-id))))))


(DEFUN send-draw-shaded-triangle (x1 y1 x2 y2 x3 y3 alu draw-first-edge
				     draw-second-edge draw-third-edge pattern-or-nil
				     Mac-window
				     &optional clip-at-the-margins)
  "Mimics sys:%draw-shaded-triangle.  See mac:si-%draw-shaded-triangle for
documentation."
   (IF (AND (= x2 x3) (= y2 y3))	   ; Catch line drawers...
       (send-draw-line x1 y1 x2 y2 alu draw-second-edge Mac-window clip-at-the-margins)
     ;; else...
     (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
       (LET (window-id (add:*no-interrupt* inhibit-scheduling-flag))
	 (dump-draw-char-cache)
	 (SETF window-id (make-bit-array-Mac-resident Mac-window))
	 (remember-call :drawing window-id)
	 ;;  Compensate for the Mac's PaintPoly which only fills the interior of a polygon,
	 ;;  does not draw the border, by expanding the requested polygon by one pixel
	 ;;  to the right and down...
	 (LET ((minx (MIN x1 x2 x3)) (miny (MIN y1 y2 y3)) (maxy (MAX y1 y2 y3)))
	   (WHEN (> (- maxy miny) 1500)
	     (FERROR nil "cannot draw a triangle more than 1500 pixels high"))
	   (COND-EVERY
	     ((/= x1 minx) (INCF x1))
	     ((/= x2 minx) (INCF x2))
	     ((/= x3 minx) (INCF x3))
	     ((/= y1 miny) (INCF y1))
	     ((/= y2 miny) (INCF y2))
	     ((/= y3 miny) (INCF y3))))
	 (IF (NOT clip-at-the-margins)
	     (SEND *mac* :DrawTriangle
		   x1 y1 x2 y2 x3 y3
		   alu draw-first-edge draw-second-edge draw-third-edge
		   pattern-or-nil window-id)
	   ;; else...
	   (with-clipping-rectangle
	     ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	      (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	     (SEND *mac* :DrawTriangle
		   x1 y1 x2 y2 x3 y3
		   alu draw-first-edge draw-second-edge draw-third-edge
		   pattern-or-nil window-id))))))
   t    ; w:draw-clipped-solid-triangle expects T returned if triangle drawn.
   )

(DEFUN send-draw-filled-polygon (x-coords y-coords alu fill-pattern-or-nil
				     Mac-window
				     &optional clip-at-the-margins)
  "Mimics sys:%draw-shaded-triangle.  See mac:si-%draw-shaded-triangle for
documentation."
  (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
    (LET (window-id vertices (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (SETF window-id (make-bit-array-Mac-resident Mac-window))
      (remember-call :drawing window-id)
      (SETF vertices (LOOP for x in x-coords
			   for y in y-coords
			   collect (ROUND x)
			   collect (ROUND y)))
      (remember-call :lars vertices)
      (IF (NOT clip-at-the-margins)
	  (SEND *mac* :DrawFilledPolygon
		vertices alu fill-pattern-or-nil window-id)
              ;; else...
	(with-clipping-rectangle
	  ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	   (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	  (SEND *mac* :DrawFilledPolygon
		vertices alu fill-pattern-or-nil window-id))))))

(DEFUN send-DrawCircle
	    (x-center y-center radius color alu num-points draw-edge Mac-window)
  (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
    (LET (window-id (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (SETF window-id (make-bit-array-Mac-resident Mac-window))
      (remember-call :drawing window-id)
      (IF *dont-clip-at-the-margins*
	  (SEND *mac* :DrawCircle
		x-center y-center radius
		color alu num-points draw-edge Window-id)
	;; else...
	(with-clipping-rectangle
	  ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	   (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	  (SEND *mac* :DrawCircle
		x-center y-center radius
		color alu num-points draw-edge Window-id))))))


(DEFUN send-DrawHollowCircle
	    (x-center y-center radius thickness color alu num-points Mac-window)
  (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
    (LET (window-id (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (SETF window-id (make-bit-array-Mac-resident Mac-window))
      (remember-call :drawing window-id)
      (IF *dont-clip-at-the-margins*
	  (SEND *mac* :DrawHollowCircle
		x-center y-center radius thickness
		color alu num-points window-id)
	;; else...
	(with-clipping-rectangle
	  ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	   (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	  (SEND *mac* :DrawHollowCircle
		x-center y-center radius thickness
		color alu num-points window-id))))))


(DEFUN send-DrawHollowRectangle (x y width height thickness color alu Mac-window)
  "Mimics sys:%draw-rectangle.  See mac:si-%draw-rectangle for documentation."
  (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
    (LET (window-id (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (SETF window-id (make-bit-array-Mac-resident Mac-window))
      (remember-call :drawing window-id)
      (IF *dont-clip-at-the-margins*
	  (SEND *mac* :DrawHollowRectangle
		x y width height thickness
		color alu window-id)
	;; else...
	(with-clipping-rectangle
	  ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	   (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	  (SEND *mac* :DrawHollowRectangle
		x y width height thickness
		color alu window-id))))))



(DEFUN send-Draw-Point (x1 y1 alu color Mac-Window)
  (WHEN (AND (tv:sheet-screen-array Mac-window)
	 (NOT *ignore-commands-for-the-Mac*))
    (LET (window-id (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (SETF window-id (make-bit-array-Mac-resident Mac-window))
      (remember-call :drawing window-id)
      (IF *dont-clip-at-the-margins*
	  (SEND *mac* :DrawLine x1 y1 x1 y1 alu NIL window-id 1 color)
	;; else...
	(with-clipping-rectangle
	  ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	   (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	  (SEND *mac* :DrawLine x1 y1 x1 y1 alu NIL window-id 1 color))))))

(DEFUN send-drawdashedline (mac-window x0 y0 x1 y1 alu dash-spacing space-literally-p offset dash-length color
			       &optional (thickness 1))
  (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
    (LET (window-id (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (SETF window-id (make-bit-array-Mac-resident Mac-window))
      (remember-call :drawingx window-id)
      (IF  *dont-clip-at-the-margins*
	  (SEND *mac* :DrawDashedLine self x0 y0 x1 y1 alu dash-spacing
	    space-literally-p offset
	    dash-length color thickness)
	;; else...
	(with-clipping-rectangle
	  ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	   (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	  (SEND *mac* :DrawDashedLine self x0 y0 x1 y1 alu dash-spacing
	    space-literally-p offset
	    dash-length color thickness))))
      t
      ))


;;;##########################################################
;;;
;;;			   Text drawing messages...
;;;
;;;##########################################################



(DEFUN send-draw-char (font char x y alu Mac-window)
  "Mimics sys:%draw-char.  See mac:si-%draw-char for documentation.
Returns NIL if character is entirely outside the right edge of the clipping
rectangle, else returns T.  Note that, as is, this routine cannot draw
into the margins if the sheet's truncate-line-out flag is set."
  
  (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
    (LET (window-id (add:*no-interrupt* inhibit-scheduling-flag))
      (UNLESS (add-to-draw-char-cache font char x y alu Mac-window)
        (SETF window-id (make-bit-array-Mac-resident Mac-window))
        (remember-call :drawing window-id)
        (SETF font (convert-explorer-font-to-mac-font font))	  ;Make sure font has been translated
        (IF (ZEROP (w:sheet-truncate-line-out-flag Mac-window))
	  (IF (>= (+ x (OR (AREF (tv:font-char-width-table font) char) 0))     
	          sys:clipping-rectangle-right-edge)
	      nil
	    ;;; else...
	    (SEND *mac* :DrawCharacter
	          font char x y alu window-id))
          ;; else...
          (IF *dont-clip-at-the-margins*
	    (UNLESS (> x sys:clipping-rectangle-right-edge)
	      (SEND *mac* :DrawCharacter
		  font char x y alu window-id))
	  ;; else...
	  (mac:with-clipping-rectangle
	    ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	     (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	    (UNLESS (> x sys:clipping-rectangle-right-edge)
	      (SEND *mac* :DrawCharacter
		  font char x y alu window-id)))))))))


(DEFUN determine-length-of-string-that-fits (STRING index end xpos xlim font)
   "Returns two values: the x-position where drawing stopped and the index within the string of
the first character not drawn.
Used only with Mac-xxx fonts that always have a font width table and never have a font
indexing table or font kern table."
  (DECLARE (inline graphic-char-p))
  (LET ((fwt (w:font-char-width-table font))
	ch)
    (DO-FOREVER
      (UNLESS (< index end)
	(RETURN xpos index))		    ; reached the end
      (UNLESS (GRAPHIC-CHAR-P (SETQ ch (CHAR-CODE (AREF string index))))
	(RETURN xpos index))		   ; non printing char
      (IF (< xlim (INCF xpos (AREF fwt ch)))
	  (RETURN (- xpos (AREF fwt ch)) index)	  ; this character goes too far
	;; else
	(INCF index)))))


(DEFUN send-draw-string-internal (mac-window string index end x y font alu)
  (LET (window-id)
    (SETF window-id (make-bit-array-mac-resident mac-window))
    (SETF font (convert-explorer-font-to-mac-font font))   ;Make sure font has been translated 
    (SEND *mac* :drawstring
	    font
	    string
	    index
	    end
	    x
	    y
	    NIL				   ;Unused
	    alu
	    window-id)))


(DEFUN send-draw-string (Mac-window string index end x y font alu) 
  "Mimics sys:%draw-string.  See mac:si-%draw-string for documentation.
XLIM taken from sys:clipping-rectangle-right-edge."
  (DECLARE (self-flavor tv:sheet))
  (WHEN (AND (tv:sheet-screen-array Mac-window) (NOT *ignore-commands-for-the-Mac*))
    (LET ((add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (IF *dont-clip-at-the-margins*
	  (send-draw-string-internal Mac-window string index end x y font alu)
              ;; else...
	(mac:with-clipping-rectangle		    ;Intersect clip rect with margins
	  ((w:sheet-inside-left Mac-window) (w:sheet-inside-top Mac-window)
	   (w:sheet-inside-right Mac-window) (w:sheet-inside-bottom Mac-window))
	  (send-draw-string-internal Mac-window string index end x y font alu))))))

;;;##########################################################
;;;
;;;   			           BITBLT messages...
;;;
;;;##########################################################

(DEFVAR *bitblt-max-size* 10000.)

(DEFUN send-bitblt (alu width height
		      from-window-id-or-array from-x from-y
		      to-window-id-or-array to-x to-y
		      &optional source-array
		      (source-from-x from-x) (source-from-y from-y)
		      restoration-p		       ; T iff a restoration of a bit array to Mac.
		      &aux source-height source-width replication)
  "Called iff the source and/or destination is on the Mac.  Mac sources and destinations
are ALWAYS the bitmaps of the windows associated with the window-ids, NEVER the
windows' graphPorts, i.e., screen-arrays.  All indirection is resolved before calling this
routine."
  (UNLESS (OR *ignore-commands-for-the-mac* (ZEROP width) (ZEROP height))
    (LET* ((inhibit-scheduling-flag t)    ; Protect big multi-copy copybits.
	   (add:*no-interrupt* inhibit-scheduling-flag))
      (dump-draw-char-cache)
      (SETF width (ABS width)
	    height (ABS height)
	    source-width width
	    source-height height)
      
      ;; find size of source to determine actual size for exp/mac transfer
      ;; if we have a source-array then we came from si:bitblt otherwise it
      ;; must be a bitarray cache handling problem
      (WHEN source-array
	(SETF source-height (ARRAY-DIMENSION source-array 0))
	(IF (FIXNUMP from-window-id-or-array)	       ; Mac-resident source
	    (SETF source-width (tv:sheet-width (AREF *all-windows-and-screens*
						     from-window-id-or-array))) 
	  (SETF source-width (ARRAY-DIMENSION source-array 1))))
      
           ;; If bitblt is not mac-to-mac, then we need to check the size of the bitblt to insure
           ;; that it is not larger than our largest acb
      (UNLESS (AND (NUMBERP from-window-id-or-array)
		   (NUMBERP to-window-id-or-array))
	(LET (max-lines lines)
	  
	  (IF (OR (< (- source-height source-from-y) height) (< (- source-width source-from-x) width))
	      (SETF replication t)
	    ;; else
	    (SETF source-height (MIN (- source-height source-from-y) height)
		  source-width (MIN (- source-width source-from-x) width)))
	  
	  (SETF max-lines (TRUNCATE (/ (- *bitblt-max-size* (* copybits-parms 2) 32)
				       (* (CEILING source-width 32.) 4))))
	  
	  
	  (WHEN (> source-height max-lines)
	    (LOOP
	      until (< height max-lines)
	      do
	      (SETF lines (MIN max-lines height))
	      (SEND *mac* :copybits
		    alu width lines
		    from-window-id-or-array from-x from-y
		    to-window-id-or-array to-x to-y
		    width lines nil (> (- height lines) 0)
		    restoration-p)
	      (SETF from-y (+ from-y lines))
	      (SETF to-y (+ to-y lines))
	      (SETF height (- height lines)))
	    ;; now set source width, height so we don't scale the damn thing
	    (SETF source-width width)
	    (SETF source-height height))))
           
      (IF (> height 0)
	  (SEND *mac* :copybits
		alu width height
		from-window-id-or-array from-x from-y
		to-window-id-or-array to-x to-y
		source-width source-height replication nil
		restoration-p)))))



