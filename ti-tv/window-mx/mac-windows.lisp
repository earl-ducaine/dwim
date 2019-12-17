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

;;;--------------------------------------------------------------------
;;;                 Patch
;;;   Date  Author  Number   Description
;;;--------------------------------------------------------------------
;;;  02-14-89   LG    5-36   Enhanced print-debug-dump now that we're getting them again.
;;;  02-13-89   LG    5-35   Support TERM ctrl-S on an mX.
;;;  02-07-89   LG    5-32   Clean-up-deactivated-windows now returns # of bytes freed in bit-array cache.
;;;  11-17-88   LG    5-25   o Do nothing in deallocate-mac-window if window has no window-id.
;;;			o Add capability to flush deactivated windows from the Mac's and our data structures.
;;;			o In allocate-Mac-explorer-window-id, If out of window ids, try to reclaim some
;;;			   by calling  clean-up-deactivated-windows before deciding to crash.  Notify
;;;			   the user if we're getting close to running out.
;;;			o In define-mac-resident-exp-screen, if screen has been left with a
;;;			   window-id of T by (sheet :init) give it a good one here. 
;;;  10-19-88   LG    5-23   Protected get-Mac-mask-image-for-a-char-glyph from the possibility that the
;;;  			specified character in the specified font might not exist in the Explorer's font.
;;;  10-16-88   LG    5-17   Added print-debug-dump and friends that understand how to print a dump
;;;			of the Mac's circular debug buffer.
;;;  09-26-88   LG     5-12  Let redirect-drawing-of-window-and-inferiors handle windows
;;;  			with null screen-arrays.
;;;  09-12-88   LG   5-9	Reset data structures at boot to show NO windows Mac-resident.
;;;  09-01-88   LG     5-8  Fixed before-disk-save to remap-all-fonts on first disk save only.
;;;  09-01-88   LG     5-7	Added mac:set-direct-drawing-mode command.
;;;  08-28-88   LG	      5-6     Removed :are-you-there? message in give-mouse-ownership-to-the-Mac
;;;			  to allow screen creation to work.
;;; 08-17-88  LG     5-5	Clear tv:mouse-buttons-buffer at boot.
;;; 07-21-88  LG     4-62    Gave mX a null definition for (:method tv:sheet :after
;;; 		   5-3	:change-of-size-or-margins) to satisfy applications (like Profile)
;;; 			compiled with Suggestions loaded.
;;; 07-17-88  LG     4-59   	Added an :after :set-array method for tv:bitblt-blinker to update
;;;			Mac's mouse cursor image.  No longer try to update by
;;;			detecting the first draw of the blinker in the (tv:bitblt-blinker
;;;			:around :blink)  method (it couldn't work in general anyway).
;;; 06-16-88  ab   4-58    	o Fix CONVERT-EXPLORER-FONT-TO-MAC-FONT not
;;;                             	   to reference unbound MAC- font symbol.
;;; 06-08-88  LG     4-57    Don't create a new font descriptor in :parse-font-descriptor
;;; 			unless the Mac's current info for that font differs from the
;;; 			Explorer's.
;;; 06-03-88  LG     4-55    Optimized handling of small (16x16) bitblt-blinkers.
;;; 05-25-88  ab     4-52	o For LG: fix CONVERT-EXPLORER-FONT-TO-MAC-FONT.
;;;                          	   Also changes to FIXUP-FONTS.
;;;                          	o Changes to PARSE-FONT-DESCRIPTOR and GET-MAC-
;;;			   GLYPHS to support mouse cursor glyphs fully.  Also new
;;;			   predicates MAC-FONT-P and EXPLORER-FONT-P.
;;; 05/10/88  LG     4-51    In transfer-mouse, move the newly selected screen's entry to
;;;                 GG             	the head of the *undisplaced-Mac-window-arrays* list.
;;;		 	Implemented automatic screen resizing at boot to fit the
;;;		 	Mac's monitor.

(DEFVAR look-for-hung-mouse nil)

;;(DEFUN look-for-hung-mouse ()
;;  (LOOP FOR saw-a-hang = (PROCESS-WAIT-WITH-TIMEOUT
;;			   "Mouse Hung?"
;;			   nil ; (* 60 20)
;;			   #'(lambda () (AND (EQ tv:window-owning-mouse 'tv:stop)
;;					     (EQ tv:mouse-reconsider t))))
;;	DO
;;    (WHEN (AND *mac-keyboard-p*
;;	       (LOOP REPEAT 60
;;		WHILE (OR (NOT saw-a-hang)
;;			  (AND (EQ tv:window-owning-mouse 'tv:stop)
;;			       (EQ tv:mouse-reconsider t)))
;;		DO (PROCESS-SLEEP 5)
;;		FINALLY (RETURN (OR (NOT saw-a-hang)
;;				    (AND (EQ tv:window-owning-mouse 'tv:stop)
;;					 (EQ tv:mouse-reconsider t))))))
;;      (WHEN *tracing-off* 
;;	(dr))		   ; Remember we came here...
;;      (remember-call :fatal)
;;      (reset-Mac-explorer-connection
;;	'(tv:kbd-terminal-arrest-all nil)))))



(DEFVAR *old-Mac-resident-Explorer-screens* nil) 
(DEFVAR *old-previously-selected-windows* nil)

(DEFUN clean-out-mx-window-data-structures ()
  (SETQ *old-Mac-resident-Explorer-screens* nil
	*old-previously-selected-windows* nil))

(ADD-INITIALIZATION "Clean out MX window data structures"
		    '(clean-out-mx-window-data-structures) :full-gc)

(DEFUN si:during-cold-boot ()
  "This should be on the cold-initialization-list of bands saved with the
Mac window system loaded."
  (SETF tv:*set-mouse-to-selected-screen* nil)
  (SETF *ignore-commands-for-the-mac* nil)
  (SETF tv:initial-lisp-listener nil)
  (SETQ *mac-keyboard-p* t)
  (FILL (THE array tv:mouse-buttons-buffer) 0)
  (setup-basic-comm)
  (SETF *total-mac-bit-array-cache-size*
        (get-total-cache-size))
  
  ;;  Reset all the resource allocation controls for Mac-Explorer windows and screens.
;;;  (SETF *list-of-free-window-ids* nil
;;;	*largest-window-id-ever-assigned* -1)
;;;  (SETF *list-of-free-screen-ids* nil
;;;	*largest-screen-id-ever-assigned* 0)

;;;  (SETF *old-previously-selected-windows* 
;;;            (COPY tv:previously-selected-windows)
;;;	*old-mac-resident-explorer-screens*
;;;	    (COPY *mac-resident-explorer-screens*))

;;;  (SETF *mac-resident-explorer-screens* 
;;;	    (MAKE-ARRAY (1+ ages-array-size)))
;;;  (SETF *undisplaced-mac-window-arrays* nil) 

  ;;  Reset our data structures to say there are NO windows on the Mac...
  (FILL (THE array *is-the-bit-array-Mac-resident?*) 0)
  (LOOP for window-id from 0 to *largest-window-id-ever-assigned*
	when (AND (AREF *all-windows-and-screens* window-id)
		  (NULL (AREF *has-the-Mac-forgotten-my-redirection?* window-id)))
	do (SETF (AREF *has-the-Mac-forgotten-my-redirection?* window-id)
		 (AREF *all-windows-and-screens* window-id)))
  ;;  Create a descriptor for the Explorer's monitor...
  (COND ((si:mx-p)
	 (SETF (AREF *mac-resident-explorer-screens* 0)
	       (make-explorer-screen
		 :the-screen nil
		 :the-last-selected-window nil)))
	(t				   ;lashup
	 (SETF (AREF *mac-resident-explorer-screens* 0)
	       (make-explorer-screen
		 :the-screen w:main-screen
		 :the-last-selected-window
		 (LOOP for w in (SEND tv:main-screen :inferiors)
		       do (WHEN (TYPEP w 'w:lisp-listener)
			    (RETURN w)))))))
  ;; (delete :mouse mac:*all-debugging-classes*)
  ;; (mac:dr)
;;  (mac:remember-call :fatal
;;		 (listarray mac:*all-windows-and-screens*)
;;		 (listarray mac:*has-the-Mac-forgotten-my-redirection?*)
;;		 (listarray mac:*is-the-bit-array-Mac-resident?*))
  )



(DEFVAR *exp-main-screen* nil)
(DEFVAR *exp-who-screen* nil)

(DEFUN fixup-a-font-ref (symbol)
  (WHEN (AND (BOUNDP symbol) (TYPEP (SYMBOL-VALUE symbol) 'tv:font))
    (SET symbol (SYMBOL-VALUE (tv:font-name (SYMBOL-VALUE symbol))))))

;;ab for LG 5-25-88.  Win-MX 53.
(DEFUN fixup-fonts (&optional do-print-p)
  "Replace the value of each symbol in the TV package whose value is an Explorer font
descriptor with the corresponding Mac font descriptor."
  (MAPATOMS 'fixup-a-font-ref 'tv)
  ;;  For every symbol FONTS:MAC-? force the value of the corresponding FONTS:?
    ;;  symbol to be that of the FONTS:MAC-? symbol.
  (LOOP with sym2
	for sym being the local-interned-symbols in 'fonts
	when (AND (BOUNDP sym)
		  (TYPEP (SYMBOL-VALUE sym) 'font)
		  (STRING= (SUBSEQ (SYMBOL-NAME sym) 0 4)
			   "MAC-")
		  (SETF sym2 (FIND-SYMBOL (SUBSEQ (SYMBOL-NAME sym) 4) 'fonts))
		  (BOUNDP sym2)
		  (TYPEP (SYMBOL-VALUE sym2) 'font)
		  ;;(NEQ sym2 'fonts:mouse) ;ab 5-23-88
		  )
	do
	(WHEN (NEQ (SYMBOL-VALUE sym) (SYMBOL-VALUE sym2))
	  (WHEN do-print-p
	    (FORMAT t "~%~s changed from ~s to ~s."
		    sym2 (SYMBOL-VALUE sym2) (SYMBOL-VALUE sym)))
	  (SET sym2 (SYMBOL-VALUE sym))))
  )

(DEFUN clean-up-explorer-windows ()
  ;; Fix up w:MAIN-SCREEN
  (WHEN (NOT (mac-screen-p w:main-screen))
    (SETF *exp-main-screen* w:main-screen
	  w:main-screen w:default-screen)
    (SETF w:all-the-screens
	  (DELETE *exp-main-screen* w:all-the-screens)))
  
  ;; Delete all explorer screens from w:ALL-THE-SCREENS (but save who-screen)
  (DOLIST (s w:all-the-screens)
    (WHEN (NOT (mac-screen-p s))
      (WHEN (STRING-EQUAL (SEND s :name) "Who Line screen")
	(SETQ *exp-who-screen* s))
      (SETQ w:all-the-screens (DELETE s w:all-the-screens))))
  
  ;; General cleanup of anyone pointing to explorer windows:
  (LOOP for w being the array-elements of w:previously-selected-windows
        doing (WHEN (AND w (NOT (mac-window-p w)))
		(tv:remove-from-previously-selected-windows w)))
  ;; General cleanup of anyone pointing to explorer windows:
  (SETQ TV:*FULL-SCREEN-HACKING-WINDOWS*
        (LOOP for w in TV:*FULL-SCREEN-HACKING-WINDOWS*
	      with lst do
	      (WHEN (mac-window-p w)
		(PUSH w lst))
	      finally (RETURN (COPY-LIST (NREVERSE lst)))))
  (SETQ TV:BACKGROUND-INTERESTING-WINDOWS
        (LOOP for (w) in  TV:BACKGROUND-INTERESTING-WINDOWS
	      with lst do
	      (WHEN (mac-window-p w)
		(PUSH (LIST w) lst))
	      finally (RETURN (COPY-LIST (NREVERSE lst)))))
  (UNLESS (mac-window-p w:sprite-window)
    (SETQ W:SPRITE-WINDOW nil))
  (UNLESS (mac-window-p w:cache-window)
    (SETQ W:CACHE-WINDOW nil))
  )

(DEFUN after-tv-initialized (&optional (cold-booting t))
  (LET (new-screens-descriptor new-ll width height needs-resizing
	 resize-screens-at-boot-p)
    (COND
      (*last-selected-exp-screens-id*		    ; Screens saved in band.
       ;;  Move all the deexposed screens' buffer arrays to the Mac, rebuilding
       ;;  the Mac's data structures on the way...
       (MULTIPLE-VALUE-SETQ (width height resize-screens-at-boot-p)
	 (SEND *mac* :GetScreenSize))
       (SETF needs-resizing nil)
       (SETF *last-selected-Exp-screens-ID* nil)
       (LOOP FOR i FROM 1 TO *largest-screen-id-ever-assigned*
	     FOR screen = (the-screen (AREF *Mac-resident-Explorer-screens* i))
	     WHEN screen DO
	     (restore-a-screens-bit-array-to-the-Mac screen i t)
	     (WHEN (OR (/= width (w:sheet-width screen))
		       (/= height (w:sheet-height screen)))
	       (SETF needs-resizing t))
	     (UNLESS *last-selected-Exp-screens-ID*
	       (SETF *last-selected-Exp-screens-ID* i)))
       ;;  Resize the existing screens to fit the Mac's preferred size, if necessary...
       (WHEN (AND needs-resizing resize-screens-at-boot-p)
	 (LOOP FOR i FROM *largest-screen-id-ever-assigned* DOWNTO 1
	       FOR screen = (the-screen (AREF *mac-resident-explorer-screens* i))
	       WHEN screen DO
	       (SETF w:mouse-sheet screen
		     w:default-screen screen)
	       (resize-screen i height width)))
       
       ;;  Make the lowest-numbered screen the default-screen...
       (SETF w:default-screen
	     (the-screen (AREF *mac-resident-explorer-screens*
			       *last-selected-Exp-screens-ID*)))

       ;;  Decide who should be the initial lisp listener...
       (SETF new-ll
	   (OR (LOOP FOR inferior IN (tv:sheet-inferiors w:default-screen)
		   WHEN (TYPEP inferior 'w:lisp-listener)
		   RETURN inferior)
	       (w:make-window 'w:lisp-listener
			  :superior w:default-screen
			  :process si:initial-process)))

       ;;  Expose the screens in reverse order so the default-screen stays on the mac's
             ;;  monitor...
       (LOOP for i from *largest-screen-id-ever-assigned* DOWNTO 1
	     FOR screen = (the-screen (AREF *mac-resident-explorer-screens* i))
	     WHEN screen
	     DO 
	     (SETF w:mouse-sheet screen)
	     (SEND screen :expose)))
      
      
      (t					    ; Clean restart.
       (IF (NULL (make-a-Mac-resident-Explorer-screen))
	   (si:%crash 25 #'after-tv-initialized nil))

       (SETF new-screens-descriptor
	   (AREF *mac-resident-explorer-screens* *last-selected-exp-screens-id*))
       (SETF w:default-screen (the-screen new-screens-descriptor))
       (COND (cold-booting
	    ;; Force new listener to be Listener #1
	    (SETF (GET 'w:lisp-listener 'tv:unnamed-window-instance-count) 0)
	    (SETF new-ll (w:make-window 'w:lisp-listener
			     :superior w:default-screen
			     :process si:initial-process)))
	   (t
	    (SETF new-ll (w:make-window 'w:lisp-listener
				  :superior w:default-screen))))))

    (WHEN (AND si:cold-booting (si:mx-p))
      (clean-up-explorer-windows))

    (FILL (THE array tv:*previously-selected-screens*) nil)
    (tv:setup-previously-selected-screens-array)

    ;;  Get this Mac's descriptions for all the standard fonts...
    (map-Lispm-fonts)
    (fixup-fonts)
    (SETF tv:initial-lisp-listener new-ll)
    (UNLESS cold-booting
      (SEND new-ll :select))))

      
(DEFUN delete-item-from-array (item array &optional fill-value)
  "Returns ARRAY with the first instance of ITEM deleted.  The deletion is effected
by moving the elements of ARRAY beyond deleted element up by one, filling in the
last element of ARRAY with FILL-VALUE.  This differs from (DELETE item array) in
that the length of ARRAY is not changed."
  (LET ((position-of-item (POSITION item (THE array array))))
    (WHEN position-of-item
      (LOOP WITH last-index = (1- (ARRAY-TOTAL-SIZE array))
	    FOR i FROM position-of-item BELOW last-index
	    DO (SETF (AREF array i) (AREF array (1+ i)))
	    FINALLY (SETF (AREF array last-index) fill-value))))
  array)

(DEFUN find-highest-superior-not-a-screen (window)
  "Returns the first superior of WINDOW that either has no superior
itself or whose superior is of type w:screen."
  (LOOP FOR candidate-window FIRST window THEN candidate-windows-superior
	FOR candidate-windows-superior = (w:sheet-superior candidate-window)
	WHEN (OR (NULL candidate-windows-superior)
		 (TYPEP candidate-windows-superior 'w:screen))
	RETURN candidate-window))

(DEFUN si:before-disk-save (&optional clean-out-the-mac-too kamakazi-p)
  (DECLARE (IGNORE kamakazi-p clean-out-the-mac-too ))
  (remember-call :fatal)
        
    ;;  Make all windows' bit-arrays and all screens' buffer arrays Explorer resident...
  (retrieve-all-bit-arrays-from-the-Mac t)
  
  ;;  Force the next boot to acquire all new Mac font descriptions...
  (UNLESS (BOUNDP 'fonts:mac-cptfont)	   ; Special to make the build's first disk save go smoothly.
    (remap-all-fonts))
  (demap-mac-fonts))


;;ab 2/9/88. Do BEFORE-DISK-SAVE directly from DISK-SAVE process.
;;(si:add-initialization "Kill Mac windows/screens"
;;		       '(si:before-disk-save t)
;;		       :before-cold)


;;;
;;;   Window id allocation/deallocation...
;;;

(DEFUN allocate-Mac-Explorer-window-id ()
  "Returns a positive integer to be used as a Mac-Explorer window id."
  (DECLARE (VALUES Mac-Explorer-window-id))
  (WITH-LOCK (*window-id-lock*)
    (LET (id n-wids-left)
      ;;  Notify user if we're about to run out of window-ids...
      (SETF n-wids-left (- *maximum-legal-mac-explorer-window-id* *largest-window-id-ever-assigned*))
      (WHEN (< n-wids-left 10)
	(INCF n-wids-left (LENGTH *list-of-free-window-ids*))
	(WHEN (MEMBER n-wids-left '(10 6 3  1))
	  (let ((si:cold-load-stream-owns-keyboard t)
		(*terminal-io* si:cold-load-stream))
	    (eh:save-screen-for-cold-load-stream t)
	    (CERROR "Proceed" "Only ~d free window-id~:P left." n-wids-left)
	    (eh:restore-screen-for-cold-load-stream))))
      (SETF id
	    ;;  First try to allocate a previously used window id...
	    (IF *list-of-free-window-ids*
		(POP *list-of-free-window-ids*)
	      ;; else try to allocate an unused window id...
	      (IF (< *largest-window-id-ever-assigned*
		     *maximum-legal-mac-explorer-window-id*)
		    (INCF *largest-window-id-ever-assigned*)
		;; else flush all deactivated windows, try for one of their window ids...
		(clean-up-deactivated-windows)
		(IF *list-of-free-window-ids*
		    (POP *list-of-free-window-ids*)
		   ;; else we're out of window ids (and luck)...
		  (IF (si:mx-p)
		      (PROG1 nil
			     (let ((si:cold-load-stream-owns-keyboard t)
				   (*terminal-io* si:cold-load-stream))
			       (eh:save-screen-for-cold-load-stream t)
			       (ferror nil "You are trying to create 257 active windows, 256 is the maximum.")
			       (eh:restore-screen-for-cold-load-stream)))
		    ;; else...
		    (reset-Mac-explorer-connection
		      '(tv:kbd-terminal-arrest-all nil)))))))
      (remember-call :id-assignment id self)
      id)))
	
(DEFUN deallocate-Mac-Explorer-window-id (id)
  "Returns the positive integer ID to the pool of available Mac-Explorer window ids."
  (WITH-LOCK (*window-id-lock*)
    (remember-call :id-assignment id self)
    (PUSH id *list-of-free-window-ids*)))

(DEFUN deactivate-Mac-screen (screen)
  (LET ((exp-screen-id (POSITION screen *mac-resident-explorer-screens*
				 :key #'mac:the-screen)))
    (send-adjust-screen-size
      Exp-screen-id (tv:Mac-explorer-window-id screen)
      0 0)			   ; Height = width = 0 ==> destroy!
    (SETF (AREF *Mac-resident-Explorer-screens* Exp-screen-id) nil)
    (deallocate-Mac-Explorer-screen-id Exp-screen-id)
    (tv:remove-from-previously-selected-screens screen)
    ;;  Completely forget about a "deactivated" screen's buffer array...
    (SETF *undisplaced-Mac-window-arrays*
          (DELETE screen *undisplaced-Mac-window-arrays*
	        :key #'REST :test 'EQ))))


(DEFUN deactivate-Mac-window (window &optional (window-id (tv:Mac-explorer-window-id window)))
  ;; Note that deactivating window W1 only removes W1 from our data structures.
  ;; It leaves all W1's inferiors as perfectly good Mac-windows.  
  (LET* ((inhibit-scheduling-flag t) (add:*no-interrupt* inhibit-scheduling-flag))
    (WHEN (INTEGERP window-id)
      (send-deactivate-window window)
      ;;  Since this window's id is being deallocated, we can't remember this window's
      ;;  redirection in *has-the-Mac-forgotten-my-redirection?*.  When a window is
      ;;  assigned a window id we must initialize *has-the-Mac-forgotten-my-redirection?*
      ;;  to the window itself.  Send-redirect must understand that a window may claim
      ;;  that the Mac has forgotten its redirection when the window in 
      ;;  *has-the-Mac-forgotten-my-redirection?* has no bit array.  In this case there's
      ;;  nothing to do...
      (SETF (AREF *has-the-Mac-forgotten-my-redirection?* window-id) nil)
      (SETF (tv:Mac-explorer-window-id window) t)
      (SETF (AREF *all-windows-and-screens* window-id) nil)
      (UNLESS (MEMBER window-id *list-of-free-window-ids*)
	(deallocate-Mac-explorer-window-id window-id))
      ;;  Move a deactivated window's entry in the *undisplaced-Mac-window-arrays* list
      ;;  to the end of the list to speed searching for those windows still active.  Remove the
      ;;  window's entry entirely if it now has no bit-array...
      (LET ((bit-array (tv:sheet-bit-array window))
	    (temp (DELETE window *undisplaced-Mac-window-arrays* :key 'REST :test 'EQ)))
	(SETF *undisplaced-Mac-window-arrays*
	      (IF bit-array
		  (NCONC temp (LIST (CONS (tv:sheet-bit-array window) window)))
		temp))))
    window-id))


(DEFUN activate-Mac-window (window)
  (LET ((window-id (tv:Mac-explorer-window-id window)))
    (WHEN (AND (NULL window-id)
	       (TYPEP (tv:sheet-get-screen window) 'Mac:Mac-screen))
      (SETF window-id (give-a-window-an-id window)))
    (TYPECASE window-id
      (symbol
       (IF (NULL window-id)
	   (WHEN (TYPEP (tv:sheet-get-screen window) 'Mac-screen)
	     (give-a-window-an-id window)
	     (send-activate-window window))
	 ;; else...
	 (give-a-window-an-id window)
	 (send-activate-window window)))
      (number
       (send-activate-window window))
      (t
       (FERROR nil "Bad window id = ~a." window-id)))
    window-id))

;;;
;;;   Screen id allocation/deallocation...
;;;

(DEFUN allocate-Mac-Explorer-screen-id ()
  "Returns a positive integer to be used as a Mac-Explorer screen id."
  (DECLARE (VALUES Mac-Explorer-screen-id))
  (WITH-LOCK (*screen-id-lock*)
    (LET (id)
      (SETF id
	    (IF *list-of-free-screen-ids*
		(POP *list-of-free-screen-ids*)
	      ;; else...
	      (IF (< *largest-screen-id-ever-assigned*
		     *maximum-legal-mac-explorer-screen-id*)
		  (INCF *largest-screen-id-ever-assigned*)
		;; else...
		  (IF (si:mx-p)
		      (PROG1 nil
			     (si:%crash *mac-screen-allocation-failure*
					*largest-screen-id-ever-assigned* t))
		      (reset-Mac-explorer-connection
			'(tv:kbd-terminal-arrest-all nil))))))
      (remember-call :id-assignment  id self)
      id)))
	

;;;  05/11/88  LG		Keep the list of free screen ids sorted so that the lowest
;;;  			possible screen number is always allocated to the next screen
;;;  			created.

(DEFUN deallocate-Mac-Explorer-screen-id (id)
  "Returns the positive integer ID to the pool of available Mac-Explorer screen ids."
  (WITH-LOCK (*screen-id-lock*)
    (remember-call :id-assignment id self)
    (SETF *list-of-free-screen-ids*
	  (SORT (CONS id *list-of-free-screen-ids*) #'<))))


;;;  05/11/88  LG		Protect against getting called when there's no
;;;  				selected window. 

(DEFUN select-a-screen (screen-id-or-screen-or-window)
  (LET (new-screen screen-id selected-window)
    ;;  Figure out the screen/id which we are selecting...
    (IF (NUMBERP screen-id-or-screen-or-window)
	(SETF screen-id-or-screen-or-window (ABS screen-id-or-screen-or-window)
	      new-screen (the-screen (AREF Mac::*Mac-resident-Explorer-screens*
				      screen-id-or-screen-or-window))
	      screen-id screen-id-or-screen-or-window)
      ;; else...
      (IF (TYPEP screen-id-or-screen-or-window 'mac-screen)
	  (LOOP FOR i FROM 0 TO *largest-screen-id-ever-assigned*
		WHEN (EQ screen-id-or-screen-or-window
			 (the-screen (AREF Mac::*Mac-resident-Explorer-screens* i)))
		DO
		(SETF new-screen screen-id-or-screen-or-window
		      screen-id i
		      selected-window (the-last-selected-window (AREF Mac::*Mac-resident-Explorer-screens* i))))
	;; else...
	(SETF new-screen (w:sheet-get-screen screen-id-or-screen-or-window)
	      screen-id (LOOP FOR i FROM 0 TO *largest-screen-id-ever-assigned*
			      WHEN (EQ new-screen
				       (the-screen (AREF Mac::*Mac-resident-Explorer-screens* i)))
			      RETURN i))))
    ;;  Now select it, if necessary...
    (UNLESS (AND w:selected-window
		 (EQ new-screen (w:sheet-get-screen w:selected-window)))
      (IF (mac-window-p new-screen)
	  (SETF *last-selected-Exp-screens-ID* screen-id))
      (WHEN w:mouse-sheet
	(toggle-mouse new-screen)
	(PROCESS-SLEEP 1))		   ; Let the mouse sheet change.
      (WHEN selected-window
	(SEND selected-window :select))
      (remember-call :selection w:mouse-sheet w:default-screen
		     *last-selected-Exp-screens-ID*)) t))

(DEFUN can-this-screen-be-resized (screen-id new-screen-width new-screen-height)
  "Returns T iff space exists in the Mac's bit-array cache to change the
size of screen SCREEN-ID from its current size (which is zero if it
isn't allocated yet) to NEW-SCREEN-WIDTH by NEW-SCREEN-HEIGHT and
accomodate one window of the same size."
  (LET ((bytes-allocated-to-all-screens 0) this-screen this-screens-size
	(bytes-allocated-to-this-screen 0) size-of-biggest-screen 
	bytes-needed-for-this-screen space-remaining
	(inhibit-scheduling-flag t))
    (SETF this-screen (the-screen (AREF *mac-resident-explorer-screens* screen-id)))
    (WHEN this-screen
      (SETF bytes-allocated-to-this-screen
	    (CEILING (* (SEND this-screen :width) (SEND this-screen :height)) 8)))
    (SETF bytes-needed-for-this-screen
	  (CEILING (* new-screen-width new-screen-height) 8))
    ;;  Scan all existing screens to get the total # of bytes of cache they occupy now and
       ;;  the size of the biggest screen after resizing this one.
    (SETF size-of-biggest-screen bytes-needed-for-this-screen)
    (LOOP FOR sid from 1 to *largest-screen-id-ever-assigned*
	  FOR screen = (the-screen (AREF *mac-resident-explorer-screens* sid))
	  WHEN screen
	  DO
	  (SETF this-screens-size
		(CEILING (* (SEND screen :width) (SEND screen :height)) 8))
	  (INCF bytes-allocated-to-all-screens this-screens-size)
	  (WHEN (AND (> this-screens-size size-of-biggest-screen)
		     (/= screen-id sid))
	    (SETF size-of-biggest-screen this-screens-size)))
    ;;  
    (SETF space-remaining
	  (+ *total-Mac-bit-array-cache-size*	       ; Total space
	     (- bytes-allocated-to-all-screens)	       ; - space now allocated to screens
	     (- (- bytes-needed-for-this-screen	       ; - increase in space allocated to
		   bytes-allocated-to-this-screen))    ;       screens due to this resizing
	     (- (* 3 size-of-biggest-screen))))	       ; - minimum tolerable free space.
    (remember-call :bit-arrays  bytes-allocated-to-this-screen size-of-biggest-screen
		    bytes-needed-for-this-screen space-remaining)
    (IF (> space-remaining 0) space-remaining)))


(DEFUN make-a-Mac-resident-Explorer-screen
       (&optional (height nil)
        (width nil) not-visible-p)
  "Create an Explorer screen/Macintosh window pair."
  (DECLARE (SPECIAL not-visible-p)) 
  (IF (OR (NULL height) (NULL width))
      (MULTIPLE-VALUE-SETQ (width height)
        (SEND *mac* :GetScreenSize)))
  (LET* ((rounded-width (* 32 (CEILING width 32))) new-Mac-screen
         (Mac-Explorer-screen-ID-for-screen-being-created 
	 (Mac::allocate-Mac-Explorer-screen-id)))
    (DECLARE (SPECIAL Mac-Explorer-screen-id-for-screen-being-created))
    (IF (NOT (can-this-screen-be-resized
	     Mac-Explorer-screen-id-for-screen-being-created
	     rounded-width height))
        (PROGN
          (deallocate-Mac-Explorer-screen-id
	    Mac-Explorer-screen-id-for-screen-being-created)
          (pop-up-format-at-origin "~
Insufficient space in Mac's screen/window image
cache to accomodate another screen.")
          nil)
        ;; else...
        ;;  Create the Explorer-resident surrogate for the Mac's screen...
        (SETF new-Mac-screen
	    (tv:define-screen
	      'Mac-screen
	      (INTERN (FORMAT nil "MAC-SCREEN-~3,'0D"
			  Mac-Explorer-screen-ID-for-screen-being-created)
		    'Mac)
	      :buffer (MAKE-ARRAY `(,height ,rounded-width)
			      :element-type '(MOD 2))
	      :bits-per-pixel 1.
	      :height height
	      :width rounded-width))
        ;;  Make the newly created screen the default screen and the mouse sheet...
        (UNLESS not-visible-p
          (SETF *last-selected-Exp-screens-ID*
	      Mac-Explorer-screen-ID-for-screen-being-created)
          (select-a-screen new-Mac-screen))
        Mac-Explorer-screen-ID-for-screen-being-created)))

(DEFUN define-mac-resident-exp-screen (screen)
  "Called by tv:make-screen when it detects that a Mac-resident
Explorer screen is being defined."
  (DECLARE (SPECIAL Mac::Mac-Explorer-screen-ID-for-screen-being-created not-visible-p))
  (LET ((window-id (tv:Mac-Explorer-window-id screen))
	(inhibit-scheduling-flag t))
    (WHEN (EQ window-id t)
      (SETF window-id (give-a-window-an-id screen)))
    (PUSH (CONS (SEND screen :buffer) screen)
	  *undisplaced-Mac-window-arrays*)
    (SETF (AREF *Mac-resident-Explorer-screens*
		Mac-Explorer-screen-ID-for-screen-being-created)
	  (make-explorer-screen :the-screen screen))
    (send-adjust-screen-size
      Mac-Explorer-screen-ID-for-screen-being-created
      window-id
      (tv:sheet-width screen) (tv:sheet-height screen) (NOT not-visible-p))
    (SETF (AREF *is-the-bit-array-Mac-resident?* window-id) 1)
    (tv:add-to-previously-selected-screens screen) 
    (activate-Mac-window screen)))


(DEFUN remember-this-screens-last-selected-window (window)
  (IF (mac-window-p window)
      (PROGN
	(SETF (the-last-selected-window (AREF Mac::*Mac-resident-Explorer-screens*
					      (ABS Mac::*last-selected-Exp-screens-ID*)))
	      window))
       ;; else remember the Explorer screen's last-selected window...
       (SETF (the-last-selected-window (AREF Mac::*Mac-resident-Explorer-screens* 0))
	       window))
  )


(DEFUN chase-displaced-pointers (array)
  "Given an ARRAY, chases all displaced pointers.  Stops either because an undisplaced
array is found or a displaced-to-physical array is found.  Returns the array ultimately
displaced to and the cumulative offset into that array"

  (DECLARE (VALUES array-displaced-to total-offset))
  (SETF array (FOLLOW-STRUCTURE-FORWARDING array))
  (LET ((possible-next-array array) (total-offset 0) p)
    (LOOP UNTIL (OR (INTEGERP possible-next-array)
		    (ZEROP (sys:%P-LDB-OFFSET sys:%%array-displaced-bit
					  (SETF array possible-next-array)
					  0)))
	  DO
	  (SETQ p (sys:%make-pointer-offset
		    sys:dtp-locative array
		    (+ (sys:%P-LDB-OFFSET sys:%%array-number-dimensions array 0)
		       (sys:%P-LDB-OFFSET sys:%%array-long-length-flag array 0))))
	  (IF (ARRAY-INDEXED-P array)
	      (INCF total-offset (sys:%p-contents-offset p 2)))
	  (SETF possible-next-array (sys:%p-contents-offset p 0)))
    (VALUES array total-offset)))

(DEFUN redirect-drawing-of-window-and-inferiors (window)
  (WHEN (mac-window-p window)
    (send-redirect-drawing window)
    (LOOP FOR inferior IN (tv:sheet-exposed-inferiors window) DO
	  (redirect-drawing-of-window-and-inferiors inferior))))


;;;
;;;  The MacScreen's font parsing methods...
;;;   They do everything the Explorer screen's methods do, then they obtain the
;;;     description from the Mac of the corresponding Mac font that the Mac will use,
;;;     create an Explorer font descriptor "describing" that font's characters,
;;;     and return that...
;;;

(DEFVAR fonts:*dont-load-fonts* t
  "Variable to control whether ot not the system tries to load
fonts from the file system. Only used on Mac screens.")

;;ab new 5.25.88.  Win-MX 53.
(DEFUN explorer-font-p (font)
  (AND (TYPEP font 'font)
       (TYPEP font '(ARRAY bit))))

;;ab new 5.25.88.  Win-MX 53.
(DEFUN mac-font-p (font)
  (AND (TYPEP font 'font)
       (TYPEP font '(ARRAY t))))


(DEFMETHOD (Mac-Screen :parse-font-specifier) (fd)
  (multiple-value-bind (fd ignore)
      (tv:screen-parse-font-descriptor fd 'fonts:cpt-font fonts:*dont-load-fonts*)
    (SETF fd (convert-explorer-font-to-mac-font fd))
    (values fd (tv:font-name fd))))

;;ab 5.25.88.  When parsing Explorer font, save off font object on symbol's plist for
;;             possible later use.  Win-MX 53.
(DEFMETHOD (Mac-Screen :parse-font-descriptor) (fd)
  (multiple-value-bind (fd ignore)
      (tv:screen-parse-font-descriptor fd 'fonts:cpt-font fonts:*dont-load-fonts*)
    ;; If this is an Explorer font, save the font object on the font-name symbol's plist
    ;; for possible later use.
    (WHEN (explorer-font-p fd)
      (SETF (GET (tv:font-name fd) :explorer-font) fd))
    (SETF fd (convert-explorer-font-to-mac-font fd))
    (values fd (tv:font-name fd))))

;;ab for lg 5.25.88.  Win-MX 53.
;;;  06-08-88  LG		Made convert-explorer-font-to-mac-font much smarter in that
;;;  			if it is called to convert Explorer font fonts:X it creates the font
;;;  			descriptor for fonts:mac-X, then compares it against the existing
;;;  			descriptor using compare-two-font-descriptors; if there's no
;;;  			change in the font, the old descriptor is left as the value of
;;;  			fonts:mac-X, otherwise the new descriptor is made its value.

(DEFUN compare-two-font-descriptors (fd1 fd2)
  "Returns  NIL if they do not describe the same sized font."
  (AND (TYPEP fd1 'font)
       (TYPEP fd2 'font)
       (EQ (w:font-name fd1) (w:font-name fd2))
       (= (w:font-char-height fd1) (w:font-char-height fd2))
       (= (w:font-char-width fd1) (w:font-char-width fd2))
       (= (w:font-raster-height fd1) (w:font-raster-height fd2))
       (= (w:font-raster-width fd1) (w:font-raster-width fd2))
       (EQ (w:font-rasters-per-word fd1) (w:font-rasters-per-word fd2))
       (EQ (w:font-words-per-char fd1) (w:font-words-per-char fd2))
       (= (w:font-baseline fd1) (w:font-baseline fd2))
       ; (= (w:font-char-width-table fd1) (w:font-char-width-table fd2))
       (EQ (w:font-left-kern-table fd1) (w:font-left-kern-table fd2))
       (EQ (w:font-indexing-table fd1) (w:font-indexing-table fd2))
       (= (w:font-blinker-width fd1) (w:font-blinker-width fd2))
       (= (w:font-blinker-height fd1) (w:font-blinker-height fd2))
       (NOT (DOTIMES (i 256)
	      (WHEN (NEQ (AREF (w:font-char-width-table fd1) i)
			 (AREF (w:font-char-width-table fd2) i))
		(RETURN t))))))
	 
(DEFUN convert-explorer-font-to-mac-font (exp-fd)
  "Given an Explorer font descriptor EXP-FD, asks the Macintosh for the description of a
corresponding Mac font, then builds and returns an Explorer font descriptor EXP-MAC-FD
with character dimensions being those of the Mac's to fake out tv:sheet-line-out and
tv:draw-string-internal."
  (DECLARE (VALUES exp-Mac-fd))
  ;;  If this is a Mac-font (already translated from an Explorer font) just pass it back...
  (COND ((GET (w:font-name exp-fd) :mac-fd) exp-fd)
	((GET (w:font-name exp-fd) :mac-translated))
	(t 
	 ;; else translate an Explorer font to a Mac font...
	 (LET ((exp-mac-fd (adjust-array (tv:make-font) 256. :initial-element 0
					 :fill-pointer 256))   ; Initializes all slots to NIL.
	       mac-fd
	       (font-name
		 (IF (SEARCH "MAC-" (SYMBOL-NAME (tv:font-name exp-fd))
			     :test #'STRING-EQUAL :end2 4)
		     ;; then ... we are just retranslating, don't make a new name
		     (tv:font-name exp-fd)
		   ;; else ... make up a similar name with MAC- as prefix
		   (INTERN (STRING-APPEND "MAC-" (w:font-name exp-fd)) 'fonts)))) 
	   (COPY-ARRAY-CONTENTS-AND-LEADER exp-fd exp-mac-fd)
	   (SETF mac-fd (SEND *mac* :fontMetrics exp-fd))
	   (SETF (w:font-name exp-Mac-fd) font-name
		 (w:font-char-height exp-Mac-fd) (ARRAY-LEADER mac-fd 2)       ; Mac's font Height
		 (w:font-char-width exp-Mac-fd) (ARRAY-LEADER mac-fd 1)	       ; Mac's widMax
		 (w:font-raster-height exp-Mac-fd) (ARRAY-LEADER mac-fd 2)
		 (w:font-raster-width exp-Mac-fd) (w:font-char-width exp-Mac-fd)
		 ;;(w:font-rasters-per-word exp-Mac-fd) nil
		 ;;(w:font-words-per-char exp-Mac-fd) nil 
		 (w:font-baseline exp-Mac-fd) (ARRAY-LEADER mac-fd 3)  ; Mac's ascent.
		 (w:font-char-width-table exp-Mac-fd) (MAKE-ARRAY 256
								  :initial-element nil)
		 (w:font-left-kern-table exp-Mac-fd) nil
		 (w:font-indexing-table exp-Mac-fd) nil
		 (w:font-blinker-width exp-mac-fd) (ARRAY-LEADER mac-fd 1)     ; Mac's widMax
		 (w:font-blinker-height exp-mac-fd) (ARRAY-LEADER mac-fd 2)    ;Mac's font Height	  
		 (w:font-chars-exist-table exp-Mac-fd) (MAKE-ARRAY 256
								   :element-type '(MOD 1)
								   :initial-element 0))
	   
	   (LOOP for i from 0 to 127
		 do
		 (SETF (AREF (w:font-chars-exist-table exp-Mac-fd) i) 1)
		 (SETF (AREF (w:font-char-width-table exp-Mac-fd) i)
		       (AREF mac-fd i)))
	   ;; Don't give lozenged characters widths
	   (LOOP for i from #xA0 to #xFF
		 do
		 (SETF (AREF (w:font-chars-exist-table exp-Mac-fd) i) 1)
		 (SETF (AREF (w:font-char-width-table exp-Mac-fd) i)
		       (AREF mac-fd i)))
	   
	   ;;  Now that we've built a new exp-Mac-fd, see if its contents are exactly
	   ;;  those of the one we already have for this font.  We cannot use EQUAL
	   ;;  because it compares arrays using EQ and our font descriptor has arrays as
	   ;;  slot values.
	(WHEN (OR (NOT (BOUNDP font-name))
	          (NOT (ARRAYP (SYMBOL-VALUE font-name)))
	          (NOT (TYPEP (SYMBOL-VALUE font-name) 'font))
	          (NOT (compare-two-font-descriptors
			 exp-Mac-fd (SYMBOL-VALUE font-name))))
	  (SET font-name exp-mac-fd))
	(PUTPROP font-name (mac-font exp-fd t) :mac-font)
	(PUTPROP font-name (mac-font-style exp-fd) :mac-font-style)
	(PUTPROP font-name (mac-font-size exp-fd) :mac-font-size)
	(PUTPROP font-name t :mac-fd)
	(UNLESS (GET (w:font-name exp-fd) :mac-fd)
	  (PUTPROP (w:font-name exp-fd) (SYMBOL-VALUE font-name) :mac-translated))
	;;  Return the old font descriptor if no change, otherwise return thre new one...
	(SYMBOL-VALUE font-name)))))

(defun adjust-zero-length-fonts ()
  (do-local-symbols (s 'fonts)
  (when (and (boundp s)
	     (typep (symbol-value s) 'font)
	     (eql 0 (array-total-size (symbol-value s))))
    (adjust-array (symbol-value s) 256. :initial-element 0))))
	     

;; Functions for an application to use to transfer mouse ownership to the desired screen
;; at the desired time.  Because of the invocation of w:mouse-set-sheet TRANSFER-MOUSE
;; always runs in its own process.

;;;   05/11/88  LG		Removed selection of window when going to the Exp to prevent
;;;   			recursion on a lashup.

(DEFUN transfer-mouse (to-screen)
  (LET (screens-selected-window)
    (UNLESS (w:sheet-exposed-p to-screen)
      (SEND to-screen :expose))
    (SETF *window-system-mouse-on-the-Mac* (mac-window-p to-screen))
    (SETF w:default-screen to-screen)
    (remember-bit-array w:default-screen (tv:screen-buffer w:default-screen))
    (w:mouse-set-sheet to-screen)
    (IF *window-system-mouse-on-the-Mac*
	;; Transfering to a Mac-resident screen...
	(WHEN *last-selected-Exp-screens-ID*
	  (SETF screens-selected-window
		(the-last-selected-window (AREF *mac-resident-explorer-screens*
						(ABS *last-selected-Exp-screens-ID*)))))
      ;; else transfering to the Explorer screen...
      (SETF screens-selected-window
	    (the-last-selected-window (AREF *mac-resident-explorer-screens* 0))))
    (remember-call :selection screens-selected-window w:selected-window)))

(DEFUN give-mouse-ownership-to-the-Mac
       (&optional (to-Mac-screen
		    (the-screen (AREF *Mac-resident-Explorer-screens*
				      (ABS *last-selected-Exp-screens-ID*)))))
  (UNLESS to-Mac-screen
    (SETF to-mac-screen
	  (LOOP for x being the array-elements of *Mac-resident-Explorer-screens*
		when x
		return (the-screen x))))
  (remember-call :selection w:default-screen w:mouse-sheet)
  (IF (NEQ current-process tv:mouse-process)
      (transfer-mouse to-Mac-screen)
    ;; else...
    (PROCESS-RUN-FUNCTION '(:name "Transfer Mouse" :priority 31)
			  #'transfer-mouse to-Mac-screen)))


(DEFUN give-mouse-ownership-to-the-Explorer ()
  (IF (NEQ current-process tv:mouse-process)
      (transfer-mouse w:main-screen)
     ;; else...
    (PROCESS-RUN-FUNCTION '(:name "Transfer Mouse" :priority 31)
			    #'transfer-mouse w:main-screen)))


(EXPORT '(give-mouse-ownership-to-the-Mac give-mouse-ownership-to-the-Explorer))


(DEFUN toggle-mouse (&optional to-screen)
  "Switch control of the mouse to the screen that does not currently own it."
  (CASE to-screen
    (1 (give-mouse-ownership-to-the-Mac))
    (2 (give-mouse-ownership-to-the-explorer))
    (t
     (IF (mac-window-p to-screen)
	 (give-mouse-ownership-to-the-Mac to-screen)
       ;; else
       (IF (TYPEP to-screen 'w:screen)
	   (give-mouse-ownership-to-the-Explorer)
	 ;; else...
	 (IF *window-system-mouse-on-the-Mac*
	     (give-mouse-ownership-to-the-explorer)
	   ;; else...
	   (give-mouse-ownership-to-the-Mac)))))))



;;;
;;; The following code, together with changes to tv:mouse-set-blinker-definition and
;;;   tv:mouse-set-blinker, implements the Explorer's customer mouse blinkers on the
;;;   microExplorer.  Whenever the above two functions are called a
;;;   :create-Mac-image-of-Explorer-mouse-cursor message is sent to the new blinker via
;;;   :send-if-handles.  Only tv:mouse-character-blinker instances support this message,
;;;   and only if the following code is loaded (microExplorer).  The effect of this message
;;;   is to send the new blinker's glyph and mask and hot-spot to the Mac to be used as
;;;   the new Mac mouse cursor.  All bitimages created are cached in the font's property
;;;   list.
;;;

(DEFUN get-real-font-name (Mac-fontname)
  (INTERN (SUBSTRING-AFTER-CHAR #\- (STRING Mac-fontname)) 'fonts))

(DEFMETHOD (tv:mouse-blinker-mixin :create-Mac-image-of-Explorer-mouse-cursor)
	   (old-blinker-font old-blinker-character)
  (DECLARE (SPECIAL mac-mouse-cursor-offset))
  (COND
    ;;  Tell the Mac to forget about the mouse cursor if we're changing to other than a
    ;;  character-blinker...
    ((NOT (TYPEP self 'tv:mouse-character-blinker))
     (SEND *mac* :set-mouse-blinker (- 40 mac-mouse-cursor-offset)))
    ;; 40 = resource number for blank Mac mouse cursor.

    ;;  Tell the Mac nothing if staying with the same character-blinker...
    ((AND (TYPEP tv:mouse-blinker 'tv:mouse-character-blinker)
	  (EQ (SYMEVAL-IN-INSTANCE self 'tv:font) old-blinker-font)
	  (EQL (SYMEVAL-IN-INSTANCE self 'tv:character) old-blinker-character))
     nil)

    ;;  We are changing to a different mouse blinker.  The new blinker is of type
    ;;  mouse-character-blinker.  Create images of its glyph and its mask and cache them on
    ;;  its font's property list if they aren't already there.  Always send the glyph image,
    ;;  mask image, and hot-spot point.
    ;;
    ;;  This is crude code that converts the entire specified font the first time it is seen,
    ;;  then uses the individual characters' images as needed...
    (t
     (LET* ((font (SYMEVAL-IN-INSTANCE self 'tv:font))
            (character (SYMEVAL-IN-INSTANCE self 'tv:character))
            (Mac-fontname (tv:font-name font))
            (fontname (get-real-font-name Mac-fontname))
            (mac-glyphs (get-Mac-glyphs fontname))
            (glyph-or-resource-number (AREF mac-glyphs character))
	    mask)
       (IF (INTEGERP glyph-or-resource-number)
	   ;;  The cursor glyph is a resource on the Mac.  Send just its resource #.
	   (SEND *mac* :set-mouse-blinker glyph-or-resource-number)
	 ;; else get the glyph's mask and send the glyph, mask, and hot spot...
	 (SETF mask (get-Mac-mask-image-for-a-char-glyph fontname
							 mac-glyphs
							 character))
	 (SEND *mac* :set-mouse-blinker 0 glyph-or-resource-number mask
	       tv:x-offset tv:y-offset))
       ))))


(DEFUN compare-bitblt-blinker-with-Mac-image (bb1 Mac-image)
  "BB1 is a bitblt-blinker; its bit array is Nx32 bits, where N<17.  MAC-IMAGE is a 16x16
bit array.  Returns T if the two arrays contain the same bit-image and it is not all zero bits,
for no real bitblt blinker could be all zeros."
  (LET (LENGTH array1)
    (SETF array1 (SEND bb1 :array))
    (WHEN (< (SETF length (ARRAY-DIMENSION array1 0)) 17)
      (ADJUST-ARRAY *16bit-array1* (* 2 length)
		    :displaced-to array1)
      (ADJUST-ARRAY *16bit-array2* length
		    :displaced-to Mac-image)
      (NOT (LOOP with all-zero-p = t
		 for i from 0 below length
		 for ii from 0 by 2
		 WHEN (/= (AREF *16bit-array1* ii) (AREF *16bit-array2* i))
		 RETURN t
		 WHEN (NOT (ZEROP (AREF *16bit-array1* ii)))
		 DO (SETF all-zero-p nil)
		 FINALLY (RETURN all-zero-p))))))


(DEFMETHOD (tv:bitblt-blinker :create-Mac-image-of-Explorer-mouse-cursor)
	   (old-blinker-font old-blinker-character)
  (DECLARE (SPECIAL mac-mouse-cursor-offset)
	   (IGNORE old-blinker-font old-blinker-character))
  (COND
    ;;  Tell the Mac to forget about the mouse cursor if the bitblt blinker we're changing to
    ;;  is larger than 16x16 pixels...
    ((OR (> tv:width 16) (> tv:height 16))
     (SEND *mac* :set-mouse-blinker (- 40 mac-mouse-cursor-offset)))
    ;; 40 = resource number for blank Mac mouse cursor.

    ;;  We are changing to a different mouse blinker.  The new blinker is of type
    ;;  bitblt-blinker.  Create images of its glyph and its mask and cache them on the list
    ;;  *known-bitblt-blinkers*.  Always send the glyph image, mask image, and hot-spot
       ;;  point.
    ;;
    (t
     (LET (entry glyph mask (sself self) sum-of-bits)
       ;;  Get or create SELF's entry on the list of *known-bitblt-blinkers*...
       (UNLESS (SETF entry (ASSOC sself *known-bitblt-blinkers*))
	 (PUSH (SETF entry (LIST sself
				 (MAKE-ARRAY '(16 16) :element-type 'bit)
				 (MAKE-ARRAY '(16 16) :element-type 'bit)))
	       *known-bitblt-blinkers*))
       (SETF glyph (OR (SECOND entry)
		       (SETF (SECOND entry)
			     (MAKE-ARRAY '(16 16) :element-type 'bit)))
	     mask (THIRD entry))
       
       ;;  Unless SELF's entry describes SELF's current blinker, update it so it does...
       (UNLESS (compare-bitblt-blinker-with-Mac-image sself glyph)
	 (SETF sum-of-bits 0)
	 (DOTIMES (i tv:height)
	   (DOTIMES (j tv:width)
	     (INCF sum-of-bits
		   (SETF (AREF glyph i j) (AREF array i j)))))
	 (IF (ZEROP sum-of-bits)
	     (SETF (SECOND entry) nil)
	     ;; else...
	   (mac:get-Mac-mask-image-for-any-glyph glyph mask)))
       
       ;;  Give the Mac SELF's current definition as the Mac's mouse cursor...
       (SEND *mac* :set-mouse-blinker 0 glyph mask tv:x-offset tv:y-offset)))))


;;ab 5.25.88.  Fix to work if MAC-font passed in (by getting Expl. font object
;;             previously saved on property list by PARSE-FONT-DESCRIPTOR).

(DEFUN get-Mac-glyphs (fontname)
  (LET* (mac-glyphs fd)
    (UNLESS (SETF mac-glyphs (GET 'mac-glyphs fontname))
      
      ;; If this is a MAC-FONT, must get the corresponding, saved Explorer font 
      ;; if there is one.  Else use default.
      (COND ((mac-font-p (SYMBOL-VALUE fontname))
	     (SETQ fd (GET fontname :explorer-font))	;was put there by PARSE-FONT-DESCRIPTOR
	     (COND (fd
		    (SETQ fontname (tv:font-name fd)))
		   (t (SETQ fontname 'fonts:mouse
			    fd (GET fontname :explorer-font))))
	     (SETQ fd (font-into-font-descriptor fd)))
	    (t					;already an Explorer font
	     (SETF fd (font-name-font-descriptor fontname))))
      (SETF mac-glyphs (SETF (GET 'mac-glyphs fontname) (MAKE-ARRAY 256)))
      (LOOP with Mac-glyph
	    for char-code from 0 below (ARRAY-TOTAL-SIZE fd)
	    for char-glyph = (AREF fd char-code)
	    when char-glyph do
	    (SETF Mac-glyph (MAKE-ARRAY '(16 16) :element-type 'bit))
	    (SETF (AREF mac-glyphs char-code) Mac-glyph)
	    (LOOP for row from 0 upto (1- (MIN (ARRAY-DIMENSION char-glyph 0) 16))
		  do
		  (LOOP for column from 0 upto (1- (MIN (ARRAY-DIMENSION char-glyph 1) 16))
			do
			(SETF (AREF Mac-glyph row column) (AREF char-glyph row column))))))
    mac-glyphs))


;;; (AREF *row-column-start-stop* N 0) = column-number of left-most black pixel in row N.
;;; (AREF *row-column-start-stop* N 1) = column-number of right-most black pixel in row N.
;;; (AREF *row-column-start-stop* N 2) = row-number of top-most black pixel in column N.
;;; (AREF *row-column-start-stop* N 3) = row-number of bottom-most black pixel in column N.
;;; Any of these may be NIL if that row/column contains no black pixels.
(DEFVAR *row-column-stop-start* (MAKE-ARRAY '(16 4)))


(DEFUN get-Mac-mask-image-for-any-glyph (glyph mask)

  (FILL (THE array *row-column-stop-start*) nil)
  (FILL (THE array mask) 0)
  
  ;;  Scan across each row from left-to-right and from right-to-left.
  ;;  Blacken the pixel preceding the first black pixel found.
  (LOOP for row from 0 to (1- (ARRAY-DIMENSION glyph 0))
	do
	(LOOP for column from 0 to (1- (ARRAY-DIMENSION glyph 1))
	      when (NOT (ZEROP (AREF glyph row column)))
	      do
	      (UNLESS (ZEROP column)
		(SETF (AREF mask row (1- column)) 1))
	      (SETF (AREF *row-column-stop-start* row 0) column)
	      (RETURN))
	(LOOP for column from (1- (ARRAY-DIMENSION glyph 1)) downto 0
	      when (NOT (ZEROP (AREF glyph row column)))
	      do
	      (UNLESS (= column (1- (ARRAY-DIMENSION glyph 1)))
		(SETF (AREF mask row (1+ column)) 1))
	      (SETF (AREF *row-column-stop-start* row 1) column)
	      (RETURN)))
  
  ;;  Scan across each row from top-to-bottom and from bottom-to-top.
  ;;  Blacken the pixel preceding the first black pixel found.
  (LOOP for column from 0 to (1- (ARRAY-DIMENSION glyph 1))
	do
	(LOOP for row from 0 to (1- (ARRAY-DIMENSION glyph 0))
	      when (NOT (ZEROP (AREF glyph row column)))
	      do
	      (UNLESS (ZEROP row)
		(SETF (AREF mask (1- row) column) 1))
	      (SETF (AREF *row-column-stop-start* column 2) row)
	      (RETURN))
	(LOOP for row from (1- (ARRAY-DIMENSION glyph 0)) downto 0
	      when (NOT (ZEROP (AREF glyph row column)))
	      do
	      (UNLESS (= row (1- (ARRAY-DIMENSION glyph 0)))
		(SETF (AREF mask (1+ row) column) 1))
	      (SETF (AREF *row-column-stop-start* column 3) row)
	      (RETURN)))
  
  ;;  Fill in the area enclosed by the pixels just set...
  (LOOP for row from 0 to (1- (ARRAY-DIMENSION mask 0))
	for row-start = (AREF *row-column-stop-start* row 0)
	for row-stop = (AREF *row-column-stop-start* row 1)
	when (AND row-start row-stop)
	do
	(LOOP for column from row-start to row-stop
	      when (<= (OR (AREF *row-column-stop-start* column 2) 15)
		       row
		       (OR (AREF *row-column-stop-start* column 3) 0))
	      do
	      (SETF (AREF mask row column) 1)))
  (LOOP for column from 0 to (1- (ARRAY-DIMENSION mask 1))
	for column-start = (AREF *row-column-stop-start* column 2)
	for column-stop = (AREF *row-column-stop-start* column 3)
	when (AND column-start column-stop)
	do
	(LOOP for row from column-start to column-stop
	      when (<= (OR (AREF *row-column-stop-start* row 0) 15)
		       column
		       (OR (AREF *row-column-stop-start* row 1) 0))
	      do
	      (SETF (AREF mask row column) 1)))
  mask)
  

(DEFUN get-Mac-mask-image-for-a-char-glyph (fontname mac-glyphs char-code)
  (LET (md glyph mask)
    (SETF md (OR (GET 'mac-mask-descriptors fontname)
		 (SETF (GET 'mac-mask-descriptors fontname)
		       (MAKE-ARRAY 256))))
    (UNLESS (OR (SETF mask (AREF md char-code))
		(NULL (AREF (tv:font-chars-exist-table (SYMBOL-VALUE fontname)) char-code)))
      (SETF mask (MAKE-ARRAY '(16 16) :element-type 'bit))
      (SETF (AREF md char-code) mask)
      (SETF glyph (AREF mac-glyphs char-code))
      (get-Mac-mask-image-for-any-glyph glyph mask))
    mask))


(DEFMETHOD (tv:BITBLT-BLINKER :around :BLINK)
	   (continuation mapping-table arg-list)
  (UNLESS (AND (mac-window-p tv:sheet)
	       (<= tv:width 16)
	       (<= tv:height 16))
    (LEXPR-FUNCALL-WITH-MAPPING-TABLE continuation mapping-table arg-list)))

(DEFMETHOD (tv:bitblt-blinker :after :set-array) (IGNORE)
     (WHEN (EQ self tv:mouse-blinker)
       (SEND self :create-Mac-image-of-Explorer-mouse-cursor nil nil)))
					   ;
;;;  Suggestions defines this daemon, so applications compiled with Suggestions loaded
;;;  expect it to be there.  We don't load Suggestions on an mX, so it doesn't exist and
;;;  such applications blow if we resize a screen containing one of their windows.  Profile
;;;  is such an application.   This null definition satisfiies everybody.

(DEFMETHOD (tv:sheet :after :change-of-size-or-margins) (&rest ignore)
  nil)

;;;
;;;  Allow the Explorer to Enable or Disable Direct Drawing on the Mac...
;;;

(DEFUN set-direct-drawing-mode (new-mode)
  (CHECK-ARG new-mode (MEMBER new-mode '(:on :off))
	     ":ON or :OFF")
  (LET ((acb (add:get-acb 4))
	(channel (add:find-channel si:%Chan-Type-Misc)))
    (add:init-acb acb
		  si:%MC-tvcalls
		  si:%TC-DIRECT-DRAW)
    (add:set-parm-8b acb 0 (IF (EQ new-mode :ON) 1 0))
    (SETF (add:requestor-complete acb) t)
    (add:transmit-packet acb channel)))



;;;
;;;   Routines to print a Mac circular debug buffer dump...
;;;

(DEFVAR *cbuffer-of-cs-strings* nil)

(DEFVAR *control-string* (MAKE-ARRAY 128 :element-type 'string-char :fill-pointer 0))

(DEFUN is-next-arg-a-string? (cs current-position)
  (LOOP for i from current-position to (LENGTH cs)
	when (CHAR= (AREF cs i) #\~)
	do (RETURN (AND (CHAR= (AREF cs i) #\~)
			(CHAR= (AREF cs (1+ i)) #\a))
		   (1+ i))
	finally (RETURN  nil i)))
  
(DEFUN decode-debug-dump-entry (buffer entry stream)
  (LET (cs-adr n-args start-of-args args arg (cs *control-string*)
	next-arg-is-a-string-p current-position)
    (SETF n-args (AREF buffer (+ entry 2))
	  cs-adr (AREF buffer (+ entry 1))
	  start-of-args 2)
    (find-a-cs-string cs-adr)
    (SETF current-position 0)
    (SETF args
	  (LOOP for j from 0 below n-args do
		(SETF arg (AREF buffer (+ entry start-of-args n-args (- j))))
		(IF (> arg #x7FFFFFFF) (SETF arg (- (1+ (logxor #xFFFFFFFF arg)))))
		(MULTIPLE-VALUE-SETQ (next-arg-is-a-string-p current-position)
		  (is-next-arg-a-string? cs current-position))	
		(IF (AND (> #x7FFFFFFF arg 100000) next-arg-is-a-string-p )
		    (LET ((*control-string* (MAKE-ARRAY 128 :element-type 'string-char :fill-pointer 0)))
		      (find-a-cs-string arg)
		      (SETF arg *control-string*))
		  ;; else...
		  ;; (SETF arg (LOGAND #xFFFFFFFF arg))
		  )
		COLLECT arg))
    (APPLY #'FORMAT stream *control-string* args)))

(DEFUN find-a-cs-string (cs-adr)
  "Leaves the control-string whose address is CS-ADR in *control-string*."
  (LET* ((buffer *cbuffer-of-cs-strings*)
	 (disp-to-backchain-head (cbuffer-start-bchain buffer))
	 %-flag)
    (SETF (FILL-POINTER *control-string*) 0)
    (LOOP FOR start-last-entry
	  FIRST disp-to-backchain-head
	  THEN start-next-entry
	  FOR start-next-entry
	  FIRST (AREF buffer disp-to-backchain-head)
	  THEN (AREF buffer start-next-entry)
	  UNTIL (OR (AND (ZEROP start-last-entry)
			 (<= start-next-entry disp-to-backchain-head))
		    (AND (> start-last-entry disp-to-backchain-head)
			 (<= start-next-entry disp-to-backchain-head)))
	  DO
	  (WHEN (= cs-adr (AREF buffer (+ start-next-entry 1)))
	    (LOOP for word-number from 3
		    for word = (AREF buffer (+ start-next-entry word-number))
		    until (LOOP for field-start from 24 downto 0 by 8
				for mac-ch = (LDB (BYTE 8 field-start) word)
				for exp-ch = (tv:MAC-TO-EXPLORER-CHAR-CODE mac-ch)
				when (ZEROP mac-ch) RETURN t
				when (CHAR= exp-ch #\%) do
				(VECTOR-PUSH-EXTEND #\~ *control-string*)
				(SETF %-flag t)
				else do
				(IF %-flag
				  (SELECTOR exp-ch char=
				    (#\s (VECTOR-PUSH-EXTEND #\a *control-string*))
				    (#\0 (VECTOR-PUSH-EXTEND #\0 *control-string*)
					 (VECTOR-PUSH-EXTEND #\, *control-string*))
				    (:otherwise (VECTOR-PUSH-EXTEND exp-ch *control-string*)))
				  ;; else...
				  (VECTOR-PUSH-EXTEND exp-ch *control-string*))
				(SETF %-flag nil)))
	    (RETURN)))))

(DEFUN print-debug-dump (&key (nth 0) (to :editor))
  (LET* ((buffer-doublet (NTH nth *debug-dumps*))
	 (buffer (FIRST buffer-doublet))
	 (*cbuffer-of-cs-strings* (SECOND buffer-doublet))
	 (disp-to-foreward-chain-start (cbuffer-start-fchain buffer))
	 (disp-to-foreward-chain-end (cbuffer-start-bchain buffer))
	 stream filename)
    (SELECT to
      (:editor (SETF filename (MAKE-PATHNAME :defaults (rest (assoc si:local-host *default-pathname-defaults*))
				      :name "mac-dump" :type "text"))
	       (SETF stream (ZWEI:MAKE-FILE-BUFFER-STREAM filename)))
      (:file (SETF filename (MAKE-PATHNAME :defaults (rest (assoc si:local-host *default-pathname-defaults*))
					   :name "mac-dump" :type "text"))
	     (SETF stream (OPEN filename :direction :output :if-exists :new-version)))
      (:display (SETF stream *terminal-io*)))
    (FORMAT stream "~%Mac debug dump received ")
    (time:print-universal-date (cbuffer-time-rcvd buffer) stream)
    (FORMAT STREAM "...~%~%")
    (UNWIND-PROTECT
	(LOOP FOR start-last-entry = start-next-entry
	      FOR start-next-entry
	      FIRST disp-to-foreward-chain-start
	      THEN (AREF buffer start-next-entry)
	      UNTIL (= start-next-entry disp-to-foreward-chain-end)
	      DO (decode-debug-dump-entry buffer start-next-entry stream))
      (WHEN filename (CLOSE stream)))))

;;;
;;;   Executing this function removes all deactivated windows from the mac's and our data
;;;   structures...
;;;

(DEFUN clean-up-deactivated-windows ()
  "Returns #-of-bytes freed in bit array cache."
  (WITHOUT-INTERRUPTS 
    (LOOP with total-size-of-deactivated-windows = 0
	  for wid from 0 to *largest-window-id-ever-assigned*
	  for window = (AREF *all-windows-and-screens* wid)
	  when (AND window
		    (NOT (TYPEP window 'tv:standard-screen))
		    (EQ (SEND window :status) :deactivated))
	  do
	  (deactivate-mac-window window wid)
	  (WHEN (tv:sheet-bit-array window)
	    (INCF total-size-of-deactivated-windows
		  (* (CEILING (tv:sheet-width window) 8) (tv:sheet-height window))))
	  finally (RETURN total-size-of-deactivated-windows))))

(DEFUN clean-up-deactivated-windows-periodically ()
  (LOOP do
	(clean-up-deactivated-windows)
	(PROCESS-SLEEP (* 60 60 60))))

(si:add-initialization "Clean up deactivated windows before disk-save"
		       '(mac:clean-up-deactivated-windows)
		       :before-cold)

(si:add-initialization "Clean up deactivated windows periodically"
		       '(LET ((our-process-name "Flush Deactivated Windows"))
			  (UNLESS (MEMBER our-process-name
					sys:all-processes
					:test #'(lambda (x y)
						  (STRING-EQUAL x (SEND y :name))))
			  (PROCESS-RUN-FUNCTION our-process-name
						#'mac:clean-up-deactivated-windows-periodically)))
		       :now)