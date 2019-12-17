;;;;-*- Mode:Common-Lisp; Package:MACINTOSH; Base:10; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB); Patch-file:T -*-


;;;;
;;;; Basic additions to get mouse input from the Mac.  All TV: modified functions and
;;;; methods are in the file CHANGED-TV-FUNCTIONS...
;;;;

(DEFUN lispm-or-mac-mouse-wakeup ()
  "This function returns non-nil if either the LISPM's or the Mac's mouse
has moved."
  (OR tv:mouse-wakeup tv:mouse-reconsider
      (mac:mac-consider-mouse)))


;;;  08-28-88   LG	      5-6     Don't remember-call each time we're called.
;;;  05/11/88  LG  	Select new window in a separate high priority process to
;;;  			avoid mouse-process invocations giving us trouble.  Select
;;;  			a new screen here iff just deselecting the current selected
;;;  			window; let :select-screen select the screen if we're
;;;  			selecting a new selected window. 

(DEFUN analyze-Mac-mouse-return (new-mouse-x button-event Exp-screen-id)
  "Returns T iff input values represent a real mouse position in the
active screen.  Handles the -1 (ignorable) and -2 (new active
screen) psuedo-mouse-coordinates.  Deselects the selected window if its
E-screen/M-window is no longer the front M-window."
  (LET ((old-screen-id (ABS *last-selected-Exp-screens-ID*))
	(new-screen-id (ABS Exp-screen-id)) (next-E-window-to-select t))
    ;;  Deselect the currently selected window if its E-screen/M-window is no longer the Mac's
    ;;  front window.  This stops its blinkers, mainly.
    (WHEN (< Exp-screen-id 0)		   ; Mouse is not in the front M-window, and
      (UNLESS (= new-mouse-x -1)	   ;    this non-front window is one of ours, and
	(WHEN (EQ new-screen-id		   ;    it just stopped being the front M-window,
		  old-screen-id)
	  (WHEN w:selected-window
	    (SEND w:selected-window :deselect nil))
	  (SETF *last-selected-Exp-screens-ID*
		(- *last-selected-Exp-screens-ID*))
	  (remember-call :selection
			 old-screen-id w:default-screen
			 w:mouse-sheet w:selected-window))))
    
    ;;  Switch which of our E-screens is selected to E-screen XXX if a button event occured in its
    ;;  M-window and it was not already our selected E-screen...
    (UNLESS (= new-mouse-x -1)		   ; Ignore if mouse is not in one of our M-windows.	  
      (WHEN button-event		   ; the mouse button is pressed, and
	(UNLESS (EQ old-screen-id new-screen-id)   ; its a new window
	  (SETF next-E-window-to-select
		(the-last-selected-window
		  (AREF *Mac-resident-Explorer-screens* new-screen-id)))
	  (PROCESS-RUN-FUNCTION
	    '(:name "Select New Window" :priority 32)
	    #'(lambda (x nsid)
		(IF x
		    (SEND x :select-screen nil)
		  ;; else...
		  (WHEN w:selected-window
		    (SEND w:selected-window :deselect nil))		
		  (select-a-screen nsid)))
	    next-E-window-to-select new-screen-id)
	  (PROCESS-SLEEP 1)
	  (remember-call :selection
			 old-screen-id w:default-screen
			 w:mouse-sheet w:selected-window
			 next-E-window-to-select))))
    
    ;;  Return T iff mouse was within the useable area of our selected E-screen/M-window...
    (AND (> new-mouse-x -1) (> Exp-screen-id 0) next-E-window-to-select)
    ))


	  
;;;  04/11/88  LG		Do not return T (thereby activating the mouse process) if the
;;;  			mouse is not now within any Explorer window and was not
;;;  			within any Explorer window the last time we looked at it, too.
			       
;;;  02/03/89  LG  	Maintain mouse-x-speed and mouse-y-speed.
(DEFUN mac-consider-mouse ()
  "Checks the Mac's mouse to see if it has moved or a button event occurred.
Sets mouse variables and returns t if anything happened."
  (WHEN (AND *window-system-mouse-on-the-mac*
	     (NOT *ignore-commands-for-the-Mac*))
;;;    (mac:dump-draw-char-cache)
    (MULTIPLE-VALUE-BIND (new-mouse-x new-mouse-y button-event Exp-screen-id)
	(SEND *mac* :mouse-position)
      (WHEN (analyze-Mac-mouse-return new-mouse-x button-event Exp-screen-id)
	(COND (button-event
	       (SETF si:mouse-x-speed (- tv:mouse-x new-mouse-x)
		     si:mouse-y-speed (- tv:mouse-y new-mouse-y))
	       (remember-call :mouse button-event new-mouse-x new-mouse-y si:mouse-x-speed si:mouse-y-speed)
	       (SETF tv:mouse-x new-mouse-x
		     tv:mouse-y new-mouse-y)
	       (mac-insert-mouse-buttons button-event)
	       t)
	      ((OR (NOT (EQL new-mouse-x tv:mouse-last-x))
		   (NOT (EQL new-mouse-y tv:mouse-last-y)))
	       (SETF si:mouse-x-speed (- tv:mouse-x new-mouse-x)
		     si:mouse-y-speed (- tv:mouse-y new-mouse-y))
	       (remember-call :mouse button-event new-mouse-x new-mouse-y si:mouse-x-speed si:mouse-y-speed)
	       (SETF tv:mouse-x new-mouse-x
		     tv:mouse-y new-mouse-y)
	       ;;  Return T unless mouse is outside of the E-window and was outside of
	       ;;  the E-window last time we looked at it, too...
	       (OR (AND (<= 0 new-mouse-x (1- (tv:sheet-width tv:mouse-sheet)))
			(<= 0 new-mouse-y (1- (tv:sheet-height tv:mouse-sheet))))
		   (NOT (OR (IF (MINUSP new-mouse-x)
				(ZEROP tv:mouse-last-x)
			      (IF (>= new-mouse-x
				      (tv:sheet-width tv:mouse-sheet))
				  (= (tv:sheet-width tv:mouse-sheet)
				     (1+ tv:mouse-last-x))
				))
			    (IF (MINUSP new-mouse-y)
				(ZEROP tv:mouse-last-y)
			      (IF (>= new-mouse-y
				      (tv:sheet-height tv:mouse-sheet))
				  (= (tv:sheet-height tv:mouse-sheet)
				     (1+ tv:mouse-last-y)))
			      )))))	       
	      (t nil))))))


(DEFMACRO update-mouse-buttons-buffer (button time)
  `(LET ((index tv:mouse-buttons-buffer-in-index))
     ;; Allocate some room in the mouse-buttons-buffer.
     (SETF tv:mouse-buttons-buffer-in-index
	   (REM (+ tv:mouse-buttons-buffer-in-index 4)
		(LENGTH tv:mouse-buttons-buffer)))
     ;; Put the button into the mouse-buttons-buffer.
     (SETF (AREF tv:mouse-buttons-buffer (+ index 3)) ,button)
     (SETF (AREF tv:mouse-buttons-buffer (+ index 1)) tv:mouse-x)
     (SETF (AREF tv:mouse-buttons-buffer (+ index 2)) tv:mouse-y)
     (SETF (AREF tv:mouse-buttons-buffer (+ index 0)) ,time)))

(DEFUN mac-insert-mouse-buttons (button-event)
  "Puts a button excursion in the tv:mouse-buttons-buffer.
Button-event is expected to be a fixnum code in the format #b0000 0000 SCUT DLMR where:
  S = 1 means mac shift key was down,
  C = 1 meand mac control key was down,
  U = 1 means a mouse button up excursion occured,
  T = 1 means a mouse tripple click occured,
  D = 1 means a mouse double click occured,
  L = 1 means it was a mouse left button,
  M = 1 means it was a mouse middle button... actually means mac option key was down,
  R = 1 means it was a mouse right button... actually means mac command key was down.
Button-event should not be nil.
Note that the Mac now detects double clicks." 
  (WITHOUT-INTERRUPTS
    (LET* ((button (COND ((LOGBITP 0 button-event) 1)
			 ((LOGBITP 1 button-event) 2)
			 ((LOGBITP 2 button-event) 4)
			 (t 0)))
	   (down (PLUSP button))
	   (up (LOGBITP 5 button-event))
	   (double (LOGBITP 3 button-event))
	   (triple (LOGBITP 4 button-event)) 
	   (now (time:fixnum-microsecond-time))
	   (mouse-bounce-time (1+ tv:mouse-bounce-time)))
      (COND ((AND down up (NOT double) (NOT triple))	   ; Single click
	     (SETQ tv:mouse-last-buttons 0)
	     (update-mouse-buttons-buffer button now)
	     (update-mouse-buttons-buffer 0 (time:time-increment now mouse-bounce-time)))
	    ((AND down up double)	   ; Double click
	     (SETQ tv:mouse-last-buttons 0)
	     (update-mouse-buttons-buffer button now)
	     (update-mouse-buttons-buffer 0 (SETF now (time:time-increment now mouse-bounce-time)))
	     (update-mouse-buttons-buffer button
					  (SETF now (time:time-increment
						      now
						      (LSH (OR tv:mouse-double-click-time
							       mouse-bounce-time) -1))))
	     (update-mouse-buttons-buffer 0 (time:time-increment now mouse-bounce-time)))
	    ((AND down up triple))	   ; Triple clicks not currenty serviced
	    ((AND down (NOT up) (NOT double))	   ; Holding button down
	     (SETQ tv:mouse-last-buttons 0)
	     (update-mouse-buttons-buffer button now))
	    ((AND (NOT down) up)	   ; Releasing button
	     (update-mouse-buttons-buffer 0 now))))))



;;;
;;; Accessor functions for the mouse-buttons-buffer
;;;


tv:
(DEFUN mouse-buttons-buffer-entry (&optional (entry 0))
  "Returns the value in the previous Nth entry in the mouse-buttons-buffer as
four values: button, time, x, and y. Entry 0 means the most recent transition."
  (LET ((index (ldb (byte (1- (haulong (length mouse-buttons-buffer)))
				 0)
			   (- mouse-buttons-buffer-in-index
			      (* -4 entry) 1))))
    (VALUES (CASE (AREF mouse-buttons-buffer index)
	      (0 nil)
	      (1 :R)
	      (2 :M)
	      (4 :L)
	      (otherwise nil))
	    (AREF mouse-buttons-buffer (- index 3))
	    (AREF mouse-buttons-buffer (- index 2))
	    (AREF mouse-buttons-buffer (- index 1)))))

tv:
(DEFUN analyze-last-button ()
  "Analyzes the current mouse-buttons-buffer and returns two values. The
first is the click type, either :single, :double, :triple, :long or :holding. The second
value is :left, :middle, or :right."
  (MULTIPLE-VALUE-BIND (last-transition last-transition-time)
      (mouse-buttons-buffer-entry 0)
    (IF last-transition
	(VALUES :holding last-transition)
      (MULTIPLE-VALUE-BIND (last-click last-click-time)
	  (mouse-buttons-buffer-entry -1)
	(MULTIPLE-VALUE-BIND (previous-click previous-click-time)
	    (mouse-buttons-buffer-entry -3)
	  (MULTIPLE-VALUE-BIND (pre-previous-click pre-previous-click-time)
	      (mouse-buttons-buffer-entry -5) 
	    (COND ((NULL mouse-double-click-time)
		   (VALUES :single last-click))
		  ((> (TIME-DIFFERENCE last-transition-time last-click-time)
		      mouse-double-click-time)
		   (VALUES :long last-click)) 
		  ((AND (EQL last-click previous-click)
			(< (TIME-DIFFERENCE last-click-time previous-click-time)
			   mouse-double-click-time))
		   (IF (AND (EQL last-click pre-previous-click)
			       (< (TIME-DIFFERENCE last-click-time pre-previous-click-time)
				  (+ mouse-double-click-time mouse-double-click-time)))
		       (VALUES :triple last-click)
		     (VALUES :double last-click)))
		  (t (VALUES :single last-click)))))))))