;;;-*- Mode:Common-Lisp; Package:TV; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1985-1989 Texas Instruments Incorporated. All rights reserved.

;;
;; ** MULTIPLE SCREEN support **
;; >> Code for creating screens, switching between screens, etc.
;;

;;; Change history:
;;;
;;;  Date      Author   Description
;;; -------------------------------------------------------------------------------------
;;; 03/27/89  MAY    Change to make-a-screen to prevent color monitor cable problems when
;;;		   w:create-color-screen calls make-a-screen.
;;; 03/23/89  MAY   Changes to quiet cwarn errors.
;;; 03/09/89  MAY Added clearing of inhibit-who-line in (standard-screen :around :expose) and
;;; 		removed deletion of screen from tv:all-the-screens in kill-screen. Done by (SHEET :DEACTIVATE).
;;; 03/07/89  JLM  Changed (standard-screen :around :expose) to refernce mp-displayp instead of MP:displayp
;;; 02/28/89  JLM  Changed (standard-screen :around :expose) to support MP cool boot.
;;; 02/10/89  LG   Make TERM ctrl-S work on the mX.
;;; 01/27/89  KJF  [MAY] Changes for Multiple Monitor (MMON) support, search for MMON
;;; 01/27/89  KJF  [MAY] Added screens-who-line-screen macros, during addition of Multiple Monitor
;;;                    (MMON) support.
;;; 07/18/88  KJF    Changes to (standard-screen :around :expose) for window system build.
;;; 05/23/88  KJF    Changed references to screen-descriptor to screens-who-line-screen due to source build
;;;                     being done which allowed instance variable to take on a meaningful name.
;;; 05/01/88  KJF     Check for identical names for screens in make-a-screen.  Prevents problem of error
;;;                      being signalled in define-screen when no screens are exposed, and mouse-sheet is nil, and...
;;;                      Also, changed arg in make-a-screen from :name-of-screen to :name and subsequent
;;;                       change to create-color-screen.
;;; 04/28/88  KJF     Change to make-a-screen to put Select Screen on system menu.
;;; 04/26/88  KJF     For :around :expose and :after :deexpose, only do things if screen was created using
;;;                      one of the new multiple screen functions.  Prevents problems on microExplorer.
;;; 04/26/88  KJF     Changes to functions to do nothing on microExplorer.  Search for (mac-system-p).
;;; 04/23/88  KJF     Created.

;;; ********* Attention ! *********
;;; This function is redefined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;
(DEFUN make-a-screen (&key
		      (color? t)
		      (name nil))
;; >>> Note, these are for POSSIBLE future enhancements.  They will be bound below to
;; reasonable values for now.  04/22/88 KJF.
;;		      (monitor :color)
;;		      (number-of-bit-planes 8)
;;		      (with-who-line? t)
;;		      (sib-number *default-sib*))
;;  		      (monitors 1)
;;		      (expose-it?)
  "Create a screen of the desired type.  The newly created screen will be
exposed and made active.  That is, it will be made the W:DEFAULT-SCREEN.
The new screen will contain a Lisp Listener.  KEYWORD arguments have the
following meaning:
color? - T means make a color screen.  NIL means a monochrome screen.
name - Name for screen.  Each screen must have an unique name.
         Defaulting this to NIL means an unique name will be created
         automatically.  NAME should be entered as a string.
NOTE:  This function is not intended for use on a microExplorer system."
;;** Note: The following are for POSSIBLE future enhancements.
;;monitor - What type of monitor will the screen be displayed on.  Valid
;;            types are :monochrome and :color.
;;number-of-planes - 1 for mono, 8 for full color, and 7 for color used in
;;                       conjuction with a 1 bit screen in dual monitor mode.
;;with-who-line? - Without a who-line, the screen will occupy the entire
;;                     space of the monitor.  Helps in setting height of screen.
;;sib-number - For POSSIBLE future multiple sib/csib support.  This should
;;               always be 0 (zero) until then."
;;monitors - # of monitors in use.
;;expose-it? - Whether or not to expose the newly created screen on the
;;              monitor specified."
  (DECLARE (SPECIAL previously-selected-windows)) ;; may 03/23/89 
  (UNLESS (mac-system-p)
    ;; Be sure that the color-monitor cables are hooked up and hardware is working on CSIB.
    (ASSERT (OR (NOT sib-is-csib)						;; may 03/27/89 
		*OK-TO-EXPOSE-COLOR-SCREENS-WHEN-COLOR-PRESENCE-TEST-FAILED*	;; may 03/27/89 
		(COLOR-MONITOR-PRESENT-ON-CSIB-P))				;; may 03/27/89 
	    (*OK-TO-EXPOSE-COLOR-SCREENS-WHEN-COLOR-PRESENCE-TEST-FAILED*)	;; may 03/27/89 
	    "Color monitor does NOT seem to be present and ~s is nil."		;; may 03/27/89 
	    '*OK-TO-EXPOSE-COLOR-SCREENS-WHEN-COLOR-PRESENCE-TEST-FAILED*)	;; may 03/27/89 
    (LET ((screen nil)
	  (listener nil)
	  (save-psw (MAKE-ARRAY 20.))
;; >>> Note, these are for POSSIBLE future enhancements.  Bind them to reasonable values
;; for now.  04/22/88 KJF.
	  (monitor (IF sib-is-csib :color :monochrome))
	  (number-of-bit-planes (IF sib-is-csib 8. 1.))
	  (with-who-line? t)
	  (sib-number *default-sib*))
      (WHEN (AND color? (NOT sib-is-csib))   ;; (EQ monitor :color))
	(ERROR "Cannot create color screen without a CSIB."))
      (WHEN (AND color? (NOT (EQ monitor :color)))
	(ERROR "Cannot create a color screen on a monochrome monitor."))
      (UNLESS color?  ;; If not color, force number-of-bit-planes to be 1.
	(SETQ number-of-bit-planes 1))
      (WHEN (AND (EQ monitor :monochrome) (> number-of-bit-planes 1))
	(ERROR "It's illegal to have a monochrome screen with multiple bit planes."))
      (WHEN name
	(DOLIST (a-screen all-the-screens)
	  (ASSERT (NOT (STRING-EQUAL name (SEND a-screen :name))) (name)
		  "Screen ~s is already using the name ~s.
SCREENS must have unique names.
Press RESUME to specify a different name for the screen being created.
NAME should be specified as a string." a-screen name)))
      ;; If MAIN-SCREEN hasn't had certain things set up for the first time, do so.
      (UNLESS (screen-screens-previously-selected-windows main-screen)
	(things-to-do-first-time))
      (WITHOUT-INTERRUPTS
	(with-mouse-usurped
	  (SETQ mouse-sheet nil)  ;; cannot do following :deexpose unless mouse-sheet nil
	  (SEND default-screen :deexpose)
	  ;; Do not alter previously-selected-windows of current screen.
	  ;; Will be copied back below.
	  (COPY-ARRAY-CONTENTS previously-selected-windows save-psw)
	  (SETQ screen (define-screen 'standard-screen name
			 :buffer (IF sib-is-csib
				     (IF color?
					 CSIB-COLOR-NO-TRANSP-VA
					 CSIB-EXPANS-NO-TRANSP-VA)
				     IO-SPACE-VIRTUAL-ADDRESS)
			 :control-address MAIN-SCREEN-CONTROL-ADDRESS
			 :property-list
			 `(:video ,(IF color? :color :black-and-white)
				  :controller :simple
				  :display-type ,monitor
				  :logical-bits-per-pixel ,number-of-bit-planes
				  :sib-number ,sib-number)
			 :screens-who-line-screen
			 (WHEN with-who-line?
			   (IF color?
			       (OR *color-who-line*
				   (SETQ *color-who-line*
					 ;; Give it an unique name.  If try to use same name as that
					 ;; specified for screen, error occurs from DEFINE-SCREEN.
					 (make-who-line-screen "Color Who Line Screen"
							       number-of-bit-planes monitor)))
			       (OR *mono-who-line*
				   (SETQ *mono-who-line*
					 ;; Give it an unique name.  If try to use same name as that
					 ;; specified for screen, error occurs from DEFINE-SCREEN.
					 (make-who-line-screen "Mono Who Line Screen"
							       number-of-bit-planes monitor)))))
			 :bits-per-pixel (IF color? 8. 1.)
			 :locations-per-line (IF color? 256. 32.)
			 :height (- MAIN-SCREEN-HEIGHT (IF with-who-line?
							   ;; Really want who-line-screen here?
							   (sheet-height who-line-screen)
							   0))
			 :width MAIN-SCREEN-WIDTH))
	  (SETQ listener
		(make-instance 'w:lisp-listener :superior screen
			       :expose-p t)) ;; Must do this to make it an inferior of screen.
	  (LET ((a (MAKE-ARRAY 20.)))
	    (SETF (aref a 0) listener)
	    (SEND screen :set-screens-previously-selected-windows a))
      ;; Do not alter previously-selected-windows of current screen.
      (COPY-ARRAY-CONTENTS save-psw previously-selected-windows)
      ;; It's O.K. to do this more than once, eventhough we'd like not to.
      ;; We could test for if already-there, but the function does it for us.  04/28/88 KJF.
      (add-to-system-menu-column
	:windows "Select Screen"
	'(SYSTEM-MENU-SELECT-screen) "Select a screen from a menu." :sorted)
      (SEND screen :expose))) ;;(next-screen screen)
      ;; When finally come back to "old" screen, this return value will print out.
      ;; Would rather have it print out on new screen.
      screen)))

;;; ********* Attention ! *********
;;; This function is redefined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;
;; This could go away when a source build was done if when MAIN-SCREEN is first created,
;; the TV:MAKE-A-SCREEN function was used.
(DEFUN things-to-do-first-time ()
  "Some things to do the first time another screen gets created.  This should happen
only ONCE."
  (DECLARE (SPECIAL previously-selected-windows)) ;; may 03/23/89 
;;  (SEND main-screen :set-screens-who-line-screen who-line-screen)
  (IF (color-sheet-p who-line-screen)
      (SETQ *color-who-line* who-line-screen)
      (SETQ *mono-who-line* who-line-screen))
  (SEND main-screen :set-screens-previously-selected-windows
	previously-selected-windows))

;; may 01/27/89 from patch window.4.109
(DEFUN make-who-line-screen
       (name logical-bits-per-pixel display-type &optional (sib-number *default-sib*))
  "Make a who-line-screen of the desired type."
  (LET* ((color? (> logical-bits-per-pixel 1))
	 ;; For multiple-monitor support, get frame buffer from optionally specified monitor-controller instance...
	 ;; CJJ  06/07/88.
	 ;;; Added by KJF on 08/19/88 for CJJ during addition of Multiple Monitor (MMON) support.
	 (frame-buffer (IF (mmon-p)
			   (SEND (AREF *all-the-monitor-controllers* sib-number)
				 :frame-buffer-for (IF color? 8. 1.))
			   (IF sib-is-csib
			       (IF color?
				   CSIB-COLOR-NO-TRANSP-VA
				   CSIB-EXPANS-NO-TRANSP-VA)
			       IO-SPACE-VIRTUAL-ADDRESS))))
    (IF frame-buffer
	(WITHOUT-INTERRUPTS
	  ;; Shadow some global variables...
	  (LET ((MAIN-SCREEN-BUFFER-ADDRESS frame-buffer)
		WHO-LINE-SCREEN
		WHO-LINE-DOCUMENTATION-WINDOW
		NWATCH-WHO-LINE-SHEET
		USER-ID-WHO-LINE-SHEET
		CURRENT-PACKAGE-WHO-LINE-SHEET
		WHO-LINE-RUN-STATE-SHEET
		WHO-LINE-FILE-STATE-SHEET)
	    (who-line-setup name color? logical-bits-per-pixel display-type sib-number)
	    who-line-screen))
	;; ELSE...
	(FERROR 'w:no-frame-buffer
		"Monitor controller ~A does not support ~A bits per pixel."
		sib-number
		logical-bits-per-pixel))))

;;; This was an old way to switch between screens.  Now, use (send SCREEN :expose).
;;; See :around :expose method of STANDARD-SCREEN.
;(COMMENT
;;; >> This should only be used for switching screens when there are multiple screens
;;; on a single monitor.  For the dual monitor case, another approach may be desired.
;;; >>> For now, we'll do some things to help when in the dual-monitor mode, at least
;;; untill the mouse-transport feature is ready.
;(DEFUN next-screen (to-screen)
;  "Switch to another screen."
;  (add-to-previously-selected-screens default-screen)
;  (remove-from-previously-selected-screens to-screen)
;  (SETQ *current-screens* nil)
;  (LET ((display-who-line? nil))
;    (sheet-open-blinkers default-screen)  ;;;>>> may only want to do this if in dual mode
;    (WITHOUT-INTERRUPTS
;      (with-mouse-usurped
;	;; Make sure the current window gets put on the PREVIOUSLY-SELECTED-WINDOWS list
;	;; for that screen.
;	(WHEN (AND *dual-monitors* selected-window)
;	  (SEND (SEND selected-window :alias-for-selected-windows)
;		:deselect nil))  ;; nil says to NOT select previous window.
;	(UNLESS *dual-monitors*
;	  (SETQ mouse-sheet nil)
;	  (SEND default-screen :deexpose)) ;; for single monitor, deexpose will :deselect nil.
;	(SETF default-screen to-screen)
;	;; Note, this sets previously-selected-windows to a different array.  Is this O.K.??
;	(SETF previously-selected-windows
;	      (SEND default-screen :screens-previously-selected-windows))
;	(SETQ *color-system* (color-sheet-p default-screen))
;	;; Not sure if this is really necessary.
;	(SETQ main-screen-buffer-address
;	      (IF *color-system* csib-color-no-transp-va csib-expans-no-transp-va))
;	(PUSH default-screen *current-screens*)
;	(IF (SEND default-screen :screens-who-line-screen)
;	    (PROGN
;	      (SEND who-line-screen :deexpose) ;; May not want to do this if single screen.
;	      (SETQ who-line-screen (SEND default-screen :screens-who-line-screen))
;	      (PUSH who-line-screen *current-screens*)
;	      (reset-who-line-variables)
;	      (SETQ display-who-line? t))
;	    (PROGN
;	      (SETQ who-line-screen nil)
;	      (SETQ inhibit-who-line t)))
;	(SETQ mouse-sheet default-screen) ;; mouse-sheet must not be nil for :expose
;	(SEND default-screen :expose) ;; Really only need to do this for single screen.
;	(mouse-set-sheet default-screen)
;	(sheet-open-blinkers default-screen) ;; May only want to do this if in dual mode.
;	(WHEN display-who-line?
;	  (SEND who-line-screen :expose)  ;; Must expose who-line !!
;	  (SEND who-line-screen :refresh)
;	  (SETQ inhibit-who-line nil))
;	;; It seems that, for some reason, if switching back from a color screen to the
;	;; initial-lisp-listener (one which contains suggestion menu bar at bottom), a window
;	;; has already been selected.  If it's a listener without that menu bar (or ZMACS or ...)
;	;; then selected-window is nil when switching.
;	;; May be in SCREEN-MANAGE-AUTOEXPOSE-INFERIORS
;	(IF selected-window
;	    (SEND selected-window :select)
;	    (send (AREF previously-selected-windows 0) :select))
;	(SETQ *screens-to-refresh* nil)
;	(WHEN who-line-screen (PUSH who-line-screen *screens-to-refresh*))
;	(PUSH default-screen *screens-to-refresh*)
;	;; If *color-sprite-window* is not NIL, then we don't need to do this again.
;	(WHEN (AND (color-sheet-p default-screen) (NOT *color-sprite-window*))
;	  (make-color-sprite-window default-screen))
;	(update-global-window-variables (color-sheet-p default-screen))
;	default-screen  ;; Return this.
;	))))
; )

(DEFUN reset-who-line-variables ()
  "Reset these global variables for the current who-line-screen."
  ;; CAREFUL, the name strings must match exactly!
  (LET ((inferiors (send who-line-screen :inferiors)))
    (SETF WHO-LINE-DOCUMENTATION-WINDOW
	    (find "Documentation Window" inferiors :key 'sheet-name :test 'equal)
	  NWATCH-WHO-LINE-SHEET
	    (find "Nwatch" inferiors :key 'sheet-name :test 'equal)
	  USER-ID-WHO-LINE-SHEET
	    (find "User Id" inferiors :key 'sheet-name :test 'equal)
	  CURRENT-PACKAGE-WHO-LINE-SHEET
	    (find "Current Package" inferiors :key 'sheet-name :test 'equal)
	  WHO-LINE-RUN-STATE-SHEET
	    (find "Run State" inferiors :key 'sheet-name :test 'equal)
	  WHO-LINE-FILE-STATE-SHEET
	    (find "File State" inferiors :key 'sheet-name :test 'equal))))

(DEFUN make-color-sprite-window (superior)
  "Make a color version of w:sprite-window."
  (DECLARE (SPECIAL w:sprite-window)) ;; may 03/23/89 
  (UNLESS *mono-sprite-window*
    (SETQ *mono-sprite-window* w:sprite-window))
  (UNLESS *color-sprite-window*
    (SETQ *color-sprite-window* (MAKE-INSTANCE 'w:sprite-window :superior superior))))

(DEFUN update-global-window-variables (color?)
  "Setup globals based on if we're on a color screen or not.  Called when
switching between screens."
  (DECLARE (SPECIAL w:sprite-window w:cache-window)) ;; may 03/23/89 
  (IF color?
      (PROGN
	(SETQ w:sprite-window *color-sprite-window*)
	(SETF (sheet-locations-per-line w:cache-window) 256.)
	(SEND w:cache-window :set-erase-aluf w:alu-setz)
	(SETQ w:alu-alist w:color-alu-alist)
	(SETQ w:color-alist w:color-color-alist))
      ;; else formS
      ;; If *mono-sprite-window* is NIL, no color screens exist.  Thus we know we are
      ;; not switching from a color screen.  So there's no reason to change things, as
      ;; they should already be set up for monochrome.
      (WHEN *mono-sprite-window*
	(SETQ w:sprite-window *mono-sprite-window*)
	(SETF (sheet-locations-per-line w:cache-window) 32.)
	(SEND w:cache-window :set-erase-aluf w:alu-andca)
	(SETQ w:alu-alist w:B&W-alu-alist)
	(SETQ w:color-alist w:B&W-color-alist))))

(DEFUN create-color-screen (&optional name)
  "Create a color screen, expose and select it.  NAME should be
different for each screen created.  Passing NIL for name means
an unique name will automatically be created.
NOTE:  This function is not intended for use on a microExplorer system."
  (unless (mac-system-p)
    (LET* ((screen (make-a-screen :name name))
	   (listener (CAR (SEND screen :inferiors))))
      (UNLESS *color-screen*
	(SETQ *color-screen* screen)
	(SETQ *initial-color-lisp-listener* listener))
      screen)))

;; may 01/27/89 
(DEFUN move-sheet-to-another-screen (sheet to-screen
				    &optional (convert-color-to-monochrome
						*convert-color-sheet-to-monochrome*))
  "Move a sheet to a different screen.  This will make sheet an inferior of
TO-SCREEN.  If SHEET is monochrome and TO-SCREEN is color, SHEET will
be converted to color.  If SHEET is color and TO-SCREEN is monochrome, and
convert-color-to-monochrome is set, then SHEET will be converted to monochrome.
If convert-color-to-monochrome is not set, an error will occur."
  (WHEN (color-sheet-p to-screen)
    ;; Check monitor-controller in question instead of SIB-IS-CSIB.  CJJ 06/10/88.
    ;; Added by KJF on 08/20/88 for CJJ during addition of Multiple Monitor (MMON) support.
    (IF (mmon-p)
	(UNLESS (TYPEP (AREF *all-the-monitor-controllers*
			     (OR (SEND to-screen :sib-number) 0))
		       'csib)
	  (ERROR "~s is a color screen, but it is not mapped to a CSIB." to-screen))
	(UNLESS sib-is-csib
	  (ERROR "~s is a color screen, but there is no CSIB in your system." to-screen))))
  (ASSERT sheet (sheet) "MONO-SHEET should not be NIL.")
  (ASSERT to-screen (to-screen) "COLOR-SCREEN should not be NIL.")
  (WHEN (EQ sheet selected-window)
    (SEND sheet :deselect))
  ;; If the above :deselect did not remove it, make sure it gets removed!
  ;; Put this window in the correct place, in case it's not on DEFAULT-SCREEN.  CJJ 06/10/88.
  (with-screens-previously-selected-windows (sheet)
    (remove-from-previously-selected-windows sheet))
  ;; :set-superior will do conversion, of sheet and all inferiors, if necessary.  A conversion
  ;; from color to monochrome will only occur if explicitly specified.
  (LET ((*convert-color-sheet-to-monochrome* convert-color-to-monochrome))
    (SEND sheet :set-superior to-screen))
  ;;  ;; If screen is color but sheet is not, convert sheet to color.
  ;;  (WHEN (AND (color-sheet-p to-screen) (NOT (color-sheet-p sheet)))
  ;;    (convert-sheet-and-inferiors-to-color sheet))
  (SEND to-screen :expose)
  (SEND sheet :send-if-handles :select)
  (refresh-all sheet))

;; may 01/27/89 
(DEFUN kill-screen (screen)
  "Blast screen and anything on it.  Removes it from the known
world.  Frees any memory it was using, we hope."
  (WHEN screen
    (WHEN (sheet-exposed-p screen)
      (ERROR "Cannot kill an exposed screen.  Select some other screen first."))
    (DOLIST (inf (SEND screen :inferiors))
      (SEND inf :kill))
    ;; Remove pointers from monitor instances and list of disk-saved screens.  CJJ 06/08/88.
    ;;; Added by KJF on 08/20/88 for CJJ during addition of Multiple Monitor (MMON) support.
    (WHEN (mmon-p)
      (DOLIST (monitor (APPEND *all-the-monitors* *unconfigured-monitors*))
	(SEND monitor :send-if-handles :set-currently-exposed-screens
	      (DELETE screen (SEND monitor :send-if-handles :currently-exposed-screens) :test #'EQ))
	(SEND monitor :send-if-handles :set-previously-exposed-screens
	      (DELETE screen (SEND monitor :send-if-handles :previously-exposed-screens) :test #'EQ))))
    (SETF *screens-exposed-at-disk-save*
	  (DELETE screen *screens-exposed-at-disk-save* :test #'EQ))
    (SEND screen :kill)
    ;; The :kill above invokes (SHEET :DEACTIVATE) which does the following : ;; may 03/09/89 
    ;;(SETQ all-the-screens (REMOVE screen all-the-screens))
    (SETQ *screens-to-refresh* (REMOVE screen *screens-to-refresh*))
    (remove-from-previously-selected-screens screen)
    (BEEP)
    (FORMAT t "~%** Make sure to SETQ all variables pointing to this screen to NIL
so that memory can be garbage collected. **")))

;;; >>> Need to complete mods to allow specifying screens to use and leave others alone.
(DEFUN switch-to-dual (&optional monochrome-screen color-screen)
  (WHEN *dual-monitors*
    (CERROR "Proceed anyway." "Already in dual monitor mode."))
  (SETQ *dual-monitors* t)
  (WHEN (AND monochrome-screen (color-sheet-p monochrome-screen))
    (ERROR "~s is not a monochrome screen." monochrome-screen))
  (WHEN (AND color-screen (NOT (color-sheet-p monochrome-screen)))
    (ERROR "~s is not a color screen." color-screen))
  (setup-mono-plane *default-dual-monitor-monochrome-plane*)  ;; Export this from TV ??
  (DOLIST (screen all-the-screens)
    (SEND screen :setup-property-list (IF (color-sheet-p screen) 7 1)
	  (IF (color-sheet-p screen) :color :monochrome) *default-sib*)
    (SETF (sheet-plane-mask screen)
	  (SEND screen :determine-plane-mask-from-property-list)))
  (kbd-screen-redisplay-some)
  (kbd-switch-screens nil)
  (kbd-switch-screens nil)
  (BEEP)
  (sheet-home *terminal-io*)
  (FORMAT t "Now in dual monitor mode."))

;;; >>> For single mode, right thing may be to force all to have plane masks of 255.
(DEFUN switch-to-single ()
  (UNLESS *dual-monitors*
    (CERROR "Proceed anyway." "Already in single monitor mode."))
  (SETQ *dual-monitors* nil)
  (setup-mono-plane *default-monochrome-plane*)  ;; Export this from TV ??
  (DOLIST (screen all-the-screens)
    (SEND screen :setup-property-list
	  (IF (color-sheet-p screen) 8 1) :color *default-sib*)
    (SETF (sheet-plane-mask screen) (SEND screen :determine-plane-mask-from-property-list))
    (WHEN (TYPEP screen 'standard-screen)
      (UNLESS (EQ screen default-screen)
	(SEND screen :deexpose))))
  (kbd-screen-redisplay)
  (BEEP)
  (sheet-home *terminal-io*)
  (FORMAT t "Now in single monitor mode."))

(DEFUN kbd-screen-redisplay-some ()
  "Like KBD-SCREEN-REDISPLAY,  but allows only *screens-to-refresh* to be refreshed.
This was added mainly for dual/multiple monitor users.  The intent is that
*screens-to-refresh* will only be the default-screen and current who-line-screen.
However, it can be changed at will."
  (DOLIST (screen *screens-to-refresh*)
    (COND ((sheet-exposed-p screen)
	   (DOLIST (i (sheet-exposed-inferiors screen))
	     (AND (sheet-can-get-lock i)
		  (SEND i :refresh)))
	   (SEND screen :screen-manage)))
    ;; Inform who-line's that they must redisplay completely.  See WHO-LINE-CLOBBERED.
    (WHEN (TYPEP screen 'who-line-screen)
      (DOLIST (I (SHEET-INFERIORS screen))
	(AND (TYPEP I 'WHO-LINE-MIXIN) (FUNCALL I :CLOBBERED))))))

(DEFUN return-matching-who-line-screen (screen)
  (IF (color-sheet-p screen)
      *color-who-line*
      *mono-who-line*))

(DEFUN dual-monitor-p (screen)
  "Determines if screen is being used in Dual Monitor mode.  It does this
by checking the plane-mask of SCREEN."
  (NOT (EQL (SEND screen :plane-mask) 255.)))


;; The following code implements the TERM-CTRL-S key sequence.
;; This allows rotating through and exposing/selecting screens.

(DEFUN setup-previously-selected-screens-array ()
  "Add all standard-screens from tv:all-the-screens to
*PREVIOUSLY-SELECTED-SCREENS* array."
  (DOLIST (screen all-the-screens)
    (WHEN (TYPEP screen 'standard-screen)
      (ADD-TO-PREVIOUSLY-SELECTED-SCREENS screen))))

(DEFUN ADD-TO-PREVIOUSLY-SELECTED-SCREENS (SCREEN &OPTIONAL AT-END)
  "Add a screen to the *PREVIOUSLY-SELECTED-SCREENS* array."
  (WITHOUT-INTERRUPTS
    (AND SCREEN (REMOVE-FROM-PREVIOUSLY-SELECTED-SCREENS SCREEN))
    (DO ((I 0 (1+ I))
	 (N (ARRAY-TOTAL-SIZE *PREVIOUSLY-SELECTED-SCREENS*)))
	((OR (NULL SCREEN) (= I N))
	 (COND
	   (SCREEN
	    (SETQ *PREVIOUSLY-SELECTED-SCREENS*
		  (ADJUST-ARRAY *PREVIOUSLY-SELECTED-SCREENS* (+ N 10)))
	    (SETF (AREF *PREVIOUSLY-SELECTED-SCREENS* N) SCREEN))))
      (LET ((TEM (AREF *PREVIOUSLY-SELECTED-SCREENS* I)))
	(COND
	  ((OR (NOT AT-END) (NULL TEM))
	   (SETF (AREF *PREVIOUSLY-SELECTED-SCREENS* I) SCREEN)
	   (SETQ SCREEN TEM)))))
    ()))

(DEFUN REMOVE-FROM-PREVIOUSLY-SELECTED-SCREENS (SCREEN)
  "Remove a screen from the *PREVIOUSLY-SELECTED-SCREENS* array."
  (WITHOUT-INTERRUPTS
    (OR (NULL SCREEN)
	(DO ((I 0 (1+ I))
	     (N (ARRAY-TOTAL-SIZE *PREVIOUSLY-SELECTED-SCREENS*)))
	    ((= I N) (NOT SCREEN))
	  (COND
	    ((EQ (AREF *PREVIOUSLY-SELECTED-SCREENS* I) SCREEN)
	     (COND
	       ((NULL SCREEN)
		(SETF (AREF *PREVIOUSLY-SELECTED-SCREENS* (1- I)) ()) (RETURN T)))
	     (SETF (AREF *PREVIOUSLY-SELECTED-SCREENS* I) ())
	     (SETQ SCREEN ()))
	    ((NULL SCREEN)
             (SETF (AREF *PREVIOUSLY-SELECTED-SCREENS* (1- I))
                   (AREF *PREVIOUSLY-SELECTED-SCREENS* I))))))))

;; may 01/27/89 
;; >> May want to change this to NOT remove non-exposable screens.
(DEFUN kbd-switch-screens (arg &aux tem)	   ;TERM-CTRL-S
  "Select a screen to switch to.  Functions like TERM-S for windows.
Press TERM-HELP for more documentation."
  ;; TERM n S rotates the n most recently selected screens, selecting the nth
  ;; TERM S = TERM 2 S
  ;; TERM 1 S selects the next most recent screen but rotates all the screens
  ;; TERM -n S rotates the same set of screens in the other direction
  (OR arg (SETQ arg 2))
  ;; If there are none yet, don't try to switch.
  (WHEN (AREF *previously-selected-screens* 0)
    ;; Put current screen on front of array.
    (add-to-previously-selected-screens default-screen)
    (WITHOUT-INTERRUPTS			   ;Get rid of any non-exposable ones
      (DOTIMES (i (ARRAY-TOTAL-SIZE *previously-selected-screens*))
	(OR (SETQ tem (AREF *previously-selected-screens* i)) (RETURN))
	(COND ((NOT (w:screen-exposable-p tem))
	       (remove-from-previously-selected-screens tem)
	       (SETQ i (1- i)))))
      (rotate-top-of-array *previously-selected-screens* arg))
    (WHEN (SETQ tem (AREF *previously-selected-screens* 0))
      (IF (mac-screen-p tem)
	  (select-a-screen tem)
	(SEND tem :expose))
           ;; Make it DEFAULT-SCREEN in addition to exposing it.  It may already be exposed.  CJJ 06/15/88.
           ;;; Added by KJF on 08/20/88 for CJJ during addition of Multiple Monitor (MMON) support.
      (SEND tem :send-if-handles :make-default-screen)))
  tem)

;;; ********* Attention ! *********
;;; This function is redefined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;
(DEFMETHOD (standard-screen :around :expose) (cont mt arg-list)
;;; For switching from one screen to another.  04/23/88 KJF.
  (DECLARE (SPECIAL previously-selected-windows)) ;; may 03/23/89 
  (unless (and (si:mp-system-p)
	       (si:cool-boot-p)
	       (not (funcall mp-displayp (logand #xf si:processor-slot-number))))
    (IF (AND (explorer-screen-p self) *do-extra-things-on-screen-expose*)
	(PROGN
	  ;; w:screen-exposable-p always returns t on microExplorer
	  (UNLESS (w:screen-exposable-p self)
	    (ERROR "~s cannot be exposed on current hardware configuration." self))
	  (WITHOUT-INTERRUPTS
	    (with-mouse-usurped
	      ;; Only bother doing this if screen was created in normal way.  That is, using
	      ;; tv:make-a-screen or tv:create-color-screen.  04/26/88 KJF
;;      (WHEN (explorer-screen-p self)
	      ;; This prevents certain things from happening when a screen is first created.
	      ;; See DEFINE-SCREEN.
;;	(WHEN *do-extra-things-on-screen-expose*
	      ;; If default-screen is nil when some screen is being exposed, no deexposing will occur.  This
	      ;; helps out during window initialization (booting).
	      (WHEN default-screen
		(sheet-open-blinkers default-screen)  ;;;>>> may only want to do this if in dual mode
		;; Make sure the current window gets put on the PREVIOUSLY-SELECTED-WINDOWS array
		;; for that screen.
		(WHEN (AND (dual-monitor-p self) selected-window)
		  (SEND (SEND selected-window :alias-for-selected-windows)
			:deselect nil))  ;; nil says to NOT select previous window.
		(UNLESS (dual-monitor-p self)
		  (SETQ mouse-sheet nil)
		  ;; For single monitor, deexpose will :deselect nil, which will put window on
		  ;; PREVIOUSLY-SELECTED-WINDOWS
		  (SEND default-screen :deexpose)))
	      (remove-from-previously-selected-screens self)
	      (SETF default-screen self)
	      ;; Note, this sets previously-selected-windows to a different array.  Is this O.K.??
	      (WHEN (AND screens-previously-selected-windows
			 (TYPEP screens-previously-selected-windows 'array)) 
		(SETF previously-selected-windows screens-previously-selected-windows))
	      (SETQ *color-system* (color-sheet-p self))
	      ;; Not sure if this is really necessary.
	      (SETQ main-screen-buffer-address (IF sib-is-csib
						   (IF *color-system*
						       CSIB-COLOR-NO-TRANSP-VA
						       CSIB-EXPANS-NO-TRANSP-VA)
						   IO-SPACE-VIRTUAL-ADDRESS))
	      (SETQ mouse-sheet self) ;; mouse-sheet must not be nil for :expose
	      ;; screens-who-line-screen is a pointer to a who-line-screen which should be exposed when
	      ;; this screen is exposed.  It may point to a who-line-screen which is on a different monitor.
	      ;; It may be NIL, which would mean that no who-line-screen should be "active".
	      (IF screens-who-line-screen
		  (PROGN
		    (WHEN (NOT (EQ screens-who-line-screen who-line-screen))
		      (WHEN who-line-screen
			(SEND who-line-screen :deexpose)) ;; May not want to do this if single screen.
		      (SETQ who-line-screen screens-who-line-screen)
		      (reset-who-line-variables)
		      (SETQ inhibit-who-line nil) ;; may 03/09/89 Added
		      (SEND who-line-screen :expose)  ;; Must expose who-line !!
		      (SEND who-line-screen :refresh)))
		  (PROGN
		    (SETQ who-line-screen nil)
		    (SETQ inhibit-who-line t)))
;;	  ))
	      (LEXPR-FUNCALL-WITH-MAPPING-TABLE cont mt arg-list)
;;      (WHEN (explorer-screen-p self)
	      ;; This prevents certain things from happening when a screen is first created.
	      ;; See DEFINE-SCREEN.
;;	(WHEN *do-extra-things-on-screen-expose*
	      (mouse-set-sheet self)
	      ;; It seems that, for some reason, if switching back from a color screen to the
	      ;; initial-lisp-listener (one which contains suggestion menu bar at bottom), a window
	      ;; has already been selected.  If it's a listener without that menu bar (or ZMACS or ...)
	      ;; then selected-window is nil when switching.
	      ;; May be in SCREEN-MANAGE-AUTOEXPOSE-INFERIORS
	      (IF (AND selected-window (sheet-me-or-my-kid-p selected-window self))
		  (SEND selected-window :select)
		  ;; During window system build, this would be NIL.  07/18/88 KJF.
		  (WHEN (AREF previously-selected-windows 0)
		    (send (AREF previously-selected-windows 0) :send-if-handles :select)))
	      (SETQ *current-screens* nil)
	      (WHEN who-line-screen
		(PUSH who-line-screen *current-screens*))
	      (PUSH self *current-screens*)
	      ;; If switch to a color screen, make sure color inits have been run.
	      (WHEN (color-sheet-p self)
		(initializations '*CONVERT-TO-COLOR-INITIALIZATION-LIST*))
;; Maybe this creation of *color-sprite-window* really belongs on the
;; *CONVERT-TO-COLOR-INITIALIZATION-LIST*. ??
	      ;; If *color-sprite-window* is not NIL, then we don't need to do this again.
	      (WHEN (AND (color-sheet-p self) (NOT *color-sprite-window*))
		(make-color-sprite-window self))
	      (update-global-window-variables (color-sheet-p self))
	      self  ;; Return this ???
;;	  ))
	      )))
	;; else
	(LEXPR-FUNCALL-WITH-MAPPING-TABLE cont mt arg-list)
	)))

;;; ********* Attention ! *********
;;; This function is UN-defined by the MMON system.
;;; If any change is made here, be sure to update the MMON version also.
;;; ********* Attention ! *********
;;;
(DEFMETHOD (standard-screen :after :deexpose) (&rest ignore)
  "When a screen is deexposed, keep track of it in *previously-selected-screens*."
  ;; Only bother doing this if screen was created in normal way.  That is, using
  ;; tv:make-a-screen or tv:create-color-screen.  04/26/88 KJF
  (WHEN (explorer-screen-p self)
    (add-to-previously-selected-screens self)))

(DEFUN explorer-screen-p (screen)
  "Returns T if screen was created using standard Explorer functions:
w:make-a-screen or w:create-color-screen.  Only works for screens of
type 'standard-screen, not who-line-screens."
  (WHEN (TYPEP screen 'standard-screen)
    (TYPEP (screen-screens-previously-selected-windows screen) 'ARRAY)))
