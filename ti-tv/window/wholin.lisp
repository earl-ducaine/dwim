;;-*- Mode: COMMON-LISP; Package: TV; Base: 10.; Fonts: CPTFONT,HL12B,HL12BI -*-

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
;;; Copyright (C) 1983- 1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1981 Massachusetts Institute of Technology **

;;; Hairier who-line system

;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;    05/09/89 MAY    Changed who-line-setup to allow :both on a monochrome monitor.
;;;   04-19-89 DAB     Added nethods (WHO-LINE-FILE-SHEET DISPLAYED-PERCENT) and (WHO-LINE-FILE-SHEET DISPLAYED-count)
;;;    03/07/89 MAY	Changed who-line-setup to use  main-screen-control-address instead of #o377760.
;;;    02-27-89 JLM     	Changed WHO-LINE-SETUP and SET-NUMER-OF-WHO-... to use
;;;			MAIN-SCREEN-OFFSET-FOR-PROC for MP split screen mode.  Changed WHO-LINE-UPDATE to check 
;;;			 %run-bar-on to see if run bars are active. 
;;;  02-23-89  DAB     Changed who-line-file-steet to use global list for open-streams and file servers. The old methods
;;;                    did not work properly with mulitiple sheets such as in color systems. This changed also involved
;;;                    fixing window-mx;mac-wholin file. The mx function display-mac-who-line was using SYSMeval-in-instance
;;;                    to get the current-stream. This must be changed to use "send".
;;;   01/27/89  KJF      [may] More changes to who-line-setup.  These for Multiple Monitor (MMON) support.
;;;   10-07-88  BJ       Conditionalized the call to initialize-status-line in N-hour-clock-setup for MX.
;;;   09-09-88 MAY      Deleted columnar formatting of who-line doc strings for 3,4 or more who lines.
;;;   04/10/88 KJF       Additional change to who-line-setup for multiple screen support.
;;;   02/21/88 KJF       Changes to who-line-setup and initialize-status-line for multiple screen/dual monitor support.
;;;   02/15/88 KJF        Changed MAIN-SCREEN's to DEFAULT-SCREEN's in set-number-of-who-line-....
;;;   9/15/87   PMH       Modified WHO-LINE-UPDATE to control color of state bars at bottom of screen.
;;;   7/23/87   PMH       Modified WHO-LINE-FILE-SHEET-COMPUTE-CURRENT-STREAM and DISPLAY-FILE-TRANSFER
;;;                          to understand :IO stream directions. 
;;;   7/1/87    PMH       In who-line-documentation-function; inhibited-style-warnings around the use of
;;;                         EQ.  EQL is not accetable in this case.
;;;   3/23/87  TWE	Changed the value of MOUSE-HANDEDNESS to be a symbol in the keyword package
;;;			to get around TV/W package problems.  This should have been done in the first place.
;;;   3/20/87  DAN	Fixed double-hack of 1/02/87. parens were wrong in second-level WHEN.
;;;   3/12/87  TWE	Fixed set-number-of-who-line-documentation-lines to use the proper flavor of Lisp
;;;			Listener.  This fixes bug number 4082.
;;;   2/19/87  TWE	Corrected the font map for the WHO-LINE-DOCUMENTATION-WINDOW to be a list again.
;;;   2/18/87  TWE	Reworked the 12/24 hour clock code so that it would work when building the WS.
;;;   2/13/87  TWE	Fixed the 12/24-HOUR-CLOCK-SETUP functions to only update the status line and not
;;;			touch the mouse documentation window.  This not only makes it work better, but it is
;;;			much faster.  This fixes bug number 2103.
;;; 01/08/87     LS       Removed some temporary kludges I had added a few months ago.
;;;  1/08/87	TWE	Hacked away more at set-number-of-who-line-documentation-lines to get it to work.
;;; 01/06/87	KDB	Added a double-hack to prevent TYPEP error if ZMACS does not exist when
;;;			SET-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES 4 is called. See 1/2/87 description.
;;; 01/02/87	KDB	Added a work around fix to prevent system shutdown when user sets the
;;;			NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES from ZMACS. The fix involves selecting
;;;			a Listener if in ZMACS, changing the number of lines, and then returning to ZMACS.
;;; 12/31/86	KDB	Added a :NO-COMMA keyword to PROCESS-WHO-LINE-DOCUMENTATION-LIST. Also added
;;;			COMMA argument to DISPLAY-WHO-LINE-MOUSE-INFO. If COMMA is nil, no commas
;;;			are drawn between mouse documentation items.
;;; 12/22/86	TWE	Made several changes to make the build process cleaner.
;;;			Moved the DEFVARs for NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES and
;;;			WHO-LINE-DOCUMENTATION-LINE-HEIGHT closer to the beginning of this file so that they
;;;			wouldn't be referenced before being defined.
;;; 12/04/86	TWE	Changed references to copy-array-contents to use replace.
;;; 11/20/86     SLM     Added calls to Suggestions macros for SET-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES
;;; 11/19/86     TWE	Changed the DISPLAY-FILE-TRANSFER function to use :STRING-FOR-WHOLINE instead
;;;			of :STRING-FOR-PRINTING, as per a suggestion by BJ (Mr. Network).
;;; 11/18/86     LS	Changed purge-servers to be generic.
;;; 11/17/86     TWE	Fixed the who line package code to not just put out the `:' for the keyword package.
;;; 11/13/86     TWE	Entered fix for bug 1642 by fixing WHO-LINE-FILE-SHEET's :UPDATE method.
;;; 11/06/86     LS       Removed CHAOS dependencies using new generic connection objects and their methods.
;;; 10/30/86	TWE	Commented out the obsolete black screen code.
;;; 10/24/86	TWE	Fixed bug 819 by putting a ignore-error form around the throw to PAGE-OVERFLOW.
;;;			Also added more code in DISPLAY-WHO-LINE-MOUSE-INFO to handle :mouse-any.
;;; 10/20/86	TWE	Changed WHO-LINE-PACKAGE to use TYPEP properly.
;;; 09/02/86	KDB	Changed "any" string to "L,M,R" for :mouse-any keyword, per consistency guidelines.
;;; 08/04/86	TWE	Changed type checks for font to use the FONT-OBJECT-P function.
;;; 07/29/86	TWE	Changed to use Common Lisp functions.
;;; 07/29/86	TWE	Changed uses of CLOCK-TYPE to be :12-HOUR instead of '12-HOUR.
;;; 06/17/86	TWE	Fixed up WHO-LINE-SETUP to use the :INITIAL-ELEMENT option
;;;			to MAKE-ARRAY instead of using fillarray.
;;; 06/13/86	TWE	Gutted the methods/function which refered to the CHAOS
;;;			package.  The code is still there, but it simply returns nil.


(DEFVAR NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES DEFAULT-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES
   "Place to keep the number of lines in the mouse documentation line.") 

(DEFVAR WHO-LINE-DOCUMENTATION-LINE-HEIGHT ()
   "Place to keep the line height of the mouse documentation window.") 

(DEFFLAVOR WHO-LINE-SCREEN () (SCREEN))

(DEFMETHOD (WHO-LINE-SCREEN :USER-VISIBLE) () NIL)

(DEFFLAVOR WHO-LINE-MIXIN ((WHO-LINE-ITEM-STATE NIL)) ()
   ;; WHO-LINE-ITEM-STATE is NIL if the contents of the window
   ;; is unknown and needs to be redrawn.  If non-NIL it
   ;; represents the current contents, to avoid extra redisplay.
   (:INCLUDED-FLAVORS MINIMUM-WINDOW)
   (:DEFAULT-INIT-PLIST
     :MORE-P NIL
     :BLINKER-P NIL
     :FONT-MAP (LIST *STATUS-LINE-STANDARD-FONT*))
   (:REQUIRED-METHODS :UPDATE)
   (:SELECT-METHOD-ORDER :UPDATE)
   (:INIT-KEYWORDS :FLAVOR)
   :INITABLE-INSTANCE-VARIABLES
   :SETTABLE-INSTANCE-VARIABLES
   :GETTABLE-INSTANCE-VARIABLES)

(DEFWRAPPER (WHO-LINE-MIXIN :UPDATE) (IGNORE . BODY)
   `(WITHOUT-INTERRUPTS
     (AND (SHEET-CAN-GET-LOCK SELF)
	  (NOT (SHEET-OUTPUT-HELD-P SELF))
	  (PROGN . ,BODY))))

(DEFMETHOD (WHO-LINE-MIXIN :AFTER :REFRESH) (&OPTIONAL TYPE)
  (COND ((NOT (AND RESTORED-BITS-P (NEQ TYPE :SIZE-CHANGED)))
	 (SEND SELF :CLOBBERED)
	 (SEND SELF :UPDATE))))

;;; Should this actually do the updates here??
(DEFMETHOD (WHO-LINE-MIXIN :CLOBBERED) ()
  (SETQ WHO-LINE-ITEM-STATE NIL))

(DEFFLAVOR WHO-LINE-SHEET
	   ((WHO-LINE-UPDATE-FUNCTION NIL) (WHO-LINE-EXTRA-STATE NIL))
           (WHO-LINE-MIXIN MINIMUM-WINDOW)
  :INITABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (WHO-LINE-SHEET :BEFORE :INIT) (PLIST)
  (SETF (GET PLIST :NAME) (GET PLIST :WHO-LINE-UPDATE-FUNCTION)))

(DEFMETHOD (WHO-LINE-SHEET :UPDATE) ()
  (AND WHO-LINE-UPDATE-FUNCTION
       (FUNCALL WHO-LINE-UPDATE-FUNCTION SELF)))

;; Added optional args for specifying type/configuration of who-line.
;; Mainly for multiple screen/dual monitor support. 02/21/88 KJF.
;; Also added name arg.  DEFINE-SCREEN wants all screens to have different
;; name (maybe they must??), so allow unique names for who-lines.
;; If no name is specified, as is the case for the first who-line created (see
;; INITIALIZE function, default will be "Who Line Screen".  Thus, this does not
;; guarantee all who-lines wil have unique names.  04/06/88 KJF.
;; Also got rid of *color-system*.  Anyone calling this should now specify if they
;; want a color one.  04/09/88 KJF.
;; may 01/27/89 New who-line-setup from patch.4.109
(DEFUN who-line-setup
       (&optional
	(name "Who Line Screen")
	(color? (AND default-screen (color-system-p default-screen))) ;; was *color-system*  04/09/88 KJF.
	(logical-bits-per-pixel 8.)		;always set to 1. for monochrome.
	(display-type :monochrome)		;always set to :COLOR for color.
	;; Added for multiple-monitor (MMON) support.  CJJ 06/07/88.
	(sib-number *default-sib*))
  (unless who-line-screen
    (LET ((who-vsp who-line-vsp)
	  (who-line-doc-lines          default-number-of-who-line-documentation-lines)
	  (who-line-doc-font-height    (font-char-height *mouse-documentation-line-standard-font*))
	  (who-line-status-font-height (font-char-height *status-line-standard-font*))
	  (monitor-controller (WHEN (mmon-p) (AREF *all-the-monitor-controllers* sib-number))))
      ;; Added some checks.  CJJ 06/07/88.
      (IF color?
	  (PROGN
	    (SETF display-type :color)
	    (UNLESS (IF (mmon-p)
			(TYPEP monitor-controller 'csib)
			sib-is-csib)
	      (ERROR "Cannot create color screen without a CSIB."))
	    (UNLESS (OR (EQL logical-bits-per-pixel 7.)
			(EQL logical-bits-per-pixel 8.))
	      (ERROR "Number of bit planes must be 7 or 8 for a color screen.")))
	  ;; ELSE...
	  (SETF logical-bits-per-pixel 1.)
	  (UNLESS (MEMBER display-type '(:color :monochrome :both) :test #'EQ)
	    (ERROR "Display-type must be one of :COLOR, :MONOCHROME, or :BOTH."))
	  (WHEN (AND (eq display-type :color) ;(NOT (EQ display-type :monochrome)) ;; may 05/09/89 :both is also valid!
		     (IF (mmon-p)
			 (TYPEP monitor-controller 'sib)
			 (NOT sib-is-csib)))
	    (ERROR "A color monitor is NOT available on an SIB."))) ;; may 05/09/89 Reworded
      (SETQ who-line-screen
	    (define-screen 'who-line-screen name ;;"Who Line Screen"  04/09/88 KJF.
	      :area who-line-area
	      :default-font *status-line-standard-font*	;not *DEFAULT-FONT*
	      :buffer main-screen-buffer-address
	      :bits-per-pixel (IF color? 8. 1.)
	      :locations-per-line (IF color? 256. 32.)
	      :control-address main-screen-control-address ;; may 03/07/89 was #o377760
	      :property-list
	      `(
		:video ,(IF color? :color :black-and-white)
		:controller :simple
		:who-line t
		;; These next 2 are needed for multiple screen/dual monitor support.
		;; The combination of these 2 determines what plane mask the
		;; screen shall have, and how things behave in general.
		:display-type ,display-type
		;; logical-bits-per-pixel for a screen would be 7 if it were a color
		;; screen being used in dual-monitor mode.  02/21/88 KJF.
		:logical-bits-per-pixel ,logical-bits-per-pixel
		:sib-number ,sib-number)
	      :plane-mask (IF color?
			      (IF (EQL logical-bits-per-pixel 7.)
				  *default-dual-monitor-color-plane-mask*
				  *default-plane-mask*)
			      ;; ELSE...
			      (IF (EQ display-type :both)
				  *default-plane-mask*
				  ;; ELSE...
				  (IF (EQ display-type :monochrome)
				      *default-dual-monitor-monochrome-plane-mask*
				      ;; ELSE...
				      *default-dual-monitor-color-plane-mask*)))
	      :width main-screen-width
	      :height (+ who-vsp
			 (* who-line-doc-lines
			    (+ who-line-doc-font-height who-vsp))
			 who-vsp
			 who-line-status-font-height
			 who-vsp)
	      :vsp who-vsp
	      :y nil				;Force this to be calculated.
	      :bottom (+ main-screen-height main-screen-offset-for-proc)))	; jlm 2/27/89
      (initialize-status-line t)
      ;; Above the status line is at least one full line of mouse button documentation.
      (SETQ who-line-documentation-window
	    (who-line-field :flavor 'who-line-sheet
			    :name "Documentation Window" ;; added for multiple screen support. 02/17/88 KJF
			    :who-line-update-function 'who-line-documentation-function
			    :font-map (LIST *mouse-documentation-line-standard-font*)
			    :height (+ (* who-line-doc-lines (+ who-line-doc-font-height who-vsp))
				       who-vsp)
			    :vsp who-vsp
			    :top 0
			    :background-color *default-documentation-background*
			    ;; on a color system, we are going to color the documentation line, not reverse video it.
			    :reverse-video-p (NOT color?)))
      (SETQ who-line-documentation-line-height
	    (sheet-line-height who-line-documentation-window)))))

;;; The following function performs all that is necessary to set up the status line.  It is used
;;; by who-line-setup and the 12/24 hour clock updating functions.  The main idea here is to
;;; localize changes made to the dimensions of the windows that comprise the status line.
(DEFUN INITIALIZE-STATUS-LINE (&OPTIONAL (INITIAL-CREATION NIL))
  (LET ((CURRENT-CHARACTER-POSITION 0)
	(LAST-CHARACTER-POSITION    0)
        (STATUS-LINE-CHAR-WIDTH     (FONT-CHAR-WIDTH *STATUS-LINE-STANDARD-FONT*))
        (WHO-LINE-HEIGHT            (SHEET-HEIGHT      WHO-LINE-SCREEN))
        (WHO-LINE-LINE-HEIGHT       (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)))
    ;; 18 or 20 characters of the date and time.
    (SETQ LAST-CHARACTER-POSITION
          ;; The 12 hour clock needs 2 more characters than
          ;; the 24 hour version to show the AM/PM indicator.
          (+ 18 (IF (12-HOUR-CLOCK-P) 2. 0.)))
    (IF INITIAL-CREATION
        (SETQ NWATCH-WHO-LINE-SHEET
              (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
			      :name "Nwatch" ;; added for multiple screen support. 02/17/88 KJF
                              :VSP 0
                              :WHO-LINE-UPDATE-FUNCTION
                              (IF (12-HOUR-CLOCK-P)
                                  'NWATCH-WHO-FUNCTION-12
                                  'NWATCH-WHO-FUNCTION-24)
			      :background-color *default-status-background*
			      :foreground-color *default-status-foreground*
                              :HEIGHT WHO-LINE-LINE-HEIGHT
                              :LEFT 0
                              :RIGHT (* LAST-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
                              :BOTTOM WHO-LINE-HEIGHT))
        ;;ELSE
        (PROGN
          ;; Only update those things that can change.
          (SEND NWATCH-WHO-LINE-SHEET :SET-SIZE
                (* LAST-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
                WHO-LINE-LINE-HEIGHT)
          (SEND NWATCH-WHO-LINE-SHEET :SET-WHO-LINE-UPDATE-FUNCTION
                (IF (12-HOUR-CLOCK-P)
                    'NWATCH-WHO-FUNCTION-12
                    'NWATCH-WHO-FUNCTION-24))
          ;; We need to clear out the extra state so that it gets initialized
          ;; in the nwatch function to its proper value.
          (SEND NWATCH-WHO-LINE-SHEET :SET-WHO-LINE-EXTRA-STATE NIL)))
    (SETQ CURRENT-CHARACTER-POSITION LAST-CHARACTER-POSITION)
    (SETQ LAST-CHARACTER-POSITION (+ LAST-CHARACTER-POSITION 13.))
    ;; 13 characters of user id or process.
    (IF INITIAL-CREATION
        (SETQ USER-ID-WHO-LINE-SHEET
              (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
			      :name "User Id" ;; added for multiple screen support. 02/17/88 KJF
                              :VSP 0
                              :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-USER-OR-PROCESS
			      :background-color *default-status-background*
			      :foreground-color *default-status-foreground*
                              :HEIGHT WHO-LINE-LINE-HEIGHT
                              :LEFT (* CURRENT-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
                              :RIGHT (* LAST-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
                              :BOTTOM WHO-LINE-HEIGHT))
        ;;ELSE
        ;; Only the position can change; the size remains constant.
        (SEND USER-ID-WHO-LINE-SHEET :SET-POSITION
              (* CURRENT-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
              (- WHO-LINE-HEIGHT WHO-LINE-LINE-HEIGHT)))
    (SETQ CURRENT-CHARACTER-POSITION LAST-CHARACTER-POSITION)
    (SETQ LAST-CHARACTER-POSITION (+ LAST-CHARACTER-POSITION 10.))
    ;; 10 characters of package.
    (IF INITIAL-CREATION
        (SETQ CURRENT-PACKAGE-WHO-LINE-SHEET
              (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
			      :name "Current Package" ;; added for multiple screen support. 02/17/88 KJF
                              :VSP 0
                              :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-PACKAGE
			      :background-color *default-status-background*
			      :foreground-color *default-status-foreground*
                              :HEIGHT WHO-LINE-LINE-HEIGHT
                              :LEFT (* CURRENT-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
                              :RIGHT (* LAST-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
                              :BOTTOM WHO-LINE-HEIGHT))
        ;;ELSE
        ;; Only the position can change; the size remains constant.
        (SEND CURRENT-PACKAGE-WHO-LINE-SHEET :SET-POSITION 
              (* CURRENT-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
              (- WHO-LINE-HEIGHT WHO-LINE-LINE-HEIGHT)))
    (SETQ CURRENT-CHARACTER-POSITION LAST-CHARACTER-POSITION)
    (SETQ LAST-CHARACTER-POSITION
          (+ LAST-CHARACTER-POSITION 21.))
    ;; 21 characters of process state.
    (IF INITIAL-CREATION
        (SETQ WHO-LINE-RUN-STATE-SHEET
              (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
			      :name "Run State" ;; added for multiple screen support. 02/17/88 KJF
                              :VSP 0
                              :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-RUN-STATE
			      :background-color *default-status-background*
			      :foreground-color *default-status-foreground*
                              :LEFT (* CURRENT-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
                              :RIGHT (* LAST-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
                              :HEIGHT WHO-LINE-LINE-HEIGHT
                              :BOTTOM WHO-LINE-HEIGHT))
        ;;ELSE
        ;; Only the position can change; the size remains constant.
        (SEND WHO-LINE-RUN-STATE-SHEET :SET-POSITION 
              (* CURRENT-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
              (- WHO-LINE-HEIGHT WHO-LINE-LINE-HEIGHT)))
    (SETQ CURRENT-CHARACTER-POSITION LAST-CHARACTER-POSITION)
    (SETQ LAST-CHARACTER-POSITION (+ LAST-CHARACTER-POSITION 36.))
    ;; The remaining characters go to the file/idle/boot state.
    (IF INITIAL-CREATION
        (SETQ WHO-LINE-FILE-STATE-SHEET
              (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-FILE-SHEET
			      :name "File State" ;; added for multiple screen support. 02/17/88 KJF
                              :VSP 0
			      :background-color *default-status-background*
			      :foreground-color *default-status-foreground*
                              :LEFT (* CURRENT-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
                              :RIGHT MAIN-SCREEN-WIDTH
                              :HEIGHT WHO-LINE-LINE-HEIGHT
                              :BOTTOM WHO-LINE-HEIGHT))
        ;;ELSE
        ;; Only the left edge will change, but the only way
        ;; we can do that is to respecify all of the edges.
        (SEND WHO-LINE-FILE-STATE-SHEET :SET-EDGES
              (* CURRENT-CHARACTER-POSITION STATUS-LINE-CHAR-WIDTH)
              (- WHO-LINE-HEIGHT WHO-LINE-LINE-HEIGHT)
              MAIN-SCREEN-WIDTH
              WHO-LINE-HEIGHT))))

(DEFUN WHO-LINE-UPDATE (&OPTIONAL RUN-STATE-ONLY-P &AUX RL)
  (OR INHIBIT-WHO-LINE
      (NULL WHO-LINE-SCREEN)
      (when (AND (mac-system-p) (NOT si:*sib-present*) (FBOUNDP 'display-mac-who-line))
	  (display-mac-who-line))
      (WITHOUT-INTERRUPTS
	(SETQ RL (if (and (mac-system-p) (NOT si:*sib-present*))
		     si:*addin-run-indicator*
		     (unless (zerop (ldb #o1010 %run-bar-on))	; jlm 2/28/89
		       (SI::RUN-LIGHT-FOR-CHAPARRAL))))
	(IF RUN-STATE-ONLY-P
	 ;; The reason this is here is that this function conspires to do some
	 ;; minor nice things for you.
	  (AND WHO-LINE-RUN-STATE-SHEET
	       (FUNCALL WHO-LINE-RUN-STATE-SHEET :UPDATE))
	  (DOLIST (I (SHEET-EXPOSED-INFERIORS WHO-LINE-SCREEN))
	    (AND (TYPEP I 'WHO-LINE-MIXIN)
		 (FUNCALL I :UPDATE))))
	(COND ((and (mac-system-p)(NOT si:*sib-present*))
	       (SETQ si:*addin-run-indicator* rl))
	      ((AND tv:sib-is-csib (boundp 'si:%run-bar-on))  ;; GRH 7/88
	       (si:csib-set-run-state RL))
	      (t (unless (zerop (ldb #o1010 %run-bar-on))		; jlm 2/28/89
		   (SETF (SI::RUN-LIGHT-FOR-CHAPARRAL) RL))))))
  T)

(DEFUN WHO-LINE-CLOBBERED ()
  "Inform the who-line that it must redisplay completely."
  (AND WHO-LINE-SCREEN
     (DOLIST (I (SHEET-INFERIORS WHO-LINE-SCREEN))
       (AND (TYPEP I 'WHO-LINE-MIXIN) (FUNCALL I :CLOBBERED)))))

(DEFUN WHO-LINE-STRING (WHO-SHEET NEW-STRING)
  "Output NEW-STRING on WHO-SHEET, a part of the who line, if it
has changed.  The last value is remembered in th
WHO-LINE-ITEM-STATE instance variable."
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (SETQ NEW-STRING (STRING NEW-STRING))
  (COND ((NEQ WHO-LINE-ITEM-STATE NEW-STRING)
	 (PREPARE-SHEET (WHO-SHEET)
	   (SHEET-CLEAR WHO-SHEET)
	   (SHEET-STRING-OUT WHO-SHEET NEW-STRING
			     0 (MIN (ARRAY-ACTIVE-LENGTH NEW-STRING)
				    (TRUNCATE (SHEET-INSIDE-WIDTH WHO-SHEET)
					      (SHEET-CHAR-WIDTH   WHO-SHEET)))))
	 (SETQ WHO-LINE-ITEM-STATE NEW-STRING))))

;;(DEFUN WHO-LINE-USER-OR-PROCESS (WHO-SHEET)
;;  (WHO-LINE-STRING WHO-SHEET (IF WHO-LINE-PROCESS (PROCESS-NAME WHO-LINE-PROCESS) USER-ID)))

(DEFUN WHO-LINE-USER-OR-PROCESS (WHO-SHEET)
  (WHO-LINE-STRING WHO-SHEET USER-ID))

(DEFUN WHO-LINE-RUN-STATE (WHO-SHEET)
  (WHO-LINE-STRING WHO-SHEET WHO-LINE-RUN-STATE))

(DEFUN WHO-LINE-PACKAGE (WHO-SHEET &AUX VAL SG)
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (LET ((PKG (COND
	       ((SETQ LAST-WHO-LINE-PROCESS (OR WHO-LINE-PROCESS
						(AND
						  SELECTED-IO-BUFFER
						  (IO-BUFFER-LAST-OUTPUT-PROCESS
						    SELECTED-IO-BUFFER))))
		(SETQ SG (PROCESS-STACK-GROUP LAST-WHO-LINE-PROCESS))
		(COND ((EQ SG %CURRENT-STACK-GROUP) *PACKAGE*)
		      ((TYPEP SG 'STACK-GROUP)
		       (SYMEVAL-IN-STACK-GROUP '*PACKAGE* SG))
		      (T *PACKAGE*))))))
    (COND ((AND PKG (ARRAYP PKG)
		(NEQ WHO-LINE-ITEM-STATE PKG))
       (PREPARE-SHEET (WHO-SHEET)
	 (SHEET-CLEAR WHO-SHEET)
	 (SETQ VAL (SI:PKG-SHORTEST-NAME PKG))
         (WHEN (ZEROP (LENGTH VAL))
               ;; Handle the keyword package.
            (SETQ VAL (PACKAGE-NAME PKG)))
	 (SHEET-STRING-OUT
	   WHO-SHEET VAL
	   0 (MIN (LENGTH VAL)
		  (1- (TRUNCATE (SHEET-INSIDE-WIDTH WHO-SHEET)
				(SHEET-CHAR-WIDTH WHO-SHEET))))))
       (SHEET-TYO WHO-SHEET #\:)
       (SETQ WHO-LINE-ITEM-STATE PKG)))))

(DEFPARAMETER mac-who-string
	      (MAKE-ARRAY si: %MX-Who-State-String-Max :type 'art-8b
			  :displaced-to-physical-address
			  (DPB si:*addin-memory-slot*
			       si:%%Nubus-F-And-Slot-Bits
			       (+ si:%Driver-Data-Start si:%DD-who-state))))

(DEFUN WHO-LINE-RUN-STATE-UPDATE
       (&AUX P)			;Separate variable since other can be setq'ed
				; asynchronously by other processes.
  (SETQ LAST-WHO-LINE-PROCESS
	(SETQ P (OR WHO-LINE-PROCESS
		 (PROGN (AND (NULL SELECTED-IO-BUFFER)
			     (NOT (NULL SELECTED-WINDOW))   ;This can happen.
			     (SETQ SELECTED-IO-BUFFER
				   (FUNCALL SELECTED-WINDOW :IO-BUFFER)))
		   (AND SELECTED-IO-BUFFER
			(IO-BUFFER-LAST-OUTPUT-PROCESS
			  SELECTED-IO-BUFFER))))))
  (SETQ WHO-LINE-RUN-STATE (COND ((NULL SELECTED-WINDOW)
				  "No selected window")
				 ((NULL P)
				  "No current process")
				 ((ASSOC P ACTIVE-PROCESSES :TEST #'EQ)
				  (PROCESS-WHOSTATE P))
				 ((NOT (NULL (SI:PROCESS-ARREST-REASONS P)))
				  "Arrest")
				 (T "Stop")))
  (cond
    (si:*sib-present* (WHO-LINE-UPDATE T))
    (t (copy-array-portion who-line-run-state 0 (LENGTH who-line-run-state)
			   mac-who-string 0 (- si:%MX-Who-State-String-Max 1))))
  )

(DEFUN WHO-LINE-FIELD (&REST ARGS &AUX W)
 ;; Do sheet type consing in special area to increase locality.
  (SETQ W (APPLY #'MAKE-WINDOW (GETF ARGS :FLAVOR)
		 :AREA WHO-LINE-AREA
		 :SUPERIOR WHO-LINE-SCREEN
		 ARGS))
  (FUNCALL W :ACTIVATE)
  (FUNCALL W :EXPOSE)
  W)


;; This is an array rather than a list to avoid consing.

(defvar *Open-streams* (MAKE-ARRAY 20. :TYPE 'ART-Q-LIST :LEADER-LIST '(0)))  ; DAB 02-22-89

(defvar *SERVERS-LIST* nil "A list with elements (chaos-connection from-machine contact-name).")  ; DAB 02-22-89

(defvar *CURRENT-STREAM* nil "The one being displayed.")  ; DAB 02-23-89

(defvar *DISPLAYED-PERCENT* nil) ; DAB 02-23-89

(defvar *DISPLAYED-COUNT* nil)  ; DAB 02-23-89

(DEFFLAVOR WHO-LINE-FILE-SHEET ; DAB 02-22-89
   ()
   (WHO-LINE-MIXIN MINIMUM-WINDOW))

;;; Take the most recently opened input stream if there is one.  Otherwise
;;; take the most recently opened output stream.
(DEFUN WHO-LINE-FILE-SHEET-COMPUTE-CURRENT-STREAM (&OPTIONAL (UPDATE-P T))
  (DECLARE (:SELF-FLAVOR WHO-LINE-FILE-SHEET) (special *OPEN-STREAMS* *CURRENT-STREAM*))
  (DO ((I (1- (ARRAY-LEADER *OPEN-STREAMS* 0)) (1- I))  ; DAB 02-22-89
       (OUTPUT-WINNER NIL) (STREAM) (DIRECTION))
      ((MINUSP I)
       (SETQ *CURRENT-STREAM* OUTPUT-WINNER))
    (SETQ STREAM (AREF *OPEN-STREAMS* I))  ; DAB 02-22-89
    (MULTIPLE-VALUE-SETQ (NIL DIRECTION)
			 (FUNCALL STREAM :WHO-LINE-INFORMATION))
    (CASE DIRECTION
	  ((:INPUT :BIDIRECTIONAL)
	   (RETURN (SETQ *CURRENT-STREAM* STREAM)))
	  ((:OUTPUT :IO)
	   (OR OUTPUT-WINNER
	       (SETQ OUTPUT-WINNER STREAM)))))
  (AND UPDATE-P (WHO-LINE-UPDATE)))

(DEFMETHOD (WHO-LINE-FILE-SHEET :servers-list) ()
  (declare (special *SERVERS-LIST*))
  *SERVERS-LIST*)

(DEFMETHOD (WHO-LINE-FILE-SHEET :ADD-STREAM) (STREAM &OPTIONAL (UPDATE-P T))
  (declare (special *OPEN-STREAMS* *CURRENT-STREAM* ))
  (AND (without-interrupts (VECTOR-PUSH-EXTEND STREAM *OPEN-STREAMS*))
     (WHO-LINE-FILE-SHEET-COMPUTE-CURRENT-STREAM UPDATE-P)))

(DEFMETHOD (WHO-LINE-FILE-SHEET :DELETE-STREAM) (STREAM &AUX POS)
  (declare (special *OPEN-STREAMS* *CURRENT-STREAM* ))
  (COND ((SETQ POS (POSITION STREAM (THE LIST (G-L-P *OPEN-STREAMS*)) :TEST #'EQ))
	 (COND ((= POS (1- (ARRAY-LEADER *OPEN-STREAMS* 0)))
		 (without-interrupts (VECTOR-POP *OPEN-STREAMS*)))
	       (T  (without-interrupts (SETF (AREF *OPEN-STREAMS* POS) (VECTOR-POP *OPEN-STREAMS*)))))
	 (AND (EQ STREAM *CURRENT-STREAM*)
	      (WHO-LINE-FILE-SHEET-COMPUTE-CURRENT-STREAM)))))

(DEFMETHOD (WHO-LINE-FILE-SHEET :DELETE-ALL-STREAMS) ()
  (declare (special *OPEN-STREAMS* *CURRENT-STREAM* ))
  (without-interrupts (STORE-ARRAY-LEADER 0 *OPEN-STREAMS* 0)) ; DAB 02-22-89
  (SETQ *CURRENT-STREAM* NIL))

(DEFMETHOD (WHO-LINE-FILE-SHEET :OPEN-STREAMS) ()
  (declare (special *OPEN-STREAMS*))
  (G-L-P *OPEN-STREAMS*))

(DEFMETHOD (WHO-LINE-FILE-SHEET :servers-list) ()
  (declare (special *SERVERS-LIST*))
  *SERVERS-LIST*)

(DEFMETHOD (WHO-LINE-FILE-SHEET :current-stream) ()
  (declare (special *CURRENT-STREAM*))
  *CURRENT-STREAM*)

(defmethod (WHO-LINE-FILE-SHEET :DISPLAYED-PERCENT) ()	; DAB 04-19-89
  (declare (special *DISPLAYED-PERCENT*))
  *DISPLAYED-PERCENT*)

(defmethod (WHO-LINE-FILE-SHEET :DISPLAYED-COUNT) ()  ; DAB 04-19-89
  (declare (special *DISPLAYED-COUNT*))
  *DISPLAYED-COUNT*)

(defvar *DISPLAYED-COUNT* nil)  ; DAB 02-23-89




;; CHANGED TO BE GENERIC - LS 11/06/86
(DEFMETHOD (WHO-LINE-FILE-SHEET :ADD-SERVER)
	   (CONNECTION CONTACT-NAME &OPTIONAL (PROCESS CURRENT-PROCESS) FUNCTION &REST ARGS)
  (declare (special  *SERVERS-LIST*))
  (SEND SELF :DELETE-SERVER CONNECTION)
  (let ((server-desc (MAKE-SERVER-DESC
		       CONNECTION CONNECTION
		       HOST-NAME (IGNORE-ERRORS (SEND CONNECTION :NAME))
		       CONTACT-NAME CONTACT-NAME
		       PROCESS PROCESS
		       FUNCTION FUNCTION
		       ARGS (COPY-LIST ARGS))))
    (without-interrupts (PUSH server-desc *SERVERS-LIST*))))

;;; This isn't usually called; Normally servers are deleted automatically when
;;; it is noticed that the connection has been closed.
(DEFMETHOD (WHO-LINE-FILE-SHEET :DELETE-SERVER)
	   (CONNECTION
	    &AUX (INHIBIT-SCHEDULING-FLAG T))
  (declare (special  *SERVERS-LIST*))
  (SETQ *SERVERS-LIST* (DELETE CONNECTION (THE LIST *SERVERS-LIST*) :TEST
			     #'(LAMBDA (X Y) (EQ X (SERVER-DESC-CONNECTION Y))))))

(DEFMETHOD (WHO-LINE-FILE-SHEET :DELETE-ALL-SERVERS) ()
  (declare (special  *SERVERS-LIST*))
  (SETQ *SERVERS-LIST* NIL))


;; CHANGED TO BE GENERIC - LS 11/06/86
(DEFMETHOD (WHO-LINE-FILE-SHEET :CLOSE-ALL-SERVERS) (REASON)
  (declare (special  *SERVERS-LIST*))
  (LOOP FOR SERVER IN *SERVERS-LIST* FINALLY (SETQ *SERVERS-LIST* NIL)
	DO
	(IGNORE-ERRORS (SEND (SERVER-DESC-CONNECTION SERVER) :CLOSE REASON))
	))

;;; Remove all servers which aren't current anymore.

;; CHANGED TO BE GENERIC - LS 11/06/86
(DEFUN PURGE-SERVERS ()
  (DECLARE (:SELF-FLAVOR WHO-LINE-FILE-SHEET)  (special  *SERVERS-LIST*))
  (WITHOUT-INTERRUPTS
    (DO ((S *SERVERS-LIST* (CDR S)))
	((NULL S)
	 (SETQ *SERVERS-LIST* (DELETE () (THE LIST *SERVERS-LIST*) :TEST #'EQ)))
      (WHEN
	(IGNORE-ERRORS
	  (NOT (SEND (SERVER-DESC-CONNECTION (CAR S)) :SEND-IF-HANDLES :SERVER-CURRENT-P)))
	(SETF (CAR S) ()))
      )))


(DEFUN BACKGROUND-NOTIFY (FORMAT-STRING &REST ARGS)
  (APPLY 'PROCESS-RUN-FUNCTION "Notify" 'NOTIFY NIL FORMAT-STRING ARGS))

(DEFMETHOD (WHO-LINE-FILE-SHEET :SERVERS) ()
  (declare (special  *SERVERS-LIST*))
  (PURGE-SERVERS)
  *SERVERS-LIST*)


;;; User level functions.
(DEFUN DESCRIBE-SERVERS ()
  "Describe all network servers currently serving."
  (DOLIST (S (FUNCALL WHO-LINE-FILE-STATE-SHEET :SERVERS))
    (FORMAT T "~%~A serving ~A in ~A"
	    (SERVER-DESC-CONTACT-NAME S)
	    (SERVER-DESC-HOST-NAME    S)
	    (SERVER-DESC-PROCESS      S))))

(DEFUN CLOSE-ALL-SERVERS (&OPTIONAL (REASON "Foo on you"))
  "Disconnect all servers on this machine from their remote users."
  (FUNCALL WHO-LINE-FILE-STATE-SHEET :CLOSE-ALL-SERVERS REASON))

(DEFMETHOD (WHO-LINE-FILE-SHEET :UPDATE)
	   (&AUX (MAX-CHARS (TRUNCATE (SHEET-INSIDE-WIDTH)
				      CHAR-WIDTH))
	    IDLE STRING)
  (declare (special  *SERVERS-LIST* *CURRENT-STREAM* *DISPLAYED-PERCENT* *DISPLAYED-COUNT*))
  (COND (*CURRENT-STREAM*
	 (LET ((OLD-STREAM WHO-LINE-ITEM-STATE)
               (PATHNAME) (DIRECTION) (PERCENT) (COUNT))
	   (MULTIPLE-VALUE-SETQ (PATHNAME DIRECTION COUNT PERCENT)
				(FUNCALL *CURRENT-STREAM* :WHO-LINE-INFORMATION))
	   (COND ((AND (EQ OLD-STREAM *CURRENT-STREAM*)
                       (EQ PERCENT *DISPLAYED-PERCENT*)
                       (EQ COUNT   *DISPLAYED-COUNT*)))
                 (T
                  (SHEET-HOME SELF)
                  (OR (EQ OLD-STREAM *CURRENT-STREAM*)
                      (SHEET-CLEAR-EOL SELF))
                  (SETQ WHO-LINE-ITEM-STATE *CURRENT-STREAM*
                        *DISPLAYED-PERCENT* PERCENT
                        *DISPLAYED-COUNT* COUNT)
                  (DISPLAY-FILE-TRANSFER SELF PATHNAME DIRECTION COUNT
                                         PERCENT MAX-CHARS)))))
	((AND (NOT (NULL *SERVERS-LIST*))
              (PROGN (PURGE-SERVERS)
                     (NOT (NULL *SERVERS-LIST*))))
	 (COND ((= (LENGTH *SERVERS-LIST*) 1)
                (COND ((NEQ WHO-LINE-ITEM-STATE (CAAR *SERVERS-LIST*))
                       (SHEET-HOME SELF)
                       (SHEET-CLEAR-EOL SELF)
                       (SETQ STRING (FORMAT NIL "~A serving ~A"
                                            (CADDAR *SERVERS-LIST*)
                                            (CADAR  *SERVERS-LIST*)))
                       (SHEET-STRING-OUT SELF STRING 0
                                         (MIN (LENGTH STRING) MAX-CHARS))
                       (RETURN-ARRAY (PROG1 STRING (SETQ STRING NIL)))
                       (SETQ WHO-LINE-ITEM-STATE (CAAR *SERVERS-LIST*)))))
               ((NEQ WHO-LINE-ITEM-STATE (LENGTH *SERVERS-LIST*))
                (SHEET-HOME      SELF)
                (SHEET-HOME      SELF)
                (SHEET-CLEAR-EOL SELF)
                (SETQ STRING (FORMAT NIL "~D Active Servers"
                                     (LENGTH *SERVERS-LIST*)))
                (SHEET-STRING-OUT SELF STRING 0 (MIN (LENGTH STRING)
                                                     MAX-CHARS))
                (RETURN-ARRAY (PROG1 STRING (SETQ STRING NIL)))
                (SETQ WHO-LINE-ITEM-STATE (LENGTH *SERVERS-LIST*)))))
  (SI::WHO-LINE-JUST-COLD-BOOTED-P
    (COND ((NEQ WHO-LINE-ITEM-STATE 'COLD)
           (SHEET-CLEAR SELF)
           (SETQ WHO-LINE-ITEM-STATE 'COLD)
           (SHEET-STRING-OUT SELF "Cold-booted"))))
  ;; Wait for 5 minutes before displaying idle time.
  ((>= (SETQ IDLE (TRUNCATE (TIME-DIFFERENCE
                              (TIME)
                              KBD-LAST-ACTIVITY-TIME) 3600.))
       5)
                                                ;Display keyboard idle time
   (LET ((OLD-IDLE WHO-LINE-ITEM-STATE))
     (COND ((OR (NOT (NUMBERP OLD-IDLE)) (NOT (= OLD-IDLE IDLE)))
            (SHEET-CLEAR SELF)
            (WITHOUT-INTERRUPTS
              (LET ((STRING (MAKE-IDLE-MESSAGE IDLE)))
                (SHEET-STRING-OUT SELF STRING)
                (RETURN-ARRAY STRING)))
            (SETQ WHO-LINE-ITEM-STATE IDLE)))))

   ;;; Fix continual display of "FILE/PRINT serving X" message.
   ;;; Message continues to be displayed even after connection
   ;;; is closed and purged from *SERVERS-LIST*.          ES
	((OR (NULL *servers-list*)
           (PROGN (purge-servers)
		  (NULL *servers-list*)))
       (COND ((NEQ who-line-item-state 'clear-server-msg) 
              (sheet-home self)
              (sheet-clear-eol self)
              (SETQ who-line-item-state 'clear-server-msg))))

  ((NEQ WHO-LINE-ITEM-STATE 'NULL)
   (SHEET-CLEAR SELF)
   (SETQ WHO-LINE-ITEM-STATE 'NULL))))

;;;;;; The following five variables are used by the functions that make the
;;;;;; screen black.
;;;(DEFVAR BLACK-SCREEN-TIME-DELAY 120.
;;;   "Number of minutes to delay before making the screen black.")

;;;(DEFVAR THE-SCREEN-IS-BLACK NIL
;;;   "the-screen-is-black is nil when the screen is normal (like now).
;;;It is T when the screen is black.")

;;;(DEFVAR OLD-WHITE-MAIN-SCREEN-ARRAY NIL
;;;   "This is the contents of the screen array before the screen was turned black.
;;;This is used to restore the screen to its previous contents, when the screen is
;;;turned white again.")

;;;(DEFVAR ZERO-MAIN-SCREEN-ARRAY NIL
;;;   "This is an array that corresponds in size to old-white-main-screen-array.
;;;When it is created, it will contain all zeros.  It is used to make the screen go
;;;black very fast.") 

;;;(DEFVAR OLD-SELECTED-BLINKER-SETTINGS NIL
;;;   "This is a list of the settings of the selected window's blinkers, including the
;;;inferiors of the selected window.")

;;;(MAKE-OBSOLETE
;;;  MAKE-THE-ENTIRE-SCREEN-BLACK
;;;   "It will be going away soon.  Use the brightness or contrast knob instead.")
     
;;;(DEFUN MAKE-THE-ENTIRE-SCREEN-BLACK ()
;;;  "Used to make the phosphors on the display last longer."
;;;  (SETQ THE-SCREEN-IS-BLACK T)
;;;  ;; During the system build process the value of TV:MAIN-SCREEN will be
;;;  ;; NIL and we will want to prevent the screen going black during that
;;;  ;; process anyway.
;;;  (IF (NULL MAIN-SCREEN)
;;;   ;; During the system build process we can prevent the continuous
;;;   ;; checking for the condition under which the screen goes black
;;;   ;; by changing the keyboard last activity time variable to the 
;;;   ;; current time setting.  This will prevent the check from being done
;;;   ;; for BLACK-SCREEN-TIME-DELAY minutes.
;;;    (SETQ KBD-LAST-ACTIVITY-TIME (TIME))
;;;    ;;ELSE
;;;    (LET (DOCUMENTATION-LINE-WINDOW        ; Instance of documentation line
;;;	  MAIN-SCREEN-ARRAY                ; Contains bits of screen-array
;;;	  MAIN-SCREEN-HEIGHT               ; Height of main screen
;;;	  MAIN-SCREEN-WIDTH                ; Width  of main screen
;;;	  (TRANSFER-FILE-WINDOW-X-OFFSET 0); x-offset of who line's file
;;;	                                   ; transfer window
;;;	  TRANSFER-FILE-WINDOW             ; Who line's file transfer window
;;;	  WHO-LINE-SCREEN-INFERIORS)       ; All inferior windows of who line
;;;      (SETQ MAIN-SCREEN-ARRAY  (SEND MAIN-SCREEN :SCREEN-ARRAY))
;;;      (SETQ MAIN-SCREEN-HEIGHT (SEND MAIN-SCREEN :HEIGHT))
;;;      (SETQ MAIN-SCREEN-WIDTH  (SEND MAIN-SCREEN :WIDTH))
;;;      (IF (OR (NULL OLD-WHITE-MAIN-SCREEN-ARRAY)
;;;	  (NOT (= (ARRAY-TOTAL-SIZE OLD-WHITE-MAIN-SCREEN-ARRAY)
;;;                  (* MAIN-SCREEN-HEIGHT MAIN-SCREEN-WIDTH))))
;;;       ;; Either, we haven't created the old-white-main-screen-array yet
;;;       ;; (the null case), or the size of the screen has changed since the
;;;       ;; last time we created it.  In either case, we need to make the
;;;       ;; save array the big enough to handle all the bits in the screen-array.
;;;	(PROGN
;;;	  (SETQ OLD-WHITE-MAIN-SCREEN-ARRAY
;;;		(MAKE-SHEET-BIT-ARRAY MAIN-SCREEN
;;;				      MAIN-SCREEN-WIDTH
;;;				      MAIN-SCREEN-HEIGHT))
;;;	  (SETQ ZERO-MAIN-SCREEN-ARRAY
;;;		(MAKE-SHEET-BIT-ARRAY
;;;		  MAIN-SCREEN
;;;		  MAIN-SCREEN-WIDTH
;;;		  MAIN-SCREEN-HEIGHT
;;;		  :INITIAL-VALUE 0))))

;;;      ;; We need to turn off the blinkers before we turn off the screen
;;;      ;; because it may blink in the middle, causing part of the blinker
;;;      ;; to remain on the screen.  Before we turn off the blinkers, we
;;;      ;; need to save the settings.
;;;      (SETQ OLD-SELECTED-BLINKER-SETTINGS
;;;	    (GET-VISIBILITY-OF-ALL-SHEETS-BLINKERS SELECTED-WINDOW))
;;;      (WITHOUT-INTERRUPTS
;;;       ;; We need to turn off interrupts so that one of the blinkers
;;;       ;; isn't turned back on by the scheduler.
;;;       (TURN-OFF-ALL-SHEETS-BLINKERS SELECTED-WINDOW)
;;;       (OPEN-ALL-SHEETS-BLINKERS SELECTED-WINDOW)
;;;       ;; Save the current contents of the screen array.  We will need this value
;;;       ;; when we want to set the screen back the way it was.
;;;       (BITBLT ALU-SETA MAIN-SCREEN-WIDTH MAIN-SCREEN-HEIGHT
;;;               MAIN-SCREEN-ARRAY 0 0 OLD-WHITE-MAIN-SCREEN-ARRAY 0 0)
;;;       ;; Turn off the screen, regardless of its organization.  This kludge is
;;;       ;; being used because the function that calls us has screen management
;;;       ;; turned off, and prevents us from using a more elegant method involving
;;;       ;; windows.
;;;       (BITBLT ALU-SETA MAIN-SCREEN-WIDTH MAIN-SCREEN-HEIGHT
;;;               ZERO-MAIN-SCREEN-ARRAY 0 0 MAIN-SCREEN-ARRAY 0 0))
;;;      ;; save the current value of screen color so we can restore the 
;;;      ;; color after a mouse button has been clicked
;;;      (LET ((TEMP *CURRENT-SCREEN-COLOR*))
;;;	(WHITE-ON-BLACK)
;;;	(SETQ *CURRENT-SCREEN-COLOR* TEMP))
;;;      ;; Now take care of the individual parts of the who line.  These parts are:
;;;      ;;   user name, process name, file name for transfers
;;;      ;;   USER:, current time, documentation line
;;;      ;; but not necessarily in that order.
;;;      (SETQ WHO-LINE-SCREEN-INFERIORS (SEND WHO-LINE-SCREEN :INFERIORS))
;;;      ;; Make most of the who line white, like it normally is.
;;;      (DOLIST (WHO-LINE-SCREEN-INFERIOR WHO-LINE-SCREEN-INFERIORS)
;;;	(SEND WHO-LINE-SCREEN-INFERIOR :SET-REVERSE-VIDEO-P T)
;;;	;; Locate the documentation line.  It is the only one as wide as the
;;;	;; main screen.
;;;	(IF (EQUAL (SEND WHO-LINE-SCREEN-INFERIOR :WIDTH) MAIN-SCREEN-WIDTH)
;;;	  (SETQ DOCUMENTATION-LINE-WINDOW WHO-LINE-SCREEN-INFERIOR))
;;;	;; The window that contains file transfer information is the one all
;;;	;; the way to the right (i.e. highest x offset).
;;;	(IF (> (SEND WHO-LINE-SCREEN-INFERIOR :X-OFFSET)
;;;               TRANSFER-FILE-WINDOW-X-OFFSET)
;;;	  (SETQ TRANSFER-FILE-WINDOW WHO-LINE-SCREEN-INFERIOR)))
;;;      ;; Set the documentation line to the way it normally is set.
;;;      (SEND DOCUMENTATION-LINE-WINDOW :SET-REVERSE-VIDEO-P ())
;;;      ;; Set the file transfer window to black, so that those pixles will
;;;      ;; not get worn out.  Normally, this field will be empty anyway, when
;;;      ;; this machine is idle.
;;;      (SEND TRANSFER-FILE-WINDOW :SET-REVERSE-VIDEO-P ())

;;;      ;; Create a process which will wait for the user to press a key, and
;;;      ;; then set the  screen back the way it was.
;;;      (PROCESS-RUN-FUNCTION '(:NAME "PRESS A KEY")
;;;			    #'(LAMBDA ()
;;;				(INHIBIT-STYLE-WARNINGS
;;;				 (PRESS-A-KEY-TO-MAKE-THE-SCREEN-WHITE-AGAIN))
;;;				(INHIBIT-STYLE-WARNINGS (MAKE-THE-SCREEN-WHITE-AGAIN))
;;;				(SEND CURRENT-PROCESS :KILL))))))


;;;(MAKE-OBSOLETE PRESS-A-KEY-TO-MAKE-THE-SCREEN-WHITE-AGAIN
;;;   "It will be going away soon.  Use the brightness or contrast knob instead.") 

;;;(DEFUN PRESS-A-KEY-TO-MAKE-THE-SCREEN-WHITE-AGAIN ()
;;;  "Wait for the user to press a key so we can make the screen white again"
;;;  ;; Wait for the user to press a mouse button, indicating that we should
;;;  ;; restore the screen to the way it was before MAKE-THE-ENTIRE-SCREEN-BLACK
;;;  ;; messed it up.
;;;  (LET ((DEFAULT-FONT FONTS:BIGFNT)       ; Use this to display in cold-load-stream
;;;	CURSOR-POS-X                      ; X cursor position of message
;;;	CURSOR-POS-Y                      ; Y cursor position of message
;;;	MAIN-SCREEN-HEIGHT                ; Height of main screen
;;;	MAIN-SCREEN-WIDTH                 ; Width  of main screen
;;;	NEW-LINE-HEIGHT                   ; Line-height for default-font
;;;	OLD-CHAR-WIDTH                    ; Current char-width in cold-load-stream
;;;	OLD-FONT                          ; Current font in cold-load-stream
;;;	OLD-LINE-HEIGHT)                  ; Current line-height in cold-load-stream
;;;    (SETQ MAIN-SCREEN-HEIGHT (SEND MAIN-SCREEN ':HEIGHT))
;;;    (SETQ MAIN-SCREEN-WIDTH (SEND MAIN-SCREEN ':WIDTH))
;;;    ;; We save the current font information in cold-load-stream so we
;;;    ;; can restore it after we have modified it.
;;;    (SETQ OLD-CHAR-WIDTH
;;;	  (SYMEVAL-IN-INSTANCE SI:COLD-LOAD-STREAM 'SI::CHAR-WIDTH))
;;;    (SETQ OLD-FONT
;;;	  (SYMEVAL-IN-INSTANCE SI:COLD-LOAD-STREAM 'FONT))
;;;    (SETQ OLD-LINE-HEIGHT
;;;	  (SYMEVAL-IN-INSTANCE SI:COLD-LOAD-STREAM 'SI::LINE-HEIGHT))
;;;    ;; Set the current font information in cold-load-stream to
;;;    ;; correspond with default-font.
;;;    (SET-IN-INSTANCE SI:COLD-LOAD-STREAM 'FONT DEFAULT-FONT)
;;;    (SET-IN-INSTANCE SI:COLD-LOAD-STREAM 'SI::CHAR-WIDTH
;;;		     (FONT-CHAR-WIDTH DEFAULT-FONT))
;;;    (SET-IN-INSTANCE SI:COLD-LOAD-STREAM 'SI::LINE-HEIGHT
;;;		     (SETQ NEW-LINE-HEIGHT (+ 2 (FONT-CHAR-HEIGHT DEFAULT-FONT))))
;;;    ;; Put the prompt in the middle of the screen, so that nobody can
;;;    ;; miss seeing it.  Randomize the (X,Y) pixle coordinate a little by adding
;;;    ;; a random number to it.  This will keep the prompt from burning into
;;;    ;; the display.
;;;    (SETQ CURSOR-POS-X (- (TRUNCATE MAIN-SCREEN-WIDTH  2) (- 50. (RANDOM 100.))))
;;;    (SETQ CURSOR-POS-Y (- (TRUNCATE MAIN-SCREEN-HEIGHT 2) (- 50. (RANDOM 100.))))
;;;    ;; If the CADR system has been cold-booted then we need to tell
;;;    ;; the user that a key on the keyboard needs to be pressed too.  This is
;;;    ;; necessary because on cold boot, the mouse is disabled until the user
;;;    ;; pressess a key on the keyboard.  If this were not done, then when the
;;;    ;; system is booted, any change in the state of the mouse would interrupt
;;;    ;; the processor, causing problems since the mouse handler probably
;;;    ;; wouldn't be initialized.  The Explorer and Lambda systems do not lock up
;;;    ;; the mouse on cold-boot, so the message isn't necessary for them.
;;;    (IF (AND SI::WHO-LINE-JUST-COLD-BOOTED-P (= PROCESSOR-TYPE-CODE CADR-TYPE-CODE))
;;;      (PROGN
;;;       ;; Output this string so that it is centered on the line.
;;;       (SEND SI:COLD-LOAD-STREAM :SET-CURSORPOS
;;;	  (- CURSOR-POS-X
;;;	     (TRUNCATE (SHEET-STRING-LENGTH MAIN-SCREEN
;;;                                            "Press a key on the keyboard before you"
;;;                                            0 () NIL DEFAULT-FONT) 2))
;;;	  CURSOR-POS-Y)
;;;       (SEND SI:COLD-LOAD-STREAM
;;;	     :STRING-OUT "Press a key on the keyboard before you ")
;;;       ;; Move on to the next line for the next string.
;;;       (SETQ CURSOR-POS-Y (+ NEW-LINE-HEIGHT CURSOR-POS-Y))))
;;;    ;; Output this string so that it is centered on the line.
;;;    (SEND SI:COLD-LOAD-STREAM ':SET-CURSORPOS
;;;       (- CURSOR-POS-X
;;;	  (TRUNCATE (SHEET-STRING-LENGTH MAIN-SCREEN
;;;                                         "Press a mouse button"
;;;                                         0 NIL NIL DEFAULT-FONT) 2))
;;;       CURSOR-POS-Y)
;;;    (SEND SI:COLD-LOAD-STREAM :STRING-OUT "Press a mouse button")
;;;    ;; Restore the font information in cold-load-stream to what it
;;;    ;; was earlier.
;;;    (SET-IN-INSTANCE SI:COLD-LOAD-STREAM 'FONT            OLD-FONT)
;;;    (SET-IN-INSTANCE SI:COLD-LOAD-STREAM 'SI::CHAR-WIDTH  OLD-CHAR-WIDTH)
;;;    (SET-IN-INSTANCE SI:COLD-LOAD-STREAM 'SI::LINE-HEIGHT OLD-LINE-HEIGHT)
;;;    (LET ((OLD-MOUSE-X MOUSE-X)          ; Save current mouse
;;;	  (OLD-MOUSE-Y MOUSE-Y))         ; (x,y) coordinates
;;;      (WITH-MOUSE-GRABBED
;;;       ; The mouse being grabbed turns off the mouse blinker.
;;;       ; Make sure that the mouse buttons are released before we wait for
;;;       ; one to be pressed.
;;;       (PROCESS-WAIT "Release Button" #'(LAMBDA () (ZEROP MOUSE-LAST-BUTTONS)))
;;;       (PROCESS-WAIT "Button"         #'(LAMBDA () (NOT (ZEROP MOUSE-LAST-BUTTONS)))))
;;;      ;; Set the mouse back to where it was. This technique will cause a mouse
;;;      ;; blinker ghost to appear only if the user moves the mouse between now and
;;;      ;; when we do the bitblt in the next function call.
;;;      (MOUSE-WARP OLD-MOUSE-X OLD-MOUSE-Y))))

;;;(MAKE-OBSOLETE MAKE-THE-SCREEN-WHITE-AGAIN
;;;   "It will be going away soon.  Use the brightness or contrast knob instead.")
     
;;;(DEFUN MAKE-THE-SCREEN-WHITE-AGAIN ()
;;;  "Restore the screen to its original image.
;;;Relates to the MAKE-THE-ENTIRE-SCREEN-BLACK function"
;;;  ;; Start by restoring the who line, then set the screen's pixles
;;;  ;; to what they were before we made the screen black.
;;;  (LET (MAIN-SCREEN-HEIGHT                     ; Height of main screen
;;;	MAIN-SCREEN-WIDTH                      ; Width  of main screen
;;;	DOCUMENTATION-LINE-WINDOW              ; Instance of documentation line
;;;	MAIN-SCREEN-ARRAY                      ; Contains bits of screen-array
;;;	WHO-LINE-SCREEN-INFERIORS)             ; All inferior windows of who line
;;;    (SETQ MAIN-SCREEN-HEIGHT (SEND MAIN-SCREEN :HEIGHT))
;;;    (SETQ MAIN-SCREEN-WIDTH  (SEND MAIN-SCREEN :WIDTH))
;;;    (SETQ MAIN-SCREEN-ARRAY  (SEND MAIN-SCREEN :SCREEN-ARRAY))
;;;    (SETQ WHO-LINE-SCREEN-INFERIORS (SEND WHO-LINE-SCREEN :INFERIORS))
;;;    ;; Set the who line back the was it was.
;;;    (DOLIST (WHO-LINE-SCREEN-INFERIOR WHO-LINE-SCREEN-INFERIORS)
;;;      (SEND WHO-LINE-SCREEN-INFERIOR :SET-REVERSE-VIDEO-P NIL)
;;;      ;; Locate the documentation line.  It is the only one as wide as the
;;;      ;; main screen.
;;;      (IF (EQUAL (SEND WHO-LINE-SCREEN-INFERIOR :WIDTH) MAIN-SCREEN-WIDTH)
;;;	(SETQ DOCUMENTATION-LINE-WINDOW WHO-LINE-SCREEN-INFERIOR)))
;;;    (SEND DOCUMENTATION-LINE-WINDOW :SET-REVERSE-VIDEO-P T)
;;;    (SET-UP-CURRENT-SCREEN-COLOR)
;;;    ;; Restore the screen-array to the value it had before we messed
;;;    ;; with it.
;;;    (BITBLT ALU-SETA MAIN-SCREEN-WIDTH MAIN-SCREEN-HEIGHT
;;;	    OLD-WHITE-MAIN-SCREEN-ARRAY 0 0 MAIN-SCREEN-ARRAY 0 0)
;;;    ;; Restore the settings of the selected window's blinkers (inferiors too)
;;;    ;; to what they were earlier.
;;;    (SET-VISIBILITY-OF-ALL-SHEETS-BLINKERS
;;;      SELECTED-WINDOW OLD-SELECTED-BLINKER-SETTINGS)
;;;    (SETQ KBD-LAST-ACTIVITY-TIME (TIME))         ; Keep from blacking out too soon.
;;;    ;; The last thing we do is the set this flag.  If we did it earlier, then
;;;    ; we might inadvertently set it black twice, forgeting the original
;;;    ; image on the screen.
;;;    (SETQ THE-SCREEN-IS-BLACK NIL)))

(DEFPARAMETER DISPLAY-FILE-TRANSFER-COUNT-STRING
   (MAKE-ARRAY 20. :TYPE ART-STRING :LEADER-LENGTH 1))

(DEFPARAMETER DISPLAY-FILE-TRANSFER-PERCENT-STRING
   (MAKE-ARRAY 5. :TYPE ART-STRING :LEADER-LENGTH 1))

(DEFVAR LAST-WHOLINE-PATHNAME NIL
  "The last pathname displayed in the who line.")
(DEFVAR LAST-WHOLINE-PATHNAME-STRING NIL
  "The string we displayed for LAST-WHOLINE-PATHNAME.")
(DEFVAR LAST-WHOLINE-PATHNAME-LENGTH NIL
   "The length we requested, when we obtained
LAST-WHOLINE-PATHNAME-STRING.")

;;; Display the who-line-information onto SHEET.  PERCENT may be NIL,
;;; but COUNT is always a fixnum.  DIRECTION is one of the keywords
;;; :INPUT, :OUTPUT, or :BIDIRECTIONAL.  MAX-CHARS is the maximum
;;; number of characters that we may output.
(DEFUN DISPLAY-FILE-TRANSFER (SHEET PATHNAME DIRECTION COUNT PERCENT MAX-CHARS)
  (SHEET-TYO SHEET (CASE DIRECTION
		     (:INPUT         #\LEFT-ARROW)
		     (:OUTPUT        #\RIGHT-ARROW)
		     ((:BIDIRECTIONAL :IO) #\DOUBLE-ARROW)
		     (T              #\?)))
  (SHEET-TYO SHEET #\SPACE)
  (LET* ((FILE-NAME        (FUNCALL PATHNAME :STRING-FOR-WHOLINE))
	 (FILE-NAME-LENGTH (LENGTH FILE-NAME))
	 (FILE-NAME-LIMIT   NIL)
	 (COUNT-STRING-LENGTH)
	 (PERCENT-STRING-LENGTH)
	 (DISPLAY-COUNT-P   NIL)
	 (DISPLAY-PERCENT-P NIL))
    (FIXNUM-INTO-STRING COUNT DISPLAY-FILE-TRANSFER-COUNT-STRING)
    (SETQ COUNT-STRING-LENGTH (ARRAY-ACTIVE-LENGTH
                                DISPLAY-FILE-TRANSFER-COUNT-STRING))
    (COND ((NULL PERCENT)
           (SETQ DISPLAY-PERCENT-P NIL
                 DISPLAY-COUNT-P T)
           ;; 4 is two for the direction and two for the spaces after the file name.
           (IF (>= (+ FILE-NAME-LENGTH COUNT-STRING-LENGTH 4) MAX-CHARS)
               ;; Truncate the file name if it doesn't fit.
               (SETQ FILE-NAME-LIMIT (- MAX-CHARS 4 COUNT-STRING-LENGTH))))
          (T
           (FIXNUM-INTO-STRING PERCENT DISPLAY-FILE-TRANSFER-PERCENT-STRING)
           (VECTOR-PUSH #\% DISPLAY-FILE-TRANSFER-PERCENT-STRING)
           (SETQ PERCENT-STRING-LENGTH
                 (ARRAY-ACTIVE-LENGTH DISPLAY-FILE-TRANSFER-PERCENT-STRING))
           ;; If we can fit both the % and the count with the file name then OK,
           ;; else don't display the count, and truncate the file name if necessary.
           (SETQ DISPLAY-PERCENT-P T)
           (COND ((<= (+ FILE-NAME-LENGTH COUNT-STRING-LENGTH
                         PERCENT-STRING-LENGTH
                         ;; 5 is the above 4 plus 1 space between percent & count.
                         5)
                      MAX-CHARS)
                  (SETQ DISPLAY-COUNT-P T))
                 ((> (+ FILE-NAME-LENGTH PERCENT-STRING-LENGTH 4) MAX-CHARS)
                  (SETQ FILE-NAME-LIMIT (- MAX-CHARS PERCENT-STRING-LENGTH 4))))))
    (WHEN FILE-NAME-LIMIT
      (IF (AND (EQ PATHNAME LAST-WHOLINE-PATHNAME)
	       (= FILE-NAME-LIMIT LAST-WHOLINE-PATHNAME-LENGTH))
	(SETQ FILE-NAME LAST-WHOLINE-PATHNAME-STRING)
        ;;ELSE
	(PROGN
	  (SETQ FILE-NAME (SEND PATHNAME :STRING-FOR-WHOLINE FILE-NAME-LIMIT))
	  (SETQ LAST-WHOLINE-PATHNAME-LENGTH FILE-NAME-LIMIT)
	  (SETQ LAST-WHOLINE-PATHNAME-STRING FILE-NAME)
	  (SETQ LAST-WHOLINE-PATHNAME        PATHNAME)))
      (AND (= FILE-NAME-LIMIT (LENGTH        FILE-NAME))
	   (SETQ FILE-NAME-LIMIT NIL)))
    (SHEET-CLEAR-EOL SHEET)
    (SHEET-STRING-OUT SHEET FILE-NAME 0
		      (IF FILE-NAME-LIMIT
			(MIN FILE-NAME-LIMIT (LENGTH FILE-NAME))))
    (SHEET-TYO SHEET (IF FILE-NAME-LIMIT
		      ;; Let the user know that we have truncated
		      ;; the file name.
		       #\CENTER-DOT
		       ;;ELSE
		       #\SPACE))
    (SHEET-TYO SHEET #\SPACE)

    (COND (DISPLAY-PERCENT-P
       (SHEET-STRING-OUT SHEET DISPLAY-FILE-TRANSFER-PERCENT-STRING)
       ;; Space between the % and the count.
       (IF DISPLAY-COUNT-P (SHEET-STRING-OUT SHEET " "))))
    (COND (DISPLAY-COUNT-P
           (SHEET-STRING-OUT SHEET DISPLAY-FILE-TRANSFER-COUNT-STRING)))))

(DEFUN FIXNUM-INTO-STRING (NUMBER STRING &OPTIONAL (RADIX 10.))
  "Store a prinout of NUMBER in RADIX into STRING.
STRING's contents are altered.  STRING is made longer if necessary."
  (SETF (ARRAY-LEADER STRING 0) 0)
  (DO ((NUM NUMBER (TRUNCATE NUM RADIX)))
      ((AND (ZEROP NUM)
            (NOT (ZEROP (ARRAY-ACTIVE-LENGTH STRING))))
       (NREVERSE (THE STRING (STRING STRING))))
    ;; Keep trying to push until we make array big enough to hold more.
    (DO () ((VECTOR-PUSH (+ #\0 (REM NUM RADIX)) STRING))
      (ADJUST-ARRAY STRING (+ 10. (ARRAY-TOTAL-SIZE STRING))))))

(DEFUN MAKE-IDLE-MESSAGE (MINUTES)
  (COND ((< MINUTES 60.)
     (FORMAT () "Console idle ~D minute~:P" MINUTES))
    ((< MINUTES (* 60. 24.))
     (LET* ((HOURS (TRUNCATE MINUTES 60.))
	    (REL-MINUTES (- MINUTES (* 60. HOURS))))
       (IF (ZEROP REL-MINUTES)
	   (FORMAT NIL "Console idle ~D hour~:P" HOURS)
	   ;;ELSE
	   (FORMAT NIL "Console idle ~D hour~:P ~D minute~:P" HOURS REL-MINUTES))))
    (T
     (LET* ((DAYS (TRUNCATE    MINUTES         (* 24. 60.)))
           (HOURS (TRUNCATE (- MINUTES (* DAYS (* 24. 60.))) 60.))
	   (REL-MINUTES     (- MINUTES         (* 24. 60. DAYS) (* 60. HOURS))))
       (COND ((AND (ZEROP HOURS) (ZEROP REL-MINUTES))
	      (FORMAT NIL "Console idle ~D day~:P" DAYS))
	     ((ZEROP HOURS)
	      (FORMAT NIL "Console idle ~D day~:P ~D minute~:P"
               DAYS REL-MINUTES))
	     ((ZEROP REL-MINUTES)
	      (FORMAT NIL "Console idle ~D day~:P ~D hour~:P"
		      DAYS HOURS))
	     (T
	      (FORMAT NIL "Console idle ~D day~:P ~D hour~:P ~D minute~:P"
		      DAYS HOURS REL-MINUTES)))))))

;;; Date and time in the who-line, continuously updating.

(DEFUN 12-HOUR-CLOCK-P ()
  (EQ CLOCK-TYPE :12-HOUR))

(DEFUN 12-HOUR-CLOCK-SETUP ()
  "Set up the WHO LINE clock to operate in 12 hour mode."
  (SETQ CLOCK-TYPE :12-HOUR)
  ;; Redo the status line according to the new clock type.
  (unless (si:addin-p)
    (INITIALIZE-STATUS-LINE)))

(DEFUN 24-HOUR-CLOCK-SETUP ()
  "Set up the WHO LINE clock to operate in 24 hour mode."
  (SETQ CLOCK-TYPE :24-HOUR)
  ;; Redo the status line according to the new clock type.
  (unless (si:addin-p)
    (INITIALIZE-STATUS-LINE)))

(DEFUN NWATCH-WHO-FUNCTION-12 (WHO-SHEET)
  "Update the 12 hour clock."
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (OR WHO-LINE-EXTRA-STATE
     (LET ((DEFAULT-CONS-AREA WHO-LINE-AREA))
       (SETQ WHO-LINE-EXTRA-STATE (STRING-APPEND "MM/DD/YY HH:MM:SSAM"))))
  (LET (YEAR MONTH DAY HOURS MINUTES SECONDS LEFTX)
    (MULTIPLE-VALUE-SETQ (SECONDS MINUTES HOURS DAY MONTH YEAR)
      (TIME:GET-TIME))
    (COND ((NULL SECONDS)
           (SHEET-SET-CURSORPOS WHO-SHEET 0 0)
           (SHEET-CLEAR-EOL WHO-SHEET)
           ;;                             0123456789012345678
           (REPLACE WHO-LINE-EXTRA-STATE "MM/DD/YY HH:MM:SSAM"))
          (T (SETQ YEAR (MOD YEAR 100.))
             (IF (> HOURS 11.)
                 (PROGN
                   (IF (> HOURS 12.)
                       (SETQ HOURS (- HOURS 12.)))
                   ;;; Put in the P from PM.
                   (SETF (AREF WHO-LINE-EXTRA-STATE 17.)
                         #\P))
                 ;;ELSE
                 (PROGN
                   (IF (= HOURS 0) (SETQ HOURS 12.))
                   (SETF (AREF WHO-LINE-EXTRA-STATE 17) #\A)))
             (SETQ LEFTX (MIN (NWATCH-N MONTH   WHO-LINE-EXTRA-STATE  0.)
                              (NWATCH-N DAY     WHO-LINE-EXTRA-STATE  3.)
                              (NWATCH-N YEAR    WHO-LINE-EXTRA-STATE  6.)
                              (NWATCH-N HOURS   WHO-LINE-EXTRA-STATE  9.)
                              (NWATCH-N MINUTES WHO-LINE-EXTRA-STATE 12.)
                              (NWATCH-N SECONDS WHO-LINE-EXTRA-STATE 15.)))
             (OR WHO-LINE-ITEM-STATE (SETQ LEFTX 0))    ;was clobbered, redisplay all.
             (SHEET-SET-CURSORPOS WHO-SHEET (* LEFTX CHAR-WIDTH) 0)
             (SHEET-CLEAR-EOL     WHO-SHEET)
             (SHEET-STRING-OUT    WHO-SHEET WHO-LINE-EXTRA-STATE LEFTX)
             (SETQ WHO-LINE-ITEM-STATE T)))))

(DEFUN NWATCH-WHO-FUNCTION-24 (WHO-SHEET)
  "Update the 24 hour clock."
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (OR WHO-LINE-EXTRA-STATE
     (LET ((DEFAULT-CONS-AREA WHO-LINE-AREA))
       (SETQ WHO-LINE-EXTRA-STATE (STRING-APPEND "MM/DD/YY HH:MM:SS"))))
  (LET (YEAR MONTH DAY HOURS MINUTES SECONDS LEFTX)
    (MULTIPLE-VALUE-SETQ (SECONDS MINUTES HOURS DAY MONTH YEAR)
      (TIME:GET-TIME))
    (COND ((NULL SECONDS)
           (SHEET-SET-CURSORPOS WHO-SHEET 0 0)
           (SHEET-CLEAR-EOL WHO-SHEET)
           ;;                             01234567890123456
           (REPLACE WHO-LINE-EXTRA-STATE "MM/DD/YY HH:MM:SS"))
          (T
           (SETQ YEAR (MOD YEAR 100.))
           (SETQ LEFTX (MIN (NWATCH-N MONTH   WHO-LINE-EXTRA-STATE  0.)
                            (NWATCH-N DAY     WHO-LINE-EXTRA-STATE  3.)
                            (NWATCH-N YEAR    WHO-LINE-EXTRA-STATE  6.)
                            (NWATCH-N HOURS   WHO-LINE-EXTRA-STATE  9.)
                            (NWATCH-N MINUTES WHO-LINE-EXTRA-STATE 12.)
                            (NWATCH-N SECONDS WHO-LINE-EXTRA-STATE 15.)))
           (OR WHO-LINE-ITEM-STATE (SETQ LEFTX 0))      ;was clobbered, redisplay all
           (SHEET-SET-CURSORPOS WHO-SHEET (* LEFTX CHAR-WIDTH) 0)
           (SHEET-CLEAR-EOL     WHO-SHEET)
           (SHEET-STRING-OUT    WHO-SHEET WHO-LINE-EXTRA-STATE LEFTX)
           (SETQ WHO-LINE-ITEM-STATE T)))))

(DEFUN NWATCH-N (N STR I)
  "Returns first character position changed."
  ;; All N's are 2 digits long.  Returns the index value into STR of the changed digit.
  (MULTIPLE-VALUE-BIND (DIG1 DIG2) (TRUNCATE N 10.)
    (SETF DIG1 (+ DIG1 #\0)
	  DIG2 (+ DIG2 #\0))
    (PROG1 (COND ((NOT (= (AREF STR     I)  DIG1)) I)
                 ((NOT (= (AREF STR (1+ I)) DIG2)) (1+ I))
                 (T (ARRAY-TOTAL-SIZE STR)))
           (SETF (AREF STR     I)  DIG1)
           (SETF (AREF STR (1+ I)) DIG2))))

;;; Support for documentation in the who line.

(DEFMETHOD (SHEET :WHO-LINE-DOCUMENTATION-STRING) () NIL)

;;; List of windows waiting for locks to print error notifications.
;;; DEFVAR in SHEET.
(PROCLAIM '(SPECIAL LOCKED-ERROR-WINDOWS))
(PROCLAIM '(SPECIAL PENDING-NOTIFICATIONS))

(DEFUN WHO-LINE-DOCUMENTATION-FUNCTION (WHO-SHEET)
  "This displays the who line documentation for the currently selected
window.  If the selected window is locked an error message is blinked
in the who line area.  Who line documentation may be either a string
of text which is to be displayed or a list of keyword value pairs.  To
see what keywords are accepted see the documentation for the
tv:process-who-line-documentation-list function."
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (LET* ((W MOUSE-WINDOW)
	 (MSG  "*** Error with window locked; try TERMINAL C-CLEAR-INPUT or TERMINAL C-C ***")
	 (MSG1 "    Error with window locked; try TERMINAL C-CLEAR-INPUT or TERMINAL C-C")
	 (NEW-STATE (COND (LOCKED-ERROR-WINDOWS
			    ;; To attract attention, make this message blink.
			    ;; The following EQ test should remain as EQ in spite of what the
			    ;; compiler says.  In this case EQ is both faster and correct.
			    (IF (compiler-let ((inhibit-style-warnings-switch t))
				  (EQ WHO-LINE-ITEM-STATE MSG))	;Compiler-let added by PMH 7/2/87
				MSG1 MSG))
                          ((SYMBOLP W)
                           (AND W WHO-LINE-MOUSE-GRABBED-DOCUMENTATION))
                          (T (MULTIPLE-VALUE-BIND (DOC ERROR)
                                 (CATCH-ERROR
                                   (FUNCALL W :WHO-LINE-DOCUMENTATION-STRING)
                                   NIL)
                               (IF ERROR
                                   "Error getting documentation string" DOC))))))
    (COND ((AND (NEQ WHO-LINE-ITEM-STATE NEW-STATE)
                (NOT (EQUAL WHO-LINE-ITEM-STATE NEW-STATE)))
           (SETQ WHO-LINE-ITEM-STATE NEW-STATE)
           (SHEET-CLEAR WHO-SHEET)
           (SHEET-SET-CURSORPOS WHO-SHEET 2 1)
           (CATCH-ALL
             (COND ((TYPEP NEW-STATE 'STRING)
                    (CATCH 'PAGE-OVERFLOW
                      (SHEET-STRING-OUT WHO-SHEET NEW-STATE)))
                   ((CONSP NEW-STATE)
                    (PROCESS-WHO-LINE-DOCUMENTATION-LIST WHO-SHEET NEW-STATE))
                   (T (CATCH 'PAGE-OVERFLOW
                        (PROCESS-WHO-LINE-DOCUMENTATION-LIST
                          WHO-SHEET
                          '(:MOUSE-R-1 "Bring up the System Menu."))))))))))

(DEFFLAVOR WHO-LINE-WINDOW () (WHO-LINE-MIXIN WINDOW))

(DEFMETHOD (WHO-LINE-WINDOW :UPDATE) ())

(DEFUN MAKE-WHO-LINE-WINDOW (&REST ARGS)
  "Create a window to be part of the who line.
ARGS are keyword args passed to MAKE-WINDOW.  The keyword
:FLAVOR specifies the window flavor (default is
TV:WHO-LINE-WINDOW).  The window's superior is always
WHO-LINE-SCREEN."
  (APPLY #'MAKE-WINDOW (OR (GETF ARGS :FLAVOR) 'WHO-LINE-WINDOW)
	 :AREA     WHO-LINE-AREA
	 :SUPERIOR WHO-LINE-SCREEN
	 :background-color *default-status-background*
	 ARGS))

(sys:declare-suggestions-for
  'TV:SET-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES
  :around
  '(locally (declare (special sys:suggestions-frame))
	    (cond ((variable-boundp sys:suggestions-frame)
		   (let ((values (sys:sugg-before-set-number-of-who-line-documentation-lines)))
		     :do-it
		     (sys:sugg-after-set-number-of-who-line-documentation-lines values)))
		  (t :do-it))))

(DEFUN SET-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES
       (&OPTIONAL (WHO-LINE-DOC-LINES DEFAULT-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES)
                  (WHO-LINE-DOC-FONT (LIST *MOUSE-DOCUMENTATION-LINE-STANDARD-FONT*))
		  (WHO-VSP WHO-LINE-VSP)
        &AUX (WHO-LINE-DOC-FONT-HEIGHT
	       (FONT-CHAR-HEIGHT *MOUSE-DOCUMENTATION-LINE-STANDARD-FONT*))
             (WHO-LINE-STATUS-FONT-HEIGHT
	       (FONT-CHAR-HEIGHT *STATUS-LINE-STANDARD-FONT* )) OSW)
  "Change the number or character lines in the who line documentation window.
The height of the who-line screen is limited to half of the physical screen height.
The font and vertical spacing for the who line documentation
window is also set.  The font parameter may be a list of fonts which
are to be put in the font map, an array which is to be used as the new
font map, or a single font which is to be used."
 (if (si:addin-p)
  (tv:notify nil  "Setting the number of mouse documentation lines is not supported on this machine.")
  (SYS:WITH-SUGGESTIONS-MENUS-FOR TV:SET-NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES
    (SETQ WHO-LINE-DOC-LINES (MIN WHO-LINE-DOC-LINES (TRUNCATE (TRUNCATE MAIN-SCREEN-HEIGHT 2)
                                                               (+ WHO-LINE-DOC-FONT-HEIGHT WHO-VSP))))
    (SETQ NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES WHO-LINE-DOC-LINES)
    ;;
    ;; Make sure that the specified font is in the right format for the
    ;; change-font-map message. huh? -mdm
    ;;
    (COND ((FONT-OBJECT-P WHO-LINE-DOC-FONT)
           (SETQ WHO-LINE-DOC-FONT (LIST WHO-LINE-DOC-FONT)))
          ((NOT (MEMBER (TYPE-OF WHO-LINE-DOC-FONT) '(ARRAY LIST CONS) :TEST #'EQ)) ;; may 9-9-88 list was keywords
           (SETQ WHO-LINE-DOC-FONT (LIST *MOUSE-DOCUMENTATION-LINE-STANDARD-FONT*))))
    ;; We are turning off much of the window system here.  This means that
    ;; we need to make sure that any errors still get displayed for the user
    ;; to see, even if only momentarily.
    ;; Double Hack Alert!
    ;; The following WHEN test is wrapped around the TYPEP tests for Zmacs windows to
    ;; avoid an error when ZMACS is not in the world (e.g. during bare band builds).
    ;; Remove the entire WHEN form when the bug that causes system shutdown
    ;; is fixed. The bug is when this function runs in zmacs.
    ;; also remove the (SEND OSW :SELECT) at end of function. 
    (WHEN (MEMBER 'ZWEI:ZMACS-FRAME *ALL-FLAVOR-NAMES* :TEST #'EQ)
      (WHEN (OR (TYPEP SELECTED-WINDOW 'ZWEI:ZMACS-WINDOW-PANE)
                (TYPEP SELECTED-WINDOW 'ZWEI:ZWEI-MINI-BUFFER))
         ;; The idea is that when this function is run from Zmacs we remember
         ;; where we came from (i.e. from Zmacs) and then go to a listener.
         ;; When we are all done, we go back to where we came from.
        (SETQ OSW SELECTED-WINDOW)                             ;moved these next 2 lines inside the second-level WHEN - DAN
        (SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'W:LISP-LISTENER)))
    (LET ((*ERROR-OUTPUT* SI:COLD-LOAD-STREAM))
      (WITH-MOUSE-USURPED
        (LOCK-SHEET (DEFAULT-SCREEN)
          (LOCK-SHEET (WHO-LINE-SCREEN)
            (WITHOUT-INTERRUPTS
              (CATCH-ALL
                (LET ((MS MOUSE-SHEET) (SW SELECTED-WINDOW))
                  (AND (SHEET-ME-OR-MY-KID-P MS DEFAULT-SCREEN)
                       (SETQ MOUSE-SHEET NIL))
                  (FUNCALL WHO-LINE-SCREEN :DEEXPOSE)
                  (FUNCALL DEFAULT-SCREEN :DEEXPOSE)
                  (SETQ MOUSE-SHEET MS)
                  (FUNCALL WHO-LINE-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                           :BOTTOM  (+ MAIN-SCREEN-HEIGHT main-screen-offset-for-proc)
                           :HEIGHT (+ WHO-VSP
                                      (* WHO-LINE-DOC-LINES
                                         (+ WHO-LINE-DOC-FONT-HEIGHT WHO-VSP))
                                      WHO-VSP
                                      WHO-LINE-STATUS-FONT-HEIGHT
                                      WHO-VSP))
                  (DOLIST (WIND (SEND WHO-LINE-SCREEN :INFERIORS))
                    (COND ((EQ WIND WHO-LINE-DOCUMENTATION-WINDOW)
                           (SEND WIND :SET-FONT-MAP WHO-LINE-DOC-FONT)
                           (SEND WIND :SET-VSP WHO-VSP)
                           (Send WIND :CHANGE-OF-SIZE-OR-MARGINS
                                 :HEIGHT (+ (* WHO-LINE-DOC-LINES
                                               (+ WHO-LINE-DOC-FONT-HEIGHT WHO-VSP)) WHO-VSP)
                                 :LEFT 0
                                 :RIGHT (SHEET-WIDTH          WHO-LINE-SCREEN)
                                 :TOP 0))
                          (T (SEND WIND :CHANGE-OF-SIZE-OR-MARGINS
                                   :BOTTOM (SHEET-HEIGHT      WHO-LINE-SCREEN)
                                   :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)))))
                  (FUNCALL DEFAULT-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                           :HEIGHT (- MAIN-SCREEN-HEIGHT
                                      (SHEET-HEIGHT WHO-LINE-SCREEN))
			   ;; jlm 2/28/89
			   :BOTTOM (- (+ MAIN-SCREEN-HEIGHT main-screen-offset-for-proc) (SHEET-HEIGHT WHO-LINE-SCREEN)))
                  (FUNCALL DEFAULT-SCREEN :EXPOSE)
                  (MOUSE-SET-SHEET MS)
                  (FUNCALL WHO-LINE-SCREEN :EXPOSE)
                  (AND SW (FUNCALL SW :SELECT)))))))))
    (WHEN OSW
      (SEND OSW :SELECT))
    (SETQ WHO-LINE-DOCUMENTATION-LINE-HEIGHT
          (SHEET-LINE-HEIGHT WHO-LINE-DOCUMENTATION-WINDOW)))))

;;; These variables are used to communicate between the functions which
;;; perform who line documentation formatting.  If all of the formatting were
;;; placed into one (large) function, then these variables could be changed
;;; into locals.
(DEFVAR STRING-TOGETHER   NIL)
(DEFVAR NOT-FIRST?        NIL)
(DEFVAR OLD-WHO-LINE-FONT NIL)
(DEFVAR NEW-WHO-LINE-FONT NIL)
(DEFVAR MAXIMUM-WHO-LINE-MOUSE-X 0)
(DEFVAR MAXIMUM-WHO-LINE-MOUSE-Y 0)

;;; The following six variables indicate the location of the where the corresponding
;;; mouse click documentation is to be placed.  The all are in terms of pixels.  These
;;; are only used if the documentation is to be formatted.
(DEFVAR LEFT-CLICK-LOC   NIL)
(DEFVAR MIDDLE-CLICK-LOC NIL)
(DEFVAR RIGHT-CLICK-LOC  NIL)
(DEFVAR MOUSE-SINGLE-LOC NIL)
(DEFVAR MOUSE-DOUBLE-LOC NIL)
(DEFVAR MOUSE-HOLD-LOC   NIL)

(DEFUN DISPLAY-WHO-LINE-MOUSE-INFO (WHO-SHEET MOUSE-KEYWORD DOCUMENTATION COMMA)
  "Display mouse information in the who line documentation window."
  ;; Do the things which need to be done before writing out the documentation string.
  (LET ((PAGE-OVERFLOW-ENCOUNTERED T)) 
    (COND (STRING-TOGETHER
           ;; If we are stringing everything together and this is not the first line
           ;; then we need to output a comma to separate this string from the previous string.
           (when (and NOT-FIRST? (not comma))            ;; if not-first? is T and the value of  :no-comma is nil, 
	     (SHEET-STRING-OUT WHO-SHEET ", ")))   ;; then we output a comma.
          (T
           ;; If we are formatting the lines, then we need to position the cursor to the correct place.
           (APPLY 'SHEET-SET-CURSORPOS WHO-SHEET
                  (CASE MOUSE-KEYWORD
                        ((:MOUSE-ANY                 ) `(,LEFT-CLICK-LOC   ,MOUSE-SINGLE-LOC))
                        ((:MOUSE-1-1    :MOUSE-L-1   ) `(,LEFT-CLICK-LOC   ,MOUSE-SINGLE-LOC))
                        ((:MOUSE-1-2    :MOUSE-L-2   ) `(,LEFT-CLICK-LOC   ,MOUSE-DOUBLE-LOC))
                        ((:MOUSE-1-HOLD :MOUSE-L-HOLD) `(,LEFT-CLICK-LOC   ,MOUSE-HOLD-LOC  ))
                        ((:MOUSE-2-1    :MOUSE-M-1   ) `(,MIDDLE-CLICK-LOC ,MOUSE-SINGLE-LOC))
                        ((:MOUSE-2-2    :MOUSE-M-2   ) `(,MIDDLE-CLICK-LOC ,MOUSE-DOUBLE-LOC))
                        ((:MOUSE-2-HOLD :MOUSE-M-HOLD) `(,MIDDLE-CLICK-LOC ,MOUSE-HOLD-LOC  ))
                        ((:MOUSE-3-1    :MOUSE-R-1   ) `(,RIGHT-CLICK-LOC  ,MOUSE-SINGLE-LOC))
                        ((:MOUSE-3-2    :MOUSE-R-2   ) `(,RIGHT-CLICK-LOC  ,MOUSE-DOUBLE-LOC))
                        ((:MOUSE-3-HOLD :MOUSE-R-HOLD) `(,RIGHT-CLICK-LOC  ,MOUSE-HOLD-LOC  ))))))
    ;; We change the font for the mouse prefix to distinguish the prefix from the mouse documentation.
    (SEND WHO-SHEET :SET-CURRENT-FONT *MOUSE-DOCUMENTATION-LINE-BUTTONS-STANDARD-FONT* T)
    (CATCH 'PAGE-OVERFLOW
      (SHEET-STRING-OUT WHO-SHEET
			(OR (CADR (IF (EQ MOUSE-HANDEDNESS :LEFT)
                                      (ASSOC MOUSE-KEYWORD
                                             '((:MOUSE-ANY "L,M,R") (:ANY "L,M,R")
                                               (:MOUSE-R-1 "L")   (:MOUSE-R-2 "L2")   (:MOUSE-R-HOLD "LH")
                                               (:MOUSE-3-1 "L")   (:MOUSE-3-2 "L2")
                                               (:MOUSE-M-1 "M")   (:MOUSE-M-2 "M2")   (:MOUSE-M-HOLD "MH")
                                               (:MOUSE-2-1 "M")   (:MOUSE-2-2 "M2")
                                               (:MOUSE-L-1 "R")   (:MOUSE-L-2 "R2")   (:MOUSE-L-HOLD "RH")
                                               (:MOUSE-1-1 "R")   (:MOUSE-1-2 "R2")) :TEST #'EQ)
                                      ;;ELSE
                                      (ASSOC MOUSE-KEYWORD
                                             '((:MOUSE-ANY "L,M,R") (:ANY "L,M,R")
                                               (:MOUSE-R-1 "R")   (:MOUSE-R-2 "R2")   (:MOUSE-R-HOLD "RH")
                                               (:MOUSE-3-1 "R")   (:MOUSE-3-2 "R2")
                                               (:MOUSE-M-1 "M")   (:MOUSE-M-2 "M2")   (:MOUSE-M-HOLD "MH")
                                               (:MOUSE-2-1 "M")   (:MOUSE-2-2 "M2")
                                               (:MOUSE-L-1 "L")   (:MOUSE-L-2 "L2")   (:MOUSE-L-HOLD "LH")
                                               (:MOUSE-1-1 "L")   (:MOUSE-1-2 "L2")) :TEST #'EQ)))
                            ;; If the caller specified an illegal mouse button
                            ;; then use the following string as the mouse prefix.
                            "Bad doc keyword"))
      (SHEET-STRING-OUT WHO-SHEET ": " 0
			(IF (STRING-EQUAL "" DOCUMENTATION)
                            ;; If the documentation for this button is empty then we do
                            ;; not want to have the space after the mouse prefix.  In
                            ;; this case there are two mouse buttons which do the same
                            ;; thing.  The next mouse button will have the documentation
                            ;; for this mouse button.  See the EDIT SCREEN menu item of
                            ;; the System Menu for an example of this.
			    ;; may 9-9-88 NOTE:
			    ;; Above reference to EDIT-SCREEN is/was no longer true.
			    ;; No doc on this "feature" exists and it seems to be
			    ;; pretty much worthless.
                            1
                            ;;ELSE
                            NIL))
      (SEND WHO-SHEET :SET-CURRENT-FONT NEW-WHO-LINE-FONT T)
      (SHEET-STRING-OUT WHO-SHEET DOCUMENTATION)
      (SETQ PAGE-OVERFLOW-ENCOUNTERED NIL))

    (IF PAGE-OVERFLOW-ENCOUNTERED
      (SETQ MAXIMUM-WHO-LINE-MOUSE-X (SHEET-INSIDE-WIDTH WHO-SHEET))
      ;;ELSE
      (WHEN (>= (SHEET-CURSOR-Y WHO-SHEET) MAXIMUM-WHO-LINE-MOUSE-Y)
	(SETQ MAXIMUM-WHO-LINE-MOUSE-Y (SHEET-CURSOR-Y WHO-SHEET)
	      MAXIMUM-WHO-LINE-MOUSE-X (SHEET-CURSOR-X WHO-SHEET))))))

(DEFUN PROCESS-WHO-LINE-DOCUMENTATION-LIST (WHO-SHEET NEW-STATE) 
  "This function displays who line mouse documentation from a keyword
list.  The list is organized in keyword, value pairs.  That is each odd
item is a keyword and the following item is the value for that keyword.
The keywords that are recognized are:

	:DOCUMENTATION	a general documentation string which will be displayed
			below the mouse button documentation lines.
	:KEYSTROKE	used to indicate that a particular keystroke corresponds
			to this who line documentation.  This can be either a
			a string or a character.  For effeciency reasons it is
			recommended that this be a string.
        :NO-COMMA       if in list, then items in who line documentation are not
                        delimited by a comma. Users are responsible for delimiters.
	:FONT		cause the following items in the list to be displayed in
			the specified font.  The font is not required to be in
			the font map for the who line documentation window.
	:MOUSE-ANY	documentation for clicking ANY mouse button clicked once.
	:MOUSE-L-1	documentation for the left   mouse button clicked once.
	:MOUSE-L-2	documentation for the left   mouse button clicked twice.
	:MOUSE-L-HOLD   documentation for the left   mouse button held down.
	:MOUSE-M-1	documentation for the middle mouse button clicked once.
	:MOUSE-M-2	documentation for the middle mouse button clicked twice.
	:MOUSE-M-HOLD   documentation for the middle mouse button held down.
	:MOUSE-R-1	documentation for the right  mouse button clicked once
	:MOUSE-R-2	documentation for the right  mouse button clicked twice.
	:MOUSE-R-HOLD   documentation for the right  mouse button held down.
        

The documentation display varies based on how many who line
documentation lines are available.  If there are less than three lines all
of the mouse documentation is squeezed onto one line instead of being
displayed at separate locations."
  ;;; Note that the old forms of the mouse keywords are supported too.
  ;;; That is writing :MOUSE-1-1 instead of :MOUSE-L-1.  This is done
  ;;; only in case someone out there is using that form.  We want users
  ;;; to use the newer form because it makes for better documentation,
  ;;; that is why we do not advertise the old form in the documentation
  ;;; line for this function.
  (LET ((HAVE-DOCUMENTATION (OR (MEMBER  :DOCUMENTATION NEW-STATE :TEST #'EQ)
                                (MEMBER :KEYSTROKE     NEW-STATE :TEST #'EQ)))
			
	;; when comma is nil, we will NOT output a comma in documentation line. 
	(COMMA (MEMBER :NO-COMMA NEW-STATE :TEST #'EQ)))
	;; We string the documentation components together in certain cases.  If there
	;; is only 1 or 2 lines then there isn't much choice.  If there are 3 lines,
	;; then we also have to have a :DOCUMENTATION component too.  If there are 4 or
	;; more lines then we can display the documentation in a 3 column format.
    (SETQ STRING-TOGETHER (OR t  ;; may 9-9-88
			      ;; 
			      ;; *** HACK ALERT - When (3)4 or more doc lines exist, special
			      ;; formatting was done to put the 9 possible keys in 3 rows
			      ;; of 3 columns. Problem with this is that long strings will
			      ;; get trashed ( overwritten ) even though the # of wholine
			      ;; lines is GREATER then before. This was a good idea that
			      ;; is just unworkable for the benefit it adds. We can't expect
			      ;; every doc-string to be tested for multiple conditions of
			      ;; who-line lines. As a result much of this code is now obsolete
			      (= NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES 1)
                              (= NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES 2)
                              (AND HAVE-DOCUMENTATION
                                   (= NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES 3))))

    ;; Initialize constants for this function execution.
    (SETQ LEFT-CLICK-LOC 2
	  MIDDLE-CLICK-LOC (IF (NOT STRING-TOGETHER) (TRUNCATE (SHEET-INSIDE-WIDTH WHO-SHEET) 3))
	  RIGHT-CLICK-LOC (IF (NOT STRING-TOGETHER)
                              (- (SHEET-INSIDE-WIDTH WHO-SHEET) MIDDLE-CLICK-LOC))
	  MOUSE-SINGLE-LOC 2
	  MOUSE-DOUBLE-LOC (AND (NOT STRING-TOGETHER)
                                (IF (OR (MEMBER :MOUSE-L-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-M-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-R-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-1-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-2-1 NEW-STATE :TEST #'EQ)
                                        (MEMBER :MOUSE-3-1 NEW-STATE :TEST #'EQ))
                                    ;; We have single click info, put this on the second line.
                                    (+ MOUSE-SINGLE-LOC WHO-LINE-DOCUMENTATION-LINE-HEIGHT)
                                    ;;ELSE Don't have single click info, put this on the first line.
                                    MOUSE-SINGLE-LOC))
	  MOUSE-HOLD-LOC (AND
                           (NOT STRING-TOGETHER)
                           (IF (OR (MEMBER :MOUSE-L-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-M-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-R-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-1-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-2-2 NEW-STATE :TEST #'EQ)
                                   (MEMBER :MOUSE-3-2 NEW-STATE :TEST #'EQ))
                               ;; We have both double click info.  The hold info must on the line after that.
                               (+ MOUSE-DOUBLE-LOC WHO-LINE-DOCUMENTATION-LINE-HEIGHT)
                               ;;ELSE
                               (IF (NOT (= MOUSE-SINGLE-LOC MOUSE-DOUBLE-LOC))
                                   ;; There was single click info. put this after that.
                                   (+ MOUSE-SINGLE-LOC WHO-LINE-DOCUMENTATION-LINE-HEIGHT)
                                   ;; ELSE This is the only mouse documentation. put on the first line.
                                   MOUSE-SINGLE-LOC))))

    (SETQ OLD-WHO-LINE-FONT        (GET-DEFAULT-FONT WHO-SHEET)
	  NEW-WHO-LINE-FONT        OLD-WHO-LINE-FONT
	  NOT-FIRST?               NIL
	  MAXIMUM-WHO-LINE-MOUSE-X 0
	  MAXIMUM-WHO-LINE-MOUSE-Y 0)


    ;; This loops through all of the non-documentation keywords.  We process them first so we can put the
    ;; documentation strings towards the bottom of the window.  If we didn't then we might intersperse them.
    (LOOP FOR DOC-SPEC = NEW-STATE THEN (CDDR DOC-SPEC)
          WHILE DOC-SPEC
          FOR OLD-KEY = NIL THEN KEY
          FOR KEY     = (FIRST  DOC-SPEC)
          FOR VALUE   = (SECOND DOC-SPEC)
          FINALLY (UNLESS (EQ (SHEET-CURRENT-FONT WHO-SHEET) OLD-WHO-LINE-FONT)
                    (SEND WHO-SHEET :SET-CURRENT-FONT OLD-WHO-LINE-FONT))
          DO
          (PROGN
            (WHEN (AND (NOT NOT-FIRST?) OLD-KEY)
              (SETQ NOT-FIRST? (AND (NOT (EQ OLD-KEY :FONT))
                                    (NOT (EQ OLD-KEY :KEYSTROKE))
				   ;; (NOT (EQ OLD-KEY :NO-COMMA))
                                    (NOT (EQ OLD-KEY :DOCUMENTATION)))))
            (IF (EQ KEY :FONT)
                (PROGN
                  ;; Change the current font.  The T argument says to
                  ;; change the font even if it isn't in the FONT-MAP.
                  (SEND WHO-SHEET :SET-CURRENT-FONT VALUE T)
                  (SETQ NEW-WHO-LINE-FONT VALUE))
                ;;ELSE
                (IF (AND (NOT (EQ KEY :KEYSTROKE))
                         (NOT (EQ KEY :DOCUMENTATION))
			 (not (eq key :no-comma)))
                    (DISPLAY-WHO-LINE-MOUSE-INFO WHO-SHEET KEY VALUE COMMA)))))

    (WHEN HAVE-DOCUMENTATION
      (SHEET-SET-CURSORPOS WHO-SHEET MAXIMUM-WHO-LINE-MOUSE-X MAXIMUM-WHO-LINE-MOUSE-Y)
      ;; If the mouse info wraps onto the last line available then we start the :DOCUMENTATION info
      ;; there.  Otherwise we put the :DOCUMENTATION on the next line.
      (SETQ NOT-FIRST? (AND STRING-TOGETHER
                            (= (1+ (TRUNCATE MAXIMUM-WHO-LINE-MOUSE-Y WHO-LINE-DOCUMENTATION-LINE-HEIGHT))
                               NUMBER-OF-WHO-LINE-DOCUMENTATION-LINES)
                            (NOT (ZEROP MAXIMUM-WHO-LINE-MOUSE-X)))) 

      (SETQ NEW-WHO-LINE-FONT (GET-DEFAULT-FONT WHO-SHEET))
      (CATCH 'PAGE-OVERFLOW
	(WHEN (NOT NOT-FIRST?)
	  (SEND WHO-SHEET :FRESH-LINE))
	;; Now we loop through again to get all of the :DOCUMENTATION info.
	(LOOP FOR DOCUMENTATION-KEYWORD IN '(:DOCUMENTATION :KEYSTROKE) ;;;:no-comma)
	      DO
	      (LOOP FOR DOC-SPEC = NEW-STATE THEN (CDDR DOC-SPEC)
		    WHILE DOC-SPEC
		    WITH OLD-KEY = NIL
		    FOR KEY      = (FIRST  DOC-SPEC)
		    FOR VALUE    = (SECOND DOC-SPEC)
		    FINALLY (UNLESS (EQ (SHEET-CURRENT-FONT WHO-SHEET) OLD-WHO-LINE-FONT)
			      (SEND WHO-SHEET :SET-CURRENT-FONT OLD-WHO-LINE-FONT))
		    WHEN (OR (EQ KEY :FONT) (EQ KEY DOCUMENTATION-KEYWORD))
		    DO
		    (PROGN
		      (IF (EQ KEY :FONT)
			  (PROGN
			    ;; Change the current font.  The T argument says to
			    ;; change the font even if it isn't in the FONT-MAP.
			    (SEND WHO-SHEET :SET-CURRENT-FONT VALUE T)
			    (SETQ NEW-WHO-LINE-FONT VALUE))
			  ;;ELSE
			  (WHEN (NOT (EQ KEY :FONT))
				(when (and NOT-FIRST? (not comma))
				       (SHEET-STRING-OUT WHO-SHEET ",  ")
				       ;;else
				    (SETQ NOT-FIRST? T)))
			    (WHEN (EQ KEY :KEYSTROKE)
			      (SEND WHO-SHEET :SET-CURRENT-FONT *MOUSE-DOCUMENTATION-LINE-BUTTONS-STANDARD-FONT* T)
			      (SHEET-STRING-OUT WHO-SHEET "Keystroke: ")
			      ;; Make sure the value is a string.
			      (WHEN (OR (CHARACTERP VALUE) (INTEGERP VALUE))
				(SETQ VALUE (FORMAT NIL "~:C" VALUE)))
			      (SEND WHO-SHEET :SET-CURRENT-FONT NEW-WHO-LINE-FONT T))
			    (SHEET-STRING-OUT WHO-SHEET VALUE))
		      (SETQ OLD-KEY KEY))
		    ))))))

(DEFMETHOD (WHO-LINE-SHEET :BEFORE :END-OF-PAGE-EXCEPTION) ()
  "This truncates text typeout in the who line documentation window.
We do not want the text in the who line documentation window to
wrap around from the last line to the first since there is no EASY way
to do more processing in the who line area, so we just truncate at the
bottom of the who line documentation window by throwing to the
tv:page-overflow tag."
  (OR (ZEROP (SHEET-END-PAGE-FLAG))
      (NEQ SELF WHO-LINE-DOCUMENTATION-WINDOW)
      ;; If the mouse documentation window (also known as the who-line-documentation-window) has
      ;; output displayed on it which doesn't go through the normal window system code (for example
      ;; (format tv:who-line-documentation-window "~%hi~%hi~%hi~%hi~%hi")) then we don't want to
      ;; get an error during the throw.
      (IGNORE-ERRORS (THROW 'PAGE-OVERFLOW T))))
