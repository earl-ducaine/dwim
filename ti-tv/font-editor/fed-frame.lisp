;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10B); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; CHANGE HISTORY
;;; 6/22/87   DKM  -  Change :around :fetch-and-execute method to better handle typeout windows
;;;                      -  Change :handle-mouse-input to not accept blips anywhere except from fed pane
;;;                      -  Add :who-line-documentation-string method to give correct system menu mouse doc

(DEFUN FED ()
  "Select a font editor window."
  (TV:SELECT-OR-CREATE-WINDOW-OF-FLAVOR 'FED-FRAME)
  :SELECT) 

(DEFMETHOD (FED-FRAME :AFTER :INIT) (&REST IGNORE)
;;; This seems to be the thing required to make selection win.
  (setq fed-frame self
        fed-pane (SEND SELF :GET-PANE 'fed-window)
	typeout-pane (SEND fed-pane :typeout-window)
	prompt-pane (SEND SELF :GET-PANE 'prompt-window)
	label-pane (SEND SELF :GET-PANE 'label-window))
  (SEND SELF :SELECT-PANE prompt-pane)
  (SEND label-pane :SET-FED-WINDOW fed-pane)
  (SEND fed-pane :SET-LABEL-WINDOW label-pane)
  (SEND fed-pane :SET-PROMPT-WINDOW prompt-pane)
  (SEND (SEND self :get-pane 'fed-title-window) :set-label
	`(:string ,(STRING-APPEND "font editor - frame "
				  (STRING-LEFT-TRIM "Fed Frame " (SEND self :name)))
		  :font fonts:tr8b))
  (colorize-fed-frame))

(defun colorize-fed-frame ()
  (declare (:self-flavor fed-frame))
  (setf (tv:label-font (send (send self :get-pane 'fed-title-window) :label))
	fonts:cptfontb)
  (setf (tv:label-color (send (send self :get-pane 'fed-title-window) :label))
	w:black)
  (setf (tv:label-background (send (send self :get-pane 'fed-title-window) :label))
	w:75%-gray-color)
  (setf (tv:sheet-background-color label-pane) w:25%-gray-color)
  (setf (tv:sheet-background-color prompt-pane) w:25%-gray-color))

(DEFMETHOD (FED-FRAME :BEFORE :INIT) (&REST IGNORE)
  "Set up configuration according to whether we have a wide or tall screen."
  (IF (> (TV:SHEET-HEIGHT TV:DEFAULT-SCREEN) (TV:SHEET-WIDTH TV:DEFAULT-SCREEN))

      ;;configuration for tall screens.
      (SETQ TV:CONSTRAINTS 
	    '((main . ((fed-exit
			 help-doc
			 help-menu 
			 editor-title-menus 
           		 editor-action-menus
			 fed-window 
			 label-window 
			 register-titles
			 registers
			 prompt-window
			 fed-title-window)
		       ((fed-title-window 0 :lines))
		       ((prompt-window 5 :lines))
		       ((label-window :ask :label-window-size))
           	       ((register-titles :horizontal (1 :lines register-title)
				(register-title all-registers-menu)
				((all-registers-menu 20 :characters))
				((register-title :even))))
		       ((registers :horizontal (:eval (send self :register-height))
				(register-pane-0 register-pane-1 register-pane-2
				 register-pane-3 register-pane-4 register-pane-5
				 register-pane-6 register-pane-7)
				((register-pane-0 :even)
				 (register-pane-1 :even)
				 (register-pane-2 :even)
				 (register-pane-3 :even)
				 (register-pane-4 :even)
				 (register-pane-5 :even)
				 (register-pane-6 :even)
				 (register-pane-7 :even))))
		       ((fed-exit 1 :lines))
		       ((help-doc 1 :lines))
		       ((help-menu 2 :lines))
		       ((editor-title-menus :horizontal (1 :lines font-io-doc)
				(font-io-doc char-io-doc editing-doc screen-doc)
				((font-io-doc 0.33s0)
				 (char-io-doc 0.17s0)
				 (editing-doc 0.33s0))
				((screen-doc :even))))
		       ((editor-action-menus :horizontal (16 :lines font-io-menu)
				(font-io-menu char-io-menu editing-menu screen-menu)
				((font-io-menu 0.33s0)
				 (char-io-menu 0.17s0)
				 (editing-menu 0.33s0))
				((screen-menu :even))))
		       ((fed-window :even))))))
    ;;configuration for wide screens.
    (SETQ TV:CONSTRAINTS 
	  '((MAIN . ((all-windows fed-title-window)
		     ((fed-title-window 0 :lines))
		     ((all-windows :horizontal (:even)
			(FED-WINDOW-and-REGISTERS-and-LABEL-WINDOW-and-PROMPT-WINDOW MENUS)
		        ((MENUS :vertical (0.25S0 :characters font-io-menu)
				(font-io-doc font-io-menu char-io-doc char-io-menu
				 editing-doc editing-menu screen-doc screen-menu
				 help-doc help-menu  fed-exit)
				;;; specifying number of lines for constraints doesn't seem to work.
				;;; the following percentages were used to get the menus looking right.
				((font-io-doc 1 :lines)
				 (font-io-menu 0.18S0)
				 (char-io-doc 1 :lines)
				 (char-io-menu 0.10S0)
				 (editing-doc 1 :lines)
				 (editing-menu 0.28S0)
				 (screen-doc 1 :lines)
				 (screen-menu 0.11S0)
				 (help-doc 1 :lines) 
				 (help-menu 0.15S0)
				 (fed-exit 1 :lines))))
			((FED-WINDOW-and-REGISTERS-and-LABEL-WINDOW-and-PROMPT-WINDOW :VERTICAL (:even)
				(FED-WINDOW-and-REGISTERS LABEL-WINDOW PROMPT-WINDOW)
				((PROMPT-WINDOW 5 :lines))
				((LABEL-WINDOW :ASK :LABEL-WINDOW-SIZE))
				((FED-WINDOW-and-REGISTERS :horizontal (:even)
					(registers-and-register-title fed-window)		   
						((REGISTERS-AND-REGISTER-TITLE :vertical (0.15s0)
							    (register-title REGISTER-PANE-0 REGISTER-PANE-1
									    REGISTER-PANE-2 REGISTER-PANE-3
									    REGISTER-PANE-4 REGISTER-PANE-5
									    REGISTER-PANE-6 REGISTER-PANE-7
									    all-registers-menu)
							    ((register-title 1 :lines))
							    ((all-registers-menu 2 :lines))
							    ((REGISTER-PANE-0 :EVEN)
							     (REGISTER-PANE-1 :EVEN)
							     (REGISTER-PANE-2 :EVEN)
							     (REGISTER-PANE-3 :EVEN)
							     (REGISTER-PANE-4 :EVEN)
							     (REGISTER-PANE-5 :EVEN)
							     (REGISTER-PANE-6 :EVEN)
							     (REGISTER-PANE-7 :EVEN))))
						((fed-window :even))						
					        ))))			
		        ))))))))

(DEFMETHOD (FED-FRAME :NAME-FOR-SELECTION) ()
   (DECLARE (:self-flavor FED-FRAME))
  "This is needed to override the default choice of the frame label which will have the version in it."
  (STRING-APPEND "font editor - frame " (STRING-LEFT-TRIM "Fed Frame " (SEND self :name))))


(DEFMETHOD (FED-FRAME :DESIGNATE-IO-STREAMS) ()
  "Redefine this ucl method so that the Ucl doesn't rebind the io."
  T) 


(DEFMETHOD (FED-FRAME :AROUND :HANDLE-PROMPT) (CONT MT IGNORE)
  "Make sure io is bound to the prompt window when displaying the prompt."
  (LET ((*TERMINAL-IO* (SEND SELF :GET-PANE 'PROMPT-WINDOW)))
    (FUNCALL-WITH-MAPPING-TABLE CONT MT :HANDLE-PROMPT))) 


(DEFMETHOD (FED :COMMAND-LOOP) (&AUX (*TERMINAL-IO* TV:TYPEOUT-WINDOW))
  "Redefine this old method to call the ucl command loop.  Note that this
   :command-loop method is not the ucl :command-loop method and is only
   for the fed pane, not the fed frame."
   ;; Wait for this sheet to be exposed before output.
  (PROCESS-WAIT "Expose" #'FIRST (LOCF (TV:SHEET-EXPOSED-P TV:SUPERIOR)))
  (PROG (COMMAND-CHAR
	 COMMAND
	 NUMERIC-ARG
	 NUMERIC-ARG-P)
     (DECLARE (SPECIAL COMMAND-CHAR COMMAND NUMERIC-ARG NUMERIC-ARG-P))
     (SEND TV:SUPERIOR :COMMAND-LOOP)))  


(DEFMETHOD (FED-FRAME :BEFORE :LOOP) ()
;; Queue the list fonts command when entering the loop.  This needs to look like a blip
;; because the format of the display is different (better) if initiated via the mouse.
  (SEND FED-PANE :FORCE-KBD-INPUT `(:TYPEOUT-EXECUTE :LIST-FONTS)))

;;; This also is a new method.  I don't know of a good spot for it.  I guess
;;; it can go in the fed-frame file - as it has a couple of other UCL methods
;;; defined there.  Again, this forces the correct who line for which mouse
;;; button brings up the system menu.

(DEFMETHOD (UCL:command-and-lisp-typein-window :WHO-LINE-DOCUMENTATION-STRING) ()
   '(:mouse-r-2 "System Menu"))


(DEFMETHOD (fed-frame :around :fetch-and-execute) (cont mt IGNORE)
  "Check for typeout, command abort etc."
  (SEND fed-pane :REDISPLAY)
  (SETQ NUMERIC-ARG 1
	NUMERIC-ARG-P NIL)	;; Give default numeric agument.
  (WHEN (EQ 'ucl:command-abort
	    (CATCH 'ucl:command-abort
	      (FUNCALL-WITH-MAPPING-TABLE cont mt :fetch-and-execute)))
	(SEND SELF :handle-prompt))

  ;; If the typeout pane still has text on it,
  (WHEN (SEND typeout-pane :INCOMPLETE-P) 
    (UNLESS (send typeout-pane :listen)    ;don't clear the screen here when input is available  dkm 5/87
	(TERPRI typeout-pane)
	(SEND typeout-pane :set-current-font 3)
	(FORMAT typeout-pane " ***  Press the space bar to remove this output.  ***    ")
	(SEND typeout-pane :set-current-font 0)
	(LET ((NEXTCH (SEND typeout-pane :READ-ANY)))
	  (and (eql nextch #\sp)           ;only redisplay when user typed in a space  dkm 5/87
	       (SEND fed-pane :REDISPLAY))
	  (WHEN (OR (CONSP nextch)
		    (NOT (EQL NEXTCH #\SP)))
	    (SEND fed-pane :UNREAD-ANY NEXTCH)        
	    )))))


(DEFMETHOD (FED-FRAME :FED-HANDLE-BLIP) ()
  "Handle special FED blips."
  (CASE (FIRST UCL:KBD-INPUT)
    (:TYPEOUT-EXECUTE
     (APPLY FED-PANE (SECOND UCL:KBD-INPUT) (CDDR UCL:KBD-INPUT))))) 


(DEFMETHOD (FED-FRAME :AROUND :HANDLE-TYPEIN-INPUT) (CONT MT IGNORE &OPTIONAL (UNTYI-FIRST-CHAR? T))
  "Make sure IO for typein is bound to the interactor pane."
  (LET* ((*TERMINAL-IO* PROMPT-PANE)
	 (*STANDARD-OUTPUT* *TERMINAL-IO*)
	 (*STANDARD-INPUT* *TERMINAL-IO*))
    (FUNCALL-WITH-MAPPING-TABLE CONT MT :HANDLE-TYPEIN-INPUT UNTYI-FIRST-CHAR?))) 


(DEFMETHOD (FED-FRAME :AROUND :HANDLE-KEY-INPUT) (CONT MT IGNORE &OPTIONAL (KEY UCL:KBD-INPUT))
  "Intercept this UCL method to check for numeric args."
  (SETQ UCL:INPUT-MECHANISM 'UCL:KEY-OR-BUTTON)	 ;from the UCL method.
  (SETQ KEY (CHAR-CODE KEY))
  (IF (<= (CHAR-CODE #\0) KEY (CHAR-CODE #\9))
      (SETQ NUMERIC-ARG (+ (IF NUMERIC-ARG-P
			       (* NUMERIC-ARG 10.)
			       0)
			   (- KEY (CHAR-CODE #\0)))
	    NUMERIC-ARG-P T)
      (FUNCALL-WITH-MAPPING-TABLE CONT MT :HANDLE-KEY-INPUT))) 


(DEFMETHOD (FED-FRAME :HANDLE-MOUSE-INPUT) ()
  "Redefine ucl method to handle mouse clicks. (not mouse sensitive items)"
  (IF (typep (third ucl:kbd-input) 'fed)                         ;only handle blips to FED window  -dkm 6/18
    (SEND (SEND SELF :GET-PANE 'FED-WINDOW) :FED-EXEC-CMD
	  (CASE (INT-CHAR  (SECOND UCL:KBD-INPUT))
	    (#\MOUSE-L 'COM-MOUSE-DRAW)
	    (#\MOUSE-M 'COM-MOUSE-CHANGE-DRAW-MODE)
	    (#\MOUSE-R 'COM-MOUSE-MOVE-CHAR-BOX)
	    (OTHERWISE (BARF)))
	  (FOURTH UCL:KBD-INPUT)				  ;Args are xpos and ypos at time
	  (FIFTH UCL:KBD-INPUT))				  ; of click, relative to window.
    ;;else
    (BARF)))


;Decide what scale to use for the register panes.
;The bigger the font, the smaller the scale.

(DEFMETHOD (FED-FRAME :REGISTER-BOX-SIZE) ()
  (LET ((CURFONT (SEND (SEND SELF :GET-PANE 'FED-WINDOW) :SELECTED-FONT)))
    (COND
      ((NULL CURFONT) 4)
      ((< (FD-LINE-SPACING (FONT-GET-FD CURFONT)) 36) 4)
      ((< (FD-LINE-SPACING (FONT-GET-FD CURFONT)) 55) 3)
      ((< (FD-LINE-SPACING (FONT-GET-FD CURFONT)) 74) 2)
      (T 1)))) 


(DEFMETHOD (FED-FRAME :REGISTER-HEIGHT) (&REST IGNORE)
 ;; Don't let the registers wipe out the whole frame height.
  (MIN (VALUES (FLOOR (* 0.2s0 TV:HEIGHT)))
       (+ 6
	  (* (SEND SELF :REGISTER-BOX-SIZE)
	     (LET ((MINHT (TV:FONT-CHAR-HEIGHT (TV:SCREEN-DEFAULT-FONT (TV:SHEET-GET-SCREEN SELF))))
		   (CURFNT (SEND (SEND SELF :GET-PANE 'FED-WINDOW) :SELECTED-FONT)))
	       (IF CURFNT
		 (MAX MINHT (FD-LINE-SPACING (FONT-GET-FD CURFNT)))
		 MINHT)))))) 


(DEFMETHOD (FED-FRAME :NUM-REGISTERS-ACROSS) (WIDTH &REST IGNORE)
  (LET ((REGWIDTH (VALUES (FLOOR (* 1.5s0 (SEND SELF :REGISTER-HEIGHT))))))
    (MAX MIN-REGISTERS (MIN MAX-REGISTERS (TRUNCATE WIDTH REGWIDTH))))) 

;;;*******************************************************************************************
;;; Put the Font Editor on the System Menu and on a System Key

(W:MODIFY-SYSTEM-ACCESS-SPEC 'FONT-EDITOR
			     :ASSIGN-DEFAULTS)

(SETF (SYS:SYSTEM-MADE-P 'FONT-EDITOR) T)

