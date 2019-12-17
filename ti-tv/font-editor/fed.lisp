;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10b); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.

;;; CHANGE HISTORY
;;;  12/17/86 MRR - Removed KST font support.  Changed com-read-file to use :font-symbol
;;;                     in file-attribute-list for fonts saved after Rel3.
;;;  12/23/86 MRR - Changed TV: references to W:
;;;                     - Com-mouse-draw-spline ignores FLIP mode and draws splines as if in DRAW mode.
;;;                     - Font-store-cd changed to only make negative kern values if really needed.
;;; 12/31/86 MRR - Fixed remove-fed-edited-char which wasn't working.
;;;   2/6/87   DKM - Changed com-display-font, com-list-fonts, and list-fonts-file-computer to
;;;                       to always write to the typeout pane, instead of whatever *standard-output* was
;;; 4/24/87  DKM - Changed display-label to handle large fonts better and mouse things in that window better.
;;;                    - Changed com-read-file so that reloading an existing font results in the FED getting the new font
;;  6/22/87  DKM - Fixed button-string to use :right instead of w:right
;;;                    - Fixed com-mouse-draw-spline to not barf when < 2 points selected.
;;;                     - Fixed com-read-file to not unintern font symbol when font already exists. Clobber plist instead.
;;;                     - Fixed remove-fed-edited-char to not care if a character wasn't found
;;;                     - Fixed select-new-mouse to correctly default cursor position over current glyph.
;;;                     - Fixed :who-line-documentation-string to have correct mouse doc

;;; Create the typeout window with multiple fonts capability
(DEFMETHOD (FED :AFTER :INIT) (&REST IGNORE)
;;
;; las changed to tv:typeout-window
  (OR TV:TYPEOUT-WINDOW
      (SETQ TV:TYPEOUT-WINDOW
	    (MAKE-INSTANCE
	      'FED-TYPEOUT-WINDOW
	      :ITEM-TYPE-ALIST
	      '((FONT :SELECT-FONT "Select this font for editing.")
		(LOAD-FONT :LOAD-FONT "Load this Font and select it for editing.")
		(CHARACTER :SELECT-CHAR "Select this character.")
		(DOCUMENT-DETAILS-FONT-IO :DOCUMENT-DETAILS-FONT-IO
 					  "Display additional documentation on this font-io command.")
		(DOCUMENT-DETAILS-CHARACTER-IO :DOCUMENT-DETAILS-CHARACTER-IO
					       "Display additional documentation on this character-io command.")
		(DOCUMENT-DETAILS-EDITING :DOCUMENT-DETAILS-EDITING
					  "Display additional documentation on this editing command.")
		(DOCUMENT-DETAILS-SCREEN :DOCUMENT-DETAILS-SCREEN
					 "Display additional documentation on this screen command."))
	      :DEEXPOSED-TYPEOUT-ACTION '(:EXPOSE-FOR-TYPEOUT)
	      :IO-BUFFER W:IO-BUFFER
	      :font-map (LIST fonts:cptfont fonts:hl12b fonts:tr8b fonts:hl12bi)
	      :SUPERIOR SELF)))
  (SETQ W:PROCESS (MAKE-PROCESS W:NAME NIL :SPECIAL-PDL-SIZE 4000. :regular-pdl-size 3500.))
  (PROCESS-PRESET W:PROCESS SELF :COMMAND-LOOP)
  (SEND W:PROCESS :RUN-REASON SELF))

(DEFMETHOD (FED :WHO-LINE-DOCUMENTATION-STRING) ()
  (OR SPECIAL-COMMAND-MOUSE-DOCUMENTATION
     (CASE DRAW-MODE
       (7
	'(:MOUSE-L-1 "Draw dots"
	  :MOUSE-M-1 "Change mode (Erase/Flip/Draw)" 
          :MOUSE-R-HOLD "Move edges of char box"
          :MOUSE-R-2  "System Menu"))
       (2
	'(:MOUSE-L-1 "Erase dots"
	  :MOUSE-M-1 "Change mode (Erase/Flip/Draw)" 
          :MOUSE-R-HOLD "Move edges of char box"
          :MOUSE-R-2  "System Menu"))

       (6
	'(:MOUSE-L-1 "Flip dots" 
	  :MOUSE-M-1 "Change mode (Erase/Flip/Draw)" 
          :MOUSE-R-HOLD "Move edges of char box"
          :MOUSE-R-2  "System Menu")))))


(DEFMETHOD (FED :FED-EXEC-CMD) (&REST CMD-AND-ARGS)
  "This method must be called by the ucl to execute any commands since some
    of the commands think that self is bound to the fed window."
  (DECLARE (SPECIAL UCL:KBD-INPUT))
  (SETQ COMMAND-CHAR ucl:kbd-input)
  (EVAL CMD-AND-ARGS)) 


(DEFMETHOD (FED :BEFORE :REDISPLAY) (&REST IGNORE)
  (COND
;;;
;;; las changed to tv:typeout-window
    ((SEND TV:TYPEOUT-WINDOW :ACTIVE-P)
     (SEND TV:TYPEOUT-WINDOW :MAKE-COMPLETE)
     (SEND TV:TYPEOUT-WINDOW :DEACTIVATE)
     (SEND W:SUPERIOR :SELECT-PANE (SEND W:SUPERIOR :GET-PANE 'PROMPT-WINDOW))
     (SETQ REDISPLAY-DEGREE REDISPLAY-ALL)))
  (COND
    ((> REDISPLAY-DEGREE REDISPLAY-NONE)
     (REDISPLAY-LABELS)))) 


;;; HANDLE CLICKS ON THE FED PANE VIA MOUSE OR CURSOR

;Don't check for double-clicks for left and middle clicks (don't make sense here).
;This makes the single clicks a bit faster.

(DEFMETHOD (FED :MOUSE-BUTTONS) (BD X Y)
  (IF (>= BD 4)					;right button was down
      (LET ((BUTTON (W:MOUSE-CHARACTER-BUTTON-ENCODE BD)))	;look for double click
	(IF (= BUTTON #\MOUSE-R-2)		;got one on right button
	    (W:MOUSE-CALL-SYSTEM-MENU)		;expose the system menu
	    (SEND SELF :MOUSE-CLICK BUTTON X Y)))	;handle right click
      (SEND SELF :MOUSE-CLICK
	    (INT-CHAR (CODE-MOUSE-CHAR (1- (HAULONG BD)) 0 1)) X Y))) ;build a left or middle mouse-char

;;; This is what is called if you click on a grid point with the mouse
(DEFUN COM-MOUSE-DRAW (XPOS YPOS)
  (DECLARE (:SELF-FLAVOR FED))
;  (SETQ CURSOR-ON nil)                          ;There isn't a separate keyboard cursor anymore.  Just mouse cursor.
  (SEND SELF :MOUSE-BOOLE-SQUARES DRAW-MODE XPOS YPOS)) 

;;; Redisplay the label after each point is modified to reflect in the label
(DEFMETHOD (FED :AFTER :DRAW-POINT) (&REST IGNORE)
  (SEND SELF :MUST-REDISPLAY-LABEL)) 

;;; KEYPAD ALTERNATIVE TO USING THE MOUSE TO MOVE CURSOR AND SET POINTS.
;;; Move the mouse cursor via the arrow keys.  Move 1 grid point at a whack.  If this is called without an arrow key
;;; being pushed, the effect is to move the mouse cursor onto the grid, if not there already.  If boole-square-p is non-nil
;;; the current draw-mode will be applied to that square before any curor movement.

(DEFUN COM-SHIFT-CURSOR (&OPTIONAL BOOLE-SQUARE-P &AUX (DISTANCE NUMERIC-ARG) DX DY ARROW)
  (DECLARE (:SELF-FLAVOR FED))
  (WHEN BOOLE-SQUARE-P
    (SEND SELF :KEYPAD-BOOLE-SQUARE DRAW-MODE W:MOUSE-X W:MOUSE-Y))
  (SETQ ARROW (INT-CHAR (CHAR-CODE COMMAND-CHAR)))	;Char object without modifier bits
  (SETQ DX (* DISTANCE (OR (SECOND (ASSOC ARROW '((#\left-arrow -1) (#\right-arrow 1)) :TEST #'CHAR-EQUAL)) 0)))
  (SETQ DY (* DISTANCE (OR (SECOND (ASSOC ARROW '((#\up-arrow -1) (#\down-arrow 1)) :TEST #'CHAR-EQUAL)) 0)))
  (SEND SELF :MOVE-MOUSE-CURSOR DX DY))


;;; EDITED CHARACTER MAINTENANCE

;;; This is the list of elements describing the chars that are being edited in the Font Editors.
;;; Each instance of the FED has it's own copy of the characters that it is looking at.
;;; Each element is (font char fed-instance).  The only one of these that might not be saved is
;;; the current character for that fed-instance.

(DEFVAR FED-EDITED-CHARS nil) 

(DEFUN UPDATE-FED-EDITED-CHARS ()
  "Update the list of edited chars to include the current char and font.
Call this after changing the font or character of this FED window."
  (DECLARE (:SELF-FLAVOR FED))
  (WHEN (AND CURRENT-FONT CURRENT-CHARACTER)
    (REMOVE-FED-EDITED-CHAR)
    (PUSH (LIST CURRENT-FONT CURRENT-CHARACTER SELF) FED-EDITED-CHARS)))

(DEFUN REMOVE-FED-EDITED-CHAR ()
  "Remove all characters being edited by the current FED instance." 
  (DECLARE (:SELF-FLAVOR FED))
  (DOLIST (ELT FED-EDITED-CHARS)
    (cond ((EQ SELF (THIRD ELT))	   
	    (SETQ FED-EDITED-CHARS (DELETE ELT (THE LIST FED-EDITED-CHARS) :TEST #'EQ))))))

;; no need to report an error here.  If there currently aren't any characters for
;; this instance of the editor, it's not an error.  dkm 5/87
;	   (t (error "Character not removed from FED-EDITED-CHARS list.")))))	


(DEFUN GET-CHAR-EDITING-PLANE (FONTNAME CHAR)
  "Return the plane storing this FED's data for character CHAR in font FONTNAME.
Otherwise return NIL."
  (DECLARE (:SELF-FLAVOR FED))
  (DOLIST (ELT FED-EDITED-CHARS)
     (AND (EQ FONTNAME (FIRST ELT))
	  (EQL CHAR (SECOND ELT))
	  (EQ SELF (THIRD ELT))
	  (RETURN  (SEND (THIRD ELT) :PLANE)))))

(DEFUN OTHER-FEDS-EDITING-CHAR-P (FONTNAME CHAR)
  "Returns T if some other editor is actively editing this character in this fontname"
  (DECLARE (:SELF-FLAVOR FED))
  (DOLIST (ELT FED-EDITED-CHARS)
    (AND (EQ FONTNAME (FIRST ELT))
	 (EQL CHAR (SECOND ELT))
	 (NEQ (THIRD ELT) SELF)
	 (EQL CHAR (SEND (THIRD ELT) :CURRENT-CHARACTER))
	 (SEND (THIRD ELT) :UNSAVED-CHANGES)
	 (RETURN T))))

(DEFMETHOD (FED :DEACTIVATE) ()
  (DOLIST (ELT FED-EDITED-CHARS)
    (IF (EQ SELF (THIRD ELT))
      (SETQ FED-EDITED-CHARS (DELETE ELT (THE LIST FED-EDITED-CHARS) :TEST #'EQ)))))


;;; UTILITY FUNCTIONS

(DEFUN BARF (&OPTIONAL STRING &REST FORMAT-ARGS)
  "Beep and print error message made with FORMAT.
If STRING is NIL, we just beep."
  (BEEP)
  (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))) 

(DEFUN PROMPT-LINE (STRING &REST FORMAT-ARGS)
  "Pass STRING and ARGS to FORMAT, outputting to the prompt window in the FED frame."
  (DECLARE (:SELF-FLAVOR FED))
  (SEND prompt-window :set-more-p nil)
  (TERPRI prompt-window)
;  (SEND PROMPT-WINDOW :CLEAR-SCREEN)
  (APPLY #'FORMAT PROMPT-WINDOW STRING FORMAT-ARGS))


(DEFUN PROMPT-LINE-READLINE (&OPTIONAL STRING &REST FORMAT-ARGS)
  "Do READLINE prompting and echoing in the prompt window of the FED frame.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt.
We return what READLINE returns."
  (DECLARE (:SELF-FLAVOR FED))
  (W:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
    (READ-LINE PROMPT-WINDOW)))   


(DEFUN PROMPT-LINE-DEFAULTED-READLINE (DEFAULT PARSER-FUNCTION STRING &REST FORMAT-ARGS)
  "Do READLINE, echoing in the prompt window, parse result with PARSER-FUNCTION or default.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt.
An empty line read means use the default; we return DEFAULT.
Otherwise we pass the line contents to PARSER-FUNCTION and return
what it returns."
  (DECLARE (:SELF-FLAVOR FED))
  (W:WINDOW-CALL (PROMPT-WINDOW) (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
     (LET ((INSTRING (STRING-TRIM " " (READ-LINE  PROMPT-WINDOW))))  
       (IF (ZEROP (LENGTH INSTRING))
	 DEFAULT
	 (SEND PARSER-FUNCTION INSTRING))))) 


(DEFUN READ-DEFAULTED-FONT-PARAMETER (NAME CURRENT-VALUE)
  "Read a number, prompting for parameter named NAME, defaulting to CURRENT-VALUE."
  (LET* ((*PRINT-BASE* 10.)
	 (*READ-BASE* 10.)
	 (VALUE
	  (PROMPT-LINE-DEFAULTED-READLINE CURRENT-VALUE 'GLOBAL:READ-FROM-STRING
					  "~&Font ~A (default ~D): " NAME CURRENT-VALUE)))
    (AND (FIXNUMP VALUE) VALUE))) 

(DEFUN READ-DEFAULTED-FILENAME (FONT OPERATION TYPE &AUX TEM SPEC)
  "Read a filename for doing OPERATION (a string), default type TYPE, default name FONT."
  (SETQ TEM (FS:MAKE-PATHNAME :DEFAULTS (PATHNAME-DEFAULTS)
			      :NAME (STRING FONT)
			      :TYPE TYPE))
  (SETQ SPEC (PROMPT-LINE-READLINE "~A ~A file: (default ~A) " OPERATION TYPE TEM))
  (SETQ TEM (FS:MERGE-PATHNAME-DEFAULTS SPEC TEM TYPE))
  (FS:SET-DEFAULT-PATHNAME TEM (PATHNAME-DEFAULTS))
  TEM)

(DEFUN PROMPT-LINE-READ (&OPTIONAL STRING &REST FORMAT-ARGS)
  "Call READ, prompting and echoing in the prompt window.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt.
We return what READ returns."
  (DECLARE (:SELF-FLAVOR FED))
  (W:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
    (READ PROMPT-WINDOW T))) 


(DEFUN PROMPT-LINE-Y-OR-N-P (&OPTIONAL STRING &REST FORMAT-ARGS)
  "As the user for Y or N confirmation, prompting and echoing in the prompt window.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt."
  (DECLARE (:SELF-FLAVOR FED))
  (W:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
    (LET ((*QUERY-IO* PROMPT-WINDOW))
      (Y-OR-N-P nil)))) 


(DEFUN PROMPT-LINE-TYI (&OPTIONAL STRING &REST FORMAT-ARGS &AUX CH)
  "Read one character, prompting and echoing in the prompt window.
STRING and FORMAT-ARGS are passed to FORMAT to make a prompt."
  (DECLARE (:SELF-FLAVOR FED))
  (W:WINDOW-CALL (PROMPT-WINDOW)
    (AND STRING (APPLY #'PROMPT-LINE STRING FORMAT-ARGS))
    ;;For some reason, :read-char is not available in the prompt-window.
    ;;So, for now, loop using read-any and only look for characters.
    ;;This should be fixed later.
    (do () ((atom (SETQ CH (SEND PROMPT-WINDOW :READ-any)))
	    (progn
	      (FORMAT PROMPT-WINDOW "~:C" CH)
	      (RETURN CH)) ))))


(DEFUN FED-Y-OR-N-P (&REST ARGS)
  "Ask for confirmation with either a menu or keyboard, whichever user is using.
If the current command was invoked from the keyboard, we use the keyboard;
if the current command was invoked with the mouse, we use a mouse menu."
  (DECLARE (:SELF-FLAVOR FED))
  (IF (CHARACTERP COMMAND-CHAR)
    (APPLY 'PROMPT-LINE-Y-OR-N-P ARGS)
    (W:MOUSE-Y-OR-N-P (APPLY 'GLOBAL:FORMAT nil ARGS)))) 

; rb 1/22/86 - changed menu handling slightly

(DEFUN FED-CHOOSE (ALIST MESSAGE &OPTIONAL WINDOW ITEM-LIST-TYPE)
  "Choose an alternative from ALIST, prompting with MESSAGE, using keyboard or mouse.
ALIST is like the first argument to MENU-CHOOSE.
If the current command was invoked with the mouse, we use a mouse menu.
If the current command was invoked from the keyboard, we use the keyboard,
reading a line and matching it against the cars of the alist elements.

Extra feature: if an element of ALIST has a null cdr,
and that element is selected, the value is the string in the car of the element,
rather than NIL."
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL WINDOW)
      (SETQ WINDOW PROMPT-WINDOW))
  (FLET ((FIND-MATCH (ITEM LIST)
	     (DOLIST (ELE LIST)
		(WHEN (STRING-EQUAL ITEM (IF (CONSP ELE) (CAR ELE) ELE))
		   (RETURN ELE)))))
    (IF (CHARACTERP COMMAND-CHAR)
	(DO (INPUT) (NIL)
	  (FORMAT WINDOW "~A (one of " MESSAGE)
	  (DOLIST (A ALIST)
		  (OR (EQ A (FIRST ALIST)) (PRINC ", " WINDOW))
		  (PRINC (IF (CONSP A) (FIRST A) A) WINDOW))
	  (PRINC "): " WINDOW)
	  (SETQ INPUT (READ-LINE WINDOW)) 
	  (LET ((MATCH (FIND-MATCH INPUT ALIST)))
	    (OR MATCH
		;; No exact match; is there a unique completion?
		(LET (MATCHES)
		  (DOLIST (ELT ALIST)
			  (IF (STRING-EQUAL (IF (CONSP ELT) (CAR ELT) ELT)
					    INPUT :END1 (LENGTH INPUT))
			      (PUSH ELT MATCHES)))
		  (IF (= (LENGTH MATCHES) 1)
		      (SETQ MATCH (FIRST MATCHES)))))
	    (IF MATCH
		(IF (CONSP MATCH)
		    (LET ((CHOOSE-INFO (AND (CONSP (CDR MATCH))          ;member can't handle dotted lists
					    (MEMBER :MENU-CHOOSE MATCH))));is there a :menu-choose option?
		      (IF CHOOSE-INFO
			  (RETURN (FED-CHOOSE (CDADR CHOOSE-INFO) (CAADR CHOOSE-INFO)))  ;process sub-menu of items
			  (CASE (LENGTH MATCH)
			     (1 (RETURN (CDR MATCH)))		    ; A dotted list       (item . value),   return value
			     (2 (RETURN (CADR MATCH)))		    ; A list                (item value),     return value
			     (OTHERWISE				       ; List with options (item type args)
			       (RETURN (OR (CADR (MEMBER :value MATCH)) ;return the :value clause
					   (CAR MATCH)))))))	        ;return item
		    (RETURN (OR (GET MATCH :VALUE) MATCH)))	    ; Not a list  -- just return item
		(BARF "   ~A is invalid.~%" INPUT))))
	;;We got here via a mouse click.  See if we write it on the typeout window with possible mouse sensitivity.
	(IF ITEM-LIST-TYPE
	    (PROGN
	      (SEND WINDOW :set-current-font 3)
	      (FORMAT WINDOW "~A" MESSAGE)
	      (SEND WINDOW :set-current-font 2)
	      (SEND WINDOW :ITEM-LIST ITEM-LIST-TYPE ALIST)
	      (SEND WINDOW :SET-CURRENT-FONT 0)
	      :mousable-display)                ;return this keyword so caller knows what happened
	    ;;else, we put out a menu of the items
	    (MULTIPLE-VALUE-BIND (VALUE ITEM)
		(W:MENU-CHOOSE ALIST :label MESSAGE)
	      (COND (VALUE)
		    (T (FIRST ITEM))))))))



;;; Calling select-font below in place of the previous call to com-display-font since select-font
;;; adjusts the screen size (:change-of-size-or-margins) which may be necessary since the
;;; new font being acknowledged may well be taller than the one from which it was generated.
(DEFUN ACKNOWLEDGE-NEW-FONT (fname fd)
   (DECLARE (:self-flavor FED))
   (FONT-NAME-SET-FONT-AND-DESCRIPTOR fname fd)
   (SELECT-FONT FNAME)
   (PROMPT-LINE "Font ~A now interned in FONTS package.~%" current-font))

;;; Correct problem when user says NO to replacing existing font and system does so anyways!
;;; This function is called by all the font-wide rotates, italicize, thicken, ... commands.
(DEFUN GET-NEW-FONT-NAME (ch input-font)
  (DECLARE (:self-flavor FED))
  (LET* ((newfont (prompt-line-defaulted-readline
		    (STRING-APPEND current-font ch)
		    'intern-font-name
		    "Name for new ~A version of ~A  (default ~A):  "
		    input-font current-font (STRING-APPEND current-font ch)))
	 (fname (IF (STRINGP newfont)
		    (intern-font-name newfont) newfont))
	 (good (NOT (IF (AND (BOUNDP fname) (SYMBOL-VALUE fname))
			(IF (fed-y-or-n-p "Font ~A already exists, overwrite it?  " fname)
			    nil
			    (OR (prompt-line "Font ~A not replaced.  " fname)
				t))))))		 ;make sure to return t here if y-or-n-p answered NO.
    (VALUES good newfont fname)))

(DEFUN VALID-FONT-FORMATS ( )
;;;This returns a list containing two  item lists, suitable for FED-CHOOSE - the first contains standard font formats, 
;;; the second contains the non-standard font formats. FOR-LOADING-P indicates that only files that can be 
;;;loaded on the local machine should be returned.  For-loading-p is no longer used. - MRR 12/86

  (LET* ((BINARY-TYPE  (SI:LOCAL-BINARY-FILE-TYPE))
	 (STD-TYPES `(("AST" :value "AST"					         ;Standard ASCII textual format
		       :documentation "Standard Ascii Format (stars and spaces)")
		      (,BINARY-TYPE :value ,BINARY-TYPE				         ;Local binary type
		       :documentation  "Binary format for local machine")))         

	 (COMPILER-TYPES (delete nil
				 (MAPCAR #'(LAMBDA (X)			                ;Binary formats supported by the compiler
				        (WHEN (neq (CAR X) binary-type)                    
  				       `(,(car x) :documentation "Binary Font Format supported by Cross Compiler")))
				  SI:VALIDATE-BINARY-FILE)))
	 )
    (list std-types compiler-types)))


;;; LISTING FONTS.

;; This method is only used as a kick start when the FED is initially brought up.  Just before the
;;FED-FRAME command loop starts up, a blip is forced on the input buffer which will call this
;;method.  See (defmethod fed-frame :before :loop).
(DEFMETHOD (FED :LIST-FONTS) ()
  (COM-LIST-FONTS))


(DEFUN COM-LIST-FONTS ()
  "List the fonts currently loaded so that one can be selected"
  (DECLARE (:SELF-FLAVOR FED))
  (LET* ((*STANDARD-OUTPUT* (SEND (SEND SELF :SUPERIOR) :TYPEOUT-PANE))           ;get the typeout pane
	 (FONT (FED-CHOOSE (LOADED-FONTS-ON-MACHINE)				    ;the list of items to display
			   (format nil "Loaded Fonts - Select one to edit it~2%")   ;message to display first
			   *STANDARD-OUTPUT*  					    ;window to write it on
			   'FONT)))						    ;mouse item type
    ;;If they went in through the keyboard, Fed-Choose returns the font they selected now.
    ;;otherwise, it is written as mouse selectable on the typeout window
    (WHEN (AND FONT (NEQ FONT :MOUSABLE-DISPLAY))
       (SELECT-FONT FONT))))


(DEFUN LOADED-FONTS-ON-MACHINE ()
  "Returns a list of symbols in the FONTS package whose values are fonts."
  (LET ((LIST nil))
    (DO-LOCAL-SYMBOLS (X 'FONTS LIST)
	(AND (BOUNDP X)      ;symbol is bound
	     (TYPEP (SYMBOL-VALUE X) 'FONT)	;binding is a font
	     (PUSH X LIST)))
    (SORT LIST #'STRING-LESSP))) 

;;; DIRECTORY command for listing fonts

(DEFUN COM-LIST-FC-FONTS ()
  (LIST-FONTS-FILE-COMPUTER))
 
(DEFUN LIST-FONTS-FILE-COMPUTER ()
  ;; Allow selection from a list of fonts in user specified directory
  (DECLARE (:SELF-FLAVOR FED))
  (LET* ((FONT-DIR (READ-DEFAULTED-FILENAME "*" "Display Directory of Fonts for" (si:local-binary-file-type)))
	 (UNLOADED-FONTS-ONLY (FED-CHOOSE '(("Yes" T)
					    ("No" N))
					  "Restrict to fonts NOT loaded?"))
	 FONT-PATHNAME)
    (WHEN UNLOADED-FONTS-ONLY
	(SETQ FONT-PATHNAME
	      (FED-CHOOSE  (SORT (DELETE NIL
					 (MAPCAR #'(LAMBDA (X)
						     (LET ((SYM (INTERN (STRING (SEND (CAR X) ':NAME)) 'FONTS)))
						       (IF (EQ UNLOADED-FONTS-ONLY 'T)
							   (IF (AND (BOUNDP SYM)
								    (TYPEP (EVAL SYM) 'FONT))
							       NIL (CONS SYM (CAR X)))
							   (CONS SYM (CAR X)))))
						 (CDR (FS:DIRECTORY-LIST FONT-DIR :FAST))))
				 #'ALPHALESSP
				 :KEY #'CAR)
			   (Format nil "Fonts in ~a - Select one to load.~2%" FONT-DIR)
			   (SEND (SEND SELF :SUPERIOR) :TYPEOUT-PANE)
			   'LOAD-FONT))		;this is the item type to put on these items.  Clicks on them call following method

	;;If he used the keyboard to get the selection, FED-CHOOSE will return the selection now
	(WHEN (PATHNAMEP FONT-PATHNAME)
	      (COM-READ-FILE (SEND FONT-PATHNAME :CANONICAL-TYPE) FONT-PATHNAME)))))

;;;This is called if he clicks on the mouse selectable display of file-computer fonts
(DEFMETHOD (FED :LOAD-FONT) (FONT-PATHNAME)
  (COM-READ-FILE (SEND FONT-PATHNAME :CANONICAL-TYPE) FONT-PATHNAME))


;;; SELECTING A FONT.

;;; Comes from clicking on the mouse sensitive display of loaded fonts.
(DEFMETHOD (FED :SELECT-FONT) (NEW-FONT)
  (SELECT-FONT NEW-FONT)) 

;;; rb 12/11/85 - coerce symbol to string for name component to MAKE-PATHNAME
;;;		  since that function doesn't understand symbols
;;; rb 1/23/86 - moved command-specific handling out of this function
(DEFUN SELECT-FONT (NEW-FONT)
   "Select the font named NEW-FONT for editing in SELF, a FED window."
   (DECLARE (:SELF-FLAVOR FED) (SPECIAL typeout-pane))
   (WHEN (AND (NEQ NEW-FONT CURRENT-FONT) UNSAVED-CHANGES)
      (IF (FED-Y-OR-N-P "This will discard the editing you have done.  Proceed? ")
	  (REMOVE-FED-EDITED-CHAR)  ;clobber old char and proceed
	  (RETURN-FROM SELECT-FONT NIL)))	                   ;get out.

   ;; Reset all this stuff if he selected a new font.
   (WHEN (NEQ NEW-FONT CURRENT-FONT)
      (SETQ CURRENT-CHARACTER NIL
	    CURRENT-FONT NEW-FONT
	    UNSAVED-CHANGES NIL
	    BOX-X-SIZE DEFAULT-BOX-SIZE
	    BOX-Y-SIZE DEFAULT-BOX-SIZE)
      (send self :DEDUCE-WINDOW-ARRAY-SIZE) 	;Resets the window array size. -mrr 9/18/86
      (FONT-GET-FD CURRENT-FONT)
      (UPDATE-FED-EDITED-CHARS)
      (W:delaying-screen-management
	(SEND SELF :ERASE-ALL)
	(DISPLAY-LABEL)))

   ;;give a display of this font in any case.
   (COM-DISPLAY-FONT))

;;; DISPLAY THE CURRENT FONT

(DEFVAR *LABEL-BASE* 10.
  "Base to display the numbers labeling the rows and columns of the font display")

(DEFVAR *SAMPLE-FONT* 'FONTS:CPTFONT
  "The font to use in the upper portion of the font box for comparison to the displayed font")

(DEFVAR *COLUMNS* 0
  "The number of columns to use to display the font.  
       NIL  - use the largest power of 2 that will fit
       positive number - use exactly that many columns for the next display only.  Not recommended.
       zero or negative - use as many colunns as will fit")

(DEFUN COM-DISPLAY-FONT (&OPTIONAL FONT)
  "Display all of the characters of the font being edited, to show what they look like.  Above each one is the corresponding
character of a SAMPLE-FONT, so two fonts can be compared.  See variables *LABEL-BASE, *SAMPLE-FONT*, and *COLUMNS*
which will control how the font is displayed."
  (DECLARE (:SELF-FLAVOR FED))
  (OR FONT
      (SETQ FONT
	    (OR (AND (BOUNDP CURRENT-FONT) (SYMBOL-VALUE CURRENT-FONT))
		(COND
		  (CURRENT-FONT
		    (FONT-NAME-SET-FONT-AND-DESCRIPTOR CURRENT-FONT (FONT-GET-FD CURRENT-FONT))
		    (SYMBOL-VALUE CURRENT-FONT))))))
  (IF (NULL FONT)
      (PROGN
	(BARF "No Current Font")
	(SEND W:SUPERIOR :HANDLE-PROMPT))

         ;;DISPLAY-FONT has been rewritten and moved to the window system.
      (W:DISPLAY-FONT FONT
		       :WINDOW (SEND (SEND SELF :SUPERIOR) :TYPEOUT-PANE)
		       :MOUSE-SENSITIVE-ITEM-TYPE 'CHARACTER
		       :LABEL-BASE *LABEL-BASE*
		       :SAMPLE-FONT (IF (SYMBOLP *SAMPLE-FONT*)
					(SYMBOL-VALUE *SAMPLE-FONT*)
					*SAMPLE-FONT*)
		       :COLUMNS *COLUMNS*)))

;;; COPYING A FONT

(DEFUN COM-COPY-FONT ()
  (DECLARE (:SELF-FLAVOR FED))
  (WHEN (NULL CURRENT-FONT)
    (BARF "No font is selected.")
    (RETURN-FROM com-copy-font))
  (LET ((NEW-FONT (PROMPT-LINE-DEFAULTED-READLINE nil 'INTERN-FONT-NAME "New font name: "))
	(OLD-FD (FONT-GET-FD CURRENT-FONT))
	NEW-FD)
    (WHEN (OR (NULL NEW-FONT)
	      (AND (BOUNDP NEW-FONT)
		   (NOT (FED-Y-OR-N-P "Font ~A exists.  Clobber it? " NEW-FONT))))
      (BARF "Aborted.")
      (RETURN-FROM COM-COPY-FONT))
    (MAKUNBOUND NEW-FONT)
    (SETQ CURRENT-FONT NEW-FONT)
    ;; Make the new font's FD a copy of the old one, but don't copy name.
    (SETQ NEW-FD (FONT-GET-FD NEW-FONT))
    (COPY-ARRAY-CONTENTS-AND-LEADER OLD-FD NEW-FD)
    (SETF (FD-NAME NEW-FD) NEW-FONT)
    ;; Replace the CD's with copies, too.
    (DOTIMES (I (ARRAY-TOTAL-SIZE NEW-FD))
      (IF (AREF NEW-FD I)
	  (SETF (AREF NEW-FD I) (SI:COPY-OBJECT (AREF NEW-FD I)))))
    (FONT-NAME-SET-FONT-AND-DESCRIPTOR NEW-FONT NEW-FD)
    (UPDATE-FED-EDITED-CHARS)
    (DISPLAY-LABEL)
    (select-font new-font)           ;; added 12/19/86 - mrr 
    ))

;;; CREATE A NEW FONT FROM SCRATCH.

; rb 1/23/86 - clarify functionality of command
(DEFUN COM-CREATE-FONT ()
   (LET ((fontname (PROMPT-LINE-DEFAULTED-READLINE NIL 'INTERN-FONT-NAME "Font to create: ")))
     (and (boundp fontname)
	  (or (fed-y-or-n-p "~A is the name of a loaded font.  Create anyway? " fontname)
	      (return-from com-create-font)))
     (makunbound fontname)
     (SELECT-FONT fontname)))

;;; REMOVE A FONT FROM THE ENVIRONMENT

(DEFUN COM-REMOVE-FONT ()
  (LET ((fname (PROMPT-LINE-READLINE "Name of font to remove:  ")))
    (IF  (fed-y-or-n-p
	       "Remove FONTS:~A?  " (STRING-UPCASE fname))
    (REMOB-FONT-NAME fname)))
  (prompt-line "")
  (com-list-fonts))

(DEFUN REMOB-FONT-NAME (STRING)
  "Given a STRING, remove the font-name symbol of that string-name from the package FONTS."
  (LET ((SYM (FIND-SYMBOL (STRING-UPCASE STRING) 'FONTS)))
    (AND SYM (UNINTERN SYM 'FONTS))))



;;; READ IN A FONT FILE AND LOAD IT INTO THE FONTS PACKAGE

(DEFUN COM-READ-FILE (&OPTIONAL TYPE FILENAME)
  (DECLARE (:SELF-FLAVOR FED))
  (LET  ((VALID-TYPES (VALID-FONT-FORMATS))
	 FD FONTNAME FONTS-BEFORE FONTS-AFTER FONTS-ADDED FILENAME-FONT-EXISTS-FLAG)
    (WHEN (NULL TYPE)
      (SETQ TYPE (FED-CHOOSE  `(,@(CAR VALID-TYPES)
				;;Add secondary file types in this other menu
				("OTHER" :MENU-CHOOSE (" LOAD OTHER FORMAT " . ,(CADR VALID-TYPES))
				 :DOCUMENTATION "Click Here To Choose a Non-Standard File Format."))
			      " LOAD file format "))
      (WHEN (OR (NULL TYPE) (EQUAL TYPE "OTHER"))
	(RETURN-FROM COM-READ-FILE)))

    (WHEN (NULL FILENAME)
      (SETQ FILENAME (READ-DEFAULTED-FILENAME CURRENT-FONT "Load" TYPE)))

; Fonts saved under Release 3 have the name of the font in the file-attribute-list. We try to use it
; to determine the actual name of the font being loaded.   For older fonts we assume the font name is
; the same as the filename, and try to detect if its different after it has been loaded.

    (cond  ((SETQ FONTNAME (getf (fs:file-attribute-list filename) :font-symbol))  
	
	    ;; Does the fontname symbol already exist and is it already a font ?      
	    (WHEN (AND (BOUNDP FONTNAME)
		       (TYPEP (EVAL FONTNAME) 'FONT))
	      ;; Then ask for confirmation before loading.         
	      (cond ((FED-Y-OR-N-P "~A is the name of a loaded font.~
                                 ~%        Load anyway? " FONTNAME)
		     (SETF (SYMBOL-PLIST FONTNAME) NIL)   ;don't unintern. just clobber the plist dkm 5/87
		     (REMOVE-FED-EDITED-CHAR)             ;get rid of current edited character too    5/87
		     (SETQ CURRENT-CHARACTER NIL)         ;current-character not current anymore      5/87
		     (SEND SELF :ERASE-ALL))              ;erase the grid 5/87

		    ;;else
		    (T (RETURN-FROM COM-READ-FILE))))
	    
	    (SELECTOR TYPE STRING-EQUAL
	      ("AST"
	       (INTERN FONTNAME "FONTS")
	       (SETQ FD (READ-AST-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
	       (PUTPROP FONTNAME FILENAME 'AST-FILE)
;	       (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME FD)                      ;mrr 04.15.87  done in select-font
	       )
	      (("XLD" "XFASL" "QFASL")
	       (LOAD FILENAME :PACKAGE "FONTS")
;	       (FONT-NAME-SET-FONT-AND-DESCRIPTOR fontname (FONT-GET-FD FONTNAME))  ;mrr 04.15.87  done in select-font
	       )
	      (otherwise
	       (BARF "COM-READ-FILE cannot handle font format ~a" type)
	       (RETURN-FROM COM-READ-FILE)))
;	    (SETQ CURRENT-FONT FONTNAME)                                             ;mrr 04.15.87  done in select-font
	    (SELECT-FONT FONTNAME))

	   ((setq fontname (intern (send filename :name) "FONTS"))  ;;Assume the fontname is same as the filename.

	     ;; Does the fontname symbol already exist and is it already a font ?      
	    (WHEN (AND (BOUNDP FONTNAME)
		       (TYPEP (EVAL FONTNAME) 'FONT))
	      ;; Then ask for confirmation before loading and set a flag if you load anyway.    
	      (if (FED-Y-OR-N-P "~A is the name of a loaded font.  Load anyway? " FONTNAME)
		  (SETQ FILENAME-FONT-EXISTS-FLAG T)
		  (RETURN-FROM COM-READ-FILE)))
    
	    (SETQ FONTS-BEFORE (LOADED-FONTS-ON-MACHINE))
	    (SELECTOR TYPE STRING-EQUAL
	      ("AST"
	       (INTERN FONTNAME "FONTS")
	       (SETQ FD (READ-AST-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
	       (PUTPROP FONTNAME FILENAME 'AST-FILE)
	       (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME FD))
	      (("XLD" "XFASL" "QFASL")
	       (FONT-NAME-SET-FONT-AND-DESCRIPTOR
		 ;; In case  file was not found and name is changed before actual loading.
		 (let* ((loaded-name (LOAD FILENAME :PACKAGE "FONTS"))
			(loaded-font (send loaded-name :name)))
		   (if (string-equal loaded-font fontname)
		       fontname
		       (setq fontname (intern (or (getf (fs:file-attribute-list loaded-name) :font-symbol)
						  loaded-font) "FONTS"))))
		 (FONT-GET-FD FONTNAME)))
	      
	      (otherwise
	       (BARF "COM-READ-FILE cannot handle font format ~a" type)
	       (RETURN-FROM COM-READ-FILE)))
    
	    (SETQ CURRENT-FONT FONTNAME)         ;; Set current font after the read operation has occurred.
	    (SEND SELF :REDEFINE-MARGINS)
	    ;;  What fonts are loaded after the read operation ?
	    (SETQ FONTS-AFTER (LOADED-FONTS-ON-MACHINE))
	    ;;  What fonts were added ?
	    (SETQ FONTS-ADDED (SET-EXCLUSIVE-OR FONTS-BEFORE FONTS-AFTER))
	    ;; Better handling for when font-name and associated file-name are different, by MRR
	    (COND ((AND (member TYPE '("XLD" "XFASL" "QFASL")  :test #'STRING-EQUAL)  ;; Just applies to binary files. 
			(= (LENGTH FONTS-ADDED) 2)) ;;neither the filename-font nor the "new" font were previously loaded.
		   (W:BEEP)
		   (PROMPT-LINE "Font name and file name do not match.  Font just loaded is:  ~A~%"
				;; Return the other "new" font and resets the current-font.
				(SETQ CURRENT-FONT (COND ((EQL FONTNAME (FIRST FONTS-ADDED))
							  (UNINTERN  FONTNAME 'FONTS)  ;; Remove the empty font
							  (SECOND FONTS-ADDED)) 
							 ((EQL FONTNAME (SECOND FONTS-ADDED))
							  (UNINTERN FONTNAME 'FONTS)  ;; Remove the empty font
							  (FIRST FONTS-ADDED)))))) 
		  ((AND FILENAME-FONT-EXISTS-FLAG
			(member TYPE '("XLD" "XFASL" "QFASL")  :test #'STRING-EQUAL)
			(= (LENGTH FONTS-ADDED) 1))    ;; filename-font existed but the actual font loaded is new.
		   (W:BEEP)
		   (PROMPT-LINE "Font name and file name do not match.  Font just loaded is:  ~A~%" 
				(SETQ CURRENT-FONT (FIRST FONTS-ADDED)))))
	    (SELECT-FONT current-font))
	   )   ; cond
    )   ; let
  )

;;; WRITE CURENT FONT TO A FILE.

(DEFUN COM-WRITE-FILE ()
  (DECLARE (:SELF-FLAVOR FED))
  (UNLESS CURRENT-FONT
     (BARF "No Current Font.")
     (RETURN-FROM COM-WRITE-FILE))
  (LET* ((VALID-FONT-FORMATS (VALID-FONT-FORMATS))
	 (TYPE (FED-CHOOSE `(,@(CAR VALID-FONT-FORMATS)
				  ;;Add secondary file types in this other menu
				  ("OTHER" :MENU-CHOOSE (" WRITE OTHER FORMAT " . ,(CADR VALID-FONT-FORMATS))
				   :DOCUMENTATION "Click Here to Select a Non-Standard File Format."))
				" Write Font Format "))
	 FILENAME)
    (UNLESS (OR (NULL TYPE) (EQUAL TYPE "OTHER"))
       (SETQ TYPE (SEND (FS:MAKE-PATHNAME :TYPE TYPE) :CANONICAL-TYPE))
       (SETQ FILENAME (READ-DEFAULTED-FILENAME CURRENT-FONT "Write" TYPE))
       (COND ((STRING-EQUAL TYPE "AST")
	      (WRITE-FONT-INTO-AST CURRENT-FONT FILENAME)
	      (PUTPROP CURRENT-FONT FILENAME 'AST-FILE))
	     
	     ((ASSOC TYPE SI:VALIDATE-BINARY-FILE)
	      ;; The compiler will write out the font in the proper format based
	      ;; on the type of the pathname passed to it. (QFASL, XFASL, XLD, etc)
	      (IF (EQ TYPE (SEND FILENAME :CANONICAL-TYPE))
		  (COMPILER:FASD-FONT CURRENT-FONT FILENAME)
		  (BARF "This font format requires the pathname type to be canonical to ~a" type)))
	     (t (BARF "COM-WRITE-FILE cannot handle font format ~a" type))))))


;;; SELECT A CHARACTER.

(DEFUN COM-SPECIFY-CHARACTER (&AUX CH)
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
    (BARF "No current font.")
    (PROGN
      (COND (NUMERIC-ARG-P 
	      (SETQ CH (INT-CHAR NUMERIC-ARG)))
	    (T (SETQ CH (PROMPT-LINE-TYI "Character: "))))
      (COND
	((> (CHAR-CODE CH) (1- *MAX-FONT-LENGTH*))
	 (BARF "Invalid Character"))
	((<= 128. (CHAR-CODE CH) 160.)
	 (BARF "Character Codes 128. - 160. are reserved for LISPM control characters."))
	(T 
	  ;; See if this character is actively being edited in another editor and issue a warning
	  (AND (OTHER-FEDS-EDITING-CHAR-P CURRENT-FONT CH) 
	       (NOT (FED-Y-OR-N-P "Warning: character-code ~d (character ~c) is being edited in another Editor.~
                                 ~%Edit it here anyway?" (CHAR-CODE CH) CH))
	       (RETURN-FROM COM-SPECIFY-CHARACTER NIL))
	     
	   (IF UNSAVED-CHANGES
	       (IF (NOT (FED-Y-OR-N-P "There are unsaved changes in this editor.~%Discard and Proceed? "))
		   (RETURN-FROM COM-SPECIFY-CHARACTER NIL)
		   (REMOVE-FED-EDITED-CHAR )))

	   (SETQ CURRENT-CHARACTER CH)
	   (LET ((NEW-PLANE (GOBBLE-CHARACTER CURRENT-FONT CURRENT-CHARACTER T)))
	     (IF (NULL NEW-PLANE)
		 (SEND SELF :ERASE-ALL)
		 (PROGN
		   (SEND SELF :MUST-REDISPLAY-CURRENT-PLANE-AREA)
		   (SETQ PLANE NEW-PLANE)
		   
		   (SEND SELF :MUST-REDISPLAY-ENTIRE-PLANE)))
	     (SETQ UNSAVED-CHANGES nil)
	     (SEND SELF :HOME-BOX)
	     (UPDATE-FED-EDITED-CHARS))))))) 


(DEFUN COM-SPECIFY-CHARACTER-BY-NUMBER ()
  (DECLARE (:SELF-FLAVOR FED))
  (IF (NULL CURRENT-FONT)
    (BARF "No current font.")
    (LET ((NUMERIC-ARG-P T)
	  (NUMERIC-ARG (PROMPT-LINE-READ "Character code number: ")))
      (COND
	((NOT (INTEGERP NUMERIC-ARG)) (BARF "~S is not a number." NUMERIC-ARG))
	((NOT (<= 0 NUMERIC-ARG (1- *MAX-FONT-LENGTH*))) (BARF "~D is out of range." NUMERIC-ARG))
	(T (COM-SPECIFY-CHARACTER)))))) 

;;;This is the guy that get called when mousing on a character in the font display.
(DEFMETHOD (FED :SELECT-CHAR) (CHAR)
  (LET ((NUMERIC-ARG-P T)
	(NUMERIC-ARG (CHAR-CODE CHAR)))
    (COM-SPECIFY-CHARACTER)))


(DEFUN  COM-READ-GRAY-CHARACTER ()
  (DECLARE (:SELF-FLAVOR FED))
  (LET ((FONT (PROMPT-LINE-DEFAULTED-READLINE
		CURRENT-FONT 'INTERN-FONT-NAME
		"Font of char to put in gray plane (default ~A): "
		CURRENT-FONT)))
    (IF (NOT (AND FONT (BOUNDP FONT)))
	(BARF "Font ~A does not exist." FONT)
      (LET ((NEW-PLANE (GOBBLE-CHARACTER FONT (PROMPT-LINE-TYI "Character: "))))
	(SEND SELF :MOVE-GRAY-PLANE 0 0 T)
	(IF (NULL NEW-PLANE)
	    (SEND SELF :ERASE-GRAY)
	    (PROGN
	      (SEND SELF :MUST-REDISPLAY-CURRENT-GRAY-AREA)
	      (SETQ GRAY-PLANE NEW-PLANE)
	      (SEND SELF :MUST-REDISPLAY-ENTIRE-GRAY-PLANE)))))))

;;; Copy the data from character CHAR in font FONT
;;;  into a plane, and return it (or NIL, if the character doesn't exist).

(DEFUN GOBBLE-CHARACTER (FONT CHAR &OPTIONAL SET-CHAR-BOX 
			 &AUX FD CD EXISTING-PLANE -PLANE-)
  "Copy the definition of character CHAR in FONT into a plane, and return the plane.
SET-CHAR-BOX says set SELF's character box for that character as well."
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ EXISTING-PLANE (GET-CHAR-EDITING-PLANE FONT CHAR))
  (IF EXISTING-PLANE
      (PROGN
	(SETQ -PLANE- (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
	(MERGE-OTHER-PLANE EXISTING-PLANE 0 0 :set -plane-)
	-PLANE-)
      ;else
      ;; If we have no FD format array for this font, make one.
      (SETQ FD (FONT-GET-FD FONT))
      
      ;; Get the character descriptor for the desired character out of the FD.
      (IF (NOT (AND (< CHAR (ARRAY-TOTAL-SIZE FD))
		    (SETQ CD (AREF FD CHAR))))
	  NIL
	  (PROGN
	    (SETQ -PLANE- (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10))
	    (SETF (PLANE-ORIGIN -PLANE-) (LIST (- (CD-CHAR-LEFT-KERN CD)) 0))
	    
	    (WHEN SET-CHAR-BOX
	       ;; Put sides of character frame at right place,
	       ;; according to char width and left kern.
	       (SETQ CHAR-BOX-X1 0
		     CHAR-BOX-X2 (ROUND (CD-CHAR-WIDTH CD)))
		  
	       ;; Put top of character at top of font line, and bottom at baseline
	       ;;   so that descenders go below the "bottom".
	       (SETQ CHAR-BOX-Y1 0
		     CHAR-BOX-Y2 (FD-BASELINE FD)
		     CHAR-BOX-Y3 (FD-LINE-SPACING FD)))
	    
	       ;; Now XWIDTH and YWIDTH get the size of the character's raster,
	    ;;   and copy the data into the plane in CHARACTER-ARRAY.
	    
	    (LET ((XWIDTH (SECOND (ARRAY-DIMENSIONS CD)))
		  (YWIDTH (FIRST (ARRAY-DIMENSIONS CD)))
		  (XORG (FIRST (PLANE-ORIGIN -PLANE-))))
	      (DO ((I 0 (1+ I))) ((= I XWIDTH))
		(DO ((J 0 (1+ J))) ((= J YWIDTH))
		  (PLANE-ASET (AREF CD J I) -PLANE- (+ I XORG) J))))))
      -PLANE-))


;;; SAVE THE EDITING THAT HAS BEEN DONE ON THE CURRENT CHARACTER IN CHARACTER DESCRIPTOR

;;; Change to using decimal for numeric positioning information
(DEFUN COM-SAVE-CHARACTER ()
  (DECLARE (:SELF-FLAVOR FED))
  (COND ((NULL CURRENT-CHARACTER)
	 (BARF "No current character."))
	((<= 128. (CHAR-CODE CURRENT-CHARACTER) 160.)
	 (BARF "Character Codes 128. - 160. are reserved for LISPM control characters."))
	(T
	 (PROMPT-LINE "Saving ~:C (~D) in ~A"
		      CURRENT-CHARACTER (CHAR-INT CURRENT-CHARACTER) CURRENT-FONT)
	 (FONT-STORE-CD CURRENT-FONT CURRENT-CHARACTER)
	 (SETQ UNSAVED-CHANGES NIL)
	 (SEND SELF :MUST-REDISPLAY-LABEL))))


(DEFUN COM-STORE-CHARACTER-EXPLICIT ()
  (DECLARE (:SELF-FLAVOR FED))
  (LET ((FONT
	 (PROMPT-LINE-DEFAULTED-READLINE CURRENT-FONT 'INTERN-FONT-NAME
					 "Font to save character in (default ~A): " CURRENT-FONT)))
    (IF (NOT (AND FONT (BOUNDP FONT)))
      (BARF "Font does not exist.")
      (LET ((CH (PROMPT-LINE-TYI "Character of ~A to store in: " FONT)))
	(IF (GET-CHAR-EDITING-PLANE FONT CH)
	  (BARF "Warning: ~A character ~:C is being edited in a FED" FONT CH))
	(FONT-STORE-CD FONT CH))))) 

;; rb 11/7/85 - function completely rewritten 

(DEFUN FONT-STORE-CD (FONTNAME CHAR &OPTIONAL (UPDATE-FONT-FLAG T)
		      &AUX FD CD XWIDTH YWIDTH KERN
		      PLANE-MIN-X PLANE-MAX-X PLANE-MIN-Y PLANE-MAX-Y
		      EVEN-MIN-X EVEN-MAX-X EVEN-MIN-Y EVEN-MAX-Y
		      ABOVE-MIN-Y ABOVE-MAX-Y BELOW-MIN-Y BELOW-MAX-Y)
  "Store the current FED data into character CHAR of FONTNAME.
If UPDATE-FONT-FLAG is NIL, only the font-descriptor for that font
is updated, not the actual font."

  (DECLARE (:SELF-FLAVOR FED))
  ;; Should we exit "from the side", nothing is done.
  (BLOCK nil
    (BLOCK FONT-STORE-CD
       ;; Get through some preliminaries.
       ;; Find the FD for this font.
      (SETQ FD (FONT-GET-FD FONTNAME))
      ;; Warn if the character box now displayed is incompatible with the font.
      (COND
	((OR (/= (- CHAR-BOX-Y2 CHAR-BOX-Y1) (FD-BASELINE FD))
	    (/= (- CHAR-BOX-Y3 CHAR-BOX-Y1) (FD-LINE-SPACING FD)))
	 (OR
	  (Y-OR-N-P
	   "This character's height and baseline are incompatible with the selected font.
            ~&If actually stored, the character will be aligned by the top of its box.
            ~&Proceed to store anyway?")
	  (RETURN-FROM FONT-STORE-CD nil))))

      ;; Now determine the smallest rectangle of the plane that encloses pixels
       ;; that have been set (the "extents" rectangle).
      ;; Get the largest and smallest x and y values for the plane.
      (SETF (LIST PLANE-MIN-X PLANE-MIN-Y) (PLANE-ORIGIN PLANE)) ;smallest x, y for plane
      (SETF (LIST PLANE-MAX-X PLANE-MAX-Y) (PLANE-END PLANE))	;largest x, y (plus 1) for plane

      ;; Due to machine conventions, pixels above the top of the character box and below its bottom
      ;; cannot be saved as part of the character, although pixels set to the left or right can be.
      ;; Therefore, first determine what parts of the plane correspond to "above", "even with", and
      ;; "below" the character box.  It's only in the "even with" zone that we need to look for the
      ;; extents rectangle.  However, if set pixels are found in the "above" and "below" zones,
      ;; let the user know that they will not be saved.
      (SETQ EVEN-MIN-X nil				 ;left coord of "even" zone
	    EVEN-MAX-X nil				 ;right
	    EVEN-MIN-Y CHAR-BOX-Y1			 ;top
	    EVEN-MAX-Y (+ CHAR-BOX-Y1 (FD-LINE-SPACING FD)) ;bottom
	    ABOVE-MIN-Y (MIN PLANE-MIN-Y EVEN-MIN-Y)	 ;top of "above" zone
	    ABOVE-MAX-Y EVEN-MIN-Y			 ;bottom
	    BELOW-MIN-Y EVEN-MAX-Y			 ;top of "below" zone
	    BELOW-MAX-Y (MAX PLANE-MAX-Y EVEN-MAX-Y))	 ;bottom

      ;;  In the next 3 loops, the less the overlap between the character box and the plane,
      ;;  the more inefficient the loops become, since we examine points outside the plane data structure
      ;;  which we know are empty.  On the other hand, situations of increasingly less overlap
      ;;  are also less likely to occur. The previous implementation tried to optimize by keeping
      ;;  plane references within the physical data structure, but it was difficult to decipher
      ;;  and was rife with subtle bugs.  The optimization was costing it too--the new version
      ;;  was written emphasizing correctness over efficiency, yet it runs faster than the previous
      ;;  "optimized" version.

      ;; Figure out the left and right coordinates of the extents.
      (LOOP FOR X FROM PLANE-MIN-X BELOW PLANE-MAX-X
	    DO (LOOP FOR Y FROM PLANE-MIN-Y BELOW PLANE-MAX-Y
		     DO (WHEN (NOT (ZEROP (PLANE-AREF PLANE X Y)))
			  (OR EVEN-MIN-X (SETQ EVEN-MIN-X X))
			  (SETQ EVEN-MAX-X (1+ X))
			  (RETURN))))
      
      ;; If no pixels were set, generate a "thin blank": (1 col) x (line-spacing) in size.
      (OR EVEN-MIN-X (SETQ EVEN-MIN-X 0
			   EVEN-MAX-X 1))

      ; If the left-kern value is going to be negative, check if the distance between the left edge of the char box
      ; and the rightmost extent of set pixels is still within range of the font's raster width.  If it is, then make the 
      ; leftmost extent be the left edge of the char box, so that the left-kern stays zero. - MRR 12/24/86
      (and (minusp (- CHAR-BOX-X1 EVEN-MIN-X))
	   (<= (- EVEN-MAX-X CHAR-BOX-X1) (tv:font-raster-width (symbol-value fontname)))
	   (setq even-min-x char-box-x1))

       ;; Notify the user of any set pixels in the "above" zone.
      (BLOCK nil
	(BLOCK FOO
	  (LOOP FOR Y FROM ABOVE-MIN-Y BELOW ABOVE-MAX-Y
		DO (LOOP FOR X FROM PLANE-MIN-X BELOW PLANE-MAX-X
			 DO (OR (ZEROP (PLANE-AREF PLANE X Y))
				(COND
				  ((Y-OR-N-P
				     "Dots above the top of the character box will be lost.  Store anyway? ")
				   (RETURN-FROM FOO nil))
				  (T (RETURN-FROM FONT-STORE-CD nil))))))
	  nil))

       ;; Similarly for the "below" zone.
      (BLOCK nil
	(BLOCK FOO
	  (LOOP FOR Y FROM BELOW-MIN-Y BELOW BELOW-MAX-Y
		DO (LOOP FOR X FROM PLANE-MIN-X BELOW PLANE-MAX-X
			 DO (OR (ZEROP (PLANE-AREF PLANE X Y))
				(COND
				  ((Y-OR-N-P
				     "Dots below the bottom of the character box will be lost.  Store anyway? ")
				   (RETURN-FROM FOO nil))
				  (T (RETURN-FROM FONT-STORE-CD nil))))))
	  nil))

       ;; Now transfer the data from the plane into the character's CD.
       ;; Some preliminaries, just for readability.
      (SETQ KERN (- CHAR-BOX-X1 EVEN-MIN-X))
      (SETQ XWIDTH (- EVEN-MAX-X EVEN-MIN-X))
      (SETQ YWIDTH (FD-LINE-SPACING FD))		 ;this attribute depends on the font, not the character
       ;; Initialize a new CD.
      (SETQ CD (MAKE-CHAR-DESCRIPTOR
		 :MAKE-ARRAY (:TYPE ART-4B :DIMENSIONS (LIST YWIDTH XWIDTH))
		 :CD-CHAR-WIDTH (- CHAR-BOX-X2 CHAR-BOX-X1)
		 :CD-CHAR-LEFT-KERN KERN))
      ;; Copy the data in the FED buffer into the CD.
      (LOOP FOR X FROM EVEN-MIN-X BELOW EVEN-MAX-X
	    AND FOR I FROM 0 BY 1
	    DO (LOOP FOR Y FROM EVEN-MIN-Y BELOW EVEN-MAX-Y
		     AND FOR J FROM 0 BY 1
		     DO (SETF (AREF CD J I) (PLANE-AREF PLANE X Y))))
       ;; Store the CD in turn into the FD.
      (COND
	(UPDATE-FONT-FLAG
	 ;; Use the CD just made to update the font itself, or make a new font.
	 (FONT-NAME-STORE-CD FONTNAME CD CHAR))
	(T
	 ;; Store the CD in the FD.
	 (LET ((CHAR-CD (CHAR-CODE CHAR)))
	   (AND (>= CHAR-CD (ARRAY-TOTAL-SIZE FD))
		(ADJUST-ARRAY FD *MAX-FONT-LENGTH*))
	   (SETF (AREF FD CHAR-CD) CD)
	   (AND (= CHAR #\SPACE)
		(SETF (FD-SPACE-WIDTH FD) (CD-CHAR-WIDTH CD))))))
      nil))) 

;;;; This will remove the character entirely from the font.
;(DEFUN COM-REMOVE-CHARACTER ()
;  (DECLARE (:self-flavor FED))
;  (IF (NULL CURRENT-FONT)
;      (BARF "No Current Font")
;      (LET* ((FD (FONT-GET-FD CURRENT-FONT))
	     
;	     (CH (PROMPT-LINE-READ "Character code to REMOVE: ")))
;	(COND
;	  ((NOT (INTEGERP CH)) (BARF "~S is not a number." CH))
;	  ((NOT (<= 0 CH (LENGTH FD))) (BARF "~D is out of range." CH))
;	  (T
;	   (WHEN (FED-Y-OR-N-P "Remove character code ~d from FONT ~a" CH CURRENT-FONT)
;	      (LET ((cd (MAKE-CHAR-DESCRIPTOR
;			      :MAKE-ARRAY (:TYPE ART-4B :DIMENSIONS (LIST 0 0))
;			      :CD-FILL-POINTER 0
;			      :CD-CHAR-WIDTH (FD-SPACE-WIDTH FD)
;			      :CD-CHAR-VERT-WIDTH 0
;			      :CD-CHAR-LEFT-KERN 0)))
;	        (FONT-NAME-STORE-CD CURRENT-FONT CD CH)
;		(SETF (AREF FD CH) NIL)
;		(LET ((CHAR-EXISTS-TABLE (W:FONT-CHARS-EXIST-TABLE (symbol-value CURRENT-FONT)))
;		      (FCWT (W:FONT-CHAR-WIDTH-TABLE (SYMBOL-VALUE CURRENT-FONT))))
;		  (AND (<= 128. CH 160.)
;		       FCWT
;		       (SETF (AREF FCWT CH) NIL))
;		  (AND CHAR-EXISTS-TABLE
;		       (SETF (AREF CHAR-EXISTS-TABLE CH) 0)))
;		(REMOVE-FED-EDITED-CHAR )
;		(WHEN (AND CURRENT-CHARACTER (CHAR-EQUAL CURRENT-CHARACTER CH))
;		      (SETQ CURRENT-CHARACTER NIL
;			    UNSAVED-CHANGES NIL)
;		      (SEND SELF :ERASE-ALL)
;		      (SEND SELF :MUST-REDISPLAY-LABEL)
;		      (SEND SELF :HOME-BOX)))))))))




;;; FED SCREEN OPERATIONS

; Define a new erase-all function that erases both the black and gray planes thus
;;;  truely erasing all!!!
(DEFUN COM-ERASE-ALL ()
  (AND (FED-Y-OR-N-P "Erase the black and gray planes? ")
       (SEND SELF :ERASE-ALL)))

(DEFMETHOD (FED :erase-all) ()
   (SEND self :erase-black)
   (SEND self :erase-gray))

;;; Define an erase-black function that erases the black plane (previously called erase-all).
(DEFUN COM-ERASE-BLACK ()
  (AND (FED-Y-OR-N-P "Erase the black plane? ")
       (SEND SELF :ERASE-BLACK)))


(DEFUN COM-ERASE-GRAY ()
  (AND (FED-Y-OR-N-P "Erase the gray plane? ")
       (SEND SELF :ERASE-GRAY))) 


(DEFUN COM-EXCHANGE-PLANES ()
  (DECLARE (:SELF-FLAVOR FED))
  (SEND SELF :MUST-REDISPLAY-CURRENT-PLANE-AREA)
  (SEND SELF :MUST-REDISPLAY-CURRENT-GRAY-AREA)
  (PSETQ GRAY-PLANE PLANE PLANE
     (OR GRAY-PLANE (MAKE-PLANE 2 :TYPE ART-4B :DEFAULT-VALUE 0 :EXTENSION 10)))
  (SETQ UNSAVED-CHANGES T)
  (SEND SELF :MOVE-CHAR-BOX GRAY-X-OFFSET GRAY-Y-OFFSET)
  (SETQ GRAY-X-OFFSET (- GRAY-X-OFFSET)
	GRAY-Y-OFFSET (- GRAY-Y-OFFSET))
  (DECF WINDOW-X-POS GRAY-X-OFFSET)
  (DECF WINDOW-Y-POS GRAY-Y-OFFSET)) 


(DEFUN COM-HOME ()
  (SEND SELF :HOME-BOX)) 

;;; Set the box-size (in both X and Y) of the fed-window to SCALE.
;;;  We try to keep the center of the window in the center.

(DEFUN COM-SCALE ()
  (DECLARE (:SELF-FLAVOR FED))
  (LET* ((SCALE
	  (IF NUMERIC-ARG-P
	      NUMERIC-ARG
	      (LET ((*READ-BASE* 10.)
		    (*PRINT-BASE* 10.))
		(PROMPT-LINE-DEFAULTED-READLINE BOX-X-SIZE 'GLOBAL:READ-FROM-STRING
						"Enter new box size, 1-50 (currently ~D) "
						BOX-X-SIZE)))))
    (COND
      ((AND (INTEGERP SCALE)
	    (> SCALE 0)
	    (< SCALE 51.)        ;1-50 base 10.
	    (< SCALE (TRUNCATE (W:SHEET-INSIDE-WIDTH SELF) 2))
	    (< SCALE (TRUNCATE (W:SHEET-INSIDE-HEIGHT SELF) 2)))
       (SETQ BOX-X-SIZE SCALE
	     BOX-Y-SIZE SCALE
	     REDISPLAY-DEGREE REDISPLAY-ALL)
       (send self :DEDUCE-WINDOW-ARRAY-SIZE)
       (SEND SELF :REDEFINE-MARGINS)
       (SEND SELF :HOME-BOX))
      ((BARF "Bad scale: ~D" SCALE))))) 

;;; Select a new mouse cursor character from a selection presented in a pop-up menu.
(DEFUN COM-SELECT-NEW-MOUSE ()
   (DECLARE (:self-flavor FED))
   (select-new-mouse basic-fed-flip-cursor "FLIP")
   (select-new-mouse basic-fed-draw-cursor "DRAW")
   (select-new-mouse basic-fed-erase-cursor "ERASE")
   (SETQ basic-fed-cursor (CASE DRAW-MODE
			   (2 basic-fed-erase-cursor)
			   (6 basic-fed-flip-cursor)
			   (7 basic-fed-draw-cursor)
			   (OTHERWISE basic-fed-flip-cursor))))

(DEFUN SELECT-NEW-MOUSE (cursor-mode mode-string)
   (DECLARE (:self-flavor FED))
   (LET* ((old-mouse-character (SEND cursor-mode :character))	 ;use current for initial menu position
	  (mouse-characters (LOOP for mouse-chars in mouse-list
			    collect (FIRST mouse-chars))) 
	  (mice-list (LOOP for mouse-entry in mouse-list
			    collect (LIST (STRING-APPEND "?" (STRING (int-char (FIRST mouse-entry))) "?")
					  :value mouse-entry
					  :font (FOURTH mouse-entry)
					  :documentation (STRING-APPEND "Select a new font editor "
									mode-string
									" mouse character."))))
	  (default-position (POSITION old-mouse-character mouse-characters :test #'eq)) ;;use position instead of find dkm

	  (new-mouse (W:menu-choose mice-list
				     :label (STRING-APPEND "Select a New " mode-string " Mouse")
				     :near-mode '(:mouse)
				     :default-item (IF default-position (NTH default-position mice-list)))))

     (WHEN new-mouse   ;use WHEN   dkm 5/13/87.
	 (SEND cursor-mode :set-character (FIRST new-mouse) (FOURTH new-mouse))
	 (SEND cursor-mode :set-offset (SECOND new-mouse) (THIRD new-mouse)))))


(DEFUN COM-MODIFY-DISPLAY-VARIABLES ()
  (DECLARE (:SELF-FLAVOR FED))
  (LET ((*PRINT-BASE* 10.)
	(*READ-BASE* 10.))
    (W:CHOOSE-VARIABLE-VALUES
      '((*SAMPLE-FONT* :MENU `,(LOADED-FONTS-ON-MACHINE))
	(*LABEL-BASE* :DOCUMENTATION "Specify the base to print the column label numbers (non-negative-fixnum)"
		      :NON-NEGATIVE-FIXNUM)
	(*COLUMNS* :DOCUMENTATION "Specify number of columns to use:  NIL - largest power of 2 that fits,
NEGATIVE or 0 - use maximum columns, POSITVE NUMBER (not recommended) - the specific number of columns to use ."
		   :fixnum-or-nil))
      :LABEL "MODIFY VARIABLES FOR DISPLAYING A FONT"
      :SUPERIOR (SEND SELF :SUPERIOR)
      :margin-choices '("Do It" ("Abort" (signal-condition  eh:*abort-object*)))
      )))
 
;;; MERGE STUFF

(DEFPARAMETER MERGE-GRAY-MENU
   '(("Copy" :VALUE :COPY) ("Set bits" :VALUE :SET) ("Clear bits" :VALUE :CLEAR)
     ("Flip bits" :VALUE :FLIP))) 

;Right button on MERGE.  Uses menu to choose merge mode.

(DEFUN COM-MERGE-GRAY-MENU ()
  (DECLARE (:SELF-FLAVOR FED))
  (LET ((SUBOP (FED-CHOOSE MERGE-GRAY-MENU "Merge gray into black")))
    (IF SUBOP
      (SEND SELF :MERGE-CONTENTS SUBOP GRAY-PLANE (+ CHAR-BOX-X1 GRAY-X-OFFSET)
	 (+ CHAR-BOX-Y1 GRAY-Y-OFFSET) (+ CHAR-BOX-X2 GRAY-X-OFFSET)
	 (+ CHAR-BOX-Y2 GRAY-Y-OFFSET) (+ CHAR-BOX-Y3 GRAY-Y-OFFSET))))) 

;;;  merge in the data from the gray plane, doing IOR,
;;;  or possibly ask give the  merge menu.
(DEFUN COM-MERGE-CHARACTER ()
  (DECLARE (:SELF-FLAVOR FED))
  (COND
    ((ZEROP (CHAR-BITS COMMAND-CHAR))		;No control bits are on
     (COM-MERGE-GRAY))
    (T (COM-MERGE-GRAY-MENU))))			;Give menu if control bits set.

;;; Simple merge-gray.  Uses IOR.

(DEFUN COM-MERGE-GRAY ()
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ UNSAVED-CHANGES T)
  (MERGE-OTHER-PLANE GRAY-PLANE GRAY-X-OFFSET GRAY-Y-OFFSET)) 


;;; Move the character and char box somewhere on the grid

(DEFUN COM-SHIFT-WINDOW (&AUX DISTANCE DX DY ARROW)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ DISTANCE (IF NUMERIC-ARG-P
		   NUMERIC-ARG
		   10))
  (SETQ ARROW (INT-CHAR (CHAR-CODE COMMAND-CHAR)))      ;Char object without modifier bits
  (SETQ DX (* DISTANCE (OR (SECOND (ASSOC ARROW '((#\LEFT-ARROW  -1) (#\RIGHT-ARROW 1)) :TEST #'=)) 0)))
  (SETQ DY (* DISTANCE (OR (SECOND (ASSOC ARROW '((#\UP-ARROW -1) (#\DOWN-ARROW 1)) :TEST #'=)) 0)))
  (SEND SELF :MOVE-PLANE DX DY)) 

;;; rb 2/3/85 - new function so prompts interpret mouse handedness correctly

(DEFUN BUTTON-STRING (BUTTON); BUTTON is which button to use (a symbol) for standard right-handed operation.
 ; Returns which button really gets used (a symbol), taking handedness into account. 
  (CASE BUTTON
    (LEFT (IF (EQ W:MOUSE-HANDEDNESS :RIGHT)  ;check for :right instead of w:right   -dkm 5/87
	    'LEFT
	    'RIGHT))
    (MIDDLE 'MIDDLE)
    (RIGHT (IF (EQ W:MOUSE-HANDEDNESS :RIGHT) ;check for :right instead of w:right   -dkm 5/87
	     'RIGHT
	     'LEFT))))

(DEFUN COM-MOUSE-SHIFT-WINDOW ()
  (MOUSE-MOVE-OPERATION :MOVE-PLANE "Move both planes.  Mark reference point with ~a button."
			(BUTTON-STRING 'LEFT) "Select another point to move the first one to."
			nil)) 


(DEFUN COM-MOUSE-SHIFT-GRAY ()
  (MOUSE-MOVE-OPERATION :MOVE-GRAY-PLANE
			"Move gray plane.  Mark reference point with ~a button."
			(BUTTON-STRING 'LEFT) "Select another point to move the first one to."
			nil)) 


(DEFUN MOUSE-MOVE-OPERATION (WINDOW-OP STRING1 ARGS1 STRING2 ARGS2 &AUX OX OY X Y)
  "Get two points using the mouse, then perform WINDOW-OP on the delta between the points.
WINDOW-OP is sent to SELF with two args, the delta X and the delta Y.
STRING1 is printed in the prompt area while waiting for the first point
and STRING2 is printed while waiting for the second."
  (DECLARE (:SELF-FLAVOR FED) (SPECIAL FED-FRAME))
  (BLOCK nil
    (UNWIND-PROTECT
	(PROGN
	  (PROMPT-LINE STRING1 ARGS1)
	  (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
		'(:MOUSE-L-1 "Specify reference point" :MOUSE-M-1 "Abort" :MOUSE-R-1
			     "Abort"))
	  (SETF (VALUES OX OY) (SEND SELF :MOUSE-SELECT-POINT))
	  (OR OX (RETURN nil))
	  (SEND SELF :GRAY-POINT OX OY)
	  (PROMPT-LINE STRING2 ARGS2)
	  (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
		'(:MOUSE-L-1 "Specify point to move reference to" :MOUSE-M-1 "Abort"
			     :MOUSE-R-1 "Abort"))
	  (UNWIND-PROTECT (SETF (VALUES X Y) (SEND SELF :MOUSE-SELECT-POINT))
	    (SEND SELF :GRAY-POINT OX OY)))
      (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION nil)
      (SEND FED-FRAME :HANDLE-PROMPT))
    (OR X (RETURN nil))
    (SEND SELF WINDOW-OP (- X OX) (- Y OY))
    nil)) 

(DEFUN COM-MOUSE-MOVE-CHAR-BOX (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ UNSAVED-CHANGES T)				;CURSOR-ON nil   ;No keyboard cursor anymore
  (SEND SELF :MOUSE-MOVE-CHAR-BOX)
  (SEND SELF :MUST-REDISPLAY-LABEL)) 


(DEFUN COM-MOUSE-DRAW-LINE (&AUX OX OY X Y)
  (DECLARE (:SELF-FLAVOR FED) (SPECIAL FED-FRAME))
  (BLOCK nil
    (UNWIND-PROTECT (PROGN
		     (PROMPT-LINE "Select end points with ~a mouse button" (BUTTON-STRING 'LEFT))
		     (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
			   '(:MOUSE-L-1 "Specify end points of line" :MOUSE-M-1 "Abort"
			     :MOUSE-R-1 "Abort"))
		     (SETF (VALUES OX OY) (SEND SELF :MOUSE-SELECT-POINT))
		     (OR OX (RETURN nil))
		     (SEND SELF :GRAY-POINT OX OY)
		     (UNWIND-PROTECT (SETF (VALUES X Y)
					   (SEND SELF :MOUSE-SELECT-POINT))
		       (SEND SELF :GRAY-POINT OX OY)))
      (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION nil)
      (SEND FED-FRAME :HANDLE-PROMPT))
    (OR X (RETURN nil))
    (SEND SELF :DRAW-GRID-LINE OX OY X Y DRAW-MODE)
    (SETQ UNSAVED-CHANGES T)
    nil)) 

;;; DRAW A SPLINE ON THE GRID

(DEFVAR SPLINE-X) 

(DEFVAR SPLINE-Y) 

(DEFVAR SPLINE-CX nil) 

(DEFVAR SPLINE-CY nil) 

(DEFUN COM-MOUSE-DRAW-SPLINE (&AUX I Y)
  (DECLARE (:SELF-FLAVOR FED) (SPECIAL FED-FRAME))
  (COND
    ((NOT (VARIABLE-BOUNDP SPLINE-X))
     (SETQ SPLINE-X (MAKE-ARRAY 144 :LEADER-LIST '(0))
	   SPLINE-Y (MAKE-ARRAY 144 :LEADER-LIST '(0)))))
  (STORE-ARRAY-LEADER 0 SPLINE-X 0)
  (STORE-ARRAY-LEADER 0 SPLINE-Y 0)
  (PROMPT-LINE
   "~A Spline.  Select points with ~a mouse button.
~a to abort.  Click ~a when done."
   (SEND SELF :DRAW-MODE-STRING) (BUTTON-STRING 'LEFT) (BUTTON-STRING 'MIDDLE)
	       (BUTTON-STRING 'RIGHT))
  (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
	'(:MOUSE-L-1 "Specify points" :MOUSE-M-1 "Abort" :MOUSE-R-1 "Finish"))
  (UNWIND-PROTECT (DO ((X)) (NIL)
		   (MULTIPLE-VALUE-SETQ (X Y)
		     (SEND SELF :MOUSE-SELECT-POINT T))
		   (OR X (RETURN nil))
		   (SEND SELF :GRAY-POINT X Y)
		   (VECTOR-PUSH-EXTEND X SPLINE-X)
		   (VECTOR-PUSH-EXTEND Y SPLINE-Y))
    (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION nil)
    (SEND FED-FRAME :HANDLE-PROMPT)
    (DOTIMES (I (ARRAY-ACTIVE-LENGTH SPLINE-X))		;Erase old marks
      (SEND SELF :GRAY-POINT (AREF SPLINE-X I) (AREF SPLINE-Y I))))
  (COND
    ((AND (> (ARRAY-ACTIVE-LENGTH SPLINE-X) 1)   ;need at least two points to draw  dkm 5/12/87
	  (CONSP Y)
	  (EQ (FIRST Y) :MOUSE-BUTTON)
	  (= (SECOND Y) #\MOUSE-R))
     (MULTIPLE-VALUE-SETQ (SPLINE-CX SPLINE-CY I)
			  (W:SPLINE SPLINE-X SPLINE-Y 12 SPLINE-CX SPLINE-CY))
;; When the current draw-mode is FLIP then we ignore it to draw the spline. 
;; This is required because otherwise the points that are used to draw the spline are so
;; close together that the same pixel is turned on and off many times during the draw
;; routine and the result is a discontinuous spline.  The best way to fix this problem would be 
;; examine the points in the spline array and remove the extra ones that fall in the same 
;; pixel in the drawing pane.  Currently it requires too much work to fix such a minor problem.
     (SEND SELF :DRAW-CURVE SPLINE-CX SPLINE-CY I (IF (= DRAW-MODE W:ALU-XOR)
						      W:ALU-IOR
						      DRAW-MODE))
     (SETQ UNSAVED-CHANGES T))
    (T (BEEP))))


;;; OPERATION ON A RECTANGLE IN THE GRID

;;;  We don't use this anymore. Just use the current mode. See Com-operate-on-rectangle definition.
;(DEFPARAMETER RECTANGLE-OPERATION-MENU-ALIST
;   `(("Clear rectangle" :VALUE ,W:ALU-ANDCA) ("Set rectangle" :VALUE ,W:ALU-IOR)
;     ("Flip rectangle" :VALUE ,W:ALU-XOR))) 


(DEFUN COM-OPERATE-ON-RECTANGLE ()
  (DECLARE (:SELF-FLAVOR FED) (SPECIAL FED-FRAME))
  (PROG (X Y OX OY
	 ;; No need to ask for an operation type.  Just use current mode like everything else does.
;	 (BOOLE-OP (IF CLEAR-FLAG
;		       W:ALU-ANDCA
;		       (FED-CHOOSE RECTANGLE-OPERATION-MENU-ALIST "Operation on rectangle")))
	 )
;    (OR BOOLE-OP (RETURN nil))
    (UNWIND-PROTECT
	(PROGN
	  (PROMPT-LINE
	    "~a rectangle.  Select one corner with ~a button." 
	    (SEND SELF :DRAW-MODE-STRING)
	    (BUTTON-STRING 'LEFT))
	  (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION
		'(:MOUSE-L-1 "Specify corner" :MOUSE-M-1 "Abort" :MOUSE-R-1 "Abort"))
	  (SETF (VALUES OX OY) (SEND SELF :MOUSE-SELECT-POINT))
	  (OR OX (RETURN nil))
	  (SEND SELF :GRAY-POINT OX OY)
	  (PROMPT-LINE "Select the other corner with ~a button."
		       (BUTTON-STRING 'LEFT))
	  (UNWIND-PROTECT (SETF
			    (VALUES X Y)
			    (SEND SELF :MOUSE-SELECT-POINT))
	    (SEND SELF :GRAY-POINT OX OY)))
      (SETQ SPECIAL-COMMAND-MOUSE-DOCUMENTATION nil)
      (SEND FED-FRAME :HANDLE-PROMPT))
    (OR X (RETURN nil))
    (LET ((MIN-X (+ WINDOW-X-POS (MIN X OX)))
	  (MIN-Y (+ WINDOW-Y-POS (MIN Y OY)))
	  (MAX-X (+ WINDOW-X-POS (MAX X OX)))
	  (MAX-Y (+ WINDOW-Y-POS (MAX Y OY))))
      (DO ((I MIN-X (1+ I)))
	  ((> I MAX-X))
	(DO ((J MIN-Y (1+ J)))
	    ((> J MAX-Y))
	  (PLANE-ASET (BOOLE DRAW-MODE 1 (PLANE-AREF PLANE I J)) PLANE I J)))
      (SETQ UNSAVED-CHANGES T)
      (SEND SELF :MUST-REDISPLAY REDISPLAY-SOME (- MIN-X WINDOW-X-POS) (- MIN-Y WINDOW-Y-POS)
	 (- MAX-X WINDOW-X-POS) (- MAX-Y WINDOW-Y-POS))))) 

;;; If the next thing the user does is click LEFT,
;;; Return the X and Y co-ords of the grid point the user clicks the mouse on.
;;; Otherwise discard his input, beep, and return NIL and the char.

(DEFMETHOD (FED :MOUSE-SELECT-POINT) (&OPTIONAL RIGHT-BUTTON-OK &AUX CH X Y)
  (SETQ CH (SEND SELF :READ-ANY))
  (COND
    ((AND (CONSP CH) (EQ (FIRST CH) :MOUSE-BUTTON) (= (SECOND CH) #\MOUSE-L))
     (MULTIPLE-VALUE-BIND (DX DY)
       (W:SHEET-CALCULATE-OFFSETS SELF W:MOUSE-SHEET)
       (SETQ X (TRUNCATE (- W:MOUSE-X DX W:LEFT-MARGIN-SIZE) BOX-X-SIZE)
	     Y (TRUNCATE (- W:MOUSE-Y DY W:TOP-MARGIN-SIZE) BOX-Y-SIZE)))
     (AND (< -1 X WINDOW-X-SIZE)
	  (< -1 Y WINDOW-Y-SIZE)
	  (VALUES X Y)))
    ((AND RIGHT-BUTTON-OK (CONSP CH) (EQ (FIRST CH) :MOUSE-BUTTON) (= (SECOND CH) #\MOUSE-R))
     (VALUES nil CH))
    (T (BEEP) (VALUES nil CH)))) 


;;; OPERATIONS ON REGISTERS.

(DEFPARAMETER FED-REGISTER-OP-MENU-ALIST
   '(("Clear register" :VALUE (:CLEAR-REG)) ("Save black" :VALUE (:LOAD-REG))
     ("Save gray" :VALUE (:LOAD-REG-GRAY)) ("Restore to black" :VALUE (:LOAD-BLACK))
     ("" :NO-SELECT NIL) ("Merge Operations" :NO-SELECT NIL :FONT :MENU-STANDOUT)
     ("Copy to black" :VALUE (:MERGE-BLACK :COPY)) ("Copy to gray" :VALUE (:MERGE-GRAY :COPY))
     ("Set bits in black" :VALUE (:MERGE-BLACK :SET))
     ("Set bits in gray" :VALUE (:MERGE-GRAY :SET))
     ("Clear bits in black" :VALUE (:MERGE-BLACK :CLEAR))
     ("Clear bits in gray" :VALUE (:MERGE-GRAY :CLEAR))
     ("Flip bits in black" :VALUE (:MERGE-BLACK :FLIP))
     ("Flip bits in gray" :VALUE (:MERGE-GRAY :FLIP)))) 


;;; We get here from a mouse click over a register.
;;; Insure Command-Char is set NIL (non-numberp) so a right click won't
;;; cause prompting in the ucl-prompt window due to a prior ucl-keyboard entry.
(DEFMETHOD (FED :REGISTER-CLICK) (REGISTER CLICK)
  (COND ((= CLICK #\MOUSE-1-1)
	 (SEND REGISTER :SET-CONTENTS PLANE
		  CHAR-BOX-X1 CHAR-BOX-Y1
		  CHAR-BOX-X2 CHAR-BOX-Y2
		  CHAR-BOX-Y3))
	((= CLICK #\MOUSE-2-1)
	 (APPLY SELF :SET-CONTENTS (SEND REGISTER :CONTENTS)))
	((= CLICK #\MOUSE-3-1)
	 (LET* ((command-char NIL)		 ;; insure following menu is mouse pop-up one
		(SUBOP (FED-CHOOSE FED-REGISTER-OP-MENU-ALIST "Register operations")))	 
	   (CASE (FIRST SUBOP)
	     (:CLEAR-REG (SEND REGISTER :ERASE-ALL)
			 (SEND REGISTER :CLEAR-SCREEN))
	     (:LOAD-REG (SEND REGISTER :SET-CONTENTS PLANE
				 CHAR-BOX-X1 CHAR-BOX-Y1
				 CHAR-BOX-X2 CHAR-BOX-Y2
				 CHAR-BOX-Y3))
	     (:LOAD-REG-GRAY (SEND REGISTER :SET-CONTENTS GRAY-PLANE
				      (+ CHAR-BOX-X1 GRAY-X-OFFSET)
				      (+ CHAR-BOX-Y1 GRAY-Y-OFFSET)
				      (+ CHAR-BOX-X2 GRAY-X-OFFSET)
				      (+ CHAR-BOX-Y2 GRAY-Y-OFFSET)
				      (+ CHAR-BOX-Y3 GRAY-Y-OFFSET)))
	     (:LOAD-BLACK (APPLY SELF :SET-CONTENTS (SEND REGISTER :CONTENTS)))
	     (:MERGE-BLACK (APPLY SELF :MERGE-CONTENTS (SECOND SUBOP) (SEND REGISTER :CONTENTS)))
	     (:MERGE-GRAY (APPLY SELF :MERGE-GRAY (SECOND SUBOP) (SEND REGISTER :CONTENTS))))))))

(DEFUN COM-CLEAR-ALL-REGISTERS ()
   (DECLARE (:self-flavor FED))
   (IF (fed-y-or-n-p "  Clear all registers?  ")
       (LET ((PANES (SEND (SEND SELF :SUPERIOR) :INFERIORS)))
	 (DOLIST (PANE PANES)
	    (WHEN (TYPEP PANE 'REGISTER-PANE)
	       (SEND PANE :ERASE-ALL)
	       (SEND PANE :CLEAR-SCREEN))))))


;;; Display Support for the Fed-Label-Window

(DEFUN REDISPLAY-LABELS ()
  "Redisplay the labels of all FED windows displaying the font current in this one."
  ;;That is in case they are displaying a sample string
  ;;which includes the char being edited in this FED.
  (DECLARE (:SELF-FLAVOR FED))
  (DISPLAY-LABEL)
  (DOLIST (ELT FED-EDITED-CHARS)
    (AND (EQ CURRENT-FONT (FIRST ELT))
	 (NEQ SELF (THIRD ELT))
	 (SEND (THIRD ELT) :REDISPLAY-LABEL-IF-EXPOSED)))
  )


(DEFMETHOD (FED :MUST-REDISPLAY-LABEL) ()
  (SEND SELF :MUST-REDISPLAY REDISPLAY-ONE
	MIN-CHANGED-X MIN-CHANGED-Y MIN-CHANGED-X MIN-CHANGED-Y)) 

   
;Redisplay the label of a FED window (usually not the selected one).
(DEFMETHOD (FED :REDISPLAY-LABEL-IF-EXPOSED) ()
  (COND
    ((SEND W:SUPERIOR :EXPOSED-P) (DISPLAY-LABEL)))) 

;;; Add additional information to label window - namely
;;;   fd-rotation value, space-width value
;;; Add use of multiple fonts to make mouse-selectable fields stand out.
;;; Add change-of-mode capability via mouse-sensitive current mode indicator.
(DEFUN DISPLAY-LABEL ()
  "Redisplay the label window of this FED window's frame."
  (DECLARE (:SELF-FLAVOR FED))
  (SEND label-window :expose)

  ;; Clean up the state of the window, to make it the ordinary one for the default font.
  (SETF (W:SHEET-LINE-HEIGHT LABEL-WINDOW)
	(W:FONT-CHAR-HEIGHT (W:SCREEN-DEFAULT-FONT (W:SHEET-GET-SCREEN SELF))))
  (SETF (W:SHEET-BASELINE LABEL-WINDOW)
	(W:FONT-BASELINE (W:SCREEN-DEFAULT-FONT (W:SHEET-GET-SCREEN LABEL-WINDOW))))
  (W:SHEET-SET-FONT LABEL-WINDOW
		    (W:SCREEN-DEFAULT-FONT (W:SHEET-GET-SCREEN LABEL-WINDOW)))
  (SEND LABEL-WINDOW :CLEAR-SCREEN)
  ;; Avoid inexplicable **MORE** from FED-TYO.
  (SETF (W:SHEET-MORE-VPOS LABEL-WINDOW) NIL)
  (send label-window :set-vsp 1)                                      ;makes mouse boxes look better -dkm 4/24/87

  (LET* ((*STANDARD-OUTPUT* LABEL-WINDOW)
;	 (SECOND-LINE-HEIGHT (SEND LABEL-WINDOW :SECOND-LINE-HEIGHT))  ;not used anymore  -dkm 4/24/87
	 (CURRENT-FONT-LINE-HEIGHT (or (and current-font (fd-line-spacing (FONT-GET-FD CURRENT-FONT))) 0))     ;-dkm 4/24/87
	 (FONT-FITS-P (<= CURRENT-FONT-LINE-HEIGHT 25.)))  ;max height font can be to show them a sample string. -dkm 4/24/87
    (SEND label-window :set-current-font 1)
    (SEND label-window :item 'document-item " Mode: ")
    (SEND label-window :set-current-font 0)
    (SEND LABEL-WINDOW :ITEM 'mouse-edit-mode
		   (SEND SELF :DRAW-MODE-STRING))
    (PRINC " ")
    (SEND label-window :set-current-font 1)
    (SEND label-window :item 'document-item " Font: ")
    (SEND label-window :set-current-font 0)
    (SEND LABEL-WINDOW :ITEM 'FONT
	  (OR CURRENT-FONT "None"))
    (SEND label-window :set-current-font 1)
    ;; Now describe the current character.
    ;; We have to print SOMETHING even if there is no current character
    ;; so that there is someplace to put the mouse sensitive items.
    ;; On the other hand, if there is no current FONT, omit these things
    ;; because we don't WANT to tempt the user to try to specify a character.
    (COND (CURRENT-FONT
	   (LET ((*PRINT-BASE* 10.) (*NOPOINT T)
		 (FD (FONT-GET-FD CURRENT-FONT)))
	     (SEND label-window :item 'document-item "  Total Ht: ")
	     (SEND label-window :set-current-font 0)
	     (SEND LABEL-WINDOW :ITEM 'LINE-HEIGHT
		   (FD-LINE-SPACING FD))
	     (SEND label-window :set-current-font 1)
	     (SEND label-window :item 'document-item "  Above Base: ")
	     (SEND label-window :set-current-font 0)
	     (SEND LABEL-WINDOW :ITEM 'BASELINE
		   (FD-BASELINE FD))
	     (SEND label-window :set-current-font 1)
	     (SEND label-window :item 'document-item "  Blinker: ")
	     (SEND label-window :set-current-font 0)
	     (SEND LABEL-WINDOW :ITEM 'BLINKER-WIDTH
		   (FD-BLINKER-WIDTH FD))
	     (SEND label-window :set-current-font 1)
	     (PRINC "x")
	     (SEND label-window :set-current-font 0)
	     (SEND LABEL-WINDOW :ITEM 'BLINKER-HEIGHT
		   (FD-BLINKER-HEIGHT FD))
	     (SEND label-window :set-current-font 1)
	     (SEND label-window :item 'document-item "  Rotate: ")
	     (SEND label-window :item 'rotation
		   (fd-rotation fd))
	     (SEND label-window :item 'document-item "  Space: ")
	     (SEND label-window :item 'space-width
		   (fd-space-width fd))
	     (COMMENT "great help for debugging"
		      (COND (current-character
		    (LET ((cd (AREF fd (CHAR-INT current-character))))
		      (COND (cd
			     (PROG nil
				   (PRINC "  (Ar-Ht: ")
				   (SEND label-window :item 'array-height
					 (ARRAY-DIMENSION cd 0))
				   (PRINC "  Ar-Wd: ")
				   (SEND label-window :item 'array-width
					 (ARRAY-DIMENSION cd 1))
				   (PRINC "  Kern: ")
				   (SEND label-window :item 'kern
					 (cd-char-left-kern cd)))
			     (PRINC ")"))))))   )
	     (SEND label-window :set-current-font 0))
	   
;	   (AND CURRENT-FONT				;This isn't necessary anymore.   -dkm 4/24/87
;		(SETF (W:SHEET-BASELINE LABEL-WINDOW)
;		      (MAX (W:SHEET-BASELINE LABEL-WINDOW)
;			   (FD-BASELINE (FONT-GET-FD CURRENT-FONT)))))

	   (SEND label-window :set-vsp 12.) ;pushes second line down a ways for the CRLF. -dkm 4/24/87

	   (W:SHEET-CRLF LABEL-WINDOW)
	   ;; This font is already current, but setting it now
	   ;; makes the vertical position come out right.
	   (SEND label-window :set-vsp 1.)  ;this makes the mouse box the items better. -dkm 4/24/87
	   (W:SHEET-SET-FONT LABEL-WINDOW
			      (W:SCREEN-DEFAULT-FONT (W:SHEET-GET-SCREEN LABEL-WINDOW)))

	   ;; this used to make the mouse sensitive items come out the right height     not needed anymore  -dkm 4/24/87
;	   (SETF (W:SHEET-LINE-HEIGHT LABEL-WINDOW) second-line-height))

	   (COMMENT (W:sheet-increment-bitpos label-window 0 (- (W:sheet-line-height label-window)
							(W:sheet-baseline label-window))))
	   (SEND label-window :set-current-font 2)
	   (IF UNSAVED-CHANGES
	       (SEND label-window :item 'document-item " CHG ")
	       (PRINC "     "))
	   (SEND label-window :set-current-font 1)
	   (SEND label-window :item 'document-item "  Char:  ")
	   (SEND label-window :set-current-font 0)
	   (IF CURRENT-CHARACTER
	       (SEND LABEL-WINDOW :ITEM 'CHAR-BY-NUMBER NIL
		     "~3D" (CHAR-INT CURRENT-CHARACTER))	 ; change to decimal representation
	       (SEND LABEL-WINDOW :ITEM 'CHAR-BY-NUMBER "nnn"))
	   (PRINC " ")
	   (LET ((OLD-X (W:SHEET-CURSOR-X LABEL-WINDOW))
		 (ADJ-Y (IF FONT-FITS-P                          ;adj-y added to box actual output text  -dkm 4/24/87
			    (- CURRENT-FONT-LINE-HEIGHT (W:SHEET-BASELINE LABEL-WINDOW) 1)
			    0)))
	     (COND ((NULL CURRENT-CHARACTER)
		    (PRINC "None")
		    (SETQ adj-y 0))				  ;No characters yet, don't adjust y  -dkm 4/24/87
		   (T
		    (IF (= CURRENT-CHARACTER #\SP)
			(PRINC "Space")
			(format label-window "~:@C" current-character)) 
		    (WRITE-CHAR #\ )
		    (if FONT-FITS-P     ;-dkm 4/23/87
			(FED-TYO LABEL-WINDOW CURRENT-CHARACTER CURRENT-FONT)
			(write-char #\space))))
	     (SEND LABEL-WINDOW :PRIMITIVE-ITEM-OUTSIDE 'CHAR NIL
		   OLD-X
		   (MIN (W:SHEET-CURSOR-Y LABEL-WINDOW)		 ;use adj-y to determine top of box  -dkm 4/24/87
			(- (W:SHEET-CURSOR-Y LABEL-WINDOW) ADJ-Y))
		   (+ 2 (W:SHEET-CURSOR-X LABEL-WINDOW))
		   (+ (W:SHEET-CURSOR-Y LABEL-WINDOW)
		      (W:SHEET-LINE-HEIGHT LABEL-WINDOW))))
	   (W:SHEET-SET-FONT LABEL-WINDOW
			      (W:SCREEN-DEFAULT-FONT (W:SHEET-GET-SCREEN LABEL-WINDOW)))

;	   (AND CURRENT-FONT                   			; No need to mess with the baseline.    dkm 4/23/87
;		(SETF (W:SHEET-BASELINE LABEL-WINDOW)
;		      (MAX (W:SHEET-BASELINE LABEL-WINDOW)
;			   (FD-BASELINE (FONT-GET-FD CURRENT-FONT)))))

	   (SEND label-window :set-current-font 1)
	   (SEND label-window :item 'document-item "  Width: ")
	   (SEND label-window :set-current-font 0)
	   (SEND label-window :item 'width NIL "~D" (- CHAR-BOX-X2 CHAR-BOX-X1))
	    
	   (cond (FONT-FITS-P   ;only write out sample string if it fits  -dkm 4/23/87
		  
		  (SEND label-window :set-current-font 1)
		  (SEND label-window :item 'document-item "  Sample: ")
		  
		  ;; Make the sample string area mouse sensitive
		  ;; regardless of whether we display any sample string
		  ;; or how wide it appears.
		  (SEND LABEL-WINDOW :PRIMITIVE-ITEM-OUTSIDE 'SAMPLE-STRING NIL
			(W:SHEET-CURSOR-X LABEL-WINDOW)
			(min (w:sheet-cursor-y label-window)    ;changed to box actual size of sample string -dkm 4/24/87
			     (- (W:SHEET-CURSOR-Y LABEL-WINDOW)
				(- current-font-line-height (w:sheet-baseline label-window) 1)))
			(W:SHEET-INSIDE-RIGHT LABEL-WINDOW)
			(+ (W:SHEET-CURSOR-Y LABEL-WINDOW)
			   (W:SHEET-LINE-HEIGHT LABEL-WINDOW)))
		  (COND ((AND SAMPLE-STRING)
			 (AND (BOUNDP CURRENT-FONT) (SYMBOL-VALUE CURRENT-FONT)
			      (W:SHEET-SET-FONT LABEL-WINDOW (SYMBOL-VALUE CURRENT-FONT)))
			 (CATCH 'tv:line-overflow
			   (DOTIMES (I (MAX (1- (LENGTH SAMPLE-STRING)) 0))
			     (FED-TYO LABEL-WINDOW (AREF SAMPLE-STRING I) CURRENT-FONT))))))
		 (T (send label-window :set-current-font 0)
		    (princ "  (Font is too large to show sample string)" LABEL-WINDOW)))
	   )))
  (W:SHEET-SET-FONT LABEL-WINDOW
		     (W:SCREEN-DEFAULT-FONT (W:SHEET-GET-SCREEN LABEL-WINDOW))))


;;; Print a character on a sheet, assuming that sheet is set up to that font.
;;;   If the character is the one being edited,
;;;   the picture being edited is displayed.


(DEFUN FED-TYO (SHEET CH FONTNAME)
  "Output CH on SHEET in font FONTNAME, as the character appears in FED.
This function is like TYO except the character appears as it would
appear in the FED that is editing the character, not as it actually is in the font.
FONTNAME should also be the current font of SHEET
in case the character is not being edited in any FED."
  (DECLARE (:SELF-FLAVOR FED))
  (IF (AND (EQ FONTNAME CURRENT-FONT)
	   (EQ CH CURRENT-CHARACTER))
      (SEND SELF :TYO-EDITED-CHAR SHEET)
      (W:SHEET-TYO SHEET CH)))


;;; rb 11/25/85 - fixes overprinting into other windows to the right of this one

(DEFMETHOD (BASIC-FED :TYO-EDITED-CHAR) (SHEET)
 ;;; This routine is the FED's substitute for the window system's DRAW-CHAR.
 ;;; It prints the current appearance of a character being edited by FED,
 ;;; whereas DRAW-CHAR prints the actual font character.
 ;;; The basic algorithm is, given the absolute pixel coordinates of the cursor,
 ;;; draw dots relative to that spot using FED's plane data structure.
  (LET* ((LEFT (+ (- (W:SHEET-CURSOR-X SHEET) CHAR-BOX-X1)
		  (FIRST (PLANE-ORIGIN PLANE))))
	 ;; Ditto for topmost pixel coordinate.
	 (TOP (+ (- (W:SHEET-CURSOR-Y SHEET) CHAR-BOX-Y2) (W:SHEET-BASELINE SHEET)
		 (SECOND (PLANE-ORIGIN PLANE))))
	 ;; Determine the smaller of the width of the plane or the distance to the margin.
	 ;; Prevents overwriting into any windows to the right.
	 ;; We should also check the distance to the left margin to prevent the same
	 ;; problem there, but given the way the FED display is currently laid out,
	 ;; this is extremely unlikely to occur.
	 (PLANE-WIDTH (MIN (FIRST (ARRAY-DIMENSIONS PLANE))
			   (- (W:SHEET-INSIDE-RIGHT SHEET) LEFT)))
	 ;; First vertical idx to print from in plane.
	 (PLANE-TOP (MAX 0 (- CHAR-BOX-Y1 (SECOND (PLANE-ORIGIN PLANE)))))
	 ;; Last+1 vertical idx to print from in plane.
	 (PLANE-BOTTOM (MIN (SECOND (ARRAY-DIMENSIONS PLANE))
			    (- CHAR-BOX-Y3 (SECOND (PLANE-ORIGIN PLANE))))))
    (W:PREPARE-SHEET (SHEET)
      (DOTIMES (HPOS PLANE-WIDTH)
	(DO ((VPOS PLANE-TOP (1+ VPOS)))
	    ((>= VPOS PLANE-BOTTOM))
	  (OR (ZEROP (AREF PLANE HPOS VPOS))
	      (SYS:%DRAW-RECTANGLE 1 1 (+ HPOS LEFT) (+ VPOS TOP) W:ALU-IOR SHEET))))
      (W:SHEET-INCREMENT-BITPOS SHEET (- CHAR-BOX-X2 CHAR-BOX-X1) 0)))) 


;;; SETTING THE FONT PARAMETERS (IN RESPONSE TO CLICKS ON THE FED-LABEL-WINDOW)

;Comes from clicking on font name in label window.
(DEFMETHOD (FED :PROMPT-LINE-SELECT-FONT) (IGNORE)
  (COM-LIST-FONTS)) 

;When you click on the sample line
(DEFMETHOD (FED :PROMPT-LINE-SET-SAMPLE) (&REST IGNORE)
  (COM-SET-SAMPLE)) 

(DEFUN COM-SET-SAMPLE ()
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ SAMPLE-STRING (PROMPT-LINE-READLINE "String to display in ~A: " CURRENT-FONT))
  (AND (ZEROP (LENGTH SAMPLE-STRING)) (SETQ SAMPLE-STRING nil))
  (SEND SELF :MUST-REDISPLAY-LABEL)) 

;When you click on the character
(DEFMETHOD (FED :PROMPT-LINE-SELECT-CHAR) (IGNORE)
  (COM-SPECIFY-CHARACTER)) 

;When you click on current character code in fed-label-window.
(DEFMETHOD (FED :PROMPT-LINE-SELECT-CHAR-CODE) (IGNORE)
  (COM-SPECIFY-CHARACTER-BY-NUMBER)) 

;;; This is executed when one clicks on the edit mode value in the label window.
(DEFMETHOD (FED :CHANGE-MOUSE-EDIT-MODE) (IGNORE)
   (com-mouse-change-draw-mode))

(DEFUN COM-MOUSE-CHANGE-DRAW-MODE (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR FED))
  (SETQ DRAW-MODE (CASE DRAW-MODE
			(2 6)
			(6 7)
			(7 2)
			(OTHERWISE 6)))
  (SETQ basic-fed-cursor (CASE DRAW-MODE
			   (2 basic-fed-erase-cursor)
			   (6 basic-fed-flip-cursor)
			   (7 basic-fed-draw-cursor)
			   (OTHERWISE basic-fed-flip-cursor)))
  (SEND SELF :mouse-standard-blinker)
  (SEND SELF :MUST-REDISPLAY-LABEL))

(DEFMETHOD (FED :SYMBOLIC-DRAW-MODE) ()
  (CASE DRAW-MODE
    (7 'SET)
    (2 'CLEAR)
    (6 'FLIP))) 

;;; Add space following draw and flip to make all strings equal length
;;; to improve appearance in label window when changing modes.
(DEFMETHOD (FED :DRAW-MODE-STRING) ()
  (CASE DRAW-MODE
    (7 "DRAW ")
    (2 "ERASE")
    (6 "FLIP ")))

;When you click on the total height
(DEFMETHOD (FED :PROMPT-LINE-SET-LINE-HEIGHT) (IGNORE)
  (LET* ((FD (FONT-GET-FD CURRENT-FONT))
	 (VALUE (READ-DEFAULTED-FONT-PARAMETER "line-height" (FD-LINE-SPACING FD))))
    (COND ((OR (NOT (INTEGERP VALUE)) (NOT (PLUSP VALUE)))
	   (BARF "~D is not a positive integer." VALUE))
	  ((< VALUE (FD-BASELINE FD))
	   (BARF "~D is smaller than the height above baseline." VALUE))
	  (T
	   (SETF (FD-LINE-SPACING FD) VALUE)
	   (AND (BOUNDP CURRENT-FONT) (SYMBOL-VALUE CURRENT-FONT)
		(SETF (W:FONT-CHAR-HEIGHT (SYMBOL-VALUE CURRENT-FONT)) VALUE))
	   (SETQ box-x-size default-box-size
		 box-y-size default-box-size)
	   (SETQ CHAR-BOX-Y3 (+ CHAR-BOX-Y1 VALUE))
	   (W:delaying-screen-management
	     (SEND W:SUPERIOR :REDEFINE-MARGINS)
					   ;added to re-size windows for taller Sample string.
	     (SEND W:superior :change-of-size-or-margins)
;;;	     (SEND W:superior :refresh)  ;redisplay font editor label
	     (DISPLAY-LABEL)
	     )))))

;When you click on baseline height
(DEFMETHOD (FED :PROMPT-LINE-SET-BASELINE) (IGNORE)
  (LET* ((FD (FONT-GET-FD CURRENT-FONT))
	 (VALUE (READ-DEFAULTED-FONT-PARAMETER "baseline" (FD-BASELINE FD))))
    (COND
      ((OR (NOT (INTEGERP VALUE)) (NOT (PLUSP VALUE)))
       (BARF "~D is not a positive fixnum." VALUE))
      ((> VALUE (FD-LINE-SPACING FD)) (BARF "~D is bigger than the total height." VALUE))
      (T (SETF (FD-BASELINE FD) VALUE)
       (AND (BOUNDP CURRENT-FONT) (SYMBOL-VALUE CURRENT-FONT)
	  (SETF (W:FONT-BASELINE (SYMBOL-VALUE CURRENT-FONT)) VALUE))
       (SETQ CHAR-BOX-Y2 (+ CHAR-BOX-Y1 VALUE)) (DISPLAY-LABEL))))) 

;;;When you click on the blinker width
(DEFMETHOD (FED :PROMPT-LINE-SET-BLINKER-WIDTH) (IGNORE)
  (LET* ((FD (FONT-GET-FD CURRENT-FONT))
	 (VALUE (READ-DEFAULTED-FONT-PARAMETER "blinker-width" (FD-BLINKER-WIDTH FD))))
    (IF (OR (NULL VALUE) (NOT (PLUSP VALUE)))
      (BARF "~D is not a positive fixnum." VALUE)
      (PROGN
	(SETF (FD-BLINKER-WIDTH FD) VALUE)
	(AND (BOUNDP CURRENT-FONT) (SYMBOL-VALUE CURRENT-FONT)
	   (SETF (W:FONT-BLINKER-WIDTH (SYMBOL-VALUE CURRENT-FONT)) VALUE))
	(DISPLAY-LABEL))))) 

;;;When you click on the blinker height
(DEFMETHOD (FED :PROMPT-LINE-SET-BLINKER-HEIGHT) (IGNORE)
  (LET* ((FD (FONT-GET-FD CURRENT-FONT))
	 (VALUE (READ-DEFAULTED-FONT-PARAMETER "blinker-height" (FD-BLINKER-HEIGHT FD))))
    (IF (OR (NULL VALUE) (NOT (PLUSP VALUE)))
      (BARF "~D is not a positive fixnum." VALUE)
      (PROGN
	(SETF (FD-BLINKER-HEIGHT FD) VALUE)
	(AND (BOUNDP CURRENT-FONT) (SYMBOL-VALUE CURRENT-FONT)
	   (SETF (W:FONT-BLINKER-HEIGHT (SYMBOL-VALUE CURRENT-FONT)) VALUE))
	(DISPLAY-LABEL))))) 


;;; When you click on the char width
(DEFMETHOD (FED :PROMPT-LINE-SET-CHAR-WIDTH) (IGNORE)
  (LET* ((*PRINT-BASE* 10.)
	 (*READ-BASE* 10.)
	 (VALUE
	  (PROMPT-LINE-DEFAULTED-READLINE (- CHAR-BOX-X2 CHAR-BOX-X1) 'GLOBAL:READ-FROM-STRING
					  "~&Character width (default ~D): "
					  (- CHAR-BOX-X2 CHAR-BOX-X1))))
    (IF (OR (NOT (FIXNUMP VALUE)) (NOT (PLUSP VALUE)))
      (BARF "Value must be a positive fixnum")
      (PROGN
	(SETQ CHAR-BOX-X2 (+ CHAR-BOX-X1 VALUE))
	(DISPLAY-LABEL))))) 

;;;********************************************************************************************
;;; This stuff currently is not used.  

;(DEFUN COM-REMOVE-TOP-ROW-PIXELS-FONT (&AUX LEN FD NFD COUNT-1 COUNT-2 OFSET)
;  (DECLARE (:SELF-FLAVOR FED))
;  (LET (font fname good)
;    (WHEN (MULTIPLE-VALUE-SETQ (good font fname) (get-new-font-name #\X "with-top-row-removed"))
;      (SETQ fd (font-get-fd current-font)
;	    len (array-active-length fd))
;      (SETQ nfd (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH len)
;				      FD-NAME fname
;				      fd-fill-pointer (fd-fill-pointer fd)
;				      FD-LINE-SPACING (1- (fd-line-spacing fd))
;				      FD-BASELINE (1- (fd-baseline fd))
;				      FD-BLINKER-HEIGHT (1- (fd-blinker-height fd))
;				      FD-BLINKER-WIDTH (fd-blinker-width fd)
;				      FD-SPACE-WIDTH (fd-space-width fd)))
;      ;;  now transfer characters from old font into new one removing the top row
;      (DO ((ch 0 (1+ ch))
;	   (cd) (ncd))
;	  ((>= ch len))
;	(AND (SETQ cd (AREF fd ch))
;	     (LET ((width (ARRAY-DIMENSION cd 1))
;		   (height (ARRAY-DIMENSION cd 0)))
;	       (SETQ ncd (make-char-descriptor
;			   :make-array (:type 'art-4b
;					      :length (LIST (MAX (1- height) 0) width))
;			   cd-char-width (cd-char-width cd)
;			   cd-char-left-kern (cd-char-left-kern cd)))
;	       (SETQ count-1 0
;		     count-2 0
;		     ofset 1)
;	       (DOTIMES (i width)
;			(SETQ count-1 (+ count-1 (AREF cd 0 i))))
;	       (DOTIMES (i width)
;			(SETQ count-2 (+ count-2 (AREF cd (1- height) i))))
;	       (IF (> count-1 0)
;		   (IF (> count-2 0)
;		       (IF (fed-y-or-n-p " ~C has ~D pixels in top row; shifting will drop ~D pixels on bottom.  Shift down?  " ch count-1 count-2)
;			   (SETQ ofset 0))
;		       (IF (fed-y-or-n-p " ~C has ~D pixels in top row.  Shift down?  " ch count-1)
;			   (SETQ ofset 0))))
;	       (DOTIMES (j (1- height))
;			(DOTIMES (i width)
;				 (SETF (AREF ncd j i) (AREF cd (+ j ofset) i))))
;	       (SETF (AREF NFD CH) NCD))))
;	  (acknowledge-new-font fname nfd))))

;(DEFUN COM-REMOVE-BOTTOM-ROW-PIXELS-FONT ()
;  (DECLARE (:SELF-FLAVOR FED))
;  (CATCH 'exit
;    (LET (font fname good len fd nfd count-1)
;      (WHEN (MULTIPLE-VALUE-SETQ (good font fname) (get-new-font-name #\X "with-bottom-row-removed"))
;	    (SETQ fd (font-get-fd current-font)
;		  len (array-active-length fd))
;	    (SETQ nfd (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH len)
;					    FD-NAME fname
;					    fd-fill-pointer (fd-fill-pointer fd)
;					    FD-LINE-SPACING (1- (fd-line-spacing fd))
;					    FD-BASELINE (fd-baseline fd)
;					    FD-BLINKER-HEIGHT (1- (fd-blinker-height fd))
;					    FD-BLINKER-WIDTH (fd-blinker-width fd)
;					    FD-SPACE-WIDTH (fd-space-width fd)))
;	    ;;  now transfer characters from old font into new one removing the top row
;	    (DO ((ch 0 (1+ ch))
;		 (cd) (ncd))
;		((>= ch len))
;	      (AND (SETQ cd (AREF fd ch))
;		   (LET ((width (ARRAY-DIMENSION cd 1))
;			 (height (ARRAY-DIMENSION cd 0)))
;		     (SETQ