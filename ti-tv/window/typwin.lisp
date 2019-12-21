;;; -*- Mode:Common-lisp;  Package: TV; Base:10.; Fonts: (CPTFONT HL12B HL12BI) -*-

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
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 02/24/89   MAY	Changed TYPEOUT-MENU-CHOOSE to use sheet-offsets to decide where to put
;;;			the popup menu. SPR 9288.
;;;  09/16/88  MAY      Changed TYPEOUT-MENU-CHOOSE to ignore command when no alist is speciifed. SPR 7437.
;;;			Allows the init-plist :item-type-alist to specify strings w/o a right menu, even though
;;;			add-typeout-item-type still insists on puting some doc for the right-click in the who line.
;;;   12/01/87 PMH      Added options to add-typeout-item-type for :NO-SELECT :FONT and :COLOR SPR#4881
;;;   7/7/87    PMH      Added :after :scroll-up method to basic-mouse-sensitive-items.
;;;   4/08/87  TWE	Backed out the changes for the W typeout-window flavors.  This is going to take a lot
;;;			more work to get it right.
;;;   3/23/87  TWE	Changed the value of MOUSE-HANDEDNESS to be a symbol in the keyword package
;;;			to get around TV/W package problems.  This should have been done in the first place.
;;;			Also changed the EQUAL test for mouse-handedness to be an EQ test.
;;; 11/13/86   TWE	Removed the ~M format code since it is in another file.
;;; 10/08/86   TWE	Fixed a bug in (BASIC-TYPEOUT-WINDOW :BEFORE :DEACTIVATE) for bug report 1510.
;;;			The recommended fix was implemented.
;;; 10/07/86   TWE	Fixed a bug in TYPEOUT-MENU-CHOOSE where the MENU-LABEL argument was a list.
;;;			The fix was to call FORMAT with ~A to convert the menu label to a string.  Also
;;;			fixed what was done on 9/17 to use the W:MENU-CHOOSE function to get the full
;;;			functionality of menus.  As part of this, the MENU instance variable was removed from
;;;			the BASIC-MOUSE-SENSITIVE-ITEMS flavor, since it is no longer needed.
;;; 09/17/86   TWE	Fixed up basic-mouse-sensitive-items to use the new menu code instead of the old.
;;; 09/05/86   TWE	Changed the ~M format code to use assoc instead of assq.
;;; 08/22/86   TWE	Added in the code for the new ~M format directive for mouse sensitivity.


;;; Typeout window and mouse-sensitive items.
;;; Menu type item typeout window.
(DEFFLAVOR BASIC-MOUSE-SENSITIVE-ITEMS
	((ITEM-TYPE-ALIST NIL)	;Associates actions with types of items
	 (ITEM-LIST NIL)	;All the currently exposed items
	 ITEM-BLINKER)		;Highlights mousable items
	()
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE STREAM-MIXIN)
  (:SETTABLE-INSTANCE-VARIABLES ITEM-TYPE-ALIST)
  (:DOCUMENTATION :MIXIN "Menu like operations for a typeout window."))

;;;Item typed out by :ITEM or :ITEM-LIST messages
(DEFSTRUCT (TYPEOUT-ITEM :LIST (:CONSTRUCTOR NIL) (:CONC-NAME NIL)
			 (:ALTERANT ALTER-TYPEOUT-ITEM) (:PREDICATE NIL) (:COPIER NIL) (:TYPE :list))
  TYPEOUT-ITEM-TYPE				;For looking in ITEM-TYPE-ALIST
  TYPEOUT-ITEM-ITEM				;Identifier of item
  TYPEOUT-ITEM-LEFT				;Screen area occupied by item, relative to
  TYPEOUT-ITEM-TOP				;sheet, not to margins
  TYPEOUT-ITEM-RIGHT
  TYPEOUT-ITEM-BOTTOM)

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :INIT) (IGNORE)
  "Make a blinker for the menu type items."
  (SETQ ITEM-BLINKER (MAKE-BLINKER SELF 'HOLLOW-RECTANGULAR-BLINKER
                                   :VISIBILITY NIL)))

(DEFUN TYPEOUT-ITEM-WINDOW-REMOVE-ITEMS (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-MOUSE-SENSITIVE-ITEMS))
  (SETQ ITEM-LIST NIL)
  (BLINKER-SET-VISIBILITY ITEM-BLINKER NIL))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :REFRESH) (&OPTIONAL IGNORE)
  "Forget any items on screen if cleared."
  (OR RESTORED-BITS-P (TYPEOUT-ITEM-WINDOW-REMOVE-ITEMS)))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :CLEAR-SCREEN)
    TYPEOUT-ITEM-WINDOW-REMOVE-ITEMS)

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :CLEAR-EOF) (&AUX TEM)
  (COND
    ((SETQ TEM (MEMBER 'WRAPAROUND ITEM-LIST :TEST #'EQ))
     (RPLACD TEM NIL)
     (MOUSE-WAKEUP))))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :EXPOSE-FOR-TYPEOUT)
    TYPEOUT-ITEM-WINDOW-REMOVE-ITEMS)

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :END-OF-PAGE-EXCEPTION) ()
  "Record a blip when the screen wraps around."
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (PUSH 'WRAPAROUND ITEM-LIST)))

(DEFMETHOD (W:BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :SCROLL-UP) (INCREMENT)
  (WHEN (> INCREMENT 0)
      (SETQ ITEM-LIST
	    (delete-if #'(lambda (item)		;delete item that would scroll off the top
			   (if (listp item)	;some elements may not be ITEMs
			       (< (TYPEOUT-ITEM-TOP ITEM) INCREMENT)))
		       item-list))
      (dolist (item item-list)			;move everyone else up
	(when (listp item)
	  (setf (TYPEOUT-ITEM-TOP ITEM) (- (TYPEOUT-ITEM-TOP ITEM) INCREMENT))
	  (setf (TYPEOUT-ITEM-BOTTOM ITEM) (- (TYPEOUT-ITEM-BOTTOM ITEM) INCREMENT)))))
  (VALUES))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :ITEM) (TYPE ITEM &REST FORMAT-ARGS)
  "Type out item, either as itself or FORMAT-ARGS.  TYPE is used for
indexing into ITEM-TYPE-ALIST."
  ;; Wrap around, if necessary, before recording the cursor.
  (SEND SELF :INCREMENT-CURSORPOS 0 0)
  (LET ((X CURSOR-X)
	(Y CURSOR-Y))
    (IF FORMAT-ARGS
      (APPLY #'FORMAT SELF FORMAT-ARGS)
      (PRINC ITEM SELF))
    (DO ((LINE-Y Y (+ LINE-Y LINE-HEIGHT))
	 (LINE-X X LEFT-MARGIN-SIZE))
	(NIL)
      (IF (> (+ LINE-Y LINE-HEIGHT) (SHEET-INSIDE-BOTTOM))
	(SETQ LINE-Y TOP-MARGIN-SIZE))
      (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
	(PUSH
	 (LIST TYPE ITEM LINE-X LINE-Y
	       (IF (= LINE-Y CURSOR-Y)
		   CURSOR-X
		   (SHEET-INSIDE-WIDTH))
	       (+ LINE-Y LINE-HEIGHT))
	 ITEM-LIST))
      (IF (= LINE-Y CURSOR-Y)
	(RETURN)))
    (MOUSE-WAKEUP)
    NIL))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :PRIMITIVE-ITEM)
           (TYPE ITEM LEFT TOP RIGHT BOTTOM)
  "Make an item without drawing anything (assuming the caller has
drawn it already) Instead you just pass in an enclosing rectangle."
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (PUSH (LIST TYPE ITEM
                (+ LEFT   (SHEET-INSIDE-LEFT))
                (+ TOP    (SHEET-INSIDE-TOP))
		(+ RIGHT  (SHEET-INSIDE-LEFT))
                (+ BOTTOM (SHEET-INSIDE-TOP)))
	  ITEM-LIST)
    (TV:MOUSE-WAKEUP)
    NIL))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :PRIMITIVE-ITEM-OUTSIDE)
	   (TYPE ITEM LEFT TOP RIGHT BOTTOM)
  "Like :PRIMITIVE-ITEM except that the edges are with respect to
the outside of the window.  so you can use values such as CURSOR-X
without subtracting (SHEET-INSIDE-TOP)."
  (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
    (PUSH (LIST TYPE ITEM LEFT TOP RIGHT BOTTOM)
	  ITEM-LIST)
    (TV:MOUSE-WAKEUP)
    NIL))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :ITEM-LIST)
	   (TYPE LIST &AUX (MAXL 0) N (INSIDE-WIDTH (SHEET-INSIDE-WIDTH)))
  "Type out list of item as many as will fit on each line, centered."
  (SEND SELF :FRESH-LINE)
  (COND
    (LIST					;Do nothing if empty list
     ;; Compute the maximum width of any item, in dots (MAXL).
     (DOLIST (ITEM LIST)
       (LET ((STRING (STRING (IF (CONSP ITEM)
				 (CAR ITEM)
				 ITEM))))
	 (SETQ MAXL (MAX (SHEET-STRING-LENGTH SELF STRING) MAXL))))
     ;; How many items go on each line (except the last)?
     (SETQ N
	   (MAX
	     (MIN (TRUNCATE INSIDE-WIDTH (+ MAXL (FONT-CHAR-WIDTH CURRENT-FONT))) (LENGTH LIST))
	     1))				;Always print something,
						; even if continuation.
     ;; Now print the items and store the data in the table.
     ;; Move to a new line when we exhaust a line, and at the end.
     ;; I counts from 1 thru N on each line.
     (DO ((I 1 (1+ I))
	  (LIST LIST (CDR LIST))
	  (WIDTH-PER (TRUNCATE INSIDE-WIDTH N)))
	 ((NULL LIST))
       ;; Actually make this item.
       (IF (CONSP (CAR LIST))
	   (SEND SELF :ITEM TYPE (CDAR LIST) "~A" (CAAR LIST))
	   (SEND SELF :ITEM TYPE (CAR LIST)))
       ;; Space out for next item, or move to new line.
       (IF (AND (NOT (= I N))
		(CDR LIST))
	   ;; Not end of line, space out for next item.
	   (MULTIPLE-VALUE-BIND (X Y)
	       (SHEET-READ-CURSORPOS SELF)
	     (SHEET-SET-CURSORPOS SELF (* WIDTH-PER (TRUNCATE (+ (1- WIDTH-PER) X) WIDTH-PER)) Y))
	   ;; else end of line
	   (PROGN
	     (SEND SELF :TYO #\NEWLINE)
	     (SETQ I 0))))))
  (MOUSE-WAKEUP)
  NIL)

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :AFTER :HANDLE-MOUSE) ()
  "When mouse leaves the window, turn off the item blinker."
  (SEND ITEM-BLINKER :SET-VISIBILITY NIL))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :MOUSE-MOVES) (X Y &AUX ITEM)
  "Blink any item the mouse points to."
  (MOUSE-SET-BLINKER-CURSORPOS)
  (COND
    ((AND (SETQ ITEM (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
	  (ASSOC (TYPEOUT-ITEM-TYPE ITEM) ITEM-TYPE-ALIST :TEST #'EQ))
     (LET ((LEFT (TYPEOUT-ITEM-LEFT ITEM))
	   (TOP (TYPEOUT-ITEM-TOP ITEM))
	   (RIGHT (TYPEOUT-ITEM-RIGHT ITEM))
	   (BOTTOM (TYPEOUT-ITEM-BOTTOM ITEM))
	   BWIDTH
	   BHEIGHT)
       (SETQ BWIDTH (- RIGHT LEFT)
	     BHEIGHT (- BOTTOM TOP))
       ;; Position the blinker to the item.
       (BLINKER-SET-CURSORPOS ITEM-BLINKER
			      (- LEFT (SHEET-INSIDE-LEFT))
			      (- TOP (SHEET-INSIDE-TOP)))
       ;; Change the size of the blinker to enclose the item.
       (BLINKER-SET-SIZE ITEM-BLINKER BWIDTH BHEIGHT)
       ;; Turn the blinker on.
       (BLINKER-SET-VISIBILITY ITEM-BLINKER T)))
    (T (BLINKER-SET-VISIBILITY ITEM-BLINKER NIL))))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :MOUSE-CLICK) (BUTTON X Y &AUX ITEM)
  "Mouse-left selects the blinking item, mouse-right pops up a menu
near it."
  (SETQ ITEM (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
  (OR
   (WHEN ITEM
     (LET ((ITEM-TYPE (ASSOC (TYPEOUT-ITEM-TYPE ITEM) ITEM-TYPE-ALIST :TEST #'EQ)))
       (WHEN ITEM-TYPE
	 (CASE (int-char BUTTON)
	   (#\MOUSE-L
	    ;; Form the blip and stuff it into the keyboard buffer.
	    (SEND SELF :FORCE-KBD-INPUT
	       (LIST :TYPEOUT-EXECUTE (CADR ITEM-TYPE) (TYPEOUT-ITEM-ITEM ITEM)))
	    T)
	   (#\MOUSE-R
	    (PROCESS-RUN-FUNCTION "Menu Choose" #'TYPEOUT-MENU-CHOOSE (CDDDR ITEM-TYPE)
				  ITEM SELF
                                  ;; Compute a label for the menu.
				  (OR
				   (AND (CONSP (THIRD ITEM-TYPE)) (CADR (THIRD ITEM-TYPE))
				      (FUNCALL (CADR (THIRD ITEM-TYPE)) ITEM))
                                   (SECOND ITEM)))
	    T)))))
   ;; If someone is out there and wants to handle this, let them.  PMH
   (if (send self :operation-handled-p :non-sensitive-mouse-click)
       (send self :send-if-handles :non-sensitive-mouse-click button x y)
       (not (= BUTTON #\MOUSE-R-2)))))

;;;Return the item the mouse if pointing to
(DEFUN TYPEOUT-MOUSE-ITEM (X Y)
  "Return the mouse-sensitive item at X, Y, or NIL if none.
SELF should be a BASIC-MOUSE-SENSITIVE-ITEMS instance."
  (DECLARE (:SELF-FLAVOR BASIC-MOUSE-SENSITIVE-ITEMS))
  (DO ((ITEMS ITEM-LIST (CDR ITEMS))
       (ITEM)
       (ITEM-Y)
       (WRAPPED-AROUND))
      ((NULL ITEMS))
    (IF (SYMBOLP (SETQ ITEM (CAR ITEMS)))
	(SETQ WRAPPED-AROUND T)
	(PROGN
	  (AND (<= (SETQ ITEM-Y (TYPEOUT-ITEM-TOP ITEM)) CURSOR-Y)
	       WRAPPED-AROUND
	       (RETURN NIL))
	  (AND (>= Y ITEM-Y)
	       (< Y (TYPEOUT-ITEM-BOTTOM ITEM))
	       (>= X (TYPEOUT-ITEM-LEFT ITEM))
	       (< X (TYPEOUT-ITEM-RIGHT ITEM))
	       (RETURN ITEM))))))

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :MOUSE-SENSITIVE-ITEM) (X Y)
  (TYPEOUT-MOUSE-ITEM X Y))

;;;patched on 12 Dec 85 for PDC by GSM
(DEFUN TYPEOUT-MENU-CHOOSE (ALIST TYPEOUT-ITEM TYPEOUT-WINDOW MENU-LABEL)
  "Select a thing to do to mouse-sensitive item TYPEOUT-ITEM.
ALIST			menu item-list to be displayed in a menu.
MENU-LABEL		an object to display as the menu's label, or NIL
			for no label.
TYPEOUT-WINDOW	typeout window in which TYPEOUT-ITEM
			appeared.
The user's choice is processed by forcing input of the same sort as is
done by clicking left on the typeout-item, except that the operation
which the user chose in the menu is supplied as the second element of
the blip, which looks like:
	(:TYPEOUT-EXECUTE operation item-information)."
  (when ALIST ;; may 9-16-88
    (LET ((old-x mouse-x)			;PDC 12/10/85
	  (old-y mouse-y))
      (multiple-value-bind (x-off y-off) (w:sheet-calculate-offsets TYPEOUT-WINDOW nil) ;; may 02/24/89
	(LET ((CHOICE-RESULT (W:MENU-CHOOSE ALIST :LABEL (FORMAT NIL "~A" MENU-LABEL)
					    :NEAR-MODE `(:RECTANGLE ,(+ (TYPEOUT-ITEM-LEFT   TYPEOUT-ITEM) x-off)
								    ,(+ (TYPEOUT-ITEM-TOP    TYPEOUT-ITEM) y-off)
								    ,(+ (TYPEOUT-ITEM-RIGHT  TYPEOUT-ITEM) x-off)
								    ,(+ (TYPEOUT-ITEM-BOTTOM TYPEOUT-ITEM) y-off)))))
	  (AND CHOICE-RESULT
	       (SEND TYPEOUT-WINDOW :FORCE-KBD-INPUT
		     (LIST :TYPEOUT-EXECUTE CHOICE-RESULT
			   (TYPEOUT-ITEM-ITEM TYPEOUT-ITEM)))))
	(SETQ mouse-x old-x			;PDC 12/10/85
	      mouse-y old-y)
	(SEND typeout-window :mouse-moves mouse-x mouse-y)))))

(DEFMACRO ADD-TYPEOUT-ITEM-TYPE (ALIST TYPE NAME-STRING FUNCTION
				 &OPTIONAL DEFAULT-P DOCUMENTATION (menu-type :value) &rest options)
  "Add a new operation named NAME-STRING for typeout items of type TYPE.
ALIST		variable used as an ITEM-TYPE-ALIST in windows that
		items of type TYPE will be output to.
FUNCTION	the function to be called when the user selects this
		operation.
DEFAULT-P	non-NIL says make this operation the default one for
		items of this type (the operation that click-left does).
		Otherwise, it just becomes one of the items in the
		click-right menu.
MENU-TYPE    The type of item in the MOUSE-R pop-up menu.
                  Either :VALUE, the default, or :NO-SELECT
options          Additional menu keyword options like :FONT or :COLOR"
  `(SETQ ,ALIST (apply 'ADD-TYPEOUT-ITEM-TYPE-1
                     ,ALIST ',TYPE ',FUNCTION
                     ,NAME-STRING ,DEFAULT-P
                     ,DOCUMENTATION ,menu-type ',options)))

(DEFUN ADD-TYPEOUT-ITEM-TYPE-1
 (ALIST TYPE FUNCTION NAME DEFAULT-P DOCUMENTATION &optional (menu-type :value) &rest args &AUX EL1 EL2)
 ;; If the type isn't there then add it to the Alist.
  (OR (SETQ EL1 (ASSOC TYPE ALIST :TEST #'EQ))
      (PUSH (SETQ EL1 (LIST TYPE NIL NIL)) ALIST))
  (AND DEFAULT-P
       (SETF (SECOND EL1) FUNCTION))
  (OR (SETQ EL2 (ASSOC NAME (CDDDR EL1) :TEST 'EQUALP))
      (PUSH (SETQ EL2 (CONS NAME NIL)) (CDDDR EL1)))
  (SETF (CDR EL2)
	;; Allow general menu type; :VALUE and :NO-SELECT are the only supported menu types
	`(,menu-type ,FUNCTION :DOCUMENTATION ,DOCUMENTATION ,.args))
  (SETF (THIRD EL1) (MAKE-TYPEOUT-MOUSE-PROMPT (THIRD EL1) (SECOND EL1) (CDDDR EL1)))
  ALIST)

(DEFMETHOD (BASIC-MOUSE-SENSITIVE-ITEMS :WHO-LINE-DOCUMENTATION-STRING)
 (&AUX ITEM ITEM-TYPE X Y)
  (MULTIPLE-VALUE-SETQ (X Y)
    (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
  (SETQ X (- MOUSE-X X)
	Y (- MOUSE-Y Y))
  (AND (SETQ ITEM (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
       (SETQ ITEM-TYPE (TYPEOUT-ITEM-TYPE ITEM))
       (SETQ ITEM-TYPE (ASSOC ITEM-TYPE ITEM-TYPE-ALIST :TEST #'EQ))
       (COND
	 ((STRINGP (THIRD ITEM-TYPE)) (THIRD ITEM-TYPE))
	 ((CONSP
	    (THIRD ITEM-TYPE)) (FUNCALL (CAR (THIRD ITEM-TYPE)) ITEM)))))

(DEFUN MAKE-TYPEOUT-MOUSE-PROMPT (STRING DEFAULT ALIST)
  (IF STRING
      (STORE-ARRAY-LEADER 0 STRING 0)
      (SETQ STRING (MAKE-ARRAY 100 :ELEMENT-TYPE 'STRING-CHAR :LEADER-LIST '(0))))
  (DO ((L ALIST (CDR L)))
      ((NULL L))
    (AND (EQ DEFAULT (GET (CAR L) :VALUE))
	 (SETQ DEFAULT (OR (GET (CAR L) :DOCUMENTATION) (CAAR L)))))
  (IF (EQ MOUSE-HANDEDNESS :LEFT)
      (FORMAT STRING "R: ~A, L: menu of " DEFAULT)
      (FORMAT STRING "L: ~A, R: menu of " DEFAULT))
  (DO ((L ALIST (CDR L))
       (FIRST-P T))
      ((NULL L)
       (VECTOR-PUSH #\. STRING))
    ;; don't include :NO-SELECT items   PMH
    (when (not (eq :no-select (cadar l)))
      (IF FIRST-P
	  (SETQ FIRST-P NIL)
	  (FORMAT STRING ", "))
      (FORMAT STRING "~A" (CAAR L))))
  STRING)

;;; Windows with typeout windows as inferiors.
(DEFFLAVOR ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN ((TYPEOUT-WINDOW NIL)) ()
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE)
  (:GETTABLE-INSTANCE-VARIABLES TYPEOUT-WINDOW)
  (:INITABLE-INSTANCE-VARIABLES TYPEOUT-WINDOW)
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES TYPEOUT-WINDOW)
  (:DOCUMENTATION :MIXIN "A window that has a typeout window as an inferior."))

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :AFTER :INIT) (IGNORE)
  (AND (CONSP TYPEOUT-WINDOW)
       (SETQ TYPEOUT-WINDOW
	     (APPLY #'MAKE-WINDOW (CAR TYPEOUT-WINDOW) :SUPERIOR SELF (CDR TYPEOUT-WINDOW)))))

(DEFWRAPPER (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :CHANGE-OF-SIZE-OR-MARGINS)
            (IGNORE . BODY)
  `(LET (.STATUS. .INCOMPLETE-P.)
     (DELAYING-SCREEN-MANAGEMENT
       (UNWIND-PROTECT
	 (PROGN
	   (COND (TYPEOUT-WINDOW		;May not be present during init
		  (SETQ .STATUS. (SEND TYPEOUT-WINDOW :STATUS))
		  (SETQ .INCOMPLETE-P. (BASIC-TYPEOUT-WINDOW-INCOMPLETE-P
                                         TYPEOUT-WINDOW))))
	   . ,BODY)
	 (WHEN .STATUS.
	   (SEND TYPEOUT-WINDOW :SET-STATUS .STATUS.)
	   (SETF (BASIC-TYPEOUT-WINDOW-INCOMPLETE-P TYPEOUT-WINDOW)
                 .INCOMPLETE-P.))))))

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN
             :AFTER :CHANGE-OF-SIZE-OR-MARGINS)
           (&REST IGNORE)
  (AND TYPEOUT-WINDOW
       (SEND TYPEOUT-WINDOW :SET-EDGES
                (SHEET-INSIDE-LEFT)
                (SHEET-INSIDE-TOP)
		(SHEET-INSIDE-RIGHT)
                (SHEET-INSIDE-BOTTOM))))

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :AFTER :CHANGE-OF-DEFAULT-FONT)
	   (OLD-FONT NEW-FONT)
  (AND TYPEOUT-WINDOW (SEND TYPEOUT-WINDOW :CHANGE-OF-DEFAULT-FONT
                               OLD-FONT NEW-FONT)))

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :TURN-OFF-BLINKERS-FOR-TYPEOUT)
           ())

(DEFMETHOD (ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN :TURN-ON-BLINKERS-FOR-TYPEOUT)
           ())

(DEFFLAVOR WINDOW-WITH-TYPEOUT-MIXIN
	()
	(NO-SCREEN-MANAGING-MIXIN ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN))

(DEFFLAVOR INTRINSIC-NO-MORE-MIXIN ((MORE-ENABLED-FOR-INFERIORS T)) ()
  (:DEFAULT-INIT-PLIST :MORE-P NIL)
  (:DOCUMENTATION
    :MIXIN
    "Suppress **more** for this window, but remember :more-p for inferiors."))

(DEFMETHOD (INTRINSIC-NO-MORE-MIXIN :MORE-P) () MORE-ENABLED-FOR-INFERIORS)

(DEFMETHOD (INTRINSIC-NO-MORE-MIXIN :SET-MORE-P) (MORE-P)
  (SETQ MORE-ENABLED-FOR-INFERIORS MORE-P))

(DEFPARAMETER *ENABLE-TYPEOUT-WINDOW-BORDERS* T
   "Non-NIL enables the line below the occupied part of a typeout
window to be drawn.")

;;; Typeout windows themselves.
(DEFFLAVOR BASIC-TYPEOUT-WINDOW
           ;; For ordinary use, the command process of the program
           ;; should check INCOMPLETE-P and wait for the user to type
           ;; space if that is set.  The redisplay process should check
           ;; BOTTOM-REACHED and redisplay (only that portion, if it
           ;; can) if that is set.  Thus things that typeout but that
           ;; need not be saved for the user (like Y-OR-N-P's) should
           ;; send the :MAKE-COMPLETE message.
	   ((BOTTOM-REACHED NIL)		;BOTTOM-REACHED is set to the
						; largest Y clobbered
						; (outside coordinatess).
;;;  or NIL if nothing is clobbered.
            (HAD-MOUSE-P NIL)                   ;T if the typeout window own's
						; the mouse, nil otherwise.
            (INCOMPLETE-P NIL)                  ;INCOMPLETE-P is set to T when
						; the window is exposed, and NIL
						; when it is deexposed or by the
						; :MAKE-COMPLETE method.
	    (WINDOW-SUBSTITUTING-FOR NIL)       ;WINDOW-SUBSTITUTING-FOR is the
						; window we are selected as a
						; substitute for.  This is not
						; necessarily the immediate
						; superior.  It is whichever
						; ancestor was selected when we
						; started to be used.
	    (PREVIOUS-SUBSTITUTE NIL)           ;PREVIOUS-SUBSTITUTE is what that
						; window's selection substitute
						; was before we set its
						; substitute to be us. By
						; default, these windows cannot
						; be selected from the
						; system menu.
	    IO-BUFFER)
	   (NO-SCREEN-MANAGING-MIXIN DONT-SELECT-WITH-MOUSE-MIXIN)
  (:REQUIRED-FLAVORS ESSENTIAL-MOUSE)
  (:GETTABLE-INSTANCE-VARIABLES INCOMPLETE-P)
  (:DEFAULT-INIT-PLIST :DEEXPOSED-TYPEOUT-ACTION '(:EXPOSE-FOR-TYPEOUT))
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES INCOMPLETE-P BOTTOM-REACHED)
  (:DOCUMENTATION :MIXIN "A window that grows over its superior."))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :INIT) (IGNORE)
  (UNLESS (VARIABLE-BOUNDP IO-BUFFER)
	  (SETQ IO-BUFFER (SEND SUPERIOR :IO-BUFFER))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :HANDLE-MOUSE) ()
  (SETQ HAD-MOUSE-P T))

;;; For MOUSE-MOVES and MOUSE-BUTTONS message, the typeout-window, if
;;; exposed, will receive the messages and if it is not in the covered
;;; area, pass them off to the superior and throw out of the original
;;; message.
(DEFWRAPPER (BASIC-TYPEOUT-WINDOW :MOUSE-MOVES) ((IGNORE IGNORE) . BODY)
  `(CATCH 'SUPERIOR-HANDLED-MOUSE
     . ,BODY))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :MOUSE-MOVES) (X Y)
  (IF (HANDLE-MOUSE-P X Y)
      (COND
	((NOT HAD-MOUSE-P) (SEND SUPERIOR :TURN-OFF-BLINKERS-FOR-TYPEOUT) (SETQ HAD-MOUSE-P T)))
      (COND
	(HAD-MOUSE-P				;Send one extra message the
						; first time out.
	 (SEND SUPERIOR :TURN-ON-BLINKERS-FOR-TYPEOUT) (SETQ HAD-MOUSE-P NIL))	;Turn off any blinkers.
	(T
	 (LET ((X (+ X X-OFFSET))
	       (Y (+ Y Y-OFFSET)))
	   (SEND SUPERIOR :MOUSE-MOVES X Y)
	   (THROW 'SUPERIOR-HANDLED-MOUSE
		  T))))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :MOUSE-MOVES) MOUSE-SET-BLINKER-CURSORPOS)

(DEFWRAPPER (BASIC-TYPEOUT-WINDOW :MOUSE-BUTTONS) (IGNORE . BODY)
  `(CATCH 'SUPERIOR-HANDLED-MOUSE
     . ,BODY))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :MOUSE-BUTTONS) (BD X Y)
  (OR (HANDLE-MOUSE-P X Y)
     (LET ((X (+ X X-OFFSET))
	   (Y (+ Y Y-OFFSET)))
       (SEND SUPERIOR :MOUSE-BUTTONS BD X Y)
       (THROW 'SUPERIOR-HANDLED-MOUSE T))))

(DEFUN HANDLE-MOUSE-P (X Y)
  "T if position X, Y is inside the part of SELF that has been output
in.  SELF must be a BASIC-TYPEOUT-WINDOW.  This is how clicks in
the part where output has reached apply to the typeout in the
typeout window, whereas clicks in the bottom part apply to the
typeout window's superior."
  (DECLARE (:SELF-FLAVOR BASIC-TYPEOUT-WINDOW))
  (AND BOTTOM-REACHED (< Y (MAX BOTTOM-REACHED (+ LINE-HEIGHT CURSOR-Y)))
       (>= X 0) (>= Y 0) (< X WIDTH)))

(DEFWRAPPER (BASIC-TYPEOUT-WINDOW :WHO-LINE-DOCUMENTATION-STRING)
            (IGNORE . BODY)
  `(MULTIPLE-VALUE-BIND (XOFF YOFF)
       (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)
     (IF (NULL (HANDLE-MOUSE-P (- MOUSE-X XOFF) (- MOUSE-Y YOFF)))
	 (SEND SUPERIOR :WHO-LINE-DOCUMENTATION-STRING)
	 . ,BODY)))

;;; We do **MORE** iff four superior does.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :MORE-P) ()
  (SEND SUPERIOR :MORE-P))

;;; Telling us whether to do **MORE** is like telling our superior.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :SET-MORE-P) (MORE-P)
  (SEND SUPERIOR :SET-MORE-P MORE-P))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :DEEXPOSED-TYPEIN-ACTION) ()
  (SEND SUPERIOR :DEEXPOSED-TYPEIN-ACTION))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :SET-DEEXPOSED-TYPEIN-ACTION) (ACTION)
  (SEND SUPERIOR :SET-DEEXPOSED-TYPEIN-ACTION ACTION))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :DEEXPOSED-TYPEOUT-ACTION) ()
  (SEND SUPERIOR :DEEXPOSED-TYPEOUT-ACTION))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :SET-DEEXPOSED-TYPEOUT-ACTION) (ACTION)
  (SEND SUPERIOR :SET-DEEXPOSED-TYPEOUT-ACTION ACTION))

(DEFWRAPPER (BASIC-TYPEOUT-WINDOW :EXPOSE) (IGNORE . BODY)
  `(LET ((.TYPEOUT-WAS-EXPOSABLE. (MEMBER SELF (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ))
	 (.OLD-INCOMPLETE-P. INCOMPLETE-P))
     (DECLARE (SPECIAL .TYPEOUT-WAS-EXPOSABLE. .OLD-INCOMPLETE-P.))
     . ,BODY))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :EXPOSE) (&REST IGNORE)
  (SETQ MORE-VPOS (AND (SEND SUPERIOR :MORE-P) (SHEET-DEDUCE-MORE-VPOS SELF))))


(DEFMETHOD (BASIC-TYPEOUT-WINDOW :MAKE-INCOMPLETE) ()
  (SETQ INCOMPLETE-P T))

;; The fundamental output operations make the window incomplete
;; and also may require redrawing the line that goes underneath
;; the occupied area.

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :TYO)        MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :STRING-OUT) MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :LINE-OUT)   MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :FRESH-LINE) MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :CLEAR-EOL)  MAYBE-MOVE-BOTTOM-REACHED)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :TERPRI) MAYBE-MOVE-BOTTOM-REACHED)

(DEFUN MAYBE-MOVE-BOTTOM-REACHED (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-TYPEOUT-WINDOW))
  (SETQ INCOMPLETE-P T)
  (AND BOTTOM-REACHED
       (LET ((OLD-BOTTOM BOTTOM-REACHED))
	 (SETQ BOTTOM-REACHED (MAX BOTTOM-REACHED (+ LINE-HEIGHT CURSOR-Y)))
	 (UNLESS (= OLD-BOTTOM BOTTOM-REACHED)
	   ;; Don't bother to undraw the line where it used to be,
	   ;; because it was erased by clearing the line when the
           ;; cursor moved down, if that was due to outputting text.
	   (OR (ZEROP BOTTOM-REACHED)
	       (= BOTTOM-REACHED (SHEET-INSIDE-BOTTOM))
	       (AND *ENABLE-TYPEOUT-WINDOW-BORDERS*
		    (PREPARE-SHEET (SELF)
		      (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) 1
                                       (SHEET-INSIDE-LEFT)
				       BOTTOM-REACHED
				       ALU-IOR SELF))))))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :SET-CURSORPOS) COMPUTE-BOTTOM-REACHED)

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :INCREMENT-CURSORPOS)
           COMPUTE-BOTTOM-REACHED)

(DEFUN COMPUTE-BOTTOM-REACHED (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-TYPEOUT-WINDOW))
  (AND BOTTOM-REACHED
       (LET ((NEW-BOTTOM (MAX BOTTOM-REACHED (+ LINE-HEIGHT CURSOR-Y))))
	 (UNLESS (= NEW-BOTTOM BOTTOM-REACHED)
	   (UNLESS (ZEROP BOTTOM-REACHED)
	     (PREPARE-SHEET (SELF)
	       (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) 1 (SHEET-INSIDE-LEFT)
				BOTTOM-REACHED
				ALU-ANDCA SELF)))
	   (OR (ZEROP NEW-BOTTOM)
	       (= NEW-BOTTOM (SHEET-INSIDE-BOTTOM))
	       (AND *ENABLE-TYPEOUT-WINDOW-BORDERS*
		    (PREPARE-SHEET (SELF)
		      (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) 1
                                       (SHEET-INSIDE-LEFT)
				       NEW-BOTTOM
				       ALU-IOR SELF)))))
	 (SETQ BOTTOM-REACHED NEW-BOTTOM))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BOTTOM-REACHED) ()
  (AND BOTTOM-REACHED (MAX BOTTOM-REACHED (+ LINE-HEIGHT CURSOR-Y))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR RESTORED-BITS-P (REACHED-BOTTOM)))

(DEFUN REACHED-BOTTOM (&REST IGNORE)
  "Say that typeout on the typeout window has occupied it all, down
to its bottom."
  (DECLARE (:SELF-FLAVOR BASIC-TYPEOUT-WINDOW))
  (AND BOTTOM-REACHED
       (NOT (ZEROP BOTTOM-REACHED))
       (NOT (= BOTTOM-REACHED (SHEET-INSIDE-BOTTOM)))
       (PREPARE-SHEET (SELF)
	 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) 1 (SHEET-INSIDE-LEFT)
			  BOTTOM-REACHED
			  ALU-ANDCA SELF)))
  (SETQ INCOMPLETE-P T
	BOTTOM-REACHED (SHEET-INSIDE-BOTTOM)))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :CLEAR-SCREEN)          REACHED-BOTTOM)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :CLEAR-EOF)             REACHED-BOTTOM)
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :END-OF-PAGE-EXCEPTION) REACHED-BOTTOM)

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :RUBOUT-HANDLER) (&REST IGNORE)
  (IF (OR (NOT BOTTOM-REACHED) (EQ BOTTOM-REACHED 0))
      (SEND SELF :CLEAR-EOL)))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :MAKE-COMPLETE) ()
  (SETQ INCOMPLETE-P NIL))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :ACTIVE-P) ()
  BOTTOM-REACHED)

;;; Expose, but don't clear the screen.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :EXPOSE-FOR-TYPEOUT) ()
  "This is here so that we don't try to activate ourselves while we are
locked, so that we don't violate locking order, because activating
requires getting a lock on our superior."
  (SEND SELF :ACTIVATE)
  (SEND SELF :EXPOSE NIL :NOOP)
  (OR EXPOSED-P
      ;; If our superior has no screen array, we won't really be
      ;; exposed.  So wait until really exposed to prevent infinite
      ;; regression.
      (PROCESS-WAIT "Typeout Exposed" #'CAR (LOCF (SHEET-EXPOSED-P SELF))))
  (SETQ BOTTOM-REACHED (OR BOTTOM-REACHED 0)
	INCOMPLETE-P T)
  ;; On becoming exposed, also be the selection substitute for an
  ;; appropriate ancestor.
  (AND (NEQ SELF SELECTED-WINDOW)
       (LET ((TEM (TYPEOUT-WINDOW-ANCESTOR-TO-SUBSTITUTE-FOR SELF)))
	 (UNLESS (EQ (SEND TEM :SELECTION-SUBSTITUTE) SELF)
	   (SETQ WINDOW-SUBSTITUTING-FOR TEM)
	   (SETQ PREVIOUS-SUBSTITUTE (SEND TEM :SELECTION-SUBSTITUTE))
	   (SEND WINDOW-SUBSTITUTING-FOR :SET-SELECTION-SUBSTITUTE SELF))))
  (SEND SELF :HOME-CURSOR)
  (SEND SELF :CLEAR-EOL))

;;; This message is sent to TERMINAL-IO.  The sender doesn't really
;;; want to wait till the typeout window is exposed; he wants to wait
;;; until the window he knows about is exposed.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AWAIT-EXPOSURE) ()
  (SEND SUPERIOR :AWAIT-EXPOSURE))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :NOTICE) (EVENT &REST IGNORE)
  (AND (EQ EVENT :ERROR)
       (SHEET-CAN-GET-LOCK SELF)	;Try not to get hung before deciding
       (SHEET-CAN-GET-LOCK SUPERIOR)	;to use the cold-load stream.
       (SHEET-SCREEN-ARRAY SUPERIOR)	;KLUDGE: don't wait in above method
;					; for screen-array,
       (EQUAL DEEXPOSED-TYPEOUT-ACTION
              '(:EXPOSE-FOR-TYPEOUT))	; and make sure of this too.
       (SEND SELF :OUTPUT-HOLD-EXCEPTION))
  NIL)

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :DEEXPOSE)
	   (&OPTIONAL IGNORE IGNORE (REMOVE-FROM-SUPERIOR T))
  (WHEN REMOVE-FROM-SUPERIOR
    (SETQ BOTTOM-REACHED NIL)
    ;; The following used to be in a :BEFORE :DEEXPOSE method, now eliminated.
    (SETQ INCOMPLETE-P NIL)))

(DEFUN TYPEOUT-WINDOW-ANCESTOR-TO-SUBSTITUTE-FOR (WINDOW)
  "Find the nearest (smallest) ancestor which is currently in the chain
of selection substitution coming down from our ultimate ancestor."
  (LET ((HIGHEST (SEND WINDOW :ALIAS-FOR-SELECTED-WINDOWS)))
    (DO ((W HIGHEST (SEND W :SELECTION-SUBSTITUTE))
	 (PREV HIGHEST W))
	((OR (NULL W)
	     (EQ W WINDOW)
	     (NOT (SHEET-ME-OR-MY-KID-P SELF W)))
	 PREV))))

(DEFMETHOD (BASIC-TYPEOUT-WINDOW :BEFORE :DEACTIVATE) ()
  ":DEACTIVATE is how the superior gets rid of the typeout window.
Allow the ancestor (or his previous substitute) to become selected
again."
  (WHEN (EQ SELF (AND WINDOW-SUBSTITUTING-FOR
                      (SEND WINDOW-SUBSTITUTING-FOR :ULTIMATE-SELECTION-SUBSTITUTE)))
    (SEND WINDOW-SUBSTITUTING-FOR :REMOVE-SELECTION-SUBSTITUTE SELF
	  (IF (NEQ PREVIOUS-SUBSTITUTE SELF)
	      PREVIOUS-SUBSTITUTE
	    WINDOW-SUBSTITUTING-FOR))))

;;; Our superior's blinkers should disappear entirely while we are
;;; selected.  In fact, that should happen whenever we are exposed,
;;; but I am not confident of how to implement that.
(DEFMETHOD (BASIC-TYPEOUT-WINDOW :AFTER :SELECT) (&REST IGNORE)
  (TURN-OFF-SHEET-BLINKERS SUPERIOR))

(DEFFLAVOR TYPEOUT-WINDOW ((LABEL NIL) (BORDERS NIL))
  (BASIC-TYPEOUT-WINDOW NOTIFICATION-MIXIN WINDOW))

(DEFFLAVOR TYPEOUT-WINDOW-WITH-MOUSE-SENSITIVE-ITEMS ((LABEL NIL) (BORDERS NIL))
  (BASIC-MOUSE-SENSITIVE-ITEMS TYPEOUT-WINDOW)
  (:DOCUMENTATION :COMBINATION "Typeout window with item operations."))
