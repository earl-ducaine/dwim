;;; -*-Base: 10.;  MODE: Common-lisp; PACKAGE: tv; FONTS: cptfont,hl12b,hl12bi -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated.  All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  10-24-88 MAY       Fix to (AUTO-SCROLLING-MIXIN :HANDLE-HOLD-OUTPUT) fixes spr 7159, 7505, 8076.
;;;                         Set more-vpos to y of previous cursorpos instead of (- inside-height line-height)
;;;                         which ignores top-margin and UNUSABLE pixel lines =
;;;                         (- (sheet-inside-height self) (* (sheet-line-height self) (sheet-number-of-inside-lines self)))
;;;   3/31/87  KK	Fixed off-by-one bug in TEXT-SCROLL-WINDOW-FLUSH-TYPEOUT.
;;;   3/16/87  KK	Changed TEXT-SCROLL-WINDOW-FLUSH-TYPEOUT to use BITBLT instead of :DRAW-RECTANGLE. This
;;;                         eliminates dependency on window graphics mixin used (TV or W) and fixes SPR # 3613.
;;; 11/20/86   SLM	Added calls to Suggestions macros for (:METHOD TV:AUTO-SCROLLING-MIXIN :HANDLE-HOLD-OUTPUT)
;;; 10/27/86   GRH	Fix a bug in :after :clear-screen for auto scrolling for pf.
;;; 10/20/86   TWE	More of the same for the TYPEP function.
;;; 09/24/86   DAN	Removed PRINT-ITEM-CONCISELY and GRIND-INTO-LIST stuff. Moved them to inspector.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 06/17/86   TWE	Fixed up :END-OF-PAGE-EXCEPTION for AUTO-SCROLLING-MIXIN to handle
;;;                        MORE-PROCESSING-GLOBAL-ENABLE proprely instead of just ignoring it.
;;;       	Changed TEXT-SCROLL-WINDOW :BEFORE :INIT and TEXT-SCROLL-WINDOW :SET-ITEMS
;;;			to use the :INITIAL-CONTENTS keyword of MAKE-ARRAY instead of using FILLARRAY.

(DEFFLAVOR TEXT-SCROLL-WINDOW
       ((ITEMS NIL)			;An array of all items
	(TOP-ITEM 0)			;The index of the topmost displayed item
	(ITEM-GENERATOR NIL)
	)
       ()
  (:REQUIRED-FLAVORS sheet)
  :GETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL)
  (:DOCUMENTATION :MIXIN "Scrolling of lines all of one type."))

;;; The item-generator feature:

;;; If the ITEM-GENERATOR variable is non-nil, then it replaces the
;;; array ITEMS in remembering what items we are displaying.  It should
;;; be a function which understands these operations:

;;; :NUMBER-OF-ITEMS			   returns the number of items
;;;                                        being displayed.
;;;  :NUMBER-OF-ITEM item-value		   returns the item-number of
;;;                                        that item-value.
;;;  :ITEM-OF-NUMBER item-number	   returns the item-value at
;;;				           that item number.
;;; The next two are optional, needed only if you want to use the
;;; :INSERT-ITEM, :DELETE-ITEM and :APPEND-ITEM operations on the
;;; window, which many applications for item generators will not need to
;;; do:
;;;  :INSERT-ITEM item-number item-value   inserts the item-value at
;;;					   that item number.
;;;  :DELETE-ITEM item-number              deletes the item at that item
;;;					   number.
;;; If ITEM-GENERATOR is nil, the variable ITEMS contains an array (with
;;; fill pointer) containing all the items.

(DEFMETHOD (TEXT-SCROLL-WINDOW :BEFORE :INIT) (PLIST)
  PLIST
  (COND ((ARRAYP ITEMS))
        ;; Change the ITEMS into an array if it isn't one already.
	((CONSP ITEMS) 
	 (SETQ ITEMS (MAKE-ARRAY (LENGTH ITEMS)
				 :FILL-POINTER (LENGTH ITEMS)
				 :INITIAL-CONTENTS ITEMS)))
	(T
	 (SETQ ITEMS (MAKE-ARRAY (OR ITEMS 100.) :FILL-POINTER 0))))) 

(DEFMETHOD (TEXT-SCROLL-WINDOW :MORE-EXCEPTION) ()
  (SETF (SHEET-MORE-FLAG) 0))

(DEFMETHOD (TEXT-SCROLL-WINDOW :SET-ITEMS) (NEW-ITEMS &OPTIONAL NO-DISPLAY)
  (SETQ ITEM-GENERATOR NIL)
  (SETQ ITEMS NEW-ITEMS)
  (COND ((ARRAYP ITEMS))
        ;; Change the ITEMS into an array if it isn't one already.
	((CONSP ITEMS) 
	 (SETQ ITEMS (MAKE-ARRAY (LENGTH ITEMS)
				 :FILL-POINTER (LENGTH ITEMS)
				 :INITIAL-CONTENTS ITEMS)))
	(T
	 (SETQ ITEMS (MAKE-ARRAY (OR ITEMS 100.) :FILL-POINTER 0))))
  (UNLESS NO-DISPLAY
    (SHEET-FORCE-ACCESS (SELF T)
      (SEND SELF :CLEAR-SCREEN)
      (SEND SELF :REDISPLAY 0 (SHEET-NUMBER-OF-INSIDE-LINES)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :SET-ITEM-GENERATOR) (NEW-ITEM-GENERATOR)
  (SETQ ITEM-GENERATOR NEW-ITEM-GENERATOR)
  (SHEET-FORCE-ACCESS (SELF T)
    (SEND SELF :CLEAR-SCREEN)
    (SEND SELF :REDISPLAY 0 (SHEET-NUMBER-OF-INSIDE-LINES))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :LAST-ITEM) ()
  (LET ((NITEMS (SEND SELF :NUMBER-OF-ITEMS)))
    (IF (PLUSP NITEMS)
	(SEND SELF :ITEM-OF-NUMBER (1- NITEMS)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :NUMBER-OF-ITEMS) ()
  (IF ITEM-GENERATOR (SEND ITEM-GENERATOR :NUMBER-OF-ITEMS)
    (ARRAY-ACTIVE-LENGTH ITEMS)))

(DEFMETHOD (TEXT-SCROLL-WINDOW :NUMBER-OF-ITEM) (ITEM)
  (IF ITEM-GENERATOR
    (SEND ITEM-GENERATOR :NUMBER-OF-ITEM ITEM)
    (DOTIMES (I (ARRAY-ACTIVE-LENGTH ITEMS))
      (AND (EQ (AREF ITEMS I) ITEM) (RETURN I))))) 

(DEFMETHOD (TEXT-SCROLL-WINDOW :ITEM-OF-NUMBER) (NUMBER)
  (IF ITEM-GENERATOR
    (SEND ITEM-GENERATOR :ITEM-OF-NUMBER NUMBER)
    (AREF ITEMS NUMBER))) 

(DEFMETHOD (TEXT-SCROLL-WINDOW :PUT-LAST-ITEM-IN-WINDOW) ()
  (OR (<= (SEND SELF :NUMBER-OF-ITEMS)
	 (+ TOP-ITEM (SHEET-NUMBER-OF-INSIDE-LINES) -1))
      ;; Last item not on screen -- put it on bottom line
      (SEND SELF :SCROLL-TO (- (SEND SELF :NUMBER-OF-ITEMS)
				   (SHEET-NUMBER-OF-INSIDE-LINES))
		    		:ABSOLUTE)))

(DEFMETHOD (TEXT-SCROLL-WINDOW :PUT-ITEM-IN-WINDOW) (ITEM)
  "If item not visible, put it in the window; if off the top, bring it to
the top.  If off the bottom, bring it to the bottom."
  (LET ((ITEM-NO (SEND SELF :NUMBER-OF-ITEM ITEM))
	(BOTTOM-ITEM (+ TOP-ITEM (SHEET-NUMBER-OF-INSIDE-LINES) -1)))
    (COND ((NULL ITEM-NO))
	  ((< ITEM-NO TOP-ITEM)
	   (SEND SELF :SCROLL-TO ITEM-NO :ABSOLUTE))
	  ((> ITEM-NO BOTTOM-ITEM)
	   (SEND SELF :SCROLL-TO (- ITEM-NO (- BOTTOM-ITEM TOP-ITEM))
                         :ABSOLUTE)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :APPEND-ITEM) (NEW-ITEM)
  (SEND SELF :INSERT-ITEM (SEND SELF :NUMBER-OF-ITEMS) NEW-ITEM))

(DEFMETHOD (TEXT-SCROLL-WINDOW :DELETE-ITEM) (ITEM-NO &AUX I)
  (IF ITEM-GENERATOR
    (SEND ITEM-GENERATOR :DELETE-ITEM ITEM-NO);Probably gets an error.
    (PROGN
      (DECF (FILL-POINTER ITEMS))
      (DO ((I ITEM-NO (1+ I)))
	  ((>= I (ARRAY-ACTIVE-LENGTH ITEMS)))
	(SETF (AREF ITEMS I) (AREF ITEMS (1+ I))))
      (COND
	((< ITEM-NO TOP-ITEM)
	 (SETQ TOP-ITEM (1- TOP-ITEM))
	 (SEND SELF :NEW-SCROLL-POSITION))
	((< ITEM-NO (+ TOP-ITEM (SHEET-NUMBER-OF-INSIDE-LINES)))
	 ;; Old item was on the screen -- flush it.
	 (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
	    (SHEET-SET-CURSORPOS SELF 0 (* LINE-HEIGHT (- ITEM-NO TOP-ITEM)))
	    (SEND SELF :DELETE-LINE 1)
	    (SEND SELF :REDISPLAY (SETQ I (1- (SHEET-NUMBER-OF-INSIDE-LINES))) (1+ I))))
	(T (SEND SELF :NEW-SCROLL-POSITION)))))
  ITEM-NO)

(DEFMETHOD (TEXT-SCROLL-WINDOW :INSERT-ITEM) (ITEM-NO NEW-ITEM)
  "Inserts an item before ITEM-NO."
  (IF ITEM-GENERATOR
    (SEND ITEM-GENERATOR :INSERT-ITEM ITEM-NO NEW-ITEM)
    (LET ((NO-ITEMS (ARRAY-LEADER ITEMS 0)))
      (SETQ ITEM-NO (MIN (MAX ITEM-NO 0) NO-ITEMS))
      (VECTOR-PUSH-EXTEND () ITEMS)
      (DOTIMES (I (- NO-ITEMS ITEM-NO))
       ;; Bubble items up.
	(SETF (AREF ITEMS (- NO-ITEMS I)) (AREF ITEMS (- NO-ITEMS I 1))))
      (SETF (AREF ITEMS ITEM-NO) NEW-ITEM)
      (COND
	((< ITEM-NO TOP-ITEM)
	 (SETQ TOP-ITEM (1+ TOP-ITEM))
	 (SEND SELF :NEW-SCROLL-POSITION))
	((< ITEM-NO (+ TOP-ITEM (SHEET-NUMBER-OF-INSIDE-LINES)))
	 ;; New item is on screen, insert a line then redisplay it.
	 (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
	    (SHEET-SET-CURSORPOS SELF 0 (* LINE-HEIGHT (SETQ ITEM-NO (- ITEM-NO TOP-ITEM))))
	    (SEND SELF :INSERT-LINE 1)
	    (SEND SELF :REDISPLAY ITEM-NO (1+ ITEM-NO))))
	(T (SEND SELF :NEW-SCROLL-POSITION)))))
  ITEM-NO) 

(DEFMETHOD (TEXT-SCROLL-WINDOW :AFTER :REFRESH) (&OPTIONAL TYPE)
  "When exposed, draw in the items."
  (AND (OR (NOT RESTORED-BITS-P) (EQ TYPE :SIZE-CHANGED))
       (SEND SELF :REDISPLAY 0 (SHEET-NUMBER-OF-INSIDE-LINES))))

(DEFMETHOD (TEXT-SCROLL-WINDOW :REDISPLAY) (START END)
  "Arguments are screen line indices -- assumes screen area already
erased."
  ;; I - screen relative line number.
  ;; J - item relative line number.
  (DO ((I START (1+ I))
       (J (+ START TOP-ITEM) (1+ J))
       (LIM (SEND SELF :NUMBER-OF-ITEMS)))
      ((OR (>= I END)
	   (AND (>= J LIM)
		;; Displaying some items can create more,
		;; so recheck how many items there are.
		(>= J (SETQ LIM (SEND SELF :NUMBER-OF-ITEMS))))))
    ;; Calculate the Y pixel number for this line.
    (SHEET-SET-CURSORPOS SELF 0 (* LINE-HEIGHT I))
    (SEND SELF :PRINT-ITEM (SEND SELF :ITEM-OF-NUMBER J) I J))
  (SEND SELF :NEW-SCROLL-POSITION))

;;; Each item is allowed only one line
(DEFWRAPPER (TEXT-SCROLL-WINDOW :PRINT-ITEM) (IGNORE . BODY)
  `(CATCH 'LINE-OVERFLOW . ,BODY))

(DEFMETHOD (TEXT-SCROLL-WINDOW :END-OF-LINE-EXCEPTION) ()
  (THROW 'LINE-OVERFLOW T) )

(DEFMETHOD (TEXT-SCROLL-WINDOW :PRINT-ITEM) (ITEM LINE-NO ITEM-NO)
  "Simplest printer, you want to redefine this probably."
  LINE-NO ITEM-NO				;Ignore these
  (PRIN1 ITEM SELF))

(DEFMETHOD (TEXT-SCROLL-WINDOW :SCROLL-BAR-P) ()
  "Here is where the scrolling is initiated."
  (OR (PLUSP TOP-ITEM)
      (> (SEND SELF :NUMBER-OF-ITEMS) (SHEET-NUMBER-OF-INSIDE-LINES))))

;;;patched on 11 Dec 85 for PDC by GSM
(DEFMETHOD (TEXT-SCROLL-WINDOW :SCROLL-POSITION) ()
  (BLOCK () (RETURN TOP-ITEM (SEND SELF :NUMBER-OF-ITEMS) LINE-HEIGHT
		   (sheet-number-of-inside-lines))))  ; pdc (12/4/85)

(DEFMETHOD (TEXT-SCROLL-WINDOW :SCROLL-TO) (NEW-TOP TYPE &AUX DELTA)
  (AND (EQ TYPE :RELATIVE) (SETQ NEW-TOP (+ TOP-ITEM NEW-TOP)))
  (SETQ NEW-TOP (MAX (MIN NEW-TOP (1- (SEND SELF :NUMBER-OF-ITEMS))) 0))
  (SETQ DELTA (- NEW-TOP TOP-ITEM))
  (OR (= DELTA 0)				;Nothing to change
      (SEND SELF :SCROLL-REDISPLAY NEW-TOP DELTA))
  (SEND SELF :NEW-SCROLL-POSITION))

(DEFMETHOD (TEXT-SCROLL-WINDOW :AFTER :NEW-SCROLL-POSITION) (&REST IGNORE)
  (MOUSE-WAKEUP))

;;; This is a message so it can have daemons
(DEFMETHOD (TEXT-SCROLL-WINDOW :SCROLL-REDISPLAY) (NEW-TOP DELTA &AUX NLINES)
  (SHEET-HOME SELF)
  (SETQ NLINES (SHEET-NUMBER-OF-INSIDE-LINES))
  (COND	((> DELTA 0)				;Scrolling forward
         ;; Make room for the new lines and then have :REDISPLAY
         ;; print them on the screen.
	 (SETQ DELTA (MIN DELTA NLINES))
	 (WITHOUT-INTERRUPTS
	   (SEND SELF :DELETE-LINE DELTA)
	   (SETQ TOP-ITEM NEW-TOP))
	 (SEND SELF :REDISPLAY (- NLINES DELTA) NLINES))
	((< DELTA 0)				;Scrolling backward
	 (SETQ DELTA (MIN (- DELTA) NLINES))
	 (WITHOUT-INTERRUPTS
	   (SEND SELF :INSERT-LINE DELTA)
	   (SETQ TOP-ITEM NEW-TOP))
	 (SEND SELF :REDISPLAY 0 DELTA)))
  (SEND SELF :NEW-SCROLL-POSITION))

(DEFFLAVOR FUNCTION-TEXT-SCROLL-WINDOW
       (PRINT-FUNCTION			; Function called to print the item
	(PRINT-FUNCTION-ARG NIL)	; Fixed argument for above
	)
       (TEXT-SCROLL-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES PRINT-FUNCTION PRINT-FUNCTION-ARG)
  (:DOCUMENTATION
    :MIXIN "Text scroll windows that print lines by calling a set function."))

;;; LIST is (print-function print-function-arg (items...)
;;;          top-item-number label item-generator)
;;; Either the list of items or the item-generator will usually be nil.
(DEFMETHOD (FUNCTION-TEXT-SCROLL-WINDOW :SETUP) (LIST)
 ;; Label changing should be first -- this may cause redisplay so flush
 ;; current items too.
  (AND ITEMS (STORE-ARRAY-LEADER 0 ITEMS 0))
  ;; The FIFTH item is the label.
  (AND (>= (LENGTH LIST) 5) (SEND SELF :SET-LABEL (FIFTH LIST)))
  (SEND SELF :SET-PRINT-FUNCTION (FIRST LIST))
  (SEND SELF :SET-PRINT-FUNCTION-ARG (SECOND LIST))
  (SETQ TOP-ITEM (OR (FOURTH LIST) 0))
  (IF (SIXTH LIST)
    (SEND SELF :SET-ITEM-GENERATOR (SIXTH LIST))
    (LET ((ARRAY (OR ITEMS (MAKE-ARRAY (LENGTH (THIRD LIST)) :LEADER-LIST '(0)))))
      (STORE-ARRAY-LEADER 0 ARRAY 0)
      (DO ((L (THIRD LIST) (CDR L)))
	  ((NULL L)
	   NIL)
	(VECTOR-PUSH-EXTEND (CAR L) ARRAY))
      (SEND SELF :SET-ITEMS ARRAY)))
  LIST) 

(DEFMETHOD (FUNCTION-TEXT-SCROLL-WINDOW :PRINT-ITEM) (ITEM IGNORE ITEM-NO)
  (FUNCALL PRINT-FUNCTION ITEM PRINT-FUNCTION-ARG SELF ITEM-NO))


(DEFFLAVOR TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN () (WINDOW-WITH-TYPEOUT-MIXIN)
  (:REQUIRED-FLAVORS TEXT-SCROLL-WINDOW)
  (:DOCUMENTATION :MIXIN "Makes a TEXT-SCROLL-WINDOW have a typeout window"))

(DEFUN TEXT-SCROLL-WINDOW-FLUSH-TYPEOUT ()
  "If the typeout window is active, deexpose it, and make sure the
  redisplayer knows how many lines were clobbered."
  (DECLARE (:SELF-FLAVOR TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN))
  (COND ((SEND TYPEOUT-WINDOW :ACTIVE-P)
	 (LET ((BR (MIN (SHEET-NUMBER-OF-INSIDE-LINES)
			(1+ (TRUNCATE (SEND TYPEOUT-WINDOW :BOTTOM-REACHED)
				      LINE-HEIGHT)))))
	   (SEND TYPEOUT-WINDOW :DEACTIVATE)
	   (PREPARE-SHEET (SELF)
	     (BITBLT ERASE-ALUF
		     (SHEET-INSIDE-WIDTH) (* BR LINE-HEIGHT)
		     W:100%-black 0 0
		     SCREEN-ARRAY (SHEET-INSIDE-LEFT) (SHEET-INSIDE-TOP)))
	   BR))))

(DEFWRAPPER (TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN :REDISPLAY) (ARGS . BODY)
  `(LET ((TO (TEXT-SCROLL-WINDOW-FLUSH-TYPEOUT)))
     (COND (TO
	    (SETF (FIRST ARGS) 0)               ; Starting line
	    (SETF (SECOND ARGS) (MAX TO (SECOND ARGS)))))       ; Ending line
     . ,BODY))

(DEFMETHOD (TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN :FLUSH-TYPEOUT) ()
  (LET ((TO (TEXT-SCROLL-WINDOW-FLUSH-TYPEOUT)))
    (AND TO (SEND SELF :REDISPLAY 0 TO))))

(DEFFLAVOR DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW
	(DISPLAYED-ITEMS		;An array of mouse sensitive items
	  )
	(TEXT-SCROLL-WINDOW)
  (:DOCUMENTATION
    :MIXIN
    "Keep track of displayed items on the screen.  We take care of
everything in maintaining DISPLAYED-ITEMS except one: the
:PRINT-ITEM operation you define is responsible for storing the desired
data into the element of DISPLAYED-ITEMS for the line displayed."))

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :AFTER :INIT) (IGNORE)
  (SETQ DISPLAYED-ITEMS (MAKE-ARRAY (SHEET-NUMBER-OF-INSIDE-LINES))))

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (LET ((NLINES (SHEET-NUMBER-OF-INSIDE-LINES)))
    (AND (< (ARRAY-TOTAL-SIZE DISPLAYED-ITEMS) NLINES)
       (ADJUST-ARRAY DISPLAYED-ITEMS NLINES)))) 

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :AFTER :CHANGE-OF-DEFAULT-FONT) (&REST IGNORE)
  (LET ((NLINES (SHEET-NUMBER-OF-INSIDE-LINES)))
    (AND (< (ARRAY-TOTAL-SIZE DISPLAYED-ITEMS) NLINES)
       (ADJUST-ARRAY DISPLAYED-ITEMS NLINES)))) 

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :DELETE-ITEM) (ITEM-NO &AUX AL)
  "Deleting an item -- if on the screen, update the displayed items
appropriately."
  (SETQ ITEM-NO (- ITEM-NO TOP-ITEM)
	AL (SHEET-NUMBER-OF-INSIDE-LINES))
  (COND
    ((AND (>= ITEM-NO 0) (< ITEM-NO AL))
     (DOTIMES (I (- AL ITEM-NO 1))
       (SETF (AREF DISPLAYED-ITEMS (+ I ITEM-NO)) (AREF DISPLAYED-ITEMS (+ I ITEM-NO 1))))
     (SETF (AREF DISPLAYED-ITEMS (1- AL)) ())))) 

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :INSERT-ITEM) (ITEM-NO IGNORE &AUX AL)
  "Inserting an item -- adjust the data structure appropriatly."
  (SETQ ITEM-NO (- ITEM-NO TOP-ITEM)
	AL (SHEET-NUMBER-OF-INSIDE-LINES))
  (COND
    ((AND (>= ITEM-NO 0) (< ITEM-NO AL))
     ;; The item will be on the screen, adjust the data structure
     (DOTIMES (I (- AL ITEM-NO 1))
       (SETF (AREF DISPLAYED-ITEMS (- AL I 1)) (AREF DISPLAYED-ITEMS (- AL I 2))))
     (SETF (AREF DISPLAYED-ITEMS ITEM-NO) ()))))


(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :REDISPLAY) (START END)
  "Forget anything that was on screen before."
  (DO ((I START (1+ I)))
      ((>= I END)
       NIL)
    (SETF (AREF DISPLAYED-ITEMS I) ()))) 

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :SET-ITEMS) (&REST IGNORE)
  "Make sure mouse isn't left pointing to gubbish."
  (DOTIMES (I (ARRAY-TOTAL-SIZE DISPLAYED-ITEMS))
    (SETF (AREF DISPLAYED-ITEMS I) ()))) 

(DEFMETHOD (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW :BEFORE :SCROLL-REDISPLAY) (IGNORE DELTA &AUX NLINES)
  (SETQ NLINES (SHEET-NUMBER-OF-INSIDE-LINES))
  (COND
    ((> DELTA 0)				;Scrolling forward
     (DO ((I DELTA (1+ I))
	  (J 0 (1+ J)))
	 ((>= I NLINES)
	  (DO ((J J (1+ J)))
	      ((>= J NLINES)
	       NIL)
	    (SETF (AREF DISPLAYED-ITEMS J) ())))
       (SETF (AREF DISPLAYED-ITEMS J) (AREF DISPLAYED-ITEMS I))))
    ((< DELTA 0)				;Scrolling backward
     (DO ((I (1- (+ NLINES DELTA)) (1- I))
	  (J (1- NLINES) (1- J)))
	 ((< I 0)
	  (DO ((J J (1- J)))
	      ((< J 0)
	       NIL)
	    (SETF (AREF DISPLAYED-ITEMS J) ())))
       (SETF (AREF DISPLAYED-ITEMS J) (AREF DISPLAYED-ITEMS I))))))

(DEFFLAVOR MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK
       (
        ;; Types of items that can be selected.
        (SENSITIVE-ITEM-TYPES T)
        ;; Blinker for displaying things.
	ITEM-BLINKER
	)
       (DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES SENSITIVE-ITEM-TYPES)
  (:DOCUMENTATION
    :MIXIN "Text scroll window that allows selection of parts of text."))

(DEFSTRUCT (MOUSE-SENSITIVE-ITEM (:CONSTRUCTOR NIL) (:CONC-NAME NIL) (:CALLABLE-CONSTRUCTORS NIL)
		       (:ALTERANT ALTER-MOUSE-SENSITIVE-ITEM) (:PREDICATE NIL) (:COPIER NIL)
		       (:TYPE :LIST))
  DISPLAYED-ITEM-ITEM
  DISPLAYED-ITEM-TYPE
  DISPLAYED-ITEM-LEFT
  DISPLAYED-ITEM-RIGHT) 

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :AFTER :INIT)
           (IGNORE)
  (SETQ ITEM-BLINKER (MAKE-BLINKER SELF 'HOLLOW-RECTANGULAR-BLINKER
                                   :VISIBILITY NIL)))

;;; Generally called inside a :PRINT-ITEM.  This is what used to be
;;; called the :ITEM operation.
(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :ITEM1)
	   (ITEM TYPE &OPTIONAL (function #'PRIN1) &REST PRINT-ARGS &AUX DISITEM)
  "Print something that is sensitive to the mouse."
  (SETQ DISITEM (LIST ITEM TYPE CURSOR-X (SHEET-INSIDE-RIGHT)))
  (PUSH DISITEM (AREF DISPLAYED-ITEMS (SHEET-LINE-NO)))
  (APPLY FUNCTION ITEM SELF PRINT-ARGS)
  ;; Try to avoid making zero-length items that cannot be selected with
  ;; the mouse.
  (SETF (DISPLAYED-ITEM-RIGHT DISITEM)
	(MIN (MAX (+ (DISPLAYED-ITEM-LEFT DISITEM) (SHEET-CHAR-WIDTH SELF))
		  CURSOR-X)
	     (SHEET-INSIDE-RIGHT)))
  (MOUSE-WAKEUP)) 

;;; Generally called inside a :PRINT-ITEM.  This is compatible with the
;;; :ITEM operation on typeout windows, etc.
(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :ITEM)
	   (TYPE ITEM &REST FORMAT-STRING-AND-ARGS &AUX DISITEM)
  "Print something that is sensitive to the mouse."
  (IF (AND FORMAT-STRING-AND-ARGS
	   (OR (TYPEP (CAR FORMAT-STRING-AND-ARGS) 'COMPILED-FUNCTION)
	       (SYMBOLP (CAR FORMAT-STRING-AND-ARGS))))
      ;; Appears to be a use of the old :ITEM operation, which is now :ITEM1.
      (LEXPR-SEND SELF :ITEM1 TYPE ITEM FORMAT-STRING-AND-ARGS)
      (PROGN
	(SETQ DISITEM (LIST ITEM TYPE CURSOR-X (SHEET-INSIDE-RIGHT)))
	(PUSH DISITEM (AREF DISPLAYED-ITEMS (SHEET-LINE-NO)))
	(COND
	  ((NULL FORMAT-STRING-AND-ARGS) (PRINC ITEM SELF))
	  ((TYPEP (CAR FORMAT-STRING-AND-ARGS) 'STRING)
	   (APPLY #'FORMAT SELF FORMAT-STRING-AND-ARGS))
	  (T (FUNCALL (CAR FORMAT-STRING-AND-ARGS) ITEM SELF)))
	;; Try to avoid making zero-length items that cannot be selected
	;; with the mouse.
	(SETF (DISPLAYED-ITEM-RIGHT DISITEM)
	      (MIN (MAX (+ (DISPLAYED-ITEM-LEFT DISITEM) (SHEET-CHAR-WIDTH SELF))
			CURSOR-X)
		   (SHEET-INSIDE-RIGHT)))
	(MOUSE-WAKEUP))))

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK
             :AFTER :HANDLE-MOUSE) ()
  (BLINKER-SET-VISIBILITY ITEM-BLINKER NIL))

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :BEFORE :SET-ITEMS)
	   (&REST IGNORE)
  "Turn off blinker before setting up new items."
  (BLINKER-SET-VISIBILITY ITEM-BLINKER NIL))

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK :MOUSE-MOVES)
	   (X Y &AUX ITEM TYPE LEFT TOP BWIDTH BHEIGHT)
  "Blink any item the mouse points to."
  (MOUSE-SET-BLINKER-CURSORPOS)
  (MULTIPLE-VALUE-SETQ (ITEM TYPE LEFT BWIDTH TOP)
    (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
  (COND
    (TYPE
     ;; The mouse is pointing at a sensitive item.
     (SETQ BWIDTH (- BWIDTH LEFT)
	   BHEIGHT (FONT-BLINKER-HEIGHT CURRENT-FONT))
     (BLINKER-SET-CURSORPOS ITEM-BLINKER (- LEFT (SHEET-INSIDE-LEFT)) (- TOP (SHEET-INSIDE-TOP)))
     (BLINKER-SET-SIZE ITEM-BLINKER BWIDTH BHEIGHT)
     (BLINKER-SET-VISIBILITY ITEM-BLINKER T))
    ;; The mouse is not pointing at a sensitive item.
    (T (BLINKER-SET-VISIBILITY ITEM-BLINKER ())))) 

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK
             :MOUSE-SENSITIVE-ITEM) (X Y)
  (MOUSE-SENSITIVE-ITEM X Y))


(DEFUN MOUSE-SENSITIVE-ITEM (X Y &optional allowed-types &AUX LINE-NO)
  "Return the mouse-sensitive-item at position X, Y in a text scroll
window.  The window must be of the flavor
MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK.
Returns NIL if there is no mouse-sensitive item there."
  (DECLARE (:SELF-FLAVOR MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK))
  (unless allowed-types (setq allowed-types SENSITIVE-ITEM-TYPES ))
  (SETQ LINE-NO (SHEET-LINE-NO () Y))
  (AND (>= Y (SHEET-INSIDE-TOP))
       (< Y (+ (SHEET-INSIDE-TOP) (* (SHEET-NUMBER-OF-INSIDE-LINES) LINE-HEIGHT)))
       (DOLIST (ITEM (AREF DISPLAYED-ITEMS LINE-NO))
	 (AND
	   (OR (EQ ALLOWED-TYPES T)	;If everything visible,
	       (IF (FUNCTIONP ALLOWED-TYPES)
		   ;; Filter function.
		   (SEND ALLOWED-TYPES ITEM)
		   (IF (NOT (OR (CONSP ALLOWED-TYPES)
				(NULL ALLOWED-TYPES)))
		       (SEND SELF ALLOWED-TYPES ITEM)
		       (MEMBER (DISPLAYED-ITEM-TYPE ITEM) ALLOWED-TYPES :TEST #'EQ))))
	   ;; And within this place on the line.
	   (<= (DISPLAYED-ITEM-LEFT ITEM) X)
	   (> (DISPLAYED-ITEM-RIGHT ITEM) X)
	   (RETURN (DISPLAYED-ITEM-ITEM ITEM) (DISPLAYED-ITEM-TYPE ITEM)
		   (DISPLAYED-ITEM-LEFT ITEM) (DISPLAYED-ITEM-RIGHT ITEM)
		   (+ (SHEET-INSIDE-TOP) (* LINE-NO LINE-HEIGHT)))))))

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK
             :AFTER :SET-SENSITIVE-ITEM-TYPES)
	   (&REST IGNORE)
  (MOUSE-WAKEUP))

(DEFFLAVOR MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW ()
	   (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK)
  (:REQUIRED-FLAVORS MINIMUM-WINDOW))

(DEFMETHOD (MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW :MOUSE-CLICK)
	   (BUTTON X Y &AUX ITEM TYPE)
  (MULTIPLE-VALUE-SETQ (ITEM TYPE)
    (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
  (COND
    (TYPE
     (SEND SELF :FORCE-KBD-INPUT (LIST TYPE ITEM SELF (MERGE-SHIFT-KEYS BUTTON)))
     T))) 

(DEFFLAVOR TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK () ()
  (:REQUIRED-FLAVORS TEXT-SCROLL-WINDOW)
  (:DOCUMENTATION
    :MIXIN
    "Text scroll window that is grayed when it has no items."))

(DEFMETHOD (TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK :AFTER :REDISPLAY)
	   EMPTY-GRAY-HACK-DRAW-GRAY)

(DEFUN EMPTY-GRAY-HACK-DRAW-GRAY (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK))
  (OR (PLUSP (SEND SELF :NUMBER-OF-ITEMS))
      (PREPARE-SHEET (SELF)
        ;; Note that when we BITBLT a GRAY array, the contents of the
        ;; gray array gets replicated enough times to fit into the width
        ;; of the rectangle.
        (BITBLT CHAR-ALUF (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
		25%-GRAY 0 0
		SCREEN-ARRAY (SHEET-INSIDE-LEFT) (SHEET-INSIDE-TOP)))))

(DEFMETHOD (TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK :BEFORE :INSERT-ITEM)
           (&REST IGNORE)
  (OR (PLUSP (SEND SELF :NUMBER-OF-ITEMS))
      ;; We must have been gray -- erase ourselves.
      (SEND SELF :CLEAR-SCREEN)))

(DEFMETHOD (TEXT-SCROLL-WINDOW-EMPTY-GRAY-HACK :AFTER :DELETE-ITEM)
	   EMPTY-GRAY-HACK-DRAW-GRAY)


;;; Mixin to provide smooth scrolling to any window that does't have mouse
;;; processing.


(DEFFLAVOR AUTO-SCROLLING-MIXIN
           ((SCROLL-PIXEL-INCREMENT -16.)
            (HOLD-OUTPUT? NIL))
           ()
  :GETTABLE-INSTANCE-VARIABLES
  (:REQUIRED-FLAVORS SHEET)
  (:INITABLE-INSTANCE-VARIABLES SCROLL-PIXEL-INCREMENT)
  (:DEFAULT-INIT-PLIST :ASYNCHRONOUS-CHARACTERS
		       (APPEND TV:KBD-STANDARD-ASYNCHRONOUS-CHARACTERS
			       '((#\HOLD-OUTPUT TOGGLE-HOLD-OUTPUT))))
  (:DOCUMENTATION :MIXIN "Move everything up at the end of the sheet."))

;;;   Define it here so we can add documentation.
(DEFMETHOD (AUTO-SCROLLING-MIXIN :SET-SCROLL-PIXEL-INCREMENT) (VALUE)
  "The scroll-pixel increment is the number of pixels to scroll up when
typing at the bottom of the screen If NIL, default to the line height
(i.e. scroll up one line).  If negative, (abs value) will be the screen
size in lines at which the scroll-pixel-increment will be one line.  If
the screen is larger than VALUE, scrolling will be done with
porportionally more lines; If the screen is smaller than VALUE, scrolling
will be done more than once per line to make it appear more smooth.
The smaller the screen the more times it will scroll per new line.  The
idea is to make displaying a block of information take the same time no
mater what size the window is.  The larger VALUE is, the faster the
display wil be.  When VALUE is zero, scrolling is disabled."
  (SETQ SCROLL-PIXEL-INCREMENT VALUE))

(DEFMETHOD (AUTO-SCROLLING-MIXIN :AFTER :CLEAR-SCREEN) (&REST IGNORE)
    (WHEN (and (SEND SELF :more-p) (not (zerop scroll-pixel-increment)))
      (SETQ more-vpos 0)))

;;; Called from within the keyboard process.
(DEFUN TOGGLE-HOLD-OUTPUT (IGNORE WINDOW)
  (SEND WINDOW :SEND-IF-HANDLES :TOGGLE-HOLD-OUTPUT))

(DEFMETHOD (AUTO-SCROLLING-MIXIN :TOGGLE-HOLD-OUTPUT) ()
  (WHEN (NOT (SETQ HOLD-OUTPUT? (NOT HOLD-OUTPUT?)))
    (SEND SELF :FORCE-KBD-INPUT #\SPACE)))

(sys:declare-suggestions-for '(:method TV:AUTO-SCROLLING-MIXIN :HANDLE-HOLD-OUTPUT)
			 :around (sys:make-default-around-form
				   (:method TV:AUTO-SCROLLING-MIXIN :HANDLE-HOLD-OUTPUT)
				   nil nil default-more-menu
				   :pop-menus-form
				   (sys:special-pop-menus
				     '(:METHOD TV:SHEET :MORE-EXCEPTION)
				     nil nil 'default-more-menu)))

(DEFMETHOD (AUTO-SCROLLING-MIXIN :HANDLE-HOLD-OUTPUT)
	   (&OPTIONAL (MORE-STRING *UNIDIRECTIONAL-MORE-STANDARD-MESSAGE*) (OPERATION :TYI)
	    &AUX CHAR (CURRENT-X CURSOR-X))
  (sys:with-suggestions-menus-for (:method tv:auto-scrolling-mixin :handle-hold-output)
  (SHEET-CLEAR-EOL SELF)
  (LET-GLOBALLY ((CURRENT-FONT (AREF FONT-MAP 0))
		 (CHAR-WIDTH (FONT-CHAR-WIDTH CURRENT-FONT)))
     (AND MORE-STRING
	  (SHEET-STRING-OUT SELF MORE-STRING)))
  (UNWIND-PROTECT
      (SETQ CHAR (SEND SELF :SEND-IF-HANDLES OPERATION))
    (SETQ HOLD-OUTPUT? ())
    (WHEN MORE-VPOS
      (SETQ MORE-VPOS (- ;(SHEET-INSIDE-HEIGHT SELF) LINE-HEIGHT))))  	;; may 10-24-88 
			 (sheet-cursor-y self) line-height))))		;; may 10-24-88 
  (COND
    (MORE-STRING (SETQ CURSOR-X CURRENT-X)	;Wipe out the **MORE**
     (SHEET-CLEAR-EOL SELF)))
  CHAR)) 

(DEFMETHOD (AUTO-SCROLLING-MIXIN :MORE-EXCEPTION) ()
  (IF (EQ SCROLL-PIXEL-INCREMENT 0)
      (OR (ZEROP (SHEET-MORE-FLAG))
	  (SHEET-MORE-HANDLER))
     (SETF (SHEET-MORE-FLAG) 0)))

;;;replaced the following defwrapper on 18 Dec 85 for GRH by GSM
(DEFWRAPPER (AUTO-SCROLLING-MIXIN :NOTICE) ((EVENT &REST IGNORE) . BODY)
  `(IF
     (AND (NOT (EQ SCROLL-PIXEL-INCREMENT 0))
	  (EQ EVENT :INPUT-WAIT)
	  MORE-VPOS
	  (SETQ MORE-VPOS (- (SHEET-INSIDE-HEIGHT SELF)
			     (- (SHEET-INSIDE-BOTTOM) CURSOR-Y))))
     T
     . ,BODY))

(DEFMETHOD (AUTO-SCROLLING-MIXIN :AROUND :END-OF-PAGE-EXCEPTION)
	   (CONT MT ARGS &REST IGNORE &AUX INCREMENT)
  "Scroll the window up at the end of the page"
  ;; Scroll the window.
  (SETQ INCREMENT (OR SCROLL-PIXEL-INCREMENT LINE-HEIGHT))
  (UNLESS (EQ SCROLL-PIXEL-INCREMENT 0)
    (SETF (SHEET-END-PAGE-FLAG SELF) 0))
  (WHEN (MINUSP INCREMENT)
    (SETQ INCREMENT
          (* (CEILING (/ (truncate (SHEET-INSIDE-HEIGHT SELF) LINE-HEIGHT)
                         (- INCREMENT)))
             LINE-HEIGHT)))
  ;; Wait for output not held.
  (COND
    ((<= INCREMENT 0)
     (LEXPR-FUNCALL-WITH-MAPPING-TABLE CONT MT ARGS))
    (HOLD-OUTPUT?
     (SEND SELF :SCROLL-UP LINE-HEIGHT)
     (SEND SELF :HANDLE-HOLD-OUTPUT))
    ((NULL MORE-PROCESSING-GLOBAL-ENABLE)
     (SEND SELF :SCROLL-UP LINE-HEIGHT))
    ((AND MORE-VPOS (<= MORE-VPOS INCREMENT))
     (SEND SELF :SCROLL-UP LINE-HEIGHT)
     (SEND SELF :HANDLE-HOLD-OUTPUT))
    (T
     (SEND SELF :SCROLL-UP INCREMENT)
     (WHEN MORE-VPOS
       (DECF MORE-VPOS (MAX INCREMENT LINE-HEIGHT))))))

(DEFMETHOD (AUTO-SCROLLING-MIXIN :SCROLL-UP) (INCREMENT &AUX REMANING-INCREMENT)
  (DECLARE (SPECIAL RUBOUT-HANDLER-STARTING-Y PROMPT-STARTING-Y))
  (COND
    ((<= INCREMENT 0))
    ((< INCREMENT LINE-HEIGHT)
     (DOTIMES (I (truncate LINE-HEIGHT INCREMENT))
       (SHEET-SCROLL-UP SELF INCREMENT))
     (IF (PLUSP (SETQ REMANING-INCREMENT (REM LINE-HEIGHT INCREMENT)))
	 (SHEET-SCROLL-UP SELF REMANING-INCREMENT))
     (SETQ INCREMENT LINE-HEIGHT))
    ((SHEET-SCROLL-UP SELF INCREMENT)))
  (SETF (SHEET-CURSOR-X SELF) (SHEET-INSIDE-LEFT SELF))
  ;; When inside the rubout handler, tell it about scrolling.
  (WHEN (VARIABLE-BOUNDP RUBOUT-HANDLER-STARTING-Y)
    (DECF RUBOUT-HANDLER-STARTING-Y INCREMENT))
  (WHEN (VARIABLE-BOUNDP PROMPT-STARTING-Y)
    (DECF PROMPT-STARTING-Y INCREMENT)))

(DEFUN sheet-scroll-up (sheet height)
  "Move sheet up HEIGHT pixels."
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (WIDTH (SHEET-INSIDE-WIDTH SHEET))
	  DELTA-HEIGHT)
      ;; Compute minus height of block to BLT.
      (SETQ DELTA-HEIGHT
	    (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET) HEIGHT))
      (IF (PLUSP DELTA-HEIGHT)			;If some bits to move, move them
	  (if (color-system-p sheet)
	      (si:%scroll
		WIDTH DELTA-HEIGHT
		(SHEET-INSIDE-LEFT SHEET) (+ (SHEET-INSIDE-TOP SHEET) HEIGHT)
		(SHEET-INSIDE-TOP SHEET) ARRAY)
	      (BITBLT
		ALU-SETA
		WIDTH DELTA-HEIGHT
		ARRAY (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-INSIDE-TOP SHEET) HEIGHT)
		ARRAY (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET)))
	(INCF HEIGHT DELTA-HEIGHT))
      (WHEN (PLUSP HEIGHT)
	(SETF (SHEET-CURSOR-Y SELF) (- (SHEET-CURSOR-Y SELF) HEIGHT)) 
	(%DRAW-RECTANGLE WIDTH HEIGHT
			 (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)
			 (SHEET-ERASE-ALUF SHEET) SHEET)))))

(DEFMETHOD (AUTO-SCROLLING-MIXIN :INCREMENT-CURSORPOS) (DX DY &OPTIONAL (UNIT :PIXEL))
  (CASE UNIT
	(:PIXEL)
	(:CHARACTER
	 (AND DX
	      (SETQ DX
		    (- (* CHAR-WIDTH DX)
		       (NTH-VALUE 1 (CEILING (- CURSOR-X LEFT-MARGIN-SIZE) CHAR-WIDTH)))))
	 (AND DY
	      (SETQ DY
		    (- (* LINE-HEIGHT DY)
		       (NTH-VALUE 1 (CEILING (- CURSOR-Y TOP-MARGIN-SIZE) LINE-HEIGHT))))))
	(OTHERWISE (FERROR () "~S is not a known unit." UNIT)))
  (PREPARE-SHEET (SELF)
    (OR (ZEROP (SHEET-EXCEPTIONS))
	(SHEET-HANDLE-EXCEPTIONS SELF))
    (SHEET-INCREMENT-BITPOS SELF DX DY)))	;Change 0 to DY -DLC

