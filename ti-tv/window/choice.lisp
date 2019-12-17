;;; -*- Mode:COMMON-LISP; Package:TV; Base:10.; Fonts:(CPTFONT HL12B HL12BI) -*-

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
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

#| This file contains line areas, multiple choice,
choose-variable-values.  Margin-scroll-region, flashy-scrolling and
user-options are now in one of the obsolete files.  -GRH 2-19-86 |#

;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  02/02/89  MAY	Removed hardcoded fonts:cptfontb in label of CHOOSE-VARIABLE-VALUES. SPR 9252.
;;;  08-13-88  MAY      Changed CHOOSE-VARIABLE-VALUES to use same kind of margin-choice's as does w:menu
;;; 			this allows documentation to be placed on margin-choices in cvv menus. Supplements SPR 8241.
;;;                         Also changed CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE, EXECUTE-MARGIN-CHOICE, 
;;;			and MORE-CHOICES to look at eighth item in margin-choice list in/of sixth item.
;;;  08-13-88  MAY      Changed CHOOSE-VARIABLE-VALUES-MENU-PRINT to allow both lists AND non-lists in
;;;			alist passed to :menu option of cvv. Was judging contents of alist on the first item ONLY. SPR 8348 
;;;  07/01/88  MAY       Changed :setup method of BASIC-CHOOSE-VARIABLE-VALUES to default -WIDTH- to t. SPR 7027
;;;  9/14/87    PMH      Added W:CVV-GET-COLOR; we should have made a :color type; until then
;;;                         use this function as a constraint or side-effect
;;;   8/7/87    PMH      Modified :number-or-nil and :fixnum-or-nil so a return on an empty string will
;;;                         supply a NIL value.  SPR#3895
;;;                         also changed :fixnum and :fixnum-or-nil to be fixnum instead of integer.
;;;   7/25/87   PMH       Modified CHOOSE-VARIABLE-VALUES-CHOICE to switch to font#2, :value-font
;;;                         also modified the :constraint test to occur in in this function so we
;;;                         could get better handling on error messages.
;;;                         also corrected problem with old value not going completely away after pressing
;;;                         the space key to make the error go away.
;;;                         also modified :print-item to bind ITEM-VALUE-FOR-SET instead of setq'ing it
;;;                         this was a problem after erasing a error message.
;;;                         also modified CVV rubout handler so someone may EDIT empty string.
;;;                         also fixed up the management of the clear-input key when EDITing
;;;                         also modified default values of for :set-variables so resizing will be the default behavior
;;;   7/20/87  PMH       Changed :move-cursor to not hang in a loop if there are no items.
;;;   4/23/87  SLM       Make the CHOOSE-VARIABLE-VALUES-KEYWORD property for CVV-type :CHARACTER
;;;                         use READ-CHAR instead of (READ-CHAR).  This makes :LIST-OF :CHARACTER work.
;;;                         Fixes PROFILE SPR# 4842.
;;;   3/27/87  KDB	Fixed (BASIC-CHOOSE-VARIABLE-VALUES :SETUP) to handle between item space properly. 
;;;                         Added CHOICE-ITEM-LAYOUT function to modularize calculation. Closed SPR# 3835.
;;;   3/20/87  KDB	Patched CVV-MULTIPLE-MENU-PRINT. Replaced CONSP with LISTP.
;;;   3/19/87  KDB	Fixed CVV :CHARACTER keyword handling. Now calls (READ-CHAR) instead of (CHAR)
;;;                          Closed SPR# 3842.
;;;   3/13/87  KDB	Modified calculation of -WIDTH- in :SET-VARIABLES. Width will be appropriate even
;;;			if user underspecifies value of :WIDTH keyword. Closed SPR# 3299.
;;;   3/12/87  KDB        Added test to ensure CVV user specified height is large enough to display at least
;;;			one item. Closed SPR# 3298.
;;;   3/12/87  KDB	Added error message to CVV-MULTIPLE-MENU-PRINT to catch use of non-list type
;;;			variables with :multiple-menu-choose option in CVVs. Closed SPR# 3804
;;;   3/11/87  KDB	Prevented CVV from accepting NIL variable lists. Closed SPR# 3813.
;;;   2/12/87  KDB	Fixed CVV margin choice item overlap bug by adding space to margin region width if
;;;                          more than two choices. Fix is in (BASIC-CHOOSE-VARIABLE-VALUES :SETUP) and
;;;                         :SET-VARIABLES. Closes bug reports 3417 and 3418. 
;;;   2/05/87  TWE	Fixed up the CVV function to return NIL on the END key case if there is no margin
;;;			choice form to evaluate.  Also fixed up the ABORT key case to signal the abort
;;;			condition when there is no ABORT margin choice.  This fixes bug 3292.
;;;   2/03/87  KDB	Added resource deallocation code to (BASIC-CHOOSE-VARIABLE-VALUES :AFTER :SELECT)
;;;                          to fix margin choice box reexposure bug.
;;;   1/20/87  TWE	Fixed the CVV function to handle the ABORT/END keys on the keyboard the same as
;;;			clicking on the ABORT/DO IT margin choice boxes.  This fixes the first half of bug
;;;			number 2731.
;;; 12/23/86   TWE	Made several changes to make the build process cleaner.
;;;			Added a :REQUIRED-FLAVORS ESSENTIAL-WINDOW for the BASIC-MULTIPLE-CHOICE flavor.  This
;;;			allows it to refer to instance variables like WIDTH.
;;; 12/12/86   TWE	Turned off the CVV-DEBUG flag which accidentally got turned on.
;;; 12/08/86   LGO	Renamed :predicatep to :TEST
;;; 11/25/86   TWE	Changed a line-area function to use draw-char instead of %draw-char to draw the
;;;			right arrow.  This makes the Window System build look better.
;;; 11/20/86   TWE	Added the EH package prefix for *current-stack-group*.  Also fixed up :STRING-LIST
;;;			to have its property set up properly.
;;; 11/19/86   TWE	Fixed the :print-item method to properly handle :assoc variable types.  Recoded the
;;;			DO as a LOOP instead.
;;; 11/13/86   TWE	Fixed CVV to not draw the leader lines for simple strings.
;;; 11/04/86   TWE	Made *CURRENT-STACK-GROUP* special in basic-cvv :setup.
;;; 11/04/86   TWE	Changed references to CURRENT-STACK-GROUP to work for release 2.x and 3.0.
;;; 11/04/86   LGO	Change choose-variable-values-character-or-nil-read to check for #\return instead of #\clear-input
;;; 10/31/86   TWE	Changed references to CURRENT-STACK-GROUP and EH:ABORT-OBJECT to use the versions with
;;;			asterisks around them.  Changed READTABLE, and friends to *READTABLE*.
;;; 10/21/86   TWE	Changed references to the mouse glyphs to refer to the correct constants.
;;; 10/20/86   TWE	Changed the choose-variable-values function to do a better check for the `Do It'
;;;			margin choice.
;;; 10/14/86   TWE	A use of %%kbd-control-meta to use the Common Lisp char-bits function instead.
;;; 10/10/86   TWE	Changed "keyboard" for the who state to use the global variable instead.
;;; 10/03/86   GRH	Scroll-bar-mixin -> w:Scroll-bar-mixin
;;; 09/30/86   GRH	Converted CVV and Multiple-choose to the new scroll-bar
;;; 09/25/86   TWE	Fixed up the :TYPEP variable type to properly handle subtypes like (integer 2 8).
;;;			Removed the :FIXNUM-IN-RANGE variable type added on 9/18/86 since :TYPEP
;;;			(INTEGER low high) will do the same thing with about the same amount of effort on
;;;			the user of CVV's part.  Also change another case of an "Exit" margin choice to "Do It".
;;; 09/23/86   TWE	Added new variable types :PREDICATEP, :TYPEP and :LIST-OF which generalize some of
;;;			those entered on 9/18/86.  Those which were generlized have been removed.
;;; 09/18/86   GRH	Added some CVV variable types which used to exist in the Profile source.
;;; 09/18/86   TWE	Changed the doit and abort margin choices to refer to the variables which contain
;;;			these strings as values.
;;; 09/05/86   KDB	Modified CVV to have "Leader Lines" between variable names and values.
;;; 09/05/86   KDB	Basic-multi-choice :MOUSE-CLICK method now accepts any click, and agrees with
;;;			documentation string for choice box.
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 07/22/86   TWE	Added a flag CVV-DEBUG to selectively print out debug information.  Instrumented
;;;			:appropriate-width.
;;; 06/17/86   TWE	Changed the resource for TEMPORARY-MULTIPLE-CHOICE-WINDOW to use make-array
;;; 			instead of fillarray.
;;;

(DEFVAR CVV-DEBUG NIL
  "Debug flag used to turn on/off printing of debug information.")

#| The LINE-AREA-TEXT-SCROLL-MIXIN defines a "line area" near the  left
edge in  which  the  mouse  cursor  becomes a rightward-arrow and mouse
clicks' meaning is changed.

Clicks while  in  the  line  area  force  an  input  blip:  (:LINE-AREA
this-line's-item   the-window   encoded-mouse-click)   this-line's-item
stands for the item displayed on this line (see TSCROL).

You should provide a  :LINE-AREA-MOUSE-DOCUMENTATION method to  provide
the mouse documentation while the cursor is in the line area.  |#

(DEFFLAVOR LINE-AREA-TEXT-SCROLL-MIXIN () ()
  (:REQUIRED-FLAVORS MARGIN-REGION-MIXIN TEXT-SCROLL-WINDOW)
  (:INIT-KEYWORDS :LINE-AREA-WIDTH)
  (:METHOD-COMBINATION (:DAEMON-WITH-OVERRIDE :BASE-FLAVOR-LAST :LINE-AREA-MOUSE-DOCUMENTATION))
  (:DOCUMENTATION :MIXIN "Allows selection of a line from the left margin."))

(DEFMETHOD (LINE-AREA-TEXT-SCROLL-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (PUSH (LIST 'LINE-AREA-REGION :LEFT
              (OR (GET INIT-PLIST :LINE-AREA-WIDTH) 24.)
              0 0 0 0)
	REGION-LIST))

(DEFMETHOD (LINE-AREA-TEXT-SCROLL-MIXIN :LINE-AREA-MOUSE-DOCUMENTATION) ()
  "Select a line.")

(DEFUN LINE-AREA-REGION (OP &OPTIONAL IGNORE Y IGNORE BD)
  (DECLARE (:SELF-FLAVOR LINE-AREA-TEXT-SCROLL-MIXIN))
  (CASE OP
     ((:REFRESH :MOUSE-MOVES) NIL)
     (:MOUSE-ENTERS-REGION 
      (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 13. 6. :ON
				    :SET-CHARACTER MOUSE-GLYPH-THIN-RIGHT-ARROW))
     (:MOUSE-LEAVES-REGION 
      (MOUSE-STANDARD-BLINKER))
     (:MOUSE-CLICK 
      (LET (ITEM LINE)
	(IF (AND (>= Y (SHEET-INSIDE-TOP))
		 (SETQ LINE (+ TOP-ITEM (SHEET-LINE-NO () Y)))
		 (< LINE (ARRAY-ACTIVE-LENGTH ITEMS)))
	    (PROGN
	      (SETQ ITEM (AREF ITEMS LINE))
	      (SEND SELF :FORCE-KBD-INPUT `(:LINE-AREA ,ITEM ,SELF ,BD)))
	    ;;ELSE
	    (BEEP))))
     (:WHO-LINE-DOCUMENTATION-STRING 
      (SEND SELF :LINE-AREA-MOUSE-DOCUMENTATION))))


(DEFFLAVOR LINE-AREA-MOUSE-SENSITIVE-TEXT-SCROLL-MIXIN ()
	   (LINE-AREA-TEXT-SCROLL-MIXIN BORDERS-MIXIN w:scroll-bar-mixin)
  (:REQUIRED-FLAVORS MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW)
  (:DOCUMENTATION :COMBINATION))

(DEFMETHOD (LINE-AREA-MOUSE-SENSITIVE-TEXT-SCROLL-MIXIN :MOUSE-LEAVES-REGION) ()
  (BLINKER-SET-VISIBILITY ITEM-BLINKER ()))


(DEFFLAVOR CURRENT-ITEM-MIXIN ((CURRENT-ITEM NIL)) ()
  (:REQUIRED-FLAVORS LINE-AREA-TEXT-SCROLL-MIXIN)
  (:GETTABLE-INSTANCE-VARIABLES CURRENT-ITEM)
  (:DOCUMENTATION
    :MIXIN "Provides an arrow in the line-area pointing to current-item."))

(DEFUN UPDATE-CURRENT-ITEM (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR CURRENT-ITEM-MIXIN))
  (LET ((REGION (ASSOC 'LINE-AREA-REGION REGION-LIST :TEST #'EQ))
	(ITEM-NO (SEND SELF :NUMBER-OF-ITEM CURRENT-ITEM)))
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
        (MARGIN-REGION-AREA REGION)
      (MULTIPLE-VALUE-BIND (FIRST-ITEM TOTAL-ITEMS ITEM-HEIGHT)
          (SEND SELF :SCROLL-POSITION)
	(LET ((CURRENT-ITEM-Y (AND ITEM-NO
                                   (<= ITEM-NO TOTAL-ITEMS)     ;Can be 1 off end
                                   (+ (* (- ITEM-NO FIRST-ITEM) ITEM-HEIGHT)
                                      (SHEET-INSIDE-TOP))))
	      (FONT (SCREEN-DEFAULT-FONT (SHEET-GET-SCREEN SELF))))
	  (SHEET-FORCE-ACCESS (SELF)
            (PREPARE-SHEET (SELF)
              (%DRAW-RECTANGLE (- RIGHT LEFT)
                               (- BOTTOM TOP)
                               LEFT TOP ERASE-ALUF SELF)
              (AND CURRENT-ITEM-Y
                   (>= CURRENT-ITEM-Y TOP)
                   (<= (+ CURRENT-ITEM-Y (FONT-CHAR-HEIGHT FONT)) BOTTOM)
                   (DRAW-CHAR FONT #\RIGHT-ARROW
                               (- RIGHT (FONT-CHAR-WIDTH FONT) 1)
                               CURRENT-ITEM-Y CHAR-ALUF SELF)))))))))

(DEFMETHOD (CURRENT-ITEM-MIXIN :SET-CURRENT-ITEM) (NEW-CURRENT-ITEM)
  (COND ((NEQ NEW-CURRENT-ITEM CURRENT-ITEM)
         (SETQ CURRENT-ITEM NEW-CURRENT-ITEM)
         (UPDATE-CURRENT-ITEM))))

(DEFMETHOD (CURRENT-ITEM-MIXIN :AFTER :REFRESH-MARGINS) UPDATE-CURRENT-ITEM)

(DEFMETHOD (CURRENT-ITEM-MIXIN :AFTER :NEW-SCROLL-POSITION) UPDATE-CURRENT-ITEM)

(DEFFLAVOR MULTIPLE-CHOICE ()
           (BORDERS-MIXIN TOP-BOX-LABEL-MIXIN BASIC-MULTIPLE-CHOICE
            WINDOW))

(DEFFLAVOR BASIC-MULTIPLE-CHOICE
	   ((ITEM-NAME NIL)
            (CHOICE-TYPES NIL)
            (MARGIN-CHOICES DEFAULT-FINISHING-CHOICES)
	    (CHOICE-VALUE))
	   (W:SCROLL-BAR-MIXIN
            MARGIN-CHOICE-MIXIN
            DISPLAYED-ITEMS-TEXT-SCROLL-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES ITEM-NAME CHOICE-TYPES)
  (:INIT-KEYWORDS :CHOICES :ITEM-LIST)
  (:REQUIRED-FLAVORS ESSENTIAL-WINDOW)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL :MORE-P NIL :SCROLL-BAR-on-off t))


(DEFSTRUCT (CHOICE-TYPE (:CONSTRUCTOR           NIL)
                        (:CONC-NAME             NIL)
                        (:CALLABLE-CONSTRUCTORS NIL)
                        (:ALTERANT              ALTER-CHOICE-TYPE)
                        (:PREDICATE             NIL)
                        (:COPIER                NIL)
                        (:TYPE                  :LIST))
  CHOICE-TYPE-KEYWORD
  CHOICE-TYPE-NAME
  CHOICE-TYPE-ON-POSITIVE-IMPLICATIONS
  CHOICE-TYPE-ON-NEGATIVE-IMPLICATIONS
  CHOICE-TYPE-OFF-POSITIVE-IMPLICATIONS
  CHOICE-TYPE-OFF-NEGATIVE-IMPLICATIONS)

(DEFSTRUCT (CHOICE-ITEM (:CONSTRUCTOR           NIL)
                        (:CONC-NAME             NIL)
                        (:CALLABLE-CONSTRUCTORS NIL)
                        (:ALTERANT              ALTER-CHOICE-ITEM)
                        (:PREDICATE             NIL)
                        (:COPIER                NIL)
                        (:TYPE                  :LIST))
  CHOICE-ITEM-ITEM
  CHOICE-ITEM-NAME
  CHOICE-ITEM-BOXES)

;;; Reversed Do It and Abort to agree with consistency guides.   5/23/86. KDB
(DEFVAR DEFAULT-FINISHING-CHOICES
	`((,W:MARGIN-CHOICE-ABORT-STRING      NIL MULTIPLE-CHOICE-ABORT NIL NIL)
          (,W:MARGIN-CHOICE-COMPLETION-STRING NIL MULTIPLE-CHOICE-DONE  NIL NIL)))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :AFTER :INIT) (INIT-PLIST &AUX CHOICES)
  (AND (SETQ CHOICES (OR (GET INIT-PLIST :CHOICES)
                         (GET INIT-PLIST :ITEM-LIST)))
       (SEND SELF :SET-CHOICES CHOICES)))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :ADJUSTABLE-SIZE-P) ()
  T)

;;  Temporary hack.  Delete both of these forms when we know that the first 
;;     method definition is no longer around.                                -GRH 9/30/86
;commented out 11/13/87 PMH
;(DEFMETHOD (BASIC-MULTIPLE-CHOICE :ENABLE-SCROLLING-P) () t)  ;;;;;
;(undefmethod (BASIC-MULTIPLE-CHOICE :ENABLE-SCROLLING-P))    ;;;;;;;;

;;; I don't think the user is supposed to call this directly; use :SETUP.
(DEFMETHOD (BASIC-MULTIPLE-CHOICE :SET-CHOICES)
           (NEW-CHOICES &AUX NAME-LENGTH CHOICE-BOXES MAX-X NITEMS NEW-LABEL)
  (DECLARE (VALUES INSIDE-WIDTH INSIDE-HEIGHT NEW-LABEL))
  ;; We need to copy the instance variable to get around a compiler strangeness.
  ;; When invoking the compiler, some times it will put things into read-only
  ;; memory.  If we copy the instance variable contents, then it will go into
  ;; a read/write area of memory so we can change the value.
  (SETQ CHOICE-TYPES (COPY-TREE CHOICE-TYPES))
  ;; Substitute the name of all types where needed
  (LET ((ALLTYPES (MAPCAR 'CAR CHOICE-TYPES)))
    (DOLIST (CHOICE-TYPE CHOICE-TYPES)
      (AND (EQ (CHOICE-TYPE-ON-POSITIVE-IMPLICATIONS  CHOICE-TYPE) T)
	   (ALTER-CHOICE-TYPE CHOICE-TYPE CHOICE-TYPE-ON-POSITIVE-IMPLICATIONS  ALLTYPES))
      (AND (EQ (CHOICE-TYPE-ON-NEGATIVE-IMPLICATIONS  CHOICE-TYPE) T)
	   (ALTER-CHOICE-TYPE CHOICE-TYPE CHOICE-TYPE-ON-NEGATIVE-IMPLICATIONS  ALLTYPES))
      (AND (EQ (CHOICE-TYPE-OFF-POSITIVE-IMPLICATIONS CHOICE-TYPE) T)
	   (ALTER-CHOICE-TYPE CHOICE-TYPE CHOICE-TYPE-OFF-POSITIVE-IMPLICATIONS ALLTYPES))
      (AND (EQ (CHOICE-TYPE-OFF-NEGATIVE-IMPLICATIONS CHOICE-TYPE) T)
	   (ALTER-CHOICE-TYPE CHOICE-TYPE CHOICE-TYPE-OFF-NEGATIVE-IMPLICATIONS ALLTYPES))))
  ;; Now compute the length of the name needed
  (SETQ NITEMS 0
        NAME-LENGTH (IF ITEM-NAME (+  CHAR-WIDTH 
                                     (SHEET-STRING-LENGTH SELF ITEM-NAME)) 0))
  (DOLIST (CHOICE NEW-CHOICES)
    (SETQ NITEMS (1+ NITEMS))
    (AND (CHOICE-ITEM-NAME CHOICE)
	 (SETQ NAME-LENGTH (MAX NAME-LENGTH
                                (+ (SHEET-STRING-LENGTH
                                     SELF
                                     (CHOICE-ITEM-NAME CHOICE))
                                    CHAR-WIDTH)))))  
  ;; Make prototype boxes
  (DO ((X NAME-LENGTH (+ X TYPE-WIDTH))
       (TYPES CHOICE-TYPES (CDR TYPES))
       (TYPE) (TYPE-WIDTH))
      ((NULL TYPES)
       (SETQ MAX-X (+ X CHAR-WIDTH)))
    (SETQ TYPE (CAR TYPES)
          TYPE-WIDTH (+ (SHEET-STRING-LENGTH SELF (CHOICE-TYPE-NAME TYPE))
                        CHAR-WIDTH))
    (PUSH (LIST (CHOICE-TYPE-KEYWORD TYPE) () 'MULTIPLE-CHOICE-CHOOSE
                (+ X (TRUNCATE TYPE-WIDTH 2)) #o177777)
          CHOICE-BOXES))
  (LET ((MAXIMUM-POSSIBLE-MAX-X (- (SHEET-INSIDE-WIDTH SUPERIOR)
                                   (+ LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE
				      (if (eq w:scroll-bar-mode :off)
					  0
					  ;; Assume we may eventually need scrolling
					  ;; Otherwise PMH
					  (w:scroll-bar-area-width self))))))
    (WHEN (> MAX-X MAXIMUM-POSSIBLE-MAX-X)
     ;; This will not fit inside the superior horizontally, so arrange
     ;; to truncate.
      (DOLIST (BOX CHOICE-BOXES)
	(DECF (CHOICE-BOX-X1 BOX) (- MAX-X MAXIMUM-POSSIBLE-MAX-X)))
      (DECF NAME-LENGTH (- MAX-X MAXIMUM-POSSIBLE-MAX-X))
      (SETQ MAX-X MAXIMUM-POSSIBLE-MAX-X)))
  ;; Compute the new label
  (SETQ NEW-LABEL (MAKE-ARRAY (TRUNCATE MAX-X CHAR-WIDTH)
                              :ELEMENT-TYPE 'STRING-CHAR
                              :LEADER-LIST '(0)))
  (AND ITEM-NAME (SETQ NEW-LABEL (STRING-NCONC NEW-LABEL ITEM-NAME)))
  (DO ((I (LENGTH NEW-LABEL) (1+ I))
       (LIM (TRUNCATE NAME-LENGTH CHAR-WIDTH)))
      ((>= I LIM)
       (STORE-ARRAY-LEADER LIM NEW-LABEL 0))
    (SETF (AREF NEW-LABEL I) #\SPACE))
  (DOLIST (CHOICE-TYPE CHOICE-TYPES)
    (SETQ NEW-LABEL (STRING-NCONC NEW-LABEL #\SPACE
                                  (CHOICE-TYPE-NAME CHOICE-TYPE))))
  ;; Now fill in the items
  (AND (> NITEMS (ARRAY-TOTAL-SIZE ITEMS))
       (ADJUST-ARRAY ITEMS NITEMS))
  (STORE-ARRAY-LEADER NITEMS ITEMS 0)
  (DO ((CHOICES NEW-CHOICES (CDR CHOICES))
       (I 0 (1+ I))
       (MAX-NAME-CHARS (TRUNCATE NAME-LENGTH CHAR-WIDTH))
       (CHOICE) (CHOICE-ITEM))
      ((NULL CHOICES))
    (SETQ CHOICE (CAR CHOICES)
          CHOICE-ITEM (LIST (CHOICE-ITEM-ITEM CHOICE)
                            (CHOICE-ITEM-NAME CHOICE) NIL))
    ;; Truncate each item name to fit the space available.
    (IF (> (LENGTH (CHOICE-ITEM-NAME CHOICE-ITEM)) MAX-NAME-CHARS)
	(SETF (CHOICE-ITEM-NAME CHOICE-ITEM) (SUBSEQ (CHOICE-ITEM-NAME CHOICE-ITEM) 0 MAX-NAME-CHARS)))
    ;; Create a set of choice boxes for this item, copied from the prototypes.
    ;; The boxes' x positions are copied from the prototypes
    ;; so the order they are stored in for this choice-item does not matter.
    (DO ((BOXES (CHOICE-ITEM-BOXES CHOICE) (CDR BOXES))
	 (BOX) (TYPE) (INITIAL-STATE))
	((NULL BOXES))
      (SETQ BOX (CAR BOXES))
      (IF (SYMBOLP BOX)
          (SETQ TYPE          BOX
                INITIAL-STATE NIL)
	  (SETQ TYPE          (CHOICE-BOX-NAME  BOX)
                INITIAL-STATE (CHOICE-BOX-STATE BOX)))
      (SETQ BOX (COPY-LIST (ASSOC TYPE CHOICE-BOXES :TEST #'EQ)))
      (SETF (CHOICE-BOX-STATE BOX) INITIAL-STATE)
      (PUSH BOX (CHOICE-ITEM-BOXES CHOICE-ITEM)))
    (SETF (AREF ITEMS I) CHOICE-ITEM))
  ;; Now we return some reasonable sizes
  (BLOCK NIL (RETURN MAX-X (* NITEMS LINE-HEIGHT) NEW-LABEL)))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :SETUP)
           (NEW-ITEM-NAME NEW-CHOICE-TYPES
            NEW-FINISHING-CHOICES NEW-CHOICES
            &OPTIONAL (MAXLINES 20.) &AUX WID HGT LBL)
  (SETQ ITEM-NAME NEW-ITEM-NAME
        CHOICE-TYPES NEW-CHOICE-TYPES)
  (MULTIPLE-VALUE-SETQ (WID HGT LBL)
    (SEND SELF :SEt-CHOICES NEW-CHOICES))
  (SETQ TOP-ITEM 0)                             ;Un-scroll
  (SEND SELF :SET-LABEL (LIST :STRING LBL
                              :FONT CURRENT-FONT))
  (send self :change-of-size-or-margins
	:height (min (sheet-height superior)
		     (+ (MIN (* MAXLINES LINE-HEIGHT) HGT)
			top-margin-size bottom-margin-size))
	:width (min (sheet-width superior)
		    (+ wid left-margin-size right-margin-size)))
  (SEND SELF :SET-MARGIN-CHOICES NEW-FINISHING-CHOICES)
  (SHEET-FORCE-ACCESS (SELF T) (SEND SELF :REFRESH)))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :PRINT-ITEM) (ITEM LINE-NO ITEM-NO)
  ITEM-NO                                       ;Not used
  (SHEET-STRING-OUT SELF (CHOICE-ITEM-NAME ITEM))
  (DOLIST (BOX (CHOICE-ITEM-BOXES ITEM))
    (SETF (CHOICE-BOX-X2 BOX)
	  (DRAW-CHOICE-BOX SELF (CHOICE-BOX-X1 BOX) CURSOR-Y
                           (CHOICE-BOX-STATE BOX))))
  (SETF (AREF DISPLAYED-ITEMS LINE-NO) ITEM))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :MOUSE-CLICK) (ignore X Y &AUX LINE-NO ITEM) ;; button arg replaced by Ignore, 9/05/86
;;;  (COND ((OR (= BUTTON #\MOUSE-L) (= BUTTON #\MOUSE-M) (= BUTTON #\MOUSE-R)) ;; TEST REMOVED. NOW ANY CLICK REALLY WORKS.
         (SETQ LINE-NO (SHEET-LINE-NO () Y))
         (COND ((AND (>= Y (SHEET-INSIDE-TOP))
                     (< Y (+ (SHEET-INSIDE-TOP)
                             (* (SHEET-NUMBER-OF-INSIDE-LINES)
                                LINE-HEIGHT)))
                     (SETQ ITEM (AREF DISPLAYED-ITEMS LINE-NO)))
                (HANDLE-CHOICE-BUTTON (CHOICE-ITEM-BOXES ITEM) X Y ITEM)
                T)))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :AFTER :MOUSE-MOVES)
           (X Y &REST IGNORE &AUX ITEM)
  "Make mouse-cursor an X when over a choice box."
  (WHEN (AND (>= Y (SHEET-INSIDE-TOP))
	(< Y (+ (SHEET-INSIDE-TOP) (* (SHEET-NUMBER-OF-INSIDE-LINES)
                                      LINE-HEIGHT))))
    (IF (AND (SETQ ITEM (AREF DISPLAYED-ITEMS (SHEET-LINE-NO () Y)))
	  (DOLIST (BOX (CHOICE-ITEM-BOXES ITEM))
	    (AND (>= X (CHOICE-BOX-X1 BOX))
                 (<  X (CHOICE-BOX-X2 BOX))
                 (RETURN T))))
     (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 4 5 :ON :SET-CHARACTER MOUSE-GLYPH-THICK-CROSS      'FONTS:MOUSE)
     (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 0 0 :ON :SET-CHARACTER MOUSE-GLYPH-NORTH-WEST-ARROW 'FONTS:MOUSE))))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :WHO-LINE-DOCUMENTATION-STRING) ()
  "Any button on a box turns it on or off.")

(DEFUN MULTIPLE-CHOICE-CHOOSE (BOX ITEM Y)
  (DECLARE (:SELF-FLAVOR BASIC-MULTIPLE-CHOICE))
  (SETQ Y (+ (SHEET-INSIDE-TOP) (* (SHEET-LINE-NO () Y) LINE-HEIGHT)))
  (SEND SELF :SET-ITEM-BOX-STATE ITEM Y (CHOICE-BOX-NAME BOX)
        (NOT (CHOICE-BOX-STATE BOX))))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :SET-ITEM-BOX-STATE)
           (ITEM Y KEYWORD NEW-STATE &AUX BOX TYP)
  (COND ((AND (SETQ BOX (ASSOC KEYWORD (CHOICE-ITEM-BOXES ITEM) :TEST #'EQ))
              (NEQ NEW-STATE (CHOICE-BOX-STATE BOX)))
         (SETF (CHOICE-BOX-STATE BOX) NEW-STATE)
         (AND Y (DRAW-CHOICE-BOX SELF (CHOICE-BOX-X1 BOX) Y NEW-STATE))
         (SETQ TYP (ASSOC KEYWORD CHOICE-TYPES :TEST #'EQ))
         (DOLIST (POS (IF NEW-STATE (CHOICE-TYPE-ON-POSITIVE-IMPLICATIONS TYP)
                          (CHOICE-TYPE-OFF-POSITIVE-IMPLICATIONS TYP)))
           (OR (EQ POS KEYWORD)
               (SEND SELF :SET-ITEM-BOX-STATE ITEM Y POS T)))
         (DOLIST (NEG (IF NEW-STATE (CHOICE-TYPE-ON-NEGATIVE-IMPLICATIONS TYP)
                          (CHOICE-TYPE-OFF-NEGATIVE-IMPLICATIONS TYP)))
           (OR (EQ NEG KEYWORD)
               (SEND SELF :SET-ITEM-BOX-STATE ITEM Y NEG NIL))))))

(DEFMETHOD (BASIC-MULTIPLE-CHOICE :CHOOSE)
           (&OPTIONAL (NEAR-MODE '(:MOUSE)) &AUX OLD-STATUS)
  (SETQ CHOICE-VALUE NIL)
  (SETQ OLD-STATUS (SEND SELF :STATUS))
  (UNWIND-PROTECT
      (PROGN
        (EXPOSE-WINDOW-NEAR SELF NEAR-MODE)
        (PROCESS-WAIT "Choose" #'CAR (LOCATE-IN-INSTANCE SELF 'CHOICE-VALUE)))
    (SEND SELF :SET-STATUS OLD-STATUS))
  (IF (CONSP CHOICE-VALUE)
      CHOICE-VALUE
      ;;ELSE
      (VALUES NIL CHOICE-VALUE)))

(DEFUN MULTIPLE-CHOICE-DONE (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-MULTIPLE-CHOICE))
  (SETQ CHOICE-VALUE
	(DO ((I 0 (1+ I))
	     (LIM (ARRAY-ACTIVE-LENGTH ITEMS))
	     (ITEM) (RET NIL))
	    ((>= I LIM) (NREVERSE RET))
	  (SETQ ITEM (AREF ITEMS I))
	  (PUSH (CONS (CHOICE-ITEM-ITEM ITEM)
                      (DO ((BOXES (CHOICE-ITEM-BOXES ITEM) (CDR BOXES))
                           (BOX) (RET NIL))
                          ((NULL BOXES) (NREVERSE RET))
                        (AND (CHOICE-BOX-STATE (SETQ BOX (CAR BOXES)))
                             (PUSH (CHOICE-BOX-NAME BOX) RET))))
                RET))))

(DEFUN MULTIPLE-CHOICE-ABORT (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-MULTIPLE-CHOICE))
  (SETQ CHOICE-VALUE :ABORT))

(DEFFLAVOR TEMPORARY-MULTIPLE-CHOICE-WINDOW ()
	   (TEMPORARY-SHADOW-BORDERS-WINDOW-MIXIN MULTIPLE-CHOICE))

(DEFMETHOD (TEMPORARY-MULTIPLE-CHOICE-WINDOW :AFTER :DEEXPOSE) (&REST IGNORE)
  (OR CHOICE-VALUE (SETQ CHOICE-VALUE :ABORT)))


(DEFWINDOW-RESOURCE TEMPORARY-MULTIPLE-CHOICE-WINDOW ()
  :MAKE-WINDOW (TEMPORARY-MULTIPLE-CHOICE-WINDOW
                 :FONT-MAP (FILL (MAKE-ARRAY 26. :LEADER-LENGTH 1) FONTS:MEDFNT)
		 :foreground-color *default-menu-foreground*
		 :background-color *default-menu-background*)
  :REUSABLE-WHEN :DEACTIVATED
  :INITIAL-COPIES 0)

(DEFUN MULTIPLE-CHOOSE (ITEM-NAME ITEM-LIST KEYWORD-ALIST
                        &OPTIONAL (NEAR-MODE '(:MOUSE)) (MAXLINES 20.) SUP)
  "Ask several multiple-choice questions with a menu-like window.

ITEM-NAME	string of the name of the type of item, e.g. BUFFER.
		It goes in the label, above the item names.
ITEM-LIST	alist, in which each element is (ITEM NAME CHOICES).
	ITEM is the item itself
	NAME is the name of the item (a string)
	CHOICES a list of possible keywords, either (KEYWORD) or
	(KEYWORD DEFAULT), where if DEFAULT is non-NIL the
	KEYWORD is initially on.  The purpose of ITEM is that it
		appears in the value returned, and allows you to
		identify what the returned answers apply to.
KEYWORD-ALIST list of the possible keywords,
	(KEYWORD NAME IMPLICATIONS).
	KEYWORD is a symbol, the same as in ITEM-LIST's CHOICES.
	NAME is a string of its name.  IMPLICATIONS is a list of
		on-positive, on-negative, off-positive, and
		off-negative implications for when the keyword is
		selected, each one either a list of (other) keywords
		or T for all other keywords.  The default for
		IMPLICATIONS is (NIL T NIL NIL).

We return two values: first, the list of choices chosen; second, the
reason why we exited (NIL means normal exit).  Each element of the
first value is a list (ITEM SELECTED-CHOICES...)."
  (DECLARE (VALUES CHOICES EXIT-REASON))
  ;; Decide what superior to use.
  (OR SUP
      (SETQ SUP (IF (EQ (CAR NEAR-MODE) :WINDOW)
                    (SHEET-SUPERIOR (CADR NEAR-MODE))
                    ;;ELSE
		    MOUSE-SHEET)))
  ;; Create another KEYWORD-ALIST with the implications filled out.
  (SETQ KEYWORD-ALIST (LOOP FOR L IN KEYWORD-ALIST
                            COLLECTING
                            (IF (< (LENGTH L) 3)
                                (APPEND L (LIST () T () ()))
                                ;;ELSE
                                L)))
  (USING-RESOURCE (WINDOW TEMPORARY-MULTIPLE-CHOICE-WINDOW SUP)
    (FUNCALL WINDOW :SETUP ITEM-NAME KEYWORD-ALIST
             DEFAULT-FINISHING-CHOICES ITEM-LIST MAXLINES)
    (UNWIND-PROTECT
        (FUNCALL WINDOW :CHOOSE NEAR-MODE)
      (FUNCALL WINDOW :DEACTIVATE))))

;;; Fixed up comment string to refer to dtp-locative instead of locative.
;;; Fixes bug report 758 from JEWELL.  Duplicate of window patch 1-1.  I
;;; don't understand why the source didn't get updated.

;;; Choose-variable-values stuff.
;;; Basic idea is that the program has a list of special variables, and
;;; the user is asked to confirm and possibly modify their values.  Values
;;; can be either general expressions, or a choice from a list (menu like).
;;;
;;; The printing of the display is not actually done in the user's stack
;;; group, but it acts as if it were.  The reading of new values is done
;;; in the user's stack group.  Thus you can bind BASE, PRINLEVEL,
;;; READTABLE, etc.

;;; The user can point at a displayed value and click the mouse, to
;;; modify it.  The new value is input from the keyboard;
;;; over-rubbing-out restores the old value.  For values chosen from a
;;; list, clicking the mouse selects the value pointed-to.

;;; VARIABLES is a list of elements, each describing one line of the
;;; display These become text-scroll items.  Kinds of elements allowed
;;; are:
;;;  string - just displayed
;;;  special-variable - value is printed, and if the user clicks on it
;;;		with the mouse a new value is read.
;;; dtp-locative - like special-variable but value is accessed by car
;;;		and written by RPLACA.
;;; Otherwise a list whose car is the variable, optionally followed by a
;;; string to use as label instead of the var, or nil for no label,
;;; followed by a keyword for the type of variable, followed by args to
;;; the keyword.  The default keyword is :SEXP
;;;
;;; Keywords are:
;;;    :SEXP, :LIST or :ANY - value of variable is a Lisp S-expression, printed
;;;		with PRIN1, read in with READ
;;;    :ASSOC values-list print-function - like :CHOOSE, but car of
;;;		values-list element is displayed, cdr is variable-value
;;;    :BOOLEAN - value is T or NIL, but displays as Yes or No
;;;    :CHARACTER - value is a character, prints with ~:@C, reads as one
;;;		keystroke.
;;;    :CHARACTER-OR-NIL - same but can also be NIL, displays as "none",
;;;		inputs as CLEAR.
;;;    :CHOOSE values-list print-function - value of variable is one of the
;;;		elements of values-list (EQUAL testing is used).  Printed
;;;		by printing all the value choices, with the current one in
;;;		boldface, read in by the user pointing and clicking.
;;;		print-function is optional and defaults to PRINC.
;;;    :DATE - a universal date-time.  It is printed with time:print-universal-time
;;;             and read by readline-trim and time:parse-universal-time.
;;;    :DATE-OR-NEVER - same as :DATE except that NIL is accepted as input and is
;;;             printed as never.
;;;    :FIXNUM - allow a fixnum only.
;;;    :FIXNUM-OR-NIL - allow a fixnum or nil only.
;;;    :INTERVAL-OR-NEVER - either NIL or a number of seconds.  Read with
;;;             time:read-interval-or-never and printed with
;;;             time:print-interval-or-never.
;;;    :LIST-OF - the next argument is another variable type with its arguments,
;;;             if any (e.g. :typep (integer 0 20)).  The value entered by the
;;;             user is a list of values of that variable type
;;;             (e.g. 0 <= fixnum-value <= 20) separated by commas.
;;;    :MENU - use this instead of :CHOOSE when there are a lot of choices.
;;;		The parameter that follows this keyword are menu items as
;;;		are passed to W:MENU-CHOOSE to bring up a menu.  Note that
;;;		is a constant is present then it must be quoted, since it
;;;		is evaluated.
;;;    :MENU-ALIST - a combination of :MENU and :CHOOSE in that the the
;;;             argument is an item list (not quoted) and what is displayed
;;;             looks like :CHOOSE.
;;;    :MULTIPLE-MENU - same as :MENU except that more than one value can
;;;             be selected.
;;;    :NON-NEGATIVE-FIXNUM - a fixnum that is >= 0.
;;;    :NUMBER - print with PRIN1, read with READ but must be a number
;;;    :NUMBER-OR-NIL - same as :NUMBER, but allow nil too.
;;;    :PATHNAME - if an argument is supplied that supplies defaults for the
;;;             pathname.
;;;    :PATHNAME-LIST - a list of pathnames
;;;    :PATHNAME-OR-NIL - same as :PATHNAME except NIL is allowed, and prints
;;;             as a blank line
;;;    :POSITIVE-FIXNUM - a fixnum that is > 0.
;;;    :TEST - the next argument is a function (e.g. functionp) of
;;;             one parameter which is called to validate the value entered
;;;             by the user.
;;;    :PRINC - same as :SEXP but print it with PRINC instead of PRIN1
;;;    :SET - like :CHOOSE except that more than one element can be
;;;		selected.  Returns a list of the items selected.
;;;    :SMALL-FRACTION - allow a number x, where 0 < x <= 1.
;;;    :STRING - print with PRINC, read with READ-STRING
;;;    :STRING-OR-NIL - same as :STRING except that NIL is ok.
;;;    :TYPEP - same as :TEST except that the next argument is a type
;;;             (e.g. character).
;;;
;;; Item list modifiers appear where the keyword is expected.  After the
;;; parameters of the item list modifier comes the keyword for the item
;;; itself.  The following are the valid item list modifiers:
;;;    :CONSTRAINT - The parameters are a function and a string.  The
;;;		function applied to the new value, and it returns nil
;;;		then the string is displayed.
;;;    :DOCUMENTATION followed by a string to display when the mouse is
;;;		pointing here.
;;;    :EDIT - puts the old value in the rubout-handler buffer when the
;;;		item is clicked on.  Without this, the old item value
;;;		disappears so that you can not edit it.
;;;    :QUOTE - the returned value is quoted.  This is useful for making
;;;               parameter lists.
;;;    :SIDE-EFFECT - the parameter is either a list or a function.  The
;;;		parameter is either evaluated or FUNCALLed whenever the
;;;		item value is changed.  If the parameter is a function,
;;;		then it is called with 4 arguments: window, variable,
;;;		old-value and new-value.  This is similar to the
;;;		:FUNCTION window option, but this one allows a different
;;;		function to go with each item.
;;;
;;; This is implemented by :DECODE-VARIABLE-TYPE (see below) so that you can
;;; change it.
;;;
;;; Should there also be ones which are constrained to be lists of
;;; chars?  Keywords automatically forced into the keyword package?
;;; Should there be a provision for documentation of a variable, and a
;;; way to make that print somewhere?  (As in ZMACS Alter Options.)
;;;
;;; The :DECODE-VARIABLE-TYPE message to the window is used to look at
;;; the keyword and options and return information about how to print
;;; and change the variable's value.  The argument to this message is
;;; the tail of a VARIABLES element starting with the keyword, and it
;;; returns 6 values:
;;;  The print function (args are object and stream).
;;;  The read function, or NIL if it works by pointing (arg is stream).
;;;    Crockishness: usually this is called inside a rubout-handler, with
;;;    the feature supplied that over-rubout causes the variable to left
;;;    at its old value.  But with a list here the car of the list is the
;;;    function which just gets called directly.
;;;  The choices to be printed (NIL if just print current value).
;;;  The function which translates a value to its printed form (NIL for
;;;    identity).
;;;  The function which translates a value to the form
;;;    which goes in the variable (NIL for identity).
;;; The who-line mouse documentation string.  If this is a symbol, then
;;;    NIL means use the default documentation, and any other symbol is the
;;;    name of a function which translates a value to its documentation.
;;; The two functions only apply when there are choices.  The default
;;; handler looks for a TV:CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION
;;; property which is a function to call or a
;;; TV:CHOOSE-VARIABLE-VALUES-KEYWORD property which is
;;; (print-func read-func choices ptransfn vtransfn mouse-documentation)

;;; FUNCTION can be NIL or a function called on window,
;;; special-variable, old-value, new-value when a variable is changed.
;;; It may make other changes.  Returns T if it did its own redisplay
;;; (typically by sending a :SET-VARIABLES), NIL if that variable's new
;;; value needs to be displayed.  Typically this function implements
;;; constraints among the variable values and sends a refresh message
;;; and returns T.
;;;
;;; STACK-GROUP is the stack-group in which the variables may be evaluated.
;;;
;;; Height of window is chosen automatically upon creation if not
;;; specified in the init-plist.  Also is automatically adjustable if
;;; you send a :SET-VARIABLES.
;;;
;;; The following messages can come back through the io-buffer:
;;;  (:CHOICE-BOX window box)
;;;  (:VARIABLE-CHOICE window VARIABLES-element value line-no)
;;;
;;; Font-map:
;;;  0  string
;;;  1  name
;;;  2  value
;;;  3  unselected-choice
;;;  4  selected-choice

(DEFVAR *DEFAULT-WHO-LINE-DOCUMENTATION*
        '(:DOCUMENTATION "Press the HELP key for command descriptions."
                         :MOUSE-L-1 "move to an item and select it" :MOUSE-R-1 "move to an item and edit it.")
  "Default documentation for Choose Variable Values when the mouse is off an item.")

(DEFFLAVOR BASIC-CHOOSE-VARIABLE-VALUES
	   ((FUNCTION NIL)
            STACK-GROUP
            (LINE-OVERFLOW-ALLOWED T)
            (DEFAULT-WHO-LINE-DOCUMENTATION *DEFAULT-WHO-LINE-DOCUMENTATION*)
            (RECURSION NIL)
	    (COMMAND-CHARACTERS NIL)
            (VALUE-TAB T))
	   (ANY-TYI-MIXIN MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW-WITHOUT-CLICK)
  (:INITTABLE-INSTANCE-VARIABLES DEFAULT-WHO-LINE-DOCUMENTATION)
  (:REQUIRED-FLAVORS STREAM-MIXIN)
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  (:INIT-KEYWORDS :VARIABLES :NAME-FONT :VALUE-FONT :STRING-FONT
                  :UNSELECTED-CHOICE-FONT :SELECTED-CHOICE-FONT)
  (:DEFAULT-INIT-PLIST
    :SAVE-BITS NIL
    :CHARACTER-WIDTH 50.
    :BLINKER-P '(:VISIBILITY NIL)
    :BLINKER-DESELECTED-VISIBILITY :ON
    :NAME-FONT              *CHOOSE-VARIABLE-VALUES-NAME-STANDARD-FONT*
    :VALUE-FONT             *CHOOSE-VARIABLE-VALUES-VALUE-STANDARD-FONT*
    :STRING-FONT            *CHOOSE-VARIABLE-VALUES-STRING-STANDARD-FONT*
    :UNSELECTED-CHOICE-FONT *CHOOSE-VARIABLE-VALUES-UNSELECTED-CHOICE-STANDARD-FONT*
    :SELECTED-CHOICE-FONT   *CHOOSE-VARIABLE-VALUES-SELECTED-STANDARD-FONT*)
  (:DOCUMENTATION :ESSENTIAL-MIXIN "Implement the multiple-choice facility."))

(DEFFLAVOR CHOOSE-VARIABLE-VALUES-WINDOW
           ()
	   (BASIC-CHOOSE-VARIABLE-VALUES
	    BORDERS-MIXIN
	    w:scroll-bar-mixin
	    TOP-BOX-LABEL-MIXIN  
	    MARGIN-CHOICE-MIXIN
            ANY-TYI-MIXIN
	    WINDOW)
  (:DEFAULT-INIT-PLIST
    :MARGIN-CHOICES `((,W:MARGIN-CHOICE-COMPLETION-STRING NIL CHOOSE-VARIABLE-VALUES-CHOICE-BOX-HANDLER NIL NIL))
    :scroll-bar-on-off t
    ))

(DEFUN CHOOSE-VARIABLE-VALUES-CHOICE-BOX-HANDLER (BOX REGION YPOS)
  REGION YPOS                                   ;ignored
  (SEND SELF :FORCE-KBD-INPUT `(:CHOICE-BOX ,SELF ,BOX)))

;;; I don't know if this function's list of options is up to date...
(DEFUN HEIGHT-SPECIFIED-IN-INIT-PLIST (PLIST)
  "Returns T if the PLIST contains anything that specifies the window height"
  (OR (GETL PLIST '(:EDGES-FROM :EDGES :HEIGHT :CHARACTER-HEIGHT))
      (AND (GETL PLIST '(:TOP :Y)) (GET PLIST :BOTTOM))))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :BEFORE :INIT) (PLIST)
  ;; Default the height according to the number of variables, unless
  ;; it was specified explicitly.
  (OR (HEIGHT-SPECIFIED-IN-INIT-PLIST PLIST)
      (SETF (GET PLIST :CHARACTER-HEIGHT) (MAX (MIN (LENGTH (GET PLIST :VARIABLES)) 25.) 1)))
  ;; Set up font map according to fonts specified by name
  (SETQ FONT-MAP (LIST (GET PLIST :STRING-FONT)
                       (GET PLIST :NAME-FONT)
                       (GET PLIST :VALUE-FONT)
                       (GET PLIST :UNSELECTED-CHOICE-FONT)
                       (GET PLIST :SELECTED-CHOICE-FONT))))

;;; This sets the variables and adjusts the scrolling but never changes the height
;;; which was set either by the before-init method or by the creator.
;;; Except that the outside height may be changed to preserve what the creator
;;; is thought to have specified as the inside height.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :AFTER :INIT) (PLIST &AUX ELEMS)
  (AND (SETQ ELEMS (GET PLIST :VARIABLES))
       (SEND SELF :SET-VARIABLES ELEMS T)))

;;; Default is that size adjusts according to the number of items present,
;;; provided that the window is de-exposed.  This is because if it was
;;; exposed the user would see it spastically redisplay several times.
;;; Also it probably looks very bad for it to change size while it's exposed.
;;; You are welcome to redefine this method.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :ADJUSTABLE-SIZE-P) ()
  (NOT EXPOSED-P))

(DEFUN RPLNTH (N LIST VALUE)
  "Replace the Nth car of list with value"
  (SETF (NTH N LIST) VALUE))

(DEFUN STRING-STRIP-CHAR (CHAR STRING &AUX MATCH-INDEX)
  "Strip all occurances of a character from a string."
  ;; Note: This is real slow for long strings with more than one match.
  (LOOP
    (IF (NULL (SETQ MATCH-INDEX (POSITION CHAR (THE STRING (STRING STRING)) :TEST #'CHAR-EQUAL)))
        (RETURN ()))
    (SETQ STRING (STRING-APPEND (SUBSEQ STRING 0 MATCH-INDEX)
                                (SUBSEQ STRING (1+ MATCH-INDEX)))))
  STRING)

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :SET-VARIABLES)
           (ELEMS  &OPTIONAL NO-SET-HEIGHT
	    ;; enable resizing as default behavior  PMH
	    (-WIDTH- (not NO-SET-HEIGHT))	
	    EXTRA-WIDTH
	    ;; take sensible default in case of resizing PMH
	    EXTRA-MARGIN-CHOICE-WIDTH 	
            &AUX (NELEM (LENGTH ELEMS)) TEMP margin-choices)
  (unless EXTRA-MARGIN-CHOICE-WIDTH		;if this flavor has margins mixed in
      (setf extra-margin-choice-width		;better make sure we get the width right PMH
	    (if (setq margin-choices (send self :send-if-handles :margin-choices))
		(let ((between-item-spaces 60))
		  (+ (CHOICE-ITEM-LAYOUT margin-choices)
		     1 3  ;; FUDGE FOR LEFT AND RIGHT BORDERS
		     (* between-item-spaces 
			(- (LENGTH  MARGIN-CHOICES) 1))))
		0)))
  (SETQ TOP-ITEM 0)                             ;Unscroll
  (AND (< (ARRAY-TOTAL-SIZE ITEMS) NELEM)
       (SETQ ITEMS (ADJUST-ARRAY ITEMS NELEM)))
  (STORE-ARRAY-LEADER 0 ITEMS 0)
  (DOLIST (ELEM ELEMS)
   ;; Remove all #\RETURNs from strings.  If we don't then the display
   ;; will not look right -- lines will overprint.
    (IF (STRINGP ELEM)
        (SETQ ELEM (STRING-STRIP-CHAR #\RETURN ELEM))
        ;;ELSE
	(IF (AND (CONSP ELEM) (STRINGP (CADR ELEM)))
	    (SETQ ELEM (CONS (CAR ELEM)
                             (CONS (STRING-STRIP-CHAR #\RETURN (CADR ELEM))
                                   (CDDR ELEM))))))
    (COND ((AND (CONSP ELEM)
		(or (SYMBOLP (CAR ELEM)) (locativep (car elem)))
		(CONSP (CADR ELEM)))  ; If a table
           (LET ((LABEL "  ")
		 (VALUE (IF (SYMBOLP (CAR ELEM))	;generalize symbol vs symbol
			    (SYMBOL-VALUE (CAR ELEM))   ; PMH 3/29/88
			    (CONTENTS (CAR ELEM)))))
						; Make up a label for the columns
             (DOLIST (ITEM (CDR ELEM))
               (SETQ LABEL (STRING-APPEND LABEL (SECOND ITEM) "  ")))
             (VECTOR-PUSH-EXTEND LABEL ITEMS)
	     (DOLIST (ROW VALUE)		; Make an item for each row in the table
	       (VECTOR-PUSH-EXTEND (LOOP FOR ENTRY IN (CDR ELEM)
					 COLLECT (CONS (LOCF (NTH (CAR ENTRY) ROW)) (CDR ENTRY)))
				   ITEMS))))
          ((AND (CONSP ELEM) (SETQ TEMP (MEMBER :DEFAULT ELEM :TEST #'EQ)))     ;Set default values
           (SETQ TEMP (SI::EVAL1 (SECOND TEMP)))
           (IF (SYMBOLP (CAR ELEM))
               (SET    (CAR ELEM) TEMP)
               (RPLACA (CAR ELEM) TEMP))
           (VECTOR-PUSH-EXTEND ELEM ITEMS))
          (T (VECTOR-PUSH-EXTEND ELEM ITEMS))))
  ;; -WIDTH- can be a string, a number of chars, or T meaning look at the variable specs.
  (when cvv-debug
    (send cold-load-stream :clear-eol)
    (send cold-load-stream :string-out "In :set-variables, -width-="))
  (COND ((STRINGP -WIDTH-)
         (SETQ -WIDTH- (MAX (FUNCALL SELF :APPROPRIATE-WIDTH EXTRA-WIDTH)
			    (FUNCALL SELF :STRING-LENGTH -WIDTH-) EXTRA-MARGIN-CHOICE-WIDTH))
	 (when cvv-debug
	   (send cold-load-stream :string-out (format nil "~d from :string-length" -width-))))
        ((NUMBERP -WIDTH-)
         (SETQ -WIDTH- (MAX (FUNCALL SELF :APPROPRIATE-WIDTH EXTRA-WIDTH)
			    (* (SHEET-CHAR-WIDTH SELF) -WIDTH-) EXTRA-MARGIN-CHOICE-WIDTH))
	 (when cvv-debug
	   (send cold-load-stream :string-out (format nil "~d from numeric calculation" -width-))))
        ((EQ -WIDTH- T)
         (SETQ -WIDTH- (MAX (FUNCALL SELF :APPROPRIATE-WIDTH EXTRA-WIDTH)
			    EXTRA-MARGIN-CHOICE-WIDTH)))) ;; DON'T LET MARGIN-CHOICES OVERLAP
  (SETQ NELEM (LENGTH ITEMS))
  (LET ((DESIRED-HEIGHT (* (MIN 25. NELEM) LINE-HEIGHT)))
    (WHEN (AND (NOT NO-SET-HEIGHT)
               -WIDTH-
               (OR (NOT (= (SHEET-INSIDE-HEIGHT) DESIRED-HEIGHT))       ;IF either one has changed THEN
                   (NOT (= (SHEET-INSIDE-WIDTH) -WIDTH-)))
               (SEND SELF :ADJUSTABLE-SIZE-P)
               (<= (+ DESIRED-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)
                   (SHEET-INSIDE-HEIGHT SUPERIOR)))
      (SEND SELF :SET-INSIDE-SIZE (OR -WIDTH- (SHEET-INSIDE-WIDTH)) DESIRED-HEIGHT)
      (when cvv-debug
	(send cold-load-stream :string-out (format nil "~%Changed size in :set-variables to ~d ~d"
						   (OR -WIDTH- (SHEET-INSIDE-WIDTH)) DESIRED-HEIGHT)))
      ))
  (SEND SELF :SET-ITEMS ITEMS t)
  (unless (SEND SELF :DECIDE-IF-SCROLLING-NECESSARY)
      (SEND SELF :SET-ITEMS ITEMS))
  )

(DEFUN CHOICE-ITEM-LAYOUT (CHOICES &AUX TOTAL-STRING-AND-BOX-WIDTH )
  "RETURN THE TOTAL PIXEL WIDTH OF MARGIN CHOICE STRINGS, BOXES AND SPACE BETWEEN THEM"
  (LET ((BOX-AND-SPACE (+ (CHOICE-BOX-WIDTH SELF)
			  (TRUNCATE (* (SHEET-CHAR-WIDTH SELF) BOX-WIDTH-FACTOR)))))
  (SETQ TOTAL-STRING-AND-BOX-WIDTH (LOOP FOR CHOICE-STRING IN CHOICES  ;; STRING LENGTHS 
					    SUMMING (+ (SHEET-STRING-LENGTH SELF (string (CAR CHOICE-STRING)))
						       BOX-AND-SPACE )) ;; SPACE BETWEEN STRING AND CHOICE BOX.
				      )))

;; NEW VERSION TO CORRECTLY CALCULATE MARGIN-CHOICE WIDTHS.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :SETUP) (ELEMS NEW-LABEL NEW-FUNCTION
                                                  NEW-MARGIN-CHOICES
                                                  &OPTIONAL (-WIDTH- t) ;; may 7-1-88 made default t in/of nil
						  EXTRA-WIDTH &AUX EXTRA-MARGIN-CHOICE-WIDTH )
#+ELROY  (DECLARE (SPECIAL EH:*CURRENT-STACK-GROUP*))
  (SETQ FUNCTION NEW-FUNCTION)
  ;; SAVE THE CURRENT STACK AWAY SO THAT WE CAN LOOK AT AND CHANGE THE
  ;; VALUES OF SYMBOLS LOCATED THERE.
  (SETQ STACK-GROUP 
#-ELROY        CURRENT-STACK-GROUP
#+ELROY        EH:*CURRENT-STACK-GROUP*)
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) CURRENT-PROCESS)      ;KLUDGE
  (SEND SELF :SET-LABEL NEW-LABEL)
  (SEND SELF :SET-MARGIN-CHOICES NEW-MARGIN-CHOICES)
  (LET ((BETWEEN-ITEM-SPACE 60)) ;; default
    (SETQ EXTRA-MARGIN-CHOICE-WIDTH (+ (CHOICE-ITEM-LAYOUT NEW-MARGIN-CHOICES)
					  1 3  ;; FUDGE FOR LEFT AND RIGHT BORDERS
					  (* between-item-space 
					     (- (LENGTH  NEW-MARGIN-CHOICES) 1))))
       (SEND SELF :SET-VARIABLES ELEMS NIL -WIDTH- EXTRA-WIDTH EXTRA-MARGIN-CHOICE-WIDTH )))


(SETF (GET :SEXP   'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRIN1 READ-SEXP))
(SETF (GET :ANY    'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRIN1 READ-SEXP))
(SETF (GET :PRINC  'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRINC READ-SEXP))
(SETF (GET :STRING 'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRINC READ-STRING))

(DEFUN READ-SEXP (STREAM)
  "Read in an s-expression and return it."
  ;; The following code was taken from the READ-STRING function.
  (LET* ((STRING-VALUE (MAKE-ARRAY '(512.) :ELEMENT-TYPE 'STRING-CHAR))
	 (LAST-INDEX (SEND STREAM :STRING-LINE-IN NIL STRING-VALUE)))
    (READ-FROM-STRING (SUBSEQ STRING-VALUE 0 LAST-INDEX) NIL NIL))) 	;patched on 11 Dec 85 for TWE by GSM

(SETF (GET :CHOOSE 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) 'CHOOSE-VARIABLE-VALUES-DECODE-CHOOSE)
(SETF (GET :ASSOC  'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) 'CHOOSE-VARIABLE-VALUES-DECODE-CHOOSE)
(DEFUN CHOOSE-VARIABLE-VALUES-DECODE-CHOOSE (KWD-AND-ARGS)
  (when cvv-debug
        (send si:cold-load-stream :line-out
              (format nil "In CVV-decode-choose, returning: ~a NIL ~a ~a ~a"
                      (OR (THIRD KWD-AND-ARGS) 'PRINC)
                      (SECOND KWD-AND-ARGS)
                      (AND (EQ (FIRST KWD-AND-ARGS) :ASSOC) 'CAR)
                      (AND (EQ (FIRST KWD-AND-ARGS) :ASSOC) 'CDR))))
  (VALUES (OR (THIRD KWD-AND-ARGS) 'PRINC)
          NIL
          (SECOND KWD-AND-ARGS)
	  (AND (EQ (FIRST KWD-AND-ARGS) :ASSOC) 'CAR)
	  (AND (EQ (FIRST KWD-AND-ARGS) :ASSOC) 'CDR)))

(SETF (GET :BOOLEAN 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(CHOOSE-VARIABLE-VALUES-BOOLEAN-PRINT NIL (T NIL)))

(DEFUN CHOOSE-VARIABLE-VALUES-BOOLEAN-PRINT (VALUE STREAM)
  (FUNCALL STREAM :STRING-OUT (IF VALUE "Yes" "No")))

(SETF (GET :CHARACTER 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(CHOOSE-VARIABLE-VALUES-CHARACTER-PRINT READ-Char NIL
					       NIL NIL "Click left to input a new character from the keyboard."))

(DEFUN CHOOSE-VARIABLE-VALUES-CHARACTER-PRINT (VALUE STREAM)
  (FORMAT STREAM "~:@C" VALUE))

(SETF (GET :CHARACTER-OR-NIL 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(CHOOSE-VARIABLE-VALUES-CHARACTER-OR-NIL-PRINT
	 CHOOSE-VARIABLE-VALUES-CHARACTER-OR-NIL-READ
	 NIL NIL NIL "Click left to input a new character from the keyboard."))
(DEFUN CHOOSE-VARIABLE-VALUES-CHARACTER-OR-NIL-PRINT (VALUE STREAM)
  (FORMAT STREAM (IF VALUE "~:@C" "none") VALUE))

(DEFUN CHOOSE-VARIABLE-VALUES-CHARACTER-OR-NIL-READ (STREAM &AUX CH)
  (IF (CHAR= (SETQ CH (READ-CHAR STREAM)) #\return)
      NIL
      (WRITE-CHAR CH STREAM)))
  
; Make it easy to add arbitrary constraints to choose variable values
(SETF (GET :CONSTRAINT 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) 'CONSTRAINT-ITEM)
(DEFUN CONSTRAINT-ITEM (KEYWORD-AND-ARGS)
  (LET ((CONSTRAINT (AND (CONSP KEYWORD-AND-ARGS) (CDR KEYWORD-AND-ARGS)))
	(VALUES (MULTIPLE-VALUE-LIST (SEND SELF :DECODE-VARIABLE-TYPE
                                           (CDDDR KEYWORD-AND-ARGS))))
	READ-FUNCTION)
    (DECLARE (SPECIAL CONSTRAINT READ-FUNCTION))
    (SETQ READ-FUNCTION (SECOND VALUES))
    (SETF (SECOND VALUES) (CLOSURE '(CONSTRAINT READ-FUNCTION) 'READ-WITH-CONSTRAINT))
    (VALUES-LIST VALUES)))

(DEFUN READ-WITH-CONSTRAINT (STREAM)
  (DECLARE (SPECIAL CONSTRAINT READ-FUNCTION))
  (LET ((VAL (FUNCALL READ-FUNCTION STREAM)))
    (UNLESS (FUNCALL (FIRST CONSTRAINT) VAL)
      (FERROR NIL (OR (SECOND CONSTRAINT)
                      "Must be a ~*~a") VAL (FUNCTION-NAME CONSTRAINT)))
    VAL))


(SETF (GET :NUMBER 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(PRIN1 CHOOSE-VARIABLE-VALUES-NUMBER-READ
	     NIL NIL NIL
	     "Click left to enter a number from the keyboard"))

(DEFUN CHOOSE-VARIABLE-VALUES-NUMBER-READ (STREAM)
  "Read in and validate a number."
  (LET ((NUMBER (READ STREAM T)))
    (OR (NUMBERP NUMBER)
        (FERROR NIL "A number is required"))
    NUMBER))


(SETF (GET :NUMBER-OR-NIL 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(PRIN1 CHOOSE-VARIABLE-VALUES-NUMBER-OR-NIL-READ
	     NIL NIL NIL
	     "Click left to enter a new number, or NIL, from the keyboard."))

(DEFUN CHOOSE-VARIABLE-VALUES-NUMBER-OR-NIL-READ (STREAM)
  "Read in and validate a number of nil value."
  (LET ((NUMBER-OR-NIL (send stream :tyi nil)))
    (when (not (eql (char-int #\return) number-or-nil))
      (send stream :untyi number-or-nil)
      (setf NUMBER-OR-NIL (READ STREAM T))
      (OR (OR (NULL NUMBER-OR-NIL) (NUMBERP NUMBER-OR-NIL))
	  (FERROR () "A number or NIL is requred"))
      NUMBER-OR-NIL)))

(SETF (GET :FIXNUM 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(PRIN1 CHOOSE-VARIABLE-VALUES-FIXNUM-READ
	     NIL NIL NIL
	     "Click left to enter a new integer value from the keyboard"))

(DEFUN CHOOSE-VARIABLE-VALUES-FIXNUM-READ (STREAM)
  "Read in and validate a fixnum."
  (LET ((FIXNUM (READ STREAM T)))
    (OR (FIXNUMP FIXNUM)
        (FERROR NIL "An integer is required"))
    FIXNUM))

(SETF (GET :FIXNUM-OR-NIL 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(PRIN1 CHOOSE-VARIABLE-VALUES-FIXNUM-OR-NIL-READ
	      NIL NIL NIL
	      "Click left to enter a new integer value or NIL from the keyboard"))

(DEFUN CHOOSE-VARIABLE-VALUES-FIXNUM-OR-NIL-READ (STREAM)
  "Read in and validate a fixnum or nil."
  (LET ((FIXNUM-OR-NIL (send stream :tyi nil)))
    (when (not (eql (char-int #\return) FIXNUM-OR-NIL))
      (send stream :untyi FIXNUM-OR-NIL)
      (setf FIXNUM-OR-NIL (READ STREAM T))
      (OR (OR (NULL FIXNUM-OR-NIL) (FIXNUMP FIXNUM-OR-NIL))
	  (FERROR () "An integer or NIL is requred"))
      FIXNUM-OR-NIL))
  )

(SETF (GET :SMALL-FRACTION 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(PRIN1 CHOOSE-VARIABLE-VALUES-SMALL-FRACTION-READ
	      NIL NIL NIL
	      "Click left to enter a fraction greater than zero and less than or equal to one"))

(DEFUN CHOOSE-VARIABLE-VALUES-SMALL-FRACTION-READ (STREAM)
  "Read in and validate a small fraction."
  (LET ((SMALL-FRACTION (READ STREAM T)))
    (OR (AND (NUMBERP SMALL-FRACTION)
             (>  SMALL-FRACTION 0)
             (<= SMALL-FRACTION 1.0))
	(FERROR NIL "Must be a number > zero and <= one"
                SMALL-FRACTION))
    SMALL-FRACTION))

(SETF (GET :SET 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
      'CHOOSE-VARIABLE-VALUES-DECODE-SET)

(DEFUN CHOOSE-VARIABLE-VALUES-DECODE-SET (KWD-AND-ARGS)
  (VALUES 'CHOOSE-VARIABLE-VALUES-SET-PRINT     ; Print function
          ()                                    ; Read function
	  (SECOND KWD-AND-ARGS)                 ; Choices to be printed
	  ()                                    ; Print translate function
	  'CHOOSE-VARIABLE-VALUES-SET-VALUE     ; Value translate function
	  "Click left to add or delete an item of the set"))

(DEFVAR ITEM-VALUE-FOR-SET NIL
  "Contains the value for SET.  This will often be a list.")

(DEFUN CHOOSE-VARIABLE-VALUES-SET-PRINT (ITEM STREAM)
  (DECLARE (SPECIAL ITEM-VALUE-FOR-SET))
  (SEND SELF :SET-CURRENT-FONT
	(IF (MEMBER ITEM
                    (IF (CONSP (CADR ITEM-VALUE-FOR-SET))
                        ;; We have a value from QUOTE.  The actual value part is in the
                        ;; second part of the variable.
                        (CADR ITEM-VALUE-FOR-SET)
                        ;;ELSE
                        ITEM-VALUE-FOR-SET)
                    :TEST #'EQ)
            4 3))
  (PRINC ITEM STREAM))

(DEFUN CHOOSE-VARIABLE-VALUES-SET-VALUE (ITEM)
  (DECLARE (SPECIAL ITEM-VALUE-FOR-SET          ;Current item value
		    CHOOSE-VARIABLE-VALUES-SET-FLAG))   ;Only bound when changing the value
  (IF (VARIABLE-BOUNDP CHOOSE-VARIABLE-VALUES-SET-FLAG)
      (IF (MEMBER ITEM ITEM-VALUE-FOR-SET :TEST #'EQ)
	  (REMOVE ITEM (THE LIST ITEM-VALUE-FOR-SET) :TEST #'EQ)
          ;;ELSE
          (CONS ITEM ITEM-VALUE-FOR-SET))
      ITEM-VALUE-FOR-SET))

;;; An item-type to quote the results
(SETF (GET :QUOTE 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
      'CHOOSE-VARIABLE-VALUES-QUOTE-ITEM)

(DEFUN CHOOSE-VARIABLE-VALUES-QUOTE-ITEM (KEYWORD-AND-ARGS)
  (LET ((VALUES (MULTIPLE-VALUE-LIST
                  (SEND SELF :DECODE-VARIABLE-TYPE (CDR KEYWORD-AND-ARGS))))
	PRINT-FUNCTION
	READ-FUNCTION
	VALUE-FUNCTION)
    (DECLARE (SPECIAL PRINT-FUNCTION READ-FUNCTION VALUE-FUNCTION))
    (WHEN (SETQ PRINT-FUNCTION (FIRST  VALUES))
      (SETF (FIRST VALUES)  (CLOSURE '(PRINT-FUNCTION) 'PRINT-AND-QUOTE)))
    (WHEN (SETQ READ-FUNCTION  (SECOND VALUES))
      (SETF (SECOND VALUES) (CLOSURE '(READ-FUNCTION ) 'READ-AND-QUOTE)))
    (WHEN (SETQ VALUE-FUNCTION (FIFTH  VALUES))
      (SETF (FIFTH VALUES)  (CLOSURE '(VALUE-FUNCTION) 'TRANSLATE-AND-QUOTE)))
    (VALUES-LIST VALUES)))

(DEFUN PRINT-AND-QUOTE (ITEM STREAM)
  (DECLARE (SPECIAL PRINT-FUNCTION))
  `(',(FUNCALL PRINT-FUNCTION ITEM STREAM)))

(DEFUN READ-AND-QUOTE (STREAM)
  (DECLARE (SPECIAL READ-FUNCTION))
  `(',(FUNCALL READ-FUNCTION STREAM)))

(DEFUN TRANSLATE-AND-QUOTE (STREAM)
  (DECLARE (SPECIAL VALUE-FUNCTION VAL))
  (WHEN (VARIABLE-BOUNDP VAL)
    (WHEN (EQ (FIRST VAL) 'QUOTE)
      (SETQ VAL (SECOND VAL))
      (SETQ ITEM-VALUE-FOR-SET VAL)))
    `(QUOTE ,(FUNCALL VALUE-FUNCTION STREAM)))


(SETF (GET :MENU 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
      'CHOOSE-VARIABLE-VALUES-DECODE-MENU)
(DEFUN CHOOSE-VARIABLE-VALUES-DECODE-MENU (KWD-AND-ARGS)
  (LET ((MENU (AND (CONSP KWD-AND-ARGS) (SECOND KWD-AND-ARGS))))
    (DECLARE (SPECIAL MENU))
    (VALUES (CLOSURE '(MENU) 'CHOOSE-VARIABLE-VALUES-MENU-PRINT) NIL NIL NIL
	    (CLOSURE '(MENU) 'CHOOSE-VARIABLE-VALUES-MENU)
	    "Click left to select a new value from a menu")))

(DEFUN CHOOSE-VARIABLE-VALUES-MENU (VALUE)
  (DECLARE (SPECIAL MENU))
  (MULTIPLE-VALUE-BIND (NEW-VALUE ITEM-CHOSEN-P)
      (W:MENU-CHOOSE (EVAL MENU))
    (IF ITEM-CHOSEN-P NEW-VALUE VALUE)))

(DEFUN CHOOSE-VARIABLE-VALUES-MENU-PRINT (VALUE STREAM)
  (DECLARE (SPECIAL MENU))
  (LET ((MENU-VALUE (EVAL MENU)))
    (PRINC
      (OR (WHEN (AND (CONSP MENU-VALUE) (CONSP (CAR MENU-VALUE)))
            (DOLIST (ITEM MENU-VALUE)
              (WHEN (EQ VALUE (W:MENU-EXECUTE-NO-SIDE-EFFECTS ITEM))
		(IF (CONSP item)		;; may 8-13-88
		    (RETURN (FIRST ITEM))
		    (RETURN item)))))		;; may 8-13-88
	  VALUE)
      STREAM)))


(DEFUN READ-STRING (STREAM)
  "Read in a string and return it."
  (LET* ((STRING-VALUE (MAKE-ARRAY '(512.) :ELEMENT-TYPE 'STRING-CHAR))
	 (LAST-INDEX (SEND STREAM :STRING-LINE-IN NIL STRING-VALUE)))
    (SUBSEQ STRING-VALUE 0 LAST-INDEX)))

(DEFUN READ-STRING-AND-TRIM (STREAM)
  "Read in a string and return it with trailing white-space removed."
  (LET* ((STRING-VALUE (MAKE-ARRAY '(512.) :ELEMENT-TYPE 'STRING-CHAR))
	 (LAST-INDEX (SEND STREAM :STRING-LINE-IN NIL STRING-VALUE)))
    (STRING-TRIM '(#\SPACE #\TAB) (SUBSEQ STRING-VALUE 0 LAST-INDEX))))

(SETF (GET :DATE 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(TIME:PRINT-UNIVERSAL-TIME READ-DATE NIL
				  NIL NIL "Click left to input a new date from the keyboard."))

(DEFUN READ-DATE (STREAM)
  (LET ((VAL (TIME:PARSE-UNIVERSAL-TIME (READ-STRING-AND-TRIM STREAM))))
    VAL))

(SETF (GET :PAST-DATE 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(TIME:PRINT-UNIVERSAL-TIME READ-PAST-DATE NIL
				  NIL NIL "Click left to input a new date from the keyboard."))
(DEFUN READ-PAST-DATE (STREAM)
  (LET ((VAL (TIME:PARSE-UNIVERSAL-TIME (READ-STRING-AND-TRIM STREAM) 0 NIL NIL)))
    VAL))

(SETF (GET :DATE-OR-NEVER 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(PRINT-UNIVERSAL-TIME-OR-NEVER READ-DATE-OR-NEVER NIL
				      NIL NIL "Click left to input a new date from the keyboard."))

(DEFUN PRINT-UNIVERSAL-TIME-OR-NEVER (TIME STREAM)
  (IF (NULL TIME) (PRINC "never" STREAM)
      (TIME:PRINT-UNIVERSAL-TIME TIME STREAM)))

(DEFUN READ-DATE-OR-NEVER (STREAM)
  (LET ((STRING (READ-STRING-AND-TRIM STREAM)))
    (IF (EQUALP STRING "never") NIL
        (LET ((VAL (TIME:PARSE-UNIVERSAL-TIME STRING)))
          VAL))))

(SETF (GET :INTERVAL-OR-NEVER 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(TIME:PRINT-INTERVAL-OR-NEVER TIME:READ-INTERVAL-OR-NEVER NIL
				     NIL NIL "Click left to input a new interval, or NEVER, from the keyboard."))

(DEFUN (:PROPERTY :MENU-ALIST CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) (KWD-AND-ARGS)
  (VALUES 'PRINC
          NIL
          (SECOND KWD-AND-ARGS)
          'CAR
          'W:MENU-EXECUTE-NO-SIDE-EFFECTS
	  'MENU-ITEM-WHO-LINE-DOCUMENTATION))

(SETF (GET :STRING-LIST 'CHOOSE-VARIABLE-VALUES-KEYWORD)
       '(PRINT-STRING-LIST READ-STRING-LIST))

(DEFUN PRINT-STRING-LIST (STRING-LIST STREAM)
  (FORMAT STREAM "~{~A~^, ~}" STRING-LIST))

(DEFUN READ-STRING-LIST (STREAM)
  (DO ((STRING (READ-STRING STREAM))
       (I 0 (1+ J))
       (J)
       (STRING-LIST NIL))
      (NIL)
    (SETQ J (POSITION #\,  STRING :START I :TEST #'CHAR-EQUAL))
    (PUSH (STRING-TRIM '(#\SPACE #\TAB) (NSUBSTRING STRING I J)) STRING-LIST)
    (OR J (RETURN (NREVERSE STRING-LIST)))))

;;;
;;;  New methods support to get wilds defaults where you need them from a CVV window. 9-15-85 MBC
;;;
(SETF (GET :DIRECTORY-PATHNAME 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
      'DECODE-DIRECTORY-PATHNAME-ITEM)
(DEFUN DECODE-DIRECTORY-PATHNAME-ITEM (KEYWORD-AND-ARGS)
  (DECLARE (IGNORE KEYWORD-AND-ARGS))
  (VALUES                                       ;args are ignored for now --> (:DIRECTORY-PATHNAME ARG1)
   'PRINC (CLOSURE NIL 'READ-DIRECTORY-PATHNAME)
   NIL NIL NIL
   "Click left to enter or click right to edit pathname."))

(DEFVAR *DEFAULT-DIRECTORY-PATHNAME* ())
(DEFUN READ-DIRECTORY-PATHNAME (STREAM)
  (send
    (FS:MERGE-PATHNAME-DEFAULTS
      (READ-STRING STREAM)
      *DEFAULT-DIRECTORY-PATHNAME*)
    :string-for-printing))
;;; Thats it! - MBC

(SETF (GET :PATHNAME 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
      'DECODE-PATHNAME-ITEM)
(DEFUN DECODE-PATHNAME-ITEM (KEYWORD-AND-ARGS)
  (LET ((DEFAULTS (AND (CONSP KEYWORD-AND-ARGS) (SECOND KEYWORD-AND-ARGS))))
    (DECLARE (SPECIAL DEFAULTS))
    (VALUES
      'PRINC
      (CLOSURE '(DEFAULTS) 'READ-PATHNAME)
      NIL NIL NIL
      "Click left to enter a new pathname from the keyboard.")))

(SETF (GET :PATHNAME-OR-NIL 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
      'DECODE-PATHNAME-OR-NIL-ITEM)
(DEFUN DECODE-PATHNAME-OR-NIL-ITEM (KEYWORD-AND-ARGS)
  (LET ((DEFAULTS (AND (CONSP KEYWORD-AND-ARGS) (SECOND KEYWORD-AND-ARGS))))
    (DECLARE (SPECIAL DEFAULTS))
    (VALUES
      'PRINC
      (CLOSURE '(DEFAULTS) 'READ-PATHNAME-OR-NIL)
      NIL NIL NIL
      "Click left to enter a new pathname from the keyboard.")))

(SETF (GET :PATHNAME-LIST 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION)
      'DECODE-PATHNAME-LIST-ITEM)
(DEFUN DECODE-PATHNAME-LIST-ITEM (KEYWORD-AND-ARGS)
  (LET ((DEFAULTS (AND (CONSP KEYWORD-AND-ARGS) (SECOND KEYWORD-AND-ARGS))))
    (DECLARE (SPECIAL DEFAULTS))
    (VALUES
      'PRINT-STRING-LIST
      (CLOSURE '(DEFAULTS) 'READ-PATHNAME-LIST)
      NIL NIL NIL
      "Click left to enter new pathnames from the keyboard.")))

(DEFUN READ-PATHNAME-OR-NIL (STREAM &AUX STRING)
  (DECLARE (SPECIAL DEFAULTS))
  (SETQ STRING (READ-STRING STREAM))
  (AND (PLUSP (LENGTH STRING))
       (SEND (FS:MERGE-PATHNAME-DEFAULTS 
	       STRING
	       (IF (SYMBOLP DEFAULTS) (SYMBOL-VALUE DEFAULTS) DEFAULTS))
	:STRING-FOR-PRINTING)))

(DEFUN READ-PATHNAME (STREAM)
  (DECLARE (SPECIAL DEFAULTS))
  (SEND
    (FS:MERGE-PATHNAME-DEFAULTS
      (READ-STRING STREAM)
      (IF (SYMBOLP DEFAULTS) (SYMBOL-VALUE DEFAULTS) DEFAULTS))
    :STRING-FOR-PRINTING))

(DEFUN READ-PATHNAME-LIST (STREAM)
  (MAPCAR #'(LAMBDA (PATHNAME) (SEND PATHNAME :STRING-FOR-PRINTING))
	  (PARSE-PATHNAME-LIST (READ-STRING STREAM))))

(DEFUN PARSE-PATHNAME-LIST (STRING &OPTIONAL (DEFAULTS DEFAULTS))
  (DECLARE (SPECIAL DEFAULTS))
  (DO ((I 0 (1+ J))
       (J)
       (STRING-LIST NIL))
      (NIL)
    (SETQ J (POSITION #\, STRING :START I :TEST #'CHAR-EQUAL))
    (PUSH (FS:MERGE-PATHNAME-DEFAULTS (NSUBSTRING STRING I J)
                                      (IF (SYMBOLP DEFAULTS) (SYMBOL-VALUE DEFAULTS) DEFAULTS))
          STRING-LIST)
    (OR J (RETURN (NREVERSE STRING-LIST)))))


;;; Following are some newer options which are being added to better support Profile. -GRH 9/18/86

;;;(SETF (GET :FUNCTION 'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRIN1 CVV-FUNCTION-READ))

;;;(DEFUN CVV-FUNCTION-READ (STREAM)
;;;  "Read in and validate a function (either lambda expr or function symbol)."
;;;  (LET ((FUNCTION (READ STREAM T)))
;;;    (UNLESS (OR (FUNCTIONP FUNCTION) (NULL FUNCTION))
;;;      (FERROR () "A function is required."))
;;;    FUNCTION)) 


(SETF (GET :LIST 'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRIN1 READ-SEXP))


(SETF (GET :NON-NEGATIVE-FIXNUM 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(PRIN1 CHOOSE-VARIABLE-VALUES-NON-NEGATIVE-FIXNUM-READ
	  () () ()
	  "Click mouse button to enter a new non-negative integer from the keyboard"))


(DEFUN CHOOSE-VARIABLE-VALUES-NON-NEGATIVE-FIXNUM-READ (STREAM)
  "Read in and validate a non-negative integer."
  (LET ((FIXNUM (READ STREAM T)))
    (UNLESS (AND (FIXNUMP FIXNUM) (NOT (MINUSP FIXNUM)))
      (FERROR () "A non-negative integer is required"))
    FIXNUM)) 



(SETF (GET :POSITIVE-FIXNUM-OR-NIL 'CHOOSE-VARIABLE-VALUES-KEYWORD)
      '(PRIN1 CHOOSE-VARIABLE-VALUES-POSITIVE-FIXNUM-OR-NIL-READ
	  () () ()
	  "Click left to enter a new positive integer value or NIL from the keyboard"))


(DEFUN CHOOSE-VARIABLE-VALUES-POSITIVE-FIXNUM-OR-NIL-READ (STREAM)
  "Read in and validate a positive fixnum or nil."
  (LET ((FIXNUM-OR-NIL (READ STREAM T)))
    (OR (OR (NULL FIXNUM-OR-NIL) (AND (NOT (MINUSP FIXNUM-OR-NIL)) (FIXNUMP FIXNUM-OR-NIL)))
       (FERROR () "A positive integer or NIL is required"))
    FIXNUM-OR-NIL)) 


;;;(SETF (GET :FONT 'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRIN1 CVV-READ-FONT))

;;;(DEFUN CVV-READ-FONT (STREAM)
;;;  "Read in and validate a font or font name."
;;;  (LET* ((*PACKAGE* (FIND-PACKAGE 'FONTS))
;;;	 (FONT (READ STREAM T)))
;;;    (UNLESS (OR (AND (SYMBOLP FONT) (BOUNDP FONT) (TYPEP (SYMBOL-VALUE FONT) 'FONT)
;;;		     (EQ (SYMBOL-PACKAGE FONT) *PACKAGE*))
;;;		(TYPEP FONT 'FONT))
;;;      (FERROR () "A Font is required."))
;;;    (IF (AND (SYMBOLP FONT) (BOUNDP FONT))
;;;	(SYMBOL-VALUE FONT)
;;;	FONT))) 


;;;(SETF (GET :FONT-LIST 'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRIN1 CVV-READ-FONT-LIST))

;;;;;;The next two functions support the cvv-type :font-list in Release 3 -- 9/86 slm
;;;(DEFUN VALIDATE-FONT (FONT)
;;;  "Given an object this function signals an error if it is not a font name."
;;;  (UNLESS (OR (AND (SYMBOLP FONT)
;;;		   (BOUNDP FONT)
;;;		   (TYPEP (SYMEVAL FONT) 'FONT)
;;;		   (EQ (SYMBOL-PACKAGE FONT) *PACKAGE*))
;;;	      (TYPEP FONT 'FONT))
;;;    (FERROR NIL "A non-Font found in font list.")))

;;;(DEFUN CVV-READ-FONT-LIST (STREAM)
;;;  "Read in and validate a list of fonts."
;;;  (LET* ((*PACKAGE* (FIND-PACKAGE 'FONTS))
;;;	 (FONTS (READ STREAM)))
;;;    (IF (NOT (CONSP FONTS))
;;;	(FERROR NIL "List of fonts is required."))
;;;    (MAPCAR #'VALIDATE-FONT FONTS)
;;;    FONTS))


;;;(SETF (GET :KEYWORD 'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRIN1 CVV-READ-KEYWORD))

;;;(DEFUN CVV-READ-KEYWORD (STREAM)
;;;  "Read in and validate a keyword."
;;;  (LET* ((*PACKAGE* (FIND-PACKAGE 'KEYWORD))
;;;	 (KEYWORD (READ STREAM T)))
;;;    (UNLESS (KEYWORDP KEYWORD)
;;;      (FERROR () "A Keyword is required."))
;;;    KEYWORD)) 


(SETF (GET :MULTIPLE-MENU 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) 'CVV-DECODE-MULTIPLE-MENU)

(DEFUN CVV-DECODE-MULTIPLE-MENU (KWD-AND-ARGS)
  (LET ((MENU (AND (CONSP KWD-AND-ARGS) (SECOND KWD-AND-ARGS))))
    (DECLARE (SPECIAL MENU ))
    (VALUES (CLOSURE '(MENU ) 'CVV-MULTIPLE-MENU-PRINT)
	    () () ()
	    (CLOSURE '(MENU) 'CVV-MULTIPLE-MENU-READ)
	    "Click left to select new values from a menu."))) 


(DEFUN CVV-MULTIPLE-MENU-READ (OLD-VALUE)
  (DECLARE (SPECIAL MENU))
  (MULTIPLE-VALUE-BIND (NEW-VALUE NOT-ABORTED-P)
      (LET* ((MENU-ALIST
	       (EVAL MENU))
	     (CURRENT-ITEMS
	       (LOOP FOR MENU-ITEM IN MENU-ALIST
		     WHEN (MEMBER (W:MENU-EXECUTE-NO-SIDE-EFFECTS MENU-ITEM) OLD-VALUE :TEST #'EQ)
		     COLLECT MENU-ITEM)))
	(W:MULTIPLE-MENU-CHOOSE
	  MENU-ALIST
	  :LABEL "Select any number of values:"
	  :NEAR-MODE '(:MOUSE)
	  :HIGHLIGHTED-ITEMS CURRENT-ITEMS))
    (IF NOT-ABORTED-P
	NEW-VALUE
	;;ELSE
	OLD-VALUE)))

(DEFUN CVV-MULTIPLE-MENU-PRINT (CURRENT-VALUES STREAM)
  (DECLARE (SPECIAL MENU ))
  (unless (listp current-values)  ;; must be a list...
    (ferror nil "A variable that you specified had a value of ~A which was the wrong type.  You must redefine 
         the variable to be a LIST in order to make the :MULTIPLE-MENU option work. 
"    current-values current-values ))
	   
  (LET* ((MENU-ALIST (EVAL MENU))		
	 (CURRENT-ITEM-STRINGS
	   (LOOP FOR VALUE IN CURRENT-VALUES
		 COLLECT (LOOP FOR MENU-ITEM IN MENU-ALIST
			   
			       WHEN (EQ VALUE (W:MENU-EXECUTE-NO-SIDE-EFFECTS MENU-ITEM))
			       RETURN (W:MENU-ITEM-STRING MENU-ITEM)
			       FINALLY (RETURN VALUE)))))
    (FORMAT STREAM "~A~{, ~A~}" (FIRST CURRENT-ITEM-STRINGS) (REST CURRENT-ITEM-STRINGS))))


;;; ========================
;;; CVV type :LIST-OF
;;; ========================
;;; Accepts a list of the specified variable type.
;;; Example Use:  (tv:choose-variable-values '((x "x" :list-of :fixnum)))
;;;                ==> Accepts a list of fixnums.
;;;

(SETF (GET :LIST-OF 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) 'DECODE-LIST-OF)

(DEFUN DECODE-LIST-OF (KWD-AND-ARGS)
  (DECLARE (:SELF-FLAVOR BASIC-CHOOSE-VARIABLE-VALUES))
  (MULTIPLE-VALUE-BIND (PF RF)
      (SEND SELF :DECODE-VARIABLE-TYPE (CDR KWD-AND-ARGS))
    (DECLARE (SPECIAL RF PF))
    (VALUES (CLOSURE '(PF)
		     #'(LAMBDA (VALUES STREAM)
			 (DECLARE (SPECIAL PF))
			 (IF (ZEROP (LENGTH VALUES))
			     (PRIN1 VALUES STREAM)
			     ;;ELSE
			     (PROGN
			       (FUNCALL PF (CAR VALUES) STREAM)
			       (LOOP FOR VALUE IN (CDR VALUES)
				     DO (PROGN
					  (WRITE-CHAR #\, STREAM)
					  (FUNCALL PF VALUE STREAM)))))))
	    (CLOSURE '(RF)
		     #'(LAMBDA (STREAM)
			 (DECLARE (SPECIAL RF))
			 (WITH-INPUT-FROM-STRING (STRING-STREAM (READ-STRING STREAM))
			   (LOOP FOR VALUE = (FUNCALL RF STRING-STREAM)
				 COLLECTING VALUE
				 FOR NEXT-CH = (READ-CHAR STRING-STREAM NIL)
				 WHILE (AND (NOT (NULL NEXT-CH)) (CHAR= NEXT-CH #\,))))))
	    NIL NIL NIL
	    `(:MOUSE-L-1
	       ,(FORMAT NIL "Click mouse to enter a list of type ~a" (SYMBOL-NAME (CAR KWD-AND-ARGS)))))))

(SETF (GET :TEST 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) 'DECODE-TEST)

(DEFUN DECODE-TEST (KWD-AND-ARGS)
  (LET ((FUNCTION-NAME (SECOND KWD-AND-ARGS)))
    (DECLARE (SPECIAL FUNCTION-NAME))
    (VALUES 'PRIN1
	    (CLOSURE '(FUNCTION-NAME)
		     #'(LAMBDA (STREAM)
			 (DECLARE (SPECIAL FUNCTION-NAME))
			 (LET ((VARIABLE (READ STREAM T)))
			   (UNLESS (FUNCALL FUNCTION-NAME VARIABLE)
			     (FERROR NIL "The value ~a did not pass the predicate ~a." VARIABLE (SYMBOL-NAME FUNCTION-NAME)))
			   VARIABLE)))
	    NIL NIL NIL
	    `(:MOUSE-L-1
	       ,(FORMAT NIL "Click mouse to enter a value that passes the predicate ~a" (SYMBOL-NAME FUNCTION-NAME))))))

(SETF (GET :TYPEP 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) 'DECODE-TYPEP)

(DEFUN DECODE-TYPEP (KWD-AND-ARGS)
  (LET ((TYPE-SPECIFIER (SECOND KWD-AND-ARGS)))
    (DECLARE (SPECIAL TYPE-SPECIFIER))
    (VALUES 'PRIN1
	    (CLOSURE '(TYPE-SPECIFIER)
		     #'(LAMBDA (STREAM)
			 (DECLARE (SPECIAL TYPE-SPECIFIER))
			 (LET ((VARIABLE (READ STREAM T)))
			   (WHEN (NOT (OR (AND (SYMBOLP VARIABLE) (TYPEP (SYMBOL-VALUE VARIABLE) TYPE-SPECIFIER))
					  (TYPEP VARIABLE TYPE-SPECIFIER)))
			     (FERROR NIL "The value ~a is not of type ~a." VARIABLE (IF (SYMBOLP TYPE-SPECIFIER)
                                                                                        (SYMBOL-NAME TYPE-SPECIFIER)
                                                                                        ;;ELSE
                                                                                        TYPE-SPECIFIER)))
			   VARIABLE)))
	    NIL NIL NIL
	    `(:MOUSE-L-1
	       ,(FORMAT NIL "Click mouse to enter a value that is of type ~a" (IF (SYMBOLP TYPE-SPECIFIER)
                                                                                  (SYMBOL-NAME TYPE-SPECIFIER)
                                                                                  ;;ELSE
                                                                                  TYPE-SPECIFIER))))))

;;; ========================
;;; CVV type :CHARACTER-LIST
;;; ========================
;;; Accepts a list of character objects or fixnums
;;; This keyword takes an optional argument, which is the representation of the characters (:FIXNUM or :CHARACTER)
;;; The default representation is based on the parameter CHARACTER-LIST-DEFAULT-REPRESENTATION-TYPE
;;; Example Use:  (tv:choose-variable-values '((a "Characters" :character-list :fixnum)))
;;;                ==> Ensures that A is a list of fixnums.
;;;

;;;(SETF (GET :CHARACTER-LIST 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) 'DECODE-CHARACTER-LIST)

;;;(DEFPARAMETER CHARACTER-LIST-DEFAULT-REPRESENTATION-TYPE :FIXNUM
;;;   "Used by the CVV variable type :CHARACTER-LIST to determine what the elements of the list will be.
;;; Must be either :fixnum or :character.") 

;;;(DEFUN DECODE-CHARACTER-LIST (KWD-AND-ARGS)
;;;  "KWD-AND-ARGS is expected to be a list of one or two elements,
;;;the keyword, optionally followed by the representation type for the characters.
;;;The optional argument must be :fixnum or :character."
;;;  (LET ((REPRESENTATION-TYPE
;;;	  (OR (SECOND KWD-AND-ARGS) CHARACTER-LIST-DEFAULT-REPRESENTATION-TYPE)))
;;;    (VALUES #'(LAMBDA (VALUE STREAM)
;;;		(FORMAT STREAM "(~{~@C~^ ~})" VALUE))
;;;	    (CASE REPRESENTATION-TYPE
;;;	      (:FIXNUM 'CVV-READ-GENERIC-CHARACTER-LIST)
;;;	      (:CHARACTER 'CVV-READ-CHARACTER-CHARACTER-LIST))
;;;	    () () () '(:MOUSE-L-1 "Click Mouse to change this list of characters.")))) 

;;;(DEFUN CVV-READ-GENERIC-CHARACTER-LIST (REPRESENTATION-TYPE STREAM)
;;;  (LET ((CHAR-LIST (READ-SEXP STREAM)))
;;;    (UNLESS (CONSP CHAR-LIST)
;;;      (SETQ CHAR-LIST (CONS CHAR-LIST ())))
;;;    (LOOP FOR SUBLIST ON CHAR-LIST
;;;	  UNLESS (TYPEP (FIRST SUBLIST) REPRESENTATION-TYPE)
;;;	  DO
;;;          (SETF (FIRST SUBLIST)
;;;		(CASE REPRESENTATION-TYPE
;;;		      (:FIXNUM (CHAR-INT (FIRST SUBLIST)))    ;Can't coerce a character into an integer
;;;		      (:CHARACTER (COERCE (FIRST SUBLIST) 'GLOBAL:CHARACTER)))))
;;;    CHAR-LIST)) 

;;;(DEFUN CVV-READ-FIXNUM-CHARACTER-LIST (STREAM)
;;;  (CVV-READ-GENERIC-CHARACTER-LIST :FIXNUM STREAM)) 

;;;(DEFUN CVV-READ-CHARACTER-CHARACTER-LIST (STREAM)
;;;  (CVV-READ-GENERIC-CHARACTER-LIST :CHARACTER STREAM)) 



;;;======================
;;; CVV Type:  :STRING-OR-NIL
;;;======================

(SETF (GET :STRING-OR-NIL 'CHOOSE-VARIABLE-VALUES-KEYWORD) '(PRINC READ-STRING-OR-NIL))

(DEFUN READ-STRING-OR-NIL (STREAM)
  "Read in a string and return it."
  (LET* ((STRING-VALUE (MAKE-ARRAY '(512) :ELEMENT-TYPE 'STRING-CHAR) )
	 (LAST-INDEX (SEND STREAM :STRING-LINE-IN () STRING-VALUE)))
    (IF (ZEROP LAST-INDEX)
	()
	(PROGN (SETQ STRING-VALUE (SUBSEQ STRING-VALUE 0 LAST-INDEX))
	       (IF (STRING-EQUAL "NIL" STRING-VALUE)
		   ()
		   STRING-VALUE))))) 

;;; The following function can be used as a constraint or side-effect
;;; on color selections.  We really should have a :COLOR type.  PMH
(DEFUN w:cvv-get-color (window variable old-value new-value &optional real-window)
  "Get color either by selection from color map display or integer input from pop-up.
Returns integer color map index value only.  Only to be used with CVV's.  Used by
Screen Editor, GWIN, and GED."
  (DECLARE (IGNORE old-value))
  ;; This function should not be used unless on a color system.
  (LET ((color-map (SEND (OR real-window (SEND window :superior) tv:default-screen) :color-map)))
    ;; If color-map is NIL (which it should never be on a color system), force it to be the system default.
    (UNLESS color-map
      (SETQ color-map tv:*default-color-map*))
    (CASE new-value
      (:color-map
       (if (color-system-p window)
	   (SETF (SYMBOL-VALUE variable) (w:select-color-with-mouse color-map))
	   (ferror nil "Color Selector only works on converted color system.")))
      (:color-number
       (SETF (SYMBOL-VALUE variable) (w:pop-up-read-number)))
      )))

;;; Modified to understand the :EDIT option.
;;;   This initializes the rubout handler buffer with the current
;;;   value when the item is clicked on.
;;; Modified to understand the :SIDE-EFFECT option.
;;;   This is a function that is called, when the item is changed,
;;;   with arguments: window, variable, old-value, new-value; if the
;;;   form after :side-effect is a list, it is evaluated when item is
;;;   changed.
;;; Modified to understand the :PREDICATE option.
;;;   This isn't implemented yet, but eventually it will be A
;;;   function to call (with one argument) or expression to evaluate
;;;   to determine if the item is to be displayed (return NIL to not
;;;   display).  This is used for implementing dynamic forms where
;;;   some questions aren't asked if they don't need to be.
;;; Modified to understand the :DEFAULT option.
;;;   This is a value to set the variable to when :SET-VARIABLES is
;;;   called.
;;; Modified to understand the :CONSTRAINT option
;;;    This is a function or form that is evaluated whenever the
;;;    item is displayed. If the evaluation result is non-nil, it is
;;;    displayed to the right of the value. (The value is supposed
;;;    to be an error string.)
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :DECODE-VARIABLE-TYPE)
           (KWD-AND-ARGS &AUX KEY TEM)
  (DECLARE (VALUES PRINT-FUNCTION READ-FUNCTION CHOICES PRINT-TRANSLATE
                        VALUE-TRANSLATE DOCUMENTATION PREDICATE EDIT-FLAG
                        SIDE-EFFECT CONSTRAINT))
  (SETQ KEY (OR (CAR KWD-AND-ARGS) :SEXP))
  (COND ((EQ KEY :DOCUMENTATION)
         (MULTIPLE-VALUE-BIND (PF RF CHOICES GPVF GVVF IGNORE PRED EDIT SIDE CONSTR)
             (SEND SELF :DECODE-VARIABLE-TYPE (CDDR KWD-AND-ARGS))
           (VALUES PF RF CHOICES GPVF GVVF (CADR KWD-AND-ARGS) PRED EDIT SIDE CONSTR)))
        ((EQ KEY :EDIT)
         (MULTIPLE-VALUE-BIND (PF RF CHOICES GPVF GVVF DOC PRED IGNORE SIDE CONSTR)
             (SEND SELF :DECODE-VARIABLE-TYPE (CDR KWD-AND-ARGS))
           (VALUES PF RF CHOICES GPVF GVVF DOC PRED T SIDE CONSTR)))
        ((EQ KEY :PREDICATE)
         (MULTIPLE-VALUE-BIND (PF RF CHOICES GPVF GVVF DOC IGNORE EDIT SIDE CONSTR)
             (SEND SELF :DECODE-VARIABLE-TYPE (CDDR KWD-AND-ARGS))
           (VALUES PF RF CHOICES GPVF GVVF DOC (CADR KWD-AND-ARGS) EDIT SIDE CONSTR)))
        ((EQ KEY :SIDE-EFFECT)
         (MULTIPLE-VALUE-BIND (PF RF CHOICES GPVF GVVF DOC PRED EDIT IGNORE CONSTR)
             (SEND SELF :DECODE-VARIABLE-TYPE (CDDR KWD-AND-ARGS))
           (VALUES PF RF CHOICES GPVF GVVF DOC PRED EDIT (CADR KWD-AND-ARGS) CONSTR)))
        ((EQ KEY :CONSTRAINT)
         (MULTIPLE-VALUE-BIND (PF RF CHOICES GPVF GVVF DOC PRED EDIT SIDE IGNORE)
             (SEND SELF :DECODE-VARIABLE-TYPE (CDDR KWD-AND-ARGS))
           (VALUES PF RF CHOICES GPVF GVVF DOC PRED EDIT SIDE (CADR KWD-AND-ARGS))))
        ((EQ KEY :DEFAULT)
         (MULTIPLE-VALUE-BIND (PF RF CHOICES GPVF GVVF DOC PRED EDIT SIDE CONSTR)
             (SEND SELF :DECODE-VARIABLE-TYPE (CDDR KWD-AND-ARGS))
           (VALUES PF RF CHOICES GPVF GVVF DOC PRED EDIT SIDE CONSTR)))
        ((SETQ TEM (GET KEY 'CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION))
         (FUNCALL TEM KWD-AND-ARGS))
        ((SETQ TEM (GET KEY 'CHOOSE-VARIABLE-VALUES-KEYWORD))
         (VALUES-LIST TEM))
        (T
         (FERROR NIL "~S bad keyword in a CHOOSE-VARIABLE-VALUES-WINDOW" KEY))))

;So lines can wrap around when reading
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :AROUND :END-OF-LINE-EXCEPTION) (&REST IGNORE)
  (IF LINE-OVERFLOW-ALLOWED
      (FUNCALL #'(:METHOD SHEET :END-OF-LINE-EXCEPTION) :END-OF-LINE-EXCEPTION)
      (THROW 'LINE-OVERFLOW T)))

;;; Make printing work in environment of owning stack group
(DEFWRAPPER (BASIC-CHOOSE-VARIABLE-VALUES :REDISPLAY) (IGNORE . BODY)
  `(LET ((*PACKAGE*      (SYMEVAL-IN-STACK-GROUP '*PACKAGE*      STACK-GROUP))
         (*READ-BASE*    (SYMEVAL-IN-STACK-GROUP '*READ-BASE*    STACK-GROUP))
         (*PRINT-BASE*   (SYMEVAL-IN-STACK-GROUP '*PRINT-BASE*   STACK-GROUP))
         (*NOPOINT       (SYMEVAL-IN-STACK-GROUP '*NOPOINT       STACK-GROUP))
         (*PRINT-LEVEL*  (SYMEVAL-IN-STACK-GROUP '*PRINT-LEVEL*  STACK-GROUP))
         (*PRINT-LENGTH* (SYMEVAL-IN-STACK-GROUP '*PRINT-LENGTH* STACK-GROUP))
         (*READTABLE*    (SYMEVAL-IN-STACK-GROUP '*READTABLE*    STACK-GROUP)))
     (BIND (LOCATE-IN-INSTANCE SELF 'LINE-OVERFLOW-ALLOWED) NIL)
     ,@BODY))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :PRINT-ITEM) ;; MODIFIED 4/21/86 - KDB
           (ITEM LINE-NO ITEM-NO
            &OPTIONAL (EXTRA-WIDTH 0) (TABLE-P NIL)
            &AUX VAR STR FONTNO CHOICES PF RF K&A GPVF GVVF PVAL CVAL
            (VSP (SEND SELF :VSP))
            CONSTRAINT
            (COMPLETE-ITEM ITEM))
  "Print a single item for choose-variable-values."
  (DECLARE (SPECIAL ITEM-VALUE-FOR-SET))
  LINE-NO ITEM-NO                               ;ignored
  (when cvv-debug
        (send si:cold-load-stream :line-out "Entering :print-item"))
  (IF (AND (CONSP ITEM) (CONSP (CAR ITEM)))
      (DO ((SUB-ITEM ITEM (CDR SUB-ITEM)))
	  ((NULL SUB-ITEM))
        ;; Call again to handle recursive definitions.
	(SEND SELF :PRINT-ITEM (FIRST SUB-ITEM) LINE-NO ITEM-NO EXTRA-WIDTH T))
      ;;ELSE
      ;; Parse ITEM into label string, font to print that in, variable, and keyword-&-arguments
      (PROGN
	(COND ((STRINGP ITEM)
               (SETQ STR ITEM
                     FONTNO 0))
              ((SYMBOLP ITEM)
               (SETQ VAR ITEM
                     STR (SYMBOL-NAME VAR)
                     FONTNO 1))
              (T
               (SETQ VAR (CAR ITEM)
                     STR (IF (OR (STRINGP (CADR ITEM)) (NUMBERP (CADR ITEM)) (NULL (CADR ITEM)))
                             (CAR (SETQ ITEM (CDR ITEM)))
                             (IF (SYMBOLP VAR)
                                 (SYMBOL-NAME VAR)
                                 NIL))
                     FONTNO 1
                     K&A (CDR ITEM))))
        ;; If any label string, print it and a colon.
	(COND (TABLE-P
               (SHEET-SET-FONT SELF (AREF FONT-MAP FONTNO))
               (SHEET-STRING-OUT SELF "  "))
              ((EQ (CAR K&A) 'MORE-CHOICES)
               ;; This is a continuation.  Print out some
               ;; space before these choices.
               (SHEET-SET-FONT SELF (AREF FONT-MAP FONTNO))
               (SHEET-STRING-OUT SELF "     "))
              (STR
               (MULTIPLE-VALUE-BIND (START-X START-Y)
                   (SEND SELF :READ-CURSORPOS)
                 (SHEET-SET-FONT SELF (AREF FONT-MAP FONTNO))
                 (SHEET-STRING-OUT SELF STR)
                 (IF VAR (SHEET-STRING-OUT SELF ": "))
		 (MULTIPLE-VALUE-BIND (NEW-X NEW-Y)
		     (SEND SELF :READ-CURSORPOS)
		   (WHEN (AND (FIXNUMP VALUE-TAB)
			      (< NEW-X (+ START-X VALUE-TAB)))
		     (MULTIPLE-VALUE-BIND (LEFT-MARGIN )
			 (SEND SELF :MARGINS)
		       (WHEN (AND VAR (< (1- (FONT-CHAR-WIDTH CURRENT-FONT))
                                         (- (+ START-X VALUE-TAB) NEW-X)))
			 ;;; ASH line-height... centers leader lines.
			 (SEND SELF :BITBLT (sheet-char-aluf self)(- (+ START-X VALUE-TAB) NEW-X) 2
			       12%-GRAY 0 0 (- NEW-X LEFT-MARGIN) (+ NEW-Y (ASH (- LINE-HEIGHT VSP 1) -1)))))
		     (SEND SELF :SET-CURSORPOS (+ START-X VALUE-TAB) START-Y))))))
	;; If any variable, get its value and decide how to print it.
	(WHEN VAR
	  ;; change ITEM-VALUE-FOR-SET to a binding instead of setq
	  ;; in choose-variable-values-choice :redisplay is called
	  ;; after an error to display an item that is not the
	  ;; current item; needless to say we don't want ITEM-VALUE-FOR-SET
	  ;; changed.  PMH7/26/87
	  (let ((ITEM-VALUE-FOR-SET (COND ((SYMBOLP VAR)
                                          (SYMEVAL-IN-STACK-GROUP VAR STACK-GROUP))
                                         (T (CAR VAR)))))
	  (MULTIPLE-VALUE-SETQ (PF RF CHOICES GPVF GVVF NIL NIL NIL NIL CONSTRAINT)
                               (SEND SELF :DECODE-VARIABLE-TYPE (OR K&A '(:SEXP))))
	  (COND ((NOT CHOICES)
                 (LET ((LABEL-WIDTH
                         (IF TABLE-P
                             (SEND SELF :STRING-LENGTH STR 0 NIL NIL (AREF FONT-MAP 0))
                             EXTRA-WIDTH)))
                   (SHEET-SET-FONT SELF (AREF FONT-MAP 2))
                   (SEND SELF :ITEM1
                         (CONS ITEM-VALUE-FOR-SET COMPLETE-ITEM) :VARIABLE-CHOICE
                         'CHOOSE-VARIABLE-VALUES-PRINT-FUNCTION PF
                         ITEM-VALUE-FOR-SET LABEL-WIDTH))
		 ;;testing for constraints was removed from here since errors
		 ;;couldn't be handled properly  PMH 7/25/87
                 )
                (T
                  (when cvv-debug
                        (send si:cold-load-stream :line-out
                              (format nil "in :print-item, pf=~a, rf=~a, choices=~a"
                                      pf rf choices)))
                  (LET (LAST-X
                         (CHOICES-LEFT CHOICES)
                         ;; We are changing the font inside of the DO loop, so we
                         ;; need to save it here.
                         (OLD-SHEET-FONT (SHEET-CURRENT-FONT SELF)))
                     ;; Print out the variable's value.
                     (CATCH 'LINE-OVERFLOW
                       (LOOP FOR CHOICE IN CHOICES-LEFT
                             DO (PROGN 
                                  (SETQ PVAL (IF GPVF (FUNCALL GPVF CHOICE) CHOICE)
                                        CVAL (IF GVVF (FUNCALL GVVF CHOICE) CHOICE))
                                  (SHEET-SET-FONT
                                    SELF (AREF FONT-MAP (IF (EQUAL CVAL ITEM-VALUE-FOR-SET) 4 3)))
                                  (SETQ LAST-X CURSOR-X)
                                  (SEND SELF :ITEM1 (CONS CHOICE COMPLETE-ITEM) :VARIABLE-CHOICE
                                        'CHOOSE-VARIABLE-VALUES-PRINT-FUNCTION PF PVAL)
                                  (SEND SELF :TYO #\SPACE)
                                  ;; Pop one off after we have successfully processed a choice.
                                  (POP CHOICES-LEFT))
                             ))
                     ;; Restore the font to its original value.
                     (SHEET-SET-FONT SELF OLD-SHEET-FONT)
                     ;; If this is a continuation line and not even one choice fits,
                     ;; leave that gigantic choice name on this line.
                     (WHEN (AND (EQ CHOICES-LEFT CHOICES) (EQ (THIRD ITEM) 'MORE-CHOICES))
                       (SETQ LAST-X CURSOR-X)
                       (POP CHOICES-LEFT))
                     ;; If choices don't fit on line, push some into following line.
                     (WHEN CHOICES-LEFT
                       (SHEET-SET-CURSORPOS SELF (- LAST-X LEFT-MARGIN-SIZE)
                                            (- CURSOR-Y TOP-MARGIN-SIZE))
                       (SHEET-CLEAR-EOL SELF)
                       (LET* ((NO-ITEMS (ARRAY-LEADER ITEMS 0))
                              (NEXT-ITEM
                                (IF (> NO-ITEMS (1+ ITEM-NO))
                                    (AREF ITEMS (1+ ITEM-NO)))))
                         ;; See if we ALREADY made a continuation item for this one.
                         (UNLESS (AND (CONSP NEXT-ITEM)
                                      (EQ (THIRD NEXT-ITEM) 'MORE-CHOICES))
                           ;; If not, make one, and put into it
                           ;; all the choices we could not fit on this line.
                           (VECTOR-PUSH-EXTEND NIL ITEMS)
                           (DO ((I 1 (1+ I))
                                (LIM (- NO-ITEMS ITEM-NO)))
                               ((= I LIM))
                             ;; Bubble items up
                             (SETF (AREF ITEMS (1+ (- NO-ITEMS I))) (AREF ITEMS (- NO-ITEMS I))))
                           (SETF (AREF ITEMS (1+ ITEM-NO))
                                 (LIST VAR () 'MORE-CHOICES
                                       (IF (EQ (THIRD ITEM) 'MORE-CHOICES)
                                           (FOURTH ITEM) ITEM)
                                       CHOICES-LEFT PF GPVF GVVF)))))
                     (WHEN EXTRA-WIDTH
                       (DOTIMES (I EXTRA-WIDTH) (SEND SELF :TYO #\SPACE))))))))))
  (when cvv-debug
        (send si:cold-load-stream :line-out "Leaving :print-item"))
  )

(DEFUN (:PROPERTY MORE-CHOICES CHOOSE-VARIABLE-VALUES-KEYWORD-FUNCTION) (KWD-AND-ARGS)
  (VALUES (FOURTH KWD-AND-ARGS) NIL (THIRD KWD-AND-ARGS)
          (FIFTH KWD-AND-ARGS) (eighTH KWD-AND-ARGS))) ;; may 9-14-88 was SIXTH

;; Return the item numbers of the first and last items
;; that are really all continuations of item number ITEM-NO.
;; In the usual case, both values equal ITEM-NO.  Both are inclusive.
(DEFUN CHOOSE-VARIABLE-VALUES-FIND-CONTINUATION-ITEMS (WINDOW ITEM-NO)
  (LET ((FIRST-ITEM ITEM-NO)
	(LAST-ITEM ITEM-NO)
	(ITEMS (SEND WINDOW :ITEMS))
	TEM)
    (DO ()
	((= FIRST-ITEM 0))
      (DECF FIRST-ITEM)
      (IF (OR (ATOM (SETQ TEM (AREF ITEMS FIRST-ITEM)))
              (NEQ (THIRD TEM) 'MORE-CHOICES))
	  (RETURN)))
    (DO ((END (1- (FUNCALL WINDOW :NUMBER-OF-ITEMS))))
	((OR (= LAST-ITEM END)
             (ATOM (SETQ TEM (AREF ITEMS (1+ LAST-ITEM))))
	     (NEQ (THIRD TEM) 'MORE-CHOICES)))
      (INCF LAST-ITEM))
    (VALUES FIRST-ITEM LAST-ITEM)))

(DEFUN CHOOSE-VARIABLE-VALUES-PRINT-FUNCTION (ITEM WINDOW PF PVAL &OPTIONAL EXTRA-WIDTH)
  ITEM                                          ;ignored
  (IF (NULL EXTRA-WIDTH)
      (FUNCALL PF PVAL WINDOW)
      ;;ELSE
      (LET* ((STRING (FORMAT:OUTPUT NIL
                       (FUNCALL PF PVAL *STANDARD-OUTPUT*)))
	     (ITEM-WIDTH (SEND SELF :STRING-LENGTH STRING)))
	(SEND SELF :INCREMENT-CURSORPOS (MAX 0 (- EXTRA-WIDTH ITEM-WIDTH)) 0)
	(SEND SELF :STRING-OUT STRING))))

;;; Modified from the :PRINT-ITEM method.  Hard to be completely
;;; modular about this. Extra-width is amount of space to allow for
;;; non-menu items to grow.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :ITEM-WIDTH)
           (ITEM &OPTIONAL (EXTRA-WIDTH 0) ITEM-NO
            &AUX VAR STR FONTNO CHOICES PF RF K&A GPVF GVVF
            PVAL (X 0) (PX 0) LONGEST-VAL)
  "Determine the width of a choice item."
  (DECLARE (SPECIAL ITEM-VALUE-FOR-SET))
  ITEM-NO
  (IF (AND (CONSP ITEM) (CONSP (CAR ITEM)))
      ;; Handle variable sublists.
      (COMMENT DO-NOTHING)
      ;; Parse ITEM into label string, font to print that in, variable, and keyword-&-arguments.
      (PROGN
	(COND ((STRINGP ITEM)
               (SETQ STR ITEM FONTNO 0))
              ((SYMBOLP ITEM)
               (SETQ VAR ITEM STR (SYMBOL-NAME VAR) FONTNO 1))
              (T (SETQ VAR (CAR ITEM)
                       STR (IF (OR (STRINGP (CADR ITEM)) (NULL (CADR ITEM)))
                               (CAR (SETQ ITEM (CDR ITEM)))
                               ;;ELSE
			       (when (symbolp var) ;var may be a locative PMH 1/15/88
				 (SYMBOL-NAME VAR)))
                       FONTNO 1
                       K&A (CDR ITEM))))
        ;; If any label string, print it and a colon
	(WHEN STR
	  (SETQ X (SEND SELF :STRING-LENGTH STR  0 NIL NIL (AREF FONT-MAP FONTNO) X))
	  (SETQ X (SEND SELF :STRING-LENGTH ": " 0 NIL NIL (AREF FONT-MAP FONTNO) X))
	  (WHEN (INTEGERP VALUE-TAB)  (SETQ X (MAX X VALUE-TAB)))
	  (WHEN (NOT (STRINGP ITEM)) (SETQ PX X)))
        ;; If any variable, get its value and decide how to print it.
	(WHEN VAR
	  (SETQ ITEM-VALUE-FOR-SET (IF (SYMBOLP VAR)
                                       (SYMEVAL-IN-STACK-GROUP VAR STACK-GROUP)
                                       ;;ELSE
                                       (CAR VAR)))
	  (MULTIPLE-VALUE-SETQ (PF RF CHOICES GPVF GVVF)
	    (SEND SELF :DECODE-VARIABLE-TYPE (OR K&A '(:SEXP))))
	  (COND ((NOT CHOICES)
                 (IF (MINUSP EXTRA-WIDTH) (INCF X (- EXTRA-WIDTH))
                     (SETQ X (+ (SEND SELF :STRING-LENGTH
                                      (FORMAT:OUTPUT NIL (FUNCALL PF ITEM-VALUE-FOR-SET *STANDARD-OUTPUT*))
                                      0 NIL NIL (AREF FONT-MAP 2) X)
                                EXTRA-WIDTH))))
                (T
                 ;; We will need to go through all choices to determine which one would use up
                 ;; the most space if it were chosen.  Then assume that LONGEST-VAL is that
                 ;; choice, and determine the line length.  This means that we will be
                 ;; calculating the maximum possible line length for this item.  The reason for
                 ;; all of this is so that we will not have a LINE-OVERFLOW condition when the
                 ;; worst-case choice is chosen.  This condition is not processed properly since
                 ;; continuation lines were not implemented correctly.  Continuation lines do
                 ;; not look very good anyway, even if they did work as expected.
                 (LET ((MAXIMUM-DELTA-WIDTH 0)  ; Largest width so far
                       CURRENT-DELTA-WIDTH)     ; Width of current choice
                   (LOOP FOR CHOICE IN CHOICES
                         WHEN (< MAXIMUM-DELTA-WIDTH
                                 (SETQ CURRENT-DELTA-WIDTH
                                       (- (SEND SELF :STRING-LENGTH
                                                (FORMAT:OUTPUT NIL (FUNCALL PF PVAL *STANDARD-OUTPUT*))
                                                0 NIL NIL
                                                (AREF FONT-MAP 4))
                                          (SEND SELF :STRING-LENGTH
                                                (FORMAT:OUTPUT NIL (FUNCALL PF PVAL *STANDARD-OUTPUT*))
                                                0 NIL NIL
                                                (AREF FONT-MAP 3)))))
                         DO (SETQ MAXIMUM-DELTA-WIDTH CURRENT-DELTA-WIDTH
                                  LONGEST-VAL CHOICE)))
                 (DOLIST (CHOICE CHOICES)
                   (SETQ PVAL (IF GPVF (FUNCALL GPVF CHOICE) CHOICE)
                         CHOICE (IF GVVF (FUNCALL GVVF CHOICE) CHOICE))
                   (SETQ X (SEND SELF :STRING-LENGTH
                                 (FORMAT:OUTPUT NIL (FUNCALL PF PVAL *STANDARD-OUTPUT*))
                                 0 NIL NIL
                                 (AREF FONT-MAP (IF (EQUAL CHOICE LONGEST-VAL) 4 3)) X))
                   (INCF X CHAR-WIDTH)))))))
  (VALUES X PX))

;;; This method modified to support the VALUE-TAB option
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :APPROPRIATE-WIDTH) (&OPTIONAL EXTRA-WIDTH)
  "Returns the inside-width appropriate to accommodate the
current set of variables with their current values.  If EXTRA-WIDTH
is specified that much room for expansion, which can be a number of
characters or a string, is left after non-menu items."
  (when cvv-debug
    (send cold-load-stream :clear-eol)
    (send cold-load-stream :line-out "In :appropriate-width"))
  (SETQ EXTRA-WIDTH
	(COND ((STRINGP EXTRA-WIDTH) (SEND SELF :STRING-LENGTH EXTRA-WIDTH))
              ((NUMBERP EXTRA-WIDTH) (* CHAR-WIDTH EXTRA-WIDTH))
              (T 0)))
  (LET ((ITEM-WIDTH 0) (LABEL-WIDTH 0))
    (DOTIMES (I (SEND SELF :NUMBER-OF-ITEMS))
      (MULTIPLE-VALUE-BIND (X PX)
          (SEND SELF :ITEM-WIDTH (AREF ITEMS I) EXTRA-WIDTH I)
	(when cvv-debug
	  (send cold-load-stream :clear-eol)
	  (send cold-load-stream :string-out (format nil "item-width(~d)=~d, label-width=~d,  "
						     i x px)))
	(SETQ ITEM-WIDTH  (MAX ITEM-WIDTH   X))
	(SETQ LABEL-WIDTH (MAX LABEL-WIDTH PX))))
    (when cvv-debug
      (send cold-load-stream :line-out ""))
    (WHEN (EQ VALUE-TAB T)
      (SETQ VALUE-TAB LABEL-WIDTH)
      (DOTIMES (I (SEND SELF :NUMBER-OF-ITEMS))
	(SETQ ITEM-WIDTH (MAX ITEM-WIDTH
			      (let ((debug-temp (SEND SELF :ITEM-WIDTH (AREF ITEMS I) EXTRA-WIDTH I)))
				(when cvv-debug
				  (send cold-load-stream :clear-eol)
				  (send cold-load-stream :string-out (format nil "item-width(~d)=~d, "
									     i debug-temp)))
				debug-temp))))
      (when cvv-debug
	(send cold-load-stream :line-out "")))
     (when cvv-debug
        (send cold-load-stream :clear-eol)
        (send cold-load-stream :line-out "Exiting :appropriate-width"))
    (MIN (MAX (SEND SELF :LABEL-SIZE) ITEM-WIDTH)
	 (- (SHEET-INSIDE-WIDTH SUPERIOR) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE))))

;This is quite a bit slower than it needs to be.  However these windows aren't used much.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :WHO-LINE-DOCUMENTATION-STRING) ()
  (MULTIPLE-VALUE-BIND (WINDOW-X-OFFSET WINDOW-Y-OFFSET)
      (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)
    (LET ((X (- MOUSE-X WINDOW-X-OFFSET))
	  (Y (- MOUSE-Y WINDOW-Y-OFFSET)))
      (MULTIPLE-VALUE-BIND (VALUE TYPE) (SEND SELF :MOUSE-SENSITIVE-ITEM X Y)
	(IF TYPE
	    (LET ((ITEM (AREF ITEMS (+ TOP-ITEM (TRUNCATE (- Y (SHEET-INSIDE-TOP))
                                                          LINE-HEIGHT)))))
		  ;; If we have a table, get the column of the table that
		  ;; the value part identifies and make that the item here.
	      (IF (AND (CONSP ITEM) (CONSP (CAR ITEM)))
		  (SETQ ITEM (ASSOC (SECOND VALUE) ITEM :TEST #'EQUAL)))
	      (IF (ATOM ITEM) '(:MOUSE-L-1 "input a new value from the keyboard"
                                :MOUSE-R-1 "edit this value")
		  ;;ELSE
		  (PROGN
		    (SETQ ITEM (CDR ITEM))
		    (AND (OR (STRINGP (CAR ITEM)) (NULL (CAR ITEM)))
                         (SETQ ITEM (CDR ITEM)))
		    (MULTIPLE-VALUE-BIND (IGNORE RF IGNORE IGNORE IGNORE DOC)
                        (SEND SELF :DECODE-VARIABLE-TYPE (OR ITEM '(:SEXP)))
		      (COND ((STRINGP DOC) DOC)
                            ((CONSP   DOC) DOC)
                            ((AND     DOC (FUNCALL DOC VALUE)))
                            ((NULL RF) '(:MOUSE-L-1 "change to this value"
                                         :MOUSE-R-1 "edit this value"))
                            (T '(:MOUSE-L-1 "input a new value from the keyboard"
                                 :MOUSE-R-1 "edit this value")))))))
	    ;;ELSE
	    (PROGN
	      (MULTIPLE-VALUE-SETQ (VALUE TYPE) (MOUSE-SENSITIVE-ITEM X Y))
	      (IF (AND VALUE (SYMBOLP (SECOND VALUE)))
                  (DOCUMENTATION (SECOND VALUE))
                  ;;ELSE
		  DEFAULT-WHO-LINE-DOCUMENTATION)))))))

(DEFVAR CHOOSE-VARIABLE-VALUES-ITEMS-NOT-EDITABLE
	'(:ASSOC :CHOOSE :SET :MENU-ALIST :MENU :BOOLEAN :MULTIPLE-MENU )
	"List of items which do not have an edittable value.")


(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :MOUSE-CLICK)
           (BUTTON X Y &AUX LINE-NO)
  "Handle the mouse clicks that the user enters."
  (WHEN (OR (= BUTTON #\MOUSE-R) (= BUTTON #\MOUSE-L))
    (MULTIPLE-VALUE-BIND (VALUE TYPE LEFT RIGHT)
        (SEND SELF :MOUSE-SENSITIVE-ITEM X Y)
      (WHEN TYPE
	(SETQ LINE-NO (TRUNCATE (- Y (SHEET-INSIDE-TOP)) LINE-HEIGHT))

	;; Grab the components of an item from what we get in.  If the
	;; user clicks right, then we will use the components to
	;; reconstruct an item with :EDIT in the keywords&arguments
	;; component.
	(LET ((ITEM (CDR VALUE))
	      STR VAR K&A NEW-ITEM)
	  (IF (AND (NOT (SYMBOLP ITEM)) (NOT (CONSP ITEM)))
	   ;; If the item isn't either a symbol or a list, then it
	   ;; must be something strange (like it is in the graphics
	   ;; editor).  In this case, use the item itself instead of
	   ;; construction a new one.
	   (SETQ NEW-ITEM (CDR VALUE))
	   ;;ELSE
	   (PROGN
	     (IF (SYMBOLP ITEM)
                 (SETQ VAR ITEM
                       STR (SYMBOL-NAME VAR))
                 ;;ELSE
		 (SETQ VAR (CAR ITEM)
                       STR (IF (OR (STRINGP (CADR ITEM)) (NULL (CADR ITEM)))
			   (CAR (SETQ ITEM (CDR ITEM)))
                           ;;ELSE
                           (when (symbolp var)  ;var may be a locative  PMH 1/15/88
			     (SYMBOL-NAME VAR)))
		       K&A (CDR ITEM)))
	     (SETQ NEW-ITEM (APPEND (LIST VAR STR)
                                    ;; If the user clicked right, and this
                                    ;; item doesn't already have an :EDIT
                                    ;; keyword in it, then add it in.
                                    (IF (AND (= BUTTON #\MOUSE-R) (NULL (MEMBER :EDIT K&A :TEST #'EQ)))
                                        (CONS :EDIT K&A)
                                        ;;EDIT
                                        K&A)))))
	  (SEND SELF :FORCE-KBD-INPUT
		(LIST TYPE SELF NEW-ITEM (CAR VALUE) LINE-NO
                      (- LEFT LEFT-MARGIN-SIZE) (- RIGHT LEFT-MARGIN-SIZE))))
	T))))

(DEFUN CHOOSE-VARIABLE-VALUES-RUBOUT-HANDLER-FUNCTION
       (RF PF EDIT STREAM &AUX SAVE-RHB WIN LEN)
  "Function called by rubout handler within CHOOSE-VARIABLE-VALUES-CHOICE."
  (DECLARE (SPECIAL REDISPLAY-FLAG))
  (LET ((STRING (MAKE-ARRAY 32. :ELEMENT-TYPE :STRING-CHAR :FILL-POINTER 0)))
    (WITH-OUTPUT-TO-STRING (STREAM STRING)
      ;; ITEM-VALUE-FOR-SET is the variable's value
      (FUNCALL PF ITEM-VALUE-FOR-SET STREAM))
    (WHEN (AND EDIT
	       ;; Sometimes the rubout handler buffer isn't a string but is an
	       ;; array.  Need to handle both cases.
	       (OR (ZEROP (LENGTH (SETQ SAVE-RHB (SEND STREAM :SAVE-RUBOUT-HANDLER-BUFFER))))
		   (EQUAL "" SAVE-RHB)))
      (SETQ LEN (LENGTH STRING))
      ;; The following two lines set up the cursor position properly.
      ;; The :INCREMENT-CURSORPOS will set the cursor to the start of
      ;; the variable's value.  The SETQ ensures that the rubout
      ;; handler's X position is the same as the cursor.  This is
      ;; necessary because the :RESTORE-RUBOUT-HANDLER-BUFFER redisplays
      ;; the STRING on the STREAM.  The redisplay needs to start at the
      ;; proper X position.
      (SEND STREAM :INCREMENT-CURSORPOS (- (* LEN (SHEET-CHAR-WIDTH STREAM))) 0)
      (SETQ RUBOUT-HANDLER-STARTING-X (SEND STREAM :READ-CURSORPOS)
	    PROMPT-STARTING-X RUBOUT-HANDLER-STARTING-X)
      (SEND STREAM :RESTORE-RUBOUT-HANDLER-BUFFER STRING LEN)))
  (UNWIND-PROTECT
      (PROG1
        (FUNCALL RF STREAM)
        (SETQ WIN T))
    (UNLESS WIN
      (SETQ REDISPLAY-FLAG T))))

(DEFUN CHOOSE-VARIABLE-VALUES-CHOICE
       (WINDOW ITEM CHOICE LINE-NO &OPTIONAL LEFT RIGHT
        &AUX FCN STR VAR OLDVAL NEWVAL NO-CHANGE
        K&A PF RF GPVF GVVF CHOICES REDIS Y
        PREDICATE EDIT SIDE-EFFECT CONSTRAINT)
  "Called when a :VARIABLE-CHOICE message comes back through the
io-buffer.  This is not a message, so that instance-variables won't be
bound in it.  This is assumed to be called in the relevant stack group
and binding environment."
  ;; Parse ITEM into label string, variable, and keyword-&-arguments
  (DECLARE (SPECIAL ITEM-VALUE-FOR-SET))
  (COND ((STRINGP ITEM)
         (SETQ STR ITEM))        ;Can't happen
        ((SYMBOLP ITEM)
         (SETQ VAR ITEM
               STR (SYMBOL-NAME VAR)))
        (T (SETQ VAR (CAR ITEM)
                 STR (IF (OR (STRINGP (CADR ITEM)) (NULL (CADR ITEM)))
                         (CAR (SETQ ITEM (CDR ITEM)))
                         (when (symbolp var) ;var may be a locative   PMH 1/15/88
			   (SYMBOL-NAME VAR)))
               K&A (CDR ITEM))))
  (MULTIPLE-VALUE-SETQ (PF RF CHOICES GPVF GVVF NIL PREDICATE EDIT SIDE-EFFECT CONSTRAINT)
    (SEND WINDOW :DECODE-VARIABLE-TYPE (OR K&A '(:SEXP))))
  (SETQ ITEM-VALUE-FOR-SET (IF (SYMBOLP VAR)
                               (SYMBOL-VALUE VAR)
                               (CAR VAR)))
  (UNLESS LEFT (SETQ LEFT (OR (AND (NULL STR) 0)
                              (+ (SHEET-STRING-LENGTH WINDOW (STRING STR))
                                 (SHEET-CHAR-WIDTH WINDOW)))))
  (UNLESS RIGHT (SETQ RIGHT (SHEET-INSIDE-WIDTH WINDOW)))
  (SETQ Y (* LINE-NO (SHEET-LINE-HEIGHT WINDOW)))
  (IF (NULL RF)                                 ; ":CHOOSE" case
      (SETQ NEWVAL CHOICE)
      ;;ELSE
      (PROGN
	;;switch to :value-font (font #2) PMH 7/25/87
	(SHEET-SET-FONT WINDOW (AREF (SHEET-FONT-MAP WINDOW) 2))
	(LET ((BL (SHEET-FOLLOWING-BLINKER WINDOW))
	      (WS (SEND WINDOW :STATUS)))
	  (UNWIND-PROTECT
              (PROGN
                ;; Next line makes the mouse highlight go away
                (SEND WINDOW :SET-SENSITIVE-ITEM-TYPES NIL)
                (BLINKER-SET-VISIBILITY BL :BLINK)
                (IF EDIT
                    (LET ((STRING (MAKE-ARRAY 32.
                                                :ELEMENT-TYPE :STRING-CHAR
                                                :FILL-POINTER 0))
			    (string-length))
                        (WITH-OUTPUT-TO-STRING (STREAM STRING)
                          ;; ITEM-VALUE-FOR-SET is the variable's value
                          (FUNCALL PF ITEM-VALUE-FOR-SET STREAM))
			(setq string-length (length string))
			(SEND WINDOW :SET-CURSORPOS
                              (+ LEFT (* (SHEET-CHAR-WIDTH WINDOW) string-length))
                              Y)
			;; we are really not editing if we start with empty string   PMH
                        (when (= 0 string-length)
			  (setf edit nil)))
                    ;;ELSE
                    (PROGN
                      (SEND WINDOW :SET-CURSORPOS LEFT Y)
                      (SEND WINDOW :DRAW-RECTANGLE (- RIGHT LEFT)
                            (SHEET-LINE-HEIGHT WINDOW)
                            LEFT Y (SHEET-ERASE-ALUF WINDOW))))
                (IF (CONSP RF)
                    (SETQ NEWVAL (FUNCALL (CAR RF) WINDOW))
                    ;;ELSE
                    ;; Hair for over-rubout => save old value
                    (LET ((REDISPLAY-FLAG NIL))
                      ;;(LOCALLY (DECLARE (SPECIAL REDISPLAY-FLAG))
                      (DECLARE (SPECIAL REDISPLAY-FLAG))
		      ;; At this point the the cursor is correctly placed
		      ;; and the text field is correct [editing or not],
		      ;; however the the rubout handler buffer has not been
		      ;; initialized for the EDIT case.  When necessary this
		      ;; is recalculated in the CVVRH function.  We have done
		      ;; all this set-up in case the first character typed
		      ;; on an empty buffer is RUBOUT.  PMH 7/29/87
                               (DO* ((*TERMINAL-IO* WINDOW)       ;Should be ERROR-OUTPUT
				     (CH)
				     (FULL-RUBOUT (not edit))	;first time initialization
				     (first-time t nil)
				     (edit edit (not full-rubout)))
                                    ((and (NOT FULL-RUBOUT)
					  (not first-time))) ;;PMH
				 (when full-rubout
				   (if (CHAR= (SETQ CH (READ-CHAR WINDOW)) #\RUBOUT)
				       (RETURN (SETQ NO-CHANGE T REDIS NIL))
				       (UNREAD-CHAR CH WINDOW)))
                                 (CONDITION-CASE (ERROR)
				   (progn
                                     (MULTIPLE-VALUE-SETQ (NEWVAL FULL-RUBOUT)
                                                          (SEND WINDOW :RUBOUT-HANDLER
                                                                `((:FULL-RUBOUT T)
                                                                  ;; We need to pass this through for the
                                                                  ;; :CHARACTER-OR-NIL variable-type.
                                                                  (:PASS-THROUGH ,(CHAR-INT #\CLEAR-INPUT))
                                                                  (:DONT-HANDLE-ERRORS T))
                                                                #'CHOOSE-VARIABLE-VALUES-RUBOUT-HANDLER-FUNCTION
                                                                RF PF EDIT WINDOW))
				     ;; process constraints here so we get good error handling  PMH 7/25/87
				     (when (and constraint (not full-rubout))
				       (let ((errmsg (if (functionp constraint)
							 (funcall constraint self var item-value-for-set newval)
							 (eval constraint))))
					 (if errmsg (ferror nil errmsg))))
				     )
                                   (ERROR
                                    (SEND WINDOW :FRESH-LINE)
                                    (LET ((ERROR-START (SHEET-CURSOR-Y WINDOW))
                                          (FONT (SEND WINDOW :CURRENT-FONT))
                                          ERROR-END)
                                      (SEND WINDOW :SET-CURRENT-FONT 4)
                                      (SEND ERROR :REPORT WINDOW)
                                      (SEND WINDOW :SET-CURRENT-FONT FONT)
                                      (SETQ ERROR-END (+ (SHEET-CURSOR-Y WINDOW)
                                                         (SHEET-LINE-HEIGHT WINDOW)))
                                      (SEND WINDOW :SET-CURSORPOS LEFT Y)
                                      (CLEAR-INPUT WINDOW)
                                      (LET ((CH (READ-CHAR WINDOW)))
                                        (UNLESS (CHAR= CH #\SPACE)
                                          (UNREAD-CHAR CH WINDOW)))
                                      ;; Redisplay changed lines.
                                      ;; :REDISPLAY doesn't erase first, so erase those lines
                                      (SEND WINDOW :DRAW-RECTANGLE
                                            (SHEET-INSIDE-WIDTH WINDOW)
                                            (- ERROR-END ERROR-START)
                                            0 (- ERROR-START (SHEET-TOP-MARGIN-SIZE WINDOW))
                                            (SHEET-ERASE-ALUF WINDOW))
                                      (SEND WINDOW :REDISPLAY
                                            (TRUNCATE (- ERROR-START (SHEET-INSIDE-TOP WINDOW))
                                                      (SHEET-LINE-HEIGHT WINDOW))
                                            (TRUNCATE (- ERROR-END (SHEET-INSIDE-TOP WINDOW))
                                                      (SHEET-LINE-HEIGHT WINDOW)))
                                      (SEND WINDOW :SET-CURRENT-FONT FONT)
                                      (SEND WINDOW :SET-CURSORPOS LEFT Y)
                                      (SEND WINDOW :DRAW-RECTANGLE
					    ;; used to be (- right left) but rigth can get
					    ;; modified is a space was hit earlier
					    ;; the following will always clear to EOL line PMH 7/26/87
                                            (- (SHEET-INSIDE-WIDTH WINDOW) LEFT)
					    (SHEET-LINE-HEIGHT WINDOW)
                                            LEFT Y (SHEET-ERASE-ALUF WINDOW))
                                      (SETQ FULL-RUBOUT T REDIS T))))
                                 ;; If we got a read error, try to avoid garbage in the display
                                 ;; This is really a kludge, is there a better way?
                                 (SETQ REDIS REDISPLAY-FLAG)))))
	    (BLINKER-SET-VISIBILITY BL NIL)
	    (SEND WINDOW :SET-SENSITIVE-ITEM-TYPES T)
	    (OR (EQ WS :SELECTED) (SEND WINDOW :SET-STATUS WS))))))
  (SETQ OLDVAL (IF (SYMBOLP VAR)
                   (SYMBOL-VALUE VAR)
                   (CAR VAR)))
  (WHEN GVVF
    (PROGW `((VAL ',OLDVAL) (CHOOSE-VARIABLE-VALUES-SET-FLAG T))
      (SETQ NEWVAL (FUNCALL GVVF NEWVAL))))
  (AND NO-CHANGE (SETQ NEWVAL OLDVAL))
  (IF (SYMBOLP VAR)
      (SET VAR NEWVAL)
      (RPLACA VAR NEWVAL))
  (WHEN SIDE-EFFECT
    (IF (CONSP SIDE-EFFECT) (SI::EVAL1 SIDE-EFFECT)
	(FUNCALL SIDE-EFFECT WINDOW VAR OLDVAL NEWVAL)))
  (OR (AND (SETQ FCN (SEND WINDOW :FUNCTION))
           (FUNCALL FCN WINDOW VAR OLDVAL NEWVAL))
      ;; Redisplay
      (LET ((LAST-LINE-CLOBBERED
	     (1+ (IF (NULL RF) LINE-NO          ;If menu always one line, otherwise could have cr'ed
		  (TRUNCATE (- (SHEET-CURSOR-Y WINDOW) (SHEET-INSIDE-TOP WINDOW))
			    (SHEET-LINE-HEIGHT WINDOW)))))
	    (N-LINES (TRUNCATE (SHEET-INSIDE-HEIGHT WINDOW) (SHEET-LINE-HEIGHT WINDOW)))
	    ;; In menu case, more than one item can be affected
	    ;; since this variable's choices may span several lines (items).
	    (THIS-ITEM (+ LINE-NO (SEND WINDOW :TOP-ITEM)))
	    FIRST-ITEM LAST-ITEM)
	(UNLESS RF
	 ;; So figure out first and last item affected.
	  (SETF (VALUES FIRST-ITEM LAST-ITEM)
		(CHOOSE-VARIABLE-VALUES-FIND-CONTINUATION-ITEMS WINDOW THIS-ITEM))
	  ;; Extend range of lines to redisplay
	  ;; by number of items that are relevant before or after this one.
	  (SETQ LAST-LINE-CLOBBERED (MIN N-LINES (+ LINE-NO 1 (- LAST-ITEM THIS-ITEM))) LINE-NO
		(MAX 0 (- LINE-NO (- THIS-ITEM FIRST-ITEM)))))
	(AND (OR (<= LAST-LINE-CLOBBERED LINE-NO)       ;wrap-around => full redisplay
		 REDIS)
	     (SETQ LAST-LINE-CLOBBERED N-LINES LINE-NO 0))
	(SHEET-FORCE-ACCESS (WINDOW T)
			    ;; :REDISPLAY doesn't erase first, so erase those lines
			    (SEND WINDOW :DRAW-RECTANGLE (SHEET-INSIDE-WIDTH WINDOW)
				  (* (- LAST-LINE-CLOBBERED LINE-NO) (SHEET-LINE-HEIGHT WINDOW))
				  0 (* LINE-NO (SHEET-LINE-HEIGHT WINDOW))
				  (SHEET-ERASE-ALUF WINDOW))
			    (SEND WINDOW :REDISPLAY LINE-NO LAST-LINE-CLOBBERED))))
  (SEND WINDOW :MOVE-CURSOR 'DOWN))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :REFRESH-RUBOUT-HANDLER)
           (&OPTIONAL DISCARD-LAST-CHARACTER)
  (WHEN DISCARD-LAST-CHARACTER
    (SETF (RHB-FILL-POINTER) (MAX 0 (1- (RHB-FILL-POINTER))))
    ;; Ensure the scan pointer does not exceed the fill pointer.
    (SETF (RHB-SCAN-POINTER) (MIN (RHB-SCAN-POINTER) (RHB-FILL-POINTER))))
  (IF (RHB-TYPEIN-POINTER)
      (SETF (RHB-TYPEIN-POINTER) (MIN (RHB-TYPEIN-POINTER) (RHB-FILL-POINTER))))
;;; START NEW STUFF
;;; this actually replaces a clear cursor position call
  (MULTIPLE-VALUE-bind (ITEM TYPE LEFT BWIDTH TOP)
      (MOUSE-SENSITIVE-ITEM cursor-x cursor-y '(:variable-choice))
    (declare (ignore top type))
    (when item ; there really should always be one
      (SEND self :DRAW-RECTANGLE (- bwidth left) (SHEET-LINE-HEIGHT self)
	    LEFT   RUBOUT-HANDLER-STARTING-Y (SHEET-ERASE-ALUF self))))
;;; END NEW STUFF
  (SEND SELF :SET-CURSORPOS RUBOUT-HANDLER-STARTING-X RUBOUT-HANDLER-STARTING-Y)
  (SEND SELF :STRING-OUT RUBOUT-HANDLER-BUFFER))

;;; Redisplay a single choice item, when you know its value has been changed elsewhere
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :REDISPLAY-VARIABLE) (VARIABLE)
  (DO ((I 0 (1+ I))
       (NITEMS (ARRAY-ACTIVE-LENGTH ITEMS))
       (ITEM))
      ((>= I NITEMS)
       (FERROR NIL "~S is not a variable in ~S" VARIABLE SELF))
    (AND (EQ VARIABLE (IF (ATOM (SETQ ITEM (AREF ITEMS I))) ITEM (CAR ITEM)))
	 (LET ((LINE-NO (- I TOP-ITEM)))
	   (COND ((AND (>= LINE-NO 0) (< LINE-NO (SHEET-NUMBER-OF-INSIDE-LINES)))
                  (SEND SELF :DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) LINE-HEIGHT
                        0 (* LINE-NO LINE-HEIGHT) ERASE-ALUF)
                  (SEND SELF :REDISPLAY LINE-NO (1+ LINE-NO))))
	   (RETURN)))))

(DEFFLAVOR CHOOSE-VARIABLE-VALUES-PANE-MIXIN () ())
(DEFFLAVOR CHOOSE-VARIABLE-VALUES-PANE
           ()
	   (CHOOSE-VARIABLE-VALUES-PANE-MIXIN CHOOSE-VARIABLE-VALUES-WINDOW))

;;; Let it be determined by the superior
(DEFMETHOD (CHOOSE-VARIABLE-VALUES-PANE-MIXIN :ADJUSTABLE-SIZE-P) ()
  NIL)



;;; Even though we the vertical and horizontal dimensions are independent, this gives
;;; what we prefer.
(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :PANE-SIZE) (REM-WIDTH REM-HEIGHT
                                                      IGNORE IGNORE STACKING)
  (CASE STACKING
        (:VERTICAL (MIN REM-HEIGHT
                        (+ TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE
                           (* (ARRAY-ACTIVE-LENGTH ITEMS) LINE-HEIGHT))))
        (:HORIZONTAL (MIN REM-WIDTH
                          (+ LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE
                             (SEND SELF :APPROPRIATE-WIDTH))))))

(DEFFLAVOR TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW ()
	   (TEMPORARY-SHADOW-BORDERS-WINDOW-MIXIN
	    CHOOSE-VARIABLE-VALUES-WINDOW))
;Should this send itself a "exit" if it gets deexposed?  I think probably not.

(DEFMETHOD (TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW :NAME-FOR-SELECTION) ()
  NIL)


(DEFWINDOW-RESOURCE TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW ()
  :MAKE-WINDOW (TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW
                 ;; Give this window some edges that are big
                 ;; enough to be useful.
                 :EDGES-FROM (LIST
                               (SHEET-INSIDE-LEFT   DEFAULT-SCREEN)
                               (SHEET-INSIDE-TOP    DEFAULT-SCREEN)
                               (SHEET-INSIDE-RIGHT  DEFAULT-SCREEN)
                               (SHEET-INSIDE-BOTTOM DEFAULT-SCREEN))
		 :foreground-color *default-menu-foreground*
		 :background-color *default-menu-background*)
  :INITIAL-COPIES 0)


;;; The following function is used to scan through a parsed margin choice list
;;; for the presence of a specific completion string.  If a match is found and
;;; a form is present then it is EVALed and the result of the evaluation is
;;; returned.  If a match is found and no form is present then NIL is returned.
;;; If no match is found then NOT-FOUND-FUNCTION is executed.

(DEFUN EXECUTE-MARGIN-CHOICE (MARGIN-CHOICES COMPLETION-STRING NOT-FOUND-FUNCTION)
  (LOOP FOR MARGIN-CHOICE IN MARGIN-CHOICES
        FOR BOX-LABEL = (CAR MARGIN-CHOICE)
        WITH FORM-TO-EVAL = NIL
        WHEN (AND (STRINGP BOX-LABEL) (STRING-EQUAL BOX-LABEL COMPLETION-STRING))
        DO (PROGN
             (SETQ FORM-TO-EVAL (eighTH MARGIN-CHOICE))  ;; may 9-14-88 was SIXTH
             (RETURN (IF FORM-TO-EVAL
                         (EVAL FORM-TO-EVAL)
                         ;;ELSE
                         NIL)))
        FINALLY (FUNCALL NOT-FOUND-FUNCTION)))

(DEFCONSTANT CHOOSE-VARIABLE-VALUES-INTERCEPTED-CHARACTERS (REMOVE-IF #'(LAMBDA (ELT)
                                                                          (ZEROP (CHAR-BITS (CAR ELT))))
                                                                      (THE LIST KBD-INTERCEPTED-CHARACTERS)))

;;; Modified to read keyboard input
;;; Modified to accept variable sub-lists (to indicate more than one item per line)
;;; Modified to do :EDIT processing
;;; Modified to do :SIDE-EFFECT processing
(DEFUN CHOOSE-VARIABLE-VALUES (VARIABLES &KEY &OPTIONAL (FUNCTION	 nil)
			       				(NEAR-MODE	 '(:MOUSE))
							(LABEL		 "Choose Variable Values")
							(WIDTH		 nil)
							(EXTRA-WIDTH	 10.)
							(HEIGHT		 nil)
							(MARGIN-CHOICES	 nil)
							(SUPERIOR	 nil)
							(REVERSE-VIDEO-P nil)
							(VALUE-TAB	 T)
							(FORCE-PERMANENT NIL)
							SELECTED-IO
							(foreground-color *default-menu-foreground*)
							(background-color *default-menu-background*)
							(label-color      *default-menu-label-foreground*)
							(label-background *default-menu-label-background*)
					 &AUX (osw selected-window))
  "Invoke a temporary Choose-variable-values window to choose VARIABLES.
VARIABLES is a list of elements, each describing one line of the display. These become
text-scroll items.  Kinds of elements allowed are:

string		Just displayed. Use as a header or for blank lines IN CONJUNCTION with 
                  other items. Should not be used as the only item in a variable list. 
symbol		value is printed, and if the user clicks on it with the mouse a
		new value is read.
dtp-locative	like special-variable but value is accessed by car and
		written by rplaca.
list		(VAR LABEL TYPE ARGS...).  VAR is the variable (symbol or
		dtp-locative), LABEL if not NIL is a string to print
		instead of VAR's name, TYPE is a keyword saying what kinds
		of values are allowed (default :SEXP), and ARGS are args
		used by the TYPE's parsing functions.

Keyword args are:
:LABEL		Window label (default is `Choose Variable Values')
:FUNCTION	Function called if user changes anything (default is NIL)
:NEAR-MODE	Where to appear the window (default is (:MOUSE))
:WIDTH	        Desired width of window.  Default is to set wide enough
		for items.
:EXTRA-WIDTH	Amount of extra width to allow for growing items.
		Default 10 characters.  Each of the above widths may be
		a number of characters or a string.
:HEIGHT	Desired height of window in pixels.  The Default is to set height
		large enough to accomodate items.  At a certain height
		based on the item sizes, scrolling is activated.  This
		keyword allows you to specify the height large enough to
		prevent the need to scroll.
:MARGIN-CHOICES List of elements.  A string is the label for the box
		which means `exit' (Default is `DO IT'), cons of a
		string and a form means eval that form if box clicked upon.
:SUPERIOR	Window to put under, default is the superior of the window it
		is supposed to be near (if any), or SELECTED-WINDOW (if it
		is running the CURRENT-PROCESS), or *TERMINAL-IO* (if it
		is running the CURRENT-PROCESS), or finally MOUSE-SHEET.
:REVERSE-VIDEO-P  T means display this window reverse-video.
:VALUE-TAB	T (the default) means to tab values out past choice
		labels; A fixnum means to tab values to that number of
		spaces.  NIL (or zero) means not to tab.
FORCE-PERMANENT T means to make the CVV window a permanent window.
		Defaults to NIL which will make the CVV window temporary."
  selected-io
  ;; Decide what superior to use.  First try the superior of the window that we are exposing near, then try
  ;; SELECTED-WINDOW, then try *TERMINAL-IO*, and last default to MOUSE-SHEET.
  ;;
  (unless variables   ;; do a sanity check for user who wants for some reason to display a margin choice only...
    (ferror nil "Variables argument was nil.  The variables arg must specify a list of variables"))
  (WHEN cvv-debug
        (send si:cold-load-stream :set-cursorpos 0 0))
  (UNLESS (TYPEP superior 'sheet)
    (SETQ SUPERIOR (COND ((EQ (CAR NEAR-MODE) :WINDOW)
			  (SHEET-SUPERIOR (CADR NEAR-MODE)))
			 ((AND (TYPEP osw 'sheet)
			       (EQ si:current-process (SEND osw :send-if-handles :process)))
			  (sheet-get-screen osw (sheet-get-screen osw)))
			 ((AND (TYPEP *terminal-io* 'sheet)
			       (EQ si:current-process (SEND *terminal-io* :send-if-handles :process)))
			  (sheet-get-screen *terminal-io* (sheet-get-screen *terminal-io*)))
			 (t MOUSE-SHEET))))
  ;; MARGIN-CHOICES must always contain a "DO IT" box so user can stop choosing.
  (LOOP FOR L IN MARGIN-CHOICES
        FINALLY (PUSH W:MARGIN-CHOICE-COMPLETION-STRING MARGIN-CHOICES)
        DO (IF (OR (STRINGP L)
                   (AND (CONSP L)
                        (STRING-EQUAL W:MARGIN-CHOICE-COMPLETION-STRING (CAR L))))
               (RETURN)
               ;;ELSE
               (WHEN (OR (ATOM L) (NOT (STRINGP (CAR L))))
                 (FERROR () "~S garbage in MARGIN-CHOICES" L))))
  (SETQ MARGIN-CHOICES (MAPCAR #'(LAMBDA (X &aux doc)
				   (LIST (IF (ATOM X) X (CAR X)) NIL
					 'CHOOSE-VARIABLE-VALUES-CHOICE-BOX-HANDLER
					 NIL NIL
					 ;; may 9-14-88 added missing doc strings
					 ;; X can be keyword, list or (ugh!) cons - per para 14.2.6.3 & 14.2.1
					 (IF (SETQ doc (AND (CONSP x)      ;; not :keyword
							    (CONSP (CDR x));; not (cons 'a 'b)
							    (getf x :documentation)))
					     :documentation nil)
					 doc
					 ;; end patch may 9-14-88 below is now EIGHTH in/of SIXTH in list
					 (IF (ATOM X) NIL (CADR X))))
			       MARGIN-CHOICES))
  ;; Handle variable sublists too.
  (DOLIST (ITEM VARIABLES) 
    ;; Make sure all variables are bound, while in caller's environment.
    (DOLIST (ELEM (IF (AND (CONSP ITEM) (CONSP (CAR ITEM))) ITEM (LIST ITEM)))
      (IF (CONSP ELEM)
          (SETQ ELEM (CAR ELEM)))
      (COND ((EQ (DATA-TYPE ELEM) 'DTP-LOCATIVE) (SETQ ELEM (CAR ELEM)))     ; Force compiler
            ((SYMBOLP ELEM) (SYMBOL-VALUE ELEM))
            ((STRINGP ELEM))
            ((AND (INTEGERP ELEM) (CONSP ITEM) (CONSP (CAR ITEM))))
            (T (FERROR () "~S is a ~S Bad data type for variable" ELEM (DATA-TYPE ELEM))))))
  ;; The following code is very contorted.  The most recent change (22-MAR-85) was done to cause the CVV
  ;; resource to be cleared when the user aborts out.  Previously, the resource was kept hanging around and
  ;; caused CVV to look broken when the user would reinvoke it.  The fix was to use ALLOCATE-RESOURCE to
  ;; acquire the CVV resource and have an UNWIND-PROTECT to deallocate the resource.  If the user aborts,
  ;; the resource gets deallocated and then cleared and execution continues.  If the user exits normally, the
  ;; resource gets deallocated only.
  (LET (WINDOW                                  ; The CVV resource
        ;; NIL for the ABORT key case, 'EXIT when the user presses the END key, and possibly
        ;; something else for a different margin choice.
        (PROCESSED-MESSAGE NIL))
    (UNWIND-PROTECT
        (LET ((current-window (IF (SEND superior :operation-handled-p :set-selection-substitute)
				   superior
				   (OR osw mouse-sheet)))
	      (OLD-SUBSTITUTE (SEND SUPERIOR :SELECTION-SUBSTITUTE)))
          (SETQ WINDOW (ALLOCATE-RESOURCE 'TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW SUPERIOR))
	  ;; Close your eyes before you read the following form (HACK ALERT!!!).
          (IF FORCE-PERMANENT
              (SET-IN-INSTANCE WINDOW 'TEMPORARY-BIT-ARRAY NIL))
          (SEND WINDOW :SET-VALUE-TAB VALUE-TAB)
          (SEND WINDOW :SET-REVERSE-VIDEO-P REVERSE-VIDEO-P)
          (SEND WINDOW :SETUP VARIABLES LABEL FUNCTION MARGIN-CHOICES (OR WIDTH T) EXTRA-WIDTH)
          
	  ;; Make sure that we don't try to expose outside of our superior.

          (SEND window :set-size   
		(MIN (SEND superior :inside-width)  (SEND window :width))
		(MIN (SEND superior :inside-height) (if height   
							(max height (send window :height)) 
							;;else 
							(send window :height))))
	  (when (color-system-p window)
	    (send window :set-foreground-color foreground-color)
	    (send window :set-background-color background-color)
	    (send window :set-label-color label-color)
	    (send window :set-label-background label-background)
;	    (setf (label-font (send window :label))  fonts:cptfontb) ;; may 02/02/89 
	    )
          (UNWIND-PROTECT
              (PROGN
                (CLEAR-INPUT WINDOW)
                (DELAYING-SCREEN-MANAGEMENT
                  (EXPOSE-WINDOW-NEAR WINDOW NEAR-MODE)
                  (SEND WINDOW :SELECT)  
		  (SEND current-window :send-if-handles :set-selection-substitute window))
                (DO () (NIL)
		  ;; Wait for something from the keyboard.
                  (LET ((KBD-INTERCEPTED-CHARACTERS CHOOSE-VARIABLE-VALUES-INTERCEPTED-CHARACTERS))
                    (PROCESS-WAIT "Choose" #'LISTEN WINDOW)
                    (AND (SETQ PROCESSED-MESSAGE (CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE WINDOW (READ-ANY WINDOW)))
                         (RETURN)))))
            (DELAYING-SCREEN-MANAGEMENT
              (SEND WINDOW :DEACTIVATE)
              (SEND current-window :send-if-handles :set-selection-substitute old-substitute)
              (AND OSW (SEND OSW :SELECT NIL)))))
      (DEALLOCATE-RESOURCE 'temporary-choose-variable-values-window window)
      (WHEN (EQ PROCESSED-MESSAGE 'ABORT)
	;; The user did something funny.  Clear the resource so that subsequent CVV calls will still work. 
	;; The NIL argument to CLEAR-RESOURCE says to not print any warning message.
	(CLEAR-RESOURCE 'TEMPORARY-CHOOSE-VARIABLE-VALUES-WINDOW WINDOW NIL)))
    ;; Make sure that pressing the END key is the same as clicking on the DOIT box by
    ;; executing the same code.  Likewise for the pressing the ABORT key and clicking
    ;; on the ABORT box.  By implementing these two keys this way we make it so that
    ;; the user doesn't have to put in special code to cause these two to be handled
    ;; like their margin choice companions.  Also, if the user did implement the special
    ;; code as recommended in the release 2 Window System manual then it will still work.
    (IF (EQ PROCESSED-MESSAGE 'EXIT)
      (EXECUTE-MARGIN-CHOICE MARGIN-CHOICES W:MARGIN-CHOICE-COMPLETION-STRING #'IGNORE)
      ;;ELSE
      (IF (EQ PROCESSED-MESSAGE 'ABORT)
          (EXECUTE-MARGIN-CHOICE MARGIN-CHOICES W:MARGIN-CHOICE-ABORT-STRING
				 #'(LAMBDA () (SIGNAL-CONDITION EH:*ABORT-OBJECT*)))))))


;;; Modified to handle character input in a general way
(DEFUN CHOOSE-VARIABLE-VALUES-PROCESS-MESSAGE
       (WINDOW MSG)
  "Returns T if message is EXIT, else does variable-changing or special
action and returns NIL.  msg is either a list that came in whose cadr is
this window, or it is a regular character :process-character is called.
WINDOW is made special so choice-box functions can find it to refresh
variables."
  (BLOCK NIL
    (COND ((CONSP MSG)
           (CASE (CAR MSG)
                 (:CHOICE-BOX
                  (SETQ MSG (eighTH (THIRD MSG))) ;NIL if done or form to eval  ;; may 9-14-88 was SIXTH
                  (IF (NULL MSG)
                      (RETURN T)
                      (EVAL MSG)))
                 (:VARIABLE-CHOICE
                  (APPLY #'CHOOSE-VARIABLE-VALUES-CHOICE (CDR MSG))
		  (RETURN NIL))
                 (OTHERWISE                     ; Ignore extraneous mouse blips
                  (RETURN NIL))))
          ((CHARACTERP MSG)
           (RETURN (PROGN
                     (SETQ MSG (SEND WINDOW :PROCESS-CHARACTER MSG))
                     (IF (OR (EQ 'EXIT MSG) (EQ 'ABORT MSG))
                         MSG
                         ;;ELSE
                         NIL)))))))

(DEFPARAMETER CHOOSE-VARIABLE-VALUES-DEFAULT-COMMAND-CHARACTERS
	      '((#\PAGE       (:REFRESH) "Refresh the screen")
	       (#\RETURN      (:MOVE-CURSOR NEXT           ) "Move the cursor to the next item")
	       (#\LINEFEED    (:RETURN-HACK                ) "Move the cursor down. Exit at end of last item")
	       (#\RUBOUT      () "Press Rubout on a blank field to restore value")
               (""            (                            ) "")
	       (#\CONTROL-P   (:MOVE-CURSOR UP             ) "Move the cursor up")
	       (#\CONTROL-N   (:MOVE-CURSOR DOWN           ) "Move the cursor down")
	       (#\CONTROL-F   (:MOVE-CURSOR NEXT           ) "Move the cursor right")
	       (#\CONTROL-B   (:MOVE-CURSOR PREVIOUS       ) "Move the cursor left")
	       (#\CONTROL-A   (:MOVE-CURSOR FIRST          ) "Move the cursor to the beginning of the line")
	       (#\CONTROL-E   (:MOVE-CURSOR LAST           ) "Move the cursor to the end of the line")
               (""            (                            ) "")
	       (#\UP-ARROW    (:MOVE-CURSOR UP             ) "Move the cursor up")
	       (#\DOWN-ARROW  (:MOVE-CURSOR DOWN           ) "Move the cursor down")
	       (#\RIGHT-ARROW (:MOVE-CURSOR NEXT           ) "Move the cursor right")
	       (#\LEFT-ARROW  (:MOVE-CURSOR PREVIOUS       ) "Move the cursor left")
               (""            (                            ) "")
	       (#\ABORT       (:EVAL-INSIDE-YOURSELF 'ABORT) "Exit abnormally")
	       (#\END         (:EVAL-INSIDE-YOURSELF 'EXIT ) "Exit normally")
	       (#\HELP        (:HELP                       ) "Print this text")
               (""            (                            ) "")
	       ("Anything Else" (                          ) "Selects that item"))
	      "An alist of (character (method-name . argument-list) documentation-string
used by basic-choose-variable-values-windows.  If the METHOD-NAME is NIL
then the other items are present for documentation purposes.  For example,
RUBOUT is handled by the rubout-handler, but is described here to tell the user
about it.  If the CHARACTER is actually a string, then it is displayed for
documentation purposes only.  If the CHARACTER is really a character, and if
the METHOD-NAME isn't NIL, then the method is called with ARGUMENT-LIST")

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :RETURN-HACK) ()
  "Go to next item, unless it's the last one. Then we exit"
  (LET ((LINE-NO (SHEET-LINE-NO () (SEND ITEM-BLINKER :Y-POS)))
	(MAX-LINE (ARRAY-ACTIVE-LENGTH ITEMS)))
    (IF (= LINE-NO (1- MAX-LINE)) 'EXIT
        (SEND SELF :MOVE-CURSOR 'DOWN))))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :HELP)
           (&OPTIONAL (HELP-ALIST
                        (APPEND COMMAND-CHARACTERS
                                CHOOSE-VARIABLE-VALUES-DEFAULT-COMMAND-CHARACTERS)))
  (USING-RESOURCE (WINDOW POP-UP-FINGER-WINDOW)
    (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 0)
    (FUNCALL WINDOW :SET-LABEL "Choose variable values commands")
    (SEND WINDOW :SET-PROCESS CURRENT-PROCESS)
    (WINDOW-CALL (WINDOW :DEACTIVATE)
      (LET ((*TERMINAL-IO* WINDOW))               ;In case of [Abort] printout and the like
        ;; Window configuration stable now, let keyboard process proceed.
        (SETQ KBD-TERMINAL-TIME NIL)
        (FORMAT WINDOW "~%The following command characters are defined:~%")
        (DOLIST (ELEMENT HELP-ALIST)
          (IF (OR (NUMBERP (CAR ELEMENT)) (CHARACTERP (CAR ELEMENT)))
              (FORMAT WINDOW "~{~%  ~20<~:C~>  ~*~A~}" ELEMENT)
              ;;ELSE
              (FORMAT WINDOW "~{~%  ~20<~A~>  ~*~A~}" ELEMENT)))
        (FORMAT WINDOW "~%~%~A" *REMOVE-TYPEOUT-STANDARD-MESSAGE*)
        ;; We are doing a key-state instead of a :TYI because if we are in
        ;; the rubout handler then it interferes with the :TYI.  We will be
        ;; in the rubout handler when the user types in a partial input
        ;; for an item and then presses the HELP key.
        (PROCESS-WAIT *DEFAULT-READ-WHOSTATE* #'KEY-STATE #\SPACE)
        (CLEAR-INPUT WINDOW)))))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :PROCESS-CHARACTER) (CH &AUX COMMAND)
  "Process command characters typed at top level.  If an unknown
character is entered, untyi it and call choose-variable-values-choice"
  (COND ((SETQ COMMAND (ASSOC CH COMMAND-CHARACTERS :TEST #'EQ))
         (LEXPR-SEND SELF (SECOND COMMAND)))
        ((SETQ COMMAND (ASSOC CH CHOOSE-VARIABLE-VALUES-DEFAULT-COMMAND-CHARACTERS :TEST #'EQ))
         ;; If there isn't any method then beep.
         (IF (SECOND COMMAND)
             (LEXPR-SEND SELF (SECOND COMMAND))
             ;;ELSE
             (BEEP)))
        ((ZEROP (CHAR-BITS CH))
         (LET (VALUE TYPE LEFT RIGHT Y LINE-NO)
           (MULTIPLE-VALUE-SETQ (VALUE TYPE LEFT RIGHT)
                                (SEND SELF :MOUSE-SENSITIVE-ITEM (SEND ITEM-BLINKER :X-POS)
                                      (SETQ Y (SEND ITEM-BLINKER :Y-POS))))
           (WHEN (EQ TYPE :VARIABLE-CHOICE)
             (UNREAD-CHAR CH SELF)
             (SETQ LINE-NO (SHEET-LINE-NO NIL Y))
             (CHOOSE-VARIABLE-VALUES-CHOICE
               SELF (CDR VALUE) (CAR VALUE) LINE-NO
               (- LEFT LEFT-MARGIN-SIZE) (- RIGHT LEFT-MARGIN-SIZE))
             ;; KLUDGE In case untyi'ed character wasn't eaten.  This
             ;; will happen for those items that don't take character
             ;; data as input.  For example, a :CHOOSE item.
             (SETQ CH (READ-CHAR-NO-HANG SELF)))))
        (T (BEEP))))

(DEFUN CIRCULAR-LIMIT (NUMBER MAX &OPTIONAL (MIN 0))
  "Limit number to be less than max and greater than or equal
to min.  If outside, wrap around."
  (SETQ NUMBER (REM NUMBER MAX))
  (COND ((>= NUMBER MAX) MIN)
        ((< NUMBER MIN) (1- MAX))
        (NUMBER)))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :MOVE-CURSOR) (DIRECTION)
  "Move the mouse cursor UP, DOWN, LEFT, RIGHT, START, END, PREVIOUS, or NEXT.
If direction is NIL, just move it to the nearest value."
  (LET ((LINE-NO (SHEET-LINE-NO NIL (SEND ITEM-BLINKER :Y-POS)))
	(MAX-LINE (ARRAY-ACTIVE-LENGTH DISPLAYED-ITEMS))
	ITEM-NUMBER NEW-ITEM)
    (CASE DIRECTION
          ((LEFT RIGHT NEXT PREVIOUS)
           (LET ((ENTRY (AREF DISPLAYED-ITEMS LINE-NO))
                 ;; Note: We are adding half of the blinker width to the X position to
                 ;; get around a mouse sensitivity problem which happens with ASSOC
                 ;; items.  When one item is selected, the X position of others may
                 ;; change.  By trying to hit the middle of an item we are almost
                 ;; guaranteed to hit it.  Previously it was off oh so slightly (but
                 ;; too much) causing the mouse sensitive item to not be found.
                 (ITEM (SEND SELF :MOUSE-SENSITIVE-ITEM (+ (SEND ITEM-BLINKER :X-POS)
                                                           (TRUNCATE (SEND ITEM-BLINKER :SIZE) 2))
                             (SEND ITEM-BLINKER :Y-POS)))
                 N)
             (WHEN ITEM
               (SETQ N (POSITION (ASSOC ITEM ENTRY :TEST #'EQ) (THE LIST ENTRY) :TEST #'EQ))
               (SETQ ITEM-NUMBER (CIRCULAR-LIMIT (IF (MEMBER DIRECTION '(LEFT PREVIOUS) :TEST #'EQ)
                                                     (1+ N)
                                                     ;;ELSE
                                                     (1- N))
                                                 (LENGTH ENTRY))
                     NEW-ITEM T)
               (COND ((AND (EQ DIRECTION 'NEXT) (= ITEM-NUMBER (1- (LENGTH ENTRY))))
                      (SEND SELF :MOVE-CURSOR 'DOWN))
                     ((AND (EQ DIRECTION 'PREVIOUS) (ZEROP ITEM-NUMBER))
                      (SEND SELF :MOVE-CURSOR 'UP))
                     (T (SETQ NEW-ITEM (NTH ITEM-NUMBER ENTRY)))))))
          ((FIRST LAST)
	   ;; changed to not be an infinite loop with no items
           (dotimes (count max-line (setq new-item nil)) 
             (WHEN (SETQ NEW-ITEM (AREF DISPLAYED-ITEMS LINE-NO))
               ;;Maybe we should do something fancier someday.
               (SETQ NEW-ITEM (IF (EQ DIRECTION 'FIRST)
                                  (CAR (LAST NEW-ITEM))
                                  ;;ELSE
                                  (FIRST NEW-ITEM)))
               (RETURN))))
          ((UP DOWN NIL)
	   ;; changed to not be an infinite loop with no items
           (dotimes (count max-line (setq new-item nil))
             (SETQ LINE-NO (CIRCULAR-LIMIT (IF (EQ DIRECTION 'UP)
                                               (1- LINE-NO) (1+ LINE-NO))
                                           MAX-LINE))
             (UNLESS DIRECTION (SETQ DIRECTION 'DOWN))
             (WHEN (SETQ NEW-ITEM (AREF DISPLAYED-ITEMS LINE-NO))
               ;;Maybe we should do something fancier someday.
               (SETQ NEW-ITEM (IF (EQ DIRECTION 'DOWN)
                                  (CAR (LAST NEW-ITEM)) (FIRST NEW-ITEM)))
               (RETURN))
	     )
	   )
          (OTHERWISE (FERROR NIL "Invalid direction ~S" DIRECTION)))
    (COND ((NULL NEW-ITEM) (BEEP))
          ((NEQ NEW-ITEM T)
           (SEND SELF :SET-MOUSE-CURSORPOS
                 (- (DISPLAYED-ITEM-RIGHT NEW-ITEM) LEFT-MARGIN-SIZE 1)
                 (* LINE-NO LINE-HEIGHT))))))

(DEFMETHOD (BASIC-CHOOSE-VARIABLE-VALUES :AFTER :SELECT) (&REST IGNORE)
  (LET ((BLINKERS (SEND SELF :BLINKER-LIST )))
   (DOLIST (BLINKER-OBJECT BLINKERS)
     (OR (WHEN (TYPEP BLINKER-OBJECT 'HOLLOW-MARGIN-CHOICE-BLINKER)
       (DEALLOCATE-RESOURCE 'HOLLOW-STRING-BOX-BLINKER BLINKER-OBJECT )
       (SEND BLINKER-OBJECT :SET-VISIBILITY :OFF))
     (WHEN (TYPEP BLINKER-OBJECT 'RECTANGULAR-MARGIN-CHOICE-BLINKER)
       (DEALLOCATE-RESOURCE 'STRING-BOX-BLINKER BLINKER-OBJECT )
       (SEND BLINKER-OBJECT :SET-VISIBILITY :OFF)))))
  (SEND SELF :MOVE-CURSOR ()))
