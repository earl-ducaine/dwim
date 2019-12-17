;;; -*- Mode:common-lisp; Package:TV; Base:10.; Fonts:CPTFONT,HL12B,HL12BI -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;   3/04/87  TWE	Made the menu-choose function use the old menu flavors.
;;;   2/24/87  TWE	Changed menu-deduce-parameters to calculate the max-height and max-width much
;;;			like it did in the old system 98 days, using a constant of 4096 instead of 512.  This
;;;			fixes bug 3011.  Also fixed up the make-obsolete string for menu-compute-font-map
;;;			to refer to menu-compute-font-map instead of menu-compute-row-map.
;;;   2/03/87  KDB	Removed call to DELAYING-COMPUTE-GEOMETRY from MULTIPLE-MENU-CHOOSE to fix
;;;                          release 3 bug report  2990.  
;;; 12/22/86   TWE	Made several changes to make the build process cleaner.
;;;			Added a :REQUIRED-FLAVORS BASIC-MENU clause to the MULTIPLE-MENU-MIXIN flavor
;;;			so that its methods could access more instance variables.
;;; 09/17/86   TWE	Fixed the exploding menu code to use truncate instead of /.  The / function will often
;;;			generate rational numbers if the arguments are fixnums, generating an error later on.
;;; 08/21/86   TWE	Fixed the use of make-obsolete for menu-margin-choice to not obsolete defun.
;;; 08/13/86   TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 07/28/86   TWE	Modified references to the pixel functions to use ARRAY-DIMENSION
;;;			and MAKE-ARRAY instead.
;;; 06/20/86   TWE	Changed calls to REDIRECT-ARRAY to use array-element-type instead of
;;;			array-type.  This macro was changed to use the adjust-array function
;;;			which needs the element type of the array instead of just its type.
;;; 04/08/86   LGO	Make FONT-EVALUATE inline for MENU-ITEM-STRING-WIDTH
;;; 04/08/86   LGO	Added DELAYING-COMPUTE-GEOMETRY macro call to MENU-CHOOSE,   ;; WHERE IS IT DEFINED?
;;;			MULTICOLUMN-MENU-CHOOSE, and MULTIPLE-MENU-CHOOSE to prevent the
;;;			geometry from being computed twice, once for the label and once for the item-list.
;;; 04/08/86   LGO	Remove call to MENU-COMPUTE-GEOMETRY from (:METHOD BASIC-MENU :SET-DEFAULT-FONT)
;;; 01/29/86   GRH	Converted to Common-lisp.  Note that this file uses the
;;;			Zetalisp EVAL to evaluate some various parameters.  This
;;;			should be changed to the Common-lisp EVAL when the rest 
;;;			of the Explorer software that uses the window system is
;;;			converted to Common-lisp.

;;; New menu system

;Documentation on menu item-lists:
;
;Each item in the item-list may be one of the following:
; 1. A string (or a symbol).  
; 2. Cons of a string (or a symbol) and an atom.
; 3. List of a string (or a symbol) and any object.  The list may
;    not be more than 2 long.
; 4. List of a string (or a symbol), a flavor keyword, and an argument.
;    After the first 3 elements of the list, the rest of the list is
;    a property list of modifier keywords and values.
;The string (or symbol) is displayed in the menu to represent this item.
;The value returned by the :CHOOSE method is the item in case 1, the cdr
;in case 2, the cadr in case 3, and varies in case 4 depending on the flavor.
;Case 4 menu items can also have side-effects.
;The following are the permissible flavor keywords:
;		:VALUE - argument is returned by the :CHOOSE method
;		:EVAL - argument is evaluated then returned
;		:FUNCALL - argument is a function of no args to be called
;		:NO-SELECT - this item cannot be selected
;		:WINDOW-OP - argument is a function of one argument.  This
;			argument is a list of window, mouse-x, mouse-y as they
;			were before the menu popped up.
;		:KBD - argument is forced into keyboard input of appropriate process.
;		:MENU - argument is a new menu to choose from.
;		:MENU-CHOOSE - arg is a list (label . menu-choose-alist)
;			passed to MENU-CHOOSE
;		:BUTTONS - argument is 3 items, which one is used depends on
;			which mouse button was clicked
;The following are the known modifier keywords:
;		:FONT - the font in which to display the item
;		:DOCUMENTATION - a string documenting this item
;		:BINDINGS - a LET-list of bindings to do with PROGW
;			before funcalling, evalling, etc.
;			With :BUTTONS, the :BINDINGS must be INSIDE the :BUTTONS.
;This stuff is largely although not entirely controlled by the :EXECUTE method,
;which you may redefine.

;;; MENU-EXECUTE-MIXIN flavor processes a menu-like item

(DEFFLAVOR MENU-EXECUTE-MIXIN () ()
  (:DOCUMENTATION :MIXIN "Processes a menu-like item
This is a part of every menu, it is a separate flavor so that it can be included in other
things which want to act like menus with regard to the format of an item passed to a
:execute message.  This message is what handles most of the interpretation of the
item-list instance variable."))

;;; Decide what to return based on the item selected.  Also have side-effects
;;; such as calling a function if the item says to.
(DEFMETHOD (MENU-EXECUTE-MIXIN :EXECUTE) (ITEM &AUX OP ARG)
  (COND
    ((ATOM ITEM) ITEM)
    ((ATOM (CDR ITEM)) (CDR ITEM))
    ((ATOM (CDDR ITEM)) (CADR ITEM))
    (T
     (LET ((BINDINGS (GETF (CDDDR ITEM) :BINDINGS)))
       (PROGW BINDINGS (SETQ ARG (CADDR ITEM) OP (CADR ITEM))
              (RETURN
               (CASE OP
                 (:VALUE ARG)
                 (:EVAL (GLOBAL:EVAL ARG))
                 (:FUNCALL (FUNCALL ARG))
                 (:FUNCALL-WITH-SELF (FUNCALL ARG SELF))
                 (:WINDOW-OP (SEND SELF :EXECUTE-WINDOW-OP ARG))
                 (:KBD (AND SELECTED-WINDOW (FUNCALL SELECTED-WINDOW :FORCE-KBD-INPUT ARG)))
                 (:MENU (FUNCALL (GLOBAL:EVAL ARG) :CHOOSE))
                 (:MENU-CHOOSE
                  (LET (RESULT
                        DONE)
                    (PROCESS-RUN-FUNCTION
		      "Menu"
		      #'(LAMBDA (ARG BINDINGS RESULT-LOC DONE-LOC)
			  (PROGW BINDINGS
			    (UNWIND-PROTECT
				(SETF (CDR RESULT-LOC)
				      (MENU-CHOOSE (CDR ARG)
						   (CAR ARG)))
			      (SETF (CDR DONE-LOC) T))))
		      ARG BINDINGS (LOCF RESULT) (LOCF DONE))
                    (OR (EQ CURRENT-PROCESS MOUSE-PROCESS)
                        (PROCESS-WAIT "Menu" #'CDR (LOCF DONE)))
                    RESULT))
                 (T (FERROR () "~S is unknown operation for :EXECUTE" OP)))))))))    

;Same as above but returns NIL if getting the value would require side-effects.
;This is used by MENU-HIGHLIGHTING-MIXIN
(DEFMETHOD (MENU-EXECUTE-MIXIN :EXECUTE-NO-SIDE-EFFECTS) (ITEM)
  (MENU-EXECUTE-NO-SIDE-EFFECTS ITEM))

(DEFMETHOD (MENU-EXECUTE-MIXIN :EXECUTE-WINDOW-OP) (FUNCTION) (FUNCALL FUNCTION))

(DEFFLAVOR BASIC-MENU
	   ((ITEM-LIST nil)		;List of items being displayed. -- initially nil -- PDC  2 Jan 86
	    CURRENT-ITEM		;Item being pointed at now.
	    LAST-ITEM			;The last item to have been selected.
	    (CHOSEN-ITEM NIL)		;The same, but it's ok to set this to NIL
					;and wait for it to become non-NIL.
	    SCREEN-ROWS			;Number of rows in menu on screen
	    TOTAL-ROWS			;Total number of rows in menu.
					;If this is greater than SCREEN-ROWS, then the latter
					;represent a window on all the rows.
	    TOP-ROW			;This is first row visible now.
	    ROW-HEIGHT			;Height in dots of a row (including vsp).
	    ROW-MAP			;Array of tails of ITEM-LIST.  For each row
					;in the menu, points to first item on that row.
					;An extra element at the end is NIL.
					;The length is thus (1+ TOTAL-ROWS).
	    (COLUMNS NIL)		;Number of columns (NIL in fill mode).
	    COLUMN-WIDTH		;Width in dots of a column (NIL in fill mode).

	    ;; GEOMETRY is the user specified geometry.  It is a list of:
	    ;; Number of columns or 0 if FILL-P, number of rows, inside width, inside height,
	    ;; maximum width, maximum height.  NIL means it's free to change, as was not
	    ;; explicitly specified by the user.  Default is to leave everything free.
	    (GEOMETRY
	     (LIST NIL NIL NIL NIL NIL NIL))
	    (LAST-INSIDE-HEIGHT NIL)		;The height and width we used the last time
	    (LAST-INSIDE-WIDTH NIL)		;we computed the row map.
	    DEFAULT-FONT
	   )
	   (MENU-EXECUTE-MIXIN)
  (:REQUIRED-FLAVORS SHEET BASIC-SCROLL-BAR)
  (:GETTABLE-INSTANCE-VARIABLES ITEM-LIST CURRENT-ITEM LAST-ITEM CHOSEN-ITEM GEOMETRY)
  (:SETTABLE-INSTANCE-VARIABLES LAST-ITEM CHOSEN-ITEM)
  (:INITABLE-INSTANCE-VARIABLES ITEM-LIST DEFAULT-FONT)
  (:INIT-KEYWORDS :ROWS :COLUMNS :FILL-P :GEOMETRY)  ;Set parts of geometry
  (:DEFAULT-INIT-PLIST :BLINKER-FLAVOR 'TV:HOLLOW-RECTANGULAR-BLINKER)
  (:DOCUMENTATION :MIXIN "Regular menu messages
Provides methods and instance variables common to all menus, such as the item-list,
the geometry hacking, a default :choose message, and a scroll bar if necessary."))


(DEFFLAVOR MENU ((LABEL NIL))
		(BASIC-MENU BORDERS-MIXIN TOP-BOX-LABEL-MIXIN BASIC-SCROLL-BAR MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "The simplest instantiatable menu.
Defaults to not having a label, a label whose position is not initially specified will
be at the top, in a small auxiliary box, unlike most windows."))

(DEFFLAVOR TEMPORARY-MENU () (Temporary-Shadow-Borders-Window-Mixin MENU)
  (:DOCUMENTATION :COMBINATION "A menu that is temporary
This is not a momentary menu, it must be exposed and deexposed normally, it does save
the state beneath itself when exposed."))

(DEFFLAVOR POP-UP-MENU () (TEMPORARY-MENU) :ALIAS-FLAVOR)

(COMPILER:MAKE-OBSOLETE MENU-ITEM-STRING-WIDTH "Use W:MENU-ITEM-STRING-WIDTH instead")
(DEFUN MENU-ITEM-STRING-WIDTH (ITEM &OPTIONAL STOP-X)
  "Return the width in pixels of the way item ITEM is displayed on the menu.
If STOP-X is specified, we will not return a value larger than that."
  (DECLARE (:SELF-FLAVOR BASIC-MENU)
	   (inline font-evaluate))
  (MULTIPLE-VALUE-BIND (STRING FONT)
      (MENU-ITEM-STRING ITEM (FONT-EVALUATE CURRENT-FONT) SELF)
    (SHEET-STRING-LENGTH SELF STRING 0 NIL STOP-X FONT)))

(COMPILER:MAKE-OBSOLETE MENU-MAX-WIDTH "Use W:MENU-MAX-WIDTH instead")
(DEFUN MENU-MAX-WIDTH (ITEM-LIST)
  "Return the maximum width in pixels of any item in ITEM-LIST.
Normally you should add allowances for interword spacing to this."
  (LOOP FOR L IN ITEM-LIST
        MAXIMIZE (MENU-ITEM-STRING-WIDTH L)))

(COMPILER:MAKE-OBSOLETE MENU-COMPUTE-FONT-MAP "Use W:MENU-COMPUTE-FONT-MAP instead")
(DEFUN MENU-COMPUTE-FONT-MAP (ITEMS &AUX (MAP (CONS DEFAULT-FONT ())) FONT)
  "Compute the font map to use for a menu, from an item list.
The font map we compute has all the fonts any items need."
  (DECLARE (:SELF-FLAVOR BASIC-MENU))
  (DOLIST (ITEM ITEMS)
    (SETQ FONT (AND (CONSP ITEM)
		    (CONSP (CDR ITEM))
		    (GET (CDDR ITEM) :FONT)))
    (AND FONT
	 (NOT (MEMBER FONT MAP :TEST #'EQ))
	 (PUSH FONT MAP)))
  (NREVERSE MAP))    

(COMPILER:MAKE-OBSOLETE MENU-COMPUTE-GEOMETRY "Use W:MENU-COMPUTE-GEOMETRY instead")
(DEFUN MENU-COMPUTE-GEOMETRY (DRAW-P &OPTIONAL INSIDE-WIDTH INSIDE-HEIGHT)
  "This function is called whenever something related to the geometry changes.
INSIDE-WIDTH and INSIDE-HEIGHT optionally specify the shape to use,
if we are recomputing the row layout but want to use a particular shape,
e.g. if the menu has been reshaped by the user.
The menu is redrawn if DRAW-P is T.  In any case, we do everything necessary
to adapt the menu to the current item-list and the optionally specified size"
  (DECLARE (:SELF-FLAVOR BASIC-MENU))
  (COND
    (*delay-compute-geometry*)			;Do nothing if computations are delayed
    ((VARIABLE-BOUNDP ITEM-LIST)		;Do nothing if item-list not specified yet
     ;; Get the new N-ROWS and so forth.
     (MULTIPLE-VALUE-SETQ (COLUMNS SCREEN-ROWS INSIDE-WIDTH INSIDE-HEIGHT)
			  (MENU-DEDUCE-PARAMETERS nil nil INSIDE-WIDTH INSIDE-HEIGHT nil nil))
     ;; Recompute the row map
     (MULTIPLE-VALUE-SETQ (ROW-MAP TOTAL-ROWS)
			  (MENU-COMPUTE-ROW-MAP INSIDE-WIDTH))
     (SETQ TOP-ROW 0 ROW-HEIGHT LINE-HEIGHT) (SEND SELF :NEW-SCROLL-POSITION TOP-ROW)
     (SETQ COLUMN-WIDTH
           (AND (NOT (GEOMETRY-FILL-P GEOMETRY))
                (TRUNCATE (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING) COLUMNS)))
     (SETQ LAST-INSIDE-WIDTH INSIDE-WIDTH LAST-INSIDE-HEIGHT INSIDE-HEIGHT)
     (COND
       ((AND (= INSIDE-HEIGHT (SHEET-INSIDE-HEIGHT))
             (= Inside-WIDTH (SHEET-INSIDE-WIDTH)))
        (AND DRAW-P (SHEET-FORCE-ACCESS (SELF :NO-PREPARE) (SEND SELF :MENU-DRAW))))
       ((SEND SELF :SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT :VERIFY)
        ;; Room to do this in current place.
        (SEND SELF :SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT :TEMPORARY))
       (T
        ;; Else try to be approximately in the same place
        (LET ((CX (+ X-OFFSET (TRUNCATE WIDTH 2)))
              (CY (+ Y-OFFSET (TRUNCATE HEIGHT 2))))
          (WITH-SHEET-DEEXPOSED (SELF)
	    (SEND SELF :SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT
		  :TEMPORARY)
	    (CENTER-WINDOW-AROUND SELF CX CY)))))))
  nil)

;;; This function, given a bunch of parameters some of which are NIL meaning
;;; unspecified, deduces the rest of the parameters from constraints.
;;; For parameters passed in as NIL, the corresponding element of GEOMETRY
;;; is used.

;;; First, compute the geometry
;;;  (1) The user has supplied the width and the number of columns or fill-p, nothing special.
;;;  (2) The user has supplied the width, we compute the number of columns
;;;             by finding the widest string in the item-list.
;;;  (3) The user has not supplied the width, but has supplied n-columns, we compute width
;;;             again by finding the widest string in the item-list.
;;;  (4) The user has supplied neither width nor n-columns.
;;;    (4a) The user has, however, supplied height or n-rows, so we pick a suitable width
;;;         to make the entire menu come out to n-rows, depending on fill mode.  Then if
;;;         it doesn't fit, this width will be wider than the screen, and will be limited.
;;;    (4b) The user has supplied no geometry, it's up to us.
;;;         Compute the total width depending on fill-mode, then pick n-rows and
;;;         n-columns to make this a square array.  Then limit each to the available
;;;         area of the screen, in case the menu is too big to fit.
;;;	    Not actually square but the prettiest looking shape.

;;; Once the horizontal business has been straightened out, if we don't have the
;;; height already, we pick a height to make it all fit on the screen, and limit that
;;; if it is too big.  Note that fill-mode has a line-breakage problem, which will
;;; need to be solved here (may change shape "slightly" from square.)

;;; Arguments:
;;; SELF, ITEM-LIST and GEOMETRY are used freely.
;;; SELF should have the right font, screen, vsp but not
;;;	  yet the right dimensions and location.
(COMPILER:MAKE-OBSOLETE MENU-DEDUCE-PARAMETERS "Use W:MENU-DEDUCE-PARAMETERS instead")
(DEFUN MENU-DEDUCE-PARAMETERS (N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT
			       MAX-WIDTH MAX-HEIGHT
			       &AUX TEM COL-WIDTH ;NIL if N-COLUMNS not chosen in here
                               (N-ITEMS (LENGTH ITEM-LIST))
                               FILL-P MIN-WIDTH)
  "Compute or default unspecified menu geometry parameters from the specified ones.
Each parameter can be specified as a number, can be :UNCONSTRAINED meaning
compute it from the others, or can be NIL meaning use the corresponding
PERMANENT parameter which is an element of GEOMETRY.
The specified parameters, together with the item list, default font and vsp,
are used to compute the ones that were not specified.
If there is not enough information to determine things, pretty choices are made.
The values have the same meanings as the first four arguments,
except that all four values will always be numbers.

N-COLUMNS = 0 means a FILLED menu is desired, where different rows
can have different number of items."
  (DECLARE (:SELF-FLAVOR BASIC-MENU)
	   (VALUES N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT))

  ;; Pick up default constraints from GEOMETRY
  (SETQ N-COLUMNS (OR N-COLUMNS (GEOMETRY-N-COLUMNS GEOMETRY))
	N-ROWS (OR N-ROWS (GEOMETRY-N-ROWS GEOMETRY))
	INSIDE-WIDTH (OR INSIDE-WIDTH (GEOMETRY-INSIDE-WIDTH GEOMETRY))
	INSIDE-HEIGHT (OR INSIDE-HEIGHT (GEOMETRY-INSIDE-HEIGHT GEOMETRY))
	MAX-WIDTH (OR MAX-WIDTH (GEOMETRY-MAX-WIDTH GEOMETRY))
	MAX-HEIGHT (OR MAX-HEIGHT (GEOMETRY-MAX-HEIGHT GEOMETRY)))

  ;; If any of the arguments was :UNCONSTRAINED, that means use NIL
  ;; even if the geometry is non-NIL, whereas if an argument was NIL
  ;; that means use any constraint that is in the geometry.
  (AND (EQ N-COLUMNS :UNCONSTRAINED) (SETQ N-COLUMNS NIL))
  (AND (EQ N-ROWS :UNCONSTRAINED) (SETQ N-ROWS NIL))
  (AND (EQ INSIDE-WIDTH :UNCONSTRAINED) (SETQ INSIDE-WIDTH NIL))
  (AND (EQ INSIDE-HEIGHT :UNCONSTRAINED) (SETQ INSIDE-HEIGHT NIL))
  (AND (EQ MAX-WIDTH :UNCONSTRAINED) (SETQ MAX-WIDTH NIL))
  (AND (EQ MAX-HEIGHT :UNCONSTRAINED) (SETQ MAX-HEIGHT NIL))
  ;; Decide whether it is fill mode or array mode
  (AND (SETQ FILL-P (AND N-COLUMNS (ZEROP N-COLUMNS)))
       (SETQ N-COLUMNS NIL))

  (IF INSIDE-HEIGHT (SETQ INSIDE-HEIGHT (MAX INSIDE-HEIGHT LINE-HEIGHT)))

  ;; Realize any immediately clear implications
  (AND N-ROWS (NULL INSIDE-HEIGHT) (SETQ INSIDE-HEIGHT (* N-ROWS LINE-HEIGHT)))
  (AND INSIDE-HEIGHT (NULL N-ROWS) (SETQ N-ROWS (TRUNCATE INSIDE-HEIGHT LINE-HEIGHT)))
  (SETQ MAX-HEIGHT (MIN (OR INSIDE-HEIGHT MAX-HEIGHT 4096.)
			(- (SHEET-INSIDE-HEIGHT SUPERIOR) TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
	MAX-WIDTH (MIN (OR INSIDE-WIDTH MAX-WIDTH 4096.)
		       (- (SHEET-INSIDE-WIDTH SUPERIOR) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))
  (SETQ MIN-WIDTH (MIN (SEND SELF :MINIMUM-WIDTH) MAX-WIDTH))
  (IF INSIDE-WIDTH (SETQ INSIDE-WIDTH (MAX INSIDE-WIDTH MIN-WIDTH)))

  ;; Compute the horizontal parameters.
  (COND ((AND INSIDE-WIDTH (OR N-COLUMNS FILL-P)) )		;It's fully-determined
	(INSIDE-WIDTH		;We have the width, and it's not in fill mode, compute
	 (SETQ N-COLUMNS	; N-COLUMNS based on widest item, but always fill the space
	       (MAX (MIN (TRUNCATE (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING)
				   (+ (MENU-MAX-WIDTH ITEM-LIST) MENU-INTERCOLUMN-SPACING))
			 (IF N-ROWS (CEILING N-ITEMS N-ROWS) N-ITEMS))
		    1)))
	(N-COLUMNS  ;We don't have the width, but do know how many columns, compute width
	 (SETQ INSIDE-WIDTH (MIN (- (* (+ (MENU-MAX-WIDTH ITEM-LIST)
					  MENU-INTERCOLUMN-SPACING)
				       N-COLUMNS)
				    MENU-INTERCOLUMN-SPACING)
				 MAX-WIDTH)))
	(N-ROWS  ;We know how high, make it wide enough to come out this high
	 (IF FILL-P
	     (SETQ INSIDE-WIDTH (MIN (CEILING (MENU-FILL-WIDTH ITEM-LIST)
					      N-ROWS)
				     MAX-WIDTH))
	     (SETQ N-COLUMNS (MAX (CEILING N-ITEMS N-ROWS) 1)
		   COL-WIDTH (+ (MENU-MAX-WIDTH ITEM-LIST)
				MENU-INTERCOLUMN-SPACING)
		   INSIDE-WIDTH (- (* COL-WIDTH
				      N-COLUMNS)
				   MENU-INTERCOLUMN-SPACING))))
	((NOT FILL-P) ;No geometry supplied, pick N-ROWS and N-COLUMNS to make it look nice
		      ;Use the largest number of columns which does not make the ratio
		      ;of height to width less than the Golden ratio
	 (SETQ TEM (* (SETQ COL-WIDTH (MENU-MAX-WIDTH ITEM-LIST))
                      N-ITEMS
                      LINE-HEIGHT)
	       COL-WIDTH (+ COL-WIDTH MENU-INTERCOLUMN-SPACING)
	       N-COLUMNS (MAX (TRUNCATE (ISQRT (FLOOR TEM MENU-GOLDEN-RATIO)) COL-WIDTH) 1)
	       INSIDE-WIDTH (- (* COL-WIDTH N-COLUMNS) MENU-INTERCOLUMN-SPACING)))
	(T	;No geometry supplied, and in fill mode, make it like above
	 (SETQ INSIDE-WIDTH (MAX (ISQRT (FLOOR (* (MENU-FILL-WIDTH ITEM-LIST)
						  LINE-HEIGHT)
					       MENU-GOLDEN-RATIO))
				 32))))  ;Don't get zero, and don't get absurdly small

  ;; If too wide, or not wide enough for label (etc.), alter the width.
  (COND ((NOT (<= MIN-WIDTH INSIDE-WIDTH MAX-WIDTH))
	 (SETQ INSIDE-WIDTH (MIN MAX-WIDTH (MAX MIN-WIDTH INSIDE-WIDTH)))
	 (AND COL-WIDTH  ;If N-COLUMNS was not user-supplied, recompute it
	      (SETQ N-COLUMNS (MAX (TRUNCATE (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING)
					     COL-WIDTH)
				   1)))))

  ;; Now figure out the vertical characteristics
  (OR N-ROWS
      (SETQ N-ROWS (IF FILL-P 
		       (CEILING (MENU-FILL-WIDTH ITEM-LIST) INSIDE-WIDTH)
		     (CEILING N-ITEMS N-COLUMNS))))
  (OR INSIDE-HEIGHT (SETQ INSIDE-HEIGHT (MAX (* N-ROWS LINE-HEIGHT) 8)))

  ;; If this came out too high, retrench
  (AND (> INSIDE-HEIGHT MAX-HEIGHT)
       (SETQ N-ROWS (TRUNCATE MAX-HEIGHT LINE-HEIGHT)
	     INSIDE-HEIGHT (* N-ROWS LINE-HEIGHT)))

  ;; At this point, INSIDE-WIDTH, INSIDE-HEIGHT, N-COLUMNS (if not FILL-P), and N-ROWS
  ;; are all valid and consistent, and not bigger than the available area,
  ;; provided that the user's original parameters were not illegally huge.

  ;; Return all the dependent parameters as multiple values
  (VALUES (IF FILL-P 0 N-COLUMNS) N-ROWS INSIDE-WIDTH INSIDE-HEIGHT))

(DEFMETHOD (BASIC-MENU :MINIMUM-WIDTH) ()
  ;; If there is a label, the menu must be at least wide enough to accomodate it
  (LET ((L (FUNCALL SELF :SEND-IF-HANDLES :LABEL-SIZE)))
    (IF L (MAX L 20.) 20.)))

;;; This function computes the ROW-MAP, which determines how many strings per line, & c.
;;; The first value is the row-map and the second is the n-total-rows
(COMPILER:MAKE-OBSOLETE MENU-COMPUTE-ROW-MAP "Use W:MENU-COMPUTE-ROW-MAP instead")
(DEFUN MENU-COMPUTE-ROW-MAP (&OPTIONAL (INSIDE-WIDTH (SHEET-INSIDE-WIDTH))
                             &AUX (MAP (MAKE-ARRAY (1+ (LENGTH ITEM-LIST)))) WID
			     (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  (DECLARE (:self-flavor basic-menu))
  (DO ((ITEMS ITEM-LIST)
       (ROW 0 (1+ ROW)))
      ((NULL ITEMS)
       (VALUES (ADJUST-ARRAY MAP (1+ ROW))	;Last element always contains NIL
	       ROW))
    (SETF (AREF MAP ROW) ITEMS)			;This is where this row starts
    (IF FILL-P					;Fill mode, we have some hairy calculation to do
	(DO ((SPACE INSIDE-WIDTH))
	    ((NULL ITEMS))
	  (SETQ WID (MENU-ITEM-STRING-WIDTH (CAR ITEMS)))
	  (COND
	    ((> WID SPACE)			;This one won't fit, break the line
	     (AND (> WID INSIDE-WIDTH)
		  (FERROR ()
			  "The item \"~A\" is too wide for this fill-mode menu"
			  (CAR ITEMS)))
	     (RETURN ())))
	  (SETQ SPACE
		(- SPACE (+ WID MENU-INTERWORD-SPACING))
		ITEMS (CDR ITEMS)))
      (SETQ ITEMS (NTHCDR COLUMNS ITEMS)))))

;;;patched 12 Dec 85 by GSM
(DEFMETHOD (BASIC-MENU :BEFORE :INIT) (INIT-PLIST &AUX (SUP SUPERIOR) TEM)
  (SETQ SUP (OR SUP (GET INIT-PLIST :SUPERIOR) DEFAULT-SCREEN))
  (SETQ DEFAULT-FONT
        (IF (VARIABLE-BOUNDP DEFAULT-FONT)
            (SEND (SHEET-GET-SCREEN SUP) :PARSE-FONT-SPECIFIER DEFAULT-FONT)
            ;; We do not have a font to use.  Use the default one.
            (SEND (SHEET-GET-SCREEN SUP) :PARSE-FONT-SPECIFIER *MENU-ITEM-STANDARD-FONT*)))
  (OR (VARIABLE-BOUNDP FONT-MAP)
      (SETQ FONT-MAP (MENU-COMPUTE-FONT-MAP (GET INIT-PLIST :ITEM-LIST))))
  (SETF (GET INIT-PLIST :MORE-P) ())
  (SETQ TEM (GET INIT-PLIST :GEOMETRY))
  (IF (> (LENGTH TEM) (LENGTH GEOMETRY))
      ;; Longer than we need, take a copy of the list
      (SETQ GEOMETRY (COPY-LIST TEM))
      ;; Else copy the appropriate piece of user specified list into our list
      (DO ((TEM TEM (CDR TEM))
           (GEO GEOMETRY (CDR GEO)))
          ((NULL TEM))
        (SETF (CAR GEO) (CAR TEM))))
  ;;;added the following line to prevent people from creating menus that they cannot move the mouse off of -- GSM 27 Nov 85
  (IF (NULL (GEOMETRY-MAX-WIDTH GEOMETRY))
      (SETF (GEOMETRY-MAX-WIDTH GEOMETRY) (- (SEND SUP :WIDTH) 61)))
  (AND (GET INIT-PLIST :FILL-P)
       (SETF (GEOMETRY-N-COLUMNS GEOMETRY) 0))
  (AND (SETQ TEM (GET INIT-PLIST :ROWS)) (SETF (GEOMETRY-N-ROWS GEOMETRY) TEM))
  (AND (SETQ TEM (GET INIT-PLIST :COLUMNS)) (SETF (GEOMETRY-N-COLUMNS GEOMETRY) TEM))
  ;; We'll handle SAVE-BITS ourselves later
  ;; This is so the bit array doesn't get created until we know the size
  (SETF (GET INIT-PLIST :MENU-SAVE-BITS) (GET INIT-PLIST :SAVE-BITS))
  (SETF (GET INIT-PLIST :SAVE-BITS) ())) 

(DEFMETHOD (BASIC-MENU :AFTER :INIT) (INIT-PLIST)
  (SETF (BLINKER-VISIBILITY (CAR BLINKER-LIST)) NIL)
  (IF item-list (MENU-COMPUTE-GEOMETRY NIL)	;PDC  2 Jan 86
      (menu-compute-geometry nil (sheet-inside-width) (sheet-inside-height)))	;PDC  2 Jan 86
  (SEND SELF :SET-SAVE-BITS (GET INIT-PLIST :MENU-SAVE-BITS)))

(DEFMETHOD (BASIC-MENU :AFTER :REFRESH) (&OPTIONAL TYPE)
  (OR (AND RESTORED-BITS-P (NEQ TYPE :SIZE-CHANGED))
      (SEND SELF :MENU-DRAW)))

;When we change our inside size, we must recompute the geometry with the new inside size,
;unless it is the same as we last computed the geometry for.
;If we get here from recomputing the geometry, that will be true, and avoid a loop.
(DEFMETHOD (BASIC-MENU :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (OR (AND (EQUAL (SHEET-INSIDE-WIDTH) LAST-INSIDE-WIDTH)
	   (EQUAL (SHEET-INSIDE-HEIGHT) LAST-INSIDE-HEIGHT))
      (MENU-COMPUTE-GEOMETRY NIL (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT))))

(DEFMETHOD (BASIC-MENU :AFTER :CHANGE-OF-DEFAULT-FONT) (IGNORE IGNORE)
  (MENU-COMPUTE-GEOMETRY T (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)))

;OPTION = :TEMPORARY means don't change the constraints.
;That is used in calls from within this file.
;If the user randomly does a :SET-EDGES, we assume he wants it to stay as he said.
(DEFMETHOD (BASIC-MENU :AFTER :SET-EDGES) (IGNORE IGNORE IGNORE IGNORE &OPTIONAL OPTION)
  (COND
    ((NOT (MEMBER OPTION '(:VERIFY :TEMPORARY) :TEST #'EQ))
     (SETF (GEOMETRY-INSIDE-WIDTH GEOMETRY) (SHEET-INSIDE-WIDTH))
     (SETF (GEOMETRY-INSIDE-HEIGHT GEOMETRY) (SHEET-INSIDE-HEIGHT))))) 

(DEFMETHOD (BASIC-MENU :SET-POSITION) (NEW-X NEW-Y &OPTIONAL OPTION)
  (SEND SELF :SET-EDGES NEW-X NEW-Y
	   (+ WIDTH NEW-X) (+ HEIGHT NEW-Y)
	   (OR OPTION :TEMPORARY)))

;Changing our borders should preserve our INSIDE size, not our outside size as normally.
(DEFWRAPPER (BASIC-MENU :REDEFINE-MARGINS) (IGNORE . BODY)
  `(LET ((IW (SHEET-INSIDE-WIDTH)) (IH (SHEET-INSIDE-HEIGHT)))
     (PROG1 (PROGN . ,BODY)
	    (MENU-COMPUTE-GEOMETRY NIL IW IH))))

(DEFMETHOD (BASIC-MENU :SET-ITEM-LIST) (NEW-ITEM-LIST)
  (SETQ NEW-ITEM-LIST (REMOVE () (THE LIST NEW-ITEM-LIST) :TEST #'EQ))
  (SETQ ITEM-LIST NEW-ITEM-LIST LAST-ITEM () CURRENT-ITEM ())
  (SEND SELF :SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST))
  NEW-ITEM-LIST) 

(DEFMETHOD (BASIC-MENU :AFTER :SET-FONT-MAP) (&REST IGNORE)
  (MENU-COMPUTE-GEOMETRY T))

(DEFMETHOD (BASIC-MENU :AROUND :SET-LABEL) (CONT MT ARGS &REST NEW-LABEL)
  (UNLESS (EQUAL NEW-LABEL (SEND SELF :LABEL))
    (AROUND-METHOD-CONTINUE CONT MT ARGS)))

(DEFMETHOD (BASIC-MENU :SET-DEFAULT-FONT) (FONT)
  (SETQ DEFAULT-FONT FONT)
  (SEND SELF :SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST))
;  (MENU-COMPUTE-GEOMETRY T) This is done with an :after method of :set-font-map
  )

(DEFMETHOD (BASIC-MENU :SET-GEOMETRY) (&REST NEW-GEOMETRY)
  (DECLARE (ARGLIST (&OPTIONAL N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT
			       MAX-WIDTH MAX-HEIGHT)))
  "NIL for an argument means make it unconstrained.  T or unsupplied means leave it alone"
  (OR (<= (LENGTH NEW-GEOMETRY) (LENGTH GEOMETRY))
      (FERROR NIL "Too many args to :SET-GEOMETRY"))
  (DO ((G NEW-GEOMETRY (CDR G))
       (CG GEOMETRY (CDR CG)))
      ((NULL G))
    (IF (NEQ (CAR G) T)
	(RPLACA CG (CAR G))))
  (MENU-COMPUTE-GEOMETRY T))

(DEFMETHOD (BASIC-MENU :CURRENT-GEOMETRY) ()
  "Like :GEOMETRY but returns the current state rather than the default"
  (LIST (IF (GEOMETRY-FILL-P GEOMETRY) 0 COLUMNS) TOTAL-ROWS
	(SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
	(GEOMETRY-MAX-WIDTH GEOMETRY) (GEOMETRY-MAX-HEIGHT GEOMETRY)))

(DEFMETHOD (BASIC-MENU :FILL-P) () (GEOMETRY-FILL-P GEOMETRY))
(DEFMETHOD (BASIC-MENU :SET-FILL-P) (FILL-P)
  (SEND SELF :SET-GEOMETRY (IF FILL-P 0 NIL)))

(DEFMETHOD (BASIC-MENU :MOUSE-STANDARD-BLINKER) ()
  ;; Change the mouse cursor to a small X so it doesn't get in the way
  (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 4 5 :ON
				:SET-CHARACTER 7))

;;; Mouse handler for menus
(DEFMETHOD (BASIC-MENU :BEFORE :HANDLE-MOUSE) ()
  ;; Forget anything we knew before about the highlight, so it will really be positioned
  (SETQ CURRENT-ITEM NIL))

(DEFMETHOD (BASIC-MENU :AFTER :HANDLE-MOUSE) ()
  ;; When mouse leaves this window, stop flashing any item
  (BLINKER-SET-VISIBILITY (CAR BLINKER-LIST) NIL))

;;; Mouse-click handler for menus.
;;; All buttons are treated the same, select the item you are on.
;;; There are no double-clicks and you can't get to the system command menu.
;;; Clicking when the menu is not exposed just exposes it.

(DEFMETHOD (BASIC-MENU :MOUSE-BUTTONS) (BD X Y)
  (COND (CURRENT-ITEM				;Any button, select item.
	 (SEND SELF :MOUSE-BUTTONS-ON-ITEM BD))
	((AND (>= X (SHEET-INSIDE-LEFT)) (< X (SHEET-INSIDE-RIGHT))
	      (>= Y (SHEET-INSIDE-TOP))  (< Y (SHEET-INSIDE-BOTTOM))))
	(T
	 ;; Here, clicked on the window, but outside of the window proper.
	 ;; Send a :MOUSE-CLICK message so things like margin regions can work.
	 (SEND SELF :MOUSE-CLICK (MOUSE-BUTTON-ENCODE BD) X Y))))

(DEFMETHOD (BASIC-MENU :MOUSE-BUTTONS-ON-ITEM) (BD)
  (SETQ LAST-ITEM CURRENT-ITEM
	CHOSEN-ITEM CURRENT-ITEM)
  (COND ((AND (CONSP CHOSEN-ITEM)
	      (>= (LENGTH CHOSEN-ITEM) 3)
	      (EQ (SECOND CHOSEN-ITEM) :BUTTONS))
	 (SETQ CHOSEN-ITEM (NTH (1- (HAULONG BD)) (THIRD CHOSEN-ITEM))))))

(DEFMETHOD (BASIC-MENU :CHOOSE) ()
  (SETQ CHOSEN-ITEM NIL)
  (OR EXPOSED-P (SEND SELF :EXPOSE))
  (PROCESS-WAIT "Menu choose" #'(LAMBDA (ITEM-LOC STATUS-LOC)
				  (OR (CAR ITEM-LOC) (NULL (CAR STATUS-LOC))))
		(LOCF CHOSEN-ITEM)
		(LOCF EXPOSED-P))
  (UNWIND-PROTECT
      (SEND SELF :EXECUTE CHOSEN-ITEM)
    (SETQ CHOSEN-ITEM NIL)))

;;; This is called from the scheduler
(DEFMETHOD (BASIC-MENU :WHO-LINE-DOCUMENTATION-STRING) ()
  (OR (AND (VARIABLE-BOUNDP CURRENT-ITEM) (MENU-ITEM-WHO-LINE-DOCUMENTATION CURRENT-ITEM))
      ""))

;;; This is the guts.  Given a menu and a set of coordinates, it finds
;;; the corresponding item, if any, sets CURRENT-ITEM to it, and sets up
;;; the blinker to mark that item.  If no item, the blinker is shut off.
;;;*** This tvobish code should be rewritten ***
(DEFMETHOD (BASIC-MENU :MOUSE-MOVES) (X Y
				      &AUX ITEM ITEMS ROW XREL BLINKER BLX (BLWIDTH 0)
				           COLN STOP-ITEM ITEM-BASELINE-ADJUST
					   (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  (MOUSE-SET-BLINKER-CURSORPOS)
  (SETQ ROW (TRUNCATE (- Y (SHEET-INSIDE-TOP) -2) ROW-HEIGHT)
	XREL (- X (SHEET-INSIDE-LEFT) -2)
	BLINKER (CAR BLINKER-LIST))
  (COND ((AND (>= X (SHEET-INSIDE-LEFT) 0)	;If inside the menu
	      (<  X (SHEET-INSIDE-RIGHT))
	      (>= Y (SHEET-INSIDE-TOP))
	      (<  Y (SHEET-INSIDE-BOTTOM)))
	 ;;If mouse is past the last displayed row, blink item on that row.
	 (AND (OR (>= (+ TOP-ROW ROW) TOTAL-ROWS) (>= ROW SCREEN-ROWS))
	      (SETQ ROW (1- (MIN SCREEN-ROWS (- TOTAL-ROWS TOP-ROW)))))
	 (IF (MINUSP ROW) (SETQ ITEMS NIL STOP-ITEM NIL)	;No items visible
	     (SETQ ITEMS (AREF ROW-MAP (+ TOP-ROW ROW))
		   STOP-ITEM (AREF ROW-MAP (+ TOP-ROW ROW 1))))
	 (COND (FILL-P				;Fill mode, cogitate
		(SETQ BLX 0)
		(DO ((L ITEMS (CDR L))
		     (ITM) (OITM NIL ITM)
		     (X 0 (+ X
			     (SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITM))
			     MENU-INTERWORD-SPACING)))
		    ((OR (NULL L)
			 (> X XREL))	    ;If this string crosses the mouse, it's the one
		     (SETQ ITEM OITM))
		  (AND (EQ L STOP-ITEM)
		       ;; The next item on next line -- punt
		       (RETURN NIL))
		  (SETQ ITM (CAR L)
			BLX X)))
	       (T						;Columnated, find which column
		(SETQ COLN (TRUNCATE XREL COLUMN-WIDTH))	;Column selected
		(SETQ ITEM (CAR (NTHCDR COLN ITEMS)))		;This may be NIL
		(SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH))
		(SETQ BLX (+ (* COLN COLUMN-WIDTH)		;Start of column
			     (MAX 0 (TRUNCATE (- COLUMN-WIDTH	;Centering
						 MENU-INTERCOLUMN-SPACING
						 BLWIDTH)
					      2))))))))
  (MULTIPLE-VALUE-BIND (NIL ITEM-FONT)
      (MENU-ITEM-STRING ITEM CURRENT-FONT SELF)
    ;; Put the top of the blinker at the top of the item.
    (SETQ ITEM-BASELINE-ADJUST (- BASELINE (FONT-BASELINE ITEM-FONT)))
    ;; If this item is non-selectable, don't select it.
    (AND (NOT (ATOM ITEM)) (NOT (ATOM (CDR ITEM))) (NOT (ATOM (CDDR ITEM)))
	 (EQ (CADR ITEM) :NO-SELECT)
	 (SETQ ITEM NIL))
    ;; Now make the blinker be where and what we have just found it should be.
    (BLINKER-SET-VISIBILITY BLINKER (NOT (NULL ITEM)))
    (SETQ CURRENT-ITEM ITEM)
    (COND (ITEM
	   ;; Position the hollow rectangular blinker around the item.
	   (FUNCALL BLINKER :SET-SIZE-AND-CURSORPOS
		    (+ BLWIDTH 1)
		    ; Get the height from the font of the item.
		    (+ 3 (font-char-height item-font))
		    BLX
		    (+ (* ROW ROW-HEIGHT) ITEM-BASELINE-ADJUST -1)
		    )))))

(DEFMETHOD (BASIC-MENU :SCROLL-POSITION) ()
  (VALUES TOP-ROW TOTAL-ROWS ROW-HEIGHT))

(DEFMETHOD (BASIC-MENU :SCROLL-TO) (LINE MODE)
  (CASE MODE
    (:ABSOLUTE)
    (:RELATIVE (SETQ LINE (+ TOP-ROW LINE)))
    (OTHERWISE (FERROR () "Illegal scroll mode ~A" MODE)))
  (COND
    ((NOT (= TOP-ROW (SETQ LINE (MAX 0 (MIN LINE (1- TOTAL-ROWS))))))
     ;; Actually changing something, update
     (SETQ TOP-ROW LINE)
     (SEND SELF :MENU-DRAW)
     (SEND SELF :NEW-SCROLL-POSITION TOP-ROW))))  

;;; Put a menu near another window.  This will normally try to put it just below
;;; it and give it the same width.
(DEFMETHOD (BASIC-MENU :MOVE-NEAR-WINDOW) (W)
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (FUNCALL W :EDGES)
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE NEW-HEIGHT)
	(MENU-DEDUCE-PARAMETERS NIL NIL (- RIGHT LEFT) NIL NIL NIL)
      (SETQ NEW-HEIGHT (+ NEW-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
      ;If it won't fit below try putting it above
      (AND (> (+ BOTTOM NEW-HEIGHT)
	      (SHEET-INSIDE-BOTTOM SUPERIOR))
	   (SETQ BOTTOM (MAX (- TOP NEW-HEIGHT) 0)))
      ;Put it there
      (SEND SELF :SET-EDGES LEFT BOTTOM RIGHT (+ BOTTOM NEW-HEIGHT) :TEMPORARY)
      (SEND SELF :EXPOSE))))

;;; This is used by othogonal things like hysteretic window
(DEFMETHOD (BASIC-MENU :SCROLL-BAR-P) () (< SCREEN-ROWS TOTAL-ROWS))

(DEFMETHOD (BASIC-MENU :MENU-DRAW) (&AUX (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
 ;; Make sure the mouse knows we're changing
  (AND EXPOSED-P (MOUSE-WAKEUP))
  (PREPARE-SHEET (SELF)
    (SHEET-CLEAR SELF)
    (DO ((ROW TOP-ROW (1+ ROW))
	 (Y-POS 0 (+ Y-POS ROW-HEIGHT))
	 (LIM (MIN TOTAL-ROWS (+ TOP-ROW SCREEN-ROWS))))
	((>= ROW LIM))
      (DO ((ITEMS (AREF ROW-MAP ROW) (CDR ITEMS))
	   (END-ITEM-LIST (AREF ROW-MAP (1+ ROW)))
	   (STR)
	   (FONT)
	   (FLAG)
	   (X-POS 0))
	  ((EQ ITEMS END-ITEM-LIST))
	(MULTIPLE-VALUE-SETQ (STR FONT)
			     (MENU-ITEM-STRING (CAR ITEMS) CURRENT-FONT SELF))
	(UNWIND-PROTECT (PROGN
			  (AND
			    (SETQ FLAG (AND (NEQ FONT CURRENT-FONT) CURRENT-FONT))
			    (SHEET-SET-FONT SELF FONT))
			  (COND
			    (FILL-P		;Filled, put string followed by spacing
			     (SHEET-SET-CURSORPOS SELF X-POS Y-POS)
			     (SHEET-STRING-OUT SELF STR)
			     (SETQ X-POS
				   (+ (SHEET-READ-CURSORPOS SELF)
				      MENU-INTERWORD-SPACING)))
			    (T			;Columnated, center text within column
			     
			     (SHEET-DISPLAY-CENTERED-STRING
			       SELF STR X-POS (- (SETQ X-POS (+ X-POS COLUMN-WIDTH))
						 MENU-INTERCOLUMN-SPACING)
			       Y-POS))))
	  (AND FLAG (SHEET-SET-FONT SELF FLAG))))))) 

(COMPILER:MAKE-OBSOLETE MENU-FILL-WIDTH "Use W:MENU-FILL-WIDTH instead")
(DEFUN MENU-FILL-WIDTH (ITEM-LIST)
  "Return an estimate of the total length required for ITEM-LIST in a filled menu.
This is what the lengths of all the lines must add up to.
We take account of spacing between items, but we can only estimate
the amount of space wasted due to line breakage."
  (DO ((L ITEM-LIST (CDR L))
       (WID 0))
      ((NULL L) WID)
    (SETQ WID (+ WID (MENU-ITEM-STRING-WIDTH (CAR L)) MENU-FILL-BREAKAGE))))

;;; Here is how we make a menu appear with the last item chosen under the mouse.

;;; Return the x and y co-ordinates (inside the margins)
;;; of the center of the specified item, NIL if scrolled off display
(DEFMETHOD (BASIC-MENU :ITEM-CURSORPOS) (ITEM &AUX (ALEN (ARRAY-total-size ROW-MAP)))
  (DO ((ROW (1- (MIN (+ TOP-ROW SCREEN-ROWS)	;last row on screen
		     ALEN))			;last row that exists
	    (1- ROW)))
      ((< ROW TOP-ROW) NIL)
    (AND (MEMBER ITEM (AREF ROW-MAP ROW) :TEST #'EQ)
         (OR (= ROW (1- ALEN)) (NOT (MEMBER ITEM (AREF ROW-MAP (1+ ROW)) :TEST #'EQ)))
         (RETURN
	   (IF (NOT (GEOMETRY-FILL-P GEOMETRY))
	       (+ (* (position item (the list (AREF ROW-MAP ROW)) :test #'eq) COLUMN-WIDTH)
		  (TRUNCATE COLUMN-WIDTH 2))
	       (DO ((L (AREF ROW-MAP ROW) (CDR L))
		    (XSTART 0 (+ XSTART SWIDTH MENU-INTERWORD-SPACING))
		    (SWIDTH))
		   (NIL)
		 (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH (CAR L)))
		 (AND (EQ (CAR L) ITEM) (RETURN (+ XSTART (TRUNCATE SWIDTH 2))))))
	   (+ (* (- ROW TOP-ROW) ROW-HEIGHT) (TRUNCATE ROW-HEIGHT 2))))))  

;;; Return the left, top, right, bottom coordinates (inside the margins)
;;; of the rectangle enclosing the specified item, including one bit of
;;; margin all around, or NIL if scrolled off the display.
;;; Note that because of the one bit of margin, returned values can be outside
;;; the window.
(DEFMETHOD (BASIC-MENU :ITEM-RECTANGLE) (ITEM &AUX (X 0) SWIDTH (ALEN (ARRAY-total-size ROW-MAP)))
  (DO ((ROW (1- (MIN (+ TOP-ROW SCREEN-ROWS)	;last row on screen
		     ALEN))			;last row that exists
	    (1- ROW)))
      ((< ROW TOP-ROW) NIL)
    (COND
      ((AND (MEMBER ITEM (AREF ROW-MAP ROW) :TEST #'EQ)
            (OR (= ROW (1- ALEN)) (NOT (MEMBER ITEM (AREF ROW-MAP (1+ ROW)) :TEST #'EQ))))
       (IF (NOT (GEOMETRY-FILL-P GEOMETRY))
           (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH) X
                 (+ (* (position ITEM (the list (AREF ROW-MAP ROW)) :test #'eq) COLUMN-WIDTH)
                    (TRUNCATE (- COLUMN-WIDTH MENU-INTERCOLUMN-SPACING SWIDTH) 2)))
           (DOLIST (IT (AREF ROW-MAP ROW))
             (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH IT))
             (AND (EQ IT ITEM) (RETURN))
             (SETQ X (+ X SWIDTH MENU-INTERWORD-SPACING))))
       (RETURN (1- X) (1- (* (- ROW TOP-ROW) ROW-HEIGHT)) (+ X SWIDTH 1)
               (- (* (1+ (- ROW TOP-ROW)) ROW-HEIGHT) 2)))))) 

;; When we move a menu to a spot, make it go so that the last item chosen
;; appears at that spot.
(DEFMETHOD (BASIC-MENU :CENTER-AROUND) (X Y &AUX (XI 0) (YI 0))
  (AND (VARIABLE-BOUNDP LAST-ITEM)
       (MEMBER LAST-ITEM ITEM-LIST :TEST #'EQ)
       ;; If we remember a previous choice,
       ;; let XI and YI get the offsets from that item to the center.
       (MULTIPLE-VALUE-BIND (X1 Y1) (SEND SELF :ITEM-CURSORPOS LAST-ITEM)
         (AND X1 Y1
              (SETQ XI (- (TRUNCATE WIDTH 2) X1 (SHEET-INSIDE-LEFT)) YI
                    (- (TRUNCATE HEIGHT 2) Y1 (SHEET-INSIDE-TOP))))))
  (MULTIPLE-VALUE-BIND (X1 Y1) (CENTER-WINDOW-AROUND SELF (+ X XI) (+ Y YI))
    (VALUES (- X1 XI) (- Y1 YI)))) 

(DEFMETHOD (BASIC-MENU :COLUMN-ROW-SIZE) ()
  (VALUES COLUMN-WIDTH ROW-HEIGHT))

;; Permanent menus for giving "keyboard" commands from a menu alist
(DEFFLAVOR COMMAND-MENU-MIXIN (IO-BUFFER) ()
  (:REQUIRED-FLAVORS BASIC-MENU)
  (:SETTABLE-INSTANCE-VARIABLES IO-BUFFER))

(DEFMETHOD (COMMAND-MENU-MIXIN :AFTER :MOUSE-BUTTONS) (BD IGNORE IGNORE)
  (COND (CHOSEN-ITEM
	 (IO-BUFFER-PUT IO-BUFFER `(:MENU ,CHOSEN-ITEM ,BD ,SELF))
	 (SETQ CHOSEN-ITEM NIL))))

(DEFFLAVOR COMMAND-MENU () (COMMAND-MENU-MIXIN MENU))

(DEFFLAVOR COMMAND-MENU-ABORT-ON-DEEXPOSE-MIXIN () ()
  (:REQUIRED-FLAVORS COMMAND-MENU)
  (:DOCUMENTATION :MIXIN "Automatically clicks on the ABORT item if the menu is deexposed"))

(DEFMETHOD (COMMAND-MENU-ABORT-ON-DEEXPOSE-MIXIN :BEFORE :DEEXPOSE) (&REST IGNORE)
  (IF EXPOSED-P
      (DOLIST (ITEM ITEM-LIST)
	(IF (STRING-EQUAL (MENU-ITEM-STRING ITEM) "ABORT")
	    (RETURN (IO-BUFFER-PUT IO-BUFFER `(:MENU ,ITEM 1 ,SELF)))))))

(DEFFLAVOR MENU-HIGHLIGHTING-MIXIN ((HIGHLIGHTED-ITEMS NIL)) ()
  (:REQUIRED-FLAVORS BASIC-MENU)
  (:GETTABLE-INSTANCE-VARIABLES HIGHLIGHTED-ITEMS)
  (:INITABLE-INSTANCE-VARIABLES HIGHLIGHTED-ITEMS)
  (:DOCUMENTATION :MIXIN "Provides for highlighting of items with inverse video"))

; This does not remember it on the list, you probably don't want to use it yourself
(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :HIGHLIGHT-ITEM) (ITEM)
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM) (SEND SELF :ITEM-RECTANGLE ITEM)
    (AND (NOT (NULL LEFT))
	 (PREPARE-SHEET (SELF)			;Clip but allow extension into margins
	   (SETQ LEFT (MAX (+ LEFT (SHEET-INSIDE-LEFT)) 0)
		 RIGHT (MIN (+ RIGHT (SHEET-INSIDE-LEFT)) WIDTH)
		 TOP (MAX (+ TOP (SHEET-INSIDE-TOP)) 0)
		 BOTTOM (MIN (+ BOTTOM (SHEET-INSIDE-TOP)) HEIGHT))
	   (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ALU-XOR SELF)))))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :ADD-HIGHLIGHTED-ITEM) (ITEM)
  (COND
    ((NOT (MEMBER ITEM HIGHLIGHTED-ITEMS :TEST #'EQ)) (PUSH ITEM HIGHLIGHTED-ITEMS)
     (SHEET-FORCE-ACCESS (SELF T) (SEND SELF :HIGHLIGHT-ITEM ITEM))))) 

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :REMOVE-HIGHLIGHTED-ITEM) (ITEM)
  (COND
    ((MEMBER ITEM HIGHLIGHTED-ITEMS :TEST #'EQ)
     (SETQ HIGHLIGHTED-ITEMS (DELETE ITEM (THE LIST HIGHLIGHTED-ITEMS) :TEST #'EQ))
     (SHEET-FORCE-ACCESS (SELF T) (SEND SELF :HIGHLIGHT-ITEM ITEM))))) 

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :SET-HIGHLIGHTED-ITEMS) (NEW-HIGHLIGHTED-ITEMS &AUX OLD)
  (SETQ OLD HIGHLIGHTED-ITEMS HIGHLIGHTED-ITEMS NEW-HIGHLIGHTED-ITEMS)
  (SHEET-FORCE-ACCESS (SELF T)
                      (DOLIST (X OLD)
                        (OR (MEMBER X NEW-HIGHLIGHTED-ITEMS :TEST #'EQ)
                            (SEND SELF :HIGHLIGHT-ITEM X)))
                      (DOLIST (X NEW-HIGHLIGHTED-ITEMS)
                        (OR (MEMBER X OLD :TEST #'EQ) (SEND SELF :HIGHLIGHT-ITEM X))))) 

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :AFTER :MENU-DRAW) ()
  (DOLIST (X HIGHLIGHTED-ITEMS)
    (SEND SELF :HIGHLIGHT-ITEM X)))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :HIGHLIGHTED-VALUES) ()
  (MAPCAR #'(LAMBDA (X) (SEND SELF :EXECUTE-NO-SIDE-EFFECTS X)) HIGHLIGHTED-ITEMS))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :SET-HIGHLIGHTED-VALUES) (VALUES &AUX ITEMS)
  (DOLIST (ITEM ITEM-LIST)
    (AND (MEMBER (SEND SELF :EXECUTE-NO-SIDE-EFFECTS ITEM) VALUES :TEST #'EQUAL)
         (PUSH ITEM ITEMS)))
  (OR (= (LENGTH ITEMS) (LENGTH VALUES)) (FERROR () "Missing or duplicate value"))
  (SEND SELF :SET-HIGHLIGHTED-ITEMS ITEMS)) 

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :ADD-HIGHLIGHTED-VALUE) (VALUE)
  (DO ((L ITEM-LIST (CDR L)))
      ((NULL L) (FERROR NIL "Value not found"))
    (AND (EQUAL (SEND SELF :EXECUTE-NO-SIDE-EFFECTS (CAR L)) VALUE)
	 (RETURN (SEND SELF :ADD-HIGHLIGHTED-ITEM (CAR L))))))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :REMOVE-HIGHLIGHTED-VALUE) (VALUE)
  (DO ((L ITEM-LIST (CDR L)))
      ((NULL L) (FERROR NIL "Value not found"))
    (AND (EQUAL (SEND SELF :EXECUTE-NO-SIDE-EFFECTS (CAR L)) VALUE)
	 (RETURN (SEND SELF :REMOVE-HIGHLIGHTED-ITEM (CAR L))))))


(DEFFLAVOR MENU-MARGIN-CHOICE-MIXIN () (MARGIN-CHOICE-MIXIN)
  (:REQUIRED-FLAVORS BASIC-MENU)
  (:DOCUMENTATION :MIXIN "Puts choice boxes in the bottom margin of a menu.
Clicking on a choice box simulates clicking on a menu item")
  (:INIT-KEYWORDS :MENU-MARGIN-CHOICES))

;An element of :MENU-MARGIN-CHOICES is just like an element of :ITEM-LIST

(DEFMETHOD (MENU-MARGIN-CHOICE-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (SETQ MARGIN-CHOICES
	(MAPCAR #'MENU-MARGIN-CHOICE-FROM-ITEM (GET INIT-PLIST :MENU-MARGIN-CHOICES))))

(DEFMETHOD (MENU-MARGIN-CHOICE-MIXIN :SET-MENU-MARGIN-CHOICES) (LIST)
  (SEND SELF :SET-MARGIN-CHOICES (MAPCAR #'MENU-MARGIN-CHOICE-FROM-ITEM LIST)))

(COMPILER:MAKE-OBSOLETE MENU-MARGIN-CHOICE-FROM-ITEM "Use W:MENU-MARGIN-CHOICE-FROM-ITEM instead")
(DEFUN MENU-MARGIN-CHOICE-FROM-ITEM (X)
  (DECLARE (:SELF-FLAVOR MENU-MARGIN-CHOICE-MIXIN))
  (LIST (MENU-ITEM-STRING X NIL) NIL 'MENU-MARGIN-CHOICE-FUNCTION NIL NIL X))

(COMPILER:MAKE-OBSOLETE MENU-MARGIN-CHOICE-FUNCTION "Use W:MENU-MARGIN-CHOICE-FUNCTION instead")
(DEFUN MENU-MARGIN-CHOICE-FUNCTION (CHOICE-BOX REGION Y-POS)
  (DECLARE (:SELF-FLAVOR MENU-MARGIN-CHOICE-MIXIN))
  REGION Y-POS ;ignored
  (SETQ CHOSEN-ITEM (SIXTH CHOICE-BOX)))

;Really we want a MAX form of method combination for this operation.
(DEFWRAPPER (MENU-MARGIN-CHOICE-MIXIN :MINIMUM-WIDTH) (IGNORE . BODY)
  `(MAX (PROGN . ,BODY)
	(SEND SELF :MARGIN-CHOICES-MINIMUM-WIDTH)))

(DEFMETHOD (MENU-MARGIN-CHOICE-MIXIN :MARGIN-CHOICES-MINIMUM-WIDTH) ()
  (DO ((W 0)
       (CHOICES MARGIN-CHOICES (CDR CHOICES)))
      ((NULL CHOICES)
       (+ W (* (LENGTH MARGIN-CHOICES)
	       (+ (* 2 (FONT-CHAR-WIDTH CURRENT-FONT))
		  (FONT-BLINKER-HEIGHT CURRENT-FONT)))))
    (INCF W (MENU-ITEM-STRING-WIDTH (CAAR CHOICES)))))

(DEFFLAVOR MARGIN-CHOICE-MENU ((LABEL NIL))
	   (BASIC-MENU BORDERS-MIXIN MENU-MARGIN-CHOICE-MIXIN
	    TOP-BOX-LABEL-MIXIN BASIC-SCROLL-BAR MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "An instantiable menu with choice boxes at bottom.
Otherwise like an ordinary menu.  This example shows how to mix in
the flavor MENU-MARGIN-CHOICE-MIXIN."))

(DEFFLAVOR MOMENTARY-MARGIN-CHOICE-MENU
	((LABEL NIL))
	(BASIC-MOMENTARY-MENU Temporary-Shadow-Borders-Window-Mixin
;Changed to share more combined methods.
	 MARGIN-CHOICE-MENU)
;	 BORDERS-MIXIN MENU-MARGIN-CHOICE-MIXIN
;	 TOP-BOX-LABEL-MIXIN BASIC-SCROLL-BAR MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "Momentary menu with choice boxes at bottom.
This is a nontrivial flavor combination because MENU-MARGIN-CHOICE-MIXIN
must be ordered properly with respect to other flavors."))

(DEFFLAVOR MULTIPLE-MENU-MIXIN (SPECIAL-CHOICE-ITEMS) (MENU-HIGHLIGHTING-MIXIN)
  (:INIT-KEYWORDS :SPECIAL-CHOICES)
  (:DEFAULT-INIT-PLIST :FONT-MAP '(FONTS:MEDFNT FONTS:HL12I)
		       :SPECIAL-CHOICES '(("Do It"
					   :EVAL (VALUES (SEND SELF :HIGHLIGHTED-VALUES)
							 T))))
  (:REQUIRED-FLAVORS BASIC-MENU)
  (:DOCUMENTATION :MIXIN "A menu in which you can select more than one choice.
 HIGHLIGHTED-ITEMS is a list of those items in the ITEM-LIST that are currently
 selected.  SPECIAL-CHOICES are those items that don't highlight when
 you click on them, but instead are executed in the usual way.  The default
 special choice is just Done, which returns a list of the values of the highlighted
 items.  SPECIAL-CHOICES are displayed in italics at the top of the menu."))

;Insert the special-choices into the item-list
;Buglet - if n-columns is not specified explicitly, and turns out to be more than 1,
;there will not be automatic blank space inserted to keep the special-choices on
;a separate row.  There is probably nothing wrong with this.
(COMPILER:MAKE-OBSOLETE MULTIPLE-MENU-HACK-ITEM-LIST "special-choices are not supported")
(DEFUN MULTIPLE-MENU-HACK-ITEM-LIST (ITM-LST &OPTIONAL N-COLUMNS)
  (DECLARE (:SELF-FLAVOR MULTIPLE-MENU-MIXIN))
    (SETQ N-COLUMNS (OR N-COLUMNS (CAR GEOMETRY) 1))
    (APPEND SPECIAL-CHOICE-ITEMS
	    (AND N-COLUMNS (> N-COLUMNS 1)
		 (DO ((N
		       (REM (LENGTH SPECIAL-CHOICE-ITEMS)
				  N-COLUMNS)
		       (1+ N))
		      (L NIL (CONS '("" :NO-SELECT NIL) L)))
		     ((OR (ZEROP N) (= N N-COLUMNS)) L)))
	    ITM-LST)) 

(DEFMETHOD (MULTIPLE-MENU-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (SETQ SPECIAL-CHOICE-ITEMS
	(MAPCAR #'(LAMBDA (X)
		    (APPEND (COND ((ATOM X) (LIST X :VALUE X))
				  ((ATOM (CDR X)) (LIST (CAR X) :VALUE (CDR X)))
				  ((NULL (CDDR X)) (LIST (CAR X) :VALUE (CADR X)))
				  (T X))
			    '(:FONT FONTS:HL12I :SPECIAL-CHOICE T)))
		(GET INIT-PLIST :SPECIAL-CHOICES)))
  (AND (VARIABLE-BOUNDP ITEM-LIST)	;Only if items specified in init-plist
       (SETQ ITEM-LIST (MULTIPLE-MENU-HACK-ITEM-LIST ITEM-LIST (GET INIT-PLIST :COLUMNS)))))

(DEFMETHOD (MULTIPLE-MENU-MIXIN :SET-ITEM-LIST) (NEW-ITEM-LIST)
  (SEND SELF :SET-HIGHLIGHTED-ITEMS NIL)
  (SETQ ITEM-LIST (MULTIPLE-MENU-HACK-ITEM-LIST NEW-ITEM-LIST)
	LAST-ITEM NIL
	CURRENT-ITEM NIL)
  (MENU-COMPUTE-GEOMETRY T)		;Recompute parameters, and redraw menu
  ITEM-LIST)

;Modified mouse-button handler.  Does normal thing for special-choices, otherwise
;just complements highlight state.
(DEFWRAPPER (MULTIPLE-MENU-MIXIN :MOUSE-BUTTONS-ON-ITEM) (IGNORE . BODY)
  `(LET ((ITEM CURRENT-ITEM))
     (COND ((AND (NOT (ATOM ITEM))		;Special-choice selected?
		 (NOT (ATOM (CDR ITEM)))
		 (GET (CDDR ITEM) :SPECIAL-CHOICE))
	    . ,BODY)				;Yes, do normal action
	   (T					;Ordinary item, highlight or un-highlight it
	    (SEND SELF (IF (MEMQ ITEM HIGHLIGHTED-ITEMS) :REMOVE-HIGHLIGHTED-ITEM
			    :ADD-HIGHLIGHTED-ITEM)
			  ITEM)))))

;(DEFFLAVOR MULTIPLE-MENU () (MULTIPLE-MENU-MIXIN MENU))

;(DEFFLAVOR MOMENTARY-MULTIPLE-MENU () (MULTIPLE-MENU-MIXIN MOMENTARY-MENU))

(DEFFLAVOR MULTIPLE-MENU () (MARGIN-MULTIPLE-MENU-MIXIN MARGIN-CHOICE-MENU))

(DEFFLAVOR MOMENTARY-MULTIPLE-MENU
	() (MARGIN-MULTIPLE-MENU-MIXIN MOMENTARY-MARGIN-CHOICE-MENU))

(DEFFLAVOR MARGIN-MULTIPLE-MENU-MIXIN () (MENU-HIGHLIGHTING-MIXIN)
  (:REQUIRED-FLAVORS MENU-MARGIN-CHOICE-MIXIN)
  (:INIT-KEYWORDS :SPECIAL-CHOICES)
  (:DEFAULT-INIT-PLIST
    :SPECIAL-CHOICES '(("Do It" :EVAL (VALUES (SEND SELF :HIGHLIGHTED-VALUES)
					      T))))
  (:DOCUMENTATION :MIXIN "A menu in which you can select more than one choice.
 HIGHLIGHTED-ITEMS is a list of those items in the ITEM-LIST that are currently
 selected.  SPECIAL-CHOICES are those items that don't highlight when
 you click on them, but instead are executed in the usual way.
 They go in choice boxes in the bottom margin."))

(DEFMETHOD (MARGIN-MULTIPLE-MENU-MIXIN :BEFORE :SET-ITEM-LIST) (IGNORE)
  (SEND SELF :SET-HIGHLIGHTED-ITEMS NIL))

(DEFMETHOD (MARGIN-MULTIPLE-MENU-MIXIN :ADD-ITEM) (NEW-ITEM)
  "This is like setting the item list, but we do not unhighlight any of
the existing items in the menu."
  (UNLESS (MEMBER NEW-ITEM ITEM-LIST :TEST #'EQUAL)
    (SETQ ITEM-LIST (NCONC ITEM-LIST (LIST NEW-ITEM)))
    (SETQ LAST-ITEM () CURRENT-ITEM ())
    (SEND SELF :SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST)))
  NEW-ITEM) 

(DEFMETHOD (MARGIN-MULTIPLE-MENU-MIXIN :MOUSE-BUTTONS-ON-ITEM) (IGNORE)
  (SEND SELF
        (IF (MEMBER CURRENT-ITEM HIGHLIGHTED-ITEMS :TEST #'EQ) :REMOVE-HIGHLIGHTED-ITEM
            :ADD-HIGHLIGHTED-ITEM)
        CURRENT-ITEM)) 

(DEFMETHOD (MARGIN-MULTIPLE-MENU-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (SETF (GET INIT-PLIST :MENU-MARGIN-CHOICES) (GET INIT-PLIST :SPECIAL-CHOICES))) 

(DEFFLAVOR BASIC-MOMENTARY-MENU () (HYSTERETIC-WINDOW-MIXIN BASIC-MENU)
  (:DOCUMENTATION :MIXIN "A menu that holds control of the mouse.
Menus of this type handle the mouse for a small area outside of their
actual edges.  They also are automatically deactivated whenever an item
is chosen or the mouse moves even further, out of its control."))

(DEFMETHOD (BASIC-MOMENTARY-MENU :AROUND :CHOOSE) (CONT MT ARGS)
  (PROG ((X MOUSE-X)
         (Y MOUSE-Y)
         (SUCCESS T))
    (UNWIND-PROTECT (PROGN
                     (CATCH 'ABORT
                       (RETURN (AROUND-METHOD-CONTINUE CONT MT ARGS)))
                     (SETQ SUCCESS ()))
                    (WHEN SUCCESS
       (MOUSE-WARP X Y))))) 

(DEFMETHOD (BASIC-MOMENTARY-MENU :BEFORE :CHOOSE) ()
  (COND
    ((NOT EXPOSED-P)
     (MULTIPLE-VALUE-BIND (X-OFF Y-OFF) (SHEET-CALCULATE-OFFSETS SUPERIOR MOUSE-SHEET)
       (MULTIPLE-VALUE-BIND (X Y) (SEND SELF :CENTER-AROUND (- MOUSE-X X-OFF) (- MOUSE-Y Y-OFF))
         (MOUSE-WARP (+ X X-OFF) (+ Y Y-OFF))))
     ;; Expose self, and seize the mouse.
     (WITH-MOUSE-GRABBED (SEND SELF :EXPOSE)
                         (COND
                           ((NEQ SELF
                                 (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y ()
                                                           :EXPOSED))
                            (SEND SELF :DEACTIVATE) (THROW 'ABORT
                                                           ())))))))  

;;; When no selection, but mouse moved out of range, deexpose menu
(DEFMETHOD (BASIC-MOMENTARY-MENU :AFTER :HANDLE-MOUSE) ()
  (OR CHOSEN-ITEM
      ;; Don't flush if mouse being usurped
      WINDOW-OWNING-MOUSE
      ;; Only flush us if either not explicitly flushing or we don't own mouse
      (AND MOUSE-RECONSIDER (EQ SELF (WINDOW-OWNING-MOUSE)))
      ;; This is called in the mouse process.  We don't want to take the chance that
      ;; we might go blocked, so run in another process.
      (PROCESS-RUN-FUNCTION '(:NAME "Menu Deactivate" :PRIORITY 20.) SELF :DEACTIVATE)))

;;; Make MOUSE-DEFAULT-HANDLER return so menu gets deactivated.
(DEFMETHOD (BASIC-MOMENTARY-MENU :AFTER :MOUSE-BUTTONS) (IGNORE IGNORE IGNORE)
  (AND CHOSEN-ITEM (SETQ MOUSE-RECONSIDER T)))

;; Get here if either 1) user clicks on an item or 2) menu is deactivated.
(DEFMETHOD (BASIC-MOMENTARY-MENU :BEFORE :EXECUTE) (ITEM &REST IGNORE)
  (SEND SELF :DEACTIVATE)
  (UNLESS ITEM
    (THROW 'ABORT
           ()))) 

(DEFFLAVOR WINDOW-HACKING-MENU-MIXIN (WINDOW-UNDER-MENU OLD-X OLD-Y) ()
  (:DOCUMENTATION :MIXIN "Menu which handles :WINDOW-OP when called over another window
The window that the menu is exposed over is remembered when the :choose message is sent,
and then used if a :window-op type item is selected."))

(DEFMETHOD (WINDOW-HACKING-MENU-MIXIN :BEFORE :CHOOSE) ()
  (SETQ WINDOW-UNDER-MENU (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y)
	OLD-X MOUSE-X
	OLD-Y MOUSE-Y))

(DEFMETHOD (WINDOW-HACKING-MENU-MIXIN :EXECUTE-WINDOW-OP) (FUNCTION)
  (FUNCALL FUNCTION WINDOW-UNDER-MENU OLD-X OLD-Y))

(DEFFLAVOR ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN () ()
  (:REQUIRED-FLAVORS BASIC-MENU)
  (:REQUIRED-METHODS :UPDATE-ITEM-LIST)
  (:DEFAULT-INIT-PLIST :ITEM-LIST NIL)
  (:DOCUMENTATION :MIXIN "Allows the menu to have an item list that's being dynamically
modified.  Causes the menu's item list to be updated at appropriate times.
The actual item list is computed via the :UPDATE-ITEM-LIST message."))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :CHOOSE) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :MOVE-NEAR-WINDOW) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :CENTER-AROUND) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :SIZE) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

(DEFMETHOD (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN :BEFORE :PANE-SIZE) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))


(DEFFLAVOR DYNAMIC-ITEM-LIST-MIXIN ((ITEM-LIST-POINTER NIL))
	   (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN)
  :INITABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :MIXIN "Allows the menu to have an item list that's being dynamically
modified.  Causes the menu's item list to be updated at appropriate times.
The ITEM-LIST-POINTER instance variable is a form to be evaluated to get the item list."))

(COMPILER:MAKE-OBSOLETE DYNAMIC-ITEM-LIST "Use W:MENU-DYNAMIC-ITEM-LIST instead")
(DEFUN DYNAMIC-ITEM-LIST ()
  (DECLARE (:SELF-FLAVOR DYNAMIC-ITEM-LIST-MIXIN))
    (IF (SYMBOLP ITEM-LIST-POINTER)
	(SYMBOL-VALUE ITEM-LIST-POINTER)
	(GLOBAL:EVAL ITEM-LIST-POINTER)))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :BEFORE :INIT) (IGNORE)
  (AND ITEM-LIST-POINTER
       (SETQ ITEM-LIST (DYNAMIC-ITEM-LIST))))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :UPDATE-ITEM-LIST) (&rest ignore &AUX NEW-ITEM-LIST)
  (AND ITEM-LIST-POINTER
       (OR (EQUAL ITEM-LIST (SETQ NEW-ITEM-LIST (DYNAMIC-ITEM-LIST)))
	   (SEND SELF :SET-ITEM-LIST NEW-ITEM-LIST))))


(DEFFLAVOR DYNAMIC-MULTICOLUMN-MIXIN (COLUMN-SPEC-LIST PREVIOUS-STATE)
	   (ABSTRACT-DYNAMIC-ITEM-LIST-MIXIN)
  (:INITABLE-INSTANCE-VARIABLES COLUMN-SPEC-LIST)
  (:GETTABLE-INSTANCE-VARIABLES COLUMN-SPEC-LIST)
  (:DOCUMENTATION :MIXIN "Makes a menu have multiple 'dynamic' columns.
Each column comes from a separate item-list which is recomputed at appropriate times.
The instance variable COLUMN-SPEC-LIST is a list of columns, each column consists
of (heading item-list-form . options).  Heading is a string to go at the top of the
column, and options are menu-item options for it (typically font).  item-list-form is
a form to be evaluated (without side-effects) to get the item list for that column."))

(DEFMETHOD (DYNAMIC-MULTICOLUMN-MIXIN :BEFORE :INIT) (IGNORE)
  (SETQ PREVIOUS-STATE (MAKE-LIST (LENGTH COLUMN-SPEC-LIST))))

(DEFMETHOD (DYNAMIC-MULTICOLUMN-MIXIN :UPDATE-ITEM-LIST) (&OPTIONAL FORCE)
  (IF
    (OR FORCE
	(LOOP FOR (HEADING FORM) IN COLUMN-SPEC-LIST AND OLD-ITEM-LIST IN PREVIOUS-STATE THEREIS
	      (NEQ
		(IF (SYMBOLP FORM) (SYMBOL-VALUE FORM)
		    (GLOBAL:EVAL FORM))
		OLD-ITEM-LIST)))
    ;; Something has changed, set up new item list.
    ;; Start by extracting the column lists and setting up the headings.
    (LOOP FOR (HEADING FORM . OPTIONS) IN COLUMN-SPEC-LIST AND STATEL ON PREVIOUS-STATE COLLECT
	  `(,HEADING ,:NO-SELECT T . ,OPTIONS) INTO NEW-ITEM-LIST COLLECT
	  (IF (SYMBOLP FORM) (SYMBOL-VALUE FORM)
	      (GLOBAL:EVAL FORM))
	  INTO COLUMN-VALUES FINALLY
	  ;; Now interleave the columns, and save the old state.
	  (SETQ NEW-ITEM-LIST (NREVERSE NEW-ITEM-LIST))
	  (LOOP FOR C IN COLUMN-VALUES AND L ON PREVIOUS-STATE DO (RPLACA L C))
	  (LOOP REPEAT (LOOP FOR C IN COLUMN-VALUES MAXIMIZE (LENGTH C)) DO
		(LOOP FOR L ON COLUMN-VALUES DO
		      (PUSH (OR (CAAR L) '("" :NO-SELECT T)) NEW-ITEM-LIST) (RPLACA L (CDAR L))))
	  (WITH-SHEET-DEEXPOSED (SELF)		;PDC  2 Jan 86
	    
	    (OR (EQ (CAR GEOMETRY) (LENGTH COLUMN-SPEC-LIST))
		(SEND SELF :SET-GEOMETRY (LENGTH COLUMN-SPEC-LIST)))
	    (SEND SELF :SET-ITEM-LIST (NREVERSE NEW-ITEM-LIST)))))) 

(DEFMETHOD (DYNAMIC-MULTICOLUMN-MIXIN :SET-COLUMN-SPEC-LIST) (NEW-COLUMN-SPEC-LIST)
  (SETQ PREVIOUS-STATE (MAKE-LIST (LENGTH NEW-COLUMN-SPEC-LIST)))
  (SETQ COLUMN-SPEC-LIST NEW-COLUMN-SPEC-LIST)
  (SEND SELF :UPDATE-ITEM-LIST T))

;;; This is a bit of a kludge.  It is necessary because this method is not
;;; loaded before the COMPILE-FLAVOR-METHODS is done, and therefore the
;;; flavor system assumes that the method has been deleted and recompiles
;;; the combined method.  So we tell it that there is going to be a method,
;;; but the actual code for the method is in FRAME.
(SI:FLAVOR-NOTICE-METHOD '(:METHOD BASIC-MENU :PANE-SIZE))


;;; Menus to be used for a momentary choice.
;;; Send a menu of this type a :CHOOSE message to use the menu.
;;; When the user selects an item, or moves the mouse off the menu,
;;; the menu will disappear, and whatever was underneath it will reappear.
;;; It will return the chosen item, or NIL.  If the item is not atomic
;;; and its cadr is non-NIL, the cadr will be called with no arguments.
;;; In this case, if the caddr of the item is also non-nil,
;;; no windows will be re-exposed before the cadr is called.
(DEFFLAVOR MOMENTARY-MENU
  ((LABEL NIL))
  (BASIC-MOMENTARY-MENU Temporary-Shadow-Borders-Window-Mixin
   BORDERS-MIXIN TOP-BOX-LABEL-MIXIN BASIC-SCROLL-BAR MINIMUM-WINDOW)	
  (:DOCUMENTATION :COMBINATION "Temporary menu that goes away after item is chosen"))

(DEFVAR EXPLODING-MOMENTARY-WINDOWS NIL
  "T will turn on Exploding Visual Effects on Momentary-Menus.")

;;; TURN-ON-BLINKERS means that this window will soon become the
;;; SELECTED-WINDOW, so it is not necessary to change blinkers from
;;; :BLINK to their DESELECTED-BLINKER-VISIBILITY.
;;; If Exploding-Momentary-Windows 
(DEFMETHOD (MOMENTARY-MENU :EXPOSE) (&OPTIONAL INHIBIT-BLINKERS BITS-ACTION (X X-OFFSET) (Y Y-OFFSET) &AUX
  (OLD-INHIBIT-SCHEDULING-FLAG INHIBIT-SCHEDULING-FLAG) (INHIBIT-SCHEDULING-FLAG T)
  SUPERIOR-HAS-SCREEN-ARRAY OK ERROR)
  "Expose a sheet (place it on the physical screen)."
  (block nil
    (SETQ RESTORED-BITS-P T)
    (OR BITS-ACTION (SETQ BITS-ACTION (IF BIT-ARRAY :RESTORE :CLEAN)))
    (AND EXPOSED-P (RETURN ()))
    (SETQ RESTORED-BITS-P ())
    (SETQ SUPERIOR-HAS-SCREEN-ARRAY (OR (NULL SUPERIOR) (SHEET-SCREEN-ARRAY SUPERIOR)))
    (MULTIPLE-VALUE-SETQ (OK BITS-ACTION ERROR)
      (SHEET-PREPARE-FOR-EXPOSE SELF T INHIBIT-BLINKERS BITS-ACTION X Y))
    (OR OK (THROW 'SHEET-ABORT-EXPOSE
                  ERROR))

    ;; Have made our area of the screen safe for us.  We'll now call
    ;; ourselves "exposed", even though we haven't put our bits on
    ;; the screen at all.  This will win, because we have ourself
    ;; locked, and if someone wants to cover us he'll have to go
    ;; blocked until we are done -- it's a cretinous thing to have
    ;; happen, but the system shouldn't come crashing to the ground
    ;; because of it.
    ;; *** INHIBIT-SCHEDULING-FLAG had better still be T ***
    (OR INHIBIT-SCHEDULING-FLAG
        (FERROR () "Hairy part of expose finished with INHIBIT-SCHEDULING-FLAG off"))

    ;; Lie by saying that we are exposed, because we aren't really,
    ;; but we are locked so it doesn't matter.
    (AND SUPERIOR-HAS-SCREEN-ARRAY (SETQ EXPOSED-P T PREPARED-SHEET ()))
    (AND SUPERIOR
         (OR
          (NOT (MEMBER SELF (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ))

          ;; Must always reorder in the case of temporary windows
          ;; since they are the only type of window that can be
          ;; exposed and overlapping some other exposed window.
          (SHEET-TEMPORARY-P))
         (SHEET-CONSING
          ;; Put us at the top.
          (SETF (SHEET-EXPOSED-INFERIORS SUPERIOR)
                (CONS SELF
                      (COPY-LIST
                       (DELETE SELF (THE LIST (SHEET-EXPOSED-INFERIORS SUPERIOR)) :TEST #'EQ))))))
    (COND
      ((AND SUPERIOR-HAS-SCREEN-ARRAY BIT-ARRAY)
       (SETF (SHEET-OUTPUT-HOLD-FLAG) 0)
       ;; Open all our blinkers, etc, but don't think this sheet
       ;; is prepared.
        (PREPARE-SHEET (SELF))
       (SETQ PREPARED-SHEET ())
       (LET ((ARRAY (IF SUPERIOR (SHEET-SUPERIOR-SCREEN-ARRAY) (SCREEN-BUFFER SELF))))
         (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-ELEMENT-TYPE SCREEN-ARRAY)
			 (ARRAY-DIMENSION        ARRAY 1)
                         (ARRAY-DIMENSION SCREEN-ARRAY 0)
			 ARRAY
                         (+ X-OFFSET (* Y-OFFSET (ARRAY-DIMENSION ARRAY 1))))))
      (SUPERIOR-HAS-SCREEN-ARRAY (SETQ SCREEN-ARRAY OLD-SCREEN-ARRAY)
       (SETF (SHEET-OUTPUT-HOLD-FLAG) 0)))
    (when (and SUPERIOR-HAS-SCREEN-ARRAY (mac-window-p self))
      (redirect-drawing-of-window-and-inferiors self))
    (COND
      ((AND SUPERIOR-HAS-SCREEN-ARRAY (SHEET-TEMPORARY-P))
       (IF (EQ TEMPORARY-BIT-ARRAY T)
           (SETQ TEMPORARY-BIT-ARRAY
                 (MAKE-ARRAY `(,HEIGHT ,(LOGAND -32 (+ 31 WIDTH)))
			     :TYPE (SHEET-ARRAY-TYPE SELF)))
           (PAGE-IN-PIXEL-ARRAY TEMPORARY-BIT-ARRAY () (LIST WIDTH HEIGHT)))
       (BITBLT ALU-SETA WIDTH HEIGHT SCREEN-ARRAY 0 0 TEMPORARY-BIT-ARRAY 0 0)
       (PAGE-OUT-PIXEL-ARRAY TEMPORARY-BIT-ARRAY () (LIST WIDTH HEIGHT))))
    (DOLIST (SHEET *SHEETS-MADE-INVISIBLE-TO-MOUSE*)
      (SETF (SHEET-INVISIBLE-TO-MOUSE-P SHEET) ()))
    (SETQ *SHEETS-MADE-INVISIBLE-TO-MOUSE* ())
    (COND
      (EXPLODING-MOMENTARY-WINDOWS
       (LET* ((THICKNESS 2)
              (RATE 13)
              (DX (TRUNCATE (- WIDTH  RIGHT-SHADOW-WIDTH ) 2))
              (DY (TRUNCATE (- HEIGHT BOTTOM-SHADOW-WIDTH) 2))
              (LR-INC (MAX 1 (TRUNCATE DX RATE)))
              (TB-INC (MAX 1 (TRUNCATE DY RATE))))
         (PREPARE-SHEET (SELF)
                        (DO* ((K 1 (+ K RATE))
                              (X1 (- (+ DX (SHEET-INSIDE-LEFT SELF)) RIGHT-MARGIN-SIZE)
                               (- X1 LR-INC))
                              (X2 X1 (+ X2 LR-INC))
                              (Y1 (- (+ DY (SHEET-INSIDE-TOP SELF)) BOTTOM-MARGIN-SIZE)
                               (- Y1 TB-INC))
                              (Y2 Y1 (+ Y2 TB-INC))
                              (MY-WIDTH THICKNESS (- X2 X1))
                              (MY-HEIGHT THICKNESS (- Y2 Y1))
                              (NOTHINK 2560 (MAX 512 (- NOTHINK 192))))
                             ((OR (>= MY-WIDTH (- WIDTH RIGHT-SHADOW-WIDTH THICKNESS))
                              (>= MY-HEIGHT HEIGHT)))
                          (WITHOUT-INTERRUPTS
                           (DRAW-RECTANGLE-INSIDE-CLIPPED MY-WIDTH THICKNESS (+ X1 THICKNESS) Y1
                                                          ALU-XOR SELF)
                           (DRAW-RECTANGLE-INSIDE-CLIPPED THICKNESS MY-HEIGHT X2
                                                          (+ Y1 THICKNESS) ALU-XOR SELF)
                           (DRAW-RECTANGLE-INSIDE-CLIPPED MY-WIDTH THICKNESS X1 Y2 ALU-XOR SELF)
                           (DRAW-RECTANGLE-INSIDE-CLIPPED THICKNESS MY-HEIGHT X1 Y1 ALU-XOR SELF)
                           (DOTIMES (X NOTHINK))
                           (DRAW-RECTANGLE-INSIDE-CLIPPED MY-WIDTH THICKNESS (+ X1 THICKNESS) Y1
                                                          ALU-XOR SELF)
                           (DRAW-RECTANGLE-INSIDE-CLIPPED THICKNESS MY-HEIGHT X2
                                                          (+ Y1 THICKNESS) ALU-XOR SELF)
                           (DRAW-RECTANGLE-INSIDE-CLIPPED MY-WIDTH THICKNESS X1 Y2 ALU-XOR SELF)
                           (DRAW-RECTANGLE-INSIDE-CLIPPED THICKNESS MY-HEIGHT X1 Y1 ALU-XOR SELF)))))))
    (MOUSE-DISCARD-CLICKAHEAD)
    (MOUSE-WAKEUP)

    ;; This goes after preceeding code so that blinkers won't accidentally
    ;; turn on before the bits get BITBLT'ed into the temporary array.
    (SETQ INHIBIT-SCHEDULING-FLAG OLD-INHIBIT-SCHEDULING-FLAG)
    (COND
      (SUPERIOR-HAS-SCREEN-ARRAY
       (CASE BITS-ACTION
         (:NOOP NIL)
         (:RESTORE (SEND SELF :REFRESH :USE-OLD-BITS))
         (:CLEAN (SHEET-HOME SELF) (SEND SELF :REFRESH :COMPLETE-REDISPLAY))
         (OTHERWISE (FERROR () "Unknown BITS-ACTION ~S" BITS-ACTION)))
       (OR INHIBIT-BLINKERS (DESELECT-SHEET-BLINKERS SELF))
       (OR BIT-ARRAY
           ;; Expose in opposite order for the sake of temporary windows
           (DOLIST (INFERIOR (REVERSE EXPOSED-INFERIORS))
             (FUNCALL INFERIOR :EXPOSE ())))
       (RETURN T))))) 

(DEFMETHOD (MOMENTARY-MENU :DEEXPOSE)
	   (&OPTIONAL (SAVE-BITS-P :DEFAULT) SCREEN-BITS-ACTION (REMOVE-FROM-SUPERIOR T))
  "Deexpose a sheet (removing it virtually from the physical screen,
some bits may remain)"
  (DELAYING-SCREEN-MANAGEMENT
   (COND
     ((AND (EQ SAVE-BITS-P :DEFAULT) (NOT (ZEROP (SHEET-FORCE-SAVE-BITS))) EXPOSED-P)
      (SETQ SAVE-BITS-P :FORCE) (SETF (SHEET-FORCE-SAVE-BITS) 0)))
   (LET ((SW SELECTED-WINDOW))
     (AND SW (SHEET-ME-OR-MY-KID-P SW SELF) (FUNCALL SW :DESELECT ())))
   (OR SCREEN-BITS-ACTION (SETQ SCREEN-BITS-ACTION :NOOP))
   (COND
     (EXPOSED-P
      (OR BIT-ARRAY
          ;; We do not have a bit-array, take our inferiors off screen.
	  (EQ SAVE-BITS-P :FORCE)      ;but leave them in EXPOSED-INFERIORS
          (DOLIST (INFERIOR EXPOSED-INFERIORS)
            (FUNCALL INFERIOR :DEEXPOSE SAVE-BITS-P :NOOP ())))
      (WITHOUT-INTERRUPTS
       (AND (EQ SAVE-BITS-P :FORCE)
            (NULL BIT-ARRAY)
            ;; We are to force a saving of the SCREEN-ARRAY and
            ;; there isn't a BIT-ARRAY.  We must create a BIT-ARRAY.
            (SETQ BIT-ARRAY
                  (MAKE-ARRAY
                   `(,HEIGHT ,(LOGAND (+ (TRUNCATE (* LOCATIONS-PER-LINE 32)
						   (SCREEN-BITS-PER-PIXEL (SHEET-GET-SCREEN SELF)))
					 31)
				      -32))
		   :TYPE (SHEET-ARRAY-TYPE SELF))
                  OLD-SCREEN-ARRAY ())
	    (WHEN (mac-system-p)
	      (send-adjust-bit-array-maybe self)))
       (PREPARE-SHEET (SELF)
                      (AND SAVE-BITS-P BIT-ARRAY
                           (PROGN
                             (PAGE-IN-PIXEL-ARRAY BIT-ARRAY () (LIST WIDTH HEIGHT))
                             (BITBLT ALU-SETA WIDTH HEIGHT SCREEN-ARRAY 0 0 BIT-ARRAY 0 0)
                             (PAGE-OUT-PIXEL-ARRAY BIT-ARRAY () (LIST WIDTH HEIGHT)))))
       (COND
         ((SHEET-TEMPORARY-P) (PAGE-IN-PIXEL-ARRAY TEMPORARY-BIT-ARRAY () (LIST WIDTH HEIGHT))
          (BITBLT ALU-SETA WIDTH HEIGHT TEMPORARY-BIT-ARRAY 0 0 SCREEN-ARRAY 0 0)
          (PAGE-OUT-PIXEL-ARRAY TEMPORARY-BIT-ARRAY () (LIST WIDTH HEIGHT))
          (DOLIST (SHEET TEMPORARY-WINDOWS-LOCKED)
            (SHEET-RELEASE-TEMPORARY-LOCK SHEET SELF))
          (SETQ TEMPORARY-WINDOWS-LOCKED ()))
         (T
          (CASE SCREEN-BITS-ACTION
            (:NOOP)
            (:CLEAN (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 (sheet-erase-aluf self) SELF)) ;;; >>> alu-andca  --- erase-aluf
            (OTHERWISE (FERROR () "~S is not a valid bit action" SCREEN-BITS-ACTION)))))
       (COND
         (EXPLODING-MOMENTARY-WINDOWS
          (PREPARE-SHEET (SELF)
                         (LET* ((THICKNESS 2)
                                (RATE 13)
                                (DX (TRUNCATE (- WIDTH  RIGHT-SHADOW-WIDTH ) 2))
                                (DY (TRUNCATE (- HEIGHT BOTTOM-SHADOW-WIDTH) 2))
                                (LR-INC (MAX 1 (TRUNCATE DX RATE)))
                                (TB-INC (MAX 1 (TRUNCATE DY RATE))))
                           (DO* ((K 1 (+ K RATE))
                                 (X1 (SHEET-INSIDE-LEFT SELF) (+ X1 LR-INC))
                                 (X2 (SHEET-INSIDE-RIGHT SELF) (- X2 LR-INC))
                                 (Y1 (SHEET-INSIDE-TOP SELF) (+ Y1 TB-INC))
                                 (Y2 (SHEET-INSIDE-BOTTOM SELF) (- Y2 TB-INC))
                                 (MY-WIDTH (- X2 X1) (- X2 X1))
                                 (MY-HEIGHT (- Y2 Y1) (- Y2 Y1))
                                 (NOTHINK 320 (MIN 2560 (+ NOTHINK 192))))
                                ((OR (< X2 X1) (< Y2 Y1)))
                             (WITHOUT-INTERRUPTS
                              (DRAW-RECTANGLE-INSIDE-CLIPPED MY-WIDTH THICKNESS (+ X1 THICKNESS)
                                                             Y1 ALU-XOR SELF)
                              (DRAW-RECTANGLE-INSIDE-CLIPPED THICKNESS MY-HEIGHT X2
                                                             (+ Y1 THICKNESS) ALU-XOR SELF)
                              (DRAW-RECTANGLE-INSIDE-CLIPPED MY-WIDTH THICKNESS X1 Y2 ALU-XOR
                                                             SELF)
                              (DRAW-RECTANGLE-INSIDE-CLIPPED THICKNESS MY-HEIGHT X1 Y1 ALU-XOR
                                                             SELF))
                             (DOTIMES (X NOTHINK))
                             (WITHOUT-INTERRUPTS
                              (DRAW-RECTANGLE-INSIDE-CLIPPED MY-WIDTH THICKNESS (+ X1 THICKNESS)
                                                             Y1 ALU-XOR SELF)
                              (DRAW-RECTANGLE-INSIDE-CLIPPED THICKNESS MY-HEIGHT X2
                                                             (+ Y1 THICKNESS) ALU-XOR SELF)
                              (DRAW-RECTANGLE-INSIDE-CLIPPED MY-WIDTH THICKNESS X1 Y2 ALU-XOR
                                                             SELF)
                              (DRAW-RECTANGLE-INSIDE-CLIPPED THICKNESS MY-HEIGHT X1 Y1 ALU-XOR
                                                             SELF)))))))
       (SETQ EXPOSED-P ())
       (AND REMOVE-FROM-SUPERIOR SUPERIOR
            (SETF (SHEET-EXPOSED-INFERIORS SUPERIOR)
                  (DELETE SELF (THE LIST (SHEET-EXPOSED-INFERIORS SUPERIOR)) :TEST #'EQ)))
       (IF (NULL BIT-ARRAY) (SETQ OLD-SCREEN-ARRAY SCREEN-ARRAY SCREEN-ARRAY ())
           (REDIRECT-ARRAY SCREEN-ARRAY (ARRAY-ELEMENT-TYPE BIT-ARRAY)
			   (ARRAY-DIMENSION BIT-ARRAY 1)
                           (ARRAY-DIMENSION BIT-ARRAY 0)
			   BIT-ARRAY 0))
       (when (mac-window-p self)
	 (redirect-drawing-of-window-and-inferiors self))
       (SETF (SHEET-OUTPUT-HOLD-FLAG) 1)))
     (REMOVE-FROM-SUPERIOR
      (AND SUPERIOR
           (SETF (SHEET-EXPOSED-INFERIORS SUPERIOR)
                 (DELETE SELF (THE LIST (SHEET-EXPOSED-INFERIORS SUPERIOR)) :TEST #'EQ))))))) 

(DEFFLAVOR MOMENTARY-WINDOW-HACKING-MENU () (WINDOW-HACKING-MENU-MIXIN MOMENTARY-MENU)
  (:DOCUMENTATION :COMBINATION))

(DEFFLAVOR DYNAMIC-MOMENTARY-MENU () (DYNAMIC-ITEM-LIST-MIXIN MOMENTARY-MENU))
(DEFFLAVOR DYNAMIC-MOMENTARY-WINDOW-HACKING-MENU
	()
	(DYNAMIC-ITEM-LIST-MIXIN MOMENTARY-WINDOW-HACKING-MENU))
(DEFFLAVOR DYNAMIC-POP-UP-MENU () (DYNAMIC-ITEM-LIST-MIXIN TEMPORARY-MENU))
(DEFFLAVOR DYNAMIC-POP-UP-COMMAND-MENU ()
  (DYNAMIC-ITEM-LIST-MIXIN Temporary-Shadow-Borders-Window-Mixin COMMAND-MENU))
(DEFFLAVOR DYNAMIC-POP-UP-ABORT-ON-DEEXPOSE-COMMAND-MENU ()
	   (COMMAND-MENU-ABORT-ON-DEEXPOSE-MIXIN DYNAMIC-POP-UP-COMMAND-MENU))
(DEFFLAVOR DYNAMIC-TEMPORARY-MENU () (DYNAMIC-ITEM-LIST-MIXIN TEMPORARY-MENU))
(DEFFLAVOR DYNAMIC-TEMPORARY-COMMAND-MENU ()
  (DYNAMIC-ITEM-LIST-MIXIN Temporary-Shadow-Borders-Window-Mixin COMMAND-MENU))
(DEFFLAVOR DYNAMIC-TEMPORARY-ABORT-ON-DEEXPOSE-COMMAND-MENU ()
	   (COMMAND-MENU-ABORT-ON-DEEXPOSE-MIXIN DYNAMIC-TEMPORARY-COMMAND-MENU))
(DEFFLAVOR DYNAMIC-MULTICOLUMN-MOMENTARY-MENU ()
	   (DYNAMIC-MULTICOLUMN-MIXIN MOMENTARY-MENU))
(DEFFLAVOR DYNAMIC-MULTICOLUMN-MOMENTARY-WINDOW-HACKING-MENU ()
	   (WINDOW-HACKING-MENU-MIXIN DYNAMIC-MULTICOLUMN-MOMENTARY-MENU))

;;; Fixed up MENU-CHOOSE to clear the MOMENTARY-MENU resource if
;;; something bad happened.  Fixes bug report 876 from FAULKNER.

;;; Define the old momentary menu resource so that the old version of
;;; menu-choose will exhibit its old behavior.
(DEFWINDOW-RESOURCE OLD-MOMENTARY-MENU ()
  :MAKE-WINDOW (MOMENTARY-MENU)
  :REUSABLE-WHEN :DEEXPOSED
  :INITIAL-COPIES 0)

(COMPILER:MAKE-OBSOLETE MENU-CHOOSE "Use W:MENU-CHOOSE instead")
(DEFUN MENU-CHOOSE (ALIST &OPTIONAL (LABEL NIL) (NEAR-MODE '(:MOUSE)) DEFAULT-ITEM (SUPERIOR MOUSE-SHEET))
  "Let user choose an element of ALIST with a menu.
ALIST		an ITEM-LIST for a menu.
LABEL		the label of the menu, if not NIL.
NEAR-MODE	specifies how to decide where to put the menu; see EXPOSE-WINDOW-NEAR.
DEFAULT-ITEM	an item that is EQ to an element of ALIST; the mouse will be positioned
		initially over that item.  Or it can be NIL.

The menu is made an inferior of SUPERIOR.

If the user chooses an item, the values are:
	1) the value computed from that item, and
	2) the item itself (an element of ALIST).
Otherwise, NIL is returned."
  (WHEN ALIST
    (AND (EQ (CAR NEAR-MODE) :WINDOW) (SETQ SUPERIOR (SHEET-SUPERIOR (CADR NEAR-MODE))))

    ;; The following code is somewhat contorted.  The idea is that if
    ;; menu code doesn't execute all the way to the end there must have
    ;; been some kind of error.  In this case, the resource is cleared
    ;; so that is will not affect a subsequent execution.
    (LET (MENU; The MENU resource
           MENU-CHOOSE-WAS-ABORTED; T when the user aborts, NIL otherwise
                VALUE-CHOSEN; Value chosen by user, or NIL
                                        LAST-ITEM); Last item chosen by user, or NIL
      (UNWIND-PROTECT (PROGN
                       (SETQ MENU (ALLOCATE-RESOURCE 'OLD-MOMENTARY-MENU SUPERIOR))
                       (SETQ MENU-CHOOSE-WAS-ABORTED MENU)
                       (delaying-compute-geometry
			 (FUNCALL MENU :SET-LABEL LABEL))
                       (FUNCALL MENU :SET-ITEM-LIST ALIST)
                       (FUNCALL MENU :SET-LAST-ITEM DEFAULT-ITEM)
                       (EXPOSE-WINDOW-NEAR MENU NEAR-MODE)
                       (AND DEFAULT-ITEM
                            (NOT (MEMBER (CAR NEAR-MODE) '(:MOUSE :POINT) :TEST #'EQ))
                            (MULTIPLE-VALUE-BIND (X Y) (FUNCALL MENU :ITEM-CURSORPOS DEFAULT-ITEM)
                              (AND X Y
                                   (FUNCALL MENU :SET-MOUSE-POSITION
                                            (+ X (SHEET-INSIDE-LEFT MENU))
                                            (+ Y (SHEET-INSIDE-TOP MENU))))))
                       (SETQ VALUE-CHOSEN (SEND MENU :CHOOSE) LAST-ITEM (SEND MENU :LAST-ITEM))
                       (SETQ MENU-CHOOSE-WAS-ABORTED ()))
                      (DEALLOCATE-RESOURCE 'OLD-MOMENTARY-MENU MENU)
        (WHEN MENU-CHOOSE-WAS-ABORTED
          (CLEAR-RESOURCE 'OLD-MOMENTARY-MENU MENU)))
      (VALUES VALUE-CHOSEN LAST-ITEM)))) 

(COMPILER:MAKE-OBSOLETE MULTICOLUMN-MENU-CHOOSE "Use W:MULTIPLE-MENU-CHOOSE instead")
(DEFUN MULTICOLUMN-MENU-CHOOSE (COLUMN-SPEC-LIST &OPTIONAL (LABEL NIL) (NEAR-MODE '(:MOUSE)) DEFAULT-ITEM
  (SUPERIOR MOUSE-SHEET))
  "Let user choose an element of COLUMN-SPEC-LIST with a multicolumn menu.
This function works very similar to TV:MENU-CHOOSE.
COLUMN-SPEC-LIST is a list of column specifications permitted in multicolumn menus; that is,
it is a list of entries of the form (<string> <item list> . <options>) where <string> is used
as the column header, <item list> is either a normal menu item list or a symbol
whose value is a normal menu item list, and <options> are keyword-arg column options
such as :FONT.

LABEL is used as the label of the menu, if not NIL.
NEAR-MODE specifies how to decide where to put the menu; see EXPOSE-WINDOW-NEAR.
DEFAULT-ITEM	an item that is EQ to an element of ALIST; the mouse will be positioned
		initially over that item.  Or it can be NIL.
The menu is made an inferior of SUPERIOR.

If the user chooses an item, the values are 1) the value computed
from that item, and 2) the item itself (an element of ALIST).
Otherwise, NIL is returned."
  (WHEN COLUMN-SPEC-LIST
    (AND (EQ (CAR NEAR-MODE) :WINDOW) (SETQ SUPERIOR (SHEET-SUPERIOR (CADR NEAR-MODE))))
    (USING-RESOURCE (MENU DYNAMIC-MULTICOLUMN-MOMENTARY-MENU SUPERIOR)
                    (delaying-compute-geometry
		      (FUNCALL MENU :SET-LABEL LABEL))
                    (FUNCALL MENU :SET-COLUMN-SPEC-LIST COLUMN-SPEC-LIST)
                    (FUNCALL MENU :UPDATE-ITEM-LIST) (FUNCALL MENU :SET-LAST-ITEM DEFAULT-ITEM)
                    (EXPOSE-WINDOW-NEAR MENU NEAR-MODE)
                    (AND DEFAULT-ITEM (NOT (MEMBER (CAR NEAR-MODE) '(:MOUSE :POINT) :TEST #'EQ))
                         (MULTIPLE-VALUE-BIND (X Y) (FUNCALL MENU :ITEM-CURSORPOS DEFAULT-ITEM)
                           (AND X Y
                                (FUNCALL MENU :SET-MOUSE-POSITION (+ X (SHEET-INSIDE-LEFT MENU))
                                         (+ Y (SHEET-INSIDE-TOP MENU))))))
                    (VALUES (SEND MENU :CHOOSE) (SEND MENU :LAST-ITEM))))) 

;;;patched on 11 Dec 85 for DLS by GSM
(COMPILER:MAKE-OBSOLETE MULTIPLE-MENU-CHOOSE "Use W:MULTIPLE-MENU-CHOOSE instead")
(DEFUN MULTIPLE-MENU-CHOOSE (ALIST &OPTIONAL (LABEL NIL) (NEAR-MODE '(:MOUSE))
			     HIGHLIGHTED-ITEMS
			     (SUPERIOR MOUSE-SHEET))
  "Let user choose some set of elements of ALIST with a multiple menu.
ALIST looks like an ITEM-LIST for a menu.
LABEL is used as the label of the menu, if not NIL.
NEAR-MODE specifies how to decide where to put the menu; see EXPOSE-WINDOW-NEAR.
HIGHLIGHTED-ITEMS is a set of elements of ALIST;
 these will form the initially chosen subset, which the user can modify.
The menu is made an inferior of SUPERIOR.

If the user exits by clicking on the Do It box, there are two values:
1) a list of the values from executing the selected set of items, and
2) T.
If the user moves the mouse away from the menu,
NIL is returned for both values."
  (WHEN ALIST
    (AND (EQ (CAR NEAR-MODE) :WINDOW) (SETQ SUPERIOR (SHEET-SUPERIOR (CADR NEAR-MODE))))
    (USING-RESOURCE (MENU MOMENTARY-MULTIPLE-MENU SUPERIOR)
;;;    (FUNCALL MENU :SET-LABEL LABEL)		; patch by dls
;;;    (FUNCALL MENU :SET-ITEM-LIST ALIST)	; patch by dls
;;;      (delaying-compute-geometry
	(FUNCALL MENU :SET-ITEM-LIST ALIST)	; patch by dls
      (FUNCALL MENU :SET-LABEL LABEL)		; patch by dls
      (FUNCALL MENU :SET-HIGHLIGHTED-ITEMS HIGHLIGHTED-ITEMS)
      (EXPOSE-WINDOW-NEAR MENU NEAR-MODE)
      (SEND MENU :CHOOSE)
      )))
