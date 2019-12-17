;;; -*- Mode:Common-Lisp; Package:W; Base:10.; Fonts:CPTFONT,HL12B,HL12BI -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1983-1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  10-25-88  MAY      Changed (MENU :AFTER :SET-FONT-MAP) and (MENU :REDEFINE-MARGINS) to handle 
;;;                         case of menu NEITHER sheet-menu-pop-up NOR sheet-menu-permanent. 
;;;                         Corrects bug introduced 09-07-88. Fixes addin-bug #194.
;;;  09-14-88  MAY	Changed DEFAULT-MENU-ITEM-DOCUMENTER to call #'documention with 'variable instead
;;;                         of default 'function so that variable with doc like *print-base* displays mouse doc in
;;; 			w:menu-choose. Changed (MENU :END-ITEM-SELECTION) MENU-MARGIN-CHOICE-FUNCTION 
;;;			and MENU-SIMPLE-MARGIN-CHOICE-FROM-ITEM to use eighth instead of sixth item in 
;;;			choice-box to put :documentation strings back in. SPR 8241.
;;;  09-07-88  MAY	Changed (MENU :REDEFINE-MARGINS) and (MENU :AFTER :SET-FONT-MAP) to preserve 
;;;                         OUTSIDE size for :permanent windows so that :geometry is not ignored.  SPR 8504.
;;;  07-29-88  MAY      Fixed Assert in w:-menu-choose for SPR 7599
;;;  02/11/88   LG         Made %draw-rectangle in :PARTIAL-MENU-DRAW write to SELF
;;;		        rather than SCREEN-ARRAY so it'll work to a Mac, too.
;;;   11/30/87   PMH     Modified :scroll-to to not use the BITBLT optimization when the highlighted-items
;;;                         list is not empty.
;;;   9/17/87  cmi        Fix several bugs found by test cases. Dynamic, Sort, and other small changes
;;;   8/27/87  KWW       Made minor changes based on code reading
;;;   7/2/87   KWW      Added code to make :remove highlight work in color
;;;   6/29/87  KWW      Modified the function menu-choose to accept keywords to set foreground and background color
;;;   6/29/87  KWW      Modified highlighting to work in color
;;;   6/25/87  KWW      Modified make-window calls to set up proper foreground and background color
;;;   4/12/87  TWE	Fixed reverse video menus for NLMENU.  Changed the :MENU-DRAW erasing to use the
;;;			erase-aluf alu instead of alu-setz.
;;;   3/20/87  TWE	Fixed up the menu-item-sorter to handle the predicate just like it handles the key
;;;			to allow the user to specify it as either a symbol or as a #'function.
;;;   3/20/87  TWE	Fixed up :partial-menu-draw so that the fixes made on 3/10/87 are back again (can
;;;			you say `leap frog'?)
;;;   3/20/87  TWE	Fixed the initialization of geometry-max-width/height in :after :set-edges so that
;;;			suggestions could be brought up.
;;;   3/13/87  KK	New :SCROLL-TO implementation for faster continuous scrolling. For a small scroll delta,
;;;			this uses BITBLT to quickly move part of the menu image up or down. Uses new
;;;			:PARTIAL-MENU-DRAW method. :MENU-DRAW modified to also use :PARTIAL-MENU-DRAW.
;;;			Fixes SPR #3499.
;;;   3/13/87  TWE	Changed the item alignment to be :LEFT when an invalid value is specified.  Also, in
;;;			menu-choose, made sure that if a command menu is specified that an IO buffer is also
;;;			specified.  This fixes bug 3746.  Changed menu-fill-width to properly handle a NIL
;;;			item list (and return 0).  This fixes bug 3729.  Changed the :END-ITEM-SELECTION
;;;			method to handle the :buttons menu item.  This fixes bug 3723.
;;;   3/12/87  TWE	Fixed up menu-deduce-parameters to change filled menus to default to a landscape
;;;			orientation instead of the portrait orientation.  Also changed the drawing functions
;;;			to properly draw the filled items.  This fixes bug 3747.  Fixed up menu-item-string-out
;;;			to work properly with the system menu.
;;;   3/11/87  TWE	Defined a geometry validatation function.  Also fixed up menu-choose to work better
;;;			with the validation function.  This fixes bug 3751.
;;;   3/10/87  TWE	Fixed the long menu item overwriting problem by outputting menu items using the new
;;;			function MENU-ITEM-STRING-OUT.  This fixes bug 3909.
;;;   3/05/87  TWE	Fixed up :choose and :process character to handle clicks on :no-select items properly.
;;;			This fixes bugs 3502 and 3928.  Also fixed up the :abort-or-doit margin choices to
;;;			work properly.  The cases of :abort-and-doit were changed appropriately.  This fixes
;;;			bug 3579.  Also fixed up make-simple-icon to allow the icon-name to be either a symbol
;;;			or a string.  This fixes bug 3503.
;;;   3/04/87  TWE	Fixed up the :SET-ITEM-LIST method to remove NILs like the old version did.  This fixes
;;;			bug 3501.
;;;   3/04/87  TWE	Changed MENU-FILL-BREAKAGE so that its value is the same as
;;;			MENU-INTERWORD-SPACING.  This fixes bug number 3218.
;;;   3/02/87  TWE	Added a :NAME-FOR-SELECTION method to make the new menus act more like the old
;;;			ones as far as being selectable is concerned.
;;;   2/27/87  TWE	Fixed the multiple-menu-choose highlighting problem by setting the highlighted items
;;;			properly.  This fixes bug 3573.
;;;   2/24/87  TWE	Changed menu-deduce-parameters to calculate the max-height and max-width much
;;;			like it did in the old system 98 days, using a constant of 4096 instead of 512.  This
;;;			fixes bug 3011.
;;;   2/09/87  KDB	Changed the :MARGIN-CHOICES-MINIMUM-WIDTH method to make the margin choice
;;;                          not overwrite themselves when there are many margin choices. (Changed 5 --> 10)
;;;  1/30/87   TWE	Fixed up dynamic menus so that the menu geometry gets recalculated when the
;;;			:UPDATE-ITEM-LIST message is sent.
;;;  1/29/87   TWE	Fixed up permanent menus so that hysteresis gets set to zero.  This makes mouse
;;;			tracking in permanent menus work like it did with the old style menus.  Also corrected
;;;			the initialization of a default border for permanent menus.
;;;  1/21/87   TWE	Updated the settable/gettable parts of the MENU flavor to agree with the documentation.
;;;  1/14/87   KDB	Continued fix done on 1/9/87.
;;;  1/09/87   KDB	Fixed bug that made ALL Top-box-labels appear in reverse-video if W:Menu-based 
;;;			by setting TEMPORARY-BIT-ARRAY in W:MENU :after :init. Also, value of :PERMANANT
;;;			is now access via  SHEET-FLAG instance variable.
;;;  12/18/86  SLM	Fixed Suggestions call to (locally (declare..))
;;;  11/26/86  TWE	Added SYS package prefix for PRINTING-RANDOM-OBJECT.  Added a DECLARE IGNORE
;;;			to DRAW-ICON to make the compiler not display a warning message.  Changed instance
;;;			variable initialization for color support.
;;;  11/25/86  TWE	Added hooks for color support.
;;;  11/20/86  SLM       Added calls to Suggestions macros for W:MENU-ITEM-STRING.
;;;  10/21/86  TWE	Moved the definition of the `Do It' and `Abort' strings from here to MARGIN-REGION.
;;;  10/21/86  TWE	Put the searching commands into the MENU-DEFAULT-COMMAND-CHARACTERS table.
;;;  10/09/86  TWE	Fixed the scrolling change done on 10/02 to be more correct (and use the MENU flavor).
;;;  10/06/86  TWE	Changed :MOVE-NEAR-WINDOW to properly send the :select method instead of nil.
;;;  10/03/86  GRH	Added call to system-menu for #\mouse-r-2 for menu panes if not over an item.
;;;  10/02/86  TWE	Added in code to turn scrolling off if the menu is small enough.  This differs from
;;;			the 9/19 version in that the guts is in the scrolling code (and it works much better).
;;;  09/19/86  TWE	Removed the code in menu-choose which turns off scrolling if the menu is too small.
;;;			It turned out to be a bit buggy.
;;;  09/18/86  TWE	Corrected the spelling of the word `mouse' for the help display.
;;;  09/17/86  TWE	Fixed scrolling in w:menu-choose to turn it off if the user didn't specify scrolling-p
;;;			and the menu is large enough to display all of the menu items.  It seems silly to show
;;;			a menu as having scrolling when all of the items are visible.
;;;  09/15/86  TWE	Changed the default item-alignment to :LEFT according to a human factors expert's
;;;			opinion at the 1986 SIGGRAPH conference.  Also fixed up menu-choose to generate
;;;			an error when the user specifies :permenant t and :pop-up t.  Fixed up :SET-ITEM-LIST)
;;;			to properly handle highlighted items (instead of ignoring them).  Fixed up :HELP to have
;;;			it only mention the space character when the menu has highlighted items.
;;;  09/12/86  TWE	Changed the documentation in MENU-CHOOSE to mention that sorting the item list is
;;;			destructive.  Also changed (MENU :EXECUTE) to allow it to handle poorly formed items
;;;			such as ("Foo" :documentation "Hi there") in a reasonable manner.  Strictly speaking
;;;			items such as these are not allowed according to the documentation, but can be 
;;;			handled in a reasonable manner.  Also fixed up MENU-CHOOSE to properly handle the
;;;			dynamic keyword.
;;;  09/10/86  TWE	Fixed up the help display to be more consistant.
;;;  08/28/86  TWE	Added a new function MOVE-TO-NEAREST-MENU-ITEM to aid in getting incremental
;;;			searching and cursor movement via the keyboard started.
;;;  08/28/86  TWE	Changed :mouse-moves and :mouse-standard-blinker to change the mouse blinker when
;;;			the mouse is on an item into a small dot.  In this way the mouse blinker does not
;;;			obscure part of the menu item, but it is still visible if the user's screen is clean enough.
;;;  08/27/86  TWE	Fixed up MENU-ITEM-STRING to check for the case where the string function can't
;;;			handle and then do what the old string function used to do.  Also added the
;;;			CONTROL-G command to make menus more like Zmacs.
;;;  08/14/86  TWE	Added more instance variables for searching.
;;;  08/13/86  TWE	Fixed the icon defstruct so that the icon-p predicate is generated.
;;;  08/13/86  TWE	Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;;  08/07/86  TWE	Fixed the ICON defstuct to contain a (:type :array-leader) to make it compatible with
;;;			the FONT defstruct.
;;;  07/29/86  TWE	Changed to use Common Lisp functions.
;;;  07/09/86  TWE	Removed all debug code.
;;;  07/08/86  TWE	Changed the :MARGIN-CHOICES-MINIMUM-WIDTH method to make the margin choice 
;;;			calculation more accurate.  (Changed 2 --> 5.)
;;;  07/02/86  TWE	Added debug code to the :before/:after :init methods to see where the system menu crashed in VM2.
;;;  05/21/86  TWE	Added support for icons.
;;;  05/06/86  TWE	Changed to use the W package instead of TV.  Also renamed the symbols to not have
;;;			the GENERAL- prefix.
;;;  04/16/86  GRH	Added updating of who-line doc after boxing menu items.
;;;  04/15/86  TWE	Fixed up many things to make menus work during a system build.
;;;  04/10/86  TWE       Fixed MENU-COMPUTE-GEOMETRY to test *delay-compute-geometry* properly.
;;;  04/08/86  LGO	Make STRING inline for MENU-ITEM-STRING 
;;;  04/08/86  LGO	Make FONT-EVALUATE inline for MENU-ITEM-STRING-WIDTH
;;;  04/08/86  LGO	Added DELAYING-COMPUTE-GEOMETRY macro call to MENU-CHOOSE to prevent
;;;			the geometry from being computed twice, once for the label and once for the item-list.
;;;  04/08/86  LGO	Remove call to MENU-COMPUTE-GEOMETRY from (:METHOD MENU :SET-DEFAULT-FONT)
;;;  04/04/86  TWE       Origional code consolidated from the MENU file.

#|

This file contains the definition of the MENU flavor.  This menu
combines several different menu types into one, yielding improvements in
several areas such as performance and programming ease.  In addition  it
is smaller than the existing menu code, making is easier to understand.

Another major improvement is  the addition of  keystroke input to  allow
the user to control the position of the mouse cursor from the  keyboard.
The HELP key  can be  used to  obtain a  list of  the commands which are
available.  These commands are a  subset of movement commands  available
in Zmacs.

The following is a list of  those things which were changed/added  which
will require a change in the documentation.

 (1) The  MENU  flavor  was  defined.   There  are  a lot of new
     initialization  options  and  instance   variables.   Be  sure   to
     advertise that the :SORT option will  destructively modify the item
     list.

 (2) The :MENU-MARGIN-CHOICES init option  allows a margin choice  list,
     or a keyword :ABORT-OR-DOIT.  A margin choice can also be a keyword
     of :ABORT or :DOIT.

 (3) The MENU-CHOOSE function was defined.  This is an extension
     of MENU-CHOOSE which allows one to specify other kinds of menus  to
     create.  This function also uses  keywords to specify the  optional
     arguments.

 (4) The function MULTICOLUMN-MENU-CHOOSE was defined.  This  is
     very similar to the MULTICOLUMN-MENU-CHOOSE function except that it
     uses keywords to specify the optional arguments.

 (5) The  function  MULTIPLE-MENU-CHOOSE  was  defined.  This is
     very similar to  the MULTIPLE-MENU-CHOOSE  function except  that it
     uses keywords  to  specify  the  optional arguments.  An additional
     keyword was  added  (menu-margin-choices)  to  allow  the caller to
     specify other margin choices.  Also, instead of returning T as  the
     second value  when  the  user  clicks  the  `Do  It' box, the value
     returned is non-NIL.

 (6) A new initialization option  was added: ITEM-ALIGNMENT.  Its  value
     can be either :LEFT, :CENTER or :RIGHT.  It defaults to :LEFT.

 (7) A new initialization option was  added: SORT.  This will cause  the

     item list to  be sorted  according to  a predicate  and an optional
     key.  Its value can  be either :ASCENDING,  :DESCENDING, predicate,
     (:ASCENDING key),  (:DESCENDING  key),  or  (predicate key).  Where
     `predicate' and `key' are as required  by the sort function.  As  a
     shorthand, the user may specify T instead of :ASCENDING.


 (8) Keyboard   support   was  added.   Pressing   the   HELP  key  will
     display a  list  of  the  commands  which are available.  Basically
     these are the same as are available in choose-variable-values.

 (9) Added  support  for   icons.  The  user  passes  an instance  of an
     icon in the place where a string would be expected.

 |#

#|
 Newer menu system

Documentation on menu item-lists:

Each item in the item-list may be one of the following:

 1.  A string or a symbol.

 2.  Cons of a string (or a symbol) and an atom.

 3.  List of  a string (or a symbol)  and any object.  The list  may not
     be more than 2 long.

 4.  List  of  a  string  (or  a  symbol),  a type value keyword, and an
     argument.  After the first 3 elements of the list, the rest of  the
     list is a property list of modifier keywords and values.

In the places where a string may  be present, an icon instance can  also
be present.  In this  way, instead of  displaying a string  in the menu,
the caller-specified icon is displayed.

The string (or  symbol or  icon) is  displayed in  the menu to represent
this item.  The value returned by the :CHOOSE method is the item in case
1, the cdr in case 2, the cadr in case 3, and varies in case 4 depending
on  the  type  value  keyword.   Case   4  menu  items  can  also   have
side-effects.  The following are the permissible type value keywords:

	:VALUE - argument is returned by the :CHOOSE method.

	:EVAL - argument is evaluated then returned.

	:FUNCALL - argument is a function of no args to be called.

	:NO-SELECT - this item cannot be selected.

        :WINDOW-OP - argument  is  a  function  of  three  arguments.  The
                arguments are the  window, mouse-x, mouse-y as  they
                were before the menu popped up.

        :KBD - argument  is forced  into keyboard  input of  appropriate
                process.

	:MENU - argument is a new menu to choose from.

        :MENU-CHOOSE - arg is a list (label .  menu-choose-alist) passed
                to MENU-CHOOSE.

        :BUTTONS - argument  is 3  items, which  one is  used depends on
                which mouse button was clicked.

The following are the known modifier keywords:

	:FONT - the font in which to display the item.

	:DOCUMENTATION - a string documenting this item.

        :BINDINGS -  a  LET-list  of  bindings  to  do with PROGW before
                funcalling, evalling, etc.  With :BUTTONS, the :BINDINGS
                must be INSIDE the :BUTTONS.

This is largely although not entirely controlled by the :EXECUTE method,
which you may redefine.

|#


;;; These special variables exist so that there are less random numbers
;;; in the code, giving somewhat more chance of understanding it.  You
;;; might even want to change them.
(DEFPARAMETER MENU-GOLDEN-RATIO 1.6s0
              "Aspect ratio used for menus when there are no constraints.")

(DEFPARAMETER MENU-INTERCOLUMN-SPACING 10.
              "Minimum spacing between columns in non-filled menus.")

(DEFPARAMETER MENU-INTERWORD-SPACING 27.
              "Minimum spacing between items used in filled menus.")

(DEFPARAMETER MENU-FILL-BREAKAGE MENU-INTERWORD-SPACING
              "Estimated spacing needed for each item in a filled menu.
Make this larger to add extra blank lines below the menu items.")

;;; new variables added to control mouse glyph while menu is up  PMH 4/6/88
(defparameter *menu-mouse-item-glyph*  (make-char MOUSE-GLYPH-SMALL-DOT)
  "Character for mouse cursor while on an item within a menu.")

(defparameter *menu-mouse-no-item-glyph*   (make-char MOUSE-GLYPH-THIN-CROSS)
  "Character for mouse cursor while not on an item within a menu.")

(DEFVAR *DEFAULT-MENU-ITEM-WHO-LINE-DOCUMENTATION-FUNCTION*
	'DEFAULT-MENU-ITEM-DOCUMENTER
  "Function to call to document menu-items when the menu item is not documented")

;The GEOMETRY variable in a menu has a structure of this type as its value.
;That variable records "permanent" constraining shape information,
;that is to be used in recomputing the shape when things (such as the item list) change.
(DEFSTRUCT (GEOMETRY (:TYPE :LIST) (:CONSTRUCTOR NIL) (:CONC-NAME NIL) (:CALLABLE-CONSTRUCTORS NIL)
  (:ALTERANT ALTER-GEOMETRY) (:PREDICATE NIL) (:COPIER NIL))
  GEOMETRY-N-COLUMNS
  GEOMETRY-N-ROWS
  GEOMETRY-INSIDE-WIDTH
  GEOMETRY-INSIDE-HEIGHT
  GEOMETRY-MAX-WIDTH
  GEOMETRY-MAX-HEIGHT)

;; Define some `macros' which will allow us to make more sense about how
;; we save the notion of fill-p in the geometry.  A filled menu has a
;; number of columns of 0.
(DEFMACRO GEOMETRY-FILL-P (GEO) `(AND (GEOMETRY-N-COLUMNS ,GEO)
				      (ZEROP (GEOMETRY-N-COLUMNS ,GEO))))

(DEFUN SETF-GEOMETRY-N-COLUMNS (OBJECT NEW-VALUE)
  (SETF (GEOMETRY-N-COLUMNS OBJECT) (IF NEW-VALUE 0 NIL)))

(DEFSETF GEOMETRY-FILL-P SETF-GEOMETRY-N-COLUMNS)


#|

Icons are now supported in menus.  The  idea is to allow one to  specify
an ICON object which knows knows how  to draw itself and how big  it is,
and pass that information into  the menu code as  a menu item where  the
string would be.   In this  way, one  can construct  a generalized  icon
which can be seen in a menu and selected by a user.

|#

;;; A ICON is very similar to a FONT defstruct.  This was done to make
;;; it possible to use an icon in places where a font is needed.  This
;;; was done to avoid a rewrite of the menu code.  If this were done
;;; another way, the use of the ROW-HEIGHT instance variable would
;;; change from being a constant to a value determined by each row.
;;; This would make row and item position calculations much more costly.
;;; In this implementation, the icon is placed into the font map to make
;;; the line-height calculation the same as it has always been.

;;; An icon needs several pieces of data: a function which draws the icon, a
;;; width, a height, a baseline (as with fonts) and a name (for sorting
;;; purposes).  The drawing function has the following arguments:
;;;
;;;	ICON-OBJECT WINDOW X Y ITEM any-additional-arguments
;;;
;;; The ICON-OBJECT is an instance of the ICON defstruct.  The WINDOW is where
;;; the function is to draw the icon.  The X and Y specify the position in
;;; WINDOW where the upper left corner of the icon is to be located.  ITEM is
;;; the menu item for this icon.  Any additional arguments are passed from the
;;; ICON-ARGUMENTS slot in the ICON defstruct. 
;;;
;;; If one wants to define an icon with additional slots then all one needs to
;;; do is to define a new defstruct and use the INCLUDE defstruct option to
;;; include the ICON definition into the new defstruct.  It is also recommended
;;; that one defines a printer function for the new defstruct, as was done for
;;; the icon defstruct.

(DEFSTRUCT (ICON  :NAMED (:type :ARRAY-LEADER) (:INCLUDE FONT) (:CONC-NAME ICON-)
		  (:CALLABLE-CONSTRUCTORS NIL) (:COPIER NIL))
  DRAW-FUNCTION       ;Function to be invoked to draw the icon.
  ARGUMENTS           ;Additional arguments to pass to FUNCTION when drawing the icon.
  )

;;; This is how we tell the print function how to print an icon object.  If
;;; we don't do this then the print function will print out the entire
;;; description of the icon object (i.e. more than is needed).
(DEFUN (:PROPERTY ICON NAMED-STRUCTURE-INVOKE) (OP &OPTIONAL SLF ARG1 &REST IGNORE)
  (CASE OP
    (:WHICH-OPERATIONS '(:PRINT-SELF))
    ((:PRINT-SELF) (SYS:PRINTING-RANDOM-OBJECT (SLF ARG1 :TYPEP)
                     ;; Print out the icon name and its dimensions.
                     (PRINC (ICON-FONT-NAME        SLF) ARG1)
                     (PRINC "-"                              ARG1)
                     (PRINC (ICON-FONT-CHAR-WIDTH  SLF) ARG1)
                     (PRINC "-"                              ARG1)
                     (PRINC (ICON-FONT-CHAR-HEIGHT SLF) ARG1)))
    (OTHERWISE (FERROR () "~S unknown message to ~S" OP SLF))))


(DEFFLAVOR MENU
	   (
            ;; List of items being displayed.
            (ITEM-LIST NIL)

            ;; Item being pointed at now.
	    CURRENT-ITEM
            CURRENT-ITEM-ROW
            CURRENT-ITEM-COLUMN

            ;; The last item to have been selected.
	    LAST-ITEM

            ;; The same, but it's ok to set this to NIL and wait for it
            ;; to become non-NIL.
            (CHOSEN-ITEM NIL)

            ;; Type of alignment of menu items.  Can be either :LEFT, :CENTER or :RIGHT.
            (ITEM-ALIGNMENT :LEFT)

            ;; Number of rows in menu on screen.
	    SCREEN-ROWS

            ;; Sort the item list according to a predicate and an optional key.  Valid
            ;; values for this initialization option are: :ASCENDING :DESCENDING
            ;; predicate (:ASCENDING key) (:DESCENDING key) (predicate key).  Where
            ;; `predicate' and `key' are as required by the sort function.  As  a
            ;; shorthand, the user may specify T instead of :ASCENDING.
            (SORT NIL)

            ;; Total number of rows in menu. If this is greater than
            ;; SCREEN-ROWS, then the latter represent a window on all
            ;; the rows.
            TOTAL-ROWS

            ;; This is first row visible now.  Rows are numbered with the first being row 0.
	    TOP-ROW

            ;; Height in pixels of a row (including vsp).
	    ROW-HEIGHT

            ;; Array of tails of ITEM-LIST.  For each row in the menu, points to first
            ;; item on that row.  An extra element at the end is NIL.  The length is
            ;; thus (1+ TOTAL-ROWS).
            ROW-MAP

            ;; Number of columns (NIL in fill mode).
	    (COLUMNS NIL)

            ;; Width in dots of a column (NIL in fill mode).
	    COLUMN-WIDTH

            ;; GEOMETRY is the user specified geometry.  It is a list
            ;; of: Number of columns or 0 if FILL-P, number of rows,
            ;; inside width, inside height, maximum width, maximum
            ;; height.  NIL means it's free to change, as was not
            ;; explicitly specified by the user.  Default is to leave
            ;; everything free.
	    (GEOMETRY
	     (LIST NIL NIL NIL NIL NIL NIL))

            ;; The instance variable COLUMN-SPEC-LIST is a list of columns, each column
            ;; consists of (heading item-list-form .  options).  Heading is a string to
            ;; go at the top of the column, and options are menu-item options for it
            ;; (typically font). item-list-form is a form to be evaluated (without
            ;; side-effects) to get the item list for that column.  
            (COLUMN-SPEC-LIST NIL)

            ;; Used in dynamic multicolumn menus to remember what has been changed.  It
            ;; is a list of length COLUMNS which will either have elements of NIL or
            ;; will have elements each of which is a column without the heading.
            (PREVIOUS-STATE NIL)

            ;; A form to be evaluated to get the item list.  This is
            ;; used when the menu is DYNAMIC.
            (ITEM-LIST-POINTER NIL)

            ;; The height and width we used the last time we computed
            ;; the row map.
	    (LAST-INSIDE-HEIGHT NIL)
	    (LAST-INSIDE-WIDTH NIL)

	    ;; An Alist of movement commands specialized for a particular menu.  See
	    ;; MENU-DEFAULT-COMMAND-CHARACTERS for a list of the default
	    ;; movement commands.
	    (COMMAND-CHARACTERS NIL)

	    DEFAULT-FONT

            ;; Used to initialize KBD-INTERCEPTED-CHARACTERS to allow
            ;; the :CHOOSE method to read any character from the keyboard.
            MENU-INTERCEPTED-CHARACTERS

            (HIGHLIGHTED-ITEMS NIL)

            ;; These three instance variables are used to implement the
            ;; :WINDOW-OP item type
            WINDOW-UNDER-MENU
            OLD-X
            OLD-Y

            ;; Hook for color support.
            (HIGHLIGHTING-COLOR tv:*default-blinker-offset*) ;;; >>> this is the value added to a menu item to highlight
            ;;; use the same offset for higligting menu items as blinkers use

            ;; The following instance variables are used to implement searching.

            ;; Current string being searched for.
            SEARCH-STRING

	    ;; Window which displays the search string.
	    (SEARCH-WINDOW NIL)

            ;; Stack of old things which were being searched for earlier
            SEARCH-STACK

	    ;; Index into the current item of where the search started.
	    SEARCH-ITEM-INDEX

            ;; The search string used in the previous search.
            (SEARCH-OLD-SEARCH-STRING (MAKE-ARRAY 0 :ELEMENT-TYPE :CHARACTER :FILL-POINTER 0))

            ;; The current direction of the search.  This can be :FORWARD or :BACKWARD
            SEARCH-DIRECTION

            ;; What `current-item' was when the search was started.  We
            ;; use this to move back there if the user aborts out of the search.
            SEARCH-OLD-CURRENT-ITEM
            SEARCH-OLD-CURRENT-ITEM-ROW
            SEARCH-OLD-CURRENT-ITEM-COLUMN
	   )
           (HYSTERETIC-WINDOW-MIXIN
            TEMPORARY-SHADOW-BORDERS-WINDOW-MIXIN
	    STREAM-MIXIN
            GRAPHICS-MIXIN
            BORDERS-MIXIN
            MARGIN-CHOICE-MIXIN
            TOP-BOX-LABEL-MIXIN
	    SELECT-MIXIN
	    DELAY-NOTIFICATION-MIXIN
            SCROLL-BAR-MIXIN
            MINIMUM-WINDOW)
  (:GETTABLE-INSTANCE-VARIABLES
    CHOSEN-ITEM
    COLUMN-SPEC-LIST
    COMMAND-CHARACTERS
    CURRENT-ITEM
    GEOMETRY
    HIGHLIGHTING-COLOR
    HIGHLIGHTED-ITEMS
    ITEM-LIST
    ITEM-LIST-POINTER
    LAST-ITEM)
  (:SETTABLE-INSTANCE-VARIABLES
    ;; COLUMN-SPEC-LIST      ; We explicitly define a method for this operation
    ;; ITEM-LIST             ; We explicitly define a method for this operation
    ITEM-LIST-POINTER
    LAST-ITEM             
    CHOSEN-ITEM
    ;; HIGHLIGHTED-ITEMS     ; We explicitly define a method for this operation
    SORT
    )
  (:INITABLE-INSTANCE-VARIABLES
    COLUMN-SPEC-LIST
    COMMAND-CHARACTERS
    DEFAULT-FONT
    HIGHLIGHTING-COLOR
    HIGHLIGHTED-ITEMS
    ITEM-ALIGNMENT
    ITEM-LIST
    ITEM-LIST-POINTER
    last-item
    SORT)
  (:INIT-KEYWORDS
    ;; The following instance variables are used to determine the type
    ;; of menu.

    ;; Automatically clicks on the ABORT item if the menu is deexposed.  If there are
    ;; margin choices then the ABORT is searched for there first, and then the menu
    ;; items if it isn't a margin choice.
    :ABORT-ON-DEEXPOSE
    

    :COMMAND-MENU
    
    ;; Allows the menu to have an item list that's being dynamically modified.  Causes
    ;; the menu's item list to be updated at appropriate times.  The actual item list
    ;; is computed via the :UPDATE-ITEM-LIST message.
    :DYNAMIC
    
    ;; Provides for highlighting of items with inverse video.  A menu in which you can
    ;; select more than one choice.  HIGHLIGHTED-ITEMS is a list of those items in the
    ;; ITEM-LIST that are currently selected.
    :HIGHLIGHTING
    
    ;; Makes a menu have multiple 'dynamic' columns.  Each column comes from a
    ;; separate item-list which is recomputed at appropriate times.  Currently this
    ;; must be dynamic too.
    :MULTICOLUMN
    
    ;; Flag which determines whether this is a temporary or a permanent menu.
    :PERMANENT

    ;; A Pop-Up window with hysteresis.  A Pop-Up menu is not the same as a menu with
    ;; (PERMANENT NIL) since the pop-up option causes the menu to be deactivated when
    ;; the mouse moves too far out of the menu.  This option used to be called a
    ;; `momentary' menu.  It was changed to make the name more meaningful.
    :POP-UP

    :COLUMNS
    :FILL-P
    :GEOMETRY
    :MENU-MARGIN-CHOICES
    :ROWS
    ;; Allow the instantiator to turn off scrolling by specifying this
    ;; as NIL.  By default this is T.
    :SCROLLING-P)
  (:DEFAULT-INIT-PLIST :BLINKER-FLAVOR 'HOLLOW-RECTANGULAR-BLINKER :PERMANENT T
		       ;; See everything so that a user may have an idea what this part of the menu is.
		       :SCROLL-BAR-MODE :MAXIMUM
                       ;; Allow the menu to change in shape if it is reasonable.
                       :SCROLL-BAR-ON-OFF T
		       :LABEL NIL)
  (:DOCUMENTATION :MIXIN "The simplest instantiable menu. Defaults to
not having a label, a label whose position is not initially specified will
be at the top, in a small auxiliary box, unlike most windows. Regular
menu messages Provides methods and instance variables common to all
menus, such as the item-list, the geometry hacking, a default :choose
message, and a scroll bar if necessary."))


#| Primitive functions are located here.   These need to be placed  here so
that the system can be built  without error.  In other words,  placing
the definition  before  the  first  reference  makes  this  file  load
properly during a system build.  |#

(sys:declare-suggestions-for 'W:menu-item-string
			 :around
			 '(locally (declare (special sys:*suggestions-menus-on?*))
				   (cond (sys:*suggestions-menus-on?*
					   (sys:sugg-around-menu-item-string
					     (multiple-value-list :do-it)))
					 (t :do-it)))
			 :use-arglist t)

(DEFUN MENU-ITEM-STRING (ITEM &OPTIONAL ITEM-DEFAULT-FONT MENU &AUX STRING FONT color)
  "Return the string to print for item ITEM, and the font to use.
Uses ITEM-DEFAULT-FONT if the item does not specify one."
  (DECLARE (VALUES STRING FONT color)
	   (INLINE STRING))
  (sys:with-suggestions-menus-for w:menu-item-string
  (IF (ATOM ITEM)
      (SETQ STRING ITEM)
      ;;ELSE
      (PROGN
        (SETQ STRING (CAR ITEM))
        (WHEN (CONSP (CDR ITEM))
	  (SETQ FONT (GETF (CDR ITEM) :FONT)))
	(WHEN (CONSP (CDR ITEM))
	  (SETQ color (GETF (CDR ITEM) :COLOR)))))
  (IF (ICON-P STRING)
      ;; The string part is an icon.  Pass back the icon itself as the
      ;; font, and the string as the name of the icon.  The string is only
      ;; used to sort icons if the :SORT option is specified.
      (SETQ FONT   STRING
            STRING (ICON-FONT-NAME STRING))
      ;;ELSE ; HACK ALERT!!!  For common-lisp the STRING function gets an error for
      ;;instances.  This hack tests for that case explicitly.  When (if) the STRING
      ;;function is fixed, this needs to become simple again.
      (SETQ STRING (IF (TYPEP STRING 'INSTANCE)
		       ;; Some instances have a string-for-printing, but windows
		       ;; do not.  For the case of windows, get the name instead.
		       (OR (SEND STRING :SEND-IF-HANDLES :STRING-FOR-PRINTING)
			   (SEND STRING :SEND-IF-HANDLES :NAME))
		       ;;ELSE
		       (STRING STRING)))
      (COND ((NULL FONT) (SETQ FONT ITEM-DEFAULT-FONT))
            ((NULL MENU))
            ((SYMBOLP FONT)
             (SETQ FONT (SEND (SHEET-GET-SCREEN MENU) :PARSE-FONT-DESCRIPTOR FONT)))
            ((NUMBERP FONT) (SETQ FONT (AREF (SHEET-FONT-MAP MENU) FONT)))))
  (VALUES STRING FONT color)))

(DEFUN DEFAULT-MENU-ITEM-DOCUMENTER (ITEM &AUX VALUE)
  "Return the documentation for the value of a menu-item"
  (SETQ VALUE (MENU-EXECUTE-NO-SIDE-EFFECTS ITEM))
  (IF (SYMBOLP VALUE) 	 		   ;; may 9-14-88 was ATOM now SYMBOLP for '("a" . :a) cons case
      (IF (fBOUNDP value) 	   	   ;; may 9-14-88 
	   (DOCUMENTATION VALUE) 
	   (DOCUMENTATION VALUE 'variable)))) ;; may 9-14-88

;;; Here is where we invoke the icon drawing function.
(DEFUN DRAW-ICON (ICON-OBJECT WINDOW X Y &OPTIONAL ITEM ICON-COLOR)
  "Draw an icon onto a window
ICON-OBJECT	the icon itself.
WINDOW		where the icon is to draw itself.
X,Y		the pixel position of the upper left corner of there the icon is to start drawing.
ITEM		the menu item which corresponds to this icon.  This is useful for icons in menus.
ICON-COLOR	the color used to draw the icon.  This is useful only on a system which has color."
  (DECLARE (IGNORE ICON-COLOR))
  (APPLY (ICON-DRAW-FUNCTION ICON-OBJECT) ICON-OBJECT WINDOW X 
         ;; Make the baseline of the icon line up with the baseline of characters in the window.
         (+ Y (- (SHEET-BASELINE WINDOW) (ICON-FONT-BASELINE ICON-OBJECT)))
         ITEM
         (ICON-ARGUMENTS ICON-OBJECT)))

(DEFUN MAKE-SIMPLE-ICON (FUNCTION ICON-NAME WIDTH HEIGHT &REST ICON-ARGS)
  "Define a simple icon using the existing ICON defstruct.
FUNCTION is a function object which is to draw the icon
ICON-NAME is the name of the icon (used only if the menu is to be sorted).
WIDTH and HEIGHT are the pixel dimensions of the icon
ICON-ARGS are any arguments which are to be passed to the icon drawing function."
  (LET ((THE-ICON (MAKE-ICON))
        (REAL-ICON-NAME ICON-NAME))
    (COND ((STRINGP REAL-ICON-NAME)
           (SETQ REAL-ICON-NAME (INTERN REAL-ICON-NAME 'USER)))
          ((SYMBOLP REAL-ICON-NAME)
           ;; The icon name is already OK.
           NIL)
          (T
           (FERROR NIL "Icon name ~A must be either a string or a symbol" ICON-NAME)))
    (SETF (ICON-FONT-NAME        THE-ICON) REAL-ICON-NAME)
    (SETF (ICON-ARGUMENTS        THE-ICON) ICON-ARGS)
    (SETF (ICON-FONT-CHAR-WIDTH  THE-ICON) WIDTH)
    (SETF (ICON-FONT-CHAR-HEIGHT THE-ICON) HEIGHT)
    (SETF (ICON-FONT-BASELINE    THE-ICON) HEIGHT)
    (SETF (ICON-DRAW-FUNCTION    THE-ICON) FUNCTION)
    THE-ICON))

(DEFUN MENU-COMPUTE-FONT-MAP (ITEMS &AUX (MAP (CONS DEFAULT-FONT NIL)) FONT STRING-COMPONENT)
  "Compute the font map to use for a menu, from an item list.
The font map we compute has all the fonts any items need."
  (DECLARE (:SELF-FLAVOR MENU))
  (DOLIST (ITEM ITEMS)
    (SETQ FONT (if (AND (CONSP      ITEM)
                        (CONSP (CDR ITEM)))
                   (GETF  (CDR ITEM) :FONT)))
    (AND FONT
	 (NOT (MEMBER FONT MAP :TEST #'EQ))
	 (PUSH FONT MAP))
    (SETQ STRING-COMPONENT (IF (ATOM ITEM) ITEM
                               (CAR ITEM)))
    ;; When the string part of a menu item is an icon, we put this icon
    ;; into the font map too.
    (WHEN (ICON-P STRING-COMPONENT)
      (NOT (MEMBER STRING-COMPONENT MAP :TEST #'EQ))
      (PUSH STRING-COMPONENT MAP)))
  (NREVERSE MAP))

(DEFUN MENU-COMPUTE-GEOMETRY (DRAW-P &OPTIONAL INSIDE-WIDTH INSIDE-HEIGHT)
  "This function is called whenever something related to the geometry changes.
INSIDE-WIDTH and INSIDE-HEIGHT optionally specify the shape to use,
if we are recomputing the row layout but want to use a particular shape,
e.g. if the menu has been reshaped by the user.
The menu is redrawn if DRAW-P is T.  In any case, we do everything necessary
to adapt the menu to the current item-list and the optionally specified size"
  (DECLARE (:SELF-FLAVOR MENU))
  (WHEN (NULL *delay-compute-geometry*)
    (VALIDATE-GEOMETRY SELF GEOMETRY))
  (COND (*delay-compute-geometry*)
        ((VARIABLE-BOUNDP ITEM-LIST)		;Do nothing if item-list not specified yet
         ;; Get the new N-ROWS and so forth.
         (MULTIPLE-VALUE-SETQ (COLUMNS SCREEN-ROWS INSIDE-WIDTH INSIDE-HEIGHT)
                              (MENU-DEDUCE-PARAMETERS NIL NIL INSIDE-WIDTH INSIDE-HEIGHT NIL NIL))
         ;; Recompute the row map
         (MULTIPLE-VALUE-SETQ (ROW-MAP TOTAL-ROWS)
                              (MENU-COMPUTE-ROW-MAP INSIDE-WIDTH))
         (SETQ TOP-ROW    0
               ROW-HEIGHT LINE-HEIGHT)
	 (AND DRAW-P
	      (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
		(SEND SELF :NEW-SCROLL-POSITION TOP-ROW)))
         (SETQ COLUMN-WIDTH
               (AND (NOT (GEOMETRY-FILL-P GEOMETRY))
                    (TRUNCATE (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING) COLUMNS)))
         (SETQ LAST-INSIDE-WIDTH  INSIDE-WIDTH
               LAST-INSIDE-HEIGHT INSIDE-HEIGHT)
         (COND ((AND (= INSIDE-HEIGHT (SHEET-INSIDE-HEIGHT))
                     (= INSIDE-WIDTH  (SHEET-INSIDE-WIDTH)))
                (AND DRAW-P
                     (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
                       (SEND SELF :MENU-DRAW))))
               ((SEND SELF :SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT :VERIFY)
                ;; Room to do this in current place.
                (SEND SELF :SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT :TEMPORARY))
               (T
                ;; Else try to be approximately in the same place
                (LET ((CX (+ X-OFFSET (TRUNCATE WIDTH  2)))
                      (CY (+ Y-OFFSET (TRUNCATE HEIGHT 2))))
                  (WITH-SHEET-DEEXPOSED (SELF)
                    (SEND SELF :SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT :TEMPORARY)
                    (CENTER-WINDOW-AROUND SELF CX CY)))))))
  NIL)

;;; This function computes the ROW-MAP, which determines how many strings per line, & c.
;;; The first value is the row-map and the second is the n-total-rows
(DEFUN MENU-COMPUTE-ROW-MAP (&OPTIONAL (INSIDE-WIDTH (SHEET-INSIDE-WIDTH))
                             &AUX (MAP (MAKE-ARRAY (1+ (LENGTH ITEM-LIST))))
                             WID
                             (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  (DECLARE (:SELF-FLAVOR MENU))
  (DO ((ITEMS ITEM-LIST) (ROW 0 (1+ ROW)))
      ((NULL ITEMS)
       (VALUES (ADJUST-ARRAY MAP (1+ ROW)) ;Last element always contains NIL
               ROW))
    (SETF (AREF MAP ROW) ITEMS)                 ;This is where this row starts
    (IF FILL-P                                  ;Fill mode, we have some hairy calculation to do
        (DO ((SPACE INSIDE-WIDTH))
            ((NULL ITEMS))
          (SETQ WID (MENU-ITEM-STRING-WIDTH (CAR ITEMS)))
          (COND ((> WID SPACE)                  ;This one won't fit, break the line
                 (AND (> WID INSIDE-WIDTH)
                      (FERROR NIL "The item \"~A\" is too wide for this fill-mode menu"
                              (CAR ITEMS)))
                 (RETURN NIL)))
          (SETQ SPACE (- SPACE (+ WID MENU-INTERWORD-SPACING))
                ITEMS (CDR ITEMS)))
        ;;ELSE
        (SETQ ITEMS (NTHCDR COLUMNS ITEMS)))))

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
  (DECLARE (:SELF-FLAVOR MENU)
	   (VALUES N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT))

  ;; Pick up default constraints from GEOMETRY
  (SETQ N-COLUMNS     (OR N-COLUMNS     (GEOMETRY-N-COLUMNS     GEOMETRY))
	N-ROWS        (OR N-ROWS        (GEOMETRY-N-ROWS        GEOMETRY))
	INSIDE-WIDTH  (OR INSIDE-WIDTH  (GEOMETRY-INSIDE-WIDTH  GEOMETRY))
	INSIDE-HEIGHT (OR INSIDE-HEIGHT (GEOMETRY-INSIDE-HEIGHT GEOMETRY))
	MAX-WIDTH     (OR MAX-WIDTH     (GEOMETRY-MAX-WIDTH     GEOMETRY))
	MAX-HEIGHT    (OR MAX-HEIGHT    (GEOMETRY-MAX-HEIGHT    GEOMETRY)))

  ;; If any of the arguments was :UNCONSTRAINED, that means use NIL
  ;; even if the geometry is non-NIL, whereas if an argument was NIL
  ;; that means use any constraint that is in the geometry.
  (AND (EQ N-COLUMNS     :UNCONSTRAINED) (SETQ N-COLUMNS     NIL))
  (AND (EQ N-ROWS        :UNCONSTRAINED) (SETQ N-ROWS        NIL))
  (AND (EQ INSIDE-WIDTH  :UNCONSTRAINED) (SETQ INSIDE-WIDTH  NIL))
  (AND (EQ INSIDE-HEIGHT :UNCONSTRAINED) (SETQ INSIDE-HEIGHT NIL))
  (AND (EQ MAX-WIDTH     :UNCONSTRAINED) (SETQ MAX-WIDTH     NIL))
  (AND (EQ MAX-HEIGHT    :UNCONSTRAINED) (SETQ MAX-HEIGHT    NIL))
  ;; Decide whether it is fill mode or array mode
  (AND (SETQ FILL-P (AND N-COLUMNS (ZEROP N-COLUMNS)))
       (SETQ N-COLUMNS NIL))

  ;; Don't let the caller specify a height smaller than the line-height.
  (IF INSIDE-HEIGHT (SETQ INSIDE-HEIGHT (MAX INSIDE-HEIGHT LINE-HEIGHT)))

  ;; Realize any immediately clear implications
  (AND N-ROWS (NULL INSIDE-HEIGHT) (SETQ INSIDE-HEIGHT (* N-ROWS LINE-HEIGHT)))
  (AND INSIDE-HEIGHT (NULL N-ROWS) (SETQ N-ROWS (TRUNCATE INSIDE-HEIGHT LINE-HEIGHT)))
  (SETQ MAX-HEIGHT (MIN (OR INSIDE-HEIGHT MAX-HEIGHT 4096.)
			(- (SHEET-INSIDE-HEIGHT SUPERIOR) TOP-MARGIN-SIZE  BOTTOM-MARGIN-SIZE))
	MAX-WIDTH  (MIN (OR INSIDE-WIDTH MAX-WIDTH 4096.)
                        (- (SHEET-INSIDE-WIDTH  SUPERIOR) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))
  (SETQ MIN-WIDTH (MIN (SEND SELF :MINIMUM-WIDTH) MAX-WIDTH))
  (IF INSIDE-WIDTH (SETQ INSIDE-WIDTH (MAX INSIDE-WIDTH MIN-WIDTH)))

  ;; Compute the horizontal parameters.
  (COND ((AND INSIDE-WIDTH (OR N-COLUMNS FILL-P)) )		;It's fully-determined

        ;; We have the width, and it's not in fill mode, compute
	;; N-COLUMNS based on widest item, but always fill the space.
	(INSIDE-WIDTH
	 (SETQ N-COLUMNS
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
	 (SETQ INSIDE-WIDTH (MAX (ISQRT (TRUNCATE (* (MENU-FILL-WIDTH ITEM-LIST)
                                                     LINE-HEIGHT
                                                     MENU-GOLDEN-RATIO)))
                                 ;; Don't get zero, and don't get absurdly small.
				 32))))
; ; ; 	 (SETQ INSIDE-WIDTH (MAX (ISQRT (FLOOR (* (MENU-FILL-WIDTH ITEM-LIST)
; ; ; 						  LINE-HEIGHT)
; ; ; 					       MENU-GOLDEN-RATIO))
; ; ; 				 32))))  ;Don't get zero, and don't get absurdly small

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

(DEFUN MENU-DYNAMIC-ITEM-LIST (ITEM-LIST-POINTER)
  (IF (SYMBOLP ITEM-LIST-POINTER)
      (SYMBOL-VALUE ITEM-LIST-POINTER)
      ;;ELSE
      (EVAL ITEM-LIST-POINTER)))

(DEFUN MENU-FILL-WIDTH (ITEM-LIST)
  "Return an estimate of the total length required for ITEM-LIST in a filled menu.
This is what the lengths of all the lines must add up to.
We take account of spacing between items, but we can only estimate
the amount of space wasted due to line breakage."
  (IF ITEM-LIST
      (- (LOOP FOR ITEM IN ITEM-LIST
               SUMMING (+ (MENU-ITEM-STRING-WIDTH ITEM) MENU-FILL-BREAKAGE))
         MENU-FILL-BREAKAGE)
      ;;ELSE
      0))

(DEFUN MENU-ITEM-STRING-OUT (STRING)
  "Primitive used to output a menu item.
Returns the final X position for the output."
  (DECLARE (:SELF-FLAVOR MENU))
  (IF (ZEROP (LENGTH STRING))
      ;; Handle empty :NO-SELECT cases too.  :String-out-explicit
      ;; returns NIL (instead of 0) for these strings.
      0
      ;;ELSE
      (- (SEND SELF :STRING-OUT-EXPLICIT STRING
               CURSOR-X (+ BASELINE-ADJ CURSOR-Y) (SHEET-INSIDE-RIGHT) (SHEET-INSIDE-BOTTOM)
               CURRENT-FONT CHAR-ALUF)
         (SHEET-INSIDE-LEFT SELF))))

(DEFUN MENU-ITEM-STRING-WIDTH (ITEM &OPTIONAL STOP-X)
  "Return the width in pixels of the way item ITEM is displayed on the menu.
If STOP-X is specified, we will not return a value larger than that."
  (DECLARE (:SELF-FLAVOR MENU)
	   (INLINE FONT-EVALUATE))
  (MULTIPLE-VALUE-BIND (STRING FONT)
      (MENU-ITEM-STRING ITEM (FONT-EVALUATE CURRENT-FONT) SELF)
    (IF (ICON-P FONT)
        ;; For icons, the width is in the icon defstruct.
        (ICON-FONT-CHAR-WIDTH FONT)
        ;;ELSE
        (SHEET-STRING-LENGTH SELF STRING 0 NIL STOP-X FONT))))

;; (name state function x1 x2 :documentation documentation-list <item-for-MENU-or-eval-form-for-CVV>) 	;; may 9-14-88
(DEFUN MENU-SIMPLE-MARGIN-CHOICE-FROM-ITEM (X &aux doc)
  (LIST (MENU-ITEM-STRING X NIL) NIL 'MENU-MARGIN-CHOICE-FUNCTION NIL NIL
	;; may 9-14-88. added 6th and 7th list items bumping X to 8th
	;; X can be keyword, list or (ugh!) cons - per para 14.2.6.3 & 14.2.1
	(IF (SETQ doc (AND (CONSP x)      ;; not :keyword
			   (CONSP (CDR x));; not (cons 'a 'b)
			   (get x :documentation)))
	    :documentation nil)
	doc			
	x))

(DEFUN MENU-UPDATE-MOUSE-CURSOR (CURRENT-ROW)
  "Update the menu and mouse after the user has positioned to another item."
  (DECLARE (:SELF-FLAVOR MENU))
  ;; Make sure that this item is visible.
  (WHEN (OR (< CURRENT-ITEM-ROW TOP-ROW)
            (> CURRENT-ITEM-ROW (+ TOP-ROW (1- SCREEN-ROWS))))
    (SEND SELF :SCROLL-TO CURRENT-ITEM-ROW :ABSOLUTE))
  (SETQ CURRENT-ITEM (NTH CURRENT-ITEM-COLUMN CURRENT-ROW))
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (SEND SELF :ITEM-RECTANGLE CURRENT-ITEM)
    ;; Move the mouse blinker to the middle of the item.
    (SEND SELF :SET-MOUSE-CURSORPOS (+ LEFT (TRUNCATE (- RIGHT LEFT) 2)) (+ TOP (TRUNCATE (- BOTTOM TOP) 2)))
    ;; Move the hollow rectangular blinker over the item.
    (SEND (CAR BLINKER-LIST) :SET-SIZE-AND-CURSORPOS (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP)))

(DEFUN DOIT-MARGIN-CHOICE ()
  "Margin choice for the Do It box.  Specify :DOIT as a margin choice to get this."
  (MENU-SIMPLE-MARGIN-CHOICE-FROM-ITEM
    `(,MARGIN-CHOICE-COMPLETION-STRING :EVAL (VALUES (SEND SELF :HIGHLIGHTED-VALUES) T))))

(DEFUN ABORT-MARGIN-CHOICE ()
  "Margin choice for the Abort box.  Specify :ABORT as a margin choice to get this."
  (MENU-SIMPLE-MARGIN-CHOICE-FROM-ITEM
    `(,MARGIN-CHOICE-ABORT-STRING :EVAL (SIGNAL-CONDITION EH:ABORT-OBJECT))))

(DEFUN ABORT-OR-DOIT-MARGIN-CHOICES ()
  "Margin choices for both the Abort and Do It boxes.  
Specify :ABORT-OR-DOIT as a margin choice to get this."
  (LIST (ABORT-MARGIN-CHOICE) (DOIT-MARGIN-CHOICE)))


(DEFUN MENU-MARGIN-CHOICE-FROM-ITEM (X)
  (COND ((EQ X :DOIT)      (DOIT-MARGIN-CHOICE))
	((EQ X :ABORT)     (ABORT-MARGIN-CHOICE))
	((eq x :ABORT-OR-DOIT) (values (abort-margin-choice) (doit-margin-choice)))
	((eq x :DOIT-OR-ABORT) (values (abort-margin-choice) (doit-margin-choice)))
        (T (MENU-SIMPLE-MARGIN-CHOICE-FROM-ITEM X))))

(DEFUN MENU-MARGIN-CHOICE-FUNCTION (CHOICE-BOX REGION Y-POS)
  (DECLARE (:SELF-FLAVOR MENU))
  REGION Y-POS ;ignored
  (SETQ CHOSEN-ITEM (eighTH CHOICE-BOX)) ;; may 9-14-88 was sixth
  (SETQ LAST-ITEM   CHOSEN-ITEM))

(DEFUN MENU-MAX-WIDTH (ITEM-LIST)
  "Return the maximum width in pixels of any item in ITEM-LIST.
Normally you should add allowances for interword spacing to this."
  (LOOP FOR L IN ITEM-LIST
        MAXIMIZE (MENU-ITEM-STRING-WIDTH L)))

(DEFUN MENU-ITEM-SORTER (ITEM-LIST PREDICATE-KEY)
  "Internal function used to sort an item list."
  (LET ((KEY NIL)
        PREDICATE)
    ;; Isolate the key from the predicate.
    (IF (CONSP PREDICATE-KEY)
        (SETQ PREDICATE (CAR  PREDICATE-KEY)
              KEY       (CADR PREDICATE-KEY))
        ;;ELSE
        (SETQ PREDICATE PREDICATE-KEY))
    ;; Take care of the case where the user forgets to backquote and evaluate the KEY
    ;; function object.  I did this a lot until I understood exactly what I was doing.  I
    ;; suspect that there will be users who will do the same thing.
    (WHEN (AND (CONSP KEY) (EQ (CAR KEY) 'FUNCTION))
      (SETQ KEY (EVAL KEY)))
    (WHEN (AND (CONSP PREDICATE) (EQ (CAR PREDICATE) 'FUNCTION))
      (SETQ PREDICATE (EVAL PREDICATE)))
    (FLET ((ITEM-KEY (ITEM) (IF KEY
                                (FUNCALL KEY (MENU-ITEM-STRING ITEM))
                                ;;ELSE
                                (MENU-ITEM-STRING ITEM))))
      ;; Allow the user to specify T instead of :ASCENDING.  This makes
      ;; this more consistent with some of the other menu window options.
      (COND ((OR (EQ PREDICATE  :ASCENDING)
		 (EQ PREDICATE T))          (SETQ PREDICATE #'STRING-LESSP))
            ((EQ     PREDICATE :DESCENDING) (SETQ PREDICATE #'(LAMBDA (A B) (NOT (STRING-LESSP A B))))))
      (STABLE-SORT ITEM-LIST PREDICATE :KEY #'ITEM-KEY))))

(PROCLAIM '(INLINE ITEM-CONTAINS-NO-SELECT-P))
(DEFUN ITEM-CONTAINS-NO-SELECT-P (ITEM)
  "Return NON-NIL if ITEM contains a :NO-SELECT keyword."
  (AND (CONSP ITEM)
       (> (LENGTH ITEM) 1)
       (MEMBER :NO-SELECT ITEM)))
  
(DEFUN MENU-EXECUTE-NO-SIDE-EFFECTS (ITEM &AUX OP ARG)
  "Try to get the value a menu would return if ITEM were chosen, but avoid side effects.
If getting the value might require a side-effect, just return NIL."
  (COND ((ATOM       ITEM )       ITEM)
	((ATOM (CDR  ITEM)) (CDR  ITEM))
	((ATOM (CDDR ITEM)) (CADR ITEM))
	((EQ (SETQ ARG (CADDR ITEM)
		   OP  (CADR  ITEM))
	     :VALUE)
	 ARG)
	(T NIL)))

(DEFUN MENU-ITEM-WHO-LINE-DOCUMENTATION (ITEM &AUX DOCUMENTATION-STRING)
  "Return the who-line string for menu item ITEM."
  (LET ((DOC (AND (CONSP ITEM)
		  (CONSP (CDR ITEM))
		  (GETF  (CDR ITEM) :DOCUMENTATION))))
    (COND ((STRINGP DOC) DOC)
          ;; Handle a mouse documentation list too.
          ((CONSP DOC) DOC)
          ;; Handle the case of a :DOCUMENTATION string in a
          ;; CHOOSE-VARIABLE-VALUES :MENU-ALIST item.
          ((AND (NULL DOC)
                (CONSP ITEM)                    ; The entire menu item
                (CONSP (CAR ITEM))              ; The thing moused + its documentation
                (SETQ DOCUMENTATION-STRING (MEMBER :DOCUMENTATION (CAR ITEM) :TEST #'EQ)))
           (CADR DOCUMENTATION-STRING))
          ((AND (NULL DOC) *DEFAULT-MENU-ITEM-WHO-LINE-DOCUMENTATION-FUNCTION*)
           (FUNCALL *DEFAULT-MENU-ITEM-WHO-LINE-DOCUMENTATION-FUNCTION* ITEM))
          ((CONSP DOC)
           (EVAL DOC))
          ((AND (SYMBOLP DOC) (FBOUNDP DOC)) (FUNCALL DOC ITEM)))))

;;; The following function computes an `outcode' value using the algorithm given on
;;; page 146 of the book "Fundamentals of Interactive Computer Graphics" by Foley
;;; and Van Dam.
(DEFUN COMPUTE-OUTCODE (X Y LEFT TOP RIGHT BOTTOM)
  "Compute the outcode value for a point given a rectangle."
  (LET ((OUTCODE 0))
    (WHEN (< X LEFT)
      (SETQ OUTCODE (DPB 1 #o0001 OUTCODE)))
    (WHEN (> X RIGHT)
      (SETQ OUTCODE (DPB 1 #o0101 OUTCODE)))
    (WHEN (> Y BOTTOM)
      (SETQ OUTCODE (DPB 1 #o0201 OUTCODE)))
    (WHEN (< Y TOP)
      (SETQ OUTCODE (DPB 1 #o0301 OUTCODE)))
    OUTCODE))

(DEFUN DISTANCE-FROM-BOX (X Y LEFT TOP RIGHT BOTTOM)
  (CASE (COMPUTE-OUTCODE X Y LEFT TOP RIGHT BOTTOM)
    (1  (- LEFT X))
    (2  (- X    RIGHT))
    (8  (- TOP  Y))
    (4  (- Y    BOTTOM))
    (9  (ISQRT (+ (EXPT (- X LEFT ) 2) (EXPT (- Y TOP   ) 2))))
    (10 (ISQRT (+ (EXPT (- X RIGHT) 2) (EXPT (- Y TOP   ) 2))))
    (5  (ISQRT (+ (EXPT (- X LEFT ) 2) (EXPT (- Y BOTTOM) 2))))
    (6  (ISQRT (+ (EXPT (- X RIGHT) 2) (EXPT (- Y BOTTOM) 2))))
    (0 0)))

(DEFUN MOVE-TO-NEAREST-MENU-ITEM ()
  "Move the mouse to a nearby item."
  (DECLARE (:SELF-FLAVOR MENU))
  ;; If we are already on an item then that is the nearest. (Stupid, eh?)
  (WHEN (NULL CURRENT-ITEM)
    (LET ((X MOUSE-X)				;Save the mouse (x,y) in case the user is moving the mouse fast.
	  (Y MOUSE-Y))
      ;; Adjust the (X Y) coordinates to be relative to the inside coordinate system for SELF.
      (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
	  (SHEET-CALCULATE-OFFSETS SELF NIL)
	(SETQ X (- X X-OFF (SHEET-INSIDE-LEFT SELF))
	      Y (- Y Y-OFF (SHEET-INSIDE-TOP  SELF))))
      ;; Loop through all items to find the nearest one.  This could be sped up
      ;; considerably by becoming more like :ITEM-RECTANGLE.  Currently it is
      ;; an order (EXPT N 2) algorithm in speed.  By folding :ITEM-RECTANGLE
      ;; into the loop, this could become an order N algorithm in speed.
      (LOOP WITH MIN-DISTANCE = MOST-POSITIVE-FIXNUM
	    WITH NEAREST-ITEM        = NIL
	    WITH NEAREST-ITEM-ROW    = NIL
	    WITH NEAREST-ITEM-COLUMN = NIL
	    FOR CURRENT-ROW-NUMBER FROM 0 BY 1
	    FOR CURRENT-ROW = (AREF ROW-MAP CURRENT-ROW-NUMBER) THEN NEXT-ROW
	    WHILE CURRENT-ROW
	    FOR NEXT-ROW = (AREF ROW-MAP (1+ CURRENT-ROW-NUMBER))
	    FOR CURRENT-ROW-LENGTH = (- (LENGTH CURRENT-ROW) (LENGTH NEXT-ROW))
	    DO (LOOP FOR CURRENT-COLUMN-NUMBER FROM 0 BELOW CURRENT-ROW-LENGTH
		     FOR THIS-ITEM = (NTH CURRENT-COLUMN-NUMBER CURRENT-ROW)
		     WITH DISTANCE  = NIL
		     DO (WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P THIS-ITEM))
			  (MULTIPLE-VALUE-BIND (LL TT RR BB)
			      (SEND SELF :ITEM-RECTANGLE THIS-ITEM)
			    ;; if the item is not displayed :ITEM rectangle returns NIL
			    ;; so calculating distance is a BAD idea PMH 11/25/87
			    (when (and LL TT RR BB)
			      (SETQ DISTANCE (DISTANCE-FROM-BOX X Y LL TT RR BB))
			      (WHEN (< DISTANCE MIN-DISTANCE)
				(SETQ MIN-DISTANCE        DISTANCE
				      NEAREST-ITEM        THIS-ITEM
				      NEAREST-ITEM-ROW    CURRENT-ROW-NUMBER
				      NEAREST-ITEM-COLUMN CURRENT-COLUMN-NUMBER))))))
	    FINALLY (SETQ CURRENT-ITEM        NEAREST-ITEM
			  CURRENT-ITEM-ROW    NEAREST-ITEM-ROW
			  CURRENT-ITEM-COLUMN NEAREST-ITEM-COLUMN))
      (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))))


(DEFUN PARSE-MARGIN-CHOICES (CHOICES &aux temp)
  "Parse margin choices.  Allow keywords too."
  (IF (OR (EQ CHOICES :ABORT-OR-DOIT) (EQ CHOICES :DOIT-OR-ABORT))
      ;; Allow the user to specify a complete margin choice by a keyword.
      (ABORT-OR-DOIT-MARGIN-CHOICES)
      ;;ELSE
      ;;;; build a list of the expaneded margin choices.  
      ;;;; This allows the use of :abort-or-doit in a list as it returns 2 values
      (dolist (item CHOICES temp)
	(dolist (other-item (multiple-value-list (MENU-MARGIN-CHOICE-FROM-ITEM item)))
	    (push-end other-item temp)))))



(DEFUN VALIDATE-GEOMETRY (WINDOW GEOMETRY)
  (LET ((N-COLUMNS     (GEOMETRY-N-COLUMNS     GEOMETRY))
        (N-ROWS        (GEOMETRY-N-ROWS        GEOMETRY))
        (INSIDE-HEIGHT (GEOMETRY-INSIDE-HEIGHT GEOMETRY))
        (INSIDE-WIDTH  (GEOMETRY-INSIDE-WIDTH  GEOMETRY))
        (MAX-HEIGHT    (GEOMETRY-MAX-HEIGHT    GEOMETRY))
        (MAX-WIDTH     (GEOMETRY-MAX-WIDTH     GEOMETRY)))
    (WHEN (NOT (OR (NULL     N-COLUMNS)
                   (AND (INTEGERP N-COLUMNS)
                        (<= 0     N-COLUMNS))
                   (EQ N-COLUMNS :UNCONSTRAINED)))
      (FERROR NIL "Number of columns, ~A, specified in menu geometry is invalid." N-COLUMNS))
    (WHEN (NOT (OR (NULL     N-ROWS)
                   (AND (INTEGERP N-ROWS)
                        (PLUSP    N-ROWS))
                   (EQ N-ROWS :UNCONSTRAINED)))
      (FERROR NIL "Number of rows, ~A, specified in menu geometry is invalid." N-ROWS))
    (WHEN (AND (INTEGERP N-ROWS) (NOT (PLUSP N-ROWS)))
      (FERROR NIL "Number of rows, ~A, specified in menu geometry is too small, must be at least ~D." N-ROWS 1))
    (WHEN (NOT (OR (NULL     INSIDE-WIDTH)
                   (AND (INTEGERP INSIDE-WIDTH)
                        (PLUSP    INSIDE-WIDTH))
                   (EQ INSIDE-WIDTH :UNCONSTRAINED)))
      (FERROR NIL "Inside width, ~A, specified in menu geometry is invalid." INSIDE-WIDTH))
    (WHEN (AND (NUMBERP INSIDE-WIDTH) (NOT (>= INSIDE-WIDTH (FONT-CHAR-WIDTH (SHEET-CURRENT-FONT WINDOW)))))
      (FERROR NIL "Inside width, ~A, specified in menu geometry is too small, must be at least ~D."
              INSIDE-WIDTH (FONT-CHAR-WIDTH (SHEET-CURRENT-FONT WINDOW))))
    (WHEN (NOT (OR (NULL     INSIDE-HEIGHT)
                   (AND (INTEGERP INSIDE-HEIGHT)
                        (PLUSP    INSIDE-HEIGHT))
                   (EQ INSIDE-HEIGHT :UNCONSTRAINED)))
      (FERROR NIL "Inside height, ~A, specified in menu geometry is invalid." INSIDE-HEIGHT))
    (WHEN (AND (NUMBERP INSIDE-HEIGHT) (NOT (>= INSIDE-HEIGHT (SHEET-LINE-HEIGHT WINDOW))))
      (FERROR NIL "Inside height, ~A, specified in menu geometry is too small, must be at least ~D."
              INSIDE-HEIGHT (SHEET-LINE-HEIGHT WINDOW)))
    (WHEN (NOT (OR (NULL     MAX-WIDTH)
                   (AND (INTEGERP MAX-WIDTH)
                        (PLUSP    MAX-WIDTH))
                   (EQ MAX-WIDTH :UNCONSTRAINED)))
      (FERROR NIL "Maximum width, ~A, specified in menu geometry is invalid." MAX-WIDTH))
    (WHEN (AND (NUMBERP MAX-WIDTH) (NOT (>= MAX-WIDTH (FONT-CHAR-WIDTH (SHEET-CURRENT-FONT WINDOW)))))
      (FERROR NIL "Maximum width, ~A, specified in menu geometry is too small, must be at least ~D."
              MAX-WIDTH (FONT-CHAR-WIDTH (SHEET-CURRENT-FONT WINDOW))))
    (WHEN (AND (NUMBERP MAX-WIDTH)
               (NUMBERP INSIDE-WIDTH)
               (< MAX-WIDTH INSIDE-WIDTH))
      (FERROR NIL "Maximum width of ~D is inconsistent with an inside width of ~D." MAX-WIDTH INSIDE-WIDTH))
    (WHEN (NOT (OR (NULL     MAX-HEIGHT)
                   (AND (INTEGERP MAX-HEIGHT)
                        (PLUSP    MAX-HEIGHT))
                   (EQ MAX-HEIGHT :UNCONSTRAINED)))
      (FERROR NIL "MAX height, ~A, specified in menu geometry is invalid." MAX-HEIGHT))
    (WHEN (AND (NUMBERP MAX-HEIGHT) (NOT (>= MAX-HEIGHT (SHEET-LINE-HEIGHT WINDOW))))
      (FERROR NIL "Maximum height, ~A, specified in menu geometry is too small, must be at least ~D."
              MAX-HEIGHT (SHEET-LINE-HEIGHT WINDOW)))
    (WHEN (AND (NUMBERP MAX-HEIGHT)
               (NUMBERP INSIDE-HEIGHT)
               (< MAX-HEIGHT INSIDE-HEIGHT))
      (FERROR NIL "Maximum height of ~D is inconsistent with an inside height of ~D." MAX-HEIGHT INSIDE-HEIGHT))))

(DEFMETHOD (MENU :BEFORE :INIT) (INIT-PLIST &AUX (SUP SUPERIOR) TEM )
  (SETQ SUP (OR SUP (GET INIT-PLIST :SUPERIOR) DEFAULT-SCREEN))
  (SETQ DEFAULT-FONT (SEND (SHEET-GET-SCREEN SUP) :PARSE-FONT-SPECIFIER 
                           (IF (VARIABLE-BOUNDP DEFAULT-FONT)
                               DEFAULT-FONT
                               ;; ELSE We do not have a font to use.  Use the default one.
                               *MENU-ITEM-STANDARD-FONT*)))
  (OR (VARIABLE-BOUNDP FONT-MAP)
      (SETQ FONT-MAP (MENU-COMPUTE-FONT-MAP (GET INIT-PLIST :ITEM-LIST))))

  (SETQ PREVIOUS-STATE (MAKE-LIST (LENGTH COLUMN-SPEC-LIST)))

  ;; Set the flags here. if user specified value and that value T, then setf to 1.
  ;; otherwise user did not specify value or value specified was NIL. and flag setf'ed to 0.
  (SETF (SHEET-MENU-ABORT-ON-DEEXPOSE) (IF (CADR (MEMBER :ABORT-ON-DEEXPOSE (CAR INIT-PLIST))) 1 0))
  (SETF (SHEET-MENU-COMMAND-MENU)      (IF (CADR (MEMBER :COMMAND-MENU      (CAR INIT-PLIST))) 1 0))
  (SETF (SHEET-MENU-DYNAMIC)           (IF (CADR (MEMBER :DYNAMIC           (CAR INIT-PLIST))) 1 0))
  (SETF (SHEET-MENU-HIGHLIGHTING)      (IF (CADR (MEMBER :HIGHLIGHTING      (CAR INIT-PLIST))) 1 0))
  (SETF (SHEET-MENU-MULTICOLUMN)       (IF (CADR (MEMBER :MULTICOLUMN       (CAR INIT-PLIST))) 1 0))
  (SETF (SHEET-MENU-POP-UP)            (IF (CADR (MEMBER :POP-UP            (CAR INIT-PLIST))) 1 0))
  (SETF (SHEET-MENU-PERMANENT)         (IF (CADR (MEMBER :PERMANENT         (CAR INIT-PLIST))) 1 0))
						
  ;; Perform some consistency checking.  Note that we can have three cases of perrmanent and pop-up:
  ;; permanent t + pop-up nil, permanent nil + pop-up t, and permanent nil pop-up nil.  The first two
  ;; cases are normal for permanent and pop-up menus respectively.  The last one is somewhat strange
  ;; in that the menu is temporary but is not pop-up.  This type of menu fill be fast in exposing, like
  ;; pop-up menus are, but must be deexposed explicitly by program control.
  (WHEN (PLUSP (SHEET-MENU-POP-UP))
        ;; Make sure that POP-UP and PERMANENT are opposites.
    (SETF (SHEET-MENU-PERMANENT) 0))

  ;; Perform some consistency checking.  Hopefully, someday this won't be necessary.
  ;; Right now it is build into the :UPDATE-ITEM method.
  (WHEN (PLUSP (SHEET-MENU-MULTICOLUMN))
    (SETF (SHEET-MENU-DYNAMIC) 1))

  (WHEN (AND (MEMBER :SCROLLING-P (CAR INIT-PLIST))
	     (NULL (GET INIT-PLIST :SCROLLING-P)))
    ;; Turn scrolling off when the instantiator specifies SCROLLING-P and its value is NIL.
    (SETQ SCROLL-BAR-MODE NIL))

  ;; Initialize the intercepted characters to allow :CHOOSE to read any character.
  (SETQ MENU-INTERCEPTED-CHARACTERS (REMOVE-IF #'(LAMBDA (ELT)
                                                (ZEROP (CHAR-BITS (CAR ELT))))
                                            (THE LIST KBD-INTERCEPTED-CHARACTERS)))

  (SETQ MARGIN-CHOICES (PARSE-MARGIN-CHOICES (GET INIT-PLIST :MENU-MARGIN-CHOICES)))
  (SETQ BORDERS (GET INIT-PLIST :BORDERS))
  ;; This is used to make the menu have the shadow borders around it.
  ;; If this is permanent window then get rid of the shadow borders.
  (IF (PLUSP (SHEET-MENU-PERMANENT))
      (PROGN
         ;; We must set the hysteresis for permanent menus to zero so that mouse tracking for a menu
         ;; is done only up to its border.  If this were not done, then mouse tracking would extend
         ;; beyond its border, causing confusion for the user.
         (SETQ HYSTERESIS 0)
        ;; If the user didn't specify any borders then we need to set up a default.
        (UNLESS BORDERS
          (SETQ BORDERS (GET 'DRAW-RECTANGULAR-BORDER 'DEFAULT-BORDER-SIZE)))
        (SETQ RIGHT-SHADOW-WIDTH  0
              BOTTOM-SHADOW-WIDTH 0))
      ;;ELSE
      (UNLESS BORDERS
        (SETQ BORDERS 3)))

  (WHEN ITEM-LIST-POINTER
    (SETQ ITEM-LIST (MENU-DYNAMIC-ITEM-LIST ITEM-LIST-POINTER)))

  ;; If the instantiator specified some items to be highlighted then
  ;; this must be a highlighting menu.
  (WHEN (GET INIT-PLIST :HIGHLIGHTED-ITEMS)
    (SETF (SHEET-MENU-HIGHLIGHTING) 1))

  (SETF (GET INIT-PLIST :MORE-P) NIL)
  (SETQ TEM (GET INIT-PLIST :GEOMETRY))
  (IF (> (LENGTH TEM) (LENGTH GEOMETRY))
      ;; Longer than we need, take a copy of the list
      (SETQ GEOMETRY (COPY-LIST TEM))
      ;; ELSE copy the appropriate piece of user specified list into our list
      (DO ((TEM TEM (CDR TEM))
           (GEO GEOMETRY (CDR GEO)))
          ((NULL TEM))
        (SETF (CAR GEO) (CAR TEM))))
  ;;;added the following line to prevent people from creating menus that they cannot move the mouse off of -- GSM 27 Nov 85
  (AND (NULL (GEOMETRY-MAX-WIDTH GEOMETRY)) (SETF (GEOMETRY-MAX-WIDTH GEOMETRY) (- (SEND SUP :WIDTH) 61.)))
  (AND           (GET INIT-PLIST :FILL-P )  (SETF (GEOMETRY-N-COLUMNS GEOMETRY) 0))
  (AND (SETQ TEM (GET INIT-PLIST :ROWS   )) (SETF (GEOMETRY-N-ROWS    GEOMETRY) TEM))
  (AND (SETQ TEM (GET INIT-PLIST :COLUMNS)) (SETF (GEOMETRY-N-COLUMNS GEOMETRY) TEM))
  ;; We'll handle SAVE-BITS ourselves later.
  ;; This is so the bit array doesn't get created until we know the size.
  (SETF (GET INIT-PLIST :MENU-SAVE-BITS) (GET INIT-PLIST :SAVE-BITS))
  (SETF (GET INIT-PLIST :SAVE-BITS) NIL))

(DEFMETHOD (MENU :AFTER :INIT) (INIT-PLIST)
   (WHEN (AND ITEM-LIST SORT)
      (SETQ ITEM-LIST (MENU-ITEM-SORTER ITEM-LIST SORT)))
   (SETF (BLINKER-VISIBILITY (CAR BLINKER-LIST)) NIL)
   (IF ITEM-LIST
        (MENU-COMPUTE-GEOMETRY NIL)
        ;;ELSE
        (MENU-COMPUTE-GEOMETRY NIL (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)))
   ;; If a not a permanent menu, give me a bit-array.  We need to do this here because the temporary-window-mixin
   ;; has an :after :init which sets it to T, which may not be correct for menus.
   (SETQ TEMPORARY-BIT-ARRAY  (ZEROP (SHEET-MENU-PERMANENT)))
   (SEND SELF :SET-SAVE-BITS (GET INIT-PLIST :MENU-SAVE-BITS) :dont-refresh)	;PMH added :dont-refresh
   )

(DEFMETHOD (MENU :ADD-HIGHLIGHTED-ITEM) (ITEM)
  (COND ((NOT (MEMBER ITEM HIGHLIGHTED-ITEMS :TEST #'EQ))
         (PUSH ITEM HIGHLIGHTED-ITEMS)
         (SHEET-FORCE-ACCESS (SELF T) (SEND SELF :HIGHLIGHT-ITEM ITEM)))))

(DEFMETHOD (MENU :ADD-HIGHLIGHTED-VALUE) (VALUE)
  (DO ((L ITEM-LIST (CDR L)))
      ((NULL L) (FERROR NIL "Value not found"))
    (AND (EQUAL  (SEND SELF :EXECUTE-NO-SIDE-EFFECTS (CAR L)) VALUE)
	 (RETURN (SEND SELF :ADD-HIGHLIGHTED-ITEM    (CAR L))))))

(DEFMETHOD (MENU :ADD-ITEM) (NEW-ITEM)
  "This is like setting the item list, but we do not unhighlight any of
the existing items in the menu."
  (UNLESS (MEMBER NEW-ITEM ITEM-LIST :TEST #'EQUAL)
    (SETQ ITEM-LIST (NCONC ITEM-LIST (LIST NEW-ITEM)))
    (SETQ LAST-ITEM           NIL
          CURRENT-ITEM        NIL
          CURRENT-ITEM-ROW    NIL
          CURRENT-ITEM-COLUMN NIL)
    (SEND SELF :SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST)))
  NEW-ITEM)

(DEFMETHOD (MENU :ADJUSTABLE-SIZE-P) ()
  ;; Return T if this is a pop-up window.
  (NOT (NULL TEMPORARY-BIT-ARRAY)))

;; When we move a menu to a spot, make it go so that the last item chosen
;; appears at that spot.
(DEFMETHOD (MENU :CENTER-AROUND) (X Y &AUX (XI 0) (YI 0))
  (WHEN (PLUSP (SHEET-MENU-DYNAMIC))
    (SEND SELF :UPDATE-ITEM-LIST))
  (AND (VARIABLE-BOUNDP LAST-ITEM)
       (MEMBER LAST-ITEM ITEM-LIST :TEST #'EQ)
       ;; If we remember a previous choice,
       ;; let XI and YI get the offsets from that item to the center.
       (MULTIPLE-VALUE-BIND (X1 Y1) (SEND SELF :ITEM-CURSORPOS LAST-ITEM)
         (AND X1 Y1
              (SETQ XI (- (TRUNCATE WIDTH  2) X1 (SHEET-INSIDE-LEFT))
                    YI (- (TRUNCATE HEIGHT 2) Y1 (SHEET-INSIDE-TOP))))))
  (MULTIPLE-VALUE-BIND (X1 Y1) (CENTER-WINDOW-AROUND SELF (+ X XI) (+ Y YI))
    (VALUES (- X1 XI) (- Y1 YI))))

(DEFMETHOD (MENU :AFTER :CHANGE-OF-DEFAULT-FONT) (IGNORE IGNORE)
  (MENU-COMPUTE-GEOMETRY T (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)))

;When we change our inside size, we must recompute the geometry with the new inside size,
;unless it is the same as we last computed the geometry for.
;If we get here from recomputing the geometry, that will be true, and avoid a loop.
(DEFMETHOD (MENU :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (OR (AND (EQUAL (SHEET-INSIDE-WIDTH) LAST-INSIDE-WIDTH)
	   (EQUAL (SHEET-INSIDE-HEIGHT) LAST-INSIDE-HEIGHT))
      (MENU-COMPUTE-GEOMETRY NIL (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT))))

(DEFUN MENU-CHOOSE-INTERNAL-WAIT-FUNCTION (WIN ITEM-LOC STATUS-LOC)
  (OR (LISTEN WIN)
      (CAR ITEM-LOC)
      (NULL (CAR STATUS-LOC))))

(DEFMETHOD (MENU :BEFORE :EXPOSE)(&REST IGNORE)
  ;; this used to be in :choose but sometimes people expose the menu
  ;; themselves before calling choose.  SPR#3559  PMH
  (SETQ WINDOW-UNDER-MENU (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y)
	OLD-X MOUSE-X
	OLD-Y MOUSE-Y))

(DEFMETHOD (MENU :CHOOSE) NIL
  (LET (RETURN-RESULT)
    (WHEN (PLUSP (SHEET-MENU-DYNAMIC))
      (SEND SELF :UPDATE-ITEM-LIST))
    (PROG ((X MOUSE-X)
	   (Y MOUSE-Y)
	   (SUCCESS T))
      (UNWIND-PROTECT (PROGN
		       (CATCH 'ABORT
			 (RETURN
			  (PROGN
			    (COND
			      ((AND (PLUSP (SHEET-MENU-POP-UP)) (NOT EXPOSED-P))
			       (MULTIPLE-VALUE-BIND (X-OFF Y-OFF) (SHEET-CALCULATE-OFFSETS SUPERIOR MOUSE-SHEET)
				 (MULTIPLE-VALUE-BIND (X Y) (SEND SELF :CENTER-AROUND (- MOUSE-X X-OFF) (- MOUSE-Y Y-OFF))
				   (MOUSE-WARP (+ X X-OFF) (+ Y Y-OFF))))
			       (WITH-MOUSE-GRABBED (SEND SELF :SELECT)
				  (COND
				    ((NEQ SELF
					  (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y
								    NIL :EXPOSED))
				     (SEND SELF :DEACTIVATE) (THROW 'ABORT
								    ()))))))
			    (SETQ CHOSEN-ITEM NIL)
			    (OR (EQ (SEND SELF :STATUS) :SELECTED) (SEND SELF :SELECT))
			    (LOOP DO
			       (PROCESS-WAIT "Menu choose" #'MENU-CHOOSE-INTERNAL-WAIT-FUNCTION
					     SELF (LOCF CHOSEN-ITEM) (LOCF EXPOSED-P))
			       UNTIL (OR CHOSEN-ITEM (NOT EXPOSED-P)) WHEN (LISTEN SELF) DO
			       (LET ((KBD-INTERCEPTED-CHARACTERS MENU-INTERCEPTED-CHARACTERS))
				 (SEND SELF :PROCESS-CHARACTER (READ-ANY SELF))))
			    (UNWIND-PROTECT (SETQ
					     RETURN-RESULT
					     (SEND SELF :EXECUTE CHOSEN-ITEM))
			      (SETQ CHOSEN-ITEM NIL)))))
		       (SETQ SUCCESS NIL))
	(WHEN SUCCESS
	  (MOUSE-WARP X Y))))
    RETURN-RESULT))


(DEFMETHOD (MENU :COLUMN-ROW-SIZE) ()
  (VALUES COLUMN-WIDTH ROW-HEIGHT))

(DEFMETHOD (MENU :COMMAND-MENU) ()
  (PLUSP (SHEET-MENU-COMMAND-MENU)))

(DEFMETHOD (MENU :CURRENT-GEOMETRY) ()
  "Like :GEOMETRY but returns the current state rather than the default"
  (LIST (IF (GEOMETRY-FILL-P GEOMETRY) 0 COLUMNS) TOTAL-ROWS
	(SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
	(GEOMETRY-MAX-WIDTH GEOMETRY) (GEOMETRY-MAX-HEIGHT GEOMETRY)))

(DEFMETHOD (MENU :BEFORE :DEEXPOSE) (&REST IGNORE &AUX CANCEL-MARGIN-CHOICE)
  "Handle the DEEXPOSE part of ABORT-ON-DEEXPOSE."
  ;; If there are margin choices, search for the ABORT there.
  (WHEN (PLUSP (SHEET-MENU-ABORT-ON-DEEXPOSE))
    (IF (AND MARGIN-CHOICES
             (SETQ CANCEL-MARGIN-CHOICE (LOOP FOR MARGIN-CHOICE IN MARGIN-CHOICES
                                              THEREIS (AND MARGIN-CHOICE
							   (STRING-EQUAL (CHOICE-BOX-NAME MARGIN-CHOICE)
									 MARGIN-CHOICE-ABORT-STRING)
							   MARGIN-CHOICE)
                                              FINALLY (RETURN NIL))))
        (PROGN
          (FUNCALL (CHOICE-BOX-FUNCTION CANCEL-MARGIN-CHOICE) CANCEL-MARGIN-CHOICE NIL NIL)
          (IO-BUFFER-PUT IO-BUFFER (LIST :MENU CHOSEN-ITEM 1 SELF))
          (SETQ CHOSEN-ITEM NIL))
        ;;ELSE
        (WHEN EXPOSED-P
          (DOLIST (ITEM ITEM-LIST)
            (WHEN (STRING-EQUAL (MENU-ITEM-STRING ITEM) MARGIN-CHOICE-ABORT-STRING)
              (RETURN
                (IF (PLUSP (SHEET-MENU-COMMAND-MENU))
                    (IO-BUFFER-PUT IO-BUFFER (LIST :MENU ITEM 1 SELF))
                    ;;ELSE
                    (SETQ CHOSEN-ITEM ITEM)))))))))


(DEFMETHOD (MENU :EXECUTE) (ITEM &AUX OP ARG ITEM-NAME)
  (WHEN (PLUSP (SHEET-MENU-POP-UP)) 
    ;; Get here if either 1) user clicks on an item or 2) menu is deactivated.
    (SEND SELF :DEACTIVATE)
    (UNLESS ITEM
      (THROW 'ABORT NIL)))
  (COND ((ATOM       ITEM )       ITEM)
        ((ATOM (CDR  ITEM)) (CDR  ITEM))
        ((ATOM (CDDR ITEM)) (CADR ITEM))
        (T
         (LET ((BINDINGS (GETF (CDDDR ITEM) :BINDINGS)))
           (PROGW BINDINGS (SETQ ARG (CADDR ITEM) OP (CADR ITEM) ITEM-NAME (CAR ITEM))
                  (RETURN
                    (CASE OP
		      ;; Do something reasonable if the caller specified a
		      ;; menu item modifier but didn't specify an item type keyword.
		      (:DOCUMENTATION                    ITEM-NAME)
		      (:FONT                             ITEM-NAME)
		      (:BINDINGS                         ITEM-NAME)
		      (:VALUE                            ARG)
		      (:EVAL                       (EVAL ARG))
		      (:FUNCALL           (FUNCALL       ARG))
		      (:FUNCALL-WITH-SELF (FUNCALL       ARG SELF))
		      (:MENU              (FUNCALL (EVAL ARG) :CHOOSE))
		      (:WINDOW-OP (SEND SELF :EXECUTE-WINDOW-OP ARG))
		      (:KBD (AND SELECTED-WINDOW (SEND SELECTED-WINDOW :FORCE-KBD-INPUT ARG)))
		      (:MENU-CHOOSE
		       (LET (RESULT
			     DONE)
			 (PROCESS-RUN-FUNCTION
			   "Menu"
			   #'(LAMBDA (ARG BINDINGS RESULT-LOC DONE-LOC)
			       (PROGW BINDINGS
				 (UNWIND-PROTECT
				     (SETF (CDR RESULT-LOC)
					   (MENU-CHOOSE (CDR ARG) :LABEL (CAR ARG)))
				   (SETF (CDR DONE-LOC) T))))
			   ARG BINDINGS (LOCF RESULT) (LOCF DONE))
			 (OR (EQ CURRENT-PROCESS MOUSE-PROCESS)
			     (PROCESS-WAIT "Menu" #'CDR (LOCF DONE)))
			 RESULT))
		      (T (FERROR () "~S is unknown operation for :EXECUTE" OP)))))))))


#| Decide  what  to  return  based  on  the  item  selected.   Also have
side-effects such as calling  a function if  the item says  to.  Same as
above but returns NIL if  getting the value would  require side-effects.
This is used by highlighting option. |#
(DEFMETHOD (MENU :EXECUTE-NO-SIDE-EFFECTS) (ITEM)
  (MENU-EXECUTE-NO-SIDE-EFFECTS ITEM))

(DEFMETHOD (MENU :EXECUTE-WINDOW-OP) (FUNCTION)
  (FUNCALL FUNCTION WINDOW-UNDER-MENU OLD-X OLD-Y))

(DEFMETHOD (MENU :FILL-P) () (GEOMETRY-FILL-P GEOMETRY))

;;; Mouse handler for menus
(DEFMETHOD (MENU :BEFORE :HANDLE-MOUSE) ()
  ;; Forget anything we knew before about the highlight, so it will really be positioned
  (SETQ CURRENT-ITEM        NIL
        CURRENT-ITEM-ROW    NIL
        CURRENT-ITEM-COLUMN NIL))

(DEFMETHOD (MENU :AFTER :HANDLE-MOUSE) ()
  ;; When no selection, but mouse moved out of range, deexpose menu.
  ;; Make MOUSE-DEFAULT-HANDLER return so menu gets deactivated.
  (WHEN (PLUSP (SHEET-MENU-POP-UP))
    (OR CHOSEN-ITEM
        ;; Don't flush if mouse being usurped
        WINDOW-OWNING-MOUSE
        ;; Only flush us if either not explicitly flushing or we don't own mouse
        (AND MOUSE-RECONSIDER (EQ SELF (WINDOW-OWNING-MOUSE)))
        ;; This is called in the mouse process.  We don't want to take the chance that
        ;; we might go blocked, so run in another process.
        (PROCESS-RUN-FUNCTION '(:NAME "Menu Deactivate" :PRIORITY 20.) SELF :DEACTIVATE)))
  ;; When mouse leaves this window, stop flashing any item
  (BLINKER-SET-VISIBILITY (CAR BLINKER-LIST) NIL))

;;; This does not remember it on the list, you probably don't want to use it yourself
(DEFMETHOD (MENU :HIGHLIGHT-ITEM) (ITEM &optional (on-state t)) ;;; >>> added to handle remove hightlight in color
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM) (SEND SELF :ITEM-RECTANGLE ITEM)
    (AND (NOT (NULL LEFT))
	 (PREPARE-SHEET (SELF)			;Clip but allow extension into margins
	   (LET (savef  ;;; >>> added for color
		 saveb  ;;; >>> added for color
		 alu)                 ;;; >>> added to handle remove highlight in color
	   (SETQ alu (IF on-state alu-add alu-sub))
           (unwind-protect 
             (progn
	       (when (color-system-p self)
		   (setf savef (tv:foreground-color-register)
			 saveb (tv:background-color-register))
	           (tv:set-foreground-color-register highlighting-color) ;;; added         changed this below to select alu
	           (tv:set-background-color-register 0) ;;; >>>
	       )
	       (DRAW-RECTANGLE-INSIDE-CLIPPED (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP 
                    (IF (color-system-p self) alu alu-xor) SELF)
              )
	      (when (color-system-p self)
   	          (tv:set-foreground-color-register savef) ;;; >>> added restore of color
	          (tv:set-background-color-register saveb)
	      ) ;end when
            ) ; end unwind-protect
)))))

(DEFMETHOD (MENU :HIGHLIGHTED-VALUES) ()
  (MAPCAR #'(LAMBDA (X) (SEND SELF :EXECUTE-NO-SIDE-EFFECTS X)) HIGHLIGHTED-ITEMS))

(DEFMETHOD (MENU :HIGHLIGHTING) ()
  (PLUSP (SHEET-MENU-HIGHLIGHTING)))

;;; Here is how we make a menu appear with the last item chosen under the mouse.
;;; Return the x and y co-ordinates (inside the margins)
;;; of the center of the specified item, NIL if scrolled off display
(DEFMETHOD (MENU :ITEM-CURSORPOS) (ITEM &AUX (ALEN (ARRAY-total-size ROW-MAP)))
  (DO ((ROW (1- (MIN (+ TOP-ROW SCREEN-ROWS)	;last row on screen
		     ALEN))			;last row that exists
	    (1- ROW)))
      ((< ROW TOP-ROW) NIL)
    (AND (MEMBER ITEM (AREF ROW-MAP ROW) :TEST #'EQ)
         (OR (= ROW (1- ALEN)) (NOT (MEMBER ITEM (AREF ROW-MAP (1+ ROW)) :TEST #'EQ)))
         (RETURN
	   (IF (NOT (GEOMETRY-FILL-P GEOMETRY))
	       (+ (* (POSITION ITEM (THE LIST (AREF ROW-MAP ROW)) :TEST #'EQ) COLUMN-WIDTH)
                  (CASE ITEM-ALIGNMENT
                        (:RIGHT (- COLUMN-WIDTH (TRUNCATE (MENU-ITEM-STRING-WIDTH ITEM) 2)))
                        (:CENTER   (TRUNCATE COLUMN-WIDTH 2))
                        ;; Left
                        (OTHERWISE (TRUNCATE (MENU-ITEM-STRING-WIDTH ITEM) 2))))
               ;;ELSE
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
(DEFMETHOD (MENU :ITEM-RECTANGLE) (ITEM &AUX (X 0) SWIDTH (ALEN (ARRAY-total-size ROW-MAP)))
  (DO ((ROW (1- (MIN (+ TOP-ROW SCREEN-ROWS)	;last row on screen
		     ALEN))			;last row that exists
	    (1- ROW)))
      ((< ROW TOP-ROW) NIL)
    (COND ((AND (MEMBER ITEM (AREF ROW-MAP ROW) :TEST #'EQ)
                (OR (= ROW (1- ALEN)) (NOT (MEMBER ITEM (AREF ROW-MAP (1+ ROW)) :TEST #'EQ))))
           (IF (NOT (GEOMETRY-FILL-P GEOMETRY))
               (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH)
                     X (+ (* (POSITION ITEM (THE LIST (AREF ROW-MAP ROW)) :TEST #'EQ) COLUMN-WIDTH)
                          (CASE ITEM-ALIGNMENT
                                (:RIGHT            (- COLUMN-WIDTH MENU-INTERCOLUMN-SPACING SWIDTH))
                                (:CENTER (TRUNCATE (- COLUMN-WIDTH MENU-INTERCOLUMN-SPACING SWIDTH) 2))
                                ;; Left
                                (OTHERWISE 0))))
               ;;ELSE
               (DOLIST (IT (AREF ROW-MAP ROW))
                 (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH IT))
                 (AND (EQ IT ITEM) (RETURN))
                 (SETQ X (+ X SWIDTH MENU-INTERWORD-SPACING))))
           (RETURN (1- X) (1- (* (- ROW TOP-ROW) ROW-HEIGHT))
                   (+ X SWIDTH 1) (- (* (1+ (- ROW TOP-ROW)) ROW-HEIGHT) 2))))))

(DEFMETHOD (MENU :MARGIN-CHOICES-MINIMUM-WIDTH) ()
  (DO ((W 0)
       (CHOICES MARGIN-CHOICES (CDR CHOICES)))
      ((NULL CHOICES)
       (+ W (* (LENGTH MARGIN-CHOICES)
	       (+ (* 10 (FONT-CHAR-WIDTH CURRENT-FONT))
		  (FONT-BLINKER-HEIGHT CURRENT-FONT)))))
    (INCF W (MENU-ITEM-STRING-WIDTH (CAAR CHOICES)))))

(DEFMETHOD (MENU :MENU-DRAW) ()
  (SEND SELF :PARTIAL-MENU-DRAW TOP-ROW 0 SCREEN-ROWS))

(DEFMETHOD (MENU :PARTIAL-MENU-DRAW) (START-ROW START-Y ROWS-TO-DRAW)
  (LET ((current-color (tv:sheet-foreground-color self)) (FILL-P (GEOMETRY-FILL-P GEOMETRY)) NEW-X-POS)
    (SETQ ROWS-TO-DRAW (MIN ROWS-TO-DRAW SCREEN-ROWS))
    (AND EXPOSED-P (MOUSE-WAKEUP))  ;; Make sure the mouse knows we're changing
    (PREPARE-SHEET (SELF)

      ;; Clear menu area to be redrawn
      (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH) (* ROWS-TO-DRAW ROW-HEIGHT)
		       (SHEET-INSIDE-LEFT)  (+ (SHEET-INSIDE-TOP) START-Y)
		       ERASE-ALUF           self)
      
      (LOOP FOR ROW   FROM START-ROW BELOW (MIN TOTAL-ROWS (+ START-ROW ROWS-TO-DRAW))
	    FOR Y-POS FROM START-Y BY ROW-HEIGHT
	    DO
	    (DO ((ITEMS (AREF ROW-MAP ROW) (CDR ITEMS))
		 (END-ITEM-LIST (AREF ROW-MAP (1+ ROW)))
		 (STR)
		 (FONT)
		 (color)
		 (FLAG)
		 (X-POS 0))
		((EQ ITEMS END-ITEM-LIST))
	      (MULTIPLE-VALUE-SETQ (STR FONT color)
		(MENU-ITEM-STRING (CAR ITEMS) CURRENT-FONT SELF))
	      (UNWIND-PROTECT
		  (PROGN
		    (AND (SETQ FLAG (AND (NEQ FONT CURRENT-FONT)
					 (NOT (TYPEP FONT 'ICON))
					 CURRENT-FONT))
			 (SHEET-SET-FONT SELF FONT))
		    
		    (IF (AND color (color-system-p self))
 		      (SEND self :set-foreground-color color)
                      (SEND self :set-foreground-color current-color)
		    )
		    (COND (FILL-P		;Filled, put string followed by spacing
			   (SHEET-SET-CURSORPOS SELF X-POS Y-POS)
			   (IF (ICON-P FONT)
			       (PROGN
				 (DRAW-ICON FONT SELF X-POS Y-POS (CAR ITEMS))
				 (SETQ X-POS (+ X-POS (ICON-FONT-CHAR-WIDTH FONT) MENU-INTERWORD-SPACING)))
			       ;;ELSE
                               (SETQ X-POS (+ (MENU-ITEM-STRING-OUT STR) MENU-INTERWORD-SPACING))))
			  (t (CASE ITEM-ALIGNMENT
			       (:RIGHT
				(SETQ NEW-X-POS (+ X-POS
						   COLUMN-WIDTH
						   (- MENU-INTERCOLUMN-SPACING)
						   (- (MENU-ITEM-STRING-WIDTH (CAR ITEMS)))))
				(IF (ICON-P FONT)
				    (DRAW-ICON FONT SELF NEW-X-POS Y-POS (CAR ITEMS))
				     ;;ELSE
				    (SHEET-SET-CURSORPOS SELF NEW-X-POS Y-POS)
				    (MENU-ITEM-STRING-OUT STR)))
			       (:CENTER
				;; Calculate the X position so that this item will be centered.
				(SETQ NEW-X-POS (+ X-POS (TRUNCATE (- COLUMN-WIDTH
								      MENU-INTERCOLUMN-SPACING
								      (MENU-ITEM-STRING-WIDTH (CAR ITEMS)))
								   2)))
				(IF (ICON-P FONT)
				    (PROGN
				      (DRAW-ICON FONT SELF NEW-X-POS Y-POS (CAR ITEMS)))
				     ;;ELSE
				    (SHEET-SET-CURSORPOS SELF NEW-X-POS Y-POS)
				    (MENU-ITEM-STRING-OUT STR)))
			        ;; Left
			       (OTHERWISE
				(IF (ICON-P FONT)
				    (DRAW-ICON FONT SELF X-POS Y-POS (CAR ITEMS))
				     ;;ELSE
				    (SHEET-SET-CURSORPOS SELF X-POS Y-POS)
				    (MENU-ITEM-STRING-OUT STR))))	;Columnated, align text within columnkk
			     (SETQ X-POS (+ X-POS COLUMN-WIDTH)))))
		;; If the font changed, set it back to the current-font.
		(AND FLAG (SHEET-SET-FONT SELF FLAG))))))

    (WHEN  HIGHLIGHTED-ITEMS ;; This used to be (PLUSP (SHEET-MENU-HIGHLIGHTING)) PMH
      (DOLIST (X HIGHLIGHTED-ITEMS)
	(SEND SELF :HIGHLIGHT-ITEM X)))
    (IF (color-system-p self) (SEND self :set-foreground-color current-color))))

(DEFMETHOD (SCREEN :MENU-FONT) ()
  (SEND SELF :FONT-NAME-FOR :MENU))

(DEFMETHOD (MENU :MINIMUM-WIDTH) ()
  ;; If there is a label, the menu must be at least wide enough to accomodate it
  (LET ((L  (OR (SEND SELF :LABEL-SIZE) 0))
        (MC (IF MARGIN-CHOICES (SEND SELF :MARGIN-CHOICES-MINIMUM-WIDTH)
                0)))
    (MAX L MC 20.)))

  

;;; Mouse-click handler for menus.
;;; All buttons are treated the same, select the item you are on.
;;; Clicking when the menu is not exposed just exposes it.
(DEFMETHOD (MENU :MOUSE-BUTTONS) (BD X Y)
  (COND ((AND (>= X (SHEET-INSIDE-LEFT)) (< X (SHEET-INSIDE-RIGHT))
	      (>= Y (SHEET-INSIDE-TOP))  (< Y (SHEET-INSIDE-BOTTOM))
	      CURRENT-ITEM)                           
	 (SEND SELF :MOUSE-BUTTONS-ON-ITEM BD))	;Any button, select item.
	(T
	 (let ((buttons (MOUSE-CHARACTER-BUTTON-ENCODE BD)))
	   (if (= buttons #\MOUSE-R-2)
	       (MOUSE-CALL-SYSTEM-MENU)
	       ;; Here, clicked on the window, but outside of the window proper.  Send a
	       ;; :MOUSE-CLICK message so things like margin regions and scrolling can work.
	       (SEND SELF :MOUSE-CLICK buttons X Y)))))
  (WHEN (PLUSP (SHEET-MENU-COMMAND-MENU))
    (COND ((PLUSP (SHEET-MENU-HIGHLIGHTING))
             (IO-BUFFER-PUT IO-BUFFER (LIST :MENU (OR CURRENT-ITEM CHOSEN-ITEM) BD SELF)))
          (CHOSEN-ITEM
           (IO-BUFFER-PUT IO-BUFFER (LIST :MENU CHOSEN-ITEM BD SELF))
           (SETQ CHOSEN-ITEM NIL))))
  (WHEN (AND (PLUSP (SHEET-MENU-POP-UP)) (ZEROP (SHEET-MENU-HIGHLIGHTING)))
    (AND CHOSEN-ITEM (SETQ MOUSE-RECONSIDER T))))

(DEFMETHOD (MENU :MOUSE-BUTTONS-ON-ITEM) (BD)
  (IF (PLUSP (SHEET-MENU-HIGHLIGHTING))
      (SEND SELF
            (IF (MEMBER CURRENT-ITEM HIGHLIGHTED-ITEMS :TEST #'EQ)
                :REMOVE-HIGHLIGHTED-ITEM
                ;;ELSE
                :ADD-HIGHLIGHTED-ITEM)
            CURRENT-ITEM)
      ;;ELSE
      (PROGN
        (SETQ LAST-ITEM   CURRENT-ITEM
              CHOSEN-ITEM CURRENT-ITEM)
        (COND ((AND (CONSP CHOSEN-ITEM)
                    (>= (LENGTH CHOSEN-ITEM) 3)
                    (EQ (SECOND CHOSEN-ITEM) :BUTTONS))
               (SETQ CHOSEN-ITEM (NTH (1- (HAULONG BD)) (THIRD CHOSEN-ITEM))))))))

;;; This is the guts.  Given a menu and a set of coordinates, it finds
;;; the corresponding item, if any, sets CURRENT-ITEM to it, and sets up
;;; the blinker to mark that item.  If no item, the blinker is shut off.
;;;*** This tvobish code should be rewritten ***
(DEFMETHOD (MENU :MOUSE-MOVES) (X Y
				      &AUX ITEM ITEMS ROW XREL BLINKER BLX (BLWIDTH 0)
				           COLN STOP-ITEM ITEM-BASELINE-ADJUST
					   (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  ;; If a search window is present then it may be covering up an item which the
  ;; user wants to see.  Remove the window to make the rest of the menu visible.
;;;  (WHEN SEARCH-WINDOW
;;;    (SEND SEARCH-WINDOW :DEACTIVATE)
;;;    (DEALLOCATE-RESOURCE 'TRAVELING-SEARCH-WINDOW SEARCH-WINDOW)
;;;    (SETQ SEARCH-WINDOW NIL))
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
                (SETQ COLN 0)
		(DO ((L ITEMS (CDR L))
		     (ITM) (OITM NIL ITM)
		     (X 0 (+ X
			     (SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITM))
			     MENU-INTERWORD-SPACING)))
		    ((OR (NULL L)
			 (> X XREL))            ; If this string crosses the mouse, it's the one
                     (DECF COLN)                ; We went too far.  Back up by one.
		     (SETQ ITEM OITM))
		  (AND (EQ L STOP-ITEM)
		       ;; The next item on next line -- punt
		       (RETURN NIL))
                  (INCF COLN)
		  (SETQ ITM (CAR L)
			BLX X)))
	       (T						;Columnated, find which column
		(SETQ COLN (TRUNCATE XREL COLUMN-WIDTH))	;Column selected
		(SETQ ITEM (CAR (NTHCDR COLN ITEMS)))		;This may be NIL
		(SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH))
		(SETQ BLX (+ (* COLN COLUMN-WIDTH)		;Start of column
                             (CASE ITEM-ALIGNMENT
                                   (:RIGHT (MAX 0 (- COLUMN-WIDTH
                                                     MENU-INTERCOLUMN-SPACING
                                                     BLWIDTH)))
                                   (:CENTER (MAX 0 (TRUNCATE (- COLUMN-WIDTH  ;Centering
                                                                  MENU-INTERCOLUMN-SPACING
                                                                  BLWIDTH)
                                                               2)))
                                   ;; Left
                                   (OTHERWISE 0))))))))
  (MULTIPLE-VALUE-BIND (NIL ITEM-FONT)
      (MENU-ITEM-STRING ITEM CURRENT-FONT SELF)
    ;; Put the top of the blinker at the top of the item.
    (SETQ ITEM-BASELINE-ADJUST (- BASELINE (FONT-BASELINE ITEM-FONT)))
    ;; If this item is non-selectable, don't select it.
    (AND (NOT (ATOM ITEM)) (NOT (ATOM (CDR ITEM))) (NOT (ATOM (CDDR ITEM)))
         (ITEM-CONTAINS-NO-SELECT-P ITEM)
	 (SETQ ITEM NIL))
    ;; Now make the blinker be where and what we have just found it should be.
    (BLINKER-SET-VISIBILITY BLINKER (NOT (NULL ITEM)))
    (SETQ CURRENT-ITEM        ITEM
          CURRENT-ITEM-ROW    (+ ROW TOP-ROW)
          CURRENT-ITEM-COLUMN COLN)
    (IF (NULL CURRENT-ITEM)
	(SEND SELF :MOUSE-STANDARD-BLINKER)
	;;ELSE
      ;; Position the hollow rectangular blinker around the item.
      (SEND BLINKER :SET-SIZE-AND-CURSORPOS
            (+ BLWIDTH 1)
            ;; Get the height from the font of the item.
            (+ 3 (FONT-CHAR-HEIGHT ITEM-FONT))
            BLX
            (+ (* ROW ROW-HEIGHT) ITEM-BASELINE-ADJUST -1))
      (send who-line-documentation-window :update)
      (SEND SELF :MOUSE-STANDARD-BLINKER (char-int *menu-mouse-item-glyph*)))))

(DEFMETHOD (MENU :MOUSE-STANDARD-BLINKER) (&OPTIONAL (CHAR  (char-int *menu-mouse-no-item-glyph*)))
  ;; Change the mouse cursor to a small X so it doesn't get in the way
  (MOUSE-SET-BLINKER-DEFINITION :CHARACTER 4 5 :ON
				:SET-CHARACTER CHAR))

;;; Put a menu near another window.  This will normally try to put it just below
;;; it and give it the same width.
(DEFMETHOD (MENU :MOVE-NEAR-WINDOW) (W)
  (WHEN (PLUSP (SHEET-MENU-DYNAMIC))
    (SEND SELF :UPDATE-ITEM-LIST))
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (SEND W :EDGES)
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE NEW-HEIGHT)
	(MENU-DEDUCE-PARAMETERS NIL NIL (- RIGHT LEFT) NIL NIL NIL)
      (SETQ NEW-HEIGHT (+ NEW-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
      ;;If it won't fit below try putting it above
      (AND (> (+ BOTTOM NEW-HEIGHT)
	      (SHEET-INSIDE-BOTTOM SUPERIOR))
	   (SETQ BOTTOM (MAX (- TOP NEW-HEIGHT) 0)))
      ;;Put it there
      (SEND SELF :SET-EDGES LEFT BOTTOM RIGHT (+ BOTTOM NEW-HEIGHT) :TEMPORARY)
      (SEND SELF (IF (PLUSP (SHEET-MENU-COMMAND-MENU))
                     :EXPOSE
                     ;;ELSE
                     :SELECT)))))

;;; Make sure that menus don't appear in the system menu's `select' menu.
(DEFMETHOD (MENU :NAME-FOR-SELECTION) ()
  NIL)

(DEFMETHOD (MENU :BEFORE :PANE-SIZE) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

;Changing our borders should preserve our INSIDE size, not our outside size as normally.
;;; [may] 9-7-88 Above comment assumes a temporary window. Panes and permanent
;;; windows should make use of :geometry. W:menu temporary windows may not have
;;; much :geometry defined at this point.
(DEFWRAPPER (MENU :REDEFINE-MARGINS) (IGNORE . BODY)
  `(LET ((IW (SHEET-INSIDE-WIDTH)) (IH (SHEET-INSIDE-HEIGHT)))
     (PROG1 (PROGN . ,BODY)
	    (unless (AND (plusp (sheet-menu-permanent))	;; may 10-24-88
			 (send self :exposed-p))	;; may 10-24-88
	      ;; NOT a permanent exposed window 		;; may 10-24-88
	      (MENU-COMPUTE-GEOMETRY NIL IW IH)))))

(DEFMETHOD (MENU :AFTER :REFRESH) (&OPTIONAL TYPE)
  (OR (AND RESTORED-BITS-P (NEQ TYPE :SIZE-CHANGED))
      (SEND SELF :MENU-DRAW)))

(DEFMETHOD (MENU :REMOVE-HIGHLIGHTED-ITEM) (ITEM)
  (COND ((MEMBER ITEM HIGHLIGHTED-ITEMS :TEST #'EQ)
         (SETQ HIGHLIGHTED-ITEMS (DELETE ITEM (THE LIST HIGHLIGHTED-ITEMS) :TEST #'EQ))
         (SHEET-FORCE-ACCESS (SELF T) (SEND SELF :HIGHLIGHT-ITEM ITEM nil)))))

(DEFMETHOD (MENU :REMOVE-HIGHLIGHTED-VALUE) (VALUE)
  (DO ((L ITEM-LIST (CDR L)))
      ((NULL L) (FERROR NIL "Value not found"))
    (AND (EQUAL  (SEND SELF :EXECUTE-NO-SIDE-EFFECTS (CAR L)) VALUE)
	 (RETURN (SEND SELF :REMOVE-HIGHLIGHTED-ITEM (CAR L))))))

;;; This is used by othogonal things like hysteretic window
(DEFMETHOD (MENU :SCROLL-BAR-P) () (< SCREEN-ROWS TOTAL-ROWS))

(DEFMETHOD (MENU :SCROLL-POSITION) ()
  (VALUES TOP-ROW TOTAL-ROWS ROW-HEIGHT SCREEN-ROWS))

(DEFMETHOD (MENU :SCROLL-TO) (NEW-TOP-ROW MODE)
  (CASE MODE
    (:ABSOLUTE)      
    (:RELATIVE (SETQ NEW-TOP-ROW (+ TOP-ROW NEW-TOP-ROW)))      
    (OTHERWISE (FERROR () "Illegal scroll mode ~A" MODE)))

  ;; Check for change. Peg NEW-TOP-ROW to first row or to a screenful above last row.
  (WHEN (NOT (= TOP-ROW (SETQ NEW-TOP-ROW (MAX 0 (MIN NEW-TOP-ROW (- TOTAL-ROWS SCREEN-ROWS))))))
    (LET* ((DELTA        (if highlighted-items
			     ;; Inhibit the BITBLT optimization here if multiple items are
			     ;; highlighted; otherwise highlighted items in the BITBLT'ed area
			     ;; are subsequently turned off by the partial menu draw.  PMH 11/30/87
			     screen-rows
			     (- NEW-TOP-ROW TOP-ROW)))
	   (DELTA-ROWS   (ABS DELTA))
	   (ROWS-SHIFTED (MAX 0 (- SCREEN-ROWS DELTA-ROWS)))
	   ROW-TO-REDRAW)
          
      (IF (PLUSP ROWS-SHIFTED)
	  ;; Scrolling less that SCREEN-ROWS -- bitblt part of current menu image up or down
	  (LET ((FROM-Y       (+ (SHEET-INSIDE-TOP) (* (MAX 0 DELTA) ROW-HEIGHT)))
		(HEIGHT-ORDER (* (SIGNUM DELTA) (* ROWS-SHIFTED ROW-HEIGHT))))	    
	    (PREPARE-SHEET (SELF)
	      (BITBLT W:ALU-SETA (SHEET-INSIDE-WIDTH) HEIGHT-ORDER
		  SCREEN-ARRAY (SHEET-INSIDE-LEFT) FROM-Y
		  SCREEN-ARRAY (SHEET-INSIDE-LEFT) (- FROM-Y (* DELTA ROW-HEIGHT))))))

      ;; Reset new TOP-ROW and redraw as many menu items as necessary
      (SETQ TOP-ROW NEW-TOP-ROW)
      (SETQ ROW-TO-REDRAW (IF (PLUSP DELTA)
			      (+ TOP-ROW ROWS-SHIFTED)
			      TOP-ROW))
      (SEND SELF :PARTIAL-MENU-DRAW
	    ROW-TO-REDRAW			;start row
	    (* (- ROW-TO-REDRAW TOP-ROW)	;start y
	       ROW-HEIGHT)
	    DELTA-ROWS)				;number of rows to draw 
						;(:PARTIAL-MENU-DRAW forces this <= SCREEN-ROWS)	
      (SEND SELF :NEW-SCROLL-POSITION TOP-ROW))))

(DEFMETHOD (MENU :SET-COLUMN-SPEC-LIST) (NEW-COLUMN-SPEC-LIST)
  (SETQ PREVIOUS-STATE (MAKE-LIST (LENGTH NEW-COLUMN-SPEC-LIST)))
  (SETQ COLUMN-SPEC-LIST NEW-COLUMN-SPEC-LIST)
  (SEND SELF :UPDATE-ITEM-LIST T))

(DEFMETHOD (MENU :SET-DEFAULT-FONT) (FONT)
  (SETQ DEFAULT-FONT FONT)
  (SEND SELF :SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST)))

;OPTION = :TEMPORARY means don't change the constraints.
;That is used in calls from within this file.
;If the user randomly does a :SET-EDGES, we assume he wants it to stay as he said.
(DEFMETHOD (MENU :AFTER :SET-EDGES) (IGNORE IGNORE IGNORE IGNORE &OPTIONAL OPTION)
  (COND ((NOT (MEMBER OPTION '(:VERIFY :TEMPORARY) :TEST #'EQ))
         (SETF (GEOMETRY-INSIDE-WIDTH  GEOMETRY) (SHEET-INSIDE-WIDTH))
         (SETF (GEOMETRY-INSIDE-HEIGHT GEOMETRY) (SHEET-INSIDE-HEIGHT))
	 ;; Since we are changing the inside width/height, we also need
	 ;; to update the maximum width/height if specified.
	 (WHEN (NUMBERP (GEOMETRY-MAX-WIDTH  GEOMETRY))
	   (SETF        (GEOMETRY-MAX-WIDTH  GEOMETRY) (SHEET-INSIDE-WIDTH)))
	 (WHEN (NUMBERP (GEOMETRY-MAX-HEIGHT GEOMETRY))
	   (SETF        (GEOMETRY-MAX-HEIGHT GEOMETRY) (SHEET-INSIDE-HEIGHT))))))

(DEFMETHOD (MENU :SET-FILL-P) (FILL-P)
  (SEND SELF :SET-GEOMETRY (IF FILL-P 0 NIL)))

;; a pop-up menu may not have much GEOMETRY at this point but a permanent window DOES!
;; prevent shrinking a permanent window ( pane ) when scroll bar removed.
(DEFMETHOD (MENU :AFTER :SET-FONT-MAP) (&REST IGNORE)
  (COND ((AND (plusp (sheet-menu-permanent))	;; may 9-7-88
	      (send self :exposed-p))		;; may 10-24-88
	 ;; a permanent exposed window 		;; may 10-24-88
	 (menu-compute-geometry t (sheet-inside-width) (sheet-inside-height)))
	(t
	 (MENU-COMPUTE-GEOMETRY T))))

(DEFMETHOD (MENU :SET-GEOMETRY) (&REST NEW-GEOMETRY)
  "NIL for an argument means make it unconstrained.  T or unsupplied means leave it alone"
  (DECLARE (ARGLIST (&OPTIONAL N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT
			       MAX-WIDTH MAX-HEIGHT)))
  (OR (<= (LENGTH NEW-GEOMETRY) (LENGTH GEOMETRY))
      (FERROR NIL "Too many args to :SET-GEOMETRY"))
  (DO ((G  NEW-GEOMETRY (CDR G))
       (CG GEOMETRY     (CDR CG)))
      ((NULL G))
    (IF (NEQ (CAR G) T)
	(RPLACA CG (CAR G))))
  (MENU-COMPUTE-GEOMETRY T))

(DEFMETHOD (MENU :SET-HIGHLIGHTED-ITEMS) (NEW-HIGHLIGHTED-ITEMS &AUX OLD)
  (WHEN (SHEET-MENU-HIGHLIGHTING)
    (SETQ OLD HIGHLIGHTED-ITEMS HIGHLIGHTED-ITEMS NEW-HIGHLIGHTED-ITEMS)
    (SHEET-FORCE-ACCESS (SELF T)
      (DOLIST (X OLD)
        (OR (MEMBER X NEW-HIGHLIGHTED-ITEMS :TEST #'EQ)
            (SEND SELF :HIGHLIGHT-ITEM X nil)))	;NIL argument turns them off
      (DOLIST (X NEW-HIGHLIGHTED-ITEMS)
        (OR (MEMBER X OLD :TEST #'EQ) (SEND SELF :HIGHLIGHT-ITEM X T))))))


(DEFMETHOD (MENU :SET-HIGHLIGHTED-VALUES) (VALUES &AUX ITEMS)
  (WHEN (SHEET-MENU-HIGHLIGHTING)
    (DOLIST (ITEM ITEM-LIST)
      (AND (MEMBER (SEND SELF :EXECUTE-NO-SIDE-EFFECTS ITEM) VALUES :TEST #'EQUAL)
           (PUSH ITEM ITEMS)))
    (OR (= (LENGTH ITEMS) (LENGTH VALUES)) (FERROR NIL "Missing or duplicate value"))
    (SEND SELF :SET-HIGHLIGHTED-ITEMS ITEMS)))

(DEFMETHOD (MENU :SET-HIGHLIGHTING-COLOR) (COLOR-VALUE)
  (DOLIST (X HIGHLIGHTED-ITEMS)
    (SEND SELF :HIGHLIGHT-ITEM X nil))	;NIL argument turns them off
  (SETF HIGHLIGHTING-COLOR COLOR-VALUE)
  (DOLIST (X HIGHLIGHTED-ITEMS)
    (SEND SELF :HIGHLIGHT-ITEM X))
  COLOR-VALUE)

(defmethod (menu :after :restore-default-colors)()
  (setf foreground-color tv:*default-menu-foreground*)
  (setf background-color tv:*default-menu-background*)
  (send self :set-highlighting-color tv:*default-blinker-offset*))

(DEFMETHOD (MENU :SET-ITEM-LIST) (NEW-ITEM-LIST)
  (WHEN (PLUSP (SHEET-MENU-HIGHLIGHTING))
    (SEND SELF :SET-HIGHLIGHTED-ITEMS HIGHLIGHTED-ITEMS))
  
;;;; If multicolumn is specified the sort funtion is performed in 
;;;; the upadte-item-list method so don't do it again
  (WHEN (and SORT (zerop (SHEET-MENU-MULTICOLUMN)))
    (MENU-ITEM-SORTER NEW-ITEM-LIST SORT))

  (SETQ NEW-ITEM-LIST (REMOVE NIL (THE LIST NEW-ITEM-LIST) :TEST #'EQ))
  (SETQ ITEM-LIST           NEW-ITEM-LIST
        LAST-ITEM           NIL
        CURRENT-ITEM        NIL
        CURRENT-ITEM-ROW    NIL
        CURRENT-ITEM-COLUMN NIL)
  (SEND SELF :SET-FONT-MAP (MENU-COMPUTE-FONT-MAP ITEM-LIST))
  NEW-ITEM-LIST)


(DEFMETHOD (MENU :SET-MENU-MARGIN-CHOICES) (LIST)
  (SEND SELF :SET-MARGIN-CHOICES (PARSE-MARGIN-CHOICES LIST)))

(DEFMETHOD (MENU :SET-POSITION) (NEW-X NEW-Y &OPTIONAL OPTION)
  (SEND SELF :SET-EDGES NEW-X NEW-Y
	   (+ WIDTH NEW-X) (+ HEIGHT NEW-Y)
	   (OR OPTION :TEMPORARY)))

(DEFMETHOD (MENU :BEFORE :SIZE) (&REST IGNORE)
  (SEND SELF :UPDATE-ITEM-LIST))

(DEFMETHOD (MENU :UPDATE-ITEM-LIST) (&OPTIONAL FORCE)
  (IF (PLUSP (SHEET-MENU-MULTICOLUMN))
      (IF (OR FORCE
              (LOOP FOR (HEADING FORM) IN COLUMN-SPEC-LIST
                    AND OLD-ITEM-LIST  IN PREVIOUS-STATE
                    THEREIS (NEQ (IF (SYMBOLP FORM)
                                     (SYMBOL-VALUE FORM)
                                     (EVAL FORM))
                                 OLD-ITEM-LIST)))
          ;; Something has changed, set up new item list.
          ;; Start by extracting the column lists and setting up the headings.
	 
	  (LOOP FOR (HEADING FORM . OPTIONS) IN COLUMN-SPEC-LIST
                AND STATEL ON PREVIOUS-STATE
		;; modified the next line to remove NILs from the item list
		;; page 14-3 11/16/87 PMH SPR#3544
                FOR INTERIOR-ITEMS = (remove nil (IF (SYMBOLP FORM) (SYMBOL-VALUE FORM) (EVAL FORM)))
                AND SORT-OPTION     = (CADR (MEMBER :SORT OPTIONS))
		AND SORT-ALL = (SEND SELF :SORT)
		IF (OR SORT-ALL SORT-OPTION)
		WHEN SORT-OPTION 
                DO (SETQ INTERIOR-ITEMS (MENU-ITEM-SORTER INTERIOR-ITEMS SORT-OPTION))
		ELSE DO (SETQ INTERIOR-ITEMS (MENU-ITEM-SORTER INTERIOR-ITEMS SORT-ALL))
                COLLECT `(,HEADING :NO-SELECT T . ,OPTIONS) INTO NEW-ITEM-LIST
                COLLECT INTERIOR-ITEMS INTO COLUMN-VALUES
                FINALLY
                ;; Documentation alert!!  The following code is somewhat complex.  The complexity is
                ;; due to a concern about efficiency.  Upon entry to this part, NEW-ITEM-LIST
                ;; contains a list of the column headings which contain the :NO-SELECT keyword
                ;; (e.g.  (("A" :no-select) ("B" :no-select) ("C" :no-select)) ) and COLUMN-VALUES
                ;; contains a list of the columns (e.g.  ((1 2 3) (4 5 6) (6 7 8)) ).  To start
                ;; off, we reverse the ordering of NEW-ITEM-LIST to make subsequent PUSHes add to
                ;; the end properly.  Next, we save the columns into PREVIOUS-STATE so we can check
                ;; later to see if any columns have changed their values (see the first LOOP in
                ;; this method).  The next LOOP iterates through the rows of the menu.  This LOOP
                ;; contains an inner LOOP which iterates through the columns of that row, hacking
                ;; COLUMN-VALUES as it goes.  For example, if NEW-ITEM-LIST upon entry contained
                ;; (("C" :no-select) ("B" :no-select) ("A" :no-select)) ) and COLUMN-VALUES
                ;; contained ((1 2 3) (4 5 6) (6 7 8)), then the following is a trace of the
                ;; execution of the inner part of this last LOOP.
                ;;   L						NEW-ITEM-LIST
                ;;   ((1 2 3) (4 5 6) (7 8 9))
                ;;				                (1 ("C" :no-select) ("B" :no-select) ("A" :no-select))
                ;;   ((  2 3) (4 5 6) (7 8 9))
                ;;   (        (4 5 6) (7 8 9))
                ;; 				              (4 1 ("C" :no-select) ("B" :no-select) ("A" :no-select))
                ;;   (        (  5 6) (7 8 9))
                ;;   (                (7 8 9))
                ;; 				            (7 4 1 ("C" :no-select) ("B" :no-select) ("A" :no-select))
                ;;   (                (  8 9))
                ;;   ((  2 3) (  5 6) (  8 9))
                ;; 				          (2 7 4 1 ("C" :no-select) ("B" :no-select) ("A" :no-select))
                ;;   ((    3) (  5 6) (  8 9))
                ;;   (        (  5 6) (  8 9))
                ;; 				        (5 2 7 4 1 ("C" :no-select) ("B" :no-select) ("A" :no-select))
                ;;   (        (    6) (  8 9))
                ;;   (                (  8 9))
                ;; 				      (8 5 2 7 4 1 ("C" :no-select) ("B" :no-select) ("A" :no-select))
                ;;   (                (    9))
                ;;   ((    3) (    6) (    9))
                ;; 				    (3 8 5 2 7 4 1 ("C" :no-select) ("B" :no-select) ("A" :no-select))
                ;;   ((     ) (    6) (    9))
                ;;   (        (    6) (    9))
                ;; 				  (6 3 8 5 2 7 4 1 ("C" :no-select) ("B" :no-select) ("A" :no-select))
                ;;   (        (     ) (    9))
                ;;   (                (    9))
                ;; 				(9 6 3 8 5 2 7 4 1 ("C" :no-select) ("B" :no-select) ("A" :no-select))
                ;;   (                (     ))

                ;; Note that this LOOP contains a test to make sure that there is an element in
                ;; each column.  If there is no element for a particular column, than one which
                ;; contains :NO-SELECT is used.  If the sample menu were displayed using
                ;; NEW-ITEM-LIST as the item list it would look something like:
                ;;   9 6 3
                ;;   8 5 2
                ;;   7 4 1
                ;;   C B A
                ;; which, of course, is in reverse order.  We need to reverse the order (which is
                ;; done after computing the geometry) which makes the menu look like:
                ;;   A B C
                ;;   1 4 7
                ;;   2 5 8
                ;;   3 6 9
                (SETQ NEW-ITEM-LIST (NREVERSE NEW-ITEM-LIST))
                ;; Save the old state of the columns in PREVIOUS-STATE.
                (LOOP FOR C IN COLUMN-VALUES
                      AND L ON PREVIOUS-STATE
                      DO (RPLACA L C))
                ;; Now interleave the columns, and save the old state.
                (LOOP REPEAT (LOOP FOR C IN COLUMN-VALUES MAXIMIZE (LENGTH C)) DO
                      (LOOP FOR L ON COLUMN-VALUES DO
                            (PUSH (OR (CAAR L) '("" :NO-SELECT T)) NEW-ITEM-LIST)
                            (RPLACA L (CDAR L))))
                (WITH-SHEET-DEEXPOSED (SELF)
                  (OR (EQ (CAR GEOMETRY) (LENGTH COLUMN-SPEC-LIST))
                      ;; Only compute the geometry if the number of columns has changed.
                      (SEND SELF :SET-GEOMETRY (LENGTH COLUMN-SPEC-LIST)))
                  (SEND SELF :SET-ITEM-LIST (NREVERSE NEW-ITEM-LIST)))))
      ;;ELSE - single column
      (LET (NEW-ITEM-LIST)
        (WHEN (AND ITEM-LIST-POINTER
                   (NOT (EQUAL ITEM-LIST (SETQ NEW-ITEM-LIST (MENU-DYNAMIC-ITEM-LIST ITEM-LIST-POINTER)))))
          (MENU-COMPUTE-GEOMETRY NIL)
          (SEND SELF :SET-ITEM-LIST NEW-ITEM-LIST)))))



;;; This is called from the scheduler
(DEFMETHOD (MENU :WHO-LINE-DOCUMENTATION-STRING) ()
  (OR (AND (VARIABLE-BOUNDP CURRENT-ITEM) (MENU-ITEM-WHO-LINE-DOCUMENTATION CURRENT-ITEM))
      ""))

#|

This code implements the keyboard interface to menus.  The idea is to
allow the user to use the keyboard to move the mouse around, just like
the user can to with Choose Variable Values windows.  Most of this code
was adapted from the Choose Variable Values code.  |#
(DEFPARAMETER MENU-DEFAULT-COMMAND-CHARACTERS
	      '((#\PAGE       (:REFRESH                   ) "Refresh the menu")
	       (#\RETURN      (:MOVE-CURSOR-NEXT-LINE     ) "Move the mouse cursor to the next line")
               (""            (                           ) "")
               (#\CONTROL-S   (:SEARCH #\CONTROL-S        ) "Search forward for an item")
               (#\CONTROL-R   (:SEARCH #\CONTROL-R        ) "Search backward for an item")
               (""            (                           ) NIL)
               (#\META-<      (:MOVE-CURSOR-TOP           ) "Move the cursor to the topmost item")
               (#\META->      (:MOVE-CURSOR-BOTTOM        ) "Move the cursor to the bottommost item")
               (""            (                           ) NIL)
	       (#\CONTROL-P   (:MOVE-CURSOR-UP            ) "Move the mouse cursor up")
	       (#\CONTROL-N   (:MOVE-CURSOR-DOWN          ) "Move the mouse cursor down")
	       (#\CONTROL-F   (:MOVE-CURSOR-FORWARD       ) "Move the mouse cursor right")
	       (#\CONTROL-B   (:MOVE-CURSOR-BACKWARD      ) "Move the mouse cursor left")
               (""            (                           ) NIL)
	       (#\CONTROL-A   (:MOVE-CURSOR-FIRST         ) "Move the mouse cursor to the leftmost column")
	       (#\CONTROL-E   (:MOVE-CURSOR-LAST          ) "Move the mouse cursor to the rightmost column")
               (""            (                           ) NIL)
               (#\CONTROL-V   (:MOVE-CURSOR-PAGE-DOWN     ) "Move the mouse cursor to the next page")
               (#\META-V      (:MOVE-CURSOR-PAGE-UP       ) "Move the mouse cursor to the previous page")
               (""            (                           ) NIL)
	       (#\UP-ARROW    (:MOVE-CURSOR-UP            ) "Move the mouse cursor up")
	       (#\DOWN-ARROW  (:MOVE-CURSOR-DOWN          ) "Move the mouse cursor down")
	       (#\RIGHT-ARROW (:MOVE-CURSOR-FORWARD       ) "Move the mouse cursor right")
	       (#\LEFT-ARROW  (:MOVE-CURSOR-BACKWARD      ) "Move the mouse cursor left")
               (""            (                           ) NIL)
               (#\SPACE       (:SELECT-CURRENT-ITEM       ) "Select this item")
	       (#\ABORT       (:ABORT-SELECTION           ) "Exit abnormally")
	       (#\CONTROL-G   (:ABORT-SELECTION           ) "Exit abnormally")
	       (#\END         (:END-ITEM-SELECTION        ) "Exit normally with item(s) selected")
	       (#\HELP        (:HELP                      ) "Display this text"))
              "An alist of (character (method-name .  argument-list)
documentation-string used by MENU windows.  If the
METHOD-NAME is NIL then the other items are present for
documentation purposes.  For example, ABORT is handled elsewhere,
but is described here to tell the user about it.  If the CHARACTER is
actually a string, then it is displayed for documentation purposes only.
If the CHARACTER is really a character, and if the METHOD-NAME has
a non-NIL value, then the method is called with ARGUMENT-LIST")

#| Implementation note:  All of  the methods  here  return  NIL at the end,
except for those which are returning  a value to the caller.   Currently
the only one  of that  nature is  :END-ITEM-SELECTION which  returns the
item under the mouse  to the caller.   The reason for  the other methods
returning NIL is  to indicate  to an  external caller  that nothing else
need be done.  An external caller will generally be one using a  command
menu who will be retrieving  the characters explicitly and  then passing
appropriate ones onto the menu by sending the :PROCESS-CHARACTER message
to the menu instance.  For that external caller, this message  returning
non-NIL indicates that an item has been selected and that it needs to be
processed.  |#

(DEFMETHOD (MENU :HELP)
           (&OPTIONAL (HELP-ALIST
                        (APPEND COMMAND-CHARACTERS
                                MENU-DEFAULT-COMMAND-CHARACTERS)))
  (SI:WITH-HELP-STREAM (WINDOW :LABEL "General menu commands" :SUPERIOR MOUSE-SHEET)
    (SEND WINDOW :SET-PROCESS CURRENT-PROCESS)
        ;; Window configuration stable now, let keyboard process proceed.
        (SETQ KBD-TERMINAL-TIME NIL)
        (FORMAT WINDOW "~%The following command characters are defined:~%")
        (DOLIST (ELEMENT HELP-ALIST)
          (IF (OR (NUMBERP (CAR ELEMENT)) (CHARACTERP (CAR ELEMENT)))
	      (PROGN
		(WHEN (NOT (AND (CHAR= (CAR ELEMENT) #\SPACE)
				(NULL HIGHLIGHTED-ITEMS)
				(STRING-EQUAL (THIRD ELEMENT) "Select this item")))
		  (FORMAT WINDOW "~{~%  ~20<~@:C~>  ~*~:[~;~:*~A.~]~}" ELEMENT)))
              ;;ELSE
              (FORMAT WINDOW "~{~%  ~20<~A~>  ~*~:[~;~:*~A.~]~}" ELEMENT))))
  NIL)

(DEFMETHOD (MENU :PROCESS-CHARACTER) (CH)
  "Process command characters typed at top level.  If an unknown
character is entered, a beep will be sounded."
  (LET (COMMAND)
    (COND ((SETQ COMMAND (ASSOC CH COMMAND-CHARACTERS :TEST #'EQL))
	   (LEXPR-SEND SELF (SECOND COMMAND)))
	  ((SETQ COMMAND (ASSOC CH MENU-DEFAULT-COMMAND-CHARACTERS :TEST #'EQL))
	   ;; If there isn't any method then beep.
	   (IF (SECOND COMMAND)
	       (LEXPR-SEND SELF (SECOND COMMAND))
	       ;;ELSE
	       (BEEP)))
	  (T
           (IF (AND (LISTP CH) (EQ (CAR CH) :MOUSE-BUTTON))
               ;; The user made a stray mouse click.  Ignore it
               NIL
               ;;ELSE
               (BEEP))))))

(DEFMETHOD (MENU :ABORT-SELECTION) ()
  "Abort item selection."
  (SEND SELF :DEACTIVATE)
  NIL)

(DEFMETHOD (MENU :END-ITEM-SELECTION) ()
  "Select the item that is under the mouse, or `press' the Do It choice box."
  (LET (SELECTION-MARGIN-CHOICE)
    (IF (AND MARGIN-CHOICES
             (SETQ SELECTION-MARGIN-CHOICE (LOOP FOR MARGIN-CHOICE IN MARGIN-CHOICES
                                                 THEREIS (AND (STRING-EQUAL (CHOICE-BOX-NAME MARGIN-CHOICE)
                                                                            MARGIN-CHOICE-COMPLETION-STRING) MARGIN-CHOICE)
                                                 FINALLY (RETURN NIL))))
        (PROGN
          (FUNCALL (CHOICE-BOX-FUNCTION SELECTION-MARGIN-CHOICE) SELECTION-MARGIN-CHOICE NIL NIL)
          (SETQ CHOSEN-ITEM (eighTH SELECTION-MARGIN-CHOICE))) ;; may 9-14-88 was sixth
        ;;ELSE
        (PROGN
          (SETQ CHOSEN-ITEM CURRENT-ITEM
                LAST-ITEM   CHOSEN-ITEM)
          (WHEN (AND (CONSP CHOSEN-ITEM)
                      (>= (LENGTH CHOSEN-ITEM) 3)
                      (EQ (SECOND CHOSEN-ITEM) :BUTTONS))
            (SETQ CHOSEN-ITEM (CAR (THIRD CHOSEN-ITEM))))))))

(DEFMETHOD (MENU :SELECT-CURRENT-ITEM) ()
  (WHEN (AND CURRENT-ITEM (PLUSP (SHEET-MENU-HIGHLIGHTING)))
    (SEND SELF :MOUSE-BUTTONS-ON-ITEM 0))
  NIL)

#| Implementation note: The following methods implement the mouse cursor  movement
commands.  The initial implementation of these was quite primitive in that  they
worked fine for  simple menus,  but did  not work  as one  would expect for more
complex menus like the system menu.  The movement problem becomes more difficult
when a menu has an item list in  which some of the items contain the  :NO-SELECT
keyword.  A movement command should  not move onto such  an item.  When that  is
attempted, the more convoluted code is executed  to place the mouse cursor at  a
reasonable position, which sometimes involves not moving it at all.  The  intent
is to have the  movement be the  same as the  movement would be  in Zmacs on  an
individual command basis.  This means that it may not be the same as Zmacs  when
a series of movement commands are executed (particularily for the  previous/next
line movement commands).  |#

(DEFMETHOD (MENU :MOVE-CURSOR-BACKWARD) (&OPTIONAL (MOVE-THE-MOUSE T))
  ;; CONTROL-B
  ;; Search backward from the current item through the remaining items to find
  ;; one which doesn't have a :NO-SELECT keyword.
  (MOVE-TO-NEAREST-MENU-ITEM)
  (LOOP NAMED OUTER-LOOP
	FOR CURRENT-ROW-NUMBER FROM CURRENT-ITEM-ROW DOWNTO 0
	FOR NEXT-ROW = (AREF ROW-MAP (1+ CURRENT-ROW-NUMBER)) THEN CURRENT-ROW
	FOR CURRENT-ROW = (AREF ROW-MAP CURRENT-ROW-NUMBER)
	WHILE CURRENT-ROW
	FOR CURRENT-ROW-LENGTH = (- (LENGTH CURRENT-ROW) (LENGTH NEXT-ROW))
	FOR INITIAL-COLUMN-INDEX = CURRENT-ITEM-COLUMN THEN CURRENT-ROW-LENGTH
	DO (LOOP FOR CURRENT-COLUMN-NUMBER FROM (1- INITIAL-COLUMN-INDEX) DOWNTO 0
		 FOR THIS-ITEM = (NTH CURRENT-COLUMN-NUMBER CURRENT-ROW)
		 WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P THIS-ITEM))
		 DO (PROGN
		      (SETQ CURRENT-ITEM-ROW    CURRENT-ROW-NUMBER
			    CURRENT-ITEM-COLUMN CURRENT-COLUMN-NUMBER
			    CURRENT-ITEM        THIS-ITEM)
		      (RETURN-FROM OUTER-LOOP T)))
	FINALLY (RETURN NIL))
  (WHEN MOVE-THE-MOUSE
    (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))
  NIL)

(DEFMETHOD (MENU :MOVE-CURSOR-BOTTOM) ()
  ;; Search backward through the items to find one which doesn't have a
  ;; :NO-SELECT keyword.
  (WHEN (LOOP NAMED OUTER-LOOP
              FOR CURRENT-ROW-NUMBER FROM (1- TOTAL-ROWS) DOWNTO 0
              FOR NEXT-ROW = NIL THEN CURRENT-ROW
              FOR CURRENT-ROW = (AREF ROW-MAP CURRENT-ROW-NUMBER)
              WHILE CURRENT-ROW
              FOR CURRENT-ROW-LENGTH = (- (LENGTH CURRENT-ROW) (LENGTH NEXT-ROW))
              DO (LOOP FOR CURRENT-COLUMN-NUMBER FROM (1- CURRENT-ROW-LENGTH) DOWNTO 0
                       FOR THIS-ITEM = (NTH CURRENT-COLUMN-NUMBER CURRENT-ROW)
                       WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P THIS-ITEM))
                       DO (PROGN
                            (SETQ CURRENT-ITEM-ROW    CURRENT-ROW-NUMBER
                                  CURRENT-ITEM-COLUMN CURRENT-COLUMN-NUMBER
                                  CURRENT-ITEM        THIS-ITEM)
                            (RETURN-FROM OUTER-LOOP T)))
              FINALLY (RETURN NIL))
    (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))
  NIL)

(DEFMETHOD (MENU :MOVE-CURSOR-FIRST) ()
  ;; CONTROL-A
  (MOVE-TO-NEAREST-MENU-ITEM)
  (LET* ((CURRENT-ROW (AREF ROW-MAP CURRENT-ITEM-ROW))
	 (NEXT-ROW (AREF ROW-MAP (1+ CURRENT-ITEM-ROW)))
	 (CURRENT-ROW-LENGTH (- (LENGTH CURRENT-ROW) (LENGTH NEXT-ROW))))
    ;; Search forward through the items of the current row for an item which
    ;; doesn't have a :NO-SELECT keyword.
    (WHEN (LOOP FOR CURRENT-COLUMN-NUMBER FROM 0 BELOW CURRENT-ROW-LENGTH
		FOR THIS-ITEM IN CURRENT-ROW
		WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P THIS-ITEM))
		DO (PROGN
		     (SETQ CURRENT-ITEM-COLUMN CURRENT-COLUMN-NUMBER
			   CURRENT-ITEM        THIS-ITEM)
		     (RETURN T))
		FINALLY (RETURN NIL))
      (MENU-UPDATE-MOUSE-CURSOR CURRENT-ROW)))
  NIL)

(DEFMETHOD (MENU :MOVE-CURSOR-FORWARD) (&OPTIONAL (MOVE-THE-MOUSE T))
  ;; CONTROL-F
  ;; Search forward from the current item through the remaining items to find one
  ;; which doesn't have a :NO-SELECT keyword.
  (MOVE-TO-NEAREST-MENU-ITEM)
  (LOOP NAMED OUTER-LOOP
	FOR CURRENT-ROW-NUMBER FROM CURRENT-ITEM-ROW BY 1
	FOR INITIAL-COLUMN-INDEX = (1+ CURRENT-ITEM-COLUMN) THEN 0
	FOR CURRENT-ROW = (AREF ROW-MAP CURRENT-ROW-NUMBER) THEN NEXT-ROW
	WHILE CURRENT-ROW
	FOR NEXT-ROW = (AREF ROW-MAP (1+ CURRENT-ROW-NUMBER))
	FOR CURRENT-ROW-LENGTH = (- (LENGTH CURRENT-ROW) (LENGTH NEXT-ROW))
	DO (LOOP FOR CURRENT-COLUMN-NUMBER FROM INITIAL-COLUMN-INDEX BELOW CURRENT-ROW-LENGTH
		 FOR THIS-ITEM = (NTH CURRENT-COLUMN-NUMBER CURRENT-ROW)
		 DO (PROGN
		      (WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P THIS-ITEM))
			(SETQ CURRENT-ITEM-ROW    CURRENT-ROW-NUMBER
			      CURRENT-ITEM-COLUMN CURRENT-COLUMN-NUMBER
			      CURRENT-ITEM        THIS-ITEM)
			(RETURN-FROM OUTER-LOOP T))))
	FINALLY (RETURN NIL))
  (WHEN MOVE-THE-MOUSE
    (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))
  NIL)

(DEFMETHOD (MENU :MOVE-CURSOR-LAST) ()
  ;; CONTROL-E
  (MOVE-TO-NEAREST-MENU-ITEM)
  (LET* ((CURRENT-ROW (AREF ROW-MAP CURRENT-ITEM-ROW))
	 (NEXT-ROW (AREF ROW-MAP (1+ CURRENT-ITEM-ROW)))
	 (CURRENT-ROW-LENGTH (- (LENGTH CURRENT-ROW) (LENGTH NEXT-ROW))))
    ;; Search backward through the items of the current row for an item which
    ;; doesn't have a :NO-SELECT keyword.
    (WHEN (LOOP FOR CURRENT-COLUMN-NUMBER FROM (1- CURRENT-ROW-LENGTH) DOWNTO 0
		FOR THIS-ITEM = (NTH CURRENT-COLUMN-NUMBER CURRENT-ROW)
		WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P THIS-ITEM))
		DO (PROGN
		     (SETQ CURRENT-ITEM-COLUMN CURRENT-COLUMN-NUMBER
			   CURRENT-ITEM        THIS-ITEM)
		     (RETURN T))
		FINALLY (RETURN NIL))
      (MENU-UPDATE-MOUSE-CURSOR CURRENT-ROW)))
  NIL)

(DEFMETHOD (MENU :MOVE-CURSOR-NEXT-LINE) ()
  ;; RETURN
  ;; Move the cursor to the beginning of the next line.
  (SEND SELF :MOVE-CURSOR-DOWN)
  (SEND SELF :MOVE-CURSOR-FIRST))

(DEFMETHOD (MENU :MOVE-CURSOR-PAGE-DOWN) ()
  ;; CONTROL-V
  (MOVE-TO-NEAREST-MENU-ITEM)
  (IF (> (+ CURRENT-ITEM-ROW SCREEN-ROWS) (1- TOTAL-ROWS))
      ;; The row of the item we want to move to isn't there.  Move to the last item.
      (SEND SELF :MOVE-CURSOR-BOTTOM)
      ;;ELSE
      (PROGN
	(INCF CURRENT-ITEM-ROW SCREEN-ROWS)
	(SEND SELF :MOVE-CURSOR-BACKWARD NIL)
	(SEND SELF :MOVE-CURSOR-FORWARD)))
  NIL)

(DEFMETHOD (MENU :MOVE-CURSOR-PAGE-UP) ()
  ;; META-V
  (MOVE-TO-NEAREST-MENU-ITEM)
  (IF (MINUSP (- CURRENT-ITEM-ROW SCREEN-ROWS))
      ;; The row of the item we want to move to isn't there.  Move to the first item.
      (SEND SELF :MOVE-CURSOR-TOP)
      ;;ELSE
      (PROGN
	(DECF CURRENT-ITEM-ROW SCREEN-ROWS)
	(SEND SELF :MOVE-CURSOR-FORWARD NIL)
	(SEND SELF :MOVE-CURSOR-BACKWARD)))
  NIL)

(DEFMETHOD (MENU :MOVE-CURSOR-TOP) ()
  ;; META-<
  ;; Search forward through the items to find one which doesn't have a :NO-SELECT
  ;; keyword.
  (WHEN (LOOP NAMED OUTER-LOOP
              FOR CURRENT-ROW-NUMBER FROM 0 BY 1
              FOR CURRENT-ROW = (AREF ROW-MAP CURRENT-ROW-NUMBER) THEN NEXT-ROW
              FOR NEXT-ROW = (AREF ROW-MAP (1+ CURRENT-ROW-NUMBER))
              WHILE CURRENT-ROW
              FOR CURRENT-ROW-LENGTH = (- (LENGTH CURRENT-ROW) (LENGTH NEXT-ROW))
              DO (LOOP FOR CURRENT-COLUMN-NUMBER FROM 0 BELOW CURRENT-ROW-LENGTH
                       FOR THIS-ITEM IN CURRENT-ROW
                       WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P THIS-ITEM))
                       DO (PROGN
                            (SETQ CURRENT-ITEM-ROW    CURRENT-ROW-NUMBER
                                  CURRENT-ITEM-COLUMN CURRENT-COLUMN-NUMBER
                                  CURRENT-ITEM        THIS-ITEM)
                            (RETURN-FROM OUTER-LOOP T)))
              FINALLY (RETURN NIL))
    (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))
  NIL)

#| Implementation note: The move cursor up and move cursor down methods are rather
complex.  They  have  a  complexity  which  Zmacs  doesn't have since there is a
possibility that a row being moved to does not have a selectable item in it.  In
this case, the  movement must  proceed until  a row  is found  which does have a
selectable item in  it.  If  no row  is found,  then the  movement operation  is
aborted, leaving  the  mouse  where  it  was  originally.   Note that these line
movement commands are  not the  same as  the corresponding  ones in  Zmacs.  The
difference is that  these line  movement commands  do not  remember the  initial
column of the first line movement.  The  column number is only remembered for  a
single line movement.  Given the  complex interaction of item  selection between
the mouse and the  keyboard, implementing this  `current column' memory  feature
would make complex code even more complex, with only a small gain in  usability.
|#

(DEFMETHOD (MENU :MOVE-CURSOR-UP) ()
  ;; CONTROL-P
  (MOVE-TO-NEAREST-MENU-ITEM)
  (WHEN (NOT (ZEROP CURRENT-ITEM-ROW))
    (LET* ((CURRENT-ROW  (AREF ROW-MAP CURRENT-ITEM-ROW))
           (OLD-COLUMN-NUMBER CURRENT-ITEM-COLUMN)
           (PREVIOUS-ROW (AREF ROW-MAP (1- CURRENT-ITEM-ROW)))
           (PREVIOUS-ROW-LENGTH (- (LENGTH PREVIOUS-ROW) (LENGTH CURRENT-ROW))))
      ;; Don't move before the first row.
      (DECF CURRENT-ITEM-ROW)
      (SETQ CURRENT-ROW         PREVIOUS-ROW
            CURRENT-ITEM-COLUMN (MIN PREVIOUS-ROW-LENGTH CURRENT-ITEM-COLUMN)
            CURRENT-ITEM        (NTH CURRENT-ITEM-COLUMN CURRENT-ROW))
      ;; If the item we just moved to has a :NO-SELECT keyword in it then we
      ;; must find an item which does not.  We do this by first trying to move
      ;; forward within this row to find one.  If we don't find one by the time
      ;; we reach the end of this row then we call the :MOVE-CURSOR-BACKWARD
      ;; method which might find an item on a previous row.  If that fails then
      ;; we move back to the item we were on originally.
      (IF (AND (ITEM-CONTAINS-NO-SELECT-P CURRENT-ITEM)
               (LOOP WITH NEXT-ROW = (AREF ROW-MAP (1+ CURRENT-ITEM-ROW))
                     WITH CURRENT-ROW-LENGTH = (- (LENGTH CURRENT-ROW) (LENGTH NEXT-ROW))
                     FOR CURRENT-COLUMN-NUMBER FROM (1+ CURRENT-ITEM-COLUMN) BELOW CURRENT-ROW-LENGTH
                     FOR THIS-ITEM = (NTH CURRENT-COLUMN-NUMBER CURRENT-ROW)
                     WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P THIS-ITEM))
                     DO (PROGN
                          (SETQ CURRENT-ITEM-COLUMN CURRENT-COLUMN-NUMBER
                                CURRENT-ITEM        THIS-ITEM)
                          (RETURN NIL))
                     FINALLY (RETURN T)))
          (PROGN
            (SEND SELF :MOVE-CURSOR-BACKWARD)
            (IF (ITEM-CONTAINS-NO-SELECT-P CURRENT-ITEM)
                ;; Didn't find a good item going backward.  Move back to where
                ;; we were originally.
                (PROGN
                  (INCF CURRENT-ITEM-ROW)
                  (SETQ CURRENT-ITEM-COLUMN OLD-COLUMN-NUMBER)
                  (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))
                ;;ELSE
                (WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P (NTH OLD-COLUMN-NUMBER (AREF ROW-MAP CURRENT-ITEM-ROW))))
                  ;; We moved to a row which has an item at the same column as the one we were in.
                  ;; Sometimes this will even be the same item.  Set the mouse cursor to the new item.
                  (SETQ CURRENT-ITEM-COLUMN OLD-COLUMN-NUMBER)
                  (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))))
          ;;ELSE
          (MENU-UPDATE-MOUSE-CURSOR CURRENT-ROW))))
  NIL)

(DEFMETHOD (MENU :MOVE-CURSOR-DOWN) ()
  ;; CONTROL-N
  (MOVE-TO-NEAREST-MENU-ITEM)
  ;; Don't move after the last row.
  (WHEN (< CURRENT-ITEM-ROW (1- TOTAL-ROWS))
    (LET* ((CURRENT-ROW  (AREF ROW-MAP CURRENT-ITEM-ROW))
           (OLD-COLUMN-NUMBER CURRENT-ITEM-COLUMN)
           (NEXT-ROW     (AREF ROW-MAP (1+ CURRENT-ITEM-ROW)))
           (NEXT-ROW-LENGTH (- (LENGTH NEXT-ROW)
                               (LENGTH (AREF ROW-MAP (+ CURRENT-ITEM-ROW 2))))))
      (INCF CURRENT-ITEM-ROW)
      (SETQ CURRENT-ROW         NEXT-ROW
            CURRENT-ITEM-COLUMN (MIN (1- NEXT-ROW-LENGTH) CURRENT-ITEM-COLUMN)
            CURRENT-ITEM        (NTH CURRENT-ITEM-COLUMN CURRENT-ROW))
      ;; If the item we just moved to has a :NO-SELECT keyword in it then we
      ;; must find an item which does not.  We do this by first trying to move
      ;; backward within this row to find one.  If we don't find one by the time
      ;; we reach the end of this row then we call the :MOVE-CURSOR-FORWARD
      ;; method which might find an item on a following row.  If that fails then
      ;; we move back to the item we were on originally.
      (IF (AND (ITEM-CONTAINS-NO-SELECT-P CURRENT-ITEM)
               (LOOP FOR CURRENT-COLUMN-NUMBER FROM (1- CURRENT-ITEM-COLUMN) DOWNTO 0
                     FOR THIS-ITEM = (NTH CURRENT-COLUMN-NUMBER CURRENT-ROW)
                     WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P THIS-ITEM))
                     DO (PROGN
                          (SETQ CURRENT-ITEM-COLUMN CURRENT-COLUMN-NUMBER
                                CURRENT-ITEM        THIS-ITEM)
                          (RETURN NIL))
                     FINALLY (RETURN T)))
          (PROGN
            ;; Didn't find a good item going backward on this row.  Try going forward.
            (SEND SELF :MOVE-CURSOR-FORWARD)
            (IF (ITEM-CONTAINS-NO-SELECT-P CURRENT-ITEM)
                ;; Didn't find a good item going forward.  Move back to where we
                ;; were originally.
                (PROGN
                  (DECF CURRENT-ITEM-ROW)
                  (SETQ CURRENT-ITEM-COLUMN OLD-COLUMN-NUMBER)
                  (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))
                ;;ELSE
                (WHEN (NOT (ITEM-CONTAINS-NO-SELECT-P (NTH OLD-COLUMN-NUMBER (AREF ROW-MAP CURRENT-ITEM-ROW))))
                  ;; We moved to a row which has an item at the same column as the one we were in.
                  ;; Sometimes this will even be the same item.  Set the mouse cursor to the new item.
                  (SETQ CURRENT-ITEM-COLUMN OLD-COLUMN-NUMBER)
                  (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))))
          ;;ELSE
          ;; The original movement moved to a good item.  Update the mouse accordingly.
          (MENU-UPDATE-MOUSE-CURSOR CURRENT-ROW))))
  NIL)
 
#| The following function is the easy-to-use interface to the MENU flavor.  The idea  is
to make full use of  the MENU flavor's initialization  options, but to provide  defaults
which allow the novice user to easily bring up a menu.  |#

#| Define  a  window  resource  which  allows  one  to specify various initialization options to
MENU.  |#
(DEFWINDOW-RESOURCE MENU (&OPTIONAL &KEY ABORT-ON-DEEXPOSE COLUMNS COMMAND-MENU
                                            DYNAMIC GEOMETRY HIGHLIGHTED-ITEMS HIGHLIGHTING
                                            IO-BUFFER ITEM-ALIGNMENT ITEM-LIST-POINTER MENU-MARGIN-CHOICES
					    MULTICOLUMN PERMANENT POP-UP SCROLLING-P SORT)
  :CONSTRUCTOR (MAKE-INSTANCE 'MENU :SUPERIOR SUPERIOR
                              :ABORT-ON-DEEXPOSE   ABORT-ON-DEEXPOSE
                              :COLUMNS             COLUMNS
                              :COMMAND-MENU        COMMAND-MENU
                              :DYNAMIC             DYNAMIC
                              :GEOMETRY            GEOMETRY
                              :HIGHLIGHTED-ITEMS   HIGHLIGHTED-ITEMS
                              :HIGHLIGHTING        HIGHLIGHTING
                              :IO-BUFFER           IO-BUFFER
                              :ITEM-ALIGNMENT      ITEM-ALIGNMENT
			      :ITEM-LIST-POINTER   ITEM-LIST-POINTER
                              :MENU-MARGIN-CHOICES MENU-MARGIN-CHOICES
                              :MULTICOLUMN         MULTICOLUMN
                              :PERMANENT           PERMANENT
                              :foreground-color    tv:*default-menu-foreground*
                              :background-color    tv:*default-menu-background*
                              :POP-UP              POP-UP
			      :SCROLLING-P         SCROLLING-P
                              :SORT                SORT)
  :INITIAL-COPIES 0
  :REUSABLE-WHEN :DEACTIVATED)

#| Replacement for MENU-CHOOSE.  This function is much more general than MENU-CHOOSE.  It allows
one to specify what kind of  menu to create.  By default  it creates the same kind  of menu that
MENU-CHOOSE does.  This  function is  slightly different  than MENU-CHOOSE  in that the optional
arguments are specified by keywords instead of by position.  |#

(DEFUN MENU-CHOOSE
       (ALIST
        &KEY
        (ABORT-ON-DEEXPOSE   NIL)
        (COLUMNS             NIL)
        (COMMAND-MENU        NIL)
        (DEFAULT-ITEM        NIL)
        (DYNAMIC             NIL)
        (GEOMETRY            NIL)
        (HIGHLIGHTED-ITEMS   NIL)
        (HIGHLIGHTING        NIL)
        (IO-BUFFER           NIL)
        (ITEM-ALIGNMENT      :LEFT)
        (LABEL               NIL)
        (MENU-MARGIN-CHOICES NIL)
        (MULTICOLUMN         NIL)
        (NEAR-MODE           '(:MOUSE))
        (PERMANENT           NIL)
        (POP-UP              T)
        (SORT                NIL)
	(foreground-color    tv:*default-menu-foreground*)
	(background-color    tv:*default-menu-background*)
	(label-color         tv:*default-menu-label-foreground*)
	(label-background    tv:*default-menu-label-background*)
	(SCROLLING-P         T         SCROLLING-P-SUPPLIED)
        (SUPERIOR            MOUSE-SHEET)
        &AUX (OSW SELECTED-WINDOW))
  "Let user choose an element of ALIST with a menu.

This is an `easy-to-use' function which interfaces with the MENU flavor.  From here
the caller can specify the often-used features of menus.  Letting all optional parameters
default causes a `pop-up' menu to be created which will disappear when the user moves the
mouse away.  Specifying other values for the optional parameters allows the caller to create
different kinds of menus.

ALIST			an ITEM-LIST for a menu.  For multi-column menus this will be the
			column-spec-list instead.  For dynamic menus this will be a form to be evaluated.
ABORT-ON-DEEXPOSE	when specified as T, if the menu becomes deexposed then the `abort' option
                        will be `clicked  on'.  If  there is  an `abort'  margin choice then that
                        will be used; otherwise the `abort' in the ALIST, if any will be used.
COMMAND-MENU 	when specified as T causes menu selections to be placed into an I/O buffer
			as a blip.  Default is NIL.
COLUMNS                 when specified is the number of columns in the menu.  Default is NIL which
			means that the number of columns is determined from the geometry.
DEFAULT-ITEM		an item that is EQ to an element of ALIST; the mouse will be positioned
			initially over that item.  Or it can be NIL.
DYNAMIC		when specified as T allows the menu ALIST to be dynamically updated.
			Defaults to NIL.  The ALIST is a form whose value can change.  After the change,
			the :update-item-list method must be called to cause the menu to be updated.
GEOMETRY		when specified indicates the layout and dimensions of the menu.  Defaults to NIL,
			which means that the layout is determined from the number of items present,
			or the dimensions of the superior if there are too many items.
HIGHLIGHTED-ITEMS	allows one to specify items which are initially highlighted.  HIGHLIGHTING
			when specified as T causes a selection to be highlighted, allows one
                        to select more than one item.  Defaults to NIL.
IO-BUFFER		the I/O buffer for command menus.
ITEM-ALIGNMENT	Specifies the alignment of items within a column.  Can be one of :LEFT,
			:CENTER or :RIGHT.  Defaults to :LEFT.
LABEL			the label of the menu, if not specified this is NIL.
MENU-MARGIN-CHOICES Margin choices to allow one to terminate a selection.  Defaults to NIL.
MULTICOLUMN		when specified as T causes the ALIST to be interpreted as a
                        column-spec-list to generate a multi-column menu.  Defaults to NIL.
NEAR-MODE		specifies how to decide where to put the menu; see
			EXPOSE-WINDOW-NEAR.  Defaults to (:MOUSE).
PERMANENT		when specified as T makes the menu `permanent'.  Defaults to NIL
                        which makes the menu `temporary'.
POP-UP			when specified as T the menu will be deexposed when the mouse
			moves out of it.  Defaults to T.
SCROLLING-P		when specified as NIL this will disable scrolling for the menu.  Defaults to T.
SORT			when specified this indicates the manner in which the menu items
                        should be ordered.  Allowable values are :ASCENDING :DESCENDING,
                        predicate (:ASCENDING key) (:DESCENDING key) (predicate key).
			Where `predicate' and `key' are as required by the sort function.  As  a
			shorthand, the user may specify T instead of :ASCENDING.
			Defaults to NIL which does not change the ordering of the menu items.
			Note that sorting the alist is a destructive operation.
SUPERIOR		The menu is made an inferior this.

If the user chooses an item, the values are:
	1) the value computed from that item, and
	2) the item itself (an element of ALIST).
Otherwise, NIL is returned."
  ;; Perform validity checking here.  The idea is to catch some menu combinations which may not
  ;; make sense.

  ;; In order to make a selection from a highlighting menu it must have margin choices, be a
  ;; command menu or a permanent menu.  If it has margin choices, then one can select one of
  ;; them to make the selection.  If it is a command menu then when one selects an item the blip
  ;; will be placed into the I/O buffer.  If it is permanent then the user is probably
  ;; specifically calling the method to find out which items were selected (i.e.
  ;; :HIGHLIGHTED-ITEMS or :HIGHLIGHTED-VALUES).
  (ASSERT (IF (OR HIGHLIGHTING HIGHLIGHTED-ITEMS) (OR MENU-MARGIN-CHOICES COMMAND-MENU PERMANENT) T)
          (HIGHLIGHTING HIGHLIGHTED-ITEMS MENU-MARGIN-CHOICES COMMAND-MENU PERMANENT)
          "A highlighting menu must have some way of completing a selection.  No manner is specified.")

  (ASSERT (NOT (AND MULTICOLUMN DYNAMIC)) ;; may 7-29-88
	  ()
	  "Both MULTICOLUMN and DYNAMIC can not be specified.")
  ;; Currently when a dynamic sends the :UPDATE-ITEM-LIST method it causes the window to become
  ;; temporarily deexposed, causing a pop-up window to stay deexposed.  This is probably a bug,
  ;; but the old code worked this way too.  The best solution is to fix it (which is not a
  ;; trivial task.  It probably has something to do with mouse ownership changing.).
  ;;(ASSERT (IF (AND DYNAMIC POP-UP) NIL T)
  ;;        (DYNAMIC POP-UP)
  ;;        "A dynamic menu currently can not also be a pop-up menu.")

  (ASSERT (IF (AND MULTICOLUMN COLUMNS)
              (PROGN
                (IF (= (LENGTH ALIST) COLUMNS)
                    T
                    NIL))
              T)
          (COLUMNS)
          "Number of columns specified in :COLUMNS does not match the number of columns in the item list.")

  ;; If the user specifies a command menu then it must also have an io buffer, otherwise there is not
  ;; place to put the chosen item blip.
  (ASSERT (IF COMMAND-MENU IO-BUFFER T)
          (COMMAND-MENU IO-BUFFER)
          "A command menu must have an I/O buffer.")


  ;; We don't bother to check to see if the user specified HIGHLIGHTED-ITEMS but not
  ;; HIGHLIGHTING.  The init method for MENU corrects this error for us.

  ;; A permanent menu with margin choices sounds kind of silly, but we might as well allow it.

  (ASSERT (IF (AND POP-UP PERMANENT) NIL T)
	  (POP-UP PERMANENT)
	  "A menu cannot be both permanent and pop-up.")

  ;; The following code is somewhat contorted.  The idea is that if
  ;; menu code doesn't execute all the way to the end there must have
  ;; been some kind of error.  In this case, the resource is cleared
  ;; so that is will not affect a subsequent execution.
  (LET (MENU                                    ; The MENU resource
        MENU-CHOOSE-WAS-ABORTED                 ; T when the user aborts, NIL otherwise
        VALUE-CHOSEN                            ; Value chosen by user, or NIL
        LAST-ITEM)                              ; Last item chosen by user, or NIL
    (UNWIND-PROTECT
        (PROGN
          (SETQ MENU (ALLOCATE-RESOURCE 'MENU
                                        :SUPERIOR            SUPERIOR
                                        :ABORT-ON-DEEXPOSE   ABORT-ON-DEEXPOSE
                                        :COLUMNS             COLUMNS
                                        :COMMAND-MENU        COMMAND-MENU
                                        :DYNAMIC             DYNAMIC
                                        :GEOMETRY            GEOMETRY
                                        :HIGHLIGHTING        HIGHLIGHTING
                                        :IO-BUFFER           IO-BUFFER
                                        :ITEM-ALIGNMENT      ITEM-ALIGNMENT
					:ITEM-LIST-POINTER   (IF DYNAMIC ALIST NIL)
                                        :MENU-MARGIN-CHOICES MENU-MARGIN-CHOICES
                                        :MULTICOLUMN         MULTICOLUMN
                                        :PERMANENT           PERMANENT
                                        :POP-UP              POP-UP
                                        :SCROLLING-P         SCROLLING-P
                                        :SORT                SORT))
          (SETQ MENU-CHOOSE-WAS-ABORTED MENU)
          (SEND MENU :SET-HIGHLIGHTED-ITEMS HIGHLIGHTED-ITEMS)
          (DELAYING-COMPUTE-GEOMETRY 
            (SEND MENU :SET-LABEL LABEL))
	  (IF MULTICOLUMN
              (SEND MENU :SET-COLUMN-SPEC-LIST ALIST)
              ;;ELSE
	      (IF (NOT DYNAMIC)
		  (SEND MENU :SET-ITEM-LIST ALIST)
		  ;;;; HACK ALERT
		  ;;;; ------ this is not the no-op it seems 
		  ;;;; some instance variables and the geometry need
		  ;;;; to be reset after label processing ------
		  (send menu :set-item-list (send menu :item-list))))
          (SEND MENU :SET-LAST-ITEM DEFAULT-ITEM)
          (CLEAR-INPUT MENU)

	  (WHEN (NULL SCROLLING-P-SUPPLIED)
            ;; If the user didn't say anything about scrolling then only turn on scrolling when
            ;; there isn't enough room to display all of the items.
            (SEND MENU :DECIDE-IF-SCROLLING-NECESSARY))

	  (IF (color-system-p menu)
	    (progn
 	    (SEND menu :set-foreground-color foreground-color)
	    (SEND menu :set-background-color background-color)
	    (send menu :set-label-color label-color)
	    (send menu :set-label-background label-background)
	    )
          )
	  
          (EXPOSE-WINDOW-NEAR MENU NEAR-MODE)
          (AND DEFAULT-ITEM
               (NOT (MEMBER (CAR NEAR-MODE) '(:MOUSE :POINT) :TEST #'EQ))
               (MULTIPLE-VALUE-BIND (X Y) (SEND MENU :ITEM-CURSORPOS DEFAULT-ITEM)
                 (AND X Y
                      (SEND MENU :SET-MOUSE-POSITION
                            (+ X (SHEET-INSIDE-LEFT MENU))
                            (+ Y (SHEET-INSIDE-TOP MENU))))))
          (SETQ VALUE-CHOSEN (SEND MENU :CHOOSE)
                LAST-ITEM    (SEND MENU :LAST-ITEM))
          (SETQ MENU-CHOOSE-WAS-ABORTED NIL))
      (AND OSW (SEND OSW :SELECT NIL))
      (WHEN MENU
        (DEALLOCATE-RESOURCE 'MENU MENU))
      (WHEN MENU-CHOOSE-WAS-ABORTED
        (CLEAR-RESOURCE 'MENU MENU)))
    (VALUES VALUE-CHOSEN LAST-ITEM)))

#| Note: the MULTICOLUMN-MENU function exists because there is a
similar one for the old window system.  There is only one difference
between this function and the MULTICOLUMN-MENU-CHOOSE function.  This
function has the optional arguments being specified as keywords.  |#
(DEFUN MULTICOLUMN-MENU-CHOOSE
       (COLUMN-SPEC-LIST
        &KEY
        (LABEL NIL) (NEAR-MODE '(:MOUSE)) (DEFAULT-ITEM NIL)
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
  (MENU-CHOOSE COLUMN-SPEC-LIST :MULTICOLUMN T
                       :LABEL LABEL :NEAR-MODE NEAR-MODE
                       :DEFAULT-ITEM DEFAULT-ITEM :SUPERIOR SUPERIOR))


#| Note: the MULTIPLE-MENU-CHOOSE function exists because there is a similar
one for the old window system.  There are three differences between this function
and the MULTIPLE-MENU-CHOOSE function.  This function has the optional arguments
being specified as keywords.  Another argument is allowed `menu-margin-choices' to
allow the caller to specify something else as a margin choice.  Also, instead of
returning T as the second value when the user clicks the `Do It' box, the value
returned is non-NIL.  |#
(DEFUN MULTIPLE-MENU-CHOOSE
       (ALIST
        &KEY
        (LABEL NIL) (NEAR-MODE '(:MOUSE)) (HIGHLIGHTED-ITEMS NIL)
        (MENU-MARGIN-CHOICES '(:DOIT)) (SUPERIOR MOUSE-SHEET)
	(foreground-color tv:*default-menu-foreground*)(background-color tv:*default-menu-background*)
	(label-color tv:*default-menu-label-foreground*)(label-background tv:*default-menu-label-background*))
  "Let user choose some set of elements of ALIST with a multiple menu.
ALIST looks like an ITEM-LIST for a menu.
LABEL is used as the label of the menu, if not NIL.
NEAR-MODE specifies how to decide where to put the menu; see EXPOSE-WINDOW-NEAR.
HIGHLIGHTED-ITEMS is a set of elements of ALIST;
 these will form the initially chosen subset, which the user can modify.
The menu is made an inferior of SUPERIOR.

If the user exits by clicking on the Do It box, there are two values:
1) a list of the values from executing the selected set of items, and
2) a non-NIL value.
If the user moves the mouse away from the menu,
NIL is returned for both values."
  (MENU-CHOOSE ALIST :LABEL LABEL :NEAR-MODE NEAR-MODE :HIGHLIGHTED-ITEMS HIGHLIGHTED-ITEMS
                       :SUPERIOR SUPERIOR :HIGHLIGHTING T
                       :MENU-MARGIN-CHOICES MENU-MARGIN-CHOICES
		       :foreground-color foreground-color :background-color background-color
		       :label-color label-color :label-background label-background ))


;;; The following prompting windows are used by various color support
;;; routines.  It works in conjunction witht he system routine
;;; PROMPT-AND-READ.  PHM

(DEFFLAVOR pop-up-prompt-and-read-window ()
           (w:temporary-window-mixin
	    w:borders-mixin
	    w:label-mixin
	    w:select-mixin
	    w:stream-mixin
	    w:minimum-window)
  (:default-init-plist
    :font-map '(medfnt)
    :label nil
    :borders 5
    :deexposed-typeout-action :permit
    ))

(defwindow-resource prompting-window ()
  :make-window (pop-up-prompt-and-read-window
		 :font-map '(medfnt)
		 :label nil
		 :borders 5
		 :deexposed-typeout-action :permit)
  :initial-copies 0
  ) 



;;; Added an unwind-protect to this so that it goes away if it gets aborted.
(DEFUN pop-up-prompt-and-read (&optional (option :number)
			       (expose-near '(:mouse))
			       (width :default)
			       (format-string (format nil "Enter a ~a: " option))
			       &rest format-args)
  (using-resource (w prompting-window)
  (LET ((*query-io* w)
	(str (APPLY #'FORMAT nil (CONS format-string format-args)))
	val)
    (WHEN (eq width :default) (setq width (max 40 (+ (length str) 20))))
    (SEND w :set-size-in-characters width 4)  
    (send w :expose-near expose-near)
    (window-call (w :deactivate)
      (loop
	(SEND w :clear-screen)
	(condition-case (the-error)
	    (return (SETQ val (si:PROMPT-AND-READ-INTERNAL option '(:DONT-HANDLE-ERRORS t) str)))
	  (error (send the-error :report *query-io*)
		 (format *query-io* "~%Press space to acknowledge and retry")
		 (send *query-io* :tyi)))))
    val)))


(DEFUN pop-up-read-number (&optional (low-range 0.) (hi-range 255.))
  (LET (read-num)
    (LOOP
      (SETQ read-num (pop-up-prompt-and-read :NUMBER
					     '(:mouse)
					     :default
                                             "Enter a number from ~d to ~d:"
                                             low-range hi-range))
      (IF (<= low-range read-num hi-range)
        (RETURN read-num)
        (BEEP)))))