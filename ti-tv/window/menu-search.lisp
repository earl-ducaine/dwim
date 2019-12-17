;;; -*- Mode:Common-Lisp; Package:W; Base:10.; Fonts:CPTFONT,HL12B,HL12BI -*-

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
;;;  Date        Author	Description
;;; -------------------------------------------------------------------------------------
;;;   4/12/87  TWE	Fix problems with menu search windows remaining activated.
;;;   2/04/87  TWE	Included the mixin NOT-EXTERNALLY-SELECTABLE-MIXIN so that the traveling search
;;;			window won't show up in the system menu Select menu.
;;; 10/21/86   TWE	Removed the hack which modified the MENU-DEFAULT-COMMAND-CHARACTERS table
;;;			to insert the searching commands.  The commands are now located in the MENU file.
;;;			Also changed all DEFVARs to DEFCONSTANTs or DEFPARAMETERs, as appropriate.
;;; 10/09/86   TWE 	Change make-array of element type :character to use :string-char instead.  This
;;;			makes the search-string instance variable look more like a normal string instead
;;;			of an array.
;;; 10/03/86   TWE 	Change a use of TYPEP to TYPE-OF to be more in tune with Common Lisp.

#|

This file implements the searching facility for menus.  

The following code implements incremental search for menus.  The idea is to make
this do exactly what Zmacs does in every case.  The only four exceptions to this
are:

  (1) the way the search string is presented to the user,
  (2) the fact that the searching does not take place in a separate process and
  (3) the ABORT and END keys exit searching (like Zmacs) but also  exit the menu
      not selecting  an  item,  or  not  selecting an item, respectively (unlike
      Zmacs).
  (4) when the user presses the help key while searching a help display is shown.

Other than  that  incremental  searching  for  menus is identical to incremental
searching in Zmacs.  A minor  extension had been made  to allow the user  to use
the UNDO command to exit the search  mode but still remember the search  string.
This differs from the CONTROL-G in that the search string is remembered.


There are  two  search  character  display  modes  implemented here: a traveling
search window and a fake minibuffer  window.  These display modes differ  in how
they present the search string for the user.  The traveling search window  moves
the mouse cursor to the matching menu item  and then pops up a window near  that
item in such a manner as to try to not obscure any other interesting menu items.
The fake minibuffer  window attempts  to simulate  Zmacs by  popping up a window
that looks like a Zmacs minibuffer in terms of position and height.  If the fake
minibuffer window  does  not  fit  below  the  menu  then  it will be positioned
elsewhere.

|#

(DEFPARAMETER MINIMUM-MENU-SEARCH-WINDOW-WIDTH 20.
  "Minimum number of characters for a search window.  If the menu is larger than this then the menu's width is used.")

(DEFPARAMETER TRAVELING-SEARCH-WINDOW-REVERSE-VIDEO? T)

;;;(DEFPARAMETER *DEFAULT-MENU-SEARCH-DISPLAY-MODE* :traveling-search)
(DEFPARAMETER *DEFAULT-MENU-SEARCH-DISPLAY-MODE* :fake-minibuffer
  "Flag which indicates the display mechanism which is used to show the search
 string.  Currently there are only two modes -- :traveling-search and :fake-minibuffer.")

(DEFFLAVOR FAKE-MINIBUFFER-WINDOW ()
	   (TEMPORARY-WINDOW-MIXIN NOT-EXTERNALLY-SELECTABLE-MIXIN WINDOW)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL
    :BORDERS 1
    :INSIDE-WIDTH 1
    :CHARACTER-HEIGHT 2
    :MORE-P NIL
    :SAVE-BITS T
    :FONT-MAP (LIST :DEFAULT)
    :LABEL NIL))

(DEFWINDOW-RESOURCE FAKE-MINIBUFFER-WINDOW ()
  :MAKE-WINDOW (FAKE-MINIBUFFER-WINDOW)
  :REUSABLE-WHEN :DEACTIVATED
  :INITIAL-COPIES 0)

(DEFFLAVOR TRAVELING-SEARCH-WINDOW ()
	   (TEMPORARY-WINDOW-MIXIN NOT-EXTERNALLY-SELECTABLE-MIXIN WINDOW)
  (:DEFAULT-INIT-PLIST :BLINKER-P NIL
    :BORDERS NIL
    :INSIDE-WIDTH 1
    :CHARACTER-HEIGHT 1
    :SAVE-BITS T
    :FONT-MAP (LIST :DEFAULT)
    :LABEL NIL))

(DEFWINDOW-RESOURCE TRAVELING-SEARCH-WINDOW ()
  :MAKE-WINDOW (TRAVELING-SEARCH-WINDOW
		 :REVERSE-VIDEO-P TRAVELING-SEARCH-WINDOW-REVERSE-VIDEO?)
  :REUSABLE-WHEN :DEACTIVATED
  :INITIAL-COPIES 0)

;;; Allocate a slot in the search window resources so that they don't take so
;;; long to come up the first time.  We need to wait until default-screen is
;;; set up to do this though.
(DEFUN PREALLOCATE-SEARCH-WINDOW-RESOURCES ()
  (DEALLOCATE-RESOURCE 'FAKE-MINIBUFFER-WINDOW  (ALLOCATE-RESOURCE 'FAKE-MINIBUFFER-WINDOW  DEFAULT-SCREEN))
  (DEALLOCATE-RESOURCE 'TRAVELING-SEARCH-WINDOW (ALLOCATE-RESOURCE 'TRAVELING-SEARCH-WINDOW DEFAULT-SCREEN)))


(DEFCONSTANT WINDOW-POSITIONING-PREFERENCE-LIST
        '((:ABOVE (:ABOVE :LEFT  :RIGHT :BELOW)) (:BELOW (:BELOW :RIGHT :LEFT  :ABOVE))
          (:LEFT  (:LEFT  :BELOW :ABOVE :RIGHT)) (:RIGHT (:RIGHT :ABOVE :BELOW :LEFT))))


;;; Position the window next to the rectangle.  Try the suggested position
;;; first.  If that fails then use WINDOW-POSITIONING-PREFERENCE-LIST to try
;;; some other alternatives.  This is different from the :expose-near method in
;;; that alignment is done, and other alternatives are attempted depending on
;;; the suggested position.  The :expose-near method does not allow a suggested
;;; position.  Perhaps this should be added.
(DEFUN POSITION-WINDOW-NEXT-TO-RECTANGLE (WINDOW POSITION LEFT TOP RIGHT BOTTOM)
  "Move WINDOW near the specified rectangle.  Try hard to put it at POSITION.
POSITION is a keyword which indicates where to put the window.
	It can be one of :ABOVE, :BELOW, :LEFT or :RIGHT or a
	list of any these keywords.  If position is a single keyword
	and that position fails then the possibilities in the
	window-positioning-preference-list are also tried.
LEFT TOP RIGHT BOTTOM specify a rectangle using the inside coordinates
	of WINDOW's superior."
  (LET* (WINDOW-WIDTH WINDOW-HEIGHT
         (SUPERIOR (SHEET-SUPERIOR WINDOW))
         ;; The amount of room on a particular side.
         (IT-FITS NIL)
         (SUPERIOR-WIDTH  (SHEET-INSIDE-WIDTH  SUPERIOR))
         (SUPERIOR-HEIGHT (SHEET-INSIDE-HEIGHT SUPERIOR))
	 (VSP (MAX 1 (SEND SUPERIOR :VSP)))
         ;; Compute the amount of room around the rectangle.  All values use
         ;; inside coordinates.
         (ROOM-LEFT   LEFT)
         (ROOM-TOP    TOP )
         (ROOM-RIGHT  (- SUPERIOR-WIDTH  RIGHT))
         (ROOM-BOTTOM (- SUPERIOR-HEIGHT BOTTOM))
	 NEW-WINDOW-X NEW-WINDOW-Y)
  (MULTIPLE-VALUE-SETQ (WINDOW-WIDTH WINDOW-HEIGHT)
    (SEND WINDOW :SIZE))
  ;; Add in some vertical spacing to separate the window from the rectangle.
  (INCF WINDOW-HEIGHT VSP)
  (INCF WINDOW-WIDTH  VSP)
  ;; Try each possibility in the preference list.  The first element in the
  ;; preference list is the same as the position argument.
  (LOOP FOR POSSIBLE-POSITION IN (IF (LISTP POSITION)
				     POSITION
				     ;;ELSE
				     (CADR (ASSOC POSITION WINDOW-POSITIONING-PREFERENCE-LIST :TEST #'EQ)))
        DO 
        (CASE POSSIBLE-POSITION
              (:ABOVE (WHEN (<= WINDOW-HEIGHT ROOM-TOP)
                        ;; The window fits above the rectangle.
                        (SETQ IT-FITS :ABOVE
                              NEW-WINDOW-Y (- TOP WINDOW-HEIGHT)
                              NEW-WINDOW-X (COND ((<= (+ LEFT WINDOW-WIDTH) SUPERIOR-WIDTH)
						  ;; Align the window with the left boundary of the rectangle.
						  ;;   .-----------.
						  ;;   | WINDOW    |
						  ;;   |           |
						  ;;   |           |
						  ;;   |___________|
						  ;;   .----.
						  ;;   |Rect|
						  ;;   |____|
						  LEFT)
						 ((NOT (MINUSP (- RIGHT WINDOW-WIDTH)))
						  ;; Align the window with the right boundary of the rectangle.
						  ;;   .-----------.
						  ;;   | WINDOW    |
						  ;;   |           |
						  ;;   |           |
						  ;;   |___________|
						  ;;          .----.
						  ;;          |Rect|
						  ;;          |____|
						  (+ (- RIGHT WINDOW-WIDTH) VSP))
						 (T
						  ;; The window was too big to fit on either boundary of the rectangle.
						  ;; Simply put the window in the superior.
						  0)))))
              (:LEFT (WHEN (<= WINDOW-WIDTH ROOM-LEFT)
                       ;; The window fits to the left of the rectangle.
                       (SETQ IT-FITS :LEFT
                             NEW-WINDOW-X (- LEFT WINDOW-WIDTH)
                             NEW-WINDOW-Y (COND ((<= (+ TOP WINDOW-HEIGHT) SUPERIOR-HEIGHT)
						 ;; Align the window with the top boundary of the rectangle.
						 ;;   .-----------.  .----.
						 ;;   | WINDOW    |  |Rect|
						 ;;   |           |  |____|
						 ;;   |           |
						 ;;   |___________|
						 TOP)
						((NOT (MINUSP (- BOTTOM WINDOW-HEIGHT)))
						 ;; Align the window with the bottom boundary of the rectangle.
						 ;;   .-----------.
						 ;;   | WINDOW    |
						 ;;   |           |  .----.
						 ;;   |           |  |Rect|
						 ;;   |___________|  |____|
						 (+ (- BOTTOM WINDOW-HEIGHT) VSP))
						(T
						 ;; The window was too big to fit on either boundary of the rectangle.
						 ;; Simply put it in the superior.
						 0)))))
              (:RIGHT (WHEN (<= WINDOW-WIDTH ROOM-RIGHT)
                        ;; The window fits to the right of the rectangle.
                        (SETQ IT-FITS :RIGHT
                              NEW-WINDOW-X (+ RIGHT VSP)
                              NEW-WINDOW-Y (COND ((<= (+ TOP WINDOW-HEIGHT) SUPERIOR-HEIGHT)
						  ;; Align the window with the top boundary of the rectangle.
						  ;;     .----.  .-----------.
						  ;;     |Rect|  | WINDOW    |
						  ;;     |____|  |           |
						  ;;             |           |
						  ;;             |___________|
						  TOP)
						 ((NOT (MINUSP (- BOTTOM WINDOW-HEIGHT)))
						  ;; Align the window with the bottom boundary of the rectangle.
						  ;;             .-----------.
						  ;;             | WINDOW    |
						  ;;     .----.  |           |
						  ;;     |Rect|  |           |
						  ;;     |____|  |___________|
						  (+ (- BOTTOM WINDOW-HEIGHT) VSP))
						 (T
						  ;; The window was too big to fit on either boundary of the rectangle.
						  ;; Simply put it in the superior.
						  0)))))
              (:BELOW (WHEN (<= WINDOW-HEIGHT ROOM-BOTTOM)
                        ;; The window fits below the rectangle.
                        (SETQ IT-FITS :BELOW
                              NEW-WINDOW-Y (+ BOTTOM VSP)
                              NEW-WINDOW-X (COND ((<= (+ LEFT WINDOW-WIDTH) SUPERIOR-WIDTH)
						  ;; Align the window with the left boundary of the rectangle.
						  ;;   .----.
						  ;;   |Rect|
						  ;;   |____|
						  ;;   .-----------.
						  ;;   | WINDOW    |
						  ;;   |           |
						  ;;   |           |
						  ;;   |___________|
						  LEFT)
						 ((NOT (MINUSP (- RIGHT WINDOW-WIDTH)))
						  ;; Align the window with the right boundary of the rectangle.
						  ;;          .----.
						  ;;          |Rect|
						  ;;          |____|
						  ;;   .-----------.
						  ;;   | WINDOW    |
						  ;;   |           |
						  ;;   |           |
						  ;;   |___________|
						  (+ (- RIGHT WINDOW-WIDTH) VSP))
						 (T
						  ;; The window was too big to fit on either boundary of the rectangle.
						  ;; Simply put it in the superior.
						  0))))))
        UNTIL IT-FITS)
  (WHEN (NOT IT-FITS)
    (IF (OR (EQ POSITION :BELOW) (EQ POSITION :RIGHT))
	;; Deep trouble, the window doesn't fit in a good place.  Just put it at
        ;; the top.  Make sure that the bottom of the window does not cover up
        ;; the rectangle.  The idea is that since the ideal position was to put
        ;; the window below the rectangle and given that this is impossible, try
        ;; to put the window as far above the rectangle as possible to keep it
        ;; from covering anything up.
	(SETQ NEW-WINDOW-X 0
	      NEW-WINDOW-Y 0))
	;;ELSE :ABOVE or :LEFT
	;; Deep trouble, the window doesn't fit in a good place.  Just put it at
        ;; the bottom.  Make sure that the top of the window does not cover up
        ;; the rectangle.
        (SETQ NEW-WINDOW-X 0
              NEW-WINDOW-Y (MAX BOTTOM (- SUPERIOR-HEIGHT WINDOW-HEIGHT))))
  ;; Convert the inside coordinates to outside coordinates for :set-position
  (SETQ NEW-WINDOW-X (+ NEW-WINDOW-X (SHEET-INSIDE-LEFT SUPERIOR))
        NEW-WINDOW-Y (+ NEW-WINDOW-Y (SHEET-INSIDE-TOP  SUPERIOR)))
  (SEND WINDOW :SET-POSITION NEW-WINDOW-X NEW-WINDOW-Y)
  (SHEET-WITHIN-SHEET-P WINDOW SUPERIOR)))

;;; State table for incremental search.  The idea is to do exactly what Zmacs
;;; does in every case.  The only known difference between what Zmacs does and
;;; what is done here is that Zmacs puts the searching in a separate process
;;; which runs independently of the redisplay.  This is not done for menus since
;;; it usually isn't a problem.  For Zmacs one can have a large buffer which may
;;; take a long time to search.  Menus normally aren't long enough for the
;;; search time to be noticable.
(DEFCONSTANT
  MENU-SEARCH-STATE-TABLE
  (MAKE-ARRAY
    '(4 9)
    :INITIAL-CONTENTS
    '(
      ;;    |             |           |          |            |         |          |       |PRINTABLE | ANYTHING
      ;;    | CONTROL-S   | CONTROL-R |CONTROL-G |   UNDO     |CONTROL-Q|  ESCAPE  |RUBOUT |CHARACTERS| ELSE
      ;;  --+-------------+-----------+----------+------------+---------+----------+-------+----------+---------
      #| 1 |# ((2 (J A  )) (4 (K A  )) ( 0   NIL) ( 0     NIL) (0   NIL) ( 0   NIL) (0 NIL) (0   NIL)  ( 0  NIL))
      #| 2 |# ((3 (J B C)) (4 (K I  )) (-1 (F H)) (-1 (F G H)) (3 (M C)) (-1 (F G)) (2 (L)) (3 (E C))  (-1 (F G)))
      #| 3 |# ((3 (J I D)) (3 (K I D)) (-1 (F H)) (-1 (F G H)) (3 (M C)) (-1 (F G)) (3 (L)) (3 (E C))  (-1 (F G)))
      #| 4 |# ((3 (J I  )) (3 (K B C)) (-1 (F H)) (-1 (F G H)) (3 (M C)) (-1 (F G)) (4 (L)) (3 (E C))  (-1 (F G))))))

;;; The state machine is controlled by the current state and the character read
;;; as input.  The current state is a row in the state-table and the character
;;; read is a column.  To determine what is done, the indexing is performed
;;; using the current state and the character read to retrieve a component of
;;; this array.  The CAR will be the new state and the CADR is a list of
;;; indicators which tell what is to be done.  The encoding of these indicators
;;; is shown below.  See the contents of ACTION-TO-FUNCTION-MAP for a listing of
;;; the functions. 
;;;
;;; The first row in the state-table is entered initially.  The only characters
;;; which exists for this row are CONTROL-S and CONTROL-R. 
;;;
;;; The second and fourth rows are transitionary states for CONTROL-S and
;;; CONTROL-R.  These are necessary in case the user presses a sequence like:
;;; CONTROL-R CONTROL-S CONTROL-R CONTROL-S CONTROL-R.  All that this sequence
;;; should do is to change the search prompt to indicate the new search
;;; direction. 
;;;
;;; The third row does most of the searching.  The current state remains on this
;;; row until an exit condition is encountered. 
;;;
;;; Note that CONTROL-Q is not handled by using the state table.  Since this is
;;; a very simple case, it is handled by a special indicator instead, which
;;; reads the next character literally. 
;;;
;;; The HELP key is also a special case.  This is checked for every time a
;;; character is read.  If the user presses the HELP key then a help display is
;;; presented and another character is read. 
;;;
;;; A `next state' of 0 indicates an unreachable entry in the state table.  A
;;; next state of -1 is the termination condition.  At this point the search
;;; command returns to its caller. 
;;;
;;; (A) Prepare for an initial search.  Set the search string to "". 
;;;     Set stack to NIL.  Remember the current position.  Display an empty
;;;     search string for the user to see that searching is in effect. 
;;;
;;; (B) Load search string with the previous search argument.  push
;;;     ((length new-search-string) position direction) onto stack. 
;;;
;;; (C) Search for a match.  When it fails, beep at the user. 
;;;
;;; (D) Search for another match.  Like (C) only this one looks further on for
;;;     the next match, rather than considering the current match. 
;;;
;;; (E) Append this character to the search string.  push (1 position
;;;     direction) onto stack. 
;;;
;;; (F) Stop searching. 
;;;
;;; (G) Save search argument for future reference (even if it is empty). 
;;;
;;; (H) Return to previously saved position (see (a)). 
;;;
;;; (I) push (0 position direction) onto stack. 
;;;
;;; (J) Set the search direction to FORWARD. 
;;;
;;; (K) Set the search direction to BACKWARD. 
;;;
;;; (L) IF stack is empty THEN
;;;       beep at user
;;;     ELSE
;;;       pop (character-count position direction) from stack. 
;;;       remove that many characters from the search string. 
;;;       position within the item according to direction. 
;;;     ENDIF
;;;
;;; (M) Read the next character literally and perform (E) with that character.




;;; The following define the columns of the state table.  Note that END and ABORT are not handled
;;; by this mechanism.  This was done intentionally.  By default, characters that are not handled
;;; will cause the searching to stop and that command to be executed.  Since END and ABORT are not
;;; handled, they will cause the searching to stop and then they will be executed; END causing the
;;; current item to be selected; ABORT causing no item to be selected.

;;; Map single characters to column numbers.
(DEFCONSTANT MENU-SEARCHING-COLUMN-MAP '((#\CONTROL-S 0) (#\CONTROL-R 1) (#\CONTROL-G 2) (#\UNDO 3)
                                         (#\CONTROL-Q 4)
					 (#\ESCAPE 5)
					 (#\RUBOUT 6)))

;;; Define the column number that matches anything that is not handled elsewhere.
(DEFCONSTANT SEARCH-COLUMN-FOUND-ANYTHING 8)

(DEFUN MAP-SEARCH-CHARACTER (CH)
  "Map a character into a column of the state table."
  (OR (CADR (ASSOC CH MENU-SEARCHING-COLUMN-MAP :TEST #'CHAR=))
      (IF (GRAPHIC-CHAR-P CH)
	  7
	  ;;ELSE
	  SEARCH-COLUMN-FOUND-ANYTHING)))

;;; The search stack is used to remember the sequence of searching commands.
;;; The reason this is necessary is to properly implement the rubout command.
;;; We need to remember everything the user entered so that we can rub it out
;;; when requested.  The general format of stack elements is:
;;;
;;;        (length position direction)
;;;
;;; Where length is a count of the number of new characters which were just
;;; added to the search string.  Position is the location of the item when this
;;; stack element was added.  The position is the search-item-index, the
;;; current-item, and the current-item-row and current-item-column.  The
;;; direction is the direction of the search, which may be either :forward or
;;; :backward.
(DEFUN SEARCH-PUSH-STACK (STRING)
  "Push onto the stack: the length of the new part of the search
string, the current direction and the current position."
  (DECLARE (:SELF-FLAVOR MENU))
  (PUSH (LIST (LENGTH STRING) SEARCH-DIRECTION SEARCH-ITEM-INDEX CURRENT-ITEM CURRENT-ITEM-ROW CURRENT-ITEM-COLUMN)
        SEARCH-STACK))

(DEFUN SEARCH-INITIALIZE (IGNORE)                     ; (A)
  (DECLARE (:SELF-FLAVOR MENU))
  (MOVE-TO-NEAREST-MENU-ITEM)
  ;; The reason for making such a special array for the search string is so that
  ;; we can append new characters using the vector-push-extend function.
  (SETQ SEARCH-STRING     (MAKE-ARRAY 0 :ELEMENT-TYPE :STRING-CHAR :FILL-POINTER 0)
        SEARCH-STACK      NIL
	SEARCH-WINDOW     NIL
	SEARCH-ITEM-INDEX (IF (EQ SEARCH-DIRECTION :FORWARD)
			      0
			      ;;ELSE
			      (LENGTH (MENU-ITEM-STRING CURRENT-ITEM)))
        SEARCH-OLD-CURRENT-ITEM        CURRENT-ITEM
        SEARCH-OLD-CURRENT-ITEM-ROW    CURRENT-ITEM-ROW
        SEARCH-OLD-CURRENT-ITEM-COLUMN CURRENT-ITEM-COLUMN)
  (DISPLAY-SEARCH-STRING))
  
(DEFUN SEARCH-RESEARCH-OLD-ARG (IGNORE)               ; (B)
  "Search for a string which was searched for earlier."
  (DECLARE (:SELF-FLAVOR MENU))
  (SETQ SEARCH-STRING SEARCH-OLD-SEARCH-STRING)
  (SEARCH-PUSH-STACK SEARCH-OLD-SEARCH-STRING))

;;; The following are prompts for searching.  The abbreviated version is used in
;;; case the normal search prompt will not fit in the search string display window.
(DEFPARAMETER *MENU-SEARCH-FORWARD-PROMPTS*  `(("I-Search: ")        (,(INT-CHAR MOUSE-GLYPH-BLOCK-DOWN-ARROW) ,FONTS:MOUSE)))
(DEFPARAMETER *MENU-SEARCH-BACKWARD-PROMPTS* `(("Reverse I-Search: ") (,(INT-CHAR MOUSE-GLYPH-BLOCK-UP-ARROW) ,FONTS:MOUSE)))

(DEFUN DISPLAY-SEARCH-STRING ()
  (DECLARE (:SELF-FLAVOR MENU))
  ;; DOCUMENTATION ALERT!  This following complicated test was derived from a truth table.  The idea is that
  ;; when the search window is a traveling-search-window then we want to call traveling-search-display-string;
  ;; when it is fake-minibuffer then we want to call display-search-string-in-fake-minibuffer.  If the search
  ;; window is NIL then the value of *DEFAULT-MENU-SEARCH-DISPLAY-MODE* determines which function gets called.
  ;; If *DEFAULT-MENU-SEARCH-DISPLAY-MODE* is :traveling-search then we call traveling-search-display-string,
  ;; otherwise call display-search-string-in-fake-minibuffer.  In this manner we call the proper function
  ;; according to the type of search window, or default according to the mode.
  (IF (OR (TYPEP SEARCH-WINDOW 'TRAVELING-SEARCH-WINDOW)
	 (AND (NOT (TYPEP SEARCH-WINDOW 'FAKE-MINIBUFFER-WINDOW))
	      (EQ *DEFAULT-MENU-SEARCH-DISPLAY-MODE* :TRAVELING-SEARCH)))
      (TRAVELING-SEARCH-DISPLAY-STRING)
      ;;ELSE
      (DISPLAY-SEARCH-STRING-IN-FAKE-MINIBUFFER)))

;;; Preference list for placement of the fake minibuffer relative to the menu.
(DEFCONSTANT FAKE-MINIBUFFER-POSITIONING-PREFERENCE-LIST '(:BELOW :ABOVE :RIGHT :LEFT))

(DEFUN DISPLAY-SEARCH-STRING-IN-FAKE-MINIBUFFER ()
  (DECLARE (:SELF-FLAVOR MENU))
  (LET ((ABORT-FAKE-MINIBUFFER NIL))
    (WHEN (NULL SEARCH-WINDOW)
      (SETQ SEARCH-WINDOW (ALLOCATE-RESOURCE 'FAKE-MINIBUFFER-WINDOW (SHEET-SUPERIOR SELF)))
      ;; Try to make the border of the search window the same as that for the menu.
      (SEND SEARCH-WINDOW :SET-BORDERS (IF (CONSP BORDERS)
					   (IF (MEMBER (CAR BORDERS) '(NIL :ZERO) :TEST #'EQ)
					       0
					       ;;ELSE
					       (- (FOURTH (FIRST BORDERS)) (SECOND (FIRST BORDERS))))
					   ;;ELSE
					   BORDERS)))
    (WHEN (NULL (SHEET-EXPOSED-P SEARCH-WINDOW))
      (SEND SEARCH-WINDOW :SET-SELECTION-SUBSTITUTE SELF)
      (SEND SEARCH-WINDOW :SET-SIZE
	    ;; Make sure that the window is wide enough to display a reasonable amount of search data.
	    (MAX (* MINIMUM-MENU-SEARCH-WINDOW-WIDTH (FONT-CHAR-WIDTH (SEND DEFAULT-SCREEN :PARSE-FONT-SPECIFIER :DEFAULT)))
		 (SHEET-WIDTH SELF))
	    ;; 2 lines high plus the borders.
	    (+ (- (SHEET-HEIGHT SEARCH-WINDOW)
		  (SHEET-INSIDE-HEIGHT SEARCH-WINDOW))
	       (* 2 (SHEET-LINE-HEIGHT SEARCH-WINDOW))))
      (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
	  (SEND SELF :EDGES)
	;; Position the fake minibuffer at the bottom if possible to make it more like Zmacs.
	(POSITION-WINDOW-NEXT-TO-RECTANGLE SEARCH-WINDOW FAKE-MINIBUFFER-POSITIONING-PREFERENCE-LIST LEFT TOP RIGHT BOTTOM))
      (IF (NULL (SHEET-WITHIN-SHEET-P SEARCH-WINDOW SUPERIOR))
	  (SETQ ABORT-FAKE-MINIBUFFER T)
	  ;;ELSE
	  (SEND SEARCH-WINDOW :EXPOSE)))
    (IF ABORT-FAKE-MINIBUFFER
	(PROGN
	  ;; We couldn't bring up the fake minibuffer because the menu was almost the size of the entire screen and there
	  ;; wasn't enough room for the fake minibuffer.  Switch to the traveling search window since that will always fit
	  ;; inside of a very large menu like this.
	  (SEND SEARCH-WINDOW :DEACTIVATE)
	  (DEALLOCATE-RESOURCE 'FAKE-MINIBUFFER-WINDOW SEARCH-WINDOW)
	  (SETQ SEARCH-WINDOW NIL)
	  (TRAVELING-SEARCH-DISPLAY-STRING))
	;;ELSE
	(PROGN
	  (SEND SEARCH-WINDOW :SELECT)
	  (SEND SEARCH-WINDOW :SET-CURSORPOS 0 0)
	  (SEND SEARCH-WINDOW :CLEAR-EOF)
	  (SEND SEARCH-WINDOW :STRING-OUT (CAAR (IF (EQ SEARCH-DIRECTION :FORWARD)
						    *MENU-SEARCH-FORWARD-PROMPTS*
						    ;;ELSE
						    *MENU-SEARCH-BACKWARD-PROMPTS*)))
	  (SEND SEARCH-WINDOW :STRING-OUT SEARCH-STRING)))))


;;; Display the search string using the traveling search window.
(DEFUN TRAVELING-SEARCH-DISPLAY-STRING ()
  (DECLARE (:SELF-FLAVOR MENU))
  (LET ((ABORT-TRAVELING-SEARCH NIL))
    (WHEN (NULL SEARCH-WINDOW)
      (SETQ SEARCH-WINDOW (ALLOCATE-RESOURCE 'TRAVELING-SEARCH-WINDOW SELF)))
    (SEND SEARCH-WINDOW :SET-SELECTION-SUBSTITUTE SELF)
    (MULTIPLE-VALUE-BIND (SEARCH-STRING-WIDTH SEARCH-STRING-HEIGHT)
	(SEND SEARCH-WINDOW :COMPUTE-MOTION SEARCH-STRING 0 NIL 0 0 NIL
	      MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM)
      ;; Loop through each search prompt possibility until we find one that fits.
      (LOOP FOR (SEARCH-PROMPT SEARCH-FONT) IN (IF (EQ SEARCH-DIRECTION :FORWARD)
						   *MENU-SEARCH-FORWARD-PROMPTS*
						   ;;ELSE
						   *MENU-SEARCH-BACKWARD-PROMPTS*)
	    WITH SEARCH-VSP = 1
	    FOR PROMPT-WIDTH = (SEND SEARCH-WINDOW :COMPUTE-MOTION (STRING SEARCH-PROMPT) 0 NIL 0 0 NIL
				     MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM MOST-POSITIVE-FIXNUM
				     SEARCH-FONT)
	    DO (WHEN (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
			 (SEND SELF :ITEM-RECTANGLE CURRENT-ITEM)
		       ;; :ITEM-RECTANGLE adds 1 pixel to the edges so that a box
		       ;; can be drawn around it.  Here we subtract that pixel out
		       ;; to make the search window's rectangle the same size as the item.
		       (SETQ LEFT    (1+ LEFT)
			     TOP     (1+ TOP)
			     RIGHT   (1- RIGHT)
			     BOTTOM  (1- BOTTOM))
		       (WITH-SHEET-DEEXPOSED (SEARCH-WINDOW)
			 ;; Move to a place where we know that we won't be outside of the superior.
			 (SEND SEARCH-WINDOW :SET-POSITION (SHEET-INSIDE-LEFT SELF) (SHEET-INSIDE-TOP SELF))
			 ;; Make the search window just large enough to hold the prompt and the search string.
			 (SEND SEARCH-WINDOW :SET-INSIDE-SIZE
			       (+ PROMPT-WIDTH SEARCH-STRING-WIDTH SEARCH-VSP)
			       (MAX (+ SEARCH-VSP (SHEET-LINE-HEIGHT SEARCH-WINDOW) SEARCH-STRING-HEIGHT)
				    (IF SEARCH-FONT
					(FONT-CHAR-HEIGHT SEARCH-FONT)
					;;ELSE
					0)))
			 (POSITION-WINDOW-NEXT-TO-RECTANGLE SEARCH-WINDOW (IF (EQ SEARCH-DIRECTION :FORWARD) :ABOVE :BELOW)
							    LEFT TOP RIGHT BOTTOM)))
		 (SEND SEARCH-WINDOW :EXPOSE)
		 (SEND SEARCH-WINDOW :SELECT)
		 (SEND SEARCH-WINDOW :SET-CURSORPOS 0 0)
		 (SEND SEARCH-WINDOW :CLEAR-EOF)
		 (IF (STRINGP SEARCH-PROMPT)
		     (SEND SEARCH-WINDOW :STRING-OUT-X-Y-CENTERED-EXPLICIT SEARCH-PROMPT 0 0 PROMPT-WIDTH)
		     ;;ELSE
		     (PROGN
		       (PREPARE-SHEET (SEARCH-WINDOW)
			 (DRAW-CHAR SEARCH-FONT SEARCH-PROMPT 0 0 TV:ALU-XOR SEARCH-WINDOW))))
		 (SEND SEARCH-WINDOW :SET-CURSORPOS PROMPT-WIDTH 0)
		 ;; Center the search string in case the height of the window is bigger than the default.
		 (SEND SEARCH-WINDOW :STRING-OUT-X-Y-CENTERED-EXPLICIT SEARCH-STRING
		       (SEND SEARCH-WINDOW :CURSOR-X))
		 (SEND SEARCH-WINDOW :SET-CURSORPOS 0 0)
		 (RETURN NIL))
	    FINALLY (SETQ ABORT-TRAVELING-SEARCH T)))
    (WHEN ABORT-TRAVELING-SEARCH
      ;; We couldn't bring up the traveling search window because the menu was too small.  Switch
      ;; to the traveling search window since that will always fit for a small menu.
      (SEND SEARCH-WINDOW :DEACTIVATE)
      (DEALLOCATE-RESOURCE 'TRAVELING-SEARCH-WINDOW SEARCH-WINDOW)
      (SETQ SEARCH-WINDOW NIL)
      (DISPLAY-SEARCH-STRING-IN-FAKE-MINIBUFFER))))

(DEFUN SEARCH-FOR-MATCH (IGNORE &OPTIONAL (MOVE-THE-MOUSE T))                      ; (C)
  "Search for a match of the current search string in the current direction.
Returns T if a match was found, NIL otherwise."
  (DECLARE (:SELF-FLAVOR MENU))
  (PROG1
    (LOOP WITH FIRST-ITEM        = CURRENT-ITEM
	  WITH FIRST-ITEM-ROW    = CURRENT-ITEM-ROW
	  WITH FIRST-ITEM-COLUMN = CURRENT-ITEM-COLUMN
	  WITH OLD-SEARCH-INDEX  = SEARCH-ITEM-INDEX
	  WITH SEARCH-LENGTH     = (LENGTH SEARCH-STRING)
	  FOR FINAL-ITEM         = CURRENT-ITEM
	  FOR MENU-ITEM          = (string (MENU-ITEM-STRING CURRENT-ITEM))
	  FOR MENU-ITEM-LENGTH   = (LENGTH MENU-ITEM)
	  FOR MATCH              = (IF (> SEARCH-LENGTH (- MENU-ITEM-LENGTH SEARCH-ITEM-INDEX))
				       ;; The search length is longer than the string being searched.
				       ;; Remove this when David H fixes search.  HACK ALERT.
				      NIL
				       ;;ELSE
				       (IF (EQ SEARCH-DIRECTION :FORWARD)
					   (SEARCH SEARCH-STRING MENU-ITEM :START2 SEARCH-ITEM-INDEX :TEST #'CHAR-EQUAL)
					   ;;ELSE
					   (SEARCH SEARCH-STRING MENU-ITEM :FROM-END T :TEST #'CHAR-EQUAL
						   :START2 0 :END2 (MIN MENU-ITEM-LENGTH
									(+ SEARCH-ITEM-INDEX SEARCH-LENGTH)))))
	  WHILE (NULL MATCH)
	  DO (PROGN
	       ;; No match here; move on to the next item.
	       (SEND SELF (IF (EQ SEARCH-DIRECTION :FORWARD)
			      :MOVE-CURSOR-FORWARD
			      ;;ELSE
			      :MOVE-CURSOR-BACKWARD)
		     ;; NIL says to not update the mouse.
		     NIL)
	       (WHEN (EQ FINAL-ITEM CURRENT-ITEM)
		 ;; There isn't anything more to search for.  Beep and move
		 ;; back to where we were before this search started.
		 (BEEP)
		 (SETQ CURRENT-ITEM        FIRST-ITEM
		       CURRENT-ITEM-ROW    FIRST-ITEM-ROW
		       CURRENT-ITEM-COLUMN FIRST-ITEM-COLUMN
		       SEARCH-ITEM-INDEX   OLD-SEARCH-INDEX)
		 (RETURN NIL))
	       (SETQ SEARCH-ITEM-INDEX (IF (EQ SEARCH-DIRECTION :FORWARD)
					   ;; Start subsequent searches with the first character in the item.
					   0
					   ;;ELSE - backward search
					   ;; Start subsequent searches with the last character in the item.
					   (- (LENGTH (string (MENU-ITEM-STRING CURRENT-ITEM))) SEARCH-LENGTH))))
	  FINALLY (PROGN
		    (SETQ SEARCH-ITEM-INDEX MATCH)
		    (RETURN T)))
    (WHEN MOVE-THE-MOUSE
      (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW))
      (DISPLAY-SEARCH-STRING))))

(DEFUN SEARCH-FOR-NEXT-MATCH (IGNORE)                      ; (D)
  "Search for the next match of the current search string in the current direction."
  (DECLARE (:SELF-FLAVOR MENU))
  ;; The idea is to move one character and search again.
  (LET ((NEED-TO-SEARCH T)
	(FINAL-ITEM         CURRENT-ITEM)
	(FINAL-ITEM-ROW     CURRENT-ITEM-ROW)
	(FINAL-ITEM-COLUMN  CURRENT-ITEM-COLUMN)
	(OLD-SEARCH-INDEX  SEARCH-ITEM-INDEX))
    (IF (EQ SEARCH-DIRECTION :FORWARD)
	(INCF SEARCH-ITEM-INDEX)
	;;ELSE
	(PROGN
	  (DECF SEARCH-ITEM-INDEX)
	  (WHEN (MINUSP SEARCH-ITEM-INDEX)
	    (SEND SELF :MOVE-CURSOR-BACKWARD)
	    (IF (EQ CURRENT-ITEM FINAL-ITEM)
		;; Couldn't move backward.  Stay where we are.
		(SETQ SEARCH-ITEM-INDEX OLD-SEARCH-INDEX
		      NEED-TO-SEARCH NIL)
		;;ELSE
		;; Point to the end of the the item we just moved to.
		(SETQ SEARCH-ITEM-INDEX (- (LENGTH (string (MENU-ITEM-STRING CURRENT-ITEM)))
					   (LENGTH SEARCH-STRING)))))))
    (WHEN NEED-TO-SEARCH
      (WHEN (NULL (SEARCH-FOR-MATCH NIL NIL))
	;; The search failed, restore everything back.
	(SETQ CURRENT-ITEM        FINAL-ITEM
	      CURRENT-ITEM-ROW    FINAL-ITEM-ROW
	      CURRENT-ITEM-COLUMN FINAL-ITEM-COLUMN
	      SEARCH-ITEM-INDEX   OLD-SEARCH-INDEX)))
    (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW))
    (DISPLAY-SEARCH-STRING)))


(DEFUN SEARCH-APPEND-CHAR-TO-SEARCH-STRING (CH)	; (E)
  (DECLARE (:SELF-FLAVOR MENU))
  (VECTOR-PUSH-EXTEND CH SEARCH-STRING)
  ;; Push (1 position direction) onto the stack.  "A" is simply an object whose LENGTH is one.
  (SEARCH-PUSH-STACK "A"))


(DEFUN STOP-MENU-SEARCHING (IGNORE)		; (F)
  (DECLARE (:SELF-FLAVOR MENU))
  (WHEN SEARCH-WINDOW
    (SEND SEARCH-WINDOW :DEACTIVATE)
    (DEALLOCATE-RESOURCE (TYPE-OF SEARCH-WINDOW) SEARCH-WINDOW)
    (SETQ SEARCH-WINDOW NIL))
  (SEND SELF :SELECT))

(DEFUN SEARCH-SAVE-SEARCH-STRING (IGNORE)	; (G)
  (DECLARE (:SELF-FLAVOR MENU))
  (SETQ SEARCH-OLD-SEARCH-STRING SEARCH-STRING))

(DEFUN SEARCH-EXIT-ABNORMALLY (CHAR)		; (H)
  (DECLARE (:SELF-FLAVOR MENU))
  (SETQ CURRENT-ITEM        SEARCH-OLD-CURRENT-ITEM
        CURRENT-ITEM-ROW    SEARCH-OLD-CURRENT-ITEM-ROW
        CURRENT-ITEM-COLUMN SEARCH-OLD-CURRENT-ITEM-COLUMN)
  (MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW))
  ;; Don't beep if the user simply is UNDOing the search.
  (WHEN (NOT (CHAR= CHAR #\UNDO))
    (BEEP)))

(DEFUN SEARCH-AGAIN (IGNORE)			; (I)
  (DECLARE (:SELF-FLAVOR MENU))
  (SEARCH-PUSH-STACK NIL))

(DEFUN SEARCH-CHANGE-DIRECTION-FORWARD (IGNORE)		; (J)
  (DECLARE (:SELF-FLAVOR MENU))
  (SETQ SEARCH-DIRECTION :FORWARD))

(DEFUN SEARCH-CHANGE-DIRECTION-BACKWARD (IGNORE)	; (K)
  (DECLARE (:SELF-FLAVOR MENU))
  (SETQ SEARCH-DIRECTION :BACKWARD))

(DEFUN SEARCH-HANDLE-RUBOUT (IGNORE)		; (L)
  (DECLARE (:SELF-FLAVOR MENU))
  (IF (NULL SEARCH-STACK)
      ;; Can't rubout anything if there isn't anything there.
      (BEEP)
      ;;ELSE
      (LET* ((STACK-ELEMENT (POP SEARCH-STACK))
	     (ELEMENT-LENGTH      (NTH 0 STACK-ELEMENT))
	     (ELEMENT-DIRECTION   (NTH 1 STACK-ELEMENT))
	     (ELEMENT-ITEM-INDEX  (NTH 2 STACK-ELEMENT))
	     (ELEMENT-ITEM        (NTH 3 STACK-ELEMENT))
	     (ELEMENT-ITEM-ROW    (NTH 4 STACK-ELEMENT))
	     (ELEMENT-ITEM-COLUMN (NTH 5 STACK-ELEMENT)))
	;; Restore the position to what it was before.
	(DECF (FILL-POINTER SEARCH-STRING) ELEMENT-LENGTH)
	(SETQ SEARCH-DIRECTION    ELEMENT-DIRECTION
	      SEARCH-ITEM-INDEX   ELEMENT-ITEM-INDEX
	      CURRENT-ITEM        ELEMENT-ITEM
	      CURRENT-ITEM-ROW    ELEMENT-ITEM-ROW
	      CURRENT-ITEM-COLUMN ELEMENT-ITEM-COLUMN)
	(DISPLAY-SEARCH-STRING)
	(MENU-UPDATE-MOUSE-CURSOR (AREF ROW-MAP CURRENT-ITEM-ROW)))))

;;; In order to remain as completely compatible with Zmacs as possible, special handling must be done
;;; with search characters prefixed by CONTROL-Q (the so-called `escape' character).  Not only must one
;;; be able to read characters like ABORT, but the handling of control characters must be done carefully.
;;; For Zmacs, ALL characters (even non-printing such as F3) that have the control bit set are changed so
;;; that ALL bits of the character, except the low order 5 bits, are set to zero.  For example,
;;; HYPER-SUPER-CONTROL-q would become the right horseshoe.
(DEFUN SEARCH-READ-CHARACTER-LITERALLY (IGNORE)	; (M)
  (DECLARE (:SELF-FLAVOR MENU))
  (LET* ((KBD-INTERCEPTED-CHARACTERS MENU-INTERCEPTED-CHARACTERS)
	 (NEW-CHARACTER (READ-CHAR SELF)))
    (WHEN (CHAR-BIT NEW-CHARACTER :CONTROL)
      (SETQ NEW-CHARACTER (MAKE-CHAR (LDB #o0005 (CHAR-CODE NEW-CHARACTER)))))
    (SEARCH-APPEND-CHAR-TO-SEARCH-STRING NEW-CHARACTER)))

;;; The following variable maps the code names in the state table to functions.
(DEFPARAMETER ACTION-TO-FUNCTION-MAP `((A ,#'SEARCH-INITIALIZE)
				       (B ,#'SEARCH-RESEARCH-OLD-ARG)
				       (C ,#'SEARCH-FOR-MATCH)
				       (D ,#'SEARCH-FOR-NEXT-MATCH)
				       (E ,#'SEARCH-APPEND-CHAR-TO-SEARCH-STRING)
				       (F ,#'STOP-MENU-SEARCHING)
				       (G ,#'SEARCH-SAVE-SEARCH-STRING)
				       (H ,#'SEARCH-EXIT-ABNORMALLY)
				       (I ,#'SEARCH-AGAIN)
				       (J ,#'SEARCH-CHANGE-DIRECTION-FORWARD)
				       (K ,#'SEARCH-CHANGE-DIRECTION-BACKWARD)
				       (L ,#'SEARCH-HANDLE-RUBOUT)
				       (M ,#'SEARCH-READ-CHARACTER-LITERALLY)))

(DEFPARAMETER MENU-SEARCH-HELP-ALIST
	      '((#\CONTROL-S           "Incremental search in the forward direction.  Press again to see next occurance")
		(#\CONTROL-R           "Incremental search in the backward direction.  Press again to see next occurance")
		(""                    NIL)
		(#\CONTROL-Q           "Various quantities command.  Read next character literally")
		(""                    NIL)
		(#\CONTROL-G           "Exit search mode, forget search string and return to where the search started")
                (#\UNDO                "Exit search mode and return to where the search started")
		(#\ESCAPE              "Exit search mode leaving mouse on the item found")
		(""                    NIL)
		(#\RUBOUT              "Rubouts out the previous character")
		("Printable character" "Search with that character appended to the current search string")
		(""                    NIL)
		(#\ABORT               "Exit the menu abnormally with no item selected")
		(#\END                 "Exit the menu normally with item(s) selected")
		(""                    NIL)
		("Anything else"        "Same as pressing END followed by that character")
		(""                    NIL)
		(#\HELP                "Display this text"))
              "An alist of (character documentation-string) used by menu
search windows to display help information.  If the CHARACTER is actually
a string, then it is displayed for documentation purposes only.  If the
documentation is NIL then nothing is displayed.")

;;; This method deactivates and deallocates the search window when the
;;; menu goes away to get rid of the search window and to allow reuse of
;;; the resource.
(DEFMETHOD (MENU :after :DEEXPOSE) (&REST IGNORE)
  (WHEN SEARCH-WINDOW
    (SEND SEARCH-WINDOW :DEACTIVATE)
    (DEALLOCATE-RESOURCE (TYPE-OF SEARCH-WINDOW) SEARCH-WINDOW)
    (SETQ SEARCH-WINDOW NIL)))

(DEFMETHOD (MENU :HELP-SEARCH) ()
  (SI:WITH-HELP-STREAM (WINDOW :LABEL "Menu commands for searching" :SUPERIOR MOUSE-SHEET)
    (SEND WINDOW :SET-PROCESS CURRENT-PROCESS)
        ;; Window configuration stable now, let keyboard process proceed.
        (SETQ KBD-TERMINAL-TIME NIL)
        (FORMAT WINDOW "~%The following command characters are defined:~%")
        (DOLIST (ELEMENT MENU-SEARCH-HELP-ALIST)
          (IF (OR (NUMBERP (CAR ELEMENT)) (CHARACTERP (CAR ELEMENT)))
              (FORMAT WINDOW "~{~%  ~20<~@:C~>  ~:[~;~:*~A.~]~}" ELEMENT)
              ;;ELSE
              (FORMAT WINDOW "~{~%  ~20<~A~>  ~:[~;~:*~A.~]~}" ELEMENT))))
  ;; Get rid of the character that the user typed (papers over a bug in with-help-stream).
  (SEND SELF :CLEAR-INPUT)
  NIL)

;;; This is the main driver for the search command.  Here we loop through
;;; the finite-state machine until we reach an exit condition.
(DEFMETHOD (MENU :SEARCH) (CH)
  "Search in the given direction for a menu item which contains characters of the
string that the user will subsequently enter."
  (LET (LAST-COLUMN)
    ;; The following form will read all characters except SYSTEM and TERM
    (LOOP FOR CURRENT-STATE  = 1 THEN NEXT-STATE
	  FOR CURRENT-COLUMN = (MAP-SEARCH-CHARACTER CH)
	  FOR STATE-ELEMENT  = (AREF MENU-SEARCH-STATE-TABLE
				    (1- CURRENT-STATE)
				    CURRENT-COLUMN)
	  FOR NEXT-STATE = (CAR STATE-ELEMENT)
	  DO (PROGN
	        ;; Execute the action routines.
	        (DOLIST (ACTION (CADR STATE-ELEMENT))
		 (SEND (CADR (ASSOC ACTION ACTION-TO-FUNCTION-MAP :TEST #'EQ)) CH))
	        (UNLESS (MINUSP NEXT-STATE)
		 ;; Read another character to move to the next state.
		 (DO-FOREVER
		    (LET ((KBD-INTERCEPTED-CHARACTERS MENU-INTERCEPTED-CHARACTERS))
		       (SETQ CH (READ-CHAR SELF)))
		    ;; If the user wanted help then give some and get another character.
		    (IF (CHAR= CH #\HELP)
		         (SEND SELF :HELP-SEARCH)
		         ;;ELSE
		         (RETURN NIL)))))
	  UNTIL (MINUSP NEXT-STATE)
	  FINALLY (SETQ LAST-COLUMN CURRENT-COLUMN))
    (WHEN (EQ LAST-COLUMN SEARCH-COLUMN-FOUND-ANYTHING)
      ;; We read one character too much.  This might have been an ordinary menu command.
      ;; All other characters were commands controlling the termination of the search.
      (UNREAD-CHAR CH SELF))))

#|

Test cases:

Tie the F2 key to a function which prints a screen image.
(defun USER:foo-print (IGNORE ignore)
  (PRINT-BITMAP (tv:sheet-screen-array tv:default-SCREEN)))

(setq TV:KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS (CONS
					      `(,(FIX #\F2)
						,#'USER:foo-print)
					      TV:KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS))

Test a window which scrolls.
(SETQ X (send tv:selected-window :which-operations)
      y nil)

Normal scrolling window
(w:menu-choose X :sort :ascending)

Window which is too tall for the search window to fit on the top or bottom.
(w:menu-choose x :sort :ascending :GEOMETRY '(1 55))
|#
