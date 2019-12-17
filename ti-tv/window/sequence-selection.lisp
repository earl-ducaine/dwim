;;; -*- Mode:Common-Lisp; Package:TV; Base:10.; Fonts:CPTFONT,HL12B,HL12BI -*-

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

#| The following sets up a menu flavor and accessor function for choosing a
sequence of menu items.  It dynamically displays the current sequence.  |#


(DEFFLAVOR SEQUENCE-SELECTION-MULTIPLE-MENU
	   ((OUTPUT-WINDOW (MAKE-WINDOW 'SEQUENCE-SELECTION-OUTPUT-WINDOW))
	    (PREVIOUS-ITEM NIL)
	    (PREVIOUS-WHO-LINE "")
	    (ANY-CHOICES-YET? NIL)
	    ;; These vars may be set to customize the output of choices.
            ;; Note that you might set these to functions that don't
            ;; even use OUTPUT-WINDOW.  Your functions may reference
            ;; instance vars if :SELF-FLAVOR is declared to be
            ;; SEQUENCE-SELECTION-MULTIPLE-MENU.
	    (INITIALIZE-OUTPUT-FUNCTION 'EXPOSE-OUTPUT-WINDOW)
	    (FINALIZE-OUTPUT-FUNCTION   'BURY-OUTPUT-WINDOW)
	    (DISPLAY-CHOICE-FUNCTION    'DISPLAY-CHOICE-ON-OUTPUT-WINDOW)
	    (REDISPLAY-CHOICES-FUNCTION 'REDISPLAY-CHOICES-ON-OUTPUT-WINDOW))
	   (W:MENU)
  (:DEFAULT-INIT-PLIST
    ;; The REAL values these should return are (VALUES (SEND SELF
    ;; :HIGHLIGHTED-VALUES) T) and (VALUES NIL NIL), but for some reason
    ;; multiple values aren't returned right by TV:MULTIPLE-MENU.  This
    ;; is a kludge-workaround, the other half being in function
    ;; TV:MULTIPLE-MENU-SEQUENCE-CHOOSE.
    :MENU-MARGIN-CHOICES '(:DOIT
                           ("Abort" :EVAL (VALUES (SETQ ABORTED? :ABORTED))))
    ;; Provide a maximum width to leave room for the ouput window.
    :GEOMETRY (LIST NIL NIL NIL NIL (- (TRUNCATE (SEND TV:DEFAULT-SCREEN :WIDTH) 2) 40) NIL))
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :INITTABLE-INSTANCE-VARIABLES
  (:DOCUMENTATION :COMBINED
    "This flavor is a variation on W:MENU to
provide a way of selecting the same options repeatedly from a menu.
In otherwords, if the menu displays A, B, C, and D, the user can select
the items in the following order: A C A B A.  The friendly interface to
this menu is the function TV:MULTIPLE-MENU-SEQUENCE-CHOOSE
which uses a resource of menus of this flavor.
  To customize the output of selected menu items, four instance
variables are provided which control the output.  They hold functions
which are FUNCALLed when output needs to be done.
  INITIALIZE-OUTPUT-FUNCTION -- a function called when the user
      selects the first menu item in the sequence.  The default
      function, TV:EXPOSE-OUTPUT-WINDOW, simply exposes an
      output window.
  FINALIZE-OUTPUT-FUNCTION -- a function called when the user
      completes input of the sequence.  The defualt function,
      TV:BURY-OUTPUT-WINDOW, simply buries the output window
      exposed by TV:EXPOSE-OUTPUT-WINDOW.
  DISPLAY-CHOICE-FUNCTION -- a function called whenever the user
      selects a menu item.  The item is passed as an argument.  The
      default function, TV:DISPLAY-CHOICE-ON-OUTPUT-WINDOW,
      prints the item to the output window.
  REDISPLAY-CHOICES-FUNCTION -- a function called whenever the
      user deselects a choice.  The default function,
     TV:DISPLAY-CHOICE-ON-OUTPUT-WINDOW, clears the output
     window and prints out the currently selected menu items."))

(DEFUN EXPOSE-OUTPUT-WINDOW ()
  "Adjusts the size of the output window and exposes it near the
menu.  The size set to be the same as the menu, with a minimum
height for displaying the sequence."
  (DECLARE (:SELF-FLAVOR SEQUENCE-SELECTION-MULTIPLE-MENU))
  (SEND OUTPUT-WINDOW :SET-SIZE WIDTH (MAX HEIGHT 64.))
  (SEND OUTPUT-WINDOW :EXPOSE-NEAR `(:WINDOW ,SELF)))

(DEFUN BURY-OUTPUT-WINDOW ()
  "Buries the output window."
  (DECLARE (:SELF-FLAVOR SEQUENCE-SELECTION-MULTIPLE-MENU))
  (SEND OUTPUT-WINDOW :BURY))

(DEFUN DISPLAY-CHOICE-ON-OUTPUT-WINDOW (ITEM)
  "Prints ITEM on the output window."
  (DECLARE (:SELF-FLAVOR SEQUENCE-SELECTION-MULTIPLE-MENU))
  (SEND OUTPUT-WINDOW :STRING-OUT (FORMAT NIL "~A~%" (IF (CONSP ITEM) (CAR ITEM) ITEM))))

(DEFUN REDISPLAY-CHOICES-ON-OUTPUT-WINDOW ()
  "Clears the output window and reprints the current sequence of
  selected menu items."
  (DECLARE (:SELF-FLAVOR SEQUENCE-SELECTION-MULTIPLE-MENU))
  (SEND OUTPUT-WINDOW :CLEAR-SCREEN)
  (DOLIST (ITEM HIGHLIGHTED-ITEMS)
    (SEND OUTPUT-WINDOW :STRING-OUT (FORMAT NIL "~A~%" (IF (CONSP ITEM) (CAR ITEM) ITEM)))))

(DEFMETHOD (SEQUENCE-SELECTION-MULTIPLE-MENU :MOUSE-BUTTONS-ON-ITEM) (MOUSE-BUTTON-USED)
  "Redefined to make middle click select again an already-selected item
and to handle the output of selected and deselected items in the
output window."
  (UNLESS ANY-CHOICES-YET?
    (SETQ ANY-CHOICES-YET? T)
    (FUNCALL INITIALIZE-OUTPUT-FUNCTION))
  (IF (= MOUSE-BUTTON-USED 2) (SEND SELF :ADD-HIGHLIGHTED-ITEM CURRENT-ITEM)
      (IF (NOT (MEMBER CURRENT-ITEM HIGHLIGHTED-ITEMS :TEST #'EQ))
          (SEND SELF :ADD-HIGHLIGHTED-ITEM CURRENT-ITEM)
          (PROGN
            (SEND SELF :REMOVE-HIGHLIGHTED-ITEM CURRENT-ITEM)
            (FUNCALL REDISPLAY-CHOICES-FUNCTION)))))

(DEFMETHOD (SEQUENCE-SELECTION-MULTIPLE-MENU :AFTER :DEEXPOSE) (&REST IGNORE)
  (SETQ ANY-CHOICES-YET? NIL)
  (FUNCALL FINALIZE-OUTPUT-FUNCTION))

(DEFMETHOD (SEQUENCE-SELECTION-MULTIPLE-MENU :ADD-HIGHLIGHTED-ITEM) (ITEM)
  "Redefined to always push the item so we can select items more
  than once, and to output the new item."
  (COND ((NOT (MEMBER ITEM HIGHLIGHTED-ITEMS :TEST #'EQ))
         (SHEET-FORCE-ACCESS (SELF T) (SEND SELF :HIGHLIGHT-ITEM ITEM))))
  (PUSH ITEM HIGHLIGHTED-ITEMS)
  (FUNCALL DISPLAY-CHOICE-FUNCTION ITEM)
  HIGHLIGHTED-ITEMS)

(DEFMETHOD (SEQUENCE-SELECTION-MULTIPLE-MENU :WHO-LINE-DOCUMENTATION-STRING) ()
  "This method is used by the mouse process to determine the
contents of the who-line.  It is a redefinition of (:method
essential-scroll-mouse-mixin :who-line- documentation-string) to tell
the user the difference between left and middle click."
  (IF (EQ CURRENT-ITEM PREVIOUS-ITEM) PREVIOUS-WHO-LINE
      (PROGN
        (SETQ PREVIOUS-ITEM CURRENT-ITEM)
        (SETQ PREVIOUS-WHO-LINE
              (LET ((MESSAGE '(:MOUSE-L-1 "Select or Remove item" :MOUSE-M-1 "Select or Reselect item"))
                    (ITEM-DOC (AND (CONSP CURRENT-ITEM) (CONSP (CDR CURRENT-ITEM))
                                   (GET (CDDR CURRENT-ITEM) :DOCUMENTATION))))
                (IF ITEM-DOC
                    `(:DOCUMENTATION ,(FORMAT NIL "~%~A" ITEM-DOC) ,@MESSAGE)
                    MESSAGE))))))


(DEFFLAVOR SEQUENCE-SELECTION-OUTPUT-WINDOW ()
	   (TEMPORARY-WINDOW-MIXIN STREAM-MIXIN BORDERS-MIXIN MINIMUM-WINDOW)
  (:DEFAULT-INIT-PLIST
    :MORE-P NIL
    :BLINKER-P NIL)
  (:DOCUMENTATION :SPECIAL
    "Used by TV:SEQUENCE-SELECTION-MULTIPLE-MENU to output the selected items."))

(DEFMETHOD (SEQUENCE-SELECTION-OUTPUT-WINDOW :WHO-LINE-DOCUMENTATION-STRING) ()
  "Displays the sequence of selected menu items.")
(DEFMETHOD (SEQUENCE-SELECTION-OUTPUT-WINDOW :MOUSE-CLICK) (&REST IGNORE)
  NIL)
(DEFMETHOD (SEQUENCE-SELECTION-OUTPUT-WINDOW :MOUSE-BUTTONS) (&REST IGNORE)
  NIL)


;;; Note: no NEAR-MODE arg since menu may be too big to be in middle of window.
(DEFUN MULTIPLE-MENU-SEQUENCE-CHOOSE (ALIST &OPTIONAL (LABEL NIL)
				      HIGHLIGHTED-ITEMS
				      (SUPERIOR MOUSE-SHEET))
  "Let user choose some set of elements of ALIST with a multiple menu.
This is a variation on tv:MULTIPLE-MENU-CHOOSE to provide a way for
menu items to be chosen more than once.

ALIST looks like an ITEM-LIST for a menu.
LABEL is used as the label of the menu, if not NIL.
HIGHLIGHTED-ITEMS is a set of elements of ALIST;
 these will form the initially chosen subset, which the user can modify.
The menu is made an inferior of SUPERIOR.

If the user exits by clicking on the Do It box, there are two values:
1) a list of the values from executing the selected set of items, IN
  THE ORDER THE USER SELECTED THEM (this is opposite from the
  value returned by TV:MULTIPLE-MENU-CHOOSE), and
2) T.
If the user aborts, NIL is returned for both values."
  (WHEN ALIST
    (USING-RESOURCE (MENU SEQUENCE-SELECTION-MULTIPLE-MENU SUPERIOR)
      (SEND MENU :SET-LABEL LABEL)
      (SEND MENU :SET-ITEM-LIST ALIST)
      (SEND MENU :SET-HIGHLIGHTED-ITEMS HIGHLIGHTED-ITEMS)
      ;; This is a precaution in case it was left non-nil somehow.
      (SEND MENU :SET-ANY-CHOICES-YET? NIL)
      (SEND MENU :EXPOSE-NEAR `(:POINT 0 ,MOUSE-Y))
      (LET (VALUE ABORTED?) ; Part of kludge mentioned above.
	(DECLARE (SPECIAL ABORTED?))
	(SETQ VALUE (REVERSE (SEND MENU :CHOOSE)))
	(SEND MENU :BURY)
	(IF ABORTED?
	    (VALUES NIL NIL)
	  (VALUES VALUE T))))))
