;;; -*- Mode:Common-Lisp; Package:FED; Fonts:(CPTFONT HL10B TR10I CPTFONT HL10b); Base:8 -*-

;;;                                    RESTRICTED RIGHTS LEGEND 
;;; Use,  duplication, or  disclosure  by  the  Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013. 
;;;
;;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 2909 AUSTIN, TEXAS 78769  
;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


;;; Commands that print out documentation

;;; CHANGE HISTORY

;;;  6/22/87  DKM  - Change :document-details-character-io to refer to SAVE X instead of SAVE CHAR X


;;; Provide documentation on the titles used in the label pane.
(DEFMETHOD (FED :DOCUMENT-LABEL-PANE-ITEM) (item-string &rest ignore)
  "Displays information when the user clicks on the titles in the label pane."
   (DECLARE (SPECIAL typeout-pane))
   (LET ((*terminal-io* typeout-pane))
     (SEND typeout-pane :set-current-font 1)
     (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%")
     (COND ((STRING-EQUAL item-string " Mode: ")
	    (FORMAT typeout-pane 
"~35T-MODE-~%~%
This field indicates the current mode of the drawing mouse, that is,
it indicates the action that takes place when you click Mouse-left
while the mouse-cursor is in the drawing pane.

Available modes are:

    FLIP  -  The current state of the pixel under the mouse is inverted
             (that is, if on, is turned off; if off, is turned on).

    DRAW  -  The pixel under the mouse is turned on.
 
    ERASE -  The pixel under the mouse is turned off.

The state of the current mode of the drawing mouse is changed by clicking 
Mouse-middle, or by clicking over the current-value field which follows the
Mode title in the Label Pane, or by executing the Change Mode command.~%"
	      ))
	   ((STRING-EQUAL item-string " Font: ")
	    (FORMAT typeout-pane
"~35T-FONT-~%~%
This field indicates the current font being edited.  If no font is being,
or has been edited, this field contains the word NONE.

If you click the mouse over this value, the font editor prompts you (in the 
Keyboard Pane) for the name of a font to be edited.~%"
	      ))
	   ((STRING-EQUAL item-string "  Total Ht: ")
	    (FORMAT typeout-pane
"~31T-TOTAL HEIGHT-~%~%
The total height field indicates the total height allowed for characters 
in this font.  This height is used by the window system to determine the 
required vertical spacing when displaying characters in this font.

All characters within this font have a height less than or equal to this 
total height value.  This total height is represented on the drawing pane 
either by the distance from the top of the character box to the line below 
the character box, if there are descenders, or to the bottom of the 
character box, if there are no descenders.

You can change this total height value by clicking on the current value 
and supplying a new value when prompted in the Keyboard Pane.  When you 
change it for a character, it is changed for all the characters in the font.

When creating a new font, this value should be the first item supplied.~%"
	      ))
	   ((STRING-EQUAL item-string "  Above Base: ")
	    (FORMAT typeout-pane
"~32T-ABOVE BASE-~%~%
The height above the baseline indicates the height of characters in this 
font that do not have descenders.  This value must be less than or equal 
to the total height specified for this font.  This above the baseline 
height is represented on the drawing pane by the distance from the top 
to the bottom of the character box.

You can change this above-the-baseline height by clicking on the current 
value and supplying a new value when prompted in the Keyboard Pane.

When creating a new font, this value should be supplied after the total 
height to provide a properly-sized character box in which to define the 
characters of the font.~%"
	      ))
	   ((STRING-EQUAL item-string "  Blinker: ")
	    (FORMAT typeout-pane
"~34T-BLINKER-~%~%
The blinker size specified here indicates the width and height of the
blinker character that the window system will display when required.

The first value, blinker width, is generally the width of the characters
in a fixed-width font or the width of a typical character in a variable-
width (proportionally spaced) font.

The second value, blinker height, is generally equal to the total height
value specified for the font.

You can change the blinker width and height values by clicking on each
current value and supplying a new value when prompted in the Keyboard Pane.~%"
	      ))
	   ((STRING-EQUAL item-string "  Rotate: ")
	    (FORMAT typeout-pane
"~34T-ROTATE-~%~%
The rotation value indicates the relative orientation of the characters
in this font to the normal or vertical orientation, value 0.

If the characters of a font are rotated using the rotate commands
provided in the Font-io menu, this rotation value will indicate their 
orientation as follows:

      0 rotation  -  normal or vertical orientation;

     90 rotation  -  characters rotated right;

    180 rotation  -  characters rotated 180;

    270 rotation  -  characters rotated left.

The value field following this title only displays the rotation of 
the current font.  This value is automatically updated by the 
aforementioned rotate commands.~%"
	      ))
	   ((STRING-EQUAL item-string "  Space: ")
	    (FORMAT typeout-pane
"~35T-SPACE-~%~%
The value following this space title indicates the width of the space
character for the current font.  This value is used as the default
width value for any new characters to be created for this font.

The value displayed is generated when the space character is created.
In creating a new font you should first establish the font's
total height and height above the baseline.  Then the space character
should be edited.  The width of this character is specified by
supplying a value to the Width field in the Label Pane.

After this character is saved in the font, when you specify another
non-existent character to edit, the default width value for that
new character is equal to the width value of the space character.~%"
	      ))
	   ((STRING-EQUAL item-string "  Char:  ")
	    (FORMAT typeout-pane
"~36T-CHAR-~%~%
Following this character title are three representations of the character
currently being edited.  First is the numerical value of the character
within the font.  Clicking on this value field allows you to specify 
which character to edit by numeric value.

Second is the character's representation in the system's default font.
This representation allows you to correspond each character position
with the appropriate keyboard character.

Third is the character in the format of the font being edited if it
currently exists.  This third representation is dynamically updated with
each mouse-click that updates a pixel on the drawing pane.  This provides
you with a real-time, actual-size display of the character being
processed.

You can click over the combined second and third fields to select a 
character for editing by entering it from the keyboard.~%"
	      ))
	   ((STRING-EQUAL item-string "  Width: ")
	    (FORMAT typeout-pane
"~35T-WIDTH-~%~%
This value represents the width of the current character currently being 
edited.  This is the value used by the window system to determine how 
far the cursor or blinker must be moved after displaying this character.
This width is represented by the width of the character box on the
drawing pane.

Characters can extend beyond the width boundaries of the character 
box -- this is called kerning.  Characters so defined are stored
and displayed in the width in which they are created, but the cursor
movement is governed by this character width value.  Thus, you can 
create characters that overlap adjacent characters on the same horizontal 
line.  (Characters cannot extend above or below the established character 
box boundaries.)

Fixed-width fonts have the same width value for all of the characters 
in a font.~%"
	      ))
	   ((STRING-EQUAL item-string "  Sample: ")
	    (FORMAT typeout-pane
"~34T-SAMPLE-~%~%
The sample field to the right of this label is an area in which you can 
display characters from the font being edited.  This facility allows you 
to view these characters in some context other than in a stand-alone 
environment.

The sample field initially defaults to the string Sample.  Clicking over
this field allows you to enter any string of characters.  This field
is dynamically updated with each mouse click on the drawing pane so that the 
characters in the sample string show the current character accurately.

This field is limited to the remainder of this line and is truncated on
the right if its length exceeds the pane width.~%"
	      ))
	   ((STRING-EQUAL item-string " CHG ")
	    (FORMAT typeout-pane
"~36T-CHG-~%~%
This CHG indicator shows you that the font editor has detected a change 
to the current character, that is, you have clicked the mouse with a 
left-click while over the drawing pane.  This indicator does not keep 
track of the changes.  For example, if you add then remove a pixel from 
a character's representation, the CHG indicator still shows a change to 
the font editor.

This indicator is cleared each time a  character is saved in its font or
when a new character is selected for editing.~%"
	      ))
       )
     (SEND typeout-pane :set-current-font 0)
     (TERPRI)))

;;; Provide detailed documentation on selected items from various typeout panes.
(DEFMETHOD (FED :DOCUMENT-DETAILS-FONT-IO) (item-string &rest ignore)
   "Displays information when the user clicks on specific items in the typeout pane."
   (DECLARE (SPECIAL typeout-pane))
   (LET ((*terminal-io* typeout-pane))
     (SEND typeout-pane :set-current-font 1)
     (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%")
						 ;; font-io additional documentation
     (COND ((STRING-EQUAL item-string " SELECT ")
	    (FORMAT typeout-pane
		    "~34T-SELECT-~%~%The SELECT command displays a menu of fonts already loaded in the FONTS package.

You can select one of these fonts for editing with the mouse.~%"
		    ))
	   ((STRING-EQUAL item-string " DIRECTORY ")
	    (FORMAT typeout-pane
"~33T-DIRECTORY-~%~%The DIRECTORY command displays a list of the fonts in a directory that you specify.
You are asked whether to list all the fonts in the directory or only the fonts that 
are not currently loaded. 

The font editor then lists the indicated fonts for viewing or editing.  If you 
select one of the listed fonts for editing, it is loaded into the FONTS package 
and becomes the current font.~%"  
		    ))
	   ((STRING-EQUAL item-string " DISPLAY ")
	    (FORMAT typeout-pane
"~34T-DISPLAY-~%~%The DISPLAY command displays all characters in the font currently being edited.

You can select one of these characters for editing with the mouse.

The display of the font shows changes made during the edit and is dynamically
updated as you edit characters.~%"
		    ))
	   ((STRING-EQUAL item-string " COPY ")
	    (FORMAT typeout-pane
"~36T-COPY-~%~%The COPY command prompts for a new font name, then copies the current font
to that new font name.  

If the new name exists, you are asked whether to overwrite the existing 
file or to abort the operation.~%"
		    ))
	   ((STRING-EQUAL item-string " LOAD ")
	    (FORMAT typeout-pane
"~36T-LOAD-~%~%The LOAD command allows you to specify a filename to load into the FONTS package.
The loaded font is then available for system use. The loaded font also 
becomes the current font for subsequent Font Editor operations.

When invoked, the LOAD command prompts you for a file type, either AST, ~a, 
or OTHER.

The AST format contains the font description in the form of asterisks within a
two-dimensional array such that the font information could be read/processed
by other programs and/or machines.

The ~a format is the format used by the Explorer for its object files.

OTHER gives you a list of non-standard font formats which the Explorer can read.~%"
		    (si:local-binary-file-type) (si:local-binary-file-type)))
	   ((STRING-EQUAL item-string " WRITE ")
	    (FORMAT typeout-pane
"~35T-WRITE-~%~%The WRITE command allows you to output a font file to disk.
When invoked, it prompts you for a file type, either AST, ~a, or OTHER.

The AST format contains the font description in the form of asterisks within a
two-dimensional array such that the font information could be processed
by other programs and/or machines.

The ~a format is the format used by the Explorer for its object files.

OTHER gives you a list of non-standard font formats which the Explorer can write.~%"
		    (si:local-binary-file-type) (si:local-binary-file-type)))
	   ((STRING-EQUAL item-string " REMOVE ")
	    (FORMAT typeout-pane
"~34T-REMOVE-~%~%The REMOVE command removes the specified font from the FONTS package.~%"
		    ))
	   ((STRING-EQUAL item-string " CREATE ")
	    (FORMAT typeout-pane
"~34T-CREATE-~%~%The CREATE command allows you to create a new font in the FONTS package.

You are  prompted for a font name and then are then able to create the various
characters desired within that font.~%"
		    ))
	   ((STRING-EQUAL item-string " ROTATE, ITALICIZE, STRETCH, THICKEN, UNTHICKEN, REVERSE ")
	    (FORMAT typeout-pane
"~11T-ROTATE, ITALICIZE, STRETCH, THICKEN, UNTHICKEN, REVERSE-~%
The ROTATE, ITALICIZE, STRETCH, THICKEN, UNTHICKEN, and REVERSE
commands perform the functions their names imply on all characters within the
font currently selected for editing.

When invoked, you are prompted for a font name for the resulting font. 
The default font name for each operations is formed by placing a suffix 
after the original font name. For example, a thickened version of the font 
MYFONT would be MYFONTB.

There are three ROTATE commands, ROTATE LEFT, ROTATE RIGHT, and
ROTATE 180 that rotate all characters in the indicated direction.  Note
that the characters are rotated roughly around their midpoint relative to
the character box and kerning information is not used or generated for the
characters.

The ITALICIZE command can be used to generate an italic font for the given
input font.  You may wish to edit the resulting font to correct any
individual characters that might not have italicized as nicely as desired.

The THICKEN command generates a bold version of the input font.  The
corresponding UNTHICKEN command attempts to generate a less-bold version
of the input font.  Note that UNTHICKENing a font that was THICKENed may
not result in the original font.

The REVERSE command generates a font of characters in reverse-video format,
that is, the characters are formed as white-pixels in a character box of
all black pixels.  Only that portion of the character within the character box
will be dealt with by the reverse-video function.

The STRETCH command allows you to specify size-modifying parameters
for both the X and Y direction, then generates a resulting font with each
character warped according to these parameters.~%"
		    ))
	   )
     (SEND typeout-pane :set-current-font 0)
     (TERPRI)))

(DEFMETHOD (FED :DOCUMENT-DETAILS-CHARACTER-IO) (item-string &rest ignore)
   "Displays information when the user clicks on specific items in the typeout pane."
   (DECLARE (SPECIAL typeout-pane))
   (LET ((*terminal-io* typeout-pane))
     (SEND typeout-pane :set-current-font 1)
     (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%")
						 ;; character-io additional documentation
     (COND ((STRING-EQUAL item-string " SAVE CHAR")
	    (FORMAT typeout-pane
"~33T-SAVE CHAR-~%~%The SAVE CHAR command saves the contents of the black plane of the drawing 
pane into the current font at the character position specifed by the current character, 
Char, in the Label Pane.~%"
		    ))
	   ((STRING-EQUAL item-string " SAVE X ")    ;changed from SAVE CHAR X   dkm 5/87
	    (FORMAT typeout-pane
"~32T-SAVE X-~%~%The SAVE eXplicit command saves the contents of the drawing pane in a font
and at a character position supplied by you.~%"
		    ))
	   ((STRING-EQUAL item-string " GET CHAR ")
	    (FORMAT typeout-pane
"~33T-GET CHAR-~%~%The GET CHAR command prompts you for the character within the current font
to be edited at this time.  The character is entered as a single-keyboard entry.~%"
		    ))
	   ((STRING-EQUAL item-string " GET CHAR NUM ")
	    (FORMAT typeout-pane
"~30T-GET CHAR NUM-~%~%The GET CHAR NUMeric command allows you to specify a character from the
current font for editing by its character code, or relative position within the font.~%"
		    ))
	   ((STRING-EQUAL item-string " GET GRAY CHAR ")
	    (FORMAT typeout-pane
"~29T-GET GRAY CHAR-~%~%The GET GRAY CHAR command prompts you for the font to access as well as the
character within that font to be loaded into the gray plane.

Any character already in the black plane within the drawing pane is not affected.~%"
		    ))
	   )
     (SEND typeout-pane :set-current-font 0)
     (TERPRI)))


(DEFMETHOD (FED :DOCUMENT-DETAILS-EDITING) (ITEM-STRING &REST IGNORE)
  "Displays information when the user clicks on specific items in the typeout pane."
  (DECLARE (SPECIAL TYPEOUT-PANE))
  (LET ((*TERMINAL-IO* TYPEOUT-PANE))
    (SEND TYPEOUT-PANE :SET-CURRENT-FONT 1)
    (FORMAT TYPEOUT-PANE "~%~30TFONT EDITOR HELP~%")
    ;; editing additional documentation
    (COND
      ((STRING-EQUAL ITEM-STRING " REFLECT ")
       (FORMAT TYPEOUT-PANE
"~34T-REFLECT-~%~%The REFLECT command rotates the character currently in the drawing pane around
an imaginary axis drawn through that pane.

When invoked, you are asked to specify the axis as one of:

    X  - a horizontal axis through the middle of the character;

    Y  - a vertical axis through the middle of the character;

    XY - a diagonal axis extending through the lower left corner and
           upper right corner of the character box; or

   -XY - a diagonal axis extending through the lower right corner and
           upper left corner of the character box.~%"))
      ((STRING-EQUAL ITEM-STRING " ROTATE, ITALICIZE, STRETCH, THICKEN, UNTHICKEN, REVERSE ")
       (FORMAT TYPEOUT-PANE
"~11T-ROTATE, ITALICIZE, STRETCH, THICKEN, UNTHICKEN, REVERSE-~%
The ROTATE, ITALICIZE, STRETCH, THICKEN, UNTHICKEN, and REVERSE commands
perform their respective operations on the character currently in the drawing pane.   

There are three ROTATE commands, ROTATE LEFT, ROTATE RIGHT, and ROTATE 180,
that rotate the character in the indicated direction.  Note that the
character is rotated roughly around its midpoint relative to the
character box but kerns are handled, that is, the entire drawing pane
is used in the rotations.  This may result in pixels being rotated
above or below the limits of the character box which will ultimately
not be stored when you attempt to save the character.

The ITALICIZE command can be used to generate an italic form of the
current character in the drawing pane.

The STRETCH command allows you to specify size-modifying parameters
for both the X and Y direction, then generates a resulting character warped
according to these parameters.

The THICKEN command generates a bold version of the current character
in the drawing pane..  The corresponding UNTHICKEN command attempts to
generate a less-bold version of the character.  Note that UNTHICKENing
a character that was THICKENed may not result in the original character.

The REVERSE command generates a character in reverse-video format,
that is, the character is formed as white pixels in a character box of
all black pixels.  Only that portion of the character within the character box
will be dealt with by the reverse-video function.~%"))
      ((STRING-EQUAL ITEM-STRING " ERASE BOTH, ERASE BLACK, ERASE GRAY ")
       (FORMAT TYPEOUT-PANE
"~20T-ERASE BOTH, ERASE BLACK, ERASE GRAY-~%
The ERASE BOTH, ERASE BLACK,  and ERASE GRAY commands clear
all pixels, only the black-plane pixels, or only the gray-plane pixels
respectively.~%"))
      ((STRING-EQUAL ITEM-STRING " LINE ")
       (FORMAT TYPEOUT-PANE
"~36T-LINE-~%~%The LINE command is used to draw a straight line between two user-specified
points on the drawing pane.  After selecting the LINE command, you click-left 
to establish the starting point of the line, then move the cursor and click-left 
again to set the end point and cause the line to be drawn/flipped/erased between 
those end points.

Note that the drawing mode is in effect and the cursor will draw, erase,
or flip the points in its path, just as if you clicked over each point with 
the mouse.~%"))
      ((STRING-EQUAL ITEM-STRING " SPLINE ")
       (FORMAT TYPEOUT-PANE
"~34T-SPLINE-~%~%The SPLINE command allows you to generate a curve-fitted line among
a number of points established on the drawing pane.  After selecting
the SPLINE command, you establish points on the drawing pane
by clicking left at the appropriate points.  When complete, a right-
click indicates the curve-fitting is to be performed.  A middle-click
will abort the operation.

Note that the drawing mode is in effect and the cursor will draw, erase,
or flip the points in its path, just as if you clicked over each
point with the mouse.~%"))
      ((STRING-EQUAL ITEM-STRING " BOX ")
       (FORMAT TYPEOUT-PANE
"~36T-BOX-~%~%The BOX command allows you to affect all pixels within a rectangle
within the drawing pane.  You are prompted for the bounds
of the rectangle by clicking-left over two opposite corners of the
box. 

Note that the drawing mode is in effect and the cursor will draw, erase,
or flip the points in its path, just as if you clicked over each
point with the mouse.~%"))
      ((STRING-EQUAL ITEM-STRING " MOVE BOTH ")
       (FORMAT TYPEOUT-PANE
"~33T-MOVE BOTH-~%~%The MOVE BOTH command allows you to move the character within the 
drawing pane.  You establish two reference points for the move,
first the move-from point, then the move-to point.  The character
in the drawing pane is then moved that relative distance.~%"))
      ((STRING-EQUAL ITEM-STRING " MOVE GRAY ")
       (FORMAT TYPEOUT-PANE
"~33T-MOVE GRAY-~%~%The MOVE GRAY command allows you to move the character within the
gray plane of the drawing pane.  You establish two reference
points for the move, first the move-from point, then the move-to point.
The character in the gray plane of the drawing pane is then moved that
relative distance.~%"))
      ((STRING-EQUAL ITEM-STRING " MERGE GRAY ")
       (FORMAT TYPEOUT-PANE
"~32T-MERGE GRAY-~%~%The MERGE GRAY command merges all pixels in the gray plane into the black plane.
The gray plane is left unchanged.  You must ERASE GRAY if the gray
plane is to be cleared.~%"))
      ((STRING-EQUAL ITEM-STRING " MERGE MENU ")
       (FORMAT TYPEOUT-PANE
"~32T-MERGE MENU-~%~%The MERGE MENU command pops up a menu of merge options including:

    Copy  -  copy the gray plane into the black plane, destroying all previous
               black plane information;

    Set bits  -  sets bits in the black plane corresponding to bits set in the
                   gray plane without destroying previous black plane data;

    Clear bits  -  clears bits in the black plane corresponding to bits that
                     were set in the gray plane; and

    Flip bits  -  change the state of the bits in the black plane corresponding
                    to bits set in the gray plane.~%"))
      ((STRING-EQUAL ITEM-STRING " SWAP PLANES ")
       (FORMAT TYPEOUT-PANE
"~32T-SWAP PLANES-~%~%The SWAP PLANES command swaps data between the black plane and the gray plane.~%")))
    (SEND TYPEOUT-PANE :SET-CURRENT-FONT 0)
    (TERPRI))) 

(DEFMETHOD (FED :DOCUMENT-DETAILS-SCREEN) (item-string &rest ignore)
   "Displays information when the user clicks on specific items in the typeout pane."
   (DECLARE (SPECIAL typeout-pane))
   (LET ((*terminal-io* typeout-pane))
     (SEND typeout-pane :set-current-font 1)
     (FORMAT typeout-pane "~%~30TFONT EDITOR HELP~%")
						 ;; screen additional documentation
     (COND ((STRING-EQUAL item-string " HOME ")
	    (FORMAT typeout-pane
"~36T-HOME-~%~%
The HOME command places the character box in the approximate center of the
drawing pane and moves the black and gray planes correspondingly.~%"
		    ))
	   ((STRING-EQUAL item-string " REDISPLAY ")
	    (FORMAT typeout-pane
"~33T-REDISPLAY-~%~%The REDISPLAY command causes the entire screen to be redrawn.~%"
		    ))
	   ((STRING-EQUAL item-string " SET SCALE ")
	    (FORMAT typeout-pane
"~33T-SET SCALE-~%~%
The SET SCALE command allows you to adjust the size of the grid spacings
on the drawing pane.  

The default is 14, that is, each single pixel is represented by a 14 pixel by 14 pixel 
black or gray box on the drawing pane.  The scale can be increased to 50 or 
reduced to 1, but values below 6 cause the grid to be supressed.~%"
		    ))
	   ((STRING-EQUAL item-string " CHANGE MODE ")
	    (FORMAT typeout-pane
"~31T-CHANGE MODE-~%~%
The CHANGE MODE command changes the current mode of the drawing mouse when 
it is in the drawing pane.

Available modes are:

    FLIP  -  The current state of the pixel under the mouse is inverted
             (that is, if on, is turned off; if off, is turned on).

    DRAW  -  The pixel under the mouse is turned on.
 
    ERASE -  The pixel under the mouse is turned off.

When you select the CHANGE MODE command, the current mode of the drawing mouse 
toggles between the three available modes, the mouse-cursor changes accordingly,
and the name of the current mode is updated in the label pane.~%"
                    ))
	   ((STRING-EQUAL item-string " CHANGE VARS ")
	    (FORMAT typeout-pane
"~31T-CHANGE VARS-~%~%
The CHANGE VARiables command allows you to change the variables that
control how the fonts are displayed.~%"
                    ))
	   ((STRING-EQUAL item-string " LEFT, UP, RIGHT, DOWN ")
	    (FORMAT typeout-pane
"~27T-LEFT, UP, RIGHT, DOWN-~%~%
The LEFT, UP, RIGHT, and DOWN commands move the character box and black and
gray planes, in the direction indicated by their names, eight grid
positions at a time.~%"
		    ))
	   ((STRING-EQUAL item-string " NEW MOUSE ")
	    (FORMAT typeout-pane
"~33T-NEW MOUSE-~%~%
The NEW MOUSE command allows you to select from a collection of mouse-cursors,
the particular mouse-cursor you wish to use for editing on the drawing pane.  

There are three mouse-cursors defined for the drawing pane, one for the draw mode, 
one for the erase mode, and one for the flip mode.  You may select different 
cursor characters for each of these modes or may use the same character for 
two or all three modes.~%"
		    ))
	   
	   )
     (SEND typeout-pane :set-current-font 0)
     (TERPRI)))


