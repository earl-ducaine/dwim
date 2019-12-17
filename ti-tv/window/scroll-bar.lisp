;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(CPTFONT HL12B TR12B) -*-

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
;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -----------------------------------------------------------------------------------------------------
;;;  02-07-89  LG     Rationalize mX code in draw-hollow-rectangle to fit new Mac code.
;;;  11-15-88  LG     Correct height/width for the mX in draw-hollow-rectangle just like for the Explorer now
;;;  		    that we are asking the Mac to draw hollow rectangles correctly.
;;;  10-24-88  MAY    Changed :after :change-of-size-or-margins to ALWAYS call :compute-region since
;;;                       :decide-if-scrolling-necessary does NOT always cause a recursive
;;;                       call to :after :change-of-size-or-margins from :redefine-margins when on 
;;;                       MAC and apple-resizing the screen with a SCROLL-BAR-ON-OFF scroll-bar displayed.
;;;  10-07-88  LG       Fixed Mac scrolling to handle a single left-click in the scroll box correctly.
;;;  09-07-88  MAY    Added DEFWRAPPER (scroll-bar-mixin :scroll-bar-mode) to prevent drawing scroll-bar 
;;;                       gray bar when adding scroll-bar to exposed window with bit-save array.
;;;		       Changed scroll-bar-draw-icon to fix drawing of more icons in pane with reverse video-p 
;;;                       set to T. Also removed kludge to draw extra icon edge for bug which has been fixed.
;;;                       Changed (scroll-bar-mixin :enable-scrolling-p) to match documentation and fix keybd hang 
;;;                       on left edge with no scroll bar (:off). Also update doc-string to match manual.
;;;		       Rewrote (scroll-bar-mixin :after :change-of-size-or-margins) to be understandable
;;;		       and to get rid of useless return values. No change in functionality.
;;;                       Changed Explorer-style-scroll-bar-update to draw 1 more pixel in top right corner of 
;;;		       bottom icon box. Most changes were for SPR 8504.
;;;  07/01/88  MAY     Make the BOTTOM of the mouse going below the window bottom trigger a :bump instead
;;;                        of the TOP of the mouse. Fixes SPR 5404. Changed (scroll-bar-mixin :bump)
;;;		       and Explorer-style-mouse-moves. 
;;; 3/5/88    KED       Several misc bug fixes to both Explorer and Mac style scrolling.
;;;  2/7/88    KED       Reworked several aspects of Mac style scrolling from my original. Its now
;;;                        faster, incorporates more Explorer style features, is more intuitive, has
;;;                        better mouse documentation, and has fewer graphical defects. Also added
;;;                        digital readout of page number in the scroll bar.
;;;  01/22/88  LG        Fixed positions of Explorer-style and Mac-style scroll bars.  Explorer-style
;;;                        scrolling looks and acts exactly like it did in Release 3.
;;;  01/05/88  LG	        Incorporated Mac-type-scrolling's logic based on run-time checking of 
;;;                        *scrolling-style*.
;;;       8/87  KED      Original mac style scrolling written in separate file.
;;;  11/30/87  PMH      Modified the scroll-bar-mixin defflavor so ICON width, height, lines, and
;;;                        delay would get an initial value.
;;;                        Also got rid of the get methods for width, lines, and delay.  
;;;                        Also added an :after method to :set-scroll-bar-icon-width to do a
;;;                        :redefine-margins.  SPR#6535
;;;  8/27/87  KWW       Color support - changed ALU-IOR to CHAR-ALUF; wrapped prepare-color around drawing operation
;;;  5/18/87  KDB	       Fixed "blank window" bug. Don't redefine margins in :AFTER :REFRESH-MARGINS 
;;;                         ever again.
;;;  3/24/87  GRH        Fix :enable-scrolling-p which got broken in a previous change.
;;;		        This makes bump scrolling work again in the Inspector and other places.
;;;   2/19/87  KDB       Fixed :SCROLL-NEXT-LINE to permit scrolling of last line to top of screen.
;;;                         While there, optimized  it and and :SCROLL-PREVIOUS-LINE by avoiding 
;;;                         unnecessary DO bindings, etc. Added some documentation.
;;;   2/19/87  KDB       Fixed scroll-bar enable bug. Scroll bar was not drawn unless more than
;;;                         a full page of info was there. Now invoked if more than 0 lines.
;;;   1/29/87  KDB	Corrected :MOUSE-R-1 documentation to be "This line to bottom"
;;;  01/19/87  KK         Modified (scroll-bar-mixin :scroll-bar-manage-drawing) to check output-hold-flag
;;;                             before drawing. Fixes SPR# 2969.
;;;  12/31/86 KDB        Reimplemented and reformatted *scroll-bar-who-line-documentation* items to be
;;;                           keywords rather than strings. Items now reverse when handedness of mouse
;;;                            changes.
;;;  11/25/86  TWE	Changed references to %draw-line to use either %draw-rectangle
;;;			or %draw-shaded-triangle, as appropriate.  Also added hooks for
;;;			color support.
;;;  11/20/86  GRH	Improved some comments.
;;;  11/13/86  GRH	Tweaked the scroll-bar who-line-doc-string.
;;;  11/13/86  GRH	Inserted a sys: package prefix for mouse-buttons-buffer-index.
;;;  11/13/86  TWE	Inserted a sys: package prefix for %draw-line and %draw-rectangle.
;;;  10/17/86  GRH	Added bump scrolling to top and bottom of scroll-bar,
;;;			changed doc string, and fixed a minor bug.
;;;  10/03/86  GRH	tv:scroll-bar-mixin -> w:scroll-bar-mixin
;;;  10/01/86  GRH	Fixed a bug in drawing the scroll-bar edge border.
;;;  10/01/86  GRH	Added some ignored init keywords to scroll-bar-mixin for compatibility.
;;;  09/30/86  GRH	Fixed bug in :decide-if-scrolling-necessary
;;;  09/29/86  GRH	Several things.
;;;			Added scroll-bar-mode to disable scroll-bar.
;;;			Added scroll-bar-on-off functionality like the old mixin.
;;;			Added an initial repeat delay, to not confuse long clicks with holds.
;;;  07/28/86  TWE	Modified references to the pixel functions to use ARRAY-DIMENSION
;;;			instead.


#|
This file defines the scroll-bar-mixin flavor and its methods.
|#

(DEFPARAMETER *scrolling-style* :Explorer
  "If :Mac, all scrolling uses a Macintosh-style scroll bar and button clicks.
 If :Explorer,all scrolling uses an Explorer-style scroll bar, button clicks and bumping.
 This variable's value should never be changed directly.  Always use the function
 set-scrolling-style.")

(defparameter *scroll-bar-default-mode* :medium
  "Default display mode to use for all scroll-bar windows.
 Value can be :minimum, :medium, or :maximum.
	NIL - Permanently disable the scroll-bar.
	:minimum - Only show icons and bar when bumped.
	:medium - Show icons, but only show bar when bumped.
	:maximum - Always show icons and bar.")

(defparameter *scroll-bar-default-side* :left
  "Default side of window to allocate scroll-bar.  Value can be :left or :right.")

(defparameter *scroll-bar-default-icon-width* 7.
  "Default width for scroll-bar.  This does not include the three pixels 
 used to draw the scroll-bar box and the region edge border.")

(defparameter *scroll-bar-default-icon-height* 9.
  "Default height of scroll-bar icon area.")

;;;  Added for MacScrolling, KED 8/87
(DEFVAR *Mac-scroll-bar-default-height* 16
  "Default height of the scroll bar")

(defparameter *scroll-bar-shade* 50%-gray
  "Shade to use when drawing scroll-bars.")

(DEFVAR *scroll-bar-mouse-blinker* (LIST mouse-glyph-north-west-arrow 0 0) 
  "Variable holding mouse blinker and offset being used while mouse is in the scroll region.")

(DEFVAR *scroll-bar-previous-mouse-blinker* (LIST mouse-glyph-north-west-arrow 0 0) 
  "Variable holding mouse blinker and offset in effect when scroll region was entered.")

(defparameter *Explorer-scroll-bar-default-clicks*
	      '(
		#\mouse-l :scroll-next-line
		#\mouse-l-2 :scroll-next-page
		#\mouse-m  :scroll-jump
		#\mouse-r :scroll-previous-line
		#\mouse-r-2 :scroll-previous-page)
  "The methods to call for mouse-clicks when the scroll-bar is active.")

;;;  Added for MacScrolling, KED 8/87
(defparameter *Mac-scroll-bar-default-clicks*
	      '(#\mouse-l :scroll-left-button
		#\mouse-l-2 :scroll-left-button-2
		#\mouse-m :scroll-middle-button
		#\mouse-m-2 :scroll-middle-button-2
		#\mouse-r :scroll-right-button
		#\mouse-r-2 :scroll-right-button-2)
  "The methods to call for mouse-clicks when the scroll-bar is active.")

(DEFVAR *scroll-bar-default-clicks* *Explorer-scroll-bar-default-clicks*
  "The methods to call for mouse-clicks when the scroll-bar is active.") 

;; This string hardcodes the layout since the layout currently given by the system is poor. Obsolete.
;;;(defparameter *scroll-bar-who-line-documentation*
;;;	      (string-append "L: This line to top,  LH: Continuous next line,  L2: Next page,  "
;;;			     "M: To fraction of buffer,  MH: Drag lines,  "
;;;			     #\return
;;;			     "R: Top line to here,  RH: Continuous previous line,  R2: Previous page.    "
;;;			     "Bump top or bottom for single line scrolling.")
;;;  "The who-line-documentation-string when in the scroll-bar.")

;;; This is the new implementation. We use keywords instead of strings. :No-Comma prevents comma insertion.
(defparameter *scroll-bar-who-line-documentation*
		    '(:MOUSE-L-1 "This line to top        "
		    :MOUSE-L-HOLD "Continuous next line        "
		    :MOUSE-L-2 "Next page       "
		    :MOUSE-M-1 "To fraction of buffer  "
		    :MOUSE-m-HOLD "Drag lines
"
		:MOUSE-R-1 "Top line to here       "
		:MOUSE-R-HOLD "Continuous previous line    "
		:MOUSE-R-2 "Previous page."
		:no-comma "  "
		 :documentation  "   Bump top or bottom for single line scrolling."
		 )
  "The who-line-documentation-string when in the scroll-bar.")
(defparameter *scroll-bar-default-delay-time* 0
  "How long to sleep per iteration when doing continuous scrolling.
 This is needed for utilities that need to do a process switch to 
 effect their scrolling.")


;;; Added for MacScrolling, KED 8/87
(DEFPARAMETER *Mac-scroll-bar-top-icon-documentation*
	      `(:MOUSE-L-1     ""
		:MOUSE-L-HOLD  "Previous Line  "	       
		:MOUSE-M-1     ""
		:MOUSE-M-HOLD  "Previous Lines  "
		:MOUSE-R-HOLD  "Drag This Line.     "
		:no-comma      ""
		:MOUSE-L-2     "Top of Form  "
		:MOUSE-M-2     "This Line to Top  "
		:MOUSE-R-2     "Top Line to Here.")
  "Who line documentation string used when mouse is in the top icon of scroll region.")
                          
(DEFPARAMETER *Mac-scroll-bar-upper-box-documentation*
	      `(:MOUSE-L-1     ""
		:MOUSE-L-HOLD  "Previous Page  "
		:MOUSE-M-1     ""
		:MOUSE-M-HOLD  "Previous Lines  "
		:MOUSE-R-HOLD  "Drag This Line.     "
		:no-comma      ""
		:MOUSE-L-2     "Fraction of Form  "
		:MOUSE-M-2     "This Line to Top  "
		:MOUSE-R-2     "Top Line to Here.")
   "Who line documentation string used when mouse is above the scroll bar but in the scroll box.")

(DEFPARAMETER *Mac-scroll-bar-documentation*
	      `(:MOUSE-L-HOLD  "Thumb  "
		:MOUSE-M-HOLD  "Real Time Thumb  "
		:MOUSE-R-HOLD  "Drag This Line.     "
		:no-comma      ""
		:MOUSE-L-2     "Fraction of Form  "
		:MOUSE-M-2     "This Line to Top  "
		:MOUSE-R-2     "Top Line to Here.")
   "Who line documentation string used when mouse is in the scroll bar.")

(DEFPARAMETER *Mac-scroll-bar-lower-box-documentation*
	      `(:MOUSE-L-1     ""
		:MOUSE-L-HOLD  "Next Page  "
		:MOUSE-M-1     ""
		:MOUSE-M-HOLD  "Next Lines  "
		:MOUSE-R-HOLD  "Drag This Line.     "
		:no-comma      ""
		:MOUSE-L-2     "Fraction of Form  "
		:MOUSE-M-2     "This Line to Top  "
		:MOUSE-R-2     "Top Line to Here.")
   "Who line documentation string used when mouse is below the scroll bar but in the scroll box.")

(DEFPARAMETER *Mac-scroll-bar-bottom-icon-documentation*
	      `(:MOUSE-L-1     ""
		:MOUSE-L-HOLD  "Next Line  "
		:MOUSE-M-1     ""
		:MOUSE-M-HOLD  "Next Lines  "
		:MOUSE-R-HOLD  "Drag This Line.     "
		:no-comma      ""
		:MOUSE-L-2     "Bottom of Form  "
		:MOUSE-M-2     "This Line to Top  "
		:MOUSE-R-2     "Top Line to Here.")
  "Who line documentation string used when mouse is in the bottom icon of scroll region.")

(defparameter *scroll-bar-initial-delay* 15.
  "How long to wait to distinguish between a mouse click and a mouse hold.") 

(defparameter *scroll-bar-default-lines* 1
  "How many lines to scroll per iteration when doing continuous scrolling.")

(defparameter *scroll-bar-char-index* mouse-glyph-thick-up-down-arrow
  "Index into the mouse font of the character to use for the mouse when the scroll-bar
 is entered.")

(defparameter *scroll-bar-char-x-offset* 6
  "The x offset for the scroll-bar mouse character blinker.")

(defparameter *scroll-bar-char-y-offset* 0
  "The y offset for the scroll-bar mouse character blinker.")

;;; Up-down arrow ICON definitions. Added for MacScrolling, KED 8/87
(DEFCONSTANT *hollow-arrow* (LIST #x00000020 #x00000050 #x00000088 #x00000104
				  #x00000202 #x0000078F #x00000088 #x00000088
				  #x00000088 #x00000088 #x000000F8)
  "Bit image for an 11x11 hollow up-arrow scroll-bar icon.")

(DEFCONSTANT *solid-arrow* (LIST #x00000020 #x00000070 #x000000F8 #x000001FC
				 #x000003FE #x000007FF #x000000F8 #x000000F8
				 #x000000F8 #x000000F8 #x000000F8)
  "Bit map for an 11x11 filled in up-arrow scroll-bar icon.")

(DEFPARAMETER *scroll-bar-hollow-up-arrow*
	      (MAKE-ARRAY 11 :element-type '(unsigned-byte 32)
			  :initial-contents *hollow-arrow*
			  :leader-length 4.
			  :leader-list '(nil 6. 11. 11.)))	   ;(nil arrow-point-x-offset arrow-width arrow-height)
(DEFPARAMETER *scroll-bar-hollow-down-arrow*
	      (MAKE-ARRAY 11 :element-type '(unsigned-byte 32)
			  :initial-contents (REVERSE *hollow-arrow*)
			  :leader-length 4.
			  :leader-list '(nil 6. 11. 11.)))
(DEFPARAMETER *scroll-bar-solid-up-arrow*
	      (MAKE-ARRAY 11 :element-type '(unsigned-byte 32)
			  :initial-contents *solid-arrow*
			  :leader-length 4.
			  :leader-list '(nil 6. 11. 11.)))
(DEFPARAMETER *scroll-bar-solid-down-arrow*
	      (MAKE-ARRAY 11 :element-type '(unsigned-byte 32)
			  :initial-contents (REVERSE *solid-arrow*)
			  :leader-length 4.
			  :leader-list '(nil 6. 11. 11.)))

;;; Changed for MacScrolling, KED 8/87
;;;  The Mac scroll region is the complete area taken up by scrolling. Includes the border, icons, box, and bar.
;;;   It always extends from the top margin to the bottom margin of the window. This is unlike Explorer
;;;   style where it extends from the top of the window to the bottom. I think this is a bug in Explorer
;;;   style scrolling since it trashes the labels on things like menus.
;;;  The scroll bar is the small box representing the current scroll position. The Mac refers to it as the thumb.
;;;  The scroll box is the area the scroll bar travels in. It is divided into an upper and lower box by the scroll bar.
(DEFSTRUCT (scroll-bar-region (:type :list))
  (side :left :type keyword)		   ;:left or :right
  (width 0 :type fixnum)		   ;width of scroll-bar-region
  (left 0 :type fixnum)			   ;left margin including scroll-bar, negative if scroll bar is on the right
  (top 0 :type fixnum)			   ;top margin - set same as window top margin
  (right 0 :type fixnum)		   ;right margin including scroll-bar, always negative
  (bottom 0 :type fixnum)		   ;bottom margin - set same as window margin but negative
  ;; Remainder are new variables added for Mac type scrolling. They are included
  ;; in the defstruct so that they can be computed once then used by all the drawing
  ;; routines rather than recalculated each time. This way is faster and all scroll bar drawers agree
  ;; on the scroll region parameters.
  (style *scrolling-style* :type symbol)   ;name of scrolling style this window is using.
  (valid-p nil :type symbol)		   ;non-Nil if region parameters have been calculated
  (height 0 :type fixnum)		   ;scroll region height
  (left-edge 0 :type fixnum)		   ;y position of the left edge of the scroll region with respect to the window
  (icon-box-height 0 :type fixnum)	   ;height of box containing the arrow icons
  (bar-width 0 :type fixnum)		   ;width of the scroll bar and box
  (bar-height 0 :type fixnum)		   ;height of the scroll bar including it's border
  (bar-left 0 :type fixnum)		   ;x position of the scroll bar and box relative to the window
  (bar-top 0 :type fixnum)		   ;current y position of the scroll bar relative to the window 
  (box-height 0 :type fixnum)		   ;height of the scroll box
  (box-top 0 :type fixnum)		   ;y position of the scroll box top relative to the window
  (box-bottom 0 :type fixnum)		   ;y position of the scroll box bottom relative to the window
  (bar-range 0 :type fixnum)		   ;range over which the scroll bar can move (0 to ...)
  (scroll-bar-spare 0 :type fixnum)
  )

(defflavor scroll-bar-mixin
           ((scroll-bar-mode :DEFAULT)
	    (scroll-bar-icon-width *scroll-bar-default-icon-width*)
	    (scroll-bar-icon-height  *scroll-bar-default-icon-height*)
	    (scroll-bar-draw-edge-p t)
	    (scroll-bar-lines *scroll-bar-default-lines*)
	    (scroll-bar-delay-time *scroll-bar-default-delay-time*)
	    (scroll-bar-on-off)
	    (scroll-bar-making-decision)
	    (scroll-bar-region)
	    (scroll-bar-active-state)
	    (scroll-bar-draw-state)
            (scroll-bar-color *default-scroll-bar-color*))
           ()
  (:required-flavors essential-window
		     borders-mixin)
  ;; The :scroll-position method does more work than is usually necessary.
  ;; It would be more efficient to require four simpler methods instead which
  ;; each returned one of the four values currently returned by :scroll-position.
  ;; However this would be less compatible with the old scroll-bar.

;;; made scroll-bar-color settable and inittable
 
  (:required-methods :scroll-to
		     :scroll-position
		     :new-scroll-position)
  (:init-keywords :scroll-bar-side
		  ; These are obsolete init options, and are ignored.
		  :scroll-bar
		  :scroll-bar-always-displayed
		  :flashy-scrolling-region
		  :margin-scroll-regions)
  (:initable-instance-variables scroll-bar-color
				scroll-bar-mode scroll-bar-icon-width scroll-bar-icon-height
				scroll-bar-lines  scroll-bar-delay-time)
  (:settable-instance-variables 
    scroll-bar-mode
    scroll-bar-icon-width
    scroll-bar-icon-height
    scroll-bar-draw-edge-p
    scroll-bar-lines    
    scroll-bar-delay-time 
    scroll-bar-on-off
    scroll-bar-color)
  (:documentation :mixin
    "Provides a scroll-bar and enables scrolling via the mouse.   This flavor requires 
 flavors essential-window and borders-mixin.  Additionally the methods :scroll-to,
 :scroll-position, and :new-scroll-position as defined for text-scroll-windows must
 also be defined.
   These instance variables are defined.
 scroll-bar-mode - Setting this overides the default value given in *scroll-bar-default-mode*.
                     Set this to nil to completely turn off the scroll-bar.
 scroll-bar-icon-width - Overides the default value in *scroll-bar-default-icon-width*.
 scroll-bar-icon-height - Overides the default value in *scroll-bar-default-icon-height*.
 scroll-bar-draw-edge-p - Draw a line to show the edge of the scroll-bar region.
 scroll-bar-lines - Overides the default value in *scroll-bar-default-lines*.
 scroll-bar-delay-time - Overides the default value in *scroll-bar-default-delay-time*.
 scroll-bar-region - Scroll-bar-region structure.
 scroll-bar-active-state - :bump when bumped, :region when mouse is over scroll-region, else nil.
 scroll-bar-draw-state - Internal variable to save redrawing icons when not necessary.
 scroll-bar-on-off - Turn scroll-bar on/off when receiving a :decide-if-scrolling-necessary
		     message allocating/deallocating the margin space as necessary.
		     This is useful for menus which do not usually need a scroll-bar.
 scroll-bar-making-decision - Internal variable to support scroll-bar-on-off feature.
   This init keyword is also defined.
 scroll-bar-side - Put the scroll-bar for this window on the :left or :right.
 scroll-bar-color - Overides the default value for the color value."))

;; Temporarily needed since this used to be in the tv package.
(export 'scroll-bar-mixin 'tv)

(DEFUN set-scrolling-style (&optional (style :explorer))
  (check-arg style (OR (EQ style :mac) (EQ style :explorer)) ":MAC or :EXPLORER")
  (CASE style
    (:mac
     (SETF *scroll-bar-default-mode* :maximum)
     (SETF *scroll-bar-shade* tv:25%-gray)
     (SETF *scroll-bar-default-icon-width* 13.)
     (SETF *scroll-bar-default-icon-height* 13.)
     (SETF *scroll-bar-default-lines* 1.)
     (SETF *scroll-bar-default-clicks* *Mac-scroll-bar-default-clicks*))
    (:explorer
     (SETF *scroll-bar-default-mode* :medium)
     (SETF *scroll-bar-shade* tv:50%-gray)
     (SETF *scroll-bar-default-icon-width* 7.)
     (SETF *scroll-bar-default-icon-height* 9.)
     (SETF *scroll-bar-default-lines* 1.)
     (SETF *scroll-bar-default-clicks* *Explorer-scroll-bar-default-clicks*)
     ))
  (SETF *scrolling-style* style))

(DEFUN force-compliance-with-current-scrolling-style ()
  (DECLARE (self-flavor scroll-bar-mixin))
  (LET ((side (scroll-bar-region-side scroll-bar-region)))
    (SETF scroll-bar-icon-width *scroll-bar-default-icon-width*
	  scroll-bar-icon-height *scroll-bar-default-icon-height*
	  scroll-bar-draw-edge-p t
	  scroll-bar-lines *scroll-bar-default-lines*
	  scroll-bar-delay-time *scroll-bar-default-delay-time*
	  scroll-bar-color *default-scroll-bar-color*)
    (SETF scroll-bar-region (make-scroll-bar-region
			      :side side))
    (SEND self :redefine-margins)))

(DEFMETHOD (scroll-bar-mixin :before :expose) (&rest ignore)
  "Switch window's scrolling style into compiance with *scrolling-style* 
 if it isn't already."
  ;; (user:remember-call :expose scroll-bar-region)
  (UNLESS (EQ (scroll-bar-region-style scroll-bar-region) *scrolling-style*)
    (force-compliance-with-current-scrolling-style)))

(DEFMETHOD (scroll-bar-mixin :before :select) (&rest ignore)
  "Switch window's scrolling style into compiance with *scrolling-style* 
 if it isn't already."
  ;; (user:remember-call :expose scroll-bar-region)
  (UNLESS (EQ (scroll-bar-region-style scroll-bar-region) *scrolling-style*)
    (force-compliance-with-current-scrolling-style)))

;;;  Added for MacScrolling, KED 8/87
(DEFUN draw-shaded-rectangle (width height x y alu window pattern-or-nil)
  "Draws a rectangle shaded with pattern."
  (IF (mac-window-p window)
      (send-draw-rectangle width (1- height) x y pattern-or-nil alu window nil) 
    (LET ((right (+ x width -1))
	  (bottom (+ y height -1)))
      (sys:%draw-shaded-triangle x bottom x y right y 
				 alu t t t pattern-or-nil window)
      (sys:%draw-shaded-triangle x bottom right y right bottom 
				 alu nil t t pattern-or-nil window))))

;;;  Added for MacScrolling, KED 8/87
(DEFUN draw-hollow-rectangle (width height x y border alu window &optional (pattern w:100%-gray))
  "Draws a hollow rectangle with a border of thickness 'border' pixels."
  (IF (mac-window-p window)
      (LET ((*dont-clip-at-the-margins* t))
	(send-DrawHollowRectangle x y width height border pattern alu window))
    ;;... else
    (LET ((right (+ x width (- border)))
	  (bottom (+ y height (- border))))
      (draw-shaded-rectangle border (1- height) x y alu window pattern)
      (draw-shaded-rectangle (1- width) border (1+ x) y alu window pattern)
      (draw-shaded-rectangle (1- width) border x bottom alu window pattern)
      (draw-shaded-rectangle border (1- height) right (1+ y) alu window pattern))))

;;; Added for MacScrolling, KED 8/87
(DEFCONSTANT *Mac-scroll-region-border* 1.
  "The width of the border around the scroll bar region.")

(defmethod (scroll-bar-mixin :scroll-bar-icon-height) ()
  "Return scroll-bar-icon-height if non nil, else the global default icon height."
  (min scroll-bar-icon-height
       (floor height 2)))

(defmethod (scroll-bar-mixin :after :set-scroll-bar-icon-width)(&rest ignore)
  (send self :redefine-margins))

(defmethod (scroll-bar-mixin :scroll-bar-mode) ()
  "If scroll-bar has been bumped return :maximum, else
 if non-nil return scroll-bar-mode, else return
 the global default display mode."
  (or (and scroll-bar-active-state
	   (eq scroll-bar-active-state :bump)
	   :maximum)
      (if (eq scroll-bar-mode :default)
	  *scroll-bar-default-mode*
	  scroll-bar-mode)))

;; may 9-7-88 added to prevent drawing scroll-bar gray bar when
;; adding scroll-bar to exposed window with bit-save array.	
(DEFWRAPPER (scroll-bar-mixin :scroll-bar-mode) (IGNORE . BODY)
  `(LET ((mode ,@BODY))
     (IF (AND scroll-bar-making-decision
	      (EQ mode :maximum)
	      (SEND self :bit-array))
	 :medium ;; don't draw gray bar when conditions are rapidly changing.
	 mode)))

(DEFMETHOD (scroll-bar-mixin :before :init) (init-plist)
  "Create this window's scroll-bar-region structure, setting on which side of the window the scroll
bar is to appear from the INIT-PLIST."
  (SETF scroll-bar-region (make-scroll-bar-region
			    :side (or (get init-plist :scroll-bar-side)
				      *scroll-bar-default-side*))))

(defmethod (scroll-bar-mixin :scroll-bar-on?) ()
  "Return t if scroll-bar-on-off is not :off."
  (if scroll-bar-on-off
      (or (eq scroll-bar-on-off :on)
	  (and (setq scroll-bar-on-off :off)
	       nil))
      t))

(DEFUN scroll-bar-area-width (window)
"Return the width for the scroll bar
 region when scroll-bar-mode, else nil."
  (LET ((w (SEND window :scroll-bar-icon-width)))
    (IF (AND (> w 0)
	     (SEND window :scroll-bar-mode))
	(CASE *scrolling-style*
	  (:Mac (+ w *Mac-scroll-region-border* *Mac-scroll-region-border*))
	  (:Explorer (+ w 3)))
	0)))

;;; Include scroll bar width in the border-margin-area width for Explorer-style
;;; scrolling...
(DEFMETHOD (scroll-bar-mixin :around :compute-border-margin-area-margins)
	   (cont mt ignore spec lm tm rm bm)
  "Wrap around this method to allocate space for the scroll-bar 
 between the border and the border margin area." 
  (LET ((side (scroll-bar-region-side scroll-bar-region))      ; set up at init
	(region-width (IF (SEND self :scroll-bar-on?)
			  (scroll-bar-area-width self)
			  0))
	left
	right)
    (WHEN (AND (EQ *scrolling-style* :Mac)
	       (PLUSP region-width))
      ;; Bury the Mac scroll region border into the window border if it has one.
      (IF (EQ (scroll-bar-region-side scroll-bar-region) :left)
	  (SETF lm (MAX 0 (- lm *mac-scroll-region-border*)))
	  (SETF rm (MAX 0 (- rm *mac-scroll-region-border*)))))
    (SETF left lm
	  right (- rm))
    ;; Unfortunately HEIGHT and WIDTH are not available now
    ;; so when the region is on the right, LEFT and RIGHT are
    ;; negative to indicate that they still need to be subtracted 
    ;; from WIDTH.  BOTTOM is always negative.
    (IF (EQ side :left)
	(SETQ lm (+ lm region-width)
	      right (1- lm))
	(SETQ rm (+ rm region-width)
	      left (- rm)))
    (SETF (scroll-bar-region-width scroll-bar-region) region-width
	  (scroll-bar-region-left scroll-bar-region) left 
	  (scroll-bar-region-right scroll-bar-region) right)
    (WHEN (EQ *scrolling-style* :Explorer)
      (SETF (scroll-bar-region-top scroll-bar-region) tm
	    (scroll-bar-region-bottom scroll-bar-region) (- bm))))
  
  (FUNCALL-WITH-MAPPING-TABLE cont mt
			      :compute-border-margin-area-margins
			      spec lm tm rm bm))

(DEFMETHOD (scroll-bar-mixin :compute-region) ()
  ;; Changed for MacScrolling, KED 8/87
  
  ;; Width and Height are sheet instance variables
  ;; Scroll-bar-region-side and scroll-bar-region-width are set at init from global variable defaults.
  ;; Scroll-bar-region-left and -right are set by the :compute-margins mixin.
  ;; Scroll-bar-region-bar-top is set and maintained by the :scroll-bar-update mixin.

  ;; => => => => =>  H A C K   A L E R T  <= <= <= <= <=
  (unless (= (length scroll-bar-region)
	     (length (fourth (get 'w:scroll-bar-region 'si:defstruct-description))))
    (SETF scroll-bar-region
	  (APPEND scroll-bar-region
		  (MAKE-LIST (- (length (fourth (get 'w:scroll-bar-region 'si:defstruct-description)))
				(length scroll-bar-region))))))

  (SETF (scroll-bar-region-style scroll-bar-region) *scrolling-style*)
  (WHEN (EQ *scrolling-style* :Mac)		;KED 3/15/88
    (SETF (scroll-bar-region-top scroll-bar-region)
	  (MAX 0 (- (w:sheet-top-margin-size self) *Mac-scroll-region-border* 1)))
    (SETF (scroll-bar-region-bottom scroll-bar-region)
	  (- (MAX 0 (- (w:sheet-bottom-margin-size self) *Mac-scroll-region-border* 1)))))
  (SETF (scroll-bar-region-height scroll-bar-region)
	(+ height
	   (- (scroll-bar-region-top scroll-bar-region))
	   (scroll-bar-region-bottom scroll-bar-region)))
  (SETF (scroll-bar-region-left-edge scroll-bar-region)
	(+ (scroll-bar-region-left scroll-bar-region)
	   (IF (EQ :left (scroll-bar-region-side scroll-bar-region)) 0 width)))
  (SETF (scroll-bar-region-icon-box-height scroll-bar-region)
	(MIN (SEND self :scroll-bar-icon-height)   ;If not enough room
	     (ASH (- (scroll-bar-region-height scroll-bar-region)
		     (* 4 *Mac-scroll-region-border*))
		  -2)))
  (SETF (scroll-bar-region-bar-width scroll-bar-region)
	(- (scroll-bar-region-width scroll-bar-region) (* 2 *Mac-scroll-region-border*)))
  (SETF (scroll-bar-region-bar-left scroll-bar-region)
	(+ (scroll-bar-region-left-edge scroll-bar-region) *Mac-scroll-region-border*))
  (SETF (scroll-bar-region-box-height scroll-bar-region)
	(MAX 0 (- (scroll-bar-region-height scroll-bar-region)
		  (* 2 (scroll-bar-region-icon-box-height scroll-bar-region))
		  (* 4 *Mac-scroll-region-border*))))
  (SETF (scroll-bar-region-bar-height scroll-bar-region)
	(MIN *Mac-scroll-bar-default-height*   ;If enough room for full size bar
	     (ASH (scroll-bar-region-box-height scroll-bar-region) -1)))   ;If not enough room for full size bar
  (SETF (scroll-bar-region-box-top scroll-bar-region)
	(+ (scroll-bar-region-top scroll-bar-region)
	   (scroll-bar-region-icon-box-height scroll-bar-region)
	   (* 2 *Mac-scroll-region-border*)))
  (SETF (scroll-bar-region-box-bottom scroll-bar-region)
	(+ height
	   (scroll-bar-region-bottom scroll-bar-region)
	   (- (scroll-bar-region-icon-box-height scroll-bar-region))
	   (- (* 2 *Mac-scroll-region-border*))))
  (SETF (scroll-bar-region-bar-range scroll-bar-region)
	(- (scroll-bar-region-box-height scroll-bar-region)
	   (scroll-bar-region-bar-height scroll-bar-region)))
  ;; Okay everything should be set up now
  (SETF (scroll-bar-region-valid-p scroll-bar-region) t))

(defmethod (scroll-bar-mixin :after :refresh-margins) ()
  "Redraw  all scroll-bar stuff when refreshing."
;;;  (if (neq (scroll-bar-area-width self)	
;;;	   (scroll-bar-region-width scroll-bar-region))
;;;      (process-run-function             ;; remove this long standing hack.
;;;	"Resize Scroll-bar"
;;;	#'(lambda (window)
;;;	    (send window :redefine-margins)
;;;	    (send window :scroll-bar-manage-drawing t))
;;;	self)
      (send self :scroll-bar-manage-drawing t))

(defmethod (scroll-bar-mixin :around :who-line-documentation-string) (cont mt ignore)
  "If in the scroll-bar, use scroll-bar documentation, else use default documentation."
  (CASE *scrolling-style*
    (:Mac
      (CASE scroll-bar-active-state
	(:top-icon *Mac-scroll-bar-top-icon-documentation*)
	(:upper-box *Mac-scroll-bar-upper-box-documentation*)
	(:bar *Mac-scroll-bar-documentation*)
	(:lower-box *Mac-scroll-bar-lower-box-documentation*)
	(:bottom-icon *Mac-scroll-bar-bottom-icon-documentation*)
	(:otherwise (funcall-with-mapping-table cont mt :who-line-documentation-string))))
    (:Explorer
     (if scroll-bar-active-state
	 *scroll-bar-who-line-documentation*
        ;; else...
       (funcall-with-mapping-table cont mt :who-line-documentation-string)))))

(defmethod (scroll-bar-mixin :mouse-buttons) (bd x y)
  "Redefine :mouse-buttons to map mouse-click R2 to the scroll-bar when scroll-bar is active."
  (let ((buttons (mouse-character-button-encode bd)))
    (if (and (= buttons #\mouse-r-2)
	     (not scroll-bar-active-state))
        (mouse-call-system-menu)
        (send self :mouse-click buttons x y))))

(defwrapper (scroll-bar-mixin :mouse-click) ((button x y) . body)
  "If in scroll-bar do scroll-bar mouse-click else do default mouse-click."
  `(if scroll-bar-active-state
       (let ((method (getf *scroll-bar-default-clicks* button)))
	 (if method
	     (send self method x y button)
	     (beep)))
      . ,body))
 
;;; Added for MacScrolling, KED 8/87
(DEFMETHOD (scroll-bar-mixin :where-is-mouse?) (y &aux draw-state bar-top)
  "Returns :top-icon, :upper-box, :bar, :lower-box, or :bottom-icon. Or nil if not in scroll region."
  (multiple-value-bind (top-line n-lines ignore n-screen-lines)
      (send self :scroll-position)
    (WHEN (AND (scroll-bar-region-valid-p scroll-bar-region)
	       (SETF bar-top (scroll-bar-region-bar-top scroll-bar-region))
	       (OR (> n-lines n-screen-lines) (PLUSP top-line)))
      (SETF draw-state
	    (COND ((< y (scroll-bar-region-top scroll-bar-region)) nil)
		  ((<= y (scroll-bar-region-box-top scroll-bar-region)) :top-icon)
		  ((<= y bar-top) :upper-box)
		  ((<= y (+ bar-top (scroll-bar-region-bar-height scroll-bar-region))) :bar)
		  ((<= y (scroll-bar-region-box-bottom scroll-bar-region)) :lower-box)
		  ((<= y (+ height (scroll-bar-region-bottom scroll-bar-region))) :bottom-icon) 
		  (t nil)))
      draw-state)))

;;; :scroll-left-button, :scroll-middle-button, :scroll-right-button,
;;; :scroll-left-button-2, :scroll-middle-button-2, :scroll-right-button-2,
;;; Added for MacScrolling, KED 8/87
(DEFMETHOD (scroll-bar-mixin :scroll-left-button) (ignore y &rest ignore)
  "This dispatches to the appropriate action when the left button is pushed
depending on the position of the mouse within the scroll region." 
  (CASE scroll-bar-active-state 
    (:top-icon (SEND self :scroll-lines (- *scroll-bar-default-lines*)))
    (:upper-box (multiple-value-bind (ignore ignore ignore lines-per-page)
		    (send self :scroll-position)
		  (SEND self :scroll-lines (- 1 lines-per-page))))
    (:bar (SEND self :scroll-drag y))
    (:lower-box (multiple-value-bind (ignore ignore ignore lines-per-page)
		    (send self :scroll-position)
		  (SEND self :scroll-lines (- lines-per-page 1))))
    (:bottom-icon (SEND self :scroll-lines *scroll-bar-default-lines*))
    (:otherwise nil)))

(DEFMETHOD (scroll-bar-mixin :scroll-middle-button) (ignore y &rest ignore)
  "This dispatches to the appropriate action when the middle button is pushed
depending on the position of the mouse within the scroll region." 
  (CASE scroll-bar-active-state 
    ((:top-icon :upper-box) (multiple-value-bind (ignore ignore ignore lines-per-page)
				(send self :scroll-position)
			      (SEND self :scroll-lines (- (TRUNCATE lines-per-page 4)))))
    (:bar (SEND self :scroll-drag y :continuous :window))
    ((:lower-box :bottom-icon) (multiple-value-bind (ignore ignore ignore lines-per-page)
				   (send self :scroll-position)
				 (SEND self :scroll-lines (TRUNCATE lines-per-page 4))))
    (:otherwise nil)))

(DEFMETHOD (scroll-bar-mixin :scroll-right-button) (ignore y &rest ignore)
  "This dispatches to the appropriate action when the right button is pushed
depending on the position of the mouse within the scroll region."
  (CASE scroll-bar-active-state 
    ((:top-icon :upper-box :bar :lower-box :bottom-icon)
     (SEND self :scroll-drag y :continuous :line))
    (:otherwise nil)))

(DEFMETHOD (scroll-bar-mixin :scroll-left-button-2) (ignore y &rest ignore)
  "This dispatches to the appropriate action when the left button is pushed twice
depending on the position of the mouse within the scroll region."
  (multiple-value-bind (ignore n-lines ignore ignore)
      (send self :scroll-position)
    (LET ((line (MAX 0
		     (MIN (1- n-lines)
			  (TRUNCATE (* n-lines
				       (FLOAT (/ (- y (scroll-bar-region-box-top scroll-bar-region))
						 (MAX 1 (scroll-bar-region-bar-range scroll-bar-region)))))))))) 
      (CASE scroll-bar-active-state 
	(:top-icon (SEND self :scroll-to 0. :absolute))
	((:upper-box :bar :lower-box) (SEND self :scroll-to line :absolute))
	(:bottom-icon (SEND self :scroll-to (1- n-lines) :absolute))
	(:otherwise nil)))))

(DEFMETHOD (scroll-bar-mixin :scroll-middle-button-2) (ignore y &rest ignore)
  "This dispatches to the appropriate action when the middle button is pushed twice
depending on the position of the mouse within the scroll region."
  (multiple-value-bind (ignore ignore ln-height ignore)
      (send self :scroll-position)
    (CASE scroll-bar-active-state 
      ((:top-icon :upper-box :bar :lower-box :bottom-icon)
       (SEND self :scroll-lines
	     (TRUNCATE (- y (scroll-bar-region-top scroll-bar-region)) ln-height)))
      (:otherwise nil))))

(DEFMETHOD (scroll-bar-mixin :scroll-right-button-2) (ignore y &rest ignore)
  "This dispatches to the appropriate action when the right button is pushed twice
depending on the position of the mouse within the scroll region."
  (multiple-value-bind (ignore ignore ln-height ignore)
      (send self :scroll-position)     
    (CASE scroll-bar-active-state 
      ((:top-icon :upper-box :bar :lower-box :bottom-icon)
       (SEND self :scroll-lines
	     (- (TRUNCATE (- y (scroll-bar-region-top scroll-bar-region)) ln-height))))
      (:otherwise nil))))

;;; Added for MacScrolling, KED 8/87
(DEFMETHOD (scroll-bar-mixin :scroll-lines) (lines)
  "Scrolls 'lines' at a time until button is released.
If lines is positive to next line. If lines is negative scrolls to previous line.
Also makes the approprate icon solid while scrolling."
  (SEND self :scroll-bar-draw-icon :point-up (MINUSP lines) :solid t)
  (multiple-value-bind (top-line n-lines ignore ignore)
      (send self :scroll-position)
    (send self :scroll-to
		(MAX 0 (MIN (- n-lines 1) (setf top-line (+ top-line lines))))
		:absolute)
    (LOOP with delay = (send self :scroll-bar-delay-time)
	  and top = (- n-lines 1)
	  and bottom = 0
	  until (or (> top-line top)
		    (<= top-line bottom)
		    (ZEROP (mouse-buttons)))
	  do
	  (when delay (process-sleep delay))
	  (send self :scroll-to
		(MAX bottom (MIN top (setf top-line (+ top-line lines))))
		:absolute)))
  (SEND self :scroll-bar-draw-icon :point-up (MINUSP lines) :solid nil))

;;; Added for MacScrolling, KED 8/87
(DEFMETHOD (scroll-bar-mixin :scroll-drag)
	   (y-init &optional &key (continuous nil))
  "Drag scrolling. Three modes are available :bar drags the bar, :window drags
the bar and the buffer in real time, and :line drags a line at a time."
  (UNLESS (ZEROP (mouse-buttons))
    (MULTIPLE-VALUE-BIND (ignore n-lines ln-height ignore)
	(SEND self :scroll-position)
      (LOOP with box-top = (scroll-bar-region-box-top scroll-bar-region)
	    with bar-height = (scroll-bar-region-bar-height scroll-bar-region)
	    with bar-width = (scroll-bar-region-bar-width scroll-bar-region) 
	    with bar-range = (scroll-bar-region-bar-range scroll-bar-region)
	    with bar-pattern = (SELECT *scroll-bar-shade*
				 (25%-gray 75%-gray)
				 (12%-gray 88%-gray)
				 (33%-gray 66%-gray)
				 (0%-gray 100%-gray)
				 (75%-gray 25%-gray)
				 (88%-gray 12%-gray)
				 (66%-gray 33%-gray)
				 (100%-gray 0%-gray)
				 (:otherwise (BIT-NOT *scroll-bar-shade*)))
	    with lower-limit = (+ box-top bar-range)
	    with current-bar-top = (scroll-bar-region-bar-top scroll-bar-region)
	    with bar-left = (scroll-bar-region-bar-left scroll-bar-region)
	    with mouse-y-init = (+ y-init
				   (CADR (MULTIPLE-VALUE-LIST
					   (sheet-calculate-offsets self mouse-sheet))))
	    with last-y = mouse-y
	    with lines-to-scroll
	    with erase = nil
	    
	    for y = (MIN lower-limit
			 (MAX box-top
			      (- mouse-y mouse-y-init (- current-bar-top)))) 
	    
	    ;; This just drags the scroll bar while the mouse is down.
	    ;; When the mouse is released the window scrolls to the new bar position.
	    when (OR (NULL continuous) (EQ continuous :bar)) do
	    (UNLESS (EQL y last-y)
	      (tv:prepare-sheet (self)
		(tv:prepare-color (self scroll-bar-color)
		  (IF erase			       ;Do it this way so bar is off as short a time as possible
		      (draw-hollow-rectangle bar-width bar-height 
					     bar-left last-y
					     1. alu-sub self bar-pattern)
		    (SETF erase t))
		  (draw-hollow-rectangle bar-width bar-height 
					 bar-left y
					 1. alu-add self bar-pattern))))
	    (SETF last-y y)
	    (PROCESS-WAIT "Mouse"
	      #'(lambda (y) (OR (NEQ y mouse-y) (ZEROP (mouse-buttons)))) mouse-y)
	    
	    ;; This is like :bar but actually drags the window in real time. Real useful in the inspector.
	    when (EQ continuous :window) do 
	    (SEND self :scroll-to
		  (TRUNCATE (* n-lines
			       (FLOAT (/ (- y box-top) (MAX 1 bar-range)))))
		  :absolute)
	    (PROCESS-WAIT "Mouse"
	      #'(lambda (y) (OR (NEQ y mouse-y) (ZEROP (mouse-buttons)))) mouse-y)
	    
	    ;; This is like grabbing a line and draging it with the mouse. Unlike :bar and :window
	    ;; the line moves in the direction of mouse movement.
	    when (EQ continuous :line) do
	    (WHEN (PLUSP (ABS (SETF lines-to-scroll (TRUNCATE (- last-y mouse-y) ln-height))))
	      (SETF last-y mouse-y)
	      (SEND self :scroll-to lines-to-scroll :relative))
	    (PROCESS-WAIT "Mouse"
	      #'(lambda (y) (OR (NEQ y mouse-y) (ZEROP (mouse-buttons)))) mouse-y)
	    
	    until (ZEROP (mouse-buttons))
	    finally				       ; Cleanup for :bar case
	    (WHEN (OR (NULL continuous) (EQ continuous :bar))
	      (tv:prepare-sheet (self)
		(tv:prepare-color (self scroll-bar-color)
		  (draw-hollow-rectangle bar-width bar-height 
					 bar-left last-y
					 1. alu-sub self bar-pattern)))
	      (UNLESS (EQL y current-bar-top) 
		(SEND self :scroll-to
		      (TRUNCATE (* n-lines
				   (FLOAT (/ (- y box-top) (MAX 1 bar-range)))))
		      :absolute)))))))

(defmethod (scroll-bar-mixin :around :mouse-moves) (cont mt ignore x y)
  (CASE *scrolling-style*
    (:Mac
     (Mac-style-mouse-moves cont mt x y))
    (:Explorer
     (Explorer-style-mouse-moves cont mt x y))))

(DEFUN Explorer-style-mouse-moves (cont mt x y)  
  "Change the mouse cursor when entering or leaving the scroll-bar area.
 Also implement bump scrolling at top and bottom of scroll-bar."
  (DECLARE (self-flavor scroll-bar-mixin))
  (if (and (< (sheet-inside-left) x (sheet-inside-right))	; don't slow down the mouse 
	   (not scroll-bar-active-state))			;  when not in scroll area
      ; the common path, not in scroll-bar.
      (funcall-with-mapping-table cont mt :mouse-moves x y)
      (if (if (eq :left (scroll-bar-region-side scroll-bar-region))
	      (<= x (scroll-bar-region-right scroll-bar-region))  ;;needs adjustment...
	      (>= x (+ width (scroll-bar-region-left scroll-bar-region))))
	  ; mouse is in scroll region.
	  (cond ((= (send mouse-blinker :character) *scroll-bar-char-index*)
		 ;;(if (or (< y 0) (>= y height)) ;;may 7-1-88
		 ;;;Top part of mouse glyph leaves bottom of window, or top part of mouse leaves top of window
		 (if (or (<= y 0) (>= y (- height (font-char-height (send mouse-blinker :font))))) 
		     (send self :bump y) 			; bumping top or bottom
		     (mouse-set-blinker-cursorpos x y)))	; staying in scroll region
		((multiple-value-bind (ignore n-lines ignore n-screen-lines)
		     (send self :scroll-position)		; entering scroll region
		   (and (< n-screen-lines n-lines)
			(plusp (send self :scroll-bar-icon-width))))
		 (setq scroll-bar-active-state :region)		 
		 (WHEN (mac-system-p) (SEND self :scroll-bar-manage-drawing))
		 (mouse-set-blinker-definition :character *scroll-bar-char-x-offset*
					       *scroll-bar-char-y-offset* :on
					       :set-character *scroll-bar-char-index*))
		(t						; nothing to scroll or no real region
		 (funcall-with-mapping-table cont mt :mouse-moves x y)))
	  ; mouse is not in scroll region.
	  (cond ((eq :region scroll-bar-active-state)		; leaving scroll region
		 (mouse-standard-blinker)
		 (setq scroll-bar-active-state nil)
		 (WHEN (mac-system-p) (SEND self :scroll-bar-manage-drawing)))
		((eq :bump scroll-bar-active-state)
		 ;;(if (or (< y 0) (>= y height)) ;;may 7-1-88			
		 (if (or (<= y 0) (>= y (- height (font-char-height (send mouse-blinker :font))))) 
		     (send self :bump y)			; bumping top or bottom
		     (mouse-set-blinker-cursorpos x y)))	; moving in bump region
		(t						; slipped by first quick check.
		 (funcall-with-mapping-table cont mt :mouse-moves x y))))))

;;; Added for MacScrolling, KED 8/87
;;; Turn off bumping - it's not Macish.
(DEFUN Mac-style-mouse-moves (cont mt x y)
  "Change the mouse cursor when entering or leaving the scroll-bar area.
 Also implement bump scrolling at top and bottom of scroll-bar."
  (DECLARE (self-flavor scroll-bar-mixin))
  (IF (AND (scroll-bar-region-bar-top scroll-bar-region)	   ;If scrolling necessary
	   (scroll-bar-region-valid-p scroll-bar-region))
      ;;...THEN see if mouse is in scroll region
      (IF (IF (EQ (scroll-bar-region-side scroll-bar-region) :left)
	      (< x (+ (scroll-bar-region-bar-left scroll-bar-region)
		      (scroll-bar-region-bar-width scroll-bar-region)))
	    (> x (scroll-bar-region-bar-left scroll-bar-region)))
	  ;;...THEN mouse is in scroll region.
	  (PROGN 
	    (IF scroll-bar-active-state
		;; Then mouse was in scroll region last time
		(SETF scroll-bar-active-state (SEND self :where-is-mouse? y))
	      ;; Else mouse is entering the scroll region
	      (WHEN (SETF scroll-bar-active-state (SEND self :where-is-mouse? y))	
		(SETF *scroll-bar-previous-mouse-blinker*
		      (CONS (SEND mouse-blinker :character)
			    (MULTIPLE-VALUE-LIST (SEND mouse-blinker :offsets))))))
	    (UNLESS (= (FIRST *scroll-bar-mouse-blinker*)
		       (SEND w:mouse-blinker :character)) 
	      (mouse-set-blinker-definition
		:character
		(SECOND *scroll-bar-mouse-blinker*) (THIRD *scroll-bar-mouse-blinker*)
		:on
		:set-character (FIRST *scroll-bar-mouse-blinker*))))
	;;...ELSE mouse is not in scroll region.
	(IF scroll-bar-active-state	   ; leaving scroll region
	    (PROGN (mouse-set-blinker-definition
		     :character
		     (SECOND *scroll-bar-previous-mouse-blinker*)
		     (THIRD *scroll-bar-previous-mouse-blinker*)
		     :on
		     :set-character (CAR *scroll-bar-previous-mouse-blinker*))
		   (SETF scroll-bar-active-state nil)) 
	  (FUNCALL-WITH-MAPPING-TABLE cont mt :mouse-moves x y)))
    ;;...ELSE normal mouse tracking
    (FUNCALL-WITH-MAPPING-TABLE cont mt :mouse-moves x y)))

(defmethod (scroll-bar-mixin :bump) (y)
  "Warp the mouse from y back into the window and scroll up or down one line."
  (MULTIPLE-VALUE-BIND (IGNORE WINDOW-Y-OFFSET)
      (SHEET-CALCULATE-OFFSETS self MOUSE-SHEET)
    ;;(MOUSE-WARP MOUSE-X (+ (IF (< y 0) 8. (- HEIGHT 8.)) WINDOW-Y-OFFSET))
    (LET ((half-mouse-char-height (ASH (font-char-height (send mouse-blinker :font)) -1)))
      (MOUSE-WARP MOUSE-X (+ (IF (<= y 0) half-mouse-char-height (- HEIGHT (* 3 half-mouse-char-height)));; may 7-1-88
			     WINDOW-Y-OFFSET)))
    (SEND SELF :SCROLL-TO (IF (<= y 0) -1 1) :RELATIVE))) ;; may 7-1-88 changed "<" to "<="


(defmethod (scroll-bar-mixin :scroll-bar) ()
  "Used by the mouse-handler to determine if there is a scroll-bar on the left side of this window,
 and there is something to scroll.  This is compatible with the old scroll-bar."
  (and (eq :left (scroll-bar-region-side scroll-bar-region))
       (send self :really-enabled?)))

(defmethod (scroll-bar-mixin :scroll-bar-on-right) ()
  "Used by the mouse-handler to determine if there is a scroll-bar on the right side of this window,
 and there is something to scroll."
  (and (eq :right (scroll-bar-region-side scroll-bar-region))
       (send self :really-enabled?)))

(defmethod (scroll-bar-mixin :really-enabled?) ()
  "Returns t if this window has an active scroll-bar mode and it has more items
 than can be displayed at one time, and it has not been turned off."
  (and (send self :enable-scrolling-p)
       (send self :scroll-bar-on?)))

(defmethod (scroll-bar-mixin :enable-scrolling-p) ()
  "Returns t if scroll-bar has an active mode
and there is something to scroll."  
   (and (send self :scroll-bar-mode)
	(SEND self :send-if-handles :scroll-bar-p))) ;; may 9-7-88

;; Support for the :SCROLL-BAR-ON-OFF option.

;; may 9-7-88 rewrite, no change in functionality
;; may 10-24-88 Changed to ALWAYS call :compute-region. 
(DEFMETHOD (scroll-bar-mixin :after :change-of-size-or-margins)
	   (&rest ignore)
  "If this is a scroll-bar-on-off window, turn the scroll-bar on or off
 depending upon the new window size."
  (IF (AND scroll-bar-on-off
	   (NOT scroll-bar-making-decision)) ;; allows :decide-if-scrolling-necessary to be called directly
      (SEND self :decide-if-scrolling-necessary))
  (SEND self :compute-region))  ;; may 10-24-88

(DEFMETHOD (SCROLL-BAR-MIXIN :AROUND :PANE-SIZE)
           (CONT MT ARGS &REST IGNORE)
  "If this is a scroll-bar-on-off window, do not include the scroll-bar region
 when determining pane size."
  (if scroll-bar-on-off
      (progn
	(BIND (LOCATE-IN-INSTANCE SELF 'SCROLL-BAR-ICON-WIDTH) 0)
	(MULTIPLE-VALUE-BIND (left NIL right NIL)
	    (SEND SELF :COMPUTE-MARGINS 0 0 0 0)
	  (BIND (LOCF left-MARGIN-SIZE) left)
	  (BIND (LOCF right-MARGIN-SIZE) right)
	  (AROUND-METHOD-CONTINUE CONT MT ARGS)))
      (AROUND-METHOD-CONTINUE CONT MT ARGS)))

(DEFMETHOD (scroll-bar-mixin :decide-if-scrolling-necessary) (&aux changep)
  "Turn the scroll-bar regions on or off for scroll-bar-on-off windows.
 This method should be called after changing the number of displayable
 items, but before doing the redisplay.  This can change the inside size
 of the window unless :ADJUSTABLE-SIZE-P has been defined and returns 
 t.  In that case the outside size should be set before entering this method."
  (WHEN scroll-bar-on-off
    (bind (LOCATE-IN-INSTANCE self 'scroll-bar-making-decision) t)
    (LET ((iw (sheet-inside-width)) (ih (sheet-inside-height))
	  scroll-now)
      ;; When we ask whether everything fits, pretend there are no scroll regions.
      (LET ()
	(bind (LOCATE-IN-INSTANCE self 'scroll-bar-on-off) :off)
	(MULTIPLE-VALUE-BIND (left nil right nil)
	    (SEND self :compute-margins 0 0 0 0)
	  (bind (LOCF left-margin-size) left)
	  (bind (LOCF right-margin-size) right)
	  (SETQ scroll-now (SEND self :enable-scrolling-p))))
      ;; Now SCROLL-NOW says whether we must now have scrolling.
      (MULTIPLE-VALUE-BIND (IGNORE n-lines  ignore n-screen-lines)      
	    (SEND self :scroll-position)	
      (IF (AND scroll-now (>  n-lines  n-screen-lines))
	  (SETQ scroll-now :on)
	  (SETQ scroll-now :off))
      (WHEN (NEQ scroll-now scroll-bar-on-off)
	(SETQ scroll-bar-on-off scroll-now
	      changep t))
      (SEND self :redefine-margins)
      (AND changep
	   (SEND self :send-if-handles :adjustable-size-p)
	   (SEND self :set-inside-size iw ih)))))
  (and scroll-bar-on-off changep))

(defmethod (scroll-bar-mixin :handle-mouse-scroll) (&aux y-off bottom)
  "Handle bumping of scroll-bar.  Change blinker and scroll-bar-active-state,
 then call mouse-default-handler with a parameter of :in so that it will hang on
 to the mouse with some persistence.  This is called from the mouse-default-handler 
 when the scroll-bar is bumped."
  (CASE *scrolling-style*
    (:Mac
     (mouse-default-handler self))		; Turn off grabbing the mouse.
    (:Explorer
     (setq scroll-bar-active-state :bump)
     ;; Give feedback by changing mouse cursor before calling :SCROLL-BAR-MANAGE-DRAWING, which pages a lot
     ;; Change the mouse to a fat double-headed up-and-down arrow.
     (mouse-set-blinker-definition :character *scroll-bar-char-x-offset*
				   *scroll-bar-char-y-offset* :on
				   :set-character *scroll-bar-char-index*)
     (send self :scroll-bar-manage-drawing)  
     (do () (())
       (mouse-default-handler self :in)
       (multiple-value-setq (nil y-off)
	 (sheet-calculate-offsets self mouse-sheet))
       (cond ((< mouse-y y-off)
	      (mouse-warp mouse-x y-off))
	     ((>= mouse-y (setq bottom (+ y-off height)))
	      (mouse-warp mouse-x (1- bottom)))
	     (t (return t))))
     (setq scroll-bar-active-state nil)
     (send self :scroll-bar-manage-drawing))))

(defmethod (scroll-bar-mixin :after :new-scroll-position) (&optional ignore)
  "Update the scroll-bar and icons unless disabled or not needed."
  (send self :scroll-bar-manage-drawing))

(DEFMETHOD (scroll-bar-mixin :scroll-bar-manage-drawing)
	   (&optional refresh)
  "Draw or undraw as necessary the scroll-bar and icons depending upon 
 SCOLL-BAR-ACTIVE-STATE, the current display mode, SCROLL-BAR-ACTIVE-STATE, and REFRESH."
  (WHEN (AND scroll-bar-mode
	     (ZEROP (sheet-output-hold-flag self)))
    (WITHOUT-INTERRUPTS
      (IF (NOT (sheet-can-get-lock self))
	  ;;There is this funny case where the sheet could be locked by the person waiting
	  ;; for us to back out.  For us to block here would be a disaster, so undraw the
	  ;; scroll bar in another process
	  (PROCESS-RUN-FUNCTION
	    "Draw Scroll Bar"
	    #'(lambda (self refresh)
		(SEND self :scroll-bar-manage-drawing refresh))
	    self refresh)
	;; else...
	(SEND self :scroll-bar-update refresh)))))
  
(DEFMETHOD (scroll-bar-mixin :scroll-bar-update) (&optional refresh)
  (CASE *scrolling-style*
    (:Mac
     (WHEN (SEND self :scroll-bar-on?)
       (Mac-style-scroll-bar-update refresh)))
    (:Explorer
     (Explorer-style-scroll-bar-update refresh))))

(DEFUN Explorer-style-scroll-bar-update (refresh)
  "Draw or undraw as necessary the scroll-bar and icons depending upon 
 SCOLL-BAR-ACTIVE-STATE, the current display mode, SCROLL-BAR-DRAW-STATE, and REFRESH."
  (DECLARE (self-flavor scroll-bar-mixin))
  (when (and (send self :scroll-bar-mode)
	     (send self :scroll-bar-on?)
	     (ZEROP (sheet-output-hold-flag self)))
    (let
      ((mode (send self :scroll-bar-mode))
       (bar-drawn (getf scroll-bar-draw-state :bar))
       (box-drawn (getf scroll-bar-draw-state :bar-box))
       (above-drawn (getf scroll-bar-draw-state :above))
       (below-drawn (getf scroll-bar-draw-state :below))
       (scroll-bar-on-left-p (eq :left (scroll-bar-region-side scroll-bar-region)))
       (region-left (scroll-bar-region-left scroll-bar-region))
       (region-top (scroll-bar-region-top scroll-bar-region))
       (region-right (scroll-bar-region-right scroll-bar-region))
       (region-bottom (+ height (scroll-bar-region-bottom scroll-bar-region))))
      
      (when (not scroll-bar-on-left-p)		; then region-left & region-right are negative
	(setq region-left (+ width region-left)
	      region-right (+ width region-right)))
      
      (multiple-value-bind (top-line n-lines ignore n-screen-lines)
	  (send self :scroll-position)
	(let*
	  ((draw-more-above (and (plusp top-line)
				 (neq mode :minimum)))
	   (draw-more-below (and (< (+ top-line n-screen-lines) n-lines)
				 (neq mode :minimum)))
	   (erase-more-above nil)
	   (erase-more-below nil)
	   (icon-height (send self :scroll-bar-icon-height))
	   (icon-width (send self :scroll-bar-icon-width))
	   (box-left (if scroll-bar-on-left-p
			 region-left
			 (1+ region-left)))	; leave room for the edge line
	   (box-top (+ region-top icon-height))
	   (box-bottom (- region-bottom icon-height 1))
	   (bar-left (1+ box-left)))
	  
	  (when (plusp icon-width)
	    (if refresh				; if refresh, force drawing or erasing even if done.
		(setq erase-more-above (not draw-more-above)
		      erase-more-below (not draw-more-below))
		(setq erase-more-above (and (not draw-more-above) above-drawn)
		      draw-more-above (and draw-more-above (not above-drawn))
		      erase-more-below (and (not draw-more-below) below-drawn)
		      draw-more-below (and draw-more-below (not below-drawn))))
	    
	    (without-interrupts
             (tv:prepare-color (self scroll-bar-color)
	      (if (not (sheet-can-get-lock self))
		  ;;There is this funny case where the sheet could be locked by the person waiting
		  ;; for us to back out.  For us to block here would be a disaster, so undraw the
		  ;; scroll bar in another process
		  (process-run-function
		    "Draw Scroll Bar"
		    #'(lambda (self refresh)
			(send self :scroll-bar-manage-drawing refresh))
		    self refresh)
		  (prepare-sheet (self)
		    
						; Draw or erase edge line
		    (when refresh
		      (let ((x (if scroll-bar-on-left-p region-right region-left)))
			;; (user:remember-call :bar x region-bottom region-top)
                        (sys:%draw-rectangle 1 (- region-bottom region-top) x region-top 
					     (if scroll-bar-draw-edge-p char-aluf erase-aluf) ;; ior -> char-aluf
					     self)))
		    
						; Draw or erase top or bottom icons as necessary.
		    (when (or draw-more-above erase-more-above
			      draw-more-below erase-more-below)
		      (cond (draw-more-above
			     (scroll-bar-draw-icon self box-left region-top icon-width icon-height)
			     (setf (getf scroll-bar-draw-state :above) t))
			    (erase-more-above
			     (setf (getf scroll-bar-draw-state :above) nil)
			     (sys:%draw-rectangle (+ 2 icon-width) icon-height
						  box-left region-top
						  erase-aluf self)))
		      (cond (draw-more-below
			     (scroll-bar-draw-icon self box-left (1+ box-bottom)
						   icon-width icon-height nil)
			     (setf (getf scroll-bar-draw-state :below) t))
			    (erase-more-below
			     (setf (getf scroll-bar-draw-state :below) nil)
			     (sys:%draw-rectangle (+ 2 icon-width) icon-height
						  box-left (- region-bottom icon-height)
						  erase-aluf self))))
		    
						; Draw or erase bar and box as necessary.
		    (if (neq mode :maximum)
			
						; Maybe erase bar, and box
			(when (or bar-drawn refresh)
			  (setf (getf scroll-bar-draw-state :bar-box) nil)
			  (setf (getf scroll-bar-draw-state :bar) nil)
			  (sys:%draw-rectangle (+ 2 icon-width) (1+ (- box-bottom box-top))
					       box-left box-top erase-aluf self))
		      
						; Draw bar and maybe box.
		      (let*
			((n-lines (max top-line n-lines))
			 (bar-area-top (1+ box-top))
			 (bar-area-bottom (1- box-bottom))
			 (bar-area-height (max 0 (- box-bottom bar-area-top)))
			 (bar-top (+ bar-area-top
				     (if (zerop n-lines) 0
				       (floor (* bar-area-height top-line)
					      n-lines))))
			 (bar-bottom (- bar-area-bottom
					(if (zerop n-lines) 0
					  (floor (* bar-area-height
						    (max 0 (- n-lines (+ n-screen-lines top-line))))
						 n-lines))))
			 (bar-right (+ bar-left icon-width)))
			(setf (getf scroll-bar-draw-state :bar) t)
						; Erase above and below scroll-bar.
			(sys:%draw-rectangle icon-width (- bar-top bar-area-top)
					     bar-left bar-area-top erase-aluf self)
			(sys:%draw-rectangle icon-width (- bar-area-bottom bar-bottom)
					     bar-left (1+ bar-bottom) erase-aluf self)
			;; Now we can draw the scroll bar.
;;; alu-ior -> char-aluf
			(sys:%draw-shaded-triangle bar-left bar-bottom bar-left bar-top bar-right bar-top 
						   char-aluf t t t *scroll-bar-shade* self)
			(sys:%draw-shaded-triangle bar-left bar-bottom bar-right bar-top bar-right bar-bottom 
						   char-aluf t t t *scroll-bar-shade* self)
			
						; Draw box if not already drawn.
			(when (or (not box-drawn) refresh)
						; draw scroll-bar box
			  (let ((right (+ bar-left icon-width)))
			    (setf (getf scroll-bar-draw-state :bar-box) t)
			    ;; (user:remember-call :bar box-top box-bottom box-left right)
			    (sys:%draw-rectangle 1 (- box-bottom box-top) box-left box-top char-aluf self)
			    (sys:%draw-rectangle (- right box-left) 1 box-left box-top  char-aluf self)
;;			    (sys:%draw-rectangle 1 (- box-bottom box-top) right box-top char-aluf self)	      ;; may 9-7-88
			    (sys:%draw-rectangle 1 (1+ (- box-bottom box-top)) right box-top char-aluf self) ;; may 9-788
			    (sys:%draw-rectangle (- right box-left) 1 box-left box-bottom char-aluf self)
			    ))))))))))))))

;;; Added for MacScrolling, KED 8/87
(DEFUN Mac-style-scroll-bar-update (refresh)
  "If the scroll-bar-region-bar-top is different than the new scroll position then this
draws the scroll box and bar at the new scroll position."
  (DECLARE (self-flavor scroll-bar-mixin))
  (UNLESS (scroll-bar-region-valid-p scroll-bar-region)
    (SEND self :compute-region)
    (SETF refresh t))
  (WHEN refresh
    (SEND self :scroll-bar-draw-region)
    (SEND self :scroll-bar-draw-icon :point-up t :solid nil)
    (SEND self :scroll-bar-draw-icon :point-up nil :solid nil))    
  (MULTIPLE-VALUE-BIND (top-line n-lines ignore n-screen-lines)
      (SEND self :scroll-position)
    (LET ((current-bar-top (scroll-bar-region-bar-top scroll-bar-region))
	  (new-bar-top (WHEN (OR (PLUSP top-line)
				 (> n-lines n-screen-lines))
			 (+ (scroll-bar-region-box-top scroll-bar-region)
			    (IF (>= top-line (1- n-lines))
				;;...THEN last line is at top - force bar to bottom
				(scroll-bar-region-bar-range scroll-bar-region)
			      ;;...ELSE proportionally position the bar
			      (TRUNCATE (* (scroll-bar-region-bar-range
					     scroll-bar-region) top-line)
					n-lines)))))
	  (page-lines (MAX 1 (1- n-screen-lines))))
      (WHEN (OR refresh
		(NOT (EQL new-bar-top current-bar-top))
		(ZEROP (REM top-line page-lines))            ;Page boundary?
		;;  This test should also cover case of scrolling past a boundary without
		;;  actually hitting it!! This needs to be fixed sometime.  
		)
	(SEND self :scroll-bar-draw-box-and-bar
	      new-bar-top
	      (CEILING (+ 1 top-line (ASH page-lines -1)) page-lines))
	(SETF (scroll-bar-region-bar-top scroll-bar-region) new-bar-top)))))

;;; Added for MacScrolling, KED 8/87
(DEFMETHOD (scroll-bar-mixin :scroll-bar-draw-box-and-bar)
	   (bar-top &optional (readout 1))
  "Draws the scroll box and bar. If bar-top is NIL the scroll box is cleared."
  (LET ((box-left (scroll-bar-region-bar-left scroll-bar-region))
	(box-width (scroll-bar-region-width scroll-bar-region))
	(box-top (scroll-bar-region-box-top scroll-bar-region))
	(box-height (scroll-bar-region-box-height scroll-bar-region))
	(bar-width (scroll-bar-region-bar-width scroll-bar-region))
	(bar-height (scroll-bar-region-bar-height scroll-bar-region)))
    (PREPARE-SHEET (self)
      (tv:prepare-color (self scroll-bar-color)
	(IF bar-top
	  ;; THEN...Draw the scroll-bar and box.
	  ;; Clear a spot for the scroll bar
	  (LET* ((mac-adjust (IF (mac-window-p self) 1 0))  ;Why???
		 (upper-box-height (- bar-top box-top (- mac-adjust)))
		 (bar-bottom (+ bar-top bar-height))
		 (lower-box-height (- box-height
				      (- bar-bottom box-top) (- mac-adjust)))) 
	    (sys:%draw-rectangle bar-width bar-height
				 box-left bar-top
				 alu-setz self) 
	    ;; Draw line around the bar
	    (draw-hollow-rectangle bar-width bar-height 
				   box-left bar-top
				   1 alu-seta self)
	    ;; Draw digital page readout in the bar
	    (LET* ((readout-font fonts:tiny)
		   (readout-font-height (tv:font-char-height readout-font)) 
		   (readout-string (FORMAT nil "~d" readout))
		   (readout-length (LENGTH readout-string))
		   (bits (* readout-length (tv:font-char-width readout-font)))
		   x y
		   (bug-fix (tv:font-char-width readout-font))
		   (*dont-clip-at-the-margins* t))
	      (WHEN (> bits bar-width)
		(SETF readout-string "*")
		(SETF readout-length 1)
		(SETF bits (* readout-length (tv:font-char-width readout-font))))
	      (WHEN (>= bar-height readout-font-height) 
		(SETF x (+ box-left
			   (- (TRUNCATE box-width 2)
			      (TRUNCATE bits 2))))
		(SETF y (MAX bar-top
			     (+ bar-top (CEILING bar-height 2)
				(- (TRUNCATE readout-font-height 2)))))
		(tv:draw-string-internal self
					 readout-string
					 0 readout-length
					 x y
					 (+ x bar-width
;;; **** HACK ALERT ****
;;; BUG in draw-string-internal clips character at wrong place... remove this when fixed
					    bug-fix	;Temporary fudge factor!!!
					    )
					 readout-font alu-transp)))
	    ;; Shade the scroll box
	    (WHEN (PLUSP upper-box-height)
	      (draw-shaded-rectangle bar-width upper-box-height
				     box-left box-top
				     alu-seta self *scroll-bar-shade*))
	    (WHEN (PLUSP lower-box-height)
	      (draw-shaded-rectangle bar-width lower-box-height
				     box-left bar-bottom
				     alu-seta self *scroll-bar-shade*)))
	;; ELSE... just clear the scroll-box
	(sys:%draw-rectangle bar-width box-height
			     box-left box-top
			     alu-setz self))))))

;;; Added for MacScrolling, KED 8/87
(DEFMETHOD (scroll-bar-mixin :scroll-bar-draw-region) ()
  "Draws the scroll bar region and outlines it."
  (LET* ((region-width (scroll-bar-region-width scroll-bar-region)) 
	 (region-left (scroll-bar-region-left-edge scroll-bar-region))
	 (region-top (scroll-bar-region-top scroll-bar-region))
	 (region-height (scroll-bar-region-height scroll-bar-region))
	 (icon-box-height (scroll-bar-region-icon-box-height scroll-bar-region))
	 (icon-box-width (scroll-bar-region-bar-width scroll-bar-region))
	 (icon-box-left (scroll-bar-region-bar-left scroll-bar-region))
	 (top-icon-top (+ region-top *Mac-scroll-region-border*)) 
	 (top-icon-bottom (+ region-top
			     (scroll-bar-region-icon-box-height scroll-bar-region)
			     *Mac-scroll-region-border*))
	 (bottom-icon-top (scroll-bar-region-box-bottom scroll-bar-region)))
    (PREPARE-SHEET (self)
      (tv:prepare-color (self scroll-bar-color)
	(sys:%draw-rectangle icon-box-width icon-box-height	;Clear spot for top icon
			     icon-box-left top-icon-top
			     alu-setz self)
	(sys:%draw-rectangle icon-box-width icon-box-height	;Clear spot for bottom icon
			     icon-box-left (+ bottom-icon-top *Mac-scroll-region-border*)
			     alu-setz self)
	(draw-hollow-rectangle region-width region-height	;border it
			       region-left region-top
			       *Mac-scroll-region-border*
			       alu-seta self)
	(sys:%draw-rectangle region-width *Mac-scroll-region-border*	;top icon box
			     region-left top-icon-bottom
			     alu-seta self)
	(sys:%draw-rectangle region-width *Mac-scroll-region-border*	;bottom icon box
			     region-left bottom-icon-top
			     alu-seta self)))))	;bottom icon box

;;; Added for MacScrolling, KED 8/87
(DEFMETHOD (scroll-bar-mixin :scroll-bar-draw-icon)
	   (&optional &key (point-up t) (solid nil))
  "Draw the up or down arrow scroll-bar icon. Almost no error checking is done."
  (LET* ((region-width (scroll-bar-region-width scroll-bar-region))
	 (icon-height (scroll-bar-region-icon-box-height scroll-bar-region))
	 (icon-box-left (scroll-bar-region-bar-left scroll-bar-region))
	 (icon-box-top (IF point-up
			   (+ (scroll-bar-region-top scroll-bar-region) *Mac-scroll-region-border*)
			 (+ height
			    (scroll-bar-region-bottom scroll-bar-region)
			    (- icon-height)
			    (- *Mac-scroll-region-border*))))
	 (arrow (IF solid
		    (IF point-up
			*scroll-bar-solid-up-arrow*
		      *scroll-bar-solid-down-arrow*)
		  (IF point-up
		      *scroll-bar-hollow-up-arrow*
		    *scroll-bar-hollow-down-arrow*)))
	 (arrow-offset (ARRAY-LEADER arrow 1.))
	 (arrow-width (ARRAY-LEADER arrow 2.))
	 (arrow-height (ARRAY-LEADER arrow 3.))
	 (icon (MAKE-ARRAY `(,arrow-height 32.)
			   :element-type 'bit
			   :displaced-to arrow)) 
	 (x-adjust (- (TRUNCATE region-width 2.)
		      arrow-offset))
	 (y-adjust (IF point-up
		       (FLOOR (MAX 0 (- icon-height arrow-height)) 2.)
		     (IF (< icon-height arrow-height)
			 (- icon-height arrow-height)	;make it negative
		       (CEILING (- icon-height arrow-height) 2.))))
	 (window-array (SEND self :screen-array))
	 (wd (MIN (- region-width (* 2 *Mac-scroll-region-border*)) 
		  arrow-width
		  (- (ARRAY-DIMENSION window-array 1) icon-box-left 3.)))	;safety
	 (ht (MIN arrow-height
		  icon-height
		  (- (ARRAY-DIMENSION window-array 0) (+ icon-box-top y-adjust) 1.))))
    (tv:prepare-sheet (self)
      (tv:prepare-color (self scroll-bar-color)
	(COND ((AND (>= x-adjust 0) (>= y-adjust 0))	;normal case, icon fits okay
	       (BITBLT alu-seta
		       wd ht
		       icon
		       0 0
		       window-array
		       (+ icon-box-left x-adjust) (+ icon-box-top y-adjust)))
	      ((AND (MINUSP x-adjust) (>= y-adjust 0))	;doesn't fit widthwise
	       (BITBLT alu-seta
		       wd ht
		       icon
		       (- x-adjust) 0
		       window-array
		       icon-box-left (+ icon-box-top y-adjust)))
	      ((AND (>= x-adjust 0) (MINUSP y-adjust))	;doesn't fit heightwise
	       (BITBLT alu-seta
		       wd ht
		       icon
		       0 (- y-adjust)
		       window-array
		       (+ icon-box-left x-adjust) icon-box-top)) 
	      (t				;doesn't fit either way
	       (BITBLT alu-seta
		       wd ht
		       icon
		       (- x-adjust) (- y-adjust)
		       window-array 
		       icon-box-left icon-box-top)))))))
  


(defun scroll-bar-draw-icon (window x y icon-width icon-height
			     &optional (point-up-p t))
  "Draw the up or down arrow scroll-bar icon.
 Almost no error checking is done."
  (let*
    ((tri-left (1+ x))	; 1 pixel margins
     (tri-right (+ x icon-width))
     (rect-left (+ tri-left (round icon-width 4)))
     (rect-width (- icon-width (* 2 (- rect-left tri-left))))
     (mid-x (+ tri-left (truncate icon-width 2)))
     mid-y tri-pt-y rect-height rect-top)
    (when (plusp icon-height)
      (if point-up-p
	  (setq mid-y (+ y (truncate icon-height 2))
		tri-pt-y (1+ y)
		rect-top (1+ mid-y)
		rect-height (- (+ y icon-height) mid-y 2))
	  (setq mid-y (+ y (- icon-height (truncate icon-height 2) 1))
		tri-pt-y (+ y icon-height -2)
		rect-top (1+ y)
		rect-height (- mid-y rect-top)))
      (sys:%draw-shaded-triangle tri-left mid-y mid-x tri-pt-y tri-right mid-y
				 (tv:sheet-char-aluf window) t t t 100%-black window)	;; may 9-7-88 was alu-transp
;;      ; draw-shaded-triangle seems to be necessary to fill out right edge.
;;    (sys:%DRAW-SHADED-TRIANGLE tri-right mid-y tri-right mid-y mid-x tri-pt-y 		;; may 9-7-88 NOT necessary
;;				 (tv:sheet-char-aluf window) t t t nil window)		;; may 9-7-88 was alu-transp
      (sys:%draw-rectangle rect-width rect-height
			   rect-left rect-top (tv:sheet-char-aluf window) window))))  	;; may 9-7-88 was alu-transp

(DEFMETHOD (scroll-bar-mixin :scroll-next-line) (IGNORE y &rest ignore)
  "Scroll this many lines for a single click, or continous scrolling while the mouse button is depressed."
  (IF (OR (ZEROP (mouse-buttons))  ;; single click?
	  (PROGN (PROCESS-SLEEP *scroll-bar-initial-delay*)
		 (ZEROP (mouse-buttons))))
      (SEND self :scroll-to (TRUNCATE (- y (sheet-inside-top)) line-height) :relative)
       ;; else  button being held 
      (MULTIPLE-VALUE-BIND (top-line n-lines ignore ignore)
	  (SEND self :scroll-position)
	(IF (> n-lines (+ top-line 1))   ;; let user scroll until bottom of file reaches top of screen
	   (LET ((max-top (- n-lines 1))
	         (delay (SEND self :scroll-bar-delay-time))) ;; not needed in do loop
	    (DO ((lines (SEND self :scroll-bar-lines))
		  (index sys:mouse-buttons-buffer-in-index))
		(())
	      (IF (mac-window-p self)
		  (mac-consider-mouse))	   ;Allow Mac mouse to update the mouse-button-buffer-in-index
	      (WHEN (OR (>= top-line max-top)
			(NEQ index sys:mouse-buttons-buffer-in-index))	;; Wait for button up or last line to top.
		(RETURN))
	      (SEND self :scroll-to (SETQ top-line (+ top-line lines))
		    :absolute)
	      (WHEN delay
		(PROCESS-SLEEP delay))))))))
 
(defmethod (scroll-bar-mixin :scroll-next-page) (&rest ignore)
  "Scroll to show next page."
  (multiple-value-bind (top-line n-lines ignore n-screen-lines)
      (send self :scroll-position)
    (send self :scroll-to (min (- n-lines n-screen-lines)
			       (+ top-line (1- n-screen-lines)))
	  :absolute)))

(DEFMETHOD (scroll-bar-mixin :scroll-previous-line) (IGNORE y &rest ignore)
  "Scroll up this many lines for a single click, or continous scrolling while the mouse button is depressed."
  (IF (OR (ZEROP (mouse-buttons))
	  (PROGN (PROCESS-SLEEP *scroll-bar-initial-delay*)
		 (ZEROP (mouse-buttons))))
      (SEND self :scroll-to (- (TRUNCATE (- y (sheet-inside-top)) line-height)) :relative)
       ;else, button held
       (LET ((delay (SEND self :scroll-bar-delay-time)))
	   (DO ((lines (SEND self :scroll-bar-lines))
	        (index sys:mouse-buttons-buffer-in-index))
	        (())
	     (IF (mac-window-p self)
		 (mac-consider-mouse))	   ;Allow Mac mouse to update the mouse-button-buffer-in-index
	      (WHEN (NEQ index sys:mouse-buttons-buffer-in-index)	; Wait for button up
	         (RETURN))
	      (SEND self :scroll-to (- lines) :relative)
	      (WHEN  delay
	         (PROCESS-SLEEP delay))))))

(defmethod (scroll-bar-mixin :scroll-previous-page) (&rest ignore)
  "Scroll to show previous page."
  (multiple-value-bind (ignore ignore ignore n-screen-lines)
      (send self :scroll-position)
    (send self :scroll-to (- (1- n-screen-lines))
	  :relative)))

(DEFMETHOD (scroll-bar-mixin :scroll-jump) (ignore y &rest ignore)
  "Scroll to this percentage of text for a single click,
 or continously drag the display while the mouse button is depressed."
  (IF (OR (ZEROP (mouse-buttons))
	  (PROGN (PROCESS-SLEEP *scroll-bar-initial-delay*)
		 (ZEROP (mouse-buttons))))
      ;; single click: scroll to percentage of text
      (LET* ((icon-height (SEND self :scroll-bar-icon-height))
	     (scroll-bar-top (+ icon-height (scroll-bar-region-top scroll-bar-region)))
	     (scroll-bar-height (- (+ (- height scroll-bar-top)
				      (scroll-bar-region-bottom scroll-bar-region))
				   (* 2 icon-height))))
	(MULTIPLE-VALUE-BIND (IGNORE n-lines ignore n-screen-lines)
	    (SEND self :scroll-position)
	  (SEND self :scroll-to
		(MIN (TRUNCATE (* n-lines
				  (QUOTIENT (- y scroll-bar-top)
					    (SMALL-FLOAT scroll-bar-height))))
		     (- n-lines n-screen-lines))
		:absolute)))
      ;; drag text
      (LET* ((first-top (SEND self :scroll-position))
	     (new-top first-top)
	     (old-top first-top)
	     (first-y mouse-y))
	(DO ((delay (SEND self :scroll-bar-delay-time))
	     (index sys:mouse-buttons-buffer-in-index))
	    (())
	  (SETQ new-top (+ first-top (TRUNCATE (- first-y mouse-y)
					       line-height)))
	  (WHEN (NOT (= new-top old-top))
	    (SEND self :scroll-to new-top :absolute)
	    (SETQ old-top new-top))
	  (WHEN (PLUSP delay)
	    (PROCESS-SLEEP delay))
	  (IF (mac-window-p self)
	      (mac-consider-mouse))	   ;Allow Mac mouse to update the mouse-button-buffer-in-index
	  (WHEN (NEQ index sys:mouse-buttons-buffer-in-index)	; Wait for button up
	    (RETURN))))))

;; It would be nice to create a continous-percentage-wise function but
;; first the primitive text-scroll display and general-scroll display
;; functions need to be combined and streamlined in order to get
;; reasonable performance. -grh



; The following three methods are mostly for compatibility with the old mouse scrolling system.
; They need to be defined at some lower level common to Text and General scroll windows,
; since they are slow here.

(DEFMETHOD (scroll-bar-mixin :SCROLL-MORE-ABOVE) ()
  "Return t if there is more above."
  (PLUSP (SEND SELF :SCROLL-POSITION)))

(DEFMETHOD (scroll-bar-mixin :SCROLL-MORE-BELOW) ()
  "Return t if there is more below."
  (MULTIPLE-VALUE-BIND (TOP-LINE N-LINES LN-HEIGHT N-SCREEN-LINES)
      (SEND SELF :SCROLL-POSITION)
    ;; Some bag-chompers forget to return this value
    (OR N-SCREEN-LINES
        (SETQ N-SCREEN-LINES (TRUNCATE (SHEET-INSIDE-HEIGHT) LN-HEIGHT)))
    (< (+ TOP-LINE N-SCREEN-LINES) N-LINES)))

(DEFMETHOD (scroll-bar-mixin :SCROLL-RELATIVE) (FROM TO)
  "Put the FROM Y-position on the TO Y-position.  This assumes that each item is LINE-HEIGHT
high, and that there is a :SCROLL-TO message which accepts a line number to scroll to,
or a relative number of lines to scroll by.
Each argument is :TOP, :BOTTOM or a number of pixels from the top of the window."
  (MULTIPLE-VALUE-BIND (IGNORE IGNORE ITEM-HEIGHT)
      (SEND SELF :SCROLL-POSITION)
    (SETQ FROM (COND ((EQ FROM :TOP) 0)
                     ((EQ FROM :BOTTOM) (TRUNCATE (- (SHEET-INSIDE-HEIGHT) (1- ITEM-HEIGHT))
                                                  ITEM-HEIGHT))
                     ((NUMBERP FROM) (TRUNCATE (- FROM TOP-MARGIN-SIZE) ITEM-HEIGHT))
                     (T (FERROR NIL "~A illegal arg to :SCROLL-RELATIVE" FROM)))
          TO (COND ((EQ TO :TOP) 0)
                   ((EQ TO :BOTTOM) (TRUNCATE (- (SHEET-INSIDE-HEIGHT) (1- ITEM-HEIGHT))
                                              ITEM-HEIGHT))
                   ((NUMBERP TO) (TRUNCATE (- TO TOP-MARGIN-SIZE) ITEM-HEIGHT))
                   (T (FERROR NIL "~A illegal arg to :SCROLL-RELATIVE" TO))))
    ;; We now know what item we are scrolling from, and what item we are scrolling to.
    ;; Scroll that relative amount.
    (SEND SELF :SCROLL-TO (- FROM TO) :RELATIVE)))

;;;  Establish default (Explorer) scrolling style...
(set-scrolling-style)