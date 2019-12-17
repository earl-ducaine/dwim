;;; -*- Mode: Common-Lisp; Package:W; Base: 10; Fonts:medfnt,HL12B,HL12BI,hl12bi -*-

;Use, duplication, or disclosure by the Government is subject to
;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;Technical Data and Computer Software clause at 52.227-7013.

;                     TEXAS INSTRUMENTS INCORPORATED.
;                              P.O. BOX 2909
;                           AUSTIN, TEXAS 78769

; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.

;;; Change History
;;;
;;;  Date      Author  Description
;;; -------------------------------------------------------------------------------------
;;; 04/24/88  KJF     Fix to select-color-with-mouse so that if its screen is not default-screen, a new
;;;                      instance gets created.  This keeps this function from causing another screen to get
;;;                      exposed.  Needed because of multiple screen support.  The right thing to do may be
;;;                      using a resource instead??
;;; 10/21/87  KJF for Bob M.  General clean-up and changes needed for use with Color Map Editor.
;;; 10/17/87  KJF     Reconfigured constraint frame to have 2 panes.  New pane is index pane for displaying
;;;                      index value as mouse moves.  Previously, the index value was being displayed inside of
;;;                      the color pane (the only pane there was).  However, when the index value was
;;;                      updated, :clear-string and FORMAT were causing the blinker (mouse sensitive blinker)
;;;                      to be turned off, then on again (because of prepare-sheet).  This caused the
;;;                      appearance of the mouse sensitive blinker around the colors to appear very slow.
;;;                      Now since output is being done to a different window, the blinkers in the color
;;;                      pane don't get turned off.
;;; 09/04/87   KJF    Put this in W package.
;;;   1987     Ken Bice, Ken Fischer - Original


;;; Contains:  Constraint frame for select-color-with-mouse function, color-selector-pane for use in frame
;;;            and by Color Map Editor, and select-color-with-mouse function.


(DEFPARAMETER *select-color-map-frame* nil "An instance of the simple-color-map-pane-frame for
use by the select-color-with-mouse function.")


(DEFFLAVOR simple-color-map-pane-frame ()
	   (w:inferiors-not-in-select-menu-mixin
	    w:select-mixin
	    w:borders-mixin
	    w:label-mixin
	    w:box-label-mixin
	    w:stream-mixin
	    w:essential-mouse
	    w:bordered-constraint-frame-with-shared-io-buffer)
  (:default-init-plist
    :save-bits :delayed
    :inside-size '(360 360)
    :Panes '((color color-selector-pane) (index color-index-pane))
    :constraints '((DEFAULT . ((color index)
			       ((index 1 :lines)) ;; force 1 line for string "Index:"
                               ((color :even))
			       )))
    :label '(:string "Color Map Selector" :centered :top :font fonts:hl12bi)
    :label-box-p t
    )
  :settable-instance-variables
  (:documentation :combination
		  "The constraint frame for the simple color selector display."))


;;; This flavor expects to be inside a constraint frame and requires a color-selector-pane
;;; to be present also. Its name in the constraint frame must be w:index, regardless of 
;;; the home package of the constraint frame itself. Look at the flavor definitions of
;;; simple-color-map-pane-frame (above) or color:color-map-frame for examples. 
(DEFFLAVOR color-index-pane ()
	   (w:stream-mixin
	    w:minimum-window
	    w:borders-mixin)
  (:documentation :combination
		  "The flavor used for the index display pane in the color map display.")
  (:default-init-plist  
    :blinker-p nil
    :borders nil))


;;; This flavor expects to be inside a constraint frame and requires a color-index-pane
;;; to be present also. Its name in the constraint frame must be w:color, regardless of 
;;; the home package of the constraint frame itself. Look at the flavor definitions of
;;; simple-color-map-pane-frame (above) or color:color-map-frame for examples.  
(DEFFLAVOR color-selector-pane
	   ((index-loc nil)                ;List of X, Y position where index value is to be displayed
            (last-index 0))
	   (w:graphics-mixin
	    w:basic-mouse-sensitive-items
	    w:stream-mixin
	    w:minimum-window)
  (:default-init-plist
    :item-type-alist
    '((:node				   ;type
	doit				   ;left-button-alternative
	"Select a color"
	))
    :blinker-p nil
    )
    :settable-instance-variables
  (:documentation :combination
		  "The flavor used for each rectangular set of colors in the color map display."))


;;; Define this method so we can warp the mouse to where we want it, too.
(defmethod (color-selector-pane :get-node-list) ()
  tv:item-list)


(DEFMETHOD (color-selector-pane :after :refresh) (&rest ignore)
  "Draws color rectangles and makes them mouse sensitive."
  (LET* ((L-width (w:sheet-inside-width))
	 (L-height (w:sheet-inside-height))
	 ;(aspect (/ L-width L-height))	   ;if >1, wider than high
	 (left (w:sheet-inside-left))
	 (top (w:sheet-inside-top))
	 (spacing 3.)
         (color 0)
	 (rect-w (QUOTIENT (- L-width (* spacing 16.)) 16.))
	 (rect-h (QUOTIENT (- L-height (* spacing 16.)) 16.))
	 new-left
	 )
    (LOOP for x from 0 below 16 do
	  (SETQ new-left left)
	  (LOOP for y from 0 below 16 do
		(SEND self :draw-filled-rectangle new-left top rect-w rect-h color)
		(SEND self :primitive-item :node color
		      (1- new-left) (1- top) (+ new-left rect-w 3) (+ top rect-h 3))
		(incf color)
		(SETQ new-left (+ new-left rect-w spacing)))
	  (SETQ top (+ top rect-h spacing)))
    (when (= color 256) (setq color 0))))


(DEFMETHOD (color-index-pane :after :refresh) (&rest ignore)
  "Displays the string 'Index:' in the index pane."
  (WHEN superior
    ;; Now display the Index
    (SEND self :set-cursorpos 0 0 :pixel)
    (MULTIPLE-VALUE-BIND (fx fy findex ignore)
	(SEND self :compute-motion "Index:")
      (WHEN (NULL findex)
	(SEND self :string-out "Index:")
	(SEND (SEND superior :get-pane 'color) :set-index-loc (LIST fx fy))))))


(DEFMETHOD (color-selector-pane :after :mouse-moves) (x y)
  "Updates index display based on which rectangle the mouse is on."
  (WHEN (AND superior index-loc)
    (LET ((lst (SEND self :mouse-sensitive-item x y))
	  (index-object (SEND superior :get-pane 'index)))
      (WHEN (AND lst (EQ :NODE (FIRST lst))
                 (NEQ last-index (SECOND lst)))
        (SETQ last-index (SECOND lst))
	(SEND index-object
	      :set-cursorpos (FIRST index-loc) (1- (SECOND index-loc)))
	(SEND index-object
	      :clear-string "   ") ;; Clear the string as it was
	(SEND index-object
	      :set-cursorpos (FIRST index-loc) (SECOND index-loc))
	(FORMAT index-object
		"~,3D" last-index)
	))))


(DEFUN select-color-with-mouse (&optional (color-map tv:*default-color-map*))
  "Displays 256 rectangles in colors of color map specified.  Each rectangle is
mouse sensitive.  Clicking on a rectangle returns the index value of the color
in the color map.  Note that the color map specified becomes the active color
map during execution of this function."
  (WHEN (OR (NULL *select-color-map-frame*)
	    ;; Prevent another screen from being exposed by making a new version
	    ;; if this belongs to another screen than default-screen.  04/24/88 KJF.
	    ;; Needed for multiple screen support.  Maybe should be using a resource
	    ;; instead. ??
	    (AND *select-color-map-frame*
		 (NOT (EQ (tv:get-screen *select-color-map-frame*)
			  default-screen))))
    (SETQ *select-color-map-frame* (make-instance 'simple-color-map-pane-frame)))
  (SEND *select-color-map-frame* :set-color-map color-map)
  (send *select-color-map-frame* :expose-near '(:mouse) t)
  (send *select-color-map-frame* :select)
  (PROG1 
    (LOOP as (node alternative value) = (SEND *select-color-map-frame* :list-tyi)
	  do (WHEN (AND (EQ node :TYPEOUT-EXECUTE)
                        (EQ alternative 'DOIT))
	       (RETURN value)))
    (SEND *select-color-map-frame* :bury)))
