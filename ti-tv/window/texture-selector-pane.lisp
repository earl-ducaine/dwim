;;; -*- Mode:Common-Lisp; Package:w; Base:10; Fonts:(CPTFONT HL12B HL12I) -*-

;;;			      RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769

;;; Copyright (C) 1984-1989 Texas Instruments Incorporated. All rights reserved.

;;; Change History
;;;
;;;  Date      Author  Description
;;; -------------------------------------------------------------------------------------
;;; 04/24/88  KJF     Fix to select-texture-with-mouse so that if its screen is not default-screen, a new
;;;                      instance gets created.  This keeps this function from causing another screen to get
;;;                      exposed.  Needed because of multiple screen support.  The right thing to do may be
;;;                      using a resource instead??
;;; 10/21/87  KJF     General clean-up and changes made based on changes to color-selector code.
;;; 10/12/87  KJF     Reconfigured constraint frame to have 2 panes.  New pane is index pane for displaying
;;;                      index value as mouse moves.  Previously, the index value was being displayed inside of
;;;                      the texture pane (the only pane there was).  However, when the index value was
;;;                      updated, :clear-string and FORMAT were causing the blinker (mouse sensitive blinker)
;;;                      to be turned off, then on again (because of prepare-sheet).  This caused the
;;;                      appearance of the mouse sensitive blinker around the textures to appear very slow.
;;;                      Now since output is being done to a different window, the blinkers in the texture
;;;                      pane don't get turned off.
;;; 09/04/87   KJF    General clean up for color and comments.
;;;   1987     Brian Kennedy, Ken Bice, Ken Fischer - Original


;;; Contains:  Constraint frame for select-texture-with-mouse function, texture-selector-pane for use in frame,
;;;            and select-texture-with-mouse function.

(defun make-texture (r0 r1 r2 r3 r4 r5 r6 r7)
  "Create an array with the specified pattern as the texture."
  (make-array '(32 32)
	      :element-type 'bit
	      :initial-contents
	      (list
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r0 r0 r0 r0)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r1 r1 r1 r1)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r2 r2 r2 r2)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r3 r3 r3 r3)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r4 r4 r4 r4)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r5 r5 r5 r5)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r6 r6 r6 r6)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r7 r7 r7 r7)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r0 r0 r0 r0)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r1 r1 r1 r1)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r2 r2 r2 r2)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r3 r3 r3 r3)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r4 r4 r4 r4)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r5 r5 r5 r5)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r6 r6 r6 r6)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r7 r7 r7 r7)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r0 r0 r0 r0)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r1 r1 r1 r1)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r2 r2 r2 r2)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r3 r3 r3 r3)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r4 r4 r4 r4)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r5 r5 r5 r5)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r6 r6 r6 r6)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r7 r7 r7 r7)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r0 r0 r0 r0)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r1 r1 r1 r1)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r2 r2 r2 r2)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r3 r3 r3 r3)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r4 r4 r4 r4)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r5 r5 r5 r5)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r6 r6 r6 r6)
		(format nil "~8,#\0B~8,#\0B~8,#\0B~8,#\0B" r7 r7 r7 r7)
		)))

(defvar *textures* 
  (make-array 64
	      :initial-contents
	      (list
		(make-texture   0   0   0   0   0   0   0   0) ;; 0
		(make-texture 128   0   0   0   0   0   0   0) ;; 1
		(make-texture 128   0   8   0 128   0   8   0) ;; 2
		(make-texture 128  16   2  32   1   8  64   4) ;; 3
		(make-texture 136   0  34   0 136   0  34   0) ;; 4
		(make-texture 136  34 136  34 136  34 136  34) ;; 5
		(make-texture 170   0 170   0 170   0 170   0) ;; 6
		(make-texture 177  48   3  27 216 192  12 141) ;; 7
		(make-texture 170  85 170  85 170  85 170  85) ;; 8
		(make-texture 221 119 221 119 221 119 221 119) ;; 9
		(make-texture 221 255 119 255 221 255 119 255) ;; 10
		(make-texture 255 255 255 255 255 255 255 255) ;; 11
		(make-texture 238 221 187 119 238 221 187 119) ;; 12
		(make-texture  17  34  68 136  17  34  68 136) ;; 13
		(make-texture   1   2   4   8  16  32  64 128) ;; 14
		(make-texture 255   0   0   0 255   0   0   0) ;; 15
		(make-texture 255   0 255   0 255   0 255   0) ;; 16
		(make-texture  85 255  85 255  85 255  85 255) ;; 17
		(make-texture 170 170 170 170 170 170 170 170) ;; 18
		(make-texture 136 136 136 136 136 136 136 136) ;; 19
		(make-texture 255 136 136 136 255 136 136 136) ;; 20
		(make-texture 255 128 128 128 255   8   8   8) ;; 21
		(make-texture 255 128 128 128 128 128 128 128) ;; 22
		(make-texture 170   0 128   0 136   0 128   0) ;; 23
		(make-texture 191   0 191 191 176 176 176 176) ;; 24
		(make-texture   8  28  34 193 128   1   2   4) ;; 25
		(make-texture   3 132  72  48  12   2   1   1) ;; 26
		(make-texture  16  32  84 170 255   2   4   8) ;; 27
		(make-texture   0   8  20  42  85  42  20   8) ;; 28
		(make-texture  32  80 136 136 136 136   5   2) ;; 29
		(make-texture 128 128  65  62   8   8  20 227) ;; 30
		(make-texture 102  24 102 129 102  24 102 129) ;; 31
		(make-texture 136  85  85  34  34  85  85 136) ;; 32
		(make-texture  65  34  20   8  20   8  20  34) ;; 33
		(make-texture 128  65  42  20  42  65 128   0) ;; 34
		(make-texture  51 102 204 129  51 102 204 129) ;; 35
		(make-texture 204 102  51 129 204 102  51 129) ;; 36
		(make-texture 254   0 239   0 254   0 239   0) ;; 37
		(make-texture 170 170 170 136 170 170 170  34) ;; 38
		(make-texture 128 254 128 191 128 254 128 191) ;; 39
		(make-texture 255 136 170 170 170 170 170  34) ;; 40
		(make-texture 248 116  34  71 143  23  34 113) ;; 41
		(make-texture 128  64  32   0   2   4   8   0) ;; 42
		(make-texture  73 146  36  74 153  36  82 137) ;; 43
		(make-texture 146  73  36  82 153  36  74 145) ;; 44
		(make-texture 130  68  40  16  32  64 128   1) ;; 45
		(make-texture 146  68  41 147  39  78 156  57) ;; 46
		(make-texture  65  34  20   8   4   2   1 128) ;; 47
		(make-texture  73  34 148 201 228 114  57 156) ;; 48
		(make-texture  64 160   0   0   4  10   0   0) ;; 49
		(make-texture 119 137 143 143 119 152 248 248) ;; 50
		(make-texture  85 160  64  64  85  10   4   4) ;; 51
		(make-texture  36  36 231  24  24 231  36  36) ;; 52
		(make-texture  60  66 189 165 165 189  66  60) ;; 53
		(make-texture 189  66 189 165 165 189  66 189) ;; 54
		(make-texture 153  60 102 195 195 102  60 153) ;; 55
		(make-texture  24 102 165  66  66 165 102  24) ;; 56
		(make-texture 130  68  57  68 130   1   1   1) ;; 57
		(make-texture 136  20  34  65 136   0 170   0) ;; 58
		(make-texture  17  34 102 153 136  68   0   0) ;; 59
		(make-texture 136  68 102 153  17  34   0   0) ;; 60
		(make-texture 146  84  40 214  40  84 146   0) ;; 61
		(make-texture 170  68 170   0 170  68 170   0) ;; 62
		(make-texture 240 240 111 111 102 102  15  15) ;; 63
		))
  "Array of texture arrays.  Suitable for bitblt'ing.")


;;;

(DEFPARAMETER *select-texture-map-frame* nil "An instance of the simple-texture-map-pane-frame for
use by the select-texture-with-mouse function.")


(DEFFLAVOR simple-texture-map-pane-frame ()
	   (w:inferiors-not-in-select-menu-mixin
	    w:select-mixin
	    w:borders-mixin
	    w:label-mixin
	    w:box-label-mixin
	    w:stream-mixin
	    w:essential-mouse
	    w:bordered-constraint-frame-with-shared-io-buffer)
  (:default-init-plist
    :save-bits t ;;:delayed  ;; needed for allowing patterns to be displayed in color.  See below.
    :inside-size '(360 360)
    :Panes '((texture texture-selector-pane) (index texture-index-pane))
    :constraints '((DEFAULT . ((texture index)
			       ((index 1 :lines)) ;; force 1 line for string "Index:"
                               ((texture :even))
			       )))
    :label '(:string "Texture Selector" :centered :top :font fonts:hl12bi)
    :label-box-p t
    )
  :settable-instance-variables
  (:documentation :combination
		  "The constraint frame for the simple texture selector display."))


;;; This flavor expects to be inside a constraint frame and requires a texture-selector-pane
;;; to be present also. Its name in the constraint frame must be w:index, regardless of 
;;; the home package of the constraint frame itself. Look at the flavor definition of
;;; simple-texture-map-pane-frame (above) for an example. 
(DEFFLAVOR texture-index-pane ()
	   (w:stream-mixin
	    w:minimum-window
	    w:borders-mixin)
  (:documentation :combination
		  "The flavor used for the index display pane in the texture display.")
  (:default-init-plist  
    :blinker-p nil
    :borders nil))


;;; This flavor expects to be inside a constraint frame and requires a texture-index-pane
;;; to be present also. Its name in the constraint frame must be w:texture, regardless of 
;;; the home package of the constraint frame itself. Look at the flavor definition of
;;; simple-texture-map-pane-frame (above) for an example.  
(DEFFLAVOR texture-selector-pane
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
	"Select a texture"
	))
    :blinker-p nil
    )
    :settable-instance-variables
  (:documentation :combination
		  "The flavor used for each rectangular set of textures in the texture display."))


(DEFMETHOD (texture-selector-pane :after :refresh) (&rest ignore)
  "Draws textured rectangles and makes them mouse sensitive."
  (LET* ((L-width (w:sheet-inside-width))
	 (L-height (w:sheet-inside-height))
						;(aspect (/ L-width L-height))	   ;if >1, wider than high
	 (left (w:sheet-inside-left))
	 (top (w:sheet-inside-top))
	 (spacing 3.)
         (texture 0)
	 (rect-w (QUOTIENT (- L-width (* spacing 8.)) 8.))
	 (rect-h (QUOTIENT (- L-height (* spacing 8.) (w:sheet-line-height self)) 8.))
	 new-left
	 ;; deexposed-typeout-action causes refresh to be called, but *select-texture-map-frame* may be nil. 
	 (color (IF (OR (NULL superior) (NULL (color-system-p self)))
		    w:black
		    (send superior :foreground-color)))
	 )
    (LOOP for x from 0 below 8 do
	  (SETQ new-left left)
	  (LOOP for y from 0 below 8 do
		(SEND self :draw-filled-rectangle
		      new-left top
		      rect-w rect-h
		      color
		      w:normal
		      t
		      (aref *textures* texture))
		(SEND self :primitive-item :node texture
		      (1- new-left) (1- top) (+ new-left rect-w 3) (+ top rect-h 3))
		(incf texture)
		(when (= texture 64) (setq texture 0))
		(SETQ new-left (+ new-left rect-w spacing)))
	  (SETQ top (+ top rect-h spacing)))
    ))


(DEFMETHOD (texture-index-pane :after :refresh) (&rest ignore)
  "Displays the string 'Index:' in the index pane."
  (WHEN superior
    ;; Now display the Index
    (SEND self :set-cursorpos 0 0 :pixel)
    (MULTIPLE-VALUE-BIND (fx fy findex ignore)
	(SEND self :compute-motion "Index:")
      (WHEN (NULL findex)
	(SEND self :string-out "Index:")
	(SEND (SEND superior :get-pane 'texture) :set-index-loc (LIST fx fy))))))


(DEFMETHOD (texture-selector-pane :after :mouse-moves) (x y)
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


(DEFUN select-texture-with-mouse (&optional (color w:black))
  "Select a texture pattern from a pre-defined set.  The value returned is the index into
the global array W:*TEXTURES*.  Color is ignored on B&W systems."
  (LET (save-foreground-color)
    (WHEN (OR (NULL *select-texture-map-frame*)
	      ;; Prevent another screen from being exposed by making a new version
	      ;; if this belongs to another screen than default-screen.  04/24/88 KJF.
	      ;; Needed for multiple screen support.  Maybe should be using a resource
	      ;; instead. ??
	      (AND *select-texture-map-frame*
		   (NOT (EQ (tv:get-screen *select-texture-map-frame*)
			    default-screen))))
      (SETQ *select-texture-map-frame* (make-instance 'simple-texture-map-pane-frame
						      :deexposed-typeout-action :permit)))
    (SETQ save-foreground-color (SEND *select-texture-map-frame* :foreground-color))
    (WHEN (color-system-p *select-texture-map-frame* )
      (SEND *select-texture-map-frame* :set-foreground-color color))
    (SEND *select-texture-map-frame* :refresh)  ;; force color to take effect everytime
    (send *select-texture-map-frame* :expose-near '(:mouse) t)
    (send *select-texture-map-frame* :select)
    (PROG1
      (LOOP as (node alternative value) = (SEND *select-texture-map-frame* :list-tyi)
            do (WHEN (AND (EQ node :TYPEOUT-EXECUTE)
			  (EQ alternative 'DOIT))
                 (RETURN value)))
      (SEND *select-texture-map-frame* :set-foreground-color save-foreground-color)
      (SEND *select-texture-map-frame* :bury))))
