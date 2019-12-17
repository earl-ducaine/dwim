;;; -*- Mode: COMMON-LISP; Base: 10; Package: TV; Fonts: CPTFONT,HL12B,HL12BI -*-

;                           RESTRICTED RIGHTS LEGEND

;Use, duplication, or disclosure by the Government is subject to
;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;Technical Data and Computer Software clause at 52.227-7013.
;
;                     TEXAS INSTRUMENTS INCORPORATED.
;                              P.O. BOX 2909
;                           AUSTIN, TEXAS 78769
;
; Copyright (C) 1985-1989 Texas Instruments Incorporated. All rights reserved.


;;; Change History
;;;
;;;  Date      Author     Description
;;; -------------------------------------------------------------------------------------
;;; 01/27/89  KJF       [may] Changes to transfer-color-lut-buffer and download-color-lut-buffer for MMON.
;;;                        Also cleaned up paren comments. ;; may 01/27/89 
;;;                        Removed defvar for *aux-csib-fslots* (in window-inits.lisp)
;;; 08/10/88  KJF       Redefined mmon-p function.
;;; 07/26/88  KJF      Added mmon-p function and fixed download-color-lut-buffer to not try to
;;;                      call csib-map-set unless MMON system is loaded.
;;; 03/01/88  KJF       Changed *default-monochrome-plane* to *default-dual-monitor-monochrome-plane*.
;;; 02/22/88  KJF       Fix to make-version-name to return something other than NIL.NIL for version number.
;;; 02/10/88   KJF      Changes for dual monitor support.  Search for KJF.
;;; 9/24/87    PMH       Added restore-default-colors
;;; 9/15/87    PMH        Modified make-color-map to push onto *color-maps*
;;; 8/25/87    KWW        Changes based on code reading:
;;;                          1. get rid of set-color-map XXX, since setf can be used
;;;                          2. palette => lut-buffer, dac => LUT
;;;                          3. Human Factors color definitions
;;; 7/14/87    KWW        modified to set up a b&w version or color version based on tv:color-system
;;; 6/22/87    KWW        changed order of colors, since using alu-add and alu-sub for blinkers
;;; 5/20/87    KWW        changed to color-map as a defstruct, and blt to load hardware
;;;                       also changed some names to match documentation. Consistent use
;;;                       of color map, LUT, and LUT buffer.
;;; 5/05/87    KWW        change from csib-mixin to sheet flavor 
;;; 4/7/87    KWW        MAKE WORKING VERSION WITH ACTUAL HARDWARE TO TEST WITH
;;; 3/20/87    Ken Wood   Complete color-table versus color-map code
;;; 1/15/87    Rick Still Original Code

;;; See the hardware reference for more details about these map buffers
;;;; HARDWARE REFERENCES (P/N 2534334-0001 DATED 31 MARCH 1987)
;;;;
;;;;   4.2.10, 4.2.10.1 THROUGH 4.2.10.5 
;;;;   


;;; this function added so we can grab the system version and put it into the
;;; color map

(DEFUN make-version-name (system)
  (LET (major minor)
    (MULTIPLE-VALUE-SETQ (major minor) (get-system-version system))
    ;; If system does not exist (NIL returned as version number), get version number of
    ;; SYSTEM.  This is the case when the window system is being built.  *default-color-map*
    ;; gets created before the window system is completely built.  As far as I know, 'window
    ;; is always passed to this function (see CSIB-DEFS.LISP).  After the window system has
    ;; been built, any further calls to w:create-color-map will make the color-map's version
    ;; number really be that of the window system.
    (IF (INTEGERP major)
	(FORMAT nil "Window Release ~D.~D" major minor)
	(MULTIPLE-VALUE-SETQ (major minor) (get-system-version 'system))
	(FORMAT nil "System Release ~D.~D" major minor))))


;;; define my own copy color map function, since the system generated version
;;; does not copy the color table
(DEFUN copy-color-map (source)
 (LET* ((local (make-color-map))
	(local-table (color-map-table local))
	(source-table (color-map-table source)))
   (SETF (color-map-name local) (color-map-name source))
   (SETF (color-map-system-version local) (color-map-system-version source))
   (SETF (color-map-CME-version local) (color-map-CME-version source))
   (SETF (color-map-saturate local) (color-map-saturate source))
   (SETF (color-map-clamp local) (color-map-clamp source))
   (SETF (color-map-reserved-slots local) (color-map-reserved-slots source))
   (COPY-ARRAY-CONTENTS source-table local-table)
   local))


(DEFUN download-color-defaults ()
 (WHEN sib-is-csib
   (download-color-lut-buffer *default-color-map* (tv:current-color-lut-buffer))
   ;(SEND window :write-plane-mask #xff)
   (send (aref *blocks* kludge-block) :set-foreground-color-register
	 (color-map-saturate *default-color-map*))
   (send (aref *blocks* kludge-block) :set-background-color-register
	 (color-map-clamp  *default-color-map*))   ))

(defun restore-default-colors (window)
  (when window
    (send (or (send window :send-if-handles :alias-for-selected-window) window)
	  :send-if-handles :restore-default-colors)))

;;; since the color map is a defstruct, a function make-color-map is automatically created.
;;; However, there is no equivalent to :after :init, so I combined make-color-map and its
;;; initialization functions into CREATE-COLOR-MAP
;;; also, a function copy-color-map is also created


(DEFPARAMETER *default-initial-colors* (list
 `(,white 255 255 255)         
 `(,12%-gray-color 239 239 239)      ;12%-gray
 `(,25%-gray-color 224 224 224)      ;25%-gray
 `(,33%-gray-color 211 211 211)      ;33%-gray
 `(,50%-gray-color 190 190 190)   ;50%-gray
 `(,66%-gray-color 179 179 179)   ;66%-gray
 `(,75%-gray-color 139 139 139)   ;75%-gray
 `(,88%-gray-color 99 99 99)   ;88%-gray
 `(,black 0 0 0)   

 ;;; switched some orders around here in order to get better mouse contrast with the light shades
 ;;; of gray
 `(,yellow 255 255 35)    ;yellow
 `(,magenta 255 0 255)    ;magenta
 `(,cyan 0 255 255)    ;cyan
 `(,pink 242 153 179) ;pink
 `(,purple 130 0 150)   ;purple
 `(,orange 255 90 0)     ;orange
 `(,red 255 0 0)       ;red
 `(,blue 0 0 255)      ;blue
 `(,dark-green 0 75 15)     ;dark green
 `(,dark-blue 0 40 65)     ;dark blue
 `(,green 0 238 0)      ;green     not fully saturated, sony monitored its too much
 `(,dark-brown 60 20 5)     ;dark-brown
 `(,blue-green 0 128 128)   ;blue-green
 `(,light-brown 150 70 40)   ;light brown
 `(,red-purple 255 000 172) ; red-purple
 ;;; now some additional colors just to fill out the remaining reserved locations...
 '(24 255 255 255)         
 '(25 239 239 239)      ;12%-gray
 '(26 224 224 224)      ;25%-gray
 '(27 211 211 211)      ;33%-gray
 '(28 190 190 190)   ;50%-gray
 '(29 179 179 179)   ;66%-gray
 '(30 139 139 139)   ;75%-gray
 '(31 99 99 99)   ;88%-gray
) "A list of R G B values for use in initializing a color map to these default values")

(DEFUN install-initial-colors (table color-list)
  (WHEN  color-list
     (DOLIST (colors color-list) 
	  (write-color-map table  (FIRST colors)
		                  (SECOND colors)
				  (THIRD colors)
				  (FOURTH colors)))))


;; Added for dual monitors by KJF on 02/10/88
(DEFUN default-map-init (the-map &optional (initial-color-list *default-initial-colors*)) 
  "Initialize color map in such a way that it works equally well on
dual monitors systems.  That is, duplicate part of the color map."
  (install-initial-colors the-map initial-color-list)
  (fill-default-map-with-rainbow the-map)
  ;; Must make 255 black for monochrome (B&W) windows on color monitor.
  ;; B&W windows typically use 0 and 255 for foreground and background colors, so to
  ;; get image on color monitor, color map slots must contain 0 - white and 255 - black.
  ;; Making 127 black will cause 255 to be black after the colors are duplicated.
  (write-color-map the-map 127 0 0 0)
  (duplicate-colors-for-default-map the-map)
  (SETF (color-map-name the-map) "DEFAULT"))

(DEFUN color-map-init (the-map &optional (initial-color-list *default-initial-colors*)) 
  "initialize to the standard colors"
  (install-initial-colors the-map initial-color-list)
  (interp-table the-map 0 255 0 0 0 0 32 106)	; red ramp
  (interp-table the-map 0 0 0 255 0 0 107 181)	; green ramp
  (interp-table the-map 0 0 0 0 0 255 182 255)	; blue ramp
  (write-color-map the-map 254 0 0 0)		; we have an odd color left over, make it black
  (write-color-map the-map 255 0 0 0)		; since the 0th color is white, make the 255th color black
  (SETF (color-map-name the-map) "COLOR"))

(DEFUN b&w-map-init (the-map)
  "initialize to all black and white for monochrome system"
  (interp-table the-map 0 0 0 0 0 0 0 127)
  (interp-table the-map 255 255 255 255 255 255 128 255)
  (write-color-map the-map 255 255 255 255)
  (SETF (color-map-name the-map) "B&W"))

;;; call this function for an initialzied color map, call make-color-map if you want it plain
;; Modified for dual monitors by KJF on 02/10/88
(DEFUN create-color-map (&optional (init-plist nil))
  "Return a color map of type default, B&W, or color.  The type is
specified by :type in the init-plist.  Default is to return a default type
color map, meaning a color map which works equally well for B&W,
color, dual monitor cases."
  (LET ((the-map (make-color-map))
	(color-list *default-initial-colors*)
	(map-type :default))
    (DOPLIST (INIT-PLIST VAL OP)
      (CASE OP
	(:name               (SETF (color-map-name the-map)                 VAL))
	(:system-version     (SETF (color-map-system-version the-map)       VAL))
	(:cme-version        (SETF (color-map-cme-version the-map)          VAL))
	(:saturate           (SETF (color-map-saturate the-map)             VAL))
	(:clamp              (SETF (color-map-clamp the-map)                VAL))
	(:reserved-slots     (SETF (color-map-reserved-slots the-map)       VAL))
	(:inital-colors      (SETF color-list                               VAL))
	(:type               (SETF map-type                                 VAL))))
    (CASE map-type
      (:default (default-map-init the-map color-list))
      (:color (color-map-init the-map color-list))
      (:B&W (b&w-map-init the-map)))
    the-map))


;;; after changing *color-system* to t, call this function
;; NO, not anymore.  In Release 4.0, we made a type of color map
;; that works for all cases: B&W, color, or dual-monitors.  Thus
;; *default-color-map* will be a :default color map type and
;; will not need to change when going to color mode, or to dual
;; mode.  I've decided that since it's here, I'd better not make
;; it go away, eventhough it may never get used.  02/10/88 KJF.
(DEFUN reset-color-map ()
  (SETQ *default-color-map* (create-color-map)))

;;; now, here are functions to change color-map-table values, or read them

(DEFUN write-color-map (the-map index red-val green-val blue-val)
  "Writes data into one location of a color map data structure"
  (SETF (AREF (color-map-table the-map) index) red-val)
  (SETF (AREF (color-map-table the-map) (+ 256 index)) green-val)
  (SETF (AREF (color-map-table the-map) (+ 512 index)) blue-val))

(DEFUN read-color-map (the-map index)
  "Reads data from one location of a color map data structure"
  (let* ((red-val    (ldb 8  (AREF (color-map-table the-map) index)))
	 (green-val  (ldb 8  (AREF (color-map-table the-map) (+ 256. index))))
	 (blue-val   (ldb 8  (AREF (color-map-table the-map) (+ 512. index)))))
    (VALUES red-val green-val blue-val)))	; this is what is returned by the function.

(DEFUN gray-table (table)
  "Initializes TABLE to a gray scale of R,G,B=0,0,0 to R,G,B = 255, mainly for test purposes."
  (LOOP for index from 0 below 256.
	do
	(write-color-map table index index index index)))

(defun random-table (table &optional
		     (max-red 256)
		     (max-green 256)
		     (max-blue 256))
  "Initializes TABLE to all random values , mainly for test purposes."
  (LOOP for index from 1 below 255.
	do
	(write-color-map table index (random max-red) (random max-green) (random max-blue))) 
  (write-color-map table 0 0 0 0)
  (write-color-map table 255 255 255 255))


; this function fills the color table with values interpolated between the low and
; high values passed into the routine

(defun interp-table (table l-red h-red l-green h-green l-blue h-blue
		     &optional (start 0) (end 255))
  (LET* ( (number (- end start))
	 (r-step (/ (- h-red l-red) number))
	 (g-step (/ (- h-green l-green) number))
	 (b-step (/ (- h-blue l-blue) number))
	 (r l-red)
	 (g l-green)
	 (b l-blue) )
    (write-color-map table start r g b) ;;; fixed boo boo - had 0 rather than start as arg.
    (loop for index from start below (+ end 1)
	  do
	  (progn
	    (setf r (+ r r-step))
	    (setf g (+ g g-step))
	    (setf b (+ b b-step))
	    (write-color-map table index (round r) (round g) (round b))))))


;; Added for dual monitors by KJF on 02/10/88
(DEFUN duplicate-colors-for-default-map (the-map)
  "Copy first 128 colors to second 128 colors.  This assumes plane 7
is the monochrome plane."
  (assert (= *default-dual-monitor-monochrome-plane* 7)
	  nil
	  "This function assumes that the monochrome plane - tv:*default-dual-monitor-monochrome-plane* - is plane 7.")
  (LET (r g b)
    (DOTIMES (i 128)
      (MULTIPLE-VALUE-SETQ (r g b) (read-color-map the-map i))
      (write-color-map the-map (+ i 128) r g b))))

;; Added for dual monitors by KJF on 02/10/88
(defun fill-default-map-with-rainbow (color-map
				      &optional (function #'generate-rainbow-for-default-map))
  "This uses function fills default maps with rainbow from slot 32 to slot 127."
  (assert (= *default-dual-monitor-monochrome-plane* 7)
	  nil
	  "This function assumes that the monochrome plane - tv:*default-dual-monitor-monochrome-plane* - is plane 7.")
  (loop for LIST in (funcall function)
	for INDEX from 32. to 128.
	doing (apply #'w:write-color-map COLOR-MAP INDEX LIST)))

;; Added for dual monitors by KJF on 02/10/88
(DEFUN generate-rainbow-for-default-map ()
  "This provides a fairly uniform rainbow from 32 to 127."
  (APPEND
    (loop for ILOOP from 0. to 15.   ;;red to orange
	  collecting (list 255.
			   (round (* ILOOP 12.))
			   0.))
    (loop for ILOOP from 0. to 15.   ;;orange to yellow
	  collecting (list 255.
			   (+ 168. (round (* ILOOP 4.)))
			   0.))
    (loop for ILOOP from 0. to 15.   ;;yellow to green
	  collecting (list (round (- 255. (* ILOOP 14.)))
			   255.
			   0.))
    (loop for ILOOP from 0. to 15.   ;;green to blue
	  collecting (list 0.
			   (round (- 255. (* ILOOP 16.)))
			   (* ILOOP 16.)))
    (loop for ILOOP from 0. to 15.   ;;blue to violet
	  collecting (list (round (* ILOOP 14.))
			   0.
			   255.))
    (loop for ILOOP from 0. to 15.   ;;violet to white
	  collecting (list 255.
			   (round (* ILOOP 16.))
			   255.))))


;;; COLOR-MAP management functions
;;; The following code provides the way to manipulate the color map buffers on the CSIB board.
;;; There are buffers on the board to hold 2 color maps concurrently -- one of which is (usually)
;;; an unmodified version of the one downloaded into the physical LUTs.

;; may 01/27/89 
(DEFUN transfer-color-lut-buffer 
       (&optional (slot *current-color-lut-buffer*))
  "Downloads buffer's data into color LUTs."
  (LET ((slt  (LOGAND slot #o1)))
    (SEND *control-register* :load-lut-from-lut-buffer slt)
    (SETQ *current-color-lut-buffer* slt)
    ;; hack for multiple monitors - download to all CSIBs.  - CJJ 9/15/88
    ;; Change for MMON by KJF for CJJ on 09/19/88.
    (WHEN (mmon-p)
      (DOLIST (fslot *aux-csib-fslots*)
	;; This function is defined by the MMON system.
	(FUNCALL 'csib-load-lut-from-buffer slt fslot))))) 

(DEFUN CURRENT-COLOR-LUT-BUFFER ()
  "Returns which buffer array is currently loaded into the LUTs."
  *current-color-lut-buffer*)



;;; the following function reads one entry directly from the specified on board
;;; color map. Please remember that you CANNOT read the actual LUT, only one
;;; of these two buffers. THIS FUNCTION DEALS DIRECTLY WITH THE HARDWARE, not WITH
;;; SOME COLOR TABLE YOU MAY HAVE CREATED IN A MEMORY BASED DATA STRUCTURE.
;;;  

(DEFUN READ-COLOR-LUT-BUFFER (index &optional (slot *current-color-lut-buffer*))
  "Returns the values of R, G, B"
  (let* ((slt    (logand slot #o01))
	 (red-val    (ldb 8 (aref (aref lut-buffers slt) index)))
	 (green-val  (ldb 8 (aref (aref lut-buffers slt) (+ 256. index))))
	 (blue-val   (ldb 8 (aref (aref lut-buffers slt) (+ 512. index)))))
    (VALUES red-val green-val blue-val)))	; this is what is returned by the function.

;;;
;;; the following function writes a R, G, and B value directly to the specified
;;; color lut buffer on the board. If the buffer specified is the current
;;; buffer, it is loaded down into the LUT so that the LUT and the buffer
;;; match each other. THIS function DEALS DIRECTLY WITH HARDWARE, not WITH 
;;; SOME COLOR MAP YOU MAY HAVE CREATED IN A MEMORY BASED DATA STRUCTURE
;;;

(DEFUN WRITE-COLOR-LUT-BUFFER (index red-val green-val blue-val 
			       &optional (slot *current-color-lut-buffer*))
  "Updates single location in current color lut buffer."
  (let* ((slt (logand slot #o1))
	 (indx (logand index #xFF))) 
    (without-interrupts				;Prevent a process switch
      ;;; now, we have to check the load bit to see if we can write to the buffer
      (COND ((plusp (send *control-register* :lut-buffer-load)))
	    ;;Most of the time it should be ready
	    ((wait-with-timeout 2 #'PLUSP (send *control-register* :lut-buffer-load)))
	    ;;If not then wait
	    (t (SETF *csib-lut-buffer-load-error* `( 'write-color-lut-buffer ,slot))))  
      ;;If timed out, Something is wrong - CSIB should have responded by now
      (setf (aref (aref lut-buffers slt) indx) red-val)
      (setf (aref (aref lut-buffers slt) (+ 256 indx)) green-val)
      (setf (aref (aref lut-buffers slt) (+ 512 indx)) blue-val))
    (when (= slt *current-color-lut-buffer*)
      (transfer-color-lut-buffer slt))))


(defun COMPLEMENT-COLOR-LUT-BUFFER (&optional (slot *current-color-lut-buffer*))
  "Replaces every entry in color LUT buffer with its complement."
  (let* (red-val green-val blue-val
	 (slt (logand slot #o1)))
    (without-interrupts				;Prevent a process switch
      ;; now, we have to check the load bit to see if we can write to the buffer
      (COND ((plusp (send *control-register* :lut-buffer-load)))
	    ;;Most of the time it should be ready
	    ((wait-with-timeout 2 #'PLUSP (send *control-register* :lut-buffer-load)))
	    ;;If not then wait
	    (t (SETF *csib-lut-buffer-load-error* `('complement ,slot))))  
      ;;If timed out, Something is wrong - CSIB should have responded by now
      (loop for index from 0 below 256.
	    do
	    (setf red-val    (ldb 8 (aref (aref lut-buffers slt) index)))
	    (setf green-val  (ldb 8 (aref (aref lut-buffers slt) (+ 256. index))))
	    (setf blue-val   (ldb 8 (aref (aref lut-buffers slt) (+ 512. index))))
	    
	    (setf (aref (aref lut-buffers slt) index) (- 256. red-val))
	    (setf (aref (aref lut-buffers slt) (+ 256. index)) (- 256. green-val))
	    (setf (aref (aref lut-buffers slt) (+ 512. index)) (- 256. blue-val)))) 
    (if (= slt *current-color-lut-buffer*)
	(transfer-color-lut-buffer slt))))

(defun aux-csib-fslots ()
  "Return a list of CSIB slots, excluding the keyboard slot."
  (or *aux-csib-fslots*
      (dolist (slot *csib-slots* *aux-csib-fslots*)
	(when (neq (ldb #o4 slot) (ldb #o4 sib-slot-number))
	  (setq *aux-csib-fslots* (cons (dpb slot #o4 #x+F0) *aux-csib-fslots*))))))

(DEFUN mmon-p ()
  "Function which returns true if the MMON system is loaded.  It checks
for :multiple-monitors on the *features* list."
  (NOT (NULL (MEMBER :multiple-monitors *features*))))

;; redefined for multiple SIB/CSIB. ;; may 01/27/89 
(DEFUN download-color-lut-buffer (the-map &optional (slot *current-color-lut-buffer*))
  "Loads the color-table portion of a color map structure into one of the color map buffer slots."
  
  ;;; if the array is loaded into the buffer that corresponds to the current
  ;;; buffer, then the buffer is loaded into the LUT.
  
  (LET* ((slt (LOGAND slot #o1)))
    (WITHOUT-INTERRUPTS				;Prevent a process switch
      ;;; now, we have to check the load bit to see if we can write to the buffer
      (COND ((PLUSP (SEND *control-register* :lut-buffer-load)))
	    ;;Most of the time it should be ready
	    ((wait-with-timeout 2 #'PLUSP (SEND *control-register* :lut-buffer-load)))
	    ;;If not then wait
	    (t (SETF *csib-lut-buffer-load-error* `('download slot))))  
      ;;If timed out, Something is wrong - CSIB should have responded by now
      (sys:%blt-to-physical (sys:array-data-buffer-address (color-map-table the-map))
			    (physical-address          
			      *lut-buffers-offset*	; points to where buffers are located
			      (* slt #x1000)	; 0 for buffer 0, 1000 for buffer 1.
			      *buffer-red-address*	; this offset points to the proper part.
			      )
			    number-of-color-locations 1))
    ;; Reversed order of following 2 expressions to allow TRANSFER-COLOR-LUT-BUFFER
    ;;  instead of CSIB-MAP-SET to handle transferring the buffer into the LUT on auxilliary CSIBs.
    ;;  CJJ 9/15/88.  MMON change by KJF for CJJ on 09/19/88.
    ;; hack for multiple monitors - download to all CSIBs.  - GRH
    (WHEN (mmon-p)
      (DOLIST (fslot *aux-csib-fslots*) ;;(aux-csib-fslots))
	;; this function is defined by the MMON system.
	(FUNCALL 'csib-map-set the-map fslot slt)))
    (WHEN (= slt *current-color-lut-buffer*)
      (transfer-color-lut-buffer slt))))


(DEFUN GET-COLOR-LUT-BUFFER  (the-map &optional (slot *current-color-lut-buffer*))
  "Loads the specified color-lut-buffer into the color table portion of a color map structure."
  (let* ((slt (logand slot #o1)))
    (sys:%blt-from-physical  (physical-address          
			       *lut-buffers-offset*	; points to where buffers are located
			       (* slt #x1000)	; 0 for buffer 0, 1000 for buffer 1.
			       *buffer-red-address*	; this offset points to the proper part.
			       )
			     (sys:array-data-buffer-address (color-map-table the-map)) 
		             number-of-color-locations 1)))

;;; if *color-system* is nil, this creates a b&w version of the map
;; NO, not anymore.  In Release 4.0, we made a type of color map
;; that works for all cases: B&W, color, or dual-monitors.
;; create-color-map will create a "default" type color map
;; unless explicitly specifying another type such as:
;; :B&W or :color.  02/10/88 KJF.
(add-initialization "Initialize Color Map"
		    '(setf *default-color-map* (create-color-map))
		    :once)
(pushnew '*default-color-map* *color-maps*)
