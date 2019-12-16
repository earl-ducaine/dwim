;;; -*- Syntax: Common-lisp; Package: DWIM -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :dwim)


;;; Operations associated with tv windows (mostly).


(defun window-under-mouse ()
  (clim-internals::find-appropriate-window *standard-input*))

(defun window-clear (window)
  (clim:window-clear window))

(defun change-size (window new-width new-height)
  (clim::window-set-inside-size window new-width new-height))

(defun pane-frame (pane)
  (clim:pane-frame pane))

(defun redisplay-frame-pane (pane &optional (force-p t))
  ;; force-p is probably wrong for redisplayable panes:
  (clim:redisplay-frame-pane (pane-frame pane) pane :force-p force-p))

(defun sheet-parent (sheet)
  (clim:sheet-parent sheet))

(defmethod (setf sheet-parent) (new sheet)
  (setf (clim:sheet-parent sheet) new))

(defun stream-current-text-style (stream)
  (clim:medium-text-style stream))

(defun stream-merged-text-style (stream)
  (clim:medium-merged-text-style stream))

(defun parse-text-style (list)
  (clim:parse-text-style list))

(defun stream-line-height (stream &optional text-style)
  (if text-style
      (truncate (clim:stream-line-height stream :text-style text-style))
      (truncate (clim:stream-line-height stream))))

(defun stream-character-width (stream &optional (char #\m))
  ;; "m" is the usual character (the term "ems" is often used in typesetting
  ;; to indicate units of width).
  (if (clim:extended-output-stream-p stream)
      (clim:stream-character-width STREAM char)
      8))

(defun stream-string-width (stream string &key (start 0) end text-style)
  (clim:stream-string-width stream string :start start :end end
			    :text-style text-style))

(defmethod stream-cursor-position* (stream)
  (if (clim:extended-output-stream-p stream)
      (multiple-value-bind (x y)
	  (clim:stream-cursor-position stream)
	;; Its nice to assume that cursor positions are fixnums,
	;; even though postscript streams meaningfully use floats.
	(values (truncate x) (truncate y)))
      (values 0 0)))

(defmethod stream-set-cursor-position* (stream x y)
  (setf (clim:stream-pointer-position stream) (values x y)))

(defmethod stream-increment-cursor-position* (stream x y)
  (clim:stream-increment-cursor-position stream x y))

(defmethod stream-viewport (stream)
  (cond ((not (clim:extended-output-stream-p stream)))
	((and (type-specifier-p 'clim-postscript::postscript-stream)
	      (typep stream 'clim-postscript::postscript-stream))
	 ;; width  = inches x 72
	 ;; height = inches x 72
	 (values 0 0 #.(* 72 7) #.(* 72 10)))
	(t
	 (let ((v (clim:window-viewport stream)))
	   (if v (clim:rectangle-edges* v)
	       (values 0 0
		       (clim:bounding-rectangle-width stream)
		       (clim:bounding-rectangle-height stream)))))))

(defmethod stream-viewport-size (stream)
  (multiple-value-bind (left top right bottom) (stream-viewport stream)
    (values (- right left) (- bottom top))))

(defmacro sheet-inside-size (stream)
  `(stream-viewport-size ,stream))

(defun stream-height (stream)
  "Height of the viewport."
  (multiple-value-bind (ignore height)
      (stream-viewport-size stream)
    (declare (ignore ignore))
    height))

(defmacro sheet-inside-width (stream)
  `(values (stream-viewport-size ,stream)))


(defmacro sheet-inside-height (stream)
  `(stream-height ,stream))

(defmacro sheet-left-margin-size (stream)
  (declare (ignore stream))
  ;; KRA: 4/11/90: CLIM Doesn't use margins.
  0)


(defmacro sheet-top-margin-size (stream)
  (declare (ignore stream))
  0)

(defun beep () (clim:beep))


;;; Mouse stuff


(defun interactive-stream-p (stream)
  (clim:extended-input-stream-p stream))

(defmethod stream-set-pointer-position* (stream x y)
  "Set position of mouse, in stream coordinates."
  (setf (clim:stream-pointer-position stream) (values x y)))


(defmethod stream-pointer-position* (stream)
  "Get position of mouse, in stream coordinates."
  (multiple-value-bind (x y)
      (clim:stream-pointer-position stream)
    (values (truncate x) (truncate y))))

(defun pointer-input-rectangle* (&key (stream *standard-input*) left top right bottom)
  (declare (ignore stream left top right bottom))
  ;; not implemented in clim
  )


;;; Frame stuff


(defvar *default-server-path*
  '(:motif))

(defvar *sheet-roots* nil)
(defvar *deactivated-frames* nil)
(defvar *activated-frames* nil)

(defmethod frame-top-level-process ((frame t))
  "Access the process associated with this frame."
  (second (car (member frame *activated-frames* :key #'car))))

(defun frame-manager (frame)
  (clim:frame-manager frame))

(defun find-frame-manager (&key (if-exists :reuse))
  (declare (ignore if-exists))
  (clim:find-frame-manager))

(defun get-reusable-frame (manager type)
  (let ((choices (clim:frame-manager-frames manager)))
    (dolist (item choices)
      (when (and (typep item type) (eq (clim:frame-state item) :disabled))
	(return item)))))

(defun deactivate-frame (frame)
  (setq *activated-frames* (remove frame *activated-frames* :key #'car))
  (push frame *deactivated-frames*))

(defmethod reset-frame (frame &key title)
  "Prepare a frame for reuse."
  (setf (clim:frame-pretty-name frame) title)
  (clim:reset-frame frame))

;;; You pay a price for this, so set it to nil if resources are scarce.
(defvar *enable-backing-store* :when-mapped
  "One of :always, :when-mapped, :not-useful, or nil")

(defmethod start-frame (frame &key
				(wait-until-done t)
				master
				(backing-store *enable-backing-store*))
  (cond (master
	 (let ((b (clim:stream-input-buffer
		   (clim:frame-top-level-sheet master)))
	       (top-level-window (clim:frame-top-level-sheet frame)))
	   (labels ((set-input-buffer (window buffer)
		      (setf (clim:stream-input-buffer window) buffer)
		      (dolist (w (clim::window-children window))
			(set-input-buffer w buffer))))
	     (set-input-buffer top-level-window b)
	     (clim::window-expose top-level-window)
	     (clim:redisplay-frame-panes frame :force-p t)
	     ;; return the window just created
	     (values top-level-window))))
	((not wait-until-done)
	 (process-run-function
	  "Frame Top Level"
	  'start-frame frame
	  :wait-until-done t
	  :master nil
	  :backing-store backing-store)
	 frame)
	(T
	 (push (list frame :current-process) *activated-frames*)
	 (unwind-protect
	      (progn
		(clim:run-frame-top-level frame))
	   (deactivate-frame frame)))))

(defun make-application-frame (type &key parent title
				      (left 10) (top 10)
				      (width 500) (height 500))
  ;; what parent does this get?
  (declare (ignore parent))
  (let ((frame (clim:make-application-frame
		type
		:pretty-name title
		:left left :top top
		:width width :height height)))
    frame))

(defmethod size-frame (frame width height)
  (clim:layout-frame frame width height))

(defmethod move-frame (frame left bottom)
  (clim::position-sheet-carefully
   (clim:frame-top-level-sheet frame)
   left bottom))

(defmethod get-frame-pane (frame pane-name)
  (clim:get-frame-pane frame pane-name))

(defmethod frame-current-layout (frame)
  (clim:frame-current-layout frame))

(defmethod set-frame-layout (frame new-layout)
  (unless (eq new-layout (frame-current-layout frame))
    (setf (clim:frame-current-layout frame) new-layout)))

(defmethod window-set-viewport-position* (stream left top)
  (setf (clim:window-viewport-position stream) (values left top)))

(defmethod window-history-limits (stream)
  (let ((history (clim:stream-output-history stream)))
    (clim:bounding-rectangle* history)))

(defmethod select-frame (frame)
  (clim-internals::note-frame-deiconified (clim:frame-manager frame) frame)
  (clim:raise-sheet (clim:frame-top-level-sheet frame))
  frame)

(defun suggest-frame-size (frame-manager width height)
  (let ((graft (clim:graft frame-manager)))
    (when graft
      (setq width width
	    height height))
    (values width height)))

(defun launch-frame
    (type
     &key
       (backing-store :when-mapped)	; specific to X windows
       create				; NIL => try first to reuse an old instance
       master
       (title "Window Frame")
       (left 0) (bottom 0)
       (width 600) (height 400)
       (wait-until-done nil)		; T => spawn its own process
       (initializer nil)			; function of 1 arg
       &allow-other-keys)
  "The preferred way to make and expose an application frame."
  ;; MASTER is either NIL or another frame.
  ;; If it is a frame, the second frame acts as an extension of the first.
  (let* ((manager (if master (frame-manager master)
		      (find-frame-manager)))
	 (frame (if (not create) (get-reusable-frame manager type))))
    (when frame (reset-frame frame :title title))
    (if frame
	(size-frame frame width height)
	(setq frame (make-application-frame type
					    ;;:left (max 0 left) :top (max 0 (- height bottom))
					    :parent manager
					    :width width :height height
					    :title title)))
    (move-frame frame (max 0 left) (max 0 bottom))
    (multiple-value-bind (w h) (suggest-frame-size manager width height)
      (when (or (not (eql w width)) (not (eql h height)))
	(size-frame frame w h)))
    (when initializer
      (let* ((application #+clim frame
			  #-clim (scl:send frame :program))
	     #-clim
	     (dw:*program* application)
	     #-clim
	     (dw:*program-frame* frame)
	     #+(and clim (not clim-0.9))
	     (clim:*application-frame* frame))
	(funcall initializer application)))
    (start-frame frame
		 :wait-until-done wait-until-done
		 :master master
		 :backing-store backing-store)))

(defmethod frame-exit (FRAME)
  (clim:frame-exit FRAME))

(defmacro for-each-frame ((symbol) &body body)
  "Iteratively bind SYMBOL to all enabled frames."
  `(clim:map-over-ports
    #'(lambda (port)
	(unless (eq (clim:port-type port) :postscript)
	  (dolist (,symbol (clim:frame-manager-frames
			    (clim:find-frame-manager :port port)))
	    (when (member (clim:frame-state ,symbol) '(:shrunk :enabled))
	      ,@body))))))

(defun find-program-window (name &key
				   (create-p nil)
				   (wait-until-done nil)
				   (width 500)
				   (height 500))
  (progn
    (for-each-frame (f)
      (when (typep f name)
	(return-from find-program-window f)))
    (when create-p
      (launch-frame name
		    :title (string name)
		    :wait-until-done wait-until-done
		    :width width
		    :height height))))
