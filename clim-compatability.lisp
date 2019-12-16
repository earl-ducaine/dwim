
;; Note, the package :clim does not use symbols from CL so of course
;; we can do things like define a function when 'in' it. The purpose
;; of :dwim-clim-util is to provide the package in which we define
;; symbols for :clm

(in-package :dwim-clim-util)

;; More or less a stupid version of the method from CLIM2. Function is
;; completely absent in McClim
(defmethod clim::host-window-margins ((stream clim-internals::window-stream))
  (let* ((parent (clim::window-parent stream)))
    (cond ((not (clim::window-parent parent))
	   (values (clim:coordinate 0) (clim:coordinate 0)
		   (clim:coordinate 0) (clim:coordinate 0)))
	  (t
	   (with-slots (clim::border-width) stream
	     (values border-width border-width border-width border-width))))))

(defmethod clim::window-inside-edges ((stream clim:sheet))
  (clim:bounding-rectangle*
   (clim:sheet-region (or (clim:pane-viewport stream) stream))))

(defmethod clim::window-set-inside-edges ((stream clim-internals::window-stream)
					  new-left new-top new-right new-bottom)
  (multiple-value-bind (lom tom rom bom) (clim::host-window-margins stream)
    (clim::bounding-rectangle-set-edges
     stream
     (- (coordinate new-left)   lom)
     (- (coordinate new-top)    tom)
     (+ (coordinate new-right)  rom)
     (+ (coordinate new-bottom) bom))))

(defun clim::window-parent (window)
  (clim:sheet-parent window))

(defmethod clim::window-set-inside-size
    ((window clim-internals::window-stream) new-width new-height)
  (multiple-value-bind (left top) (clim::window-inside-edges window)
    (window-set-inside-edges window left top (+ left new-width) (+ top new-height))))

(defun clim-internals::find-appropriate-window (stream)
  ;;--- How do we hack multiple pointers?
  (when (extended-input-stream-p stream)
    (let* ((pointer (stream-primary-pointer stream))
           (window (pointer-sheet pointer)))
      ;; It ain't no good if it doesn't have a history.
      (when (and window
                 (port window)
                 (output-recording-stream-p window))
        window))))

(defun clim::window-children (window)
  (clim::sheet-children window))

(defmethod clim::window-expose ((stream clim:sheet))
  (setf (clim::window-visibility stream) t))

;; Moves the sheet to the specified position, taking care not to move
;; it outside of the graft.  It's safest to use this on a top-level sheet.
(defun clim::position-sheet-carefully (sheet x y)
  (multiple-value-bind (width height) (clim::bounding-rectangle-size sheet)
    (multiple-value-bind (graft-width graft-height)
        (clim::bounding-rectangle-size (or (graft sheet) (find-graft)))
      (let* ((left x)
             (top y)
             (right (+ left width))
             (bottom (+ top height)))
        (when (> right graft-width)
          (setq left (- graft-width width)))
        (when (> bottom graft-height)
          (setq top (- graft-height height)))
        (clim::port-move-frame (port sheet) (pane-frame sheet)
			 (max 0 left) (max 0 top))))))
