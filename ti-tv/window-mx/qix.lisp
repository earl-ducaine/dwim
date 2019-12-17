;;;;-*- Mode:Common-Lisp; Package:MAC; Base:10; Fonts:(COURIER HL12B HL12BI) -*-

(defvar qix-list nil)
(defvar qix-list-length nil)

(defun qix (&optional (length 100) (stream *terminal-io*) (times NIL))
  "Non-consing QIX."
  (let* ((list (if (and qix-list (<= (1+ length) qix-list-length))
		    qix-list
		    (progn
		      (setq qix-list-length length
			    qix-list (make-list (1+ qix-list-length)))
		      ;; Make history a circular list.
		      (si:%p-store-cdr-code (cdr (nthcdr (1- length) qix-list)) cdr-error)
		      (si:%p-store-cdr-code (nthcdr (1- length) qix-list) cdr-normal)
		      (rplacd (nthcdr (1- length) qix-list) qix-list)
		      (loop repeat length
			    for h =(nthcdr (1- length) qix-list) then (cdr h)
			    do (setf (car h) (make-list 4)))
		      qix-list)))
	  (history (nthcdr (1- length) list)))
    (LOOP FOR blinker in (tv:sheet-blinker-list stream)
	  WHEN (SEND blinker :follow-p)
	  DO (SEND blinker :set-visibility nil))
     (send stream :clear-screen)
     (multiple-value-bind (xlim ylim)
	 (send stream :inside-size)
       (loop with x1 = (random xlim)
	     and y1 = (random ylim)
	     and x2 = (random xlim)
	     and y2 = (random ylim)
	     and dx1 = 5
	     and dy1 = 12
	     and dx2 = 12
	     and dy2 = 5
	     with tem and tem1 = 0
	     until (if times (= (setf times (1- times)) 0) NIL)
	     when (caar history)
	     do (tv:prepare-sheet (stream)
			      (si:%draw-line
			        (first (car history))(second (car history))
			        (third (car history))(fourth (car history))
				tv:alu-xor nil stream))
	     do
	     (WHEN (ZEROP (MOD (INCF tem1) 50))
	       (PROCESS-ALLOW-SCHEDULE))
	     (setf (first (car history)) x1)
	     (setf (second (car history)) y1)
	     (setf (third (car history)) x2)
	     (setf (fourth (car history)) y2)
	     (setf history (cdr history))
	     (tv:prepare-sheet (stream)
			      (si:%draw-line
			        x1 y1 x2 y2
				tv:alu-xor nil stream))
	     (setf dx1 (1- (+ dx1 (random 3)))
		   dy1 (1- (+ dy1 (random 3)))
		   dx2 (1- (+ dx2 (random 3)))
		   dy2 (1- (+ dy2 (random 3))))
	     (cond ((> dx1 12) (setf dx1 12))
		   ((< dx1 -12) (setf dx1 -12)))
	     (cond ((> dy1 12) (setf dy1 12))
		   ((< dy1 -12) (setf dy1 -12)))
	     (cond ((> dx2 12) (setf dx2 12))	
		   ((< dx2 -12) (setf dx2 -12)))
	     (cond ((> dy2 12) (setf dy2 12))
		   ((< dy2 -12) (setf dy2 -12)))
	     (cond ((or (>= (setf tem (+ x1 dx1)) xlim)
			(minusp tem))
		    (setf dx1 (- dx1))))
	     (cond ((or (>= (setf tem (+ x2 dx2)) xlim)
			(minusp tem))
		    (setf dx2 (- dx2))))
	     (cond ((or (>= (setf tem (+ y1 dy1)) ylim)
			(minusp tem))
		    (setf dy1 (- dy1))))
	     (cond ((or (>= (setf tem (+ y2 dy2)) ylim)
			(minusp tem))
		    (setf dy2 (- dy2))))
	     (setf x1 (+ x1 dx1)
		     y1 (+ y1 dy1)
		     x2 (+ x2 dx2)
		     y2 (+ y2 dy2))
	  finally (loop repeat length
			when (caar history)
			 do (tv:prepare-sheet (stream)
			      (si:%draw-line
			        (first (car history))(second (car history))
			        (third (car history))(fourth (car history)) 
				tv:alu-xor nil stream))
			do (setf history (cdr history))))))
    )