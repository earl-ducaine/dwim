;;; -*- Mode:Common-Lisp; Package:MACINTOSH; Base:10; Fonts:(COURIER HL12B HL12BI COURIER MEDFNB) -*-

(PROCLAIM '(SPECIAL *calls* *images* *tracing-off*
		    *ci-waited* *ack-waited*))

(DEFVAR *list-to-print* nil)

(DEFUN dr (&optional (debugging-classes-to-enable nil) and-return)
  "Reset debugging info."
  (LET ((c *calls*))
    (COND
      ((NULL debugging-classes-to-enable)  ; Turn debugging on, all classes enabled.
       (SETF *tracing-off* nil)
       (SETF *enabled-debugging-classes* *all-debugging-classes*)
       (SETF debugging-classes-to-enable t))
      ((EQ debugging-classes-to-enable t)  ; Turn debugging off but leave enabled
       (SETF *tracing-off* t)
       (SETF debugging-classes-to-enable nil))
      (t				   ; Turn debugging on, may specify one or
					   ; more classes to
       (WHEN (SYMBOLP debugging-classes-to-enable) ; enable.
	 (SETF debugging-classes-to-enable (LIST debugging-classes-to-enable)))
       (LOOP FOR debugging-class IN debugging-classes-to-enable
	     UNLESS (MEMBER debugging-class *all-debugging-classes*)
	     DO (FERROR nil "~a not a debugging-class-name" debugging-class))
       (SETF *tracing-off* nil)
       (SETF *enabled-debugging-classes* debugging-classes-to-enable)))
    ;;  Prevent the :fatal class of debug traces from ever being turned off...
    (UNLESS (MEMBER :fatal *enabled-debugging-classes*)
      (PUSH :fatal *enabled-debugging-classes*))
    ;;  If we must return *calls*'s contents, break the circular chain... 
    (WHEN and-return
      (RPLACD (NTHCDR (- *length-of-circular-list* 2) *calls*)  nil))
       ;;  Wipe out *calls* with a new circular list of NILs...
    (SETF *calls* (MAKE-LIST *length-of-circular-list*))
    (RPLACD (LAST *calls*) *calls*)
    ;;  Return the contents of *calls* if so requested...
    (WHEN and-return
      (SETF *list-to-print* c)
      (LOOP for e IN c counting e))))

(DEFUN suppress-tracing-p (&optional (debugging-class :fatal))
  "Returns T iff no tracing is to be done by REMEMBER-CALL."
   (OR *tracing-off*
       (NOT (MEMBER debugging-class *enabled-debugging-classes*))
       ))


(DEFUN si:name-of-function (fef)
  "Given a function spec FEF, returns a string of the print name of the
function spec prefixed with its package."
  (LET (dbi name)
    (IF (SYMBOLP fef) (SETF fef (SYMBOL-VALUE fef)))
    (SETQ dbi (si:%p-contents-offset fef si:%fef-debugging-info-word))
    (IF dbi
	(SETQ name (si:get-debug-info-field dbi :name))
      (SETF name "unknown"))
    (FORMAT nil "~s" name)))



(DEFUN report-caller (&optional stream)
  si:(LET* ((sg %current-stack-group)
	    (rp (sg-regular-pdl sg))
	    (callers-frame
	      (eh:sg-next-frame
		sg (%pointer-difference
		     (%stack-frame-pointer) (LOCF (AREF rp 0)))))
	    (callers-callers-frame
	      (eh:sg-next-frame
		sg callers-frame)))
       (FORMAT stream "~a called by ~a running ~a."
	       (name-of-function (rp-function-word rp callers-frame))
	       (name-of-function (rp-function-word rp callers-callers-frame))
	       current-process)))

(DEFUN report-all-frames (&optional (stream t) (max-depth 300))
  (CONS 
    si:(LOOP for depth from 1
	     for sg =  %current-stack-group
	     for rp = (sg-regular-pdl sg)
	     for frame first (eh:sg-next-frame sg (eh:sg-innermost-frame sg))
	     then (eh:sg-next-frame sg frame)
	     until (OR (NULL frame) (> depth mac:max-depth))
	     for func-and-args = (EH:GET-FRAME-FUNCTION-AND-ARGS sg frame)
	     when (NULL stream) collect func-and-args
	     else do
	     (FORMAT stream "~&    called by ~a"
		     (EH:GET-FRAME-FUNCTION-AND-ARGS sg frame)
					   ;(name-of-function (rp-function-word rp frame))
		     ))
    ;;  Capture the current process always...
    (WITH-OUTPUT-TO-STRING (s) (SEND current-process :print-self s))))


(DEFUN ho-ha ()
  "A batch of predictaes sometimes useful to include in
suppress-tracing-p."
   (AND (NOT remember-call-regardless)
       (OR *tracing-off*
	   (si:function-active-p 'tv:who-line-run-state-update)
	   (AND				   ; (NOT (si:function-active-p 'tv:edit-screen))
	     (OR (si:function-active-p 'tv:open-blinker)
		 (si:function-active-p 'tv:mouse-set-blinker-cursorpos)
		 (si:function-active-p 'tv:mouse-default-handler)))
	   (si:function-active-p 'sys:process-scheduler-for-chaparral)))
  ;(NOT (tv:key-state :mode-lock))
      (si:function-active-p 'tv:make-window)
      (si:function-active-p 'tv::screen-manage-sheet)
      (si:function-active-p 'tv::draw-shadow-border)
      (si:function-active-p 'tv:who-line-run-state-update) 
      (AND ; (NOT (si:function-active-p 'tv:edit-screen))
	   (OR (si:function-active-p 'tv:open-blinker)
	       (si:function-active-p 'tv:mouse-set-blinker-cursorpos)
	       (si:function-active-p 'tv:mouse-default-handler)))
      (si:function-active-p 'sys:process-scheduler-for-chaparral))


(DEFUN get-callers ()
  (LOOP for symbol in '(%draw-char %draw-rectangle %draw-shaded-triangle %draw-line %draw-string bitblt)
	for sys-symbol = (FIND-SYMBOL (SYMBOL-NAME symbol) 'sys)
	for tv-symbol = (FIND-SYMBOL (SYMBOL-NAME symbol) 'tv)
	when sys-symbol
	collect (LIST sys-symbol
		      (WHO-CALLS sys-symbol 'sys)
		      (WHO-CALLS sys-symbol 'tv)
		      (WHO-CALLS sys-symbol 'zwei))
	when tv-symbol
	collect (LIST tv-symbol
		      (WHO-CALLS tv-symbol 'tv)
		      (WHO-CALLS tv-symbol 'zwei))))

(DEFUN clean-up1 (list-of-callers)
  (LOOP FOR (symbol . caller-lists) IN list-of-callers
	COLLECT (LIST symbol
		      (LOOP FOR caller-list IN caller-lists
			    COLLECT (LOOP FOR caller IN caller-list
					  UNLESS (AND (LISTP caller)
						      (EQ (FIRST caller) :PROPERTY)
						      (EQ (THIRD caller) :PREVIOUS-DEFINITION))
					  COLLECT caller)))))

(DEFUN who-did-we-miss? (list-of-callers)

  (LOOP FOR (symbol . caller-lists) IN list-of-callers
	COLLECT (LIST symbol 
		      (LOOP FOR caller-list IN (FIRST caller-lists)
			    APPEND
			    (LOOP FOR caller IN caller-list
				  FOR source-property = (GET caller :source-file-name)
				  
				  UNLESS
				  (COND ((LISTP source-property)
					 (LOOP FOR source-file IN (REST (FIRST source-property))
					       WHEN (EQUAL '("MAC") (SEND source-file :directory))
					       RETURN T))
					(t (EQUAL '("MAC") (SEND source-property :directory))))
				  COLLECT caller)))))

(defun remove-dups (list-of-callers)
  (LET (list-so-far)
      (LOOP FOR (symbol . caller-lists) IN list-of-callers
	    DO
	    (LOOP FOR caller-list IN caller-lists
		  DO
		  (LOOP FOR caller IN caller-list
			UNLESS (MEMBER caller list-so-far)
			DO (PUSH caller list-so-far))))
      list-so-far))

(DEFUN debugging-stuff ()
  (breakon 'sys:%draw-char
	   '(AND
	      ;;  Don't break unless we're holding down the right shift key...
	      (tv:key-state :right-shift)
	      ;;  Don't break if we're drawing a (mouse) blinker...
	      (AND (not (si:function-active-p 'tv:open-blinker))
	             (NOT (si:function-active-p 'tv:blinker-clock)))
	      ;;  Don't break if we're drawing on one of the sheets of the wholine screen...
	      (or (null tv:prepared-sheet)
		  (neq (tv:sheet-superior tv:prepared-sheet)
		       (third TV:ALL-THE-SCREENS)))))
  (breakon 'w:sheet-insert-char
	   '(AND
	      ;;  Don't break unless we're holding down the right shift key...
	      (tv:key-state :right-shift)
	      ;;  Don't break if we're drawing a (mouse) blinker...
	      (AND (not (si:function-active-p 'tv:open-blinker))
	             (NOT (si:function-active-p 'tv:blinker-clock)))
	      ;;  Don't break if we're drawing on one of the sheets of the wholine screen...
	      (or (null tv:prepared-sheet)
		  (neq (tv:sheet-superior tv:prepared-sheet)
		       (third TV:ALL-THE-SCREENS)))))
  (breakon 'tv:draw-string-internal
	   '(AND
	      ;;  Don't break unless we're holding down the right shift key...
	      (tv:key-state :right-shift)
	      ;;  Don't break if we're drawing a (mouse) blinker...
	      (AND (not (si:function-active-p 'tv:open-blinker))
	             (NOT (si:function-active-p 'tv:blinker-clock)))
	      ;;  Don't break if we're drawing on one of the sheets of the wholine screen...
	      (or (null tv:prepared-sheet)
		  (neq (tv:sheet-superior tv:prepared-sheet)
		       (third TV:ALL-THE-SCREENS)))))
  )

(DEFUN print-list (&key (name "junk")
		     (filename (make-PATHNAME 
				  :defaults (rest (assoc si:local-host *default-pathname-defaults*))
				  :name name  :type "dr"))
		     (editor-p t)
		     (list-to-print *list-to-print*) length level)
  (LET ((*print-pretty* t) (*print-length* length) (*print-level* level) (i 0)
	universal-time real-list
	(s (IF editor-p (ZWEI:MAKE-FILE-BUFFER-STREAM FILENAME)
			 (OPEN filename :direction :output :if-exists
				    :new-version))))
    (UNWIND-PROTECT
	(PROGN
	  (WHEN editor-p (SEND s :delete-text) (CLOSE s) (SETF s (ZWEI:MAKE-FILE-BUFFER-STREAM FILENAME)))
	  (SEND s :line-out ";;; -*- Mode:Common-Lisp; Package:USER; Fonts:(CPTFONTB) -*-")
	  (LOOP for rest-of-list on list-to-print
		until (FIRST rest-of-list)
		finally
		(FORMAT t "
Printing a ~d entry list:" (LENGTH rest-of-list))
		(DOLIST (entry rest-of-list)
		  (WHEN entry
		    (SETF universal-time (FIRST entry)
			  real-list (REST entry))
		    (TYPECASE universal-time
		      (integer
		       (FORMAT t " ~d" (INCF i))
		       (FORMAT s "~%~%~%~%")
		       (time:print-universal-time universal-time s)
		       (FORMAT s "~%~s" real-list))
		      (t
		       (FORMAT t " ~d" (INCF i))
		       (FORMAT s "~%~%~%~%~s" entry)))))))
      ;; but always...
      (CLOSE s))))
  
(DEFUN report-histogram-as-list (histogram-array)
  (LOOP with cum = 0
	FOR i FROM 0
	FOR x BEING THE ARRAY-ELEMENTS OF histogram-array
	WHEN (OR (LISTP x) (AND (NUMBERP x) (NOT (ZEROP x))))
	COLLECT (LIST i x)
	WHEN (NUMBERP x) do (INCF cum x)
	finally (FORMAT t "CUM = ~d." cum)))

;;(DEFUN keith (&optional (reset nil))
;;  "Reports statistics collected by tv:draw-string-internal."
;;  (LET ((strings (MAKE-ARRAY 261.)) median mode)
;;    (SETF median
;;	  (loop for i from 0 to 255.
;;		for count = (aref strings i)
;;		summing count into cum
;;		when (> cum (/ (AREF strings 260.) 2))
;;		return i))
;;    (LOOP with max-count = -1
;;	  for i from 0 to 255
;;	  for count = (aref strings i)
;;	  when (> count max-count)
;;	  do (SETF max-count count mode i))
;;    (FORMAT t "~%~d strings, total length ~d.
;;Mean length of ~4,1f, ~
;;a median length of ~d, a mode length of ~d.~%"
;;	    (AREF strings 260.) (AREF strings 259)
;;	    (FLOAT (/  (AREF strings 259) (AREF strings 260.))) median mode)
;;    (FORMAT t "~%  String  Freq-  Per-   Cumulative  Cumulative")
;;    (FORMAT t "~%  Length  uency   Cent   Frequency   Percent")
;;    (loop for i from 0 to 255.
;;	  for count = (aref strings i)
;;	  summing count into cum
;;	  unless (zerop count)
;;	  do
;;	  (format t "~%   ~3d   ~5d   ~4,1f%   ~6d       ~4,1f%."
;;		  i count (* 100.0 (/ count (aref strings 260.)))
;;		  cum (* 100.0 (/ cum (aref strings 260.))))
;;	  finally )
;;    ))

;(DEFUN back-to-back-acbs ()
;  (MAP-RESOURCE #'(lambda (acb in-use-p resource)
;		    (WHEN in-use-p
;		      (FORMAT t "~% ~a~30t~d"
;			      (add:owner acb)
;			      (ARRAY-LEADER acb si:%acb-leader-spare-1))))
;		'add:acb-resource))