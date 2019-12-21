

(defparameter *inspected-forms* '())

;; Representation of file is:
;; (:file-name <file-name>
;;    :contents (:forms
;;                 (:form <form>
;;                  :end-position <char-pos>
;;                  :text  <text from char-pos to char-pos>)
(defvar *parsed-files* '())

(defvar *symbols-packages* '())

(defparameter *files-to-process*
  '((:directory "/home/rett/dev/common-lisp/dwim/dwim-git/ti-tv/window-mx/"
     :files
     ("definitions.lisp"
      "basic-comm-definitions.lisp"
      "basic-comm-methods.lisp"
      "basic-comm-protocols.lisp"
      "compile-last.lisp"
      "defsystem.lisp"
      "fast-rubberbanding.lisp"
      "fed-functions.lisp"
      "font-translations.lisp"
      "function-name.lisp"
      "mac-initiated-commands.lisp"
      "mac-messages.lisp"
      "mac-wholin.lisp"
      "mac-windows.lisp"
      "mouse-tracking.lisp"
      "mx-tv-module-ops.lisp"
      "package.lisp"
      "qix.lisp"
      "variables.lisp"))))



;; (let* ((function-object (sb-kernel::%fun-fun function))
;;        (function-header (sb-kernel:fun-code-header function-object))
;;        (debug-info (sb-kernel:%code-debug-info function-header))
;;        (debug-source (sb-c::debug-info-source debug-info))
;;        (debug-fun (debug-info-debug-function function debug-info)))
;;   (sb-c::%make-definition-source-location
;;    (sb-c::debug-source-namestring debug-source)
;;    (sb-c::compiled-debug-info-tlf-number debug-info)
;;    (sb-c::compiled-debug-fun-form-number debug-fun)))

(defparameter *current-file*
  (open "/home/rett/dev/common-lisp/dwim/dwim-git/ti-tv/window-mx/definitions.lisp"))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defparameter *lispm-packages*
    '(tv))

  (dolist (lispm-package *lispm-packages*)
    (unless (find-package lispm-package)
      (make-package lispm-package))))

(defun stream-eof-p (stream)
  (eq (peek-char nil stream nil :eof) :eof))


(defun run-process-file ()
  (process-file *current-file*))

(defun process-file (file)
  (do ()
      ((stream-eof-p *current-file*)
       (values *parsed-files* *symbols-packages*))
    (handler-case  (parse-file *current-file*)
      (sb-int:simple-reader-package-error (condition)
	;; In SBCL two element list: (<symbol-name> <package-name>)
	(let ((symbol-package-list
	       (if (slot-exists-p condition 'sb-kernel::format-arguments)
		   (slot-value condition 'sb-kernel::format-arguments))))
	  (cond
	    ;; symbol-package-list contains both symbol and package
	    ;; strings respectively.
	    ((= (length symbol-package-list) 2)
	     (intern (car symbol-package-list) (cadr symbol-package-list))
	     (export (intern (car symbol-package-list) (cadr symbol-package-list))
		     (cadr symbol-package-list)))
	    ((= (length symbol-package-list) 1)
	     (make-package (car symbol-package-list))))
	  (format t "simple-reader-error: ~s~%" symbol-package-list)
	  (push symbol-package-list *symbols-packages*)
	  (file-position *current-file* :start))))))

(defun parse-file (file-stream)
  (let (locations forms)
    (do ((form (read file-stream nil :eof )
	       (read file-stream nil :eof )))
	((eq form :eof)
	 (push (list :file-name
		     (list (pathname-name file-stream)
			   (pathname-name file-stream))
		     :contents
		     (list :forms forms
			   forms))
	       *parsed-files*))
      (push (list :form form
		  :end-position (file-position file-stream))
	    forms))))

;;   (condition () "Hardly worth mentioning.")))

;; (handler-case ((sb-int:simple-reader-error
;; 		  (lambda (condition)
;; 		    (format t "simple-reader-error: ~s~%"
;; 			    (if (slot-exists-p condition 'SB-KERNEL::FORMAT-ARGUMENTS)
;; 				(slot-value condition 'SB-KERNEL::FORMAT-ARGUMENTS)))))
;; 		 (sb-int:simple-reader-package-error
;; 		  (lambda (condition)
;; 		    (format t "simple-reader-package-error: ~s~%"
;; 			    (if (slot-exists-p condition 'format-arguments)
;; 				(slot-value condition 'format-arguments)))))
;; 		 (error #'(lambda (condition)
;; 			    (format t "Other error: ~s~%"  condition))))
;;   (parse-file *current-file*)))
