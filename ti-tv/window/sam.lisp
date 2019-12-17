;;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(CPTFONT HL12B HL12I) -*-


;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1989 Texas Instruments Incorporated. All rights reserved.

;;;;;;                SYSTEM ACCESS SOFTWARE

;;;  Definitions:
;;;     system access -- the interface between the user and the systems available from
;;;                        his computer (i.e. the System Menu and the SYSTEM keys).
;;;     system access list -- the system-specific information concerning how to access
;;;                            the system.  This information is kept on the system's
;;;                            Defsystem.  It is a property list containing the following:
;;;                            :name, :documentation, :default-system-key, :default-menu-
;;;                            column, :instance-type, :instance-finder, and :instance-creator.
;;;                            The software for this is in the DEFSYSTEM software.  See
;;;                            GET- and SET-SYSTEM-ACCESS-LIST and DEFSYSTEM.
;;;     system keys      -- the user-specific information concerning the user interface
;;;                             to the system.  This information is kept in *SYSTEM-KEYS*
;;;                             (see below).  



(DEFUN REMOVE-SYSTEM-ACCESS-SPEC (SYSTEM-NAME
				  &AUX S-A-KEY COLUMN)
  "SYSTEM-NAME is the name of the system you want to remove from the
               system keys and system menu.  Supplied as a keyword.  
   Returns the spec removed"
  (WHEN (SETQ S-A-KEY (find SYSTEM-NAME *SYSTEM-KEYS*
			    :key #'(lambda (elt) (tv:system-key-system elt))))
    (REMOVE-SYSTEM-KEY (TV:SYSTEM-KEY-CHAR S-A-KEY) SYSTEM-NAME)
    (WHEN (SETQ COLUMN (tv:system-key-column S-A-KEY))
      (UNLESS (EQ :NONE COLUMN)
	(DELETE-FROM-SYSTEM-MENU-COLUMN COLUMN
				 (tv:system-key-print-name S-A-KEY)))) 
    )
  S-A-KEY)

(DEFUN MODIFY-SYSTEM-ACCESS-SPEC (SYSTEM-NAME &REST OPTIONS
				  &AUX SYSTEM S-A-KEY)
  "This is the programatic interface to the system access facility.  If you want to add,
    change, or remove a system's access specification, use this function.
     SYSTEM-NAME specifies the system whose access spec is being modified.  What is done to
    it is specified by the options:
      :REMOVE                      -- take the system off the System Menu, off the system
                                          keys.  This option will override all others.
      (:ASSIGN-NAME name)      -- change the system's print-name to name, name is supplied as a
                                string or symbol
      (:ASSIGN-COLUMN column) -- change the System Menu column in which the system
                                          appears to column, one of :USER-AIDS, :PROGRAMS,
                                          :DEBUG-TOOLS, or :NONE.
      (:ASSIGN-KEY key)          -- change the system's System-key to the supplied character, key
      :ASSIGN-DEFAULTS          -- Assign the system's default print-name, the system's 
                                          default system menu column, and its default system 
                                          key, if the key is not already taken.
      :DO-NOT-LOAD               -- Do not load the system's DEFSYSTEM.  If this option
                                          is used with the :ASSIGN-DEFAULTS option, then the
                                          defaults will only be assigned if the system's DEFSYSTEM
                                          has already been loaded.
      If no options are specified, this function simply makes sure that an access spec
      exists for SYSTEM-NAME."
  (WHEN (SETQ SYSTEM
	      (SYS:FIND-SYSTEM-NAMED (SETQ SYSTEM-NAME
					    (INTERN SYSTEM-NAME 'KEYWORD))
				      T T))
    (SETQ SYSTEM-NAME (SYS:SYSTEM-SYMBOLIC-NAME SYSTEM)))
  (UNLESS (SETQ S-A-KEY (REMOVE-SYSTEM-ACCESS-SPEC SYSTEM-NAME))
    (SETQ S-A-KEY (tv:make-system-key :char NIL
				   :window-or-flavor NIL
				   :documentation NIL
				   :create NIL
				   :system SYSTEM-NAME
				   :print-name NIL
				   :column :NONE)))

  (UNLESS (MEMBER :REMOVE OPTIONS) ;; we've already removed it
    (UNLESS (OR SYSTEM
		(MEMBER :DO-NOT-LOAD OPTIONS))  ;; if defsystem hasn't been
      ;; loaded, load it via 
      ;; find-system-named
      (SETQ SYSTEM (SYS:FIND-SYSTEM-NAMED SYSTEM-NAME T)))
    (IF (NOT SYSTEM)
	;; system doesn't exist or :DO-NOT-LOAD
	(unless (MEMBER :DO-NOT-LOAD OPTIONS)
	  (WARN "There is no such system ~a" SYSTEM-NAME))
	;; process :ASSIGN-DEFAULTS
	(IF (MEMBER :ASSIGN-DEFAULTS OPTIONS)
	    (progn
	      (setf (tv:system-key-char s-a-key)
		    (GETF (SYS:SYSTEM-PLIST SYSTEM) :DEFAULT-SYSTEM-KEY))
	      (SETF (tv:system-key-window-or-flavor s-a-key)
		    (GETF (SYS:SYSTEM-PLIST SYSTEM) :INSTANCE-finder))
              (SETF (tv:system-key-documentation s-a-key)
		    (GETF (sys:system-plist system) :DOCUMENTATION))
	      (SETF (tv:system-key-create s-a-key)
		    (GETF (sys:system-plist system) :instance-creator))
	      (SETF (tv:system-key-system s-a-key) system-name)
	      (setf (tv:system-key-print-name s-a-key) 
		    (SYS:SYSTEM-NAME SYSTEM))
	      (setf (tv:system-key-column s-a-key)
		    (OR (GETF (SYS:SYSTEM-PLIST SYSTEM)
			      :DEFAULT-MENU-COLUMN)
			:NONE)))
	    
	    (UNLESS (TV:SYSTEM-KEY-PRINT-NAME S-A-KEY)
	      (SETF (TV:SYSTEM-KEY-PRINT-NAME S-A-KEY) (SYS:SYSTEM-NAME SYSTEM)))))
    
    ;;; process assignment options
    (DOLIST (OPTION OPTIONS)
      (WHEN (CONSP OPTION)
	(let ((second-option (second option)))
	  (CASE (CAR OPTION)
	    (:ASSIGN-NAME   (SETF (TV:SYSTEM-KEY-PRINT-NAME S-A-KEY)
				  SECOND-OPTION))
	    (:ASSIGN-COLUMN (SETF (TV:SYSTEM-KEY-COLUMN  S-A-KEY)
				  SECOND-OPTION))
	    (:ASSIGN-KEY    (SETF (TV:SYSTEM-KEY-CHAR S-A-KEY)
				  SECOND-OPTION))) )))
    
    ;;; add new system-access-item
    (APPLY #'TV:ADD-SYSTEM-KEY
	   S-A-KEY) ))

(DEFUN UPDATE-SYSTEM-ACCESS-SPECS ()
  "Update W:*SYSTEM-KEYS*.  It finds all potentially accessible
    systems in the environment and in SYS:SITE (by looking in the contents
    file) and adds them to W:*SYSTEM-KEYS*."
  (DOLIST (SYSTEM SYS:*SYSTEMS-LIST*)
    (WHEN (TYPEP SYSTEM 'SYS:SYSTEM)
      (WHEN (GETF (SYS:SYSTEM-PLIST SYSTEM) :INSTANCE-TYPE)
	(MODIFY-SYSTEM-ACCESS-SPEC (SYS:SYSTEM-SYMBOLIC-NAME SYSTEM)) )))
  (DOLIST (SYSTEM-NAME (READ-SITE-CONTENTS))
    (MODIFY-SYSTEM-ACCESS-SPEC SYSTEM-NAME :ASSIGN-DEFAULTS)))
  


;;;            SITE-CONTENTS FILE MAINTENANCE

(DEFVAR *SITE-CONTENTS-PATHNAME*
	(PATHNAME "SYS:SITE;CONTENTS.SITE#>")
  "The pathname of the file containing the systems available on SYS:SITE;.")

(DEFUN READ-SITE-CONTENTS ()
  "Read contents file on SYS:SITE;."
  (WITH-OPEN-FILE (CONTENTS-FILE *SITE-CONTENTS-PATHNAME*
				 :DIRECTION :INPUT
				 :IF-DOES-NOT-EXIST NIL)
    (WHEN CONTENTS-FILE
      (READ CONTENTS-FILE))))

(DEFUN ADD-TO-SITE-CONTENTS (SYSTEM-NAME)
  "Add a system to contents file on SYS:SITE;."
  (SETQ SYSTEM-NAME (INTERN SYSTEM-NAME 'KEYWORD))
  (LET ((SITE-CONTENTS (READ-SITE-CONTENTS)))
    (WHEN (NOT (MEMBER SYSTEM-NAME SITE-CONTENTS))
      (PUSH SYSTEM-NAME SITE-CONTENTS)
      (WITH-OPEN-FILE (CONTENTS-FILE *SITE-CONTENTS-PATHNAME*
				     :DIRECTION :OUTPUT
				     :IF-EXISTS :OVERWRITE
				     :IF-DOES-NOT-EXIST :CREATE)
	(PRIN1 SITE-CONTENTS CONTENTS-FILE)))))

(DEFUN REMOVE-FROM-SITE-CONTENTS (SYSTEM-NAME)
  "Remove a system from contents file on SYS:SITE;."
  (SETQ SYSTEM-NAME (INTERN SYSTEM-NAME 'KEYWORD))
  (LET ((SITE-CONTENTS (READ-SITE-CONTENTS)))
    (WHEN (MEMBER SYSTEM-NAME SITE-CONTENTS)
      (SETF SITE-CONTENTS (DELETE SYSTEM-NAME SITE-CONTENTS))     ;; modified to delete - las
      (WITH-OPEN-FILE (CONTENTS-FILE *SITE-CONTENTS-PATHNAME*
				     :DIRECTION :OUTPUT
				     :IF-EXISTS :OVERWRITE
				     :IF-DOES-NOT-EXIST :CREATE)
	(PRIN1 SITE-CONTENTS CONTENTS-FILE)))))

(DEFUN MAKE-NEW-SITE-CONTENTS ()
  "Clear contents file on SYS:SITE;."
  (WITH-OPEN-FILE (CONTENTS-FILE *SITE-CONTENTS-PATHNAME*
				     :DIRECTION :OUTPUT
				     :IF-EXISTS :NEW-VERSION
				     :IF-DOES-NOT-EXIST :CREATE)
	(PRIN1 NIL CONTENTS-FILE)))

(DEFUN MAKE-SITE-CONTENTS ()
  "If no contents file on SYS:SITE; exists, create one"
  (WITH-OPEN-FILE (CONTENTS-FILE *SITE-CONTENTS-PATHNAME*
				     :DIRECTION :OUTPUT
				     :IF-EXISTS NIL
				     :IF-DOES-NOT-EXIST :CREATE)
	(WHEN CONTENTS-FILE
	  (PRIN1 NIL CONTENTS-FILE))))


;;;
(DEFUN FIND-SYSTEM-INSTANCE (SYSTEM-NAME CREATE create-new-screen-p &OPTIONAL (MAKE-SYSTEM-IF-NEEDED :ASK)
			     &AUX CURRENT ALIAS WINDOW)
  "Find an instance of the system named SYSTEM-NAME.  If this system is not made,
    then it will be made first.  An instance of it will then be found, or created if none
    exists yet, and selected as specified by the system's DEFSYSTEM. "
  (LET* ((PLIST   (SYS:GET-SYSTEM-ACCESS-LIST SYSTEM-NAME))
	 (TYPE    (GETF PLIST :INSTANCE-TYPE))
	 (FINDER  (GETF PLIST :INSTANCE-FINDER))
	 (CREATOR (GETF PLIST :INSTANCE-CREATOR))
	 (MADE-P  (GETF PLIST :MADE-P))
	 (have-screen-for-application-p t))
    (WHEN (OR MADE-P
	      (COND
		((EQ :ASK MAKE-SYSTEM-IF-NEEDED)
		 (WHEN (W:MOUSE-CONFIRM (FORMAT NIL "The system ~a has not been made" system-name)
					(FORMAT NIL
						"Click mouse or press ” to make system.~%~
				                  Move off window or press <N> to abort."))
		   (MAKE-SYSTEM SYSTEM-NAME :SILENT :NOWARN)))
		(MAKE-SYSTEM-IF-NEEDED
		 (MAKE-SYSTEM SYSTEM-NAME :SILENT :NOWARN))
		(T NIL)))
      (W:DELAYING-SCREEN-MANAGEMENT
	(CASE TYPE
	  (:EVAL   (IF CREATE               ;; creator is either a 
		       (IF (ATOM CREATOR)   ;;  t - evaluate finder to create instance
                                            ;; nil- cannot be created
                                            ;; a form - to be evaluated 
			   (IF (NULL CREATOR) 
			       (NOTIFY-CANNOT-CREATE SYSTEM-NAME)
			       (EVAL FINDER))
			   (EVAL CREATOR))
		       (EVAL FINDER)))
	  (:WINDOW (IF CREATE
		       (IF (NULL CREATOR)
			   (NOTIFY-CANNOT-CREATE SYSTEM-NAME)
			   (IF (ATOM CREATOR)
			       (IF (EQ CREATOR T)
				   (SEND FINDER :MOUSE-SELECT)
				   (SEND CREATOR :MOUSE-SELECT))
			       (SEND (EVAL CREATOR) :MOUSE-SELECT)))
		       (If (ATOM FINDER)
			   (SEND FINDER :MOUSE-SELECT)
			   (SEND (EVAL FINDER) :MOUSE-SELECT))))
	  (:FLAVOR (SETQ CURRENT W:SELECTED-WINDOW)
		   (WHEN CURRENT
		     (SETQ ALIAS (SEND CURRENT :ALIAS-FOR-SELECTED-WINDOWS)))
		   (COND
		     ;; There are unselected windows of this flavor to cycle through
		     ((AND (NOT CREATE) (SETQ WINDOW (W:FIND-WINDOW-OF-FLAVOR FINDER
							    CURRENT)))
		      (WHEN CURRENT
			(SEND CURRENT :DESELECT (WHEN (TYPEP ALIAS FINDER)
						  :END)))
		      (SEND WINDOW :MOUSE-SELECT))
		     ;; The CURRENT window is the only window of this flavor
		     ((AND (NOT CREATE) CURRENT
			   (TYPEP ALIAS FINDER))
		      (BEEP))
		     ;; Must create a new window but can't (CREATOR is NIL)
		     ((NULL CREATOR)
		      (BEEP))
		     ;; Create a new window on default-screen
		     ((ATOM CREATOR)
		      (WHEN CURRENT
			(SEND CURRENT :DESELECT (IF (TYPEP ALIAS FINDER)
						    :END)))
		      (WHEN (AND (mac-system-p) create-new-screen-p)
			(SETF have-screen-for-application-p
			      (make-a-Mac-resident-Explorer-screen)))
		      (WHEN have-screen-for-application-p
			(SEND (MAKE-INSTANCE (IF (EQ CREATOR T)
						 FINDER
					       CREATOR)
					     :SUPERIOR W:DEFAULT-SCREEN
					     :color-map
					     (copy-color-map (OR (AND default-screen
							       (tv:sheet-color-map default-screen))
							  (AND mouse-sheet
							       (tv:sheet-color-map mouse-sheet))
							  tv:*default-color-map*)))
			      :MOUSE-SELECT)))
		     ;; Evaluate CREATOR
		     (T (EVAL CREATOR)))))
	NIL))))


(DEFUN NOTIFY-CANNOT-CREATE (NAME)
  "Pops up a notification that this system cannot be recreated."
  (W:NOTIFY NIL
	    "The system ~A can only have one instance.~%~
              A new version cannot be created."
	    NAME))







 