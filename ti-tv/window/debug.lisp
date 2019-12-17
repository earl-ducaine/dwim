;;;-*- Mode:Common-Lisp; Package:TV; Base:10; Fonts:(MEDFNT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1985-1989 Texas Instruments Incorporated. All rights reserved.

;;
;; Helpful functions for debugging.
;;

;;; Change history:
;;;
;;;  Date      Author   Description
;;; -------------------------------------------------------------------------------------
;;; 02/22/89  MAY    Changed "Watch" to "Nwatch" and "USER ID" to "User Id".
;;; 04/23/88  KJF     Created.


(DEFUN print-who-line-variables (which-who-screen)
  (LET ((inferiors (SEND which-who-screen :inferiors)))
    (PRINT (FIND "Documentation Window" inferiors :key 'sheet-name :test 'equal))
    (PRINT (FIND "Nwatch" inferiors :key 'sheet-name :test 'equal)) ;; may 02/22/89 
    (PRINT (FIND "User Id" inferiors :key 'sheet-name :test 'equal)) ;; may 02/22/89 
    (PRINT (FIND "Current Package" inferiors :key 'sheet-name :test 'equal))
    (PRINT (FIND "Run State" inferiors :key 'sheet-name :test 'equal))
    (PRINT (FIND "File State" inferiors :key 'sheet-name :test 'equal))))

(DEFUN all-color-windows? (which-who-screen)
  (LET ((inferiors (SEND which-who-screen :inferiors)))
    (PRINT (color-system-p (FIND "Documentation Window" inferiors :key 'sheet-name :test 'equal)))
    (PRINT (color-system-p (FIND "Nwatch" inferiors :key 'sheet-name :test 'equal))) ;; may 02/22/89 
    (PRINT (color-system-p (FIND "User Id" inferiors :key 'sheet-name :test 'equal))) ;; may 02/22/89 
    (PRINT (color-system-p (FIND "Current Package" inferiors :key 'sheet-name :test 'equal)))
    (PRINT (color-system-p (FIND "Run State" inferiors :key 'sheet-name :test 'equal)))
    (PRINT (color-system-p (FIND "File State" inferiors :key 'sheet-name :test 'equal)))))

(DEFUN print-all-info ()
  (FORMAT t "~%tv:main-screen      = ~s" tv:main-screen)
  (FORMAT t "~%tv:default-screen   = ~s" tv:default-screen)
  (FORMAT t "~%tv:mouse-sheet      = ~s" tv:mouse-sheet)
  (FORMAT t "~%tv:who-line-screen  = ~s" tv:who-line-screen)
  (FORMAT t "~%tv:all-the-screens  = ~s" tv:all-the-screens)
  (FORMAT t "~%tv:*color-who-line* = ~s" tv:*color-who-line*)
  (FORMAT t "~%tv:*mono-who-line*  = ~s" tv:*mono-who-line*)
  (FORMAT t "~%w:selected-window   = ~s" w:selected-window)
  (FORMAT t "~%tv:*color-system*   = ~s" tv:*color-system*)
  )

(DEFUN show-all-registers ()
  "Returns value of background color from H/W register."
  (FORMAT t "~%Foreground color register = ~d" (foreground-color-register))
  (FORMAT t "~%Background color register = ~d" (background-color-register))
  (FORMAT t "~%Transparency register = ~d" (transparency-register))
  (FORMAT t "~%Plane mask register = ~d" (plane-mask-register))
  (FORMAT t "~%Monochrome plane is set to = ~d" (SEND *control-register* :monochrome-plane)))

(DEFUN show-sheet-info (sheet operation)
  (FORMAT t "~%~s's ~s = ~s" sheet operation (SEND sheet operation))
  (DOLIST (inf (SEND sheet :inferiors))
    (FORMAT t "~%~s's ~s = ~s" inf operation (SEND inf operation))))

(DEFUN fg ()
  "Returns value of foreground color from H/W register."
  (foreground-color-register))

(DEFUN bg ()
  "Returns value of background color from H/W register."
  (background-color-register))

(DEFUN show-sheet-properties (sheet)
  (FORMAT t "~%~s's property-list = ~%~s" sheet (screen-property-list sheet))
  (DOLIST (inf (SEND sheet :inferiors))
    (FORMAT t "~%~s's property-list = ~%~s" inf (screen-property-list sheet))))

(DEFUN do-operation-on-all-inferiors (sheet operation)
  (ASSERT sheet (sheet) "SHEET should not be NIL.")
  (PRINT sheet)
  (PRINT (SEND sheet operation))
  (DOLIST (inf (SEND sheet :inferiors))
    (do-operation-on-all-inferiors inf operation)))

(DEFUN dump-previously-selected-screens-array ()
  "Print all screens from *PREVIOUSLY-SELECTED-SCREENS*."
  (DO ((I 0 (1+ I))
       (N (ARRAY-TOTAL-SIZE *PREVIOUSLY-SELECTED-SCREENS*)))
      ((= I N))
    (LET ((screen (AREF *PREVIOUSLY-SELECTED-SCREENS* i)))
      (WHEN screen (FORMAT t "~%~s" screen)))))
