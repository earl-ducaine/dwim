;; -*- Mode:Common-Lisp; Package:W; Base:10; Fonts:(medfnt hl12b hl12bi) -*-

;;;			    RESTRICTED RIGHTS LEGEND

;;; Use, duplication, or disclosure by the Government is subject to restrictions
;;; as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and
;;; Computer Software clause at 52.227-7013.

;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769

;;; Copyright (C) 1987- 1989 Texas Instruments Incorporated.  All rights reserved.


;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;; 1/29/87  DLS		Fixed the NEW-WINDOW method to not always set the window to the default.
;;; 12/04/86 TWE		Fixed the initialization of the transform matrix to not use copy-array-constants.
;;; 10/22/86 TWE		Changed (DECLARE (RETURN-LIST ...)) to (DECLARE (VALUES ...) for Common Lisp.
;;; 10/22/86 TWE		Changed the flavor transform-mixin so that it doesn't use fillarray.
;;; 09/11/86 JEB		Moved functions from GWIN: to W:
;;;

;;;
;;;                TRANSFORM
;;
;; OVERVIEW:
;;
;; The TRANSFORM-MIXIN provides the mapping between a graphics world and a drawing
;; device (in this case, a Lisp Machine window).  A window's transform determines the rectangular
;; area of the world it is displaying and the level of magnification.  The mixin provides some useful
;; methods for modifying a window's transform.
;;
;;	flavor:	TRANSFORM-MIXIN
;;
;;	methods:	default-window
;;		new-window
;;		pan
;;		transform-deltas
;;		transform-point
;;		untransform-point
;;		world-edges
;;		zoom
;;



(defvar *default-world-coordinates* NIL
  "What style of world coordinates should transformable windows begin with.
   Appropriate values are NIL, which means use outside edges for the orgin.
   The values :START-FROM-INSIDE or :RELEASE2 will start the world coordinates
   from the inside (borders+margins).")

(defflavor transform-mixin
           ((identity? t)
	    (transform  (make-array '(3 3) :initial-contents identity-structured-list)))
	   ()
   (:required-flavors minimum-window)
   :gettable-instance-variables
   (:initable-instance-variables transform)
   (:settable-instance-variables transform identity?)
   (:init-keywords :world-coordinates)		;allow setting of world coordinates origin  PMH 3/22
   (:default-init-plist :world-coordinates *default-world-coordinates*)
   (:documentation :mixin
    "Provides the graphics-window transform (mapping between world and device
coordinates) and basic methods for modifying a window's transform."))

;;; The purpose of this initialization is to allow folks to set their world coordinates
;;; to START from the inside (borders+margins), as in release 2.  Of course if they do
;;; a return to :WORLD-EDGES the effect of this initialization is lost, but then this
;;; capability did not exist in release2 tv:GRAPHICS-MIXIN. 
;;; PMH 3/22
(defmethod (transform-mixin :after :init)(plist)
  (let ((world-coordinates (getf (contents plist) :world-coordinates)))
    (when (and (typep self 'tv:sheet)
	       (or (eq world-coordinates :start-from-inside) (eq world-coordinates :release2)))
      (send self :pan (w:sheet-inside-left self) (w:sheet-inside-top self)))))


;;
;; Methods which adjust the viewing window into the world by modifying the transform.
;;


(DEFMETHOD (transform-mixin :default-window) (&rest ignore)
  "This method changes the transform for the window to the identity transform.
This is the default window condition upon initialization."
  (SETQ identity? t)
  (COPY-ARRAY-CONTENTS identity-array transform))


(DEFMETHOD (transform-mixin :get-scaling-factors) ()
  "This method returns the scaling factors of the transform."
  (DECLARE (VALUES x-scale y-scale))
  (VALUES (AREF transform 0 0)
	  (AREF transform 1 1)))


(DEFMETHOD (transform-mixin :new-window) (x y dx dy)
  "This changes the tranform to fit a specified rectangular area  in the window.
The x and y scales are made equal to avoid weird stretching, so the rectangle is
expanded to a square. Nil is returned if the transform was not changed."
  (DECLARE (VALUES transform-changed?))
  (SETQ dx (COND ((= dx 0) 0.01)
		 (t	   (FLOAT (ABS dx)))))
  (SETQ dy (COND ((= dy 0) 0.01)
		 (t	   (FLOAT (ABS dy)))))
  (LET ((left		    (MIN x (+ x dx)))
	(top		    (MIN y (+ y dy)))
	(s		    (MIN (/ (sheet-inside-width)  dx)
				 (/ (sheet-inside-height) dy)))
	(transform-changed? nil))
    (UNLESS (= (AREF transform 0 0) (SETF (AREF transform 0 0) s))
      (SETQ transform-changed? t))
    (UNLESS (= (AREF transform 0 1) (SETF (AREF transform 0 1) 0))
      (SETQ transform-changed? t))
    (UNLESS (= (AREF transform 0 2) (SETF (AREF transform 0 2) 0))
      (SETQ transform-changed? t))
    (UNLESS (= (AREF transform 1 0) (SETF (AREF transform 1 0) 0))
      (SETQ transform-changed? t))
    (UNLESS (= (AREF transform 1 1) (SETF (AREF transform 1 1) s))
      (SETQ transform-changed? t))
    (UNLESS (= (AREF transform 1 2) (SETF (AREF transform 1 2) 0))
      (SETQ transform-changed? t))
    (UNLESS (= (AREF transform 2 0) (SETF (AREF transform 2 0) (- (* left s))))
      (SETQ transform-changed? t))
    (UNLESS (= (AREF transform 2 1) (SETF (AREF transform 2 1) (- (* top s))))
      (SETQ transform-changed? t))
    (UNLESS (= (AREF transform 2 2) (SETF (AREF transform 2 2) 1))
      (SETQ transform-changed? t))
    (SETQ identity? (EQUAL transform identity-array))
    transform-changed?))


(DEFMETHOD (transform-mixin :pan) (dx dy)
  "This changes the transform by translating both horizontally and vertically."
  (INCF (AREF transform 2 0) dx)
  (INCF (AREF transform 2 1) dy)
  (SETQ identity? (EQUAL transform identity-array)))


(DEFMETHOD (transform-mixin :set-scales) (x-scale &optional (y-scale x-scale))
  "This sets the scale values in the transformation for the cache window.
It also makes sure that any global data that is derived from the transform is
updated."
  (SETF (AREF transform 0 0) x-scale)
  (SETF (AREF transform 1 1) y-scale)
  (SETQ identity? (EQUAL transform identity-array)))


(DEFMETHOD (transform-mixin :after :set-transform) (IGNORE)
  "This updates the identity transform flag after a new transform has been set."
  (SETQ identity? (EQUAL transform identity-array)))


(DEFMETHOD (transform-mixin :zoom) (sx sy)
  "This changes the transform by scaling both horizontally and vertically.
The center of the window is kept the same world location."
  (UNLESS (AND (= sx 1) (= sy 1))
    (scalef (AREF transform 0 0) sx)
    (scalef (AREF transform 1 1) sy)
    (scalef (AREF transform 2 0) sx)
    (INCF (AREF transform 2 0)
	  (* 0.5 (- 1 sx) (- (sheet-inside-right) (sheet-inside-left))))
    (scalef (AREF transform 2 1) sy)
    (INCF (AREF transform 2 1)
	  (* 0.5 (- 1 sy) (- (sheet-inside-bottom) (sheet-inside-top))))
    (SETQ identity? (EQUAL transform identity-array))))


;;
;; Methods which translates between world and device (screen) coordinates.
;;


(DEFMETHOD (transform-mixin :transform-deltas) (dx dy)
  "This method translates a pair of distances from world to window coordinates.
The distances are not relative to any fixed point in space so the translation
part of the transformation is not applied to them."
  (DECLARE (VALUES transformed-dx transformed-dy))
  (VALUES (g-round (+ (* dx (AREF transform 0 0))
		      (* dy (AREF transform 1 0))))
	  (g-round (+ (* dx (AREF transform 0 1))
		      (* dy (AREF transform 1 1))))))


(DEFMETHOD (transform-mixin :untransform-deltas) (dx dy)
  "This method tranlates a pair of distances from window to world coordinates.
    The distances are not relative to any fixed point in space so the translation
    part of the transformation is not applied to them."
  (COND (identity? (VALUES dx dy))
	(t	   (untransform-deltas dx dy transform))))


(DEFMETHOD (transform-mixin :transform-point) (x y)
  "This method translates a point from world coordinates to window coordinates."
  (DECLARE (VALUES transformed-x transformed-y))
  (COND (identity? (VALUES (g-round x) (g-round y)))
	(t	   (transform-point x y transform))))


(DEFMETHOD (transform-mixin :untransform-point) (x y)
  "This method translates a point from window coordinates to world coordinates."
  (DECLARE (VALUES untransformed-x untransformed-y))
  (COND (identity? (VALUES (g-round x) (g-round y)))
	(t	   (untransform-point x y transform))))


(DEFMETHOD (transform-mixin :world-edges) ()
  "This method returns the inside edges of the window in world coordinates."
  (DECLARE (VALUES inside-left inside-top inside-right inside-bottom))
  (MULTIPLE-VALUE-BIND (left top right bottom)
    (SEND self :inside-edges)
    (MULTIPLE-VALUE-SETQ (left top)
      (SEND self :untransform-point left top))
    (MULTIPLE-VALUE-SETQ (right bottom)
      (SEND self :untransform-point right bottom))
    (VALUES left top right bottom)))
