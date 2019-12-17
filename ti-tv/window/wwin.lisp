;;; -*- Mode: common-LISP;  Package: W; Base:10. ; Fonts: (CPTFONT CPTFONTB HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1986-1989 Texas Instruments Incorporated. All rights reserved.


;;; Change History
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;   4/08/87  TWE	Backed out the changes for the W typeout-window flavors.
;;;			This is going to take a lot more work to get it right.
;;;   4/01/87  TWE	Added essential-window-with-typeout-mixin and
;;;			window-with-typeout-mixin to define a W:TYPEOUT-WINDOW
;;;			instance variable.
;;; 09/26/86 JEB	Initial version to add two window flavors to the package W.


(DEFFLAVOR WINDOW-WITHOUT-LABEL () (GRAPHICS-MIXIN STREAM-MIXIN BORDERS-MIXIN SELECT-MIXIN
				    DELAY-NOTIFICATION-MIXIN MINIMUM-WINDOW))

(DEFFLAVOR WINDOW () (GRAPHICS-MIXIN STREAM-MIXIN BORDERS-MIXIN LABEL-MIXIN SELECT-MIXIN
		      DELAY-NOTIFICATION-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "This is the simplest practical window
It probably isn't what you want, except for testing purposes; although it is useful for
mixing with one or two simple mixins to get something useful."))
