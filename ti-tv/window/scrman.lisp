;;; -*- Mode: Common-lisp;  Package: TV; Base:10.; Fonts: (CPTFONT HL12B HL12BI) -*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1983- 1989 Texas Instruments Incorporated. All rights reserved.
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;;
;;; Change history:
;;;
;;;  Date      Author	Description
;;; -------------------------------------------------------------------------------------
;;;  6/3/87     KWW     Changed 'bit to (sheet-array-type default-screen)
;;;  1/14/87   TWE,KDB	Fixed SCREEN-MANAGE-GRAY-RECTANGLE by twiddling values given ARRAY-DIMENSION.
;;; 11/03/86   TWE	Moved the gray constant stuff from here to TVDEFS.
;;; 07/31/86   TWE	Changed use of :type 'art-1b to :element-type 'bit.
;;; 07/29/86   TWE	Changed to use Common Lisp functions.
;;; 07/28/86   TWE	Modified references to the pixel functions to use ARRAY-DIMENSION
;;;			and MAKE-ARRAY instead.

;;; The screen manager, an unseen entity that many have cursed and
;;; many have praised, is herewithin presented for all to see.

;;; Priorities:
;;;Each sheet has a priority (which is basically how hard it tries to be on
;;;the screen).  The priorities are used for ordering the list of inferiors
;;;of a sheet, and therefore affect what windows the mouse sees, how the
;;;screen manager works, and how the automatic exposing decides what to
;;;expose.  If the priority is null, then it is considered to have a
;;;priority smaller than that of any with explicit priorities (this
;;;includes negative priorities, but that doesn't really matter).  Positive
;;;priorities mean an absolute priority as compared to all other sheets
;;;with a numerical priority -- namely, the larger the number, the more
;;;important the sheet is considered to be.  A negative priority indicates
;;;that the sheet is to be considered "inactive" by various routines when
;;;it is not exposed.  -1 means only be considered inactive by the screen
;;;manager, and -2 or less means don't even be a candidate for
;;;auto-exposure.  Exposed sheets are always uncovered, and therefore are
;;;guaranteed to have the largest priority (virtually, and since they don't
;;;overlap, all exposed sheets are considered the same).
;;;
;;;Notes: A set of rectangles is a list of rectangles.  All functions that
;;;operate on sets expect them in the canonical form (a list of rectangles
;;;that don't mutually overlap).  All user-level functions that return sets
;;;canonicalize them first.
;;;
;;;A rectangle is a four-list of (SOURCE LEFT TOP RIGHT BOTTOM).  SOURCE is
;;;where the rectangle came from.  Rectangles cannot have their LEFT, TOP,
;;;RIGHT, or BOTTOM destructively altered.
;;;  ;; end Comment


(DEFUN CANONICALIZE-RECTANGLE-SET (S)
  "Given a set of rectangles, returns a set in canonical form (that
have no overlaps)."
  (DO ((NEW NIL NIL))
      (NIL)
;;; It's not clear whether the sorting helps at all
;    (SETQ S
;	  (SORT S #'(LAMBDA (X Y)
;		      (NOT (< (* (- (RECT-RIGHT X) (RECT-LEFT X))
;				 (- (RECT-BOTTOM X) (RECT-TOP X)))
;			      (* (- (RECT-RIGHT Y) (RECT-LEFT Y))
;				 (- (RECT-BOTTOM Y) (RECT-TOP Y))))))))
    (DO ((R (CAR S) (CAR L))
	 (L (CDR S) (CDR L))
	 (S-TEM))
	((NULL L))
      (DOLIST (RA L)
	(COND
	  ((RECT-NOT-OVERLAP-RECT-P R RA)
	   ;; No overlap, ok
	   )
	  ((RECT-WITHIN-RECT-P R RA)
	   ;; R completely within RA, throw R away
	   (SETQ S (DELETE R (THE LIST S) :TEST #'EQ))
	   (RETURN ()))
	  ((RECT-WITHIN-RECT-P RA R)
	   ;; RA completely within R, throw RA away
	   (SETQ S (DELETE RA (THE LIST S) :TEST #'EQ))
	   (RETURN ()))
	  (T (SETQ S-TEM
		   ;; Get all sections of RA outside of R.
		   (RECTANGLE-NOT-INTERSECTION R RA))
	     (OR S-TEM
		 ;; No result can't happen if above checks don't succeed
		 (FERROR () "Null not-intersection impossible: ~S ~S" R RA))
	   (DOLIST (RB S-TEM)
	     (AND (RECT-WITHIN-RECT-P RB R)
		  (SETQ S-TEM (DELETE RB (THE LIST S-TEM) :TEST #'EQ))))
	   (SETQ NEW (NCONC S-TEM NEW)
		 S (DELETE RA (THE LIST S) :TEST #'EQ))))))
    ;; When no new rectangles generated, return the old list
    (OR NEW
	(RETURN S))
    (SETQ S (NCONC NEW S)))) 

(DEFUN %RECTANGLE-NOT-INTERSECTION (RPRIME RAUX &AUX SET)
  "Return a set of rectangles which consists of all the area in RAUX
that is not also in RPRIME.  The set is guaranteed to be canonical. "
  (COND ((RECT-NOT-OVERLAP-RECT-P RPRIME RAUX)
	 ;; No intersection at all, just return RAUX
	 RAUX)
	((RECT-WITHIN-RECT-P RAUX RPRIME)
	 ;; No area that isn't in RPRIME
	 NIL)
	(T
         ;; This is the worst case.  The rectangles look something like:
         ;;
         ;;	.---------------------------------------.
         ;;	|                                       |
         ;;	|                 RAUX                  |
         ;;	|                                       |
         ;;	|                                       |
         ;;	|                                       |
         ;;	|             .-----------.             |
         ;;	|             |           |             |
         ;;	|             |  RPRIME   |             |
         ;;	|             |           |             |
         ;;	|             `-----------'             |
         ;;	|                                       |
         ;;	|                                       |
         ;;	|                                       |
         ;;	|                                       |
         ;;	|                                       |
         ;;	`---------------------------------------'
         ;;
         ;; We want to define new rectangles so that there is no overlap
         ;; at all.  For the worst case, we will define rectangles A, B,
         ;; C and D that cover the same area as RAUX but do not overlap
         ;; RPRIME.  That is, they look like:
         ;;
         ;;	.---------------------------------------.
         ;;	|                                       |
         ;;	|                                       |
         ;;	|                  A                    |
         ;;	|                                       |
         ;;	|                                       |
         ;;	|=============.-----------.=============|
         ;;	|             |           |             |
         ;;	|      C      |           |      D      |
         ;;	|             |           |             |
         ;;	|============='___________`=============|
         ;;	|                                       |
         ;;	|                                       |
         ;;	|                  B                    |
         ;;	|                                       |
         ;;	|                                       |
         ;;	`---------------------------------------'
         ;;
	 (AND (< (RECT-TOP RAUX) (RECT-TOP RPRIME))           ; Define rectangle A
	      (PUSH (LIST (RECT-SOURCE RAUX)
			  (RECT-LEFT   RAUX) (RECT-TOP RAUX)
			  (RECT-RIGHT  RAUX) (RECT-TOP RPRIME))
		    SET))
	 (AND (> (RECT-BOTTOM RAUX) (RECT-BOTTOM RPRIME))     ; Define rectangle B
	      (PUSH (LIST (RECT-SOURCE RAUX)
			  (RECT-LEFT   RAUX) (RECT-BOTTOM RPRIME)
			  (RECT-RIGHT  RAUX) (RECT-BOTTOM RAUX))
		    SET))
	 (AND (< (RECT-LEFT RAUX) (RECT-LEFT RPRIME))         ; Define rectangle C
	      (PUSH (LIST (RECT-SOURCE RAUX)
			  (RECT-LEFT   RAUX)   (MAX (RECT-TOP    RPRIME) (RECT-TOP    RAUX))
			  (RECT-LEFT   RPRIME) (MIN (RECT-BOTTOM RPRIME) (RECT-BOTTOM RAUX)))
		    SET))
	 (AND (> (RECT-RIGHT RAUX) (RECT-RIGHT RPRIME))       ; Define rectangle D
	      (PUSH (LIST (RECT-SOURCE RAUX)
			  (RECT-RIGHT  RPRIME) (MAX (RECT-TOP    RPRIME) (RECT-TOP    RAUX))
			  (RECT-RIGHT  RAUX)   (MIN (RECT-BOTTOM RPRIME) (RECT-BOTTOM RAUX)))
		    SET))
	 SET))) 

;;; The real screen manager:
(DEFRESOURCE SCREEN-MANAGER-BIT-ARRAY-RESOURCE ()
  :CONSTRUCTOR (MAKE-ARRAY `(,(SHEET-HEIGHT DEFAULT-SCREEN)
			     ,(SHEET-WIDTH DEFAULT-SCREEN))
			   :ELEMENT-TYPE (sheet-array-type-cl default-screen))
  :INITIAL-COPIES 0)

(DEFUN SCREEN-MANAGE-SHEET (SHEET &OPTIONAL BOUND-RECTANGLES ARRAY-TO-DRAW-ON (X 0) (Y 0) ALU &AUX RECTANGLE-LIST
  NOT-WHOLE)
  "Perfornm screen management on a sheet.
Screen management causes the exposed or partially-visible inferiors of
SHEET to have their contents appearing in the appropriate places
within SHEET.  Should be called with SHEET locked, and inferiors
ordered, and inside a method handling a message to that sheet.  The
rectangles passed in here better be destructable."
  (LET ((LEFT (SHEET-INSIDE-LEFT SHEET))
	(TOP (SHEET-INSIDE-TOP SHEET))
	(RIGHT (SHEET-INSIDE-RIGHT SHEET))
	(BOTTOM (SHEET-INSIDE-BOTTOM SHEET)))
    (DOLIST (R BOUND-RECTANGLES)
      (SETF (RECT-LEFT R) (MAX LEFT (RECT-LEFT R)))
      (SETF (RECT-TOP R) (MAX TOP (RECT-TOP R)))
      (SETF (RECT-RIGHT R) (MIN RIGHT (RECT-RIGHT R)))
      (SETF (RECT-BOTTOM R) (MIN BOTTOM (RECT-BOTTOM R)))
      ;; Is this now an illegal rectangle?  If so, then punt it altogether
      (OR (AND (< (RECT-LEFT R) (RECT-RIGHT R))
	       (< (RECT-TOP R) (RECT-BOTTOM R)))
	 (SETQ BOUND-RECTANGLES (DELETE R (THE LIST BOUND-RECTANGLES) :COUNT 1 :TEST #'EQ)))))
  (COND
    (BOUND-RECTANGLES
     (SETQ NOT-WHOLE T
	   BOUND-RECTANGLES (CANONICALIZE-RECTANGLE-SET BOUND-RECTANGLES)))
    (T
     (SETQ BOUND-RECTANGLES
	   (LIST
	    (LIST (LIST SHEET 0 0)	; SHEET X-OFFSET Y-OFFSET
		  (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET) (SHEET-INSIDE-RIGHT SHEET)
		  (SHEET-INSIDE-BOTTOM SHEET))))))
  ;; Figure out what should be visible.
  ;; This loop is executed with S each of the inferiors then with S the sheet itself.
  (DO ((INFS (SHEET-INFERIORS SHEET) (CDR INFS))
       (S)
       (R-TEM))
      ((NULL BOUND-RECTANGLES))
    (AND (NULL INFS)
	 (RETURN (SETQ RECTANGLE-LIST (NCONC BOUND-RECTANGLES RECTANGLE-LIST))))
    (SETQ S (CAR INFS))
    ;; determine visible rectangles of that window
    (LET ((X-OFF (SHEET-X-OFFSET S))
	  (Y-OFF (SHEET-Y-OFFSET S))
	  (SUPERIOR SHEET)
	  (SHEET S)
	  (LEFT -1)
	  (TOP -1)
	  (RIGHT (1- #o40000000))		; 2**23 - 1
	  (BOTTOM (1- #o40000000)))
      (COND
	((OR (MEMBER SHEET (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ)
	     (SEND SHEET :SCREEN-MANAGE-DEEXPOSED-VISIBILITY))
	 (COND
	   (SUPERIOR
	    (SETQ LEFT (SHEET-INSIDE-LEFT SUPERIOR)
		  TOP (SHEET-INSIDE-TOP SUPERIOR)
		  RIGHT (SHEET-INSIDE-RIGHT SUPERIOR)
		  BOTTOM (SHEET-INSIDE-BOTTOM SUPERIOR))))
	 ;; Intersect the rectangles with the bounds of the specified
	 ;; sheet, and push  a list of the resulting rectangles.
	 ;; Include in the source description the bit array so we force
	 ;; an update if that changes.
	 (DO ((RA-AUX BOUND-RECTANGLES (CDR RA-AUX))
	      (BOUND)
	      (TRAIL)
	      (R))
	     ((NULL RA-AUX)
	      NIL)
	   (SETQ BOUND (CAR RA-AUX))
	   (IF (EQ BOUND
		   (SETQ R-TEM
			 (%RECTANGLE-NOT-INTERSECTION
			   (SETF R
				 (LIST (LIST SHEET X-OFF Y-OFF (SHEET-BIT-ARRAY SHEET))
				       (MAX X-OFF (RECT-LEFT BOUND) LEFT)
				       (MAX Y-OFF (RECT-TOP BOUND) TOP)
				       (MIN (+ X-OFF (SHEET-WIDTH SHEET)) (RECT-RIGHT BOUND) RIGHT)
				       (MIN (+ Y-OFF (SHEET-HEIGHT SHEET)) (RECT-BOTTOM BOUND)
					    BOTTOM)))
			   BOUND)))
	       (IF TRAIL
		   (SETF TRAIL (CDR TRAIL))
		   (SETF TRAIL BOUND-RECTANGLES))
	       ;;delete the rectangle
	       (PROGN
		 (IF TRAIL
		     (SETF (CDR TRAIL) (CDR RA-AUX))
		     (SETQ BOUND-RECTANGLES (CDR BOUND-RECTANGLES)))
		 (WHEN R-TEM
		   ;;We have new ones to insert
		   (UNLESS TRAIL
		     (SETQ TRAIL (LAST R-TEM)))
		   (SETQ BOUND-RECTANGLES (NCONC R-TEM BOUND-RECTANGLES)))))
	   (IF (SHEET-OVERLAPS-P SHEET (RECT-LEFT BOUND) (RECT-TOP BOUND)
				 (- (RECT-RIGHT BOUND) (RECT-LEFT BOUND))
				 (- (RECT-BOTTOM BOUND) (RECT-TOP BOUND)))
	       (OR (AND (SHEET-EXPOSED-P SHEET)
			(SHEET-SCREEN-ARRAY SUPERIOR))
		   ;; Never need to restore exposed sheets if superior
		   ;; has a screen image
		   (PUSH R RECTANGLE-LIST))))))))
  (SCREEN-MANAGE-FLUSH-KNOWLEDGE SHEET)
  ;; Now do the updates
  (AND RECTANGLE-LIST
       (IF ARRAY-TO-DRAW-ON
	   (SCREEN-MANAGE-SHEET-FINAL SHEET RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU)
	   (SHEET-FORCE-ACCESS (SHEET)
	     (SCREEN-MANAGE-SHEET-FINAL SHEET RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU))))) 

(DEFUN SCREEN-MANAGE-SHEET-FINAL (SHEET RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU)
  "Update screen area of some rectangles within SCREEN in ARRAY-TO-DRAW-ON.
If ARRAY-TO-DRAW-ON is NIL, SHEET's screen-array is used.  Drawing
in that array is done using ALU at offsets X and Y.  Each rectangle is
processed as appropriate to the sheet which is its source."
  (OR ARRAY-TO-DRAW-ON (SETQ ARRAY-TO-DRAW-ON (SHEET-SCREEN-ARRAY SHEET)))
  (DO ((MASTER (CAR RECTANGLE-LIST) (CAR RECTANGLE-LIST))
       (CURRENT-SHEET))
      ((NULL MASTER))
    ;; For all deexposed windows that are of interest, tell them to put
    ;; their bits up (all the sheets are locked by us, so no problem
    ;; with change of state).  If it's an inferior, put up its bits, but
    ;; if it's the sheet being managed, it means there is nothing there.
    ;; This is rather misleading; it means there are no inferiors there,
    ;; but not that it is really necessarily blank!  See comments on
    ;; SCREEN-MANAGE-MAYBE-BLT-RECTANGLE.
    (SETQ CURRENT-SHEET (CAR (RECT-SOURCE MASTER)))
    (COND ((EQ SHEET CURRENT-SHEET)
	   (SETQ RECTANGLE-LIST
		 (SEND SHEET :SCREEN-MANAGE-UNCOVERED-AREA
			       RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU)))
	  (T
	   (SETQ RECTANGLE-LIST
		 (SEND CURRENT-SHEET :SCREEN-MANAGE-RESTORE-AREA
			  RECTANGLE-LIST ARRAY-TO-DRAW-ON X Y ALU))))))

(DEFUN SCREEN-MANAGE-FLUSH-KNOWLEDGE (SHEET)
  (SETF (SHEET-SCREEN-MANAGER-SCREEN-IMAGE SHEET) NIL))

(DEFUN SCREEN-MANAGE-SHEET-RECTANGLES
       (SHEET BOUND-RECTANGLES
        &AUX
        (X-OFF    (SHEET-X-OFFSET SHEET))
        (Y-OFF    (SHEET-Y-OFFSET SHEET))
        (SUPERIOR (SHEET-SUPERIOR SHEET))
        (LEFT -1)
        (TOP  -1)
        (RIGHT  (1- #o40000000))                      ; 2**23 - 1
        (BOTTOM (1- #o40000000))
        RECTS)
  "Return a list of rectangles describing where SHEET intersects any
of BOUND-RECTANGLES.  The rectangles returned have (SHEET
its-x-offset its-y-offset its-bit-array) as source.  All rectangle edges
are relative to SHEET's superior, or to -1, -1 for a screen."
  (COND
    ((OR (MEMBER SHEET (SHEET-EXPOSED-INFERIORS SUPERIOR) :TEST #'EQ)
	 (SEND SHEET :SCREEN-MANAGE-DEEXPOSED-VISIBILITY))
     (COND
       (SUPERIOR
	(SETQ LEFT (SHEET-INSIDE-LEFT SUPERIOR)
	      TOP (SHEET-INSIDE-TOP SUPERIOR)
	      RIGHT (SHEET-INSIDE-RIGHT SUPERIOR)
	      BOTTOM (SHEET-INSIDE-BOTTOM SUPERIOR))))
     ;; Intersect the rectangles with the bounds of the specified
     ;; sheet, and return a list of the resulting rectangles.
     ;; Include in the source description the bit array so we force
     ;; an update if that changes.
     (DOLIST (BOUND BOUND-RECTANGLES)
       (AND
	 (SHEET-OVERLAPS-P SHEET (RECT-LEFT BOUND) (RECT-TOP BOUND)
			   (- (RECT-RIGHT BOUND) (RECT-LEFT BOUND))
			   (- (RECT-BOTTOM BOUND) (RECT-TOP BOUND)))
	 (PUSH
	   (LIST (LIST SHEET X-OFF Y-OFF (SHEET-BIT-ARRAY SHEET)) (MAX X-OFF (RECT-LEFT BOUND) LEFT)
		 (MAX Y-OFF (RECT-TOP BOUND) TOP)
		 (MIN (+ X-OFF (SHEET-WIDTH SHEET)) (RECT-RIGHT BOUND) RIGHT)
		 (MIN (+ Y-OFF (SHEET-HEIGHT SHEET)) (RECT-BOTTOM BOUND) BOTTOM))
	   RECTS)))
     RECTS))) 

;;; Subroutines used by bit restorers and blank area managers

;;ab 10/10/88.  Added a gross hack to keep this code from executing on a printer screen.
;;              For some reason doing this "clear" to a printer screen causes a garbaged
;;              1st page on some printers, but only during the initial :EXPOSE of the
;;              newly-created screen (ie, when we execute this by way of TV:DEFINE-SCREEN).
(DEFUN SCREEN-MANAGE-CLEAR-RECTANGLE (R ARRAY X Y ALU)
  (DECLARE (:SELF-FLAVOR SHEET))
  (UNLESS (printer-screen-p self)
    (BITBLT (OR ALU ERASE-ALUF)
	    (- (RECT-RIGHT R) (RECT-LEFT R)) (- (RECT-BOTTOM R) (RECT-TOP R))
	    ARRAY (+ X (RECT-LEFT R)) (+ Y (RECT-TOP R))
	    ARRAY (+ X (RECT-LEFT R)) (+ Y (RECT-TOP R)))))

(DEFUN SCREEN-MANAGE-MAYBE-BLT-RECTANGLE (R ARRAY X Y ALU)
  "Restore or clear the rectangle R in ARRAY with offset X and Y.
If SELF has a bit-array, restore from that; otherwise, clear it.  This is
a reasonable screen management protocol for blank areas for sheets
which might have bit save arrays and get screen managed, such as
LISP-LISTENERS with inferiors."
  (DECLARE (:SELF-FLAVOR SHEET))
  (COND (BIT-ARRAY
	 (PAGE-IN-PIXEL-ARRAY BIT-ARRAY NIL (LIST WIDTH HEIGHT))
	 (BITBLT (OR ALU ALU-SETA)
		 (- (RECT-RIGHT R) (RECT-LEFT R)) (- (RECT-BOTTOM R) (RECT-TOP R))
		 ;; The rectangle is defined to be zero based
		 BIT-ARRAY  (RECT-LEFT R) (RECT-TOP R)
		 ARRAY (+ X (RECT-LEFT R)) (+ Y (RECT-TOP R))))
	(T
	 (SCREEN-MANAGE-CLEAR-RECTANGLE R ARRAY X Y ALU))))

(DEFUN PAGE-IN-PIXEL-ARRAY (ARRAY &OPTIONAL FROM TO)
  "Page in array of pixels ARRAY, or the part from FROM to TO.
FROM or TO can be NIL or a list (WIDTH HEIGHT).  NIL means that
the arrays are in row-major order."
  (SETQ FROM (REVERSE FROM) TO (REVERSE TO))
  (SI:PAGE-IN-ARRAY ARRAY FROM TO))

(DEFUN PAGE-OUT-PIXEL-ARRAY (ARRAY &OPTIONAL FROM TO)
  "Page out array of pixels ARRAY, or the part from FROM to TO.
FROM or TO can be NIL or a list (WIDTH HEIGHT).  NIL means that
the arrays are in row-major order"
  (SETQ FROM (REVERSE FROM) TO (REVERSE TO))
  (SI:PAGE-OUT-ARRAY ARRAY FROM TO))

(DEFUN SCREEN-MANAGE-CLEAR-AREA (RECTS ARRAY X Y ALU)
  (DECLARE (:SELF-FLAVOR SHEET))
  (DOLIST (R RECTS)
    (COND
      ((EQ SELF (CAR (RECT-SOURCE R)))
       (SCREEN-MANAGE-CLEAR-RECTANGLE R ARRAY X Y ALU)
       (SETQ RECTS (DELETE R (THE LIST RECTS) :TEST #'EQ)))))
  RECTS)

(DEFUN SCREEN-MANAGE-RESTORE-AREA
       (RECTANGLE-LIST TO-ARRAY X Y ALU &OPTIONAL CLEAR-AREA)
  "Restore contents of rectangles in RECTANGLE-LIST to TO-ARRAY
with offsets X and Y.
ALU		alu-function used in BITBLTing.
CLEAR-AREA	non-NIL says just use zeros if no data is remembered;
		otherwise, regenerate the contents of SELF and use
		them.

Return RECTANGLE-LIST sans the rectangles that applied to this
window."
  (DECLARE (:SELF-FLAVOR SHEET))
  (COND (BIT-ARRAY
	 (SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY RECTANGLE-LIST
						    BIT-ARRAY
						    TO-ARRAY X Y
						    T (OR ALU ALU-SETA)))
	(CLEAR-AREA
	 (SCREEN-MANAGE-CLEAR-AREA RECTANGLE-LIST TO-ARRAY X Y ALU))
	(T
	 ;; If no saved bits, Refresh into a temporary array and use
         ;; that as the bits.
	 (UNWIND-PROTECT
	     (USING-RESOURCE (ARRAY SCREEN-MANAGER-BIT-ARRAY-RESOURCE)
	       (SETQ SCREEN-ARRAY ARRAY)
	       (PAGE-IN-PIXEL-ARRAY ARRAY NIL (LIST WIDTH HEIGHT))
	       (SHEET-FORCE-ACCESS (SELF T)
		 (SEND SELF :REFRESH))
	       (SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY RECTANGLE-LIST
							  ARRAY
							  TO-ARRAY X Y
							  NIL (OR ALU ALU-SETA)))
	   (SETQ SCREEN-ARRAY NIL)))))

(DEFUN SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY
       (RECTANGLE-LIST ARRAY TO-ARRAY X Y PAGE-FLAG ALU
	&AUX (FROM (LIST 0 0)) (TO (LIST 0 0)))
  (DOLIST (R RECTANGLE-LIST)
    (COND
      ((EQ (CAR (RECT-SOURCE R)) SELF)
       (SETF (CAR FROM) (- (RECT-LEFT R) (CADR (RECT-SOURCE R))))
       (SETF (CADR FROM) (- (RECT-TOP R) (CADDR (RECT-SOURCE R))))
       (SETF (CAR TO) (+ (CAR FROM) (- (RECT-RIGHT R) (RECT-LEFT R))))
       (SETF (CADR TO) (+ (CADR FROM) (- (RECT-BOTTOM R) (RECT-TOP R))))
       (AND PAGE-FLAG (PAGE-IN-PIXEL-ARRAY ARRAY FROM TO))
       (BITBLT (OR ALU ALU-SETA) (- (RECT-RIGHT R) (RECT-LEFT R))
	       (- (RECT-BOTTOM R) (RECT-TOP R))
	       ARRAY
	       ;; Take chunk offset to rectangle origin
	       (CAR FROM) (CADR FROM)
	       TO-ARRAY (+ X (RECT-LEFT R)) (+ Y (RECT-TOP R)))
       (SETQ RECTANGLE-LIST (DELETE R (THE LIST RECTANGLE-LIST) :TEST #'EQ)))))
  (PAGE-OUT-ARRAY ARRAY)
  RECTANGLE-LIST) 

;;; Screen manager message handlers and flavors

(DEFWRAPPER (SHEET :SCREEN-MANAGE) (IGNORE . BODY)
  `(LOCK-SHEET (SELF)
     . ,BODY))

;;; Deexposed sheets are defaultly "visible" -- they will show through if they can
;;; and if their priority allows them to.
;;; If you redefine this to return T even if there is no bit-array, the right thing
;;; will happen (the window will be refreshed into a temporary array and the
;;; results of that will show through).
(DEFWRAPPER (SHEET :SCREEN-MANAGE-DEEXPOSED-VISIBILITY) (IGNORE . BODY)
  `(AND (OR (NULL PRIORITY) (>= PRIORITY 0))
	(PROGN . ,BODY)))

(DEFMETHOD (SHEET :SCREEN-MANAGE-DEEXPOSED-VISIBILITY) ()
  (NOT (NULL BIT-ARRAY)))

(DEFMETHOD (SHEET :SCREEN-MANAGE) (&REST ARGS)
  "This performs screen management on a sheet.  This always works,
even if screen management is inhibited.  It will also do autoexposure on
the sheet, unless screen management is inhibited.  This allows you to
batch a series of screen manages without running autoexposure each
time.  It is expected that autoexposure gets run explicitly in this
case."
  (SEND SELF :ORDER-INFERIORS)
  (SEND SELF :SCREEN-MANAGE-AUTOEXPOSE-INFERIORS)
  (APPLY #'SCREEN-MANAGE-SHEET SELF ARGS)) 

;;; Tell SHEET to do something appropriate for a rectangle
;;; that is not covered by any inferior of SHEET.
;;; Note: Rectangles given to :SCREEN-MANAGE-UNCOVERED-AREA are 0 origin and
;;;       point into SELF.  This is guaranteed, and need not be checked for.
(DEFMETHOD (SHEET :SCREEN-MANAGE-UNCOVERED-AREA) (RECTS ARRAY X Y ALU)
  ARRAY X Y ALU		;Unused
  (DOLIST (R RECTS)
    (AND (EQ (CAR (RECT-SOURCE R)) SELF) (SETQ RECTS (DELETE R (THE LIST RECTS) :TEST #'EQ))))
  RECTS) 

(DEFMETHOD (SCREEN :SCREEN-MANAGE-UNCOVERED-AREA)
           SCREEN-MANAGE-CLEAR-UNCOVERED-AREA)

(DEFUN SCREEN-MANAGE-CLEAR-UNCOVERED-AREA (IGNORE RECTS ARRAY X Y ALU)
  "Default is to clear area.  This can be redefined if that isn't
desireable."
  (DOLIST (R RECTS)
    (COND
      ((EQ (CAR (RECT-SOURCE R)) SELF)
       (SCREEN-MANAGE-CLEAR-RECTANGLE R ARRAY X Y ALU)
       (SETQ RECTS (DELETE R (THE LIST RECTS) :TEST #'EQ)))))
  RECTS) 

(DEFMETHOD (SHEET :SCREEN-MANAGE-RESTORE-AREA) (RECTS ARRAY X Y ALU)
  "Default way to restore bits.
If there is a bit array, restore from there.  If there is no bit array,
simply clear the area, unless :SCREEN-MANAGE-DEEXPOSED-VISIBILITY
returns T in which case refresh the bits into a temporary array and
restore from that."
  (SCREEN-MANAGE-RESTORE-AREA
    RECTS ARRAY X Y ALU
    (NOT (SEND SELF :SCREEN-MANAGE-DEEXPOSED-VISIBILITY))))


(DEFFLAVOR NO-SCREEN-MANAGING-MIXIN () ()
  (:DOCUMENTATION :MIXIN "Prevents screen managing within windows of
this flavor.  That is, the screen manager will not update the parts of
the inferiors of these windows into their proper places."))

(DEFMETHOD (NO-SCREEN-MANAGING-MIXIN :SCREEN-MANAGE)
           (&REST IGNORE) NIL)
(DEFMETHOD (NO-SCREEN-MANAGING-MIXIN :SCREEN-MANAGE-UNCOVERED-AREA)
           (&REST IGNORE) NIL)

(DEFFLAVOR SHOW-PARTIALLY-VISIBLE-MIXIN () ()
  (:DOCUMENTATION :MIXIN "If parts of this window are visible but not
all, show partial contents."))

(DEFMETHOD (SHOW-PARTIALLY-VISIBLE-MIXIN :SCREEN-MANAGE-DEEXPOSED-VISIBILITY) () T)

;;; Graying stuff

(DEFFLAVOR GRAY-DEEXPOSED-WRONG-MIXIN ((GRAY-ARRAY HES-gray)) ()
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :INITABLE-INSTANCE-VARIABLES
  (:INCLUDED-FLAVORS SHEET)
  (:DOCUMENTATION :MIXIN "Grayed over when deexposed"))

(DEFWRAPPER (GRAY-DEEXPOSED-WRONG-MIXIN :SCREEN-MANAGE-RESTORE-AREA)
	        ((RECTS ARRAY X Y ALU) . BODY)
  `(LET ((SI:.DAEMON-CALLER-ARGS. (LIST NIL RECTS ARRAY X Y (OR ALU CHAR-ALUF))))
     (DOLIST (R RECTS)
       (AND (EQ (CAR (RECT-SOURCE R)) SELF)
	    (SCREEN-MANAGE-GRAY-RECTANGLE R ARRAY X Y ALU-SETA)))
     . ,BODY))

(DEFMETHOD (GRAY-DEEXPOSED-WRONG-MIXIN :SCREEN-MANAGE-UNCOVERED-AREA) (RECTS ARRAY X Y IGNORE)
  (DOLIST (R RECTS)
    (COND
      ((EQ (CAR (RECT-SOURCE R)) SELF)
       (SCREEN-MANAGE-GRAY-RECTANGLE R ARRAY X Y ALU-SETA)
       (SETQ RECTS (DELETE R (THE LIST RECTS) :TEST #'EQ)))))
  RECTS) 

;;; This flavor causes the deexposed window to be grayed over.  It works for windows
;;; which have inferiors, as well as those which do not.
(DEFFLAVOR GRAY-DEEXPOSED-RIGHT-MIXIN ((GRAY-ARRAY hes-gray)) ()
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :INITABLE-INSTANCE-VARIABLES
  (:INCLUDED-FLAVORS SHEET)
  (:DOCUMENTATION :MIXIN "Grayed over when deexposed"))

(DEFUN SCREEN-MANAGE-GRAY-RECTANGLE (RECT ARRAY X Y ALU)
  "Gray the specified rectangle on the specified array.
All graying is relative to (0, 0) on the sheet that the rectangle is on."
  (DECLARE (:SELF-FLAVOR GRAY-DEEXPOSED-RIGHT-MIXIN))
  (LET ((X-OFF (- (RECT-LEFT RECT) (SECOND (RECT-SOURCE RECT))))
	(Y-OFF (- (RECT-TOP RECT) (THIRD (RECT-SOURCE RECT)))))
    (prepare-sheet (self)
      (BITBLT (OR ALU CHAR-ALUF)
	      (- (RECT-RIGHT RECT) (RECT-LEFT RECT))
	      (- (RECT-BOTTOM RECT) (RECT-TOP RECT))
	      GRAY-ARRAY
	      (REM x-OFF (ARRAY-DIMENSION GRAY-ARRAY 1))
	      (REM y-OFF (ARRAY-DIMENSION GRAY-ARRAY 0))
	      ARRAY (+ X (RECT-LEFT RECT))
	      (+ Y (RECT-TOP RECT))))))

(DEFUN GRAY-DEEXPOSED-RIGHT-RESTORE-INTERNAL (RECTS KLUDGE-ARRAY ARRAY X Y ALU)
  "This is an internal function for the wrapper of the grayer.  It grays
the window in the internal bit array, and then causes the appropriate
rectangles to be blted onto the screen."
  (DECLARE (:SELF-FLAVOR GRAY-DEEXPOSED-RIGHT-MIXIN))
  (SCREEN-MANAGE-GRAY-RECTANGLE `((,SELF 0 0) 0 0 ,WIDTH ,HEIGHT)
				KLUDGE-ARRAY 0 0 CHAR-ALUF)
  (SCREEN-MANAGE-RESTORE-AREA-FROM-BIT-ARRAY RECTS KLUDGE-ARRAY ARRAY X Y NIL
					     (OR ALU ALU-SETA)))

(DEFMETHOD (GRAY-DEEXPOSED-RIGHT-MIXIN :SCREEN-MANAGE-UNCOVERED-AREA) (RECTS ARRAY X Y IGNORE)
  (DOLIST (R RECTS)
    (COND
      ((EQ (CAR (RECT-SOURCE R)) SELF)
       (SCREEN-MANAGE-GRAY-RECTANGLE R ARRAY X Y ALU-SETA)
       (SETQ RECTS (DELETE R (THE LIST RECTS) :TEST #'EQ)))))
  RECTS) 

(DEFWRAPPER (GRAY-DEEXPOSED-RIGHT-MIXIN :SCREEN-MANAGE-RESTORE-AREA)
	        ((RECTS ARRAY X Y ALU) . BODY)
  `(USING-RESOURCE (KLUDGE-ARRAY SCREEN-MANAGER-BIT-ARRAY-RESOURCE)
     (PAGE-IN-PIXEL-ARRAY KLUDGE-ARRAY NIL (LIST WIDTH HEIGHT))
     ;; This is a kludge -- fudge the arguments to all methods inside
     (LET ((SI:.DAEMON-CALLER-ARGS. (LIST NIL (COPY-TREE RECTS) KLUDGE-ARRAY
					  (- X-OFFSET) (- Y-OFFSET) ALU)))
       . ,BODY)
     (GRAY-DEEXPOSED-RIGHT-RESTORE-INTERNAL RECTS KLUDGE-ARRAY ARRAY X Y ALU)))

;;; Interfaces to the other software
(DEFVAR SCREEN-MANAGE-TRACE-OUTPUT NIL
  "If non-NIL, stream for debugging journal of stream-manager actions.")

(DEFUN SCREEN-ACTIVITY-HAS-CHANGED (WINDOW ON-P)
  "Update screen manager when WINDOW becomes active or inactive.
ON-P is T for WINDOW becoming active."
  ON-P						;Isn't very interesting
  (AND SCREEN-MANAGE-TRACE-OUTPUT
       (FORMAT SCREEN-MANAGE-TRACE-OUTPUT
	       "~&Activity change: window ~S, ~:[Deactivate~;Activate~]~%" WINDOW ON-P))
  (COND
    ((SEND WINDOW :SCREEN-MANAGE-DEEXPOSED-VISIBILITY)
     ;; If window is visible when deexposed, then screen management is useful
     (SCREEN-MANAGE-WINDOW-AREA WINDOW))
    (T (SCREEN-MANAGE-FLUSH-KNOWLEDGE WINDOW)))) 

(DEFUN SCREEN-CONFIGURATION-HAS-CHANGED
       (WINDOW &OPTIONAL (WHY :FORCE)
	&AUX DO-IT (SUP (SHEET-SUPERIOR WINDOW)))
  "Update screen when WINDOW becomes exposed or deexposed.
WHY should be :EXPOSE if WINDOW is being exposed, :DEEXPOSE if
WINDOW is being deexposed, or :FORCE (actually same as :DEEXPOSE
here)."
  (COND
    ((MEMBER WINDOW (SHEET-INFERIORS SUP) :TEST #'EQ)
     ;; Only consider active windows
     (CASE WHY
       (:EXPOSE
	;; Just exposed a window, don't need to hack it's area any
	;; (except that one can optimize by removing all areas from
	;; consideration for screen management that it subsumes).
	(LET ((RECT
	       (LIST () (SHEET-X-OFFSET WINDOW) (SHEET-Y-OFFSET WINDOW)
		     (+ (SHEET-X-OFFSET WINDOW) (SHEET-WIDTH WINDOW))
		     (+ (SHEET-Y-OFFSET WINDOW) (SHEET-HEIGHT WINDOW)))))
	  (WITHOUT-INTERRUPTS
	   (DOLIST (QE SCREEN-MANAGER-QUEUE)
	     (AND (EQ (CAR (RECT-SOURCE QE)) SUP)
		  (RECT-WITHIN-RECT-P QE RECT)
		  (SETQ SCREEN-MANAGER-QUEUE
			(DELETE QE (THE LIST SCREEN-MANAGER-QUEUE) :TEST #'EQ)))))))
       ((:DEEXPOSE :FORCE)
	;; Deexposing, things may have changed underneath it, so screen manage even
	;; if it is temporary
	(SETQ DO-IT T)))
     (AND SUP (SEND SUP :ORDER-INFERIORS))
     (AND SCREEN-MANAGE-TRACE-OUTPUT
	(FORMAT SCREEN-MANAGE-TRACE-OUTPUT "~&Configuration change: window ~S, reason ~A~%"
		WINDOW WHY))
     (COND
       (DO-IT (SCREEN-MANAGE-WINDOW-AREA WINDOW))
       (T (SCREEN-MANAGE-FLUSH-KNOWLEDGE SUP)))))) 

(DEFUN SCREEN-AREA-HAS-CHANGED (WINDOW &REST RECT &AUX (SUP (SHEET-SUPERIOR WINDOW)))
  "Update screen when WINDOW's edges have changed.
RECT should be the edges of the area to update."
  (COND
    ((MEMBER WINDOW (SHEET-INFERIORS SUP) :TEST #'EQ)
     (AND SUP (SEND SUP :ORDER-INFERIORS))
     (COND
       ((OR (AND RECT (SHEET-EXPOSED-P WINDOW))	;Explicit rectangle, and exposed
	    (SEND WINDOW :SCREEN-MANAGE-DEEXPOSED-VISIBILITY))
	(AND SCREEN-MANAGE-TRACE-OUTPUT
	     (FORMAT SCREEN-MANAGE-TRACE-OUTPUT "~&Area change: window ~S~%" WINDOW))
	(APPLY #'SCREEN-MANAGE-WINDOW-AREA WINDOW RECT)))))) 

(DEFUN SCREEN-MANAGE-WINDOW-AREA
       (WINDOW
        &OPTIONAL
        (LEFT      (SHEET-X-OFFSET WINDOW))
        (TOP       (SHEET-Y-OFFSET WINDOW))
        (RIGHT (+  (SHEET-X-OFFSET WINDOW)
                   (SHEET-WIDTH    WINDOW)))
        (BOTTOM (+ (SHEET-Y-OFFSET WINDOW)
                   (SHEET-HEIGHT   WINDOW)))
        &AUX
        (SUP       (SHEET-SUPERIOR WINDOW)))

  (AND SUP (SCREEN-MANAGE-QUEUE SUP LEFT TOP RIGHT BOTTOM)))


(DEFUN SCREEN-MANAGE-QUEUE (SHEET &OPTIONAL LEFT TOP RIGHT BOTTOM &AUX E (INHIBIT-SCHEDULING-FLAG T))
  "Add request to update the specified part of the window SUP.
The four edges are relative to SUP.  The request is processed now or
goes on the screen manager queue to be processed later.  Processing
puts the bits of the correct inferior of SUP onto the rectangle."
  (SETQ E (IF (CONSP SHEET)
	      SHEET
	      (LIST (LIST SHEET 0 0) LEFT TOP RIGHT BOTTOM)))
  ;; Add to queue, eliminating redundant entries
  (COND
    ((DOLIST (QE SCREEN-MANAGER-QUEUE)
       (COND
	 ((EQ (CAR (RECT-SOURCE E)) (CAR (RECT-SOURCE QE)))
	  (AND (RECT-WITHIN-RECT-P E QE)
	       (RETURN T))
	  (AND (RECT-WITHIN-RECT-P QE E)
	       (SETQ SCREEN-MANAGER-QUEUE (DELETE QE (THE LIST SCREEN-MANAGER-QUEUE) :TEST #'EQ)))))))
    (T
     (AND SCREEN-MANAGE-TRACE-OUTPUT
	  (FORMAT SCREEN-MANAGE-TRACE-OUTPUT "~&Queueing rectangle ~S, inhibit ~A~%" E
		  INHIBIT-SCREEN-MANAGEMENT))
     (PUSH E SCREEN-MANAGER-QUEUE)
     ;; Do right away if possible, otherwise leave on queue
     (OR INHIBIT-SCREEN-MANAGEMENT
	 (SCREEN-MANAGE-DEQUEUE-ENTRY E))))) 

(DEFUN SCREEN-MANAGE-DELAYING-SCREEN-MANAGEMENT-INTERNAL
       (;&OPTIONAL OLD-STYLE
        &AUX (INHIBIT-SCHEDULING-FLAG T))
  "Called if stuff got queued during a DELAYING-SCREEN-MANAGEMENT.
Process the queued requests now unless management is still being
delayed, but leave them queued if they want to wait for a lock."
  (OR INHIBIT-SCREEN-MANAGEMENT
      ;; Only try to dequeue if not delaying anymore
      (SCREEN-MANAGE-DEQUEUE)))

(DEFUN SCREEN-MANAGE-DEQUEUE (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Process the requests for screen management queued during a
DELAYING-SCREEN-MANAGEMENT.  If a request requires a lock that is
locked, leave it queued."
  (DO ((Q SCREEN-MANAGER-QUEUE))
      ((NULL Q))
    (IF (SCREEN-MANAGE-DEQUEUE-ENTRY (CAR Q))
	;; If the entry actually got dequeued, then interrupts were
        ;; allowed and so the queue might have gotten hacked.  Restart
        ;; from the beginning.
	(SETQ Q SCREEN-MANAGER-QUEUE)
	(SETQ Q (CDR Q)))))

(DEFUN SCREEN-MANAGE-DEQUEUE-DELAYED-ENTRIES (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Process queued requests for screen management, waiting for locks if
necessary."
  (DO ((Q SCREEN-MANAGER-QUEUE SCREEN-MANAGER-QUEUE))
      ((NULL Q))
    ;; This reenables scheduling if it does anything.
    (SCREEN-MANAGE-DEQUEUE-ENTRY (CAR Q) T)))

(DEFUN SCREEN-MANAGE-DEQUEUE-ENTRY (ENTRY &OPTIONAL UNCOND &AUX ALL)
  "Handle one entry from the screen manager's queue.
UNCOND non-NIL says wait for a lock if one is needed,
otherwise return NIL and don't dequeue the entry, in that case.

Must be called with INHIBIT-SCHEDULING-FLAG bound to T.  May set
that flag to NIL and then set it back to T again, and during that time
more requests may get queued!  The code returns T if it actually
dequeued the entry, else NIL."
  (COND
    ((OR UNCOND (SHEET-CAN-GET-LOCK (CAR (RECT-SOURCE ENTRY))))
     ;; May as well do all rectangles on this sheet together.  ALL
     ;; gets a list of them.
     (SETQ SCREEN-MANAGER-QUEUE (DELETE ENTRY (THE LIST SCREEN-MANAGER-QUEUE) :TEST #'EQ)
	   ALL (CONS ENTRY ()))
     (DOLIST (E SCREEN-MANAGER-QUEUE)
       (COND
	 ((EQ (CAR (RECT-SOURCE ENTRY)) (CAR (RECT-SOURCE E)))
	  (SETQ SCREEN-MANAGER-QUEUE (DELETE E (THE LIST SCREEN-MANAGER-QUEUE) :TEST #'EQ))
	  (PUSH E ALL))))
     (AND SCREEN-MANAGE-TRACE-OUTPUT
	  (FORMAT SCREEN-MANAGE-TRACE-OUTPUT "~&Dequeueing ~S~%Queue is ~S~%" ALL
		  SCREEN-MANAGER-QUEUE))
     (LET ((SHEET (CAR (RECT-SOURCE ENTRY))))
       (IF (SHEET-SCREEN-ARRAY SHEET)
	   ;; FORCE-ACCESS so that PREPARE-SHEET won't look at the
	   ;; output-hold flag.
	   (SHEET-FORCE-ACCESS (SHEET :NO-PREPARE) (SETQ INHIBIT-SCHEDULING-FLAG ())
			       (SEND (CAR (RECT-SOURCE ENTRY)) :SCREEN-MANAGE ALL))
	   ;; If can't screen manage (no screen!), then just do autoexposure.
	   (LOCK-SHEET (SHEET) (SETQ INHIBIT-SCHEDULING-FLAG ()) (SEND SHEET :ORDER-INFERIORS)
		       (SEND SHEET :SCREEN-MANAGE-AUTOEXPOSE-INFERIORS)))
       (SETQ INHIBIT-SCHEDULING-FLAG T)
       T))
    (T NIL))) 

;;; Note that this message does not mean automatically expose this sheet.
;;; It means consider the inferiors of this sheet for automatic exposing.
(DEFMETHOD (SHEET :SCREEN-MANAGE-AUTOEXPOSE-INFERIORS) ()
  "Consider the inferiors of this sheet for automatic exposing."
  (SCREEN-MANAGE-AUTOEXPOSE-INFERIORS SELF))

(DEFUN SCREEN-MANAGE-AUTOEXPOSE-INFERIORS (SHEET &AUX INTERESTING-INFERIORS)
  "Expose all inferiors of SHEET that are uncovered but not exposed.
SHEET should be locked."
  ;; No need to do any screen management,
  ;; since exposure always does the right thing,
  ;; and this can never cause a sheet to become deexposed.
  ;; First, get an ordered list of all sheets of interest
  ;; SHEET-INFERIORS has been ordered by priority.
  (LOCK-SHEET (SHEET)
    (DOLIST (I (SHEET-INFERIORS SHEET))
      (OR (MEMBER I (SHEET-EXPOSED-INFERIORS SHEET) :TEST #'EQ)
;	  (NOT (SEND I :SCREEN-MANAGE-DEEXPOSED-VISIBILITY))
	  (NOT (SHEET-WITHIN-SHEET-P I SHEET))
	  (<= (OR (SHEET-PRIORITY I) 0) -1)
	  (PUSH I INTERESTING-INFERIORS)))
    (SETQ INTERESTING-INFERIORS (NREVERSE INTERESTING-INFERIORS))
    (AND SCREEN-MANAGE-TRACE-OUTPUT
	 (FORMAT SCREEN-MANAGE-TRACE-OUTPUT "~&Autoexpose-inferiors: ~S~%" INTERESTING-INFERIORS))
    ;; Now, we have a list of interesting: deexposed and active
    ;; Expose them one by one if they aren't covered.
    (DOLIST (I INTERESTING-INFERIORS)
      (COND
	((DOLIST (EI (SHEET-EXPOSED-INFERIORS SHEET))
	   (AND (SHEET-OVERLAPS-SHEET-P EI I)
		;; This clause is covered: do nothing.
		(RETURN T))))
	;; Don't expose if it would cover anything earlier in the
	;; list.  What this does is prevent violations of priority;
	;; something earlier in the list might not be exposed
	;; because some other part of it was covered.
	((DOLIST (HP INTERESTING-INFERIORS)
	   (AND (EQ I HP)
		(RETURN T))
	   (AND (SEND HP :SCREEN-MANAGE-DEEXPOSED-VISIBILITY)
		(SHEET-OVERLAPS-SHEET-P I HP)
		(RETURN ())))
	 (SEND I :EXPOSE)
	 (SETQ INTERESTING-INFERIORS (DELETE I (THE LIST INTERESTING-INFERIORS) :TEST #'EQ)))))
    (AND (EQ SHEET MOUSE-SHEET)
	 (NULL SELECTED-WINDOW)
	 (SETQ INTERESTING-INFERIORS (SHEET-EXPOSED-INFERIORS SHEET))
	 ;; If hacking the sheet the mouse is on, and there is no window
	 ;; currently selected, select a window.
	 (DOLIST (I INTERESTING-INFERIORS)
	   (AND (SEND I :NAME-FOR-SELECTION)
		(RETURN (SEND I :SELECT))))))) 

;;; Screen manager's background process

;;; The background process is responsible for trying to handle the
;;; pending queue of screen manages, as well as updating windows which
;;; are in :PERMIT mode, deexposed, and have actually done typeout since
;;; we were last here.

(DEFVAR SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS NIL
  "NIL not to do it, or time to sleep.")

(DEFVAR SCREEN-MANAGE-TIME-BETWEEN-DEQUEUES 10.
  "Check queue every 1/6 second while it is non-empty (i.e. try again
to get locks this often).")

(DEFUN SCREEN-MANAGE-BACKGROUND-TOP-LEVEL ()
  (DO ((HEAD-OF-QUEUE SCREEN-MANAGER-QUEUE SCREEN-MANAGER-QUEUE)
       (SLEEP-TIME))
      (NIL)
    (SETQ SLEEP-TIME
	  (COND
	    (SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS
	     (COND
	       ((NULL HEAD-OF-QUEUE) SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS)
	       (T
		(MIN SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS SCREEN-MANAGE-TIME-BETWEEN-DEQUEUES))))
	    (INHIBIT-SCREEN-MANAGEMENT NIL)
	    ((NULL HEAD-OF-QUEUE) NIL)
	    (T SCREEN-MANAGE-TIME-BETWEEN-DEQUEUES)))
    ;; Wait until queue has changed, and screen management not inhibited.
    ;; Except SLEEP-TIME if non-null is a timeout.
    (PROCESS-WAIT "Screen Manage"
		  #'(LAMBDA (SLEEP-TIME LAST-TIME HEAD-OF-QUEUE)
		      (OR (AND SLEEP-TIME
			       (> (TIME-DIFFERENCE (TIME) LAST-TIME) SLEEP-TIME))
			  (AND (NULL SLEEP-TIME)
			       SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS)
			  (AND (NOT INHIBIT-SCREEN-MANAGEMENT)
			       (NEQ SCREEN-MANAGER-QUEUE HEAD-OF-QUEUE))))
		  SLEEP-TIME (TIME) HEAD-OF-QUEUE)
    (OR INHIBIT-SCREEN-MANAGEMENT
	(SCREEN-MANAGE-DEQUEUE))
    (AND SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS
	 (WITHOUT-INTERRUPTS
	   (DOLIST (S ALL-THE-SCREENS)
	     (AND (SHEET-EXPOSED-P S)
		  (SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS S))))))) 

(DEFUN SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS
       (SHEET &AUX (EXPSD-INFS (SHEET-EXPOSED-INFERIORS SHEET)))
  (DOLIST (I (SHEET-INFERIORS SHEET))
    (SCREEN-MANAGE-UPDATE-PERMITTED-WINDOWS I)
    (COND
      ((AND (NOT (MEMBER I EXPSD-INFS :TEST #'EQ))
	    (>= (OR (SHEET-PRIORITY I) 0) 0)
	    (EQ (SHEET-DEEXPOSED-TYPEOUT-ACTION I) :PERMIT)
	    (ZEROP (SHEET-OUTPUT-HOLD-FLAG I)))
       ;; Sheet is permitted, deexposed, and has been typed on.
       ;; Update it on screen.
       (SETF (SHEET-OUTPUT-HOLD-FLAG I) 1) (SCREEN-CONFIGURATION-HAS-CHANGED I)))))

(DEFVAR SCREEN-MANAGER-BACKGROUND-PROCESS NIL)

;;; This function exists solely to initialize the SCREEN-MANAGER-BACKGROUND-PROCESS
;;; variable for the screen manager.  This function is referenced in an
;;; ADD-INITIALIZATION which is where it should have been in the first place.
(DEFUN INITIALIZE-SCREEN-MANAGER ()
  (SETQ SCREEN-MANAGER-BACKGROUND-PROCESS (PROCESS-RUN-RESTARTABLE-FUNCTION "Screen Manager Background"
                                                                            'SCREEN-MANAGE-BACKGROUND-TOP-LEVEL)))
