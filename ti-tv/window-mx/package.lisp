

(defpackage :macintosh
  (:use cl #+ticl ticl)
  ;; (:import make-mac)
  (:nicknames :mac :mac-windows))

(defpackage :tv
  (:use cl))


;; these symbols should be imported by the mac package

;; Roughly, the following symbols are those needed to support the
;; 'classic' Lisp Machine TV system.
(defparameter *mac-support-symbols*
  '(*cold-load-streams-screen-selected-p*
    *default-Mac-window-height*
    *default-Mac-window-width*
    *dont-clip-at-the-margins*
    *Explorer-to-Mac-char-code-map*
    *ignore-commands-for-the-mac*
    *mac*
    *mac-keyboard-p*
    *mac-system-p*
    ;; dab 8/24/88
    *Mac-resident-Explorer-screens*
    ;; dab 8/24/88
    *undisplaced-Mac-window-arrays*
    *window-system-mouse-on-the-mac*
    ;; dab 08-24-88 DAB
    dump-draw-char-cache
    lispm-shift-bit
    lispm-greek-bit
    lispm-top-bit
    lispm-capslock-bit
    lispm-control-bit
    lispm-meta-bit
    lispm-super-bit
    lispm-hyper-bit
    lispm-altlock-bit
    lispm-modelock-bit
    lispm-repeat-bit

    Mac-CONTROL-bit
    Mac-OPTION-bit
    Mac-CAPSLOCK-bit
    Mac-SHIFT-bit
    Mac-COMMAND-bit
    Mac-MOUSE-bit
    Mac-AUTOKEY-bit
    Mac-Window-Activate-bit
    ;; dab 8/24/88
    mac-explorer-screen-ID-for-screen-being-created
    mac-extended-kbd-descriptor
    mac-kbd-modifier-table
    mac-kbd-modifier-table-zmacs-mode
    mac-screen
    ;; dab 8/24/88
    Mac-screen-for-printer
    mac-standard-kbd-descriptor
    ;; dab 8/24/88
    not-visible-p
    kbd-mac-table

    ;; functions
    activate-mac-window
    after-tv-initialized
    analyze-last-button
    deactivate-mac-screen
    deactivate-mac-window
    define-mac-resident-exp-screen
    deselect-cold-load-streams-screen
    give-mouse-ownership-to-the-explorer
    kbd-convert-mac
    kbd-make-mac-table
    lispm-or-mac-mouse-wakeup
    mac-consider-mouse
    mac-key
    mac-screen-p
    mac-system-p
    mac-window-p
    make-a-Mac-resident-Explorer-screen
    ;; dab 8/24/88
    make-explorer-screen
    redirect-drawing-of-window-and-inferiors
    ;; 10/18/88
    remember-bit-array
    remember-this-screens-last-selected-window
    select-cold-load-streams-screen
    send-adjust-bit-array-maybe
    send-draw-char
    send-draw-line
    send-draw-point
    send-draw-rectangle
    send-draw-string
    send-drawcircle
    send-drawhollowcircle
    send-drawhollowrectangle
    send-draw-filled-polygon
    send-select-window
    select-a-screen
    si-bitblt))


;; (import tv:*mac-support-symbols* 'mac)
