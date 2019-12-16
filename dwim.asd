



(asdf:defsystem :dwim
  :depends-on (:mcclim :closer-mop :zeta-lisp-compatability :flavors :asdf)
  ;; The DWIM part of SCIGRAPH
  :components
  ((:file package)
   ;; Not using bundled feature machinery, use trivial features
   ;; instead.
   (:file clim-compatability)
   (:file feature-case)
   (:file macros)
   (:file tv)
   (:file draw)
   (:file present)
   ))

      ;; "package"
      ;; "feature-case"
      ;; "macros"
      ;; "tv"
      ;; "draw"
      ;; "present"
      ;; "extensions"
      ;; "wholine"
      ;; "export"
      ;; )))
