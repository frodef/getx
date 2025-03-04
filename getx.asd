
(asdf:defsystem #:getx
  :description "Query a hierarcical data structure, see GETX:?."
  :author "Frode Fjeld <frodevf@gmail.com"
  :license  "Unlicense"
  :version "0.5.0"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "getx")))
