(asdf:defsystem #:cl-getopt
  :serial t
  :author "Gary Hollis"
  :license "Public Domain"
  :description "CFFI wrapper to the libc getopt_long function"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (:cffi)
  :components
  ((:file "package")
   (:cffi-grovel-file "grovel")
   (:file "cl-getopt")))
