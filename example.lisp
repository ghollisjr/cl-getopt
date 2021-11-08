#!/usr/bin/env -S sbcl --core sbcl-core/cl-getopt.core --script
;;;; Note that you need to
;;;;
;;;; 0. Install SBCL.
;;;;
;;;; 1. Install quicklisp.
;;;; 
;;;; 2. Place cl-getopt somewhere that quicklisp knows about (e.g. add
;;;; the location to:
;;;; ~/.config/common-lisp/source-registry.conf.d/projects.conf).
;;;; 
;;;; 3. Run "make" to generate the SBCL core.
;;;;
;;;; before this script will run properly.
(in-package :cl-getopt)

(defparameter *options*
  (list (list :short "h"
              :long "help"
              :description "display this help message"
              :argspec :none)
        (list :short "v"
              :long "verbose"
              :description "increase verbosity"
              :argspec :none)
        (list :short "i"
              :long "input"
              :description "input file"
              :argspec :required)
        (list :short "o"
              :long "output"
              :description "output file"
              :argspec :required)
        (list :short "O"
              :long "optimize"
              :description "optimization with optional level (default 1)"
              :argspec :optional)))

(defun help ()
  (format t "example.lisp [options] <some message>~%~%~a~%"
          (option-descriptions *options*)))

(let* ((args sb-ext:*posix-argv*))
  (multiple-value-bind (options remaining-arguments)
      (getopt args *options*)
    (cond
      ((null (rest args))
       (format t "Use -h/--help for usage.~%"))
      ((gethash "h" options)
       (help))
      (t
       (let* ((alist
               (loop
                  for k being the hash-keys in options
                  for v being the hash-values in options
                  collecting (cons k v))))
         (format t "Your message: ~s~%Your options: ~s~%"
                 remaining-arguments
                 alist))))))
