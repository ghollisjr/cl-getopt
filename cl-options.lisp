(in-package :cl-options)

(define-foreign-library libc
  (:unix "libc.so.6")
  (t (:default "libc")))
(use-foreign-library libc)

(defcstruct (option :class option)
  (:name :string)
  (:argspec :int)
  (:flag (:pointer :int))
  (:val :int))

(defcfun ("getopt_long" c-getopt-long) :char
  (argc :int)
  (argv (:pointer :string))
  (shortopts :string)
  (longopts (:pointer (:struct option)))
  (index (:pointer :int)))

(defmacro with-foreign-args ((argc argv) args &body body)
  "Converts a Lisp list of strings denoting the arguments supplied to
a command into two values:

* argc: number of argument strings
* argv: array of argument strings

These are bound to the symbols supplied in the (argc argv) list."
  (let* ((argsym (gensym)))
    `(let* ((,argsym ,args)
            (,argc (length ,args)))
       (with-foreign-array (,argv (coerce ,argsym 'vector)
                                  (list :array :string ,argc))
         ,@body))))

(defun option-descriptions (options)
  "Generates a description string for the list of options supplied."
  (flet ((safestr (s)
           (string-downcase (string s))))
    (let* ((noptions (length options)))
      (with-output-to-string (s)
        (loop
           for option in options
           for i from 1
           do (destructuring-bind (&key short long description
                                        &allow-other-keys)
                  option
                (format s "~a~c~c~a"
                        (cond
                          ((and short long)
                           (format nil "-~a, --~a:"
                                   (safestr short)
                                   (safestr long)))
                          (short
                           (format nil "-~a:"
                                   (safestr short)))
                          (long
                           (format nil "--~a:"
                                   (safestr long))))
                        #\tab #\tab
                        (if description
                            description
                            "not documented"))
                (when (not (equal i noptions))
                  (format s "~%"))))))))

(defun ->keysym (x)
  (intern (string-upcase (string x)) :keyword))

(defun ->shortchar (x)
  (elt (string-downcase (string x)) 0))

(defun option-shortarg (option)
  (when (getf option :short)
    (->shortchar (getf option :short))))

(defun options->shortargs (options)
  (let* ((shorts
          (loop
             for option in options
             for s = (option-shortarg option)
             for argspec = (getf option :argspec)
             when s
             appending
               (cond
                 ((or (null argspec)
                      (eq argspec :none))
                  (list s))
                 ((eq argspec :optional)
                  (list s #\: #\:))
                 ((eq argspec :required)
                  (list s #\:))))))
    (coerce shorts 'string)))

(defmacro with-foreign-longopts (array-binding options &body body)
  (let* ((opts (gensym))
         (opt (gensym))
         (long (gensym))
         (argspec (gensym)))
    `(let* ((,opts ,options))
       (with-foreign-array (,array-binding
                            (coerce
                             (loop
                                for ,opt in ,opts
                                for ,long = (getf ,opt :long)
                                for ,argspec = (getf ,opt :argspec)
                                when ,long
                                collecting
                                  (let* ((,argspec (if ,argspec
                                                       ,argspec
                                                       :none)))
                                    (list :name (string-downcase (string ,long))
                                          :argspec
                                          (cond
                                            ((eq ,argspec :none)
                                             +NO-ARGUMENT+)
                                            ((eq ,argspec :required)
                                             +REQUIRED-ARGUMENT+)
                                            ((eq ,argspec :optional)
                                             +OPTIONAL-ARGUMENT+))
                                          :flag (null-pointer))))
                             'vector)
                            (list :array '(:struct option) (length ,opts)))
         ,@body))))

(defmacro with-foreign-shortopts (shortopts-binding options
                                  &body body)
  (let* ((opts (gensym)))
    `(let* ((,opts ,options))
       (with-foreign-string (,shortopts-binding
                             (options->shortargs ,opts))
         ,@body))))

(defun getopt (args options)
  "Returns the values

* option-values
* remaining-arguments

given the list of input arguments and options specification.
option-values is a plist of the short or long argument keyword
name (short if present) and the value of that option.
remaining-arguments is a list of the remaining arguments after
processing the options.

options should be a list of plists, each plist of the form (&key long
short argspec default documentation) where

* long is a symbol, string or character naming the long argument name.
* short is a symbol, string or character naming the short argument name.
* argspec is one of the values :none, :required, :optional.
* default is a default value for options when no value is supplied.
* description is a string to be used in an automatically generated argument description message."
  (with-foreign-args (argc argv) args
    (with-foreign-longopts longopts options
      (with-foreign-shortopts shortopts options
        ;; stub
        ))))
