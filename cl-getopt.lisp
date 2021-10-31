(in-package :cl-getopt)

(declaim (optimize (debug 3)))

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

(defun ->optionstr (x)
  (string x)
  ;; (intern (string-upcase (string x)) :keyword)
  )

(defun ->shortchar (x)
  (elt (string x) 0))

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

(defun short-char (x)
  (if (characterp x)
      (char-code x)
      (char-code (elt (string x) 0))))

(defmacro with-foreign-longopts ((foreign-array long-only-list)
                                 options
                                 &body body)
  (let* ((opts (gensym))
         (opt (gensym))
         (short (gensym))
         (long (gensym))
         (nlongopts (gensym))
         (argspec (gensym)))
    `(let* ((,opts ,options)
            (,nlongopts 0)
            (,long-only-list
             (remove-if-not (lambda (x)
                              (getf x :long))
                            ,opts))
            (,nlongopts (length ,long-only-list)))
       (with-foreign-array (,foreign-array
                            (coerce
                             (loop
                                for ,opt in ,long-only-list
                                for ,long = (getf ,opt :long)
                                for ,short = (getf ,opt :short)
                                for ,argspec = (getf ,opt :argspec)
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
                                          :val (if ,short
                                                   (short-char ,short)
                                                   0)
                                          :flag (null-pointer))))
                             'vector)
                            (list :array '(:struct option) ,nlongopts))
         ,@body))))

(defmacro with-foreign-shortopts (shortopts-binding options
                                  &body body)
  (let* ((opts (gensym)))
    `(let* ((,opts ,options))
       (with-foreign-string (,shortopts-binding
                             (options->shortargs ,opts))
         ,@body))))

(defun getopt (args options
               &key
                 (command-arg-present-p t))
  "Returns the values

* option-values
* remaining-arguments

given the list of input arguments and options specification.
option-values is hash-table of the short or long argument name
string (short if present) and the value of that option.
remaining-arguments is a list of the remaining arguments after
processing the options.

Note that if no argument is supplied to an option, whether because the
argspec is :none or :optional, there will still be a list of NIL
elements for each time the option was supplied without an argument.
When the argspec is :required, some kind of value must be in the
argument list for each time that option was supplied.

options should be a list of plists, each plist of the form (&key long
short argspec default documentation) where

* long is a symbol, string or character naming the long argument name.
* short is a symbol, string or character naming the short argument name.
* argspec is one of the values :none, :required, :optional.
* description is a string to be used in an automatically generated argument description message.

command-arg-present-p should be T whenever the argument list includes
the command as the first element, and NIL when it's not included.  For
general scripting use it should be present, but this option is
included to allow easier use of getopt for parsing argument lists
without the command being present."
  (let* ((result-ht (make-hash-table :test 'equalp))
         (args (if command-arg-present-p
                   args
                   (list* "command" args))))
    (with-foreign-args (argc argv) args
      (with-foreign-longopts (longopts long-only) options
        (with-foreign-shortopts shortopts options
          (with-foreign-object (longindex :int)
            (setf +OPTIND+ 1)
            (loop
               for optchar = (c-getopt-long argc
                                            argv
                                            shortopts
                                            longopts
                                            longindex)
               while (and (not (= optchar -1))
                          (not (= optchar (char-code #\?))))
               do
                 (let* ((key
                         (if (zerop optchar)
                             ;; long only
                             (let* ((spec
                                     (elt long-only (mem-ref longindex :int))))
                               (->optionstr (getf spec :long)))
                             ;; short found
                             (->optionstr (code-char optchar))))
                        (val (convert-from-foreign +OPTARG+
                                                   :string)))
                   (push val (gethash key result-ht))))
            (let* ((option-values
                    (let* ((ht (make-hash-table :test 'equalp)))
                      (loop
                         for k being the hash-keys in result-ht
                         for v being the hash-values in result-ht
                         do (setf (gethash k ht)
                                  (reverse v)))
                      ht))
                   (remaining-arguments
                    (subseq args +OPTIND+)))
              (values option-values
                      remaining-arguments))))))))
