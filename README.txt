cl-getopt is a Common Lisp CFFI wrapper to the libc utility
"getopt_long".

See example.lisp for an example SBCL script using this library.  Note
that it makes more sense in practice to create an SBCL core file and
load that in the script command rather than loading the library at
script run time.
