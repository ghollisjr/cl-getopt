cl-getopt is a Common Lisp CFFI wrapper to the libc utility
"getopt_long".

See example.lisp for an example SBCL script using this library.  To
run the example, follow the instructions at the top of the script to
install it and then run

./example.lisp

with whatever options you want to see.  Note that for old systems
/usr/bin/env won't support the "-S" option which makes it difficult to
pass arguments to sbcl as a script.  In this case you are probably
already familiar with how to run Lisp scripts in some other way, but
if you're not then the general rule is to create an ordinary shell
script and call SBCL with whatever arguments you like rather than
exploiting the hash-bang script syntax.

There is an "install" target in the Makefile if you want to place the
cl-getopt SBCL core file somewhere it will be useful for other
scripts, e.g. genpass (https://www.github.com/ghollisjr/genpass).

cl-getopt is in the public domain.
