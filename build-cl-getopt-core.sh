#!/bin/bash
echo '(ql:quickload "cl-getopt") (sb-ext:save-lisp-and-die "sbcl-core/cl-getopt.core")' | sbcl
