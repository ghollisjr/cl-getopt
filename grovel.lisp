(in-package :cl-options)
(include "getopt.h")

(cvar ("optind" +OPTIND+) :int)
(cvar ("opterr" +OPTERR+) :int)
(cvar ("optopt" +OPTOPT+) :int)

(constant (+NO-ARGUMENT+ "no_argument") :type integer)
(constant (+REQUIRED-ARGUMENT+ "required_argument") :type integer)
(constant (+OPTIONAL-ARGUMENT+ "optional_argument") :type integer)
