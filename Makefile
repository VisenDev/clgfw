sbcl:
	sbcl --script "build.lisp"

clisp:
	clisp -x "(load \"build.lisp\")"

ecl:
	ecl --load "build.lisp"
