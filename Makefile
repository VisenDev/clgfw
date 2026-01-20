CODE='(load "build.lisp")'

sbcl:
	sbcl --script build.lisp

ecl:
	ecl --load build.lisp

clean:
	if [ -e hello ]; then trash hello; fi
	find . -type f -name '*.fasl' -exec trash {} \;
	find . -type f -name '*.fas' -exec trash {} \;

.PHONY: clean ecl sbcl
