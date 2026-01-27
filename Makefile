CODE='(load "build.lisp")'

sbcl:
	sbcl --script build.lisp

ecl:
	ecl --load build.lisp

clean:
	if [ -e hello ]; then trash hello; fi
	if [ -e a.out ]; then trash a.out; fi
	find . -type f -name '*.fasl' -exec trash {} \;
	find . -type f -name '*.fas' -exec trash {} \;
	find . -type f -name '*.abcl' -exec trash {} \;
	find . -type f -name '*.dx64fsl' -exec trash {} \;

.PHONY: clean ecl sbcl
