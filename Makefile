sbcl:
	sbcl --load scripts/build-hello.lisp

ecl:
	ecl --load scripts/build-hello.lisp

clisp:
	clisp -i ~/quicklisp/setup.lisp scripts/build-hello.lisp

clgfw.js:
	jscl scripts/jscl-build.lisp
	mkdir -p build
	mv clgfw.js build
	cp src/web-resources/* build
	cd build && php -S localhost:8000

clean:
	if [ -e hello ]; then trash hello; fi
	if [ -e a.out ]; then trash a.out; fi
	if [ -e clgfw.js ]; then trash clgfw.js; fi	
	find . -type f -name '*.fasl' -exec trash {} \;
	find . -type f -name '*.fas' -exec trash {} \;
	find . -type f -name '*.abcl' -exec trash {} \;
	find . -type f -name '*.dx64fsl' -exec trash {} \;

.PHONY: clean ecl sbcl clgfw.js
