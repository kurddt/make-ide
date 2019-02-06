EMACS ?= emacs
CASK ?= cask

#EMACS_LOAD := -L /home/glager/.emacs.d/elpa/levenshtein-20090830.1040/ -L /home/glager/.emacs.d/elpa/s-20180406.808/ -L /home/glager/.emacs.d/elpa/dash-20180910.1856/

all: test

travis: test


test: clean-elc
	${MAKE} all-tests
	${MAKE} compile
	${MAKE} all-tests
	${MAKE} clean-elc

all-tests:
	${MAKE} unit
	${MAKE} file-test

unit:
	${CASK} exec ert-runner test/make-ide-test.el test/utils-test.el

file-test:
	${CASK} exec ert-runner test/file-test.el

compile:
	${CASK} exec ${EMACS} -Q ${EMACS_LOAD} -batch -f batch-byte-compile make-ide.el 

clean-elc:
	rm -f *.elc

.PHONY:	all test unit
