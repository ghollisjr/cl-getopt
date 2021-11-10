all: sbcl-core/cl-getopt.core

sbcl-core/cl-getopt.core:
	./build-cl-getopt-core.sh
install: sbcl-core/cl-getopt.core
	mkdir -p ${HOME}/lib/sbcl-cores
	cp sbcl-core/cl-getopt.core ${HOME}/lib/sbcl-cores
clean:
	rm -f sbcl-core/*.core
