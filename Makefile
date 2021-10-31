all: sbcl-core/cl-getopt.core

sbcl-core/cl-getopt.core:
	./build-cl-getopt-core.sh
clean:
	rm -f cores/*.core
