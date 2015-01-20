QUICKLISP_PATH=$(HOME)/quicklisp
SBCL=sbcl --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP=$(SBCL) --load $(QUICKLISP_PATH)/setup.lisp

all: cloudfront-log-processor

deps.txt: *.asd
	$(QUICKLISP) --eval "(push '*default-pathname-defaults* asdf:*central-registry*)" --eval '(ql:quickload "cloudfront-log-processor")'
	touch deps.txt

manifest.txt: deps.txt
	$(QUICKLISP) --eval '(ql:write-asdf-manifest-file "manifest.txt")'

cloudfront-log-processor: *.lisp *.asd manifest.txt
	buildapp --manifest-file manifest.txt --asdf-path . \
		--load-system cloudfront-log-processor \
		--entry cloudfront-log-processor::main \
		--output cloudfront-log-processor

clean:
	rm -f cloudfront-log-processor manifest.txt deps.txt

