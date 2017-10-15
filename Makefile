.PHONY: vendor mac clean

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp)$$')

mac: $(lisps)
	sbcl --load "build/build.lisp"

# Clean -----------------------------------------------------------------------

clean:
	rm -rf bin
