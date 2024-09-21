BIN=cl-snake
LISP=sbcl
BUNDLE=bundle
LIBS=:sdl2 :cl-opengl
BNFLAGS=--no-sysinit --non-interactive \
        --eval "(ql:quickload '($(LIBS)))" \
        --eval "(ql:bundle-systems '($(LIBS)) :to \"$(BUNDLE)/\")" \
        --eval '(exit)'
BUILDFLAGS=--no-sysinit --no-userinit --non-interactive \
	   --load "$(BUNDLE)/bundle.lisp" \
	   --eval '(asdf:load-system :sdl2)' \
	   --eval '(asdf:load-system :cl-opengl)' \
	   --eval '(load "snake.asd")' \
	   --eval '(asdf:make :snake)'

all: $(BIN)

$(BIN): $(BUNDLE)
	$(LISP) $(BUILDFLAGS)

$(BUNDLE):
	$(LISP) $(BNFLAGS)

clean_all:
	rm -rf bin/$(BIN) $(BUNDLE)

clean:
	rm -f bin/$(BIN)
