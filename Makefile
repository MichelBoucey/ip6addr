RHS=runhaskell Setup.hs

all: clean configure build install

clean:
	$(RHS) clean

configure: clean
	$(RHS) configure

build: clean configure
	$(RHS) build


.PHONY: test
test:
	./test/test.pl

install: clean configure build
	$(RHS) install
