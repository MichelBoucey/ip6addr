RHS=runhaskell Setup.hs

all: clean configure build install

clean:
	$(RHS) clean

configure: clean
	$(RHS) configure 

build: clean configure
	$(RHS) build

install: build
	$(RHS) install

sdist: build
	$(RHS) sdist
