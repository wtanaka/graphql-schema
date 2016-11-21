SOURCES=$(shell find src -name "*.hs")

all: lint test package

test: cabal-install cabal-configure build
	cabal test

lint:
	hlint .

.cabal-sandbox/bin/hlint: .cabal-sandbox/bin/happy

clean:
	find . -name "*~" -exec rm \{\} \;
	rm -rf dist

realclean: clean
	cabal sandbox delete

package: cabal-install haddock build

haddock: cabal-install cabal-configure
	cabal haddock

.cabal-sandbox/bin/%: cabal.sandbox.config
	cabal install $*

build: $(SOURCES)
	cabal build

cabal-configure:
	cabal configure --enable-tests

cabal-install: cabal-sandbox
	cabal install --only-dependencies --enable-tests

cabal-sandbox: cabal.sandbox.config

.cabal-sandbox cabal.sandbox.config:
	cabal sandbox init
	cabal update

%.hs: %.x
	alex $<

%.hs: %.y
	happy $<
