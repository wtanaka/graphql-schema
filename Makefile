SOURCES=$(shell find src -name "*.hs")

all: package

test: cabal-install build
	cabal configure --enable-tests && cabal build && cabal test

lint:
	hlint .

.cabal-sandbox/bin/hlint: .cabal-sandbox/bin/happy

clean:
	find . -name "*~" -exec rm \{\} \;
	rm -rf dist

realclean: clean
	cabal sandbox delete

package: cabal-install haddock build

haddock: cabal-install
	cabal haddock

.cabal-sandbox/bin/%: cabal.sandbox.config
	cabal install $*

build: $(SOURCES)
	cabal build

cabal-install: cabal-sandbox
	cabal install --only-dependencies --enable-tests

cabal-sandbox: cabal.sandbox.config

.cabal-sandbox cabal.sandbox.config:
	cabal sandbox init
	cabal update

%.hs: %.x .cabal-sandbox/bin/alex
	.cabal-sandbox/bin/alex $<

%.hs: %.y .cabal-sandbox/bin/happy
	.cabal-sandbox/bin/happy $<
