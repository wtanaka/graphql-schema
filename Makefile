SOURCES=$(shell find src -name "*.hs")

all: lint test package

test: cabal-install cabal-configure build
	cabal test

lint:
	hlint .

.cabal-sandbox/bin/hlint: .cabal-sandbox/bin/happy

clean:
	find . \( -name "*~" \
		-o -name "*.pyc" \
		\) -exec rm \{\} \;
	rm -rf dist

realclean: clean
	cabal sandbox delete
	rm -rf alex-sandbox

package: cabal-install haddock build

haddock: cabal-install cabal-configure
	cabal haddock

.cabal-sandbox/bin/%: cabal.sandbox.config
	cabal install $*

build: $(SOURCES)
	cabal build

cabal-configure:
	cabal configure --enable-tests

cabal-install: cabal-sandbox alex-sandbox/.cabal-sandbox/bin/alex
	env PATH="alex-sandbox/.cabal-sandbox/bin:$$PATH" cabal install --only-dependencies --enable-tests

cabal-sandbox: cabal.sandbox.config

.cabal-sandbox cabal.sandbox.config:
	cabal sandbox init
	cabal update

alex-sandbox/cabal.sandbox.config:
	mkdir alex-sandbox
	(cd alex-sandbox; cabal sandbox init; cabal update)

alex-sandbox/.cabal-sandbox/bin/%: alex-sandbox/cabal.sandbox.config
	(cd alex-sandbox; cabal install $*)

%.hs: %.x alex-sandbox/.cabal-sandbox/bin/alex
	alex-sandbox/.cabal-sandbox/bin/alex $<

%.hs: %.y
	happy $<
