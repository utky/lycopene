LYCOHOME = schema

.PHONY: prep submodules all quick bench clean veryclean install sdist init configure

all: configure
	cabal build

configure:
	cabal configure --enable-tests --enable-benchmarks

test: all
	cabal test

prof:
	cabal configure --disable-tests --enable-library-profiling --enable-executable-profiling && cabal build

deps: 
	cabal install --only-dependencies --enable-library-profiling --enable-tests --enable-benchmarks

init: prep

prep: submodules
	#(cabal --version || (cabal update && cabal install cabal-dev)) && 
	cabal sandbox init && \
	cabal update && \
	cabal install --only-dependencies --enable-library-profiling --enable-tests --enable-benchmarks

submodules:
	git submodule update --init

quick:
	cabal configure --enable-tests --disable-optimization && cabal build

relocatable:
	cabal configure -fembed_data_files && cabal build

bench:
	cabal configure --enable-benchmarks && cabal build

sdist:
	dist/setup/setup sdist
	# cabal sdist won't work, see https://github.com/haskell/cabal/issues/403

clean:
	cabal clean

veryclean: clean
	rm -rf dist .cabal-sandbox cabal.sandbox.config

install:
	cabal install --enable-tests
