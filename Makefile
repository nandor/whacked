
all:
	cabal update
	cabal install --only-dependencies --force-reinstalls
	cabal configure
	cabal build