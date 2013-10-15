OPT = -Wall -O2

all: Average Bcp Cmty Cnf Graph Group Horn HornCount Maximum Minimum Shuffle Subgroup Transpose

cabal:
	cabal update
	cabal install random-shuffle
	cabal install split
	cabal install containers
	cabal install MonadRandom
	cabal install monad-supply

Average: Average.hs
	ghc $(OPT) Average.hs

Bcp: Bcp.hs Sat.hs
	ghc $(OPT) Bcp.hs

Cmty: Cmty.hs Sat.hs
	ghc $(OPT) Cmty.hs

Cnf: Cnf.hs Sat.hs
	ghc $(OPT) Cnf.hs

Graph: Graph.hs Sat.hs
	ghc $(OPT) Graph.hs

Group: Group.hs
	ghc $(OPT) Group.hs

Horn: Horn.hs Sat.hs
	ghc $(OPT) Horn.hs

HornCount: HornCount.hs Sat.hs
	ghc $(OPT) HornCount.hs

Maximum: Maximum.hs
	ghc $(OPT) Maximum.hs

Minimum: Minimum.hs
	ghc $(OPT) Minimum.hs

Shuffle: Shuffle.hs Sat.hs
	ghc $(OPT) Shuffle.hs

Subgroup: Subgroup.hs Sat.hs
	ghc $(OPT) Subgroup.hs

Transpose: Transpose.hs Sat.hs
	ghc $(OPT) Transpose.hs

clean:
	rm *.o *.hi Average Bcp Cmty Cnf Graph Group Horn HornCount Maximum Minimum Shuffle Subgroup Transpose
