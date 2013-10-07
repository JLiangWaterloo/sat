all:
	make -c Haskell cabal
	make -c Haskell
	make -c Snap
	cp Snap/examples/community/community Bin/

satcomp: Benchmarks/sc13-benchmarks-application.tgz
	tar -xvzf Benchmarks/sc13-benchmarks-application.tgz

Benchmarks/sc13-benchmarks-application.tgz:
	wget -P Benchmarks http://www.satcompetition.org/2013/files/sc13-benchmarks-application.tgz
