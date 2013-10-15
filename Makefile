all: Benchmarks/sc13-benchmarks-application-info.csv
	make -C Haskell cabal
	make -C Haskell
	make -C Snap
	mkdir -p Bin
	cp Snap/examples/community/community Bin/
	chmod +x Community

Benchmarks/sc13-benchmarks-application-info.csv: Benchmarks/sc13-benchmarks-application.tgz
	tar -C Benchmarks -xvzf Benchmarks/sc13-benchmarks-application.tgz
	touch Benchmarks/sc13-benchmarks-application-info.csv

Benchmarks/sc13-benchmarks-application.tgz:
	wget -P Benchmarks http://www.satcompetition.org/2013/files/sc13-benchmarks-application.tgz