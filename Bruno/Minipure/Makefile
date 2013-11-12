all:
	mkdir -p binary
	cd code/simp; make rs
	cp ./code/simp/minisat_static ./binary/minipure
	
clean:
	rm binary/*
	cd code/simp; make clean