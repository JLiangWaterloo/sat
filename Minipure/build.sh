#!/bin/bash
mkdir binary
cd code
cd simp
gmake rs
cd ..
cd ..
cp ./code/simp/minisat_static ./binary/minipure
