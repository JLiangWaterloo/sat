minipure
========

Solver version: 1.0.1


This is an automatically generated README file by the EDACC system.
For further information, dependencies, licensing, warranty etc. about
the solver please consult the solver files.

Authors
=======

Hsiao-Lun Wang

Description
===========

pure literal detection embedded in minisat
Please also see the description PDF file.

Usage
=====

After extracting the binary.zip archive please make sure the solver files are executable
by setting the appropriate flag ("chmod -R +x files/"). Within the EDACC system the solver
was launched with the following command line arguments. <instance>, <seed> and <tempdir>,
if present, have to be replaced with the path to a problem instance, an integer random seed
and a temporary directory the solver can use for writing and reading files during execution.

Launch Command:  ././minipure  <instance>  -freq_pure=3
