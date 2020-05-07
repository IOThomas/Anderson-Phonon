Generation of test routines
============================= 

Code is generated from the files in `test_fun` using 
[funit](https://github.com/kleb/nasarb/tree/master/funit) plus a few necessary 
manual modifications such that it works (see `NOTES.md` in `test_fun` for more 
information). The resulting test suites are stored in `test_code` in directories 
corresponding to the relevent module.

In order to keep the code generated from the .fun files comprehensible, useful 
functions and subroutines (eg generation of test values) may be refactored out to 
an additional file (stored in the relevent test_code directory) with a name of 
the form ${FOO}_test_routines.
