iSAX
====

Several versions of the code for Symbolic Aggregate approXimation and indexing. At the moment the R implementation only.

Build
!!!!!!

R package
---------

To build R package clone repository and execute
	R CMD Build iSAX
	R CMD Check iSAX_0.0-6.tar.gz
	R CMD Install iSAX_0.0-6.tar.gz

You should get package installed. To use it within R call:
	library(iSAXr)
	


