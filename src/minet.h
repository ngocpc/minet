#include <iostream>
using namespace std;
#include <algorithm>
#include <ctime>
#include <cmath>
#include <vector>
#include <map>
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#define NA -2000000
/* These functions are defined in the mim.cpp file */

double digamma(double z);
double entropy_empirical(map< vector<double> ,int > frequencies, int nb_samples);
double entropy_dirichlet(map< vector<double> ,int > frequencies, int nb_samples, double beta);
double entropy_shrink2(map< vector<double> ,int > frequencies, int nb_samples);
double entropy_shrink(map< vector<double> ,int > frequencies, int nb_samples);
double minformation(const double* d, int N, int n, int i, int j, char c);
/* Entry points called from the R functions */
extern "C" 
{
SEXP discEF( SEXP data, SEXP nrows, SEXP ncols, SEXP nbins );
SEXP discEW( SEXP data, SEXP nrows, SEXP ncols, SEXP nbins );
SEXP buildMIMshrink(SEXP data, SEXP nrows, SEXP ncols);
SEXP buildMIMempirical(SEXP data, SEXP nrows, SEXP ncols);
SEXP buildMIMmillermadow(SEXP data, SEXP nrows, SEXP ncols);
SEXP buildMIMdirichlet(SEXP data, SEXP nrows, SEXP ncols);
SEXP aracne(SEXP mim, SEXP nbvar, SEXP e);
SEXP clr(SEXP mim, SEXP nbvar);
SEXP mrnet( SEXP mim, SEXP nbvar);
SEXP symmetrize( SEXP mat, SEXP size);
SEXP validate( SEXP inet,SEXP tnet,SEXP min,SEXP max,SEXP n,SEXP steps );
}

