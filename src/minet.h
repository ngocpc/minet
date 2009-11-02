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

/* Entry points called from the R functions */
extern "C" 
{
SEXP aracne(SEXP mim, SEXP nbvar, SEXP e);
SEXP clr(SEXP mim, SEXP nbvar);
SEXP mrnet( SEXP mim, SEXP nbvar);
SEXP mrnetb( SEXP mim, SEXP nbvar);
SEXP symetrize( SEXP mat, SEXP size );
SEXP validate( SEXP inet,SEXP tnet,SEXP n,SEXP steps, SEXP thresh );
}

