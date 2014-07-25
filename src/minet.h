/*
This file belong to
minet: Mutual Information NETworks, <http://minet.meyerp.com>
a package that implements various algorithms for inferring mutual information networks from data.

Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
<License full notice: at the root of the package 
and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 
*/

#include <iostream>
//using namespace std;
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
SEXP clr(SEXP mim, SEXP nbvar,SEXP skipDiagonal);
SEXP mrnet( SEXP mim, SEXP nbvar);
SEXP mrnetb( SEXP mim, SEXP nbvar);
SEXP validate( SEXP inet,SEXP tnet,SEXP n);
}

