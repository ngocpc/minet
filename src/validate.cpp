#include "minet.h"
SEXP validate( SEXP Rinet,SEXP Rtnet,SEXP Rmin,SEXP Rmax,SEXP Rn,SEXP Rsteps )
{
      enum x {THRSH,TP,FP,TN,FN};
      double *inet, *tnet, *min, *max, *res, t, stepsize;
      int *n, *steps, k;
      SEXP Rres;     
      PROTECT(Rinet=AS_NUMERIC(Rinet));
      PROTECT(Rtnet=AS_NUMERIC(Rtnet));
      PROTECT(Rmin =AS_NUMERIC(Rmin));
      PROTECT(Rmax =AS_NUMERIC(Rmax));
      PROTECT(Rn = AS_INTEGER(Rn));
      PROTECT(Rsteps=AS_INTEGER(Rsteps));
      inet = NUMERIC_POINTER(Rinet);
      tnet = NUMERIC_POINTER(Rtnet);
      min = NUMERIC_POINTER(Rmin);
      max = NUMERIC_POINTER(Rmax);
      n = INTEGER_POINTER(Rn);
      steps=INTEGER_POINTER(Rsteps);
      PROTECT(Rres = NEW_NUMERIC(5*(*steps)));
      res = NUMERIC_POINTER(Rres);
      stepsize = ((*max)-(*min))/(*steps);
      for( t=(*min), k=0; t<(*max) && k<(*steps); t+=stepsize, k++ )
      {
            res[TP*(*steps)+k]=res[FP*(*steps)+k]=res[TN*(*steps)+k]=res[FN*(*steps)+k]=0;
            res[THRSH*(*steps)+k]=t;
            for( int i=0; i<(*n); ++i )
               for( int j=0; j<(*n); ++j )
                  if( (inet[i*(*n)+j] >= t) == tnet[i*(*n)+j] )
                        ( inet[i*(*n)+j] >= t )? ++res[TP*(*steps)+k] : ++res[TN*(*steps)+k];
                  else  ( inet[i*(*n)+j] >= t )? ++res[FP*(*steps)+k] : ++res[FN*(*steps)+k];
      }
      UNPROTECT(7);
      return Rres;
}

