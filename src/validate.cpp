#include "minet.h"
SEXP validate( SEXP Rinet,SEXP Rtnet,SEXP Rn,  SEXP Rsteps, SEXP Rthresh )
{
      enum x {THRSH,TP,FP,TN,FN};
      double *inet, *tnet, *thresh, *res;
      int *n, *steps, k;
      SEXP Rres;     
      PROTECT(Rinet = AS_NUMERIC(Rinet));
      PROTECT(Rtnet = AS_NUMERIC(Rtnet));
      PROTECT(Rthresh = AS_NUMERIC(Rthresh));
      PROTECT(Rn = AS_INTEGER(Rn));
	  PROTECT(Rsteps = AS_INTEGER(Rsteps));
      inet = NUMERIC_POINTER(Rinet);
      tnet = NUMERIC_POINTER(Rtnet);
      thresh = NUMERIC_POINTER(Rthresh);
      n = INTEGER_POINTER(Rn);
      steps = INTEGER_POINTER(Rsteps);
      PROTECT(Rres = NEW_NUMERIC(5*(*steps)));
      res = NUMERIC_POINTER(Rres);
      for( k=0; k<(*steps); k++ )
      {
            res[TP*(*steps)+k]=res[FP*(*steps)+k]=res[TN*(*steps)+k]=res[FN*(*steps)+k]=0;
            res[THRSH*(*steps)+k]=thresh[k];
            for( int i=0; i<(*n); ++i )
               for( int j=0; j<(*n); ++j )
                  if( (inet[i*(*n)+j] >= thresh[k]) == tnet[i*(*n)+j] )
                        ( inet[i*(*n)+j] >= thresh[k] )? ++res[TP*(*steps)+k] : ++res[TN*(*steps)+k];
                  else  ( inet[i*(*n)+j] >= thresh[k] )? ++res[FP*(*steps)+k] : ++res[FN*(*steps)+k];
      }
      UNPROTECT(6);
      return Rres;
}

