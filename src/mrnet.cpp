#include "minet.h"
SEXP mrnet( SEXP Rmim, SEXP Rsize )
{     
      const double *mim;
      const int* size;
      double *rel, *red, *res, score=1;
      int den=1, jmax=0;
      SEXP Rres,Rred,Rrel;
      PROTECT(Rmim = AS_NUMERIC(Rmim));
      PROTECT(Rsize= AS_INTEGER(Rsize));
      mim = NUMERIC_POINTER(Rmim);
      size = INTEGER_POINTER(Rsize);
      PROTECT(Rres = NEW_NUMERIC((*size)*(*size)));
      PROTECT(Rrel = NEW_NUMERIC(*size));
      PROTECT(Rred = NEW_NUMERIC(*size));      
      res = NUMERIC_POINTER(Rres);
      rel = NUMERIC_POINTER(Rrel);
      red = NUMERIC_POINTER(Rred);
      for( int i=0; i<(*size)*(*size); ++i ) res[i]=0;
      for( int i=0; i<(*size); ++i )
      {
            for( int j=0; j<(*size); ++j ) {
                  rel[j]=mim[i*(*size)+j];
                  red[j]=0;
            }
            for( int k=0; k<(*size)-1; k++ ) { 
                  for( int j=1; j<(*size); ++j ) {   
                         if( k==0 ) den=1;        
                         else den=k;
                         if( (rel[j]-red[j]/den) > rel[jmax]-red[jmax]/den ) 
                              jmax = j;
                  }      
                  score = rel[jmax]-red[jmax]/den;
                  if( res[jmax*(*size)+i] < score ) 
                        res[jmax*(*size)+i] = res[i*(*size)+jmax] = score;
                  rel[jmax]=NA; 
                  for( int l=0; l<(*size); ++l )  
                        red[l] += mim[l*(*size)+jmax];
                  if( score<0 ) k=(*size);
            }
      }
      UNPROTECT(5);
      return Rres;
}
