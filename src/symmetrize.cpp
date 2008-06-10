#include "minet.h"
SEXP symmetrize( SEXP Rmat, SEXP Rsize )
{
      int* n;
      double *mat, *res;
      SEXP Rres;
      PROTECT(Rmat = AS_NUMERIC(Rmat));
      PROTECT(Rsize= AS_INTEGER(Rsize));
      mat = NUMERIC_POINTER(Rmat);
      n = INTEGER_POINTER(Rsize);
      PROTECT(Rres=NEW_NUMERIC((*n)*(*n)));
      res = NUMERIC_POINTER(Rres);
      for( int i=0; i<(*n)*(*n); ++i ) res[i]=0;
      for( int i=0; i<(*n); ++i )
        for( int j=0; j<(*n); ++j )
            if( mat[i*(*n)+j]!=0 || mat[j*(*n)+i]!=0 )
                  res[i*(*n)+j]=res[j*(*n)+i]=1;
      UNPROTECT(3);
      return Rres;
}

