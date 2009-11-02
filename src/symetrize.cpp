#include "minet.h"
SEXP symetrize( SEXP Rmat, SEXP Rsize )
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
      for( int i=0; i<(*n); ++i )
        for( int j=0; j<=i; ++j )
            if( mat[i*(*n)+j] > mat[j*(*n)+i])
                  res[i*(*n)+j] = res[j*(*n)+i] = mat[i*(*n)+j];
			else
				  res[i*(*n)+j] = res[j*(*n)+i] = mat[j*(*n)+i];
      UNPROTECT(3);
      return Rres;
}

