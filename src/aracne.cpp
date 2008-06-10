#include "minet.h"
SEXP aracne( SEXP Rmim, SEXP Rsize, SEXP Re )
{
      const int *size;
      const double *mim, *e;
      double *res, eps, eps1, eps2, eps3;
      int n, *tag;   
      SEXP Rres, Rtag;
      PROTECT(Rmim = AS_NUMERIC(Rmim));
      PROTECT(Rsize= AS_INTEGER(Rsize));
      PROTECT(Re = AS_NUMERIC(Re) );
      mim = NUMERIC_POINTER(Rmim);
      size= INTEGER_POINTER(Rsize);
      e = NUMERIC_POINTER(Re);   
      PROTECT(Rres=NEW_NUMERIC((*size)*(*size)));
      PROTECT(Rtag=NEW_INTEGER((*size)*(*size)));
      res = NUMERIC_POINTER(Rres);
      tag = INTEGER_POINTER(Rtag);
      eps = *e;
      n = *size;
      for( int i=0; i<n*n; ++i ) tag[i]=1;
      for( int i=0; i<(*size)*(*size); ++i ) res[i]=0;      
      for(int i = 2; i < n ; ++i) 
         for(int j = 1; j < i; ++j) 
            for(int k = 0; k < j ; ++k) {
                  eps1 = mim[j*n+k] - mim[i*n+j];
                  eps2 = mim[i*n+k] - mim[i*n+j];
                  eps3 = mim[i*n+k] - mim[j*n+k];
                  if( abs(eps1)>eps || abs(eps2)>eps || abs(eps3)>eps ) { 
                        if((eps1 > eps) and (eps2 > eps)) // if (ij) minimum tag (ij) for elimination
                             tag[i*n+j]=tag[j*n+i]=0; 
                        else if(eps3 > eps) // if a_ij is not minimal and a_ik neither then tag a_jk
                             tag[j*n+k]=tag[k*n+j]=0;
                        else
                             tag[i*n+k]=tag[k*n+i]=0;
                  }
            }
      for( int i=0; i<n*n; ++i )
            if(tag[i]) res[i]=mim[i];
      UNPROTECT(5);
      return Rres;
}
