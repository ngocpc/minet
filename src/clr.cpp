#include "minet.h"
SEXP clr( SEXP Rmim, SEXP Rsize )
{
      const double *mim;
      const int *size;
      double *res, *avg, *var, tmp, zi, zj;
      unsigned int n;
      SEXP Rres, Ravg, Rvar;
      PROTECT(Rmim = AS_NUMERIC(Rmim));
      PROTECT(Rsize= AS_INTEGER(Rsize));
      mim = NUMERIC_POINTER(Rmim);
      size= INTEGER_POINTER(Rsize);
	  n=*size;
      PROTECT(Rres=NEW_NUMERIC(n*n));
      PROTECT(Ravg=NEW_NUMERIC(n));
      PROTECT(Rvar=NEW_NUMERIC(n));
      res = NUMERIC_POINTER(Rres);
      avg = NUMERIC_POINTER(Ravg);
      var = NUMERIC_POINTER(Rvar);
      
      for( unsigned int i=0; i<n*n; ++i ) res[i]=0;
      //compute mean and variance
      for(unsigned int i = 0; i < n; ++i) 
      {
         avg[i]=0;
         for(unsigned int j = 0; j < n; ++j)
               avg[i] += mim[i*n+j];   
         avg[i] /= n;
         var[i]=0;
         for(unsigned int j = 0; j < n; ++j) 
         {
            tmp = (mim[i*n+j]-avg[i]); 
            var[i] += tmp*tmp;
         }
         var[i] /= n;
      }
      //build network
      for(unsigned int i=1; i<n; ++i) 
         for(unsigned int j=0; j<i; ++j)
         {
	      tmp = (mim[i*n+j] - avg[i]);
		if( tmp<0 ) zi = 0;
		else zi = tmp*tmp/var[i];
		tmp = (mim[i*n+j] - avg[j]);
		if( tmp<0 ) zj = 0;
		else zj = tmp*tmp/var[j];			
		res[i*n+j] = sqrt(zi*zi+zj*zj);
		res[j*n+i] = sqrt(zi*zi+zj*zj);
	   }
      UNPROTECT(5);
      return Rres;
}