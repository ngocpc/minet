/*
This file belong to
minet: Mutual Information NETworks, <http://minet.meyerp.com>
a package that implements various algorithms for inferring mutual information networks from data.

Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
<License full notice: at the root of the package 
and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 
*/



#include "minet.h"
SEXP aracne( SEXP Rmim, SEXP Rsize, SEXP Re )
{
      const int *size;
      const double *mim, *e;
      double *res, eps, eps1, eps2, eps3;
      int *tag;  
	  unsigned int n; 
      SEXP Rres, Rtag;
      PROTECT(Rmim = AS_NUMERIC(Rmim));
      PROTECT(Rsize= AS_INTEGER(Rsize));
      PROTECT(Re = AS_NUMERIC(Re) );
      mim = NUMERIC_POINTER(Rmim);
      size= INTEGER_POINTER(Rsize);
	  n = *size;
      e = NUMERIC_POINTER(Re);   
      PROTECT(Rres=NEW_NUMERIC(n*n));
      PROTECT(Rtag=NEW_INTEGER(n*n));
      res = NUMERIC_POINTER(Rres);
      tag = INTEGER_POINTER(Rtag);
      eps = *e;
      
      for( unsigned int i=0; i<n*n; ++i ) tag[i]=1;
      for( unsigned int i=0; i<n*n; ++i ) res[i]=0;      
      for(unsigned int i = 2; i < n ; ++i) 
         for(unsigned int j = 1; j < i; ++j) 
            for(unsigned int k = 0; k < j ; ++k) {
                  eps1 = mim[j*n+k] - mim[i*n+j];
                  eps2 = mim[i*n+k] - mim[i*n+j];
                  eps3 = mim[i*n+k] - mim[j*n+k];
				if(eps3 > 0) //(ik)>(jk)
					if(eps1 >0) { // (ik)>(jk)>(ij)
						if(eps1 > eps)
							tag[i*n+j]=tag[j*n+i]=0; 
						}
					else  //jk is minimum 
						if(eps2>0) {//(ik)>(ij)>(jk)
							if((-eps1) > eps)
								tag[j*n+k]=tag[k*n+j]=0; 
						}
						else { //(ij)>(ik)>(jk)
							if(eps3 > eps)
								tag[j*n+k]=tag[k*n+j]=0; 
						}
				else //(jk)>(ik)
					if(eps2 > 0) {//(jk)>(ik)>(ij)
						if(eps2 > eps)
							tag[i*n+j]=tag[j*n+i]=0; 
					}
					else //ik is minimum
						if(eps1>0){ //(jk)>(ij)>(ik)
							if((-eps2) > eps)
								tag[i*n+k]=tag[k*n+i]=0;
						}
						else //(ij)>(jk)>(ik)
							if((-eps3) > eps)
								tag[i*n+k]=tag[k*n+i]=0;
					
			}
      for(unsigned int i=0; i<n*n; ++i )
            if(tag[i]) res[i]=mim[i];
      UNPROTECT(5);
      return Rres;
}
