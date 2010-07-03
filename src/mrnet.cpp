/*
This file belong to
minet: Mutual Information NETworks, <http://minet.meyerp.com>
a package that implements various algorithms for inferring mutual information networks from data.

Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
<License full notice: at the root of the package 
and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 
*/

#include "minet.h"

SEXP mrnetb( SEXP Rmim, SEXP Rsize )
{     
      const double *mim;
      const int* size;
      double *rel, *red, *res, fact=1, *sum;
	  unsigned int n, worst, best, k,temp;
	  
      SEXP Rres,Rred,Rrel,Rsum;
      PROTECT(Rmim = AS_NUMERIC(Rmim));
      PROTECT(Rsize= AS_INTEGER(Rsize));
      mim = NUMERIC_POINTER(Rmim);
      size = INTEGER_POINTER(Rsize);
	  n = *size;
      PROTECT(Rres = NEW_NUMERIC(n*n));
      PROTECT(Rrel = NEW_NUMERIC(n));
      PROTECT(Rred = NEW_NUMERIC(n));      
	  PROTECT(Rsum = NEW_NUMERIC(n));
	  
      res = NUMERIC_POINTER(Rres);
      rel = NUMERIC_POINTER(Rrel);
      red = NUMERIC_POINTER(Rred);
	  sum = NUMERIC_POINTER(Rsum);
	  
	  //pre-initialisation
	  const int eliminated = 10000;
	  for( unsigned int i=0; i< n; ++i ) {
			sum[i]=0;
			for( unsigned int j=0; j<n; ++j ) {
				res[i*n+j]=0;
				sum[i] += mim[j*n+i];
	  		}
	  }
	  
	  //for each variable
	  for(unsigned int j=0; j<n; ++j ) {
	  	 
	  	 //initialisation selection
	  	 k=0; 
	  	 for( unsigned int i=0; i< n; ++i ) {
	  	 	rel[i] = mim[i*n+j];
	  	 	red[i] = sum[i] - mim[j*n+i];
	  	 	k++;
	     }
	     
	     //select worst
	     worst = 0;
	     for( unsigned int i=1; i< n; ++i ) 
	  	 	if( (rel[i]-fact*red[i]/k) < (rel[worst]-fact*red[worst]/k) )
	  	 		worst = i;
	  	 best=worst;
	  	 
	  	 //backward elimination
	   	 while( (k>1) && (rel[worst]-fact*red[worst]/k) < 0 ) {
	   		
	   		//eliminate worst
	   		rel[worst] = eliminated;
	   		--k;
	   		
	   			
	   		//update scores
	   		for( unsigned int i=0; i< n; ++i )  
	   		   red[i] -= mim[worst*n+i];
	   		  
	   		//select worst
	   		worst = 0;
	     	for( unsigned int i=1; i< n; ++i ) 
	  	 		if( (rel[i]-fact*red[i]/k) < (rel[worst]-fact*red[worst]/k) )
	  	 			worst=i;
	   	 }
	   	 
		 
	   	 //sequential replacement
	   	 for( unsigned int i=0; i< n; ++i ) 
	     	if(rel[i]==eliminated) {
	  	 		if( (mim[i*n+j]-fact*red[i]/k) > (mim[best*n+j]-fact*red[best]/k) )
	  	 			best=i;
	  	 		}
				
		 bool ok = true;
	   	 while(ok){// (mim[best*n+j]-fact*red[best]/k) > (mim[worst*n+j]-fact*red[worst]/k) ) {
	   		
	   		//swap
	   		rel[best] = mim[best*n+j];
	   		rel[worst] = eliminated;
	   		for( unsigned int i=0; i< n; ++i )  
	   		   red[i] += mim[best*n+i]-mim[worst*n+i];
	   		temp = best;
	   		best = worst;
	   		
	   		//select worst
	   		worst = temp;
			ok = false;
	     	for( unsigned int i=0; i< n; ++i ) 
	     		if(rel[i] != eliminated) {
	  	 			if( (rel[i]-fact*red[i]/k) < (rel[worst]-fact*red[worst]/k) ) {
	  	 				worst=i;
						ok = true;
					}
	  	 		}
	  	 		else
	  	 			if( (mim[i*n+j]-fact*red[i]/k) > (mim[best*n+j]-fact*red[best]/k) ) {
	  	 				best=i;
						ok = true;
					}
				    	  	 			
	   	 }
	   	 
	   	 //set scores
	   	 for( unsigned int i=0; i< n; ++i )
	   	 	if(rel[i] == eliminated)
	   	 		res[i*n+j] = 0;
	   	 	else
	   	 		res[i*n+j] = rel[i]-fact*red[i]/k;
	  }
	     	    
      UNPROTECT(6);
      return Rres;     
}

SEXP mrnet( SEXP Rmim, SEXP Rsize )
{     
      const double *mim;
      const int* size;
      double *rel, *red, *res, score=1;
	  unsigned int n, jmax=0;
      SEXP Rres,Rred,Rrel;
      PROTECT(Rmim = AS_NUMERIC(Rmim));
      PROTECT(Rsize= AS_INTEGER(Rsize));
      mim = NUMERIC_POINTER(Rmim);
      size = INTEGER_POINTER(Rsize);
	  n = *size;
      PROTECT(Rres = NEW_NUMERIC(n*n));
      PROTECT(Rrel = NEW_NUMERIC(n));
      PROTECT(Rred = NEW_NUMERIC(n));      
	 
      res = NUMERIC_POINTER(Rres);
      rel = NUMERIC_POINTER(Rrel);
      red = NUMERIC_POINTER(Rred);
	
	  
        for( unsigned int i=0; i< n; ++i ) 
			for( unsigned int j=0; j<n; ++j ) 
				res[i*n+j]=0;

		for(unsigned int i=0; i<n; ++i ) {
			//init rel and red and select first
            for( unsigned int j=0; j<n; ++j ) {
                  rel[j]= mim[i*n+j];
				  red[j]=0;
				  if( rel[j] > rel[jmax])
                    jmax = j;
            }
			
			score = rel[jmax];
			if( res[i*n+jmax] < score ) {
				res[jmax*n+i] = score;
				res[i*n+jmax] = score;
			}
			rel[jmax]=-1000; 
			for(unsigned int l=0; l<n; ++l )  
				red[l] += mim[l*n+jmax];
			
			//select others
            for(unsigned int k=1; k < n-1; k++ ) { 
				  jmax = 0;
                  for(unsigned int j=1; j < n; ++j ) {   
                         if( (rel[j] - red[j]/k) > (rel[jmax] - red[jmax]/k) ) 
                              jmax = j;
                  }      
                  score = (rel[jmax] - (red[jmax]/k));
                  if( res[i*n+jmax] < score ) {
                        res[jmax*n+i] = score;
						res[i*n+jmax] = score;
				  }
					                  
				  //update rel and red
				  rel[jmax]=-1000; 
                  for( int l=0; l<n; ++l )  
                        red[l] += mim[l*n+jmax];
						
				  // stop criterion
                  if( score < 0 ) k=n;
            }
      }
      UNPROTECT(5);
      return Rres;
}

