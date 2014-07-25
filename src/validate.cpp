/*
This file belong to
minet: Mutual Information NETworks, <http://minet.meyerp.com>
a package that implements various algorithms for inferring mutual information networks from data.

Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
<License full notice: at the root of the package 
and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 
Last update july 2014
*/

#include "minet.h"
SEXP validate( SEXP Rinet,SEXP Rtnet,SEXP Rn )
{
      enum x {THR,TP,FP,FN,TN};
      double *inet, *tnet;
      double *res;
      int *n, k;
      SEXP Rres;     
      PROTECT(Rinet = AS_NUMERIC(Rinet));
      PROTECT(Rtnet = AS_NUMERIC(Rtnet));
      PROTECT(Rn = AS_INTEGER(Rn));
      inet = NUMERIC_POINTER(Rinet);
      tnet = NUMERIC_POINTER(Rtnet);
      n = INTEGER_POINTER(Rn);
      
		int TPs=0, FPs=0;
		std::multimap<double,int> m;
		for(int i = 0; i <((*n)*(*n)); ++i) {
			m.insert(std::make_pair(inet[i],i));
			if(tnet[i]==1)
				++TPs;
			else
				++FPs;
		}
		int count = 1+((*n)*(*n));//1;
		std::multimap< double ,int>::const_iterator iter=m.begin(); 
		
		/*while(iter!=m.end()) {
			++count;
			iter=m.upper_bound(iter->first);
		}*/
		int col=5;
		PROTECT(Rres = NEW_NUMERIC(col*count));
		res = NUMERIC_POINTER(Rres);
		k= count-1;
		res[THR*count+k]=0;
		res[TP*count+k]=TPs;
		res[FP*count+k]=FPs;
		res[FN*count+k]=0;
		res[TN*count+k]=0;
		--k;
		iter=m.begin();
		res[THR*count+k] = iter->first;
		res[TP*count+k]=res[TP*count+k+1];
		res[FP*count+k]=res[FP*count+k+1];
		res[FN*count+k]=res[FN*count+k+1];
		res[TN*count+k]=res[TN*count+k+1];
		++iter;
		for(std::multimap< double ,int>::const_iterator iter2=m.begin(); iter!=m.end(); ++iter2,++iter) {
			if(tnet[iter2->second]==1){
		 		--res[TP*count+k];
		 		++res[FN*count+k];
		 	}
		 	else {
		 	   	--res[FP*count+k];
		 		++res[TN*count+k];
		 	}
		 	//if(iter!=m.end()) && iter2->first!=iter->first) 
			--k;
			res[THR*count+k] = iter->first;
			res[TP*count+k]=res[TP*count+k+1];
			res[FP*count+k]=res[FP*count+k+1];
			res[FN*count+k]=res[FN*count+k+1];
			res[TN*count+k]=res[TN*count+k+1];
		}
		 
	UNPROTECT(4);
    return Rres;
}	
       

