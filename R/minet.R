
#This file belong to
#minet: Mutual Information NETworks, <http://minet.meyerp.com>
#a package that implements various algorithms for inferring mutual information networks from data.

#Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
#<License full notice: at the root of the package 
#and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 

minet <- function(dataset, method="mrnet", estimator="spearman", disc="none", nbins=sqrt(NROW(dataset)))
{
      net <- NULL
      mim<-build.mim(dataset,estimator,disc,nbins)
      
      if( method=="clr" )
            net <- clr(mim)
      else if( method=="mrnet")
            net <- mrnet(mim)
	  else if( method=="mrnetb")
            net <- mrnetb(mim)
      else if( method=="aracne")
            net <- aracne(mim)
      else stop( "uknown method" )
	  
      net/max(net) #normalization
}
