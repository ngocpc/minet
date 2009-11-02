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
