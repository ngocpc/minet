minet <- function(dataset, method="mrnet", estimator="mi.empirical", disc="equalfreq",nbins=sqrt(nrow(dataset)) )
{
      net <- NULL
      if( estimator=="mi.empirical" ||
          estimator=="mi.shrink" ||
          estimator=="mi.mm" ||
          estimator=="mi.sg" )
          dataset <- discretize(dataset,disc,nbins)
      else #if( estimator!="gaussian")
          stop("unknown estimator")
      mim <- build.mim(dataset,estimator)
      if( method=="clr" )
            net <- clr(mim)
      else if( method=="mrnet")
            net <- mrnet(mim)
      else if( method=="aracne")
            net <- aracne(mim)
      else stop( "uknown method" )
      as.matrix(norm(net))
}
