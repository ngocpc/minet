build.mim <- function( dataset, estimator = "spearman", disc = "none", nbins = sqrt(NROW(dataset)))
{
	if( disc == "equalfreq" || disc == "equalwidth" || disc == "globalequalwidth")
				dataset<-discretize(dataset, disc, nbins)
	if( estimator=="spearman" || estimator=="pearson" || estimator=="kendall") {
		  mim<-cor(dataset,method=estimator,use="complete.obs")^2
		  diag(mim)<-0
		  maxi<-0.999999
		  mim[which(mim>maxi)]<-maxi
		  mim <--0.5*log(1-mim)
	}
	
	else if(estimator == "mi.mm")
		estimator = "mm"
	else if(estimator == "mi.empirical")
		estimator = "emp"
	else if(estimator == "mi.sg")
		estimator = "sg"
	else if (estimator == "mi.shrink")
		estimator = "shrink"
	else
          stop("unknown estimator")
		  
	if( estimator=="mm" || estimator=="emp" || estimator=="sg" || estimator=="shrink") {
		   mim <-mutinformation(dataset,method=estimator)
		   diag(mim) <- 0
	}
	mim
}
