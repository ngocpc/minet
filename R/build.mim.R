
#This file belong to
#minet: Mutual Information NETworks, <http://minet.meyerp.com>
#a package that implements various algorithms for inferring mutual information networks from data.

#Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
#<License full notice: at the root of the package 
#and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 

    
build.mim <- function( dataset, estimator = "spearman", disc = "none", nbins = sqrt(NROW(dataset)))
{
	if( disc == "equalfreq" || disc == "equalwidth" || disc == "globalequalwidth")
				dataset<-infotheo::discretize(dataset, disc, nbins)
	if( estimator=="pearson" || estimator=="spearman" || estimator=="kendall") {
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
		   mim <-infotheo::mutinformation(dataset,method=estimator)
		   diag(mim) <- 0
	}
	mim[mim<0]<-0
	mim
}
