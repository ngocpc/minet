#This file belong to
#minet: Mutual Information NETworks, <http://minet.meyerp.com>
#a package that implements various algorithms for inferring mutual information networks from data.

#Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
#<License full notice: at the root of the package 
#and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 


validate <- function(inet,tnet) {

      if(any(tnet!=0 & tnet!=1))
         stop("argument 'tnet' must contain values 0 or 1")
      if(!all( dim(inet)==dim(tnet) ))
         stop("networks must have the same size")
      if(!all( names(as.data.frame(inet))==names(as.data.frame(tnet))) )
         warning("networks have not the same node names")
      if(!is.matrix(inet)) 
		 stop("infered network should be a matrix")
      if(!is.matrix(tnet))
         stop("true network should be a matrix")
      if( !isSymmetric(inet) && isSymmetric(tnet)) {
         #warning("infered network is directed but true network is undirected")
		 warning("infered network arcs will be consider as undirected edges")
		 inet <- pmax(inet,t(inet)) 
		 dim(inet) <- c(ncol(tnet),ncol(tnet))
      }
      else if(isSymmetric(inet) && !isSymmetric(tnet)) {
         #warning("infered network is undirected but true network is directed")
		 warning("true network arcs will be considerd as undirected edges")
         tnet <- pmax(tnet,t(tnet))
		 dim(tnet) <- c(ncol(inet),ncol(inet))
      }
      res <- .Call("validate",inet,tnet, NCOL(inet), PACKAGE="minet")

      dim(res) <- c(length(res)/5,5)
      res <- as.data.frame(res)
      names(res) <- c("thrsh","tp","fp","fn","tn")      
      res
}

