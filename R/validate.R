validate <- function( inet, tnet, steps=50, thresholds = pretty(inet,steps) ) {
#thresholds = seq(range(inet)[1],range(inet)[2],(range(inet)[2]-range(inet)[1])/steps)) 

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
		 inet <- .Call("symetrize",as.double(inet),ncol(inet),PACKAGE="minet")
		 dim(inet) <- c(ncol(tnet),ncol(tnet))
      }
      else if(isSymmetric(inet) && !isSymmetric(tnet)) {
         #warning("infered network is undirected but true network is directed")
		 warning("true network arcs will be considerd as undirected edges")
         tnet <- .Call("symetrize",as.double(tnet),ncol(tnet),PACKAGE="minet")
		 dim(tnet) <- c(ncol(inet),ncol(inet))
      }
      res <- .Call("validate",inet,tnet, ncol(inet), length(thresholds), thresholds, PACKAGE="minet")

      dim(res) <- c(length(thresholds),5)
      res <- as.data.frame(res)
      names(res) <- c("thrsh","tp","fp","tn","fn")      
      res
}
