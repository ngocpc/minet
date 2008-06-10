validate <- function( inet, tnet, steps=50 )
{
      if(max(tnet)!=1)
         stop("argument 'tnet' must contain values 0 or 1")
      if(!all( dim(inet)==dim(tnet) ))
         stop("networks must have the same size")
      if(!all( names(as.data.frame(inet))==names(as.data.frame(tnet))) )
         stop("networks must have the same node names")
      if(!is.matrix(inet)) 
         inet <- as.matrix(inet)
      if(!is.matrix(tnet))
         tnet <- as.matrix(tnet)
      if( !isSymmetric(inet) && isSymmetric(tnet)) {
         warning("infered network arcs will be consider as undirected edges")
         inet <- .Call("symmetrize",as.double(inet),ncol(inet),PACKAGE="minet")
		 dim(inet) <- c(ncol(tnet),ncol(tnet))
      }
      else if(isSymmetric(inet) && !isSymmetric(tnet)) {
         warning("true network arcs will be considerd as undirected edges")
         tnet <- .Call("symmetrize",as.double(tnet),ncol(tnet),PACKAGE="minet")
		 dim(tnet) <- c(ncol(inet),ncol(inet))
      }
      res <- .Call("validate",inet,tnet,min(inet),max(inet),ncol(inet),steps,PACKAGE="minet")

      dim(res) <- c(steps,5)
      res <- as.data.frame(res)
      names(res) <- c("thrsh","tp","fp","tn","fn")      
      res
}
