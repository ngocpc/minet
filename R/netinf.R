mrnet <- function( mim )
{
      var.id<-NULL
      if(is.data.frame(mim)) {
            var.id <- names(mim)
            mim <- as.matrix(mim)
      }
      else if( is.matrix(mim) ) 
            var.id <- names(as.data.frame(mim))
      else  stop("Supply a matrix-like argument")      
      if(ncol(mim) != nrow(mim))
            stop("Argument matrix must be square")
	  res <- .Call("mrnet", mim, nrow(mim), DUP=FALSE, PACKAGE="minet")
	  dim(res) <- dim(mim)
	  res <- as.matrix(res)
	  rownames(res) <- var.id
	  colnames(res) <- var.id
	  res
}

mrnetb <- function( mim)
{
"method not yet available"
}

clr<- function( mim )
{
      var.id<-NULL
      if(is.data.frame(mim)) {
            var.id <- names(mim)
            mim <- as.matrix(mim)
      }
      else if( is.matrix(mim) ) 
            var.id <- names(as.data.frame(mim))
      else stop("Supply a matrix-like argument")      
      if(ncol(mim)!=nrow(mim))
          stop("Argument matrix must be square")
      res <- .Call( "clr", mim, nrow(mim), DUP=FALSE,PACKAGE="minet" )
      dim(res) <- dim(mim)
      res <- as.matrix(res)
	  rownames(res) <- var.id
	  colnames(res) <- var.id
	  res              
}

aracne <- function( mim, eps=0 )
{
      var.id<-NULL
      if(is.data.frame(mim)) {
            var.id <- names(mim)
            mim <- as.matrix(mim)
      }
      else if( is.matrix(mim) )
            var.id <- names(as.data.frame(mim))
      else stop("Supply a matrix-like argument")      
      if(ncol(mim) != nrow(mim))
          stop("Argument matrix must be square")
      res <- .Call("aracne", mim, nrow(mim),eps,DUP=FALSE,PACKAGE="minet")
      dim(res) <- dim(mim)
      res <- as.matrix(res)
	  rownames(res) <- var.id
	  colnames(res) <- var.id
	  res
}