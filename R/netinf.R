#This file belong to
#minet: Mutual Information NETworks, <http://minet.meyerp.com>
#a package that implements various algorithms for inferring mutual information networks from data.

#Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
#<License full notice: at the root of the package 
#and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 


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
	  res <- .Call("mrnetb", mim, nrow(mim), DUP=FALSE, PACKAGE="minet")
	  dim(res) <- dim(mim)
	  res <- as.matrix(res)
	  rownames(res) <- var.id
	  colnames(res) <- var.id
	  res<-pmax(res,t(res))
	  res[res<0]<-0
	  res
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