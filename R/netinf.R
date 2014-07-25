#This file belong to
#minet: Mutual Information NETworks, <http://minet.meyerp.com>
#a package that implements various algorithms for inferring mutual information networks from data.

#Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
#<License full notice: at the root of the package 
#and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 

#Fixed in clr implementation September 13 by J.C.J. van Dam
#Calculation of the sd now divide by N-1 instead of N
#zi*zi -> zi as it is already squared
#1 Added option, skip diagonal for the mean and sd calculation

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
	  res <- .Call("mrnet", mim, nrow(mim), PACKAGE="minet")
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
	  res <- .Call("mrnetb", mim, nrow(mim), PACKAGE="minet")
	  dim(res) <- dim(mim)
	  res <- as.matrix(res)
	  rownames(res) <- var.id
	  colnames(res) <- var.id
	  res<-pmax(res,t(res))
	  res[res<0]<-0
	  res
}

clr<- function( mim,skipDiagonal=1)
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
  if(!isSymmetric(mim)) #added extra check to verify matrix symetric
    stop("Please enter a symetric matrix")
  res <- .Call( "clr", mim, nrow(mim),skipDiagonal,PACKAGE="minet" )
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
      res <- .Call("aracne", mim, nrow(mim),eps,PACKAGE="minet")
      dim(res) <- dim(mim)
      res <- as.matrix(res)
	  rownames(res) <- var.id
	  colnames(res) <- var.id
	  res
}
