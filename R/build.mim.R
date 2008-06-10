build.mim <- function( data, estimator = "empirical")
{
      n <- ncol(data)
      N <- nrow(data)
      var.id <- NULL
    if(is.data.frame(data)) 
         var.id <- names(data)            
    else if( is.matrix(data) )
          var.id <- names(as.data.frame(data))
    else stop("Supply a matrix-like argument")
      data <- data.matrix(data)
    if( !is.numeric(data) )
          stop("Supply numeric data")
    if( estimator != "gaussian" )
        if( !(all(data==round(data)) ))
	          stop("This estimator requires discrete values")                      
      data[which(is.na(data))] <- -2000000

      mim <- NULL
    if( estimator == "gaussian" )
          mim <- .Call( "buildMIMgaussian",data,N,n,
                        DUP=FALSE,PACKAGE="minet")     

    else if( estimator == "empirical")
          mim <- .Call( "buildMIMempirical",data,N,n,
                        DUP=FALSE,PACKAGE="minet")

    else if( estimator == "millermadow" )
          mim <- .Call( "buildMIMmillermadow",data,N,n,
                        DUP=FALSE,PACKAGE="minet")

    else if(estimator == "shrink")
          mim <- .Call( "buildMIMshrink",data,N,n,
                        DUP=FALSE,PACKAGE="minet")

    else if( estimator == "dirichlet.sg" )
          mim <- .Call( "buildMIMdirichlet",data,N,n,
                        DUP=FALSE,PACKAGE="minet")

    else stop("unknown estimator")

      dim(mim) <- c(n,n)
      mim <- as.data.frame(mim)
      names(mim) <- var.id
      row.names(mim) <- var.id
      as.matrix(mim)
}
