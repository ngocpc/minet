build.mim <- function( data, estimator = "mi.empirical")
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
    #if( !is.numeric(data) )
          #stop("Supply numeric data")
    #if( !(all(data==round(data)) ))
	      #stop("This estimator requires discrete values")                      
      data[which(is.na(data))] <- -2000000

      res <- NULL 

    if( estimator == "mi.empirical")
          res <- .Call( "buildMIMempirical",data,N,n,
                        DUP=FALSE,PACKAGE="minet")

    else if( estimator == "mi.mm" )
          res <- .Call( "buildMIMmillermadow",data,N,n,
                        DUP=FALSE,PACKAGE="minet")

    else if(estimator == "mi.shrink")
          res <- .Call( "buildMIMshrink",data,N,n,
                        DUP=FALSE,PACKAGE="minet")

    else if( estimator == "mi.sg" )
          res <- .Call( "buildMIMdirichlet",data,N,n,
                        DUP=FALSE,PACKAGE="minet")

    else stop("unknown estimator")

      dim(res) <- c(n,n)
      res <- as.data.frame(res)
      names(res) <- var.id
      row.names(res) <- var.id
      as.matrix(res)
}
