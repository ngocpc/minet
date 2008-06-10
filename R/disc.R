disc <- function( data, disc.method="equalfreq", nbins=sqrt(nrow(data)) )
{
      if(!is.data.frame(data))
            data <- as.data.frame(data)
      varnames <- names(data)
      dimensions <- dim(data)
      data <- data.matrix(data)
      if(!is.numeric(data))
            stop("Supply numeric data")
      data[which(is.na(data))] <- -2000000
      dim(data) <- dimensions
      res <- NULL
      if( disc.method=="equalfreq" )
            res <- .Call("discEF",data,nrow(data),ncol(data),
                          as.integer(nbins),DUP=FALSE, PACKAGE="minet")
      else if( disc.method=="equalwidth" )
            res <- .Call("discEW",data,nrow(data),ncol(data),
                          as.integer(nbins),DUP=FALSE, PACKAGE="minet")
      else stop("unknown discretization method")  
      dim(res) <- dimensions
      res <- as.data.frame(res)
      names(res) <- varnames
      res
}
