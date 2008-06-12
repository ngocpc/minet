\name{norm} 
\alias{norm}

\title{Data Normalization}
\usage{norm(x)}
\arguments{
   \item{x}{A data.frame or matrix or vector containing the data to be normalized. In this package the \code{norm} method is used to 
			normalize the network's weighted adjacency matrices.}
}
\value{
      \code{norm} returns the data.frame \code{x} with normalized values (i.e. values ranging from 0 to 1)
}
\description{
      Normalizes data \code{x} using the following code : \eqn{(x-\min(x))/(\max(x-min(x)))}{(x-min(x))/(\max(x-min(x)))}.
}
\examples{
data <- runif(100,-10,10)
ndata <- norm(data)
}
\keyword{misc}

