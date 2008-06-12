\name{discretize}
\alias{discretize}

\title{Unsupervized Data Discretization}
\usage{discretize( data,disc="equalfreq",nbins=sqrt(nrow(data)) )}
\arguments{
  \item{data}{ A data.frame containing data to be discretized. The columns contains variables and the rows samples.}
  \item{disc}{ The name of the discretization method to be used :"equalfreq" or "equalwidth" (default : "equalfreq") - see references.}
  \item{nbins}{ Integer specifying the number of bins to be used for the discretization. By default the number of bins is set to 
			  \eqn{\sqrt{N}}{sqrt(N)} where N is the number of samples.}
}
\value{\code{discretize} returns the discretized dataset.}
\description{
      \code{discretize} discretizes \code{data} using the equal frequencies or equal width binning algorithm. 
}
\references{
      Supervised and unsupervised discretization of continuous features. J.Dougherty, R. Kohavi, M. Sahami.
      ICML, 1995.
}
\author{
  Patrick E. Meyer, Frederic Lafitte, Gianluca Bontempi
}
\seealso{\code{\link{build.mim}}}
\examples{
data(syn.data)
ew.data <- discretize(syn.data,"equalwidth")
ef.data <- discretize(syn.data,"equalfreq")
}
\keyword{misc}
