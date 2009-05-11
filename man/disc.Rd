\name{discretize}
\alias{discretize}

\title{Unsupervized Data Discretization}
\usage{discretize( data,disc="equalfreq",nbins=sqrt(nrow(data)) )}
\arguments{
  \item{data}{ A data.frame containing data to be discretized. The columns contains variables and the rows samples.}
  \item{disc}{ The name of the discretization method to be used :"equalfreq", "equalwidth" or "globalequalwidth" (default : "equalfreq") - see references.}
  \item{nbins}{ Integer specifying the number of bins to be used for the discretization. By default the number of bins is set to 
			  \eqn{\sqrt{N}}{sqrt(N)} where N is the number of samples.}
}
\value{\code{discretize} returns the discretized dataset.}
\description{
      \code{discretize} discretizes \code{data} using the equal frequencies or equal width binning algorithm. 
      "equalwidth" and "equalfreq" discretizes each random variable (each column) of the data into \code{nbins}. 
      "globalequalwidth" discretizes the range of the random vector \code{data} into \code{nbins}. 
}
\references{
      Supervised and unsupervised discretization of continuous features. J.Dougherty, R. Kohavi, M. Sahami.
      ICML, 1995.
}
\author{
  Patrick E. Meyer, Frederic Lafitte, Gianluca Bontempi, Korbinian Strimmer
}
\seealso{\code{\link{build.mim}}}
\examples{
data(syn.data)
ew.data <- discretize(syn.data,"equalwidth")
ef.data <- discretize(syn.data,"equalfreq")
gew.data <- discretize(syn.data,"globalequalwidth")
}
\keyword{misc}
