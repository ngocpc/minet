\name{minet}
\alias{minet}

\title{Mutual Information Network}
\usage{
minet(dataset, method="mrnet", estimator="spearman", disc="none", nbins=sqrt(NROW(dataset)))
}
\arguments{
  \item{dataset}{ data.frame where columns contain variables/features and rows contain outcomes/samples.}
  \item{method}{The name of the inference algorithm : "clr", "aracne", "mrnet" or "mrnetb" (default: "mrnet") - see references.}
  \item{estimator}{ The name of the entropy estimator to be used for mutual information computation: "mi.empirical", "mi.mm", "mi.shrink", "mi.sg", "spearman","kendall","pearson" (default: "spearman"). - see \code{\link{build.mim}}.}
  \item{disc}{ The name of the discretization method to be used, if required by the estimator :"none" ,"equalfreq", "equalwidth" or "globalequalwidth" (default : "none") - see infotheo package.}
  \item{nbins}{ Integer specifying the number of bins to be used for the discretization if disc is set properly. By default the number of bins is set to 
			  \eqn{\sqrt{N}}{sqrt(N)} where N is the number of samples.}
}
\value{
\code{minet} returns a matrix which is the weighted adjacency matrix of the network. The weights range from 0 to 1 and can be seen
      as a confidence measure on the presence of the arcs.  In order to display the network, load the package Rgraphviz and use the following command: 
      \cr plot( as(returned.matrix ,"graphNEL") )
}
\description{
  For a given dataset, \code{minet} infers the network in two steps. 
  First, the mutual information between all pairs of variables in \code{dataset} is computed
  according to the \code{estimator} argument. Then the algorithm given by \code{method} 
  considers the estimated mutual informations in order to build the network.
}
\author{
  Patrick E. Meyer, Frederic Lafitte, Gianluca Bontempi
}
\seealso{ \code{\link{build.mim}},  \code{\link{clr}},  \code{\link{mrnet}}, \code{\link{mrnetb}}, \code{\link{aracne}}  }
\references{
 Patrick E. Meyer, Frederic Lafitte, and Gianluca Bontempi. 
 minet: A R/Bioconductor Package for Inferring Large Transcriptional Networks Using Mutual Information. BMC Bioinformatics, Vol 9,
 2008.
}     
\examples{
data(syn.data)
net1 <- minet( syn.data )
net2 <- minet( syn.data, estimator="pearson" )
net3 <- minet( syn.data, method="clr", estimator="spearman" )
}
\keyword{misc}
