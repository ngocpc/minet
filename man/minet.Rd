\name{minet}
\alias{minet}

\title{Mutual Information Network}
\usage{
minet(dataset, method="mrnet", estimator="mi.empirical", 
      disc="equalfreq", nbins=sqrt(nrow(dataset)) )
}
\arguments{
  \item{dataset}{ data.frame where columns contain variables/features and rows contain outcomes/samples.}
  \item{method}{The name of the inference algorithm : "clr", "aracne" or "mrnet" (default: "mrnet") - see references.}
  \item{estimator}{ The name of the mutual information estimator : "mi.empirical", "mi.mm", "mi.shrink" or "mi.sg"(default: "mi.empirical") - see \code{\link{build.mim}}.}
  \item{disc}{ The name of the discretization method to be used : "equalfreq" or "equalwidth" (default: "equalfreq") - see \code{\link{discretize}}.}
  \item{nbins}{ Integer giving the number of bins to be used in the discretization. }
}
\value{
\code{minet} returns a matrix which is the weighted adjacency matrix of the network. The weights range from 0 to 1 and can be seen
      as a confidence measure on the presence of the arcs.  In order to display the network, load the package Rgraphviz and use the following command: 
      \cr plot( as( returned.matrix ,"graphNEL") )
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
\seealso{ \code{\link{build.mim}},  \code{\link{clr}},  \code{\link{mrnet}},  \code{\link{aracne}}  }
\references{
 Patrick E. Meyer, Kevin Kontos, Frederic Lafitte, and Gianluca Bontempi. 
 Information-theoretic inference of large transcriptional regulatory
 networks. EURASIP Journal on Bioinformatics and Systems Biology,
 2007.    

 Adam A. Margolin, Ilya Nemenman, Katia Basso, Chris Wiggins, Gustavo
 Stolovitzky, Riccardo Dalla Favera, and Andrea Califano. Aracne : An 
 algorithm for the reconstruction of gene regulatory networks in a mammalian
 cellular context. BMC Bioinformatics, 2006.

 Jeremiah J. Faith, Boris Hayete, Joshua T. Thaden, Ilaria Mogno, Jamey
 Wierzbowski, Guillaume Cottarel, Simon Kasif, James J. Collins, and 
 Timothy S. Gardner. Large-scale mapping and validation of escherichia coli
 transcriptional regulation from a compendium of expression profiles. PLoS
 Biology, 2007.
}     
\examples{
data(syn.data)
net1 <- minet( syn.data )
net2 <- minet( syn.data, estimator="mi.shrink" )
net3 <- minet( syn.data, method="clr", estimator="mi.sg" )
}
\keyword{misc}
