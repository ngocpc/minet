\name{mrnetb}
\alias{mrnetb}

\title{Maximum Relevance Minimum Redundancy Backward}
\usage{mrnetb(mim)}
\arguments{
  \item{mim}{ A square matrix whose i,j th element is the mutual information 
		 between variables \eqn{Xi}{X_i} and \eqn{Xj}{X_j} - see \code{\link{build.mim}}.}
   }
\value{\code{mrnetb} returns a matrix which is the weighted adjacency matrix of the network.
      In order to display the network, load the package Rgraphviz and use the following command: \cr
          plot( as( returned.matrix ,"graphNEL") )

}

\description{
  \code{mrnetb} takes the mutual information matrix as input in order to infer the network using 
	the maximum relevance/minimum redundancy criterion combined with a backward elimination and a sequential replacement - see references.
	This method is a variant of mrnet.
}

\references{
 Patrick E. Meyer, Daniel Marbach, Sushmita Roy and Manolis Kellis. 
 Information-Theoretic Inference of Gene Networks Using Backward Elimination. 
 The 2010 International Conference on Bioinformatics and Computational Biology. 
 
  
 Patrick E. Meyer, Kevin Kontos, Frederic Lafitte and Gianluca Bontempi. 
 Information-theoretic inference of large transcriptional regulatory
 networks. EURASIP Journal on Bioinformatics and Systems Biology, 2007.
}
\seealso{\code{\link{build.mim}},   \code{\link{clr}},  \code{\link{mrnet}}, \code{\link{aracne}}}
\examples{
data(syn.data)
mim <- build.mim(syn.data, estimator="spearman")
net <- mrnetb(mim)
}
\keyword{misc}
