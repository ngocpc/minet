\name{aracne}
\alias{aracne}

\title{Algorithm for the Reconstruction of Accurate Cellular NEtworks}
\usage{aracne( mim, eps=0 )}
\arguments{
  \item{mim}{A square matrix whose i,j th element is the mutual information  
			between variables \eqn{Xi}{X_i} and \eqn{Xj}{X_j} - see \code{\link{build.mim}}.}
  \item{eps}{Numeric value indicating the threshold used when removing an edge : for each triplet of nodes (i,j,k), the 
			weakest edge, say (ij), is removed if its weight is below min\{(ik),(jk)\}-eps - see references. }
}
\value{
      \code{aracne} returns a matrix which is the weighted adjacency matrix of the network.
       In order to display the network, load the package Rgraphviz and use the following command: \cr
          plot( as( returned.matrix ,"graphNEL") )
}
\description{
	  This function takes the mutual information matrix as input in order 
	  to return the infered network according to the Aracne algorithm.
      This algorithm applies the data processing inequality to 
      all triplets of nodes in order to remove the least significant 
      edge in each triplet.
}
\details{
      The Aracne procedure starts by assigning to each pair of
      nodes a weight equal to their mutual information. Then, 
      the weakest edge of each triplet is interpreted as an indirect interaction and is
      removed if the difference between the two lowest weights is
      above a threshold \code{eps}.
}
\references{
 Adam A. Margolin, Ilya Nemenman, Katia Basso, Chris Wiggins, Gustavo
 Stolovitzky, Riccardo Dalla Favera, and Andrea Califano. Aracne : An 
 algorithm for the reconstruction of gene regulatory networks in a mammalian
 cellular context. BMC Bioinformatics, 2006.
}
\seealso{\code{\link{build.mim}},   \code{\link{clr}},    \code{\link{mrnet}}}
\examples{
data(syn.data)
mim <- build.mim(discretize(syn.data))
net <- aracne(mim)
}
\keyword{misc}
