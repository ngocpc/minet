\name{build.mim}
\alias{build.mim}

\title{Build Mutual Information Matrix}
\usage{build.mim(data, estimator="mi.empirical")}
\arguments{
  \item{data}{data.frame containing gene expression data or any dataset where columns contain variables/features and rows contain outcomes/samples.}
  \item{estimator}{The name of the mutual information estimator. The package implements four estimators : 
			    "mi.empirical", "mi.mm", "mi.shrink", "mi.sg" (default:"mi.empirical") - see details. 
               These estimators require discrete data values - see \code{\link{discretize}}.}
}
\value{ \code{build.mim} returns the mutual information matrix.}
\description{
	  \code{build.mim} takes the dataset as input and computes the 
	  mutual information beetween all pair of variables according 
	  to the mutual inforamtion estimator \code{estimator}. 
	  The results are saved in the mutual information matrix (MIM), a square 
      matrix whose (i,j) element is the mutual information between variables
      \eqn{X_i}{Xi} and \eqn{X_j}{Xj}. 
}
\details{
      \item "mi.empirical" : This estimator computes the entropy of the empirical probability distribution.
      \item "mi.mm" : This is the Miller-Madow asymptotic bias corrected empirical estimator.
      \item "mi.shrink" : This is a shrinkage estimate of the entropy of a Dirichlet probability distribution.
	  \item "mi.sg" : This is the Schurmann-Grassberger estimate of the entropy of a Dirichlet probability distribution.
}
\author{
  Patrick E. Meyer, Frederic Lafitte, Gianluca Bontempi
}
\references{
 Patrick E. Meyer, Kevin Kontos, Frederic Lafitte, and Gianluca Bontempi.
 Information-theoretic inference of large transcriptional regulatory
 networks. EURASIP Journal on Bioinformatics and Systems Biology,
 2007.
 
 J. Beirlant, E. J. Dudewica, L. Gyofi, and E. van der Meulen. Nonparametric 
 entropy estimation : An overview. Journal of Statistics, 1997.

 Jean Hausser. Improving entropy estimation and the inference of genetic regulatory networks.
 Master thesis of the National Institute of Applied Sciences of Lyon, 2006.
}
\seealso{\code{\link{clr}},   \code{\link{aracne}},   \code{\link{mrnet}}}
\examples{
  data(syn.data)
  #mutual information estimator
  estimator="mi.empirical"
  #number of bins used to discretize
  nb.bins = sqrt(nrow(syn.data))
  mim <- build.mim(discretize(syn.data,nbins=nb.bins),estimator)
}
\keyword{misc}
