
\name{clr}
\alias{clr}

\title{Context Likelihood or Relatedness Network}
\usage{clr( mim )}
\arguments{
  \item{mim}{A square matrix whose i,j th element is the mutual information  
			between variables \eqn{Xi}{X_i} and \eqn{Xj}{X_j} - see \code{\link{build.mim}}.}
}
\value{
  \code{clr} returns a matrix which is the weighted adjacency matrix of the network.
  In order to display the network, load the package Rgraphviz and use the following comand
          plot( as( returned.matrix ,"graphNEL") )

}
\description{
  \code{clr} takes the mutual information matrix as input in order to return the infered network - see details.
}
\details{
      The CLR algorithm is an extension of relevance network. Instead of 
      considering the mutual information \eqn{I(X_i;X_j)}{I(Xi;Xj)} between features
      \eqn{X_i}{Xi} and \eqn{X_j}{Xj}, it takes into account the score 
      \eqn{\sqrt{z_i^2+z_j^2}}{sqrt(zi^2+zj^2)}, where \cr
      \deqn{ z_i = \max \bigg\lbrace 0, \frac{I(X_i;X_j)-\mu_i}{\sigma_i} \bigg\rbrace }{ 
            zi = max( 0, ( I(Xi;Xj)-mean(Xi) )/sd(Xi) )} \cr 
      and \eqn{\mu_i}{mean(Xi)} and \eqn{\sigma_i}{sd(Xi)} are, respectively, 
      the mean and the standard deviation of the empirical distribution 
      of the mutual information values \eqn{I(X_i;X_k)}{I(Xi,Xk)}, 
      k=1,...,n.
}
\references{
      Jeremiah J. Faith, Boris Hayete, Joshua T. Thaden, Ilaria Mogno, Jamey
      Wierzbowski, Guillaume Cottarel, Simon Kasif, James J. Collins, and 
      Timothy S. Gardner. Large-scale mapping and validation of escherichia coli
      transcriptional regulation from a compendium of expression profiles. PLoS
      Biology, 2007.    
}

\seealso{\code{\link{build.mim}},   \code{\link{aracne}},   \code{\link{mrnet}}, \code{\link{mrnetb}} }

\examples{
data(syn.data)
mim <- build.mim(syn.data,estimator="spearman")
net <- clr(mim)
}
\keyword{misc}

