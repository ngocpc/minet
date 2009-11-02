\name{mrnet}
\alias{mrnet}

\title{Maximum Relevance Minimum Redundancy}
\usage{mrnet(mim)}
\arguments{
  \item{mim}{ A square matrix whose i,j th element is the mutual information 
		 between variables \eqn{Xi}{X_i} and \eqn{Xj}{X_j} - see \code{\link{build.mim}}.}
  }
\value{\code{mrnet} returns a matrix which is the weighted adjacency matrix of the network.
      In order to display the network, load the package Rgraphviz and use the following command: \cr
          plot( as( returned.matrix ,"graphNEL") )

}

\description{
  \code{mrnet} takes the mutual information matrix as input in order to infer the network using 
	the maximum relevance/minimum redundancy feature selection method - see details.
}
\details{
      The MRNET approach consists in repeating a MRMR feature selection procedure for 
      each variable of the dataset.
      The MRMR method starts by selecting the variable \eqn{X_i}{Xi} having the highest
      mutual information with the target \eqn{Y}{Y}. 
      In the following steps, given a set \eqn{\mathcal{S}}{S} of selected variables, the criterion 
      updates \eqn{\mathcal{S}}{S} by choosing the variable \eqn{X_k}{Xk} that maximizes 
      \eqn{ I(X_k;Y) - \frac{1}{|\mathcal{S}|}\sum_{X_i \in \mathcal{S}} I(X_k;X_i)}{%
      I(Xk;Y) - mean(I(Xk;Xi)), Xi in S.}\cr
      The weight of each pair \eqn{X_i,X_j}{Xi,Xj} will be the maximum score between the one 
      computed when \eqn{X_i}{Xi} is the target and the one computed when \eqn{X_j}{Xj} is 
      the target.
}
\author{
  Patrick E. Meyer, Frederic Lafitte, Gianluca Bontempi
}
\references{
 Patrick E. Meyer, Kevin Kontos, Frederic Lafitte, and Gianluca Bontempi. 
 Information-theoretic inference of large transcriptional regulatory
 networks. EURASIP Journal on Bioinformatics and Systems Biology,
 2007. 
 
 H. Peng, F.long and C.Ding.
 Feature selection based on mutual information: Criteria of max-dependency, max relevance and min redundancy.
 IEEE transaction on Pattern Analysis and Machine Intelligence, 2005
}
\seealso{\code{\link{build.mim}},   \code{\link{clr}},   \code{\link{aracne}}, \code{\link{mrnetb}}}
\examples{
data(syn.data)
mim <- build.mim(syn.data, estimator="spearman")
net <- mrnet(mim)
}
\keyword{misc}
