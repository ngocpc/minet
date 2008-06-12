\name{mrnet}
\alias{mrnet}

\title{Maximum Relevance Minimum Redundancy}
\usage{mrnet( mim )}
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
      Consider a supervised learning task, where the output is denoted by \eqn{Y}{Y}
      and \eqn{\mathcal{V}}{V} is the set of input variables. The method ranks the set 
      \eqn{\mathcal{V}}{V} of inputs according to a score that is the difference between 
      the mutual information with the output variable \eqn{Y}{Y} (maximum relevance) 
      and the average mutual information with the previously ranked variables
      (minimum redundancy).
      The greedy search starts by selecting the variable \eqn{X_i}{Xi} having the highest
      mutual information with the target \eqn{Y}{Y}. The second selected variable \eqn{X_j}{Xj} 
      will be the one that maximizes \eqn{I(X_j;Y)-I(X_j;X_i)}{I(Xj;Y)-I(Xj;Xi)}. 
      In the following steps, given a set \eqn{\mathcal{S}}{S} of selected variables, the criterion 
      updates \eqn{\mathcal{S}}{S} by choosing the variable \eqn{X_k}{Xk} that maximizes 
      \eqn{ I(X_k;Y) - \frac{1}{|\mathcal{S}|}\sum_{X_i \in \mathcal{S}} I(X_k;X_i)}{%
      I(Xk;Y) - mean(I(Xk;Xi)), Xi in S.}\cr
      The MRNET approach consists in repeating this selection procedure for 
      each target variable by putting \eqn{Y=X_i}{Y=Xi} and 
      \eqn{\mathcal{V}=\mathcal{X}\backslash\lbrace X_i\rbrace}{V = X\\{Xi}},
      i=1,...,n where \eqn{\mathcal{X}}{X} is the set of outcomes of all variables.
      The weight of each pair \eqn{X_i,X_j}{Xi,Xj} will be the maximum score between the one 
      computed when \eqn{X_i}{Xi} is the output and the one computed when \eqn{X_j}{Xj} is 
      the output.
}
\author{
  Patrick E. Meyer, Frederic Lafitte, Gianluca Bontempi
}
\references{
 Patrick E. Meyer, Kevin Kontos, Frederic Lafitte, and Gianluca Bontempi. 
 Information-theoretic inference of large transcriptional regulatory
 networks. EURASIP Journal on Bioinformatics and Systems Biology,
 2007.    
}
\seealso{\code{\link{build.mim}},   \code{\link{clr}},   \code{\link{aracne}}}
\examples{
data(syn.data)
mim <- build.mim(discretize(syn.data))
net <- mrnet(mim)
}
\keyword{misc}
