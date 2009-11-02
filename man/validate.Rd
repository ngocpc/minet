
\name{validate}
\alias{validate}
\title{Inference Validation}

\description{
  \code{validate} compares the infered network to the true underlying network for several threshold values 
      and appends the resulting confusion matrices to the returned object.     
}
\usage{
validate(inet,tnet,steps=50,thresholds=pretty(inet,steps))
}
\arguments{
  \item{inet}{This is the infered network, a data.frame or matrix obtained by one of the functions \link{minet}, \link{aracne},
			\link{clr} or \link{mrnet} .}
  \item{tnet}{The true underlying network. This network must have the same size and variable names as \code{inet}.}
  \item{steps}{The number of threshold values to be used in the validation process - see \code{details}.}
  \item{thresholds}{The vector of thresholds to be used. By default it is a sequence of about steps+1 equally spaced intervals.}
}
\value{
  \code{validate} returns a data.frame whith four columns named \code{thrsh, tp, fp, fn}. These values are 
      computed for each of the \code{steps} thresholds. Thus each row of the returned object contains
      the confusion matrix for a different threshold.      
}
\details{
      For each of the \code{steps} threshold values \eqn{I_0}{T}, the edges whose weight are (strictly) 
      below \eqn{I_0}{T} are eliminated. All the other edges will have a weight 1. 
	  Thus for each threshold, we obtain a boolean network from the infered network. This 
	  network is compared to the true underlying network, \code{tnet}, in order to compute a 
	  confusion (adjacency) matrix.
	  All the confusion matrices, obtained with different threshold values, are appended to the 
	  returned object.
	  In the end the \code{validate} function returns a data.frame containing \code{steps+1} confusion matrices.
}
\seealso{ \code{\link{minet}},   \code{\link{vis.res}} }

\examples{
data(syn.data)
data(syn.net)
inf.net <- mrnet(build.mim(syn.data, estimator="spearman"))
table <- validate( inf.net, syn.net, steps=100 )
table <- validate( inf.net, syn.net, steps=1, thresholds=0.5 )
}
\keyword{misc}

