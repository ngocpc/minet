
\docType{data}
\name{syn.net}
\alias{syn.net}
\title{SynTReN Source Network}
\description{
  This is the true underlying network used to generate the dataset loaded
  by \code{data(syn.data)} - see \code{\link{syn.data}}.
}
\usage{data(syn.net)}
 \format{
 \code{syn.net} is a boolean adjacency matrix representing an undirected graph of 50 nodes.
}
\source{
 \code{syn.net} is the "yeast\_nn.sif" source network from the SynTReN generator 
 where all the variables/nodes not in \code{syn.data} were removed.
}

\references{
 Tim Van den Bulcke, Koenraad Van Leemput, Bart Naudts, Piet van Remortel, 
 Hongwu Ma, Alain Verschoren, Bart De Moor, and Kathleen Marchal. 
 Syntren : a generator of synthetic gene expression dataset for design
 and analysis of structure learning algorithms. BMC Bioinformatics, 2006.
}
\examples{
data(syn.data)
data(syn.net)
mim <- build.mim(disc(syn.data),estimator="empirical")
infered.net <- mr.net(mim)
max(fscores(validate( infered.net, syn.net )))

}
\keyword{datasets}
