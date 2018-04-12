\name{topsort}
\alias{topsort}
\title{Topologically sorts the rows and columns of an Omega matrix}
\description{

  The structural part of the \eqn{\Omega}-matrix is an incidence matrix
  where the entry is 1 if the node represented by the column is a parent
  of the node represented by the child.  This sorts the rows and columns
  of the matrix (which should have the same names) so that the ancestors
  of a node always appear prior to it in the sequence.  As a
  consequence, the values in the upper triangle of the
  \eqn{\Omega}-matrix are always zero after sorting.

}
\usage{
topsort(Omega, noisy = FALSE)
}
\arguments{
  \item{Omega}{A square matrix of 1's and zeros which corresponds to an
    acyclic directed graph.}
  \item{noisy}{A logical value. If true, details of progress through the
    algorithm are printed.}
}
\value{

  An ordering of the rows and columns which will sort the matrix.

}
\author{Russell Almond}
\note{

  This will generate an error if the graph represented by the matrix is
  cyclic. 
}
\seealso{

  \code{\link{Pnet2Omega}} uses this function to sort the columns in the
  Omega matrix.

}
\examples{

## Sample Omega matrix.
omegamat <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "miniPP-omega.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)
omega <- as.matrix(omegamat[,2:6])

## omega is already sorted so scramble it.
shuffle <- sample.int(5)
omegas <- omega[shuffle,shuffle]

ord <- topsort(omegas)
omegas[ord,ord]

}
\keyword{ graph }

