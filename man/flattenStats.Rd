\name{flattenStats}
\alias{flattenStats}
\title{Flattens a statistic list into a numeric or character vector.}
\description{

  Margin statitics are list valued, however, it is often useful to break
  them down into separate statistics for each state.  In particular,
  this facilitates putting the data into a data.frame format for further
  analysis. 

}
\usage{
flattenStats(statlist)
}
\arguments{
  \item{statlist}{A named list contatining the statsitics.}
}
\details{

  Vector valued tatistics produces by \code{\link{PnodeMargin}} are
  replaced by multiple statistics with the state name appended (after a
  dot) to the variable name.

  Scalar valued statistics are left as is.

}
\value{

  A vector with the statistics.  If all statistics are numeric, it will
  be a numeric vector.  If some are character valued (e.g.,
  \code{\link{PnodeMode}}), it will be a
  character vector.

}
\author{Russell Almond}
\seealso{

  \code{\link{PnodeMargin}}, \code{\link{PnodeEAP}}, \code{\link{PnodeMode}},
  
}
\examples{

slist <- list("Physics_EAP"=.3,
              "Physics_Margin"=c("High"=.5,"Medium"=.3,"Low"=.2),
              "Physics_Mode"="High")
flattenStats(slist)


}
\keyword{ manip }

