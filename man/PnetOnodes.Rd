\name{PnetOnodes}
\alias{PnetOnodes}
\alias{PnetOnodes<-}
\alias{is.Onode}
\alias{is.Onode<-}
\alias{is.Onode,Pnode-method}
\alias{is.Onode<-,Pnode-method}
\title{Get or set the observable nodes in a network.}
\description{

  An observable node is one whos value may be set in the course of evidence.  

}
\usage{
PnetOnodes(net)
PnetOnodes(net) <- value
is.Onode(x)
\S4method{is.Onode}{Pnode}(x)
is.Onode(x)<- value
}
\arguments{
  \item{net}{An object of class \code{\linkS4class{Pnet}} in which the observables are nodes.}
  \item{x}{An object of class \code{\linkS4class{Pnode}} which may or may not be an observable.}
  \item{value}{For \code{is.Onode<-} a logical value giving the new state for the variable;  For \code{PnetOnodes<-} it is a list of variables which should be marked as observables.}
}
\details{

  A node is marked as an observable by adding 
  \dQuote{onode} to its \code{\link{PnodeLabels}}.  
  Certain functions operate on observables.
  
}
\value{
  The functions \code{PnetOnodes} returns a list of \code{\linkS4class{Pnode}} objects.  
  The function \code{is.Onode} returns true or false.
  The function
}
\author{Russell Almond}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
