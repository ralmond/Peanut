\name{PnetSerialize}
\alias{PnetSerialize}
\alias{PnetUnserialize}
\alias{unserializePnet}
\title{Writes/restores network from a string.}
\description{
  The \code{PnetSerialize} method writes the network to a string and
  returns a list containting both the serialized data and type
  information. The \code{PnetUnserialize} method restores the data.
  Note that the serialized form must contain either the name of the type
  or the name of the factory used to restore the object (see details).
}
\usage{
PnetSerialize(net)
PnetUnserialize(serial)
unserializePnet(factory,data)
}
\arguments{
  \item{net}{A \code{\link{Pnet}} to be serialized.}
  \item{serial}{A list containing at least to elements.  One is the
    \code{data} element which contains the serialized data.  The other
    is either a \code{factory} element containing the name of a global
    symbol containing a factory for reading the object or a \code{type}
    argument giving the name of the constructor.}
  \item{factory}{A character scalar containing the name of a global
    variable which contains a factory object capable of recreating the
    network from the data.}
  \item{data}{A character scalar containing the serialized data.}
}
\details{

  The intention of this function is to serialize the network in such a
  way that it can be saved to a database and restored.  The result of a
  call to \code{PnetSerialize} is a list with three elements.  One
  element is called \code{data} and contains the actual serialize data.
  The second element is called \code{name} and it should be an
  identifier for the network (the result of \code{\link{PnetName}}).  
  The last element is either \code{factory} or \code{type}.  In either
  case, they should be a string.  The list may contain other elements,
  but these may be ignored by other programs.

  The intent is to provide a representation that can be saved to a
  database.  The \code{data} element will be stored as a blob (binary
  large object) and the other elements as strings.  Document based
  databases (e.g., \link[mongolite]{mongo}) may handle the additional
  fields but relational database will have difficulty with them, so
  implementers should only rely on the three fields.

  The function \code{PnetUnserialize} reverses this operation.  If
  \code{factory} is supplied, then the factory protocol is used for
  restoration.  If \code{type} is supplied instead, then the type string
  protocol is used.  If both are supplied, then the factory protocol is
  preferred, and if neither is supplied, an error is signaled.  The
  function \code{unserializePnet} is a generic function used by the
  factory protocol.  If a \code{Pnet} already exists with the given
  name, then it is replaced, otherwise a new one is created.

}
\section{Factory Protocol}{
  
  A factory is an object of a class for which a method for the
  \code{unserializePnet} generic function is defined.  This method
  should return an object of type \code{\link{Pnet}}.  Thus the
  \code{Peanut} package doesn't need to know the implementaiton details.

  Typically factories are global (static in java lanugage) objects.  In
  this case the \code{factory} object should be the name of the factory
  (as it will need to be serialized).  The \code{\link[base]{get}}
  function is used to retrieve its value, so typically it is stored in
  \code{.GlobalEnv}.

  The factory protocol allows other kind of flexibility as well,
  including being able to encapsulate a reference to loaded objects, so
  this is the preferred method.

}
\section{Type String Protocol}{



}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (net) 
standardGeneric("PnetSerialize"), generic = structure("PnetSerialize", package = "Peanut"), package = "Peanut", group = list(), valueClass = character(0), signature = "net", default = structure(function (net) 
UseMethod("PnetSerialize"), target = structure("ANY", class = structure("signature", package = "methods"), .Names = "net", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "net", package = "methods"), generic = structure("PnetSerialize", package = "Peanut"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = (structure(function (net) 
UseMethod("PnetSerialize"), target = structure("ANY", class = structure("signature", package = "methods"), .Names = "net", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "net", package = "methods"), generic = structure("PnetSerialize", package = "Peanut"), class = structure("derivedDefaultMethod", package = "methods")))(net), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
