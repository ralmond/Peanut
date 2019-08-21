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
  \item{factory}{A character scalar containing the name of a global
    variable which contains a factory object capable of recreating the
    network from the data.}
  \item{serial}{A list containing at least three elements.  One is the
     name of the network.  One is the \code{data} element which contains
     the serialized data as a raw vector. The third 
     is either a \code{factory} element containing the name of a global
     symbol containing a factory for reading the object or a \code{type}
     argument giving the name of the constructor.}
  \item{data}{A list containing at least two elements.  One is the
    name of the network.  One is the \code{data} element which contains
    the serialized data as a raw vector.}
  
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
  database.  The \code{data} element should be a raw vector (e.g., the
  output of \code{\link[base]{serialize}(...,NULL)}) and will be stored
  as a blob (binary large object) and the other elements should be
  strings.  Document based databases (e.g., \link[mongolite]{mongo}) may
  handle the additional fields but relational database will have
  difficulty with them, so implementers should only rely on the three
  fields.

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

  This mechanism mimics the S3 method dispatch method, although it
  doesn't really use it.  If the argument to \code{PnetUnserialize} has
  a \code{type} field (but no \code{factory} field) then it will call a
  funciton called \code{PnetUnserialize.}\emph{type}.
}

\value{

  The \code{PnetSerialize} function returns a list with the following
  elements: 
  \item{name}{The name of the network.  If this matches an existing
    network, then it will be replaced on unserialize, otherwise a new
    network will be created.}
  \item{data}{Serialized data for the network.  This should be a raw
  vector.}
  \item{factory}{The name of a global object which can restore networks
    from serialized data.}
  \item{type}{The name of a class for which an
    \code{PnetUnserialize.}\emph{type} method exits.}
  \item{...}{There may be other data, but note that programs
    saving/restoring the serialized representation may not know how to
    handle these extra fields.}

  The \code{PnetUnserialize} and \code{unserializePnet} functions return
  an object of type \code{\link{Pnet}}.

}
\author{Russell Almond}
\note{
  The first use of this function was designed to save/restore a network
  from a \link[mongolite]{mongo} database.  This format easily supports
  extra fields in the return list.  The samething is true if the network
  is serialized using either JSON/BSON or the normal R
  \code{\link[base]{dump}} mechanism.

  On the other hand, if the network is to be stored in a SQL database,
  the using program will not have places to store the extra fields.
}
\seealso{
  \code{\link{Pnet}}, \code{\link{Warehouse}}
}
\examples{
\dontrun{
library(mongolite)
library(jsonlite)
library(PNetica)
sess <- NeticaSession()
startSession(sess)

collect <- mongo("studentModels","test",
                     "mongodb://127.0.0.1:27017/test")
## Or "mongodb://user:pwd@127.0.0.1:27017/test"
sm.net <- ReadNetworks(file.path(library(help="PNetica")$path, "testnets",
     "miniPP-CM.dne"), session=sess)

sm.ser <- PnetSerialize(sm.net)
## base 64 encode the data to make it easier to store.
sm.ser$data <- base64_enc(sm.ser$data)

collect$replace(paste('{"name":"',sm.ser$name,'"}'),
                toJSON(lapply(sm.ser,unbox)),
                upsert=TRUE)

## Use iterator method to find, so we get in list rather than data frame
## representation. 

it <- collect$iterate(sprintf('{"name":"\%s"}',PnetName(sm.net)),limit=1)
sm1.ser <- it$one()
## Decode back to the raw vector.
sm1.ser$data <- base64_dec(sm1.ser$data)
sm1 <- unserializePnet(sess,sm1.ser)

sm1.ser$factory <- "sess"
sm1a <- PnetUnserialize(sm1.ser)

}
}
\keyword{ IO }
\keyword{ graphs }
