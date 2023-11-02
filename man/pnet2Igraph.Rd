\name{pnet2Igraph}
\alias{pnet2Igraph}
\title{Converts a pnet to a igraph::igraph}
\description{

  The \code{\link[igraph:igraph-package]{igraph}} package supports a number of 
  methods for both printing graphs and querying them.  This function creates a lightweight
  \code{igraph} containing the same topology as the \code{pnet} argument.

}
\usage{
pnet2Igraph(net)
}
\arguments{
  \item{net}{An object of class \code{\linkS4class{Pnet}} to be desribed.}
}
\details{

  The function as currently implemented only copies superficial features of the graph.  
  In particular, it copies the nodes and the edges, but only these properties of the
  edges.
  \describe{
    \item{name}{Corresponds to \code{\link{PnodeName}(node)}.}
    \item{title}{Corresponds to \code{\link{PnodeTitle}(node)}.}
    \item{states}{Corresponds to \code{\link{PnodeStates}(node)}.}    
    \item{sets}{Corresponds to \code{\link{PnodeLabels}(node)}.}
  }
  
  This is deliberately lightweight.  The goal is to make something to pass along to a
  graphical layout and display algorithm rather than a complete representation of the 
  probability information in the pnets.
}
\value{
  An object of class \code{\link[igraph:igraph-package]{igraph}} containing the 
  topology of the input network.
}
\references{
  Csardi G, Nepusz T (2006). The igraph software package for complex network research.
  \emph{InterJournal, Complex Systems}, 1695. \url{https://r.igraph.org/index.html}
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{Pnet}}, \code{\linkS4class{Pnode}}, 
  \code{\link{PnetPnodes}}, \code{\link{PnodeName}}, 
  \code{\link{PnodeTitle}}, \code{\link{PnodeLabels}}, 
  \code{\link{PnodeStates}}
}
\examples{

  \dontrun{
      sess <- RNetica::NeticaSession()
  RNetica::startSession(sess)
  
  netman1 <- read.csv(system.file("auxdata", "Mini-PP-Nets.csv", 
                                  package="Peanut"),
                      row.names=1,stringsAsFactors=FALSE)
  
  nodeman1 <- read.csv(system.file("auxdata", "Mini-PP-Nodes.csv", 
                                   package="Peanut"),
                       row.names=1,stringsAsFactors=FALSE)
  
  omegamat <- read.csv(system.file("auxdata", "miniPP-omega.csv",
                                   package="Peanut"),
                       row.names=1,stringsAsFactors=FALSE)
  

  ## Network and node warehouse, to create networks and nodes on demand.
  Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name")
  
  Nodehouse <- PNetica::NNWarehouse(manifest=nodeman1,
                           key=c("Model","NodeName"),
                           session=sess)
  
  ## Build the proficiency model first:
  CM <- WarehouseSupply(Nethouse,"miniPP_CM")
  CM1 <- Omega2Pnet(omegamat,CM,Nodehouse,override=TRUE,debug=TRUE)
  
  ig <- pnet2Igraph(CM1)

  plot(ig)
  ## Write the file in GraphViz format
  igraph::write_graph(ig,tmpfile("ig",fileext="dot"),format="dot")
  }
}
\keyword{ graphs }
\keyword{ interface }

