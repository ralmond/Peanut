### Parameterized networks.
### Generic functions

## Parameterized networks have the following properties:

## A node set called Pnodes which contains a list of all Pnodes to
## maximize.
## A field called "priorWeight" which gives the default prior weight
## to use.

is.Pnet <- function (x) {
  UseMethod("is.Pnet")
}
is.Pnet.default <- function (x) {
  "Pnet" %in% class(x)
}
as.Pnet <- function (x) {
  UseMethod("as.Pnet")
}

PnetPriorWeight <- function (net) {
  UseMethod("PnetPriorWeight")
}

"PnetPriorWeight<-" <- function (net,value) {
  UseMethod("PnetPriorWeight<-")
}

PnetPnodes <- function (net) {
  UseMethod("PnetPnodes")
}

"PnetPnodes<-" <- function (net, value) {
  UseMethod("PnetPnodes<-")
}

Pnet <- function (net, priorWeight=10, pnodes=list()) {
  UseMethod("Pnet")
}
Pnet.default <- function (net, priorWeight=10, pnodes=list()) {
  if (!("Pnet" %in% class(net)))
    class(net) <- c(class(net),"Pnet")
  PnetPriorWeight(net) <- priorWeight
  PnetPnodes(net) <- pnodes
  net
}

BuildAllTables <- function (net) {
  lapply(PnetPnodes(net),BuildTable)
  invisible(net)
}





## A parameterized node has the following fields:

## rules -- the name of the structure function
## link -- the name of the link function
## lnAlphas -- a list of discrimination parameters
## betas -- a list of difficulty parameters
## linkScale -- a list of scale parameters
## priorWeight -- a numeric value or a vector of numeric values for
## each row of the CPT.   Inherits from the net if not available.

is.Pnode <- function (x) {
  UseMethod("is.Pnode")
}
is.Pnode.default <- function(x) {
    "Pnode" %in% class(x)
}
as.Pnode <- function (x) {
  UseMethod("as.Pnode")
}

PnodeNet <- function (node) {
  UseMethod("PnodeNet")
}

PnodeRules <- function (node) {
  UseMethod("PnodeRules")
}

"PnodeRules<-" <- function (node,value) {
  UseMethod("PnodeRules<-")
}

PnodeLink <- function (node) {
  UseMethod("PnodeLink")
}

"PnodeLink<-" <- function (node,value) {
  UseMethod("PnodeLink<-")
}

PnodeLnAlphas <- function (node) {
  UseMethod("PnodeLnAlphas")
}

"PnodeLnAlphas<-" <- function (node,value) {
  UseMethod("PnodeLnAlphas<-")
}

PnodeAlphas <- function (node) {
  UseMethod("PnodeAlphas")
}

"PnodeAlphas<-" <- function (node,value) {
  UseMethod("PnodeAlphas<-")
}

PnodeAlphas.default <- function(node) {
  result <- PnodeLnAlphas(node)
  if (is.list(result)) {
    return (lapply(result,exp))
  } else {
    return (exp(result))
  }
}

"PnodeAlphas<-.default" <- function(node,value) {
  if (is.list(value)) {
    value <- lapply(value,log)
  } else {
    value <- log(value)
  }
  PnodeLnAlphas(node) <- value
  node
}

PnodeBetas <- function (node) {
  UseMethod("PnodeBetas")
}

"PnodeBetas<-" <- function (node,value) {
  UseMethod("PnodeBetas<-")
}

PnodeQ <- function (node) {
  UseMethod("PnodeQ")
}

"PnodeQ<-" <- function (node,value) {
  UseMethod("PnodeQ<-")
}

PnodeLinkScale <- function (node) {
  UseMethod("PnodeLinkScale")
}

"PnodeLinkScale<-" <- function (node,value) {
  UseMethod("PnodeLinkScale<-")
}

Pnode <- function (node, lnAlphas, betas, rules="Compensatory",
                   link="partialCredit",Q=TRUE,linkScale=NULL,
                   priorWeight=NULL) {
  UseMethod("Pnode")
}


## No effective way to do container inheretence using the R UseMethod
## (which rebinds the function call rather than generating a new
## one). This function trys to fetch the prior weight for a node from
## the node, and if it is not set, uses the default from the net.
GetPriorWeight <- function (node) {
  result <- PnodePriorWeight(node)
  if (is.null(result))
    return(PnetPriorWeight(PnodeNet(node)))
  result
}

PnodePriorWeight <- function (node) {
  UseMethod("PnodePriorWeight")
}

"PnodePriorWeight<-" <- function (node,value) {
  UseMethod("PnodePriorWeight<-")
}

PnodeParentTvals <- function (node) {
  UseMethod("PnodeParentTvals")
}

BuildTable <- function (node) {
  UseMethod("BuildTable")
}

