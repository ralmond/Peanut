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
setGeneric("is.Pnet")
is.Pnet.default <- function (x) {
  "Pnet" %in% class(x)
}
as.Pnet <- function (x) {
  UseMethod("as.Pnet")
}
setGeneric("as.Pnet")

PnetPriorWeight <- function (net) {
  UseMethod("PnetPriorWeight")
}
setGeneric("PnetPriorWeight")

"PnetPriorWeight<-" <- function (net,value) {
  UseMethod("PnetPriorWeight<-")
}
setGeneric("PnetPriorWeight<-")

PnetPnodes <- function (net) {
  UseMethod("PnetPnodes")
}
setGeneric("PnetPnodes")

"PnetPnodes<-" <- function (net, value) {
  UseMethod("PnetPnodes<-")
}
setGeneric("PnetPnodes<-")

Pnet <- function (net, priorWeight=10, pnodes=list()) {
  net <- as.Pnet(net)
  PnetPriorWeight(net) <- priorWeight
  PnetPnodes(net) <- pnodes
  net
}
setGeneric("Pnet")



BuildAllTables <- function (net, debug=FALSE) {
  netnm <- PnetName(net)
  Errs <- list()
  lapply(PnetPnodes(net),
         function (node) {
           ndnm <- PnodeName(node)
           flog.debug("Building CPT for node %s in net %s", ndnm, netnm)
           out<- flog.try(BuildTable(node),
                          context=sprintf("Building CPT for node %s in net %s",
                                          ndnm, netnm))
           if (is(out,'try-error')) {
             Errs <- c(Errs,out)
             if (debug) recover()
           }
         })
  if (length(Errs) >0L)
    stop("Errors encountered while updating parameters for ",netnm)
  invisible(net)
}

setGeneric("BuildAllTables")

### To fit hub and spoke model.

### This takes a list of nodes from different networks, and makes copies in the given net.
PnetMakeStubNodes <- function (net,nodes) {
  UseMethod("PnetMakeStubNodes")
}
setGeneric("PnetMakeStubNodes")

### This takes nodes copied from the hub network and removes them leaving only references.
PnetRemoveStubNodes <- function (net,nodes) {
  UseMethod("PnetRemoveStubNodes")
}
setGeneric("PnetRemoveStubNodes")

PnetAdjoin <- function (hub, spoke) {
  UseMethod("PnetAdjoin")
}
setGeneric("PnetAdjoin")

PnetDetach <- function (motif, spoke) {
  UseMethod("PnetDetach")
}
setGeneric("PnetDetach")



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
setGeneric("is.Pnode")
is.Pnode.default <- function(x) {
    "Pnode" %in% class(x)
}
as.Pnode <- function (x) {
  UseMethod("as.Pnode")
}
setGeneric("as.Pnode")

PnodeNet <- function (node) {
  UseMethod("PnodeNet")
}
setGeneric("PnodeNet")

PnodeRules <- function (node) {
  UseMethod("PnodeRules")
}
setGeneric("PnodeRules")

"PnodeRules<-" <- function (node,value) {
  UseMethod("PnodeRules<-")
}
setGeneric("PnodeRules<-")

PnodeLink <- function (node) {
  UseMethod("PnodeLink")
}
setGeneric("PnodeLink")

"PnodeLink<-" <- function (node,value) {
  UseMethod("PnodeLink<-")
}
setGeneric("PnodeLink<-")

PnodeLnAlphas <- function (node) {
  UseMethod("PnodeLnAlphas")
}
setGeneric("PnodeLnAlphas")

"PnodeLnAlphas<-" <- function (node,value) {
  UseMethod("PnodeLnAlphas<-")
}
setGeneric("PnodeLnAlphas<-")

PnodeAlphas <- function (node) {
  result <- PnodeLnAlphas(node)
  if(is.null(result))
    return (NULL)
  if (is.list(result)) {
    return (lapply(result,exp))
  } else {
    return (exp(result))
  }
  ## This doesn't seem to be working, can't tell why.
  ##  UseMethod("PnodeAlphas")
}
setGeneric("PnodeAlphas")

"PnodeAlphas<-" <- function (node,value) {
  if (is.list(value)) {
    value <- lapply(value,log)
  } else {
    value <- log(value)
  }
  PnodeLnAlphas(node) <- value
  node
  ## This doesn't seem to be working, can't tell why.
  ## UseMethod("PnodeAlphas<-")
}
setGeneric("PnodeAlphas<-")

PnodeAlphas.default <- function(node) {
  result <- PnodeLnAlphas(node)
  if(is.null(result))
    return (NULL)
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
setGeneric("PnodeBetas")

"PnodeBetas<-" <- function (node,value) {
  UseMethod("PnodeBetas<-")
}
setGeneric("PnodeBetas<-")

PnodeQ <- function (node) {
  UseMethod("PnodeQ")
}
setGeneric("PnodeQ")

"PnodeQ<-" <- function (node,value) {
  UseMethod("PnodeQ<-")
}
setGeneric("PnodeQ<-")

PnodeLinkScale <- function (node) {
  UseMethod("PnodeLinkScale")
}
setGeneric("PnodeLinkScale")

"PnodeLinkScale<-" <- function (node,value) {
  UseMethod("PnodeLinkScale<-")
}
setGeneric("PnodeLinkScale<-")

Pnode <- function (node, lnAlphas, betas, rules="Compensatory",
                   link="partialCredit",Q=TRUE,linkScale=NULL,
                   priorWeight=NULL) {
  UseMethod("Pnode")
}
setGeneric("Pnode")


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
setGeneric("PnodePriorWeight")

"PnodePriorWeight<-" <- function (node,value) {
  UseMethod("PnodePriorWeight<-")
}
setGeneric("PnodePriorWeight<-")

PnodePostWeight <- function (node) {
  UseMethod("PnodePostWeight")
}
setGeneric("PnodePostWeight")

PnodeProbs <- function (node) {
  UseMethod("PnodeProbs")
}
setGeneric("PnodeProbs")

"PnodeProbs<-" <- function (node,value) {
  UseMethod("PnodeProbs<-")
}
setGeneric("PnodeProbs<-")

PnodeParentTvals <- function (node) {
  UseMethod("PnodeParentTvals")
}
setGeneric("PnodeParentTvals")

PnodeParentTvals.default <- function (node) {
  effectiveThetas(PnodeNumStates(node))
}


BuildTable <- function (node) {
  UseMethod("BuildTable")
}
setGeneric("BuildTable")

#####
## Offset Mechinism

defaultAlphas <- function (node, rule) {
  if (isOffsetRule(rule)[1]) return(0)
  rep(0, PnodeNumParents(node))
}

defaultBetas <- function (node, rule) {
  if (!isOffsetRule(rule)[1]) return(0)
  rep(0, PnodeNumParents(node))
}
