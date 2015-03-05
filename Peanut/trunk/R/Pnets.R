### Parameterized networks.

## A utility function for converting objects to strings and vise
## versa.
dputToString <- function (obj) {
  con <- textConnection(NULL,open="w")
  tryCatch({dput(obj,con);
           textConnectionValue(con)},
           finally=close(con))
}

dgetFromString <- function (str) {
  con <- textConnection(str,open="r")
  tryCatch(dget(con), finally=close(con))
}


## Parameterized networks have the following properties:

## A node set called Pnodes which contains a list of all Pnodes to
## maximize.
## A field called "priorWeight" which gives the default prior weight
## to use.

is.Pnet <- function (x) {
  is.NeticaBN(x) && length(NetworkNodesInSet(x,"Pnodes")) > 0
}

PnetPriorWeight <- function (net) {
  dgetFromString(NetworkUserField(net,"priorWeight"))
}

"PnetPriorWeight<-" <- function (net,value) {
  NetworkUserField(net,"priorWeight") <- dputToString(value)
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
  is.NeticaNode(x) && "pnodes" %in% NodeSets(x)
}


PnodeRules <- function (node) {
  dgetFromString(NodeUserField(node,"rules"))
}

"PnodeRules<-" <- function (node,value) {
  NodeUserField(node,"rules") <- dputToString(value)
}

PnodeLink <- function (node) {
  dgetFromString(NodeUserField(node,"link"))
}

"PnodeLink<-" <- function (node,value) {
  NodeUserField(node,"link") <- dputToString(value)
}

PnodeLnAlphas <- function (node) {
  dgetFromString(NodeUserField(node,"lnAlphas"))
}

"PnodeLnAlphas<-" <- function (node,value) {
  NodeUserField(node,"lnAlphas") <- dputToString(value)
}

PnodeBetas <- function (node) {
  dgetFromString(NodeUserField(node,"betas"))
}

"PnodeBetas<-" <- function (node,value) {
  NodeUserField(node,"betas") <- dputToString(value)
}


PnodeLinkScale <- function (node) {
  dgetFromString(NodeUserField(node,"linkScale"))
}

"PnodeLinkScale<-" <- function (node,value) {
  NodeUserField(node,"linkScale") <- dputToString(value)
}

PnodePriorWeight <- function (node) {
  result <- dgetFromString(NodeUserField(node,"priorWeight"))
  if (is.na(result))
    return PnetPriorWeight(NodeNet(node))
  result
}

"PnodePriorWeight<-" <- function (node,value) {
  NodeUserField(node,"priorWeight") <- dgetFromString(value)
}


### Build CPTs from parameters

PnodeBuildTable <- function (node) {
  node[] <- calcDPCTable(ParentStates(node),NodeStates(node),
                         PnodeLnAlphas(node), PnodeBeta(node),
                         PnodeRules(node),PnodeLink(node),
                         PnodeLinkScale(node))
  NodeExperience(node) <- PnodePriorWeight(node)
  invisible(node)
}

PnetBuildTables <- function (net) {
  lapply(NetworkNodesInSet(net,"pnodes"),PnodeBuildTable)
}
