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

is.Pnet.NeticaBN <- function (x) {
  length(NetworkNodesInSet(x,"Pnodes")) > 0
}

PnetPriorWeight.NeticaBN <- function (net) {
  result <- dgetFromString(NetworkUserField(net,"priorWeight"))
  if (is.na(result)) result <- NULL
  result
}

"PnetPriorWeight<-.NeticaBN" <- function (net,value) {
  NetworkUserField(net,"priorWeight") <- dputToString(value)
  net
}

PnetPnodes.NeticaBN <- function (net) {
  NetworkNodesInSet(net,"pnodes")
}


## A parameterized node has the following fields:

## rules -- the name of the structure function
## link -- the name of the link function
## lnAlphas -- a list of discrimination parameters
## betas -- a list of difficulty parameters
## linkScale -- a list of scale parameters
## priorWeight -- a numeric value or a vector of numeric values for
## each row of the CPT.   Inherits from the net if not available.

is.Pnode.NeticaNode <- function (x) {
  "pnodes" %in% NodeSets(x)
}

PnodeNet.NeticaNode <- function (node) {
  NodeNet(node)
}

PnodeRules.NeticaNode <- function (node) {
  dgetFromString(NodeUserField(node,"rules"))
}

"PnodeRules<-.NeticaNode" <- function (node,value) {
  NodeUserField(node,"rules") <- dputToString(value)
  node
}

PnodeLink.NeticaNode <- function (node) {
  dgetFromString(NodeUserField(node,"link"))
}

"PnodeLink<-.NeticaNode" <- function (node,value) {
  NodeUserField(node,"link") <- dputToString(value)
  node
}

PnodeLnAlphas.NeticaNode <- function (node) {
  dgetFromString(NodeUserField(node,"lnAlphas"))
}

"PnodeLnAlphas<-.NeticaNode" <- function (node,value) {
  NodeUserField(node,"lnAlphas") <- dputToString(value)
  node
}

PnodeBetas.NeticaNode <- function (node) {
  dgetFromString(NodeUserField(node,"betas"))
}

"PnodeBetas<-.NeticaNode" <- function (node,value) {
  NodeUserField(node,"betas") <- dputToString(value)
  node
}


PnodeLinkScale.NeticaNode <- function (node) {
  dgetFromString(NodeUserField(node,"linkScale"))
}

"PnodeLinkScale<-.NeticaNode" <- function (node,value) {
  NodeUserField(node,"linkScale") <- dputToString(value)
  node
}

PnodePriorWeight.NeticaNode <- function (node) {
  result <- dgetFromString(NodeUserField(node,"priorWeight"))
  ## RNetic returns NA, not null for missing field.
  if (is.na(result)) result <- NULL
  result
}

"PnodePriorWeight<-.NeticaNode" <- function (node,value) {
  NodeUserField(node,"priorWeight") <- dgetFromString(value)
  node
}

PnodeParentTvals.NeticaNode <- function (node) {
  lapply(NodeParents(node),NodeLevels)
}

### Build CPTs from parameters

PnodeBuildTable.NeticaNode <- function (node) {
  node[] <- calcDPCFrame(ParentStates(node),NodeStates(node),
                         PnodeLnAlphas(node), PnodeBetas(node),
                         PnodeRules(node),PnodeLink(node),
                         PnodeLinkScale(node),PnodeParentTvals(node))
  NodeExperience(node) <- GetPriorWeight(node)
  invisible(node)
}

calcPnetLLike.NeticaBN <- function (net,cases){
  llike <- 0
  nextRec <- "FIRST"
  onodes <- NetworkNodesInSet(net,"onodes")
  pos <- 0
  WithOpenCaseStream(cases,
    while(!is.na(pos)) {
      ReadFindings(onodes,cases,nextRec)
      nextRec <- "NEXT"
      pos <- getCaseStreamPos(cases)
      w <- getCaseStreamLastFreq(cases)
      if (w<0) w<-1
      llike <- llike + w*log(FindingsProbability(net))
      lapply(onodes,RetractNodeFinding)
    })
  llike
}

calcExpTables.NeticaBN <- function (net, cases, Estepit=1,
                                    tol=sqrt(.Machine$double.eps)) {
  pnodes <- NetworkNodesInSet(net,"pnodes")
  LearnCPTs(cases,pnodes,"EM",Estepit,tol)
}


maxCPTParam.NeticaNode <- function (node, Mstepit=3,
                                    tol=sqrt(.Machine$double.eps)) {
  ## Get the posterior pseudo-counts by multiplying each row of the
  ## CPT by its experience.
  counts <- sweep(node[[]],1,NodeExperience(node),"*")
  mapDPC(counts,ParentStates(node),NodeStates(node),
         PnodeLnAlphas(node), PnodeBeta(node),
         PnodeRules(node),PnodeLink(node),
         PnodeLinkScale(node),
         control=list(reltol=tol,maxits=Mstepit)
         )
}
