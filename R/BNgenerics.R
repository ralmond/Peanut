################################
## Generic objects.  These are implemented as class unions, so that
## they can added to by implementing classes.

setClassUnion("Pnode","NULL")
setClassUnion("Pnet","NULL")


###
## These are functions which it is pretty safe to assume that every
## Bayes net package has.  We can put them here to make generics so
## the conforming Bayes net implementations will implement them.

PnodeName <- function (node)
  UseMethod("PnodeName")
setGeneric("PnodeName")

"PnodeName<-" <- function (node,value)
  UseMethod("PnodeName<-")
setGeneric("PnodeName<-")

PnodeTitle <- function (node)
  UseMethod("PnodeTitle")
setGeneric("PnodeTitle")

"PnodeTitle<-" <- function (node,value)
  UseMethod("PnodeTitle<-")
setGeneric("PnodeTitle<-")


PnodeDescription <- function (node)
  UseMethod("PnodeDescription")
setGeneric("PnodeDescription")

"PnodeDescription<-" <- function (node,value)
  UseMethod("PnodeDescription<-")
setGeneric("PnodeDescription<-")

PnodeLabels <- function (node)
  UseMethod("PnodeLabels")
setGeneric("PnodeLabels")

"PnodeLabels<-" <- function (node,value)
  UseMethod("PnodeLabels<-")
setGeneric("PnodeLabels<-")


PnodeVisPos <- function (node)
  UseMethod("PnodeVisPos")
setGeneric("PnodeVisPos")

"PnodeVisPos<-" <- function (node,value)
  UseMethod("PnodeVisPos")
setGeneric("PnodeVisPos<-")

##### States


PnodeStates <- function (node)
  UseMethod("PnodeStates")
setGeneric("PnodeStates")

"PnodeStates<-" <- function (node,value)
  UseMethod("PnodeStates<-")
setGeneric("PnodeStates<-")

PnodeNumStates <- function (node)
  UseMethod("PnodeNumStates")
setGeneric("PnodeNumStates")

PnodeNumStates.default <- function (node) length(PnodeStates(node))

PnodeStateTitles <- function (node)
  UseMethod("PnodeStateTitles")
setGeneric("PnodeStateTitles")

"PnodeStateTitles<-" <- function (node,value)
  UseMethod("PnodeStateTitles<-")
setGeneric("PnodeStateTitles<-")

PnodeStateDescriptions <- function (node)
  UseMethod("PnodeStateDescriptions")
setGeneric("PnodeStateDescriptions")

"PnodeStateDescriptions<-" <- function (node,value)
  UseMethod("PnodeStateDescriptions<-")
setGeneric("PnodeStateDescriptions<-")

PnodeStateValues <- function (node)
  UseMethod("PnodeStateValues")
setGeneric("PnodeStateValues")

"PnodeStateValues<-" <- function (node,value)
  UseMethod("PnodeStateValues<-")
setGeneric("PnodeStateValues<-")


PnodeStateBounds <- function (node)
  UseMethod("PnodeStateBounds")
setGeneric("PnodeStateBounds")

"PnodeStateBounds<-" <- function (node,value)
  UseMethod("PnodeStateBounds<-")
setGeneric("PnodeStateBounds<-")


isPnodeContinuous <- function (node)
  UseMethod("isPnodeContinuous")
setGeneric("isPnodeContinuous")

PnodeEvidence <- function (node)
  UseMethod("PnodeEvidence")
setGeneric("PnodeEvidence")

"PnodeEvidence<-" <- function (node,value)
  UseMethod("PnodeEvidence<-")
setGeneric("PnodeEvidence<-")


#### Parents

PnodeParents <- function (node)
  UseMethod("PnodeParents")
setGeneric("PnodeParents")

"PnodeParents<-" <- function (node,value)
  UseMethod("PnodeParents<-")
setGeneric("PnodeParents<-")

PnodeParentNames <- function (node)
  UseMethod("PnodeParentNames")
setGeneric("PnodeParentNames")

PnodeParentNames.default <- function (node)
  sapply(PnodeParents(node),PnodeName)

PnodeParentStates <- function(node)
  UseMethod("PnodeParentStates")
setGeneric("PnodeParentStates")

PnodeParentStates.default <- function (node)
  lapply(PnodeParents(node),PnodeStates)


PnodeNumParents <- function (node)
  UseMethod("PnodeNumParents")
setGeneric("PnodeNumParents")

PnodeNumParents.default <- function (node) length(PnodeParents)

###############################################################
## Pnets
PnetName <- function (net)
  UseMethod("PnetName")
setGeneric("PnetName")
setMethod("PnetName","NULL",function(net) return("NULL"))


"PnetName<-" <- function (net, value)
  UseMethod("PnetName<-")
setGeneric("PnetName<-")

PnetTitle <- function (net)
  UseMethod("PnetTitle")
setGeneric("PnetTitle")

"PnetTitle<-" <- function (net, value)
  UseMethod("PnetTitle<-")
setGeneric("PnetTitle<-")

## The HUB is the name of the CM for an EM, or "" for an CM.
PnetHub <- function (net)
  UseMethod("PnetHub")
setGeneric("PnetHub")

## Value could be the actual model or its name.
"PnetHub<-" <- function (net, value)
  UseMethod("PnetHub<-")
setGeneric("PnetHub<-")

PnetPathname <- function (net)
  UseMethod("PnetPathname")
setGeneric("PnetPathname")

"PnetPathname<-" <- function (net, value)
  UseMethod("PnetPathname<-")
setGeneric("PnetPathname<-")

PnetDescription <- function (net)
  UseMethod("PnetDescription")
setGeneric("PnetDescription")

"PnetDescription<-" <- function (net, value)
  UseMethod("PnetDescription<-")
setGeneric("PnetDescription<-")




PnetFindNode <- function(net, name)
  UseMethod("PnetFindNode")
setGeneric("PnetFindNode")

## This should return a list of two objects, a string indicating the
## type and a serialized version of the object.
PnetSerialize <- function(net) UseMethod("PnetSerialize")
setGeneric("PnetSerialize")


setGeneric("unserializePnet", function(factory,data)
  standardGeneric("unserializePnet"))
## Uses fake UseMethod protocol.
PnetUnserialize <- function (serial) {
  if (is.null(serial$factory)) {
    stop("Factory not supplied for network ",serial$name)
  }
  factory <- get(serial$factory)
  if (is.null(factory)) {
    stop("Could not find factory ",serial$factory)
  }
  unserializePnet(factory,serial)
}

PnetCompile <- function(net)
  UseMethod("PnetCompile")
setGeneric("PnetCompile")


PnetCanvas <- function (net)
  UseMethod("PnetCanvas")
setGeneric("PnetCanvas")

PnetCanvas.default <- function (net) {
  coords <- sapply(PnetPnodes(net),PnodeVisPos)
  coords <- cbind(coords,c(0,0))
  ur <- apply(coords,1,max,na.rm=TRUE)
  ll <- apply(coords,1,min,na.rm=TRUE)
  list(ll=ll,ur=ur)
}   



"PnetCanvas<-" <- function (net,value)
  UseMethod("PnetCanvas<-")
setGeneric("PnetCanvas<-")

"PnetCanvas<-.default" <- function (net, value)
  stop("Setting Network Canvas not supported for graphs of class ",class(net))

