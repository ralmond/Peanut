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

isPnodeContinuous <- function (node)
  UseMethod("isPnodeContinuous")
setGeneric("isPnodeContinuous")


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

PnodeNumParents <- function (node)
  UseMethod("PnodeNumParents")
setGeneric("PnodeNumParents")

## Pnets
PnetName <- function (net)
  UseMethod("PnetName")
setGeneric("PnetName")

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
  if (!is.null(serial$factory)) {
    factory <- get(serial$factory)
    if (is.null(factory)) {
      stop("Could not find factory ",serial$factory)
    }
    unserializePnet(factory,serial)
  } else {
    if (is.null(serial$type)) {
      stop("Neither type or factory supplied.")
    }
    do.call(paste("PnetUnserialize",serial$type,sep="."),serial)
  }
}



