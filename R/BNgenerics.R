###
## These are functions which it is pretty safe to assume that every
## Bayes net package has.  We can put them here to make generics so
## the conforming Bayes net implementations will implement them.

PnodeName <- function (node)
  UseMethod("PnodeName")

"PnodeName<-" <- function (node,value)
  UseMethod("PnodeName<-")

PnodeTitle <- function (node)
  UseMethod("PnodeTitle")

"PnodeTitle<-" <- function (node,value)
  UseMethod("PnodeTitle<-")


PnodeDescription <- function (node)
  UseMethod("PnodeDescription")

"PnodeDescription<-" <- function (node,value)
  UseMethod("PnodeDescription<-")

PnodeLabels <- function (node)
  UseMethod("PnodeLabels")

"PnodeLabels<-" <- function (node,value)
  UseMethod("PnodeLabels<-")

##### States


PnodeStates <- function (node)
  UseMethod("PnodeStates")

"PnodeStates<-" <- function (node,value)
  UseMethod("PnodeStates<-")

PnodeNumStates <- function (node)
  UseMethod("PnodeNumStates")

PnodeStateTitles <- function (node)
  UseMethod("PnodeStateTitles")

"PnodeStateTitles<-" <- function (node,value)
  UseMethod("PnodeStateTitles<-")

PnodeStateDescriptions <- function (node)
  UseMethod("PnodeStateDescriptions")

"PnodeStateDescriptions<-" <- function (node,value)
  UseMethod("PnodeStateDescriptions<-")

PnodeStateValues <- function (node)
  UseMethod("PnodeStateValues")

"PnodeStateValues<-" <- function (node,value)
  UseMethod("PnodeStateValues<-")

#### Parents

PnodeParents <- function (node)
  UseMethod("PnodeParents")

"PnodeParents<-" <- function (node)
  UseMethod("PnodeParents<-")

PnodeParentNames <- function (node)
  UseMethod("PnodeParentNames")

PnodeNumParents <- function (node)
  UseMethod("PnodeNumParents")

## Pnets
PnetName <- function (net)
  UseMethod("PnetName")

"PnetName<-" <- function (net, value)
  UseMethod("PnetName<-")

PnetTitle <- function (net)
  UseMethod("PnetTitle")

"PnetTitle<-" <- function (net, value)
  UseMethod("PnetTitle<-")

## The HUB is the name of the CM for an EM, or "" for an CM.
PnetHub <- function (net)
  UseMethod("PnetHub")

## Value could be the actual model or its name.
"PnetHub<-" <- function (net, value)
  UseMethod("PnetHub<-")

PnetPathname <- function (net)
  UseMethod("PnetPathname")

"PnetPathname<-" <- function (net, value)
  UseMethod("PnetPathname<-")

PnetDescription <- function (net)
  UseMethod("PnetDescription")

"PnetDescription<-" <- function (net, value)
  UseMethod("PnetDescription<-")




PnetFindNode <- function(net, name)
  UseMethod("PnetFindNode")

