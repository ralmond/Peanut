###
## These are functions which it is pretty safe to assume that every
## Bayes net package has.  We can put them here to make generics so
## the conforming Bayes net implementations will implement them.

PnodeName <- function (node)
  UseMethod("PnodeName")

PnodeStates <- function (node)
  UseMethod("PnodeStates")

PnodeNumStates <- function (node)
  UseMethod("PnodeNumStates")

PnodeParents <- function (node)
  UseMethod("PnodeParents")

PnodeParentNames <- function (node)
  UseMethod("PnodeParentNames")

PnodeNumParents <- function (node)
  UseMethod("PnodeNumParents")

PnetFindNode <- function(net, name)
  UseMethod("PnetFindNode")
