
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


is.PnodeContinuous <- function (node)
  UseMethod("is.PnodeContinuous")
setGeneric("is.PnodeContinuous")


PnodeStateDF <- function (node)
  UseMethod("PnodeStateDF")
setGeneric("PnodeStateDF")

"PnodeStateDF<-" <- function (node,value)
  UseMethod("PnodeStateDF<-")
setGeneric("PnodeStateDF<-")

PnodeStateDF.default <- function (node) {
  data.frame(Name=PnodeStates(node),Title=PnodeStateTitles(node),
             Description=PnodeStateDescriptions(node),
             Value=PnodeStateValues(node),
             Bounds=PnodeStateBounds(node),
             row.names=PnodeStates(node))
}

"PnodeStateDF<-.default" <- function (node,value) {
  PnodeStates(node) <- value$Name
  PnodeStateTitles(node) <- value$Title
  PnodeStateDescriptions(node) <- value$Description
  PnodeStateValues(node) <- value$Value
  PnodeStateBounds(node) <- as.numeric(value[,5:6])
  node
}


PnodeAddStates <- function(node, newstates, after=Inf)
  UseMethod("PnodeAddStates")
setGeneric("PnodeAddStates")

PnodeAddStates <- function(node, newstates, after=Inf) {
  sdf <- PnodeStateDF(node)
  K <- nrow(sdf)
  if (!is.numeric(after)) {
    pm <- pmatch(after,sdf$Name)
    if (is.na(pm))
      stop("Position ",after," is not a number or a state name in ",
           PnodeName(node))
    after <- pm
  }
  K1 <- K+length(newstates)
  sdf <- sdf[1L:K1,]
  sdf$Name[(K+1L):K1]<-newnames
  if (after < 1L) {
    sdf <- sdf[c((K+1L):K1,1L:K),]
  } else if (after > K) {
    ## Already in right order
  } else {
    sdf <- sdf[c(1L:after,(K+1L):K1,(after+1L):K),]
  }
  row.names(sdf) <- sdf$Name
  PnodeStateDF(node) <- sdf
}
  


PnodeRemoveStates <- function(node, whichstates)
  UseMethod("PnodeRemoveStates")
setGeneric("PnodeRemoveStates")
PnodeRemoveStates.default <- function(node, whichstates) {
  sdf <- PnodeStateDF(node)
  if (!is.numeric(whichstates)) {
    pm <- pmatch(whichstates,sdf$Name)
    if (any(is.na(pm)))
      stop("Did not find states ",whichstates, " in node ",PnodeName(node))
    whichstates <- pm
  }
  PnodeStateDF(node) <- sdf[-whichstates,]
}



PnodeReorderStates <- function(node, neworder)
  UseMethod("PnodeReorderStates")
setGeneric("PnodeReorderStates")
PnodeReorderStates.default <- function(node, neworder) {
  sdf <- PnodeStateDF(node)
  if (!is.numeric(neworder)) {
    pm <- pmatch(neworder,sdf$Name)
    if (any(is.na(pm)))
      stop("Did not find states ",whichstates, " in node ",PnodeName(node))
    neworder <- pm
  }
  PnodeStateDF(node) <- sdf[neworder,]
}
  
PnodeStateCuts <- function (node)
  UseMethod("PnodeStateCuts")
setGeneric("PnodeStateCuts")

PnodeStateCuts.default <- function (node) {
  bounds <- PnodeStateBounds(node)
  c(bounds[1,1],bounds[,2])
}

"PnodeStateCuts<-" <- function (node,value)
  UseMethod("PnodeStateCuts<-")
setGeneric("PnodeStateCuts<-")
"PnodeStateCuts<-.default" <- function (node,value) {
  PnodeStateBounds(node) <- cbind(value[-length(value)],value[-1])
}

PnodeSetQuad <- function(node,cuts,vals=(cuts[-length(cuts)]+cuts[-1])/2)
  UseMethod("PnodeSetQuad")
setGeneric("PnodeSetQuad")

PnodeSetQuad.default <- function(node,cuts,vals=(cuts[-length(cuts)]+cuts[-1])/2) {
  bounds <- cbind(cuts[-length(cuts)],cuts[-1])
  names <- paste0("Q",1:length(cuts))
  titles <- paste("[",bounds[,1],",",bounds[,2],")")
  PnodeStates(node)<-names
  PnodeStateTitles(node)<-titles
  PnodeStateValues(node) <- values
  PnodeStateBounds(node) <- bounds
}



PnodeDistQuad <- function(node,K,Fp=qnorm) {
  PnodeSetQuad(node,Fp((0:K)/K),Fp((.5+1:(K-1))/K))
}
  
