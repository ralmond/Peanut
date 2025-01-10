
setClass("Statistic",
         slots=c(name="character",
                 node="character",
                 fun="character"))

Statistic <- function (fun,node,name=sprintf("%s(%s)",fun,node),...) {
  new("Statistic",name=name,node=node,fun=fun,...)
}

setMethod("toString","Statistic",function (x,...) {
  sprintf("Statistic: { %s }",x@name)
  })
setMethod("show","Statistic",function(object) {
  cat(toString(object),"\n")
})

setGeneric("StatName",function(x) standardGeneric("StatName"))
setMethod("StatName","Statistic", function(x) x@name)

setGeneric("StatNode",function(x) standardGeneric("StatNode"))
setMethod("StatNode","Statistic", function(x) x@node)

setGeneric("StatFun",function(x) standardGeneric("StatFun"))
setMethod("StatFun","Statistic", function(x) x@fun)


setGeneric("calcStat",function (stat,net) standardGeneric("calcStat"))
setMethod("calcStat",c("Statistic"),
          function (stat,net) {
            if (!is.Pnet(net)) 
              stop("Second argument is not a Pnet.")
            node <- PnetFindNode(net,stat@node)
            do.call(stat@fun,list(node,net))
          })


setGeneric("PnodeMargin",function(node,net=NULL) standardGeneric("PnodeMargin"))
setGeneric("PnodeEAP",function(node,net=NULL) standardGeneric("PnodeEAP"))
setGeneric("PnodeSD",function(node,net=NULL) standardGeneric("PnodeSD"))
setGeneric("PnodeMedian",function(node,net=NULL) standardGeneric("PnodeMedian"))
setGeneric("PnodeMode",function(node,net=NULL) standardGeneric("PnodeMode"))

setMethod("PnodeMargin",c("character"),
          function(node,net=NULL) {
            if (missing(net) || !is.Pnet(net)) {
              stop("When node is a name, network argument must be a Pnet.")
            }
            anode <- PnetFindNode(net,node)
            if (is.null(anode)) {
              stop("Node ",node," not found in ",PnetName(net),".")
            }
            PnodeMargin(anode,net)
          })
setMethod("PnodeEAP",c("character"),
          function(node,net=NULL) {
            if (missing(net) || !is.Pnet(net)) {
              stop("When node is a name, network argument must be a Pnet.")
            }
            anode <- PnetFindNode(net,node)
            if (is.null(anode)) {
              stop("Node ",node," not found in ",PnetName(net),".")
            }
            PnodeEAP(anode,net)
          })
setMethod("PnodeSD",c("character"),
          function(node,net=NULL) {
            if (missing(net) || !is.Pnet(net)) {
              stop("When node is a name, network argument must be a Pnet.")
            }
            anode <- PnetFindNode(net,node)
            if (is.null(anode)) {
              stop("Node ",node," not found in ",PnetName(net),".")
            }
            PnodeSD(anode,net)
          })
setMethod("PnodeMedian",c("character"),
          function(node,net=NULL) {
            if (missing(net) || !is.Pnet(net)) {
              stop("When node is a name, network argument must be a Pnet.")
            }
            anode <- PnetFindNode(net,node)
            if (is.null(anode)) {
              stop("Node ",node," not found in ",PnetName(net),".")
            }
            PnodeMedian(anode,net)
          })
setMethod("PnodeMode",c("character"),
          function(node,net=NULL) {
            if (missing(net) || !is.Pnet(net)) {
              stop("When node is a name, network argument must be a Pnet.")
            }
            anode <- PnetFindNode(net,node)
            if (is.null(anode)) {
              stop("Node ",node," not found in ",PnetName(net),".")
            }
            PnodeMode(anode,net)
          })

          

buildStats <- function(nodes,fun=c("PnodeEAP","PnodeMargin",
                                   "PnodeMedian","PnodeSD",
                                   "PnodeMode")) {
  if (length(fun) > 1L) {
    do.call("c",sapply(fun,function(f) buildStats(nodes,f))) 
  } else {
    if (is.character(nodes))
        return(sapply(nodes,function(nd) Statistic(fun,nd)))
    if (!is.list(nodes)) nodes <- list(nodes)
    sapply(nodes,function(nd) Statistic(fun,PnodeName(nd)))
  }
}

flattenStats <- function(statlist) {
    do.call("c",statlist)
}

calcStats <- function (stats,net) {
  if (!is.list(stats)) stats <- list(stats)
  result <- sapply(stats,function(h) calcStat(h,net))
  if (is.list(result)) result <- flattenStats(result)
  result
}

setNodes <- function (net,values,row=1) {
  for (nname in names(values)) {
    node <- PnetFindNode(net,nname)
    if (is.null(node)) stop ("Could not find node named ",nname)
    PnodeEvidence(node) <- values[row,nname]
  }
  invisible(net)
}