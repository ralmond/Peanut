
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

setGeneric("name",function(x) standardGeneric("name"))
setMethod("name","Statistic", function(x) x@name)

setGeneric("calcStat",function (stat,net) standardGeneric("calcStat"))
setMethod("calcStat",c("Statistic"),
          function (stat,net) {
            node <- PnetFindNode(net,stat@node)
            do.call(stat@fun,list(net,node))
          })


setGeneric("PnodeMargin",function(net,node) standardGeneric("PnodeMargin"))
setGeneric("PnodeEAP",function(net,node) standardGeneric("PnodeEAP"))
setGeneric("PnodeSD",function(net,node) standardGeneric("PnodeSD"))
setGeneric("PnodeMedian",function(net,node) standardGeneric("PnodeMedian"))
setGeneric("PnodeMode",function(net,node) standardGeneric("PnodeMode"))


