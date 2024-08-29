## EM Tables -- tools for displaying the effect of evidence models.

allCombinations <- function (nodes, includeNULLs=TRUE) {
  statelist <- lapply(nodes,PnodeStates)
  if (isTRUE(includeNULLs))
    statelist <- lapply(statelist,function(sl) c(NA,sl))
  result <- do.call(expand.grid,statelist)
  if (isTRUE(includeNULLs)) return(result[-1,,drop=FALSE])
  result
}
  
buildEMTable <- function(cm,em,stats=buildStats(PnetPnodes(cm),"PnodeEAP"),
                           combos=allCombinations(PnetOnodes(em))) {
  onodes <- PnetAdjoin(cm,em)
  PnetCompile(cm)
  blstats <- calcStats(stats,cm)
  result <- data.frame(as.list(blstats))
  result[1+1:nrow(combos),] <- NA
  combos <- rbind(rep(NA,ncol(combos)),combos)
  for (i in 2:nrow(combos)) {
    setNodes(cm,combos,i)
    result[i,] <- calcStats(stats,cm)
  }
  cbind(combos,result)
}
    
  

  