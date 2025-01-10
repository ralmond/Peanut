## EM Tables -- tools for displaying the effect of evidence models.

allCombinations <- function (nodes, includeNULLs=TRUE) {
  if (!is.list(nodes)) nodes <- list(nodes)
  statelist <- lapply(nodes,function(n) unname(PnodeStates(n)))
  if (isTRUE(includeNULLs))
    statelist <- lapply(statelist,function(sl) c(NA,sl))
  result <- do.call(expand.grid,statelist)
  names(result) <- sapply(nodes,PnodeName)
  if (isTRUE(includeNULLs)) return(result[-1,,drop=FALSE])
  result
}
  
buildEMTable <- function(cm,em,stats=buildStats(PnetPnodes(cm),"PnodeEAP"),
                           combos=allCombinations(PnetOnodes(em))) {
  cm1 <- RNetica::local_copy_nets(cm,paste0(PnetName(cm),"_emtest"))
  onodes <- PnetAdjoin(cm1,em)
  PnetCompile(cm1)
  if (!is.list(stats)) stats <- list(stats)
  blstats <- calcStats(stats,cm1)
  result <- data.frame(as.list(blstats))
  names(result) <- sapply(stats,StatName)
  row.names(result)=NULL
  result[1+1:nrow(combos),] <- NA
  combos <- rbind(baseline=rep(NA,ncol(combos)),combos)
  row.names(combos) <- NULL
  for (i in 2:nrow(combos)) {
    setNodes(cm1,combos,i)
    result[i,] <- calcStats(stats,cm1)
  }
  cbind(combos,result)
}
    
  

  