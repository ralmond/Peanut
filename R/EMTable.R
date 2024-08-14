## EM Tables -- tools for displaying the effect of evidence models.

allCombinations <- function (em, obs=PnetOnodes(em), includeNULLs=TRUE) {
  statelist <- lapply(obs,PnodeStates)
  if (isTRUE(includeNULLs))
    statelist <- lapply(statelist,function(sl) c(NA,sl))
  result <- do.call(expand.grid,statelist)
  if (isTRUE(includeNULLs)) return(result[-1,,drop=FALSE])
  result
}
  
buildEMTable <- function(cm,em,stats=buildStats(cm,PnodeEAP),
                           combos=allCombinations(obs)) {
  
  
}
  