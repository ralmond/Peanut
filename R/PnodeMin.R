


PnodeMin <- function (name,states=c("Yes","No"), parents=list(),
                      title=title(), description=character(),
                      labels=character(), continuous=FALSE,
                      tvals=effectiveThetas(length(states)),
                      rules="Compensatory", link="partialCredit",
                      alphas=1, betas=1, linkScale=NA, QQ=1,
                      PriorWeight=NA,CPT=NULL) {
  pnames <- names(parents)
  if (is.null(pnames))
    pnames <- sapply(parents,PnodeName)
  if (missing(alphas)) {
    alphas <- lapply(rules, function(r) {
      a <- 1
      if (!isOffsetRule(r)) {
        a <- rep(1,length(pnames))
        names(a) <- pnames
      }
      a})
    if (length(alphas)==1L) alphas <- alphas[[1]]
  }
  if (missing(beta)) {
    betas <- lapply(rules, function(r) {
      b <- 0
      if (!isOffsetRule(r)) {
        b <- rep(0,length(pnames))
        names(b) <- pnames
      }
      b})
    if (length(betas)==1L) betas <- betas[[1]]
  }
  if (link=="gradedResponse") {
    if (!is.list(betas))
      betas <- rep(list(betas),length(states)-1)
  }
  node <- list(name=name,states=states,parents=parents,
               tvals=tvals,rules=rules,link=link,
               alphas=alphas, betas=betas, linkScale=linkScale,
               QQ = QQ, PriorWeight=PriorWeight, CPT=CPT)
  class(node) <- c("Pnode","PnodeMin")
  node
}

PnodeName.PnodeMin <- function (node) node$name
"PnodeName<-.PnodeMin" <- function (node, value) {
  node$name <- value
  node
}
PnodeParents.PnodeMin <- function (node) node$parents
"PnodeParents<-.PnodeMin" <- function (node, value) {
  node$parents <- value
  node
}
PnodeStates.PnodeMin <- function (node) node$states
"PnodeStates<-.PnodeMin" <- function (node, value) {
  node$states <- value
  node
}
PnodeStateValues.PnodeMin <- function (node) node$tvals
"PnodeStateValues<-.PnodeMin" <- function (node, value) {
  node$tvals <- value
  node
}
PnodeRules.PnodeMin <- function (node) node$rules
"PnodeRules<-.PnodeMin" <- function (node, value) {
  node$rules <- value
  node
}
PnodeLink.PnodeMin <- function (node) node$link
"PnodeLink<-.PnodeMin" <- function (node, value) {
  node$link <- value
  node
}
PnodeAlphas.PnodeMin <- function (node) node$alphas
"PnodeAlphas<-.PnodeMin" <- function (node, value) {
  node$alphas <- value
  node
}
PnodeBetas.PnodeMin <- function (node) node$betas
"PnodeBetas<-.PnodeMin" <- function (node, value) {
  node$betas <- value
  node
}
PnodeLinkScale.PnodeMin <- function (node) node$linkScale
"PnodeLinkScale<-.PnodeMin" <- function (node, value) {
  node$linkScale <- value
  node
}
PnodeQ.PnodeMin <- function (node) node$QQ
"PnodeQ<-.PnodeMin" <- function (node, value) {
  node$QQ <- value
  node
}

PnodePriorWeight.PnodeMin <- function (node) node$PriorWeight
"PnodePriorWeight<-.PnodeMin" <- function (node, value) {
  node$PriorWeight <- value
  node
}


PnodeBuildTable.PnodeMin (node) {
  node$CPT <- calcDPCTable(PnodeParentStates(node),PnodeStates(node),
                          PnodeLnAlphas(node), PnodeBetas(node),
                          PnodeRules(node),PnodeLink(node),
                          PnodeLinkScale(node),PnodeParentTvals(node))
  ## NodeExperience(node) <- GetPriorWeight(node)
  invisible(node)
}



parsePnode(json) {

}

serializePnode
