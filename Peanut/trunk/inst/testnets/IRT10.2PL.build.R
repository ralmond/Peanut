## This is a simple 10 item 2PL IRT model for testing the general framework.
## This script builds the networks and data files for running this test.

## Build network.
IRT10.2PL <- CreateNetwork("IRT10_2PL")
PnetPriorWeight(IRT10.2PL) <- 10

theta <- NewDiscreteNode(IRT10.2PL,"theta",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta) <- effectiveThetas(NodeNumStates(theta))
NodeProbs(theta) <- rep(1/NodeNumStates(theta),NodeNumStates(theta))

J <- 10
btrue <- rnorm(J)
lnatrue <- rnorm(J)/sqrt(3)
dump(c("btrue","lnatrue"),"IRT10.2PL.params.R")

items <- NewDiscreteNode(IRT10.2PL,paste("item",1:J,sep=""),
                         c("Correct","Incorrect"))
for (j in 1:J) {
  NodeParents(items[[j]]) <- list(theta)
  NodeLevels(items[[j]]) <- c(1,0)
  NodeSets(items[[j]]) <- c("pnodes","observables")
  PnodeRules(items[[j]]) <- "Compensatory"
  PnodeLink(items[[j]]) <- "partialCredit"
  PnodeLnAlphas(items[[j]]) <- lnatrue[j]
  PnodeBetas(items[[j]]) <- btrue[j]
}
PnetBuildTables(IRT10.2PL)
WriteNetworks(IRT10.2PL,"IRT10.2PL.true.dne")

## Generate some case files.
N <- 200L
rnodes <- c(list(theta),items)
allfile <- paste("IRT10","2PL",N,"all","cas",sep=".")
itemfile <- paste("IRT10","2PL",N,"items","cas",sep=".")
allstream <- CaseFileStream(allfile)
itemstream <- CaseFileStream(itemfile)
rng <- NewNeticaRNG(123456779)
WithOpenCaseStream(allstream,
  WithOpenCaseStream(itemstream,
    WithRNG(rng,
      for (n in 1L:N) {
        GenerateRandomCase(rnodes,rng=rng)
        WriteFindings(rnodes,allstream,n)
        WriteFindings(items,itemstream,n)
        lapply(rnodes,RetractNodeFinding) # Only retract findings for
                                        # generated nodes
    })))

## Okay, now build a base net for learning.
baseb <- 0
baselna <- 0
for (j in 1:J) {
  PnodeLnAlphas(items[[j]]) <- baselna
  PnodeBetas(items[[j]]) <- baseb
}
BuildAllTables(IRT10.2PL)
WriteNetworks(IRT10.2PL,"IRT10.2PL.base.dne")





