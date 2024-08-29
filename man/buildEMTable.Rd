\name{buildEMTable}
\alias{buildEMTable}
\title{Builds a Table of Evidence for an Evidence Model}
\description{
  
  An evidence model will update the estimates of competency 
  according to the evidence it recieves.  This produces a table 
  in which the rows correspond to a possible pattern of evidence 
  (configuration of evidence model variables) and columns to 
  statistic values.  This provides a summary of how the evidence
  model is working.
  
}
\usage{
buildEMTable(cm, em, stats = buildStats(PnetPnodes(cm), PnodeEAP), combos = allCombinations(PnetOnodes(em)))
}
\arguments{
  \item{cm}{A \code{\linkS4class{Pnet}} object which contains the 
    competency model.}
  \item{em}{A \code{\linkS4class{Pnet}} object which contains the 
    evidence model.}
  \item{stats}{A list of \code{\linkS4class{Statitic}} objects 
    which correspond to the columns of the output table.}
  \item{combos}{A data frame whose columns represent observable 
    variables, and whose cells contain values for those variables. 
    See details.}
}
\details{

  Each row of the \code{combos} table represents a possible evidence 
  pattern for the evidence model.  The goal is to see what
  the impact of observing that evidence would have on the 
  competency estimates.
  
  To calculate this, the \code{cm} and \code{em} networks are 
  joined to make a motif for the evidence model.  Then all of 
  the statistics are calculated against this baseline motif, 
  yielding a vector of baseline statistics.
  
  Next, for each row of the \code{combos} data frame, the 
  observable node in the motif are set to the corresponding value,
  or set to unobserved if the value in the cell is \code{NA}.  
  Then the statistics are calculated, yielding a new row for the 
  output table.
  
  The function \code{\link{allCombinations}} can be used to make
  a table of all the possible configurations for a set of 
  observable variables.  As the baseline row corresponds to the 
  no variable set (all \code{NA}) row, this should be omitted 
  from the combination table.
}
\value{

  A data frame produced by joining the columns of the 
  \code{combos} table and the statistics.  The list of calculated 
  statistics is passed through \code{\link{flattenStats}}, so if 
  \code{\link{PnodeMargin}} statistic is used, then one column is 
  included for each state of the variable.  The number of rows is 
  one more than the number of rows in the \code{combos} argument 
  as a first baseline row is always added to the output.

}
\author{Russell Almond}
\seealso{
  \code{\link{PnetOnodes}}, \code{\linkS4class{Statistic}},
  \code{\link{PnetAdjoin}}, \code{\link{allCombinations}}
}
\examples{
\dontrun{
library(PNetica) ## Needs PNetica
sess <- NeticaSession()
startSession(sess)

netman1 <- read.csv(system.file("auxdata", "Mini-PP-Nets.csv", 
                                package="Peanut"),
                    row.names=1,stringsAsFactors=FALSE)

nodeman1 <- read.csv(system.file("auxdata", "Mini-PP-Nodes.csv", 
                                package="Peanut"),
                     row.names=1,stringsAsFactors=FALSE)

## Network and node warehouse, to create networks and nodes on demand.
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name",
            address=system.file("testnets",package="PNetica"))

Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)
CM <- WarehouseSupply(Nethouse,"miniPP_CM")

EM1 <- WarehouseSupply(Nethouse,"PPcompEM")
Motif1 <- WarehouseCopy(Nethouse,CM,"CM_EM1")

buildEMTable(Motif1,EM1)

WarehouseFree(Nethouse,EM1)
WarehouseFree(Nethouse,Motif1)


WarehouseFree(Nethouse,CM)

setwd(curd)
}
}
\keyword{ manip }
\concept{ evidence }
\concept{ testing }

