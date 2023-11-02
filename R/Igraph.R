## Maybe need to add position information here.


pnet2Igraph <- function (net) {
 ig <- igraph::make_empty_graph()
 for (nd in PnetPnodes(net)) {
   ig <- ig + igraph::vertex(name=PnodeName(nd),title=PnodeTitle(nd),
                     sets=list(PnodeLabels(nd)),
                     states=list(PnodeStates(nd)))
 }
 for (nd in PnetPnodes(net)) {
   if (PnodeNumParents(nd)==0L) next
   edg <- as.list(as.vector(rbind(PnodeParentNames(nd),PnodeName(nd))))
   class(edg) <- "igraph.edge"
   ig <- ig + edg
 }
 ig
}
                     

  
  
