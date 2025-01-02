test_that("Statistic",{
  astat <- Statistic("PnodeMargin","theta","Pr(theta)")
  expect_s4_class(astat,"Statistic")
})

test_that("show Statistic ",{
  astat <- Statistic("PnodeMargin","theta","Pr(theta)")
  expect_output(show(astat),"Statistic: { Pr(theta) }",fixed=TRUE)
})


test_that("StatName",{
  astat <- Statistic("PnodeMargin","theta","Pr(theta)")
  expect_equal(StatName(astat),"Pr(theta)")
  expect_equal(StatNode(astat),"theta")
  expect_equal(StatFun(astat),"PnodeMargin")
})

test_that("calcStat",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  mt <- Statistic("PnodeEAP","theta","E[theta]")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  expect_equal(calcStat(mt,irt10),0,tolerance=.0001)
  
  irt10.copy <- RNetica::local_copy_nets(irt10,"irt10_copy")
  PnetCompile(irt10.copy)
  PnodeEvidence(PnetOnodes(irt10.copy)$item1) <- "Correct"
  PnodeEvidence(PnetOnodes(irt10.copy)$item2) <- "Correct"
  expect_gt(calcStat(mt,irt10.copy),0)
  ## Original net should not be affected
  expect_equal(calcStat(mt,irt10),0,tolerance=.0001)
  
})

test_that("PnodeMargin",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  
  expect_equal(PnodeMargin("theta",irt10),
               c(VH=.2,High=.2,Mid=.2,Low=.2,VL=.2),
               tolerance=.00001)
    
})

test_that("PnodeEAP",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  
  expect_equal(PnodeEAP("theta",irt10),0,tolerance=.0001)
  
})

test_that("PnodeSD",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  
  expect_equal(PnodeSD("theta",irt10),.8757,tolerance=.0001)
  
})

test_that("PnodeMedian",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  
  expect_equal(PnodeMedian("theta",irt10),"Mid")
  
})

test_that("PnodeMode",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  
  expect_equal(PnodeMode("theta",irt10),"VH")
  
})

test_that("buildStats names",{
  expect_setequal(buildStats(paste0("theta",1:2)),
                 list(Statistic("PnodeEAP","theta1"),
                      Statistic("PnodeEAP","theta2"),
                      Statistic("PnodeMargin","theta1"),
                      Statistic("PnodeMargin","theta2"),
                      Statistic("PnodeMedian","theta1"),
                      Statistic("PnodeMedian","theta2"),
                      Statistic("PnodeSD","theta1"),
                      Statistic("PnodeSD","theta2"),
                      Statistic("PnodeMode","theta1"),
                      Statistic("PnodeMode","theta2")))
    expect_equal(buildStats("theta","PnodeMode"),
               list(theta=Statistic("PnodeMode","theta")))
  
})


test_that("buildStats nodes",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  lang <- RNetica::local_RNetica_net("LanguagePM.dne")
  nodes <- RNetica::NetworkAllNodes(lang)
  
  stats <- buildStats(nodes,fun="PnodeMargin")
  
  expect_length(stats,length(nodes))
  expect_equal(names(stats),names(nodes))
  expect_equal(sapply(stats,StatNode),sapply(nodes,PnodeName))
  expect_equal(sapply(stats,StatFun),sapply(nodes,function(n) "PnodeMargin"))
  
})

test_that("flattenStats",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  slist <- list("Physics_EAP"=.3,
                "Physics_Margin"=c("High"=.5,"Medium"=.3,"Low"=.2),
                "Physics_Mode"="High")
  expect_equal(flattenStats(slist),
               c("Physics_EAP"="0.3",
                 "Physics_Margin.High"="0.5",
                 "Physics_Margin.Medium"="0.3",
                 "Physics_Margin.Low"="0.2",
                 "Physics_Mode"="High"))
  expect_equal(flattenStats(slist[1:2]),
               c("Physics_EAP"=0.3,
                 "Physics_Margin.High"=0.5,
                 "Physics_Margin.Medium"=0.3,
                 "Physics_Margin.Low"=0.2)
  )
  
})

test_that("calcStats",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  
  stats1 <- buildStats("theta",c("PnodeEAP","PnodeMedian",
                                "PnodeMode"))
  expect_equal(calcStats(stats1,irt10),
               c("PnodeEAP.theta"=0,"PnodeMedian.theta"="Mid",
                 "PnodeMode.theta"="VH"))
  stats2 <- buildStats("theta",c("PnodeEAP","PnodeMargin",
                                 "PnodeSD"))
  expect_equal(calcStats(stats2,irt10),
               c(PnodeEAP.theta=0,PnodeMargin.theta.VH=.2,
                 PnodeMargin.theta.High=.2,
                 PnodeMargin.theta.Mid=.2,
                 PnodeMargin.theta.Low=.2,
                 PnodeMargin.theta.VL=.2,
                 PnodeSD.theta=sqrt(sum(effectiveThetas(5)^2)/5)),
               tolerance=.00001)
})

test_that("setNodes",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  item1 <- PnetFindNode(irt10,"item1")
  item2 <- PnetFindNode(irt10,"item2")
  item3 <- PnetFindNode(irt10,"item3")
  
  vals1 <- data.frame(item1=c("Correct","Correct",NA),
                      item2=c("Correct","Incorrect","Correct"))
  
  setNodes(irt10,vals1)
  expect_equal(PnodeEvidence(item1),
               c(Correct="Correct"))
  expect_equal(PnodeEvidence(item2),
               c(Correct="Correct"))
  expect_equal(PnodeEvidence(item3),NA)
  expect_equal(PnodeMargin(item1),
               c(Correct=1,Incorrect=0))
  expect_equal(PnodeMargin(item2),
               c(Correct=1,Incorrect=0))
  expect_gt(PnodeMargin(item3)[1],0)
  expect_lt(PnodeMargin(item3)[1],1)
  
  setNodes(irt10,vals1,2)
  expect_equal(PnodeEvidence(item1),
               c(Correct="Correct"))
  expect_equal(PnodeEvidence(item2),
               c(Incorrect="Incorrect"))
  expect_equal(PnodeEvidence(item3),NA)
  expect_equal(PnodeMargin(item1),
               c(Correct=1,Incorrect=0))
  expect_equal(PnodeMargin(item2),
               c(Correct=0,Incorrect=1))
  expect_gt(PnodeMargin(item3)[1],0)
  expect_lt(PnodeMargin(item3)[1],1)
  
  setNodes(irt10,vals1,3)
  expect_equal(PnodeEvidence(item1),NA)
  expect_equal(PnodeEvidence(item2),
               c(Correct="Correct"))
  expect_equal(PnodeEvidence(item3),NA)
  expect_gt(PnodeMargin(item1)[1],0)
  expect_lt(PnodeMargin(item1)[1],1)
  expect_equal(PnodeMargin(item2),
               c(Correct=1,Incorrect=0))
  expect_gt(PnodeMargin(item3)[1],0)
  expect_lt(PnodeMargin(item3)[1],1)

  
  

})
