


test_that("allCombinations",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  items <- PnetFindNode(irt10,paste0("item",1:3))

  K <- sapply(items,PnodeNumStates)
  nnames <- sapply(items,PnodeName)
  state1 <- sapply(items,function(i) PnodeStates(i)[1])
  
  comb12<- allCombinations(items[1:2],includeNULLs = FALSE)
  expect_equal(names(comb12),nnames[1:2],ignore_attr=TRUE)
  expect_false(any(is.na(comb12)))
  expect_equal(nrow(comb12),prod(K[1:2]))
  expect_equal(sum(comb12[,1]==state1[1]),K[2],ignore_attr=TRUE)
  expect_equal(sum(comb12[,2]==state1[2]),K[1],ignore_attr=TRUE)
  
  comb12<- allCombinations(items[1:2],includeNULLs = TRUE)
  expect_equal(names(comb12),nnames[1:2],ignore_attr=TRUE)
  expect_equal(nrow(comb12),prod(K[1:2]+1)-1)
  expect_equal(sum(comb12[,1]==state1[1],na.rm=TRUE),K[2]+1,ignore_attr=TRUE)
  expect_equal(sum(comb12[,2]==state1[2],na.rm=TRUE),K[1]+1,ignore_attr=TRUE)
  
  comb123 <- allCombinations(items)
  expect_equal(names(comb123),nnames,ignore_attr=TRUE)
  expect_equal(nrow(comb123),prod(K+1)-1)
  
})

test_that("allSingleObs",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  items <- PnetFindNode(irt10,paste0("item",1:3))

  K <- sapply(items,PnodeNumStates)
  nnames <- sapply(items,PnodeName)
  
      
  comb12<- allSingleObs(items[1:2])
  expect_equal(names(comb12),nnames[1:2],ignore_attr=TRUE)
  expect_equal(nrow(comb12),sum(K[1:2]))
  expect_equal(sum(!is.na(comb12[,1])),K[1],ignore_attr=TRUE)
  expect_equal(sum(!is.na(comb12[,2])),K[1],ignore_attr=TRUE)

  comb123 <- allSingleObs(items)
  expect_equal(names(comb123),nnames,ignore_attr=TRUE)
  expect_equal(nrow(comb123),sum(K))
  
})

test_that("buildEMTable",{
  skip_on_cran()
  skip_if_not_installed("PNetica")
  irt10 <- PNetica::local_PNetica_net("IRT10.2PL.base.dne")
  BuildAllTables(irt10)
  PnetCompile(irt10)
  items <- PnetFindNode(irt10,paste0("item",1:2))
  
})

