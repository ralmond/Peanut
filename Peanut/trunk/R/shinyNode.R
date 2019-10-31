########
## Shiny editor for a node.

CompensatoryGadget <- function(pnode, color="firebrick") {

  ## Node Structure
  pstates <- PnodeStates(pnode)
  nps <- length(pstates)
  ppar <- PnodeParents(pnode)
  parnames <- PnodeParentNames(pnode)
  npar <- length(ppar)
  parStates <- lapply(ppar,PnodeStates)
  tvals <- PnodeParentTvals(pnode)
  thetas <- do.call("expand.grid",tvals)
  if (nrow(thetas) == 0L) {
    thetas <- data.frame(X0=0)
  }
  if (npar >0L) markers <- expand.grid(parStates)

  ## Node Parameters
  pRules <- PnodeRules(pnode)
  if (is.null(pRules)) pRules <- "Compensatory"
  pLink <- PnodeLink(pnode)
  if (is.null(pLink)) pLink <- "partialCredit"
  pQ <- TRUE
  pa <- PnodeAlphas(pnode)
  if (!is.numeric(pa)) {
    pa <- rep(1.0,npar)
  }
  if (is.null(names(pa))) names(pa) <- parnames
  pb <- PnodeBetas(pnode)
  if (!is.list(pb) || length(pb)!=nps-1L) {
    pb <- rep(0,nps-1L)
    pb <- as.list(pb)
  }
  if (is.null(names(pb))) names(pb) <- pstates[1L:(nps-1L)]

  ui <- fluidPage(
      title=(paste("Editor for node ",PnodeName(pnode))),
      wellPanel(h1(paste("Editor for node ",PnodeName(pnode))),
                actionButton("cancel","Cancel"),
                actionButton("done","OK")),
      ## Structure and Link
      fluidRow(column(width=4,
                      selectInput("link","Link Function:",
                                  c("Partial Credit"="partialCredit",
                                    "Graded Response"="gradedResponse"),
                                  selected=pLink)),
               column(width=6,
                      selectInput("rules","Structure Function (Rule):",
                                  c("Compensatory","Conjunctive",
                                    "Disjunctive"),
                                  selected=pRules))
               ),
      fluidRow(column(width=1,h4("Alphas:")),
               lapply(names(pa),function(par) {
                 column(width=3,
                        sliderInput(paste("a",par,sep="."),par,
                                    min=0.01,max=2,value=pa[par])
                        )})),
      fluidRow(column(width=1,h4("Betas:")),
               lapply(names(pb),function(st) {
                 column(width=3,
                        sliderInput(paste("b",st,sep="."),st,
                                    min=-3,max=3,value=pb[[st]])
                        )})),
      ## conditionalPanel("input.link == normalLink",
      ##                  sliderInput(lsp,"Link Scale Parameter:",
      ##                              min=0.01,max=1,value=lsp)),
      ## Resulting CPT
      fluidRow(
          column(width=12,
                 tabsetPanel(
                     tabPanel("Plot",plotOutput("barchart")),
                     tabPanel("Table",tableOutput("cptFrame")),
                     tabPanel("Effective Thetas",tableOutput("etFrame")))))

  )

  server <-  function (input, output, session) {

    ## Reassemble vectors
    newpa <- reactive({
      newa <- lapply(names(pa),function(par) input[[paste("a",par,sep=".")]])
      newa <- as.numeric(newa)
      names(newa) <- names(pa)
      newa
    })

    newpb <- reactive({
      newb <- lapply(names(pb),function(st) input[[paste("b",st,sep=".")]])
      newb <- as.list(newb)
      names(newb) <- names(pb)
      newb
    })


    ## Reassemble Node
    reassembleNode <- reactive( {
      PnodeQ(pnode) <- pQ
      PnodeRules(pnode) <- input$rules
      PnodeLink(pnode) <- input$link
      PnodeAlphas(pnode) <- newpa()
      PnodeBetas(pnode) <- newpb()
      pnode
    })
    ## Build CPF
    buildCPF <- reactive({
      calcDPCFrame(parStates,pstates,log(newpa()),newpb(),
                   input$rules,input$link,NULL,pQ)
    })

    ## Effective Thetas table
    eThetasFrame <- reactive({
      rules <- input$rules
      newa <- newpa()
      newb <- newpb()
      et <- matrix(0,nrow(thetas),nps-1L) #Take care of no parent case
      for (kk in 1L:(nps-1L)) {
        et[,kk] <- do.call(rules,
                           list(thetas,newa,newb[[kk]]))
      }
      colnames(et) <- pstates[1L:(nps-1L)]
      if (npar >0L)
        result <- data.frame(markers,et)
      else
        result <- data.frame(et)
    })


    output$barchart <- renderPlot({
      barchart.CPF(buildCPF(),baseCol=color)
    })

    output$cptFrame <- renderTable(buildCPF(),striped=TRUE,digits=3)
    output$etFrame <- renderTable(eThetasFrame(),striped=TRUE,digits=3)

    observeEvent(input$done, {
      stopApp(reassembleNode())
    })
  }
  runGadget(ui,server,
            viewer=dialogViewer(paste("Editor for node ",PnodeName(pnode)),
                                800,800))
}

OffsetGadget <- function(pnode, color="plum") {

  ## Node Structure
  pstates <- PnodeStates(pnode)
  nps <- length(pstates)
  ppar <- PnodeParents(pnode)
  parnames <- PnodeParentNames(pnode)
  npar <- length(ppar)
  parStates <- lapply(ppar,PnodeStates)
  tvals <- PnodeParentTvals(pnode)
  thetas <- do.call("expand.grid",tvals)
  if (nrow(thetas) == 0L) {
    thetas <- data.frame(X0=0)
  }
  if (npar >0L) markers <- expand.grid(parStates)

  ## Node Parameters
  pRules <- PnodeRules(pnode)
  if (is.null(pRules)) pRules <- "OffsetConjunctive"
  pLink <- PnodeLink(pnode)
  if (is.null(pLink)) pLink <- "partialCredit"
  pQ <- TRUE
  #Alphas -- one per state-1
  pa <- PnodeAlphas(pnode)
  if (is.null(pa) || length(pa)==0L) pa <- 1.0
  if (length(pa) != nps-1L) pa <- rep(pa,nps-1L)
  pa <- as.list(pa)
  if (is.null(names(pa))) names(pa) <- pstates[1L:(nps-1L)]

  #Betas -- one per parent
  pb <- PnodeBetas(pnode)
  if (length(pb)!=npar) {
    pb <- rep(0,npar)
  }
  if (is.null(names(pb))) names(pb) <- parnames

  ui <- fluidPage(
      title=(paste("Editor for node ",PnodeName(pnode))),
      wellPanel(h1(paste("Editor for node ",PnodeName(pnode))),
                actionButton("cancel","Cancel"),
                actionButton("done","OK")),
      ## Structure and Link
      fluidRow(column(width=4,
                      selectInput("link","Link Function:",
                                  c("Parital Credit"="partialCredit",
                                    "Graded Response"="gradedResponse"),
                                  selected=pLink)),
               column(width=6,
                      selectInput("rules","Structure Function (Rule):",
                                  c("OffsetConjunctive",
                                    "OffsetDisjunctive"),
                                  selected=pRules))
               ),
      conditionalPanel("input.link == 'gradedResponse'",
                       fluidRow(column(width=1,h4("Alpha:")),
                                column(width=3,
                                       sliderInput("a","Alpha",
                                                   min=0.01,max=2,
                                                   value=pa[[1]]))
                        )),
      conditionalPanel("input.link == 'partialCredit'",
                       fluidRow(column(width=1,h4("Alphas:")),
                                lapply(names(pa),function(st) {
                                  column(width=3,
                                         sliderInput(paste("a",st,sep="."),st,
                                                     min=0.01,max=2,
                                                     value=pa[[st]])
                        )}))),
      fluidRow(column(width=1,h4("Betas:")),
               lapply(names(pb),function(par) {
                 column(width=3,
                        sliderInput(paste("b",par,sep="."),par,
                                    min=-3,max=3,value=pb[par])
                        )})),
      ## conditionalPanel("input.link == normalLink",
      ##                  sliderInput(lsp,"Link Scale Parameter:",
      ##                              min=0.01,max=1,value=lsp)),
      ## Resulting CPT
      fluidRow(
          column(width=12,
                 tabsetPanel(
                     tabPanel("Plot",plotOutput("barchart")),
                     tabPanel("Table",tableOutput("cptFrame")),
                     tabPanel("Effective Thetas",tableOutput("etFrame")))))

  )

  server <-  function (input, output, session) {

    ## Reassemble vectors
    newpa <- reactive({
      if (input$link=='gradedResponse')
        newa <- input$a
      else {
        newa <- lapply(names(pa),function(st) input[[paste("a",st,sep=".")]])
        newa <- as.list(newa)
        names(newa) <- names(pa)
      }
      newa
    })

    newpb <- reactive({
      newb <- lapply(names(pb),function(par) input[[paste("b",par,sep=".")]])
      newb <- as.numeric(newb)
      names(newb) <- names(pb)
      newb
    })


    ## Reassemble Node
    reassembleNode <- reactive( {
      PnodeQ(pnode) <- pQ
      PnodeRules(pnode) <- input$rules
      PnodeLink(pnode) <- input$link
      PnodeAlphas(pnode) <- newpa()
      PnodeBetas(pnode) <- newpb()
      pnode
    })
    ## Build CPF
    buildCPF <- reactive({
      calcDPCFrame(parStates,pstates,lapply(newpa(),log),newpb(),
                   input$rules,input$link,NULL,pQ)
    })
    ## Effective Thetas table
    eThetasFrame <- reactive({
      rules <- input$rules
      newa <- newpa()
      newb <- newpb()
      et <- matrix(0,nrow(thetas),nps-1L) #Take care of no parent case
      for (kk in 1L:(nps-1L)) {
        et[,kk] <- do.call(rules,
                           list(thetas,newa[[kk]],newb))
      }
      colnames(et) <- pstates[1L:(nps-1L)]
      if (npar >0L)
        result <- data.frame(markers,et)
      else
        result <- data.frame(et)
    })

    output$barchart <- renderPlot({
      barchart.CPF(buildCPF(), baseCol=color)
    })

    output$cptFrame <- renderTable(buildCPF(),striped=TRUE,digits=3)
    output$etFrame <- renderTable(eThetasFrame(),striped=TRUE,digits=3)

    observeEvent(input$done, {
      stopApp(reassembleNode())
    })
  }
  runGadget(ui,server,
            viewer=dialogViewer(paste("Editor for node ",PnodeName(pnode)),
                                800,800))
}


RegressionGadget <- function(pnode, useR2=PnodeNumParents(pnode)>0L,
                             color = "sienna") {

  ## Node Structure
  pstates <- PnodeStates(pnode)
  nps <- length(pstates)
  ppar <- PnodeParents(pnode)
  parnames <- PnodeParentNames(pnode)
  npar <- length(ppar)
  parStates <- lapply(ppar,PnodeStates)
  tvals <- PnodeParentTvals(pnode)
  thetas <- do.call("expand.grid",tvals)
  if (nrow(thetas) == 0L) {
    thetas <- data.frame(X0=0)
  }
  if (npar >0L) markers <- expand.grid(parStates)

  ## Node Parameters
  pRules <- PnodeRules(pnode)
  if (is.null(pRules)) pRules <- "Compensatory"
  pLink <- "normalLink"
  pQ <- TRUE
  pa <- PnodeAlphas(pnode)
  if (is.null(pa)) pa <- 1
  if (is.list(pa)) pa <- pa[[1]]
  if (length(pa) != npar) pa <- rep(pa,length.out=npar)
  if (is.null(names(pa))) names(pa) <- parnames
  pb <- PnodeBetas(pnode)
  pb <- as.numeric(pb)
  pb <- pb[1]
  pls <- PnodeLinkScale(pnode)
  R2 <- sum(pa*pa)/length(pa)/(sum(pa*pa)/length(pa)+pls*pls)

  ui <- fluidPage(
      title=(paste("Editor for node ",PnodeName(pnode))),
      wellPanel(h1(paste("Editor for node ",PnodeName(pnode))),
                actionButton("cancel","Cancel"),
                actionButton("done","OK")),
      ## Structure and Link
      conditionalPanel(paste(tolower(as.character(npar>0L))),
                       fluidRow(column(width=6,offset=4,
                                       selectInput("rules",
                                                   "Structure Function (Rule):",
                                                   c("Compensatory",
                                                     "Conjunctive",
                                                     "Disjunctive"),
                                                   selected=pRules)))),
      fluidRow(column(width=1,h4("Slopes:")),
               lapply(names(pa),function(par) {
                 column(width=3,
                        sliderInput(paste("a",par,sep="."),par,
                                    min=0.01,max=2,value=pa[par])
                        )})),
      fluidRow(column(width=1,h4("Intercept:")),
               column(width=3,
                      sliderInput("b","(Intercept)",
                                  min=-3,max=3,value=-pb)
                      )),
      conditionalPanel(paste(tolower(as.character(useR2))),
                       fluidRow(column(width=1,h4("Scale Parameter")),
                                column(width=3,
                                       sliderInput("rsq","R-squared",
                                                   min=0.01,max=1,value=R2)))),
      conditionalPanel(paste(tolower(as.character(!useR2))),
                       fluidRow(column(width=1,h4("Scale Parameter")),
                                column(width=3,
                                       sliderInput("pls",
                                                   "Residual Standard Error",
                                                   min=0.01,max=2,value=pls)))),
      ## Resulting CPT
      fluidRow(
          column(width=12,
                 tabsetPanel(
                     tabPanel("Plot",plotOutput("barchart")),
                     tabPanel("Table",tableOutput("cptFrame")),
                     tabPanel("Effective Thetas",tableOutput("etFrame")))))

  )

  server <-  function (input, output, session) {

    ## Reassemble vectors
    newpa <- reactive({
      newa <- lapply(names(pa),function(par) input[[paste("a",par,sep=".")]])
      newa <- as.numeric(newa)
      names(newa) <- names(pa)
      newa
    })

    newpb <- reactive({
      newb <- -input$b
      newb
    })

    newrules <- reactive({
      if (npar>0L)
        input$rules
      else
        pRules
    })

    newpls <- reactive({
      if (useR2) {
        newr2 <- input$rsq
        a2 <- sum(newpa()^2)/length(pa)
        sqrt(a2*(1/newr2-1))
      } else {
        input$pls
      }
    })

    ## Reassemble Node
    reassembleNode <- reactive( {
      PnodeQ(pnode) <- pQ
      PnodeRules(pnode) <- newrules()
      PnodeLink(pnode) <- input$link
      PnodeAlphas(pnode) <- newpa()
      PnodeBetas(pnode) <- newpb()
      PnodeLinkScale(pnode) <- newpls()
      pnode
    })
    ## Build CPF
    buildCPF <- reactive({
      calcDPCFrame(parStates,pstates,log(newpa()),newpb(),
                   newrules(),pLink,newpls(),pQ)
    })

    ## Effective Thetas table
    eThetasFrame <- reactive({
      rules <- input$rules
      newa <- newpa()
      newb <- newpb()
      et <- matrix(0,nrow(thetas),nps-1L) #Take care of no parent case
        et <- do.call(rules,
                           list(thetas,newa,newb))
      et <- matrix(et,nrow(thetas),1L)
      if (npar >0L)
        result <- data.frame(markers,theta=et)
      else
        result <- data.frame(theta=et)
    })

    output$barchart <- renderPlot({
      barchart.CPF(buildCPF(), baseCol=color)
    })

    output$cptFrame <- renderTable(buildCPF(),striped=TRUE,digits=3)
    output$etFrame <- renderTable(eThetasFrame(),striped=TRUE,digits=3)

    observeEvent(input$done, {
      stopApp(reassembleNode())
    })
  }
  runGadget(ui,server,
            viewer=dialogViewer(paste("Editor for node ",PnodeName(pnode)),
                                800,800))
}

DPCGadget <- function(pnode, color="steelblue") {

  ## Node Structure
  pstates <- PnodeStates(pnode)
  nps <- length(pstates)
  nps1 <- nps -1L
  ppar <- PnodeParents(pnode)
  parnames <- PnodeParentNames(pnode)
  npar <- length(ppar)
  parStates <- lapply(ppar,PnodeStates)
  tvals <- PnodeParentTvals(pnode)
  thetas <- do.call("expand.grid",tvals)
  if (nrow(thetas) == 0L) {
    thetas <- data.frame(X0=0)
  }
  if (npar >0L) markers <- expand.grid(parStates)


  ## Node Parameters
  pLink <- "partialCredit"
  pRules <- PnodeRules(pnode)
  if (is.null(pRules)) pRules <- "Compensatory"
  pRules <- rep(pRules,length.out=nps1)
  pRules <- as.list(pRules)
  names(pRules) <- pstates[1L:nps1]

  ## Q
  pQ <- PnodeQ(pnode)
  if (isTRUE(pQ)) pQ <- matrix(TRUE,nps1,npar)
  rownames(pQ) <- pstates[1L:nps1]
  colnames(pQ) <- parnames

  ## Alpha Structure
  anames <- c("CommonAlpha",parnames)
  pa0 <- rep(1.0,length(anames))
  names(pa0) <- anames
  paa <- PnodeAlphas(pnode)
  if (is.numeric(paa)) {
    if (!is.null(names(paa)))
      pa0[names(paa)] <- paa
    else {
      if (length(paa) == 1L)
        pa0[1L] <- paa
      else {
        if (length(paa)==npar)
          pa0[2L:(npar+1L)] <- paa
      }
    }
  }
  pa <- rep(list(pa0),nps1)
  names(pa) <- names(pRules)
  if (is.list(paa) && length(paa)==nps1) {
    for (i in 1:nps1) {
      pai <- paa[[i]]
      if (!is.null(names(pai)))
        pa[[i]][names(pai)] <- pai
      else {
        if (length(pai) == 1L)
          pa[[i]][1L] <- pai
        else {
          if (length(pai)==npar)
            pa[[i]][2L:(npar+1L)] <- pai
        }
      }
    }
  }

  ## Beta Structure
  bnames <- c("CommonBeta",parnames)
  pb0 <- rep(0.0,length(bnames))
  names(pb0) <- bnames
  pbb <- PnodeBetas(pnode)
  if (is.numeric(pbb)) {
    if (is.null(names(pbb)))
      pb0[names(pbb)] <- pbb
    else {
      if (length(pbb) == 1L)
        pb0[1L] <- pbb
      else {
        if (length(pbb)==npar)
          pb0[2L:(npar+1L)] <- pbb
      }
    }
  }
  pb <- rep(list(pb0),nps1)
  names(pb) <- names(pRules)
  if (is.list(pbb) && length(pbb)==nps1) {
    for (i in 1:nps1) {
      pbi <- pbb[[i]]
      if (!is.null(names(pbi)))
        pb[[i]][names(pbi)] <- pbi
      else {
        if (length(pbi) == 1L)
          pb[[i]][1L] <- pbi
        else {
          if (length(pbi)==npar)
            pb[[i]][2L:(npar+1L)] <- pbi
        }
      }
    }
  }

  ui <- fluidPage(
    useShinyjs(),
    title=(paste("Editor for node ",PnodeName(pnode))),
    wellPanel(h1(paste("Editor for node ",PnodeName(pnode))),
              actionButton("cancel","Cancel"),
              actionButton("done","OK")),
    {
      tabs <- lapply(names(pRules),
                 function(st) {
                   tabPanel(st,
                     fluidRow(column(2,h3("Transition to state ",st)),
                              column(width=4,offset=2,
                                     selectInput(paste("rules",st,sep="."),
                                                 "Structure Function (Rule):",
                                                 c("Compensatory","Conjunctive",
                                                   "Disjunctive","OffsetConjunctive",
                                                   "OffsetDisjunctive"),
                                                 selected=pRules[[st]]))),
                     fluidRow(column(width=4,h4(paste("Q-matrix row for ",st))),
                              lapply(parnames,function(par) {
                                column(width=3,
                                       checkboxInput(paste("Q",st,par,sep="."),
                                                     par,pQ[st,par]))
                                })),
                     fluidRow(column(width=1,h4("Alphas:")),
                              lapply(anames,function(par) {
                                column(width=3,
                                       sliderInput(paste("a",st,par,sep="."),
                                                   par,
                                                   min=0.01,max=2,
                                                   value=pa[[st]][par])
                                       )})),
                     fluidRow(column(width=1,h4("Betas:")),
                              lapply(bnames,function(par) {
                                column(width=3,
                                       sliderInput(paste("b",st,par,sep="."),
                                                   par,
                                                   min=-3,max=3,
                                                   value=pb[[st]][par])
                                       )})))
                   })
      fluidRow(column(width=12,(do.call(tabsetPanel,tabs))))
    },
    fluidRow(
        column(width=12,
               tabsetPanel(
                   tabPanel("Plot",plotOutput("barchart")),
                   tabPanel("CP Table",tableOutput("cptFrame")),
                   tabPanel("Effective Thetas",tableOutput("etFrame")))))
  )

  server <-  function (input, output, session) {

    newRules <- reactive({
      nrules <-
        lapply(names(pRules),
               function (st) {
                 input[[paste("rules",st,sep=".")]]
               })
      names(nrules) <- names(pRules)
      ## print("Rules:")
      ## print(nrules)
      nrules
      })

    offsetRules <- reactive({
      orules <- sapply(newRules(),isOffsetRule)
      names(orules) <- names(pRules)
      ## print("Offsets:")
      ## print(orules)
      orules
    })

    newQ <- reactive({
      QQ <- pQ
      for (st in rownames(pQ)) {
        for (par in colnames(pQ)) {
          QQ[st,par] <- input[[paste("Q",st,par,sep=".")]]
        }
      }
      ## print("Q:")
      ## print(QQ)
      QQ
    })

    ## Reassemble vectors
    newpa <- reactive({
      orules <- offsetRules()
      QQ <- newQ()
      newa <-
        lapply(names(orules),
               function(st) {
                 if (orules[st])
                   input[[paste("a",st,anames[1L],sep=".")]]
                 else {
                   rowa <- sapply(anames[2L:(npar+1L)][QQ[st,]],
                                  function(par) {
                                    input[[paste("a",st,par,sep=".")]]
                                  })
                   names(rowa) <- bnames[2L:(npar+1L)][QQ[st,]]
                   rowa
                 }
               })
      ## print("Alphas:")
      ## print(newa)
      names(newa) <- names(orules)
      newa
    })


    ## Reassemble vectors
    newpb <- reactive({
      orules <- offsetRules()
      QQ <- newQ()
      newb <-
        lapply(names(orules),
               function(st) {
                 if (!orules[st])
                   input[[paste("b",st,bnames[1L],sep=".")]]
                 else {
                   rowb <- sapply(bnames[2L:(npar+1L)][QQ[st,]],
                                  function(par) {
                                    input[[paste("b",st,par,sep=".")]]
                                  })
                   names(rowb) <- bnames[2L:(npar+1L)][QQ[st,]]
                   rowb
                 }
               })
      names(newb) <- names(orules)
      ## print("Betas:")
      ## print(newb)
      newb
    })

    eThetasFrame <- reactive({
      rules <- newRules()
      newa <- newpa()
      newb <- newpb()
      QQ <- newQ()
      et <- matrix(0,nrow(thetas),nps-1L) #Take care of no parent case
      for (kk in 1L:(nps-1L)) {
        et[,kk] <- do.call(rules[[kk]],
                           list(thetas[,QQ[kk,],drop=FALSE],
                                newa[[kk]],newb[[kk]]))
      }
      colnames(et) <- pstates[1L:(nps-1L)]
      if (npar >0L)
        result <- data.frame(markers,et)
      else
        result <- data.frame(et)
    })

    ## Reassemble Node
    reassembleNode <- reactive( {
      PnodeQ(pnode) <- newQ()
      PnodeRules(pnode) <- newRules()
      PnodeLink(pnode) <- pLink
      PnodeAlphas(pnode) <- newpa()
      PnodeBetas(pnode) <- newpb()
      pnode
    })
    ## Build CPF
    buildCPF <- reactive({
      calcDPCFrame(parStates,pstates,lapply(newpa(),log),
                   newpb(),newRules(),pLink,NULL,newQ())
    })

    output$barchart <- renderPlot({
      barchart.CPF(buildCPF(), baseCol=color)
    })

    output$cptFrame <- renderTable(buildCPF(),striped=TRUE,digits=3)
    output$etFrame <- renderTable(eThetasFrame(),striped=TRUE,digits=3)
    observeEvent(input$done, {
      stopApp(reassembleNode())
    })

    observe({
      orules <- offsetRules()
      QQ <- newQ()
      for (st in rownames(pQ)) {
        for (i in 2L:(npar+1L)) {
          toggleState(paste("a",st,anames[i],sep="."),
                      condition = !orules[[st]] & QQ[st,anames[i]])
          toggleState(paste("b",st,bnames[i],sep="."),
                      condition =  orules[[st]] & QQ[st,bnames[i]])
        }
      }
    })
    observe({
      orules <- offsetRules()
      #print(orules)
      for (st in rownames(pQ)) {
        toggleState(paste("a",st,anames[1L],sep="."),
                    condition=isTRUE(orules[[st]]))
        toggleState(paste("b",st,bnames[1L],sep="."),
                    condition=!isTRUE(orules[[st]]))
      }
    })
  }
  runGadget(ui,server,
            viewer=dialogViewer(paste("Editor for node ",PnodeName(pnode)),
                                800,800))
}

##########################################
## Shiny breaks the show command

show <- methods::show
