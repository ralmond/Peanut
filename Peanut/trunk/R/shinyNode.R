########
## Shiny editor for a node.

CompensatoryGadget <- function(pnode) {

  ## Node Structure
  pstates <- PnodeStates(pnode)
  nps <- length(pstates)
  ppar <- PnodeParents(pnode)
  parnames <- PnodeParentNames(pnode)
  npar <- length(ppar)
  parStates <- lapply(ppar,PnodeStates)

  ## Node Parameters
  pRules <- PnodeRules(pnode)
  if (is.null(pRules)) pRules <- "Compensatory"
  pLink <- PnodeLink(pnode)
  if (is.null(pLink)) pLink <- "partialCredit"
  pQ <- TRUE
  pa <- PnodeAlphas(pnode)
  if (!is.numeric(pa)) {
    pa <- rep(1.0,pa)
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
                                  c("Parital Credit"="partialCredit",
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
                     tabPanel("Table",tableOutput("cptFrame")))))
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

    output$barchart <- renderPlot({
      barchart.CPF(buildCPF())
    })

    output$cptFrame <- renderTable(buildCPF(),striped=TRUE,digits=3)

    observeEvent(input$done, {
      stopApp(reassembleNode())
    })
  }
  runGadget(ui,server,
            viewer=dialogViewer(paste("Editor for node ",PnodeName(pnode)),
                                800,800))
}

OffsetGadget <- function(pnode) {

  ## Node Structure
  pstates <- PnodeStates(pnode)
  nps <- length(pstates)
  ppar <- PnodeParents(pnode)
  parnames <- PnodeParentNames(pnode)
  npar <- length(ppar)
  parStates <- lapply(ppar,PnodeStates)

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
                     tabPanel("Table",tableOutput("cptFrame")))))
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

    output$barchart <- renderPlot({
      barchart.CPF(buildCPF())
    })

    output$cptFrame <- renderTable(buildCPF(),striped=TRUE,digits=3)

    observeEvent(input$done, {
      stopApp(reassembleNode())
    })
  }
  runGadget(ui,server,
            viewer=dialogViewer(paste("Editor for node ",PnodeName(pnode)),
                                800,800))
}


RegressionGadget <- function(pnode, useR2=PnodeNumParents(pnode)>0L) {

  ## Node Structure
  pstates <- PnodeStates(pnode)
  nps <- length(pstates)
  ppar <- PnodeParents(pnode)
  parnames <- PnodeParentNames(pnode)
  npar <- length(ppar)
  parStates <- lapply(ppar,PnodeStates)

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
  print(useR2)

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
                     tabPanel("Table",tableOutput("cptFrame")))))
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

    output$barchart <- renderPlot({
      barchart.CPF(buildCPF())
    })

    output$cptFrame <- renderTable(buildCPF(),striped=TRUE,digits=3)

    observeEvent(input$done, {
      stopApp(reassembleNode())
    })
  }
  runGadget(ui,server,
            viewer=dialogViewer(paste("Editor for node ",PnodeName(pnode)),
                                800,800))
}
