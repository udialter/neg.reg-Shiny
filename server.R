server <- function(input, output) { 
  library(shiny)
  library(shinydashboard)
  library(shinyBS)
  library(shinyjs)
  library(negligible)
  library(shinybusy)
  library(shinyFeedback)
  library(DT)
  library(shinyWidgets)
  library(graphics)
  
  NUM_PAGES = 2
  source('neg.regr.R')
  
  rv <- reactiveValues(page = 1)
  
  navPage <- function(direction) {
    rv$page <- rv$page + direction
    print(rv$page)
  }
  
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "submit", condition = rv$page < NUM_PAGES)
    toggleState(id = "submitr", condition = rv$page < NUM_PAGES)

    hide(selector = ".page")
    show(
      paste0("page", rv$page)
    )
  })
  
  observe({
    toggleState(id = "submit", condition = (input$b != 0 & input$se != 0 & input$nop != 0 & input$eil != 0 & input$eiu != 0 & input$n != 0))
    toggleState(id = "submitr", condition = (input$br != 0 & input$se != 0 & input$nop != 0 & input$eilr != 0 & input$eiur != 0 & input$n != 0))
    })
  
  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$submit, navPage(+1))
  observeEvent(input$submitr, navPage(+2))
  observeEvent(input$prevBtn2, navPage(-2))
  
  observe(
               {
                 feedbackDanger("b", input$b == 0, "Please enter the regression coefficient")
                 feedbackDanger("br", input$br == 0, "Please enter the regression coefficient")
                 feedbackDanger("se", input$se == 0, "Please enter the standard error")
                 feedbackDanger("nop", input$nop == 0, "Please enter the number of predictors")
                 feedbackDanger("eil", input$eil == 0, "Please enter the lower boundary of your equivalence interval")
                 feedbackDanger("eiu", input$eiu == 0, "Please enter the upper boundary of your equivalence interval")
                 feedbackDanger("eilr", input$eilr == 0, "Please enter the lower boundary of your equivalence interval")
                 feedbackDanger("eiur", input$eiur == 0, "Please enter the upper boundary of your equivalence interval")
                 feedbackDanger("n", input$n == 0, "Please enter a sample size")
               })
  
    results <- eventReactive(input$submit, {
      b<-input$b 
      se<-input$se
      nop<-input$nop
      n<-input$n
      eil<-input$eil
      eiu<-input$eiu 
      mod.neg<-neg.reg(b=b, se=se, nop=nop, n=n, ei=eil, eiu=eiu, std=T)
      combo <- list(decision = mod.neg$decision, tvalue = mod.neg$t.value, pvalue = mod.neg$p.value, coef = mod.neg$b, cil = mod.neg$l.ci, ciu = mod.neg$u.ci, se = mod.neg$se, ciu2a = mod.neg$u.ci.2a, cil2a = mod.neg$l.ci.2a)
    })
    
    resultsr <- eventReactive(input$submitr, {
      b<-input$br 
      se<-input$se
      nop<-input$nop
      n<-input$n
      eil<-input$eilr
      eiu<-input$eiur
      mod.neg<-neg.reg(b=b, se=se, nop=nop, n=n, ei=eil, eiu=eiu, std=F)
      combo <- list(decision = mod.neg$decision, tvalue = mod.neg$t.value, pvalue = mod.neg$p.value, coef = mod.neg$b, cil = mod.neg$l.ci, ciu = mod.neg$u.ci, se = mod.neg$se, ciu2a = mod.neg$u.ci.2a, cil2a = mod.neg$l.ci.2a)
    })
  
  plot.results <- eventReactive(input$submit, {
    b<-input$b
    se<-input$se
    nop<-input$nop
    n<-input$n
    eil<-input$eil
    eiu<-input$eiu
    neg.reg(b=b, se=se, nop=nop, n=n, ei=eil, eiu=eiu, std=T)
  })
  
  plot.resultsr <- eventReactive(input$submitr, {
    b<-input$br
    se<-input$se
    nop<-input$nop
    n<-input$n
    eil<-input$eilr
    eiu<-input$eiur
    neg.reg(b=b, se=se, nop=nop, n=n, ei=eil, eiu=eiu, std=F)
  })
  
  plot.results3 <- eventReactive(input$submit, {
    b<-input$b
    se<-input$se
    nop<-input$nop
    n<-input$n
    eil<-input$eil
    eiu<-input$eiu
    regr(b=b, se=se, nop=nop, n=n, ei=eil, eiu=eiu, std=T)
  })
  
  plot.results2 <- eventReactive(input$submitr, {
    b<-input$br
    se<-input$se
    nop<-input$nop
    n<-input$n
    eil<-input$eilr
    eiu<-input$eiur
    regr(b=b, se=se, nop=nop, n=n, ei=eil, eiu=eiu, std=F)
  })
  
  # text output
  output$text <- renderText({
    combo <- results()
    decision <- combo$decision
  })
  
  output$textr <- renderText({
    combo <- resultsr()
    decisionr <- combo$decision
  })
  
  output$tvalue <- renderText({
    combo <- results()
    tvalue<- combo$tvalue
  })
  
  output$tvaluer <- renderText({
    combo <- resultsr()
    tvaluer<- combo$tvalue
  })
  
  output$pvalue <- renderText({
    combo <- results()
    pvalue <- round(combo$pvalue, digits = 2)
  })
  
  output$pvaluer <- renderText({
    combo <- resultsr()
    pvaluer <- round(combo$pvalue, digits = 2)
  })
  
  output$coef <- renderText({
    combo <- results()
    coef <- combo$coef
  })
  
  output$coefr <- renderText({
    combo <- resultsr()
    coefr <- combo$coef
  })
  
  output$cil <- renderText({
    combo <- results()
    cil <- round(combo$cil, digits = 3)
  })
  
  output$cilr <- renderText({
    combo <- resultsr()
    cilr <- round(combo$cil, digits = 3)
  })
  
  output$ciu <- renderText({
    combo <- results()
    ciu <- round(combo$ciu, digits = 3)
  })
  
  output$ciur <- renderText({
    combo <- resultsr()
    ciur <- round(combo$ciu, digits = 3)
  })
  
  output$se <- renderText({
    combo <- results()
    se <- combo$se
  })
  
  output$ser <- renderText({
    combo <- resultsr()
    ser <- combo$se
  })
  
  output$plot <- renderPlot({
    plot.results()
  })
  
  output$plotr <- renderPlot({
    plot.resultsr()
  })
  
  output$plot2 <- renderPlot({
    plot.results2()
  })
  
  output$plot3 <- renderPlot({
    plot.results3()
  })
  
  observeEvent({
    input$choice
  },
  {
    shinyjs::show("nominal")
  })
  observeEvent({
    input$nominal
  },
  if (input$nominal=="Yes")
  {
    shinyjs::show("marks")
    shinyjs::hide("submitr")
    shinyjs::hide("file")
    shinyjs::hide("br")
    shinyjs::hide("eilr")
    shinyjs::hide("eiur")
    shinyjs::hide("yvariable")
    shinyjs::hide("xvariable")
    shinyjs::show("b")
    shinyjs::show("se")   
    shinyjs::show("nop")
    shinyjs::show("n")
    shinyjs::show("eil")
    shinyjs::show("eiu")
    shinyjs::show("submit")
  })
  
  
  observeEvent({
    input$nominal
  },
  if (input$nominal=="No")
  {
    shinyjs::show("marks")
    shinyjs::hide("file")
    shinyjs::hide("b")
    shinyjs::hide("eil")
    shinyjs::hide("eiu")
    shinyjs::hide("yvariable")
    shinyjs::hide("xvariable")
    shinyjs::show("br")
    shinyjs::show("se")   
    shinyjs::show("nop")
    shinyjs::show("n")
    shinyjs::show("eilr")
    shinyjs::show("eiur")
    shinyjs::hide("submit")
    shinyjs::show("submitr")
  })

}
