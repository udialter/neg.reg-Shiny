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
library(r2symbols)


ui <- dashboardPage(
  dashboardHeader(
    titleWidth = 325,
    tags$li(class="dropdown",tags$a(href="https://twitter.com/udialter", icon("twitter"), "@UdiAlter", target="_blank")),
    tags$li(class="dropdown",tags$a(href="https://twitter.com/alyssacounsell", icon("twitter"), "@AlyssaCounsell", target="_blank")),
    tags$li(class="dropdown",tags$a(icon("envelope"), "udialter@yorku.ca", target="_blank"))),
  dashboardSidebar(
    tags$a(href='https://github.com/cribbie/negligible/',
           img(id="image",src='logo.png', height = 100, width = 100, style="display: block; margin-left: auto; margin-right: auto;")),
    fluidRow(column(12, align="center", 
    h4(strong("Input"), style='padding:7px; font-size:170%')
    )),
    width = 325,
    useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    hidden(
      div(
        id = "marks",
        fileInput("filedata", label = h4("File input",tags$head(tags$style("#q1{display:inline}")),bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small")),accept = c(".csv", ".xlsx", ".sav"), buttonLabel = "Choosing ...", placeholder = "No files selected yet"),
        bsPopover(id = "q1", title = "",content = paste0("Accepted file extensions: .csv"),placement = "right",trigger = "focus",options = list(container = "body")),
        uiOutput("xvariable"),
        uiOutput("yvariable"),
        uiOutput("zvariable"),
        numericInput("b", label = h4(HTML("Standardized Regression Coefficient of Interest (&beta;)")), value = NULL),
        numericInput("br", label = h4("Regression Coefficient of Interest (b)"), NULL),
        numericInput("se", h4("Standard Error Associated With the Above Regression Coefficient (se)"), NULL),
        numericInput("nop", h4("Number of Predictors (Excluding Intercept) (nop)"), NULL),
        numericInput("n", h4("Sample Size (n)"), NULL),
        numericInput("eil", h4("Standardized Lower Bound of the Equivalence Interval (eil)"), NULL),
        numericInput("eiu", h4("Standardized Upper Bound of the Equivalence Interval (eiu)"), NULL),
        numericInput("eilr", h4("Lower Bound of the Equivalence Interval (eil)"), NULL),
        numericInput("eiur", h4("Upper Bound of the Equivalence Interval (eiu)"), NULL),
        fluidRow(column(11, align="center", 
                        actionButton("submit", "Run", style='padding:7px; font-size:110%'),
                        actionButton("submitr", "Run", style='padding:7px; font-size:110%'),
                        actionButton("submitd", "Run", style='padding:7px; font-size:110%'),
                        actionButton("submitdr", "Run", style='padding:7px; font-size:110%')
                        ))
      ))
  ),
  dashboardBody(
    # Use this function somewhere in UI
    add_busy_bar(
      timeout = 1000,
      color = "#112446",
      centered = FALSE,
      height = "8px"
    ),
    
    tags$head(tags$style(HTML('
          .main-header .logo {
        font-family: "Arial", sans-serif;
        font-weight: bold;
        font-size: 24px;
          }
      
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #3182bd;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #3182bd;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #3182bd;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #005073;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #3182bd;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #3182bd;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #3182bd;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #3182bd;
         }
                              
        /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }
                                
                                #choice .radio label  { font-size: 16px; }
                                #nominal .radio label  { font-size: 16px; }
                              '))),
    
    fluidRow(column(12, align="left", 
                    hidden(
                      div(
                        class = "page",
                        id = "page1",
                        h4(tags$b("Test for Evaluating Negligible Effect Between a Predictor and Outcome in a Multiple Regression Model")),
                        p(id="par1","Welcome! This Shiny application evaluates whether a certain predictor variable in a multiple regression model can be considered statistically and practically negligible according to a predefined interval. i.e., minimally meaningful effect size (MMES)/smallest effect size of interest (SESOI). Where the effect tested is the relationship between the predictor of interest and the outcome variable, holding all other predictors constant.", style = "font-size:17px;"),
                        br(),
                        verbatimTextOutput('lmSummary'),
                        fluidRow(column(12, align="center", 
                                        radioButtons("choice", h4(tags$b("Select your preferred method:")), choices = c("I would like to upload a dataset", "I would like to input the exact values"), selected = character(0)),
                                        radioButtons(inputId="nominal", label=h4(tags$b("Are you using a standardized equivalence interval?")), 
                                                     choices=c("Yes","No"), selected = character(0)))))),
                    hidden(
                      div(
                        class = "page",
                        id = "page2",
                        column(12, align="left", 
                               h4(id="reg","Regression coefficient for predictor of interest:"),
                               p(id="coef",HTML("&beta; = "),tags$head(tags$style("#coef{display:inline}"))),textOutput("coef"),p(id="cil",", 95% CI = [",tags$head(tags$style("#cil{display:inline}"))),textOutput("cil"), p(id="cil", ",", tags$head(tags$style("#cil{display:inline}"))), textOutput("ciu"), p(id="ciu", "], standard error = ", tags$head(tags$style("#ciu{display:inline}"))),textOutput("se"), p(id="se", " ", tags$head(tags$style("#se{display:inline}"))),
                               h4(id="AH", "Anderson-Hauck (AH) procedure:"),
                               tags$head(tags$style(
                                 "#tvalue{
        display:inline
        }"
                               )),
                               p(id="t","Anderson-Hauck T statistic =",style="display:inline"),textOutput("tvalue"),
                               tags$head(tags$style(
                                 "#pvalue{
        display:inline
        }"
                               )),
                               p(id="p",", p-value =",style="display:inline"),textOutput("pvalue"),
                               h4(id="conclusion","Conclusion:"),
                               textOutput("text"),
                               h4(id="graphs", "Graphs:"),
                               p(strong("Figure 1")),
                               p("The unstandardized effect and its 95% CI"),
                               plotOutput("plot3"), 
                               p(strong("Figure 2")),
                               p("The proportional Distance"),
                               plotOutput("plot"), 
                               fluidRow(column(12, align="center", 
                                               actionButton("prevBtn", "< Go Back", style='padding:7px; font-size:110%')))
                               
                        ))),
                    hidden(
                      div(
                        class = "page",
                        id = "page3",
                        column(12, align="left", 
                               h4(id="reg","Regression coefficient for predictor of interest:"),
                               p(id="coef","b = ",tags$head(tags$style("#coefr{display:inline}"))),textOutput("coefr"),p(id="cilr",", 95% CI = [",tags$head(tags$style("#cilr{display:inline}"))),textOutput("cilr"), p(id="cilr", ",", tags$head(tags$style("#cilr{display:inline}"))), textOutput("ciur"), p(id="ciur", "], standard error = ", tags$head(tags$style("#ciur{display:inline}"))),textOutput("ser"), p(id="ser", " ", tags$head(tags$style("#ser{display:inline}"))),
                               h4(id="AH", "Anderson-Hauck (AH) procedure:"),
                               tags$head(tags$style(
                                 "#tvaluer{
        display:inline
        }"
                               )),
                               p(id="t","Anderson-Hauck T statistic =",style="display:inline"),textOutput("tvaluer"),
                               tags$head(tags$style(
                                 "#pvaluer{
        display:inline
        }"
                               )),
                               p(id="p",", p-value =",style="display:inline"),textOutput("pvaluer"),
                               h4(id="conclusion","Conclusion:"),
                               textOutput("textr"),
                               h4(id="graphs", "Graphs:"),
                               p(strong("Figure 1")),
                               p("The Standardized effect and its 95% CI"),
                               plotOutput("plot2"), 
                               p(strong("Figure 2")),
                               p("The proportional Distance"),
                               plotOutput("plotr"), 
                               fluidRow(column(12, align="center", 
                                               actionButton("prevBtn2", "< Go Back", style='padding:7px; font-size:110%')))
                               
                        ))),
                    hidden(
                      div(
                        class = "page",
                        id = "page4",
                        column(12, align="left", 
                               h4(id="reg","Regression coefficient for predictor of interest:"),
                               p(id="coef",HTML("&beta; = "),tags$head(tags$style("#coefd{display:inline}"))),textOutput("coefd"),p(id="cil",", 95% CI = [",tags$head(tags$style("#cild{display:inline}"))),textOutput("cild"), p(id="cil", ",", tags$head(tags$style("#cild{display:inline}"))), textOutput("ciud"), p(id="ciu", "], standard error = ", tags$head(tags$style("#ciud{display:inline}"))),textOutput("sed"), p(id="se", " ", tags$head(tags$style("#sed{display:inline}"))),
                               h4(id="AH", "Anderson-Hauck (AH) procedure:"),
                               tags$head(tags$style(
                                 "#tvalued{
        display:inline
        }"
                               )),
                               p(id="t","Anderson-Hauck T statistic =",style="display:inline"),textOutput("tvalued"),
                               tags$head(tags$style(
                                 "#pvalued{
        display:inline
        }"
                               )),
                               p(id="p",", p-value =",style="display:inline"),textOutput("pvalued"),
                               h4(id="conclusion","Conclusion:"),
                               textOutput("textd"),
                               h4(id="graphs", "Graphs:"),
                               p(strong("Figure 1")),
                               p("The Standardized effect and its 95% CI"),
                               plotOutput("plotd"), 
                               p(strong("Figure 2")),
                               p("The proportional Distance"),
                               plotOutput("plot4"), 
                               fluidRow(column(12, align="center", 
                                               actionButton("prevBtn3", "< Go Back", style='padding:7px; font-size:110%')))
                               
                        ))),
                    hidden(
                      div(
                        class = "page",
                        id = "page5",
                        column(12, align="left", 
                               h4(id="reg","Regression coefficient for predictor of interest:"),
                               p(id="coef","b = ",tags$head(tags$style("#coefdr{display:inline}"))),textOutput("coefdr"),p(id="cildr",", 95% CI = [",tags$head(tags$style("#cildr{display:inline}"))),textOutput("cildr"), p(id="cildr", ",", tags$head(tags$style("#cildr{display:inline}"))), textOutput("ciudr"), p(id="ciudr", "], standard error = ", tags$head(tags$style("#ciudr{display:inline}"))),textOutput("sedr"), p(id="sedr", " ", tags$head(tags$style("#sedr{display:inline}"))),
                               h4(id="AH", "Anderson-Hauck (AH) procedure:"),
                               tags$head(tags$style(
                                 "#tvaluedr{
        display:inline
        }"
                               )),
                               p(id="t","Anderson-Hauck T statistic =",style="display:inline"),textOutput("tvaluedr"),
                               tags$head(tags$style(
                                 "#pvaluedr{
        display:inline
        }"
                               )),
                               p(id="p",", p-value =",style="display:inline"),textOutput("pvaluedr"),
                               h4(id="conclusion","Conclusion:"),
                               textOutput("textdr"),
                               h4(id="graphs", "Graphs:"),
                               p(strong("Figure 1")),
                               p("The Standardized effect and its 95% CI"),
                               plotOutput("plotdr"), 
                               p(strong("Figure 2")),
                               p("The proportional Distance"),
                               plotOutput("plot5"), 
                               fluidRow(column(12, align="center", 
                                               actionButton("prevBtn4", "< Go Back", style='padding:7px; font-size:110%')))
                               
                        ))),
                    symbol("copyright"),p(id="none","Alter & Counsell"),p("",tags$head(tags$style("#none{display:inline}")))
    )
    )
  )
)
