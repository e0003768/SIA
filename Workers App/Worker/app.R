---
output: html_document
runtime: shiny
---
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(RODBC)
library(shiny)
library(shinyjs)
library(DT)
library(leaflet)
require(shinydashboard)
library(readr)
library(V8)


connectionString <- "Driver={ODBC Driver 13 for SQL Server};Server=tcp:sutdtest.database.windows.net,1433;Database=sutdtest;Uid=zhyong@sutdtest;Pwd=BuslahBT3103;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;"
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- fluidPage(
  
  style="background-color:#ffffff;",
  useShinyjs(),
  extendShinyjs(text = jsResetCode),
  tabsetPanel(
    id = "navbar",
    tabPanel(
      title = "Home", value = "home",
      fluidRow(
        column(8, align = "center", offset = 2,
               br(),
               br(),
               br(),
               h2("Worker's Task", style="color:black; font-size: 400%; font-family: verdana;"),
               br(),
               textInput("userName", "Username:", placeholder="Enter your username"),
               passwordInput("passWord", "Password:", placeholder="Enter your password"),
               actionButton(
                 style = "background-color:	#4169E1;",
                 inputId = "loginButton",
                 label = h5("Login", style = "color:white;"),
                 width = 100
               )
        )
      )),
    tabPanel(
      title = "Report",
      value = "report",
      fluidRow(
        column(8, align = "center", offset = 2,
               h1("Your Tasks")
        )
      ),
      fluidRow(
                 box(width = 12,
                     dataTableOutput("userData"))
               ),
               br(),br()
               
        )
      
    ))
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  hide("navbar")
  
  values <- reactiveValues(df_data = NULL)
  
  observeEvent(input$loginButton,{
    myconn <- odbcDriverConnect(connectionString)
    qry <- paste0("SELECT tDate, staffName, flightNo, airlineModel, defectType, timeDiscovery, zone, equipment, seat, defectNature, completed, assigned FROM defects WHERE assigned = \'",
                  input$userName,"\'")
    values$df_data <- sqlQuery(myconn,qry)
    odbcClose(myconn)
    
    updateTabsetPanel(session = session, inputId = "navbar", selected = "report")
  })
   
  output$userData <- renderDataTable(
    values$df_data
    ,selection='single'
    ,filter = 'top'
    ,options = list(
      pageLength = 5
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

