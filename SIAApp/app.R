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
library(V8)
library(rdrop2)
#token <- drop_auth()
#saveRDS(token, "C:/Users/Zhun/Documents/SIAApp/droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
#token <- readRDS("C:/Users/Zhun/Documents/SIAApp/droptoken.rds")
# Then pass the token to each drop_ function
#drop_acc(dtoken=token)
#drop_auth(key="t1p2mx4twg9ko4z", secret="52xh1lpwfmfv9iy")

#SQL Connection String
connectionString <- "Driver={ODBC Driver 13 for SQL Server};Server=tcp:sutdtest.database.windows.net,1433;Database=sutdtest;Uid=zhyong@sutdtest;Pwd=BuslahBT3103;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;"




jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

# Define UI for application that draws a histogram
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
               h2("Cabin Defect Report", style="color:black; font-size: 400%; font-family: verdana;"),
               br(),
               textInput("staffId", "Staff ID:", placeholder="Enter your Staff ID"),
               textInput("staffName", "Staff Name:", placeholder="Enter your Staff Name"),
               actionButton(
                 style = "background-color:	#4169E1;",
                 inputId = "reportButton",
                 label = h5("Start", style = "color:white;"),
                 width = 100
               )
        )
      )),
    tabPanel(
      title = "Report",
      value = "report",
      fluidRow(
        column(8, align = "center", offset = 2,
               h1("Report a Defect")
        )
      ),
      fluidRow(
        column(4,style = "background-color:#ffffff;", align="center",
               tags$img(src='Layout1.png', width = 225, height = 525)),
        column(8,
               selectInput("defectType", "Type of Defect:",
                           c("Cabin Defect" = "Cabin Defect",
                             "CMIV Defect" = "CMIV Defect",
                             "Safety Equipment Defect and Usage" = "Safety Equipment")),
               selectInput("timeDiscovery", "Time of Discovery:",
                           c("Before Flight" = "Before Flight",
                             "During Flight" = "During Flight",
                             "After Flight" = "After Flight")),
               selectInput("zone", "Zone:",
                           c("1" = "Zone 1",
                             "2" = "Zone 2",
                             "3" = "Zone 3")),
               selectInput('equipment', 'Equipment:', c("Choose Equipment"="")),
               uiOutput("textbox_ui"),
               textAreaInput("defectNature", "Nature of Defect", placeholder = "Write a brief description"),
               
               fileInput("defectImage", "Upload a Photo", accept = c('image/png', 'image/jpeg')),
               actionButton(
                 style = "background-color:#DC143C;",
                 inputId = "backButton",
                 label = h5("Cancel", style = "color:white;")
               ),
               actionButton(
                 style = "background-color:green;",
                 inputId = "submitButton",
                 label = h5("Submit", style = "color:white;")
               ),
               br(),br()
               
        )
      )
    ),
    tabPanel(
      title = "Submitted", value = "submit",
      fluidRow(align = "center", offset = 2,
               h1("Report Submitted", style="color:black;"),
               tags$img(src='tick.jpg', width = 100, height = 100),
               br(),
               actionButton(
                 style = "background-color:green;",
                 inputId = "submitAgainButton",
                 label = h5("Submit Another Report", style = "color:white;")),
               br(),br(),
               actionButton(
                 style = "background-color:#DC143C;",
                 inputId = "logOutButton",
                 label = h5("Log Out", style = "color:white;"),
                 width = 100
               )
               
        
      ))
  ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  hide("navbar")
  
  observe({
    x <- input$defectType
    
    # Can use character(0) to remove all choices
    if (x ==  'Cabin Defect') {
      x <- list(
        Galley = sort(c(`Coffee Maker` = 'Coffee Maker', `Oven` = 'Oven', `Service Cart` = 'Service Cart',
                        `Oven Rack` = 'Oven Rack', `Oven Carrier` = 'Oven Carrier', `Water Boiler` = 'Water Boiler',
                        `Microwave` = 'Microwave', `Container` = 'Container', `Drawer` = 'Drawer', `Hot Liquid Container` = 'Hot Liquid Container')),
        Cabin = sort(c(`Seat and Seatbelt` = 'Seat and Seatbelt', `Foot Rest` = 'Foot Rest', `Tray Table` = 'Tray Table'))
      )
    } else if (x == 'Safety Equipment') {
      x <- sort(c(`Megaphone` = "Mega Phone", `Oxygen Mask` = "Oxygen Mask", `Fire Extinguisher` = "Fire Extinguisher", `Flash Light` = "Flash Light",
                  `First Aid Kit` = "First Aid Kit", `Crash Axe` = "Crash Axe", `Portable Oxygen Bottle` = "Portable Oxygen Bottle", `Location Transmitter` = "Location Transmitter",
                  `Protective Breathing Equipment(PBE)` = "Protective Breathing Equipment", `Life Vest` = "Life Vest"))
    } else if (x == 'CMIV Defect') {
      x <- sort(c(`System Unable To Boot` = "System Unable To Boot", `System Hangs Often` = "System Hangs Often", `Handset Error` = "Handset Error"))
    }
    
    # Can also set the label and select items
    updateSelectInput(session, "equipment",
                      choices = x
    )
  })
  
  observeEvent(input$reportButton,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "report")
  })
  
  observeEvent(input$backButton,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "home")
  })
  
  observeEvent(input$submitButton,{
    if (is.null(input$seatNumber)) {
      seat <- "0"
    } else {
      seat <- input$seatNumber
    }
    staffName <- input$staffName
    staffId <- input$staffId
    defectNature <- input$defectNature
    defectType <- input$defectType
    timeDiscovery <- input$timeDiscovery
    zone <- input$zone
    equipment <- input$equipment
    tTime <- format(Sys.time(), format="%H:%M:%S")
    #Create a dataframe to upload to SQL database
    tDate = c(Sys.Date())
    tTime = c(tTime)
    staffId = c(staffId)
    staffName = c(staffName)
    flightNo = c("SQ306")
    airlineModel = c("A350-900")
    defectType = c(defectType)
    timeDiscovery = c(timeDiscovery)
    zone = c(zone)
    equipment = c(equipment)
    seat = c(seat)
    defectNature = c(defectNature)
    completed = c("0")
    assigned = c("Unassigned")
    df <- data.frame(tDate,
                     tTime,
                     staffId,
                     staffName,
                     flightNo,
                     airlineModel,
                     defectType,
                     timeDiscovery,
                     zone,
                     equipment,
                     seat,
                     defectNature,
                     completed,
                     assigned)
                    
    myconn <- odbcDriverConnect(connectionString)
    #Inserts into SQL database
    sqlSave(myconn, df, tablename = "defects",rownames=FALSE, append=TRUE)
    
    odbcClose(myconn)
    updateTabsetPanel(session = session, inputId = "navbar", selected = "submit")
  })
  
  observeEvent(input$submitAgainButton,{
    updateTextAreaInput(session, "defectNature","Nature of Defect", "")
    updateTabsetPanel(session = session, inputId = "navbar", selected = "report")
    
  })
  
  observeEvent(input$logOutButton,{
    js$reset()
    
  })
  
#  observeEvent(input$defectImage, {
#    inFile <- input$defectImage
#    if (is.null(inFile))
#      return()
#    
#    file.copy(inFile$datapath, file.path("C:/Users/Zhun/Documents/SIAApp", inFile$name) )
#    drop_upload(inFile$name, dest = "SIA App", dtoken = token)
#  })
  
  
  textboxToggle <- reactive({
    
    if ((input$equipment %in% c('Seat and Seatbelt','Foot Rest', 'Tray Table')) | input$defectType == "CMIV Defect") {
      textInput("seatNumber", "Seat Number:")
    }
    
  })
  
  
  output$textbox_ui <- renderUI({ textboxToggle() })
}


#pic_binary <- paste(readBin("temp.jpg", what="raw", n=1e6), collapse="")
#sqlQuery(myconn, paste("insert into test values (",pic_binary,")", sep=""))

# Run the application 
shinyApp(ui = ui, server = server)

