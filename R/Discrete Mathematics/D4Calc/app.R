#Starter file for any Shiny dashboard app
#This should replace the default app.r that displays Old Faithful data
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
setwd('/Users/hyeongjihyeon/OneDrive - nyu.edu/Dashboard/D4Calc')
#The user interface
header <- dashboardHeader(title = "D4 According to Biggs")
sidebar <- dashboardSidebar(
  width = 110,
  actionButton("btninit", "Initialize"),
  actionButton("btni","Apply i"),
  actionButton("btnr90","Apply r90"),
  actionButton("btnr180","Apply r180"),
  actionButton("btnr270","Apply r270"),    
  actionButton("btnh","Apply h"),
  actionButton("btnv","Apply v"),
  actionButton("btnd","Apply d"),
  actionButton("btnd'","Apply d'")
)
body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      plotOutput("configs", height =200)
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("square", height = 300)
    ),
    column(
      width = 6,
      dataTableOutput("multable")
    )
  )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("d4calc.R")

#Variables that are shared among server functions
D4DF <- D4.makeDataFrame()
config <- "ABCD"

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
    #Initialization
  output$configs <- renderPlot(D4.showConfigs(D4DF))
  output$square <- renderPlot(D4.showSquare(config))
  tbl <-outer(D4DF$name,D4DF$name,vD4.multiply,DF=D4DF)
  colnames(tbl) <- D4DF$name
  rownames(tbl) <- D4DF$name
  #Use options to suppress the fancy controls
  output$multable <- renderDataTable(tbl, options = list(dom = "t"))
    #Functions that respond to events in the input
  observeEvent(input$btninit,{
    config <<- "ABCD"
    output$square <- renderPlot(D4.showSquare(config))
  })

  observeEvent(input$btni,{
      config <<- D4.apply("i",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnr90,{
      config <<- D4.apply("r90",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnr180,{
      config <<- D4.apply("r180",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnr270,{
      config <<- D4.apply("r270",config)
      output$square <- renderPlot(D4.showSquare(config))
  })      
  observeEvent(input$btnh,{
      config <<- D4.apply("h",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnv,{
      config <<- D4.apply("v",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnd,{
      config <<- D4.apply("d",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btndd,{
      config <<- D4.apply("d'",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
}

#Run the app
shinyApp(ui = ui, server = server)
