#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
rsconnect::setAccountInfo(name='jihyeonhyeong', token='0C17F16C75FCAEB7B057D9E361586951', secret='+okLcveFtZbVAZeHoOW9/3ZSmijsHoDyUjPRnqyK')
# #Starter file for any Shiny dashboard app
# #This should replace the default app.r that displays Old Faithful data
# library(shiny)
# library(shinydashboard)
# library(shinyWidgets)


#The user interface
header <- dashboardHeader(title = "Modular Multiplication")
sidebar <- dashboardSidebar(
    width = 200,
    fluidRow(
        column(12,  
               numericInput("q", label = "q", value = 5),
        )
    ),
    fluidRow(
        column( 12,  
                actionButton("btndisplay", "Display Table")
        )
    )
)
body <- dashboardBody(
    fluidRow(
        column(
            width = 12,
            h3("Multiplication Table"),
            dataTableOutput("multable", height =600)
        )
    ),
    fluidRow(
        column(
            width = 12,
            h3("Order of Elements"),
            plotOutput("orders", height =300)
        )
    ) 
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("modular.R")

#Variables that are shared among server functions
MDF <- M.makeDataFrame(q)
q <- 5
#Functions that read the input and modify the output and input
shinyServer <- function(input, output, session) {
    tbl <-outer(MDF$coprime, MDF$coprime, vM.multiply, q = q)
    colnames(tbl) <- MDF$coprime
    rownames(tbl) <- MDF$coprime
    output$multable <- renderDataTable(tbl, options = list(dom = "t"))
    
    orders <-(sapply(DF$coprime, vM.orders, q = q))
    table(orders)
    output$orders <- renderPlot({
        barplot(table(orders))
        text(orders, labels = DF$coprime)
    }, res = 96)
    observeEvent(input$btndisplay,{
        MDF <- M.makeDataFrame(input$q)
        tbl <-outer(MDF$coprime, M.orders, q = input$q)
        colnames(tbl) <- MDF$coprime
        rownames(tbl) <- MDF$coprime
        output$multable <- renderDataTable(tbl, options = list(dom = "t"))
        
        orders <-(sapply(DF$coprime, vM.orders, q = input$q))
        table(orders)
        output$orders <- renderPlot({
            barplot(table(orders))
            text(orders, labels = DF$coprime)
        }, res = 96)
        
    })
    
}
#Run the app
shinyApp(ui = ui, server = server)

