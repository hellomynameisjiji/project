# Fleury's Algorithm


library(shiny)
library(shinydashboard)
library(shinyWidgets)

header <- dashboardHeader(title = "Euler Walks",
                          titleWidth = 600
                          )
#setwd("/Users/hyeongjihyeon/OneDrive - nyu.edu/Dashboard/EulerWalk")                         
source("FleurysWalk.R")

body <- dashboardBody( 
	fluidRow(
		column(12,
		h2("This graph permits an Euler walk"),
		plotOutput("graph", height = 600, click = "plot_click")
		)
	),
	
	fluidRow(
		column(12,
		textOutput("message")
		),
	
	fluidRow(
		column(12,
		textOutput("walk"),
		)
		
	)
)
)
		

sidebar <- dashboardSidebar(
  fluidRow(
    column(12,  
      sliderInput(inputId = "n_vertices",
                  label = "Number of Vertices",
                  min = 6,
                  max = 12,
                  value = 1)

    )
   ),
  fluidRow(
    column(12,  
    actionButton("btngenerate", "Generate New Graph")
    )
	)
)

ui <- dashboardPage(header, sidebar, body)

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
    edges_df <- euler.makeDataFrame(6)
    eulerwalk <- ""
    
    observeEvent(input$btngenerate, {
    	edges_df <- euler.makeDataFrame(input$n_vertices)
    	node_names <- c(1:nrow(edges_df))
		dfgraph <<- graph_from_data_frame(edges_df, directed = FALSE, vertices = node_names) 
		coords <<- layout_(dfgraph, as_star())
	  output$graph <- renderPlot(plot(dfgraph, layout=coords))
    	output$message <- renderText("Click on vertices to do your Euler walk")
    })

   
    observeEvent(input$plot_click, {
    	if(eulerwalk==""){
    		ver <- which(coords[,1] > (input$plot_click$x-0.001) && coords[,1]<(input$plot_click$x+0.01)&& coords[,2]> (input$plot_click$y-0.001) && coords[,2]<(input$plot_click$y+0.01))
				output$walk <- renderText(ver)
    	} else {
				ver <- which(coords[,1] > (input$plot_click$x-0.001) && coords[,1]<(input$plot_click$x+0.01)&& coords[,2]> (input$plot_click$y-0.001) && coords[,2]<(input$plot_click$y+0.01))
				eulerwalk <- paste0(eulerwalk, ", ", ver)
				output$walk <- renderText({eulerwalk})
    		output$graph <- renderPlot(plot(dfgraph, layout=coords, edge.color = "red"))		
    	}
		
	})
				  
}




#Run the app
shinyApp(ui = ui, server = server)

library(rsconnect)
rsconnect::setAccountInfo(name='jihyeonhyeong', token='0C17F16C75FCAEB7B057D9E361586951', secret='+okLcveFtZbVAZeHoOW9/3ZSmijsHoDyUjPRnqyK') 
deployApp()