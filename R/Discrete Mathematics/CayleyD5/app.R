library(shiny)
library(shinydashboard)
library(shinyWidgets)
#The user interface
header <- dashboardHeader(title = "Cayley Graph for Group A5 (Dodecahedron)",
                          titleWidth = 600
                          )
sidebar <- dashboardSidebar(disable = TRUE)



#Functions that implement the mathematics
source("cayleya5calc.R")


# dashboard functions

body <- dashboardBody(
    column(
        width = 3,
        h3("Multiplication", style=kahua.s),
        h4("Defining relations:", style=kahua),
        div(HTML(paste(Leerraum(4),CA5.manifestWord("rrrrr")," = I<br>",Leerraum(7),CA5.manifestWord("ff")," = I<br>",Leerraum(3),CA5.manifestWord("frfrfr")," = I",sep="")),style=kahua.s),
        h4("Rewrite rules:", style=kahua),                 
        p(HTML(paste(Leerraum(7),CA5.manifestWord("ff")," &#8658; I<br>",
                       Leerraum(6),CA5.manifestWord("frf")," &#8658; ",CA5.manifestWord("rrrrfrrrr"),"<br>",
                       Leerraum(3),CA5.manifestWord("frrrrf")," &#8658; ",CA5.manifestWord("rfr"),"<br>",
                       Leerraum(4),CA5.manifestWord("rfrrf")," &#8658; ",CA5.manifestWord("frrrfrrrr"),"<br>",
                       CA5.manifestWord("frrrfrrrf")," &#8658; ",CA5.manifestWord("rfrrrfr"),"<br>",
                       Leerraum(4),CA5.manifestWord("rrrrr")," &#8658; I",
                       sep="")),style=kahua.s),
        actionBttn("btnright","Select right operand (red)",style="jelly",color="danger"),
        uiOutput("right"),
        actionBttn("btnleft","Select left operand (green)",style="jelly",color="success"),
        uiOutput("left"),
        actionBttn("btncalc","Calculate the product (purple)",style="jelly",color="royal"),
        uiOutput("prod"),
        uiOutput("message")
    ),
    column(
        width = 6,
        h2("The Cayley Graph", align="center", style = kahua.s),
        div(HTML(paste("Labeling rule: no ",CA5.manifestWord("r")," to the left of three ",CA5.manifestWord("f"),"<br>",
                       "Labeling rule: two ", CA5.manifestWord("f")," appear only as ",CA5.manifestWord("frrrf"),"<br>",
                       "Labeling rule: three ",CA5.manifestWord("f")," appear only as ",CA5.manifestWord("frrfrrrf"),
                       sep="")),style=kahua),
        plotOutput("cayley",height = 768, click = "plot_click"),
#        uiOutput("message")

    ),
    column(
        width = 3,
        h3("Permutations", align="center", style = kahua.s),
        selectInput("chooser",helpText("Choose",span("r", style = "color:royalblue; font-weight: bold"),span(": Order 5",style="font-size:initial"),style=kahua),c("(12345)","(13245)")),
        selectInput("choosef",helpText("Choose",span("f", style = "color:darkorange; font-weight: bold"),span(": Order 2",style="font-size:initial"),style=kahua),c("(12)(34)","(12)(35)","(12)(45)","(13)(45)","(23)(45)")),
        actionBttn("makeperm","Make Permutations",style="jelly")
    )
    
)


ui <- dashboardPage(header, sidebar, body)



#Variables that are shared among server functions
A5DF <- CA5.makeDataFrame()
oldrightop <- rightop <- "" # we're going to store the previous value for the right operand
oldleftop <- leftop <- ""   # as well as both old and new values for the left operand as well
product <- ""
rperm <- "(12345)" # our default r 
fperm <- "(12)(34)" # our default f
#chooseRight <- TRUE
opchoice <- "" # I replaced the chooseRight boolean with a flag that can be "L", "R", or "".
               # This way you can't start accidentally marking vertices without having pushed a button
               # It also lets us reset the colors of vertices once we're done looking at the product
               # or when we change one of the factors

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
    #Initialization
    output$cayley <- renderPlot(CA5.drawGraph(A5DF)) 

#Set a flag so we know how to use the next mouse click
  observeEvent(input$btnleft,{
    output$message <- renderUI(h3("Click on a vertex"))
    opchoice <<- "L"
  })
  observeEvent(input$btnright,{
    output$message <- renderUI(h3("Click on a vertex"))
    opchoice <<- "R"
  })
#Use the mouse click to select a vertex
  observeEvent(input$plot_click,{
    i <- CA5.findClosestVertex(input$plot_click$x,input$plot_click$y)
    if (opchoice == "R"){
      oldrightop <<- rightop
      rightop <<- A5DF$sana[i]
      output$right <- renderUI(h3(HTML(CA5.manifestWord(rightop))))
      oldi <- which(A5DF$sana == oldrightop)
      A5DF <<- CA5.markVertex(A5DF,oldi,"lightgray") # first reset the old right operand
      A5DF <<- CA5.markVertex(A5DF,product,"lightgray") # also reset the product
      A5DF <<- CA5.markVertex(A5DF,i,"orangered") # color the vertex for the new right operand

      output$message <- renderUI("")
    }
    if (opchoice=="L"){
      oldleftop <<- leftop # remember the previous left operand
      leftop <<- A5DF[i,]$sana # find the new left operand
      output$left <- renderUI(h3(HTML(CA5.manifestWord(leftop)))) # tell user what generator word applies
      oldi <- which(A5DF$sana == oldleftop) # we are going need to figure out what to recolor
      A5DF <<- CA5.markVertex(A5DF,oldi,"lightgray")
      A5DF <<- CA5.markVertex(A5DF,product,"lightgray")
      A5DF <<- CA5.markVertex(A5DF,i,"darkturquoise")
      output$message <- renderUI("")
    }
    #Redraw the graph to show the selected vertex
    output$cayley <- renderPlot(CA5.drawGraph(A5DF))
  })									  

#Multiply the selected group elements
  observeEvent(input$btncalc,{
    output$message <- renderUI("")
    product <<- CA5.multiply(leftop,rightop)
    msg <- paste0("The product is ",CA5.manifestWord(leftop),CA5.manifestWord(rightop)," which simplifies to ",CA5.manifestWord(product),".")
    output$prod <- renderUI(h3(HTML(msg)))
    A5DF <<- CA5.markVertex(A5DF,product,"slateblue")
    #Redraw the graph to show the result
    output$cayley <- renderPlot(CA5.drawGraph(A5DF))
  })


#This depends on three inputs but responds only to the button
  observeEvent(input$makeperm,{
    A5DF <<- CA5.makePerms(A5DF,r=input$chooser,f=input$choosef) # consider radio buttons instead of drop-down menu
    output$cayley <- renderPlot(CA5.drawGraph(A5DF,permlabel=TRUE))    
  })							  
}

#Run the app
shinyApp(ui = ui, server = server)