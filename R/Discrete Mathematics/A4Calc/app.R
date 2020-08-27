#GroupD6

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)


source("buttonrows.R")

ui <- dashboardPage(
    dashboardHeader(title = "Group D6, rotational symmetries of the hexagon",
                    titleWidth = 500),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            column(width=4,
                   box(
                       width = NULL,
                       height = 450,
                       h3 ("Elements of the group"),
                       h4("The identity"),
                       controlRow1("ctrlI"),
                       h4("Order 6 elements (rotations through 60 degrees)"),
                       controlRow4(
                           c("ctrl123456", "ctrl135246", "ctrl142536")
                       ),   
                       controlRow4(
                           c( "ctrl153264", "ctrl165432")
                       ),   

                       h4("Order 2 elements (flips through 180 degrees)"),
                       controlRow3(
                           c("ctrl2635", "ctrl123645", "ctrl1346")
                       ),
                       controlRow3(
                           c("ctrl142356", "ctrl162534", "ctrl1524")
                       ),
                   ),
                       box(
                         width = NULL,
                         height = 100,
                         title = "Subgroups",
                         buttonRow3(
                           inputIds = c("btnC2", "btnC3", "btnC6"),
                           labels = list("Show C2", "ShowC3", "ShowC6"),
                           btnStyle = "padding:4px;font-size:120%"
                         )   
                       ),#box
                       box(
                         width = NULL,
                         height = 100,
                         title = "Cosets",
                         buttonRow2(
                           inputIds = c("btnLC", "btnRC"),
                           labels = list("Left Cosets", "Right Cosets"),
                           btnStyle = "padding:4px;font-size:120%"
                         )  #agb
                       ),#box
                       box(
                         width = NULL,
                         height = 120,
                         title = "Conjugate Subgroup",
                         buttonRow2(
                           inputIds = c("btnmark", "btnconj"),
                           labels = list("Select a", "Generate Subgroup"),
                           btnStyle = "padding:4px;font-size:120%"
                         ),  
                         h4(uiOutput("conjmsg"))
                       ),#box
                       box(
                         width = NULL,
                         height = 120,
                         title = "Generate a Subgroup",
                         buttonRow4(
                           inputIds = c("btnmarkgena", "btnmarkgenb", "btngen", "btnclear"),
                           labels = list("Generator a", "Generator b","Generate","Clear"),
                           btnStyle = "padding:4px;font-size:120%"
                         ),  
                         h4(uiOutput("genmsg"))
                       )#box
                    

            ),
            #col
            column(
                width = 8,
                box(
                  width = NULL,
                  height = 380,
                  fluidRow(
                    column(
                      width = 8,
                      h3("Inputs and Products"),

                      htmlOutput("results"),
                      tags$head(tags$style("#results{color:red; font-size:20px; font-style:italic; 
overflow-y:scroll; max-height: 300px; background: ghostwhite;}"))
                    ),
                    column(
                      width = 4,
                      actionBttn("reset", "Clear Inputs and Products")
                    )

                  )
                ),
                box(width = NULL,
                    height = 430,
                    tableOutput("multable")
                )
                
                
            )
        )  #fluid
    )  
)


source("d6calc.R")
source("permutecalc.R")

#Global variables accessible to server()
N <- 12

D6DF <- makeD6data()

#colors for cosets
color.list <- c("pink","aquamarine","beige","hotpink", "violet")

#Output to display in the text box
result.list <- ""
#Result of all multiplications so far
product <- "I"
subgroup <- numeric(0)
conjugating <- FALSE
generating <- 0
a <-"I"
gena <- "I"
genb <- "I"

#Computes a product as specified by "a" and "b" in vector v
evaluate <- function(v,a,b) {
  result <- "I"
  for (i in 1:length(v)){
    result <- Perm.multiply(result,ifelse(v[i]=="a",a,b))
  }
  return (result)
}


server <- function(input, output, session) {
  #Elements in the chosen subgroup
  displayButton <- function(i) {
    renderUI({actionButton(D6DF[i,1],D6DF[i,2],
                           style=paste("padding:4px;
                   font-size:120%;background:",D6DF[i,3]))}) 
  }
  #show all the buttons
  showButtons <- function() {
    output$ctrl2635 <- displayButton(1)
    output$ctrl123645<- displayButton(2)                                     
    output$ctrl1346<- displayButton(3)
    output$ctrl142356<- displayButton(4)
    output$ctrl162534 <- displayButton(5)
    output$ctrl1524<- displayButton(6)                                     
    output$ctrl165432<- displayButton(7)
    output$ctrl153264<- displayButton(8)
    output$ctrl142536 <- displayButton(9)
    output$ctrl135246<- displayButton(10)                                     
    output$ctrl123456<- displayButton(11)
    output$ctrlI<- displayButton(12)
  }
  showButtons()
  #Display the multiplication table
  tbl <- outer(D6DF[,2],D6DF[,2],Vectorize(Perm.multiply,c("a","b")))
  colnames(tbl) <- D6DF[,2]
  rownames(tbl) <- D6DF[,2] 
  output$multable <- renderTable(tbl,rownames = TRUE)
#Multiplies by a specified permutation and displays all calculations so far
    compute.and.show <- function(perm){
      if (conjugating) {
        a <<- perm
        output$conjmsg <- renderUI(paste0("Conjugating by element ",perm,collapse=""))
        conjugating <<- FALSE
        return()
      }
      if (generating==1) {
        gena <<- perm
        output$genmsg <- renderUI(paste0("Generating with element ",gena,collapse=""))
        return()
      }
      if (generating==2) {
        genb <<- perm
        output$genmsg <- 
          renderUI(paste0("Generating with elements ",gena," and ", genb,collapse=""))
        return()
      }
      product <<- Perm.multiply(perm,product)
      line.out <- paste(perm,product,sep = "&emsp;")
      result.list <<- paste(result.list, line.out, "<br/>")
      output$results<-renderUI(HTML(result.list))
    }
    #Marks all elements in a subgroup with a color
    mark.subgroup <- function() {
      for (i in 1:N){
        D6DF$color[i] <<- ifelse(i  %in% subgroup,"yellow","gray90")
      }
    }
#Event handlers for all the element buttons 
    observeEvent(input$btnI,{
      compute.and.show("I")
    })
    observeEvent(input$btn2635,{
       compute.and.show("(26)(35)")
    })
    observeEvent(input$btn123645,{
      compute.and.show("(12)(36)(45)")
    })
    observeEvent(input$btn1346,{
      compute.and.show("(13)(46)")
    })
    observeEvent(input$btn142356,{
      compute.and.show("(14)(23)(56)")
    })
    observeEvent(input$btn162534,{
      compute.and.show("(16)(25)(34)")
    })
    observeEvent(input$btn1524,{
      compute.and.show("(15)(24)")
    })
    observeEvent(input$btn165432,{
      compute.and.show("(165432)")
    })
    observeEvent(input$btn153264,{
      compute.and.show("(153)(264)")
    })
    observeEvent(input$btn142536,{
        compute.and.show("(14)(25)(36)")
    })
    observeEvent(input$btn135246,{
      compute.and.show("(135)(246)")
    })
    observeEvent(input$btn123456,{
      compute.and.show("(123456)")
    })
#The reset button clears the output and reinitializes the product
    observeEvent(input$reset,{
        result.list <<- ""
        product <<- "I"
        output$results<-renderUI(HTML(result.list))
    })
#Event handlers for the subgroup buttons
    observeEvent(input$btnC2,{
      subgroup <<- c(1,9, 12)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$btnC3,{
      subgroup <<- c(8,10,12)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$btnC6,{
      subgroup <<- c(7:12)
      mark.subgroup()
      showButtons()
    })
    #Event handler for left cosets
    observeEvent(input$btnLC,{
      mark.subgroup()
      idx = 1   #index into the color list -- one for each coset
      #Keep creating cosets as long as there are elements that are still gray
      while(length(which(D6DF$color == "gray90") >0)){
        #Find the first unassigned group element
        in.coset <- which(D6DF$color == "gray90")[1]
        #Generate its left coset and put a new color on the buttons
        for (j in 1:N) {
          if(j %in% subgroup) {
            element <- Perm.multiply(D6DF[in.coset,2],D6DF[j,2])
            k <- which(D6DF[,2] == element)
            D6DF[k,3] <<- color.list[idx]
          }
        }
        idx <- idx + 1
      }
      showButtons()
    })
    #Right cosets work the same way
    observeEvent(input$btnRC,{
      mark.subgroup()
      idx = 1   #index into the color list -- one for each coset
      #Keep creating cosets as long as there are elements that are still gray
      while(length(which(D6DF$color == "gray90") >0)){
        #Find the first unassigned group element
        in.coset <- which(D6DF$color == "gray90")[1]
        #Generate its left coset and put a new color on the buttons
        for (j in 1:N) {
          if(j %in% subgroup) {
            element <- Perm.multiply(D6DF[j,2],D6DF[in.coset,2])
            k <- which(D6DF[,2] == element)
            D6DF[k,3] <<- color.list[idx]
          }
        }
        idx <- idx + 1
      }
      showButtons()
    })
    
    
    observeEvent(input$btnmark,{
      conjugating <<- TRUE
      output$conjmsg <- renderUI("Click the button for the desired element a")
    })
    observeEvent(input$btnmarkgena,{
      generating <<- 1
      D6DF[,3] <<- rep("gray90",N)
      showButtons()
      output$genmsg <- renderUI("Click the button for generator a")
    })
    observeEvent(input$btnmarkgenb,{
      generating <<- 2
      D6DF[,3] <<- rep("gray90",N)
      showButtons()
      output$genmsg <- renderUI("Click the button for generator b")
    })
    #Generate random sequences of generators.
    #If we generate more than half the group, it's the entire group
    #This algorithm could turn out to be inefficient,and in principle it can fail
    observeEvent(input$btngen,{
      subgroup <<- numeric(0)
      for (j in 1:(4*N)) {
        v <- sample(c("a","b"),sample(7:10,1),replace = TRUE)
        element <- evaluate(v,gena,genb)
        k <- which(D6DF[,2] == element)[1]
        if(!(k %in% subgroup)){
          subgroup <<- c(subgroup,k)
          D6DF[k,3] <<- "yellow"
        }
        #If subgroup has more than N/2 elements, it's the entire group
        if (length(subgroup) > N/2){
          subgroup <<- 1:N
          break
        } 
      }  
      mark.subgroup()
      showButtons()
      output$genmsg <- 
        renderUI(paste0("The subgroup generated by ",gena," and ", genb," is now yellow"))
    })
    observeEvent(input$btnclear,{
      subgroup <<- rep(FALSE,N)
      generating <<- 0
      gena <<- "I"
      genb <<- "I"
      mark.subgroup()
      showButtons()
      output$genmsg <- renderUI("")
    })
    observeEvent(input$btnconj,{
      aInv <- Perm.inverse(a)
      D6DF[,3] <<- rep("gray90",N)
      for (j in 1:N) {
        if (j %in% subgroup){
          element <- Perm.conjugate(a,D6DF[j,2])
          k <- which(D6DF[,2] == element)[1]
          D6DF[k,3] <<- "pink"
        }
      }  
      showButtons()
      output$conjmsg <- renderUI(paste0("The subgroup ",a,"H",aInv," is now pink"))
    })}

# Run the application 
shinyApp(ui = ui, server = server)

rsconnect::setAccountInfo(name='jihyeonhyeong', token='0C17F16C75FCAEB7B057D9E361586951', secret='+okLcveFtZbVAZeHoOW9/3ZSmijsHoDyUjPRnqyK') 

deployApp()


