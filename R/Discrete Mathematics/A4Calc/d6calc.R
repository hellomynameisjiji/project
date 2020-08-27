#a4calc.R

makeD6data <- function(output) {
  N <- 12
  DF <- data.frame(button=character(N),
                   perm =character(N),color=character(N),stringsAsFactors= FALSE)

 
  DF[1,] <- c("btn2635","(26)(35)","gray90")
    DF[2,] <- c("btn123645","(12)(36)(45)","gray90")
   DF[3,] <- c("btn1346","(13)(46)","gray90")
  DF[4,] <- c("btn142356","(14)(23)(56)","gray90") 
  DF[5,] <- c("btn162534","(16)(25)(34)","gray90")
    DF[6,] <- c("btn1524","(15)(24)","gray90")
  DF[7,] <- c("btn165432","(165432)","gray90")
    DF[8,] <- c("btn153264","(153)(264)","gray90")
     DF[9,] <- c("btn142536","(14)(25)(36)","gray90")
  DF[10,] <- c("btn135246","(135)(246)","gray90")
  DF[11,] <- c("btn123456","(123456)","gray90")
  DF[12,] <- c("btnI","I","gray90")
  return(DF)
}

DF <- makeD6data()



