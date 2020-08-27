#a4calc.R

makeA4data <- function(output) {
  N <- 12
  DF <- data.frame(button=character(N),
                   perm =character(N),color=character(N),stringsAsFactors= FALSE)
  DF[1,] <- c("btn2413","(13)(24)","gray90")
  DF[2,] <- c("btn2314","(14)(23)","gray90")
  DF[3,] <- c("btn3412","(12)(34)","gray90")
  DF[4,] <- c("btn234","(234)","gray90")
  DF[5,] <- c("btn243","(243)","gray90")
  DF[6,] <- c("btn134","(134)","gray90")
  DF[7,] <- c("btn143","(143)","gray90")
  DF[8,] <- c("btn124","(124)","gray90")
  DF[9,] <- c("btn142","(142)","gray90")
  DF[10,] <- c("btn123","(123)","gray90")
  DF[11,] <- c("btn132","(132)","gray90")
  DF[12,] <- c("btnI","I","gray90")
  return(DF)
}

#DF <- makeA4data()

