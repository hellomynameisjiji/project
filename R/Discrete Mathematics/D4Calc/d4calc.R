#D4calc.R - Symmetries of the square

D4.makeDataFrame <- function() {
  DF <- data.frame(name=rep("",8),ABCD=rep("",8),stringsAsFactors = FALSE)
  DF[1,] <- c("i","ABCD")
  DF[2,] <- c("r90","DABC")
  DF[3,] <- c("r180","CDAB")
  DF[4,] <- c("r270","BCDA")
  DF[5,] <- c("h","DCBA")
  DF[6,] <- c("v","BADC")
  DF[7,] <- c("d","CBAD")
  DF[8,] <- c("d'","ADCB")
  return(DF)
}

DF <- D4.makeDataFrame()
D4.showConfigs <- function(DF) {
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,24),ylim = c(-1,3), asp = 1, axes = FALSE)
  for (i in 0:7) {
    points(c(0,2,2,0,0)+3*i,c(0,0,2,2,0),type = "l")    
    lbl <- strsplit(DF[i+1,2],"")[[1]]

    text(c(0.25,1.75,1.75,0.25)+3*i,c(1.75,1.75,0.25,0.25),lbl)
    text(1+3*i,-0.5,DF[i+1,1])
    segments(c(12,16,18,23),c(1,0,0,0),
             c(14,16,20,21),
             c(1,2,2,2),lty = 2)
  }
}
D4.showConfigs(DF)

#ABCD is a string of symbols, reading counterclockwise from the left-top
D4.showSquare <- function(ABCD){
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,3),ylim = c(-1,2), asp = 1, axes = FALSE)
  points(c(0,2,2,0,0),c(0,0,2,2,0),type = "l", lwd = 2)
  lbl <- strsplit(ABCD,"")[[1]]
  text(c(0.25,1.75,1.75,0.25),c(1.75,1.75,0.25,0.25),lbl)
}
D4.showSquare("ABCD")

#a is one of the Biggs symbols for an operation

#The return value is the new configuration
D4.apply <- function(a,ABCD){
  v <-strsplit(ABCD,"")[[1]]
  w <- switch(a,
              "i" = v,
              "r90" = c(v[4], v[1], v[2], v[3]),
              "r180" = c(v[3], v[4], v[1], v[2]),
              "r270" = c(v[2], v[3], v[4], v[1]),
              "h" = c(v[4], v[3], v[2], v[1]),
              "v" = c(v[2], v[1], v[4], v[3]),
              "d" = c(v[3], v[2], v[1], v[4]),
              "d'" = c(v[1], v[4], v[3], v[2]),
  )
  s <- paste(w,sep="",collapse="")
  return(s)
}


D4.multiply <- function(DF,a,b){
  #Look up the name
  idx <- which(DF$name==b)[1]
  #Find the corresponding configuration
  ABCD <- DF$ABCD[idx]
  #Apply the group operation to it
  newABCD <- D4.apply(a,ABCD)
 # Look up the configuration
  idx <- which(DF$ABCD==newABCD)[1]
  return (DF$name[idx])
}
vD4.multiply <- Vectorize(D4.multiply,c("a","b"))

