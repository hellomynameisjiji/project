#install.packages("numbers")
library(numbers)
M.makeDataFrame <- function(q) {
  coprime <- c()
  for (i in 1:q) {
    if (coprime(i, q)) {
      coprime <- c(coprime, i)
    }
  }
  DF <- data.frame(coprime, stringsAsFactors = FALSE)
  return(DF)
}
DF <- M.makeDataFrame(5)


M.multiply  <- function(a, b, q) {
  x <- (a * b) %% q
  return (x)
}

vM.multiply <- Vectorize(M.multiply)


M.orders <- function(a, q) {
  for (i in 1:(q-1)) {
    if (a^i %% q == 1)
      break
  }		
  return(i)
}

vM.orders<- Vectorize(M.orders)
