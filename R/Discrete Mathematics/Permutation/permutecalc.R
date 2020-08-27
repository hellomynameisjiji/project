# Assignment 1: permute
#
# <- this hash (#) symbol precedes comments in R. You compupter
# will ignore lines that begin with a hash.
#
#
#
#
#
# This assignment has 8 sub-parts. You should do them in order.
# Collaboration is encouraged, and your code should be your own.
# See the details of the extyension school collaboration policy in the syllabus
# (TLDR you must cite your sources **including stack overflow** and collaborators)
#
# 1) download and install R and RStudio
#
# 2) create a folder named Dashboard. This is where you will
# keep all of your R code. Make a subfolder named Permutation.
# Put these files (app.R and permutecalc.R) and into that folder.
#
#  Dashboard
#  |
#  +-- Permute
#      |
#      +-- permutecalc.R
#      +-- app.R
#
# 3) write and test code for the function Perm.apply (see below)
#
# below, we have marked the places where you will write code like this
# ** your code here **
#
# you may find the following functions useful as you are writing your functions:
# print() # (for testing and debugging)
# str_detect()
# str_locate()
# character()
# str_sub()
# paste()
# any function that you have already written
#
# 4) write and test code for the function Perm.cycle.convert (see below)
#
# 5) write and test code for the function Perm.multiply (see below)
#
# 6) write and test code for the function Perm.powerString (see below)
#
# 7) write and test code for the function Perm.inverse (see below)
#
# 8) write and test code for the function Perm.conjugate (see below)
#
# When you are done, run app.R as a final test to make sure all of the parts are working
# and to bask in the glory of having written your own library of permutation functions.
# When you are finished, submit this file, still named permutecalc.R, on gradescope.
#
#
# If you get stuck at any point, spend at least 10-20 minutes trying to get
# un-stuck before you give up. Consult friends and the internet and come to 
# office hours. You are also welcome to email us! If we can't answer your question
# in more than a sentence or two, we will tell you to join our office hours 
# or, if we are feeling extremely generous, we will set up a zoom call with you :)



#The code in this file should work for any set of characters. ABCDE, 12345, etc.
library(stringr)






#3)
# Perm.apply has two inputs and one output
# x - a single character, passed as a string like "1" or "A"
# perm - a permutation written in cycle notation like "(13)(245)"
# output - a single character, returned as a string like "2" or "B"
#
# Perm.apply(x,perm) returns the result of the permutation perm applied to x
# Example usage:
#
# Perm.apply("5",("(13)(245)"))
# [1] "2"
#
Perm.apply <- function(x,perm){
  
  # dummy return value - delete the following line when you
  # start writing your version of this function:
  # return("5")
  
  # If x is not in the permutation, return x
  perm_s <- str_split(perm, "")
  
  if (!(x %in% perm_s[[1]])) {
      return(x)
  # Otherwise locate it and find the location of the next symbol in the permutation
  # ** your code here **
  } else {
      curr_loc = match(x ,perm_s[[1]])
      next_loc = curr_loc+1
      # To extract a character, get the next one-character substring
      # ** your code here **
      next_sym = perm_s[[1]][next_loc]
      # If it is not a special character like "(" or ")", return the string
      # ** your code here **
      if (!(next_sym %in% c("(", ")"))) {
          return(next_sym)
      } else {
      # Otherwise back up to find the number at the start of the cycle
      # and return that instead
      # ** your code here **
      marker = curr_loc
      while (perm_s[[1]][marker] != "(") {
          marker = marker - 1
      }
      return(perm_s[[1]][marker+1])
      }
  }
}

#You may find the following lines useful for testing your code:
#Perm.apply("5",("(13)(245)"))
#Perm.apply("5",("(13)(24)"))
#Perm.apply("5",("(13245)"))
#debug(Perm.apply)
#undebug(Perm.apply)






#4)
# Perm.cycle.convert has one input and one output
# fval - a vector consisting only of non-repeating digits
# corresponding to a permutation, like c("2","4","3","1"), 
# which corresponds to (124)
# output - a permutation written in cycle notation like "(124)"
#
# Perm.cycle.convert(fval) converts the vector fval to cycle notation
# The fval argument corresponds to what we've been calling the "function
# list", so fval=c("2","4","3","1") means a(1) = 2, a(2) = 4, a(3) = 3, a(4)=1
# This is specific to permutations of the digits 1 through 9
# It will be reused in other apps
# Example usage:
#
# Perm.cycle.convert(c("2","4","3","1"))
# [1] "(124)"
#


Perm.cycle.convert <- function(fval){
  
  # dummy return value - delete the following line when you
  # start writing your version of this function:
  # return("(124)")
  #Build the answer as a vector of characters
     output <- "("
     input <- paste0("(", paste(fval, collapse="") , ")")
  # loop through the digits 1:9 in order
  for (i in 1:9) {
  # If the symbol is unchanged or is already in a cycle, there is nothing to do
  # start a new cycle if you reach a number that you have already added to your cycle
    index <-i
       if (!(as.character(i) %in% fval) || (str_detect(output, as.character(index)))) {
      break
    }
      # Otherwise keep tracing through the cycle until you find the end
    # don't forget to close the cycle when it returns to its starting character
       if (substr(output, length(str_split(output,"")[[1]]),length(str_split(output,"")[[1]])) == ")") {
              output <- paste0(output, "(")
          }
          while((!(str_detect(output, as.character(index))))) {
              output <- paste0(output, index)
              temp <- Perm.apply(index, input)
              index <- temp
                }
  #Collapse the vector of cycles into a string and return it
  # ** your code here **
    output <- paste0(output, ")")
      }
  return(output)
}

#You may find the following lines useful for testing your code:
#Perm.cycle.convert(c("2","4","3","1"))
#Perm.cycle.convert(c("2","1","3","4"))
#Perm.cycle.convert(c("2","1","4","3"))
#debug(Perm.cycle.convert)
#undebug(Perm.cycle.convert)






#5)
# Perm.multiply has two inputs and one output
# a - a permutation written in cycle notation like "(24)(567)"
# b - a permutation written in cycle notation like "(123)(4689)"
# output - a permutation written in cycle notation like "(134)"
#
# Perm.multiply(a,b) returns the result of multiplying the permutations ab
# or "b followed by a"
# Example usage:
#
# Perm.multiply("(123)","(12)(34)")
# [1] "(134)"
#
#Compute the product ab of two permutations of the symbols "1" through "9"
Perm.multiply <- function(a,b){
  
  # dummy return value - delete the following line when you 
  # start writing your version of this function:
  #return("(134)")
  
  a_s <- str_split(a, "")
  b_s <- str_split(b, "")
  input <- paste0(a,b)
  # don't forget to include a special case for the identity "I"
  # aI = Ia = a, bI = Ib = b
  # if one permutation is the identity, just return the other
  # ** your code here **
  
  if (isIdentity(a_s[[1]],1)) return(b)
  else if (isIdentity(b_s[[1]],1)) return(a)
  
  # if not, make a vector of the function outputs
  # you may choose to save space/code by using the order
  # if the letters of the inputs as indices (so that you can ignore them)
  # ** your code here **
  else {
      output <- "("
      input <- paste0(a,b)
      sub <- Sub(input, c(), 1)
        

      for (i in 1:9) {
          index <-i
          if (!(str_detect(input, as.character(i))) || (str_detect(output, as.character(index)))) {
              break
          }
                  
          if (substr(output, length(str_split(output,"")[[1]]),length(str_split(output,"")[[1]])) == ")") {
              output <- paste0(output, "(")
          }

          while((!(str_detect(output, as.character(index))))) {
              output <- paste0(output, index)
          if (str_detect(input, as.character(index))){
             for (j in length(sub):1) {
                if(str_detect(sub[j], as.character(index))) {
                  temp <- Perm.apply(index, sub[j])
                      index <- temp
                }
            }
          }
          }
       output <- paste0(output, ")")
      }
  #If input and output are equal, return the identity
  # ** your code here **
  if (all(input == output)) return(Identity(input))
  
  #Otherwise generate cycle notation and return that
  # ** your code here **
   else return(output)
  }
}
#You may find the following lines useful for testing your code:
#Perm.multiply("(123)","(12)(34)")
#Perm.multiply("(12)(34)","(123)")
#Perm.multiply("(12)(34)","(15)(23)")
#debug(Perm.multiply)
#undebug(Perm.multiply)
#if you did it right, your code will be vectorizable as well:
#vPerm.multiply <-   Vectorize(Perm.multiply,c("a","b"))

## isIdentity: determine whether a permutation is an identity or not
    isIdentity <- function(s, marker){
        if (marker >= length(s)) return(TRUE)
                else {
                walk <- marker
            e_num <- -1
             while(s[walk] != ")") {
                 walk <- walk+1
             }
             marker <- walk
             
             while(s[walk] != "(") {
                 walk <- walk-1
                 e_num <- e_num + 1
             }
             
             if (e_num == 1)   return(identity(s, marker+1))
                else    return(FALSE)
             }
    }

##  Sub: separate each cycles into substrings
    Sub <- function(s, permutations, marker){
        s2 <- str_split(s, "")[[1]]
        if (marker >= length(s2)) return(permutations)
                else {
                walk <- marker
             while(s2[walk] != ")") {
                 walk <- walk+1
             }
             marker <- walk
             
             while(s2[walk] != "(") {
                 walk <- walk-1
             }
            
             permutations <- append(permutations, str_sub(s, walk, marker))
             return(Sub(s, permutations, marker+1))
             }
    }

##  Identity: make identity permutation
Identity <- function(s) {
    output <- c()
    for (i in 1:9) {
        if (!(str_detect(s, as.character(i)))) {
            next
        }
        
        output <- paste0(output, "(")
        output <- paste0(output, as.character(i))
        output <- paste0(output, ")")
    }
    
    return(output)
}

#6)
# Perm.powerString has two inputs and one output
# perm - a permutation written in cycle notation like "(135)"
# output - a list of permutations like "(135)<br/>(153)<br/>I"
#
# Perm.powerString(perm) makes a list of powers or perm separated by HTML 
# line breaks ("<br/>")
# Example usage:
#
# Perm.powerString("(135)")
# [1] "(135)<br/>(153)<br/>I"
#
# Makes a list of powers separated by HTML line breaks

Perm.powerString <- function(perm) {
    output <- Perm.multiply(perm, perm)
    return(paste0(perm, "<br/>", output, "<br/>", "I"))
}
Perm.powerString("(135)")
#Perm.powerString("(135)")





#7)
# Perm.inverse has one input and one output
# perm - a permutation written in cycle notation like "(123)(4689)"
# output - a permutation written in cycle notation like "(132)(4986)"
# 
# Perm.inverse(perm) returns the inverse permutation of perm, denoted perm^(-1)
# You can check your code by confirming that perm*perm^(-1) == I, the identity permutation.
# Example usage:
#
# Perm.inverse("(123)(4689)")
# [1] "(132)(4986)"
#
Perm.inverse <- function(perm) {
  output <- c()
  # dummy return value - delete the following line when you
  # start writing your version of this function:
  # return("(135)<br/>(153)<br/>I")
  perm_s <- Sub(perm, c(), 1)
  
  for (i in 1:2) {
        perm_ss <- str_sub(perm_s[i], 2,nchar(perm_s[i]) - 1)
  n<-nchar(perm_ss)
  low <- 1
  high <- nchar(perm_ss)
  while (low <= high){
      temp <- substr(perm_ss, low, low)
      substr(perm_ss,low, low) <- substr(perm_ss, high, high)
    substr(perm_ss, high, high)  <- temp
      low <- low+1
      high <- high -1
  }
  
  if (substr(perm_ss, 1,  1) > substr(perm_ss, nchar(perm_ss),  nchar(perm_ss))) {
      temp <- substr(perm_ss, nchar(perm_ss),  nchar(perm_ss))
      perm_ss <- paste0(temp, substr(perm_ss, 1,nchar(perm_ss)-1))
  }

 output <- paste0(output, "(", perm_ss, ")")
  }
  return(output)
  # ** your code here **
}
#You may find the following lines useful for testing your code:
#Perm.inverse("(123)(4689)")






#8)
# Perm.conjugate has two inputs and one output
# a - a permutation written in cycle notation like "(24)(567)"
# b - a permutation written in cycle notation like "(123)(4689)"
# output - a permutation written in cycle notation like "(143)(2789)"
#
# Perm.conjugate(a,b) returns the conjugate aba^(-1), where a^(-1) is 
# the inverse of a. Said differently, aa^(-1) == I, the identity permutation.
# Example usage:
#
# Perm.conjugate("(24)(567)","(123)(4689)")
# [1] "(143)(2789)"
#
Perm.conjugate <- function(a,b) {
  
  # dummy return value - delete the following line when you 
  # start writing your version of this function:
  #return("(143)(2789)")
  a_inv <- Perm.inverse(a)
  output <- Perm.multiply(a, b)
  output <- Perm.multiply(output, a_inv)
  # ** your code here **
  return(output)
}
#You may find the following lines useful for testing your code:
#Perm.conjugate("(24)(567)","(123)(4689)")

}
