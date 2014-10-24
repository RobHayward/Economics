rm(list = ls())
# Set up the transition matrix first.  This is the probability that the bond will migrate from one
# rating to another.  There is a 97% chance that an A rated bond will remain A. 
Pi <- list()
Pi[[1]] <- matrix(c(0.97, 0.05, 0.01, 0.00, 0.00, 0.02, 0.80, 0.02, 0.00, 0.00, 
                    0.01, 0.15, 0.75, 0.00, 0.00, 0.00, 0.00, 0.22, 0.00, 0.00, 
                    0.00, 0.00, 0.00, 1, 1), nrow = 5)
Pi[[1]]
bondprice <- function(F = 100, Q = 0.05, Rate = "B", Pi, t, n, lambda = 0.4){  
# Is the period (t) before maturity *n)?
PO <- function(Q, lambda){
  POa <- c(Q, Q, Q, lambda, 0)
POb <- c(1 + Q, 1 + Q, 1 + Q, lambda, 0)
  if(t < n){
    PO <- POa
 } else if(t == n) {
   PO <- POb
} else if(t > n) {
  PO <- 0
}
return(PO)
}
n = 2; t = 1
Q = 0.05
lambda = 0.5
PO
# Initial matrix (In) from rating ("Rate")
Rate <- function(Rate){
  if(Rate == "A"){
  In <- c(1, 0, 0, 0, 0)
} else if(Rate == "B"){
  In <- c(0, 1, 0, 0, 0)
} else if(Rate == "C"){
  In <- c(0, 0, 1, 0, 0)
} else if(Rate == "D"){
  In <- c(0, 0, 0, 1, 0)
} else if(Rate == "E"){
  In <- c(0, 0, 0, 0, 1)
}
}
print(In)
# Pi is the transition matrix. These probabilities are given.                       
# Probabilities for n periods ahead.  
Trans <- function(Pi, n){
    for(i in 1:(n - 1))
 Pi[[1 + i]] <- Pi[[i]] %*% Pi[[1]]
Pi[[n]]
 }   
R <- Trans(Pi, n)
print(R)
return <- In %*% R %*% PO  
}  
s <- bondprice(Rate = "C", Pi = Pi[1], t = 9, n = 10, lambda = 0.5)
s
# These are all for testing.  It seems to work. Need to consider the discounted cash flow. 
n = 10
R
n = 10
t = 2
Q = 100
lambda = 0.4
PO
Rate <- c("A", "B", "C", "D", "E")
b <- sapply(Rate, bondprice, Pi = Pi[1], t = 10, n = 10, lambda = 0.9)
b


# In is the initial state
# Pi is the transission matrix
Pi <- matrix(c(0.97, 0.05, 0.01, 0.00, 0.00, 0.02, 0.80, 0.02, 0.00, 0.00, 
                    0.01, 0.15, 0.75, 0.00, 0.00, 0.00, 0.00, 0.22, 0.00, 0.00, 0.00, 
                    0.00, 0.00, 1, 1), nrow = 5)

Pi[[1]] <- matrix(c(0.97, 0.05, 0.01, 0.00, 0.00, 0.02, 0.80, 0.02, 0.00, 0.00, 
                    0.01, 0.15, 0.75, 0.00, 0.00, 0.00, 0.00, 0.22, 0.00, 0.00, 0.00, 
                    0.00, 0.00, 1, 1), nrow = 5)
EFlow <- function(Pi, Rate = "B", t, n, lambda = 0.4, Q, Y = 100){
  # Pipower will run the trasmission matrix forward n periods 
  Pipower <- function(Pi, n){
    for(i in 1:(n - 1))
      Pi[[1 + i]] <- Pi[[i]] %*% Pi[[1]]
    return(Pi[[n]])
  }  
n = 2
Pi  
Pi[[n]] <- Pipower(Pi, n)
Pi[[2]]
# Is the payout t = n or t < n
  POa <- c(Q, Q, Q, lambda, 0)
  POb <- c(1 + Q, 1 + Q, 1 + Q, lambda, 0)
  if(t < n){
    PO <- POa
  } else if(t == n){
    PO <- POb
  } else {
    PO <- 0
  }
In <- c(rep, 0, 5)
  if(Rate == "A"){
    In <- c(1, 0, 0, 0, 0)
  } else if(Rate == "B"){
    In <- c(0, 1, 0, 0, 0)
  } else if(Rate == "C"){
    In <- c(0, 0, 1, 0, 0)
  } else if(Rate == "D"){
    In <- c(0, 0, 0, 1, 0)
  } else if(Rate == "E"){
    In <- c(0, 0, 0, 0, 1)
  }
Int <- t(In)  
  Eflow <- Int %*% Pi[[n]] %*% PO
  Return(Eflow)
}
a <- EFlow(Pi, Q = 0.08, n = 2, t = 2)
Q <- 0.08
lambda = 0.4
t = 2
n = 2
Rate = "B"

