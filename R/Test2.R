rm(list = ls())
# Set up the transition matrix first.  This is the probability that the bond will migrate from one
# rating to another.  There is a 97% chance that an A rated bond will remain A. 
Pi <- list()
Pi[[1]] <- matrix(c(0.97, 0.05, 0.01, 0.00, 0.00, 0.02, 0.80, 0.02, 0.00, 0.00, 
                    0.01, 0.15, 0.75, 0.00, 0.00, 0.00, 0.00, 0.22, 0.00, 0.00, 
                    0.00, 0.00, 0.00, 1, 1), nrow = 5)
Pi[1]
bondprice <- function(F = 100, Q = 0.05, Rate = "B", Pi, t, n, lambda = 0.4){  
# Is the period (t) before maturity *n)?
POa <- c(Q, Q, Q, lambda, 0)
POb <- c(1 + Q, 1 + Q, 1 + Q, lambda, 0)
     if(t < n){
    PO <- POa
 } else {
   PO <- POb
} 
n = 2; t = 1
PO
# Initial matrix (In) from rating ("Rate")
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
# Pi is the transition matrix. These probabilities are given.                       
# Probabilities for n periods ahead.  
Trans <- function(Pi, n){
    for(i in 1:(n - 1))
 Pi[[1 + i]] <- Pi[[i]] %*% Pi[[1]]
Pi[[n]]
 }   
R <- Trans(Pi, n)
Return <- In %*% R %*% PO  
}  
s <- bondprice(Rate = "B", Pi = Pi[1], t = 10, n = 10, lambda = 0.5)
s * 100
# These are all for testing.  It seems to work. Need to consider the discounted cash flow. 
R
n = 2
t = 2
Q = 100
lambda = 0.4
PO
Rate <- c("A", "B", "C", "D", "E")
b <- sapply(Rate, bondprice, Pi = Pi[1], t = 10, n = 10, lambda = 0.9)
b