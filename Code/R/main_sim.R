# Simulation for LLG

rm(list = ls())
# setwd("E:/Dropbox/GitHub/LLG/Code/R")
setwd("/Users/Runze/Dropbox/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

require(igraph)
require(rARPACK)
# library("rARPACK")
# source("graphFunctions.R")
# source("USVT.R")

m = 50
n = 200

maxDim = 6
maxIter = 10

B = matrix(c(0.42, 0.2, 0.2, 0.7), ncol = 2)
rho = c(0.5, 0.5)

set.seed(12345)

K = length(rho)
tau = rep(1:K,n*rho)
P = B[tau,tau]
diag(P) = 0
# tauTmp = rmultinom(n, 1, rho)
# tau = rep(0, 1, n)
# for (k in 1:K) {
#   tau[tauTmp[k,] == 1] = k
# }

MSE_P_Hat = array(rep(0, maxDim*maxIter), dim=c(maxDim, maxIter))
MSE_P_Hat[1,] = NA
MSE_A_Bar = rep(0, 1, maxIter)

for (iter in 1:maxIter) {
  A_all = array(rep(0, n*n*m), dim=c(n, n, m))
  for (i in 1:m) {
    g = sample_sbm(n, B, n*rho, directed=F, loops=F)
    A = as_adj(g, type="both", sparse=FALSE)
    A_all[,,i] = A
  }
  
  A_Bar = rowSums(A_all, dims=2)/m
  MSE_A_Bar[iter] = norm(A_Bar - P, "F")/n/(n-1)
  
  P_Hat = A_Bar
  diag(P_Hat) = rowSums(A_Bar)/(n-1)
  
  ASE = eigs_sym(P_Hat, maxDim, which="LA")
  for (d in 1:maxDim) {
    # Calculate P_Hat
    if (d == 1){
      P_Hat = (ASE$vectors*ASE$values) %*% t(ASE$vectors)
    } else {
      P_Hat = ASE$vectors[,1:d] %*% diag(ASE$values[1:d]) %*% t(ASE$vectors[,1:d])
    }
    diag(P_Hat) = 0
    P_Hat[P_Hat>1] = 1
    P_Hat[P_Hat<0] = 0
    
    MSE_P_Hat[d, iter] = norm(P_Hat - P, "F")/n/(n-1)
  }
}

# Plot
plot(1:d, type="n", ylim=c(0, 0.002), xlab="embedding dimension", ylab="MSE")
points(rowMeans(MSE_P_Hat), type="l", col="blue")
points(rep(mean(MSE_A_Bar), 1, maxDim), type="l", col="red")
