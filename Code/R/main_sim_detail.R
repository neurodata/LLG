# Simulation for LLG

rm(list = ls())

# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# require(igraph)
# require(rARPACK)
source("function_collection.R")
# source("USVT.R")


## Fix n
n = 1000
# mVec = c(100, 250, 500, 1000, 2000)
mVec = c(250, 500, 1000, 2000)
isSVD = 0

nIter = 1000
nCores = 2

iModel = 1

B = matrix(c(0.42, 0.2, 0.2, 0.7), ncol = 2)
rho = c(0.5, 0.5)

K = length(rho)
tau = rep(1:K,n*rho)
P = B[tau,tau]
diag(P) = 0

d = 2

require(parallel)

for (m in mVec) {
  print(m)
  
  error_P_hat = matrix(0, 2, nIter)
  error_A_bar = matrix(0, 2, nIter)
  
  out <- mclapply(1:nIter, function(x) sim_all(m, n, tau, B, d, isSVD), 
                  mc.cores=nCores)
  out = array(unlist(out), dim = c(3, 2, nIter))
  
  error_A_bar = out[,1,]
  error_P_hat = out[,2,]
  
  if (isSVD) {
    fileName = paste("../../Result/result_sim_", iModel, "_d_", d, "_n_", n, "_m_", m, "_svd.RData", sep="")
  } else {
    fileName = paste("../../Result/result_sim_", iModel, "_d_", d, "_n_", n, "_m_", m, "_eig.RData", sep="")
  }
  
  save(error_A_bar, error_P_hat, n, m, rho, tau, B, d, nIter, file=fileName)
  
}