# Simulation for LLG

rm(list = ls())

# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# require(igraph)
# require(rARPACK)
source("function_collection.R")
# source("USVT.R")

n = 200

# mVec = c(1,5,10,50)
mVec = 50

for (m in mVec) {
  for (isSVD in 0:1) {
    
    print(c(m, isSVD))
    
    nIter = 100
    nCores = 2
    
    iModel = 1
    
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
    
    # MSE_P_Hat = array(rep(0, maxDim*maxIter), dim=c(maxDim, maxIter))
    # MSE_A_Bar = rep(0, 1, maxIter)
    
    dVec = 1:n
    nD = length(dVec)
    
    error_P_hat = matrix(0, nD, nIter)
    error_A_bar = matrix(0, nD, nIter)
    
    require(parallel)
    
    out <- mclapply(1:nIter, function(x) dim_brute(m, n, rho, tau, B, dVec, isSVD), 
                    mc.cores=nCores)
    out = array(unlist(out), dim = c(nD+1, nIter))
    
    error_A_bar = out[1,]
    error_P_hat = out[2:(nD+1),]
    
    if (isSVD) {
      fileName = paste("../../Result/result_sim_", iModel, "_brute_n_", n, "_m_", m, "_svd.RData", sep="")
    } else {
      fileName = paste("../../Result/result_sim_", iModel, "_brute_n_", n, "_m_", m, "_eig.RData", sep="")
    }
    
    save(error_A_bar, error_P_hat, n, m, rho, tau, B, dVec, nIter, file=fileName)
    
  }
}