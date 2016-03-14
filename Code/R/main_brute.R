
rm(list = ls())
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("E:/GitHub/Experiment_SWU4")
# setwd("/cis/home/rtang/Experiment_SWU4")

require(rARPACK)
# library("igraph")
# source("graphFunctions.R")
# source("USVT.R")

dataName = "CPAC200"
# dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"

################ Read M graphs ################
source("function_collection.R")
tmpList = read_data(dataName, DA=F)
A_all = tmpList[[1]]
n = tmpList[[2]]
M = tmpList[[3]]
rm(tmpList)



m = 5;
nIter = 10;
dVec = 2:5;

nD = length(dVec)

add <- function(x) Reduce("+", x)
A_sum = add(A_all)

error_P_hat = matrix(0, nD, nIter)
error_A_bar = matrix(0, nD, nIter)

for (iD in 1:nD) {
  print(iD)
  d = dVec[iD]
  for (iIter in 1:nIter) {
    print(iIter)
    sampleVec = sample.int(M, m)
    A_bar = add(A_all[sampleVec])
    P_bar = (A_sum - A_bar)/(M - m);
    A_bar = A_bar/m;
    
    ASE = eigs_sym(as.matrix(diag_aug(A_bar)), d, which = "LM")
    P_hat =  regularize(ASE$vectors %*% diag(ASE$values) %*% t(ASE$vectors))
    
    error_P_hat[iD, iIter] = norm(P_bar - P_hat, "F")/n/(n-1)
    error_A_bar[iD, iIter] = norm(P_bar - A_bar, "F")/n/(n-1)
  }
}

fileName = paste("../../Result/result_", dataName, "_", n, "brute_", m, ".RData", sep="")
save(error_P_hat, error_A_bar, n, M, m, dVec, file=fileName)
