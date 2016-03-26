
# args <- commandArgs(trailingOnly = TRUE)

setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("E:/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# m = 2
nIter = 1000
nCores = 4
# isSVD = 0
nElb = 2

# dataName = "CPAC200"
# dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
dataName = "Talairach"


source("function_collection.R")
tmpList = read_data(dataName, DA=F)
A_all = tmpList[[1]]
n = tmpList[[2]]
M = tmpList[[3]]
rm(tmpList)


add <- function(x) Reduce("+", x)
A_sum = add(A_all)

dMax = ceiling(n*3/5)
elbMat = matrix(0, M, nElb)
for (i in 1:M) {
  print(i)
  A = A_all[[i]]
  evalVec = ase(A, dMax, isSVD)[[1]]
  elbMat[i,] = getElbows(evalVec, n=nElb, plot=F)
}
elbSum = summary(elbMat[,2])
dHat = elbSum[5]

for (m in 1:4) {
  for (isSVD in 0:1) {
    
    print(c(m, isSVD))
    
    error_P_hat = matrix(0, 1, nIter)
    error_A_bar = matrix(0, 1, nIter)
    
    # ptm <- proc.time()
    # proc.time() - ptm
    
    source("getElbows.R")
    
    require(parallel)
    out = mclapply(1:nIter, function(x) llg_d(M, m, A_all, A_sum, dHat), mc.cores=nCores)
    out = array(unlist(out), dim = c(3, nIter))
    error_A_bar = out[1,]
    error_P_hat = out[2,]
    dVec = out[3,]
    
    # mean(error_A_bar)
    # mean(error_P_hat)
    
    if (isSVD) {
      fileName = paste("../../Result/result_", dataName, "_", n, "_", m, "_svd.RData", sep="")
    } else {
      fileName = paste("../../Result/result_", dataName, "_", n, "_", m, "_eig.RData", sep="")
    }
    save(error_A_bar, error_P_hat, n, M, m, dVec, file=fileName)
  }
}