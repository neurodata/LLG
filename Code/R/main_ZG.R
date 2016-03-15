
# args <- commandArgs(trailingOnly = TRUE)

setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("E:/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# require(rARPACK)

# m = as.numeric(args[1])
m = 5
nIter = 50
nCores = 4

dataName = "CPAC200"
# dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"


source("function_collection.R")
tmpList = read_data(dataName, DA=F)
A_all = tmpList[[1]]
n = tmpList[[2]]
M = tmpList[[3]]
rm(tmpList)


add <- function(x) Reduce("+", x)
A_sum = add(A_all)

error_P_hat = matrix(0, 1, nIter)
error_A_bar = matrix(0, 1, nIter)

# ptm <- proc.time()
# proc.time() - ptm

require(parallel)
out <- mclapply(1:nIter, function(x) sapply(dVec, function(d) dim_brute(M, m, d, A_all)),
                mc.cores=nCores)
out = array(unlist(out), dim = c(2, nD, nIter))
error_A_bar = out[1,,]
error_P_hat = out[2,,]

# for (iD in 1:nD) {
#   print(iD)
#   d = dVec[iD]
#   tmp = replicate(nIter, dim_brute(M, m, d, A_all))
# }

# library(foreach)
# library(doParallel)
# registerDoParallel(4)
# ptm <- proc.time()
# foreach(iD = 1:nD) %dopar% {
#   print(iD)
#   d = dVec[iD]
#   for (iIter in 1:nIter) {
#     out = dim_brute(M, m, d, A_all)
#     error_A_bar[iD, iIter] = out[1]
#     error_P_hat[iD, iIter] = out[2]
#   }
# }
# proc.time() - ptm
# stopImplicitCluster()


# for (iD in 1:nD) {
#   print(iD)
#   d = dVec[iD]
#   for (iIter in 1:nIter) {
#     out = dim_brute(M, m, d, A_all)
#     error_A_bar[iD, iIter] = out[1]
#     error_P_hat[iD, iIter] = out[2]
#   }
# }

fileName = paste("../../Result/result_", dataName, "_", n, "_brute_", m, ".RData", sep="")
save(error_A_bar, error_P_hat, n, M, m, dVec, file=fileName)
