
rm(list = ls())
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("E:/GitHub/Experiment_SWU4")
# setwd("/cis/home/rtang/Experiment_SWU4")

require(rARPACK)

m = 5;
nIter = 500;
dVec = 2:199;

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


nD = length(dVec)

add <- function(x) Reduce("+", x)
A_sum = add(A_all)

error_P_hat = matrix(0, nD, nIter)
error_A_bar = matrix(0, nD, nIter)

# ptm <- proc.time()

# sapply(dVec, function(d) replicate(nIter, dim_brute(M, m, d, A_all)))

for (iD in 1:nD) {
  print(iD)
  d = dVec[iD]
  tmp = replicate(nIter, dim_brute(M, m, d, A_all))
}

# proc.time() - ptm

fileName = paste("../../Result/result_", dataName, "_", n, "brute_", m, ".RData", sep="")
save(error_P_hat, error_A_bar, n, M, m, dVec, file=fileName)
