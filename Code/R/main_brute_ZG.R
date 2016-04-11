
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("E:/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# dataName = "CPAC200"
dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"

nElbow = 2
mVec = c(1,2,5,10)
isSVD = 0
nIter = 10

source("function_collection.R")
source("getElbows.R")

dZG = matrix(0, length(mVec), nIter)

for (iM in 1:length(mVec)) {
  m = mVec[iM]
  for (iIter in 1:nIter) {
    print(c(m, iIter))
    
    tmpList = read_data(dataName, DA=F)
    A_all = tmpList[[1]]
    n = tmpList[[2]]
    M = tmpList[[3]]
    
    A_bar = add(A_all)/M
    evalVec = ase(A_bar, ceiling(n*3/5), isSVD)[[1]]
    dZG[iM, iIter] = getElbows(evalVec, n=nElbow, plot=F)[[nElbow]]
  }
}

