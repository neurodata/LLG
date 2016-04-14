
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("E:/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

dataName = "CPAC200"
# dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"

nElbow = 2
mVec = c(1,2,5,10)
isSVD = 0
nIter = 100

source("function_collection.R")
source("getElbows.R")

dZG = matrix(0, length(mVec), nIter)
tmpList = read_data(dataName, DA=F)
A_all = tmpList[[1]]
n = tmpList[[2]]
M = tmpList[[3]]

A_bar = add(A_all)/M


for (iM in 1:length(mVec)) {
  m = mVec[iM]
  for (iIter in 1:nIter) {
    print(c(m, iIter))
    sampleVec = sample.int(M, m)
    A_bar = add(A_all[sampleVec])/m
    evalVec = ase(A_bar, ceiling(n*3/5), isSVD)[[1]]
    dZG[iM, iIter] = getElbows(evalVec, n=nElbow, plot=F)[[nElbow]]
  }
}

for (iM in 1:length(mVec)) {
  print(mean(dZG[iM,]))
  print(c(mean(dZG[iM,]) - sqrt(var(dZG[iM,])/nIter)*1.96,
               mean(dZG[iM,]) + sqrt(var(dZG[iM,])/nIter)*1.96))
}