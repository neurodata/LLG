
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("E:/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

dataName = "CPAC200"
# dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"

mVec = c(1,2,5,10)
isSVD = 0
nIter = 100

source("function_collection.R")
source("USVT.R")

dUSVT = matrix(0, length(mVec), nIter)
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
    dUSVT[iM, iIter] = length(usvt(A_bar, 1, m)$d)
  }
}

for (iM in 1:length(mVec)) {
  print(mean(dUSVT[iM,]))
  print(c(mean(dUSVT[iM,]) - sqrt(var(dUSVT[iM,])/nIter)*1.96,
          mean(dUSVT[iM,]) + sqrt(var(dUSVT[iM,])/nIter)*1.96))
}