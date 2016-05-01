rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# dataName = "CPAC200"
# dataName = "desikan"
dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"

mVec = 1:100

library(ggplot2)
source("function_collection.R")

dHatEIG = matrix(0, 1, length(mVec))
dHatSVD = matrix(0, 1, length(mVec))

for (iM in 1:length(mVec)) {
  m = mVec[iM]
  
  # SVD
  fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_svd.RData", sep="")
  load(fileName)
  errorPhatSVDMean = rowMeans(error_P_hat)
  dHatSVD[iM] = which.min(errorPhatSVDMean)
  
  # Eigen-decomposition
  fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_eig.RData", sep="")
  load(fileName)
  errorPhatEIGMean = rowMeans(error_P_hat)
  dHatEIG[iM] = which.min(errorPhatEIGMean)
}

df = data.frame(m=rep(mVec,2), dHat=c(dHatSVD, dHatEIG),
                flag=c(rep("Phat_SVD",length(mVec)),
                       rep("Phat_EIG",length(mVec))))

p = ggplot(data=df, aes(x=m, y=dHat, color=flag)) + geom_point() + geom_line()
p = p + theme(legend.text=element_text(size = 12, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title = element_text(face="bold"))
p = p + scale_x_continuous(name="m") + scale_y_continuous(name="d_hat")
p = p + ggtitle(paste(dataName, ", n=", n, ", M=", M, sep=""))
# p = p + expand_limits(y=c(yMin, yMax))
p

