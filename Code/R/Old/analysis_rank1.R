rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

dataName = "CPAC200"
# dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"

mVec = c(1, 2, 5, 10)

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)

mVec = c(1,2,5,10)

for (iM in 1:length(mVec)) {
  m = mVec[iM]
  
  # Eigen-decomposition
  fileName = paste("../../Result/result_", dataName, "_brute_rank1_",
                   "m_", m, "_eig.RData", sep="")
  #   fileName = paste("../../Result/result_", dataName, "_brute_",
  #                    "m_", m, "_eig.RData", sep="")
  load(fileName)
  
  if (iM == 1) {
    errorPhatEIGMean = array(0, dim=c(length(mVec), n))
    errorPhatEIGLower = array(0, dim=c(length(mVec), n))
    errorPhatEIGUpper = array(0, dim=c(length(mVec), n))
  }
  
  error_P_hat = error_P_hat^2*n*(n-1)
  
  errorPhatEIGMean[iM,] = rowMeans(error_P_hat)
  errorPhatEIGLower[iM,] = errorPhatEIGMean[iM,] - 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  errorPhatEIGUpper[iM,] = errorPhatEIGMean[iM,] + 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  
}

error_by_dim_df <- rbind(
  data.frame(mse=c(errorPhatEIGMean),lci=c(errorPhatEIGLower),uci=c(errorPhatEIGUpper),
             which="Phat",m=rep(mVec,n),d=rep(1:n,each=4))) %>%
  mutate(m=factor(paste0("m=",m),c("m=1","m=2","m=5","m=10")))

label_y <- with(error_by_dim_df, .75*max(mse)+.25*min(mse))

gg <- ggplot(error_by_dim_df,aes(x=d,y=mse,linetype=factor(which)))+
  geom_line(alpha=.75,size=.333)+
  geom_linerange(aes(ymin=lci,ymax=uci),alpha=.5)+
  scale_linetype_manual(name="",values=c(2,2,1,1,2),guide=FALSE)+
  scale_color_discrete(guide=FALSE)+
  facet_wrap(~m)+
  xlab("dimension")+ylab("mean square error")
print(gg)

