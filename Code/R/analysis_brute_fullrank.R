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
# mVec = 1

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)

for (iM in 1:length(mVec)) {
  m = mVec[iM]
  
  # Eigen-decomposition
  fileName = paste("../../Result/result_", dataName, "_brute_fullrank0_",
                   "m_", m, "_eig.RData", sep="")
  load(fileName)
  
  if (iM == 1) {
    errorAbarMean = array(0, dim=c(length(mVec), 1))
    errorAbarLower = array(0, dim=c(length(mVec), 1))
    errorAbarUpper = array(0, dim=c(length(mVec), 1))
    errorPhatEIGMean = array(0, dim=c(length(mVec), n))
    errorPhatEIGLower = array(0, dim=c(length(mVec), n))
    errorPhatEIGUpper = array(0, dim=c(length(mVec), n))
  }
  
  error_A_bar = error_A_bar^2*n*(n-1)
  error_P_hat = error_P_hat^2*n*(n-1)
  
  errorAbarMean[iM] = rep(mean(error_A_bar))
  errorAbarLower[iM] = errorAbarMean[iM] - sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorAbarUpper[iM] = errorAbarMean[iM] + sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorPhatEIGMean[iM,] = rowMeans(error_P_hat)
  errorPhatEIGLower[iM,] = errorPhatEIGMean[iM,] - 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  errorPhatEIGUpper[iM,] = errorPhatEIGMean[iM,] + 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  
  errorPhatEIG0Mean = errorPhatEIGMean
  errorPhatEIG0Lower = errorPhatEIGLower
  errorPhatEIG0Upper = errorPhatEIGUpper

  # Eigen-decomposition
  fileName = paste("../../Result/result_", dataName, "_brute_fullrank1_",
                   "m_", m, "_eig.RData", sep="")
  load(fileName)
  
  error_A_bar = error_A_bar^2*n*(n-1)
  error_P_hat = error_P_hat^2*n*(n-1)
  
  errorAbarMean[iM] = rep(mean(error_A_bar))
  errorAbarLower[iM] = errorAbarMean[iM] - sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorAbarUpper[iM] = errorAbarMean[iM] + sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorPhatEIGMean[iM,] = rowMeans(error_P_hat)
  errorPhatEIGLower[iM,] = errorPhatEIGMean[iM,] - 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  errorPhatEIGUpper[iM,] = errorPhatEIGMean[iM,] + 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  
  errorPhatEIG1Mean = errorPhatEIGMean
  errorPhatEIG1Lower = errorPhatEIGLower
  errorPhatEIG1Upper = errorPhatEIGUpper
  
}

error_by_dim_df <- rbind(
  data.frame(mse=errorAbarMean,lci=errorAbarLower,uci=errorAbarUpper,
             which="Abar",m=mVec,d=n),
  data.frame(mse=errorAbarMean,lci=errorAbarLower,uci=errorAbarUpper,
             which="Abar",m=mVec,d=1),
  data.frame(mse=c(errorPhatEIG0Mean),lci=c(errorPhatEIG0Lower),uci=c(errorPhatEIG0Upper),
             which="Phat0",m=rep(mVec,n),d=rep(1:n,each=length(mVec))),
  data.frame(mse=c(errorPhatEIG1Mean),lci=c(errorPhatEIG1Lower),uci=c(errorPhatEIG1Upper),
             which="Phat1",m=rep(mVec,n),d=rep(1:n,each=length(mVec)))) %>%
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

