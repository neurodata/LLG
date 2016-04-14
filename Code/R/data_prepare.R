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

mVec = c(1, 2, 5, 10)

# 2nd elbow
if (dataName == "JHU") {
  dZG2Mean = c(7.06, 6.39, 6.37, 6.37)
  dZG2L = c(6.724751, 6.112961, 6.206003, 6.240362)
  dZG2U = c(7.395249, 6.667039, 6.533997, 6.499638)
  
  dZG3Mean = c(15.81, 14.57, 13.59, 12.95)
  dZG3L = c(15.41929, 14.24008, 13.25443, 12.6837)
  dZG3U = c(16.20071, 14.89992, 13.92557, 13.2163)
} else if (dataName == "desikan") {
  dZG2Mean = c(4.87, 4.4, 3.98, 4)
  dZG2L = c(4.459004, 4.120028, 3.887689, 4)
  dZG2U = c(5.280996, 4.679972, 4.072311, 4)
  
  dZG3Mean = c(15.95, 14.05, 11.09, 9.87)
  dZG3L = c(15.34967, 13.55842, 10.70389, 9.629296)
  dZG3U = c(16.55033, 14.54158, 11.47611, 10.110704)
} else if (dataName == "CPAC200") {
  dZG2Mean = c(25.72, 22.23, 19.02, 18.7)
  dZG2L = c(24.09797, 21.33469, 18.50483, 18.41658)
  dZG2U = c(27.34203, 23.12531, 19.53517, 18.98342)
  
  dZG3Mean = c(60.91, 54.55, 47.64, 42.54)
  dZG3L = c(59.17358, 53.36795, 46.85278, 41.98464)
  dZG3U = c(62.64642, 55.73205, 48.42722, 43.09536)
}



for (iM in 1:length(mVec)) {
  m = mVec[iM]
  
  # Eigen-decomposition
  fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_eig.RData", sep="")
  load(fileName)
  
  if (iM == 1) {
    errorAbarMean = array(0, dim=c(length(mVec), 1))
    errorAbarLower = array(0, dim=c(length(mVec), 1))
    errorAbarUpper = array(0, dim=c(length(mVec), 1))
    errorPhatEIGMean = array(0, dim=c(length(mVec), n))
    errorPhatEIGLower = array(0, dim=c(length(mVec), n))
    errorPhatEIGUpper = array(0, dim=c(length(mVec), n))
  }
  
  error_A_bar = error_A_bar^2
  error_P_hat = error_P_hat^2
  
  errorAbarMean[iM] = rep(mean(error_A_bar))
  errorAbarLower[iM] = errorAbarMean[iM] - sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorAbarUpper[iM] = errorAbarMean[iM] + sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorPhatEIGMean[iM,] = rowMeans(error_P_hat)
  errorPhatEIGLower[iM,] = errorPhatEIGMean[iM,] - 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  errorPhatEIGUpper[iM,] = errorPhatEIGMean[iM,] + 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  
}


save(mVec, dZG2Mean, dZG2L, dZG2U, dZG3Mean, dZG3L, dZG3U,
     errorAbarMean, errorAbarLower, errorAbarUpper,
     errorPhatEIGMean, errorPhatEIGLower, errorPhatEIGUpper,
     n, file=paste("data_", dataName, ".RData", sep=""))