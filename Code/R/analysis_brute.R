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




yMin = Inf
yMax = 0
for (m in mVec) {
  # EIG
  fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_eig.RData", sep="")
  load(fileName)
  yMin = min(yMin, rowMeans(error_P_hat^2), mean(error_A_bar^2))
  yMax = max(yMax, rowMeans(error_P_hat^2), mean(error_A_bar^2))
}
yMax = yMax*1.1
yMin = yMin*0.9

library(ggplot2)
library(grid)
source("function_collection.R")

pp = list()

for (iM in 1:length(mVec)) {
  m = mVec[iM]
  
  # Eigen-decomposition
  fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_eig.RData", sep="")
  load(fileName)
  
  error_A_bar = error_A_bar^2
  error_P_hat = error_P_hat^2
  
  errorAbarMean = rep(mean(error_A_bar))
  errorAbarLower = errorAbarMean - sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorAbarUpper = errorAbarMean + sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorPhatEIGMean = rowMeans(error_P_hat)
  errorPhatEIGLower = errorPhatEIGMean - 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  errorPhatEIGUpper = errorPhatEIGMean + 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  
  df = data.frame(d=c(rep(dVec,2), NaN, NaN), 
                  errorMean=c(rep(mean(error_A_bar), length(dVec)),
                              errorPhatEIGMean, NaN, NaN),
                  errorLower=c(rep(errorAbarLower, length(dVec)),
                               errorPhatEIGLower, NaN, NaN),
                  errorUpper=c(rep(errorAbarUpper, length(dVec)),
                               errorPhatEIGUpper, NaN, NaN),
                  flag=c(rep("Abar",length(dVec)),
                         rep("Phat",length(dVec)),
                         "ZG 2nd elbow",
                         "ZG 3rd elbow"))
  
  p = ggplot(data=df, aes(x=d, y=errorMean, color=flag)) + geom_point() + geom_line() +
    geom_ribbon(aes(x=d, ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
  
  p = p + geom_vline(xintercept=dZG2Mean[iM], linetype="solid", color="green")
  p = p + geom_vline(xintercept=dZG2L[iM], linetype="dashed", color="green")
  p = p + geom_vline(xintercept=dZG2U[iM], linetype="dashed", color="green")
  
  p = p + geom_vline(xintercept=dZG3Mean[iM], linetype="solid", color="purple")
  p = p + geom_vline(xintercept=dZG3L[iM], linetype="dashed", color="purple")
  p = p + geom_vline(xintercept=dZG3U[iM], linetype="dashed", color="purple")
  
  p = p + theme(legend.text=element_text(size = 20, face="bold"))
  p = p + theme(legend.title=element_blank())
  p = p + theme(plot.title = element_text(size = 16, face="bold"))
  p = p + scale_x_continuous(name="Dimension") + scale_y_continuous(name="MSE")
  p = p + theme(axis.text=element_text(size=14,face="bold"),
                axis.title=element_text(size=14,face="bold"))
  p = p + ggtitle(paste("M = ", m, sep=""))
  p = p + expand_limits(y=c(yMin, yMax))
  p = p + scale_color_manual(values=c("red", "blue", "green", "purple"))
  pp[[iM]] = p
}

# layout <- matrix(1:4, nrow = 2, byrow = TRUE)
# multiplot(plotlist = pp, layout = layout)

# grid.newpage()
# pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(1, 8, 8), "null"))))
# grid.text(paste(dataName, ", n=", n, ", M=", M, sep=""),
#           vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
#           gp=gpar(fontsize=30))
# for (iRow in 2:3) {
#   for (iCol in 1:2) {
#     print(pp[[(iRow-2)*2+iCol]], vp = viewport(layout.pos.row = iRow, layout.pos.col = iCol))
#   }
# }


library(gridExtra)

grid_arrange_shared_legend <- function(t, ...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  theight <- lheight*0.8
  grid.arrange(
    main=textGrob(t, gp=gpar(cex=1.5)),
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(theight, unit(1, "npc") - lheight - theight, lheight))
}

grid_arrange_shared_legend(paste(dataName, ", N=", n, ", ", M, " graphs", sep=""), 
                           pp[[1]], pp[[2]], pp[[3]], pp[[4]])


