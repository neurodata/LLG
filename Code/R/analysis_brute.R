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

# 2nd and 3rd elbow
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


# USVT
if (dataName == "JHU") {
  dUSVT05Mean = c(9.38, 12.66, 19.72, 27.24)
  dUSVT05L = c(9.111663, 12.39393, 19.53089, 27.07727)
  dUSVT05U = c(9.648337, 12.92607, 19.90911, 27.40273)
  
  dUSVT07Mean = c(4.61, 6.13, 11.61, 18.18)
  dUSVT07L = c(4.465526, 5.866221, 11.39987, 17.95997)
  dUSVT07U = c(4.754474, 6.393779, 11.82013, 18.40003)
  
  dUSVT08Mean = c(3.07, 4.82, 8.54, 14.8)
  dUSVT08L = c(2.911783, 4.642974, 8.293301, 14.59528)
  dUSVT08U = c(3.228217, 4.997026, 8.786699, 15.00472)
  
  dUSVT1Mean = c(1.84, 2.54, 5.1, 9.45)
  dUSVT1L = c(1.744629, 2.423727, 4.941184, 9.264424)
  dUSVT1U = c(1.935371, 2.656273, 5.258816, 9.635576)
} else if (dataName == "desikan") {
  dUSVT05Mean = c(11.54, 14.94, 22.19, 31.81)
  dUSVT05L = c(11.3383, 14.70556, 21.93362, 31.61963)
  dUSVT05U = c(11.7417, 15.17444, 22.44638, 32.00037)
  
  dUSVT07Mean = c(6.36, 8.02, 12.68, 19.55)
  dUSVT07L = c(6.198711, 7.843853, 12.47551, 19.35032)
  dUSVT07U = c(6.521289, 8.196147, 12.88449, 19.74968)
  
  dUSVT08Mean = c(4.79, 6.28, 9.7, 15.89)
  dUSVT08L = c(4.626857, 6.062278, 9.496235, 15.69923)
  dUSVT08U = c(4.953143, 6.497722, 9.903765, 16.08077)
  
  dUSVT1Mean = c(2.86, 4.44, 6.64, 9.89)
  dUSVT1L = c(2.738505, 4.289153, 6.473969, 9.723095)
  dUSVT1U = c(2.981495, 4.590847, 6.806031, 10.056905)
} else if (dataName == "CPAC200") {
  dUSVT05Mean = c(18.6, 23.14, 34.82, 54.69)
  dUSVT05L = c(17.97707, 22.57316, 34.34121, 54.30129)
  dUSVT05U = c(19.22293, 23.70684, 35.29879, 55.07871)
  
  dUSVT07Mean = c(10.08, 12.8, 18.86, 27.77)
  dUSVT07L = c(9.732408, 12.47156, 18.51539, 27.44055)
  dUSVT07U = c(10.427592, 13.12844, 19.20461, 28.09945)
  
  dUSVT08Mean = c(7.81, 10.08, 15.22, 22.02)
  dUSVT08L = c(7.546162, 9.842501, 14.9218, 21.73867)
  dUSVT08U = c(8.073838, 10.317499, 15.5182, 22.30133)
  
  dUSVT1Mean = c(4.92, 7.01, 10.88, 15.75)
  dUSVT1L = c(4.715892, 6.861291, 10.67857, 15.53534)
  dUSVT1U = c(5.124108, 7.158709, 11.08143, 15.96466)
}


library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)

mVec = c(1,2,5,10)

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
  
}

errorPhatUSVT07 = rep(0, length(mVec))
errorPhatZG3 = rep(0, length(mVec))
for (iM in 1:length(mVec)) {
  x = dUSVT07Mean[iM]
  x1 = floor(x)
  y1 = errorPhatEIGMean[iM, x1]
  x2 = ceiling(x)
  y2 = errorPhatEIGMean[iM, x2]
  errorPhatUSVT07[iM] = (y2-y1)/(x2-x1)*(x-x1)+y1
  
  x = dZG3Mean[iM]
  x1 = floor(x)
  y1 = errorPhatEIGMean[iM, x1]
  x2 = ceiling(x)
  y2 = errorPhatEIGMean[iM, x2]
  errorPhatZG3[iM] = (y2-y1)/(x2-x1)*(x-x1)+y1
}

error_by_dim_df <- rbind(
  data.frame(mse=errorAbarMean,lci=errorAbarLower,uci=errorAbarUpper,
             which="Abar",m=mVec,d=n),
  data.frame(mse=errorAbarMean,lci=errorAbarLower,uci=errorAbarUpper,
             which="Abar",m=mVec,d=1),
  data.frame(mse=c(errorPhatEIGMean),lci=c(errorPhatEIGLower),uci=c(errorPhatEIGUpper),
             which="Phat",m=rep(mVec,n),d=rep(1:n,each=4))) %>%
  mutate(m=factor(paste0("M=",m),c("M=1","M=2","M=5","M=10")))

dim_selection_df <- rbind(
  data.frame(mse=errorPhatZG3,lci=errorPhatZG3,uci=errorPhatZG3,
             which="ZG 3rd",m=mVec,d=dZG3Mean),
  data.frame(mse=errorPhatUSVT07,lci=errorPhatUSVT07,uci=errorPhatUSVT07,
             which="USVT c=0.7",m=mVec,d=dUSVT07Mean)) %>%
  mutate(m=factor(paste0("M=",m),c("M=1","M=2","M=5","M=10")))


label_y <- with(error_by_dim_df, .75*max(mse)+.25*min(mse))

gg <- ggplot(error_by_dim_df,aes(x=d,y=mse,linetype=factor(which),shape=factor(which)))+
  facet_wrap(~m)+
#   geom_point(data=subset(dim_selection_df,which=="ZG 3rd"),size=2,colour="red")+
#   geom_point(data=subset(dim_selection_df,which=="USVT c=0.7"),size=2,colour="blue")+
  geom_point(data=dim_selection_df,size=3)+
  scale_linetype_manual(name="",values=c(1,2,0,0))+
  scale_shape_manual(name="",values=c(-1,-1,15,17))+
#   geom_point(dim_selection_df,aes(shape=which))+
  geom_line(alpha=1,size=.5)+
  geom_linerange(aes(ymin=lci,ymax=uci),alpha=.5,size=1)+
#   geom_vline(data=dim_selection_df,
#              aes(xintercept=value,color=which,linetype=variable))+
#   scale_linetype_manual(name="",values=c(1,2,3,4))+
#   geom_text(data=dim_selection_df %>% filter(variable=="mean"),
#             aes(x=value+n/30,y=label_y,linetype=variable,label=which,color=which),angle=90)+
#   scale_color_discrete(guide=FALSE)+
  xlab("Dimension")+ylab("Mean Squared Error")+
  theme(strip.text.x = element_text(size=20,face="bold"))+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20,face="bold"))+
  theme(panel.grid.major = element_line(colour="grey95"),
        panel.grid.minor = element_blank())+
  theme(panel.background = element_rect(fill = 'white', colour = 'grey70'))+
  theme(legend.text=element_text(size=20,face="bold"))+
  theme(legend.position="bottom")+
  ggtitle(paste0(dataName, ", N=", n, ", ", M, " graphs"))+
  theme(plot.title=element_text(lineheight=.8,size=20,face="bold"))
print(gg)






## --- Previous Plots --- ##
# 
# yMin = Inf
# yMax = 0
# for (m in mVec) {
#   # EIG
#   fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_eig.RData", sep="")
#   load(fileName)
#   yMin = min(yMin, rowMeans(error_P_hat^2), mean(error_A_bar^2))
#   yMax = max(yMax, rowMeans(error_P_hat^2), mean(error_A_bar^2))
# }
# yMax = yMax*1.1
# yMin = yMin*0.9
# 
# library(ggplot2)
# library(grid)
# source("function_collection.R")
# 
# pp = list()
# 
# for (iM in 1:length(mVec)) {
#   m = mVec[iM]
#   
#   # Eigen-decomposition
#   fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_eig.RData", sep="")
#   load(fileName)
#   
#   error_A_bar = error_A_bar^2*n*(n-1)
#   error_P_hat = error_P_hat^2*n*(n-1)
#   
#   errorAbarMean = rep(mean(error_A_bar))
#   errorAbarLower = errorAbarMean - sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
#   errorAbarUpper = errorAbarMean + sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
#   errorPhatEIGMean = rowMeans(error_P_hat)
#   errorPhatEIGLower = errorPhatEIGMean - 
#     sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
#   errorPhatEIGUpper = errorPhatEIGMean + 
#     sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
#   
#   df = data.frame(d=c(rep(dVec,2), NaN, NaN), 
#                   errorMean=c(rep(mean(error_A_bar), length(dVec)),
#                               errorPhatEIGMean, NaN, NaN),
#                   errorLower=c(rep(errorAbarLower, length(dVec)),
#                                errorPhatEIGLower, NaN, NaN),
#                   errorUpper=c(rep(errorAbarUpper, length(dVec)),
#                                errorPhatEIGUpper, NaN, NaN),
#                   flag=c(rep("Abar",length(dVec)),
#                          rep("Phat",length(dVec)),
#                          "USVT c=0.7",
#                          "ZG 3rd elbow"))
#   
#   p = ggplot(data=df, aes(x=d, y=errorMean, color=flag)) + geom_point() + geom_line() +
#     geom_ribbon(aes(x=d, ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
#   
#   p = p + geom_vline(xintercept=dUSVT07Mean[iM], linetype="solid", color="green")
#   p = p + geom_vline(xintercept=dUSVT07L[iM], linetype="dashed", color="green")
#   p = p + geom_vline(xintercept=dUSVT07U[iM], linetype="dashed", color="green")
#   
#   p = p + geom_vline(xintercept=dZG3Mean[iM], linetype="solid", color="purple")
#   p = p + geom_vline(xintercept=dZG3L[iM], linetype="dashed", color="purple")
#   p = p + geom_vline(xintercept=dZG3U[iM], linetype="dashed", color="purple")
#   
#   p = p + theme(legend.text=element_text(size = 20, face="bold"))
#   p = p + theme(legend.title=element_blank())
#   p = p + theme(plot.title = element_text(size = 16, face="bold"))
#   p = p + scale_x_continuous(name="Dimension") + scale_y_continuous(name="MSE")
#   p = p + theme(axis.text=element_text(size=14,face="bold"),
#                 axis.title=element_text(size=14,face="bold"))
#   p = p + ggtitle(paste("M = ", m, sep=""))
#   p = p + expand_limits(y=c(yMin, yMax))
#   p = p + scale_color_manual(values=c("red", "blue", "green", "purple"))
#   pp[[iM]] = p
# }
# 
# # layout <- matrix(1:4, nrow = 2, byrow = TRUE)
# # multiplot(plotlist = pp, layout = layout)
# 
# # grid.newpage()
# # pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(1, 8, 8), "null"))))
# # grid.text(paste(dataName, ", n=", n, ", M=", M, sep=""),
# #           vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
# #           gp=gpar(fontsize=30))
# # for (iRow in 2:3) {
# #   for (iCol in 1:2) {
# #     print(pp[[(iRow-2)*2+iCol]], vp = viewport(layout.pos.row = iRow, layout.pos.col = iCol))
# #   }
# # }
# 
# 
# library(gridExtra)
# 
# grid_arrange_shared_legend <- function(t, ...) {
#   plots <- list(...)
#   g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
#   legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
#   lheight <- sum(legend$height)
#   theight <- lheight*0.8
#   grid.arrange(
#     main=textGrob(t, gp=gpar(cex=1.5)),
#     do.call(arrangeGrob, lapply(plots, function(x)
#       x + theme(legend.position="none"))),
#     legend,
#     ncol = 1,
#     heights = unit.c(theight, unit(1, "npc") - lheight - theight, lheight))
# }
# 
# grid_arrange_shared_legend(paste(dataName, ", N=", n, ", ", M, " graphs", sep=""), 
#                            pp[[1]], pp[[2]], pp[[3]], pp[[4]])
# 
