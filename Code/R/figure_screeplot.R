rm(list = ls())

setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("E:/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# dataName = "CPAC200"
# dataName = "desikan"
# dataName = "JHU"

dataNameVec = c("JHU", "desikan", "CPAC200")
dataNameDisplayVec = c("JHU", "Desikan", "CPAC200")

source("function_collection.R")
require(ggplot2)
eigenResult <- list()
pp_scree <- list()
for (iData in 1:length(dataNameVec)) {
  dataName = dataNameVec[iData]
  tmpList = read_data(dataName, DA=F, newGraph=F)
  A_all = tmpList[[1]]
  n = tmpList[[2]]
  M = tmpList[[3]]
  rm(tmpList)
  Abar = add(A_all)/M
  AbarDiagAug = diag_aug(Abar)
  eigenResult[[iData]] = eigen(AbarDiagAug)$values
  yMax = max(eigenResult[[iData]])
  yMin = min(eigenResult[[iData]])
  df <- data.frame(eval=eigenResult[[iData]], k=1:n)
  label_y <- with(df, .75*yMax+.25*yMin)
  
  pp_scree[[iData]] <- ggplot(df,aes(x=k,y=eval))+
    geom_line()+
    scale_linetype_manual(name="",values=c("longdash","dotted","dotdash"))+
    xlab("order in algebraic") + ylab("eigenvalue")+
    theme(panel.grid.major = element_line(colour="grey95"),
          panel.grid.minor = element_blank())+
    theme(panel.background = element_rect(fill = 'white', colour = 'grey70'))+
    theme(legend.position="none")+
    ggtitle(dataNameDisplayVec[[iData]])
  
  ggsave(paste0("../../Draft/screeplot_", dataName, ".pdf"),
         pp_scree[[iData]]+theme(text=element_text(size=10,family="Times")),
         # pp_scree[[iData]]+theme(text=element_text(size=10,family="CM Roman")),
         width=2, height=2)
}

