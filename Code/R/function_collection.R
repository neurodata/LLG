# Read M graphs
readData <- function(dataName) {
  fileName = paste("../../Data/data_", dataName, ".RData", sep="")
  if (file.exists(fileName)) {
    load(fileName)
    return(list(A_all, n, M))
  } else {
    require(igrap)
    subjectsID = readLines("../../Data/subnames.txt")
    A = read_graph(paste("../../Data/", dataName, "/SWU4_", subjectsID[1], 
                         "_1_", dataName, "_sg.graphml", sep =""), format="graphml")
    A = as_adj(A, type="both", sparse=FALSE)
    n = dim(A)[1]
    
    M = 227*2;
    A_all = array(rep(0, n*n*M), dim=c(n, n, M))
    for (sub in 1:227) {
      for (session in 1:2) {
        A = read_graph(paste("../../Data/", dataName, "/SWU4_", subjectsID[sub], 
                             "_", session, "_", dataName, "_sg.graphml",sep=""), format = "graphml")
        A = as_adj(A, type="both", sparse=FALSE)
        A_all[,,(sub-1)*2 + session] = A;
      }
    }
    save(A_all, n, M, file=fileName)
    return(list(A_all, n, M))
  }  
}


