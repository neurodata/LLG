

# Read M graphs
read_data <- function(dataName, DA=T) {
  if (DA) {
    fileName = paste("../../Data/data_", dataName, "_DA.RData", sep="")
  } else {
    fileName = paste("../../Data/data_", dataName, ".RData", sep="")
  }
  if (file.exists(fileName)) {
    load(fileName)
    return(list(A_all, n, M))
  } else {
    require(igraph)
    subjectsID = readLines("../../Data/subnames.txt")
    g = read_graph(paste("../../Data/", dataName, "/SWU4_", subjectsID[1], 
                         "_1_", dataName, "_sg.graphml", sep =""), format="graphml")
    n = vcount(g)
    
    M = 227*2;
    A_all = list()
    for (sub in 1:227) {
      for (session in 1:2) {
        g = read_graph(paste("../../Data/", dataName, "/SWU4_", subjectsID[sub], 
                             "_", session, "_", dataName, "_sg.graphml",sep=""), format = "graphml")
        A = as_adj(g, type="both", sparse=FALSE)
        if (DA) {
          A = diag_aug(A)
        }
        A_all[[(sub-1)*2 + session]] = A;
      }
    }
    
    save(A_all, n, M, file=fileName)
    return(list(A_all, n, M))
  }  
}




# Diagonal Augmentation
diag_aug <- function(A) {
  require(Matrix)
  n = dim(A)[1]
  return(A + Diagonal(n, x=rowSums(A))/(n-1))
}




# Regularize probability matrix
regularize <- function(A) {
  diag(A) = 0
  A[A > 1] = 1
  A[A < 0] = 0
  return(A)
}



# Dimensions bruteforce exploring
dim_brute <- function(M, m, d, A_all) {
  require(rARPACK)
  
  sampleVec = sample.int(M, m)
  A_bar = add(A_all[sampleVec])
  P_bar = (A_sum - A_bar)/(M - m);
  A_bar = A_bar/m;
  
  ASE = eigs_sym(as.matrix(diag_aug(A_bar)), d, which = "LM")
  P_hat =  regularize(ASE$vectors %*% diag(ASE$values) %*% t(ASE$vectors))
  
  return(c(norm(P_bar - A_bar, "F")/n/(n-1), norm(P_bar - P_hat, "F")/n/(n-1)))
}



# Using ZG to choose dimension
dim_ZG <- function(M, m, A_all, d) {
  require(rARPACK)
  
  sampleVec = sample.int(M, m)
  A_bar = add(A_all[sampleVec])
  P_bar = (A_sum - A_bar)/(M - m);
  A_bar = A_bar/m;
  
  ASE = eigs_sym(as.matrix(diag_aug(A_bar)), d, which = "LM")
  P_hat =  regularize(ASE$vectors %*% diag(ASE$values) %*% t(ASE$vectors))
  
  return(c(norm(P_bar - A_bar, "F")/n/(n-1), norm(P_bar - P_hat, "F")/n/(n-1)))
}

