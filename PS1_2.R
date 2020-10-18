#problem2.1
M1 <- matrix(sample(0:50,50,replace = T),nrow=5,ncol=10)
M2 <- matrix(sample(0:50,50,replace = T),nrow=10,ncol=5)

#problem2.2
Matrix_multip <-function(M1,M2){
  M3 <- matrix(0,nrow=nrow(M1),ncol=ncol(M2))
  for (i in 1:nrow(M1)){
    for(j in 1:ncol(M2)){
      for(k in 1:ncol(M1)){
        M3[i,j]<-M3[i,j]+M1[i,k]*M2[k,j]
      }
    }
  }
  print(M3)
}
# good work
