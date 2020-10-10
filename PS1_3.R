#Problem3
Pascal_triangle <- function(k){
  line1 <-1
  line2 <-c(1,1)
  
  if (k==1){
    print(line1)
  }
  else if (k==2){
    print(line2)
  }
  
  else{
    lineiq<-line2
    for (i in 3:k){
      linei<-vector(length=i)
      linei[1]<-1
      linei[i]<-1
      m<-i-1
      for (j in 2:m){
        linei[j]<-lineiq[j-1]+lineiq[j]
      }
      lineiq<-linei
    }
    print(linei)
  }
}

Pascal_triangle(100)
Pascal_triangle(200)