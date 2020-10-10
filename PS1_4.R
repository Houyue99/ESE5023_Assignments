#Problem4
Least_moves <- function(x){
  i <- 0
  while(x!=1){
    y <- x/2
    z <- as.integer(y)
    k <- y-z
    if(k==0){
      x <- x/2
      i=i+1
    }
    else{
      x <- x-1
      i=i+1
    }
  }
  print(i)
}