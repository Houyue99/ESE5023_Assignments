#problem1
Print_values <- function(a,b,c){
  if(a>b){
    if(b>c){
      print(c(a,b,c))}
    else if(a>c){
      print(c(a,c,b))}
    else{
      print(c(c,a,b))}
  }
  else if(b>c){
    if(a>c){
      print(c(a,c,b))}
    else{
      print(c(c,a,b))}
  }
  else{
    print(c(c,b,a))
  }
}
# good work
