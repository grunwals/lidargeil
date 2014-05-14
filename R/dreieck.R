dreieck <- function (alpha, c) {
  winkelinrad <-function(alpha){
    
    alpha <- alpha*pi/180
    
    
  }
  a <- sin(alpha)*c
  b <- cos(alpha)*c
  
  werte <-data.frame(x=round(a,2),y=round(b,2))
  
  return(werte)
  
}

