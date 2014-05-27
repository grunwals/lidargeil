dreieck <- function (alpha, c) {

    
    alpha <- alpha*(pi/180)
    
    

  x <- sin(alpha)*c
  y <- cos(alpha)*c
  
  werte <-data.frame(x=round(x,2),y=round(y,2))
  
  return(werte)
  
}

