calcCorners <- function(x.known, y.known,
                        orientation, declination,
                        size.x, size.y) {
  
  orientation <- (orientation-declination)*pi/180
  
  x <- sin(orientation)*size.x
  y <- cos(orientation)*size.x
  
  Cx=round(x,2)+x.known
  Cy=round(y,2)+y.known
  
  werteC <-data.frame(Cx,Cy)
  
  
  Ax=round(x.known-cos(orientation)*size.y)
  Ay=round(y.known+sin(orientation)*size.y)
  
  werteA <- data.frame(Ax,Ay)
  
  Bx=round(Ax+sin(orientation)*size.x,2)
  By=round(Ay+ cos(orientation)*size.x,2)
  
  
  werteB <-data.frame(Bx, By)
  
  werte <- data.frame(werteA, werteB, werteC, x.known, y.known)
  werte}

calcCorners(481054,5645540,9,1.6,30,48)