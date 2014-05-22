## newdistance
## umrechnung der distance in 2d; unter Beruecksichtigung der inclination

source("R/deg2rad.R")
source("R/calcCorners.R")

2ddistance<- function (distance, inclination) {
  
  inclination <- deg2rad (inclination)
  2ddist <- round ((cos(inclination) * distance),2)
  
  return (2ddist)}

2ddistance (5.73,18 )
