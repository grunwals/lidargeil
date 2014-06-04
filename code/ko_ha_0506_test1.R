#hiermit beginnt es
fls <- list.files("C:/Users/Korbinian/Desktop/uni/fernerkundung/data",
                  pattern = "locations.csv", full.names = TRUE)

loc.list <- lapply(fls, function(...) {
  read.csv(..., stringsAsFactors = FALSE, fill = TRUE)
})

loc <- do.call("rbind", loc.list)


spec <- read.csv("C:/Users/Korbinian/Desktop/uni/fernerkundung/data/species_inventory.csv",
                 stringsAsFactors = FALSE, fill = TRUE)

loc.spec <- merge(loc, spec)

save(loc.spec, file = "C:/Users/Korbinian/Desktop/uni/fernerkundung/Rfiles/script/tree_locations_species_all.RData")


#dann kommt das lange, was man noch auseinander nehmen kann


load("C:/Users/Korbinian/Desktop/uni/fernerkundung/data/tree_locations_species_all.RData")

load("C:/Users/Korbinian/Desktop/uni/fernerkundung/data/corner_points.RData")

source("C:/Users/Korbinian/Desktop/uni/fernerkundung/Rfiles/function/kood.R")

plot(corner.points)

coords.ele <- data.frame(origin = corner.points@data$corner,
                         origin.x = corner.points@coords[, 1],
                         origin.y = corner.points@coords[, 2],
                         origin.z = corner.points@data$ele)

loc.spec.xyz <- merge(loc.spec, coords.ele, all.x = TRUE)

names(loc.spec.xyz) <-c("origin", "treeID","distance",    "bearing",
                        "inclination",    "species",    "origin.x",    "origin.y",
                        "origin.z")



# alle eintraege als zahlen verstehen

loc.spec.xyz$origin.x <- as.numeric(loc.spec.xyz$origin.x)
loc.spec.xyz$origin.y <- as.numeric(loc.spec.xyz$origin.y)
loc.spec.xyz$origin.z <- as.numeric(loc.spec.xyz$origin.z)
loc.spec.xyz$distance <- as.numeric(loc.spec.xyz$distance)
loc.spec.xyz$bearing <- as.numeric(loc.spec.xyz$bearing)
loc.spec.xyz$inclination <- as.numeric(loc.spec.xyz$inclination)




#KOORDIANTEN BERECHNEN

n <- c(1:129)
neue_kood <-data.frame(kood(loc.spec.xyz$origin.x[n], loc.spec.xyz$origin.y[n],
                            loc.spec.xyz$origin.z[n], loc.spec.xyz$distance[n],
                            loc.spec.xyz$bearing[n],
                            loc.spec.xyz$inclination[n]))



#NEUEN DATENSATZ KREIEREN


data.all <- data.frame( loc.spec.xyz, neue_kood)

# um die nas kuemmern

nummer <- c(1:129)
na <- is.na(loc.spec.xyz$origin.x)
spaltennummer_mit_na <- data.frame(cbind(na, nummer))

spaltennummer_mit_na <- spaltennummer_mit_na[]

names(spaltennummer_mit_na) <- c("na", "nummer")

na_angabe <- data.frame(spaltennummer_mit_na[spaltennummer_mit_na$na == 1, ])
names(na_angabe) <- c("na", "zeile")


#die folgenden schritte sind mehrmals hintereinander auszuführen: solange bis data.all vollst.. dh. keine na mmehr
# Zeilen mit NA aendern
i <- (na_angabe$zeile[])

for (i in i){data.all$origin.x[i] <- data.all$x[data.all$treeID == data.all$origin[i]]}

i <- c(na_angabe$zeile[])
for (i in i){data.all$origin.y[i] <- data.all$y[data.all$treeID == data.all$origin[i]]}
i <- (na_angabe$zeile[])

for (i in i){data.all$origin.z[i] <- data.all$z[data.all$treeID == data.all$origin[i]]}
i <- (na_angabe$zeile[])


# wieder koordianten berechen

n <- c(1:129)
neue_kood <-data.frame(kood(data.all$origin.x[n], data.all$origin.y[n],
                            data.all$origin.z[n], data.all$distance[n],
                            data.all$bearing[n],
                            data.all$inclination[n]))
data.all$x <- neue_kood$x
data.all$y <- neue_kood$y
data.all$z <- neue_kood$z


save(data.all, file= "C:/Users/Korbinian/Desktop/uni/fernerkundung/data/data_all.csv")


#und hier noch die funktion, auf dir im 2ten teil auch zurückgeriffen wird

#Koordinatenberechnung

kood <- function (origin.x, origin.y, origin.z, distance, bearing,
                  inclination){
  alpha <- (bearing - 1.6)*pi/180
  distance <- cos(inclination*pi/180)*distance
  x <- c(cos(alpha)*distance+origin.x)
  y<- c(sin(alpha)*distance+origin.y)
  z <- c(sin(inclination*pi/180)*distance+origin.z)
  kood <- data.frame(x,y,z)
  names(kood) <- c("x","y","z")
  kood}

kood(1:129, 5645538.43101407, 246.676422, 26.30, 9, 27.0)