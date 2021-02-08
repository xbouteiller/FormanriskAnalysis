# Script 1
# Computing Environmental variable

cat(
"
This script is for merging environmental variable to the population information table,
use world clim data, a varenv.csv table is saved and some map figures too
"
)
## set Working directory
setwd("~/Documents/research/FORMANRISK/data/data_formanrisk")

## library
library('raster')
library('sp')
library('RgoogleMaps')

## creating var env data frame

### importing worlclim data
r = getData("worldclim", var="bio", res=0.5, lat=44.73696, lon=-0.77265)
col_names = names(r)
r2 = getData("worldclim", var="bio", res=0.5, lat=42.652968, lon=2.906672)
r3 = merge(r, r2)

### importing pop info data base
site = read.table("Formanrisk sampling - site information.csv",  h= T, sep = ',')

### Merging worlclim and pop info based on X Y coordinates
coords <- data.frame(x=site$X,y=site$Y)
points <- SpatialPoints(coords, proj4string = r3@crs)
values <- extract(r3,points)
values = data.frame(values)
names(values)= col_names
df = cbind.data.frame(coordinates(points),values)
df = cbind.data.frame(df, site)

### Assertion
if(any(is.na(df[,"bio1_15"]))){print('Na are still remaining')}else{print('No Na')}


### Slighly shifting Branas latitude to avoid empy value
inc = 0.00001
while(is.na(df[site$site == "Brañas Verdes", "bio1_15"])){
  site[site$site == "Brañas Verdes"  ,"Y"] = 43.70114 - inc
  ### Merging worlclim and pop info based on X Y coordinates
  coords <- data.frame(x=site$X,y=site$Y)
  points <- SpatialPoints(coords, proj4string = r3@crs)
  values <- extract(r3,points)
  values = data.frame(values)
  names(values)= col_names
  df = cbind.data.frame(coordinates(points),values)
  df = cbind.data.frame(df, site)
  df[,c("site","x", "y", "X", "Y", "bio1_15")]
  inc = inc * 2
}

### Assertion
if(any(is.na(df[,"bio1_15"]))){print('NA are still remaining')}else{print('No NA')}

### Aridity index
str_name="/home/xavier/Downloads/7504448/global-ai_et0/ai_et0/ai_et0.tif"
imported_raster=raster(str_name)

### Paste Aridity values to data frame
coords <- data.frame(x=site$X,y=site$Y)
points <- SpatialPoints(coords, proj4string = imported_raster@crs)
values <- extract(imported_raster,points)
values = data.frame(values)
names(values)= 'AI'
df1 = cbind.data.frame(coordinates(points),values)
df = cbind.data.frame(df, df1)
df$AI = df$AI/10000


## Drop some columns
# library('dplyr')
# drop.cols = c('x','y', 'email')
# df %>% select(-one_of(drop.cols))


## Saving table
write.table(df, 'varenv.csv', sep=";", row.names = FALSE)

## plot
save_path = "/home/xavier/Documents/research/FORMANRISK/analyse/forman_cavit/output/fig"

tiff(filename = paste0(save_path, '/map1.tiff'), h = 600, w=600, units = "px", pointsize = 12, compression = "zip")
plot(r3[[5]],xlim=c(-11,6),ylim=c(38,45.5), col = terrain.colors(1000))
# plot(r3[[1]],xlim=c(-7.9,-7.7),ylim=c(43.6,43.8))
plot(points, add=T, col = 'black', lwd = 2)
dev.off()


tiff(filename = paste0(save_path, '/map2.tiff'), h = 600, w=600, units = "px", pointsize = 12, compression = "zip")
plot(r3[[5]],xlim=c(-11,6),ylim=c(38,45.5), col = terrain.colors(1000))
# plot(r3[[1]],xlim=c(-7.9,-7.7),ylim=c(43.6,43.8))
plot(points, add=T,  lwd = 2, col=as.integer(df$species_2))
text(x=df$x, y=df$y , df$site, cex = 0.8 , col=as.integer(df$species_2), pos = 4, offset = 0.2)
dev.off()


tiff(filename = paste0(save_path, '/map4.tiff'), h = 600, w=600, units = "px", pointsize = 12, compression = "zip")
plot(imported_raster[[1]],xlim=c(-11,6),ylim=c(38,45.5), col = terrain.colors(1000))
# plot(r3[[1]],xlim=c(-7.9,-7.7),ylim=c(43.6,43.8))
plot(points, add=T,  lwd = 2, col=as.integer(df$species_2))
text(x=df$x, y=df$y , df$site, cex = 0.8 , col=as.integer(df$species_2), pos = 4, offset = 0.2)
dev.off()


## Google fig

lat <- c(min(site$Y)-0.5,max(site$Y)+0.5) #define our map's ylim
lon <- c(min(site$X)-0.5,max(site$X)+0.5) #define our map's xlim
center = c(mean(lat), mean(lon))  #tell what point to center on
zoom <- 5  #zoom: 1 = furthest out (entire globe), larger numbers = closer in
site$size <- "tiny"  #create a column indicating size of marker
site$col <- "black"   #create a column indicating color of marker
site$char <- ""   #normal Google Maps pinpoints will be drawn
mymarkers <- cbind.data.frame(site$Y, site$X, site$size, 
                              site$col, site$char)   #create the data frame by binding my data columns of GPS coordinates with the newly created columns
names(mymarkers) <- c("lat", "lon", "size", "col", "char")  #assign column headings

bb = qbbox(lat=mymarkers$lat, lon=mymarkers$lon)
terrain_close <- GetMap.bbox(bb$lonR, bb$latR,  destfile= "terrclose.png",
                             markers= mymarkers,  maptype = "satellite")
tiff(filename = paste0(save_path, '/map3.tiff'), h = 600, w=600, units = "px", pointsize = 12, compression = "zip")     
          
tmp <- PlotOnStaticMap(terrain_close, lat = mymarkers$lat, lon = mymarkers$lon, cex=3,pch=20,col=c('red'));
dev.off()



