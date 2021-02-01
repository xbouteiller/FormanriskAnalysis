library('raster')
library('sp')

setwd("~/Documents/research/FORMANRISK/data/plotetdata")

r <- getData("worldclim", var="bio", res=0.5, lat=44.73696, lon=-0.77265)
site = read.table("Formanrisk sampling - site information.csv",  h= T, sep = ',')
str(site)
coords <- data.frame(x=site$X,y=site$Y)
points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)
df <- cbind.data.frame(coordinates(points),values)
df = cbind.data.frame(df, site)

plot(r[[1]])
plot(points,add=T)

r2 <- getData("worldclim", var="bio", res=0.5, lat=42.652968, lon=2.906672)
coords <- data.frame(x=site$X,y=site$Y)

points <- SpatialPoints(coords, proj4string =  r2@crs)
values <- extract(r2,points)
df2 <- cbind.data.frame(coordinates(points),values)
df2 = cbind.data.frame(df2, site)
names(df2) = names(df)

df3 = rbind(df,df2)
df3 = df3[!is.na(df3[,"bio1_15"]),]


write.table(df3, 'varenv.csv', sep=";", row.names = FALSE)
# ?write.table

plot(r[[1]])
plot(points,add=T)
