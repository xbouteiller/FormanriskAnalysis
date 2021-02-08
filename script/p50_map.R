## set Working directory
setwd("~/Documents/research/FORMANRISK/data/data_formanrisk")

## library
library('raster')
library('sp')
library('RgoogleMaps')
library('mapplots')
library('scales')

### Aridity raster
str_name="/home/xavier/Downloads/7504448/global-ai_et0/ai_et0/ai_et0.tif"
imported_raster=raster(str_name)

### Paste Aridity values to data frame
# coords <- data.frame(x=site$X,y=site$Y)
# points <- SpatialPoints(coords, proj4string = imported_raster@crs)
# values <- extract(imported_raster,points)
# values = data.frame(values)
# names(values)= 'AI'
# df1 = cbind.data.frame(coordinates(points),values)
# df = cbind.data.frame(df, df1)
# df$AI = df$AI/10000


### df pinus pinaster
dfpp = read.table('/home/xavier/Documents/research/FORMANRISK/analyse/forman_cavit/output/table/df_mean_PP.csv', header =TRUE, sep = ",")
head(dfpp)

dfpp_y = dfpp[dfpp$Treatment == 'young',]
dfpp_a = dfpp[dfpp$Treatment == 'adult',]



## plot
save_path = "/home/xavier/Documents/research/FORMANRISK/analyse/forman_cavit/output/fig"

tiff(filename = paste0(save_path, '/map6.tiff'), h = 800, w=800, units = "px", pointsize = 12, compression = "zip")

plot(imported_raster[[1]]/10000,xlim=c(-11,6),ylim=c(38,45.5), col = terrain.colors(1000), main = 'Aridity index')

for(i in seq(1,nrow(dfpp_y))){
  add.pie(z=c(-1*dfpp_y$P50_mean[i], -1*dfpp_a$P50_mean[i]),
          x=dfpp_y$x_mean[i], 
          y=dfpp_y$y_mean[i], 
          radius=0.2, 
          col=c(alpha("orange", 0.6), alpha("blue", 0.6)),
          labels=dfpp_y$site[i])
}

# plot(points, add=T,  lwd = 2)
# text(x=df$x, y=df$y , df$site, cex = 0.8 , col=as.integer(df$species_2), pos = 4, offset = 0.2)


dev.off()