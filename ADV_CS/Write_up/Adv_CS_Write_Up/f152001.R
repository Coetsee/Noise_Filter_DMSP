
##### F152001 #####

rm(list =ls())
gc()

# Open/install required packages
#################################################
lop <- c("raster", "rgdal", "tictoc", "rgeos", "foreach", "doSNOW", "stringr","dplyr",
         "sp", "parallel", "ggplot2", "randomForest", "plm", "glmnet",
         "foreign", "tree", "gbm","fasterize","sf","caret")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)
rasterOptions(progress= "text", timer =T)
memory.limit(size=5000000)
#################################################

#################################################

### Cropping to only South Africa to speed up entire process

f152001avg <- raster("data/downloaded/nightlight/avg/F152001.v4b_web.avg_vis.tif")
f152001pct <- raster("data/downloaded/nightlight/pct/F152001.v4b.pct_lights.tif")

shpward <- st_read("data/c1a23db3-8f2f-40c7-bd3e-ae9fed1eb0322020329-1-ihxzob.tk9kh.shp") %>% filter(OBJECTID < 4278)
shpward <- as_Spatial(shpward)

f152001avgcrop <- crop(f152001avg, shpward) # crop raster layer to only SA
#f121999avgmask <- mask(f121999avgcrop, shpward) # mask the extent to only include data within the shapefile boundaries
writeRaster(f152001avgcrop, paste0("data/downloaded/nightlight/avg/f152001ave.tif"), format="GTiff", overwrite=TRUE)

f152001pctcrop <- crop(f152001pct, shpward) # crop raster layer to only SA
#f121999pctmask <- mask(f121999pctcrop, shpward) # mask the extent to only include data within the shapefile boundaries
writeRaster(f152001pctcrop, paste0("data/downloaded/nightlight/pct/f152001pct.tif"), format="GTiff", overwrite=TRUE)


# lm_freq_5 
freq_light <- raster(paste0("data/downloaded/nightlight/pct/f152001pct.tif"))

# calculate average freq_light for every pixel in 5x5 window
r.mean1 <- focal(freq_light, w=matrix(1/25, ncol=5, nrow=5),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_5_f152001.tif"), format="GTiff", overwrite=TRUE)

# lm_avg_25
avg_light <- raster(paste0("data/downloaded/nightlight/avg/f152001ave.tif"))

r.mean1 <- focal(avg_light, w=matrix(1/625, ncol=25, nrow=25),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_avg_25_f152001.tif"), format="GTiff", overwrite=TRUE)

# lm_freq_99
r.mean1 <- focal(freq_light, w=matrix(1/9801, ncol=99, nrow=99),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_99_f152001.tif"), format="GTiff", overwrite=TRUE)

# lm_avg_199
r.mean1 <- focal(avg_light, w=matrix(1/39601, ncol=199, nrow=199),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_avg_199_f152001.tif"), format="GTiff", overwrite=TRUE)

# lm_freq_49
r.mean1 <- focal(freq_light, w=matrix(1/2401, ncol=49, nrow=49),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_49_f152001.tif"), format="GTiff", overwrite=TRUE)

# Hannes Local Noise: include the extra ones made for the Nigeria case originally

myfun <- function(x) {
  nullcount <- sum(x < 6)
  return(nullcount)
}

avg_light <- raster(paste0("data/downloaded/nightlight/avg/f152001ave.tif"))
r1= avg_light

r.mean1 <- focal(avg_light, w=matrix(1, ncol=199, nrow=199), fun = myfun,pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_noise_199_f152001.tif"), format="GTiff", overwrite=TRUE)

### Big noise - this takes long to run 
r.mean1 <- focal(r1, w=matrix(1, ncol=499, nrow=499), fun = myfun, pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_noise_f152001.tif"), format="GTiff", overwrite=TRUE)

# Hannes Local Detections: include the extra ones made for the Nigeria case originally

myfun <- function(x) {
  nullcount <- sum(x == 0)
  return(nullcount)
}

freq_light <- raster(paste0("data/downloaded/nightlight/pct/F152001pct.tif"))
r1 = freq_light

r.mean1 <- focal(freq_light, w=matrix(1, ncol=149, nrow=149), fun = myfun,pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_detections_149_f152001.tif"), format="GTiff", overwrite=TRUE)

### Big detections - this takes long to run 
r.mean1 <- focal(r1, w=matrix(1, ncol=399, nrow=399), fun = myfun, pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_detections_f152001.tif"), format="GTiff", overwrite=TRUE)
