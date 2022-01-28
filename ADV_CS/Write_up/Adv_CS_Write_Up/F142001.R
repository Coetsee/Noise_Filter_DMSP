
##### F142001 #####

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

f142001avg <- raster("data/downloaded/nightlight/avg/F142001.v4b_web.avg_vis.tif")
f142001pct <- raster("data/downloaded/nightlight/pct/F142001.v4b.pct_lights.tif")

shpward <- st_read("data/c1a23db3-8f2f-40c7-bd3e-ae9fed1eb0322020329-1-ihxzob.tk9kh.shp") %>% filter(OBJECTID < 4278)
shpward <- as_Spatial(shpward)

f142001avgcrop <- crop(f142001avg, shpward) # crop raster layer to only SA
#f121999avgmask <- mask(f121999avgcrop, shpward) # mask the extent to only include data within the shapefile boundaries
writeRaster(f142001avgcrop, paste0("data/downloaded/nightlight/avg/f142001ave.tif"), format="GTiff", overwrite=TRUE)


f142001pctcrop <- crop(f142001pct, shpward) # crop raster layer to only SA
#f121999pctmask <- mask(f121999pctcrop, shpward) # mask the extent to only include data within the shapefile boundaries
writeRaster(f142001pctcrop, paste0("data/downloaded/nightlight/pct/f142001pct.tif"), format="GTiff", overwrite=TRUE)


# lm_freq_5 
freq_light <- raster(paste0("data/downloaded/nightlight/pct/f142001pct.tif"))

# calculate average freq_light for every pixel in 5x5 window
r.mean1 <- focal(freq_light, w=matrix(1/25, ncol=5, nrow=5),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_5_f142001.tif"), format="GTiff", overwrite=TRUE)

# lm_avg_25
avg_light <- raster(paste0("data/downloaded/nightlight/avg/f142001ave.tif"))

r.mean1 <- focal(avg_light, w=matrix(1/625, ncol=25, nrow=25),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_avg_25_f142001.tif"), format="GTiff", overwrite=TRUE)

# lm_freq_99
r.mean1 <- focal(freq_light, w=matrix(1/9801, ncol=99, nrow=99),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_99_f142001.tif"), format="GTiff", overwrite=TRUE)

# lm_avg_199
r.mean1 <- focal(avg_light, w=matrix(1/39601, ncol=199, nrow=199),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_avg_199_f142001.tif"), format="GTiff", overwrite=TRUE)

# lm_freq_49
#r.mean1 <- focal(freq_light, w=matrix(1/2401, ncol=49, nrow=49),pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_49_f142001.tif"), format="GTiff", overwrite=TRUE)

# Hannes Local Noise: include the extra ones made for the Nigeria case originally

myfun <- function(x) {
  nullcount <- sum(x < 6)
  return(nullcount)
}

avg_light <- raster(paste0("data/downloaded/nightlight/avg/f142001ave.tif"))
r1= avg_light

r.mean1 <- focal(r1, w=matrix(1, ncol=499, nrow=499), fun = myfun, pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_noise_f142001.tif"), format="GTiff", overwrite=TRUE)

#r.mean1 <- focal(avg_light, w=matrix(1, ncol=199, nrow=199), fun = myfun,pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_noise_199_f142001.tif"), format="GTiff", overwrite=TRUE)

# Hannes Local Detections: include the extra ones made for the Nigeria case originally

myfun <- function(x) {
  nullcount <- sum(x == 0)
  return(nullcount)
}

freq_light <- raster(paste0("data/downloaded/nightlight/pct/F142001pct.tif"))
r1 = freq_light

r.mean1 <- focal(r1, w=matrix(1, ncol=399, nrow=399), fun = myfun, pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_detections_f142001.tif"), format="GTiff", overwrite=TRUE)

#r.mean1 <- focal(freq_light, w=matrix(1, ncol=149, nrow=149), fun = myfun,pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_detections_149_f142001.tif"), format="GTiff", overwrite=TRUE)


#################################################

# step 1: 
# prepare regional light characteristic variables
#################################################

# lm_freq_5_f142001 
freq_light <- raster(paste0("data/downloaded/nightlight/pct/F142001.v4b.pct_lights.tif"))
# calculate average freq_light for every pixel in 5x5 window
r.mean1 <- focal(freq_light, w=matrix(1/25, ncol=5, nrow=5),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_5_f142001.tif"), format="GTiff", overwrite=TRUE)

# lm_avg_25
avg_light <- raster(paste0("data/downloaded/nightlight/avg/F142001.v4b_web.avg_vis.tif"))
r.mean1 <- focal(avg_light, w=matrix(1/625, ncol=25, nrow=25),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_avg_25_f142001.tif"), format="GTiff", overwrite=TRUE)

# lm_freq_99
r.mean1 <- focal(freq_light, w=matrix(1/9801, ncol=99, nrow=99),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_99_f142001.tif"), format="GTiff", overwrite=TRUE)

# lm_avg_199
r.mean1 <- focal(avg_light, w=matrix(1/39601, ncol=199, nrow=199),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_avg_199_f142001.tif"), format="GTiff", overwrite=TRUE)

# local_noise 
# The focal window size in local_noise variable is too large and
# leads to comptutational problems. Therefore we break the global image
# into smaller subregions and use parallel processing to 
# spped up the process.
avg_light <- raster(paste0("data/downloaded/nightlight/avg/F142001.v4b_web.avg_vis.tif"))
r1= avg_light
# create a list of coordinates (as pixel numbers) to crop subwindows
loop_list1 <- seq(1,14002,2000)
loop_list2 <- seq(1,42502,2000)
full_looplist = expand.grid(loop_list1,loop_list2)
full_looplist$Var3 = full_looplist$Var1+2500
full_looplist$Var4 = full_looplist$Var2+2500
# set maximum values at the end of the global image
full_looplist$Var3 = ifelse(full_looplist$Var3 > 15725, 15725,  full_looplist$Var3)
full_looplist$Var4 = ifelse(full_looplist$Var4 > 43180, 43180,  full_looplist$Var4)

# here we count the amount of pixels in a 499x499 focal window,
# so we define our own function:
myfun <- function(x) {
  nullcount <- sum(x < 6)
  return(nullcount)
}

# setup parallel processing
detectCores()
cl <- makeCluster(10, type="SOCK")
registerDoSNOW(cl)
getDoParWorkers()
# run the parallel loop for all subregion windows
foreach (x =1:nrow(full_looplist) ,  .packages = c("raster","dplyr")) %dopar% {
  
  # make padding around the windows
  a = full_looplist[x,3]+400
  b = full_looplist[x,4]+400
  c = full_looplist[x,1]-400
  d = full_looplist[x,2]-400
  
  # set windows within the global limits
  a= ifelse(a>15725,15725,a)
  b= ifelse(b>43180,43180,b)
  c= ifelse(c<1,1,c)
  d= ifelse(d<1,1,d)
  
  # crop
  win1 <- crop(r1, extent(r1, c, a, d, b))
  
  # focal
  r.mean1 <- focal(win1, w=matrix(1, ncol=499, nrow=499), fun = myfun)
  # save
  writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_noise_subregions_f142001/win",x,".tif"), format="GTiff", overwrite=TRUE)
}
stopCluster(cl)

# mosaic subregion windows back to global level
files <- list.files(path="data/prepared/nightlight/local_variables/local_noise_subregions_f142001/", pattern=paste0("win"), all.files=T, full.names=T)
ListRasters <- function(list_names) {
  raster_list <- list() 
  for (i in 1:(length(list_names))){
    grd_name <- list_names[i]
    raster_file <- raster(grd_name)
  }
  raster_list <- append(raster_list, raster_file)
}
list_names <- NULL
for (i in 1:length(files)) {
  list_names <- c(list_names, files[i])
}
raster.list <-sapply(list_names, FUN = ListRasters)
names(raster.list) <- NULL
raster.list$fun <- mean
mos <- do.call(mosaic, raster.list)
writeRaster(mos, paste0("data/prepared/nightlight/local_variables/local_noise_f142001.tif"), format="GTiff", overwrite=TRUE)

# myFun <- function(x) { ifelse( is.na(x) , 159201, x) }
# mos <- overlay(mos, fun = myFun)
# writeRaster(mos, paste0("D:/ilari_BG//win_nullcount_399_noedge.tif"), format="GTiff", overwrite=TRUE)

# same for local_detections_f142001
freq_light <- raster(paste0("data/downloaded/nightlight/pct/F142001.v4b.pct_lights.tif"))
r1 = freq_light
loop_list1 <- seq(1,14002,2000)
loop_list2 <- seq(1,42502,2000)
full_looplist = expand.grid(loop_list1,loop_list2)
full_looplist$Var3 = full_looplist$Var1+2500
full_looplist$Var4 = full_looplist$Var2+2500
full_looplist$Var3 = ifelse(full_looplist$Var3 > 15725, 15725,  full_looplist$Var3)
full_looplist$Var4 = ifelse(full_looplist$Var4 > 43180, 43180,  full_looplist$Var4)
# here we count the amount of pixels in a 399x399 focal window,
# so we define our own function:
myfun <- function(x) {
  nullcount <- sum(x == 0)
  return(nullcount)
}
# setup parallel processing
detectCores()
cl <- makeCluster(10, type="SOCK")
registerDoSNOW(cl)
getDoParWorkers()
foreach (x =1:nrow(full_looplist) ,  .packages = c("raster","dplyr")) %dopar% {
  a = full_looplist[x,3]+400
  b = full_looplist[x,4]+400
  c = full_looplist[x,1]-400
  d = full_looplist[x,2]-400
  a= ifelse(a>15725,15725,a)
  b= ifelse(b>43180,43180,b)
  c= ifelse(c<1,1,c)
  d= ifelse(d<1,1,d)
  win1 <- crop(r1, extent(r1, c, a, d, b))
  r.mean1 <- focal(win1, w=matrix(1, ncol=399, nrow=399), fun = myfun)
  writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_detections_subregions_f142001/win",x,".tif"), format="GTiff", overwrite=TRUE)
}
stopCluster(cl)
# mosaic subregion windows back to global level
files <- list.files(path="data/prepared/nightlight/local_variables/local_detections_f142001_subregions/", pattern=paste0("win"), all.files=T, full.names=T)
ListRasters <- function(list_names) {
  raster_list <- list() 
  for (i in 1:(length(list_names))){
    grd_name <- list_names[i]
    raster_file <- raster(grd_name)
  }
  raster_list <- append(raster_list, raster_file)
}
list_names <- NULL
for (i in 1:length(files)) {
  list_names <- c(list_names, files[i])
}
raster.list <-sapply(list_names, FUN = ListRasters)
names(raster.list) <- NULL
raster.list$fun <- mean
mos <- do.call(mosaic, raster.list)
writeRaster(mos, paste0("data/prepared/nightlight/local_variables/local_detections_f142001.tif"), format="GTiff", overwrite=TRUE)
# myFun <- function(x) { ifelse( is.na(x) , 159201, x) }
# mos <- overlay(mos, fun = myFun)
# writeRaster(mos, paste0("D:/Human_Lights/data/prepared/nightlight/local_variables/local_detections.tif"), format="GTiff", overwrite=TRUE)

# We create 4 more local light variables, which are
# only used in the Nigeria example. 
# lm_freq_49
#r.mean1 <- focal(freq_light, w=matrix(1/2401, ncol=49, nrow=49),pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_49_f142001.tif"), format="GTiff", overwrite=TRUE)
# lm_avg_99
#r.mean1 <- focal(avg_light, w=matrix(1/9801, ncol=99, nrow=99),pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_avg_99.tif"), format="GTiff", overwrite=TRUE)
# local_noise_199
#myfun <- function(x) {
#  nullcount <- sum(x < 6)
#  return(nullcount)
#}
#r.mean1 <- focal(avg_light, w=matrix(1, ncol=199, nrow=199), fun = myfun,pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_noise_199b.tif"), format="GTiff", overwrite=TRUE)

# local_detections_149
#myfun <- function(x) {
#  nullcount <- sum(x == 0)
#  return(nullcount)
#}
#r.mean1 <- focal(freq_light, w=matrix(1, ncol=149, nrow=149), fun = myfun,pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_detections_149.tif"), format="GTiff", overwrite=TRUE)

#################################################

# step 2: 
# Global Human Lights prediction
#################################################

# open prepared data
avg_light <- raster(paste0("data/downloaded/nightlight/avg/F142001.v4b_web.avg_vis.tif"))
freq_light <- raster(paste0("data/downloaded/nightlight/pct/F142001.v4b.pct_lights.tif"))
local_noise <- raster(paste0("data/prepared/nightlight/local_variables/local_noise_f142001.tif"))
local_detections <- raster(paste0("data/prepared/nightlight/local_variables/local_detections_f142001.tif"))
built <- raster(paste0("data/prepared/builtup/built_2001.tif"))

# match extent of the input raster images
avg_light = crop(avg_light,extent(local_noise))
freq_light = crop(freq_light,extent(local_noise))
built = crop(built,extent(local_noise))

# stack all variables
model_stack = stack(avg_light,freq_light,local_noise,local_detections,built)
names(model_stack) <- c("avg_light", "freq_light","local_noise","local_detections","built")
# create stack for prediction, where the built-up variable is excluded
pred_stack = dropLayer(model_stack, c(5))
# Get the data into a tabular format
valuetable <- getValues(model_stack)
valuetable1 <- as.data.frame(valuetable)
df = valuetable1

# create 100 subsamples
for (y in 1:100) {
  set.seed(y)
  df_train = sample(1: nrow(df), nrow(df)/100)
  df_train = df[df_train,]
  df_train$built =  ifelse(df_train$built>0,"No","Yes")
  saveRDS(df_train, file=paste0("data/predictions/Global_Human_Lights/100_samples/sample_",y,".Rda"))
}

# make predictions for each subsample
set.seed(42)
detectCores()
cl <- makeCluster(5, type="SOCK")
registerDoSNOW(cl)
getDoParWorkers()
foreach (y =1:100 ,  .packages = c("raster","caret","ranger","dplyr")) %dopar% {
  df_train <- readRDS(file=paste0("data/predictions/Global_Human_Lights/100_samples/sample_",y,".Rda"))
  tgrid <- expand.grid(.mtry = 1,.splitrule = "gini", .min.node.size = 1000)
  trainControl = trainControl(classProbs=TRUE,method = "none")
  ranger.model<-train(as.factor(built) ~ ., data=df_train,method = "ranger",tuneGrid = tgrid, trControl=trainControl,num.trees=100, always.split.variables = c("avg_light", "freq_light"))
  ranger.pred<-predict(pred_stack,ranger.model, type = "prob")
  writeRaster(ranger.pred, paste0("data/predictions/Global_Human_Lights_f142001/100_preditions/prediction_",y,".Rda"), format="GTiff", overwrite=TRUE)
}

# average predictions 
files <- list.files(path="data/predictions/Global_Human_Lights_f142001/100_preditions/", pattern=".tif$", all.files=T, full.names=T)
ListRasters <- function(list_names) {
  raster_list <- list() 
  for (i in 1:(length(list_names))){ 
    grd_name <- list_names[i] 
    raster_file <- raster(grd_name)
  }
  raster_list <- append(raster_list, raster_file)
}
list_names <- NULL
for (i in 1:length(files)) {
  list_names <- c(list_names, files[i])
}
raster.list <-sapply(list_names, FUN = ListRasters)
names(raster.list) <- NULL
raster.list$fun <- mean
mos <- do.call(mosaic, raster.list)
writeRaster(mos, paste0("data/final_products/GHL_average_prediction_f142001.tif"), format="GTiff", overwrite=TRUE)

#################################################

# step 1-2: 
# Local Human Lights prediction
#################################################

# open prepared data
avg <- raster(paste0("data/downloaded/nightlight/avg/F142001.v4b_web.avg_vis.tif"))
pct <- raster(paste0("data/downloaded/nightlight/pct/F142001.v4b.pct_lights.tif"))
lma <- raster(paste0("data/prepared/nightlight/local_variables/lm_freq_5_f142001.tif"))
lmb <- raster(paste0("data/prepared/nightlight/local_variables/lm_avg_25_f142001.tif"))
lmc <- raster(paste0("data/prepared/nightlight/local_variables/lm_freq_99_f142001.tif"))
lmd <- raster(paste0("data/prepared/nightlight/local_variables/lm_avg_199_f142001.tif"))
lme <- raster(paste0("data/prepared/nightlight/local_variables/local_noise_f142001.tif"))
lmf <- raster(paste0("data/prepared/nightlight/local_variables/local_detections_f142001.tif"))
bui <- raster(paste0("data/prepared/builtup/built_2001.tif"))

# match extent of the input raster images
avg = crop(avg,extent(lme))
pct = crop(pct,extent(lme))
lma = crop(lma,extent(lme))
lmb = crop(lmb,extent(lme))
lmc = crop(lmc,extent(lme))
lmd = crop(lmd,extent(lme))
lmf = crop(lmf,extent(lme))
bui = crop(bui,extent(lme))

jump_list1 <- c(0,250,500,750,1000,1250,1500,1750)
jump_list2 <- c(0,250,500,750,1000,1250,1500,1750)
full_jumplist = expand.grid(jump_list1,jump_list2)

detectCores()
cl <- makeCluster(5, type="SOCK")
registerDoSNOW(cl)
getDoParWorkers()

set.seed(42)
foreach (y =1:nrow(full_jumplist) ,  .packages = c("raster","caret","ranger","dplyr")) %dopar% {
  
  # use specific temp folder that is cleaned at every step to avoid using all space
  temp.rand <- paste0(sample(1:9,5,replace=T), collapse = "")
  mytemp <- file.path("C:/Projects/Masters Thesis/Thesis/", paste0("r_temp/r_", temp.rand)) # hannes - check this folder
  dir.create(mytemp, recursive = T)
  rasterOptions(tmpdir=mytemp, progress = "text", timer=T)
  Sys.setenv(TEMP = mytemp)
  
  
  # create indice for regional subwindows
  start1 = full_jumplist[y,1]
  start2 = full_jumplist[y,2]
  loop_list1 <- seq(start1,16000,2000)
  loop_list2 <- seq(start2,45000,2000)
  full_looplist = expand.grid(loop_list1,loop_list2)
  full_looplist$Var3 = full_looplist$Var1+1999
  full_looplist$Var4 = full_looplist$Var2+1999
  full_looplist = filter(full_looplist, Var3 < 15801)
  full_looplist$Var3 = ifelse(full_looplist$Var3 > 15725, 15725,  full_looplist$Var3)
  full_looplist = filter(full_looplist, Var4 < 43251)
  full_looplist$Var4 = ifelse(full_looplist$Var4 > 43180, 43180,  full_looplist$Var4)
  
  outdir <- paste0("data/predictions/Local_Human_Lights/sub_window_predictions/part_",y)
  unlink(outdir, recursive = TRUE)
  dir.create(outdir)
  
  # prediction loop
  for (x in 1:nrow(full_looplist)) {  
    
    # pick indice row
    i = full_looplist[x,1]
    j = full_looplist[x,2] 
    
    # crop data to regional subwindows
    avg1 <- crop(avg, extent(avg, full_looplist[x,1], full_looplist[x,3], full_looplist[x,2], full_looplist[x,4]))
    pct1 <- crop(pct, extent(pct, full_looplist[x,1], full_looplist[x,3], full_looplist[x,2], full_looplist[x,4]))
    bui1 <- crop(bui, extent(bui, full_looplist[x,1], full_looplist[x,3], full_looplist[x,2], full_looplist[x,4]))
    
    lma1 <- crop(lma, extent(lma, full_looplist[x,1], full_looplist[x,3], full_looplist[x,2], full_looplist[x,4]))
    lmb1 <- crop(lmb, extent(lmb, full_looplist[x,1], full_looplist[x,3], full_looplist[x,2], full_looplist[x,4]))
    lmc1 <- crop(lmc, extent(lmc, full_looplist[x,1], full_looplist[x,3], full_looplist[x,2], full_looplist[x,4]))
    lmd1 <- crop(lmd, extent(lmd, full_looplist[x,1], full_looplist[x,3], full_looplist[x,2], full_looplist[x,4]))
    lme1 <- crop(lme, extent(lme, full_looplist[x,1], full_looplist[x,3], full_looplist[x,2], full_looplist[x,4]))
    lmf1 <- crop(lmf, extent(lmf, full_looplist[x,1], full_looplist[x,3], full_looplist[x,2], full_looplist[x,4]))
    
    # prepare data
    model_stack = stack(avg1,pct1,lma1,lmb1,lmc1,lmd1,lme1,lmf1,bui1)
    names(model_stack) <- c("avg1", "pct1","lma1","lmb1","lmc1","lmd1","lme1","lmf1","bui1")
    pred_stack <- dropLayer(model_stack, c(9))
    valuetable <- getValues(model_stack)
    df_train <- as.data.frame(valuetable)
    df_train = na.omit(df_train)
    df_train2 = sample(1: nrow(df_train), nrow(df_train)/10)
    df_train = df_train[df_train2,]
    
    # set predictions to 0 for windows where built up % is lower than 0.0001
    if(mean(df_train$bui1)<0.000001) {
      
      values(avg1)[values(avg1) >= 0] = 0
      writeRaster(avg1, paste0("data/predictions/Local_Human_Lights/sub_window_predictions_f142001/part_",y,"/part_",y,"_",i,"_",j,".tif"), format="GTiff", overwrite=TRUE)
      
    } else {
      
      # make predictions
      df_train$bui1 =  ifelse(df_train$bui1>0,"No","Yes")
      tgrid <- expand.grid(.mtry = 1,.splitrule = "gini", .min.node.size = 10)
      trainControl = trainControl(classProbs=TRUE,method = "none")
      ranger.model<-train(as.factor(bui1) ~ ., data=df_train,method = "ranger",tuneGrid = tgrid, trControl=trainControl,num.trees=200, always.split.variables = c("avg1","pct1"))
      ranger.pred<-predict(pred_stack,ranger.model, type = "prob")
      writeRaster(ranger.pred, paste0("data/predictions/Local_Human_Lights/sub_window_predictions_f142001/part_",y,"/part_",y,"_",i,"_",j,".tif"), format="GTiff", overwrite=TRUE)
      
    }
  }
  unlink(Sys.getenv("TEMP"), recursive = TRUE)
}
stopCluster(cl)

#################################################

# step 3: 
# Global Human Lights mask
#################################################

built <- raster(paste0("data/prepared/builtup/built_2001.tif"))
GHL_avg_pred = raster(paste0("data/final_products/GHL_average_prediction_f142001.tif"))
avg_light <- raster(paste0("data/downloaded/nightlight/avg/F142001.v4b_web.avg_vis.tif"))

avg_light = crop(avg_light,extent(GHL_avg_pred))
built = crop(built,extent(GHL_avg_pred))
# find TH
THsearch_stack = stack(GHL_avg_pred,built,avg_light)
valuetable <- getValues(THsearch_stack)
valuetable1 <- as.data.frame(valuetable)
df_THsearch = valuetable1
names(df_THsearch) <- c("GHL_avg_pred", "built","avg_light")

# global probability threshold of 32 % gives tolerance of 
# 0.000766103, which is rounded to 0.08 % in the article.
under_5_pixels_GHL = sum(df_THsearch$GHL_avg_pred>0.32 & df_THsearch$avg_light<5)
under_5_pixels_total = sum(df_THsearch$avg_light<5)
tolerance = under_5_pixels_GHL / under_5_pixels_total

# we use the probability thershold that gives the 
# wanted tolerance to create the mask.
myFun <- function(x) { ifelse( x > 0.32, 1, 0) }
GHL_mask <- overlay(GHL_avg_pred, fun = myFun) 
writeRaster(GHL_mask, "data/final_products/GHL_mask_f142001.tif", format="GTiff", overwrite=TRUE)

#################################################

# step 3: 
# Local Human Lights mask
#################################################

avg <- raster(paste0("data/downloaded/nightlight/avg/F142001.v4b_web.avg_vis.tif"))

detectCores()
cl <- makeCluster(10, type="SOCK")
registerDoSNOW(cl)
getDoParWorkers()

foreach (y =1:64 ,  .packages = c("raster","dplyr")) %dopar% {
  files <- list.files(paste0(path="data/predictions/Local_Human_Lights/sub_window_predictions_f142001/part_",y), pattern=".tif$", all.files=T, full.names=T)
  
  outdir <- paste0("data/predictions/Local_Human_Lights/mask_f142001/part_",y)
  unlink(outdir, recursive = TRUE)
  dir.create(outdir)
  
  for (i in 1:length(files)) {
    
    z = files[i]
    print(i)
    
    # open sub region window prediction
    ranger.pred <- raster(paste0(z))
    # crop matching average light window
    avg1 <- crop(avg, ranger.pred)
    
    # get raster values into table
    avg1_values_stack = stack(avg1,ranger.pred)
    names(avg1_values_stack) <- c("avg1","ile1")
    avg_valuetable <- getValues(avg1_values_stack)
    avg1_values <- as.data.frame(avg_valuetable)
    avg1_values = na.omit(avg1_values)
    # calculate number of pixels under value 5 in the average light image
    avg1_valuesz <- filter(avg1_values, avg1<5)
    avg1_lit = length(avg1_valuesz$avg1)
    avg1_lit
    # calculate share of pixels under value 5 in the average light image
    avg1_lit_pr = avg1_lit/length(avg1_values$avg1)
    avg1_lit_pr
    
    # extract unique predicted probabilities to try as thresholds
    pos_values = as.numeric(unlist(unique(avg1_valuesz$ile1)))
    pos_values = sort(pos_values,decreasing=T)
    pos_values_length = length(pos_values)
    
    # if all predictions are 0, then all values in the mask are set to 0
    if (mean(avg1_values$ile1)==0) {
      ranger.pred.bin = ranger.pred
      myFun <- function(x) { ifelse( x >= 0, 0, x) }
      ranger.pred.bin <- overlay(ranger.pred.bin, fun = myFun)
      writeRaster(ranger.pred.bin, paste0("data/predictions/Local_Human_Lights/mask_f142001/part_",y,"/part_",y,"_",i,".tif"), format="GTiff", overwrite=TRUE)
      
    } else {  
      
      # tolerance threshold of 4%
      TH_condition = 0.04
      # try every 1000th value in the threshold candidate list.
      test_TH_position_add = pos_values_length/1000
      # start at first threshold candidate
      test_TH_position = 1    
      test_TH = pos_values[test_TH_position]
      whiletest = 1
      round = 1
      # run the loop until the iteration finds exactly the right threshold candidate
      while (test_TH_position_add > 2 & round < 10000) {
        
        # calculate share of lit pixels under value 5 in the average light image at a tested threshold
        correct_class = sum(avg1_values$ile>=test_TH & avg1_values$avg1<5)
        acc_now = correct_class/avg1_lit
        current_TH = test_TH
        
        # if the threshold is below backstop of 20%, then stop the loop
        round = round+1
        round = ifelse(current_TH<0.2,10001,round)
        
        # if the share is lower than 4%, then try the next 1000th value in the threshold candidate list
        if(acc_now < TH_condition) {
          test_TH_position = test_TH_position_add+test_TH_position
          test_TH_position = ifelse(test_TH_position > pos_values_length,pos_values_length,test_TH_position)
          test_TH = pos_values[test_TH_position] 
          # if the share is higher than 4%, then take a step back and halve the added position 
          # (first to 500, then to 250... until reaching 2)
        } else {
          test_TH_position = test_TH_position-test_TH_position_add
          test_TH_position_add = round(test_TH_position_add /2)
          test_TH_position = test_TH_position_add+test_TH_position
          test_TH = pos_values[test_TH_position] 
        } 
        
        acc_last =acc_now
        
        print(round)
        print(test_TH)
        print(acc_now)
        print(test_TH_position_add)
      }
      
      # the remaining threshold candidate is the one that is closest to 4%
      max_acc_TH = current_TH 
      max_acc_pixels = sum(avg1_values$ile1>max_acc_TH) 
      max_acc_pixels/length(avg1_values$avg1)
      TH_condition
      
      # use the threshold to mask the subregion window
      TH = current_TH
      ranger.pred.bin = ranger.pred
      myFun <- function(x) { ifelse( x >= TH, 1, 0) }
      ranger.pred.bin <- overlay(ranger.pred.bin, fun = myFun)
      
      writeRaster(ranger.pred.bin, paste0("data/predictions/Local_Human_Lights/mask_f142001/part_",y,"/part_",y,"_",i,".tif"), format="GTiff", overwrite=TRUE)
    }
  }
  
}

stopCluster(cl)


#################################################

# step 4: 
# Global Human Lights final
#################################################

# use the mask to filter out background noise in the average light image
GHL_mask = raster(paste0("data/final_products/GHL_mask_f142001.tif"))
avg_light <- raster(paste0("data/downloaded/nightlight/avg/F142001.v4b_web.avg_vis.tif"))
avg_light = crop(avg_light,extent(GHL_mask))
Global_Human_Lights_version_1 <- overlay(GHL_mask, avg_light, fun=function(x,y){(x*y)} )
writeRaster(Global_Human_Lights_version_1, "data/final_products/Global_Human_Lights_version_1_f142001.tif", format="GTiff", overwrite=TRUE)

#################################################

# step 3-4: 
# Local Human Lights final
#################################################

# mosaic all subregion window masks
detectCores()
cl <- makeCluster(5, type="SOCK")
registerDoSNOW(cl)
getDoParWorkers()
foreach (y =1:64 ,  .packages = c("raster","dplyr")) %dopar% {
  files <- list.files(paste0(path="data/predictions/Local_Human_Lights/mask_f142001/part_",y), pattern=".tif$", all.files=T, full.names=T)
  ListRasters <- function(list_names) {
    raster_list <- list() 
    for (i in 1:(length(list_names))){ 
      grd_name <- list_names[i] 
      raster_file <- raster(grd_name)
    }
    raster_list <- append(raster_list, raster_file) 
  }
  list_names <- NULL
  for (i in 1:length(files)) {
    list_names <- c(list_names, files[i])
  }
  raster.list <-sapply(list_names, FUN = ListRasters)
  names(raster.list) <- NULL
  raster.list$fun <- mean
  mos <- do.call(mosaic, raster.list)
  writeRaster(mos, paste0("data/predictions/Local_Human_Lights/mosaic1_f142001/complete_part_",y,".tif"), format="GTiff", overwrite=TRUE)
}
stopCluster(cl)

# mosaic complete parts
files <- list.files(paste0(path="data/predictions/Local_Human_Lights/mosaic1_f142001/"), pattern=".tif$", all.files=T, full.names=T)
ListRasters <- function(list_names) {
  raster_list <- list()
  for (i in 1:(length(list_names))){ 
    grd_name <- list_names[i] 
    raster_file <- raster(grd_name)
  }
  raster_list <- append(raster_list, raster_file) 
}
list_names <- NULL
for (i in 1:length(files)) {
  list_names <- c(list_names, files[i])
}
raster.list <-sapply(list_names, FUN = ListRasters)
names(raster.list) <- NULL
raster.list$fun <- mean
mos <- do.call(mosaic, raster.list)
writeRaster(mos, paste0("data/final_products/LHLmask_average_f142001.tif"), format="GTiff", overwrite=TRUE)

# we use the probability thershold that gives the 
# wanted tolerance to create the mask.
myFun <- function(x) { ifelse( x > 0.13, 1, 0) }
LHL_mask <- overlay(mos, fun = myFun) 
writeRaster(LHL_mask, "data/final_products/LHL_mask_f142001.tif", format="GTiff", overwrite=TRUE)

LHL_mask = raster(paste0("data/final_products/LHL_mask_f142001.tif"))
avg_light <- raster(paste0("data/downloaded/nightlight/avg/F142001.v4b_web.avg_vis.tif"))
avg_light = crop(avg_light,extent(LHL_mask))
Local_Human_Lights_version_1 <- overlay(LHL_mask, avg_light, fun=function(x,y){(x*y)} )
writeRaster(Local_Human_Lights_version_1, "data/final_products/Local_Human_Lights_version_1_f142001.tif", format="GTiff", overwrite=TRUE)

