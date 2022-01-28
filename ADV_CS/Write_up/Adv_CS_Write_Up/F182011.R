
##### F182011 #####

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

### Cropping to only South Africa to speed up process

f182011ave <- raster("data/downloaded/nightlight/avg/F182011.v4c_web.avg_vis.tif")
f182011pct <- raster("data/downloaded/nightlight/pct/F182011.v4c.avg_lights_x_pct.tif")

shpward <- st_read("data/c1a23db3-8f2f-40c7-bd3e-ae9fed1eb0322020329-1-ihxzob.tk9kh.shp") %>% filter(OBJECTID < 4278)
shpward <- as_Spatial(shpward)

f182011avecrop <- crop(f182011ave, shpward) # crop raster layer to only SA
#f121999avgmask <- mask(f121999avgcrop, shpward) # mask the extent to only include data within the shapefile boundaries
writeRaster(f182011avecrop, paste0("data/downloaded/nightlight/avg/f182011ave.tif"), format="GTiff", overwrite=TRUE)


f182011pctcrop <- crop(f182011pct, shpward) # crop raster layer to only SA
#f121999pctmask <- mask(f121999pctcrop, shpward) # mask the extent to only include data within the shapefile boundaries
writeRaster(f182011pctcrop, paste0("data/downloaded/nightlight/pct/f182011pct.tif"), format="GTiff", overwrite=TRUE)

##### Local Vars

# lm_freq_5 
freq_light <- raster(paste0("data/downloaded/nightlight/pct/f182011pct.tif"))

# calculate average freq_light for every pixel in 5x5 window
r.mean1 <- focal(freq_light, w=matrix(1/25, ncol=5, nrow=5),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_5_f182011.tif"), format="GTiff", overwrite=TRUE)

# lm_avg_25
avg_light <- raster(paste0("data/downloaded/nightlight/avg/f182011ave.tif"))

r.mean1 <- focal(avg_light, w=matrix(1/625, ncol=25, nrow=25),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_avg_25_f182011.tif"), format="GTiff", overwrite=TRUE)

# lm_freq_99
r.mean1 <- focal(freq_light, w=matrix(1/9801, ncol=99, nrow=99),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_99_f182011.tif"), format="GTiff", overwrite=TRUE)

# lm_avg_199
r.mean1 <- focal(avg_light, w=matrix(1/39601, ncol=199, nrow=199),pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_avg_199_f182011.tif"), format="GTiff", overwrite=TRUE)

# lm_freq_49
#r.mean1 <- focal(freq_light, w=matrix(1/2401, ncol=49, nrow=49),pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/lm_freq_49_f182011.tif"), format="GTiff", overwrite=TRUE)

# Hannes Local Noise: include the extra ones made for the Nigeria case originally

myfun <- function(x) {
  nullcount <- sum(x < 6)
  return(nullcount)
}

avg_light <- raster(paste0("data/downloaded/nightlight/avg/f182011ave.tif"))
r1= avg_light

r.mean1 <- focal(r1, w=matrix(1, ncol=499, nrow=499), fun = myfun, pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_noise_f182011.tif"), format="GTiff", overwrite=TRUE)

#r.mean1 <- focal(avg_light, w=matrix(1, ncol=199, nrow=199), fun = myfun,pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_noise_199_f182011.tif"), format="GTiff", overwrite=TRUE)

# Hannes Local Detections: 

myfun <- function(x) {
  nullcount <- sum(x == 0)
  return(nullcount)
}

freq_light <- raster(paste0("data/downloaded/nightlight/pct/F182011pct.tif"))
r1 = freq_light

r.mean1 <- focal(r1, w=matrix(1, ncol=399, nrow=399), fun = myfun, pad=TRUE, padValue=0)
writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_detections_f182011.tif"), format="GTiff", overwrite=TRUE)

#r.mean1 <- focal(freq_light, w=matrix(1, ncol=149, nrow=149), fun = myfun,pad=TRUE, padValue=0)
#writeRaster(r.mean1, paste0("data/prepared/nightlight/local_variables/local_detections_149_f182011.tif"), format="GTiff", overwrite=TRUE)

#################################################

# step 1-2: 
# Local Human Lights prediction
#################################################

# open prepared data
avg <- raster(paste0("data/downloaded/nightlight/avg/F182011.v4c_web.avg_vis.tif"))
pct <- raster(paste0("data/downloaded/nightlight/pct/F182011.v4c.pct_lights.tif"))
lma <- raster(paste0("data/prepared/nightlight/local_variables/lm_freq_5_f182011.tif"))
lmb <- raster(paste0("data/prepared/nightlight/local_variables/lm_avg_25_f182011.tif"))
lmc <- raster(paste0("data/prepared/nightlight/local_variables/lm_freq_99_f182011.tif"))
lmd <- raster(paste0("data/prepared/nightlight/local_variables/lm_avg_199_f182011.tif"))
lme <- raster(paste0("data/prepared/nightlight/local_variables/local_noise_f182011.tif"))
lmf <- raster(paste0("data/prepared/nightlight/local_variables/local_detections_f182011.tif"))
bui <- raster(paste0("data/prepared/builtup/built_2011.tif"))

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
      writeRaster(avg1, paste0("data/predictions/Local_Human_Lights/sub_window_predictions_f182011/part_",y,"/part_",y,"_",i,"_",j,".tif"), format="GTiff", overwrite=TRUE)
      
    } else {
      
      # make predictions
      df_train$bui1 =  ifelse(df_train$bui1>0,"No","Yes")
      tgrid <- expand.grid(.mtry = 1,.splitrule = "gini", .min.node.size = 10)
      trainControl = trainControl(classProbs=TRUE,method = "none")
      ranger.model<-train(as.factor(bui1) ~ ., data=df_train,method = "ranger",tuneGrid = tgrid, trControl=trainControl,num.trees=200, always.split.variables = c("avg1","pct1"))
      ranger.pred<-predict(pred_stack,ranger.model, type = "prob")
      writeRaster(ranger.pred, paste0("data/predictions/Local_Human_Lights/sub_window_predictions_f182011/part_",y,"/part_",y,"_",i,"_",j,".tif"), format="GTiff", overwrite=TRUE)
      
    }
  }
  unlink(Sys.getenv("TEMP"), recursive = TRUE)
}
stopCluster(cl)

#################################################

# step 3: 
# Local Human Lights mask
#################################################

avg <- raster(paste0("data/downloaded/nightlight/avg/F182011.v4c_web.avg_vis.tif"))

detectCores()
cl <- makeCluster(10, type="SOCK")
registerDoSNOW(cl)
getDoParWorkers()

foreach (y =1:64 ,  .packages = c("raster","dplyr")) %dopar% {
  files <- list.files(paste0(path="data/predictions/Local_Human_Lights/sub_window_predictions_f182011/part_",y), pattern=".tif$", all.files=T, full.names=T)
  
  outdir <- paste0("data/predictions/Local_Human_Lights/mask_f182011/part_",y)
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
      writeRaster(ranger.pred.bin, paste0("data/predictions/Local_Human_Lights/mask_f182011/part_",y,"/part_",y,"_",i,".tif"), format="GTiff", overwrite=TRUE)
      
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
      
      writeRaster(ranger.pred.bin, paste0("data/predictions/Local_Human_Lights/mask_f182011/part_",y,"/part_",y,"_",i,".tif"), format="GTiff", overwrite=TRUE)
    }
  }
  
}

stopCluster(cl)

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
  files <- list.files(paste0(path="data/predictions/Local_Human_Lights/mask_f182011/part_",y), pattern=".tif$", all.files=T, full.names=T)
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
  writeRaster(mos, paste0("data/predictions/Local_Human_Lights/mosaic1_f182011/complete_part_",y,".tif"), format="GTiff", overwrite=TRUE)
}
stopCluster(cl)

# mosaic complete parts
files <- list.files(paste0(path="data/predictions/Local_Human_Lights/mosaic1_f182011/"), pattern=".tif$", all.files=T, full.names=T)
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
writeRaster(mos, paste0("data/final_products/LHLmask_average_f182011.tif"), format="GTiff", overwrite=TRUE)

# we use the probability thershold that gives the 
# wanted tolerance to create the mask.
myFun <- function(x) { ifelse( x > 0.13, 1, 0) }
LHL_mask <- overlay(mos, fun = myFun) 
writeRaster(LHL_mask, "data/final_products/LHL_mask_f182011.tif", format="GTiff", overwrite=TRUE)

LHL_mask = raster(paste0("data/final_products/LHL_mask_f182011.tif"))
avg_light <- raster(paste0("data/downloaded/nightlight/avg/F182011.v4c_web.avg_vis.tif"))
avg_light = crop(avg_light,extent(LHL_mask))
Local_Human_Lights_version_1 <- overlay(LHL_mask, avg_light, fun=function(x,y){(x*y)} )
writeRaster(Local_Human_Lights_version_1, "data/final_products/Local_Human_Lights_version_1_f182011.tif", format="GTiff", overwrite=TRUE)
