############### SESYNC Research Support: Agbirds project ########## 
## Processing agricultural crop data by week.
## 
##
## DATE CREATED: 08/03/2018
## DATE MODIFIED: 11/13/2018
## AUTHORS: Benoit Parmentier  
## Version: 1
## PROJECT: Agbirds
## ISSUE: 
## TO DO:
##
## COMMIT: Fixing some NA issue in the crop haversting and planting
##

###################################################
#

###### Library used

library(sp) # spatial/geographic objects and functions
library(rgdal) #GDAL/OGR binding for R with functionalities
library(spdep) #spatial analyses operations, functions etc.
library(gtools) # contains mixsort and other useful functions
library(maptools) # tools to manipulate spatial data
library(parallel) # parallel computation, part of base package no
library(rasterVis) # raster visualization operations
library(raster) # raster functionalities
library(forecast) #ARIMA forecasting
library(xts) #extension for time series object and analyses
library(zoo) # time series object and analysis
library(lubridate) # dates functionality
library(colorRamps) #contains matlab.like color palette
library(rgeos) #contains topological operations
library(sphet) #contains spreg, spatial regression modeling
library(BMS) #contains hex2bin and bin2hex, Bayesian methods
library(bitops) # function for bitwise operations
library(foreign) # import datasets from SAS, spss, stata and other sources
library(gdata) #read xls, dbf etc., not recently updated but useful
library(classInt) #methods to generate class limits
library(plyr) #data wrangling: various operations for splitting, combining data
#library(gstat) #spatial interpolation and kriging methods
library(readxl) #functionalities to read in excel type data
library(psych) #pca/eigenvector decomposition functionalities
library(snow)
library(sf)
library(car)

###### Functions used in this script and sourced from other files


screen_for_crop_status <- function(state_val,data_in){
  ##This function recode crop values and check for overlap between planting and 
  #harvesting periods. The steps in the recoding and screening:
  # - subset by state
  # - find unique crop types
  # - for each crop recode values for haversting from 1 and 2 to 3 and 4
  #
  ######## Start script ######
  
  #### Step 1: subset by state:
  data_subset <- subset(data_in,State==state_val)
  #dim(data_sub  set)
  
  #### Step 2: find unique crop types
  
  crop_type <- unique(data_subset$Crop)
   
  #### Step 3: recode crop values for havesting
  
  #undebug(recode_crop)
  obj_crop <- recode_crop(crop_type=crop_type[2],data_crop=data_subset)
  
  list_obj_crop <- mclapply(crop_type,
           FUN=recode_crop,
           data_crop=data_subset,
           mc.preschedule = F,
           mc.cores = num_cores)
  
  ### Assign crop values
  names(list_obj_crop) <- crop_type
  
  ### return object
  return(list_obj_crop)
}

recode_crop <- function(crop_type,data_crop){
  ##This functions recode values for Harvesting and Plangint
  # Harvesting values of 1 and 2 to 3 and 4, and
  #then merge the planting and harvesting rows for each crop and each state. 
  #There should not be overlap between the planting and harvesting, but using xtab 
  #to find errors will be useful.
  
  data_tmp <- subset(data_crop,data_crop$Crop==crop_type)
  names(data_tmp)
  head(data_tmp)
  selected_col <- grepl("X", names(data_tmp))
  
  row.names(data_tmp) <- data_tmp$Plant_Harvest
  
  weeks_df <- as.data.frame(t(data_tmp[,selected_col]))
  
  names(weeks_df)
  val_range <- range(weeks_df[,1]+weeks_df[,2])
  
  val_tabs <- table(weeks_df[,1],weeks_df[,2])
  
  range_df <- data.frame(min=val_range[1],max=val_range[2])
  range_df$crop <- crop_type
  #weeks_df$Harvesting
  #test <- car::recode(weeks_df$Harvesting,"1=3";"2=4")
  weeks_df$Harvesting <- dplyr::recode(weeks_df$Harvesting, `1` = 3L, `2` = 4L)
  #test <- dplyr::recode(weeks_df$Harvesting, `1` = 3, `2` = 4)
  
  #weeks_df$Harvesting <-recode(weeks_df$Harvesting,"1=3;2=4")
  
  data_out <- as.data.frame(t(weeks_df))
  
  data_out <- cbind(data_tmp[,!selected_col],data_out)
  #dim(data_out)
  #class(data_out)
  
  ### Fixing some NA issue in the crop haversting and planting
  if(!is.na(val_range[1]) || !is.na(val_range[2])){
    if(range_df$max > 2){
      data_out$flag <- 1
    }else{
      data_out$flag <- 0
    }
  }else{
    data_out$flag <- NA
  }
  
  obj <- list(val_tabs,range_df,data_out)
  names(obj) <- c("val_tabs","range_df","data_out")
  
  return(obj)
}

reclassify_raster <- function(j,crop_status,in_filename,file_format){
  
  col_val <- paste0("X",j) # week
  
  val_to_code <- sum(crop_status_df[[col_val]]) #first value is planting, the other one is havesting
  
  if(val_to_recode>0){
    if(algorithm=="R"){
      
      df <- data.frame(id=val, v=val_to_code)
      out_filename <- "tmp.tif"
      r_out <- subs(r_val, df,filename=out_filename)
      #x2 <- subs(r, df, subsWithNA=FALSE)
      gald_command <- NULL
    }
    
    if(algorithm=="GDAL"){
      
      #Just use gdal_calc.py
      
      #For example, below will convert the values below 3 to 0 and above 3 to 1. You can use equals as well.
      in_filename <- crop_out_filename
      #gdal_calc.py -A C:temp\raster.tif --outfile=result.tiff --calc="0*(A<3)" --calc="1*(A>3)"
      #gdal_command <- gdal_calc.py -A C:temp\raster.tif --outfile=result.tiff --calc="val_to_recode*(A==val)" --calc="0*(A==val)"
      #gdal_command <- gdal_calc.py -A C:temp\raster.tif --outfile=result.tiff --calc="val_to_recode*(A==val)" --calc="0*(A==val)"
      out_filename <- paste0(region_name,"_",crop_name,"_","week_",j,file_format)
      
      gdal_command <- paste0("gdal_calc.py",
                             " -A ",in_filename,
                             " --outfile=",out_filename,
                             " --calc=",paste0("'(",val_to_code,"*(A==",val,"))'")#,
                             #" --calc=",paste0("'(0*(A!=",val,"))'")
      )
      gdal_command
      system(gdal_command)
      #r<- raster(out_filename)
      
    }
    
  }else{
    out_filename <- NULL
  }
  
  obj_out <- list(out_filename,gdal_command)
  
  return(obj_out)
}

generate_crop_status_raster <- function(in_filename_raster,crop_name,crop_status_df,
                                        algorithm,num_cores,file_format,out_dir,out_suffix){
  #
  #
  #1)  in_filename_raster
  #2) crop_name,algorithm
  #3) num_cores
  #4) file_format
  #5) out_dir
  #6) out_suffix
  #
  
  ##### Start script #######
  
  r_region <- raster(file.path(in_dir,in_filename_raster))
  
  #plot(r_region)
  #r_region
  
  #str(r_region)
  r_val <- r_region
  
  #mask(r_val,inverse=T,mask_value=val)
  
  fun <- function(x) { x[x!=val] <- NA; return(x) }
  r_val <- calc(r_val, fun)
  crop_out_filename <- paste0(crop_name,"_",val,file_format)
  writeRaster(r_val,filename = file.path(out_dir,crop_out_filename))
  
  ### Now generate for 52 weeks:
  
  j <- 1
  #use_r <- TRUE
  test_obj <- reclassify_raster(1,
                       crop_status=crop_status,
                       in_filename=in_filename,
                       file_format=file_format)
  
  list_obj <- mclapply(1:52,
                       FUN=reclassify_raster,
                       crop_status=crop_status,
                       in_filename=in_filename,
                       file_format=file_format)
  
  return(out_filename)
}

##################  End of script #########