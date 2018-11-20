############### SESYNC Research Support: Agbirds project ########## 
## Processing agricultural crop data by week.
## 
##
## DATE CREATED: 08/03/2018
## DATE MODIFIED: 11/20/2018
## AUTHORS: Benoit Parmentier  
## Version: 1
## PROJECT: Agbirds
## ISSUE: 
## TO DO:
##
## COMMIT: extracting output information.
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

reclassify_raster <- function(j,crop_status,in_filename,algorithm,file_format){
  # This function reclassifies a raster into a specific class given data.frame of input.
  # The goal is to generate one raster for every week in a year.
  #
  #INPUTS:
  #1) j: week considered from 1 to 52
  #2) crop_status_df: input file containing crop status: 0,1,2,3,4
  #3)
  #4) algorithm: GDAL or R, use GDAL for larger images
  #OUTPUTS
  #
  
  col_val <- paste0("X",j) # week
  
  val_to_code <- sum(crop_status_df[[col_val]]) #first value is planting, the other one is havesting
  
  if(val_to_code>0){
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
      #in_filename <- crop_out_filename
      #gdal_calc.py -A C:temp\raster.tif --outfile=result.tiff --calc="0*(A<3)" --calc="1*(A>3)"
      #gdal_command <- gdal_calc.py -A C:temp\raster.tif --outfile=result.tiff --calc="val_to_recode*(A==val)" --calc="0*(A==val)"
      #gdal_command <- gdal_calc.py -A C:temp\raster.tif --outfile=result.tiff --calc="val_to_recode*(A==val)" --calc="0*(A==val)"
      out_filename <- paste0(region_name,"_",crop_name,"_","week_",j,file_format)
      
      gdal_command <- paste0("gdal_calc.py",
                             " -A ",in_filename,
                             " --outfile=",out_filename,
                             " --calc=",paste0("'(",val_to_code,"*(A==",val,"))'"),
                             " --overwrite")
      
      gdal_command
      system(gdal_command)
      #r<- raster(out_filename)
      
    }
    
  }else{
    out_filename <- NULL
    gdal_command <- NULL
  }
  
  obj_out <- list(out_filename,gdal_command)
  names(obj_out) <- c("out_filename","gdal_command")
  
  return(obj_out)
}

generate_crop_status_raster <- function(in_filename_raster,crop_name,crop_status_df,
                                        algorithm,num_cores,file_format,out_dir,out_suffix){
  # This function generates crop raster status for a given region (state).
  # The status of crops by pixel is based on four categories:
  # 0=not active
  # 1=planting active
  # 2=planting intense
  # 3=harvesting active
  # 4=harvesting intense
  #
  #INPUTS:
  #1) in_filename_raster: input raster file name
  #2) crop_name: name of crop (check in fiel provided)
  #3) crop_status_df: input file containing crop status: 0,1,2,3,4
  #4) algorithm: GDAL or R, use GDAL for larger images
  #5) num_cores: default is 1
  #6) file_format:default value is ".tif"
  #7) out_dir: output dir
  #8) out_suffix: suffix added to filename
  #OUTPUTS
  #An object as a list made of two items:
  #1) 
  #2)
  
  ##### Start script #######
  
  r_region <- raster(file.path(in_dir,in_filename_raster))
  r_val <- r_region
  
  fun <- function(x) { x[x!=val] <- NA; return(x) }
  r_val <- calc(r_val, fun)
  crop_out_filename <- paste0(crop_name,"_",val,file_format)
  writeRaster(r_val,
              filename = file.path(out_dir,crop_out_filename),
              overwrite=TRUE)
  
  ### Now generate for 52 weeks:
  
  j <- 1
  #debug(reclassify_raster)
  
  #test_obj <- reclassify_raster(15,
  #                     crop_status=crop_status_df,
  #                     in_filename=file.path(out_dir,crop_out_filename),
  #                     algorithm="GDAL",
  #                    file_format=file_format)
  
  list_obj <- mclapply(1:52,
                       FUN=reclassify_raster,
                       crop_status=crop_status,
                       in_filename=file.path(out_dir,crop_out_filename),
                       algorithm="GDAL",
                       file_format=file_format,
                       mc.cores = num_cores,
                       mc.preschedule = FALSE)
   browser()
   
   list_obj[[14]]$out_filename
   
   extract_outputs <- function(x){
     # extract output filenames:
     out_filename <- x$out_filename
     gdal_command <- x$gdal_command
     
     if(is.null(out_filename)){
       out_filename <- NA
       gdal_command <- NA
     }
     
     out_df <- data.frame(filename=out_filename,gdal_command=gdal_command)
     return(out_df)
   }
   
   rows_out_df <- lapply(list_obj,FUN=extract_outputs)
   out_df <- do.call(rbind,rows_out_df)
   #View(out_df)
   
   n_col <- ncol(crop_status_df)
   crop_df <- t(crop_status_df[,-c(1,2,3,n_col)])
   out_df$status <- rowSums(crop_df)
   
   barplot(out_df$status,names=1:52)
   
   out_filename_df <- paste0("output_df_",out_suffix,".txt")
   write.table(out_df,out_filename_df)
   
   ### Now prepare to return object
   raster_generation_obj <- list(out_df,list_obj)
   names(raster_generation_obj) <- c("out_df","list_obj")
   
   return(raster_generation_obj)
}

###########################  End of script
