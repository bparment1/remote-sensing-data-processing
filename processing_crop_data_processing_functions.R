############### SESYNC Research Support: Agbirds project ########## 
## Processing agricultural crop data by week.
## 
##
## DATE CREATED: 08/03/2018
## DATE MODIFIED: 08/29/2018
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



##################  End of script #########