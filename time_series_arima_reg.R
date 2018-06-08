####################################    Time Series Analyses  #######################################
############################  ARIMA and other time series methods  #######################################
#This script functions to produce ARIMA predictions for raster time series stack.       
#AUTHORS: Benoit Parmentier                                             
#DATE CREATED: 03/09/2014 
#DATE MODIFIED: 06/08/2018
#Version: 2
#PROJECT: GLP Conference Berlin,YUCATAN CASE STUDY with Marco Millones            
#PROJECT: Workshop for William and Mary: an intro to spatial regression with R 
#PROJECT: Geocomputation and AAG 2015
#PROJECT: Space beats time project
#PROJECT: SESYNC Research support

#TO DO:
# modify the rasterize_df_fun function to allow ref image
# add the ARIMA method to run more efficiently
#
#COMMIT: testing changes to arima and documentation
#
#################################################################################################

###Loading R library and packages                                                      

library(sp)
library(rgdal)
library(spdep)
library(gtools)
library(maptools)
library(parallel)
library(rasterVis)
library(raster)
library(forecast) #ARIMA forecasting
library(xts)
library(zoo)
library(lubridate)
library(colorRamps) #contains matlab.like color palette
library(rgeos)
library(sphet) #contains spreg
library(BMS) #contains hex2bin and bin2hex
library(bitops)

### Other functions ####

function_time_series_arima <- "time_series_arima_reg_functions_06082018.R" #PARAM 1
script_path <- "/home/bparmentier/z_drive/Data/projects/climatelandfeedbacks/scripts/" #path to script #PARAM 
source(file.path(script_path,function_time_series_arima)) #source all functions used in this script 1.


############################################################################
#####  Parameters and argument set up ###########

out_suffix <- "ts_arima_test_06082018" #output suffix for the files and ouptut folder #param 12

in_dir <- "~/z_drive/Data/projects/climatelandfeedbacks/data"
out_dir <- "~/z_drive/Data/projects/climatelandfeedbacks/outputs"

file_format <- ".tif" #PARAM5
NA_flag_val <- -9999 #PARAM7
create_out_dir_param=TRUE #PARAM9

### param

############## START SCRIPT ############################

######### PART 0: Set up the output dir ################

if(is.null(out_dir)){
  out_dir <- in_dir #output will be created in the input dir
}
#out_dir <- in_dir #output will be created in the input dir

out_suffix_s <- out_suffix #can modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}


###################### PART 1: ###########


#agg_5_r_NDVI_261_NDVI_Dean_06062018.tif
in_dir_NDVI <- "~/z_drive/Data/projects/climatelandfeedbacks/data/yucatan_NDVI"

lf_NDVI <- mixedsort(list.files(pattern="agg_5_r_NDVI_.*._NDVI_Dean_06062018.tif",
                                path="~/z_drive/Data/projects/climatelandfeedbacks/data/yucatan_NDVI",
                     full.names=T))
r_stack <- stack(lf_NDVI)

#testing arima on aggregated data


################# End of script #####################