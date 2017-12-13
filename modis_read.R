############### SESYNC Research Support: Animals Trade ########## 
## Functions used in the processing of data from google search on species for the animals-trade project at SESYNC.
## 
## DATE CREATED: 12/13/2017
## DATE MODIFIED: 12/11/2017
## AUTHORS: Benoit Parmentier  
## Version: 1
## PROJECT: Hurricane Management
## ISSUE: 
## TO DO:
##
## COMMIT: documenting and testing function to import data sent by google
##
## Links to investigate:

###################################################
#

###### Library used

library(gtools)                              # loading some useful tools 
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(raster)                              # raster functions and spatial utilities
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gdata)                               # various tools with xls reading, cbindX
library(parallel)                            # Parallelization of processes with multiple cores
library(maptools)                            # Tools and functions for sp and other spatial objects e.g. spCbind
library(maps)                                # Tools and data for spatial/geographic objects
library(plyr)                                # Various tools including rbind.fill
library(dplyr)                               # data wrangling
library(rgeos)                               # Geometric, topologic library of functions
library(gridExtra)                           # Combining lattice plots
library(colorRamps)                          # Palette/color ramps for symbology
library(ggplot2)                             # plotting functionality

###### Functions used in this script and sourced from other files

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#Used to load RData object saved within the functions produced.
load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

###### Functions used in this script

functions_time_series_analyses_script <- "time_series_functions_08012017.R" #PARAM 1
functions_processing_data_script <- "processing_data_google_search_time_series_functions_12112017.R" #PARAM 1
functions_time_series_cycles_analyses_script <- "time_series_cycles_analyses_functions_11202017.R" #PARAM 1

############################################################################
#####  Parameters and argument set up ###########

#ARGS 1
in_dir <- "/nfs/teamhurricane-data/Habitat Group/Geospatial_Data/Raw_Data/MODIS"

hdf_file <-"test.hdf"

scaling_factor <- 100 #MODIFY THE SCALING FACTOR - FOR NORMALIZED DATA SHOULD BE 10,000 AT LEAST
#scaling_factor <- 1000 
out_dir <- "/nfs/bparmentier-data/Data/projects/animals_trade/outputs"
#ARGS 7
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 8
out_suffix <-"modis_processing_12132017" #output suffix for the files and ouptut folder #param 12
num_cores <- 2 # number of cores

################# START SCRIPT ###############################

######### PART 0: Set up the output dir ################

options(scipen=999)


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

#######################################
### PART I READ AND PREPARE DATA #######
#set up the working directory
#Create output directory


hdf_file <- file.path(in_dir,hdf_file)
GDALinfo_hdf <- GDALinfo(hdf_file,returnScaleOffset = F)

str(GDALinfo_hdf)
modis_subdataset <- attributes(GDALinfo_hdf)$subdsmdata
print(modis_subdataset)

hdf_df <- (strsplit(modis_subdataset,":"))
#length(modis_subdataset)
hdf_df <- as.data.frame(do.call(rbind,hdf_df),stringsAsFactors=F)
#hdf_df <- data.frame(lapply(hdf_df, as.character))
#hdf_df %>% 
#  mutate_all(as.character)

names(hdf_df) <- c("subdataset_name","description","dir","product","var_name")
#Select automatically QC flag!!
View(hdf_df)

write.table(hdf_df,"hdf_subdataset.txt",sep=",")

modis_subset_layer_Day <- paste("HDF4_EOS:EOS_GRID:",
                                hdf_file,subdataset,sep="")

#NDVI variable
modis_layer_str1 <- unlist(strsplit(modis_subdataset[1],"\""))[3] #Get day NDVI layer
#QC
modis_layer_str2 <- unlist(strsplit(modis_subdataset[5],"\""))[3] #Get day VI QC layer

subdataset <- modis_layer_str1
modis_subset_layer_Day <- paste("HDF4_EOS:EOS_GRID:",hdf_file,subdataset,sep="")

r <-readGDAL(modis_subset_layer_Day)
r  <-raster(r)

plot(r)

####### END OF SCRIPT ###################