############### SESYNC Research Support: Hurricane Management ########## 
## Functions taken from earlier processing raster images.
##
## 
## DATE CREATED: 12/13/2017
## DATE MODIFIED: 12/14/2017
## AUTHORS: Benoit Parmentier  
## Version: 1
## PROJECT: Hurricane Management
## ISSUE: 
## TO DO:
##
## COMMIT: 
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


##### Functions available:

#import_modis_layer_fun: convert hdf to other type e.g. tif for a given dataset


import_modis_layer_fun <-function(hdf_file,subdataset,NA_flag,out_rast_name="test.tif",memory=TRUE){
  
  #PARSE input arguments/parameters
  
  modis_subset_layer_Day <- paste("HDF4_EOS:EOS_GRID:",hdf_file,subdataset,sep="")
  r <-readGDAL(modis_subset_layer_Day)
  r  <-raster(r)
  
  if(memory==TRUE){
    return(r)
  }else{
    #Finish this part...write out
    raster_name<- out_rast_name
    writeRaster(r_spat, NAflag=NA_flag_val,filename=raster_name,bylayer=TRUE,bandorder="BSQ",overwrite=TRUE)       
    return(raster_name)
  }  
}

############ END OF SCRIPT  ###########################