############### Spatial utility: General code for generating tiles/spatial subsets for AREA  ########## 
## 
## DATE CREATED: 06/08/2017
## DATE MODIFIED: 06/15/2018
## AUTHORS: Benoit Parmentier 
## Version: 1
## PROJECT: General purpose
## ISSUE: 
## TO DO: Make this a function later
##
## COMMIT: initial commit
##
## Links to investigate:
#
###################################################
#

###### Library used

library(gtools)                              # loading some useful tools 
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gdata)                               # various tools with xls reading, cbindX
library(rasterVis)                           # Raster plotting functions
library(parallel)                            # Parallelization of processes with multiple cores
library(maptools)                            # Tools and functions for sp and other spatial objects e.g. spCbind
library(maps)                                # Tools and data for spatial/geographic objects
library(plyr)                                # Various tools including rbind.fill
library(spgwr)                               # GWR method
library(rgeos)                               # Geometric, topologic library of functions
library(gridExtra)                           # Combining lattice plots
library(colorRamps)                          # Palette/color ramps for symbology
library(ggplot2)

###### Functions used in this script

#This function creates a spatial polygon data frame object for the extent matching a raster input
create_polygon_from_extent<-function(reg_ref_rast,outDir=NULL,outSuffix=NULL){
  #This functions returns polygon sp from input rast
  #Input Arguments: 
  #reg_ref_rast: input ref rast
  #outDir : output directory, if NULL then the current dir in used
  #outSuffix: output suffix used for the naming of the shapefile
  #Output: 
  #reg_outline_poly: spatial polygon data.frame
  #
  if(is.null(outDir)){
    outDir=getwd()
  }
  if(is.null(outSuffix)){
    outSuffix=""
  }
  ref_e <- extent(reg_ref_rast) #extract extent from raster object
  reg_outline_poly <- as(ref_e, "SpatialPolygons") #coerce raster extent object to SpatialPolygons from sp package 
  reg_outline_poly <- as(reg_outline_poly, "SpatialPolygonsDataFrame") #promote to spdf
  proj4string(reg_outline_poly) <- projection(reg_ref_rast) #Assign projection to spdf
  infile_reg_outline <- paste("reg_out_line_",out_suffix,".shp",sep="") #name of newly crated shapefile with the extent
  writeOGR(reg_outline_poly,dsn= outDir,layer= sub(".shp","",infile_reg_outline), 
           driver="ESRI Shapefile",overwrite_layer="TRUE")
  
  return(reg_outline_poly) #return spdf
}

generate_grid_tiles <- function(ref_file,n_tile,n_tile_x, n_tile_y,out_suffix,out_dir){
  
  r <- raster(ref_file)
  
  # for the time being generate a non-overlapping grid tiling and crop
  extent_val <- extent(r)
  bbox_val <- st_bbox(r)
  test_sp <- as(extent_val, 'SpatialPolygons')
  outline_sf <-as(test_sp,"sf")
  
  #Can buffer?
  
  #test_grid <- st_make_grid(outline_sf, n=18)
  test_grid <- st_make_grid(outline_sf, n=n_tile)
  
  plot(r)
  plot(test_grid,add=T)
  plot(test_grid[56],add=T,col="red")
  
  out_grid_filename <- file.path(out_dir,"test_grid.shp")
  st_write(test_grid,dsn=out_grid_filename)
  
  #Generate overlapping grid option to come later
  
  return(test_grid)
}

########################### end of script #########################
