############### Spatial utility: General code for generating tiles/spatial subsets for AREA  ########## 
## 
## DATE CREATED: 06/08/2017
## DATE MODIFIED: 06/08/2018
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

#ARGS 4
infile_ref_r <- "/research-home/bparmentier/Data/slurm_test/Exercise_2/data/r_mask_Alaska_11112014.tif" #WWF ecoregions 2001 for Alaska

#ARGS 6
NA_value <- -9999 #PARAM6
#ARGS 7
out_suffix <-"test_tile" #output suffix for the files and ouptu folder #PARAM 8
#ARGS 8

r_ref <- raster(infile_ref_r)
plot(r_ref)

reg_outline_sp <- create_polygon_from_extent(reg_ref_rast=r_ref,outDir=NULL,outSuffix=NULL)
plot(reg_outline_sp,add=T,border="red")

reg_centroid <- gCentroid(reg_outline_sp)

reg_extent <- extent( reg_outline_sp) #get boudning box of extent

#Now make this an extent and a polygon, then shift the polygon on the right by half

xmin_new <- xmin(reg_extent)
xmax_new <- xmin(reg_extent) + ((xmax(reg_extent)- xmin(reg_extent))/2)
ymin_new <- ymax(reg_extent)-((ymax(reg_extent)- ymin(reg_extent))/2)
ymax_new <- ymax(reg_extent)

range_e_x <- ((xmax(reg_extent)- xmin(reg_extent))/2)
range_e_y <- ((ymax(reg_extent)- ymin(reg_extent))/2)

#xmin,xmax,ymin,ymax
tile_1_e <- extent(xmin_new,xmax_new,ymin_new,ymax_new)
tile_1_extent_sp <- as(tile_1_e, "SpatialPolygons") #coerce raster extent object to SpatialPolygons from sp package 
tile_1_extent_spdf <- as(tile_1_extent_sp, "SpatialPolygonsDataFrame") #promote to spdf
proj4string(tile_1_extent_sp) <- projection(r_ref) #Assign projection to spdf


tile_2_extent_spdf <- shift(tile_1_extent_spdf,x=range_e_x,y=0)
tile_3_extent_spdf <- shift(tile_1_extent_spdf,x=0,y= -range_e_y)
tile_4_extent_spdf <- shift(tile_1_extent_spdf,x=range_e_x,y=-range_e_y)

plot(tile_1_extent_spdf,add=T,border="green")

plot(tile_3_extent_spdf,add=T,border="blue")
plot(tile_4_extent_spdf,add=T,border="pink")

list_tiles_spdf<- list(tile_1_extent_spdf,
                       tile_2_extent_spdf,
                       tile_3_extent_spdf,
                       tile_4_extent_spdf)

out_suffix <- "alaska"
list_tiles_names <- paste("tile_",1:length(list_tiles_spdf),"_",out_suffix,".shp",sep="")

out_dir <- "."
## Save the tiles
for(i in 1:length(list_tiles_names)){
  infile_reg_outline <- list_tiles_names[i]
  tile_spdf <- list_tiles_spdf[[i]]
  writeOGR(tile_spdf,dsn= out_dir,layer= sub(".shp","",infile_reg_outline), 
           driver="ESRI Shapefile",overwrite_layer="TRUE")
}


############# end of script #############
