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
function_tiling <- "generate_spatial_tiles_functions_06152018b.R" #PARAM 1
script_path <- "/nfs/bparmentier-data/Data/projects/climatelandfeedbacks/scripts"
source(file.path(script_path,function_tiling)) #source all functions used in this script 1.

#####  Parameters and argument set up ###########

#ARGS 1
in_dir <- "/nfs/bparmentier-data/Data/projects/climatelandfeedbacks/data"
#ARGS 2
out_dir <- "/nfs/bparmentier-data/Data/projects/climatelandfeedbacks/outputs"
#ARGS 3
NA_flag_val <- NULL
#ARGS 4:
file_format <- ".tif"
#ARGS 5:
scaling_factor <- 0.0001 #MODIFY THE SCALING FACTOR - FOR NORMALIZED DATA SHOULD BE 10,000 AT LEAST
#ARGS 6
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 7
out_suffix <-"test_tile" #output suffix for the files and ouptu folder #PARAM 8
#ARGS 8
num_cores <- 2 # number of cores

#ARGS 9
infile_ref_r <- "/research-home/bparmentier/Data/slurm_test/Exercise_2/data/r_mask_Alaska_11112014.tif" #WWF ecoregions 2001 for Alaska

################# START SCRIPT ###############################

######### PART 0: Set up the output dir ################

options(scipen=999)

#set up the working directory
#Create output directory

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
### PART 1: Generate tiles with method 1 #######

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


#### compare to output from grid generation with sf

#This can be non-overlapping or overlapping

#lf_gimms <- mixedsort(list.files(pattern=file_format,path=in_dir,full.names=T))

#ref_file <- lf_gimms[1]

#######################################
### PART 2: Generate tiles with method 2 #######

##### Generate a grid/tile for processing:
## Must fix the function

debug(generate_grid_tiles)
generate_grid_tiles(ref_file=infile_ref_r,n_tile=4)


############# end of script #############
