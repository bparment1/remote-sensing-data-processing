############### Spatial utility: General code for mosaicing tiles/spatial subsets for AREA  ########## 
## 
## DATE CREATED: 06/05/2018
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
mosaic_m_raster_list<-function(j,list_param){
  #This functions returns a subset of tiles from the modis grid.
  #Arguments: modies grid tile,list of tiles
  #Output: spatial grid data frame of the subset of tiles
  #Note that rasters are assumed to be in the same projection system!!
  
  #rast_list<-vector("list",length(mosaic_list))
  #for (i in 1:length(mosaic_list)){  
  # read the individual rasters into a list of RasterLayer objects
  # this may be changed so that it is not read in the memory!!!
  
  #parse output...
  
  #j<-list_param$j
  mosaic_list<-list_param$mosaic_list
  out_path<-list_param$out_path
  out_names<-list_param$out_rastnames
  file_format <- list_param$file_format
  NA_flag_val <- list_param$NA_flag_val
  ## Start
  
  input.rasters <- lapply(as.character(mosaic_list[[j]]), raster)
  mosaiced_rast<-input.rasters[[1]]
  
  for (k in 2:length(input.rasters)){
    mosaiced_rast<-mosaic(mosaiced_rast,input.rasters[[k]], fun=mean)
    #mosaiced_rast<-mosaic(mosaiced_rast,raster(input.rasters[[k]]), fun=mean)
  }
  
  data_name<-paste("mosaiced_",sep="") #can add more later...
  #raster_name<-paste(data_name,out_names[j],".tif", sep="")
  raster_name<-paste(data_name,out_names[j],file_format, sep="")
  
  writeRaster(mosaiced_rast, NAflag=NA_flag_val,filename=file.path(out_path,raster_name),overwrite=TRUE)  
  #Writing the data in a raster file format...  
  rast_list<-file.path(out_path,raster_name)
  
  ## The Raster and rgdal packages write temporary files on the disk when memory is an issue. This can potential build up
  ## in long  loops and can fill up hard drives resulting in errors. The following  sections removes these files 
  ## as they are created in the loop. This code section  can be transformed into a "clean-up function later on
  ## Start remove
  tempfiles<-list.files(tempdir(),full.names=T) #GDAL transient files are not removed
  files_to_remove<-grep(out_suffix,tempfiles,value=T) #list files to remove
  if(length(files_to_remove)>0){
    file.remove(files_to_remove)
  }
  #now remove temp files from raster package located in rasterTmpDir
  removeTmpFiles(h=0) #did not work if h is not set to 0
  ## end of remove section
  
  return(rast_list)
}

mosaic_m_raster_list<-function(j,list_param){
  #This functions returns a subset of tiles from the modis grid.
  #Arguments: modies grid tile,list of tiles
  #Output: spatial grid data frame of the subset of tiles
  #Note that rasters are assumed to be in the same projection system!!
  
  #rast_list<-vector("list",length(mosaic_list))
  #for (i in 1:length(mosaic_list)){  
  # read the individual rasters into a list of RasterLayer objects
  # this may be changed so that it is not read in the memory!!!
  
  ## CREATED ON: 09/16/2013
  ## MODIFIED ON: 03/01/2018
  ## AUTHOR: Benoit Parmentier
  ##
  ## INPUTS
  # 1)
  # 2)
  # 3)
  ## OUTPUTS
  # 1)
  #
  
  #### Start function ####
  
  ### Step 1: Parse inputs
  
  #j<-list_param$j
  mosaic_list<-list_param$mosaic_list
  out_dir<-list_param$out_dir
  out_suffix <- list_param$out_suffix
  out_names<-list_param$out_rastnames
  file_format <- list_param$file_format
  NA_flag_val <- list_param$NA_flag_val
  multiband <- list_param$multiband
  
  
  ### Step 2: Mosaic lists of files
  
  ## Check input to see if this is a multiband file
  r_in <- brick(as.character(mosaic_list[[j]])[1])
  n_layer <- nlayers(r_in)
  
  if(n_layer>1){
    input.rasters <- lapply(as.character(mosaic_list[[j]]), brick)
  }else{
    input.rasters <- lapply(as.character(mosaic_list[[j]]), raster)
  }
  
  mosaiced_rast <- input.rasters[[1]]
  
  for (k in 2:length(input.rasters)){
    mosaiced_rast <- mosaic(mosaiced_rast,input.rasters[[k]], fun=mean)
    #mosaiced_rast<-mosaic(mosaiced_rast,raster(input.rasters[[k]]), fun=mean)
  }
  
  #### Step 3: Write out images and return values
  
  data_name<-paste("mosaiced_",sep="") #can add more later...
  #raster_name<-paste(data_name,out_names[j],".tif", sep="")
  #raster_name<-paste(data_name,out_names[j],file_format, sep="")
  raster_name<-paste(data_name,out_names[j], sep="") #don't add file format here
  
  #raster_name <- basename(sub(extension(rast_name_var),"",rast_name_var))
  #raster_name <- paste(raster_name,"_",out_suffix,extension(rast_name_var),sep="")
  
  #### change to compress if format is tif!! add this later...
  #file_format <- extension(mosaic_list[[j]])[1] #assumes that all inputs have the same file type, take first
  
  #Write out as brick
  data_type_str <- dataType(mosaiced_rast) #find the dataType, this should be a future input param
  
  if(is.null(NA_flag_val)){
    NA_flag_val <- NAvalue(mosaiced_rast)
  }
  
  #browser()
  
  if(n_layer>1){
    suffix_str <- 1:n_layer
    if(out_suffix!=""){
      suffix_str <- paste(out_suffix,suffix_str,sep="_")
    }
    #if not, don't add out_sufffix
    
    if(multiband==TRUE){
      #raster_name_tmp <- basename(rast_name_var)
      #raster_name <- basename(sub(file_format,"",raster_name))
      if(out_suffix!=""){
        raster_name_tmp <- paste(raster_name,"_",out_suffix,file_format,sep="")
      }else{
        raster_name_tmp <- paste(raster_name,file_format,sep="")
      }
      bylayer_val <- FALSE #don't write out separate layer files for each "band"
      rast_list <- file.path(out_dir,raster_name_tmp) #as return from function
    }
    if(multiband==FALSE){
      raster_name_tmp <- paste(raster_name,file_format,sep="") #don't add output suffix because in suffix_str
      bylayer_val <- TRUE #write out separate layer files for each "band"
      rast_list <- file.path(out_dir,(paste(raster_name,"_",suffix_str,file_format,sep=""))) 
    }
    
    if(file_format==".tif"){
      #Use compression option for tif
      writeRaster(mosaiced_rast,
                  filename=file.path(out_dir,raster_name_tmp),
                  bylayer=bylayer_val,
                  suffix=suffix_str,
                  overwrite=TRUE,
                  NAflag=NA_flag_val,
                  datatype=data_type_str,
                  options=c("COMPRESS=LZW"))
    }else{
      #Don't use compression option if not tif
      writeRaster(mosaiced_rast,
                  filename=file.path(out_dir,raster_name_tmp),
                  bylayer=multiband,
                  suffix=suffix_str,
                  overwrite=TRUE,
                  NAflag=NA_flag_val,
                  datatype=data_type_str)
    }
    
  }
  
  if(n_layer==1){
    #raster_name_tmp <- basename(rast_name_var)
    #raster_name <- basename(sub(extension(rast_name_var),"",rast_name_var))
    raster_name_tmp <- paste(raster_name,"_",out_suffix,file_format,sep="")
    writeRaster(mosaiced_rast, 
                NAflag=NA_flag_val,
                filename=file.path(out_dir,raster_name_tmp),
                bylayer=FALSE,
                bandorder="BSQ",
                datatype=data_type_str,
                overwrite=TRUE)
    rast_list <- file.path(out_dir,raster_name_tmp)
  }
  
  ## The Raster and rgdal packages write temporary files on the disk when memory is an issue. This can potential build up
  ## in long  loops and can fill up hard drives resulting in errors. The following  sections removes these files 
  ## as they are created in the loop. This code section  can be transformed into a "clean-up function later on
  ## Start remove
  tempfiles <- list.files(tempdir(),full.names=T) #GDAL transient files are not removed
  files_to_remove<-grep(out_suffix,tempfiles,value=T) #list files to remove
  if(length(files_to_remove)>0){
    file.remove(files_to_remove)
  }
  #now remove temp files from raster package located in rasterTmpDir
  removeTmpFiles(h=0) #did not work if h is not set to 0
  ## end of remove section
  
  return(rast_list)
}

############# end of script #############
