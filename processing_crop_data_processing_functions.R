############### SESYNC Research Support: Agbirds project ########## 
## Processing agricultural crop data by week.
## 
##
## DATE CREATED: 08/03/2018
## DATE MODIFIED: 05/24/2019
## AUTHORS: Benoit Parmentier  
## Version: 1
## PROJECT: Agbirds
## ISSUE: 
## TO DO:
##
## COMMIT: fix bug in generate_multiband and compression
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
  #obj_crop <- recode_crop(crop_type=crop_type[6],data_crop=data_subset)
  
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

reclassify_raster <- function(j,crop_status_df,val,in_filename,algorithm,file_format,data_type=NULL,out_dir=NULL,out_suffix=NULL){
  # This function reclassifies a raster into a specific class given data.frame of input.
  # The goal is to generate one raster for every week in a year.
  #
  #INPUTS:
  #1) j: week considered from 1 to 52
  #2) crop_status_df: input file containing crop status: 0,1,2,3,4
  #3) val: value corresponding to specific crop and to be reclassified
  #4) in_filename:
  #5) algorithm: GDAL or R, use GDAL for larger images
  #6) file_format
  #7) data_type
  #8) out_dir
  #9) out_suffix
  #OUTPUTS
  # 1) obj_out: list made of two components: 
  # out_filename: output file for reclassified crop status
  # gdal_command: gdal command used in the reclassification, NULL if R is used
  
  ######### Begin script ######
  
  if(is.null(out_suffix)){
    out_suffix=""
  }else{
    out_suffix=paste0("_",out_suffix)
  }
  
  col_val <- paste0("X",j) # week
  
  val_to_code <- sum(crop_status_df[[col_val]]) #first value is planting, the other one is havesting
  crop_name_processed <- unique(crop_status_df$Crop)
  
  dataType_table <- generate_raster_dataType_table()
  data_type_gdal <- dataType_table[dataType_table$r_type==data_type,]$gdal_type 
  
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
      
      out_filename <- paste0(region_name,"_",crop_name_processed,"_",val,"_week_",j,out_suffix,file_format)
      if(!is.null(out_dir)){
        out_filename <- file.path(out_dir,out_filename)
      }
      
      gdal_command <- paste0("gdal_calc.py",
                             " -A ",in_filename,
                             " --outfile=",out_filename,
                             paste(" --type=",data_type_gdal,sep=""),
                             " --co='COMPRESS=LZW'",
                             #paste("--NoDataValue=",NA_flag_val,sep=""),
                             " --calc=",paste0("'(",val_to_code,"*(A==",val,"))'"),
                             " --overwrite")
    
      ## Data type:INT1U	
      
      gdal_command
      system(gdal_command)

    }
    
  }else{
    out_filename <- NULL
    gdal_command <- NULL
  }
  
  obj_out <- list(out_filename,gdal_command)
  names(obj_out) <- c("out_filename","gdal_command")
  #class(obj_out) <- append(class(obj_out),"reclassify_cropscape")
  
  return(obj_out)
}

#Function to extract produced raster
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


generate_crop_status_raster <- function(crop_name,
                                        in_filename_raster,
                                        region_name,
                                        crop_status_df,
                                        legend_df,
                                        algorithm,
                                        num_cores,
                                        data_type,
                                        file_format,
                                        out_dir,
                                        out_suffix){
  #
  # CREATED: 10/22/2018
  # MODIFIED: 06/10/2019
  # AUTHORS: Benoit Parmentier
  #
  # This function generates crop raster status for a given region (state).
  # The status of crops by pixel is based on four categories:
  # 0=not active
  # 1=planting active
  # 2=planting intense
  # 3=harvesting active
  # 4=harvesting intense
  #
  # The input layers are from the cropscape project:
  # https://nassgeodata.gmu.edu/CropScape/
  #
  #INPUTS:
  #1) crop_name: name of crop (check in fiel provided)
  #2) in_filename_raster: input raster file name
  #3) region_name: relevant region name (state in this case)
  #4) crop_status_df: input file containing crop status: 0,1,2,3,4
  #5) legend_df: legend from crop raster
  #6) algorithm: GDAL or R, use GDAL for larger images
  #7) num_cores: default is 1
  #8) data_type: raster package data type for the output
  #9) file_format:default value is ".tif"
  #10) out_dir: output dir
  #10) out_suffix: suffix added to filename
  #OUTPUTS
  #Data frame:
  #1) out_df: data.frame containing output raster name and status
  
  ##### Start script #######
  
  crop_status_df <- filter(crop_status_df,Crop==crop_name) %>%
    filter(State==region_name)
  
  #### Genereate specific crop layer
  
  r_region <- raster(in_filename_raster)
  r_val <- r_region
  
  ## crop value in the cropscape product
  val <- legend_df[legend_df$Class_Name==crop_name,]$Value
  
  #### This can be changed to gdal_calc later
  fun <- function(x) { x[x!=val] <- NA; return(x) }
  crop_out_filename <- paste0(crop_name,"_",val,file_format)
  
  r_val <- calc(r_val, 
                fun,
                filename = file.path(out_dir,crop_out_filename),
                datatype=data_type,
                options=c("COMPRESS=LZW"),
                overwrite=TRUE)
  
  #writeRaster(r_val,
  #            filename = file.path(out_dir,crop_out_filename),
  #            datatype=data_type,
  #            options=c("COMPRESS=LZW"),
  #            overwrite=TRUE)
  
  ### Now generate for 52 weeks:
  
  j <- 1
  #debug(reclassify_raster)
  
  
  #test_obj <- reclassify_raster(15,
  #                     crop_status=crop_status_df,
  #                     val=val,
  #                     in_filename=file.path(out_dir,crop_out_filename),
  #                     algorithm="GDAL",
  #                     file_format=file_format,
  #                     data_type= data_type,
  #                     out_dir=out_dir,
  #                     out_suffix=NULL)
  
  if(nrow(crop_status_df)>0){
    list_obj <- mclapply(1:52,
                         FUN=reclassify_raster,
                         crop_status=crop_status_df,
                         val=val,
                         in_filename=file.path(out_dir,crop_out_filename),
                         algorithm="GDAL",
                         file_format=file_format,
                         data_type=data_type,
                         out_dir=out_dir,
                         out_suffix=NULL,
                         mc.cores = num_cores,
                         mc.preschedule = FALSE)
    #browser()
    
    ## in case no rows? should do try-error
    #inherits(list_obj)
    
    rows_out_df <- lapply(list_obj,FUN=extract_outputs)
    out_df <- do.call(rbind,rows_out_df)
    
    #View(out_df)
    
    #Browse[2]> out_df$status <- rowSums(crop_df)
    #Error in `$<-.data.frame`(`*tmp*`, status, value = c(0, 0, 0, 0, 0, 0,  : 
    #                                                        replacement has 52 rows, data has 144
    
    n_col <- ncol(crop_status_df)
    crop_df <- t(crop_status_df[,-c(1,2,3,n_col)])
    out_df$status <- rowSums(crop_df)
    
    out_df$region <- region_name
    out_df$crop_name <- crop_name
    
    out_df$val <- val #crop id in cropscale layer
    
  }else{
    #"filename" "gdal_command" "status" "region" "crop_name" "val"
    #"1" NA NA 0 "Iowa" "Tobacco" 11
    #"2" NA NA 0 "Iowa" "Tobacco" 11
    out_df  <- data.frame(filename=rep(NA,52),
               gdal_command=rep(NA,52),
               region=rep(region_name,52),
               status=rep(NA,52),
               crop_name=rep(crop_name,52),
               val=rep(val,52)
               )
  }
   
   out_filename_df <- paste0("output_df_",region_name,"_",crop_name,"_",out_suffix,".txt")
   write.table(out_df,out_filename_df)
   
   ## This is where you can add the gdal merge info?
   
   return(out_df)
}

extract_from_crop_status_obj <- function(crop_status_obj){
  #crop_status_obj[[1]]
  list_data_out_val <- lapply(1:length(crop_status_obj),function(i){try(crop_status_obj[[i]]$data_out)})
  return(list_data_out_val)
}

remove_duplicates_fun <- function(df,selection_val){
  
  list_index_val <- lapply(1:nrow(selection_val),
                           function(i){which(df$State==selection_val[i,1] & df$Crop==selection_val[i,2])})
  
  list_df_processed <- lapply(1:length(list_index_val),function(i){df[list_index_val[[i]],]})
  ## Drop the last two rows:
  list_df_processed_dropped <- lapply(list_df_processed,function(x){x[1:2,]})
  df_processed_dropped <- do.call(rbind,list_df_processed_dropped)
  
  #remove duplicates
  df <- df[- unlist(list_index_val),]
  df <- rbind(df,df_processed_dropped)
  #add back duplicates:
  
  #list(index_val)
  return(df)
}

generate_raster_dataType_table <- function(){
  #Goal: this function generate a table (data.frame) with data types
  # and valid value range used in the raster package R. The corresponding
  # data type in the GDAL library is provided to allow matching when using
  # GDAL commands.
  
  # Note that we are using the specific data types for tif.
  # The following links provide more information:
  #https://www.gdal.org/frmt_gtiff.html
  #urrently band types of Byte, UInt16, Int16, UInt32, Int32, Float32, 
  #Float64, CInt16, CInt32, CFloat32 and CFloat64 are supported for reading and writing.
  
  ######### Start scripts ################
  
  vals <- c("LOG1S",NA,	FALSE,TRUE, 
            "INT1S",NA,	-127,	127,
            "INT1U","Byte",0, 255,
            "INT2S","Int16",	"-32,767","32,767",
            "INT2U","UInt16",	0,	"65,534",
            "INT4S","int32",	"-2,147,483,647",	"2,147,483,647",
            "INT4U","UInt32",	0,	"4,294,967,296",
            "FLT4S","Float32",	"-3.4e+38",	"3.4e+38",
            "FLT8S","Float64",	"-1.7e+308",	"1.7e+308")
  
  dataType_table <- matrix(vals,nrow=9,ncol=4,byrow=T)
  
  dataType_table <-data.frame(dataType_table)
  
  names(dataType_table) <- c("r_type","gdal_type","min","max")
  ### bug error, columns have become factor: changed this here
  dataType_table <- data.frame(lapply(dataType_table, as.character), stringsAsFactors=FALSE)
  
  #class(dataType_table$gdal_type)
  
  return(dataType_table)
}

## Generate mulitband files from input tif and record band names in description field
generate_multiband <- function(infile_names, band_names, out_filename,
                               python_bin=".",remove_files=F){
  ## Function to merge separate image files in a multiband file
  ##INPUTS
  #1) infile_names: list of files or string/character vector
  #2) band_names: names of bands being aggregated
  #3) out_filename: name of the multiband file created, if tif, input file names are added in the description
  #4) python_bin: path to the add description script used here
  #5) remove_files: if true, removes files after merging into single multiband file
  
  if(is.null(out_filename)){
    out_filename <- paste0("raster_merged.tif")
  }  
  
  #if(!is.null(out_dir)){
  #  out_filename <- file.path(out_dir,out_filename)
  #}
  
  if(file.exists(out_filename)){
    #Delete file if it exists
    file.remove(out_filename)
    
  } 
  
  ## If list/vector of files,then collapse into on unique string 
  if(length(infile_names)>1){
    list_files_vector <- paste(infile_names,collapse = " ")
  }else{
    list_files_vector <- infile_names #assume the format is ok.
  }
  
  #lf=$(lf -v Alabama_C otton_2_week_*.tif)
  #gdal_merg.py -o test_multiband.tif -separate $lf
  
  gdal_command <- paste0("gdal_merge.py",
                         " -o ",out_filename,
                         " -co COMPRESS=LZW",
                         " -separate ",list_files_vector)
  
  gdal_command
  system(gdal_command)
  
  if(length(band_names)>1){
    band_val <- paste(1:length(band_names),shQuote(band_names)) 
    band_val <- paste(band_val,collapse=" ")
  }else{
    band_val <- unlist(strsplit(band_names," "))
    band_val <- paste(1:length(band_val),shQuote(band_val))
    band_val <- paste(band_val,collapse=" ")
  }
  
  
  if(extension(out_filename)==".tif"){
    #"/nfs/bparmentier-data/Data/projects/agbirds-data/scripts/set_band_descriptions.py"
    
    band_description_command <- paste0("python ",
                                       python_bin," ",
                                       out_filename," ", #this is the file to update
                                       band_val)
    system(band_description_command)
  }
  
  if(remove_files==TRUE){
    lapply(infile_names,
           FUN=function(x){file.remove(x)})
  } 
  
  # Prepare output object
  obj_out <- list(out_filename,gdal_command,band_description_command)
  names(obj_out) <- c("out_filename","gdal_command",band_description_command)
  
  return(obj_out)
}

#############################  End of script ####################################
