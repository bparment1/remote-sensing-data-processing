############### SESYNC Research Support: Agbirds project ########## 
## Processing agricultural crop data by week.
## 
##
## DATE CREATED: 09/12/2018
## DATE MODIFIED: 06/05/2019
## AUTHORS: Benoit Parmentier  
## Version: 2
## PROJECT: Agbirds
## ISSUE: 
## TO DO:
##
## COMMIT: fixing output name problem and test over two crops
##

#### Instructions:

# Please find attached the crop table. Within this table each crop type is separated
#by state and condition, either planting or harvesting. 
# Within this table we are hoping to combine the planting and harvesting row 
#for each crop type, by state. Currently planting and harvesting are coded 
#as 0, 1, or 2 for each of the 52 weeks of the year. 
#We want to change the harvesting values of 1 and 2 to 3 and 4, and
#then merge the planting and harvesting rows for each crop and each state. 
#There should not be overlap between the planting and harvesting, but using xtab 
#to find errors will be useful.
# 
# For the next step, we are hoping to convert this table into a spatial dataset. 
#The final product should be 52 rasters (1 for each week) for each crop type 
#(18 crop types listed in table) which includes cell values that represent 
#the crop condition (0,1,2,3,4) which will vary by state.
# 
# To test the following steps, we placed the CDL data for the state of Alabama in the agbirds-data folder under cdl_alabama. We placed a READ ME file with download information if helpful.
# 
# The steps we discussed during our meeting are the following, but we are open to following a different work plan based on your recommendations:
#   1) Clip USDA CDL layer into states
# 
# 2) Extract each crop type from state layers created in step 1 (keep only those crops listed in the table)
# 
# 3) Duplicate crop type into 52 rasters
# 
# 4) Assign crop condition code (0,1,2,3 or 4) to each raster for each crop type (presence of crop)/state
# 
# 5) Merge crops by state into national layer. Final product will be crop layer for U.S.; value within a state will be 0-4 based on crop condition (no action 0, planting active 1, planting intense 2, harvesting active 3, harvesting intense 4).
# 
# 6) Combine all crop layers to have 52 rasters (1 for each week) with cells that have 2 values: crop type and crop condition.
# 
# 7) Create virtual raster table.

#### Note that the current code may be run on the SESYNC HPC cluster by using the 11 input parameters. 
#### The input must be set up to call from the shell and generate array job

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
library(dplyr)

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

#Benoit setup
script_path <- "/nfs/bparmentier-data/Data/projects/agbirds-data/scripts"
crop_data_processing_functions <- "processing_crop_data_processing_functions_06052019b.R"
source(file.path(script_path,crop_data_processing_functions))

############################################################################
#####  Parameters and argument set up ########### 

#ARGS 1
in_dir <- "/nfs/bparmentier-data/Data/projects/agbirds-data/data"
#ARGS 2
out_dir <- "/nfs/bparmentier-data/Data/projects/agbirds-data/outputs"
#ARGS 3:
#NA_flag <- -999999
NA_flag_val <- NULL
#ARGS 4:
file_format <- ".tif"
#ARGS 5:
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 6
out_suffix <-"agbirds_processing_06052019b" #output suffix for the files and ouptut folder
#ARGS 7
num_cores <- 2 # number of cores
#ARGS 8
#in_filename <- "Crop_Data_modified.csv"
in_filename <- "Crop_Data_modified_AD4Benoit.csv" #updated names
#ARGS 9: Change to general file for the whole US
#in_filename_raster <- "cdl_alabama.tif" #this should be the general image for the whole US
in_filename_raster <- "2016_30m_cdls.img"
#ARGS 10
state_val <- "Iowa" #if null should loop through?
#state_val <- c("Alabama","South Dakota, "Nebraska,"Iowa") #should go on a node

#ARGS 11
crop_name <- NULL #if NULL run for all crops in the given state(s)
#crop_name <- "Cotton"
## 
regions_infile <- "cb_2016_us_state_500k.shp" #states
data_type <- "INT1U" #byte data (0,255)

##### Constant:

#in_filename_legend <- "CDL_2017_01.tif.vat.dbf"
in_filename_legend <- "2016_30m_cdls.img.vat.dbf"

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
### PART 1: Read in DATA and crop to area of interest #######

region_name <- state_val

regions_sf <- st_read(file.path(in_dir,regions_infile))
plot(regions_sf$geometry)
#View(regions_sf)
### This is where you crop the cropscape product:
r_cropland <- raster(file.path(in_dir,in_filename_raster))
#infile_reg_outline <- infile_reg_outline_Houston_city_limits
#reg_sf <- st_read(infile_reg_outline)

reg_sf <- st_transform(regions_sf,crs=projection(r_cropland))
reg_sp <-as(reg_sf, "Spatial") 
reg_sp <- reg_sp[reg_sp$NAME==region_name,]

ref_rast_name_generated <- paste("ref_rast_crop_",region_name,"_",out_suffix,file_format,sep="")

ref_rast <- crop(r_cropland,
                 reg_sp,
                 filename=file.path(out_dir,ref_rast_name_generated),
                 overwrite=T)  
#writeRaster(ref_rast,
#            file.path(out_dir,ref_rast_name_generated)
#            )


#### end of crop in

data_df <- read.table(file.path(in_dir,in_filename),
                      sep=",",
                      header=T,
                      stringsAsFactors = F)

### First clean up before setting up the coding:

table(data_df$Plant_Harvest)

data_df$Plant_Harvest[data_df$Plant_Harvest=="Harvest"] <- "Harvesting"
table(data_df$Plant_Harvest)

#View(data_df)
names(data_df)
dim(data_df)
table(data_df$State)

data_in <- data_df
dim(data_in)

#######################################
### PART 2: Screen status crop data #######

### Dealing with duplicate rows
test <- as.data.frame(table(data_in$State,data_in$Crop))
selection_val <- test[test$Freq == 4,]
#df <- data_in 

data_in <- remove_duplicates_fun(data_in,selection_val)
dim(data_in)

#### Fix naming in crops, remove space and /

names(data_in)
crop_val <- (data_in[['Crop']])
crop_val <- as.character(gsub(" ","_",crop_val))
crop_val <- as.character(gsub("\\/","_",crop_val))
table(crop_val)

data_in[['Crop']] <- crop_val

##### test the function:
#undebug(screen_for_crop_status)
state_val
crop_status_obj <- screen_for_crop_status(state_val,data_in)
#crop_status_obj <- screen_for_crop_status(list_states[43],data_in)

### Exploring to recombine values:
length(crop_status_obj)
names(crop_status_obj)

#crop_status_obj$Corn_Grain
#crop_status_obj$Corn_Grain
crop_status_obj[[1]]

### Now you can do this across all the states and have a summary

list_states <- unique(data_in$State)
#note that New England is listed as state: "New England 1/"

list_crop_status_obj <- mclapply(list_states,
         FUN=screen_for_crop_status,
         data_in,
         mc.preschedule = FALSE,
         mc.cores= num_cores)

list_summary_crop <- vector("list",length=length(list_crop_status_obj))

for(i in 1:length(list_crop_status_obj)){
  crop_status_obj <- list_crop_status_obj[[i]]
  
  #list_data_out <- lapply(list_crop_status_obj[[i]],
  #                        function(x){try(lapply(z,function(z){(z$data_out)}),x=z}))
  #undebug(extract_from_crop_status_obj)
  
  list_data_out <- extract_from_crop_status_obj(crop_status_obj)
                       
  #names(list_crop_status_obj[[1]])
  #combine data_out
  data_species_df <- do.call(rbind,list_data_out)
  
  list_summary_crop[[i]]<- data_species_df
}

data_screened_df <- do.call(rbind,list_summary_crop)
dim(data_screened_df)
dim(data_in)

head(data_screened_df)
table(data_screened_df$flag)
sum(is.na(data_screened_df$flag))

write.table(data_screened_df,
            file.path(out_dir,paste0("data_screened_df_",out_suffix,".txt"))
            )

#######################################
### PART 3: Select relevant crops for the region #######

##### raster: test on alabama: will need to subset by 
#state 

legend_df <- read.dbf(file.path(in_dir,in_filename_legend))

#View(test)
legend_df$Class_Name <- as.character(legend_df$Class_Name)
unique(legend_df$Class_Name) #about 132 crops

unique(data_screened_df$Crop) #about 16 crops
### will need to match the names of crop in the legend to the names in the dataset created by the workshop!

common_crop_list <- intersect(unique(data_screened_df$Crop),unique(legend_df$Class_Name))

legend_df_subset <- subset(legend_df,Class_Name%in% common_crop_list)

###############################################
########### PART 4: Now generate raster crop status


if(!is.null(crop_name)){
  
  #undebug(generate_crop_status_raster)
  ## Takes about 10 minutes for Cotton in Alabama
  list_out_df <- generate_crop_status_raster(crop_name,
                                             ref_rast_name_generated,
                                      region_name,
                                      data_screened_df,
                                      legend_df=legend_df_subset,
                                      algorithm,
                                      num_cores,
                                      data_type=data_type,
                                      file_format,
                                      out_dir,
                                      out_suffix)
}else{
  
  i <- 1
  crop_name <- legend_df_subset$Class_Name
  
  #undebug(generate_crop_status_raster)
  # do test for tobaco!!
  
  test <- generate_crop_status_raster(crop_name[i],
                                      ref_rast_name_generated,
                                      region_name,
                                      data_screened_df,
                                      legend_df=legend_df_subset,
                                      algorithm,
                                      num_cores,#set the cores here,
                                      data_type=data_type,
                                      file_format, 
                                      out_dir,
                                      out_suffix)

  list_out_df <- mclapply(crop_name,
                        FUN=generate_crop_status_raster,
                        ref_rast_name_generated,
                        region_name,
                        data_screened_df,
                        legend_df=legend_df_subset,
                        algorithm,
                        num_cores,#set the cores here,
                        data_type=data_type,
                        file_format, 
                        out_dir,
                        out_suffix,
                        mc.cores = 1,
                        mc.preschedule = FALSE)
  
}

### let's report on the output created
names(list_out_df[[1]])
list_out_df[[1]]$crop_name
barplot(list_out_df[[1]]$status,
        names=1:52,
        main=unique(list_out_df[[1]]$crop_name))

out_df <- list_out_df[[1]]

# The status of crops by pixel is based on four categories:
# 0=not active
# 1=planting active
# 2=planting intense
# 3=harvesting active
# 4=harvesting intense

###############################################
########### PART 5: Mutliband merging of bands and adding description

## add function:
infile_names <- as.character(na.omit(list_out_df[[2]]$filename))
#list_infile_names <- lapply(list_out_df,FUN=function(x){x$filename})
list_infile_names <- lapply(list_out_df,FUN=function(x){as.character(na.omit(x$filename))})
error_list <- unlist(lapply(list_infile_names,FUN=function(x){length(x)>0}))
list_infile_names <- list_infile_names[error_list]  
list_band_names <- lapply(list_infile_names,
                          FUN=function(x){gsub(extension(basename(x)),"",basename(x))})

#fun_test <- function(x){gsub(extension(basename(x)),"",basename(x))}
#fun_test(list_infile_names[[3]])
list_out_filename <- paste0(crop_name[error_list],"_",region_name,file_format)
list_infile_names <- unlist(lapply(list_infile_names,function(x){list_files_vector <- paste(x,collapse = " ")}))
list_band_names <- unlist(lapply(list_band_names,function(x){list_files_vector <- paste(x,collapse = " ")}))

out_filename <- "Cotton_test.tif"
band_names <- (basename(infile_names))
band_names <- gsub(extension(band_names),"",band_names)
python_bin <- "/nfs/bparmentier-data/Data/projects/agbirds-data/scripts/set_band_descriptions.py"

### Need to change data type from Float32 to byte!!!
### Also need to compress.
### Add dates??


data_inputs_df <- data.frame(infile_name=list_infile_names,
                             band_names=list_band_names,
                             out_filename=list_out_filename,
                             python_bin = python_bin)

data_inputs_df$python_bin <- as.character(data_inputs_df$python_bin)
data_inputs_df$out_filename <- as.character(data_inputs_df$out_filename)
data_inputs_df$band_names <- as.character(data_inputs_df$band_names)
data_inputs_df$infile_name <- as.character(data_inputs_df$infile_name)


#debug(generate_multiband)
#i<-2
test_out <- generate_multiband(infile_names = data_inputs_df$infile_name[i], 
                               band_names = data_inputs_df$band_names[i], 
                               out_filename = data_inputs_df$out_filename[i],
                               python_bin=data_inputs_df$python_bin[i])


#r_brick <- brick(test_out$out_filename)
#plot(r_brick)

list_merged_crop_files <- mcmapply(generate_multiband,
                                   infile_name = data_inputs_df$infile_name,
                                   band_names = data_inputs_df$band_names, 
                                   out_filename = data_inputs_df$out_filename,
                                   python_bin = data_inputs_df$python_bin,
                                   mc.cores = 3,
                                   mc.preschedule = FALSE)


#####################  End of script ###############################