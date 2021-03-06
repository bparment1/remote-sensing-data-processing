############### SESYNC Research Support: Agbirds project ########## 
## Processing agricultural crop data by week.
## 
##
## DATE CREATED: 08/03/2018
## DATE MODIFIED: 09/12/2018
## AUTHORS: Benoit Parmentier  
## Version: 1
## PROJECT: Agbirds
## ISSUE: 
## TO DO:
##
## COMMIT: clean up and recoding 
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

crop_data_processing_functions <- "processing_crop_data_processing_functions_09072018.R"
source(file.path(script_path,crop_data_processing_functions))

#########cd ###################################################################
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
#ARGS 7
out_suffix <-"agbirds_processing_09122018" #output suffix for the files and ouptut folder
#ARGS 8
num_cores <- 2 # number of cores
#ARGS 9
#date_param <- "1982.01.01;1982.12.31" #start date, end date

in_filename <- "Crop_Data_modified.csv"

in_filename_raster <- "cdl_alabama.tif"

#state_val <- "Alabama"
state_val <- "California"

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
### PART 1: Read in DATA #######


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
#state_val <- "California"
dim(data_in)

##### test the function:
#debug(screen_for_crop_status)
state_val
list_crop_status_obj <- screen_for_crop_status(state_val,data_in)
  
### Exploring to recombine values:
length(list_crop_status_obj)
names(list_crop_status_obj)

list_crop_status_obj$Spring_Barley[[1]]
list_crop_status_obj$Spring_Barley
list_crop_status_obj$Spring_Barley$data_out

list_data_out <- lapply(list_crop_status_obj,function(x){x$data_out})
#combine data_out
data_species_df <- do.call(rbind,list_data_out)

dim(data_species_df)
#View(data_species_df)

### Now you can do this across all the state and have a summary

list_states <- unique(data_in$State)

list_crop_status_obj <- mclapply(list_states,
         FUN=screen_for_crop_status,
         data_in,
         mc.preschedule = FALSE,
         mc.cores= num_cores)

#undebug(screen_for_crop_status)
#test <- screen_for_crop_status(list_states[21],data_in)
#### summarize results:

### Still getting error here!!!

list_summary_crop <- vector("list",length=length(list_crop_status_obj))
for(i in 1:length(list_crop_status_obj)){
  list_data_out <- lapply(list_crop_status_obj[[i]],function(x){x$data_out})
  names(list_crop_status_obj[[1]])
  #combine data_out
  data_species_df <- do.call(rbind,list_data_out)
  
  list_summary_crop[[i]]<- data_species_df
}

data_screened_df <- do.call(rbind,list_summary_crop)

dim(data_screened_df)
dim(data_screened_df)
dim(data_in)

View(data_screened_df)

table(data_screened_df$flag)
sum(is.na(data_screened_df$flag))

write.table(data_screened_df,
            paste0("data_screened_df_",out_suffix,".txt")
            )

subset(data_screened_df,flag==1)[,1:6]

##################  End of script #########