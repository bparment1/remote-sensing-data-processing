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

#This script currently contains 7 functions:

#Add arima functions
#[9] "convert_arima_pred_to_raster"         
#[10] "extract_arima_mod_info"              
#[11] "pixel_ts_arima_predict"              
#[12] "raster_NA_image"                     
#[13] "raster_ts_arima"                      
#[14] "raster_ts_arima_predict"                         

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



create_dir_fun <- function(out_dir,out_suffix){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    out_dir <- file.path(out_dir,out_name)
  }
  #create if does not exists
  if(!file.exists(out_dir)){
    dir.create(out_dir)
  }
  return(out_dir)
}

load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

###ARIMA RELATED FUNCTIONS...need to be improved a lot!!

raster_ts_arima<-function(pixel,na.rm=T,arima_order){
  arima_obj<-arima(pixel,order=arima_order)
  a<-as.numeric(coef(arima_obj)[1]) 
  return(a)
}

#This takes a pixel time series ... extracted from a stack

raster_ts_arima_predict <- function(pixel,na.rm=T,arima_order=NULL,n_ahead=2){
  if(is.null(arima_order)){
    arima_mod <- auto.arima(pixel)
    p_arima<- try(predict(arima_mod,n.ahead=n_ahead))
  }else{
    arima_mod<-arima(pixel,order=arima_order)
    p_arima<- try(predict(arima_mod,n.ahead=n_ahead))
  }
  if (!inherits(p_arima,"try-error")){
    y<- t(as.data.frame(p_arima$pred)) #this makes a matrix...should probably be a data.frame
    y_error <- t(as.data.frame(p_arima$se)) #this contains the standard errrors related to the value predicted by arima
    #y_error <- rep(0,n_ahead)
  }
  if (inherits(p_arima,"try-error")){
    y<- rep(NA,n_ahead)
    y_error <- rep(1,n_ahead)
  }
  pred_obj <- list(y,y_error)
  names(pred_obj)<-c("pred","error")
                                  
  return(pred_obj)
}

#This is the main function!!!

pixel_ts_arima_predict <- function(i,list_param){
  # This function fits an ARIMA model and generate predictions
  # This is done using one single time series extracted pixels by pixels.
  #
  #
  
  ###### Start function
  
  ## Extract parameters
  
  pixel <-list_param$pix_val[i]
  arima_order <-list_param$arima_order
  n_ahead <- list_param$n_ahead
  out_dir <- list_param$out_dir
  out_suffix <- list_param$out_suffix
  pixel_xy <- try(list_param$df_xy[i,])
  na.rm=T
  
  #####
  # Fit and predict

  if(is.null(arima_order)){
    arima_mod <- try(auto.arima(pixel))
    p_arima<- try(predict(arima_mod,n.ahead=n_ahead))
  }else{
    arima_mod<- try(arima(pixel,order=arima_order))
    p_arima<- try(predict(arima_mod,n.ahead=n_ahead))
  }
  if (!inherits(p_arima,"try-error")){
    y <- t(as.data.frame(p_arima$pred)) #this makes a matrix...should probably be a data.frame
    y_error <- t(as.data.frame(p_arima$se)) #this contains the standard errrors related to the value predicted by arima
    #y_error <- rep(0,n_ahead)
  }
  if (inherits(p_arima,"try-error")){
    y <- rep(NA,n_ahead)
    #y_error <- rep(1,n_ahead)
    y_error <- rep(NA,n_ahead)
  }
  
  arima_mod_filename <- file.path(out_dir,paste("arima_mod_","pixel_",i,"_",out_suffix,".RData",sep=""))
  
  save(arima_mod,file=arima_mod_filename)             
  
  ## Prepare object to return...
  #note that the arima mod object could be included but is not at the time being
  #if pixel time series is geographically referenced
  #if(!is.null(pixel_xy)){
  if(!inherits(p_arima,"try-error")){
    pred_obj <- list(y,y_error,arima_mod_filename,pixel_xy)
    names(pred_obj)<-c("pred","error","arima_mod_filename","pixel_xy")
  }else{
    pred_obj <- list(y,y_error,arima_mod_filename,pixel_xy)
    names(pred_obj)<-c("pred","error","arima_mod_filename","pixel_xy")
  }
  
  return(pred_obj)
}

raster_NA_image <- function(r_stack){
  list_r_NA <- vector("list",length=nlayers(r_stack))
  for (i in 1:nlayers(r_stack)){
    r <- subset(r_stack,i)
    r_NA <- is.na(r)
    list_r_NA[[i]] <- r_NA
  }
  return(list_r_NA)
}

convert_arima_pred_to_raster <- function(i,list_param){
  #This function produces a raster image from ariam pred obj
  #Read in the parameters...
  r_ref <-list_param$r_ref
  ttx <- list_param$ttx
  file_format <- list_param$file_format
  out_dir <-list_param$out_dir
  out_suffix <- list_param$out_suffix
  out_rastname <-list_param$out_rastnames[i]
  file_format <- list_param$file_format
  NA_flag_val <- list_param$NA_flag_val
  
  #start script
  #pred_t <- lapply(ttx,FUN=function(x){x$pred[i]})
  #error_t <- lapply(ttx,FUN=function(x){x$error[i]})
  
  l_r <-vector("list", length=2)
  l_r[[1]]<-lapply(ttx,FUN=function(x){x$pred[i]})
  l_r[[2]] <- lapply(ttx,FUN=function(x){x$error[i]})
  
  for (j in 1:2){
    tt_dat <- do.call(rbind,l_r[[j]])
    tt_dat <- as.data.frame(tt_dat)
    pred_t <-as(r_ref,"SpatialPointsDataFrame")
    pred_t <- as.data.frame(pred_t)
    pred_t <- cbind(pred_t,tt_dat)
    coordinates(pred_t) <- cbind(pred_t$x,pred_t$y)
    raster_pred <- rasterize(pred_t,r_ref,"V1",fun=mean)
    l_r[[j]] <- raster_pred
  }
  
  #tmp_name <- extension(out_rastname)
  #modify output name to for error image
  tmp_name <- unlist(strsplit(out_rastname,"_"))
  nb<- length(tmp_name)
  tmp_name <-paste(paste(tmp_name[1:(nb-1)],collapse="_"),
             "error",tmp_name[nb],sep="_")
  writeRaster( l_r[[2]],NAflag=NA_flag_val,
              filename=file.path(out_dir,tmp_name),
              overwrite=TRUE)  
  writeRaster( l_r[[1]],NAflag=NA_flag_val,
              filename=file.path(out_dir,out_rastname),
              overwrite=TRUE)  
  return(list(out_rastname,tmp_name))
}

extract_arima_mod_info <- function(i,list_param){
  fname <- list_param$arima_mod_name[i]
  arima_mod <- load_obj(fname)
  #summary(arima_mod)
  #coef(arima_mod)
  arima_specification <- arima_mod$arma
  arima_coef <-  coef(arima_mod)
  #http://stackoverflow.com/questions/19483952/how-to-extract-integration-order-d-from-auto-arima
  #a$arma[length(a$arma)-1] is the order d
  #[1] 2 0 0 0 1 0 0
  #A compact form of the specification, as a vector giving the number of AR (1), MA (2), 
  #seasonal AR (3) and seasonal MA coefficients (4), 
  #plus the period (5) and the number of non-seasonal (6) and seasonal differences (7).
  
  return(list(arima_specification,arima_coef))
} 

#Still in process, apply function pixel by pixel in a block read from disk for stack or brick raster
#not used here because we loose the pixel id for the object for ARIMA, this needs to be work out at some point!!!

#    readBloackRaster <- function(r_var,r_mask,bs_read=NULL,out_rast_fname=NULL,pixelFun=NULL,list_param=NULL){
#      
#      colNo <- ncol(r_var) #number of column
#      rowNo <- nrow(r_var)
#      #out <- raster(x,1)
#      #bs <- blockSize(out)#
#      if(is.null(bs_read)){
#        bs <- blockSize(r_var)
#        bsno <- bs$n #number of lbocks to read
#      }
#      #bs <- blockSize(r_var)
#      if(!is.null(out_rast_fname)){
#        out_rast <- writeStart(out_rast_fname, out_rast, overwrite=TRUE)
#      }
#      
#      for (i in 1:bsno){
#        v <- getValuesBlock(r_var, 
#                           row=bs$row[i], #starting row
#                           nrows=bs$nrows, 
#                           col=1, 
#                           ncols=colNo)#, 
#                           #format='')
#        v_out <- v
#        if(!is.null(pixFun)){
#          v_out <- lapply(1:nrow(v),FUN=pixelFun,list_param=list_param) #assumes that this is one value per pixel
#          out_rast <- writeValues(out_rast, v_out, bs$row[i])
#
#          }
#        }  
#      return(v_out)
#    }


#Predict using time model
predict_temp_reg_fun <-function(i,list_param){
  #####
  #This function gnerates a prediction based on time dimension and neighbour.
  ##Inputs are raster stack with different options for regression estimators:  ARIMA
  #####
  ## Date created: 03/09/2014
  ## Date modified: 06/08/2018
  # Authors: Benoit Parmentier
  #
  #INPUTS:
  #1) out_dir: path to output directory
  #2) r_ref_s: raster stack containing data to predict
  #3) r_clip: raster image to use for clipping raster stack
  #4) proj_str: projection and reference system information (in PROJ4 format)
  #5) list_models: model formula, this is not currently in use
  #6) out_suffix: output suffix 
  #7) file_format: ouptut raste file format, default is .tif
  #8) estimator: general type of estimator e.g. mle, ols etc.
  #9) estimation_method: algorithm type or method used 
  #10)  NA_flag_val: NA value for raster
  #Arima specific parameters:
  #11) num_cores: number of cor used in the processing
  #12) time_step: this is the time step for which to start the arima model with
  #13) n_pred_ahead: limit to the prediction of arima in the future horizon in time steps 
  #14) r_stack: raster stack with observations 
  #15) arima_order: model used in ARIMA
  
  #OUTPUTS
  #Object made of a list of elements:
  #1) temp_mod: model object, may vary of structure according to the method use (e.g. errorsarlm etc.)
  #2) r_poly_name: shapefile name screeened for NA and no neighbours features
  #3) raster_pred: predicted raster based on spatial regression and neighborhood structure 
  #4) raster_res: residuals raster 
  #5) estimation_process: vector with estimator and method of estimation used
  
  #### Begin script ###
  
  #Extract parameters/arguments
  out_dir  <- list_param$out_dir
  r_ref_s    <- list_param$r_var #if NULL, no image is created, this is the reference image
  #list_param$ <- rast_ref
  r_clip     <- list_param$r_clip
  proj_str <- list_param$proj_str
  list_models <- list_param$list_models
  file_format <- list_param$file_format
  estimator <- list_param$estimator
  estimation_method <- list_param$estimation_method #currently used only for mle from errorsarlm
  NA_flag_val <- list_param$NA_flag_val
  
  if(estimator== "arima"){
    out_suffix <- list_param$out_suffix[1]
  }else{
    out_suffix <- list_param$out_suffix[i] #changed this for now...
  }
  #ARIMA specific
  num_cores <- list_param$num_cores #paraallelization in space.this should be done by row or til enot by pixel!!!!
  time_step <- list_param$time_step #this is the time step for which to start the arima model with
  n_pred_ahead <- list_param$n_pred_ahead
  r_stack <- list_param$r_stack
  arima_order <- list_param$arima_order
  
  #### START SCRIPT
  
  if(!is.null(list_models)){
    list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!  
    formula <-list_formulas[[i]]
  }
  #r_ref_s <- subset(r_ref_s,i)
  #out_dir and out_suffix set earlier
  

  if(estimation_method=="arima"){
    
    #This will be made a function callled from here!!!
    #also add line by line reading to avoid reading all file in memory
    
    #PARAM
    #r_stack
    #n_pred_ahead <- 4 #number of temporal ARIMA predictions ahead..
    #time_step
    # file_format <- ".rst"
    #NA_flag_val <- -9999
    #r_ref_s
    #r_clip
    #out_dir
    #out_suffix
    
    ### start of function
    
    if(!is.null(r_clip)){
      #r_stack2<-r_stack #this is should not be in memoor!!!
      r_stack <- crop(r_stack,r_clip)
      r_ref_s <- crop(r_ref_s,r_clip)
    }
    
    n_start <- c(time_step) +1
    n_end   <- c(time_step)+n_pred_ahead
    r_obs_s <- subset(r_stack,n_start:n_end) #stack of observed layers, 35
    
    #r1 <- subset(r_obs_s,1)
    #xy <-coordinates(r_obs_s)  #get x and y projected coordinates...
    #CRS_interp<-proj4string(r1)
    r_x <-init(r_clip,v="x")
    r_y <-init(r_clip,v="y")
    names(r_x) <- "x"
    names(r_y) <- "y"
    r_stack <- stack(r_stack,r_x,r_y)
    r_stack <- mask(r_stack,r_clip)
    #rm(r1)

    ## 11/15/2015 Still needs to be changed here
    #Very inefficient, will be changed to avoid reading in memory: problem to be sloved
    #readBlockRaster() see earlier
    
    pix_val <- as(r_stack,"SpatialPointsDataFrame") #this will be changed later...to read line by line!!!!
    pix_val2 <- as.data.frame(pix_val)
    df_xy <- pix_val2[,c("x","y")]
    pix_val2 <-  pix_val2[,1:time_step] #152 or 135 in this case, predictions starts at timestep+1
    pix_val2 <- as.data.frame(t(as.matrix(pix_val2 )))#dim 152x26,616

    ### Should add a window option to subset the pixels time series
    #

    ## Now prepare to call pixel based predictions for arima:
    
    out_suffix_s <- paste("arima","_",out_suffix,sep="") #can modify name of output suffix
    out_dir_arima <- create_dir_fun(out_dir,out_suffix_s) #arima models will be stored here

    #pixel <-list_param$pix_val[i]
    #arima_order <-list_param$arima_order
    #n_ahead <- list_param$n_ahead
    #out_dir <- list_param$out_dir
    #out_suffix <- list_param$out_suffix
    #na.rm=T
    #list_param_predict_arima_2 <- list(pix_val=pix_val2,na.rm=T,arima_order=NULL,n_ahead=n_pred_ahead)
    #adde coordinates: df_xy
    list_param_predict_arima_2 <- list(pix_val=pix_val2,arima_order=arima_order,n_ahead=n_pred_ahead,out_dir=out_dir_arima,out_suffix=out_suffix,na.rm=T,df_xy=df_xy)

    #debug(pixel_ts_arima_predict)
    #test_pix_obj <- pixel_ts_arima_predict(7904,list_param=list_param_predict_arima_2)
    #test_pixel_pred_obj <- mclapply(1:66, FUN=pixel_ts_arima_predict,list_param=list_param_predict_arima_2,mc.preschedule=FALSE,mc.cores = num_cores) 

    #note that we are subsetting by column!!
    arima_pixel_pred_obj <- mclapply(1:length(pix_val2), 
                                     FUN=pixel_ts_arima_predict,
                                     list_param=list_param_predict_arima_2,
                                     mc.preschedule=FALSE,
                                     mc.cores = num_cores) 
    
    #find error here:
    #names(arima_pixel_pred_obj) <- 1:length
    
    #list_error <-lapply(1:length(arima_pixel_pred_obj),
    #                    function(i){inherits(arima_pixel_pred_obj[[i]],"try-error")})
    #list_error <- as.numeric(unlist(list_error))
    #index_error <- which(list_error==1)
    
    #for(1:length(index_error)){
    #  d
    #  d
    #}
    
    #i <- index_error[1]
    #generate_NA_arima_obj <- function(i,df_xy,out_suffix){
    #  coords_pix <- df_xy[i,]
    #  val_arima_pixel_pred_obj <-  list(pred=NA, #note this should be a matrix
    #       error=NA, #note this should be a matrix
    #       arima_mod_filename=NA,
    #       pixel_xy=coord_pix) #find coordinates
    #  #out_filename_obj <- "arima_mod_pixel_10945_t_107_tile_1_NDVI_Rita_11032017.RData"
    #  out_filename_obj <- paste0("arima_mod_pixel_",i,"_",out_suffix,".RData")
      
    #  save(val_arima_pixel_pred_obj,out_filename_obj)
    #}
    
    #ref_test<- mclapply(1:length(shps_tiles),
    #                    FUN=generate_raster_tile_ref,
    #                    shps_tiles = shps_tiles,
    #                    r_mask=r_mask,
    #                    list_r_ref_error=list_r_ref_error,
    #                    mc.preschedule=FALSE,
    #                    mc.cores = num_cores)
    
    #find try-error: missing raster
    #list_r_ref_error <- as.numeric(unlist(lapply(list_r_ref,function(x){class(x)=="try-error"})))
    
    ## Select tiles without raster, generate raster and from r_mask
    
    #ref_test<- mclapply(1:length(shps_tiles),
    #                   FUN=generate_raster_tile_ref,
    #                    shps_tiles = shps_tiles,
    #                    r_mask=r_mask,
    #                    list_r_ref_error=list_r_ref_error,
    #                    mc.preschedule=FALSE,
    #                    mc.cores = num_cores)
    
    #now fill in list_r_ref with ref_test
    
    
    #pred_t_l <- mclapply(1:n_pred_ahead,FUN=convert_arima_pred_to_raster,list_param=list_param_arima_convert,mc.preschedule=FALSE,mc.cores = num_cores)
   
    #save this in a separate folder!!!
   
    save(arima_pixel_pred_obj,file=file.path(out_dir,paste("arima_pixel_pred_obj","_",out_suffix,".RData",sep="")))

    #r_ref_s
   
    #### Now convert predictions to raster images
   
    #out_rastnames <- paste(paste("NDVI_pred_mooore_auto",1:n_pred_ahead,sep="_"),"_",out_suffix,file_format,sep="")
    #out_rastnames <- paste(paste("r_temp_pred_arima_",time_step:c(time_step+n_pred_ahead),sep="_"),"_",out_suffix,file_format,sep="")
    #out_rastnames <- paste(paste("r_temp_pred_arima_",1:n_pred_ahead,sep="_"),"_",out_suffix,file_format,sep="")
    #out_rastnames_res <- paste(paste("r_temp_res_arima_",1:n_pred_ahead,sep="_"),"_",out_suffix,file_format,sep="")
    
    #can change later to have t_step
    #to avoid confusion on the time step, we now label raster images with the time step
    raster_name_pred <- paste(paste("r_temp_pred","_",estimator,"_",estimation_method,"_",n_start:n_end,sep=""),"_",out_suffix,file_format,sep="")
    raster_name_res <- paste(paste("r_temp_res","_",estimator,"_",estimation_method,"_",n_start:n_end,sep=""),"_",out_suffix,file_format,sep="")

    #list_param_arima_convert <- list(r_ref_s,arima_pixel_pred_obj,file_format,out_dir,raster_name_pred,file_format,NA_flag_val)
    list_param_arima_convert <- list(r_clip,arima_pixel_pred_obj,file_format,out_dir,raster_name_pred,file_format,NA_flag_val)
   
    names(list_param_arima_convert) <- c("r_ref","ttx","file_format","out_dir","out_rastnames","file_format","NA_flag_val")

    #debug(convert_arima_pred_to_raster)
    ## Convert predicted values to raster...
    #pred_t_l<-lapply(1:1,FUN=convert_arima_pred_to_raster,list_param=list_param_arima_convert) #,mc.preschedule=FALSE,mc.cores = num_cores)

    #pred_t_l <- lapply(1:n_pred_ahead,FUN=convert_arima_pred_to_raster,list_param=list_param_arima_convert) #,mc.preschedule=FALSE,mc.cores = num_cores)
    pred_t_l <- mclapply(1:n_pred_ahead,
                         FUN=convert_arima_pred_to_raster,
                         list_param=list_param_arima_convert,
                         mc.preschedule=FALSE,
                         mc.cores = num_cores)

    #arima_pixel_pred_obj <- mclapply(1:length(pix_val2), FUN=pixel_ts_arima_predict,list_param=list_param_predict_arima_2,mc.preschedule=FALSE,mc.cores = num_cores) 

    pred_t_l <- unlist(pred_t_l)
    r_temp_pred  <- stack(pred_t_l[-c(grep(pattern="error",pred_t_l))])
    r_temp_error <- stack(pred_t_l[c(grep(pattern="error",pred_t_l))])
    r_temp_res <- r_temp_pred - r_obs_s
   
    names(r_temp_res) <- raster_name_res
   
    #raster_name_pred <- out_rastnames
    #raster_name_res <-  out_rastnames_res
    #writeRaster(r_temp_res,bylayer=T)
    writeRaster(r_temp_res,filename=file.path(out_dir,raster_name_res),NAflag=NA_flag_val,bylayer=T,overwrite=TRUE)
   
    #temp_mod <- NA #too many to store?
    temp_mod <- file.path(out_dir,paste("arima_pixel_pred_obj","_",out_suffix,".RData",sep=""))
     
  }
  
   #### PREPARE OBJECT TO RETURN

  #Adding mod object, the ARIMA model will be different...function...most likely
  temp_reg_obj <- list(temp_mod,file.path(out_dir,raster_name_pred),file.path(out_dir,raster_name_res),c(estimator,estimation_method))
  names(temp_reg_obj) <- c("temp_mod","raster_pred","raster_res","estimation_process")
  save(temp_reg_obj,file= file.path(out_dir,paste("temp_reg_obj","_",estimator,"_",estimation_method,"_t_",i,"_",out_suffix,".RData",sep="")))
  
  #write a log file with predictions parameters used at the time:
  #Extract parameters/arguments
  
  return(temp_reg_obj)
  
}





################### END OF SCRIPT ##################