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