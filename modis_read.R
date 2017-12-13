library(raster)
library(rgdal)

in_dir <- "/nfs/teamhurricane-data/Habitat Group/Geospatial_Data/Raw_Data/MODIS"

hdf_file <-"test.hdf"

hdf_file <- file.path(in_dir,hdf_file)
GDALinfo_hdf <- GDALinfo(hdf_file,returnScaleOffset = F)

str(GDALinfo_hdf)
modis_subdataset <- attributes(GDALinfo_hdf)$subdsmdata
print(modis_subdataset)

hdf_df <- (strsplit(modis_subdataset,":"))
#length(modis_subdataset)
hdf_df <- as.data.frame(do.call(rbind,hdf_df),stringsAsFactors=F)
#hdf_df <- data.frame(lapply(hdf_df, as.character))
#hdf_df %>% 
#  mutate_all(as.character)

names(hdf_df) <- c("subdataset_name","description","dir","product","var_name")
#Select automatically QC flag!!
View(hdf_df)

write.table(hdf_df,"hdf_subdataset.txt",sep=",")

modis_subset_layer_Day <- paste("HDF4_EOS:EOS_GRID:",
                                hdf_file,subdataset,sep="")

#NDVI variable
modis_layer_str1 <- unlist(strsplit(modis_subdataset[1],"\""))[3] #Get day NDVI layer
#QC
modis_layer_str2 <- unlist(strsplit(modis_subdataset[5],"\""))[3] #Get day VI QC layer

subdataset <- modis_layer_str1
modis_subset_layer_Day <- paste("HDF4_EOS:EOS_GRID:",hdf_file,subdataset,sep="")

r <-readGDAL(modis_subset_layer_Day)
r  <-raster(r)

plot(r)

####### END OF SCRIPT ######