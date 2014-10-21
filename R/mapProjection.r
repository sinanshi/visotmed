 
 
 require(raster)
adjust.longitude<-function(ncdata){
	if(any(ncdata[["ncdim"]][["lon"]]>180)){
		cat("convert longitude from [0-360] deg to [-180,180] deg\n")
		ind<-ncdata$ncdim$lon>180
		ncdata$ncdim$lon<-c((ncdata$ncdim$lon[ind]-360),ncdata$ncdim$lon[!ind])
		return(ncdata)
	}else
	return(ncdata)
}



map2raster<-function(data,proj,...){
	raster<-rasterFromXYZ(data,...)
	projection(raster)<-CRS(proj)
	return(raster)
}


#=====
#input data{dataframe, missval}
#====
mapProjection<-function(data,from,to,method="ngb",...){
	raster<-map2raster(data$dataframe,proj=from,...)
	raster_proj<-projectRaster(raster,crs=CRS(to),method=method)
	points<-as.data.frame(rasterToPoints(raster_proj))
	names(points)<-c("lon","lat","val")
	points$val[abs(points$val)>data$missval]<-NA
	return(points)
}