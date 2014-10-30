 #cut window from final display data frame
cutWindow<-function(data,proj,window=NULL){
	if(is.null(window)){
		return(data)
	}
	else{
		corners<-data.frame(lon=c(window[1],window[2]),lat=c(window[3],window[4]))
		if(proj!="NA"){
			corners<-SpatialPoints(corners,CRS(proj_latlon))
			corners<-as.data.frame(spTransform(corners,CRS(proj)))
		}
		data<-data[data$lon>corners$lon[1]&data$lon<corners$lon[2], ]
		data<-data[data$lat>corners$lat[1]&data$lat<corners$lat[2], ]
		return(data)
	}
}