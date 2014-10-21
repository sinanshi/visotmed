 require(ncdf)
 require(raster)
 
 
 setClass("mapncdf",representation(
	filename="character",
	varname="character",
	start_ind="numeric",
	count_ind="numeric"))
 

 map2spatial<-function(lon,lat,map,missval){
	 lonarray<-array(rep(lon,length(lat)), dim=dim(map))
	 latarray<-t(array(rep(lat,length(lon)),dim=dim(t(map))))	 
	sp<-data.frame("lon"=as.vector(lonarray),"lat"=as.vector(latarray),"val"=as.vector(map))
	sp$lon[sp$lon>180]<-sp$lon[sp$lon>180]-360
	sp
}
  
 map.info.ncdf<-function(filename){
	nc<-open.ncdf(filename)
	ncdim<-list()
	cat("=========\n")
	cat(filename, ":\n")
	cat("=========\n")
	cat(length(nc$var), "variables:\n")	
	
	print(names(nc$var))
	cat("\nvariables have",nc$ndims,"dimensions.\n")
	print(names(nc$dim))
	for(i in 1:nc$ndims){
		len<-nc$dim[[i]]$len
		cat(nc$dim[[i]]$name, nc$dim[[i]]$len, nc$dim[[i]]$units,"\n")
		if(len>8)  cat("[", nc$dim[[i]]$vals[1:4],",...,",nc$dim[[i]]$vals[(len-3):len],"]\n")
		else print(nc$dim[[i]]$vals)
		ncdim[[nc$dim[[i]]$name]]<-nc$dim[[i]]$vals
	}	
	ncdim	
}



 setMethod("mapConvert", signature(obj="mapncdf"),
	function(obj){
		 ncdim<-map.info.ncdf(obj@filename)
		 nc<-open.ncdf(obj@filename)
		 ndims<-nc$ndims
 		 data<-get.var.ncdf(nc, obj@varname, start=obj@start_ind,count=obj@count_ind)
 		 missval<-nc$var[[obj@varname]]$missval
 		 
 		 dataframe<-map2spatial(ncdim[[1]],ncdim[[2]], data,missval)
 		 mappoints<-list()
 		 mappoints[["dataframe"]]<-dataframe
 		 mappoints[["missval"]]<-missval
 		 
 		mappoints	 
	 })
	
	

	
	


	 
	
	
