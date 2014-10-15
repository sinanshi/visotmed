 require(ncdf)
 require(raster)
 
 
 setClass("mapncdf",representation(
	filename="character",
	varname="character",
	start_ind="numeric",
	count_ind="numeric"))
 

 
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
 		 
 		 spdataframe<-map2spatial(ncdim[[1]],ncdim[[2]], data,missval)
 		 spdataframe 		 
	 })
	
	

	
	


	 
	
	
