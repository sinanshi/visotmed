 #============
 #digits: get number of bits of an integer 
 #		 or a floating number. 
 #		 [-1,1] return negative values
 #		 0 return 1
 #		others return positive values
 #============
digits<-function(num){
	digt<-0
	if(num==0){
		return(1)
	}
	if(num<1 & num>-1 & num!=0){	
		residule<-0
		while(residule==0){
			residule<-round(num,digits=digt)
			digt<-digt+1
		}
	return(-digt+1)
	}else{
		residule<- num
		while(residule!=0){
			residule<-round(residule/10)
			digt<-digt+1
		}
		return(digt-1)
	}
}

#==========
#getGrid: Derive grid with two input vector
#		    (general function)
#==========
getGrid<-function(lon,lat){
	lon_<-rep(lon,length(lat))
	lat_<-sort(rep(lat,length(lon)))
	grid<-as.data.frame(cbind("lon"=lon_,"lat"=lat_))
	grid
}
	
#==========
#维持在同个scale上，即原res是多少，输出应在同一个数量级上
#==========
roundUnregular<-function(data){
	diff<-diff(sort(unique(data$lon)))
	digit<-1
	while(!all(diff==diff[1])){
		lon<-round(data$lon/digit)*digit
		#data$lat<-round(data$lat/digit)*digit
		diff<-diff(sort(unique(lon)))
		#cat(digit, "  ")
		digit<-digit+1000
	}
	digit<-1
	diff<-diff(sort(unique(data$lat)))
	while(!all(diff==diff[1])){
		lat<-round(data$lat/digit)*digit
		#data$lat<-round(data$lat/digit)*digit
		diff<-diff(sort(unique(lat)))
		#cat(digit, "  ")
		digit<-digit+1000
	}
	if(digit!=1){
	data$lon<-lon
	data$lat<-lat
	}
	 #cat("map(res=", "Deg) will be projected on a ", length(unique(data$lon)),
	#	"*",length(unique(data$lat)), " grid. \n",sep="")

	 data
}

#==========
#
#==========
getRegularGrid<-function(data,res){
	data<-roundUnregular(data,res)
	lon<-sort(unique(data$lon))
	lat<-sort(unique(data$lat))
	grid<-getGrid(lon,lat)	
	grid
}




