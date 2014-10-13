 
 map2spatial<-function(lon,lat,map,missval){
	 lonarray<<-array(rep(lon,length(lat)), dim=dim(map))
	 latarray<<-t(array(rep(lat,length(lon)),dim=dim(t(map))))
	 
	sp<-data.frame("lon"=as.vector(lonarray),"lat"=as.vector(latarray),"val"=as.vector(map))
	sp<-sp[abs(sp$val)<missval,]
	sp
}