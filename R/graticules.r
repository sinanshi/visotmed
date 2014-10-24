prj_latlon<<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
graticules<-function(disx=10,disy=10,proj="NA"){
	x<-seq(-180,180,disx)
	y<-seq(-90,90,disy)
	
	EW<-list()
	for(i in 1:length(x)){
		l<-cbind(rep(x[i],1000),seq(-90,90,length.out=1000))
		EW[[i]]<-Line(l)
	}
	EW<-Lines(EW,ID=c("EW"))
	NS<-list()
	for(i in 1:length(y)){
		l<-cbind(seq(-180,180,length.out=1000),rep(y[i],1000))
		NS[[i]]<-Line(l)
	}	
	NS<-Lines(NS,ID=c("NS"))	
	out<-SpatialLines(list(EW,NS),proj4string=CRS(prj_latlon))
	if(proj!="NA"){
		out<-spTransform(out,CRS(proj))
	}
	return(out)
}
	
LabelsDegreeEW<-function (x) {
    x <- ifelse(x > 180, x - 360, x)
    pos = sign(x) + 2
    if (any(x == -180)) 
        pos[x == -180] = 2
    if (any(x == 180)) 
        pos[x == 180] = 2
    dir = c("°W", "°", "°E")
    paste(abs(x), dir[pos])
}

LabelsDegreeNS<-function (x) 
{
    pos = sign(x) + 2
    dir = c("°S", "°", "°N")
    paste(abs(x), dir[pos])
}


gratText<-function(graticule,lim){
	if(graticule@lines[[1]]@ID!="EW") stop("graticule class id is not correct.\n")
	if(graticule@lines[[2]]@ID!="NS") stop("graticule class id is not correct.\n")
	wbound_lon<-lim[1]
	sbound_lat<-lim[3]	
	ebound_lon<-lim[2]
	nbound_lat<-lim[4]
	
	grat_ew<-graticule@lines[[1]]@Lines
	grat_ns<-graticule@lines[[2]]@Lines

	txt_pos_ew<-data.frame()
	txt_pos_ns<-data.frame()
	for(i in 1:length(grat_ew)){
		ew_lon<-grat_ew[[i]]@coords[,1]
		ew_lat<-grat_ew[[i]]@coords[,2]
		
		diff<-abs(sbound_lat-ew_lat)	
		diff<-cbind(diff,seq(1,length(diff)))
		diff<-diff[order(diff[,1]),]
		# check three closest values and exclude the values that are outbound
		for(j in 1:3){
			ind_<-diff[j,2]
			if(ew_lon[ind_]>=wbound_lon&ew_lon[ind_]<=ebound_lon){
				ind<-ind_
				break
			}else{
				ind<-NA
			}
		}
		if(!is.na(ind) ){
			west_bound<-c(ew_lon[ind],ew_lat[ind]) 
			txt_pos_ew<-rbind(txt_pos_ew,west_bound)
		}
	}

	for(i in 1:length(grat_ns)){
		sn_lat<-grat_ns[[i]]@coords[,2]
		sn_lon<-grat_ns[[i]]@coords[,1]

		diff<-abs(wbound_lon-sn_lon)	
		diff<-cbind(diff,seq(1,length(diff)))
		diff<-diff[order(diff[,1]),]
		for(j in 1:3){ # check three closest values and exclude the values that are outbound
			ind_<-diff[j,2]
			if(sn_lat[ind_]>=sbound_lat&sn_lat[ind_]<=nbound_lat){
				ind<-ind_
				break
			}else{
				ind<-NA
			}
		}
		if(!is.na(ind) ){
		south_bound<-c(sn_lon[ind],sn_lat[ind]) 
		txt_pos_ns<-rbind(txt_pos_ns,south_bound)
		}

	}
	spoints_ew<-SpatialPoints(txt_pos_ew, CRS(projection(graticule)))
	spoints_ns<-SpatialPoints(txt_pos_ns, CRS(projection(graticule)))
	
 	if(projection(graticule)!=prj_latlon){
		latlon_ew<-as.data.frame(spTransform(spoints_ew,CRS(prj_latlon)))
		latlon_ns<-as.data.frame(spTransform(spoints_ns,CRS(prj_latlon)))
	}else{
		latlon_ew<-as.data.frame(spoints_ew)
		latlon_ns<-as.data.frame(spoints_ns)
	}

		
		
	text<-list()
	text[["x"]]<-data.frame(lon=txt_pos_ew[,1],txt=LabelsDegreeEW(round(latlon_ew[,1],digits=1)))
	text[["y"]]<-data.frame(lon=txt_pos_ns[,2],txt=LabelsDegreeNS(round(latlon_ns[,2],digits=1)))
	
	return(text)
}	