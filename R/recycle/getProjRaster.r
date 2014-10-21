#  
# getProjRaster<-function(data, res){
# 	grid<-getRegularGrid(data,res)
# 	
# 
# 
# }
# 
# llon<-length(sort(unique(sp_pj$lon)))
# lon_large<-sort(rep(unique(grid$lon),length.out=llon))
# lon_table<-cbind(sort(unique(sp_pj$lon)),lon_large)
# llat<-length(sort(unique(sp_pj$lat)))
# lat_large<-sort(rep(unique(grid$lat),length.out=llat))
# lat_table<-cbind(sort(unique(sp_pj$lat)),lat_large)
# 
# #  plot(lon_table[,1][1:100],type="l")
# #  lines(lon_table[,2])
# # x11()
# #  plot(lat_table[,1],type="l")
# # lines(lat_table[,2])
# # lon<-array(NA,21632)
# # lat<-array(NA,21632)
# # val<-array(NA,21632)
# # k<-1
# # for(j in 1:length(unique(grid$lat))){
# # 	for(i in 1:length(unique(grid$lon))){
# # 		lon[k]<-grid$lon[i]
# # 		lat[k]<-grid$lat[j]
# # 		dis<-(sp_pj$lon-grid$lon[i])^2+(sp_pj$lat-grid$lat[j])^2
# # 		
# # 		
# # 
# # 		val[k]<-sp_pj[dis==min(dis),3]
# # 		cat(lon[k],lat[k],sp_pj$lon[dis==min(dis),],"\n")
# # 		k<-k+1
# # 		
# # 	}
# # }
# p<-data.frame(lon,lat,val)
# 
# # for(i in 1:length(sp_pj$lon)){
# # 	lon_new<-lon_table[,2][lon_table[,1]==sp_pj$lon[i]]
# # 	
# # 	lat_new<-lat_table[,2][lat_table[,1]==sp_pj$lat[i]]
# # 	#out[out$lon==lon_new&out$lat==lat_new,3]<-sp_pj$val[i]
# # 
# # }
# # 
# 
# # out2<-out
# # for(i in 1:length(out[,1])){
# # 	if(is.na(out2[,3][i])){
# # 		out2[,3][i]<-out2[,3][i-1]
# # 	}
# # }
#  names(p)<-c("lon","lat","val")
#  ggplot(p,aes(lon,lat,fill=val))+geom_raster()+scale_alpha_manual(values=c(0,1),guide="none")