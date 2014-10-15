
 
 
 source("R/header.r")

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

theme_opts <- list(theme(
			panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        #panel.background=element_blank(),
                        panel.background = element_rect(color = "black",fill=NA,  size = 0.4, linetype = "solid"),
                        #panel.border=element_rect(color = "black",fill=NA,  size = 0.4, linetype = "solid"),
                        plot.background = element_blank(),
                        #plot.background=element_blank(),
                        #panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        #panel.margin = unit(5,"lines"),
                        #axis.title.x = element_blank(),
                        #axis.title.y = element_blank(),
                        plot.title = element_text()))

 plot.title = "General Circulation Models Climate Data Output"
 plot.subtitle = 'created by GCM runs'
#  nc<-new("mapncdf", filename="data/Climate_CX_v2_2000.nc",
#  		varname="TmaxdegC",year_index=5)
 

 nc<-new("mapncdf", filename="data/tas_A1_2.nc",
 		varname="tas",start_ind=c(1,1,1),count_ind=c(-1,-1,1))
  kkk<-mapConvert(nc)

 sp<-SpatialPoints(kkk,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#  
# sp_pj<-spTransform(sp,CRS("+proj=laea +lat_0=52   +lon_0=10      +x_0=3 210 000    +y_0=4 321 000"))
sp_pj<-spTransform(sp,CRS("+proj=robin"))
#  sp_pj<-data.frame(sp_pj)

 sp_pj<-as.data.frame(sp_pj)
 s2<-303618.6#145579.5
 s1<-270000
  sp_pj<-data.frame(round(sp_pj[,1]/s1)*s1,round(sp_pj[,2]/s2)*s2,sp_pj[,3])
 names(sp_pj)<-c("lon","lat","val")

 
 g<-ggplot(sp_pj,aes(lon,lat))+geom_raster(aes(fill=val))+
 geom_path(data=countries_robin_df,aes(long,lat,group=group),color="black",size=0.2)+
     scale_fill_gradientn(colours=myPalette(100), 
 		      guide= guide_colorbar(title=expression(degree~C), title.position="top",
 			      #    barwidth = 25, barheight = 1,nbin=100, 
 			     draw.ulim = FALSE, draw.llim = FALSE ))+
 		      theme_opts+
 		      ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))
		      
		      
h<-ggplot(kkk,aes(lon,lat,fill=val))+geom_raster()+
 geom_path(data=wmap_gg,aes(long+180,lat,group=group),color="black",size=0.2)+
scale_fill_gradientn(colours=myPalette(100), 
 		      guide= guide_colorbar(title=expression(degree~C), title.position="top",
 			      #    barwidth = 25, barheight = 1,nbin=100, 
 			     draw.ulim = FALSE, draw.llim = FALSE ))+
 		      theme_opts+
 		      ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))
