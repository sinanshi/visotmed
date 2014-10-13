
 
 
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
 nc<-new("mapncdf", filename="data/Climate_CX_v2_2000.nc",
 		varname="TmaxdegC",year_index=5)
 kkk<-mapConvert(nc)
  val<-as.data.frame(kkk$val)
 coordinates(kkk)=~lon+lat
 spkkk<-SpatialPixelsDataFrame(data=as.data.frame(val),points=kkk)
 projection(spkkk)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  sp_pj<-spTransform(spkkk,CRS("+proj=laea +lat_0=52   +lon_0=10      +x_0=3 210 000    +y_0=4 321 000"))

pp<-as.data.frame(sp_pj)
 names(pp)<-c("val","lon","lat")

 
 
  g<-ggplot(as.data.frame(pp),aes(lon,lat))+geom_tile(aes(fill=kkk.val))#+
#   scale_fill_gradientn(colours=myPalette(100), 
#  		      guide= guide_colorbar(title=expression(degree~C), title.position="top",
#  			      #    barwidth = 25, barheight = 1,nbin=100, 
#  			     draw.ulim = FALSE, draw.llim = FALSE ))+
#  		      theme_opts+
#  		      ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))
		      
		      
	