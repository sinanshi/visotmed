 
source("R/header.r")

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

theme_opts <- list(theme(
			panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background=element_blank(),
                        #panel.background = element_rect(color = "black",fill=NA,  size = 0.4, linetype = "solid"),
                        #panel.border=element_rect(color = "black",fill=NA,  size = 0.4, linetype = "solid"),
                        plot.background = element_blank(),
                        plot.background=element_blank(),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        #panel.margin = unit(5,"lines"),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text()))

 plot.title = paste("LPJmL 0.25 Deg climate input",expression(degree~C))
 plot.subtitle = "Mean Surface Temperature"
 nc<-new("mapncdf", filename="data/Climate_CX_v2_2000.nc",
 		varname="TmaxdegC",start_ind=c(1,1,1),count_ind=c(-1,-1,1))
 
 
   getBox<-function(spdata,len=100){
	  box<-bbox(spdata)
# 	  line[[1]]<-data.frame("x"=rep(box[1,1],len),"y"=seq(box[2,1],box[2,2],length.out=len))
# 	  line[[2]]<-data.frame("x"=rep(box[1,2],len),"y"=seq(box[2,1],box[2,2],length.out=len))
# 	  line[[3]]<-data.frame("x"=seq(box[1,1],box[1,2],length.out=len),"y"=rep(box[2,1],len))
# 	  line[[4]]<-data.frame("y"=seq(box[1,1],box[1,2],length.out=len),"x"=rep(box[2,2],len))
	  L1<-Line(cbind(rep(box[1,1],len),seq(box[2,1],box[2,2],length.out=len)))
	  L2<-Line(cbind(rep(box[1,2],len),seq(box[2,1],box[2,2],length.out=len)))
	  L3<-Line(cbind(seq(box[1,1],box[1,2],length.out=len),rep(box[2,1],len)))
	  L4<-Line(cbind(seq(box[1,1],box[1,2],length.out=len),rep(box[2,2],len)))
	  Lines<-Lines(list(L1,L2,L3,L4),ID="box")
	  return(SpatialLines(list(Lines)))
}
   
   
 kkk1<-mapConvert(nc)
  from<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  to<-"+proj=laea +lat_0=52   +lon_0=10      +x_0=3 210 000    +y_0=4 321 000"
  coordinates(kkk1$dataframe)<- ~lon+lat
  kkk1<-kkk1$dataframe
  projection(kkk1)<-from
  
  projection(kkk1)<-from
  lines<-gridlines(kkk1)
  text<-gridat(kkk1)
  box<-getBox(kkk1)
  
  projection(lines)<-from
  projection(text)<-from
  projection(box)<-from


  lines2dataframe<-function(lines){
	  a<-data.frame()
	  k<-1
	  for(i in 1:length(lines)){
		  for(j in 1:length(lines@lines[[i]]@Lines)){
			  l<-cbind(lines@lines[[i]]@Lines[[j]]@coords,rep(k,length(lines@lines[[i]]@Lines[[j]]@coords[,1])))
			  a<-rbind(a,l)
			  k<-k+1
		}
	}
	names(a)<-c("lon","lat","group")
	return(a)
}
		  
		  
  lines<-lines2dataframe(spTransform(lines,CRS(to)))
  text<-as.data.frame(spTransform(text,CRS(to)))
  
  box<-lines2dataframe(spTransform(box,CRS(to)))
  kkk<-mapProjection(kkk$dataframe,from,to)
  

	  
  


 
 g<-ggplot(kkk,aes(lon,lat))+geom_raster(aes(fill=val))+
 #geom_path(data=countries_robin_df,aes(long,lat,group=group),color="black",size=0.2)+
     scale_fill_gradientn(colours=myPalette(100), na.value="lightblue",
 		      guide= guide_colorbar(title=expression(degree~C), title.position="top",
 			      #    barwidth = 25, barheight = 1,nbin=100, 
 			     draw.ulim = FALSE, draw.llim = FALSE ))+
 		      theme_opts+
 		      ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))#+
#  		      annotation_custom(a, xmin=0, xmax=Inf, ymin=0, ymax=Inf)



saveMap("plot.png",g, LOGO=TRUE)



#popViewport()
# 
#     # The title
# pushViewport(viewport(layout.pos.row=1, layout.pos.col = 1))
# print(grid.draw(title), newpage=FALSE)
# popViewport()
# popViewport()




