 
source("R/header.r")
myPalette <- colorRampPalette((brewer.pal(9, "YlOrRd")), space="Lab")
myPalette<-brewer.pal(9, "YlOrRd")

theme_opts <- list(theme(
			panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                       # panel.background=element_blank(),
                        panel.background = element_rect(color = "black",fill=NA,  size = .5, linetype = "solid"),
                        #panel.border=element_rect(color = "black",fill=NA,  size = 0.4, linetype = "solid"),
                        plot.background = element_blank(),
                        plot.background=element_blank(),
                        panel.border=element_rect(color = "black",fill=NA,  size = .5, linetype = "solid"),
                        legend.position="right",
                        #panel.border = element_blank(),
                        #axis.line = element_blank(),
                        #axis.text.x = element_blank(),
                        #axis.text.y = element_blank(),
                        #axis.ticks = element_blank(),
                        #panel.margin = unit(5,"lines"),
                        #panel.margin = unit(0,"null"),
                        #plot.margin = rep(unit(0,"null"),4),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text()))


 nc<-new("mapncdf", filename="data/Climate_CX_v2_2000.nc",
 		varname="TmaxdegC",start_ind=c(1,1,180),count_ind=c(-1,-1,1))
 #nc<-new("mapncdf", filename="data/tas_A1_2.nc",
# 		varname="tas",start_ind=c(1,1,1),count_ind=c(-1,-1,1))


 kkk1<-mapConvert(nc)
 kkk1$dataframe$lat<- kkk1$dataframe$lat-0.25/2
 kkk1$dataframe$lon<- kkk1$dataframe$lon-0.25/2

 kkk<-kkk1
 from<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
to<-"NA"
 to<-"+proj=laea +lat_0=52   +lon_0=10      +x_0=3 210 000    +y_0=4 321 000"
 #to<-"+proj=robin"
  coordinates(kkk1$dataframe)<- ~lon+lat
  kkk1<-kkk1$dataframe
  projection(kkk1)<-from
  lines_proj<-graticules(10,5,proj=to)

   lines<-lines2dataframe(lines_proj)
    kkk<-mapProjection(kkk,from,to)
	  
  




 knn<-kkk[!is.na(kkk$val),]
 

classifyMap<-function(map,class,prec=1){
	len<-length(map[[1]])
	ind<-sort(rep(seq(1,class),length.out=len))
	map<-map[order(map$val),]
	map<-data.frame(map,"class"=ind)
	
	text<-vector()
	for(i in 1:class){
		minval<-map$val[min(which(map$class==i))]
		maxval<-map$val[max(which(map$class==i))]
		text[i]<-paste(round(minval,digits=prec)," - ",round(maxval,digits=prec),sep="")
	}
	out<-list()
	out[["map"]]<-map
	out[["range"]]<-text
	return(out)
}
map<-classifyMap(knn,9,prec=0)
knn<-map$map
labels<-map$range

	

	
 
 knn<-cutWindow(knn,proj=to,window=c(-10,45,25,46))
 boundary<-c(min(knn$lon),max(knn$lon),min(knn$lat),max(knn$lat))
 
 plot.title = paste("LPJmL 0.25 Degree NCDF Climate Inputs")
 plot.subtitle = "Parameter: daily average tas - Time: 2006-07-01"
p<-gratText(lines_proj,boundary)
 g<-ggplot(knn,aes(lon,lat))+geom_raster(aes(fill=class))+
# scale_fill_manual(values=myPalette(12))+
# scale_fill_gradient(low = "pink", high = "green")+
     scale_fill_gradientn(colours=myPalette, na.value="white",breaks=c(9:1),labels=rev(labels),
 		      guide= guide_legend(title=expression(degree~C), title.position="top",#guide_legend
 			          barwidth = 1, barheight = 10, direction="vertical", 
 			     draw.ulim = T, draw.llim =T ))+
#   scale_colour_brewer() +
 		      theme_opts+
 		      coord_equal()+
 		      ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))+
 		      scale_x_continuous(limits = c(min(knn$lon), max(knn$lon)), expand = c(0, 0),
			    breaks=as.vector(p$x[,1]), 
			    labels=bquote(.(as.vector(p$x$txt))))+
 		      scale_y_continuous(limits = c(min(knn$lat), max(knn$lat)), expand = c(0, 0),
			    breaks=as.vector(p$y[,1]), labels=p$y[,2])+
 		      geom_path(data=lines,aes(lon,as.numeric(lat),group=group),color="grey",size=0.25)#graticules
 		      
#  		      annotation_custom(a, xmin=0, xmax=Inf, ymin=0, ymax=Inf)

 country<-new("shapefile",dir="data/admin_countries/10m/",layer="ne_10m_admin_0_countries",
 	     proj_to= to)


d<-addLines(country,gg=g,size=0.35)
# 
saveMap(paste("std_window_discrete.jpeg",sep=""),d, LOGO=TRUE,width=7,height=4)