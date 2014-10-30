 
source("R/header.r")

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
theme_opts <- list(theme(
	panel.grid.minor = element_blank(),
	panel.grid.major = element_blank(),
	# panel.background=element_blank(),
	panel.background = element_rect(color = "black",fill=NA, size = .5, linetype = "solid"),
	#panel.border=element_rect(color = "black",fill=NA, size = 0.4, linetype = "solid"),
	plot.background = element_blank(),
	plot.background=element_blank(),
	panel.border=element_rect(color = "black",fill=NA, size = .5, linetype = "solid"),
	legend.position="right",
	#panel.border = element_blank(),
	#axis.line = element_blank(),
	axis.text.x = element_blank(),
	axis.text.y = element_blank(),
	axis.ticks = element_blank(),
	#panel.margin = unit(5,"lines"),
	#panel.margin = unit(0,"null"),
	#plot.margin = rep(unit(0,"null"),4),
	axis.title.x = element_blank(),
	axis.title.y = element_blank(),
	plot.title = element_text()))

nc<-new("mapncdf", filename="data/Climate_CX_v2_2000.nc",
	varname="TmaxdegC",start_ind=c(1,1,180),count_ind=c(-1,-1,1))
#nc<-new("mapncdf", filename="data/tas_A1_2.nc",
# varname="tas",start_ind=c(1,1,1),count_ind=c(-1,-1,1))
kkk1<-mapConvert(nc)
kkk1$dataframe$lat<- kkk1$dataframe$lat-0.25/2
kkk1$dataframe$lon<- kkk1$dataframe$lon-0.25/2
kkk<-kkk1
from<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
to<-"NA"
to<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=3 210 000 +y_0=4 321 000"
#to<-"+proj=robin"
coordinates(kkk1$dataframe)<- ~lon+lat
kkk1<-kkk1$dataframe
projection(kkk1)<-from

kkk<-mapProjection(kkk,from,to)
knn<-kkk[!is.na(kkk$val),]


plot.title = paste("LPJmL 0.25 Degree NCDF Climate Inputs")
plot.subtitle = "Parameter: daily average tas - Time: 2006-07-01"



		
#knn<-cutWindow(knn,proj=to,window=c(-10,45,25,46))
#knn<-cutWindow(knn,proj=to,window=c(-10,45,20,52))


g<-ggplot(knn,aes(lon,lat))+geom_raster(aes(fill=val))+
scale_fill_gradientn(colours=myPalette(11), na.value="white",
		     guide= guide_colorbar(title=expression(degree~C), title.position="top",#guide_legend
			     barwidth = 1, barheight = 10, direction="vertical",
			     draw.ulim = T, draw.llim =T ))+
 theme_opts+
 coord_equal()+
 ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))#+
 g<-addLines(country,gg=g,size=0.35)
 #g<-definePlotBoundary(g)
 g<-addGraticules(g,x=10,y=10,proj=to,plot.text=TRUE, plot.line=TRUE)

country<-new("shapefile",dir="data/admin_countries/10m/",layer="ne_10m_admin_0_countries",
	     proj_to= to)
 
 saveMap(paste("projection_with_graticules.jpeg",sep=""),g, LOGO=TRUE,width=6,height=4)
