
library(RColorBrewer)
library(rgdal)
library(raster)
library(sp)
library(ggplot2)
load("test/clim.Rdata")

#pj<-"+proj=laea +lat_0=52   +lon_0=10      +x_0=3 210 000    +y_0=4 321 000 "
pj<-"+proj=robin"
map2<-map
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


#==========
#(a) ggplot without projection
#==========
wmap <- readOGR(dsn="data/ne_110m_land", layer="ne_110m_land")
wmap_gg<-fortify(wmap)
grat <- readOGR("data/ne_110m_graticules_all", layer="ne_110m_graticules_15") 
grat_df <- fortify(grat)
bbox <- readOGR("data/ne_110m_graticules_all", layer="ne_110m_wgs84_bounding_box") 
bbox_df<- fortify(bbox)
countries <- readOGR("data/ne_110m_admin_0_countries", layer="ne_110m_admin_0_countries") 

a<-ggplot(data=map2,aes(x=lon,y=lat))+
geom_tile(aes(fill=val))+
geom_polygon(data=wmap_gg,aes(long,lat,group=group),fill=NA,color="black")+
geom_path(data=bbox_df,aes(long,lat,group=group,fill=NULL),color="black")+
geom_path(data=grat_df,aes(long,lat,group=group),fill=NA,color="black")+xlim(-25,50)+ylim(20,55)



#==========
#(a) ggplot with  projection without boarder
#==========
wmap_robin <- spTransform(wmap, CRS(pj))
wmap_df_robin <- fortify(wmap_robin)
grat_robin <- spTransform(grat, CRS(pj))  # reproject graticule
grat_df_robin <- fortify(grat_robin)
countries_robin <- spTransform(countries, CRS(pj))
countries_robin_df<-fortify(countries_robin)
coordinates(map)=~lon+lat
map_<-as.data.frame(map)
map_<-as.data.frame(map_[,3])
names(map_)<-"val"
map_pixel<-SpatialPixelsDataFrame(data=map_,points=map,tolerance=0.007)
map_raster<-raster(map_pixel)
projection(map_raster)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
raster_proj<-projectRaster(map_raster,crs=pj)
point_proj<-as.data.frame(rasterToPoints(raster_proj))






# 
# Rsp<-SpatialPointsDataFrame(coordinates(map),data=as.data.frame(map),
# 			    proj4string=(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
# 





projection(map)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
kk<-coordinates(map)
pp<-as.data.frame(map)
Rsp<-SpatialPointsDataFrame(kk,data=pp,proj4string=(CRS("+init=epsg:4326")))
# tt<-spTransform(Rsp,CRS("+init=epsg:"))

Grid_wgs84<-gridat(Rsp)
line_wgs84<-gridlines(Rsp)

line_prj<-spTransform(line_wgs84,CRS(pj))
grid_prj<-spTransform(Grid_wgs84,CRS(pj))
label_prj<-as.data.frame(coordinates(grid_prj))
label<-as.character(grid_prj$labels)




label_prj<-cbind(label_prj,label)
names(label_prj)<-c("x","y","label")
# label_prj$x<-label_prj$x-max(point_proj$x)*0.1
# label_prj$y<-label_prj$y-max(point_proj$y)*0.1
cline<-coordinates(line_prj)
k<-1
df<-NULL
for(i in 1:2){
	for(j in 1:length(cline[[i]])){
		d<-cline[[i]][[j]]
		group<-rep(k,length.out=length(d[,1]))
		d<-cbind(d,group)
		df<-rbind(df,d)
		k<-k+1
		}
	}
	df<-as.data.frame(df)
	names(df)<-c("x", "y","group")
	
	
	plot.title = "General Circulation Models Climate Data Output"
 plot.subtitle = 'created by GCM runs'

f<-ggplot(point_proj,aes(x,y))+geom_tile(aes(fill=val))+
geom_path(data=countries_robin_df,aes(long,lat,group=group),color="black",size=0.2)+
xlim(min(point_proj$x)*1.01,max(point_proj$x))+
ylim(min(point_proj$y)*1.01,max(point_proj$y))+
geom_line(data=df,aes(x=x,y=y,group=group,fill=NULL),size=0.2,color="black",linetype="dashed")+
geom_text(data=label_prj,aes(x,y,label=label),parse=TRUE,size=3.5)+
theme_opts+
coord_equal()+
 labs(title="World map (robinson)",x="",y="") +
scale_fill_gradientn(colours = myPalette(100),breaks=c(-15,-10, -5,10,5, 0,5,15),
		     guide= guide_colorbar(title=expression(degree~C), title.position="top",
			 #    barwidth = 25, barheight = 1,nbin=100, 
			     draw.ulim = FALSE, draw.llim = FALSE ))+
ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))
ggsave("map11.png",width=12.5,height=6.5,dpi=72)
#
#
#
#
# Rsp<-SpatialPointsDataFrame(Rc,Rd,proj4string=(CRS("+init=epsg:3035"))) ///// 
# 
# 
# 
# R1<-raster("sinan11.tif")
# 
# > proj4string(R1)
# [1] "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# 
# 
# > library(rgdal)
# 
# 
# > Rc<-coordinates(R1)
# > Rd<-as.data.frame(R1)
# > Rsp<-SpatialPointsDataFrame(Rc,Rd,proj4string=(CRS("+init=epsg:3035"))) ///// raster to SpatialPointsDataFrame
# 
# >  grid1<-gridlines(Rsp) //// GRID creation
# 
# > plot(grid1)
# 
# > proj4string(grid1)
# [1] "+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# 
# 
# > C84<-spTransform(Rsp, CRS("+init=epsg:4326"))
# 
# > GridA<-gridat(C84)  //// points and labels in WGS84
# 
# 
# > GridB<-spTransform(GridA,CRS("+init=epsg:3035")) //// CREATE points in ETRS 89 WITH LABELS FROM WGS84
# 
# 
# 
# > plot(GridB)
# 
# 
# > GridB
#          coordinates          labels pos offset
# 1 (4194850, 2023090)  8.5 *degree *E   1    0.5
# 2 (4236900, 2022390)    9 *degree *E   1    0.5
# 3 (4278950, 2021980)  9.5 *degree *E   1    0.5
# 4 (4171720, 2046030) 41.5 *degree *N   2    0.5
# 5 (4172940, 2101340)   42 *degree *N   2    0.5
# 6 (4174150, 2156680) 42.5 *degree *N   2    0.5
# 7 (4175380, 2212040)   43 *degree *N   2    0.5
# 
# > text(coordinates(GridB), labels=parse(text=GridB$labels))   /// PLOT LABELS WGS 84 WITH ETRS89 points
# 
# > plot(R1, add=TRUE)
# 
# 
# 
# 
# /////// AJOUT GRID 84 projected in 89
# 
# 
# 
# > grid84<-gridlines(C84)
# > grid84_89<-spTransform(grid84,CRS("+init=epsg:3035"))
# > plot(grid84_89, add=TRUE)
# > plot(R1)
# > plot(grid84_89, add=TRUE)
# > labels=parse(text=GridB$labels)
# > text(coordinates(GridB), labels=parse(text=GridB$labels)) 


