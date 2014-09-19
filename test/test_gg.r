library(tikzDevice)
 library(raster)
library(rgdal)
library(ggplot2)
library(RColorBrewer)
library("grid") 
load("../data/clim.R")
#  map<-cru_raster_10min_window$val[,,2]
#  Longitude<<-cru_raster_10min_window$lon
#  Latitude<<-cru_raster_10min_window$lat
# x<-vector()
# y<-vector()
# z<-vector()
# k<-1
#  for(i in 1:length(Longitude)){
# 	 for(j in 1: length(Latitude)){
# 		 z[k]<-map[i,j]
# 		 x[k]<-Longitude[i]
# 		 y[k]<-Latitude[j]
# 		 k<-k+1
# 		 }
# 	 }
# 	map<-data.frame(lon=x,lat=y,val=z)
# 	map<-map[which(!is.na(map$val)),]
	 
# read shapefile
wmap <- readOGR(dsn="ne_110m_land", layer="ne_110m_land")
# convert to dataframe
wmap_df <- fortify(wmap)

# # create a blank ggplot theme
theme_opts <- list(theme(
			panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_rect(color = "black",fill=NA,  size = 0.4, linetype = "solid"),
                        #panel.border=element_rect(color = "black",fill=NA,  size = 0.4, linetype = "solid"),
                        plot.background = element_blank(),
                        #plot.background=element_blank(),
                        #panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        panel.margin = unit(5,"lines"),
                        #axis.title.x = element_blank(),
                        #axis.title.y = element_blank(),
                        plot.title = element_text()))
# 
# # plot map
a<-ggplot(wmap_df, aes(long,lat, group=group)) +   geom_polygon() +   labs(title="World map (longlat)") + 
  coord_equal() +   theme_opts

ggsave("map1.png",  width=12.5, height=8.25, dpi=72) 


wmap_robin <- spTransform(wmap, CRS("+proj=robin"))
wmap_df_robin <- fortify(wmap_robin)
ggplot(wmap_df_robin, aes(long,lat, group=group)) + 
  geom_polygon() + 
  labs(title="World map (robinson)") + 
  coord_equal() +
  theme_opts

ggsave("map2.png", width=12.5, height=8.25, dpi=72)

ggplot(wmap_df_robin, aes(long,lat, group=group, fill=hole)) +
  geom_polygon() + 
  labs(title="World map (robin)") +
  coord_equal() + 
  theme_opts
ggsave("map3.png", width=12.5, height=8.25, dpi=72) 


# 
# ggplot(wmap_df_robin, aes(long,lat, group=group, fill=hole)) + 
#   geom_polygon() + 
#   labs(title="World map (Robinson)") + 
#   coord_equal() + 
#   theme_opts +
#   scale_fill_manual(values=c("#262626", "#e6e8ed"), guide="none") # change colors & remove legend
# 
# ggsave("map4.png", width=12.5, height=8.25, dpi=72) 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
grat <- readOGR("ne_110m_graticules_all", layer="ne_110m_graticules_15") 
grat_df <- fortify(grat)

bbox <- readOGR("ne_110m_graticules_all", layer="ne_110m_wgs84_bounding_box") 
bbox_df<- fortify(bbox)
# 
# a<-ggplot(bbox_df, aes(long,lat, group=group)) + 
#   geom_polygon(fill="white") +
#   geom_polygon(data=wmap_df, aes(long,lat, group=group, fill=hole)) + 
#   geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
#   labs(title="World map + graticule (longlat)") + 
#   coord_equal()  +
#    theme_opts +
#    scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend
# 
# #ggsave("map5.png", width=12.5, height=8.25, dpi=72) 
# # 
# # graticule (Robin)
grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(grat_robin)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)
# 
# ggplot(bbox_robin_df, aes(long,lat, group=group)) + 
#   geom_polygon(fill="white") +
#   geom_polygon(data=wmap_df_robin, aes(long,lat, group=group, fill=hole)) + 
#   geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
#   labs(title="World map (Robinson)") + 
#   coord_equal() + 
#   theme_opts +
#   scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend
# 
# ggsave("map6.png", width=12.5, height=8.25, dpi=72) 
# 
# 
# 
# 
# # add country borders
# countries <- readOGR("ne_110m_admin_0_countries", layer="ne_110m_admin_0_countries") 
# countries_robin <- spTransform(countries, CRS("+proj=robin"))
# countries_robin_df <- fortify(countries_robin)
# 
# ggplot(bbox_robin_df, aes(long,lat, group=group)) + 
#   geom_polygon(fill="white") +
#     theme_opts +
#   geom_polygon(data=countries_robin_df, aes(long,lat, group=group, fill=hole)) + 
#   geom_path(data=countries_robin_df, aes(long,lat, group=group, fill=hole), color="white", size=0.3) +
#   geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
#   labs(title="World map (Robinson)") + 
#   coord_equal() + xlim(-2e6,5e6)	   +    ylim(2e6,7e6)+
#   scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend
# 
 myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
# #  
# # zp1 <- ggplot(longData,
# # aes(x = Var2, y = Var1, fill = value))
# # zp1 <- zp1 + geom_tile()
# #  zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))

#===============
#scale bottom (a)
#===============
# plot.title = "General Circulation Models Climate Data Output"
# plot.subtitle = 'created by GCM runs'
# 
# a<-ggplot(map,aes(lon,lat,fill=val))+
# geom_raster(hjust = 0, vjust = 0)+
# scale_fill_gradientn(colours = myPalette(100),breaks=c(-15,-10, -5,10,5, 0,5,15),
# 		     guide= guide_colorbar(title=expression(Surface~Temperature~degree~C), title.position="top",
# 			     barwidth = 25, barheight = 1,nbin=100, 
# 			     draw.ulim = FALSE, draw.llim = FALSE ))+
# #geom_path(data=wmap_df,aes(long,lat,group=group,fill=NULL))+
# #coord_cartesian()+coord_map()+
# geom_path(data=grat_df,aes(long,lat,group=group,fill=NULL),linetype="dashed", color="grey50")+
# geom_path(data=countries,aes(long,lat,goup=group,fill=NULL))+
# xlim(min(map$lon),max(map$lon))+ylim(min(map$lat),max(map$lat))+
# theme_opts+theme(legend.position="bottom", legend.background = element_rect(color = "black", 
#     fill = "grey90", size = 0.4, linetype = "solid"))+
# coord_equal()+
# ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
# labs( x = "", y="")
# # #labs(title="Mean Surface Temperature ",x="GCM")#+
# # #scale_x_discrete(grat@data$display)
# # #expression(Depth[mm])
# #  ggsave("map10.png",width=12.5,height=6,dpi=72)
# #===============
# #scale right (b)
# #===============
# b<-ggplot(map,aes(lon,lat,fill=val))+
# geom_raster(hjust = 0, vjust = 0)+
# scale_fill_gradientn(colours = myPalette(100),breaks=c(-15,-10, -5,10,5, 0,5,15),
# 		     guide= guide_colorbar(title=expression(degree~C), title.position="top",
# 			     barwidth = 1, barheight = 15,#nbin=100, 
# 			     draw.ulim = FALSE, draw.llim = FALSE ))+
# #geom_path(data=wmap_df,aes(long,lat,group=group,fill=NULL))+
# #coord_cartesian()+coord_map()+
# geom_path(data=grat_df,aes(long,lat,group=group,fill=NULL),linetype="dashed", color="grey50")+
# geom_path(data=countries,aes(long,lat,goup=group,fill=NULL))+
# xlim(min(map$lon),max(map$lon))+ylim(min(map$lat),max(map$lat))+
# theme_opts+theme(legend.position="right", legend.background = element_rect(color = "black", 
#     fill = "white", size = 0.4, linetype = "solid"))+
# coord_equal()+
# ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
# labs( x = "", y="")
# #labs(title="Mean Surface Temperature ",x="GCM")#+
# #scale_x_discrete(grat@data$display)
# #expression(Depth[mm])
# # ggsave("map10.png",width=12.5,height=6,dpi=72)
# #===============
# #scale inside (c)
# #===============
# plot.title = "General Circulation Models Climate Data Output"
# plot.subtitle = 'created by GCM runs'
# 
# c<-ggplot(map,aes(lon,lat,fill=val))+
# geom_raster(hjust = 0, vjust = 0)+
# scale_fill_gradientn(colours = myPalette(100),breaks=c(-15,-10, -5,10,5, 0,5,15),
# 		     guide= guide_colorbar(title=expression(degree~C), title.position="top",
# 			 #    barwidth = 25, barheight = 1,nbin=100, 
# 			     draw.ulim = FALSE, draw.llim = FALSE ))+
# #geom_path(data=wmap_df,aes(long,lat,group=group,fill=NULL))+
# #coord_cartesian()+coord_map()+
# geom_path(data=grat_df,aes(long,lat,group=group,fill=NULL),linetype="dashed", color="grey50")+
# geom_path(data=countries,aes(long,lat,goup=group,fill=NULL))+
# xlim(min(map$lon),max(map$lon))+ylim(min(map$lat),max(map$lat))+
# theme_opts+theme(legend.position=c(0.9,0.3), legend.background = element_rect(color = "black", 
#     fill = "grey90", size = 0.4, linetype = "solid"))+
# coord_equal()+
# ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
# labs( x = "", y="")

#===============
#scale inside (d)
#===============
# plot.title = "GCM Surface Temperature"
# plot.subtitle = 'created by GCM runs'
# # plot.title = ""
# # plot.subtitle = ''
# # mr<-fortify(map_robin)
# d<-ggplot(map,aes(long,lat,fill=group))+
# geom_tile(hjust = 0, vjust = 0)+
# scale_fill_gradientn(colours= myPalette(10),breaks=c(15,5,0,-5,-15),
# 		     guide= guide_legend(title=expression(degree~C), title.position="top",
# 			 #    barwidth = 25, barheight = 1,nbin=100, 
# 			     draw.ulim = FALSE, draw.llim = FALSE ))+
# 
# #scale_color_manual(values=myPalette(1000))+
# # scale_fill_brewer(palette="Spectral")+
# #geom_path(data=wmap_df,aes(long,lat,group=group,fill=NULL))+
# #coord_cartesian()+coord_map()+
# #geom_path(data=grat_df,aes(long,lat,group=group,fill=NULL),linetype="dashed", color="grey50")+
# #geom_path(data=countries,aes(long,lat,goup=group,fill=NULL))+
# xlim(min(map$lon),max(map$lon))+ylim(min(map$lat),max(map$lat))+
# theme_opts+theme(legend.position=c(0.9,0.3), legend.background = element_rect(color = "black", 
#     fill = "grey90", size = 0.4, linetype = "solid"))+
# coord_equal()+
# ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
# labs( x = "", y="")
#  ggsave("map10.png",width=12.5,height=6,dpi=72)
 
 
# names(map)<-c("long","lat","val")
# grid<-expand.grid(lon=unique(map$lon),lat=unique(map$lat))
# val<-array(NA,dim(grid)[1] )
# for(i in 1:dim(map)[1]){
# 	val[i]<-map$val[which(grid$lon==map$lon[i]&grid$lat==map$lat[i])]
# }	
##  coordinates (map)= ~long+lat
 projection(map)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# k<-spTransform(map,CRS("+proj=robin"))
# 
# cor<-as.data.frame(k@coords)
# pp<-data.frame("long"=cor$x,"lat"=cor$y,"val"=k@data)
# 
# ggplot(pp,aes(long,lat,fill=z))+geom_tile()#+scale_fill_gradientn(colours= myPalette(10))
#  

coordinates (map)= ~long+lat
projection(map)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
pp_robin<-spTransform(pp,CRS("+proj=robin"))
a<-data.frame(pp_robin@coords,pp_robin@data)


map_pj_c$lat<-round(map_pj$lat/10000)


# s100 <- matrix(c(267573.9, 2633781, 213.29545, 262224.4, 2633781, 69.78261, 263742.7, 2633781, 51.21951, 259328.4, 2633781, 301.98413, 264109.8, 2633781, 141.72414, 255094.8, 2633781, 88.90244),  ncol=3,  byrow=TRUE)
# colnames(s100) <- c('X', 'Y', 'Z')
# 
# library(raster)
# # set up an 'empty' raster, here via an extent object derived from your data
# e <- extent(s100[,1:2])
# e <- e + 1000 # add this as all y's are the same
# 
# r <- raster(e, ncol=10, nrow=2)
# # or r <- raster(xmn=, xmx=,  ...
# 
# # you need to provide a function 'fun' for when there are multiple points per cell
# x <- rasterize(s100[, 1:2], r, s100[,3], fun=mean)
# plot(x)