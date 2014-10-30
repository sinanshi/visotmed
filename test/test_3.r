 source("R/header.r")


theme_blank <- list(theme(
			panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background=element_blank(),
#                        panel.background = element_rect(color = "black",fill=NA,  size = .5, linetype = "solid"),
                        #panel.border=element_rect(color = "black",fill=NA,  size = 0.4, linetype = "solid"),
                        plot.background = element_blank(),
                        plot.background=element_blank(),
 						panel.border=element_blank(),
 #                       panel.border=element_rect(color = "black",fill=NA,  size = .5, linetype = "solid"),
                        legend.position="right",
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        panel.margin = unit(5,"lines"),
                        panel.margin = unit(0,"null"),
                        plot.margin = rep(unit(0,"null"),4),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text()))




 country<-new("shapefile",dir="data/admin_countries/110m/",layer="ne_110m_admin_0_countries",
 	     proj_to= to)
g<-ggplot()
w_h<-function(lim){
	return(abs((lim[2]-lim[1])/(lim[3]-lim[4])))
}
a<-readOGR("data/shape/110m_cultural/","ne_110m_populated_places")
a<-spTransform(a,CRS(to))
pp<-as.data.frame(cbind(a@coords,a$POP2010,a$MIN_AREAKM))
d<-addPoly(country,gg=g,size=0.1,color="black",fill="grey")
lines_proj<-graticules(20,10,to)
lines<-lines2dataframe(lines_proj)
#p<-gratText(lines_proj)

names(pp)<-c("lon","lat","pop","area")

dd<-d+geom_point(data=pp,aes(lon,lat,size=pop,color=area),alpha=0.6)+
scale_colour_gradient(low = "blue",high="red")+
scale_size_continuous(range=c(0,10))+theme_opts+coord_equal()+
ggtitle(bquote("World Metropolitan Population and Area in 2010"))+
geom_path(data=lines,aes(lon,as.numeric(lat),group=group),color="black",size=0.2)+
#scale_x_continuous(expand = c(0, 0),breaks=as.vector(p$x[,1]),labels=bquote(.(as.vector(p$x$txt))))+
#scale_y_continuous(expand = c(0, 0),breaks=as.vector(p$y[,1]), labels=p$y[,2])+

guides(size=guide_legend("K Habitants",title.position="top",direction="vertical",label.position="right"),color=guide_colorbar(title="Area KM²",direction="vertical"))+
theme_blank



saveMap("point_pj.jpeg",plot=dd,width=7,height=4,dpi=600)




f<-getShpAttrib(country,slot=c("labpt"),"pop_est")
names(f)<-c("lon","lat","pop")
