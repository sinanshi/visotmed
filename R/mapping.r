 
definePlotBoundary<-function(gg,lim=NULL){
	if(is.null(lim))
		lim<-c(min(g$data$lon),max(g$data$lon),min(g$data$lat),max(g$data$lat))

	gg<-gg+ scale_x_continuous(limits = c(lim[1],lim[2]), expand = c(0, 0))
	gg<-gg+ scale_y_continuous(limits = c(lim[3],lim[4]), expand = c(0, 0))
	return(gg)		
}

addGraticules<-function(gg,x=10,y=10,proj,plot.line=TRUE,plot.text=TRUE,color="grey", size=0.25,...){
	lines_proj<-graticules(x,y,proj=proj)
	lines<-lines2dataframe(lines_proj)
	if(plot.line==TRUE){
		gg<-gg+geom_path(data=lines,aes(lon,as.numeric(lat),group=group),colour=color,
			size=size,...)
	}
	lim<-c(min(gg$data$lon),max(gg$data$lon),min(gg$data$lat),max(gg$data$lat))
	p<-gratText(lines_proj,lim)
	if(plot.text==TRUE){
		gg<-gg+theme(axis.text.x = element_text(),
					axis.text.y = element_text(),
					axis.ticks = element_line())
		gg<-gg+scale_x_continuous(limits = c(lim[1],lim[2]), expand = c(0, 0),
			    breaks=as.vector(p$x[,1]),labels=p$x$txt)
		gg<-gg+scale_y_continuous(limits = c(lim[3],lim[4]), expand = c(0, 0),
			    breaks=as.vector(p$y[,1]), labels=p$y[,2])
	}else	gg<-definePlotBoundary(gg)
	
	return(gg)
}