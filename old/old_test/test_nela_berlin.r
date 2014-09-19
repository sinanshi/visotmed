load("temp.Rdata")
library(maps)
library(fields)
library(maps)
library(maptools) 
library(jpeg)
library(biOps)
COLS <- colorRampPalette(c("navy","blue","skyblue","grey","orange","red","red4"))
Longitude<<-seq(-17.75,51.75,0.5)
Latitude<<-seq(-3.75,52.25,0.5)




 putOTMedLogo<-function(){#This function should always put at the end of all processes
	 
	 par(new=TRUE, plt=c(0,1,0,1), usr=c(0,1,0,1), mfrow=c(1,1),
      #mai=c(0,vPO@width*0.94,0.97*vPO@height, 0.01*vPO@height))# on the top
      #mai=c(0.97*vPO@height,vPO@width*0.94,0, 0.01*vPO@height))# on the bottom
         mai=c(0.97*7,11.32*0.94,0, 0.01*11.32))# on the bottom
          
	 plot.new()
	 image <- readJpeg("../data/logo_otmed.jpg")
	 plot(image)
}
 
 

mapValUnique<-function(vMap){
	value_u<-t(sort(unique(as.vector(vMap))))
	return(value_u)
	}
 
 
 setColour<-function(vMap,col){
	 cols<-COLS(length(t(mapValUnique(vMap))))
	 return(cols)
	 }
 
 visoColScale<-function(map.matrix,colour){
 	  current_mai<-par("mai")
          par(new=TRUE, #plt=c(0,1,0,1), 
 	  mai=c(current_mai[1]+0.2, par("fin")[1]-par("mai")[4]+0.4,current_mai[2]+0.2,current_mai[4]-0.7),
 	  usr=c(0,1,0,1))#,mgp = c(0.1, 0.3, 0))#,cex.axis=0.35)
	  plot.new()
	  map_unique<-mapValUnique(map.matrix)
	  image(map_unique, col=colour,xaxt="n",yaxt="n")
	  mtext("[°C]", side=3, adj=-3.5, line=0.5, cex=0.7, font=1)
	  label<-round(seq(min(map_unique),max(map_unique),length=7),digits=0)
	  label[7]<-">25"
	  axis(4,at=seq(0,1,length=7),labels=label,las=1,tck=1,line=0,cex.axis=0.7)
	 }


 visoImage<-function(map.matrix){
	  bitmap("out.jpeg",type="jpeg", height=7,width=11.32,pointsize=24,res=280)
	  op<-par(cex.axis=.7,cex.main=1,mgp = c(3, .1, 0))
	  fin<-par("fin")
	  par(mai=c(0.1*fin[2],0.1*fin[1],0.15*fin[2],0.15*fin[1]))
	 #default colour as as same levels as data had
	 image(Longitude,Latitude, map.matrix,xlim=c(10,45),ylim=c(27,46))
	 rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="lightskyblue")
         map(add=T,fill=T,col="grey",boundary=FALSE)
         map.matrix[which(map.matrix>25)]<-25
         colour<-setColour(map.matrix,col)
	 image(Longitude,Latitude, map.matrix,add=T,axes=T,xlab=F,ylab=F,col=colour,xlim=c(10,45),ylim=c(27,46))
         map(add=T)
         axis(1,las=1,tck=-0.03)
 	 axis(2,las=3,tck=-0.03)
 	 grid(col="black")
 	 title("LPJmL Input Temperature")
 	 visoColScale(map.matrix,colour)
 	 putOTMedLogo()

 	 dev.off()
 	 
 	 }

	 
	 