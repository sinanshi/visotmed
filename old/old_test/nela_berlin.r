load("temp.Rdata")
library(maps)
library(fields)
library(maps)
library(maptools)
library(jpeg)
library(biOps)
COLS <- colorRampPalette(c("navy","blue","skyblue","grey","orange","red","red4"))
#COLS <- colorRampPalette(c("palegreen","yellow","lightgoldenrod","orange","tomato","red4"))
Longitude<<-seq(-17.75,51.75,0.5)
Latitude<<-seq(-3.75,52.25,0.5)

#Usage and call of 
#visoImage(
#map.matrix=matrix to be plotted, OBLIGATORY
#path="out path", OBLIGATORY
#name="name of the out file", OBLIGATORY
#unitL="units you want to have above the legend", DEFAUL=unitF
#unitF="units you want to have in the file name", do not use weird symbols,OBLIGATORY
#lim_up=values above this limit will be plotted with the last colour, DEFAUL=max(matrix)
#lim_down=values under this limit will be plotted with the first colour, it can be smaller than min(matrix), DEFAUL=min(matrix)
#time="time data you want to have in the file name",OBLIGATORY
#others="other stuff you want to have in the file name",DEFAUL=""
#title="title of the map",DEFAULT="",
#text="what you want to have in the left/down corner of the plot",
#grid=  1 for plot lat/lon lines, !=1 for no lat/lon lines, DEFAUL=1
#colrange=colors in form of COLS(100), OBLIGATORY
#decimals=number od decimal places you want to have for the numbers in the legend, DEFAUL=2


#country line for MED

PlotMedShape <- function(CowMed) {
      l=length(CowMed)
      for (c in 1:l) {
	PSP<- get(sprintf("SPDF_%s",CowMed[c]))
	plot(PSP,add=TRUE,col="transparent")
      }  
}
# Admin. Borders shape files       

# HRV Croatie, BIH Bosnie, MKD Macedonia, KO_ Kosovo, MDA Moldavia, MNE Montenegro, ESH Western Sahara

ListCountries <- c("FRA","ALB","HRV","GRC","ITA","PRT","ESP","BIH","SRB","MKD",
                   "SVN","SVK","CHE","CZE","AUT","BGR","KO_","ROU","BEL","HUN",
                   "UKR","LUX","MDA","MNE","RUS","CYP","KAZ","AZE","GEO","ARM",
                   "MLT","GBR","IRL","DEU","NLD","DNK","POL","LTU","LVA","EST",
                   "FIN","NOR","SWE","BLR","MAR","DZA","TUN","LBY","EGY","ISR",
                   "LBN","SYR","TUR","SAU","IRQ","JOR","IRN","KWT","PSE","ESH")               


# ListCountries <- c("FRA","ALB","HRV","GRC","ITA","PRT","ESP","BIH","SRB","MKD",
#                    "SVN","SVK","CHE","CZE","AUT","BGR","KO_","ROU","BEL","HUN",
#                    "UKR","LUX","MDA","MNE","RUS","CYP","KAZ","AZE","GEO","ARM",
#                    "MLT","GBR","IRL","DEU","NLD","DNK","POL","LTU","LVA","EST",
#                    "FIN","NOR","SWE","BLR","EGY","ISR","LBN","SYR","TUR", "PSE","ESH")    


#  Reading Shape Files without a function

l=length(ListCountries)
for (c in 1:l) {
      RSP<- readShapePoly(paste(sprintf("/home/sinan/OT-Med/World_adm/%s_adm/%s_adm0.shp",ListCountries[c],ListCountries[c]),sep=""))
      assign(paste(sprintf("SPDF_%s",ListCountries[c])),RSP)
}


putOTMedLogo<-function(){#This function should always put at the end of all processes

par(new=TRUE, plt=c(0,1,0,1), usr=c(0,1,0,1), mfrow=c(1,1),
      #mai=c(0,vPO@width*0.94,0.97*vPO@height, 0.01*vPO@height))# on the top
      #mai=c(0.97*vPO@height,vPO@width*0.94,0, 0.01*vPO@height))# on the bottom
         mai=c(0.97*7,11.32*0.94,0, 0.01*11.32))# on the bottom
          
plot.new()
image <- readJpeg("../data/logo_otmed.jpg")
plot(image)
}
 

visoSetColour<-function(image,col,range){
  min_val<-min(image,na.rm=T)
  max_val<-max(image,na.rm=T)
  col.length<-length(col)
  col.index<-seq(range[1],range[2],length=col.length)
  chosen.col=col[which(col.index>=min_val & col.index<=max_val)]
  return(chosen.col)
} 

mapValUnique<-function(vMap){
value_u<-t(sort(unique(as.vector(vMap))))
return(value_u)
}
 
 
setColour<-function(vMap,col){
cols<-COLS(length(t(mapValUnique(vMap))))
return(cols)
}
 
 visoColScale<-function(map.matrix,colour,unit,bigger,range,decimals){
  current_mai<-par("mai")
          par(new=TRUE, #plt=c(0,1,0,1),
  mai=c(current_mai[1]+0.2, par("fin")[1]-par("mai")[4]+0.4,current_mai[2]+0.2,current_mai[4]-0.7),
  usr=c(0,1,0,1))#,mgp = c(0.1, 0.3, 0))#,cex.axis=0.35)
plot.new()
#map_unique<-mapValUnique(map.matrix)
min_val<-min(map.matrix,na.rm=T)
max_val<-max(map.matrix,na.rm=T)
image(t(seq(range[1],range[2],length=length(colour))), col=colour,xaxt="n",yaxt="n")
#mtext(unit, side=3, adj=-3.5, line=0.5, cex=0.7, font=1)
mtext(unit, side=3, adj=0, line=0.5, cex=0.7, font=1)
label<-round(seq(range[1],range[2],length=7),digits=decimals)
label[7]<-paste(bigger,round(range[2],digits=decimals),sep="")
axis(4,at=seq(0,1,length=7),labels=label,las=1,tck=1,line=0,cex.axis=0.7)
}


visoImage<-function(map.matrix,path,name,unitL=unitF,unitF,lim_up=max(map.matrix,na.rm=TRUE),
	time,others="",title="",text="",grid=1,lim_down=min(map.matrix,na.rm=TRUE),colrange,decimals=2){
	min_val<-min(map.matrix,na.rm=TRUE)
	max_val<-max(map.matrix,na.rm=TRUE)
	
	if(lim_up<=lim_down) stop("STOP lim_down >lim_up")
	if(lim_up>max_val) stop("STOP lim_up > max map")
	
	if(max_val>lim_up||min_val<lim_down||lim_down>lim_up)
	    cat("range of the image","[",min_val,",",max_val,"]","[",lim_down,",",lim_up,"]\n")
	    
	bitmap(paste(path,name,"_",unitF,"_",time,"_",others,".jpeg",sep=""),type="jpeg", height=7,width=11.32,pointsize=24,res=280)
	op<-par(cex.axis=.7,cex.main=1,mgp = c(3, .1, 0))
	fin<-par("fin")
	
	if(lim_up==max(map.matrix,na.rm=TRUE)) bigger<-""
	if(lim_up<max(map.matrix,na.rm=TRUE)) bigger<-">"
	
	par(mai=c(0.1*fin[2],0.1*fin[1],0.15*fin[2],0.15*fin[1]))
	#default colour as as same levels as data had
	image(Longitude,Latitude, map.matrix,axes=T, xlim=c(-10,45),ylim=c(27,46))
	rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="lightskyblue")
	map(add=T,fill=T,col="grey",interior=FALSE)
	map.matrix[which(map.matrix<lim_down)]<-lim_down
	map.matrix[which(map.matrix>lim_up)]<-lim_up
	
	colour<-visoSetColour(image=map.matrix,col=colrange,range=c(lim_down,lim_up))
	
	mtext(text, side=1, adj=0, line=0.5, cex=0.7, font=1)
	image(Longitude,Latitude, map.matrix,add=T,axes=T,xlab=F,ylab=F,col=colour, xlim=c(-10,45),ylim=c(27,46))
	#map(add=T)
	PlotMedShape(ListCountries)
	axis(1,las=1,tck=-0.03)
	axis(2,las=3,tck=-0.03)
	if(grid==1) grid(col="black")
	title(title)
	visoColScale(map.matrix,colour=colrange,unit=unitL,bigger=bigger,range=c(lim_down,lim_up),decimals=decimals)
	putOTMedLogo()

  dev.off()
 
  }
 

 
 
 visoImage(map.matrix=tmp,path="",name="berlin.jpeg",unitF="°C",time="",
 title="LPJmL Ouput: Surface Temperature [Jan]",grid=1,colrange=COLS(100),lim_up=20,decimals=0)
