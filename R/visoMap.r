 #
 #
 #
 #
 
 setClass("visoMap",
	  representation(
		  MapMatrix="matrix",
		  Long="numeric",
		  Lat="numeric",
		  projection="character",
		  scale="numeric"),
	  prototype=list(
		  scale=1))
	visoMap.organise<-function(vMap){
		vMap@MapMatrix<-vMap@MapMatrix*vMap@scale
		return(vMap)
		}

setClass("visoPlotPar",
	 representation(
		  ImageName="character",
		  ImageType="character",
		  title="character"
		),
	  prototype = list(
		  ImageName=character(),
		  ImageType="jpeg"
		  ))
 
setClass("visoPlotOptions",
	  representation(
		  color.style="character",
		  height="numeric",
		  width="numeric",
		  pointsize="numeric",
		  res="numeric",
		  cex.axis="numeric",#size of axis 
		  cex.main="numeric",#size of title
		  color.levels="numeric",
		  colorscale.precision="numeric",
		  colorscale.tickle="numeric",
		  mai="numeric"),
	    prototype = list(
		  height=7,
		  width=11.32,
		  pointsize=24,
		  res=300,
		  cex.axis=0.5,
		  cex.main=1,
		  colorscale.precision=1,
		  colorscale.tickle=7,
		  color.levels=NULL,
		  mai=c(1.5,1.5,1.5,1.5)))
 
 setClass("visoGridPar",
	    representation(
		    col="character",
		    lty="numeric"
	),
	    prototype=list(
		    col="grey13",
		    lty=2))
setClass("visoMultiLayout",
	 representation(
		 mfrow="numeric",
		 layout="numeric"),
	 prototype=list(
		 layout=NULL
		 ))
 
 putOTMedText<-function(){#This function should always put at the end of all processes
	 par(new=TRUE, plt=c(0,1,0,1), mai=c(0,0,0,0), usr=c(0,1,0,1))
	 plot.new()
	 text(0.98,0,"Labex:OT-Med",font=3, cex=0.4)
	 }
 
 putOTMedLogo<-function(vPO){#This function should always put at the end of all processes
	 par(new=TRUE, plt=c(0,1,0,1), usr=c(0,1,0,1),
                   mai=c(0,vPO@width*0.94,0.97*vPO@height, 0.01*vPO@height))# on the top
                  #mai=c(0.97*vPO@height,vPO@width*0.94,0, 0.01*vPO@height))# on the bottom
	 plot.new()
	 image <- readJpeg("data/logo_otmed.jpg")
	 plot(image)
	 
	 }
 

mapValUnique<-function(vMap){
	value_u<-t(sort(unique(as.vector(vMap@MapMatrix))))
	return(value_u)
	}
 
 
 setColour<-function(vMap,col){
	 cols<-COLS2(mapValUnique(vMap))
	 return(cols)
	 }
 
 visoPutPoly<-function(vPoly){}
 visoPutLines<-function(vLine){}
visoPutPoints<-function(vPoint){}


 visoColScale<-function(vMap,vPO){
 	  current_mai<-par("mai")
          par(new=TRUE, #plt=c(0,1,0,1), 
 	  mai=c(current_mai[1]+0.2, par("fin")[1]-par("mai")[4]+0.1,current_mai[2]+0.2,current_mai[4]-0.3),
 	     usr=c(0,1,0,1),mgp = c(0.1, 0.3, 0),cex.axis=0.35)

	 print(par("pin"))
	 print(par("mai"))
	 print("--------")
        plot.new()
         map_unique<-mapValUnique(vMap)
         image(map_unique, #ylim=c(min(map_unique),max(map_unique)),
	       col=COLS(vPO@color.levels),xaxt="n",yaxt="n")
         title("[°C]",cex.main=0.5,line=0.4)
         label<-round(seq(min(map_unique),max(map_unique),length=vPO@colorscale.tickle),digits=vPO@colorscale.precision)
         axis(4,at=seq(0,1,length=vPO@colorscale.tickle),labels=label,las=1,tck=-0.3,line=0)

     }


 visoImageSingle<-function(vMap,vPP, vPO,vGP, vLine=NULL, vPoly=NULL, vPoint=NULL,... ){
	#  op<-par(mai=vPO@mai,cex.axis=vPO@cex.axis,cex.main=vPO@cex.axis,mgp = c(3, .1, 0))
	 #default colour as as same levels as data had
 	 par(new=TRUE)
	 if(is.null(vPO@color.levels)==TRUE){ 
		 vPO@color.levels<-length(mapValUnique(vMap))
		 }
	 image(vMap@MapMatrix,x=vMap@Long, y=vMap@Lat, col=COLS(vPO@color.levels),axes=F,xlab="",ylab="")#xlim=xlim_,ylim=ylim_)
	 rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="lightskyblue")
	 axis(1,las=1,tck=-0.03)
	 axis(2,las=3,tck=-0.03)
	 image(vMap@MapMatrix,x=vMap@Long, y=vMap@Lat, col=COLS(vPO@color.levels),axes=T,add=T,xlab=F,ylab=F)#, xlim=xlim_,ylim=ylim_)
         map(add=T)
         if(is.null(vPoly)==FALSE){
		 visoPutPoly(vPoly)
		 }
         if(is.null(vLine)==FALSE){
		 visoPutLines(vLine)
		 }
         if(is.null(vPoly)==FALSE){
		 visoPutPoints(vPoint)
		 }
         title(vPP@title)
         #need to be changed by proper grid ploting 
          print(par("pin"))
          print(par("mai"))
          map.grid.my(labels=F,pretty=T,col=vGP@col,lty=vGP@lty)#,lim=c(-15,50,20,60))
          #visoColScale(vMap,vPO)
         

          
          
        
 }

visoImageMulti<-function(LMAP,LPP, LPO,LGP, LLine=NULL, LPoly=NULL, LPoint=NULL,vML,...){
	count<-vML@mfrow[1]*vML@mfrow[2]
	par(mfrow=vML@mfrow)
	old.par <- par(no.readonly = TRUE)
 	for( i in 1:count){
# 		for(j in 1:2){
  		par(mfg=c(1,i,1,4))
		par(old.par)
		
 		print(par("pin"))
 		print(par("mai"))
 		visoImageSingle(vMap=LMAP[[i]], vPP=LPP[[i]], vPO=LPO[[i]],vGP=LGP[[i]],
                                 vLine=LLine[[i]], vPoly=LPoly[[i]], vPoint=LPoint[[i]])
	
	#	}
	}
		
	
	
	
	}
