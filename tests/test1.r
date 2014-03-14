source("R/init.r")

load("tests/tmp")
load("tests/wheat")




#-------------------------------------------------- 
#-------------------------------------------------- 


WEST<--179.75
SOUTH<- -55.75
 EAST<- 179.75
 NORTH<- 83.75
 RES<-0.5
Longitude<<-seq(WEST,EAST,RES)
Latitude<<-seq(SOUTH,NORTH,RES) 


 vMap<-new("visoMap")
 vMap@MapMatrix<-map_tmp
 vMap@Long<-Longitude
 vMap@Lat<-Latitude
 vMap@scale<-0.01
 vMap<-visoMap.organise(vMap)

vPlotPar<-new("visoPlotPar")
 vPlotPar@ImageName<-"test2"
 vPlotPar@title<-"Surface Temperature"
 
 
 vMultiLayout<-new("visoMultiLayout")
 vMultiLayout@mfrow<-c(2,2)
 
 
 vGridOption<-new("visoGridPar")
  vPlotOption<-new("visoPlotOptions")
  vPlotOption@width=30
  vPlotOption@height=16
 vPlotOption@cex.axis=1
 vPlotOption@cex.main=1.2
 #vPlotOption@cex.title=1.2
 
 
 
lMap<-list(vMap,vMap,vMap,vMap)
lPlotPar<-list(vPlotPar,vPlotPar,vPlotPar,vPlotPar)
lPlotOption<-list(vPlotOption,vPlotOption,vPlotOption,vPlotOption)
lGridOption<-list(vGridOption,vGridOption,vGridOption,vGridOption)

lMap[[3]]@MapMatrix<-map_wheat 
lPlotPar[[1]]@title<- "(a) Surface Temperature 1"
lPlotPar[[2]]@title<- "(b) Surface Temperature 2"
lPlotPar[[3]]@title<- "(c) Surface Temperature 3"
lPlotPar[[4]]@title<- "(d) Surface Temperature 4"
 
 
 
bitmap(paste(vPlotPar@ImageName,".",vPlotPar@ImageType,sep=""),type=vPlotPar@ImageType,onefile=FALSE,
	 height=vPlotOption@height,width=vPlotOption@width,pointsize=vPlotOption@pointsize,res=vPlotOption@res)
# visoImageSingle(vMap,vPlotPar,vPlotOption,vGridOption)

  visoImageMulti(lMap,lPlotPar, lPlotOption,lGridOption, LLine=NULL, LPoly=NULL, LPoint=NULL,vMultiLayout)


#  putOTMedText()
 putOTMedLogo(vPlotOption)
  dev.off()
# 
# 
# 
# 
# 
# jordan<-readShapePoly("~/Desktop/shp/jrd")
#  jm<-fortify(jordan)
# 
# xlim_=c(-15,50)
# ylim_=c(20,60)
# #op<-par(mar=c(5.1,4.1,4.1,7.1))
# polygon(jm[,1],jm[,2],col=3,border=10)
#   #par(new=TRUE, plt=c(0,1,0,1), mar=c(0,0,0,0), usr=c(0,1,0,1))
# 
# #  a=max(which(cm[,5]==1))
# # polygon(cm[1:a,1],cm[1:a,2],col=3,border=1)
#  
# # multi.polygon<-function(poly, detail=1){
# # 	group<-as.numeric(poly[,"group"])
# # 	group_id<-unique(group)[1:as.integer(length(unique(group))*detail)]
# # 	for(i in  group_id){
# # 		id_range<-which(group==i)
# # 		id_start<-min(id_range)
# # 		id_end<-max(id_range)
# # 		id<-seq(id_start,id_end)
# # 		polygon(poly[id,"long"], poly[id, "lat"])
# # 		}
# # 	}
# 	
# 	