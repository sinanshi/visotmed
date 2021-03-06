 
 setClass("shapefile",representation(
	 dir="character",
	 layer="character",
	 proj_from="character",
	 proj_to="character"),
	 prototype=list(
		 proj_from="NA",
		 proj_to="NA"))
 
 setGeneric("polygonConvert",function(obj){
	 cat(class(obj)," is not available\n")})

 
setMethod("polygonConvert", signature(obj="shapefile"),
	function(obj){
		cat("read polygons:\n----\n")
		shape_file<-readOGR(obj@dir,obj@layer)
		if(obj@proj_from!="NA") 
			projection(shape_file)<-obj@proj_from
		if(obj@proj_to!="NA"){ 
			cat("transform polygons projection:\n----\n")
			shape_file<-spTransform(shape_file,CRS(obj@proj_to))
		}
		
		dataframe<-fortify(shape_file)
		return(dataframe)
	})




	
 setGeneric("addLines",function(obj,gg,...){
	 cat(class(obj)," is not available\n")})
 
 
 setMethod("addLines", signature(obj="shapefile"),
	function(obj,gg,...){
		cat("add polygons\n====\n")
		dataframe<-polygonConvert(obj)
		g2<-gg+geom_path(data=dataframe,aes(long,lat,group=group),...)
		return(g2)
		
	})
	
	
	
 setGeneric("addPoly",function(obj,gg,...){
	 cat(class(obj)," is not available\n")})
 
 
 setMethod("addPoly", signature(obj="shapefile"),
	function(obj,gg,...){
		cat("add polygons\n====\n")
		dataframe<-polygonConvert(obj)
		g2<-gg+geom_polygon(data=dataframe,aes(long,lat,group=group),...)
		return(g2)
		
	})


getShpAttrib<-function(obj,slot=c("labpt","area"),att){
	if(!inherits(obj,"shapefile"))
		stop("obj has to be shapefile class.\n")
	shape_file<-readOGR(obj@dir,obj@layer)
	slots<-slotNames(shape_file@polygons[[1]])
	if(length(intersect(slots,slot))!=length(slot)) 
		stop("shape file does not contain all selected slots\n")
	
	sv<-list()
	for(i in 1:length(shape_file@polygons)){
		for(j in 1:length(slot)){
			sv[[slot[j]]]<-rbind(sv[[slot[j]]],slot(shape_file@polygons[[i]],slot[j]))
			
		}

	}
	for(i in att)
	sv[[i]]<-shape_file[[i]]
	
	
	return(as.data.frame(sv))
}		
		
	
	
	
	
