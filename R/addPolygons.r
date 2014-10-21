 
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
		dataframe<-dataframe[dataframe$long<max(g$data$lon), ]
		dataframe<-dataframe[dataframe$long>min(g$data$lon), ]
		dataframe<-dataframe[dataframe$lat>min(g$data$lat), ]
		dataframe<-dataframe[dataframe$lat<max(g$data$lat), ]
		g2<-gg+geom_path(data=dataframe,aes(long,lat,group=group),...)
		return(g2)
		
	})
	
