
multi.polygon<-function(poly, detail=1){
	group<-as.numeric(poly[,"group"])
	group_id<-unique(group)[1:as.integer(length(unique(group))*detail)]
	for(i in  group_id){
		id_range<-which(group==i)
		id_start<-min(id_range)
		id_end<-max(id_range)
		id<-seq(id_start,id_end)
		polygon(poly[id,"long"], poly[id, "lat"])
		}
	}