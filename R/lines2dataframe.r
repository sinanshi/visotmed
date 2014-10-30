 
#==========
#convert sp lines to dataframe
#==========
  lines2dataframe<-function(lines){
	  a<-data.frame()
	  k<-1
	  for(i in 1:length(lines)){
		  for(j in 1:length(lines@lines[[i]]@Lines)){
			  l<-data.frame(lines@lines[[i]]@Lines[[j]]@coords,
						rep(k,length(lines@lines[[i]]@Lines[[j]]@coords[,1])),
						rep(lines@lines[[i]]@ID,length(lines@lines[[i]]@Lines[[j]]@coords[,1])))
			  
			  a<-rbind(a,l)			  
			  k<-k+1
		}
	}
						
	names(a)<-c("lon","lat","group","id")
	return(a)
}