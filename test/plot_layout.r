library("ggplot2")
library("reshape2")
library("RColorBrewer")
# 
# #  rm(list=ls())
# # load("data/clim.R")
# #  map<-cru_raster_10min_window$val[,,2]
# #  Longitude<<-cru_raster_10min_window$lon
# #  Latitude<<-cru_raster_10min_window$lat
#  
# #  
# # bitmap("test.jpeg",pointsize=19,width=297,height=210,res=120,type="jpeg",unit="mm")
# #  width<-par()$fin[1]
# # mar<-par("mar")
# # mar[1]<-5.1#south
# # mar[2]<-4.1#west
# # mar[3]<-4.1#north
# # mar[4]<-6.1#east
# # par("mar"=mar)
# # image(Longitude,Latitude,map)
# # title("Monthly Mean Surface Temperature of  [BCC-CSM1-1]")
# # #  par(usr=c(0,1,0,1))
# # # par(oldpar)
# # width<-par()$fin[1]
# # print(width)
# # mar[2]<-width+mar[2]+mar[4]/3#west 
# # mar[4]<-mar[4]/3
# #   current_mai<-par("mai")
# # par(new=TRUE, plt=c(0,1,0,1), 
# #  	  mai=c(current_mai[1], par("fin")[1]-par("mai")[4]+0.1,current_mai[3],current_mai[4]-0.1),
# #  	  usr=c(0,1,0,1))#,mgp = c(0.1, 0.3, 0))#,cex.axis=0.35)
# # 
# # # 
# # # par(new=TRUE, plt=c(0,1,0,1),mar=mar, usr=c(0,1,0,1))
# # 
# # # par("mar"=mar)
# # # print(par()$mar)
# # 
# # 
# # 
# #  image(matrix(c(1,10),1,10))
# #  title("x")
# # #  par(new=TRUE, mar=c(0,0,0,0))
# # #   image(matrix(c(1,10),1,10))
# #   dev.off()
# #   
#   
#   
#   
#   nRow <- 9
# nCol <- 16
#  
# myData <- matrix(rnorm(nRow * nCol), ncol = nCol)
# rownames(myData) <- letters[1:nRow]
# colnames(myData) <- LETTERS[1:nCol]
#  
# # Replace with numbers that actually have a relationship:
# for(ii in 2:ncol(myData)){ myData[, ii] <- myData[, ii-1] + rnorm(nrow(myData)) }
# for(ii in 2:nrow(myData)){ myData[ii, ] <- myData[ii-1, ] + rnorm(ncol(myData)) }
#  
# # For melt() to work seamlessly, myData has to be a matrix.
# longData <- melt(myData)
# head(longData, 20)
#  
# # Optionally, reorder both the row and column variables in any order
# # Here, they are sorted by mean value
# longData$Var1 <- factor(longData$Var1, names(sort(with(longData, by(value, Var1, mean)))))
# longData$Var2 <- factor(longData$Var2, names(sort(with(longData, by(value, Var2, mean)))))
#  
# # Define palette
# myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
#  
# zp1 <- ggplot(longData,
# aes(x = Var2, y = Var1, fill = value))
# zp1 <- zp1 + geom_tile()
#  zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
#  zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
#  zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
#  zp1 <- zp1 + coord_equal()
#  zp1 <- zp1 + theme_bw()
# # print(zp1) # Your plot will look different, depending on the seed




