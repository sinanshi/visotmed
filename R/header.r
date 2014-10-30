library(RColorBrewer)
library(rgdal)
library(raster)
library(sp)
library(ggplot2)
proj_latlon<<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#source("/home/sinan/workspace/visotmed/R/getRegularGrid.r")
#source("/home/sinan/workspace/visotmed/R/getProjRaster.r")
#source("/home/sinan/workspace/visotmed/R/mapProjection.r")

source("/home/sinan/workspace/visotmed/R/mapConvert.r")
source("/home/sinan/workspace/visotmed/R/mapConvert.nc.r") 

source("/home/sinan/workspace/visotmed/R/mapProjection.r")
source("/home/sinan/workspace/visotmed/R/addPolygons.r")
source("/home/sinan/workspace/visotmed/R/saveMap.r") 
source("/home/sinan/workspace/visotmed/R/graticules.r")