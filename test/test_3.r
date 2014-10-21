 source("R/header.r")
country<-new("shapefile",dir="data/admin_countries/110m/",layer="ne_110m_admin_0_countries",
	     proj_to= "+proj=laea +lat_0=52   +lon_0=10      +x_0=3 210 000    +y_0=4 321 000 ")
# country<-new("shapefile",dir="data/admin_countries/110m/",layer="ne_110m_admin_0_countries")
	 #   proj_to= "+proj=laea +lat_0=52   +lon_0=10      +x_0=3 210 000    +y_0=4 321 000 ")
  l<-polygonConvert(country)
#  ll<-l[l$long> -10& l$long< 30 & l$lat<40 & l$lat>20,]
#  lg<-as.vector(unique(ll$group))
#  for(i in 1:length(lg) ){
# 	 long<-ll$long[ll$group==lg[i]]
	 
d<-addLines(country,gg=g)


