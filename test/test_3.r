 source("R/header.r")
 country<-new("shapefile",dir="data/admin_countries/110m/",layer="ne_110m_admin_0_countries",
 	     proj_to= "+proj=laea +lat_0=52   +lon_0=10      +x_0=3 210 000    +y_0=4 321 000 ")

d<-addLines(country,gg=g)


