 library(maptools)
 library(maps)
 departements<-readShapeSpatial("DEPARTEMENT.SHP")
 region<-tapply(baseFREQ[,"nbre"],
 as.factor(baseFREQ[,"region"]),sum)/
 tapply(baseFREQ[,"exposition"],
 as.factor(baseFREQ[,"region"]),sum)
 depFREQ=rep(NA,nrow(departements))
 names(depFREQ)=as.character(
 departements$CODE_REG)
 for(nom in names(region)){
 depFREQ[names(depFREQ)==nom] =
 region[nom]}
 plot(departements,col=gray((depFREQ-.05)*20))
 legend(166963,6561753,legend=seq(1,0,by=-.1)/20+.05,
 fill=gray(seq(1,0,by=-.1)),cex=1.25, bty="n")