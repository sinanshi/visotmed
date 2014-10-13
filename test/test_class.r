 
pts<-list("x"=c(1:10),"y"=c(5:15))
class(pts)="my"
my<-function(x, y){
	pts<-list("x"=x,"y"=y)
	class(pts)<-"my"
	pts
}


print.my<-function(my){
	cat(my$x, my$y,"-\n")
}


setClass("mapstd",representation(
	lon="numeric",
	lat="numeric",
	val="array"))