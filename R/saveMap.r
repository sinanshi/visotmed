require("png")
require("grid")

saveMap<-function(filename,plot,width=8, height=5,dpi=300,LOGO=TRUE,...){
        if (!inherits(plot, "ggplot")) 
		stop("plot should be a ggplot2 plot")
	logo<-"data/logo_otmed.png"
	eps <- ps <- function(..., width, height) grDevices::postscript(..., 
				width = width, height = height, onefile = FALSE, horizontal = FALSE, 
				 paper = "special")
	tex <- function(..., width, height) grDevices::pictex(..., 
						       width = width, height = height)
	pdf <- function(..., version = "1.4") grDevices::pdf(..., 
						      version = version)
	svg <- function(...) grDevices::svg(...)
	wmf <- function(..., width, height) grDevices::win.metafile(..., 
							     width = width, height = height)
	emf <- function(..., width, height) grDevices::win.metafile(..., 
							     width = width, height = height)
	png <- function(..., width, height) grDevices::png(..., width = width, 
						    height = height, res = dpi, units = "in")
	jpg <- jpeg <- function(..., width, height) grDevices::jpeg(..., 
							     width = width, height = height, res = dpi, units = "in")
	bmp <- function(..., width, height) grDevices::bmp(..., width = width, 
						    height = height, res = dpi, units = "in")
	tiff <- function(..., width, height) grDevices::tiff(..., 
						      width = width, height = height, res = dpi, units = "in")
	default_device <- function(filename) {
		pieces <- strsplit(filename, "\\.")[[1]]
		ext <- tolower(pieces[length(pieces)])
		match.fun(ext)
	}
	
	default_device(filename)(file=filename,  width = width, height = height,...)
	size = unit(1.5, "cm")
	logo<-readPNG(logo)
	logo<- rasterGrob(logo)
	heights = unit.c(unit(1, "npc") - size,size)
	widths = unit.c(unit(1, "npc") - 1.5*size, size)
	lo = grid.layout(2, 2, widths = widths, heights = heights)
	# Position the elements within the viewports
	grid.newpage()
	pushViewport(viewport(layout = lo))

	# The plot
	pushViewport(viewport(layout.pos.row=1:2, layout.pos.col = 1:2))
	print(plot, newpage=FALSE)
	popViewport()
	
	# The logo
	if(LOGO==TRUE){
		pushViewport(viewport(layout.pos.row=2, layout.pos.col = 2))
		grid.draw(logo)
	}
	dev.off()
}






