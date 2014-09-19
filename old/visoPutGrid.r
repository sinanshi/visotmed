map.grid.my<-function (lim, nx = 9, ny = 9, labels = TRUE, pretty = TRUE, 
    cex = 1, col = 4, lty = 2, font = 2, ...) 
{
    pretty.range <- function(lim, ...) {
        x = pretty(lim, ...)
        if (abs(x[1] - lim[1]) > abs(x[2] - lim[1])) 
            x = x[-1]
        n = length(x)
        if (abs(x[n] - lim[2]) > abs(x[n - 1] - lim[2])) 
            x = x[-n]
        x[1] = lim[1]
        x[length(x)] = lim[2]
        x
    }
    auto.format <- function(x) {
        for (digits in 0:6) {
            s = formatC(x, digits = digits, format = "f")
            if (all(duplicated(s) == duplicated(x))) 
                break
        }
        s
    }
    if (missing(lim)) 
        lim = .map.range()
    if (is.list(lim)) {
        lim <- lim$range
    }
    if (lim[2] - lim[1] > 360) {
        lim[2] <- lim[1] + 360
    }
    if (pretty) {
        x <- pretty.range(lim[1:2], n = nx)
        y <- pretty.range(lim[3:4], n = ny)
    }
    else {
        x <- seq(lim[1], lim[2], len = nx)
        y <- seq(lim[3], lim[4], len = ny)
    }
    p = mapproject(expand.grid(x = c(seq(lim[1], lim[2], len = 100), NA), y = y))
    limg<<-lim
    p = map.wrap(p)
    lines(p, col = col, lty = lty, ...)
    lines(mapproject(expand.grid(y = c(seq(lim[3], lim[4], len = 100), 
        NA), x = x)), col = col, lty = lty, ...)
    if (labels) {
        tx <- x[2]
        xinc <- median(diff(x))
        ty <- y[length(y) - 2]
        yinc <- median(diff(y))
        text(mapproject(expand.grid(x = x + xinc * 0.05, y = ty + 
            yinc * 0.5)), labels = auto.format(x), cex = cex, 
            adj = c(0, 0), col = col, font = font, ...)
        text(mapproject(expand.grid(x = tx + xinc * 0.5, y = y + 
            yinc * 0.05)), labels = auto.format(y), cex = cex, 
            adj = c(0, 0), col = col, font = font, ...)
    }
}