


grey.MyColors <- function (n, start = 0.1, end = .9, gamma = 2.2, alpha = NULL, rev = FALSE)
	{
    cols <- gray(seq.int(from = start^gamma, to = end^gamma,
        length.out = n)^(1/gamma), alpha)
    if (rev)
        cols <- rev(cols)
    cols
	}


















grey.MyColorsRev <- function (n, start = 0.1, end = .9, gamma = 2.2, alpha = NULL, rev = FALSE)
	{
    cols <- gray(seq.int(from = start^gamma, to = end^gamma,
        length.out = n)^(1/gamma), alpha)
    
	cols <- rev(cols)
    cols
	}














































filledContourGrey <- function (x, y = 1, maxpixels = 1e+05, n_cexAxis = 3, l_reverse = FALSE, l_plotAxes = FALSE)
	{
    if (nlayers(x) > 1) {
        y <- min(max(1, y), nlayers(x))
        x <- raster(x, y)
    }
    x <- sampleRegular(x, maxpixels, asRaster = TRUE, useGDAL = TRUE)
    X <- xFromCol(x, 1:ncol(x))
    Y <- yFromRow(x, nrow(x):1)
    Z <- t(matrix(getValues(x), ncol = x@ncols, byrow = TRUE)[nrow(x):1,])
    
    lonlat <- couldBeLonLat(x, warnings = FALSE)
    asp <- list()$asp
    
    color.function <- grey.MyColors
    
    if(l_reverse){color.function <- grey.MyColorsRev}
	
	par(cex.axis = n_cexAxis)
	
    if (is.null(asp)) {
        if (lonlat) {
            ym <- mean(c(x@extent@ymax, x@extent@ymin))
            asp <- 1/cos((ym * pi)/180)
        }
        else {
            asp <- 1
        }
        filled.contour(x = X, y = Y, z = Z, asp = asp, color.palette  = color.function, plot.axes = l_plotAxes)
    }
    else {
        filled.contour(x = X, y = Y, z = Z, color.palette = color.function, plot.axes = l_plotAxes)
    }
}



#filledContourGrey(dem.Field)
#filledContourGrey(dem.Field, l_reverse = TRUE)

