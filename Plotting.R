


PlotBBox <- function(crd1, crd2)
	{
	x_lim <- c(crd1[1], crd2[1])
	y_lim <- c(crd1[2], crd2[2])
	
	plot(crd1, xlim = x_lim, ylim = y_lim, type = 'n', xlab = 'Longitude', ylab = 'Latitude')
	}









PlotPoly <- function(n_poly, l_catch = TRUE, v_row = c(1,1))
	{
	par(mfrow = v_row)
	data_sub <- paste("poly", Labels1()[n_poly], sep = '')
	
	data_sub <- get(data_sub)
	Drain <- data_sub$DrainagePolygon
	Hull <- data_sub$ConvexHull

	p4 <- proj4string(Hull)


	#plot(spTransform(Drain, DecimalLonLat), col = 'grey50', lty = 0)
	plot(Drain, col = 'grey50', lty = 0)
	
	if(l_catch)plot(spTransform(Catch.shp, p4))
		{
		plot(Drain, add = TRUE, col = 'grey50', lty = 0)
		}
	
	
	plot(Hull, add = TRUE, lwd = 2)
	}

