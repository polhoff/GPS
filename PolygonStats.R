

CalcFieldLineStats <- function(SpDataFrame, SpLines, n_width, c_var = 'Field_ID')
	{

	#field_sub <- analdata[analdata$Field_ID == F_ID,]
	#SpDataFrame <- field_sub
	
	stopifnot(class(SpLines) %in% c('SpatialLines','SpatialLinesDataFrame'))
	
	
	
	names_ndx <- names(SpDataFrame) %in% c_var
	check_ndx <- TRUE %in% names_ndx
	stopifnot(check_ndx)

	
	#for lineLength
	library(SDraw)
	
	
	
	#Field_ID <- SpDataFrame[,names_ndx]
	Field_ID <- as.data.frame(SpDataFrame[,names_ndx])
	Field_ID <- as.numeric(unique(Field_ID))
	stopifnot(length(Field_ID) < 2)
	#warning(length(Field_ID) > 1, "Hello")


	field_sub.big <- InflatePolygon(SpDataFrame, n_width = n_width)


	#aggregates polgons and transforms into single spatial polygon
	field_sub <- aggregate(SpDataFrame)
	field_sub.big <- aggregate(field_sub.big)


	FieldLines <- LineInPoly(SpLines, field_sub.big)


	class(FieldLines)
	n_Lines <- length(FieldLines)


	#a1 <- lineLength(FieldLines)


	a2 <- CalcPerimeter(field_sub)
	a3 <- CalcArea(field_sub)
	
	a4 <- CalcPerimeter(field_sub.big)
	a5 <- CalcArea(field_sub.big)
	
	a6 <- gLength(FieldLines)
	a7 <- try(lineLength(FieldLines))

	
	if(!is.numeric(a6)) {a6 <- NA} 
	if(!is.numeric(a7)) {a7 <- NA} 

	
	out <- data.frame(matrix(c(Field_ID, n_Lines, a2, a3, a4, a5, a6, a7, n_width), nrow = 1))



	names(out) <- c('Field_ID', 'LinesCount', 'FieldPerimeter', 'FieldArea', 'FieldPerimeter.Big', 'FieldArea.Big', 'LinesLength', 'LinesLength.SDraw','BufferedWidth')

	return(out)
	}





























































CalcPolyStats <- function(poly0xxx)
	{
	data_sub <- poly0xxx
	
	ConvHull <- data_sub$ConvexHull
	WetRegion <- data_sub$DrainagePolygon
	
	
	Perimeter.ConvexHull <- try(CalcPerimeter(ConvHull))
	Perimeter.WetRegion <- try(sum(CalcPerimeter(WetRegion)))

	Area.ConvexHull <- try(CalcArea.geosphere(ConvHull))
	Area.ConvexHull01 <- try(CalcPolyArea(ConvHull))

	Area.WetRegion <- try(sum(CalcArea.geosphere(WetRegion)))

	Circle.ConvexHull <- try(CircumCircle(ConvHull))



	#centres
	Centroid.WetRegion <- try(Centroid(WetRegion))
	Centroid.ConvexHull <- try(Centroid(ConvHull))
	Centre.CircumCircle <- try(Centroid(Circle.ConvexHull))


	Area.CircumCircle <- try(CalcArea.geosphere(Circle.ConvexHull))
	Radius.CircumCircle <- try((Area.CircumCircle/pi)^0.5)
	
	
	
	
	#output <- c('Perimeter.ConvexHull' = Perimeter.ConvexHull, Perimeter.WetRegion, Area.ConvexHull, Area.WetRegion, Centroid.ConvexHull, Centroid.WetRegion)
	
	output <- list('Perimeter.ConvexHull' = Perimeter.ConvexHull, Perimeter.WetRegion=Perimeter.WetRegion, Area.ConvexHull=Area.ConvexHull, Area.WetRegion=Area.WetRegion, Centroid.ConvexHull=Centroid.ConvexHull, Centroid.WetRegion=Centroid.WetRegion, Circle.ConvexHull=Circle.ConvexHull, Area.CircumCircle=Area.CircumCircle, Radius.CircumCircle=Radius.CircumCircle, Centre.CircumCircle = Centre.CircumCircle, Area.ConvexHull01=Area.ConvexHull01)
	
	return(output)
	
	}






















































DelPtsInPoly <- function(SpPoints = HandDP.shp, SpPoly, l_29903 = TRUE)
	{
	
	DelPts <- PointInPoly(SpPoints, SpPoly)
	if(l_29903)
		{
		DelPts <- spTransform(DelPts, init29903)
		}
	
	return(DelPts)
	}

#DelPtsInPoly(HandDP.shp, poly0001$ConvexHull, l_29903 = TRUE)

















DelPtsInPolyNames <- function(SpPoints = HandDP.shp, c_poly, l_29903 = TRUE)
	{
	
	data_sub <- get(c_poly)
	
	ConvHull <- data_sub$ConvexHull
	
	DelPts <- PointInPoly(SpPoints, ConvHull)
	if(l_29903)
		{
		DelPts <- spTransform(DelPts, init29903)
		}
	
	return(DelPts)
	}


#DelPtsInPolyNames(HandDP.shp, 'poly0001', l_29903 = TRUE)
#do.call(DelPtsInPolyNames, list(HandDP.shp, 'poly0001'))
#c_poly <- 'poly0001'; assign(paste(c_poly, '.DelPts', sep = ''), do.call(DelPtsInPolyNames, list(HandDP.shp, c_poly)))


DelPtsInPolyNames01 <- function(c_poly, SpPoints = HandDP.shp, l_29903 = TRUE)
	{
	
	assign(paste(c_poly, '.DelPts', sep = ''), do.call(DelPtsInPolyNames, list(SpPoints, c_poly)), env = .GlobalEnv)
	
	}

#DelPtsInPolyNames01(c_poly = 'poly0001')





DelPtsInPolyGlobal <- function(c_poly, SpPoints = HandDP.shp, l_29903 = TRUE)
	{
	
	assign(paste(c_poly, '.DelPts', sep = ''), DelPtsInPolyNames(SpPoints, c_poly), env = .GlobalEnv)
	#DelPtsInPolyNames(HandDP.shp, 'poly0001', l_29903 = TRUE)
	}

#DelPtsInPolyGlobal('poly0004')


































































PlotPolygon <- function(n_poly, l_catch = TRUE, c_color = 'grey50')
	{
	
	data_sub <- paste("poly", Labels1()[n_poly], sep = '')
	
	data_sub <- get(data_sub)
	Drain <- data_sub$DrainagePolygon
	Hull <- data_sub$ConvexHull

	plot(spTransform(Drain, DecimalLonLat), col = c_color, lty = 0)
	if(l_catch)plot(Catch.shp)
		{
		plot(spTransform(Drain, DecimalLonLat), add = TRUE, col = 'grey50', lty = 0)
		}
	
	
	plot(spTransform(Hull, DecimalLonLat), add = TRUE, lwd = 2)
	}



#PlotPolygon(5, l_catch = FALSE)
#PlotPolygon(1, l_catch = FALSE)



















































































PlotPolyMainDelPt <- function(n_poly, n_main, l_catch = FALSE, l_numbers = FALSE, c_color = 'grey70', l_hull = FALSE)
	{
	
	data_sub <- paste("poly", Labels1()[n_poly], sep = '')
	data_sub1 <- paste("poly", Labels1()[n_poly], '.DelPts', sep = '')

	
	data_sub <- get(data_sub)

	
	data_sub1 <- get(data_sub1)
	data_sub1 <- spTransform(data_sub1, DecimalLonLat)
	

	SpPoint.main <- data_sub1[n_main]
	SpPoint.others <- data_sub1[-n_main]

	
	Drain <- data_sub$DrainagePolygon
	Hull <- data_sub$ConvexHull
	
	Drain <- spTransform(Drain, DecimalLonLat)
	Hull <- spTransform(Hull, DecimalLonLat)
	

	plot(Drain, col = c_color, lty = 0)
	if(l_catch)plot(Catch.shp)
		{
		plot(spTransform(Drain, DecimalLonLat), add = TRUE, col = c_color, lty = 0)
		}
	
	if(l_hull)
		{
		plot(Hull, add = TRUE, lwd = 2)
		}
	
	plot(SpPoint.others, add = TRUE, lwd = 2, pch = 1, cex = 3)
	plot(SpPoint.main, add = TRUE, lwd = 3, pch = 4, cex = 3)


	if(l_numbers)
		{
		for(j in 1:length(data_sub1))
			{
			#xy_coords <- coordinates(data_sub1)
			#text(xy_coords[1], xy_coords[2], j)
			text(data_sub1, j)
			}
		}
	}



#PlotPolyNumbers(5, l_catch = FALSE)
#PlotPolyMainDelPt(5, 1)































































PlotPolyNumbers <- function(n_poly, l_catch = FALSE, l_numbers = TRUE, l_hull = TRUE, c_color = 'grey70')
	{
	
	data_sub <- paste("poly", Labels1()[n_poly], sep = '')
	data_sub1 <- paste("poly", Labels1()[n_poly], '.DelPts', sep = '')

	
	data_sub <- get(data_sub)

	
	data_sub1 <- get(data_sub1)
	data_sub1 <- spTransform(data_sub1, DecimalLonLat)
	
	
	
	Drain <- data_sub$DrainagePolygon
	Hull <- data_sub$ConvexHull


	Drain <- spTransform(Drain, DecimalLonLat)
	Hull <- spTransform(Hull, DecimalLonLat)
	
	
	plot(Drain, col = c_color, lty = 0)
	if(l_catch)plot(Catch.shp)
		{
		plot(Drain, add = TRUE, col = c_color, lty = 0)
		}
	
	if(l_hull)
		{
		plot(Hull, add = TRUE, lwd = 2)
		}
	
	
	
	plot(data_sub1, add = TRUE, lwd = 2, pch = 1, cex = 3)
	
	if(l_numbers)
		{
		for(j in 1:length(data_sub1))
			{
			#xy_coords <- coordinates(data_sub1)
			#text(xy_coords[1], xy_coords[2], j)
			text(data_sub1, j)
			}
		}
	}



#PlotPolyNumbers(5, l_catch = FALSE)

