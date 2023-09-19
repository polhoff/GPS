
#TotalClipLinesRaster1

CalculateAreaAsPercentFromPolygon <- function(SpPoly, l_plot = TRUE, l_outPolygons = TRUE)
	{

	p4_1 <- proj4string(SpPoly)
	#expecting single polygon or set of points
	chull.poly <- Chull.SpatialPolygon(SpPoly)
	coords.pl2 <- Geom(SpPoly)
	
	if(l_plot)
		{
		plot(SpPoly, col = 'grey70', lty = 0)
		#plot(coords.pl2, pch = 1, cex = 0.6, add=  TRUE)
		#plot(chull.pl2, pch = 1, cex = 2, add=  TRUE)
		plot(chull.poly, lwd = 2, add = TRUE)
		}



	AreaPoly1 <- sum(area(SpPoly))
	AreaChull <- area(chull.poly)
	Ratio <- round(AreaPoly1/AreaChull,2)
	Npoints <- dim(coordinates(coords.pl2))[1]
	PointsPerUnitArea <- Npoints/AreaPoly1


	#out <- list(pl2, chull.poly,
	#out <- c(AreaPoly1, AreaChull, round(AreaPoly1/AreaChull,2), Npoints, Npoints/AreaPoly1)
	#out <- c(AreaPoly1, AreaChull, Ratio, Npoints, PointsPerUnitArea, n_ctr)
	#names(out) <- c('AreaPoly', 'AreaChull', 'Ratio', 'Npoints', 'PointsPerUnitArea', 'PolyID')

	out <- c(AreaPoly1, AreaChull, Ratio, Npoints, PointsPerUnitArea)
	names(out) <- c('AreaPoly', 'AreaChull', 'Ratio', 'Npoints', 'PointsPerUnitArea')


	#out <- c('AreaPoly'=AreaPoly1, 'AreaChull'=AreaChull, 'Ratio'=round(AreaPoly1/AreaChull,2), 'Npoints'=Npoints, 'PointsPerUnitArea'=Npoints/AreaPoly1)

	#out <- c("AreaPoly"=AreaPoly1, "AreaChull"=AreaChull, "Ratio"=round(AreaPoly1/AreaChull,2), "Npoints"=Npoints, "PointsPerUnitArea"=Npoints/AreaPoly1)


	#names(out) <- c("AreaPoly", "AreaChull", "Ratio", "Npoints", "PointsPerUnitArea")

	if(l_outPolygons)
		{
		#out <- list(n_ctr, SpPoly, chull.poly, out)
		#names(out) <- c("PolyID", "DrainagePolygon", "ConvexHull", "Stats")
		out <- list(SpPoly, chull.poly, out)
		names(out) <- c("DrainagePolygon", "ConvexHull", "Stats")
		
		}


	return(out)
	}


#test1 <- CalculateAreaAsPercentFromPolygon(data_sub1)






























































































#wrapper for CalcArea.geosphere
CalcArea <- function(SpPoly)
	{

	x <- CalcArea.geosphere (SpPoly)
	return(x)

	}


#CalcArea(y1)





































































CalcArea.geosphere <- function(SpPoly)
	{
	#stopifnot(class(SpPoly) == "SpatialPolygons")
	stopifnot(class(SpPoly) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))
	
	xin <- (spTransform(SpPoly, DecimalLonLat))
	xout <- geosphere::areaPolygon(xin)
	print(paste("Area is ", round(xout,0), " square metres", sep = ''))
	return(xout)
	}


#CalcArea.geosphere(y1)

































































CalcPerimeter <- function(SpPoly)
	{
	stopifnot(class(SpPoly) == "SpatialPolygons")

	xin <- (spTransform(SpPoly, DecimalLonLat))
	xout <- geosphere::perimeter(xin)
	print(paste("Perimeter is ", round(xout,0), " metres", sep = ''))
	return(xout)
	}


#CalcPerimeter(y1)

























































CalcPolyArea <- function(indata)
	{
	#x <- sapply(indataDrainageAreas.NGR.shp@polygons, function(x) x@area)
	Area <- sapply(indata@polygons, function(x) x@area)
	return(Area)
	}

#x <- CalcPolyArea(DrainageAreas.NGR.shp)

















































































































































#see also COGravity {SDMTools}	R Documentation
#Centre of Gravity or Mass calculations for spatial data


Centroid <- function(SpPoly)
	{
	#stopifnot(class(SpPoly) %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame'))
	#p4 <- GetProj4(SpPoly)



	#https://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
	#from rgeos
	trueCentroids <- gCentroid(SpPoly)


	#x <- coordinates(SpPoly)
	#x <- as.data.frame(x)
	#names(x) <- c('x','y')

	#x <- as(x, 'Spatial')
	#x <- SpatialPointsDataFrame(x, data = x, proj4string = CRS(p4))
	#x <- as(x, 'SpatialPoints')

	return(trueCentroids)
	}

#Centroid(a1)
#Centroid(poly0001$DrainagePolygon)
#plot(poly0001$ConvexHull)
#plot(Centroid(poly0001$DrainagePolygon),  add = TRUE)
#plot(Centroid(poly0001$ConvexHull),  add = TRUE, pch = 1)
#plot(poly0001$ConvexHull); plot(Centroid(poly0001$DrainagePolygon),  add = TRUE); plot(Centroid(poly0001$ConvexHull),  add = TRUE, pch = 1)





























































































Centroid.Circle <- function(SpPoly)
	{
	stopifnot(class(SpPoly) %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame'))
	p4 <- GetProj4(SpPoly)

	#from geosphere
	x <- centroid(SpPoly)
	x <- as.data.frame(x)
	#names(x) <- c('x','y')

	#x <- as(x, 'Spatial')
	x <- SpatialPointsDataFrame(x, data = x, proj4string = CRS(p4))
	x <- as(x, 'SpatialPoints')

	return(x)
	}


#xtest <- Centroid(y1)

























































































Centroid.sp <- function(SpPoly)
	{
	stopifnot(class(SpPoly) %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame'))
	p4 <- GetProj4(SpPoly)

	#sp package
	x <- coordinates(SpPoly)
	x <- as.data.frame(x)
	#names(x) <- c('x','y')

	#x <- as(x, 'Spatial')
	x <- SpatialPointsDataFrame(x, data = x, proj4string = CRS(p4))
	x <- as(x, 'SpatialPoints')

	return(x)
	}






















































































Chull.SpatialGrid <- function(indata, l_spatial = TRUE, l_polygon = TRUE)
	{
	stopifnot(class(indata) %in% c("SpatialGridDataFrame", "SpatialGrid"))
	indata <- as(indata, "SpatialPolygonsDataFrame")
	
	c_proj4string <- proj4string(indata)
	out <- data.frame(indata)

	x_ndx <- chull(out)
	out <- indata[x_ndx,]

	#if(l_polygon)	{		out <- MakeSpatialPolygon(out)		}

	if(l_spatial)
		{
		out <- SpatialPoints(out, proj4string = CRS(c_proj4string))
		}

	return(out)
	}



#x3 <- Chull.SpatialGrid(carea.5m.shp_sub)
#x4 <- Chull.SpatialGrid(x3)
#x4 <- Chull.SpatialGrid(x3, l_spatial = FALSE)
#x4 <- Chull.SpatialGrid(x3, l_polygon = FALSE)



































Chull.SpatialPoints <- function(indata, l_spatial = TRUE, l_polygon = TRUE)
	{
	stopifnot(class(indata) == "SpatialPoints")

	c_proj4string <- proj4string(indata)
	out <- data.frame(indata)

	x_ndx <- chull(out)
	out <- indata[x_ndx,]

	#if(l_polygon)	{		out <- MakeSpatialPolygon(out)		}

	if(l_spatial)
		{
		out <- SpatialPoints(out, proj4string = CRS(c_proj4string))
		}

	return(out)
	}



#x3 <- Chull.SpatialPoints(pl3)
#x4 <- Chull.SpatialPolygon(x3)
#x4 <- Chull.SpatialPolygon(x3, l_spatial = FALSE)
#x4 <- Chull.SpatialPolygon(x3, l_polygon = FALSE)






















































































Chull.SpatialPolygon <- function(indata, l_spatial = TRUE, l_polygon = TRUE)
	{
	stopifnot(class(indata) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialPoints", "SpatialPointsDataFrame"))

	c_proj4string <- proj4string(indata)
	#out <- data.frame(indata)

    #coords.pl2 <- Geom(out)
	coords.pl2 <- Geom(indata)
	coords.pl2_df <- data.frame(coords.pl2)
	
	chull.out_ndx <- chull(coords.pl2_df)
	
	out <- coords.pl2[chull.out_ndx,]
	
	if(l_spatial)
		{
		out <- SpatialPoints(out, proj4string = CRS(c_proj4string))
		}

	if(l_polygon)
		{
		out <- ConvertSpatialPointsToSpatialPolygons(out)
		}
	#return(chull.out)
	return(out)
	}


#xx <- Chull.SpatialPolygon(test_data)

























































































Chull.SpatialPolygon.Arc <- function(indata, l_spatial = TRUE, l_polygon = TRUE)
	{
	stopifnot(class(indata) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))

	c_proj4string <- proj4string(indata)
	out <- indata@polygons[[1]]@Polygons[[1]]@coords

	x_ndx <- chull(out)
	out <- out[x_ndx,]



	if(l_spatial)
		{

		out <- SpatialPoints(out, proj4string = CRS(c_proj4string))

		if(l_polygon)
			{
			out <- MakeSpatialPolygon(out)
			}
		}

	return(out)
	}

#x4 <- Chull.SpatialPolygon(x3)
#x4 <- Chull.SpatialPolygon(x3, l_spatial = FALSE)
#x4 <- Chull.SpatialPolygon(x3, l_polygon = FALSE)














































CircumCircle <- function(SpObj, l_poly = TRUE)
	{

	#SpObj = ppoly
	p4 <- GetProj4(SpObj)

	coords.vertices <- geom(SpObj)[,c('x','y')]

	x <- st_multipoint(coords.vertices)
	mbcx <- st_minimum_bounding_circle(x)

	x <- as.matrix(mbcx)
	y <- ConvertMatrixToSpatialPoints(x, p4string = p4)
	y <- as(y, "SpatialPoints")

	if(l_poly)
		{
		y <- ConvertSpatialPointsToSpatialPolygons(y)
		}

	print(class(y))
	return(y)
	}


#y1 <- CircumCircle(data_sub$ConvexHull)
#y1 <- CircumCircle(data_sub$ConvexHull, l_poly = FALSE)
#y1 <- CircumCircle(ppoly)
#y1 <- CircumCircle(ppoly, l_poly = FALSE)



































































































Coordinates.SpatialPolygon <- function(indata, l_spatial = TRUE)
	{
	stopifnot(class(indata) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))

	p4 <- GetProj4(indata)

	indata <- try(as(indata, "SpatialPolygonsDataFrame"))
	xy_coords <- coordinates(indata)

	xy_coords <- data.frame(xy_coords)
	names(xy_coords) <- c('x', 'y')

	#out.points <- st_as_sf(x = xy_coords, coords = c("x", "y"), crs = p4)


	if(l_spatial)
		{
		xy_coords <- SpatialPoints(xy_coords, proj4string = CRS(p4))
		}

	print(class(xy_coords))
	return(xy_coords)
	}


#x_sp1 <- Coordinates.SpatialPolygon()
#x_sp2 <- Coordinates.SpatialPolygon()



















































Coords.SpatialPolygon <- function(indata, l_spatial = TRUE)
	{
	stopifnot(class(indata) == "SpatialPolygons")

	c_proj4string <- proj4string(indata)
	out <- indata@polygons[[1]]@Polygons[[1]]@coords

	if(l_spatial)
		{
		out <- SpatialPoints(out, proj4string = CRS(c_proj4string))
		}

	print(class(out))
	return(out)
	}


#x_sp1 <- Coords.SpatialPolygon(x3)
#x_sp2 <- Coords.SpatialPolygon(x3, l_spatial = FALSE)
#x_sp3 <- getMinCircle(x_sp2)






















































FitCircle <- function(SpObj, n_scale = 100)
	{

	p4 <- GetProj4(SpObj)

	coords.vertices <- geom(SpObj)[,c('x','y')]

	mmm1 <- colMeans(coords.vertices)
	q1 <- coords.vertices[,1] - mmm1[1]
	q2 <- coords.vertices[,2] - mmm1[2]

	q1 <- q1/n_scale
	q2 <- q2/n_scale

	out <- circlefit(q1,q2)

	x1 <- circlefit$xp * n_scale
	y1 <- circlefit$yp * n_scale

	x1 <- x1 + mmm1[1]
	y1 <- y1 + mmm1[2]

	n_radius <- circlefit[3] * n_scale

	}

































































gClip <- function(shp, bb)
	{
	if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
	else b_poly <- as(extent(bb), "SpatialPolygons")
	gIntersection(shp, b_poly, byid = T)
	}



























































Geom <- function(indata, l_spatial = TRUE)
	{

	#fundamentally a wrapper for raster::geom
	#stopifnot(class(indata) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))

	p4 <- GetProj4(indata)

	#indata <- try(as(indata, "SpatialPolygonsDataFrame"))
	xy_coords <- geom(indata)[,c('x','y')]

	#xy_coords <- data.frame(xy_coords)
	#names(xy_coords) <- c('x', 'y')

	#out.points <- st_as_sf(x = xy_coords, coords = c("x", "y"), crs = p4)


	if(l_spatial)
		{
		xy_coords <- SpatialPoints(xy_coords, proj4string = CRS(p4))
		}

	print(class(xy_coords))
	return(xy_coords)
	}


#xx <- Geom(pl2)
#xx <- Geom(pl2, l_spatial = FALSE)









































GetAreaRank <- function(indata, n_rank)
	{
	out_ndx <- order(indata,decreasing = TRUE)[n_rank]
	}


#n_big <- GetAreaRank(DrainagePolys.area, 1)












GetNthLargestPolygon <- function(indata, n_rank)
	{
	out_ndx <- order(indata,decreasing = TRUE)[n_rank]
	outdata <- indata[out_ndx,]
	}


#GetNthLargestPolygon(indata, 1)



#n_big <- GetRank(DrainagePolys.area, 1)


























GetPolyCoordsFromSpdf <- function(indata, ndx)
	{
	#xddd <- DrainageAreas.NGR.shp@polygons[[ndx]]@Polygons[[1]]@coords
	ClippedPolyCoords <- indata@polygons[[ndx]]@Polygons[[1]]@coords
	return(ClippedPolyCoords)
	}



















































































InCircle <- function(SpPoly, l_circle = TRUE)
	{
	stopifnot(class(SpPoly) %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame'))
	p4 <- GetProj4(SpPoly)

	xin <- as.owin(SpPoly)
	xout <- incircle(xin)

	#coords <- matrix(c(xout$x, xout$y), ncol = 2)
	coords <- as.data.frame(xout)
	out.points <- st_as_sf(x = coords, coords = c('x','y'), crs = p4)
	Circle <- out.points

	if(l_circle)
		{

		out.points <- as(out.points, "Spatial")


		#gBuffer from geos package
		Circle <- gBuffer(out.points, width = xout$r)
		}

	return(Circle)
	}

#InCircle(ppoly)
#InCircle(ppoly, l_circle = FALSE)
















































































#essentially a wrapper for gBuffer function
InflatePolygon <- function(SpPoly, n_width = 2, c_init = ThomasNov2019, outinit = DecimalLonLat, l_byid = TRUE, l_keepProjection = TRUE)
	{

	data(Proj4Strings)
	p4 <- GetProj4(SpPoly)
	
	
	SpPoly <- spTransform(SpPoly, c_init)
	
	#add width
	#gBuffer from rgeos library
	out <- gBuffer(SpPoly, width = n_width, byid = l_byid)
	
	if(l_keepProjection)
		{
		out <- spTransform(out, p4)
		}
	else
		{
		out <- spTransform(out, outinit)
		}

	return(out)
	}














































































exists.false <- function(indata)
	{
	x <- !is.na(indata) & indata
	out <- FALSE %in% x
	
	return(out)
	}


#x <- is.true(in_ndx)
#summary(x)









is.true <- function(indata)
	{
	!is.na(indata) & indata
	}


#x <- is.true(in_ndx)
#summary(x)



















































































LineInPoly <- function(SpLines, SpPolygon, l_points = TRUE, l_forceSameProj4 = TRUE)
	{

	SpLines <- try(as(SpLines, 'SpatialLines'))
	stopifnot <- class(SpLines) == 'SpatialLines'

	p4_1 <- proj4string(SpLines)
	p4_2 <- proj4string(SpPolygon)

	if(l_forceSameProj4)
		{
		if(p4_1 != p4_2)
			{
			SpPolygon <- spTransform(SpPolygon, p4_1)
			}
		}

	in_ndx <- over(SpLines, SpPolygon)
	in_ndx[is.na(in_ndx)] <- FALSE
	in_ndx <- as.logical(in_ndx)

	#over works better?
	#data_sub1 <- intersect(SpLines, SpPolygon)


	out <- in_ndx
	if(l_points)
		{
		out <- SpLines[in_ndx]
		}

	return(out)
	}


#test_poly <-
#LineInPoly(xd, data_sub)
#LineInPoly(HandDP.shp, data_sub)






















































































MergePolygon <- function(SpPoly1, SpPoly2, c_var = 'Field_ID', l_SPDF = TRUE)
	{


	c_class1 <- class(SpPoly1)
	c_class2 <- class(SpPoly2)

	stopifnot(c_class1 %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame'))
	stopifnot(c_class2 %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame'))


	p_1 <- GetProj4(SpPoly1)
	p_2 <- GetProj4(SpPoly2)

	stopifnot(p_1 == p_2)


	#horribly clunky
	x_ndx <- names(SpPoly1) %in% c_var
	#gives SPDF with one variable
	valueTodf <- SpPoly1[,x_ndx]

	valueTodf <- as.data.frame(SpPoly1[,x_ndx])[1,1]
	valueTodf <- as.vector(valueTodf)


	out <- union(SpPoly1, SpPoly2)
	out <- raster::aggregate(out)

	if(l_SPDF)
		{
		out <- ConvertSpatialPolygonsToSPDF(out, c_Var = c_var, valueTodf)
		}

	return(out)

	}

#MergePolygon(data_sub1, data_sub2)



































































PointInPoly <- function(SpPoints, SpPolygon, l_points = TRUE, l_forceSameProj4 = TRUE)
	{

	SpPoints <- try(as(SpPoints, 'SpatialPoints'))
	stopifnot <- class(SpPoints) == 'SpatialPoints'

	p4_1 <- proj4string(SpPoints)
	p4_2 <- proj4string(SpPolygon)

	if(l_forceSameProj4)
		{
		if(p4_1 != p4_2)
			{
			SpPolygon <- spTransform(SpPolygon, p4_1)
			}
		}

	in_ndx <- over(SpPoints, SpPolygon)
	in_ndx[is.na(in_ndx)] <- FALSE
	in_ndx <- as.logical(in_ndx)


	out <- in_ndx
	if(l_points)
		{
		out <- SpPoints[in_ndx]
		}

	return(out)
	}

#PointInPoly(xd, data_sub)
#PointInPoly(HandDP.shp, data_sub)





































































SelectPolygon <- function(indata, ndx, l_SpatialDataFrame = TRUE)
	{
	c_proj4string <- proj4string(indata)
	print(c_proj4string)

	out <- indata@polygons[[ndx]]

	if(l_SpatialDataFrame)
		{
		out <- SpatialPolygons(list(out), proj4string = CRS(c_proj4string))
		}

	return(out)
	}



#aa1 <- DrainageAreas.NGR.shp
#x2 <- SelectPolygon(DrainageAreas.NGR.shp, 1)


#n_biggest <- GetAreaRank(DrainagePolys.area, 1)
#x3 <- SelectPolygon(DrainageAreas.NGR.shp, n_biggest)





































































SimplifyPolygon <- function (SpPoly, n_Tol = 5, l_plot = FALSE, l_Sp = TRUE, l_returnPoints = FALSE)
	{
	
	#data_sub <- SpPoly
	#SpPoly <- data_sub

	#check for spatial polygons or sf object
    stopifnot(class(SpPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame") | "sf" %in% class(SpPoly))

	SpPoly <- as(SpPoly, 'sf')

	out <- st_simplify(SpPoly, dTolerance = n_Tol)
	out.points <- st_cast(out$geometry, "POINT")
	
	if(l_plot)
		{
		plot(as(SpPoly, "Spatial"), col = 'grey50', lty = 0)
		plot(out.points, add = TRUE, cex = 2)
		}
	
	if(l_Sp)
		{
		out.points <- as(out.points, "Spatial")
		out <- as(out, "Spatial")
		}
	
	if(l_returnPoints) out <- out.points

	return(out)


	}


#x1 <- SimplifyPolygon(data_sub, l_plot= TRUE)
#x1 <- SimplifyPolygon(data_sub, l_Sp= TRUE)























































































































SquareClip <- function (poly1, poly2)
	{
    stopifnot(class(poly1) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
    stopifnot(class(poly2) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))

	prj4 <- GetProj4(poly2)

    b_poly <- as(extent(poly2), "SpatialPolygons")

    b_poly <- AssignProj4(b_poly, prj4)

    b_poly <- gIntersection(poly1, b_poly, byid = TRUE)
    return(b_poly)
	}

































SquareClipRaster <- function (ToBeClippedPpoly, ContainerPoly)
	{

	out <- SquareClipRaster1(ToBeClippedPpoly=ToBeClippedPpoly, ContainerPoly=ContainerPoly)
	return(out)

	}


#x <- TotalClipRaster(dem, Catch.shp)







SquareClipRaster1 <- function (ToBeClippedPpoly, ContainerPoly)
	{
    #stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
    stopifnot(class(ToBeClippedPpoly) %in% c("RasterLayer","SpatialGridDataFrame","SpatialGrid","SpatialLinesDataFrame"))
    stopifnot(class(ContainerPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))


	#ContainerPoly <- gBuffer(ContainerPoly, byid = TRUE, width = 0)
	ContainerPoly <- as(ContainerPoly, 'SpatialPolygons')
	
	
	p4_1 <- proj4string(ContainerPoly)
	p4_2 <- proj4string(ToBeClippedPpoly)

	test_poly <- ToBeClippedPpoly


	bb <- BoundingBox(ContainerPoly)
	bb <- spTransform(bb, p4_2)

	out <- crop(test_poly, bb)
	
	return(out)
	}



















































TotalClip <- function (ToBeClippedPpoly, ContainerPoly)
	{

	out <- TotalClip1(ToBeClippedPpoly=ToBeClippedPpoly, ContainerPoly=ContainerPoly)
	return(out)

	}


#x <- TotalClip(DLines, test1$DrainagePolygon)
#x <- TotalClip(csa.shp, field_sub.big)

















































TotalClipLines <- function (ToBeClippedPpoly, ContainerPoly, l_UseRaster = TRUE)
	{

	if(!l_UseRaster)	out <- TotalClipLines1(ToBeClippedPpoly=ToBeClippedPpoly, ContainerPoly=ContainerPoly)
	
	if(l_UseRaster)		out <- TotalClipLinesRaster1(ToBeClippedPpoly=ToBeClippedPpoly, ContainerPoly=ContainerPoly)
	
	return(out)
	}


#x <- TotalClipLines(DLines, test1$DrainagePolygon)
#x <- TotalClipLines(csa.shp, field_sub.big)




































TotalClipLinesToPolygon <- function (ToBeClippedPpoly, ContainerPoly, n_width = 0.01)
	{

	out <- TotalClipLinesToPolygon1(ToBeClippedPpoly=ToBeClippedPpoly, ContainerPoly=ContainerPoly, n_width = n_width)
	return(out)

	}


#x <- TotalClipLines(DLines, test1$DrainagePolygon)
#x <- TotalClipLines(csa.shp, field_sub.big)































TotalClipRaster <- function (ToBeClippedPpoly, ContainerPoly)
	{

	out <- TotalClipRaster3(ToBeClippedPpoly=ToBeClippedPpoly, ContainerPoly=ContainerPoly)
	return(out)

	}


#x <- TotalClipRaster(dem, Catch.shp)
















































































TotalClip1 <- function (ToBeClippedPpoly, ContainerPoly)
	{
	stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
	stopifnot(class(ContainerPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))

	#stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame", "SpatialGridDataFrame"))
    #stopifnot(class(ContainerPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame", "SpatialGridDataFrame"))

	
	
	#You have invalid (self intersecting) geometries. You can see that by running gIsValid on your SpatialPolygons* objects. They need to be fixed (the zero buffer approach does the trick). Also, for area calculations to be meaningful, SpatialPolygons* should be projected, preferably using an equal area projection See the approach below using sp (imported via raster)
	
	#https://gis.stackexchange.com/questions/223252/how-to-overcome-invalid-input-geom-and-self-intersection-when-intersecting-shape
	
	ContainerPoly <- gBuffer(ContainerPoly, byid = TRUE, width = 0)
	ToBeClippedPpoly <- gBuffer(ToBeClippedPpoly, byid = TRUE, width = 0)


	p4_1 <- GetProj4(ContainerPoly)
	p4_2 <- GetProj4(ToBeClippedPpoly)

	test_poly <- ToBeClippedPpoly
	
	if(p4_1 != p4_2)
		{
		test_poly <- AssignProj4(ToBeClippedPpoly, p4_1)
		}

	
	#rgeos::gIntersection

    out <- gIntersection(test_poly, ContainerPoly, byid = TRUE)
    return(out)
	}



























































TotalClipLines1 <- function (ToBeClippedPpoly, ContainerPoly)
	{
    #stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
    stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPoints","SpatialPointsDataFrame","SpatialLines","SpatialLinesDataFrame"))
    stopifnot(class(ContainerPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))

	
	
	
	#You have invalid (self intersecting) geometries. You can see that by running gIsValid on your SpatialPolygons* objects. They need to be fixed (the zero buffer approach does the trick). Also, for area calculations to be meaningful, SpatialPolygons* should be projected, preferably using an equal area projection See the approach below using sp (imported via raster)
	
	#https://gis.stackexchange.com/questions/223252/how-to-overcome-invalid-input-geom-and-self-intersection-when-intersecting-shape
	
	ContainerPoly <- gBuffer(ContainerPoly, byid = TRUE, width = 0)
	#ToBeClippedPpoly <- gBuffer(ToBeClippedPpoly, byid = TRUE, width = 0)


	p4_1 <- GetProj4(ContainerPoly)
	p4_2 <- GetProj4(ToBeClippedPpoly)

	test_poly <- ToBeClippedPpoly
	
	if(p4_1 != p4_2)
		{
		test_poly <- AssignProj4(ToBeClippedPpoly, p4_1)
		}


    out <- gIntersection(test_poly, ContainerPoly, byid = TRUE)
    
    if(is.null(out))
		{
		out <- MakeEmptySp(p4_1)
		}
    
    return(out)
	}


























































































TotalClipLinesRaster1 <- function (ToBeClippedPpoly, ContainerPoly)
	{
    #stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
    stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPoints","SpatialPointsDataFrame","SpatialLines","SpatialLinesDataFrame"))
    stopifnot(class(ContainerPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))

	
	
	
	#You have invalid (self intersecting) geometries. You can see that by running gIsValid on your SpatialPolygons* objects. They need to be fixed (the zero buffer approach does the trick). Also, for area calculations to be meaningful, SpatialPolygons* should be projected, preferably using an equal area projection See the approach below using sp (imported via raster)
	
	#https://gis.stackexchange.com/questions/223252/how-to-overcome-invalid-input-geom-and-self-intersection-when-intersecting-shape
	
	ContainerPoly <- gBuffer(ContainerPoly, byid = TRUE, width = 0)
	#ToBeClippedPpoly <- gBuffer(ToBeClippedPpoly, byid = TRUE, width = 0)


	p4_1 <- GetProj4(ContainerPoly)
	p4_2 <- GetProj4(ToBeClippedPpoly)

	test_poly <- ToBeClippedPpoly
	
	if(p4_1 != p4_2)
		{
		test_poly <- AssignProj4(ToBeClippedPpoly, p4_1)
		}


    out <- raster::intersect(test_poly, ContainerPoly)
    
    if(is.null(out))
		{
		out <- MakeEmptySp(p4_1)
		}
    
    return(out)
	}














































TotalClipLinesToPolygon1 <- function (ToBeClippedPpoly, ContainerPoly, n_width = 0.01)
	{
    #stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
    stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPoints","SpatialPointsDataFrame","SpatialLines","SpatialLinesDataFrame"))
    stopifnot(class(ContainerPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))

	
	
	
	#You have invalid (self intersecting) geometries. You can see that by running gIsValid on your SpatialPolygons* objects. They need to be fixed (the zero buffer approach does the trick). Also, for area calculations to be meaningful, SpatialPolygons* should be projected, preferably using an equal area projection See the approach below using sp (imported via raster)
	
	#https://gis.stackexchange.com/questions/223252/how-to-overcome-invalid-input-geom-and-self-intersection-when-intersecting-shape
	
	ContainerPoly <- gBuffer(ContainerPoly, byid = TRUE, width = 0)
	ToBeClippedPpoly <- gBuffer(ToBeClippedPpoly, byid = TRUE, width = n_width)


	p4_1 <- GetProj4(ContainerPoly)
	p4_2 <- GetProj4(ToBeClippedPpoly)

	test_poly <- ToBeClippedPpoly
	
	if(p4_1 != p4_2)
		{
		test_poly <- AssignProj4(ToBeClippedPpoly, p4_1)
		}


    out <- gIntersection(test_poly, ContainerPoly, byid = TRUE)
    
    
    if(is.null(out))
		{
		out <- MakeEmptySp(p4_1)
		}
    
    return(out)
	}































































































































TotalClipRaster1 <- function (ToBeClippedPpoly, ContainerPoly)
	{
    #stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
    stopifnot(class(ToBeClippedPpoly) %in% c("RasterLayer","SpatialGridDataFrame","SpatialGrid","SpatialLinesDataFrame"))
    stopifnot(class(ContainerPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))


	#ContainerPoly <- gBuffer(ContainerPoly, byid = TRUE, width = 0)
	ContainerPoly <- as(ContainerPoly, 'SpatialPolygons')
	
	
	p4_1 <- GetProj4(ContainerPoly)
	p4_2 <- GetProj4(ToBeClippedPpoly)

	test_poly <- ToBeClippedPpoly
	
	if(p4_1 != p4_2)
		{
		test_poly <- AssignProj4(ToBeClippedPpoly, p4_1)
		}


	dem.sp <- as(ToBeClippedPpoly, 'SpatialGridDataFrame')
	#class(dem.sp)


	dem.sp.points <- as(dem.sp, 'SpatialPoints')
	


	clipped.points_ndx <- PointInPoly(dem.sp.points, ContainerPoly, l_points = FALSE)


	out <- dem.sp[clipped.points_ndx,]
	
	#out <- gIntersection(test_poly, ContainerPoly, byid = TRUE)
    
    if(is.null(out))
		{
		out <- MakeEmptySp(p4_1)
		}
    
    return(out)
	}



#x <- TotalClipRaster1(dem, Catch.WGS84.big)
















































TotalClipRaster2 <- function (ToBeClippedPpoly, ContainerPoly)
	{
    #stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
    stopifnot(class(ToBeClippedPpoly) %in% c("RasterLayer","SpatialGridDataFrame","SpatialGrid","SpatialLinesDataFrame"))
    stopifnot(class(ContainerPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))


	#ContainerPoly <- gBuffer(ContainerPoly, byid = TRUE, width = 0)
	ContainerPoly <- as(ContainerPoly, 'SpatialPolygons')
	
	
	p4_1 <- proj4string(ContainerPoly)
	p4_2 <- proj4string(ToBeClippedPpoly)

	test_poly <- ToBeClippedPpoly
	
	if(p4_1 != p4_2)
		{
		test_poly <- AssignProj4(ToBeClippedPpoly, p4_1)
		}


	bb <- BoundingBox(ContainerPoly)
	bb <- spTransform(bb, p4_2)

	out <- crop(test_poly, bb)
	
	return(out)
	}




#x <- TotalClipRaster1(dem, Catch.WGS84.big)

























































TotalClipRaster3 <- function (ToBeClippedPpoly, ContainerPoly)
	{
    #stopifnot(class(ToBeClippedPpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
    stopifnot(class(ToBeClippedPpoly) %in% c("RasterLayer","SpatialGridDataFrame","SpatialGrid","SpatialLinesDataFrame"))
    stopifnot(class(ContainerPoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))


	#ContainerPoly <- gBuffer(ContainerPoly, byid = TRUE, width = 0)
	ContainerPoly <- as(ContainerPoly, 'SpatialPolygons')
	
	
	p4_1 <- proj4string(ContainerPoly)
	p4_2 <- proj4string(ToBeClippedPpoly)

	test_poly <- ToBeClippedPpoly
	
	#if(p4_1 != p4_2)		{		test_poly <- AssignProj4(ToBeClippedPpoly, p4_1)		}


	bb <- BoundingBox(ContainerPoly)
	bb <- spTransform(bb, p4_2)
	cp <- spTransform(ContainerPoly, p4_2)
	
	#https://stackoverflow.com/questions/23073669/clipping-raster-using-shapefile-in-r-but-keeping-the-geometry-of-the-shapefile


	#raster::crop
	#raster::mask
	
	out <- crop(test_poly, bb)
	out <- mask(out, cp)
	
	
	return(out)
	}





































































































































TotalClip.Arc <- function (largepoly, smallpoly)
	{
    stopifnot(class(largepoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))
    stopifnot(class(smallpoly) %in% c("SpatialPolygons" ,"SpatialPolygonsDataFrame"))

	box.poly <- SquareClip(largepoly, smallpoly)

	out.poly <- gIntersection(smallpoly, box.poly, byid = TRUE)

	return(out.poly)
	}
