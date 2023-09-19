#ConvertRasterToSpatialPoints
#ConvertSpatialGridToSpatialPoints
#MakeAnnulusSpatialPolygon


BoundingBox <- function(SpPoly)
	{

	p4 <- proj4string(SpPoly)

	out <- as(extent(SpPoly), "SpatialPolygons")
	proj4string(out) <- CRS(p4)
	
	return(out)
	}


#BoundingBox(poly0001$ConvexHull)























































ConvertDataFrameToSpatialPoints <- function(indata)
	{
	
	stopifnot(class(indata) == 'data.frame')
	stopifnot(names(indata) %in% c('lon', 'lat'))
	
	#make sure order is correct
	indata <- indata[c('lon', 'lat')]

	y1 = SpatialPoints(indata,  proj4string =  CRS('+proj=longlat +datum=WGS84 +no_defs '))
	
	return(y1)
	}




#x = read.table(paste(dirtop, 'Ballycanew/data/', 'AnneMurphyPolygon.tx', sep = ''))
#y = x[c(2,1)]
#names(y) = c('lon','lat')

#y1 = SpatialPoints(y,  proj4string =  CRS('+proj=longlat +datum=WGS84 +no_defs '))

#ConvertDataFrameToSpatialPoints(y)







































































ConvertRasterToSpatialPoints <- function(indata, l_convertToSp = TRUE)
	{
	
	stopifnot(class(indata) %in% c('RasterLayer', 'Raster','raster'))
	p4 <- proj4string(indata)	

	indata <- as(indata, 'SpatialGridDataFrame')
	
	indata <- ConvertSpatialGridToSpatialPoints(indata)

	return(indata)
	}


#ConvertRasterToSpatialPoints(FloodMap)




















































ConvertSpatialGridToSpatialPoints <- function(indata, l_convertToSp = TRUE)
	{
	
	stopifnot(class(indata) == 'SpatialGridDataFrame')
	p4 <- proj4string(indata)	

	ndx <- !is.na(indata@data)
	coords <- coordinates(indata)[ndx,]
	
	#grid <- indata@grid[ndx,]
	
    indata <- as.data.frame(coords)
	names(indata) <- c('x','y')
	
    out.points <- st_as_sf(x = indata, coords = c('x','y'), crs = p4)
    
    if (l_convertToSp) {
        out.points <- as(out.points, "Spatial")
		}
    
    
    
	return(out.points)
	}

#FloodMapGrid <- as(FloodMap, 'SpatialGridDataFrame')
#ConvertSpatialGridToSpatialPoints(FloodMapGrid)
























































ConvertSpatialPointsToSpatialPolygons <- function(indata, l_spatialPoly = TRUE)
	{
	
	stopifnot(class(indata) %in% c('SpatialPoints', 'SpatialPointsDataFrame'))
	c_proj4string <- proj4string(indata)	

	coords <- coordinates(indata)
	outdata <- Polygon(coords)

	if(l_spatialPoly)
		{
		#out1 <- Polygons(list(Polygon(coords.poly)),'1')
		out1 <- Polygons(list(outdata),'1')

		outdata <- SpatialPolygons(list(out1))
		proj4string(outdata) <- c_proj4string

		outdata
		}

	return(outdata)
	}


#ConvertSpatialPointsToSpatialPolygons(aa1)
#ConvertSpatialPointsToSpatialPolygons(aa1, l_spatialPoly = FALSE)








































































ConvertSpatialPolygonsToSPDF <- function(SpPoly, c_Var, value)
	{

	#out <- SpatialPolygonsDataFrame(SpPoly, c_name)
	out <- as(SpPoly, 'SpatialPolygonsDataFrame')

	out$dummy <- value
	names(out) <- c_Var
	

	return(out)
	}



#x <- MergePolygon(data_sub1, data_sub2)
#ConvertSpatialPolygonsToSPDF(x, 'Field_ID', 2262)































































































HexagonInner.Arc1 <- function(SpPolyHexagonalGrid, n_inflate = 1, l_inner = TRUE)
	{

	
	OuterGrid <- aggregate(SpPolyHexagonalGrid)

	n_elements <- length(SpPolyHexagonalGrid)
	x_ndx <- rep(NA, n_elements)
	
	for(i in 1:n_elements)
		{
		
		data_sub <- SpPolyHexagonalGrid[i]
		x <- InflatePolygon(data_sub, n_inflate)

		x_inf <- union(x, OuterGrid)
		x_inf <- aggregate(x_inf)
		
		l_test <- gArea(x_inf) == gArea(OuterGrid)
		x_ndx[i] <- l_test
		}
	
	InnerHexagons <- SpPolyHexagonalGrid[x_ndx]
	OuterHexagons <- SpPolyHexagonalGrid[!x_ndx]
	
	if(!l_inner) return(OuterHexagons)
	return(InnerHexagons)
	
	}

#x <- HexagonInner(HexagonalGrid)
#plot(x)
























































































































































HexagonInner <- function(SpPolyHexagonalGrid, n_inflate = 1, l_inner = TRUE)
	{

	
	OuterGrid <- aggregate(SpPolyHexagonalGrid)
	print('Donkey')
	n_elements <- length(SpPolyHexagonalGrid)
	x_ndx <- rep(NA, n_elements)
	
	for(i in 1:n_elements)
		{
		
		data_sub <- SpPolyHexagonalGrid[i]

print('Cow')
		#x <- InflatePolygon(data_sub, n_inflate)


    	poly.buffered <- gBuffer(data_sub, width = n_inflate, byid = TRUE)

print('Horse')


		#x_inflated <- union(poly.buffered, OuterGrid)
		x_inflated <- raster::union(poly.buffered, OuterGrid)

print('Horse1')


		x_inflated <- aggregate(x_inflated)


print('Horse2')

		
		l_test <- gArea(x_inflated) == gArea(OuterGrid)


print('PHorse')

		x_ndx[i] <- l_test
		}
	
	InnerHexagons <- SpPolyHexagonalGrid[x_ndx]
	OuterHexagons <- SpPolyHexagonalGrid[!x_ndx]
	
	if(!l_inner) return(OuterHexagons)
	return(InnerHexagons)
	
	}

#x3 <- HexagonInner(HexagonalGrid)
#x3 <- HexagonInner(HexagonalGrid)
#plot(x)


































































InoutSpatial <- function(inshape, inpoly)
	{
	#inshape <- Drainage.shp
	#inpoly <- Circle
	
	proj4string.shape <- proj4string(inshape)
	proj4string.poly <- proj4string(inpoly)
	
	stopifnot(proj4string.shape == proj4string.poly)
	
	coords.shape <- coordinates(inshape)
	coords.poly <- coordinates(inpoly)
	
	x_ndx <- inout(coords.shape, coords.poly)
	
	}





















































#same as MakeCircle (above) function, but different name

MakeAnnulusSpatialPolygon <- function(crd, n_radius, n_inc)
	{

	#data(Proj4Strings)
	stopifnot(class(crd) %in% c('SpatialPoints', 'SpatialPointsDataFrame'))
	p4 <- proj4string(crd)
	
	Circle1 <- gBuffer(crd, width = n_radius, byid = FALSE)
	Circle2 <- gBuffer(crd, width = n_radius+n_inc, byid = FALSE)
	
	#if(l_lonlat) {Circle <- ConvertNGRToLonLat(Circle)}
	#if(l_lonlat) {Circle <- spTransform(Circle , DecimalLonLat)}
	annulus <- gDifference(Circle2, Circle1)
	return(annulus)
	
	}

#x <- MakeAnnulusSpatialPolygon(Line1.pts[1], 50, 2)

























































MakeCircle <- function(crd, n_radius, l_lonlat = TRUE, l_EIRE = TRUE)
	{

	#data(Proj4Strings)
	stopifnot(class(crd) == 'SpatialPoints')

	if(l_EIRE){coords <- ConvertLonLatToIrishNGR(crd)}
	
	
	Circle <- gBuffer(coords, width = n_radius)
	
	#if(l_lonlat) {Circle <- ConvertNGRToLonLat(Circle)}
	if(l_lonlat) {Circle <- spTransform(Circle , DecimalLonLat)}
	return(Circle)
	
	}

#out <- MakeCircle(c(-7,53), 1000)
#y1 = SpatialPoints(y,  proj4string =  CRS('+proj=longlat +datum=WGS84 +no_defs '))

#ConvertDataFrameToSpatialPoints(y)



#xx <- c(-7,53)
#xx1 <- ConvertMatrixToDataFrame(xx)
#xx2 <- ConvertDataFrameToSpatialPoints(xx1)
#out <- MakeCircle(xx2, 1000)
#out <- MakeCircles(xx2, 1000)
#out <- MakeCircles(xx2, 1000, l_lonlat = FALSE)
#xx2 <- GPS::MakeCircle(a1, 10)






















































































#same as MakeCircle (above) function, but different name

MakeCircleSpatialPoints <- function(indata, n_radius, c_proj4 = init29903, l_lonlat = TRUE, l_ConvertToPoly = FALSE, l_PrintClass = TRUE)
	{

	#data(Proj4Strings)
	stopifnot(class(indata) == 'SpatialPoints')
	#p4 <- GetProj4(indata)
	
	coords <- try(spTransform(indata, c_proj4))
	Circle <- gBuffer(coords, width = n_radius)


	if(!l_ConvertToPoly)
		{
		Circle <- ConvertSpatialPolygonsToSpatialPoints(Circle)
		}
		
	if(l_lonlat) {Circle <- spTransform(Circle , DecimalLonLat)}

	
	if(l_PrintClass)
		{
		print(class(Circle))
		}
	
	
	return(Circle)
	
	}

#out <- MakeCircleSpatialPoints(a1,2)
#y1 = SpatialPoints(y,  proj4string =  CRS('+proj=longlat +datum=WGS84 +no_defs '))

#ConvertDataFrameToSpatialPoints(y)
#out22 <- MakeCircleSpatialPoints(test_x1, n_radius = 10, c_proj4 = Mercator)
#out22 <- MakeCircleSpatialPoints(test_x1, n_radius = 10, c_proj4 = Mercator, l_lonlat = FALSE)
#out22 <- MakeCircleSpatialPoints(test_x1, n_radius = 10, c_proj4 = Mercator, l_lonlat = FALSE, l_ConvertToPoly = TRUE)


#out22 <- MakeCircleSpatialPoints(test_x1, n_radius = 100, c_proj4 = Mercator, l_lonlat = FALSE, l_ConvertToPoly = TRUE)


















































#same as MakeCircle (above) function, but different name

MakeCircleSpatialPoints.Arc <- function(indata, n_radius, c_crs = EIREgridInitString)
	{

	#data(Proj4Strings)
	stopifnot(class(indata) == 'SpatialPoints')
	p4 <- GetProj4(indata)
	
	coords <- spTransform(indata, CRS(c_crs))
	Circle <- gBuffer(coords, width = n_radius)

	Circle <- ConvertSpatialPolygonsToSpatialPoints(Circle)

	print(class(Circle))
	return(Circle)
	
	}

#out <- MakeCircleSpatialPoints(a1,2)
#y1 = SpatialPoints(y,  proj4string =  CRS('+proj=longlat +datum=WGS84 +no_defs '))

#ConvertDataFrameToSpatialPoints(y)





























































































#same as MakeCircle (above) function, but different name

MakeCircleSpatialPolygon <- function(crd, n_radius)
	{

	#data(Proj4Strings)
	crd <- try(as(crd, 'SpatialPoints'))
	stopifnot(class(crd) == 'SpatialPoints')
	p4 <- proj4string(crd)
	
	Circle <- gBuffer(crd, width = n_radius, byid = FALSE)
	
	#if(l_lonlat) {Circle <- ConvertNGRToLonLat(Circle)}
	#if(l_lonlat) {Circle <- spTransform(Circle , DecimalLonLat)}
	return(Circle)
	
	}

#MakeCircleSpatialPolygon



















































































#same as MakeCircle (above) function, but different name

MakeCircleSpatialPolygon.Arc <- function(crd, n_radius, l_lonlat = TRUE, l_EIRE = TRUE)
	{

	#data(Proj4Strings)
	stopifnot(class(crd) == 'SpatialPoints')
	p4 <- proj4string(crd)
	
	if(l_EIRE){coords <- ConvertLonLatToIrishNGR(crd)}
	
	
	Circle <- gBuffer(crd, width = n_radius, byid = FALSE)
	
	#if(l_lonlat) {Circle <- ConvertNGRToLonLat(Circle)}
	if(l_lonlat) {Circle <- spTransform(Circle , DecimalLonLat)}
	return(Circle)
	
	}

#MakeCircleSpatialPolygon
#out <- MakeCircle(c(-7,53), 1000)
#y1 = SpatialPoints(y,  proj4string =  CRS('+proj=longlat +datum=WGS84 +no_defs '))

#ConvertDataFrameToSpatialPoints(y)



#xx <- c(-7,53)
#xx1 <- ConvertMatrixToDataFrame(xx)
#xx2 <- ConvertDataFrameToSpatialPoints(xx1)
#xx2 <- ConvertLonLatDataFrameToSpatialPoints(xx1)
#out <- MakeCircle(xx2, 1000)
#out <- MakeCircle(xx2, 1000)
#out <- MakeCircle(xx2, 1000, l_lonlat = FALSE)
#xx2 <- MakeCircleSpatialPolygon(a1, 10)






























































MakeCircleSpatialPoints.Arc <- function(indata, n_radius)
#l_lonlat = TRUE, l_EIRE = TRUE)
	{
	
	#indata = a1
	stopifnot(class(indata) == 'SpatialPoints')
	p4 <- proj4string(indata)

	coords <- geom(indata)[,c('x','y')]
	coords <- data.frame(coords['x'], coords['y'])

	Circle <- gBuffer(coords, width = n_radius)
	
	
	outdata <- geom(Circle)[,c('x','y')]
	
	
	#n_mid <- c(267200, 194800)
	#xx <- parker::MakeCircle(indata, n_radius)
	#xx1 <- ConvertNGRDataFrameToSpatialPoints(xx)

	
	#xx1 <- MakeSpatialPolygon(xx1)


	return(outdata)
	}

#xx2 <- MakeCircleSpatialPoints(c(267200, 194800), 100)
#xx2 <- MakeCircleSpatialPoints(a1, 10)

























































MakeCircleSpatialPoints.Arc <- function(n_mid, n_radius, c_proj4 = 'EIREgridInitString')#l_lonlat = TRUE, l_EIRE = TRUE)
	{
	#data(Proj4Strings)
	#stopifnot(class(crd) == 'SpatialPoints')

	
	#n_mid <- c(267200, 194800)
	xx <- parker::MakeCircle(n_mid, n_radius)
	xx1 <- ConvertNGRDataFrameToSpatialPoints(xx)

	
	#xx1 <- MakeSpatialPolygon(xx1)


	return(xx1)
	}

#xx2 <- MakeCircleSpatialPoints(c(267200, 194800), 100)



























































































































MakeEmptySp.Arc <- function(p4)
	{
	out <- SpatialPoints(data.frame(x=0, y=0))[-1]
	proj4string(out) <- p4
	return(out)
	}


#MakeEmptySp(init29903)























































MakeEmptySp <- function (p4, n_element = 0)
	{
    out <- SpatialPoints(data.frame(x = 0, y = 0))[-1]
    
    if(n_element >0)
		{
		indataframe <- data.frame(x = rep(0,n_element), y = rep(0,n_element))
		
		
		#out1 <- SpatialPoints(data.frame(x = 0, y = 0))
		out <- SpatialPoints(indataframe)
		}
    
   
    proj4string(out) <- p4
    return(out)
	}



#MakeEmptySp(p4=init29903, n_element = 0)
#MakeEmptySp(p4=init29903, n_element = 1)
#MakeEmptySp(p4=init29903, n_element = 2)
#MakeEmptySp(p4=init29903, n_element = 10)

















































































MakeHexagon <- function(SpPoints, n_radius = 1, l_Sp = TRUE, p4new = NA)
	{

	stopifnot(class(SpPoints) == 'SpatialPoints')

	data(Proj4Strings)
	p4 <- proj4string(SpPoints)
	


	p4new <- try(get(p4new))

	print(p4new)

	if(is.na(p4new)) p4new <- p4
	
	SpPoints <- spTransform(SpPoints, p4new)


	n_centre <- coordinates(SpPoints)
	#library(spatstat)
	#out <- hexagon(centre = SpPoints, edge = n_radius)
	out <- hexagon(centre = n_centre, edge = n_radius)
	#n_centre
	
	out1 <- as(out, 'SpatialPolygons')
	proj4string(out1) <- p4new
	#geom(out1)
	
	return(out1)
	}



#MakeHexagon(xx2)
#MakeHexagon(xx2, p4new = 'init29903')

























































































MakeHexagonalGrid.Arc <- function(SpPoly, n_factor = 2, n_cellsize, l_Simplify = TRUE, nTol = 50)
	{
	
	p4 <- proj4string(SpPoly)
	
	SpPoly <- try(as(SpPoly, 'SpatialPolygons'))
	SpPoints <- ConvertSpatialPolygonsToSpatialPoints(SpPoly)

	if(l_Simplify)  SpPoly <- SimplifyPolygon(SpPoly, l_Sp= TRUE, n_Tol = nTol)
	n_width <- 1

	l_contained <- FALSE


	while (!l_contained)
		{
		print(n_width)
		poly.large <- gBuffer(SpPoly, width = n_width)
		
		
		HexPts <- spsample(poly.large, type="hexagonal", cellsize = n_cellsize)
		HexPols <- HexPoints2SpatialPolygons(HexPts)
		
		HexPols.aggregate <- aggregate(HexPols)
		
		ndx <- PointInPoly(SpPoints, HexPols.aggregate, l_points = FALSE)
		
		#if(!is.element(ndx, FALSE)) l_contained <- TRUE
		if(!is.element(FALSE, ndx)) l_contained <- TRUE


		n_width <- n_width * n_factor
		
		
		
		}
	
	HexPols <- spTransform(HexPols, p4)
	x_ndx <- rep(NA, length(HexPols))
	
	for(i in 1:length(HexPols))
		{
		data_sub <- HexPols[i]
		x_ndx[i] <- gIntersects(data_sub, Catch.shp)
		}
	
	print(x_ndx)
	HexPols <- HexPols[x_ndx]
	
	
	return(HexPols)
	#return(ndx)
	}



#x4 <- HexagonalGrid <- MakeHexagonalGrid(Catch.shp, n_cellsize = 1000)
#x1 <- MakeHexagonalGrid(boundary, n_cellsize = 500)
#x2 <- MakeHexagonalGrid(boundary, n_cellsize = 1000)
#x3 <- MakeHexagonalGrid(boundary, n_cellsize = 600)
#boundary <- Catch.shp
#plot(x2); plot(boundary, add = TRUE)























































MakeHexagonalGrid <- function(SpPoly, n_factor = 2, n_cellsize, l_Simplify = TRUE, nTol = 50)
	{
	
	p4 <- proj4string(SpPoly)
	
	SpPoly <- try(as(SpPoly, 'SpatialPolygons'))
	SpPoints <- ConvertSpatialPolygonsToSpatialPoints(SpPoly)

	if(l_Simplify)  SpPoly <- SimplifyPolygon(SpPoly, l_Sp= TRUE, n_Tol = nTol)
	n_width <- 1

	l_contained <- FALSE


	while (!l_contained)
		{
		print(n_width)
		poly.large <- gBuffer(SpPoly, width = n_width)
		
		
		HexPts <- spsample(poly.large, type="hexagonal", cellsize = n_cellsize)
		HexPols <- HexPoints2SpatialPolygons(HexPts)
		
		HexPols.aggregate <- aggregate(HexPols)
		
		ndx <- PointInPoly(SpPoints, HexPols.aggregate, l_points = FALSE)
		
		#if(!is.element(ndx, FALSE)) l_contained <- TRUE
		if(!is.element(FALSE, ndx)) l_contained <- TRUE


		n_width <- n_width * n_factor
		
		
		
		}
	
	HexPols <- spTransform(HexPols, p4)
	x_ndx <- rep(NA, length(HexPols))
	
	for(i in 1:length(HexPols))
		{
		data_sub <- HexPols[i]
		#x_ndx[i] <- gIntersects(data_sub, Catch.shp)
		x_ndx[i] <- gIntersects(data_sub, SpPoly)
		}
	
	print(x_ndx)
	HexPols <- HexPols[x_ndx]
	
	
	return(HexPols)
	#return(ndx)
	}



#x4 <- HexagonalGrid <- MakeHexagonalGrid(Catch.shp, n_cellsize = 1000)
#x1 <- MakeHexagonalGrid(boundary, n_cellsize = 500)
#x2 <- MakeHexagonalGrid(boundary, n_cellsize = 1000)
#x3 <- MakeHexagonalGrid(boundary, n_cellsize = 600)
#boundary <- Catch.shp
#plot(x2); plot(boundary, add = TRUE)












































































































MakePoly.Arc <- function(indata)
	{

	stopifnot(class(indata) %in% c('data.frame','SpatialPoints'))
	
	if(class(indata) == 'data.frame'){indata <- ConvertDataFrameToSpatialPoints(indata)}

	out.poly <- Polygon(indata)
	
	return(out.poly)
	}










































MakePoly <- function(indata)
	{

	stopifnot(class(indata) == 'SpatialPoints')
	
	out.poly <- Polygon(indata)
	return(out.poly)
	}






















































































#converts to Bbox
MakeSpatialPolygon <- function(indata)
	{
	stopifnot(class(indata) == 'SpatialPoints')
	
	outdata <- as(raster::extent(indata), "SpatialPolygons")
	
	# add projection information
	proj4string(outdata) <- proj4string(indata)
	
	return(outdata)
	}


#MakeSpatialPolygon(xx4)














































MakeSpatialPolygon.Temp <- function(indata)
	{

	out.poly <- Polygon(indata)
	out.poly <- out.poly@coords

	out.poly <- data.frame(out.poly)
	
	out.poly <- SpatialPoints(indata,  proj4string =  CRS('+proj=longlat +datum=WGS84 +no_defs '))
	
	stopifnot(class(indata) == 'data.frame')
	stopifnot(names(indata) %in% c('lon', 'lat'))
	
	#make sure order is correct
	indata <- indata[c('lon', 'lat')]

	y1 = SpatialPoints(indata,  proj4string =  CRS('+proj=longlat +datum=WGS84 +no_defs '))
	
	return(y1)
	
	}
















































MakeSquare <- function(crd, n_length, l_lonlat = TRUE, l_EIRE = TRUE, l_outpoly = TRUE)
	{
	stopifnot(class(crd) == 'SpatialPoints')

	if(l_EIRE){coords <- ConvertLonLatToIrishNGR(crd)}
	
	coords_df <- as.data.frame(coords)
	coords_df <- rbind(coords_df, coords_df, coords_df, coords_df)
	
	
	coords_df[2,2] <- coords_df[2,2] + n_length
	coords_df[3,] <- coords_df[1,] + n_length
	coords_df[4,1] <- coords_df[4,1] + n_length
	#coords
	
	out.data <- ConvertNGRDataFrameToSpatialPoints(coords_df)
	#out.data <- ConvertNGRToLonLat(coords_df)
	
	if(l_lonlat){out.data <- spTransform(out.data, DecimalLonLat)}
	if(l_outpoly){out.data <- MakePoly(out.data)}
	
	
	#if(l_lonlat) {Circle <- ConvertNGRToLonLat(Circle)}
	#if(l_lonlat) {out.data <- spTransform(out.data , DecimalLonLat)}
	return(out.data)
	}



#x <- c( -4, 52)
#ConvertMatrixToDataFrame(v_crd)

#v_crd <- c(-7,53)
#v_crd <- ConvertMatrixToDataFrame(v_crd)
#v_crd <- ConvertLonLatDataFrameToSpatialPoints(v_crd)
#outSquare <- MakeSquare(v_crd, n_length = 1000)


































































MakeSquareSpatial <- function(crd, n_length, l_lonlat = TRUE, l_EIRE = TRUE)
	{
	stopifnot(class(crd) == 'SpatialPoints')

	if(l_EIRE){coords <- ConvertLonLatToIrishNGR(crd)}
	
	coords_df <- as.data.frame(coords)
	coords_df <- rbind(coords_df, coords_df, coords_df, coords_df)
	
	
	coords_df[2,2] <- coords_df[2,2] + n_length
	coords_df[3,] <- coords_df[1,] + n_length
	coords_df[4,1] <- coords_df[4,1] + n_length
	#coords
	
	out.data <- ConvertNGRDataFrameToSpatialPoints(coords_df)
	#out.data <- ConvertNGRToLonLat(coords_df)

	out.data <- MakePoly(out.data)
	
	
	#if(l_lonlat) {Circle <- ConvertNGRToLonLat(Circle)}
	#if(l_lonlat) {out.data <- spTransform(out.data , DecimalLonLat)}
	return(out.data)
	}



#x <- c( -4, 52)
#ConvertMatrixToDataFrame(v_crd)

#v_crd <- c(-7,53)
#v_crd <- ConvertMatrixToDataFrame(v_crd)
#v_crd <- ConvertLonLatDataFrameToSpatialPoints(v_crd)
#outSquare <- MakeSquare(v_crd, n_length = 1000)

















































MakeSquare.Arc <- function(v_crd, n_length, country = 'EIRE')
	{

	
	coords <- ConvertMatrixToSpatialPoints(v_crd)
	
	if(country == 'EIRE')coords <- ConvertLonLatToIrishNGR(coords)
	if(country == 'UK')coords <- ConvertLonLatToUKNGR(coords)
	
	coords_df <- as.data.frame(coords)
	coords_df <- rbind(coords_df, coords_df, coords_df, coords_df)


	coords_df[2,2] <- coords_df[2,2] + n_length
	coords_df[3,] <- coords_df[1,] + n_length
	coords_df[4,1] <- coords_df[4,1] + n_length
	#coords
	
	out.data <- ConvertDataFrameToSpatialPoints(coords_df)
	out.data <- ConvertNGRToLonLat(coords_df)

	out.data <- MakePoly(out.data)
	
	return(out.data)
	}


#v_crd <- c(-7,53)
#outSquare <- MakeSquare(c(-7,53), n_length = 1000)























MakeSquare.Arc2 <- function(indata, n_length, country = 'EIRE')
	{

	stopifnot(class(indata == 'SpatialPoints'))
	coords <- ConvertMatrixToSpatialPoints(v_crd)
	
	if(country == 'EIRE')coords <- ConvertLonLatToIrishNGR(coords)
	if(country == 'UK')coords <- ConvertLonLatToUKNGR(coords)
	
	coords_df <- as.data.frame(coords)
	coords_df <- rbind(coords_df, coords_df, coords_df, coords_df)


	coords_df[2,2] <- coords_df[2,2] + n_length
	coords_df[3,] <- coords_df[1,] + n_length
	coords_df[4,1] <- coords_df[4,1] + n_length
	#coords
	
	out.data <- ConvertDataFrameToSpatialPoints(coords_df)
	out.data <- ConvertNGRToLonLat(coords_df)

	out.data <- MakePoly(out.data)
	
	return(out.data)
	}


#v_crd <- c(-7,53)
#outSquare <- MakeSquare(c(-7,53), n_length = 1000)
