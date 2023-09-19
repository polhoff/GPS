
#MergeSpatialPoints
#MovePointByDistance
#Over
#RotateSpatialLine
#RotateSpatialPoint


















































MergeSpatialPoints <- function(SpPoints1, SpPoints2, l_convertToSp = TRUE)
	{
	
	stopifnot(class(SpPoints1) %in% c('SpatialPoints', 'SpatialPointsDataFrame'))
	stopifnot(class(SpPoints2) %in% c('SpatialPoints', 'SpatialPointsDataFrame'))
	
	p4 <- proj4string(SpPoints1)	
	SpPoints2 <- spTransform(SpPoints2, p4)

	
	coords1 <- coordinates(SpPoints1)
	coords2 <- coordinates(SpPoints2)
	
	coords3 <- rbind(coords1, coords2)
	#grid <- indata@grid[ndx,]
	
    indata <- as.data.frame(coords3)
	names(indata) <- c('x','y')
	
	#out.poly <- Polygon(coords3)
    #out.points <- st_as_sf(x = indata, coords = c('x','y'), crs = p4)
    out.points <- st_as_sf(x = indata, coords = c('x','y'), crs = p4)
    
    if (l_convertToSp) {
        out.points <- as(out.points, "Spatial")
		}
    
    
    
	return(out.points)
	}


#xff <- MergeSpatialPoints(StartPoint, ComparePoint)
#xff <- MergeSpatialPoints(xff, CentrePoint)




































































MovePointByDistance <- function(crd, n_distance, direction)
	{

	#crd <- OutletPointTopSp; n_distance = 10; direction = 'e'
	#data(Proj4Strings)
	stopifnot(class(crd) %in% c('SpatialPoints', 'SpatialPointsDataFrame'))
	stopifnot(direction %in% c('e','s','w','n','E','S','W','N'))

	p4 <- proj4string(crd)
	
	coords <- coordinates(crd)
	
	moveSign <- 1
	if(direction %in% c('s','w','S','W')){ moveSign <- -1}

	xy <- 1
	if(direction %in% c('s','n','S','N')){ xy <- 2}

	pickCoord <- as.numeric(coords[,xy])
	newCoord <- pickCoord + (n_distance * moveSign)

	coords[,xy] <- newCoord
	outdata <- SpatialPoints(coords)
	proj4string(outdata) <-  p4

	return(outdata)
	}



#OutletPointTopSp <- as(OutletPointTop, 'Spatial')
#out <- MovePointByDistance(OutletPointTopSp, 10, 'e')
#OutletPointTopSp









































Over <- function(SpPoints, SpPoly)
	{
	x_ndx <- over(SpPoints, SpPoly)
	x_ndx <- as.numeric(x_ndx[,1])
	
	x_ndx[is.na(x_ndx)] <- 0
	
	#x_ndx <- as.logical(x_ndx[,1])
	
	x_ndx <- as.logical(x_ndx)
	out <- SpPoints[x_ndx,]
	
	return(out)
	}

#SelectPoints <- Over(TransectPoints, SelectPoly)
#SelectPoints


























































RotateSpatialLine <- function(SpLine, CentrePoint = NA, n_angle = pi/2, l_convertToSp = TRUE)
	{

	xx <- as(SpLine, 'SpatialPoints')
	if(is.na(CentrePoint))
		{
		a <-  gCentroid(xx)
		}
	
	P1 <- xx[1]
	P2 <- xx[2]

	P1new <- RotateSpatialPoint(P1, a, n_angle = n_angle)
	P2new <- RotateSpatialPoint(P2, a, n_angle = n_angle)

	LineNew <- MergeSpatialPoints(P1new, P2new)

	LineNew <- as(LineNew, 'SpatialLines')
	return(LineNew)
	}


#RotateSpatialLine(PointPair, 2)
#plot(RotateSpatialLine(PointPair, n_angle = 2))
































































RotateSpatialPoint <- function(SpPoints1, SpPoints2, n_angle = pi, l_convertToSp = TRUE)
	{
	p4 <- proj4string(SpPoints1)
	SpPoints2 <- spTransform(SpPoints2, p4)
	
	a1 <- as(SpPoints1, 'ppp')
	a2 <- as(SpPoints2, 'ppp')
	
	a3 <- rotate(a1, angle = n_angle, centre = a2)
	
	out <- as(a3, 'SpatialPoints')
	proj4string(out) <- p4
	return(out)
	}


#qq <- RotateSpatialPoint(CentrePoint, centre = StartPoint)
#qq <- RotateSpatialPoint(CentrePoint, StartPoint)

