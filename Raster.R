
#CalcAreaRaster
#CircularTransect
#CrossSectionElevation
#ExtractTransectDEM
#FindOutlet
#FloodExtent
#GetHighLowPoint
#RasterCentroid
#RasterEdge
#RasterEdgeWithContainerPoly
#RasterIntersect
#RasterToDataFrame
#RasterToSpatialPoints



























CalcAreaRaster <- function(inRaster)
	{
	
	n_values <- getValues(inRaster)
	n_values <- n_values[!is.na(n_values)]
	
	n_count <- length(n_values)
	
	baseArea <- res(inRaster)[1] * res(inRaster)[2]
	TotalAreaRaster <- n_count * baseArea
	TotalAreaRaster <- sum(TotalAreaRaster)
	
	return(TotalAreaRaster)
	}

#CalcAreaRaster(TestZoneRaster)
































CircularTransect <- function(CentrePoint, n_radius, inDEM, n_res = NA)
	{

	
	if(is.na(n_res))
		{
		n_res <- res(inDEM)
		#too difficult to automate this choice
		stopifnot(n_res[1] == n_res[2])
		n_res <- n_res[1]

		}

	doughnut <- MakeAnnulusSpatialPolygon(CentrePoint, n_radius, n_res)
	#CircTrn <- TotalClipRaster(doughnut, inDEM)
	CircTrn <- TotalClipRaster(inDEM, doughnut)
	return(CircTrn)
	}


#x <- CircularTransect(Line1.pts[1], 50, dem_2mAnast)


















































CrossSectionElevation <- function(InLine, inDEM, n_res = NA, c_label = 'elevation')
	{

		
	p_4 <- 	proj4string(inDEM)
	InLine <- spTransform(InLine, p_4)

	
	transectDEM <- ExtractTransectDEM(InLine, inDEM, n_res = n_res)
	transectDEM.df <- RasterToDataFrame(transectDEM, c_label = c_label)


	nValues <- values(transectDEM)
	transectDEM.min <- min(nValues, na.rm = TRUE)
	transectDEM.max <- min(nValues, na.rm = TRUE)
	
	
	InLine.pts <- as(InLine, 'SpatialPoints')
	
	#coordinates(transectDEM.df) <- ~x+y

	b1 <- CalcDistance(transectDEM.df, InLine.pts[1])
	transectDEM.df$distance <- b1
	transectDEM.df$relative <- as.matrix(transectDEM.df[c_label]@data - transectDEM.min)


	return(transectDEM.df)
	}


#transectDEM <- ExtractTransectDEM(Line1, dem_2mAnast)
#transectDEM.df <- RasterToDataFrame(transectDEM)
#CrossSectionElevation(Line1, dem_2mAnast)

#demT1.pts <- CrossSectionElevation(SlopeTransects[1,], dem.Ballycanew, 5)
#plot(demT1.pts$distance, demT1.pts$relative, xlim = c(100,0))























































ExtractTransectDEM <- function(InLine, inDEM, n_res = NA)
	{
	
	p_4 <- 	proj4string(inDEM)
	
	InLine <- spTransform(InLine, p_4)
	
	if(is.na(n_res))
		{
		n_res <- res(inDEM)
		#too difficult to automate this choice
		stopifnot(n_res[1] == n_res[2])
		n_res <- n_res[1]

		}
	
	InLinePoly <- gBuffer(InLine, width = n_res, byid = FALSE)
	
	outDEM <-TotalClipRaster(inDEM, InLinePoly)
	return(outDEM)
	}

#transectDEM <- ExtractTransectDEM(Line1, dem_2mAnast)







































FindOutlet <- function(ContributingArea)
	{

	#ContributingArea is a product of 	rsaga.topdown.processing in CalcContributingArea()
	#class(ContributingArea) is likely to be RasterLayer
	
	
	stopifnot(class(ContributingArea) %in% c('Raster','RasterLayer','SpatialGridDataFrame','SpatialPixelsDataFrame'))

	stopifnot(!is.na(proj4string(ContributingArea)))
	
	
	
	p_4 <- proj4string(ContributingArea)

	maxArea <- cellStats(ContributingArea, max)

	ContributingArea <- as(ContributingArea, 'SpatialPixelsDataFrame')

	
	pick_ndx <- which(ContributingArea@data == maxArea)



	location <- ContributingArea@coords[pick_ndx,]
	location <- data.frame(t(location))
	
	#location.sp = SpatialPoints(location,  proj4string =  p_4)
	location.sp = SpatialPoints(location)
	proj4string(location.sp) <- p_4
	
	return(location.sp)
	}


#field.outlet <- FindOutlet(dem.c)






























































FindOutletMultiple <- function(ContributingArea, PickHowMany = 20, l_convertToSp = FALSE)
	{

	#ContributingArea is a product of 	rsaga.topdown.processing in CalcContributingArea()
	#class(ContributingArea) is likely to be RasterLayer
	
	
	stopifnot(class(ContributingArea) %in% c('Raster','RasterLayer','SpatialGridDataFrame','SpatialPixelsDataFrame'))

	stopifnot(!is.na(proj4string(ContributingArea)))
	
	
	p4 <- proj4string(ContributingArea)
	
	n_values <- getValues(ContributingArea)
	ndx <- order(n_values, decreasing = TRUE)
	
	n_valuesSelect <- n_values[ndx[1:PickHowMany]]
	#print(n_valuesSelect)
	ContributingArea <- as(ContributingArea, 'SpatialPixelsDataFrame')

	
	#pick_ndx <- which(ContributingArea@data == n_valuesSelect)
	#pick_ndx <- ContributingArea@data == n_valuesSelect
	#print(ContributingArea@data)
	pick_ndx <- ContributingArea@data[,1] %in% n_valuesSelect
	#print(pick_ndx)


	location <- ContributingArea@coords[pick_ndx,]
	location <- as.data.frame(location)
	
	SelectedValues <- ContributingArea@data[pick_ndx,1]
	newOrder <- order(SelectedValues, decreasing = TRUE)
 
	SelectedValues <- SelectedValues[newOrder]
	#order according to largest first
	location <- location[newOrder,]
	#return(SelectedValues)
	
	#location <- data.frame(t(location))
	#print(location)
	out.points <- st_as_sf(x = location, coords = c('x','y'), crs = p4)
    out.points$Carea <- SelectedValues
    
    if (l_convertToSp) {
        out.points <- as(out.points, "Spatial")
		}
    
    
	#location.sp = SpatialPoints(location,  proj4string =  p_4)
	#location.sp = SpatialPoints(location)
	#proj4string(location.sp) <- p_4
	
	return(out.points)
	#return(location.sp)
	}


#field.outlets <- FindOutletMultiple(CareaEdge)















































































FloodExtent <- function(inRaster, TestVolume = 100)
	{

	minHeight <- min(getValues(inRaster), na.rm = TRUE)
	maxHeight <- max(getValues(inRaster), na.rm = TRUE)

	baseArea <- res(inRaster)[1] * res(inRaster)[2]

	InitialValues <- getValues(inRaster)


	HeightTest <- minHeight


	l_test <- FALSE
	while(!l_test)
		{
		x <- pmax(InitialValues, HeightTest)
		TotalVolume <- sum((x - InitialValues) * baseArea, na.rm = TRUE)

		l_test <- (TotalVolume > TestVolume)
		
		HeightTest <- HeightTest + 0.1
		}
		


	HeightTest <- HeightTest - 0.2

	l_test <- FALSE
	while(!l_test)
		{
		x <- pmax(InitialValues, HeightTest)
		TotalVolume <- sum((x - InitialValues) * baseArea, na.rm = TRUE)

		l_test <- (TotalVolume > TestVolume)
		
		HeightTest <- HeightTest + 0.05
		}





	HeightTest <- HeightTest - 0.1

	l_test <- FALSE
	while(!l_test)
		{
		x <- pmax(InitialValues, HeightTest)
		TotalVolume <- sum((x - InitialValues) * baseArea, na.rm = TRUE)

		l_test <- (TotalVolume > TestVolume)
		
		HeightTest <- HeightTest + 0.01
		}



	FloodCompare <- (x != InitialValues)
	FloodCompare <- is.element(FloodCompare, TRUE)

	
	outRaster <- inRaster
	outRaster[!FloodCompare] <- NA
	
	outRaster[FloodCompare] <- HeightTest
	
	#outRaster <- inRaster[FloodCompare]
	#outRaster <- inRaster[[which(FloodCompare)]]
	#xy_coord <- xyFromCell(inRaster, FloodCompare)

	#out <- list(outRaster
	#return(FloodCompare)
	return(outRaster)
	#return(x)
	}
	
#xs <- FloodExtent(TestZoneRaster)




































#this function would be better 
GetHighLowPoint <- function(indem, l_low = FALSE, l_NoDuplicates = TRUE, l_convertToSp  = TRUE)
	{

	p_4 <- proj4string(indem)
	stopifnot(!is.na(p_4))
	
	
	ndx_hi <- raster::which.max(indem)
	#only one just in case there are duplicates
	if(l_NoDuplicates)	ndx_hi <- ndx_hi[1]
	
	High <- extract(indem, ndx_hi)
	
	
	
	ndx_lo <- raster::which.min(indem)
	#only one just in case there are duplicates
	if(l_NoDuplicates)	ndx_lo <- ndx_lo[1]
	
	Low <- extract(indem, ndx_lo)
	
	
	xy_coord <- xyFromCell(indem, c(ndx_lo, ndx_hi))
	#xy_coord <- ConvertMatrixToSpatialPoints(xy_coord, p4string = p_4)
	
	xy_coord <- as.data.frame(xy_coord)
	xy_coord <- st_as_sf(xy_coord, coords = c('x','y'), crs = p_4)

	xy_coord$ElevationPoint <- c('Low', 'High')
	
	xy_coord$Elevation <- c(Low, High)

	
	if(l_low)	{ xy_coord <- xy_coord[xy_coord$ElevationPoint == 'Low',] }
	

	if (l_convertToSp)        {xy_coord <- as(xy_coord, "Spatial")}

	return(xy_coord)
	}

#LowPoint <- GetHighLowPoint(dem.Field)
#LowPoint <- GetHighLowPoint(dem.Field, l_convertToSp = T)
#LowPoint























































#this function would be better 
GetHighLowPoint.Arc1 <- function(indem, l_low = FALSE, l_convertToSp  = TRUE)
	{

	p_4 <- proj4string(indem)
	stopifnot(!is.na(p_4))
	
	
	ndx_hi <- raster::which.max(indem)
	High <- extract(indem, ndx_hi)
	
	ndx_lo <- raster::which.min(indem)
	Low <- extract(indem, ndx_lo)
	
	
	xy_coord <- xyFromCell(indem, c(ndx_lo, ndx_hi))
	#xy_coord <- ConvertMatrixToSpatialPoints(xy_coord, p4string = p_4)
	
	xy_coord <- as.data.frame(xy_coord)
	xy_coord <- st_as_sf(xy_coord, coords = c('x','y'), crs = p_4)

	xy_coord$ElevationPoint <- c('Low', 'High')
	
	xy_coord$Elevation <- c(Low, High)

	
	if(l_low)	{ xy_coord <- xy_coord[xy_coord$ElevationPoint == 'Low',] }
	

	if (l_convertToSp)        {xy_coord <- as(xy_coord, "Spatial")}

	return(xy_coord)
	}

#LowPoint <- GetHighLowPoint(dem.Field)
#LowPoint <- GetHighLowPoint(dem.Field, l_convertToSp = T)
#LowPoint






























































GetHighLowPoint.Arc <- function(indem, l_low = FALSE, l_convertToSp  = FALSE)
	{

	p_4 <- proj4string(indem)
	stopifnot(!is.na(p_4))
	
	
	ndx_hi <- raster::which.max(indem)
	High <- extract(indem, ndx_hi)
	
	ndx_lo <- raster::which.min(indem)
	Low <- extract(indem, ndx_lo)
	
	
	xy_coord <- xyFromCell(indem, ndx_lo)
	#xy_coord <- ConvertMatrixToSpatialPoints(xy_coord, p4string = p_4)
	
	xy_coord <- as.data.frame(xy_coord)
	xy_coord <- st_as_sf(xy_coord, coords = c('x','y'), crs = p_4)

	xy_coord1 <- xyFromCell(indem, ndx_hi)
	xy_coord1 <- as.data.frame(xy_coord1)
	xy_coord1 <- st_as_sf(xy_coord1, coords = c('x','y'), crs = p_4)

	xy_coord <- rbind(xy_coord, xy_coord1)
	xy_coord$ElevationPoint <- c('Low', 'High')
	
	xy_coord$Elevation <- c(Low, High)

	
	if(l_low)	{ xy_coord <- xy_coord[xy_coord$ElevationPoint == 'Low',] }
	

	if (l_convertToSp)        {xy_coord <- as(xy_coord, "Spatial")}

	return(xy_coord)
	}

#LowPoint <- GetHighLowPoint.Arc(dem.Field)
#LowPoint <- GetHighLowPoint.Arc(dem.Field, l_convertToSp = T)
#LowPoint















	













































































HamsterWheel <- function(inDEM, inDEMLarge, CentrePoint = NA, l_singlePointPair = TRUE, l_SpLine = TRUE, l_SpPolygon = FALSE, n_res = 2)
	{
	
	
	#inDEM <- SinglePointAnnulusCircularTransect

	inDEM.df <- RasterToDataFrame(inDEM)
	inDEM.df$ElevDiff <- NA
	inDEM.df$PointPair <- NA
	inDEM.df$ID <- 1:dim(inDEM.df)[1]
	
	if(is.na(CentrePoint))
		{
		CentrePoint <- RasterCentroid(inDEM)
		}


	oppositePoints <- RotateSpatialPoint(inDEM.df, CentrePoint)



	n_dim <- dim(inDEM.df)



	for(i in 1:n_dim[1])
		{
		StartPoint <- inDEM.df[i,]
		ComparePoint <- oppositePoints[i,]
		
		ComparePointBuffer <- gBuffer(ComparePoint, n_res, byid = FALSE)
		ComparePointRaster <- TotalClipRaster(inDEMLarge, ComparePointBuffer)
	
		ComparePointRasterValues <- getValues(ComparePointRaster)
		n_increment <- n_res
		
		while(is.na(ComparePointRasterValues))
			{
			ComparePointBuffer <- MakeCircleSpatialPolygon(ComparePoint, n_increment)
			ComparePointRaster <- TotalClipRaster(inDEMLarge, ComparePointBuffer)
			ComparePointRasterValues <- getValues(ComparePointRaster)
			
			n_increment <- n_increment + 1
			}

		ComparePointRasterValues <- mean(ComparePointRasterValues, na.rm = TRUE)
		
		stopifnot(!is.na(ComparePointRasterValues))
		
		ElevDiff <- ComparePointRasterValues - StartPoint$elevation
		
		inDEM.df$ElevDiff[i] <- abs(ElevDiff)
		#inDEM.df$PointPair[i] <- x_ndx
		}


	x_ndx <- which.min(inDEM.df$ElevDiff)
	
	Point1 <- inDEM.df[x_ndx,]
	Point2 <- oppositePoints[x_ndx]

	outPoints <- MergeSpatialPoints(Point1, Point2)

	out <- outPoints

	if(l_SpLine)
		{
		out <- as(outPoints, 'SpatialLines')
		if(l_SpPolygon)
			{
			out <- try(gBuffer(out, n_res, byid = FALSE))
			}
		}	

	
	if(!l_singlePointPair)
		{
		out <- list(inDEM.df, oppositePoints)
		}
	
	return(out)
	}


#outHamster <- HamsterWheel(x)


#outHamster <- HamsterWheel(inDEM=SinglePointAnnulusCircularTransect,inDEMLarge=SinglePointCircleDEM, CentrePoint = NA, n_res = 2)

#plot(SinglePointAnnulusCircularTransect); plot(outHamster, add = T)






























































































RasterCentroid <- function(inRaster)
	{
	
	SpPoints <- ConvertRasterToSpatialPoints(inRaster)
	
	centre <- gCentroid(SpPoints)
	
	return(centre)
	}


#RasterCentroid(FloodMap)



























































RasterEdge <- function(inRaster, n_wide = 20, l_index = FALSE)
	{

	out <- RasterEdge1(inRaster = inRaster, n_wide = n_wide, l_index = l_index)
	return(out)

	}


#x <- RasterEdge(dem.Field)
#x <- RasterEdge(dem.Field, l_index = TRUE)









RasterEdge1 <- function(inRaster, n_wide = 20, l_index = FALSE)
	{

	boxx <- BoundingBox(inRaster)
	boxx <- InflatePolygon(boxx, n_wide)
	
	extent.large <- extent(boxx)

	inRaster <- try(extend(inRaster, extent.large))

	
	
	demEdge <- boundaries(inRaster)
	demEdge[demEdge == 0] <- NA
	
	if(!l_index)
		{
		demEdge <- mask(inRaster, demEdge)
		}

	return(demEdge)
	}


#x <- RasterEdge(dem.Field.minus)
#RasterFieldEdgeIndex <- RasterEdge(dem.Field.minus, l_index = TRUE)





























RasterEdge.Arc1 <- function(inDem, l_index = FALSE)
	{

	demEdge <- boundaries(inDem)
	demEdge[demEdge == 0] <- NA
	
	if(!l_index)
		{
		demEdge <- mask(inDem, demEdge)
		}

	return(demEdge)
	}


#x <- RasterEdge(dem.Field.minus)
#RasterFieldEdgeIndex <- RasterEdge(dem.Field.minus, l_index = TRUE)













































RasterEdgeWithContainerPoly <- function(inRaster, SpPoly, l_index = FALSE)
	{

	p_4 <- proj4string(inRaster)
	SpPoly <- spTransform(SpPoly, p_4)

	inRaster <- TotalClipRaster(inRaster, SpPoly)
	
	demEdge <- boundaries(inRaster)
	demEdge[demEdge == 0] <- NA
	
	if(!l_index)
		{
		demEdge <- mask(inRaster, demEdge)
		}

	return(demEdge)
	}




#CareaEdge <- RasterEdgeWithContainerPoly(carea, field.Selected.minus)
#OutletPoint <- FindOutlet(CareaEdge)
#TestZoneCircle <- MakeCircleSpatialPolygon(OutletPoint, 25, l_lonlat = FALSE, l_EIRE = FALSE)
#x1 <- RasterEdgeWithContainerPoly



















RasterIntersect <- function(ToBeCropped, CroppingRaster)
	{
	
	#this function is a wrapper for crop()	
	stopifnot(proj4string(ToBeCropped) == proj4string(CroppingRaster))
	out <- crop(ToBeCropped, CroppingRaster)
	
	return(out)
	}
	
























RasterToDataFrame <- function(inDEM, c_label = 'elevation')
	{

	p_4 <- proj4string(inDEM)
	inDEM.df <- raster::as.data.frame(inDEM, xy = TRUE)
	
	x_ndx <- !is.na(inDEM.df[3])
	outDF <- inDEM.df[x_ndx,]
	
	names(outDF)[3] <- c_label
	
	coordinates(outDF) <- ~x+y
	
	proj4string(outDF) <- p_4
	return(outDF)
	}



#transectDEM.df <- RasterToDataFrame(transectDEM)


































































		
RasterToSpatialPoints <- function(in.raster)
	{

	#in.raster <- InputGrid.rst
	stopifnot(class(in.raster) %in% c('Raster','RasterLayer','SpatialGridDataFrame','SpatialPixelsDataFrame'))

	p_4 <- proj4string(in.raster)

	in.raster.SpPoly <- rasterToPolygons(in.raster, dissolve = TRUE)
	in.raster <- as(in.raster, 'SpatialPixelsDataFrame')


	location <- in.raster@coords
	location <- data.frame(location)

	
	location.sp = SpatialPoints(location)
	location.sp <- PointInPoly(location.sp, in.raster.SpPoly)
	
	
	proj4string(location.sp) <- p_4
	
	return(location.sp)
	}


#x <- FindOutlet(dem.c)
#x <- RasterToSpatialPoints(dem.d)


