
ConvertLonLatDataFrameToSpatialPoints <- function(indata)
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



ConvertNGRDataFrameToSpatialPoints <- function(indata, proj4name = 'EIREgridInitString')
	{


	stopifnot(class(indata) %in% c('matrix', 'data.frame'))
	#convert to spatial points

	#data(Proj4)
	data(Proj4Strings)
	indata <- SpatialPoints(indata)

	#initString <- Proj4[Proj4$MyName == proj4name, 'ProjString']
	initString <- get(proj4name)
	#assign projection
	proj4string(indata) <- CRS(initString)

	return(indata)
	}


#coords_df <- data.frame(matrix(c(267188,194797.3,267188,194897.3, 267288,194897.3,267288,194797.3), ncol = 2, byrow = TRUE))
#xx4 <- ConvertNGRDataFrameToSpatialPoints(coords_df)





















































ConvertSpatialLinesToSLDF <- function(SpLines, l_convertToSp = TRUE)
	{

	data <- data.frame(ID = 1:length(SpLines))
	rownames(data) <- data$ID
	out <- SpatialLinesDataFrame(SpLines, data, match.ID = FALSE)
	
	return(out)
	}


#xc <- ConvertSpatialLinesToSLDF(data_sub)



































ConvertSpatialPointsToSPDF <- function(SpPoints)
	{

	data <- data.frame(ID = 1:length(SpPoints))
	rownames(data) <- data$ID
	out <- SpatialPointsDataFrame(SpPoints, data, match.ID = FALSE)
	
	return(out)
	}


#xc <- ConvertSpatialPointsToSPDF(data_sub)





































































ConvertSpatialPolygonsToSpatialPoints <- function(SpPoly, l_convertToSp = TRUE)
	{
	stopifnot(class(SpPoly) == 'SpatialPolygons')
	
	p4 <- GetProj4(SpPoly)
	out <- geom(SpPoly)
	
	out1 <- data.frame('x' = out[,'x'], 'y' = out[,'y'])

	out.points <- st_as_sf(x = out1, coords = c("x", "y"), crs = p4)


	if(l_convertToSp)
		{
		out.points <- as(out.points, "Spatial")
		}

	return(out.points)
	}

#ConvertSpatialPolygonsToSpatialPoints(xx2)
	
