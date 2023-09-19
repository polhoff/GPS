
ConvertMatrixToSpatialPoints <- function(indata, l_convertToSp = TRUE, p4string = DecimalLonLat)
	{
	library(sf)

	l_proceed <- FALSE
	
	
	if(class(indata) == 'numeric')
		{
		l_proceed <- c('coords.x1', 'coords.x2') == names(indata)[1:2]

		stopifnot(l_proceed)
		indata <- matrix(indata, ncol = 2)
		names(indata)[1:2] <- c('lon', 'lat')
		}


	stopifnot(class(indata) %in% (c('matrix', 'data.frame')))
	
	
	l_proceed1 <- c('lon', 'lat') == names(indata)[1:2]
	l_proceed2 <- c('coords.x1', 'coords.x2') == names(indata)[1:2]
	l_proceed3 <- c('x', 'y') == names(indata)[1:2]


	l_proceed <- l_proceed1 | l_proceed2 | l_proceed3
	
	stopifnot(l_proceed)


	indata <- as.data.frame(indata)


	#in case of coords.x1, coords.x2
	try(names(indata)[1:2] <- c('lon', 'lat'))


	#out.points <- st_as_sf(x = indata, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")
	out.points <- st_as_sf(x = indata, coords = c("lon", "lat"), crs = p4string)
	#proj4string(out.points) <- p4string

	if(l_convertToSp)
		{
		out.points <- as(out.points, "Spatial")
		}

	return(out.points)
	}


#proj4string(crd1) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


#ConvertMatrixToSpatialPoints(coordinates(HandDP.shp)[1:2,])
#ConvertMatrixToSpatialPoints(coordinates(HandDP.shp)[1,])























































ConvertMatrixToSpatialPoints.Arc <- function(indata, l_convertToSp = TRUE)
	{
	library(sf)

	l_proceed <- FALSE
	
	
	if(class(indata) == 'numeric')
		{
		l_proceed <- c('coords.x1', 'coords.x2') == names(indata)[1:2]

		stopifnot(l_proceed)
		indata <- matrix(indata, ncol = 2)
		names(indata)[1:2] <- c('lon', 'lat')
		}


	stopifnot(class(indata) %in% (c('matrix', 'data.frame')))
	
	
	l_proceed1 <- c('lon', 'lat') == names(indata)[1:2]
	l_proceed2 <- c('coords.x1', 'coords.x2') == names(indata)[1:2]
	l_proceed3 <- c('x', 'y') == names(indata)[1:2]


	l_proceed <- l_proceed1 | l_proceed2 | l_proceed3
	
	stopifnot(l_proceed)
	
	


	indata <- as.data.frame(indata)


	#in case of coords.x1, coords.x2
	try(names(indata)[1:2] <- c('lon', 'lat'))


	out.points <- st_as_sf(x = indata, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

	if(l_convertToSp)
		{
		out.points <- as(out.points, "Spatial")
		}

	return(out.points)
	}


#proj4string(crd1) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


#ConvertMatrixToSpatialPoints(coordinates(HandDP.shp)[1:2,])
#ConvertMatrixToSpatialPoints(coordinates(HandDP.shp)[1,])

