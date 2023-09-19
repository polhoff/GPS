AssignProj4 <- function(indata, p4)
	{
	proj4string(indata) <- CRS(p4)
	return(indata)
	}


GetBbox	<- function(indata)
	{

	stopifnot(class(indata) == "SpatialPolygons")
	outdata <- as(extent(indata), "SpatialPolygons")
	return(outdata)
	}


GetObjectSelfName <- function(indata)
	{
	outname = deparse(substitute(indata))
	return(outname)
	}

#SomeNumber = 1; GetObjectSelfName(SomeNumber)


#You can obtain the EPSG CRS strings from:
#http://spatialreference.org/

GetProj4 <- function(indata)
	{
	proj4string(indata)
	}
