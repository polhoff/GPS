
ConvertNGRToLonLat.Arc <- function(indata, c_CRS = "+init=epsg:4326", country = 'EIRE')
	{

	stopifnot(class(indata) == 'SpatialPoints')

	EIREgrid <- "+init=EPSG:29903"
	latlong = "+init=epsg:4326"

	outdata <- spTransform(infile, CRS(c_CRS))
	}




ConvertNGRToLonLat <- function(indata)
	{
	stopifnot(class(indata) == 'SpatialPoints')
	latlong = "+init=epsg:4326"

	outdata <- spTransform(indata, CRS(latlong))
	return(outdata)
	}






ConvertLonLatToIrishNGR.Arc <- function(infile)
	{

	#infile is assumed to be latitude longitude
	EIREgrid <- "+init=EPSG:29903"
	latlong = "+init=epsg:4326"

	outdata <- spTransform(infile, CRS(EIREgrid))
	return(outdata)
	}


ConvertLonLatToIrishNGR <- function(indata)
	{
	stopifnot(class(indata) == 'SpatialPoints')
	EIREgrid <- "+init=EPSG:29903"

	outdata <- spTransform(indata, CRS(EIREgrid))
	return(outdata)
	}



#use this website to compare:
#https://gnss.osi.ie/new-converter/

#crd3 <- ConvertMatrixToSpatialPoints(c(-7,53))
#ConvertLonLatToIrishNGR(crd3)


#crd3 <- ConvertMatrixToSpatialPoints(c(-6.5,52.3))
#ConvertLonLatToIrishNGR(crd3)

#CSA_shp.latlon <- ConvertToLonLat(CSA_shp)








#https://stephendavidgregory.github.io/useful/UKgrid_to_LatLon

ConvertLonLatToUKNGR.Arc <- function(infile)
	{

	#infile is assumed to be latitude longitude
	EIREgrid <- "+init=EPSG:29903"
	UKgrid <- "+init=epsg:27700"
	latlong = "+init=epsg:4326"
	
	
	outdata <- spTransform(infile, CRS(UKgrid))
	}
	


ConvertLonLatToUKNGR <- function(indata)
	{
	stopifnot(class(indata) == 'SpatialPoints')
	UKgrid <- "+init=epsg:27700"
	
	outdata <- spTransform(indata, CRS(UKgrid))
	return(outdata)
	}


#crd3 <- ConvertMatrixToSpatialPoints(c(-6.5,52.3))
#ConvertLonLatToUKNGR(crd3)


#crd3 <- ConvertMatrixToSpatialPoints(c(-6.5,52.3))
#ConvertLonLatToUKNGR(crd3)

