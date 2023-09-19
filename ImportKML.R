
ImportKML <- function(infile)
	{
	library(rgdal)
	x <- readOGR(infile)

	return(x)
	}

#ImportKML('FieldCentresSep24.kml')
