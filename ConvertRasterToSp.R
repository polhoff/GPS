
ConvertRasterToSpatialGrid <- function(inraster)
	{
	out <- as(inraster, 'SpatialGridDataFrame')
	return(out)
	}


#out <- ConvertRasterToSpatialGrid(carea.2m)
#str(out)


ConvertRasterToSpatialPixels <- function(inraster)
	{
	out <- as(inraster, 'SpatialPixels')
	return(out)
	}


#out <- ConvertRasterToSpatialPixels(carea.2m)
#str(out)

