
ReadGeoTiff <- function(c_infile, l_assignGlobal = TRUE)
	{
	
	#raster() from package raster
	filename <- basename(c_infile)
	indata <- raster::raster(c_infile)
	
	if(l_assignGlobal)
		{
		assign(filename, indata, envir = .GlobalEnv)
		}
	
	return(indata)
	}



#dirdata <- paste(dirtop,'orig/LiDAR/SRTM/Ballycanew/', sep = '')
#infile <- paste(dirdata, 'n52_w007_3arc_v2.tif', sep = '')
#ReadGeoTiff(infile)
#dem1 <- ReadGeoTiff(infile, l_assignGlobal = FALSE)
