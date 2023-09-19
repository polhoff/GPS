
CalcMinMaxCoords <- function(indata)
	{
	
	coords <- data.frame(coordinates(indata))
	
	names(coords) <- c('lon','lat')
	
	x_min <- min(coords$lon)
	x_max <- max(coords$lon)
	
	
	y_min <- min(coords$lat)
	y_max <- max(coords$lat)
	
	#output <- c('x_min','x_max','y_min','y_max')
	output <- c(x_min,x_max,y_min,y_max)
	output <- list(x_min,x_max,y_min,y_max)
	
	output <- c('x_min' = x_min, 'x_max' = x_max, 'y_min' = y_min, 'y_max' = y_max)
	return(output)
	}


#CalcMinMaxCoords(indataGPX)


























#see also spDistsN1
CalcDistance <- function(spManypoints, spSinglePoint)
	{
	out <- spDists(spManypoints, spSinglePoint)
	return(out)
	}


#b1 <- CalcDistance(a1, contPtsStart)
