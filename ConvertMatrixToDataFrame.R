
ConvertMatrixToDataFrame <- function(indata, l_spatialnames = TRUE)
	{

	indata <- matrix(indata, ncol = 2)
	indata <- as.data.frame(indata)
	
	if(l_spatialnames){	names(indata)[1:2] <- c('lon', 'lat') }

	print(class(indata))
	return(indata)
	}

#x <- c( -4, 52)
#ConvertMatrixToDataFrame(x)

#x <- matrix(c( -4, 52, -5,53), ncol = 2, byrow = TRUE)
#ConvertMatrixToDataFrame(x)
#ConvertMatrixToDataFrame(x, l_spatialnames = FALSE)


