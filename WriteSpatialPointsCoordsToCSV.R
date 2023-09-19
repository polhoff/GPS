ExportShapefileToCSV <- function (indata, outfile = NA, outdir = dirdmp)
	{
    if (is.na(outfile)) {
        outfile = GetObjectSelfName(indata)
		}

	#if (!("name" %in% names(indata))) {indata$name <- NA}
	#if (!("id" %in% names(indata))) {indata$id <- NA}
	#if (!("id" %in% names(indata))) {indata$id <- NA}

	
	coords <- try(data.frame(coordinates(indata), indata$name))
	coords <- try(data.frame(coordinates(indata), indata$id))
	
	stopifnot(class(coords) != "try-error")
    try(names(coords) <- c("lon", "lat", "SiteID"))

    outfile <- paste(basename(outfile), ".csv", sep = "")

    write.csv(coords, file = paste(outdir, outfile, sep = ""), row.names = FALSE)

    return(coords)
	}

#ExportShapefileToCSV(HandDPSub_01.shp)
































WriteSpatialPointsCoordsToCSV <- function(indata, outfile = c_infile, outdir = dirdmp)
	{
	
	if(!('name' %in% names(indata)))
		{
		indata$name <- NA
		}

	coords <- try(data.frame(coordinates(indata), indata$name))

	stopifnot(class(coords) != "try-error")
	try(names(coords) <- c('lon','lat','SiteID'))
	
	#outfile <- GetObjectSelfName(indata)

	
	outfile <- paste(basename(outfile), '.csv', sep = '')
	
	write.csv(coords, file = paste(outdir, outfile, sep = ''), row.names = FALSE)
	#write.csv(coords, file = paste(outdir, outfile, sep = ''))
	
	return(coords)
	}




#df_test <- indataGPX['name']; df_test1 <- indataGPX['dgpsid']
#WriteSpatialPointsCoordsToCSV(indataGPX)
#WriteSpatialPointsCoordsToCSV(df_test)
#WriteSpatialPointsCoordsToCSV(df_test1)
#WriteSpatialPointsCoordsToCSV(df_test, outfile = 'a')
#Bp('GPS')



































WriteSpatialPointsCoordsToCSVArchive1 <- function(indata, outfile = c_infile, outdir = dirdmp)
	{
	
	if(!('name' %in% names(indata)))
		{
		indata$name <- NA
		}

	coords <- try(data.frame(coordinates(indata), indata$name))
	
	if(class(coords) == "try-error")
		{
		coords <- try(data.frame(coordinates(indata)))
		coords$name <- NA
		#try(names(coords) <- c('lon','lat'))
		}

	stopifnot(class(coords) != "try-error")
	try(names(coords) <- c('lon','lat','SiteID'))
	
	#outfile <- GetObjectSelfName(indata)

	
	outfile <- paste(basename(outfile), '.csv', sep = '')
	
	write.csv(coords, file = paste(outdir, outfile, sep = ''), row.names = FALSE)
	#write.csv(coords, file = paste(outdir, outfile, sep = ''))
	
	return(coords)
	}

