
ReadGPX <- function(c_infile, l_assignGlobal = TRUE)
	{
	
	filename <- basename(c_infile)
	indata <- readOGR(c_infile)
	
	if(l_assignGlobal)
		{
		assign(filename, indata, envir = .GlobalEnv)
		}
	
	return(indata)
	}


#setwd("D:\\sp\\Ballycanew\\field\\gpx\\")
#cc1 <- "D:\\sp\\Ballycanew\\field\\gpx\\Waypoints_10-SEP-19.gpx"
#cc1 <- "Waypoints_20-SEP-19.gpx"
#ReadGPX(cc1)



















ReadWriteGPX <- function(infile, outfile = NA, c_catch = "Ballycanew", l_output = TRUE)
	{

	if(is.na(outfile)) outfile <- infile

	dirgpx <- paste(dirtop, c_catch, '/field/gpx/', sep = '')
	diroutput <- paste(dirtop, c_catch, '/field/processed/', sep = '')
	
	indata <- readOGR(paste(dirgpx, infile, '.gpx', sep = ''))
	
	olddir <- getwd()
	
	
	setwd(diroutput)

	if(l_output)
		{
		try(writeOGR(indata, dsn=getwd(), layer = outfile, driver = "ESRI Shapefile", overwrite_layer = FALSE))
	}	
	
	
	assign("indataGPX", indata, envir = .GlobalEnv)
	
	setwd(olddir)
	
	#return(indata)
	}

#ReadGPX("Route_2019-06-27_093033")
#ReadGPX("Waypoints_27-JUN-19")




















































WriteGPX <- function(indata = indataGPX, outfile, c_catch = "Ballycanew")
	{

	olddir <- getwd()
	
	diroutput <- paste(dirtop, c_catch, '/field/processed/', sep = '')
	setwd(diroutput)

	try(writeOGR(indata, dsn=getwd(), layer = outfile, driver = "ESRI Shapefile", overwrite_layer = FALSE))
	
	setwd(olddir)
	
	}

#ReadGPX("Route_2019-06-27_093033")
#ReadGPX("Waypoints_27-JUN-19")

#WriteGPX(outfile = "Field2276")


















































































#x <- c(-85.57768, -85.53748, -85.56880, -85.59405, -85.57524, -85.56148, -85.59133, -85.58460, -85.55561, -85.53497)

#y <- c(30.30360, 30.32251, 30.28610, 30.31114, 30.32091, 30.34385, 30.26825, 30.31113, 30.35082, 30.32276)

#a <- c(1:10)

#xy <- data.frame(x,y,a)

#latslongs <- SpatialPointsDataFrame(coords=xy[,c(1,2)],data=xy,proj4string =CRS("+proj=longlat + ellps=WGS84"))


#writeOGR(latslongs, dsn="D:/sp/dump/gpxTEST.gpx",     dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = TRUE)




WriteShpToGPX <- function(indata, outname = deparse(substitute(indata)), diroutput = dirdmp)
	{
	
	outfile <- paste(diroutput, outname, '.gpx', sep = '')
	
	olddir <- getwd()
	
	setwd(diroutput)


	try(writeOGR(indata, dsn=outfile, dataset_options="GPX_USE_EXTENSIONS=yes",layer = 'HSA_subset', driver = "GPX", overwrite_layer = TRUE))
	
	setwd(olddir)
	}


#WriteShpToGPX(HSA_shp)



