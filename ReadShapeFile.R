



ListShapeFiles.Arc <- function(indir)
	{

	Shape_files.shp <- list.files(indir, pattern = "shp")
	Shape_files.xml <- list.files(indir, pattern = "xml")


	#Shape_files_ndx <- Shape_files %in% Shape_files.shp
	Shape_files.xml_ndx <- Shape_files.shp %in% Shape_files.xml
	Shape_files.shp <- Shape_files.shp[!Shape_files.xml_ndx]

	#x_ndx <- Shape_files_ndx & !Shape_files.xml_ndx


	#Shape_files <- Shape_files.shp[x_ndx]
	#Shape_files


	#rm(Shape_files.shp)
	return(Shape_files.shp)
	}



#dirt <- paste(dirtop, "Ballycanew\\ThomasNov2019\\Grassland_B_Run_One\\Run_One\\2009\\", sep = '')
#ListShapeFiles(dirt)
































#https://stackoverflow.com/questions/55910398/how-do-i-get-rgdal-to-open-a-geodatabase-gdb-file

#ReadGeodatabase

#ogrListLayers("/data/gdb/RI_geodatabase_wetlands.gdb")



#dirGDB <- paste(dirtop, '/orig/LiDAR/20191126_SParker_LiDAR/', sep = '')
#dirGDB <- paste(dirtop, '/orig/LiDAR/20191126_SParker_LiDAR/Parker_LiDAR.gdb/', sep = '')

#dirdmp1 <- paste(dirtop, '/dump1/', sep = '')

#gdb <- path.expand(dirGDB)
#ogrListLayers(gdb)


#ogr2ogr(dirGDB, dirdmp1)













































ReadShapeFile <- function(c_infile)
	{
	indata <- readOGR(c_infile)
	return(indata)
	}

#infile <- paste(dirtop, '/Ballycanew/Thomas/', 'hsa_delivery_points.shp', sep = '')
#x <- ReadShapeFile(infile)




























SaveShapefile <- function(indata, outfile = NA, outdir = dirdmp)
	{
	if(is.na(outfile))
		{
		outfile = GetObjectSelfName(indata)
		}

	dircur <- getwd()
	
	setwd(outdir)
	writeOGR(indata, ".", outfile, driver="ESRI Shapefile")
	
	setwd(dircur)
	}

#SaveShapefile01(HandDPSub_01.shp)
#SaveShapefile01(HandDPSub_01.shp, outfile = 'a')
#do.call(SaveShapefile, list(get(filename), filename, dirdmp))






























#only transforms one point
#coords <- coordinates(Bally_shp_poly)
#coords <- SpatialPoints(coords, proj4string = CRS(EIREgrid))
#coords_latlon.Bally <- spTransform(coords, CRS(latlong))


SaveShapefile.Dud <- function(indata, outfile = NA, outdir = dirdmp)
	{
	if(is.na(outfile))
		{
		outfile = GetObjectSelfName(indata)
		}

	outfile <- paste(outdir, outfile, sep = '')
	writeOGR(indata, ".", outfile, driver="ESRI Shapefile")
	}


#do.call(SaveShapefile, list(get(filename), filename, dirdmp))









SaveShapefile.Dud <- function(indata, outfile, outdir = dirdmp)
	{
	#if(is.na(outfile))
	l_test <- exists('outfile')
	print(l_test)
	
	#if(!exists('outfile'))
	if(!l_test)
		{
		outfile = GetObjectSelfName(indata)
		}
	
	writeOGR(indata, ".", outfile, driver="ESRI Shapefile")
	}

#SaveShapefile(CSA_shp.latlon)












































WriteShapeFileCoordinatesToCSV <- function(c_infile, outdir = dirdmp)
	{
	indata <- ReadShapeFile (c_infile)
	coords <- data.frame(coordinates(indata))
	
	names(coords) <- c('lon','lat')
	outfile <- basename(c_infile)
	outfile <- gsub('shp','csv', outfile)
	#write.csv(coords, file = paste(outdir, outfile, sep = ''), row.names = FALSE)
	write.csv(coords, file = paste(outdir, outfile, sep = ''))
	
	return(coords)
	}

#infile <- paste(dirtop, '/Ballycanew/Thomas/', 'hsa_delivery_points.shp', sep = ''); x <- WriteShapeFileCoordinatesToCSV(infile)

#infile <- paste(dirtop, '/Ballycanew/Shapefiles.LatLon/', 'HSA_shp.latlon.shp', sep = '')
#x <- WriteShapeFileCoordinatesToCSV(infile)
