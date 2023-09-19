



ConcatenateMultipleDataframes <- function(c_names)
	{
	
	#library(plyr)
	library(data.table)
	
	#inherits = TRUE so that it can see objects in parent environment
	out <- mget(c_names, inherits = TRUE)
	
	
	#much slower  than C code below
	#rbind.fill(out)
	
	#rblind list written in C
	out <- rbindlist(out)
	
	return(out)
	}


#a11 <- GetProjection('eqc30')












































































GetProjectionTmap <- function(c_shortname, outtype = "character")
	{
		
	library(tmaptools)
	
	#"longlat"Not really a projection, but a plot of the longitude-latitude coordinates (WGS84 datum).

	#"wintri" Winkel Tripel (1921). Popular projection that is useful in world maps. It is the standard of world maps made by the National Geographic Society. Type: compromise

	#"robin" Robinson (1963). Another popular projection for world maps. Type: compromise

	#"eck4" Eckert IV (1906). Projection useful for world maps. Area sizes are preserved, which makes it particularly useful for truthful choropleths. Type: equal-area

	#"hd" Hobo-Dyer (2002). Another projection useful for world maps in which area sizes are preserved. Type: equal-area

	#"gall" Gall (Peters) (1855). Another projection useful for world maps in which area sizes are preserved. Type: equal-area

	#"merc" Web Mercator. Projection in which shapes are locally preserved, a variant of the original Mercator (1569), used by Google Maps, Bing Maps, and OpenStreetMap. Areas close to the poles are inflated. Type: conformal

	#"utmXX(s)" Universal Transverse Mercator. Set of 60 projections where each projection is a traverse mercator optimized for a 6 degree longitude range. These ranges are called UTM zones. Zone 01 covers -180 to -174 degrees (West) and zone 60 174 to 180 east. Replace XX in the character string with the zone number. For southern hemisphere, add "s". So, for instance, the Netherlands is "utm31" and New Zealand is "utm59s"

	#"mill" Miller (1942). Projetion based on Mercator, in which poles are displayed. Type: compromise

	#"eqc0" Equirectangular (120). Projection in which distances along meridians are conserved. The equator is the standard parallel. Also known as Plate Carr. Type: equidistant

	#"eqc30" Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 30 is the standard parallel. Type: equidistant

	#"eqc45" Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 45 is the standard parallel. Also known as Gall isographic. Type: equidistant

	#"laea_Eur" European Lambert Azimuthal Equal Area Projection. Similar to EPSG code 3035.

	#"laea_NA" North American Lambert Azimuthal Equal Area Projection. Known as SR-ORG:7314.

	#"rd" Rijksdriehoekstelsel. Triangulation coordinate system used in the Netherlands.

	#output the output format of the projection, one of "character", "crs","epsg", or "CRS"

	#See Also http://en.wikipedia.org/wiki/List_of_map_projections for a overview of projections. http://trac.osgeo.org/proj/ for the PROJ.4 project home page. An extensive list of PROJ.4 codes can be created with rgdal's make_EPSG.


	out <- tmaptools::get_proj4(c_shortname, output = outtype)
	return(out)
	}


#c_testProj <- GetProjection('robin')








































































































ListShapeFiles <- function(indir, l_Full = FALSE)
	{

	Shape_files.shp <- list.files(indir, pattern = "shp")
	Shape_files.xml <- list.files(indir, pattern = "xml")


	if(l_Full)
		{
		Shape_files.shp <- list.files(indir, pattern = "shp",  full.names = TRUE)
		Shape_files.xml <- list.files(indir, pattern = "xml",  full.names = TRUE)
		}


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


































ReadShapeFilesInDirectory <- function(indir)
	{
	
	filelist <- ListShapeFiles(indir)
	
	
	for(i in filelist)
		{
		#assign(i, readOGR(paste(indir , i, sep = '')), envir = .GlobalEnv)
		try(assign(i, readOGR(paste(indir , i, sep = '')), env = .GlobalEnv))
		}

	Shape_files <- ls(pattern = 'shp')	

	}


#dirt <- paste(dirtop, "Ballycanew\\ThomasNov2019\\Grassland_B_Run_One\\Run_One\\2009\\", sep = '')
#ReadShapeFilesInDirectory(dirt)


	









































SaveObjects <- function(c_string, outname, outdir = dirdmp)
	{
	
	stopifnot(class(c_string) == 'character')
	save(list = c_string, file = paste(outdir, outname, '.rda', sep = ''))

	}

#SaveObjects(c('catch', 'catch.shp'), 'ab')
