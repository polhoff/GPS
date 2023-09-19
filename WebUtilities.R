



ReadGeoJsonWeb <- function(c_http, c_what = 'sp', l_plot = FALSE, l_Example = FALSE)
	{
	library(geojsonio)


	if(l_Example)
		{
		c_http <- 'http://data-osi.opendata.arcgis.com/datasets/25d1b5cd3fbe49ccba0aa35660852949_7.geojson?outSR={%22latestWkid%22:4326,%22wkid%22:4326}'
		}	

	out <- geojson_read(c_http, what = c_what)



	if(l_plot)
		{
		library(sp)
		plot(out)
		}
		
	return(out)
	}



#x1 <- ReadGeoJsonWeb(l_Example = TRUE)
#x1 <- ReadGeoJsonWeb(l_Example = TRUE, l_plot=TRUE)


#x1 <- ReadGeoJsonWeb(c_http = 'http://data-osi.opendata.arcgis.com/datasets/0682a204da954275bd02df5d11d523b7_6.geojson?outSR={%22latestWkid%22:4326,%22wkid%22:4326}')
#plot(x1)



#dirDownloads <- paste(dirtop, 'Downloads/', sep = '')



#x1 <- url('https://data.gov.ie/dataset/hydro-nodes-osi-national-250k-map-of-ireland')
#x2 <- wget(x1)


