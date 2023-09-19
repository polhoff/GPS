

#Calculating TWI in R?
#https://stackoverflow.com/questions/58553966/calculating-twi-in-r

CreateLayers <- function (dem, fill.sinks = TRUE, deg = 0.1) 
{
  layers <- stack(dem)
  message("Building Upslope areas...")
  a.atb <- Upslope(dem, atb = TRUE, fill.sinks = fill.sinks, deg = deg)
  layers <- addLayer(layers, a.atb)
  names(layers) <- c("filled.elevations", "Upslope.area", "twi")
  return(layers)
}



Upslope <- function (dem, log = TRUE, atb = FALSE, deg = 0.12, fill.sinks = TRUE) 
{
  if (!all.equal(xres(dem), yres(dem))) {
    stop("Raster has differing x and y cell resolutions. Check that it is in a projected coordinate system (e.g. UTM) and use raster::projectRaster to reproject to one if not. Otherwise consider using raster::resample")
  }
  if (fill.sinks) {
    capture.output(dem <- invisible(raster::setValues(dem, topmodel::sinkfill(raster::as.matrix(dem), res = xres(dem), degree = deg))))
  }
  topidx <- topmodel::topidx(raster::as.matrix(dem), res = xres(dem))
  a <- raster::setValues(dem, topidx$area)
  if (log) {
    a <- log(a)
  }
  if (atb) {
    atb <- raster::setValues(dem, topidx$atb)
    a <- addLayer(a, atb)
    names(a) <- c("a", "atb")
  }
  return(a)
}


#dirdata <- paste(dirtop,'orig/LiDAR/Ballycanew/5metre/', sep = '')
#infile <- paste(dirdata, 'Ballycanew_SAGA.tif', sep = '')
#dem.5m <- ReadGeoTiff(infile, l_assignGlobal = FALSE)
#UpslopeArea <- Upslope(dem.5m, atb = TRUE, deg = 0.1)
#plot(UpslopeArea)
#output <- CreateLayers(dem.5m)

#X11(); plot(output$twi)
#X11(); plot(output$filled.elevations)


#topidx <- topmodel::topidx(raster::as.matrix(dem.5m), res = xres(dem.5m))
