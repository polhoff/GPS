

CalculateAreaAsPercent <- function(CatchmentPolys, HandPolys, n_ctr, l_plot = TRUE, l_outPolygons = TRUE)
	{

	#HandPolys <- HandDrawnChull.NGR.shp
	#CatchmentPolys <- DrainageAreas.NGR.shp
	#n_ctr <- 10

	poly1 <- SelectPolygon(HandPolys, n_ctr)

	#pl2 <- SquareClip(CatchmentPolys, poly1)
	pl2 <- TotalClip(CatchmentPolys, poly1)


	#old - didn't work because only extracts first set of coordinates
	#coords.pl2 <- Coordinates.SpatialPolygon(pl2)
	coords.pl2 <- Geom(pl2)
	chull.pl2 <- Chull.SpatialPoints(coords.pl2)
	chull.poly <- ConvertSpatialPointsToSpatialPolygons(chull.pl2)


	if(l_plot)
		{
		plot(pl2, col = 'grey70', lty = 0)
		plot(coords.pl2, pch = 1, cex = 0.6, add=  TRUE)
		plot(chull.pl2, pch = 1, cex = 2, add=  TRUE)
		plot(chull.poly, lwd = 2, add = TRUE)
		}



	AreaPoly1 <- sum(area(pl2))
	AreaChull <- area(chull.poly)
	Ratio <- round(AreaPoly1/AreaChull,2)
	Npoints <- dim(coordinates(coords.pl2))[1]
	PointsPerUnitArea <- Npoints/AreaPoly1


	#out <- list(pl2, chull.poly,
	#out <- c(AreaPoly1, AreaChull, round(AreaPoly1/AreaChull,2), Npoints, Npoints/AreaPoly1)
	out <- c(AreaPoly1, AreaChull, Ratio, Npoints, PointsPerUnitArea, n_ctr)
	names(out) <- c('AreaPoly', 'AreaChull', 'Ratio', 'Npoints', 'PointsPerUnitArea', 'PolyID')


	#out <- c('AreaPoly'=AreaPoly1, 'AreaChull'=AreaChull, 'Ratio'=round(AreaPoly1/AreaChull,2), 'Npoints'=Npoints, 'PointsPerUnitArea'=Npoints/AreaPoly1)

	#out <- c("AreaPoly"=AreaPoly1, "AreaChull"=AreaChull, "Ratio"=round(AreaPoly1/AreaChull,2), "Npoints"=Npoints, "PointsPerUnitArea"=Npoints/AreaPoly1)


	#names(out) <- c("AreaPoly", "AreaChull", "Ratio", "Npoints", "PointsPerUnitArea")

	if(l_outPolygons)
		{
		out <- list(n_ctr, pl2, chull.poly, out)
		names(out) <- c("PolyID", "DrainagePolygon", "ConvexHull", "Stats")
		}


	return(out)
	}

#test1 <- CalculateAreaAsPercent(DrainageAreas.NGR.shp, HandDrawnChull.NGR.shp, 1)
#test1 <- CalculateAreaAsPercent(DrainageAreas.NGR.shp, HandDrawnChull.NGR.shp, 1, l_outPolygons = FALSE)
#CalculateAreaAsPercent



































































































MakeDir <- function(c_catch = 'Ballycanew', c_year = '2009', c_ver = "Nov2019", c_run = 'Run_One', c_ACPdes, l_Thomas = TRUE)
	{
	
	if(c_catch == 'Ballycanew')
		{
		c_ACPdes <- 'Grassland_B'
		}
	
	
	c_1 = ''


	
	if(l_Thomas) c_1 = 'Thomas'

	dirout <- paste(dirtop, c_catch, '/', c_1, c_ver, '/', c_ACPdes, '_', c_run, '/', c_run, '/', c_year, '/', sep = '')
	
	return(dirout)
	}


#MakeDir()


















MakeDirWk2 <- function(c_catch = 'Ballycanew', c_year = '2009')
	{
	
	if(c_catch == 'Ballycanew')
		{
		c_ACPdes <- 'GrasslandB'
		}
	
	
	dirout <- paste(dirtop, '/orig/Simon_Parker_Work_v2/', c_year, '_', c_ACPdes, '_final', '/', sep = '')
	
	return(dirout)
	}


#MakeDirWk2()













































MergeObjects <- function(SpObject1, SpObject2, byVar = HSA_ID)
	{

	p4_1 <- proj4string(SpPoly)
	#expecting single polygon or set of points
	chull.poly <- Chull.SpatialPolygon(SpPoly)
	coords.pl2 <- Geom(SpPoly)
	
	if(l_plot)
		{
		plot(SpPoly, col = 'grey70', lty = 0)
		#plot(coords.pl2, pch = 1, cex = 0.6, add=  TRUE)
		#plot(chull.pl2, pch = 1, cex = 2, add=  TRUE)
		plot(chull.poly, lwd = 2, add = TRUE)
		}



	AreaPoly1 <- sum(area(SpPoly))
	AreaChull <- area(chull.poly)
	Ratio <- round(AreaPoly1/AreaChull,2)
	Npoints <- dim(coordinates(coords.pl2))[1]
	PointsPerUnitArea <- Npoints/AreaPoly1


	#out <- list(pl2, chull.poly,
	#out <- c(AreaPoly1, AreaChull, round(AreaPoly1/AreaChull,2), Npoints, Npoints/AreaPoly1)
	#out <- c(AreaPoly1, AreaChull, Ratio, Npoints, PointsPerUnitArea, n_ctr)
	#names(out) <- c('AreaPoly', 'AreaChull', 'Ratio', 'Npoints', 'PointsPerUnitArea', 'PolyID')

	out <- c(AreaPoly1, AreaChull, Ratio, Npoints, PointsPerUnitArea)
	names(out) <- c('AreaPoly', 'AreaChull', 'Ratio', 'Npoints', 'PointsPerUnitArea')


	#out <- c('AreaPoly'=AreaPoly1, 'AreaChull'=AreaChull, 'Ratio'=round(AreaPoly1/AreaChull,2), 'Npoints'=Npoints, 'PointsPerUnitArea'=Npoints/AreaPoly1)

	#out <- c("AreaPoly"=AreaPoly1, "AreaChull"=AreaChull, "Ratio"=round(AreaPoly1/AreaChull,2), "Npoints"=Npoints, "PointsPerUnitArea"=Npoints/AreaPoly1)


	#names(out) <- c("AreaPoly", "AreaChull", "Ratio", "Npoints", "PointsPerUnitArea")

	if(l_outPolygons)
		{
		#out <- list(n_ctr, SpPoly, chull.poly, out)
		#names(out) <- c("PolyID", "DrainagePolygon", "ConvexHull", "Stats")
		out <- list(SpPoly, chull.poly, out)
		names(out) <- c("DrainagePolygon", "ConvexHull", "Stats")
		
		}


	return(out)
	}


#test1 <- CalculateAreaAsPercentFromPolygon(data_sub1)
















































PlotContributingArea <- function(CAreaShape, inDem, FieldPoly, n_cex = 2, fieldPolycolour = 'grey80', fieldPolywidth = 2, l_legend = FALSE)
	{
	
	par(mar=c(3,3,3,3))
	my.palette <- brewer.pal(n=9, name = 'Greys')
	#FieldPolyPlus <- InflatePolygon(FieldPoly, 5)
	
	BigField <- InflatePolygon(FieldPoly, 5)
	Box <- try(BoundingBox(FieldPoly))
	Box <- try(InflatePolygon(Box, 5))
	#plot(dem.Field, col.regions = my.palette, cuts = 8)
	#plot(Box, lty = 0)
	#plot(FieldPolyPlus, lty = 0)
	

	plot(BoundingBox(BigField), lty = 0)
	plot(carea, col = my.palette ,add=  TRUE, legend = l_legend)

	contour(inDem, labcex = n_cex, add = TRUE)
	#spplot(CAreaShape, col = my.palette, add = TRUE)
	#plot(carea, col = my.palette ,add=  TRUE)

	#plot(FieldPoly, add = TRUE)
	
	plot(FieldPoly, add = TRUE, lwd = fieldPolywidth, border = fieldPolycolour)
	}


#PlotContributingArea(carea, dem.Field, field.Selected.p)




























































PlotContributingArea.Arc <- function(CAreaShape, inDem, FieldPoly, n_cex = 1.2, fieldPolycolour = 'grey80', fieldPolywidth = 2)
	{
	
	par(mar=c(10,10,10,10))
	my.palette <- brewer.pal(n=9, name = 'Greys')
	#FieldPolyPlus <- InflatePolygon(FieldPoly, 5)
	
	#BigField <- InflatePolygon(FieldPoly, 5)
	Box <- try(BoundingBox(FieldPoly))
	Box <- try(InflatePolygon(Box, 5))
	#plot(dem.Field, col.regions = my.palette, cuts = 8)
	#plot(Box, lty = 0)
	#plot(FieldPolyPlus, lty = 0)
	
	#plot(CAreaShape, col = my.palette, add = TRUE)
	plot(CAreaShape, col = my.palette)
	
	contour(inDem, labcex = n_cex, add = TRUE)
	spplot(CAreaShape, col = my.palette, add = TRUE)
	#plot(FieldPoly, add = TRUE)
	
	plot(FieldPoly, add = TRUE, lwd = fieldPolywidth, border = fieldPolycolour)
	}

#PlotContributingArea(carea.shp, dem.Field, field.MM)
#PlotContributingArea(carea.shp, dem.Field, field.MM.singleEntity)
#PlotContributingArea(carea.shp, dem.Field, field.Selected.p)





































PlotFieldContour <- function(inDem, FieldPoly, n_cex = 1.4)
	{
	
	my.palette <- brewer.pal(n=9, name = 'Greys')

	BigField <- InflatePolygon(FieldPoly, 5)
	#plot(dem.Field, col.regions = my.palette, cuts = 8)
	plot(inDem, col = my.palette)
	plot(FieldPoly, add = TRUE)
	contour(inDem, labcex = n_cex, add = TRUE)
	plot(FieldPoly, add = TRUE, lwd = 3, border = 'grey60')
	}

#PlotFieldContour(dem.Field, field.MM)





























PlotFieldPerspective <- function(inDem, l_flip = FALSE)
	{
	
	n_phi = 60
	n_theta = 0
	
	if(l_flip) {n_phi = 30; n_theta = 180}
	
	#my.palette <- brewer.pal(n=9, name = 'Greys')
	
	persp(inDem, phi = n_phi, expand = 0.5 ,shade = 0.75, theta = n_theta, ltheta = 90)
	}

#PlotFieldPerspective(dem.Field)
#PlotFieldPerspective(dem.Field, l_flip = TRUE)


