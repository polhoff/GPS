
FindNearestPoint <- function(SingleTestPoint, TestPoints, n_StartRadius = 30000, n_exponent = 1.05, 	n_OutElements = 5, p4 = Mercator, output = c('OutPoints', 'OutIndices','OutDistance'))
	{


	n_rad <- n_StartRadius

	AllIndices <- 1:length(TestPoints)

	p4_TestPoint <- proj4string(SingleTestPoint)
	
	l_test = FALSE
	while(!l_test)
		{
		Test.Circle <- MakeCircleSpatialPoints(SingleTestPoint, n_radius = n_rad, c_proj4 = p4, l_lonlat = TRUE, l_ConvertToPoly = TRUE, l_PrintClass = FALSE)
		#CalcArea(out22)/1000000


		Test.Circle <- spTransform(Test.Circle, p4_TestPoint)


		#in_test <- gIntersection(out22, TestPoints)
		OutPoints <- PointInPoly(TestPoints, Test.Circle)
		#in_test

		
		l_test <- length(OutPoints) > n_OutElements
		
		n_rad <- n_rad * n_exponent
		l_test

		}


	in_ndx <- over(TestPoints, Test.Circle)
	in_ndx <- is.element(in_ndx, 1)
	
	
	OutIndices <- AllIndices[in_ndx]
	table(is.element(in_ndx, 1))


	OutDistance <- sp::spDists(SingleTestPoint, OutPoints)
	#gDistance(SingleTestPoint, OutPoints)

	RankFromSmallToLargeDistance <- order(OutDistance)

	OutIndices <- OutIndices[RankFromSmallToLargeDistance]
	OutPoints <- OutPoints[RankFromSmallToLargeDistance]
	OutDistance <- OutDistance[RankFromSmallToLargeDistance]

	PointsInCircle <- OutPoints
	
	OutIndices <- OutIndices[1:n_OutElements]
	OutPoints <- OutPoints[1:n_OutElements]
	OutDistance <- OutDistance[1:n_OutElements]
	

	#check 
	#TestPoints[OutIndices] == OutPoints


	all <- list(OutPoints, OutIndices, OutDistance, SingleTestPoint, PointsInCircle, Test.Circle)
	names(all) <- c('OutPoints', 'OutIndices', 'OutDistance', 'TestPoint', 'PointsInCircle', 'Circle')
	
	
	out_ndx <- match(output, c('OutPoints', 'OutIndices', 'OutDistance', 'all'))
	
	out <- switch(out_ndx, 'OutPoints', 'OutIndices', 'OutDistance', 'all')
	#out <- switch(output, OutPoints, OutIndices, OutDistance)
	out <- get(out)
	

	return(out)

	}



#x22 <- FindNearestPoint(test_x1, y1, output = 'OutPoints')
#x23 <- FindNearestPoint(test_x1, y1, output = 'OutIndices')
#x24 <- FindNearestPoint(test_x1, y1, output = 'OutDistance')
#x25 <- FindNearestPoint(test_x1, y1, output = 'all')
#x26 <- FindNearestPoint(test_x1, y1, output = 'all', n_StartRadius = 60000)



