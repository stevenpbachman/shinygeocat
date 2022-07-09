#Contains the functions for analysis
#mostly ripped from rCAT2




###########################################################
#calculates the EOO area
###########################################################
#' @title Extent of Occurrence (EOO) Area
#' @description 
#' Calculates the Extent of Occurrence in km2 or returns a simple feature polygon from a set of points (x,y)
#' @author Justin Moat. J.Moat@kew.org
#' @param thepoints dataframe of points in metres i.e. c(x,y)
#' 
#' @return float_value area of EOO polygon and sf polygon in geographic project
#' @note area returned is in x,y units, but negative as polygon is constructed anticlockwise
#' @examples
#' x <- runif (20,0,10)
#' y <- runif (20,0,10)
#' df <- data.frame(x,y) 
#' eoo (df)
#' #######
#' spoly <- eoo (df,TRUE)
#' plot(spoly)
#' @seealso \code{\link{ratingEoo}} for EOO Ratings
#' @export
#' @importFrom grDevices chull
#' @importFrom pracma polyarea
#' @import sf
#' @references
#' Bachman, S., Moat, J., Hill, A.W., de Torre, J., Scott, B., 2011. Supporting Red List threat assessments with GeoCAT: geospatial conservation assessment tool. Zookeys 126, 117–26. doi:10.3897/zookeys.150.2109 
#' 
#' Joppa, L.N., Butchart, S.H.M., Hoffmann, M., Bachman, S.P., Akçakaya, H.R., Moat, J.F., Böhm, M., Holland, R.A., Newton, A., Polidoro, B., Hughes, A., 2016. Impact of alternative metrics on estimates of extent of occurrence for extinction risk assessment. Conserv. Biol. 30, 362–370. doi:10.1111/cobi.12591
library(pracma)
library (sf)

eoosh <- function(points) {
  if (! "X" %in% colnames(points) | ! "Y" %in% colnames(points)) {
    stop("Point coordinates must be supplied in columns named 'X' and 'Y'.")
  }
  
  hull_idx <- chull(points)
  hull <- points[hull_idx,]
  
  area <- polyarea(x=hull$X, y=hull$Y)
  # hull is constructed backwards, so area is negative and in m^2
  area <- -1 * area / 1e6
  
  #JMJMJM WORKING
  p1 <- matrix(c(hull$X,hull$Y),ncol=2)
  #close it 
  p1 <- rbind(p1,c(hull[1,]$X,hull[1,]$Y))
  p1 <- st_polygon(list(p1))
  #set projection
  p1 <- st_sfc(p1, crs = attr(points,'crs'))
  #to geographic
  p1 <- st_transform(p1,4326)
  
 list(area = area,polysf = p1)
  
  #JMJMJM
} 


###########################################################
#calculates the initial AOO, with simple grid 0,0         #
###########################################################
#' calculates a very simple AOO area from a set of points
#' @title Area of Occupancy (AOO), grid orgin 0,0
#' @description 
#' Calculates the number area the of occupied cells for (Area of Occupancy AOO) from a set of points (x,y), projected into metres, with origin 0,0. 
#' Please cite: Moat, J., Bachman, S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to address the modifiable areal unit problem in ecology and conservation. Conservation biology, 32(6), 1278-1289. if using this algorithm:
#' @author Justin Moat. J.Moat@kew.org
#' @param thepoints set of points in metres i.e. c(x,y)
#' @param cellsize size of cell (length) in metres
#' @param returnV, switches to return different sets of results: \cr
#' S = Simple, returns just the AOO area in km2, (DEFAULT) \cr
#' E = Expended simple, returns the solution for the AOO as list of (area,number of cells, rotation (0 degrees), shift in x direction(0), shift in y direction(0)). This is used so as be compatiable with other AOO calculations. \cr
#' SF = returns a polygon simple feature for mapping and plotting in ggplot/plot or export to GIS format.
#' @return as returnV, default is area in km2
#' @examples
#'library(ggplot2)
#'#Build and project some points
#'thepoints <- ptsSquare(19,0.1)
#'names(thepoints) <- c("lat","long")
#'thepoints <- simProjWiz(thepoints)
#'attr(thepoints,'crs')
#'cellsize = 2000
#'
#'#return area in km2
#'aoo (thepoints,cellsize)
#'#return list of parameters
#'aoo (thepoints,cellsize,returnV="E")
#'#return polygon for plotting
#'gridpoly <- aoo(thepoints,cellsize,returnV="SF")
#'ggplot(data=gridpoly) 
#'   + geom_sf(color="black",fill="red") 
#'   + geom_point(data=thepoints,aes(X,Y))
#' @seealso \code{\link{ratingAoo}} for AOO Ratings from IUCN categories
#' @export
#' @references
#' Moat, J., Bachman, S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to address the modifiable areal unit problem in ecology and conservation. Conservation biology, 32(6), 1278-1289.
#' 
#' Bachman, S., Moat, J., Hill, A.W., de Torre, J., Scott, B., (2011). Supporting Red List threat assessments with GeoCAT: geospatial conservation assessment tool. Zookeys 126, 117–26. doi:10.3897/zookeys.150.2109 

aoosh <- function(thepoints, cellsize=2000, returnV="S"){
  bottomleftpoints <- unique(floor(thepoints/cellsize))
  
  cellp <- data.frame(
    x=(bottomleftpoints$X * cellsize), 
    y=(bottomleftpoints$Y * cellsize)
  )
  
  # if (returnV == "E") {
  #   return(list(
  #     area=nrow(cellp) * (cellsize^2)/1000000,
  #     nocells=nrow(cellp), 
  #     rotation=0,
  #     xshift=0, 
  #     yshift=0
  #   ))
  # }
  
  area <- (nrow(cellp) * (cellsize^2)/1000000)
  p1 <- buildCells(cellp, cellsize, 0, 0, 0, attr(thepoints,'crs'))
  p1 <- st_transform(p1,4326)
  #print("AOO")
  return (list(area = area, polysf=p1))
  # if (returnV == "SF") {
  #   buildCells(cellp, cellsize, 0, 0, 0, attr(thepoints,'crs'))
  # } else {
  #   return(nrow(cellp) * (cellsize^2)/1000000)
  # }
}


###############building blocks for polygon production######
###########################################################
#builds polygons from points and rotation, shift in X and y
#returns polygons for ggplot2 and mapping
###########################################################
#' @title Build simple feature polygons from point data, rotation and shift in x and y direction
#' @description 
#' Builds cell polygons (as simple features) from points and rotation, shift in X and y returns polygons for ggplot2 and mapping.
#' Generally used to plot data from AOO calculations.
#' @author Justin Moat. J.Moat@kew.org
#' @param thepoints set of points in metres i.e. c(x,y)
#' @param cellsize size of cell (length) in metres
#' @param rot rotation of the grid in radian
#' @param shiftx shift in the x direction in metres
#' @param shifty shift in the y direction in metres
#' @return Simple Feature of polygons
#' @examples
#'#Build and project some points
#'thepoints <- ptsSquare(19,0.1)
#'names(thepoints) <- c("lat","long")
#'thepoints <- simProjWiz(thepoints)
#'#Check projection information is attributed
#'attr(thepoints,'crs')
#'cellsize = 2000
#'all <- aooFixedRotation(thepoints,cellsize,1296,"ALL")
#'worstgrid <- all[which.max(all$nofcells),]
#'worstpoly <- buildCellPolys_rxy(thepoints,cellsize,worstgrid$rotation,worstgrid$xshift,worstgrid$yshift)
#'ggplot(data=worstpoly) 
#'   + geom_sf(color="black",fill="red") 
#'   + geom_point(data=thepoints,aes(X,Y))
#'
#' @export
#' @references
#' Moat, J., Bachman, S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to address the modifiable areal unit problem in ecology and conservation. Conservation biology, 32(6), 1278-1289.

buildCellPolys_rxy<- function(thepoints,cellsize,rot,shiftx,shifty){
  #shift first
  testps <- cbind(thepoints$X - shiftx,thepoints$Y - shifty)
  #then rotate
  rpoints <- rotateP(testps,rot)
  testcps <- unique(floor(rpoints/cellsize))*cellsize
  colnames(testcps)<-c("x","y")
  buildCells(testcps,cellsize,-rot,shiftx,shifty,attr(thepoints,'crs'))
  
}





###########################################################
#Rotates a set of points                                  #
###########################################################
#Note angle in radians and only needed between 0 and 2pi for 360's
#but if using with shift you really only need 0 and pi/2
#' @title Rotates a set of points
#' @description 
#' Rotates a set of point by an angle in radians. Used as part of the AOO rotation calculations.
#' 
#' @author Justin Moat. J.Moat@kew.org
#' @param thepoints set of points in metres i.e. c(x,y)
#' @param angle in radians
#' @return dataframe of points
#' @examples
#'#Build and project some points
#'thepoints <- ptsSquare(200,0.1)
#'#rotate by 45 degrees
#'rpoints <- rotateP(thepoints,deg2rad(45))
#'plot(rpoints,asp=1)
#' @export
#' @references
#' Moat, J., Bachman, S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to address the modifiable areal unit problem in ecology and conservation. Conservation biology, 32(6), 1278-1289.

rotateP <- function(thepoints, angle){
  pointmatrix <- as.matrix(thepoints)
  rotationmatrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)),byrow = TRUE, 2, 2)
  pr <- pointmatrix %*% rotationmatrix
  pdf <- as.data.frame(pr)
  colnames(pdf) <- c("x","y")
  return(pdf)
}

###########################################################
#builds all corners for a square from the lower left corner, returns df id,x,y for use in ggplot
###########################################################
buildCellPolys <- function (llcorners,cellsize){
  mydf <- data.frame(id=integer(),x=double(),y=double())
  for (i in 1:nrow(llcorners)){
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x,llcorners[i,]$y)
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x+cellsize,llcorners[i,]$y)
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x+cellsize,llcorners[i,]$y+cellsize)
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x,llcorners[i,]$y+cellsize)
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x,llcorners[i,]$y)
  }
  return(mydf)
}

###########################################################
#builds all corners for a square from the lower left corner,
#rotation, shift in X and y
#returns polygons
###########################################################
#internal called from main scripts

buildCells <- function (llcorners, cellsize, rot=0, shiftx=0, shifty=0, crs=""){
  #build cells 
  mincells <- buildCellPolys(as.data.frame(llcorners),cellsize)
  #rotate these back to original point orientation
  cells <- rotateP(mincells[, 2:3], rot)
  #shift
  cells$x <- cells$x + shiftx
  cells$y <- cells$y + shifty
  
  cell_list <- split(cells, f=mincells$id)
  poly_list <- lapply(cell_list, function(x) constructPolygon(x$x, x$y, crs))
  
  do.call(c, poly_list)
}


#' Construct a polygon from vertices.
#' 
#' Accepts an x and a y vector to define the vertices of
#' the polygon, to make it easier 
constructPolygon <- function(x, y, crs){
  points <- cbind(x, y)
  
  is_closed <- all(points[1,] == points[nrow(points),])
  
  if (! is_closed) {
    points <- rbind(points, points[1,])  
  }
  
  geom <- st_polygon(list(points))
  
  # put geometry into an sfc so we can attach a crs
  if (is.null(crs)) {
    crs <- ""
  }
  
  polygon <- st_sfc(geom, crs=crs)
  
  if (is.na(st_crs(polygon))) {
    warning("No valid CRS provided so setting it to `NA`")
  }
  
  polygon
}
