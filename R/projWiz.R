###########################################################
#Simple area Projection Wizard
###########################################################
#' @title Simple Projection Wizard
#' @description 
#' Projects any set of latitude and longitude points to a "suitable" area projection, based on thieir "true centre of gravity".
#' Data is expected as lat long in decimal degrees and returned in metres.
#' Input data is checked to make sure it’s sensible before projection (i.e. lat and longs on the earth no null or NA values)
#' @author Justin Moat. J.Moat@kew.org
#' @note
#' Based around a simple continental projection, using two sets of projections \cr
#' equal area cylindrical = Cylindrical equal-area \cr
#' equal area azimuthal for polar (above 70) = Lambert azimuthal equal-area \cr
#'  
#' NB these are not cartographically pleasing projections, they are just so we can get the data into something simple for areal and distance analysis.
#' See Šavric et al for a more cartographically pleasing projection engine \cr
#' Šavric, B., Jenny, B., Jenny, H., 2016. Projection Wizard – An Online Map Projection Selection Tool. Cartogr. J. 53, 1–9. doi:10.1080/00087041.2015.1131938 and https://projectionwizard.org/
#' @param thepoints set of points as a dataframe with latitude and longitude 
#' @param thecentre one point i.e. c(lat,long), if not specified this will be calculated from the center of gravity of all points
#' @param returnV switches to return either  dataframe (x,y) or simple feature of points  \cr
#' S = simple, returns as dataframe of x,y \cr
#' SF = simple feature of points
#' @return Defaults is a set of points in meters as a dataframe with projection details attributed (stored as crs to retrieve attr(myprojectedpoints,'crs'))
#' @examples 
#'lat <- runif (200,-24,-12)
#'long <- runif (200,43,51)
#'ll <- data.frame(lat,long)
#'#let it choose the centre point from your set of points
#'pointsprojected <- simProjWiz(ll)
#'#using your own set of points
#'pointsprojected <- simProjWiz(ll,c(-18,47))
#'#check projection returned
#'attr(pointsprojected,'crs')
#'#return a simple features
#'sf_points <- simProjWiz(ll,,"SF")
#'ppoints <- simProjWiz(ll,,"SF")
#'
#' @references 
#' Šavric, B., Jenny, B., Jenny, H., 2016. Projection Wizard – An Online Map Projection Selection Tool. Cartogr. J. 53, 1–9. doi:10.1080/00087041.2015.1131938
#' https://projectionwizard.org
#' 
#' Snyder, J.P., 1987. Map projections: A working manual, Professional Paper. Washington, D.C.
#' @export
#' @import sf
#' @import rgdal
#' 
#' 

library (sf)

simProjWiz <- function(thepoints,thecentre,returnV="S"){
  #check dataframe is sensible
  #llCheck(thepoints)
  names(thepoints) <- c("long", "lat")
  #
  if (missing(thecentre)){
    thecentre <- trueCOGll(thepoints)
  }
  #setup and set projection to WGS84
  thepoints <- st_as_sf(thepoints, coords = c("long", "lat"), crs = 4326)
  #depending on centre point, choose projection
  if((thecentre[1] < 70) & (thecentre[1] > -70)){
    CRSstring <- paste("+proj=cea +lon_0=", thecentre[2],   " +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep = "")
  } else {
    CRSstring <- paste("+proj=laea +lat_0=", thecentre[1]," +lon_0=", thecentre[2], " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep = "")
  }

  #reproject
  xysp <- st_transform(thepoints, CRSstring)
  
  if(returnV=="SF"){return (xysp)}
  else {
    xy <- as.data.frame(st_coordinates(xysp))
    attr(xy,'crs') <- CRSstring
    return(list(p=xy,crs=CRSstring))
  }
}

######################################################################
#calculates 'true' centre of gravity from a set of lat long points in decimal degrees   #
#note z from mean of cartesian give some idea of weighted spread on the globe#
######################################################################
#' @title True centre of gravity from a set of Lat longs
#' @description 
#' Calculates the "true" centre of gravity (weighted) from a set of lat longs, using cartesian geometry. Used as part of the projection wizard.
#' @author Justin Moat. J.Moat@kew.org
#' @param thepoints set of points c(lat,long)
#' @return a point (lat,long) from centre
#' @examples 
#'lat <- runif (200,-24,-12)
#'long <- runif (200,43,51)
#'ll <- data.frame(lat,long)
#'cp <- trueCOGll(ll)
#' @references Descartes, R., 1637. Discours de la methode. A Leyde, De l’imprimerie de I. Maire, Paris.
#' @export


trueCOGll <-function(thepoints){
  
  llrad <- deg2rad(thepoints) #to radians
  cartp <- ll2cart(llrad$lat,llrad$long) #to cartesian
  mp <- data.frame(x=mean(cartp$x),y=mean(cartp$y),z=mean(cartp$z)) #central point
  pmp <- pro2sph(mp$x,mp$y,mp$z) #projection to surface
  pmprll <- cart2ll(pmp$x,pmp$y,pmp$z) #to ll in radians
  pmpll <- rad2deg(pmprll) #to degrees
  return(data.frame(lat=pmpll$latr,long=pmpll$longr))
}

######################################################################
#calculates the Cartesian cordinates (x,y,z) from lat long in radians#
######################################################################
#' @title Geographic coordinates to cartesian (x,y,z)
#' @description 
#' Calculates the Cartesian coordinates (x,y,z) from lat long in radians. Used as part of the projection wizard.
#' @author Justin Moat. J.Moat@kew.org
#' @param latr latitude point in radians
#' @param longr longtitude point in radians
#' @return dataframe of x,y,z
#' @examples 
#'lat <- runif (200,-24,-12)
#'long <- runif (200,43,51)
#'thepoints <- data.frame(lat,long)
#'llrad <- deg2rad(thepoints)
#'cartp <- ll2cart(llrad$lat,llrad$long)
#' @references Descartes, R., 1637. Discours de la methode. A Leyde, De l’imprimerie de I. Maire, Paris.
#' @export

ll2cart <- function(latr,longr){
  x <- cos(latr) * cos(longr)
  y <- cos(latr) * sin(longr)
  z <- sin(latr)
  return(data.frame(x,y,z))
}

######################################################################
#calculates the lat long cordinates in radians from Cartesian (x,y,z)#
######################################################################
#' @title Cartesian (x,y,z) to Geographic coordinates
#' @description 
#' calculates the latitude and longitude cordinates in radians from Cartesian coordinates (x,y,z). Used as part of the projection wizard.
#' @author Justin Moat. J.Moat@kew.org
#' @param x East to West coordinate in metres
#' @param y South to North coordinate in metres
#' @param z height coordinate in metres
#' @return dataframe of latitude,longtitude
#' @export

cart2ll <-function (x,y,z){
  latr <- asin(z)
  longr <- atan2(y,x)
  return(data.frame(latr,longr))
}


######################################################################
#calculates Cartesian (x,y,z), projected from the centre of the sphere 
#to the earth surface, returns cartesian (x,y,z)
#used to calculate "true" centre of set of lat longs
# http://stackoverflow.com/questions/9604132/how-to-project-a-point-on-to-a-sphere
######################################################################
#' @title Cartesian coordinate projection
#' @description 
#' Used as part of the projection wizard, calculates Cartesian (x,y,z), projected from the centre of the sphere to the earth surface, returns cartesian coordinates (x,y,z)
#' 
#' @author Justin Moat. J.Moat@kew.org
#' @note
#' http://stackoverflow.com/questions/9604132/how-to-project-a-point-on-to-a-sphere
#' 
#' @param x East to West coordinate in metres
#' @param y South to North coordinate in metres
#' @param z height coordinate in metres
#' @return x,y,z
#' @references 
#' Descartes, R., 1637. Discours de la methode. A Leyde, De l’imprimerie de I. Maire, Paris.
#' @export

pro2sph <- function (x,y,z){
  sc <- 1/sqrt(x^2 + y^2 + z^2)
  x <- x * sc
  y <- y * sc
  z <- z * sc
  return(data.frame(x,y,z))
}

######################################################################
#radians to degrees and degrees to radians
######################################################################
#' @title Radians to Degrees
#' @description 
#' Calculates degrees from radians
#' @author Justin Moat. J.Moat@kew.org
#' @param rad number in radians
#' @return number
#' @examples 
#' b <- 0.392699
#' rad2deg(b)
#' @export

rad2deg <- function(rad) {(rad * 180) / (pi)}

######################################################################
#radians to degrees and degrees to radians
######################################################################
#' @title 
#' Degrees to radians
#' @description 
#' Calculates radians from degrees
#' @author Justin Moat. J.Moat@kew.org
#' @param deg number in degrees
#' @return number
#' @examples 
#' a <- 30
#' deg2rad(a)
#' @export

deg2rad <- function(deg) {(deg * pi) / (180)}

