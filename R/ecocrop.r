#' ecocrop
#'
#' dismo::ecocrop function, copied here temporarily to see if we can extract parts
#' and/or increase flexibility.
#' With added comments.
#'
#' adds suitability for a single pixel to the ecocrop object
#'
#' This is from .doEcocrop, some additional data setup occurs in dismo::ecocrop and .ecoSpat()
#'
#' source : https://r-forge.r-project.org/scm/viewvc.php/pkg/dismo/R/ecocrop.R?view=markup&root=dismo
#'
#'
#' @param crop an ecocrop cropname or object
#' @param tmin single tmin value for a site
#' @param tavg single tavg value for a site
#' @param prec single precipitation value for a site
#' @param rainfed FALSE for irrigated
#'
#' @import dismo
#'
#don't export for now @export
#'
#' @examples
#' ecocrop('potato', 5:16, 15:26, runif(12)*100)
#' crop <- getCrop('Hot pepper')
#' ecocrop(crop, 5:16, 15:26, rainfed=FALSE)

ecocrop <- function(crop, tmin, tavg, prec, rainfed) {

  if (rainfed) {
    nasum <- sum(is.na(c(tmin, tavg, prec)))
  } else {
    nasum <- sum(is.na(c(tmin, tavg)))
  }

  if (nasum > 0) { return( new('ECOCROP')) }

  #from div-gis documentation
  #the growing period is defined in days between Gmin and Gmax (start of growth and end of growth).
  #The length of the growing season is defined as the average of Gmin and Gmax.
  #GMIN and GMAX are in months, div by 60 converts to days
  duration <- round((crop@GMIN + crop@GMAX) / 60)

  #temperature
  tmp <- c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX)
  temp <- .getY(tmp, tavg)
  ktmp <- c(crop@KTMP, crop@KTMP, Inf, Inf)
  tmin <- .getY(ktmp, tmin-5)

  #precipitation
  if (rainfed) {
    #put limits into vector 1=min, 2=opmin, 3=opmax, 4=max
    pre <- c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX)
    #create precip vector with december first
    shftprec <- c(prec[12], prec[-12])
    #calc moving sum of precip
    cumprec <- movingFun(prec, n=duration+1, fun=sum, type='from', circular=TRUE)  + shftprec
    #apply suitability fun to precip
    prec <- .getY(pre, cumprec)
    allv <- cbind(temp, tmin, prec)
  } else {
    allv <- cbind(temp, tmin)
  }
  minv <-  apply(allv, 1, min)
  obj <- new('ECOCROP')
  obj@crop <- crop

  #movingFun(fun=min, type=from) finds
  #the minimum value in the duration after each month
  #for precipitation (not temperature)

  obj@suitability <- movingFun(minv, n=duration, fun=min, type='from', circular=TRUE)
  obj@maxsuit <- max(obj@suitability)
  if (obj@maxsuit > 0) {
    obj@maxper <- which(obj@suitability==max(obj@suitability))
  } else {
    obj@maxper <- 0
  }
  return(obj)
}

#' dismo::.getY() used by dismo::ecocrop()
#' sets suitability based on absolute and optimal ranges
#' 0          <= abs min.
#' rising     > abs min, <= opt min.
#' 1          > opt min, <= opt max.
#' declining  > opt max, < abs max.
#' 0          >= abs max.

.getY <- function(a, x) {
  inter <- function(x1, y1, x2, y2, x) {
    y1 + (y2-y1) * (x-x1) / (x2-x1)
  }
  y <- x
  if (is.null(a)) {
    y[] <- 1
  } else {
    y[] <- NA
    y[ x <= a[1] ] <- 0
    y[ x <= a[2] & x > a[1] ] <- inter(a[1], 0, a[2], 1, x[ x <= a[2] & x > a[1] ] )
    y[ x <= a[3] & x > a[2] ] <- 1
    y[ x <= a[4] & x > a[3] ] <- inter(a[3], 1, a[4], 0, x[ x <= a[4] & x > a[3] ] )
    y[ x >= a[4] ] <- 0
  }
  return(y)
}
