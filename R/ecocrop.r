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
#' crop <- getCrop('potato')
#' ecocrop(crop, 5:16, 15:26, runif(12)*100, rainfed=TRUE)
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
  #to return a temperature suitability for each month
  temp <- .getY(tmp, tavg)
  ktmp <- c(crop@KTMP, crop@KTMP, Inf, Inf)
  #to return a 0/1 suit for whether tmin-5 in each month goes below killing temp
  tmin <- .getY(ktmp, tmin-5)

  #precipitation
  if (rainfed) {
    #put limits into vector 1=min, 2=opmin, 3=opmax, 4=max
    pre <- c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX)

    #create precip vector with december first
    shftprec <- c(prec[12], prec[-12])

    #calc moving sum of precip & add on to the shifted vector
    #? still not quite sure how it works
    #for each month
    #  sum precipitation in the following duration+1 months (allowing circular)
    #add on shifted precip
    cumprec <- movingFun(prec, n=duration+1, fun=sum, type='from', circular=TRUE)  + shftprec

    #apply suitability fun to precip
    prec <- .getY(pre, cumprec)

    #bind on columns of temperature suitability and min kill temp calculated above
    allv <- cbind(temp, tmin, prec)

  } else {
    #if irrigated only assess temperature
    allv <- cbind(temp, tmin)
  }

  #find the minimum suitability value for each month (from temp, kill temp, precip)
  minv <-  apply(allv, 1, min)

  obj <- new('ECOCROP')
  obj@crop <- crop

  #movingFun(fun=min, type=from) finds
  #the minimum value in the duration after each month
  #for the minimum suitability value calculated above

  obj@suitability <- movingFun(minv, n=duration, fun=min, type='from', circular=TRUE)

  #find the max of the monthly suit values
  obj@maxsuit <- max(obj@suitability)
  #if the max suit is > 0 find the month(s) that have the max suit
  if (obj@maxsuit > 0) {
    obj@maxper <- which(obj@suitability==max(obj@suitability))
  } else {
    obj@maxper <- 0
  }
  return(obj)
}

# simpler suitability function
# use crop@GMIN / 30 as minimum months of crop cycle needed
# temp_suit : min cycle consecutive months where tavg is within crop@TMIN, crop@TMAX (not opt)
# kill_temp : tmin-5 > crop@KTMP
# precip : total precip within growing cycle should be within crop@RMIN, crop@RMAX
# beware that temp & precip have to be suitable at same time !
# perhaps I can use movingFun to estimate whether each starting month is suitable ?

cr_suit_simpler <- function(crop, tmin, tavg, prec, rainfed) {

  # use crop@GMIN / 30 as minimum months of crop cycle needed
  mincycle <- round( crop@GMIN / 30)
  maxcycle <- round( crop@GMAX / 30)

  # calculate for each month whether (0/1) it is the start of  :
  # 1) GMIN months where tavg > TMIN & tavg < TMAX
  # 2) GMIN months where tmin-5 > KTMP
  # 3) GMIN months where cumulative precip < RMAX
  # 4) GMAX months where cumulative precip > RMIN
  #
  # If the minimum of these for a month is 1 then that month is suitable.
  # If any months in the year are suitable then that area is suitable.

  # 1) is tavg within TMIN & TMAX for duration months after each start month
  #first calc monthly tmin & tmax suit
  #m_tmin_suit <- ifelse(tavg >= crop@TMIN, 1, 0)
  #m_tmax_suit <- ifelse(tavg <= crop@TMAX, 1, 0)
  #or can do both at same time
  m_temp_suit <- ifelse(tavg >= crop@TMIN & tavg <= crop@TMAX, 1, 0)

  #find if each month is start of growing cycle of temp suitability
  m_temp_suit_cycle <- movingFun(m_t_suit, n=mincycle, fun=min, type='from', circular=TRUE)

  # 2) GMIN months where tmin-5 > KTMP
  # min temp must be at least 5 degrees greater than killtemp to guarantee killtemps not reached
  m_tkil_suit <- ifelse(tmin-5 > crop@KTMP, 1, 0)

  #find if each month is start of growing cycle of kill temp suitability
  m_tkil_suit_cycle <- movingFun(m_tkil_suit, n=mincycle, fun=min, type='from', circular=TRUE)

  # 3) GMIN months where cumulative precip <= RMAX
  # 4) GMAX months where cumulative precip >= RMIN
  # need to total up precip in the following months first
  # so its diff from temp
  m_rmin_cum <- movingFun(prec, n=mincycle, fun=sum, type='from', circular=TRUE)
  m_rmax_cum <- movingFun(prec, n=maxcycle, fun=sum, type='from', circular=TRUE)

  m_rmin_suit_cycle <- ifelse(m_rmin_cum <= crop@RMAX, 1, 0)
  m_rmax_suit_cycle <- ifelse(m_rmax_cum <= crop@RMIN, 1, 0)







  #precipitation
  if (rainfed) {
    #put limits into vector 1=min, 2=opmin, 3=opmax, 4=max
    pre <- c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX)

    #create precip vector with december first
    shftprec <- c(prec[12], prec[-12])

    #calc moving sum of precip & add on to the shifted vector
    #? still not quite sure how it works
    #for each month
    #  sum precipitation in the following duration+1 months (allowing circular)
    #add on shifted precip
    cumprec <- movingFun(prec, n=duration+1, fun=sum, type='from', circular=TRUE)  + shftprec

    #apply suitability fun to precip
    prec <- .getY(pre, cumprec)

    #bind on columns of temperature suitability and min kill temp calculated above
    allv <- cbind(temp, tmin, prec)

  } else {
    #if irrigated only assess temperature
    allv <- cbind(temp, tmin)
  }

  #find the minimum suitability value for each month (from temp, kill temp, precip)
  minv <-  apply(allv, 1, min)

  obj <- new('ECOCROP')
  obj@crop <- crop

  #movingFun(fun=min, type=from) finds
  #the minimum value in the duration after each month
  #for the minimum suitability value calculated above

  obj@suitability <- movingFun(minv, n=duration, fun=min, type='from', circular=TRUE)

  #find the max of the monthly suit values
  obj@maxsuit <- max(obj@suitability)
  #if the max suit is > 0 find the month(s) that have the max suit
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
