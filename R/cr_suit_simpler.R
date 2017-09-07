#' cr_suit_simpler a simpler suitability function
#' use crop@GMIN / 30 as minimum months of crop cycle needed
#' temp_suit : min cycle consecutive months where tavg is within crop@TMIN, crop@TMAX (not opt)
#' kill_temp : tmin-5 > crop@KTMP
#' precip : total precip within growing cycle should be within crop@RMIN, crop@RMAX
#' beware that temp & precip have to be suitable at same time !
#' perhaps I can use movingFun to estimate whether each starting month is suitable ?
#' cr_plot_cycle plot length of crop cycle from ecocrop database
#'
#' @param crop an ecocrop cropname or object
#' @param tmin single tmin value for a site
#' @param tavg single tavg value for a site
#' @param prec single precipitation value for a site
#' @param rainfed FALSE for irrigated
#'
#' @return 1/0 for suitable/unsuitable
#' @export
#' @examples
#' crop <- getCrop('potato')
#' cr_suit_simpler(crop, 5:16, 15:26, runif(12)*100, rainfed=TRUE)

cr_suit_simpler <- function(crop, tmin, tavg, prec, rainfed) {

  dfmonthsuit <- cr_suit_simpler_month(crop, tmin, tavg, prec, rainfed)

  # If any months in the year are suitable then that area is suitable.
  # can return max of the monthly suitabilities
  #suit <- max(m_suit_min)
  suit <- max(dfmonthsuit$all)

  return(suit)
}


#' cr_suit_simpler_month
#'
#' returns suitabilities by month and attribute
#'
#' @param crop an ecocrop cropname or object
#' @param tmin single tmin value for a site
#' @param tavg single tavg value for a site
#' @param prec single precipitation value for a site
#' @param rainfed FALSE for irrigated
#'
#' @return dataframe of suitabilities by month and attribute
#' @export
#' @examples
#' crop <- getCrop('potato')
#' cr_suit_simpler_month(crop, 5:16, 15:26, runif(12)*100, rainfed=TRUE)
#'
cr_suit_simpler_month <- function(crop, tmin, tavg, prec, rainfed) {

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

  # try putting suit results into a dataframe
  dfmonthsuit <- data.frame(temp_min=rep(NA,12),
                            temp_max=rep(NA,12),
                            temp_kill=NA,
                            rain_high=NA,
                            rain_low=NA,
                            all=NA)

  # 1) is tavg within TMIN & TMAX for duration months after each start month
  #first calc monthly tmin & tmax suit
  m_tmin_suit <- ifelse(tavg >= crop@TMIN, 1, 0)
  m_tmax_suit <- ifelse(tavg <= crop@TMAX, 1, 0)
  #or can do both at same time
  #m_temp_suit <- ifelse(tavg >= crop@TMIN & tavg <= crop@TMAX, 1, 0)

  #find if each month is start of growing cycle of temp suitability
  #m_temp_suit_cycle <- movingFun(m_t_suit, n=mincycle, fun=min, type='from', circular=TRUE)
  #dfmonthsuit$temperature <- movingFun(m_t_suit, n=mincycle, fun=min, type='from', circular=TRUE)
  dfmonthsuit$temp_min <- movingFun(m_tmin_suit, n=mincycle, fun=min, type='from', circular=TRUE)
  dfmonthsuit$temp_max <- movingFun(m_tmax_suit, n=mincycle, fun=min, type='from', circular=TRUE)


  # 2) GMIN months where tmin-5 > KTMP
  # min temp must be at least 5 degrees greater than killtemp to guarantee killtemps not reached
  m_tkil_suit <- ifelse(tmin-5 > crop@KTMP, 1, 0)

  #find if each month is start of growing cycle of kill temp suitability
  #m_tkil_suit_cycle <- movingFun(m_tkil_suit, n=mincycle, fun=min, type='from', circular=TRUE)
  dfmonthsuit$temp_kill <- movingFun(m_tkil_suit, n=mincycle, fun=min, type='from', circular=TRUE)

  #bind 2 suitability columns together
  # m_suit_all <- cbind(m_temp_suit_cycle,
  #                     m_tkil_suit_cycle)

  #precipitation
  if (rainfed) {
    # 3) GMIN months where cumulative precip <= RMAX
    # 4) GMAX months where cumulative precip >= RMIN
    # need to total up precip in the following months first
    # so its diff from temp
    m_rmin_cum <- movingFun(prec, n=mincycle, fun=sum, type='from', circular=TRUE)
    m_rmax_cum <- movingFun(prec, n=maxcycle, fun=sum, type='from', circular=TRUE)

    # m_rmin_suit_cycle <- ifelse(m_rmin_cum <= crop@RMAX, 1, 0)
    # m_rmax_suit_cycle <- ifelse(m_rmax_cum >= crop@RMIN, 1, 0)
    dfmonthsuit$rain_high <- ifelse(m_rmin_cum <= crop@RMAX, 1, 0)
    dfmonthsuit$rain_low <- ifelse(m_rmax_cum >= crop@RMIN, 1, 0)

    #bind on columns of precip suit to temperature calc above
    # m_suit_all <- cbind(m_suit_all,
    #                     m_rmin_suit_cycle,
    #                     m_rmax_suit_cycle)

  }

  # If the minimum of all columns for a month is 1 then that month is suitable.

  # find the minimum suitability value for each month (from temp, kill temp, precip)
  #m_suit_min <-  apply(m_suit_all, 1, min)
  dfmonthsuit$all <- apply(dfmonthsuit, 1, min, na.rm=TRUE)

  return(dfmonthsuit)
}
