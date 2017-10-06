#' cr_plot_cycle plot length of crop cycle from ecocrop database
#'
#' for a single or vector of crops
#'
#' @param cropnames name or vector of crop names
#'
#' @return ggplot2 object
#' @export
#' @examples
#' cropnames <- c('broom-corn','maize','rice','potato','soya bean','sugarcane')
#' cr_plot_cycle(cropnames)
#' cr_plot_cycle(c('asparagus','maize','celery','rape','cabbage'))

cr_plot_cycle <- function(cropnames=c('maize','rice'))
{
  #todo, do I want to allow cropnames arg to be an ecocrop object ?
  #I don't yet

  dftoplot <- filter(df_ecocrop, NAME %in% cropnames)

  # plot crop cycle
  ggplot(dftoplot, aes(x=NAME))+
    geom_point(aes(y=GMIN), shape=2) + #up triangle
    geom_point(aes(y=GMAX), shape=6) +   #down triangle
    geom_point(aes(y=0.5*(GMIN+GMAX)), shape=1) +
    ylab('days required for crop growth') +
    xlab('') +
    ylim(0,365)+
    theme_minimal()
}


cr_plot_temp <- function(cropnames=c('maize','rice'))
{

  dftoplot <- filter(df_ecocrop, NAME %in% cropnames)

  ggplot(dftoplot, aes(x=NAME))+
    geom_point(aes(y=TMIN), shape=2) + #up triangle
    geom_point(aes(y=TMAX), shape=6) +   #down triangle
    geom_point(aes(y=TOPMN), shape=24, size=1, fill='blue') + #up triangle
    geom_point(aes(y=TOPMX), shape=25, size=1, fill='blue') +   #down triangle
    geom_point(aes(y=KTMP), size=1, fill='red') +   #kill temp red dot, is assessed -5
    ylab('min and max temperatures degrees C') +
    xlab('') +
    ylim(0,NA) +
    theme_minimal()
}

cr_plot_precip <- function(cropnames=c('maize','rice'))
{

  dftoplot <- filter(df_ecocrop, NAME %in% cropnames)

  ggplot(dftoplot, aes(x=NAME))+
    geom_point(aes(y=RMIN), shape=2) + #up triangle
    geom_point(aes(y=RMAX), shape=6) +   #down triangle
    geom_point(aes(y=ROPMN), shape=24, size=1, fill='blue') + #up triangle
    geom_point(aes(y=ROPMX), shape=25, size=1, fill='blue') +   #down triangle
    ylab('min and max precipitation') +
    xlab('') +
    ylim(0,NA) +
    theme_minimal()
}

