#' true_pos simple method to calculate true positive rate between an observed and predicted raster where each is greater than zero
#'
#' treats cells that are greater than 0 in both layers as true positive
#'
#' @param obs raster of observed values
#' @param pred raster of predicted values
#'
#' @return true positive rate between 0 & 1
#' @export
#' @examples
#' crop <- 'maize'
#' obs <- raster::subset(st,paste0(crop,'_mirca'))
#' pred <- raster::subset(st,paste0(crop,'_suitsimp'))
#' tpos_s <- true_pos(obs=obs, pred=pred)

true_pos <- function(obs, pred) {

  #True Positive Rate: When it's actually yes, how often does it predict yes?
  #Number observed positive cells which are predicted positive / Total observed positive cells

  df <- data.frame( obs=raster::values(obs),
                    pred=raster::values(pred))

  #true positive cells, if obs>0 & pred>0
  df <- dplyr::mutate(df, truepos = ifelse(obs>0 & pred>0,1,0))

  #true positive rate
  tpr <- sum(df$truepos,na.rm=TRUE) / sum(df$obs>0)

  return(tpr)
}


#' true_neg simple method to calculate true negative rate between an observed and predicted raster
#'
#' treats cells that are 0 in both layers as true negative
#'
#' @param obs raster of observed values
#' @param pred raster of predicted values
#'
#' @return true negative rate between 0 & 1
#' @export
#' @examples
#' crop <- 'maize'
#' obs <- raster::subset(st,paste0(crop,'_mirca'))
#' pred <- raster::subset(st,paste0(crop,'_suitsimp'))
#' tneg <- true_neg(obs=obs, pred=pred)

true_neg <- function(obs, pred) {

  #Specificity (also called the true negative rate) measures the proportion of negatives that are correctly identified as such

  #Number observed negative cells which are predicted negative / Total observed negative cells

  df <- data.frame( obs=raster::values(obs),
                    pred=raster::values(pred))

  #true negative cells, if obs=0 & pred=0
  df <- dplyr::mutate(df, trueneg = ifelse(obs==0 & pred==0,1,0))

  #true negative rate
  tneg <- sum(df$trueneg,na.rm=TRUE) / sum(df$obs==0)

  return(tneg)
}
