#' true_pos simple method to calculate true positive rate between an observed and predicted raster
#'
#' @param obs raster of observed values
#' @param pred raster of predicted values
#'
#' @return true positive rate between 0 & 1
#' @export
#' @examples
#' obs <- subset(st,paste0(crop,'_mirca'))
#' pred <- subset(st,paste0(crop,'_suitsimp'))
#' tpos_s <- true_pos(obs=obs, pred=pred)

true_pos <- function(obs, pred) {

  #True Positive Rate: When it's actually yes, how often does it predict yes?
  #Number observed positive cells which are predicted positive / Total observed positive cells

  #pred, obs

  # n(1,1) / n(*,1)

  #obs@data@values
  #pred@data@values

  df <- data.frame( obs=raster::values(obs),
                    pred=raster::values(pred))

  #true positive cells
  df <- dplyr::mutate(df, truepos = ifelse(obs>0 & pred>0,1,0))

  #true positive rate
  tpr <- sum(df$truepos,na.rm=TRUE) / sum(df$obs>0)

  return(tpr)
}


