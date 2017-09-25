
#example

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


