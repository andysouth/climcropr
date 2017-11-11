#' mirca2ecocrop converts from mirca name to the closest ecocrop name
#'
#' @param name a mirca cropname
#'
#' @return a translated cropname
#' @export
#' @examples
#' mirca2ecocrop('potatoes')
#' mirca2ecocrop('sorghum')

mirca2ecocrop <- function(name) {

    if (toupper(name) %in% toupper(df_mirca[["mirca_name"]])) {

      df <-
        filter(df_mirca, grepl(name, mirca_name, ignore.case=TRUE))

      name_return <- df$ecocrop_name

    }
    else stop("\nNo crops found, please see the 'climcropr::df_mirca'",
              "data frame for a list.\n")

  return(name_return)
}

#' ecocrop2mirca converts from mirca name to the closest ecocrop name
#'
#' @param name an ecocrop cropname
#'
#' @return a translated cropname
#' @export
#' @examples
#' ecocrop2mirca('potato')
#' ecocrop2mirca('broom-corn')

ecocrop2mirca <- function(name) {

  if (toupper(name) %in% toupper(df_mirca[["ecocrop_name"]])) {

    df <-
      filter(df_mirca, grepl(name, ecocrop_name, ignore.case=TRUE))

    name_return <- df$mirca_name

  }
  else stop("\nNo crops found, please see the 'climcropr::df_mirca'",
            "data frame for a list.\n")

  return(name_return)
}

