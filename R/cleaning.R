#' Functions to help with data cleaning

#' @title Remove columns based on a number of distinct values
#'
#' @description
#' Remove columns with less than a specified number of distinct values.
#'
#' @param df the input dataframe
#' @param .uniqueness the number of unique values the value should take on, omitting NAs.
#'
#' @return A dataframe with columns removed.
#' Note: using 0 will remove columns where all observations are NA.
#' @examples
#' df <- head(data.frame('x' = letters, 'y' = rep(0, length(letters)), z = rep(NA, length(letters))))
#' remove_columns(df)
#' remove_columns(df, 0)
#' @export
remove_columns <- function(df, .uniqueness = 1) {
  return(df[sapply(df, function(.x) length(unique(na.omit(.x))) > .uniqueness)])
}

