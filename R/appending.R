#' Functions to help with adding statistics to data

#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

devtools::use_package("dplyr")
devtools::use_package("purrr")
devtools::use_package("tibble")
devtools::use_package("magrittr")

#' @title Add a total row to a dataframe
#'
#' @description
#' Add a total row to a dataframe where the first column is the category.
#'
#' @param df the input dataframe
#' @param .na_rm option to remove NA from sum
#'
#' @return Dataframe with an added row for the totals of numeric variables.
#' May require explicitly converting NAs.
#' @examples
#' df <- data.frame('a' = letters, 'b' = 1:length(letters), 'c' = rep(NA, length(letters)))
#' add_total_row(df)
#' @export
add_total_row <- function(df, .na_rm = FALSE) {
  if("grouped_df" %in% class(df)){ df %<>% dplyr::ungroup() }

  added_row <- df %>%
    dplyr::select_if(is.numeric) %>%
    purrr::map_dbl(sum, na.rm = .na_rm) %>%
    as.data.frame %>%
    t %>%
    tibble::as_tibble()

  df %<>%
    dplyr::mutate_at(1, function(x) ifelse(is.na(x), 'NA', as.character(x))) %>%
    dplyr::bind_rows(added_row)
  df[nrow(df), 1] <- "Total"
  return(df)
}

#' @title Add a mean row to a dataframe
#'
#' @description
#' Add a mean row to a dataframe where the first column is the category.
#'
#' @param df the input dataframe
#' @param .na_rm option to remove NA from sum
#'
#' @return Dataframe with an added row for the totals of numeric variables.
#' May require explicitly converting NAs.
#' @examples
#' df <- data.frame('a' = letters, 'b' = 1:length(letters), 'c' = rep(NA, length(letters)))
#' add_mean_row(df)
#' @export
add_mean_row <- function(df, .na_rm = FALSE) {
  if("grouped_df" %in% class(df)){ df %<>% dplyr::ungroup() }
  added_row <-
    df %>%
    dplyr::select_if(is.numeric) %>%
    purrr::map_dbl(mean, na.rm = .na_rm) %>%
    as.data.frame %>%
    t %>%
    tibble::as_tibble()

  df %<>%
    dplyr::mutate_at(1, function(x) ifelse(is.na(x), 'NA', as.character(x))) %>%
    dplyr::bind_rows(added_row)
  df[nrow(df), 1] <- "Average"
  return(df)
}
