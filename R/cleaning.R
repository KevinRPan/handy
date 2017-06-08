#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

devtools::use_package("dplyr")
devtools::use_package("tidyverse")
devtools::use_package("magrittr")
devtools::use_package("stringr")

#' @title Standardize variable names
#' @name make_names
#'
#' @description
#' Normalize names to lowercase separated by underscores from character vector.
#'
#' @param names character vector to be coerced to syntactically valid names. This is coerced to character if necessary.
#'
#' @return A character vector of same length as \code{names} with each changed to a normalized variable name.
#'
#' @export
make_names <- function(names) {
  # Function to clean up column names
  names %>%
    stringr::str_trim %>%
    tolower %>%
    make.names(unique = TRUE) %>%
    stringr::str_replace_all('[.]', '_') %>%
    stringr::str_replace_all('__{1,4}', '_') %>%
    stringr::str_replace_all('_$', '') %>%
    return
}

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
#' removeColumns(df)
#' removeColumns(df, 0)
#' @export
removeColumns <- function(df, .uniqueness = 1) {
  return(df[df %>% purrr::map_lgl(~ length(unique(na.omit(.x)))>.uniqueness)])
}

