#' Functions to help with renaming data

#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

usethis::use_package("dplyr")
usethis::use_package("purrr")
usethis::use_package("magrittr")
usethis::use_package("stringr")


#' @title Named List
#' @name named_list
#'
#' @description
#' Set the names of a list object as the same as the object itself
#'
#' @param ... objects to be passed into a list object
#'
#' @return A named list containing the input objects.
#'
#' @examples
#' a <- TRUE
#' b <- 1:5
#' named_list(a, b)
#' #> $a
#' #> [1] TRUE
#' #>
#' #> $b
#' #> [1] 1 2 3 4 5
#'
#' @export
named_list <- function(...){
  strings <- as.character(match.call(expand.dots = FALSE)[[2]])
  named_list <- list(...) %>% magrittr::set_names(strings)
  return(named_list)
}

#' @title Standardize variable names
#' @name make_names
#'
#' @description
#' Normalize names to lowercase separated by underscores from character vector.
#'
#' @param names character vector to be coerced to syntactically valid names. This is coerced to character if necessary.
#'
#' @return A character vector of same length as \code{names} with each changed to a normalized variable name.
#' @examples
#' n <- c('An', 'ex.amPLE', ' of me$$y', 'var__i@ble names  ')
#' make_names(n)
#' @export
make_names <- function(names) {
  # Function to clean up column names
  names %>%
    stringr::str_trim() %>%
    stringr::str_replace("%","percent") %>%
    tolower() %>%
    make.names(unique = TRUE) %>%
    stringr::str_replace_all('[.]', '_') %>%
    stringr::str_replace_all('__{1,4}', '_') %>%
    stringr::str_replace_all('_$', '') %>%
    return
}


#' @title Make variable names into title case
#' @name title_names
#'
#' @description
#' Replace underscores with spaces and make title case.
#'
#' @param df dataframe with names as character vector with underscore separation
#'
#' @return dataframe with names printed for reading
#' @export
title_names <- function(df) {
  df %<>%
    magrittr::set_names(
      names(df) %>%
        stringr::str_replace_all('_', ' ') %>%
        stringr::str_to_title()
    )
}


#' @title Standardize variable names
#' @name clean_names
#'
#' @description
#' Normalize names to lowercase separated by underscores from character vector.
#'
#' @param df A dataframe to be coerced to have syntactically valid names.
#'
#' @return A dataframe with normalized variable names.
#' @examples
#' d <- data.frame(list('An'=1, 'ex.amPLE'=2, ' of me$$y'=3, 'var__i@ble names  '=4))
#' clean_names(d)
#' @export
clean_names <- function(df) {
  # Function to clean up column names
  return(magrittr::set_names(df, make_names(names(df))))
}
