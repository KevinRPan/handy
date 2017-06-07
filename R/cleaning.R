#' Hello, world!
#'
#' This is an example function named 'hello'
#' which prints 'Hello, world!'.
#'
#' You can learn more about package authoring with RStudio at:
#'
#'   http://r-pkgs.had.co.nz/
#'
#' Some useful keyboard shortcuts for package authoring:
#'
#'   Build and Reload Package:  'Ctrl + Shift + B'
#'   Check Package:             'Ctrl + Shift + E'
#'   Test Package:              'Ctrl + Shift + T'
#' importFrom magrittr "%>%"

# exportPattern("^[[:alpha:]]+")
devtools::use_package("dplyr")
devtools::use_package("tidyverse")
devtools::use_package("magrittr")
devtools::use_package("stringr")
# roxygen2::roxygenise()

hello <- function() {
  print("Hello, world!")
}

#' @title Standardize variable names
#'
#' @description
#' \code{make_names} returns a vector that has been normalized to lowercase separated by underscores
#'
#' @param var_names vector of variable names
#'
#' @return cleaned up variable names
#'
#' @usage
#' @export
make_names <- function(var_names) {
  # Function to clean up column names
  var_names %>%
    str_trim %>%
    tolower %>%
    make.names(unique = TRUE) %>%
    str_replace_all('[.]', '_') %>%
    str_replace_all('__{1,4}', '_') %>%
    str_replace_all('_$', '') %>%
    return
}

#
# my_fun <- function(a, b) {
#   if (!requireNamespace("pkg", quietly = TRUE)) {
#     stop("Pkg needed for this function to work. Please install it.",
#          call. = FALSE)
#   }
# }
#
