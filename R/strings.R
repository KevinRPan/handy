#' Functions to help with string handling


#' @title Paste magic function
#' @description Paste characters.
#'
#' @param x1
#'
#' @export
`%p%` <- function(x1, x2) {
  paste0(x1, x2)
}

#' @title c for strings
#' @name cs
#'
#' @description
#' Make a character vector without quotes.
#' Ignores if the values entered are variable names
#'
#' @param ... This is coerced to a character vector
#'
#' @return A character vector of the input values.
#'
#' @examples
#' a <- TRUE
#' string_arb <- cs(a, b, c)
#' string_arb
#' #> "a" "b" "c"
#'
#' @export
cs <- function(...){
  strings <- as.character(match.call(expand.dots = FALSE)[[2]])
  return(strings)
}
