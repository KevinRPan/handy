#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

devtools::use_package("dplyr")
devtools::use_package("tidyverse")
devtools::use_package("magrittr")
devtools::use_package("stringr")

#' @title asQuarterDate
#' @description Convert discrete year and quarter to date for plotting purposes.
#'
#' @return a date variable according to the start date of the quarter
#' @param year a year that makes sense
#' @param quarter a quarter, 1 to 4
#' @param day a day that makes sense
#'
#' @examples
#' asQuarterDate(2008,1,
#' @export
asQuarterDate <- function(year, quarter, day = 1) {
  stopifnot(quarter %in% 1:4)
  stopifnot(day %in% 1:31)
  as.Date(paste(year, (as.numeric(quarter)-1) * 3 + 1, day, sep = '-'))
}


#' @title %+%
#' @description Paste characters. Yay.
#' @export
`%+%` <- function(x1, x2) paste0(x1, x2)

# ---------------------------------------------------------------------------------------------
# Formatting functions for ggplot  graph axis
# ---------------------------------------------------------------------------------------------

#' @title Human Numbers
#' @description Format numbers so they're legible for humans
#' Use this in ggplot for labels where you might use the comma or percent functions from the
#' Scales package.
#'
#' Checks whether numbers are positive or negative.
#' Allows up to 1 significant figure
#' sapply used for element-wise application of the humanity function as a vector may include
#' numbers where billions, millions or thousands are appropriate.
#'
#' @return a character vector the same length as the input vector
#' @param x a numeric vector to format,
#' @param smbl a symbol you'd like to prefix your numbers by e.g. "$"
#' @param signif the number of significant places you want the function to return
#' @examples
#' human_numbers(c(1000000 , 1500000, 10000000000))
#' human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#' ggplot2 + scale_y_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_numbers)
#' @export

human_numbers <- function(x = NULL, smbl ="", signif = 1){
  humanity <- function(y){

    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)

      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }

      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"bn")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }

  sapply(x,humanity)
}

#' @export
scale_y_human <- function(..., smbl = "", signif = 1) {
  scale_y_continuous(labels = function(x) human_numbers(x, smbl = smbl, signif = signif), ...)
}
#' @export
scale_x_human <- function(...) {
  scale_x_continuous(labels = function(x) human_numbers(x, smbl = smbl, signif = signif), ...)
}

#' @export
human_num   <- function(x){human_numbers(x, smbl = "")}

#' @export
human_usd   <- function(x){human_numbers(x, smbl = "$")}

