#' Functions to help with plotting

#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

devtools::use_package("dplyr")
devtools::use_package("ggplot2")
devtools::use_package("grid")
devtools::use_package("lubridate")
devtools::use_package("magrittr")
devtools::use_package("scales")
devtools::use_package("stringr")

#' @title quarter_as_date
#' @description Convert discrete year and quarter to date for plotting purposes.
#'
#' @return a date variable according to the start date of the quarter
#' @param year a year
#' @param quarter a quarter, 1 to 4
#' @param day a day
#'
#' @examples
#' quarter_as_date(2008, 1, 15)
#' @export
quarter_as_date <- function(year, quarter, day = 1) {
  stopifnot(quarter %in% 1:4)
  stopifnot(day %in% 1:31)
  lubridate::make_date(year = year,
                       month = (as.numeric(quarter) - 1) * 3 + 1,
                       day = day)
}


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
#' @param ... other arguments passed to scale_*_continuous
#' @examples
#' human_numbers(c(1000000 , 1500000, 10000000000))
#' human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#' \dontrun{
#' ggplot2 + scale_y_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_numbers)
#' ggplot2 + scale_y_human()
#' ggplot2 + scale_x_human()
#' }
#' @source https://github.com/fdryan/R/blob/master/ggplot2_formatter.r
#' @rdname human_numbers
#' @family plotting functions
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
        paste0 (y_is_positive, smbl,  scales::comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }

  sapply(x,humanity)
}


#' @rdname human_numbers
#' @export
scale_y_human <- function(..., smbl = "", signif = 1) {
  ggplot2::scale_y_continuous(labels = function(x) human_numbers(x, smbl = smbl, signif = signif), ...)
}

#' @rdname human_numbers
#' @export
scale_x_human <- function(..., smbl = "", signif = 1) {
  ggplot2::scale_x_continuous(labels = function(x) human_numbers(x, smbl = smbl, signif = signif), ...)
}

#' @rdname human_numbers
#' @export
human_num   <- function(x){human_numbers(x, smbl = "")}

#' @rdname human_numbers
#' @export
human_usd   <- function(x){human_numbers(x, smbl = "$")}


#' @title Multiple plot function
#' @description Format multiple plots in a grid layout
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @return arrangement of plots
#' @param ...    ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' @param cols   Number of columns in layout
#' @param layout Matrix specifying the layout. If present, 'cols' is ignored.
#' @param plotlist List object of plots. Can be specified in addition to individual plots
#' @examples
#'
#' library(ggplot2)
#' # This example uses the ChickWeight dataset, which comes with ggplot2
#' # First plot
#' p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
#' geom_line() +
#'  ggtitle("Growth curve for individual chicks")
#'
#' # Second plot
#' p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
#'  geom_point(alpha=.3) +
#'  geom_smooth(alpha=.2, size=1) +
#'  ggtitle("Fitted growth curve per diet")
#'
#' multiplot(p1, p2)
#' @source http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @export
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots == 1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()

    grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))) %>%
      grid::pushViewport()

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}
