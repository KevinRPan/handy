#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

devtools::use_package("dplyr")
devtools::use_package("purrr")
devtools::use_package("tibble")
devtools::use_package("tidyr")
devtools::use_package("magrittr")
devtools::use_package("stringr")

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
#' addTotalRow(df)
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
#' addTotalRow(df)
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


pct_missings_chr <- function(df) {
  df %>%
    purrr::map_df(~ (sum(is.na(.x)) * 100 /nrow(df)) %>% round(2) %>% as.character)
}

#' @title Check variables in the dataset
#'
#' @description
#' Get a snapshot preview of what variables are available and what they look like.
#'
#' @param df              the input dataframe
#' @param num_unique_vals number of unique values to display
#' @param sort_examples   whether first few examples or random
#'
#' @return Dataframe with Unique Values, Percent Missing and Examples
#' @examples
#' df <- data.frame('a' = letters, 'b' = 1:length(letters), 'c' = rep(NA, length(letters)))
#' checkVariables(df)
#' @export
check_variables <- function(df, num_unique_vals = 3, sort_examples = FALSE) {
  ## How many unique values do variables take on?
  df %>%
    purrr::map_df(~ .x %>% unique %>% length %>% as.character) %>%
    dplyr::bind_rows(df %>% pct_missings_chr) %>%
    dplyr::bind_rows(df %>%
                       purrr::map_df(~ paste(
                         .x %>%
                           unique %>%
                           na.omit %>%
                           {if(sort_examples) sort(.) else .} %>%
                           head(num_unique_vals),
                         collapse = ", "))) %>%
    dplyr::bind_cols() %>%
    t %>%
    as.data.frame %>%
    magrittr::set_names(c('Unique Values', 'Percent Missing', 'Example Values'))
}

#' @title Spread based on selected variables
#'
#' @description
#' Select and spread data
#'
#' @param df the input dataframe
#' @param group_cols the columns to group by
#' @param spread_col the column to spread by
#' @param spread_val the column values to spread
#' @param fill_val filling missing with what value
#'
#' @return dataframe that is spread based on values
#' @examples
#' df <- data.frame('id' = c(rep('x',2), rep('y',3),'z'), 'g' = letters[1:6], 'v' = 1:6)
#' spreadColumn(df, 'id','g','v')
#' @export
spread_column <- function(df, group_cols, spread_col, spread_val, fill_val = 0) {
  df %>%
    dplyr::select_(.dots = c(group_cols, spread_col, spread_val)) %>%
    tidyr::spread_(spread_col, spread_val, fill = fill_val)
}

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}


#' @title Memory check
#'
#' @description
#' Describe objects in memory
#'
#' @param n     Numeric of how many objects to return
#' @param ...   Other optional parameters:
#' \itemize{
#' \item \code{pos(integer)}: position in search list
#' \item \code{pattern(character)}: an optional regular expression
#' \item \code{order.by(character or integer)}: which column to order by
#' \item \code{decreasing(logical)}: whether to sort by decreasing
#' \item \code{head(logical)}: whether to return the first part only
#' }
#'
#' @return printed description of objects in memory
#'
#' @source Tony Breyal's post attributed to JD Long at:
#' @source https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#'
#' @examples
#' lsos()
#'
#' @export
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
