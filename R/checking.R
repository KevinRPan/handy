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
    magrittr::set_names(c('Unique Values', 'Percent Missing', 'Example Values')) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_at(1:2, as.numeric) %>%
    tibble::rownames_to_column('Variable')
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
