#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

devtools::use_package("dplyr")
devtools::use_package("tidyverse")
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
#' \dontrun{
#' df <- data.frame('a' = letters, 'b' = 1:length(letters), 'c' = rep(NA, length(letters)))
#' addTotalRow(df)
#' }
#' @export
addTotalRow <- function(df, .na_rm = FALSE) {
  if("grouped_df" %in% class(df)){ df %<>% dplyr::ungroup }
  df %>%
    dplyr::mutate_at(1, function(x) ifelse(is.na(x), 'NA', as.character(x))) %>%
    dplyr::bind_rows(
      df %>%
        dplyr::select(-1) %>%
        purrr::map_dbl(sum, na.rm = .na_rm) %>%
        as.data.frame %>%
        t %>%
        tibble::as_tibble()
    ) %>%
    dplyr::mutate_at(1, function(x) ifelse(is.na(x), 'Total', x)) %>%
    return
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
#' @param df the input dataframe
#' @param num_unique_vals number of unique values to display
#'
#' @return Dataframe with Unique Values, Percent Missing and Examples
#' @examples
#' \dontrun{
#' df <- data.frame('a' = letters, 'b' = 1:length(letters), 'c' = rep(NA, length(letters)))
#' checkVariables(df)
#' }
#' @export
checkVariables <- function(df,num_unique_vals = 3) {
  ## How many unique values do variables take on?
  dplyr::bind_cols(
    df %>%
      purrr::map_df(~ .x %>% unique %>% length %>% as.character) %>%
      dplyr::bind_rows(df %>% pct_missings_chr) %>%
      dplyr::bind_rows(df %>%
                         purrr::map_df(~ paste(
                           .x %>%
                             unique %>%
                             na.omit %>%
                             head(num_unique_vals),
                           collapse = ", ")))
  ) %>%
    t %>%
    as.data.frame %>%
    stats::setNames(c('Unique Values', 'Percent Missing', 'Example Values'))
}


