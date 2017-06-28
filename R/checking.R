#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

devtools::use_package("dplyr")
devtools::use_package("tidyverse")
devtools::use_package("magrittr")
devtools::use_package("stringr")
devtools::use_package("XLConnect")

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
addTotalRow <- function(df, .na_rm = FALSE) {
  if("grouped_df" %in% class(df)){ df %<>% dplyr::ungroup() }
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
addMeanRow <- function(df, .na_rm = FALSE) {
  if("grouped_df" %in% class(df)){ df %<>% dplyr::ungroup() }
  df %>%
    dplyr::mutate_at(1, function(x) ifelse(is.na(x), 'NA', as.character(x))) %>%
    dplyr::bind_rows(
      df %>%
        dplyr::select(-1) %>%
        purrr::map_dbl(mean, na.rm = .na_rm) %>%
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
#' df <- data.frame('a' = letters, 'b' = 1:length(letters), 'c' = rep(NA, length(letters)))
#' checkVariables(df)
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
spreadColumn <- function(df, group_cols, spread_col, spread_val, fill_val = 0) {
  df %>%
    dplyr::select_(.dots = c(group_cols, spread_col, spread_val)) %>%
    tidyr::spread_(spread_col, spread_val, fill = fill_val)
}


#' @title Write to Excel
#'
#' @description
#' Writes a list of dataframe objects to an Excel workbook.
#'
#'
#' @param dfs the input dataframes as a list
#' @param sheet_names formatted names of sheets
#' @param workbook_fname workbook file name to save to
#' @param title_names whether to title format variable names
#'
#' @return nothing
#' @examples
#' df <- data.frame('a' = letters, 'b' = 1:length(letters), 'c' = rep(NA, length(letters)))
#' writeExcel(list(df),
#'            'DF',
#'            'wb.xlsx',
#'            title_names = TRUE)
#' @export
writeExcel <-
  function(dfs,
           sheet_names,
           workbook_fname,
           title_names = FALSE) {
    wb <- XLConnect::loadWorkbook(workbook_fname, create = TRUE)
    ## Set a header style
    csHeader <- XLConnect::createCellStyle(wb, name = "header")
    XLConnect::setFillPattern(csHeader, fill = XLC$FILL.NO_FILL)
    XLConnect::setBorder(csHeader,
                         side = "bottom",
                         type = XLC$BORDER.THIN,
                         color = XLC$COLOR.BLACK)
    purrr::map2(dfs, sheet_names,
                function(df, sheet_name) {
                  XLConnect::createSheet(wb, sheet_name)
                  if (title_names) {
                    df %<>%
                      setNames(names(df) %>%
                                 stringr::str_replace_all('_', ' ') %>%
                                 stringr::str_to_title())
                  }
                  XLConnect::writeWorksheet(wb, df, sheet_name)
                  XLConnect::setCellStyle(wb,
                                          sheet = sheet_name,
                                          row = 1,
                                          col =  seq(length.out = ncol(df)),
                                          cellstyle = csHeader)
                })
    XLConnect::saveWorkbook(wb)
  }

