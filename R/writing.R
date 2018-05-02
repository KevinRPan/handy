#' Functions to help with writing data, especially to Excel

#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

devtools::use_package("broom")
devtools::use_package("magrittr")
devtools::use_package("purrr")
devtools::use_package("stringr")
devtools::use_package("XLConnect")
devtools::use_package("XLConnectJars")

#' @title Write to Excel
#'
#' @description
#' Writes a list of dataframe objects to an Excel workbook.
#'
#'
#' @param dfs the input dataframes as a list
#' @param workbook_fname workbook file name to save to
#' @param sheet_names (optional) formatted names of sheets, if not using object names as defaults
#' @param title_names (optional) whether to title format variable names
#' @param add_row_border (optional) whether to include an underline border under the names row
#'
#' @return nothing
#' @examples
#' df <- data.frame('a' = letters, 'b' = 1:length(letters), 'c' = rep(NA, length(letters)))
#' write_excel(df,
#'             'wb.xlsx',
#'             title_names = TRUE)
#' @export
write_excel <-
  function(dfs,
           workbook_fname,
           sheet_names,
           title_names = FALSE,
           add_row_border = FALSE) {

    ## Transform to list if only single dataframe
    if ("data.frame" %in% class(dfs)) {
      dfs <- list(df = dfs)
    }

    ## Fill sheet_names with object names as default
    if (missing(sheet_names)) {
      sheet_names <- names(dfs)
      if(is.null(sheet_names)) {
        stop("Data input not named and no sheet names specified. \nUse a named list or specify sheet names.")
      }
    }

    wb <- XLConnect::loadWorkbook(workbook_fname, create = TRUE)

    ## Set a cell style header

    csHeader <- tryCatch({
      # try to create one
      csHeader <- XLConnect::createCellStyle(wb, name = "header")
    }, error = function(e) {
      # if one exists, use existing
      csHeader <- XLConnect::getCellStyle(wb, "header")
    })

    XLConnect::setFillPattern(
      csHeader,
      fill  = XLConnect::XLC$FILL.NO_FILL)
    if(add_row_border) {
      XLConnect::setBorder(
        csHeader,
        side  = "bottom",
        type  = XLConnect::XLC$BORDER.THIN,
        color = XLConnect::XLC$COLOR.BLACK
      )
    }

    ## Map to sheets
    purrr::map2(dfs, sheet_names,
                function(df, sheet_name) {
                  XLConnect::createSheet(wb, sheet_name)
                  if (title_names) {
                    df %<>%
                      magrittr::set_names(names(df) %>%
                                 stringr::str_replace_all('_', ' ') %>%
                                 stringr::str_to_title())
                  }
                  XLConnect::writeWorksheet(wb, df, sheet_name)
                  XLConnect::setCellStyle(
                    wb,
                    sheet = sheet_name,
                    row = 1,
                    col =  seq(length.out = ncol(df)),
                    cellstyle = csHeader
                  )
                })
    XLConnect::saveWorkbook(wb)
  }


#' @title Write a regression to Excel
#'
#' @description
#' Writes a list of dataframe objects to an Excel workbook.
#'
#'
#' @param reg_models regression models, as a list
#' @param workbook_fname workbook file name to save to
#' @param sheet_names formatted names of sheets
#' @param title_names whether to title format variable names
#'
#' @return nothing
#' @examples
#' # Basic Example
#' mod <- lm(mpg ~ cyl, mtcars)
#' write_regression_to_excel(mod, 'simple_reg.xlsx')
#'
#' # Writing a list of regressions
#' mod_hp <- lm(mpg ~ hp, mtcars)
#'
#' mod_list <- list(mod_cyl = mod, mod_hp = mod_hp)
#' write_regression_to_excel(mod_list, 'mpg_regs.xlsx')
#'
#' write_regression_to_excel(mod_list, 'simple_reg.xlsx', c("Cylinder Model", "HP Model"))
#' @export
write_regression_to_excel <- function(reg_models,
                                      workbook_fname,
                                      sheet_names,
                                      title_names = TRUE
) {

  ## Transform to list if only single dataframe
  if ("lm" %in% class(reg_models)) {
    reg_models <- list(mod = reg_models)
  }
  ## Fill sheet_names with object names as default
  if (missing(sheets)) {
    sheets <- names(reg_models)
    if(is.null(sheets)) {
      stop("Data input not named and no sheet names specified. \nUse a named list or specify sheet names.")
    }
  }
  tidy_regs <- purrr::map(reg_models, broom::tidy)

  write_excel(tidy_regs,
              workbook_fname = workbook_fname,
              sheet_names = sheet_names,
              title_names = title_names)
}
