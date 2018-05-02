## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(handy)
library(magrittr)
library(knitr)
iris3obs <- head(iris, 3) 
iris3obs %>% kable()
iris3obs %>% clean_names %>% kable()

## ------------------------------------------------------------------------
iris3obs %>% remove_columns(1) %>% kable()

## ------------------------------------------------------------------------
mtcars %>% check_variables %>% kable

## ------------------------------------------------------------------------
arrests <- USArrests %>% tibble::rownames_to_column() %>% dplyr::as_tibble()
arrests %>% head %>% add_mean_row %>% kable

## ------------------------------------------------------------------------
arrests %>% head %>% add_total_row %>% kable

## ------------------------------------------------------------------------
library(ggplot2)
p1 <- Seatbelts %>% tibble::as_tibble() %>% 
  ggplot(aes(kms, DriversKilled)) +
  geom_point() +
  scale_x_human() 
p2 <- Seatbelts %>% tibble::as_tibble() %>% 
  ggplot(aes(DriversKilled)) +
  geom_histogram(alpha = .5, bins = 30)
multiplot(p1, p2)

## ------------------------------------------------------------------------
named_list(mtcars, iris) %>% 
  write_excel(workbook_fname = 'wb.xlsx')

readxl::excel_sheets('wb.xlsx')
readxl::read_excel('wb.xlsx', sheet = 'iris') %>% head %>% kable

## ------------------------------------------------------------------------
lm_assault <- lm(Assault ~ UrbanPop, data = arrests)
lm_anscombe <- lm(y1 ~ x1, data = anscombe)
reg_list <- named_list(lm_assault, lm_anscombe)

reg_list

reg_list %>% write_regression_to_excel('reg.xlsx')
readxl::read_excel('reg.xlsx', sheet = 'lm_anscombe') %>% head %>% kable

## ------------------------------------------------------------------------
lsos(n = 5) %>% kable

