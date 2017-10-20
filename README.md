# handy
The handy package is an R toolbox of commonly used functions for working with data. 

This package aims to reduce repetitive data cleaning code into easy to use functions compatible with tidy data philosophy.


## Installation

The user can use the **devtools** package to install the development version:

```r
## devtools is required
library(devtools)
install_github("kevinrpan/handy")
```

Note: Windows users need [devtools](http://CRAN.R-project.org/package=devtools) to install this way.

## Contact

You are welcome to:
* submit suggestions and bug-reports at: <https://github.com/kevinrpan/handy/issues>
* send a pull request on: <https://github.com/kevinrpan/handy/>

## Cleaning data with handy

**Quick Reference Table** 

| handy Function | Description |
|----------------------|----------------------|
| `cs`  | Makes a character vector without quotes |
| `normVarNames` |  Normalize variable names of a data.frame to snake_case |
| `addTotalRow`  |  Add a total row to a data.frame of numeric values |
| `removeColumns` | Removes columns that are either missing or non-unique |   
| `scale_x_human` | Scales ggplot axis to K, M, B, etc. |
| `checkVariables` | Create table describing % missing and number of unique values for all variables |
| `writeExcel` | Write a list of data.frames in R to named sheets in an Excel workbook |

Note: package is under development and functions available may change. 
