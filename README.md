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

## Cleaning data with handy

### Philosophy 

There are common steps that a data analyst faces:

- Import (messy) data
- **Clean** messy data
- **Describe** cleaned data
- **Plot** cleaned data
- Model data
- **Write** output tables

This package aims to facilitate this basic data handling. 

These functions are ones that I've found handy. 

### Quick Reference Table

| handy Function    | Use Case     | Description |
|-------------------|--------------|----------------------|
| `clean_names`     |  Clean Data      | Normalize variable names of a data.frame to snake_case |
| `title_names`     |  Clean Data      | Normalize variable names of a data.frame to Title Case |
| `remove_columns`  |  Clean Data      | Removes columns that are either missing or non-unique |   
| `add_mean_row`    |  Describe Data   | Add a mean row to a data.frame of numeric values |
| `add_total_row`   |  Describe Data   | Add a total row to a data.frame of numeric values |
| `check_variables` |  Describe Data   | Calculate % missing and number of unique values for all variables |
| `%p%`             |  Shorthand       | Allow string concatenation by piping |
| `cs`              |  Shorthand       | Makes a character vector without quotes |
| `named_list`      |  Shorthand       | Makes a named list using object names |
| `multiplot`       |  Plotting        | Arrange multiple plots |
| `scale_x_human`   |  Plotting        | Scales ggplot axis to easily readable K, M, B, etc. |
| `scale_y_human`   |  Plotting        | Scales ggplot axis to easily readable K, M, B, etc. |
| `write_excel`     |  Write Output    | Write a list of data.frames in R to named sheets in an Excel workbook |
| `write_regression_to_excel` | Write Output | Write a list of data.frames in R to named sheets in an Excel workbook |

Note: package is under development and functions available may change. 

## Contact

You are welcome to:
* submit suggestions and bug-reports at: <https://github.com/kevinrpan/handy/issues>
* send a pull request on: <https://github.com/kevinrpan/handy/>
