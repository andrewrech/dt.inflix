[![Build Status](https://travis-ci.org/andrewrech/dt.inflix.svg?branch=master)](https://travis-ci.org/andrewrech/dt.inflix) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dt.inflix)](http://cran.r-project.org/package=dt.inflix)

# dt.inflix

Inflix operators for the R package [`data.table`](https://github.com/Rdatatable/data.table)

## Description

Extra inflix operators and helper functions to use with the R package `data.table` during exploratory data analysis.

## Installation

```r
devtools::install_github("tidyverse/magrittr")  ## Github version is required

devtools::install_github("andrewrech/dt.inflix")
```

## Manifest

* `allduplicated`: Return all duplicated rows of a data.table
* `%likef%`: Return logical vector of elements matching fixed pattern
* `%include%`: Return vector elements matching regexpr
* `%includef%`: Return vector elements matching fixed pattern
* `%exclude%`: Return vector elements excluding regexpr
* `%excludef%`: Return vector elements excluding fixed pattern
* `%withoutrows%`: Remove data.table rows by reference
* `%with%`: Return data.table with columns matching regexpr
* `%without%`: Remove columns from a data.table by reference
* `withoutna`: Remove all(NA) or all(NULL) columns from a data.table
* `chunk`: chunk a data.table to disk using fwrite

## Bugs

## Authors

[Andrew J. Rech](http://info.rech.io)

## License

GNU General Public License v3.0