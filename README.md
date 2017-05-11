[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/flowAutomateR)](http://cran.r-project.org/package=datatable_inflix_operators)

<h1>datatable.inflix.operators</h1>

Additional inflix operators for the R package datatable

## Description ##

Additional inflix operators and helper functions to use with R data.table objects during exploratory data analysis or parallel execution.

## Installation ##

`devtools::install_github("andrewrech/datatable.inflix.operators")`

## Manifest ##

* `allduplicated`: Return all duplicated rows of a data.table
* `%likef%`: Return logical vector of elements matching fixed pattern
* `%include%`: Return vector elements matching regexpr
* `%includef%`: Return vector elements matching fixed pattern
* `%exclude%`: Return vector elements excluding regexpr
* `%excludef%`: Return vector elements excluding fixed pattern
* `%withoutrows%`: Remove data.table rows by reference
* `%with%`: Return data.table with columns matching regexpr
* `%without%`: Remove columns from a data.table by reference
* `withoutna`: Remove all(NA/NULL) columns from a data.table
* `chunk`: chunk a data.table to disk using fwrite

## Bugs ##

## Authors ##

Andrew J. Rech (andrewrech@gmail.com)

## License ##

GNU General Public License v3.0