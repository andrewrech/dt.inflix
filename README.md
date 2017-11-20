[![Build Status](https://travis-ci.org/andrewrech/dt.inflix.svg?branch=master)](https://travis-ci.org/andrewrech/dt.inflix) [![codecov.io](https://codecov.io/github/andrewrech/dt.inflix/coverage.svg?branch=master)](https://codecov.io/github/andrewrech/dt.inflix?branch=master) ![](https://img.shields.io/badge/version-0.0.1-blue.svg)


# dt.inflix

Inflix operators for the R package [`data.table`](https://github.com/Rdatatable/data.table)

## Description

Extra inflix operators and helper functions to use with the R package `data.table` during exploratory data analysis.

## Installation dependencies and `dt.inflix`

```sh
curl -fsSL http://get.rech.io/install_dt.inflix.sh | sudo sh
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