## ---- allduplicated
#' Return all duplicated rows of a data.table.
#'
#' @param dt Data table.
#' @param by Character vector. Names of columns to test for duplicates.
#'
#' @return A data.table of duplicated rows from \code{dt}.
#'
#' @example
#' library(magrittr)
#' library(data.table)
#' library(dt.inflix)
#'
#'   dt <- data.table::data.table(a = c(1, 1, 1),
#'                                b = c(2, 2, 3))
#'
#'   dt %>% allduplicated
#' @export allduplicated
#' @import data.table
#' @import parallel
#' @import stringi
#' @import testthat
#' @importFrom magrittr %>% %T>% %$% %<>%
#' @importFrom stats na.omit
#' @importFrom utils tail
allduplicated <- function(dt, by = NULL){

fD <- NULL

if (by %>% is.null){
  data.table::setkey(dt)
  dups <- duplicated(dt)
}
if (!by %>% is.null){
  data.table::setkeyv(dt, cols = by)
  dups <- duplicated(dt, by = by)
  }

dt[, fD := dups | c(utils::tail(dups, -1), FALSE)]
dt_dup <- dt[fD == TRUE]
dt_dup[, fD := NULL]
dt[, fD := NULL]

return(dt_dup)

}



## ---- `%likef%`
#' Convenience inflix operator to return logical vector of elements matching a fixed pattern.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @return A logical vector the same length as \code{vector} of elements matching \code{pattern}.
#'
#' @example
#' library(magrittr)
#' library(dt.inflix)
#'
#'   v <- c("Apple", "apple", "APPLE")
#'
#'   v %likef% "ap"
#' @export %likef%
`%likef%` <- function(vector, pattern)
{
    if (is.factor(vector)){
       lv <- as.integer(vector) %in% stringi::stri_detect_fixed(levels(vector), pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    else {
       lv <- stringi::stri_detect_fixed(vector, pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    return(lv)
}



## ---- `%include%`
#' Convenience inflix operator to return vector elements matching a regular expression.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @return A vector of elements in \code{vector} matching \code{pattern}.
#'
#' @example
#' library(magrittr)
#' library(dt.inflix)
#'
#'   v <- c("Apple", "apple", "APPLE")
#'
#'   v %include% "(Ap|AP)"
#' @export %include%
`%include%` <- function(vector, pattern)
{
    if (is.factor(vector)){
       lv <- as.integer(vector) %in% stringi::stri_detect_regex(levels(vector), pattern, opts_regex = stringi::stri_opts_regex(case_insensitive = FALSE, comments = TRUE,  error_on_unknown_escapes = TRUE))
    }
    else {
       lv <- stringi::stri_detect_regex(vector, pattern, opts_regex = stringi::stri_opts_regex(
       case_insensitive = FALSE, comments = TRUE, error_on_unknown_escapes = TRUE))
    }
    return(vector[lv])
}



## ---- `%includef%`
#' Convenience inflix operator to return vector elements matching a fixed pattern.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @return A vector of elements in \code{vector} matching \code{pattern}.
#'
#' @example
#' library(magrittr)
#' library(dt.inflix)
#'
#'   v <- c("Apple", "apple", "APPLE")
#'
#'   v %include% "Ap"
#' @export %includef%
`%includef%` <- function(vector, pattern)
{
    if (is.factor(vector)){
        lv <- as.integer(vector) %in% stringi::stri_detect_fixed(levels(vector), pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    else {
        lv <- stringi::stri_detect_fixed(vector, pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    return(vector[lv])
}



## ---- `%exclude%`
#' Convenience inflix operator to return vector elements excluding those matching a regular expression.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @return A vector of elements in \code{vector} not matching \code{pattern}.
#'
#' @example
#' library(magrittr)
#' library(dt.inflix)
#'
#'   v <- c("Apple", "apple", "APPLE")
#'
#'   v %exclude% "Ap"
#' @export %exclude%
`%exclude%` <- function(vector, pattern)
{
    if (is.factor(vector)){

       lv <- as.integer(vector) %in% stringi::stri_detect_regex(levels(vector), pattern, opts_regex = stringi::stri_opts_regex(case_insensitive = FALSE, comments = TRUE,  error_on_unknown_escapes = TRUE))
    }
    else {
       lv <- stringi::stri_detect_regex(vector, pattern, opts_regex = stringi::stri_opts_regex(
       case_insensitive = FALSE, comments = TRUE, error_on_unknown_escapes = TRUE))
    }
    return(vector[!lv])
}



## ---- `%excludef%`
#' Convenience inflix operator to return vector elements excluding a fixed pattern.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @return A vector of elements in \code{vector} not matching \code{pattern}.
#'
#' @example
#' library(magrittr)
#' library(dt.inflix)
#'
#'   v <- c("Apple", "apple", "APPLE")
#'
#'   v %include% "Ap"
#' @export %excludef%

`%excludef%` <- function(vector, pattern)
{
    if (is.factor(vector)){
        lv <- as.integer(vector) %in% stringi::stri_detect_fixed(levels(vector), pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    else {
        lv <- stringi::stri_detect_fixed(vector, pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    return(vector[!lv])
}



## ---- `%withoutrows%`
#' Convenience inflix operator to remove data.table rows.
#'
#' @param dt A data.table.
#' @param rows Row indicies to delete.
#'
#' @return A data.table subset of \code{dt} with rows from index \code{rows}.
#'
#' @example
#' library(magrittr)
#' library(data.table)
#' library(dt.inflix)
#'
#'   dt <- data.table::data.table(a = c(1, 1, 1),
#'                                b = c(2, 2, 3))
#'
#'   dt %withoutrows% 2
#'   print(dt)
#' @export %withoutrows%

`%withoutrows%` <- function(dt, rows){

  if (rows %>% is.na %>% any | rows %>% is.null %>% any | !rows %>% as.integer) stop("Error in row indicies.")

  keep <- setdiff(dt[, .I], rows)
  cols = names(dt)
  dt.subset <- data.table::data.table(dt[[1]][keep])
  data.table::setnames(dt.subset, cols[1])
  for (col in cols[2:length(cols)]){
    dt.subset[, (col) := dt[[col]][keep]]
    dt[, (col) := NULL]  # delete
  }
  # remove remaining dt col
  dt[, (dt %>% names) := NULL]

# assign new subset dt back to parent env
assign(deparse(substitute(dt)), dt.subset, envir =  parent.frame())
}



## ---- `%with%`
#' Convenience inflix operator to return a data.table of columns matching a regular expression.
#'
#' @param dt A data.table.
#' @param cols Vector of column patterns to include.
#'
#' @return A data.table subset of \code{dt} with column names matching \code{cols}.
#'
#' @example
#' library(magrittr)
#' library(data.table)
#' library(dt.inflix)
#'
#'   dt <- data.table::data.table(a = c(1, 1, 1),
#'                                b = c(2, 2, 3))
#'
#'   dto <- dt %with% "b"
#'   print(dto)
#' @export %with%

`%with%` <- function(dt, cols){

dt_cols <- lapply(cols, function(x){
dt %>% names %include% x
}) %>% unlist %>% unique %>% stats::na.omit %>% as.vector

if (dt_cols %>% length == 0){
  stop("No columns selected.")
}
return(invisible(dt[, .SD, .SDcols = dt_cols]))

}



## ---- `%without%`
#' Convenience inflix operator to remove columns from a data.table by reference.
#'
#' @param dt A data.table.
#' @param cols Vector of columns patterns to remove.
#'
#' @return A data.table subset of \code{dt} with column names not matching \code{cols}.
#'
#' @example
#' library(magrittr)
#' library(data.table)
#' library(dt.inflix)
#'
#'   dt <- data.table::data.table(a = c(1, 1, 1),
#'                                b = c(2, 2, 3))
#'
#'   dto <- dt %without% "b"
#'   print(dto)
#' @export %without%

`%without%` <- function(dt, cols = NULL){

dt_cols <- lapply(cols, function(x){
dt %>% names %include% x
}) %>% unlist %>% unique %>% stats::na.omit %>% as.vector

if (dt_cols %>% length > 0) dt[, `:=`(dt_cols %>% eval, NULL)]

return(invisible(dt))
}



## ---- withoutna
#' Convenience inflix operator to remove all(NA) or all(NULL) columns from a data.table by reference.
#'
#' Remove columns from a data table if all values are NA or NULL; useful when subsetting.
#'
#' @param dt A data.table.
#'
#' @return A data.table subset of \code{dt} with columns of all \code{NA} or \code{NULL} removed.
#'
#' @example
#' library(magrittr)
#' library(data.table)
#' library(dt.inflix)
#'
#'   dt <- data.table::data.table(a = c(1, 1, 1),
#'                                b = c(NA, NA, NA))
#'
#'   dt %>% withoutna
#'   print(dt)
#' @export withoutna

withoutna <- function(dt){

`.` <- NULL

na_col <- lapply(1:length(dt), function(x){
    if (all(is.na(dt[[x]])) || all(is.null(dt[[x]]))) return(dt %>% names %>% .[x])
    }) %>% unlist

if (na_col %>% length > 0) dt[, `:=`(na_col %>% eval, NULL)]

return(invisible(dt))
}



## ---- chunk
#' Chunk a data table to disk for parallel operations
#'
#' Write a data.table to disk using \code{data.table::fwrite} in \code{chunks} segments. If is often faster to write out large tabular data to disk then read in parallel vs. \code{foreach} or \code{mclapply}
#'
#' @param dt A data.table.
#' @param chunks Number of chunks.
#' @param col.names Logical. Write column names?
#' @param sep Character. Column separator.
#'
#' @example
#'\dontrun{
#' library(magrittr)
#' library(data.table)
#' library(dt.inflix)
#'
#'   dt <- data.table::data.table(a = c(1:1E7),
#'                                b = LETTERS[1:20])
#'
#'   dt %>% chunk
#'}
#' @export chunk

chunk <- function(dt, chunks = parallel::detectCores(),
                  col.names = TRUE,
                  sep = "\t"){

`.` <- N <- NULL

fn <- deparse(substitute(dt))

splits <- dt %>% split(1:chunks)

parallel::mclapply(splits %>% seq_along, function(x){
  data.table::fwrite(dt[splits[x]],
                   paste0(fn, "_", x, "_chunk.tsv"),
                   col.names = col.names,
                   sep = sep)
  return(NULL)
})

return(NULL)

}
