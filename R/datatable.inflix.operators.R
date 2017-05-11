## ---- allduplicated
#' Return all duplicated rows of a data.table.
#'
#' @param dt Data table.
#'
#' @import data.table
#' @export allduplicated

allduplicated <- function(dt, by = NULL)
{
if (by %>% is.null) {
  setkey(dt)
  dups <- duplicated(dt)
}
if (!by %>% is.null) {
  setkeyv(dt, cols = by)
  dups <- duplicated(dt, by = by)
  }

dt[, fD := dups | c(tail(dups, -1), FALSE)]
dt_dup <- dt[fD == TRUE]
dt[, fD := NULL]

return(dt_dup)

}



## ---- `%likef%`
#' Convenience inflix operator to return logical vector of elements matching fixed pattern.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @import stringi
#' @export %likef%

`%likef%` <- function(vector, pattern)
{
    if (is.factor(vector)) {
       lv <- as.integer(vector) %in% stringi::stri_detect_fixed(levels(vector), pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    else {
       lv <- stringi::stri_detect_fixed(vector, pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    return(lv)
}



## ---- `%include%`
#' Convenience inflix operator to return vector elements matching regexpr.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @import stringi
#' @export %include%

`%include%` <- function(vector, pattern)
{
    if (is.factor(vector)) {
       lv <- as.integer(vector) %in% stringi::stri_detect_regex(levels(vector), pattern, opts_regex = stringi::stri_opts_regex(case_insensitive = FALSE, comments = TRUE,  error_on_unknown_escapes = TRUE))
    }
    else {
       lv <- stringi::stri_detect_regex(vector, pattern, opts_regex = stringi::stri_opts_regex(
       case_insensitive = FALSE, comments = TRUE, error_on_unknown_escapes = TRUE))
    }
    return(vector[lv])
}



## ---- `%includef%`
#' Convenience inflix operator to return vector elements matching fixed pattern.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @import stringi
#' @export %includef%

`%includef%` <- function(vector, pattern)
{
    if (is.factor(vector)) {
        lv <- as.integer(vector) %in% stringi::stri_detect_fixed(levels(vector), pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    else {
        lv <- stringi::stri_detect_fixed(vector, pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    return(vector[lv])
}



## ---- `%exclude%`
#' Convenience inflix operator to return vector elements excluding regexpr.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @import stringi
#' @export %exclude%

`%exclude%` <- function(vector, pattern)
{
    if (is.factor(vector)) {

       lv <- as.integer(vector) %in% stringi::stri_detect_regex(levels(vector), pattern, opts_regex = stringi::stri_opts_regex(case_insensitive = FALSE, comments = TRUE,  error_on_unknown_escapes = TRUE))
    }
    else {
       lv <- stringi::stri_detect_regex(vector, pattern, opts_regex = stringi::stri_opts_regex(
       case_insensitive = FALSE, comments = TRUE, error_on_unknown_escapes = TRUE))
    }
    return(vector[!lv])
}



## ---- `%excludef%`
#' Convenience inflix operator to return vector elements excluding fixed pattern.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @import stringi
#' @export %excludef%

`%excludef%` <- function(vector, pattern)
{
    if (is.factor(vector)) {
        lv <- as.integer(vector) %in% stringi::stri_detect_fixed(levels(vector), pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    else {
        lv <- stringi::stri_detect_fixed(vector, pattern, opts_fixed = stringi::stri_opts_fixed())
    }
    return(vector[!lv])
}



## ---- `%withoutrows%`
#' Remove data.table without rows matching index.
#'
#' @param dt A data.table.
#' @param rows Row indicies to delete.
#'
#' @import data.table
#' @export %withoutrows%

`%withoutrows%` <- function(dt, rows) {

  if (rows %>% is.na %>% any | rows %>% is.null %>% any | !rows %>% is.integer) stop("Error in row indicies.")

  keep <- setdiff(dt[, .I], rows)
  cols = names(dt)
  dt.subset <- data.table::data.table(dt[[1]][keep])
  data.table::setnames(dt.subset, cols[1])
  for (col in cols[2:length(cols)]) {
    dt.subset[, (col) := dt[[col]][keep]]
    dt[, (col) := NULL]  # delete
  }
  # remove remaining dt col
  dt[, (dt %>% names) := NULL]

# assign new subset dt back to parent env
assign(deparse(substitute(dt)), dt.subset, envir =  parent.frame())
}



## ---- `%with%`
#' Return data.table with columns matching regexpr.
#'
#' @param dt A data.table.
#' @param cols Vector of column patterns to include.
#'
#' @import data.table
#' @import magrittr
#' @export %with%

`%with%` <- function(dt, cols){

dt_cols <- lapply(cols, function(x){
dt %>% names %include% x
}) %>% unlist %>% unique %>% na.omit %>% as.vector

if (dt_cols %>% length == 0) {
  stop("No columns selected.")
}
return(invisible(dt[, .SD, .SDcols = dt_cols]))

}



## ---- `%without%`
#' Remove columns from a data.table by reference.
#'
#' @param dt A data.table.
#' @param cols Vector of columns patterns to remove.
#'
#' @import data.table
#' @import magrittr
#' @export %without%

`%without%` <- function(dt, cols = NULL){

dt_cols <- lapply(cols, function(x){
dt %>% names %include% x
}) %>% unlist %>% unique %>% na.omit %>% as.vector

if (dt_cols %>% length > 0) dt[, `:=`(dt_cols %>% eval, NULL)]

return(invisible(dt))
}



## ---- withoutna
#' Remove NA/NULL columns from a datatable by reference
#'
#' Remove columns from a datatable if all values are NA or NULL; useful when subsetting.
#'
#' @param dt A data.table.
#'
#' @import data.table
#' @import magrittr
#' @export withoutna

`withoutna` <- function(dt){

na_col <- lapply(1:length(dt), function(x){
    if (all(is.na(dt[[x]])) || all(is.null(dt[[x]]))) return(dt %>% names %>% .[x])
    }) %>% unlist

if (na_col %>% length > 0) dt[, `:=`(na_col %>% eval, NULL)]

return(invisible(dt))
}



## ---- chunk
#' Chunk a datatable to disk for parallel operations
#'
#' @param dt A data.table.
#' @param col Column name to chunk by.
#' @param chunks Number of chunks.
#'
#' @import parallel
#' @import magrittr
#' @import data.table
#' @export chunk

chunk <- function(dt, col, chunks = detectCores()){

fn <- deparse(substitute(dt))
dt %>% setkeyv(col)

chunk_ids <- dt[, .N, by = get(col)][order(N)]$get %>% split(1:chunks)
chunkln <- chunk_ids %>% length

parallel::mclapply(1:length(chunk_ids), function(x){

fwrite(dt[get(col) %chin% chunk_ids[[x]]], paste0(fn, "_", x, "_chunk.tsv"), sep = "\t")

return(NULL)

}, mc.cores = chunks)  ## approximate max CPU use

return(NULL)
}