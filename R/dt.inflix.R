## ---- allduplicated
#' Return all duplicated rows of a data.table.
#'
#' @param dt Data table.
#' @param by Character vector. Names of columns to test for duplicates.
#'
#' @return A data.table of duplicated rows from \code{dt}.
#'
#' @export allduplicated
#' @import data.table
#' @import parallel
#' @import stringi
#' @import glue
#' @import future.apply
#' @import crayon
#' @import testthat
#' @importFrom magrittr %>% %T>% %$% %<>%
#' @importFrom Rdpack reprompt
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



## ---- coerce_dt
#' Recursively coerce a list to a flat data table, correctly naming columns.
#'
#' @param l List to coerce.
#' @param ix Numeric vector. Vector of nested indices to extract recursively.
#' @param names Logical. Preserve names?
#'
#' @return Data table with non-coercible objects elided.
#' @export coerce_dt
#'

coerce_dt <- function(l, ix = NULL, names = TRUE){

	lname <- ""

	# generate correct flat data table names if possible

		if (!ix %>% is.null)
			for (i in ix){

				iname <- try(l[i] %>% names %>% .[1])
				if (!iname %>% is.null &
		  		  iname %>% class == "character"){

		  			if (lname != "")
		  				lname <- paste0(lname, "_", iname)

		  			if (lname == "")
	  				lname <- iname
		  		}

					l <- l[[i]]

					}

				if (l %>% class %>% .[1] != "list")
					l <- list(l)


	if (!"list" %chin% (l %>% class))
		stop("l is not a list.")

	dt <- parallel::mclapply(1:length(l), function(i){

		print(i)

		# if a list, recurse

	  if (
	      (
	       (l[[i]] %>%
	       	       class %>%
	       	       .[1]) == "list") %>%
	    	any
	    	){

	  	# set name
	  		iname <- try(l[i] %>% names %>% .[1])

	  		if (!iname %>% is.null &
	  		    iname %>% class == "character"){

	  			if (lname != "")
	  				lname <- paste0(lname, "_", iname)

	  			if (lname == "")
	  				lname <- iname

	  		}

					return(l[[i]] %>% coerce_dt)
		  		}

		  #
	    dt <- try(l[[i]] %>% data.table::as.data.table(keep.rownames = TRUE))

	  	# set name
		    iname <- try(l[i] %>% names %>% .[1])

				if (dt %>% class %>% .[1] != "data.table" ||
				    dt %>% nrow == 0 ||
				    dt %>% length == 0)
						return(NULL)

		  		if (!iname %>% is.null &
		  		    iname %>% class == "character"){

		  			if (lname != "")
		  				lname <- paste0(lname, "_", iname)

		  			if (lname == "")
		  				lname <- iname

					}
				if (!names)
					dt %>%
						data.table::setnames(
               paste0(lname, "_", dt %>% names))

					return(dt)

  	}) %>%
			  rbindlist(use.names = TRUE, fill = TRUE)

			if (dt %>% class %>% .[1] != "data.table" ||
			    dt %>% nrow == 0 ||
			    dt %>% length == 0)
					return(NULL)

					dt %>% data.table::setnames(
					   dt %>%
					   names %>%
					   stringi::stri_replace_all_regex("__", "_") %>%
					   stringi::stri_replace_all_regex("(_|\\.)$", ""))
					return(dt)


}



## ---- `%likef%`
#' Convenience inflix operator to return logical vector of elements matching a fixed pattern.
#'
#' @param vector Vector.
#' @param pattern Pattern.
#'
#' @return A logical vector the same length as \code{vector} of elements matching \code{pattern}.
#'
#' @export %likef%
#' @md

`%likef%` <- function(vector, pattern){
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
#' @export %include%
#' @md

`%include%` <- function(vector, pattern){
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
#' @export %includef%
#' @md

`%includef%` <- function(vector, pattern){
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
#' @export %exclude%

`%exclude%` <- function(vector, pattern){
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
#' @export %excludef%

`%excludef%` <- function(vector, pattern){

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
#' @export %without%

`%without%` <- function(dt, cols = NULL){

dt_cols <- lapply(cols, function(x){
dt %>% names %include% x
}) %>% unlist %>% unique %>% stats::na.omit %>% as.vector

if (dt_cols %>% length > 0) dt[, `:=`(dt_cols %>% eval, NULL)]

return(invisible(dt))
}


## ---- chunk
#' Data.table::split but with aggregate "chunks".
#'
#' `chunk` performs `data.table::split` for a column and then uses `fun` on each chunk. The purpose of this function is to parallelize over `by` more efficiently when `by` >> `cl`.
#'
#' @param dt A data.table.
#' @param by Column to chunk by.
#' @param fun Monadic data table function to use on chunks. Function must return a data table of columns to add and `.id`, and index created by `chunk`.
#'
#'Example function:
#'```r
#'  .fn <- function(dt){
#'    print("start")
#'     dt[, uuid := full_name %>%
#'        stringr::str_extract("[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}")]
#'
#'    return(dt[, .SD, .SDcols = c("uuid", ".chunk_id")])
#'
#'  }
#'```r
#'
#' @param cl Number of chunks.
#'
#' @return `cl` data.table chunks with `by` split exclusively. The data table returned has new columns called `.id`, the default by parameter of non was specified, and .vec_chunk, the assignment in `cl`.
#'
#' @export chunk

chunk <- function(dt, fun, by = ".id", cl = parallel::detectCores()){

  if (!by %chin% (dt %>% names))
    dt[, .chunk_by, .I]

  dt[, .chunk_by := get(by)]

  dt[, .chunk_id := .I]

  # key to speed unique and join
  dt %>% data.table::setkey(.chunk_by)

  levels <- dt[, .chunk_by %>% unique]

  suppressWarnings(dt[, .vec_chunk := NULL])


  # divide levels of by into chunks of cl

  x <- data.table::data.table(
      .vec_chunk = rep(1:cl, (length(levels) /cl) %>% ceiling) %>%
          .[1:length(levels)],
      levels = levels,
      key = "levels") %>%
      data.table::setnames(c(".vec_chunk", ".chunk_by"))


 # join on a subset of dt to then add .vec_chunk by reference
 # to avoid large re-allocation for merged table
  y <- x[dt[, .SD, .SDcols = ".chunk_by"]]

  set(dt, j = ".vec_chunk", value = y[[".vec_chunk"]])


  glue::glue("Running {deparse(substitute(fun))}") %>%
  crayon::red %>%
  cat("\n")

  z <-  split(dt, by = ".vec_chunk") %>%
          future.apply::future_lapply(function(x){

              crayon::blue("...chunk") %>%
              cat("\n")
              x %<>% fun
              crayon::yellow("   done") %>%
              cat("\n")

              return(x)

              }) %>%
          data.table::rbindlist(use.names = TRUE, fill = TRUE)

  z %>% data.table::setkey(".chunk_id")
  dt %>% setkey(".chunk_id")

  for (i in z %>% names %exclude% "^\\.id$")

    set(dt, j = i, value = z[[i]])

  dt[, c(".chunk_id",
         ".vec_chunk",
         ".chunk_by") := NULL]


  return(invisible(dt))

}



## ---- withoutna
#' Convenience inflix operator to remove all(NA) or all(NULL) columns and rows from a data.table by reference.
#'
#' Remove columns from a data table if all values are NA or NULL; useful when subsetting.
#'
#' @param dt A data.table.
#'
#' @return A data.table subset of \code{dt} with columns of all \code{NA} or \code{NULL} removed.
#'
#' @export withoutna

withoutna <- function(dt){

`.` <- NULL

na_col <- lapply(1:length(dt), function(x){
    if (all(is.na(dt[[x]])) || all(is.null(dt[[x]]))) return(dt %>% names %>% .[x])
    }) %>% unlist

if (na_col %>% length > 0) dt[, `:=`(na_col %>% eval, NULL)]

dt[, .SD %>% is.na %>% all, by = 1:nrow(dt)]

n_col <- ncol(dt)
dt %<>% .[rowSums(is.na(dt)) != n_col, ]

return(invisible(dt))
}

