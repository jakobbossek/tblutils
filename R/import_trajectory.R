#' @title Import trajectories of search heuristics.
#'
#' @description Given a set of files, the function reads all files, extracts rows
#' as desired, appends the file path as \dQuote{job.id} column and returns a \code{data.frame}.
#' As alternative, one can pass a vector of integer |code{ids}, a \code{path} and a file extension
#' \code{ext}. In this case the file paths are build given this information and the
#' \dQuote{job.id} corresponds to the ID.
#'
#' @details Internally, each file is by default imported with \code{\link[readr]{read_delim}}.
#' The variable length arguments \code{...} are passed down to this function.
#'
#' @param files [\code{character}]\cr
#'   Paths to files.
#'   May be \code{NULL} if \code{ids} is not \code{NULL}.
#' @param ids [\code{integer}]\cr
#'   Vector of job IDs.
#'   May be \code{NULL}. If not \code{NULL}, parameter \code{files} is ignored.
#' @param path [\code{character(1)}]|cr
#'   Directory containing the trajectory files. Only necessary if \code{ids} is
#'   not \code{NULL}.
#' @param ext [\code{character(1)}]\cr
#'   File extension of trajectory files. Only relevant if \code{ids} is not
#'   \code{NULL}.
#' @param filter.rows [\code{numeric}]\cr
#'   Which rows should be selected?
#'   Either relative by passing a sequence of values between zero and one or
#'   absolute by passing a sequence of integer values. In any case at least two
#'   values must be passed.
#'   Default is \code{NULL}.
#' @param reader.fun [\code{function(file, ...)}]\cr
#'   Function used to import each single file.
#'   Default is \code{\link[readr]{read_delim}}.
#'   The first argument needs to be \dQuote{file}.
#'   Remaining arguments may be passed down via \code{...} (see below).
#' @param ... [any]\cr
#'   Furhter arguments passed down to \code{reader.fun}.
#' @return [\code{data.frame}]
#' @export
import_trajectory = function(files = NULL, ids = NULL, path = NULL, ext = ".out", filter.rows = NULL, reader.fun = readr::read_delim, ...) {
  if (!is.null(files) && !is.null(ids))
    re::stopf("[import_trajectory] Either pass files or ids, but not both.")
  if (is.null(files) && is.null(ids))
    re::stopf("[import_trajectory] Either files or ids must be given.")
  checkmate::assert_character(files, min.len = 1L, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  checkmate::assert_integer(ids, lower = 1L, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  checkmate::assert_numeric(filter.rows, lower = 0, min.len = 2L, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  if (!is.null(ids)) {
    checkmate::assert_directory(path, access = "r")
    files = file.path(path, paste0(ids, ext))
  }

  n = length(files)
  do.call(rbind, lapply(seq_len(n), function(i) {
    f = files[i]
    tmp = reader.fun(f, ...)
    if (!is.null(filter.rows)) {
      row.ids = NA_integer_
      nrows = nrow(tmp)
      if (all(filter.rows >= 1)) {
        if (any(filter.rows > nrows)) {
          warning(sprintf("[import_trajectory] At least one value of filter.rows is larger than the number of rows in
            the trajectory in file '%s'%s.", f, if (is.null(ids)) "" else paste0(" (job.id=", ids[i], ")")))
        }
        row.ids = filter.rows
      } else {
        row.ids = ceiling(filter.rows * nrows)
        # if zero percent passed, we want the 1st row and  not the 0th
        if (0 %in% row.ids) {
          row.ids[which(row.ids == 0)] = row.ids[which(row.ids == 0)] + 1L
        }
      }
      tmp = tmp[row.ids, , drop = FALSE]
    }
    # add id column to be able to join with meta data table
    if (!is.null(ids))
      tmp$job.id = ids[i]
    else
      tmp$job.id = f
    return(tmp)
  }))
}
