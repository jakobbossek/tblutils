#' @title Split string/factor column.
#'
#' @description Given a data frame \code{x}, a column name and a naming
#' scheme, the function splits every element in \code{x[[split.col]]},
#' using the naming scheme. Next, either a data frame with column names
#' indicating the split parts for each unique value in \code{x[[split.col]]}
#' is returned, or, if \code{append=TRUE}, this data frame is left-joined
#' with \code{x} by \code{split.col}.
#'
#' @param x [\code{data.frame}]\cr
#'   Source data frame.
#' @param split.col [\code{string}]\cr
#'   Column name of column used for splitting.
#' @param scheme [\code{string}]\cr
#'   Single string encoding the names and types of the split parts.
#'   E.g. \dQuote{group{c}/problem{c}_n{i}_m{i}/algorithm{c}_pc{r}}.
#'   Passed down to \code{\link{decode_scheme}}.
#' @param delim [\code{character}]\cr
#'   Set of characters used to split the entries in the column.
#' @param append [\code{logical(1)}]\cr
#'   Should the splitted data be appended to \code{x} by \code{split.col}?
#'   Default is \code{FALSE}.
#' @return [\code{data.frame}]
#' @export
df_split_col = function(x, split.col, scheme, delim, append = TRUE) {
  uniqx = as.character(unique(x[[split.col]]))
  decoded = do.call(rbind, lapply(uniqx, function(e) {
    as.data.frame(decode_string(e, scheme, delim))
  }))
  id = data.frame(A = uniqx)
  colnames(id) = split.col
  decoded = cbind(id, decoded)
  if (!append)
    return(dplyr::as_tibble(decoded))
  dplyr::left_join(x, decoded, by = split.col)
}
