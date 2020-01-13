#' @title Widen data frame
#'
#' @description Split a data frame by a factor or character
#' column and piece together a wide data frame by arranging
#' socalled widening columns side by side.
#'
#' @param x [\code{data.frame}]\cr
#'   Data frame.
#' @param split.col [\code{character(1)}]\cr
#'   Column name of column used for splitting.
#' @param widen.cols [\code{character}]\cr
#'   Column name(s) of column(s) which shall be aligned beside
#'   each other.
#' @return [\code{data.frame}]
#' @export
widen = function(x, split.col, widen.cols) {
  checkmate::assertDataFrame(x)
  checkmate::assertString(split.col)

  cns = colnames(x)
  if (!(split.col %in% cns))
    BBmisc::stopf("[widen] Argument split.col must be a valid column name of x.")

  if (!BBmisc::isSubset(widen.cols, cns))
    BBmisc::stopf("[widen] There are no column names %s in x.",
      BBmisc::collapse(setdiff(widen.cols, cns), sep = " and "))

  split.values = unique(x[[split.col]])
  n.values = length(split.values)

  x.parts = lapply(seq_len(n.values), function(i) {
    # split data frame in pieces by split.col(umn)
    split.value = split.values[i]
    idx = (x[[split.col]] == split.value)
    # select part which shall be widened
    x.part = x[idx, , drop = FALSE]
    if (i == 1) {
      # assure potential reordering given by widen.cols
      ids.nonwiden = setdiff(colnames(x.part), widen.cols)
      x.common = x.part[, ids.nonwiden, drop = FALSE]
      x.part = cbind(x.common, x.part[, widen.cols, drop = FALSE])
    }
    if (i >= 2) {
      x.part = x.part[, widen.cols, drop = FALSE]
      colnames(x.part) = paste0(widen.cols, i)
    }
    return(x.part)
  })

  return(do.call(cbind, x.parts))
}
