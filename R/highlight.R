#' @title Highlight cells in data frame
#'
#' @description Splits data frame by some columns, determines
#' ordering of certain values in column \dQuote{which} and
#' emphasizes the best values visually.
#'
#' @param x [\code{data.frame}]\cr
#'   Data frame.
#' @param by [\code{character}]\cr
#'   Column names used for splitting the data frame. Highlighting
#'   is performed for each split seperately.
#' @param which [\code{character(1)}]\cr
#'   Column name of the column which contains the values we want
#'   to highlight.
#' @param order.fun [\code{function(x, ...)}]\cr
#'   Function used to determine numeric ordering of the values \code{x[[which]]}.
#'   This function needs to return an integer vector of the same length
#'   as the input vector.
#'   Semantic: the lower an ordering value the better.
#' @param highlight.fun [\code{function}]\cr
#'   The function that does the highlight magic. It expects at least
#'.  a vector of numeric values \code{values} and a vector of corresponding order
#'   values \code{ordering}.
#'   See \code{\link{baseLaTeXHighlighter}} for a flexible highlighter function.
#' @param order.fun.args [\code{list}]\cr
#'   List of arguments passed down to \code{order.fun}.
#' @param ... [any]\cr
#'   Further parameters passed down to \code{highlight.fun}.
#' @return [\code{data.frame}]
#' @export
highlight = function(x, by, which, order.fun = base::order, highlight.fun, order.fun.args = list(), ...) {
  # sanity checks
  checkmate::assertDataFrame(x)
  checkmate::assertCharacter(by)
  checkmate::assertFunction(order.fun)
  checkmate::assertFunction(highlight.fun)

  # split data frame (using dplyr here is a pain in the a**, since
  # we need to pass down variables)
  splits = lapply(by, function(b) x[[b]])
  x.parts = split(x, splits)
  # some combinations of by-values may be empty. Get rid of these!
  idx.nonempty = which(sapply(x.parts, nrow) != 0)
  x.parts = x.parts[idx.nonempty]

  # now do the highlight magic
  x.parts = lapply(x.parts, function(part) {
    #FIXME: rename
    #print(part)
    # get values to highlight
    which.values = part[[which]]
    # determine ordering
    of.args = BBmisc::insert(list(x = which.values), order.fun.args)
    which.order = do.call(order.fun, of.args)
    # eventually based on ordering apply highlighting
    part[[which]] = highlight.fun(part[[which]], which.order, ...)
    return(part)
  })

  # bind and return
  x.parts = do.call(rbind, x.parts)
  return(x.parts)
}


#' @title Base LaTeX highlighter
#'
#' @description Highlight best values and optionally highlights the
#' \code{k} best values with background colors of different opacity.
#'
#' @param values [\code{numeric}]\cr
#'   Vector of numeric values.
#' @param ordering [\code{integer}]\cr
#'   Ordering of \code{values}. I.e., if \code{ordering[i] == 3}, then
#'   the i-th element is 3rd in the sorted sequence.
#' @param bg.color [\code{character(1)}]\cr
#'   Base color used for LaTeX table cellcolor.
#' @param bg.saturation.max [\code{character(1)}]\cr
#'   Maximal color saturation used for cellcolor.
#' @param bg.saturation.min [\code{character(1)}]\cr
#'   Minimal color saturation used for cellcolor.
#' @param n.highlight [\code{integer(1)}]\cr
#'   The \code{n.highlight} best -- according to \code{ordering} --
#'   values are highlighted with a background color.
#' @return [\code{character}] Modified \code{values} vector.
#' @export
baseLaTeXHighlighter = function(values, ordering, n.highlight = 1,
  bg.color = "gray", bg.saturation.max = 80, bg.saturation.min = 20) {
  n.values = length(values)
  # transform to nicely formated characters
  values = sprintf("%.2f", values)

  # highlight best value in bold
  max.val.idx = which(ordering == 1L)
  values[max.val.idx] = sprintf("\\textbf{%s}", values[max.val.idx])

  # Now for the cellcolors
  col.values = rep(bg.saturation.max, n.values)

  # ordering - 1 achieves no desaturation for best value
  desaturation.factor = (bg.saturation.max - bg.saturation.min) / n.highlight
  col.values = col.values - (ordering - 1L) * 20
  if (n.highlight > n.values)
    n.highlight = n.values

  # which elements shall be highlighted
  idx = which(ordering <= n.highlight)

  col.values = sprintf("%s!%i", bg.color, col.values)
  values[idx] = sprintf("\\cellcolor{%s}{%s}", col.values[idx], values[idx])
  return(values)
}
