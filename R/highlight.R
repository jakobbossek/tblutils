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
#' #FIXME: either function or character(1)
#'   Function used to determine numeric ordering of the values \code{x[[which]]}.
#'   This function needs to return an integer vector of the same length
#'   as the input vector.
#'   Semantic: the lower an ordering value the better.
#' @param highlight.fun [\code{function}]\cr
#'   The function that does the highlight magic. It expects at least
#'.  a vector of numeric values \code{values} and a vector of corresponding order
#'   values \code{ordering}.
#'   See \code{\link{baseLaTeXHighlighter}} for a flexible highlighter function.
#' @param ... [any]\cr
#'   Further parameters passed down to \code{highlight.fun}.
#' @return [\code{data.frame}]
#' @export
highlight = function(x,
  by, which,
  order.fun = "min",
  highlight.fun = tblutils::baseLaTeXHighlighter,
  ...) {
  # sanity checks
  checkmate::assertDataFrame(x)
  checkmate::assertCharacter(by)

  rankinv = function(x) {
    tmp = rank(x, ties.method = "first")
    max(tmp) - tmp + 1L
  }

  if (is.character(order.fun)) {
    #checkmate::assertChoice(order.fun, choices = c("min", "max"))
    order.fun = c("min" = base::rank, "max" = rankinv)[order.fun]
  }

  if (length(which) > 1 & length(order.fun) == 1L)
    BBmisc::stopf("[highlight] If you want to highlight several columns you need to pass several order.funs.")

  #checkmate::assertFunction(order.fun)
  checkmate::assertFunction(highlight.fun)

  # split data frame (using dplyr here is a pain in the a**, since
  # we need to pass down variables)
  for (i in 1:length(which)) {
    BBmisc::catf("Highlighting columns: %s", which[i])
    which2 = which[i]
    order.fun2 = order.fun[[i]]

    splits = lapply(by, function(b) x[[b]])
    x.parts = split(x, splits)
    # some combinations of by-values may be empty. Get rid of these!
    idx.nonempty = which(sapply(x.parts, nrow) != 0)
    x.parts = x.parts[idx.nonempty]

    # now do the highlight magic
    x.parts = lapply(x.parts, function(part) {
      # get values to highlight
      which.values = part[[which2]]
      # determine ordering
      which.order = order.fun2(x = which.values)
      # eventually based on ordering apply highlighting
      part[[which2]] = highlight.fun(which.values, which.order, ...)
      return(part)
    })

    # bind and return
    x = do.call(rbind, x.parts)
  }
  return(x)
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
  bg.color = "gray", bg.saturation.max = 80, bg.saturation.min = 20, digits = 2L) {
  n.values = length(values)
  # transform to nicely formated characters
  values = sprintf(paste0("%.", digits, "f"), values)

  # highlight best value in bold
  max.val.idx = which(ordering == min(ordering))
  values[max.val.idx] = sprintf("\\textbf{%s}", values[max.val.idx])

  # Now for the cellcolors
  col.values = rep(bg.saturation.max, n.values)

  # ordering - 1 achieves no desaturation for best value
  desaturation.factor = ceiling((bg.saturation.max - bg.saturation.min) / n.highlight)
  # FIXME: magic number 20
  col.values = col.values - (ordering - 1L) * 20
  if (n.highlight > n.values)
    n.highlight = n.values

  # which elements shall be highlighted
  idx = which(ordering <= n.highlight)

  col.values = sprintf("%s!%i", bg.color, col.values)
  values[idx] = sprintf("\\cellcolor{%s}{%s}", col.values[idx], values[idx])
  return(values)
}
