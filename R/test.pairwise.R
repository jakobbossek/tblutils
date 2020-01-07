#FIXME: this is a very ugly implementation
#' @title Appnds formatted results of pairwise tests to data frame
#'
#' @description Split by some columns, further split by \dQuote{split.col},
#' and do pairwise comparisons of the \code{value.col} column based on
#' a statistical test. Results are stored in \code{testresult.col}.
#'
#' @param x [\code{data.frame}]\cr
#'   Data frame.
#' @param by [\code{character}]\cr
#'   Column names used for splitting the data frame.
#'   This will usually be columns that describe the problems / problem parameters.
#' @param split.col [\code{character(1)}]\cr
#'   Single column name. This will usually be the algorithm column.
#' @param value.col [\code{character(1)}]\cr
#'   Single column name with numeric values to undergo statistical tests.
#' @param alpha [\code{numeric(1)}]\cr
#'   Significance level.
#'   Default is 0.05, i.e. five percent.
#' @param testresult.col [\code{character(1)}]\cr
#'   Column name of new column where test results should be stored.
#' @param test.fun [\code{function(x, y, ...)}]\cr
#'   Statistical test function.
#'   Default is \code{wilcox.test}.
#' @param ... [any]\cr
#'   Further parameters passed down to \code{test.fun}.
#'   E.g., \code{alternative = "less"}.
#' @return [\code{data.frame}]
#' @export
test.pairwise = function(x,
  by, split.col, value.col, testresult.col,
  alpha = 0.05, test.fun = stats::wilcox.test, ...) {
  checkmate::assertDataFrame(x)
  checkmate::assertCharacter(by, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertString(split.col)
  checkmate::assertString(value.col)
  checkmate::assertString(testresult.col)
  checkmate::assertNumber(alpha, lower = 0.0000001, upper = 0.9999999)
  checkmate::assertFunction(test.fun)

  #FIXME: sanity checks

  # initialize results column
  x[[testresult.col]] = NA_character_

  #FIXME: C&P -> write helper
  splits = lapply(by, function(b) x[[b]])
  x.parts = split(x, splits)
  # some combinations of by-values may be empty. Get rid of these!
  idx.nonempty = which(sapply(x.parts, nrow) != 0)
  x.parts = x.parts[idx.nonempty]

  x.parts = lapply(x.parts, function(part) {
    # now split by split.col
    part2 = split(part, part[[split.col]])
    idx.nonempty = which(sapply(part2, nrow) != 0)
    part2 = part2[idx.nonempty]

    part2.ns = names(part2)
    for (n1 in part2.ns) {
      alternatives = c()
      results = c()
      for (n2 in part2.ns) {
        if (n1 == n2) {
          next
        }
        vals1 = part2[[n1]][[value.col]]
        vals2 = part2[[n2]][[value.col]]
        test.result = test.fun(x = vals1, y = vals2, ...)
        alternatives = c(alternatives, n2)
        results = c(results, test.result$p.value < alpha)
      }
      alternatives = sprintf("$\\text{%s}^{%s}$", alternatives, ifelse(results, "+", "-"))
      alternatives = BBmisc::collapse(alternatives, sep = ", ")
      part2[[n1]][[testresult.col]] = rep(alternatives, nrow(part2[[n1]]))
    }
    do.call(rbind, part2)
  })
  x.parts = do.call(rbind, x.parts)
  return(x.parts)
}
