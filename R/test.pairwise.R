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
#' @param show.positive.only [\code{logical(1)}]\cr
#'   Show entries for rejected zero hypothesis only?
#'   Default is \code{FALSE}.
#' @param show.numbers.only [\code{logical(1)}]\cr
#'   Show algorithm number only, i.e., without plus/minus indicator for test
#'   result? The value \code{TRUE} is only meaningful
#'   if \code{show.positive.only = TRUE}.
#'   Default is \code{FALSE}.
#' @param colors [\code{character}]\cr
#'   Vector of colors used to highlight test results. If one color is passed it
#'   is simply repeated k times where k is the number of algorithms. Otherwise
#'   the k-th color is used for latex output if the zero hypothesis is rejected
#'   for this algorithm.
#' @param show.intervals [\code{logical(1)}]\cr
#'   Show results in intervals when possible and not comma-seperated?
#'   This might lead to narrower columns sometimes.
#'   Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Further parameters passed down to \code{test.fun}.
#'   E.g., \code{alternative = "less"}.
#' @return [\code{data.frame}]
#' @export
test.pairwise = function(x,
  by, split.col, value.col, testresult.col,
  show.positive.only = FALSE,
  show.numbers.only = FALSE,
  colors = "black",
  show.intervals = FALSE,
  alpha = 0.05, test.fun = stats::wilcox.test, ...) {
  checkmate::assertDataFrame(x)
  checkmate::assertCharacter(by, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertString(split.col)
  checkmate::assertString(value.col)
  checkmate::assertString(testresult.col)
  checkmate::assertLogical(show.positive.only)
  checkmate::assertLogical(show.numbers.only)
  checkmate::assertLogical(show.intervals)
  checkmate::assertNumber(alpha, lower = 0.0000001, upper = 0.9999999)
  checkmate::assertFunction(test.fun)

  if (!show.positive.only & show.numbers.only) {
    BBmisc::stopf("[test.pairwise] show.positive.only = FALSE and show.numbers.only = TRUE does not allow to distinguish test results.")
  }

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
    n.part2 = length(part2.ns)
    if (length(colors) != n.part2) {
      if (length(colors) == 1)
        colors = rep(colors, n.part2)
      else
        BBmisc::stopf("[test.pairwise] There are %i algorithms, but only %i colors passed.", n.part2, length(colors))
    }
    for (i in seq_len(n.part2)) {
      n1 = part2.ns[i]
      alternatives = c()
      results = c()
      for (j in seq_len(n.part2)) {
        n2 = part2.ns[j]
        if (n1 == n2) {
          next
        }
        vals1 = part2[[n1]][[value.col]]
        vals2 = part2[[n2]][[value.col]]
        test.result = test.fun(x = vals1, y = vals2, ...)
        alternatives = c(alternatives, n2)
        results = c(results, test.result$p.value < alpha)
      }
      if (show.intervals) {
        altpos = alternatives[results]
        altneg = alternatives[!results]
        #print(altpos)
        #print(altneg)
        alternatives = ""
        if (length(altpos) > 0) {
          alternatives = buildIntervalString(as.integer(altpos))
          stopifnot(length(alternatives) == 1L)
          if (!show.numbers.only) {
            if (nchar(alternatives) > 1L) {
              alternatives = sprintf("$[\\text{%s}]^{+}$", alternatives)
            } else {
              alternatives = sprintf("$\\text{%s}^{+}$", alternatives)
            }
          }
        }
        if (length(altneg) > 0 & !show.positive.only) {
          interval.string.neg = buildIntervalString(as.integer(altneg))
          if (!show.numbers.only) {
            if (nchar(interval.string.neg) > 1L) {
              interval.string.neg = sprintf("$[\\text{%s}]^{+}$", interval.string.neg)
            } else {
              interval.string.neg = sprintf("$\\text{%s}^{+}$", interval.string.neg)
            }
          }
          alternatives = sprintf("%s%s%s", interval.string.neg, if(alternatives == "") "" else ",", alternatives)
        }
        #print(alternatives)
        #stop()
      } else {
        if (show.numbers.only) {
          alternatives = sprintf("$\\text{%s}$", alternatives)
        } else {
          alternatives = sprintf("$\\text{%s}^{%s}$", alternatives, ifelse(results, "+", "-"))
        }
        # assign colors
        the.colors = colors[-i] # all but color of the first algorithm in this iteration
        alternatives[results] = sprintf("\\textcolor{%s}{%s}", the.colors[results], alternatives[results])

        # show positive and negative results?
        if (show.positive.only) {
          alternatives = alternatives[results]
        }
        alternatives = BBmisc::collapse(alternatives, sep = ", ")
      }
      part2[[n1]][[testresult.col]] = rep(alternatives, nrow(part2[[n1]]))
    }
    do.call(rbind, part2)
  })
  x.parts = do.call(rbind, x.parts)
  return(x.parts)
}

buildIntervalString = function(x) {
  #x = sort(x)
  st = ""
  last.pos = -100
  inInterval = FALSE
  for (i in 1:length(x)) {
    curr.pos = x[i]
    if (last.pos < 0) {
      st = paste0(st, curr.pos)
    } else if(((curr.pos - 1L) == last.pos) & !inInterval) {
      inInterval = TRUE
      st = paste0(st, "-") # interval begins
    } else if(((curr.pos - 1L) == last.pos) & inInterval) {
    } else if (inInterval & ((curr.pos - 1L) != last.pos)) {
      inInterval = FALSE
      st = paste0(st, last.pos, ",", curr.pos)
    } else {
      st = paste0(st, ",", curr.pos)
    }
    last.pos = curr.pos
  }
  if (inInterval) {
    st = paste0(st, last.pos)
  }
  return(st)
}
