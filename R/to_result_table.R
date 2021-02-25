#' @title Generate data frame with aggregated data and statistical test results.
#'
#' @description Often we are given a large \code{data.frame} with results of
#' multiple (stochastic/randomized) algorithms on multiple instances with varying parameters.
#' One common way to present such results is to aggregate over runs, e.g. on instance
#' level, apply statistical singnificance tests to underpin observations and
#' eventually to present these ggregated results by means of result tables.
#' These result tables contain meta columns (e.g. problem, parameters) and each
#' at least one column for each algorithm.
#'
#' This function serves for aggregation and calculation of statistical tests.
#' The result is a list of aggregated result tables which may be passed on to
#' \code{\link{to_latex}}.
#'
#' @param x [\code{data.frame}]\cr
#'   Input data frame.
#' @param split.cols [\code{character}]\cr
#'   Column name of columns used to split the data. Usually this will be something
#'   like \dQuote{problem/instance}, \dQuote{parameter}.
#' @param widen.col [\code{character(1)}]\cr
#'   Column used to widen the table, convert long to wide format. Usually, this is
#'   the \dQuote{algorithm}.
#' @param measure.cols [\code{character}]\cr
#'   Which columns contain the actual measurements (e.g. performance values, number
#'   of iterations etc.). If multiple are provided and \code{do.pairwise.test} is
#'   \code{TRUE}, the data is additionally split by \code{measure.cols}.
#' @param do.pairwise.test [\code{logical(1)}]\cr
#'   Should pairwise test of measurements be performed?
#'   Default is \code{TRUE}.
#' @param test.alternative [\code{character}]\cr
#'   Alternatives, i.e. \eqn{H_1}-hypothesis, for statistical tests.
#' @param testresult.col [\code{character}]\cr
#'   Column name where test result shall be stored.
#'   Default is \dQuote{testresult}.
#' @param alpha [\code{numeric(1)}]\cr
#'   Significance level for statistical tests. Default to \code{0.05}.
#' @param stats.formatter.args [\code{list of lists}]\cr
#'   Named list (names are aggregated measures) of arguments used to format the
#'   aggregated measurement.
#' @param testresult.formatter.args [\code{list}]\cr
#'   Named list of parameters passed down to LaTeX-formatter for test results.
#' @param highlighter.pars [\code{list}]\cr
#'   Named list of arguments for highlighter, i.e. function that is used to
#'   emphasize certain results in tables (e.g. the minimal mean value on each
#'   instance).
#' @return [\code{list}] List of aggregated data frames.
#' @export
to_result_table = function(x,
  split.cols,
  widen.col,
  measure.cols,
  do.pairwise.test = TRUE,
  test.alternative,
  testresult.col = "testresult",
  alpha = 0.05,
  stats.formatter.args = list(sd = list(digits = 2L)),
  testresult.formatter.args = list(positive.only = TRUE),
  highlighter.args = NULL
  ) {
  checkmate::assert_data_frame(x)
  cns = colnames(x)

  n.measures = length(measure.cols)

  checkmate::assert_string(widen.col, min.chars = 1L)
  checkmate::assert_flag(do.pairwise.test)
  checkmate::assert_subset(split.cols, cns)
  checkmate::assert_subset(measure.cols, cns)

  if (!(widen.col %in% cns))
    re::stopf("[to_result_table] There must be a column named 'widen.col=%s' in x.", widen.col)

  if (do.pairwise.test && !is.factor(x[[widen.col]]))
    re::stopf("[to_result_table] Column widen.col must be a factor if pariwise tests should be done.")

  if (do.pairwise.test) {
    x[[testresult.col]] = NA_character_
    x = lapply(measure.cols, function(mc) {
      do_pairwise_test(x, by = split.cols, split.col = widen.col, test.col = mc, testresult.col = testresult.col, alternative = test.alternative[mc], alpha = alpha)
    })
  }

  # now aggregate
  for (i in seq_len(n.measures)) {
    m.name = measure.cols[i]
    #FIXME: write helper
    #FIXME: make parameterizable, e.g., median (not mean) and IQR (not sd)
    summ_methods = c(paste0('mean(', m.name, ')'), paste0('sd(', m.name, ')'), paste0(testresult.col, "[1L]")) # summary method
    summ_names = c(paste0('mean.', m.name), paste0('sd.', m.name), paste0('stat.', m.name))

    # aggregate
    x[[i]] = x[[i]] %>%
      group_by_(.dots = as.list(c(split.cols, widen.col))) %>%
      dplyr::summarise_(.dots = setNames(summ_methods, summ_names)) %>%
      ungroup()

    # format test results
    fargs = c(list(x = x[[i]][[paste0('stat.', m.name)]]), testresult.formatter.args)
    x[[i]][[paste0('stat.', m.name)]] = do.call(format_test_results, fargs)

    # format summary statistics
    stat.formatter.ns = names(stats.formatter.args)
    for (name in stat.formatter.ns) {
      fargs = c(list(x[[i]][[paste0(name, ".", m.name)]]), stats.formatter.args[[name]])
      x[[i]][[paste0(name, ".", m.name)]] = do.call(round, fargs)
    }

    # highlight
    if (!is.null(highlighter.args)) {
      fargs = c(list(x[[i]], by = split.cols, which = summ_names[1L]), highlighter.args)
      x[[i]] = do.call(highlight, fargs)
    }

    x[[i]] = dplyr::arrange_(x[[i]], .dots = as.list(split.cols))

    x[[i]] = widen(x[[i]],
      split.col = widen.col,
      widen.cols = summ_names)
    x[[i]][[widen.col]] = NULL
  }

  return(x)
}


#' @title Convert result table into basic kable LaTeX-table.
#'
#' @description Used to generate a basic result table object based on results
#' of \code{to_result_table}.
#'
#' @param tbl [|code{data.frame}]\cr
#'   Input data frame (usually result of \code{\link{to_result_table}}).
#' @param param.col.names [\code{character}]\cr
#'   Names of parameter columns used for table header. Parameter column names
#'   are supposed to be the first \eqn{k} column in \code{tbl}.
#' @param measure.col.names [\code{character}]\cr
#'   Names of measurement columns. These are repeated for each algorithm (see
#'   \code{algo.names}).
#' @param reps [\code{integer(1)}]\cr
#'   How often should measurement columns be repeated? Defaults to 1.
#'   This is useful if results for multiple performance measures should be displayed
#'   side by side in the same table.
#' @param algo.names [\code{character}]\cr
#'   Names of algorithms. Used to group measurement columns. Note that algorithms
#'   are numbered by their index.
#' @param algo.colors [\code{character}]\cr
#'   Vector of color names used to color-code algorithms for better visual
#'   interpretation.
#' @param caption [\code{character(1)}]\cr
#'   LaTeX-table caption.
#' @param longtable [|code{logical(1)}]\cr
#'   Should the table be used in a longtable environment?
#'   Default is \code{FALSE}.
#' @return [character(1)] A character vector of the table source code.
#' @export
to_latex = function(
  tbl,
  param.col.names,
  measure.col.names,
  reps = 1L,
  algo.names,
  algo.colors = NULL,
  caption = "Placeholder",
  longtable = FALSE) {
  checkmate::assert_data_frame(tbl, min.rows = 2L)
  checkmate::assert_character(param.col.names, any.missing = FALSE, all.missing = FALSE, min.len = 1L)
  checkmate::assert_character(measure.col.names, any.missing = FALSE, all.missing = FALSE, min.len = 1L)
  checkmate::assert_count(reps, positive = TRUE)
  checkmate::assert_character(algo.colors, min.len = length(algo.names), null.ok = TRUE)
  checkmate::assert_string(caption)
  checkmate::assert_flag(longtable)

  nr = nrow(tbl)
  nc = ncol(tbl)
  n.params = length(param.col.names)
  n.algos = length(algo.names)
  n.measures = length(measure.col.names)

  if (nr > 50 && !longtable)
    re::catf("[to_latex] Number of rows is quite high: %i. Consider using a longtable.", nr)

  cns = c(param.col.names, rep(measure.col.names, n.algos * reps))
  if (is.null(algo.colors)) {
    algo.names = rep(paste0(algo.names, " (", seq_len(n.algos), ")"), reps)
  } else {
    algo.ids = seq_len(n.algos)
    algo.ids = sprintf(" (\\\\textcolor{%s}{%i})", algo.colors[algo.ids], algo.ids)
    algo.names = rep(paste0(algo.names, algo.ids), reps)
  }

  header = c(rep(" ", n.params), setNames(rep(n.measures, n.algos * reps), algo.names))
  # if (nc != length(header))
  #   re::stopf("[to_latex] Number of columns (%i) and length of header (%i) do not match.", nc, length(header))

  align = rep("r", length(cns))
  ktbl = kableExtra::kable(tbl, "latex", col.names = cns, align = align, longtable = longtable, escape = FALSE, booktabs = TRUE, caption = caption) %>%
    kable_styling() %>%
    add_header_above(header, bold = TRUE, escape = FALSE)
  return(ktbl)
}

do_pairwise_test = function(x, by, split.col, test.col, testresult.col, alternative, alpha) {
  cns = colnames(x)

  # split first by parameters, e.g. (instnace, n, mu, ...)
  x.parts = get_parts(x, by)

  # now for each part, split by algorithms and do pairwise tests
  x.parts = lapply(x.parts, function(x.part) {
    # "integerize" split factor
    x.part$xxx = as.integer(x.part[[split.col]])
    n = re::nunique(x.part$xxx)
    y.parts = get_parts(x.part, by = split.col)
    #print(y.parts)
    y.parts.names = names(y.parts)
    m = length(y.parts.names)
    for (n1 in seq_len(m)) {
      alternatives = c()
      for (n2 in seq_len(m)) {
        if (n1 == n2)
          next
        a = as.numeric(y.parts[[n1]][[test.col]])
        b = as.numeric(y.parts[[n2]][[test.col]])
        test.result.dir1 = wilcox.test(a, b, alternative = alternative)
        test.result.dir2 = wilcox.test(b, a, alternative = alternative)
        direction = NA
        if (test.result.dir1$p.value < alpha && test.result.dir2$p.value < alpha)
          direction = "*"
        else if (test.result.dir1$p.value < alpha)
          direction = "+"
        else
          direction = "-"
        alternatives = c(alternatives, sprintf("%i%s", n2, direction))
      }
      y.parts[[n1]][[testresult.col]] = rep(BBmisc::collapse(alternatives, sep = ","), nrow(y.parts[[n1]]))
    }
    tmpres = do.call(rbind, y.parts)
  })
  x.parts = do.call(rbind, x.parts)
  x.parts$xxx = NULL
  return(x.parts)
}

get_parts = function(x, by) {
  splits = lapply(by, function(b) x[[b]])
  x.parts = split(x, splits)
  # some combinations of by-values may be empty. Get rid of these!
  idx.nonempty = which(sapply(x.parts, nrow) != 0)
  x.parts = x.parts[idx.nonempty]
  #print(x.parts)
  return(x.parts)
}

