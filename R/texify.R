texify = function(x,
  split.cols,
  widen.col,
  measure.cols,
  do.pairwise.test = TRUE,
  testresult.col = "testresult",
  alpha = 0.05,
  split.col.nice.names,
  widen.col.nice.names,
  highlighter.pars = NULL
  ) {
  checkmate::assert_data_frame(x)
  cns = colnames(x)

  n.measures = length(measure.cols)

  checkmate::assert_string(widen.col, min.chars = 1L)
  checkmate::assert_flag(do.pairwise.test)
  checkmate::assert_subset(split.cols, cns)
  checkmate::assert_subset(measure.cols, cns)

  if (!(widen.col %in% cns))
    re::stopf("[texify] There must be a column named 'widen.col=%s' in x.", widen.col)

  if (do.pairwise.test && !is.factor(x[[widen.col]]))
    re::stopf("[texify] Column widen.col must be a factor if pariwise tests should be done.")

  if (do.pairwise.test) {
    x[[testresult.col]] = NA_character_
    x = lapply(measure.cols, function(mc) {
      do_pairwise_test(x, by = split.cols, split.col = widen.col, test.col = mc, testresult.col = testresult.col, alpha = alpha)
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

    # highlight
    x[[i]] = highlight(x[[i]], by = split.cols, which = "mean.diversity", order.fun = "max", bg.color = "gray", bg.saturation.max = 15)

    x[[i]] = dplyr::arrange_(x[[i]], .dots = as.list(split.cols))
  }

  xxx <<- x[[1]]


  tbl = widen(x[[1]],
    split.col = widen.col,
    widen.cols = c("mean.diversity", "sd.diversity", "stat.diversity"))
  tbl[[widen.col]] = NULL

  return(tbl)
}

do_pairwise_test = function(x, by, split.col, test.col, testresult.col, alpha) {
  cns = colnames(x)

  # split first by parameters, e.g. (instnace, n, mu, ...)
  x.parts = get_parts(x, by)

  # now for each part, split by algorithms and do pairwise tests
  x.parts = lapply(x.parts, function(x.part) {
    # "integerize" split factor
    x.part$xxx = as.integer(x.part[[split.col]])
    n = re::nunique(x.part$xxx)
    y.parts = get_parts(x.part, by = split.col)
    print(y.parts)
    y.parts.names = names(y.parts)
    m = length(y.parts.names)
    alternatives = c()
    for (n1 in seq_len(m)) {
      for (n2 in seq_len(m)) {
        if (n1 == n2)
          next
        a = as.numeric(y.parts[[n1]][[test.col]])
        b = as.numeric(y.parts[[n2]][[test.col]])
        test.result = wilcox.test(a, b, alternative = "greater")
        if (test.result$p.value < alpha) {
          alternatives = c(alternatives, n2)
        }
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
