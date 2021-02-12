format_test_results = function(x, positive.only = FALSE, interval = FALSE, colors = NULL, use.xcolor = FALSE, ...) {
  add_exponent = function(x, e, interval) {
    sapply(x, function(xp) {
      if (interval) {
        if (nchar(xp) == 0)
          return("")
        else if (nchar(xp) == 1L)
          return(paste0("$", xp, "^{", e, "}$"))
        else
          return(paste0("$[", xp, "]^{", e, "}$"))
      } else {
        paste0("$", xp, "^{", e, "}$")
      }
    })
  }

  colorize = function(x, colors) {
    if (length(x) == 0)
      return("")
    paste0("\\textcolor{", colors[x], "}{", x, "}")
  }

  format_single_test_result = function(x, ...) {
    # decode 1-,2+,3+,...
    expl = strsplit(x, split = ",")[[1L]]
    h0 = as.integer(gsub("-", "", expl[grepl("-", expl, fixed = TRUE)], fixed = TRUE))
    h1 = as.integer(gsub("+", "", expl[grepl("+", expl, fixed = TRUE)], fixed = TRUE))

    # do we want intervals?
    if (interval) {
      h0 = add_exponent(buildIntervalString(h0), "-", TRUE)
      h1 = add_exponent(buildIntervalString(h1), "+", TRUE)
    } else {
      if (!is.null(colors)) {
        h0 = colorize(h0, colors)
        h1 = colorize(h1, colors)
        h0 = h0[which(nchar(h0) > 0L)]
        h1 = h1[which(nchar(h1) > 0L)]
      }
      h0 = add_exponent(h0, "-", FALSE)
      h1 = add_exponent(h1, "+", FALSE)
    }

    h0 = h0[which(nchar(h0) > 0L)]
    h1 = h1[which(nchar(h1) > 0L)]

    if (positive.only) {
      return(BBmisc::collapse(h1, sep = ","))
    }
    return(BBmisc::collapse(c(h0,h1), sep = ","))
  }

  if (use.xcolor && !is.null(colors))
    re::stopf("[format_test_results] Arguments use.xcolor and colors must not be used together.")

  if (use.xcolor) {
    colors = c("blue", "brown", "olive", "orange", "purple", "teal", "violet")
  }

  if (!is.null(colors) && interval)
    re::stopf("[format_test_results] Colors and interval mode are incomptible.")

  sapply(x, format_single_test_result, ..., USE.NAMES = FALSE)
}
