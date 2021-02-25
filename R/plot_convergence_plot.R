#' @title Convergence plot
#'
#' @description Line plot of trajectories. Given a data frame and values for
#' abszissa and ordinate the function returns a modifiable ggplot2 object.
#'
#' @param tbl [\code{data.frame}]\cr
#'   Data frame with input data.
#' @param x [\code{string}]\cr
#'   Column name of x-value.
#' @param y [\code{string}]\cr
#'   Column name of y-value.
#' @param color [\code{string}]\cr
#'   Column name of column used for coloring.
#' @param linetype [\code{string}]\cr
#'   Column name of column used for linetype. Defaults to \code{color}.
#' @param shape [\code{string}]\cr
#'   Column name of column used for shape. If not \code{NULL}, in addition to lines,
#'   points are plotted.
#' @param xlab [\code{string}]\cr
#'   x-axis label. Defaults to \code{x}.
#' @param ylab [\code{string}]\cr
#'   y-axis label. Defaults to \code{y}.
#' @param vlines.at [\code{numeric}]\cr
#'   If not \code{NULL}, indicates the positions of dashed gray vertical lines.
#' @param hlines.at [\code{numeric}]\cr
#'   If not \code{NULL}, indicates the positions of dashed gray horizontal lines.
#' @param x.logscale.base [\code{numeric(1)}]\cr
#'   Base used for log-scaled abszissa.
#' @param ... [any]\cr
#'  Not used.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @export
convergence_plot = function(
  tbl,
  x, y,
  color, linetype = color, shape = NULL,
  xlab = x, ylab = y,
  vlines.at = NULL,
  hlines.at = NULL,
  x.logscale.base = NULL,
  ...) {
  checkmate::assert_data_frame(tbl)
  cns = colnames(tbl)
  checkmate::assert_choice(x, cns)
  checkmate::assert_choice(y, cns)
  checkmate::assert_choice(color, cns, null.ok = TRUE)
  checkmate::assert_choice(linetype, cns, null.ok = TRUE)
  checkmate::assert_choice(shape, cns, null.ok = TRUE)
  if (is.null(color) && is.null(linetype))
    re::stopf("[convergence_plot] At least one of 'color' or 'linetype' must not be NULL.")
  checkmate::assert_string(xlab)
  checkmate::assert_string(ylab)
  checkmate::assert_numeric(hlines.at, null.ok = TRUE)
  checkmate::assert_numeric(vlines.at, null.ok = TRUE)
  checkmate::assert_number(x.logscale.base, lower = 2)

  g = ggplot2::ggplot(tbl, aes_string(x = x, y = y, color = color, linetype = linetype, shape = shape, group = color))
  if (!is.null(hlines.at))
    g = g + ggplot2::geom_vline(xintercept = vlines.at, color = "darkgray", linetype = "dashed")
  if (!is.null(vlines.at))
    g = g + ggplot2::geom_hline(yintercept = hlines.at, color = "darkgray", linetype = "dashed")
  g = g + ggplot2::geom_path(alpha = 0.6)
  if (!is.null(shape))
    g = g + ggplot2::geom_point()
  g = g + ggplot2::scale_color_brewer(palette = "Dark2")

  if (!is.null(x.logscale.base))
    g = g + scale_x_lognice(x.logscale.base)
  g = g + theme_jboss()
  g = g + labs(x = x, y = y)
  # g = g + ggplot2::guides(colour = guide_legend(nrow = 1))
  return(g)
}

scale_x_lognice = function(base = 10) {
  #FIXME: base not used
  ggplot2::scale_x_log10(breaks = trans_breaks(paste0("log", base), function(x) base^x), labels = trans_format("log10", math_format(10^.x)))
}

theme_jboss = function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 170,
  base_rect_size = base_size / 170) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size) %+replace%
    theme(
      legend.position = "top",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-8, -8, -8, -8),
      complete = TRUE
    )
}
