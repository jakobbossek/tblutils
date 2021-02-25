ggsave2 = function(filename, plot, ...) {
  args = list(filename = filename, plot = plot, limitsize = FALSE, device = cairo_pdf)
  args = BBmisc::insert(args, list(...))
  do.call(ggplot2::ggsave, args)
}
