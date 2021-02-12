texify_helper = function(x, before, after) {
  checkmate::assertString(before)
  checkmate::assertString(after)
  paste0(before, as.character(x), after)
}

toTableStar = function(x, pos = "htbp") {
  if (!is.character(x))
    x = as.character(x)
  x = gsub("begin{table}", sprintf("begin{table*}[%s]", pos), x, fixed = TRUE)
  x = gsub("end{table}", "end{table*}", x, fixed = TRUE)
  return(x)
}

stretchTable = function(x, arraystretch = NULL, tabcolsep = NULL) {
  renewcommands = ""
  if (!is.null(arraystretch))
    renewcommands = paste0(renewcommands, "\\renewcommand{\\arraystretch}{" , arraystretch, "}")
  if (!is.null(arraystretch))
    renewcommands = paste0(renewcommands, "\\renewcommand{\\tabcolsep}{", tabcolsep, "}")
  #FIXME: check how to use backslash as a symbol in regex
  #gsub("\\\caption", paste0(renewcommands, "\caption"), x, fixed = TRUE)
}
