#' @title Preview of LaTeX tables.
#'
#' @param ... [any]\cr
#'   list of kable tables.
#' @return [invisible(NULL)]
preview = function(...) {
  tbls = list(...)
  brew.file = system.file("report.brew", package = "tblutils")
  if (brew.file == "") {
    brew.file = "/Users/bossek/repos/software/r/tblutils/data-raw/report.brew"
  }
  BBmisc::catf("Template file path is '%s'.", brew.file)
  tex.file = tempfile(fileext = ".tex")
  brew::brew(brew.file, tex.file)
  dir.create("temptexoutput")
  #file.copy(tex.file, paste0("temptexoutput/", basename(tex.file)))
  system2("pdflatex", args = list("--enable-write18", "-output-directory temptexoutput", tex.file))
  system2("pdflatex", args = list("--enable-write18", "-output-directory temptexoutput", tex.file))
  pdf.file = paste0("temptexoutput/", gsub(".tex", ".pdf", basename(tex.file), fixed = TRUE))
  on.exit(unlink(c(rds.file, tex.file)))
  system2("open", args = list(pdf.file))
  return(invisible(NULL))
}
