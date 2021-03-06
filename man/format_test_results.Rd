% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatter.R
\name{format_test_results}
\alias{format_test_results}
\title{Formatter for test results in tables.}
\usage{
format_test_results(x, positive.only = FALSE, interval = FALSE,
  colors = NULL, use.xcolor = FALSE, ...)
}
\arguments{
\item{x}{[\code{character}]\cr
Character vector with encoded test results (e.g. \code{c("1+,2+,4-,6+")}).}

\item{positive.only}{[\code{logical(1)}]\cr
Show only positive test results?
Default is \code{FALSE}.}

\item{interval}{[\code{logical(1)}]\cr
Use interval notation? If \code{TRUE}, e.g. \code{c("1+,2+,3+,4+")} is formatted to \code{"[1-4]+"}.
Default is \code{FALSE}.}

\item{colors}{[\code{character} | \code{NULL}]\cr
Vector of colors used to highlight algorithm IDs via LaTeX textcolor. If not \code{NULL}
must provide at least as many colors as there are algorithms.
Default is \code{NULL}.}

\item{...}{[any]\cr
Not used at the moment.}

\item{use.xcolor[\code{logical(1)}]\cr}{Use xcolor palette? If \code{TRUE}, \code{colors} is set to \code{c("blue", "brown", "olive", "orange", "purple", "teal", "violet")}.
Default is \code{FALSE}.}
}
\value{
[\code{character}] Vector with formatted test results.
}
\description{
...
}
