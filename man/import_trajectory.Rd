% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_trajectory.R
\name{import_trajectory}
\alias{import_trajectory}
\title{Import trajectories of search heuristics.}
\usage{
import_trajectory(files = NULL, ids = NULL, path = NULL,
  ext = ".out", filter.rows = NULL, reader.fun = readr::read_delim,
  ...)
}
\arguments{
\item{files}{[\code{character}]\cr
Paths to files.
May be \code{NULL} if \code{ids} is not \code{NULL}.}

\item{ids}{[\code{integer}]\cr
Vector of job IDs.
May be \code{NULL}. If not \code{NULL}, parameter \code{files} is ignored.}

\item{path}{[\code{character(1)}]|cr
Directory containing the trajectory files. Only necessary if \code{ids} is
not \code{NULL}.}

\item{ext}{[\code{character(1)}]\cr
File extension of trajectory files. Only relevant if \code{ids} is not
\code{NULL}.}

\item{filter.rows}{[\code{numeric}]\cr
Which rows should be selected?
Either relative by passing a sequence of values between zero and one or
absolute by passing a sequence of integer values. In any case at least two
values must be passed.
Default is \code{NULL}.}

\item{reader.fun}{[\code{function(file, ...)}]\cr
Function used to import each single file.
Default is \code{\link[readr]{read_delim}}.
The first argument needs to be \dQuote{file}.
Remaining arguments may be passed down via \code{...} (see below).}

\item{...}{[any]\cr
Furhter arguments passed down to \code{reader.fun}.}
}
\value{
[\code{data.frame}]
}
\description{
Given a set of files, the function reads all files, extracts rows
as desired, appends the file path as \dQuote{job.id} column and returns a \code{data.frame}.
As alternative, one can pass a vector of integer |code{ids}, a \code{path} and a file extension
\code{ext}. In this case the file paths are build given this information and the
\dQuote{job.id} corresponds to the ID.
}
\details{
Internally, each file is by default imported with \code{\link[readr]{read_delim}}.
The variable length arguments \code{...} are passed down to this function.
}
