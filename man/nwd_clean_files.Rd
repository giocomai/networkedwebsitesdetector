% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{nwd_clean_files}
\alias{nwd_clean_files}
\title{Check if there are unusually small or unusually big files}
\usage{
nwd_clean_files(
  min_size = 0,
  max_size = 1e+08,
  remove_exceeding = FALSE,
  language = NULL
)
}
\arguments{
\item{min_size}{Minimum size in bytes, defaults to 0 (only files of size 0 are selected).}

\item{max_size}{Maximum size in bytes, defaults to about 100 megabytes.}

\item{remove_exceeding}{Logical, defaults to FALSE. If TRUE, listed files are removed.}
}
\value{
If remove_exceeding==FALSE, returns a data frame with reference to files exceeding given criteria.
}
\description{
Check if there are unusually small or unusually big files
}
\examples{

}
