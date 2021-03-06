% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/archive_restore.R
\name{nwd_list_available_backups_on_google_drive}
\alias{nwd_list_available_backups_on_google_drive}
\title{List available backups on Google Drive}
\usage{
nwd_list_available_backups_on_google_drive(
  date = NULL,
  folder = "tweets",
  timeframe = "daily",
  language = NULL,
  filetype = "rds"
)
}
\arguments{
\item{language}{A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.}
}
\value{
A dribble, a data frame (tibble) of the 'googledrive' package including details on the file on Google Drive.
}
\description{
List available backups on Google Drive
}
\examples{

}
