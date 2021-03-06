% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_networks.R
\name{nwd_find_network}
\alias{nwd_find_network}
\title{Find domains related to each across key identifiers}
\usage{
nwd_find_network(
  domain,
  identifiers_df = nwd_load_identifiers_df(),
  identifiers = NULL,
  identifiers_to_exclude = default_excluded_df,
  language = NULL,
  max_run_n = 10
)
}
\arguments{
\item{identifiers_df}{A dataframe of indentifiers in the long format, typically created with `nwd_load_identifiers_df()`.}

\item{identifiers_to_exclude}{A dataframe with two columns, `type` and `id`. See `default_excluded_df` for an example.}

\item{language}{A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.}

\item{run_n}{Number of times to go through all identifiers. For example, if a new domain is found within the network through a common fb_app_id, it may be useful to see if the new domain has any ca_pub codes with others.}
}
\value{
Nothing, used for its side effects.
}
\description{
Find domains related to each across key identifiers
}
\examples{

}
