% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_urls_from_tweets.R
\name{nwd_extract_urls_from_tweets}
\alias{nwd_extract_urls_from_tweets}
\title{Extract links from tweets and expands shortened urls}
\usage{
nwd_extract_urls_from_tweets(
  tweets = NULL,
  language = NULL,
  date = NULL,
  expand_url = TRUE,
  n_char = 30,
  n_retry = 3
)
}
\arguments{
\item{tweets}{A data frame of tweets, as created by `rtweet`. If not given, it processes tweets available in `tweets` subfolder.}

\item{n_char}{An integer, defaults to 30. Number of characters in the url: only urls shorter than n_char will be expanded, others will be kept as they are.}

\item{n_retry}{An integer, defaults to 3. Number of times it tries to expand urls if first attempt fails. Set to 0 for attempting only once.}
}
\value{
Nothing, used for its side effects, i.e. stores expanded urls in `tweet_links` subfolder.
}
\description{
Extract links from tweets and expands shortened urls
}
\examples{

}
