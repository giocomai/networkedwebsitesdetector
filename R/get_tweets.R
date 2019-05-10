#' Searches keywords on Twitter. 
#' 
#' It assumes that a valid token is already present in the working environment. For details, see https://rtweet.info/articles/auth.html
#'
#' @param keywords Defaults to NULL. If NULL, tries to import previously stored keywords for given language and date. If a data frame, it assumes that the first columns includes the keywords. If a character vector, it processes it directly. 
#' @param n_tweets An integer. Number of tweets to request. 
#' @param date Used to find keywords locally, if none are provided. Defaults to current day. To get data for the previous day, use `Sys.Date()-1` 
#' @param tweet_type Passed to `rtweet`. Defaults to "recent". Other valid types include "mixed" and "popular".
#' @return A data.frame (a tibble) with `n` number of rows and two columns, `words` and `n` for number of occurrences.
#' @examples
#' 
#' @export


nwd_get_tweets <- function(keywords = NULL,
                           date = Sys.Date(),
                           language,
                           n_tweets = 1000,
                           tweet_type = "recent",
                           wait = 10, 
                           store = TRUE) {
  
  if (is.null(keywords)) {
    keywords_day_folder <- fs::path("keywords", 
                                    language,
                                    as.character(lubridate::year(date)),
                                    stringr::str_pad(lubridate::month(date), width = 2, pad = "0"),
                                    stringr::str_pad(lubridate::day(date), width = 2, pad = "0"))
    keywords <- readRDS(fs::dir_ls(keywords_day_folder))
  }
  
  if (is.element("data.frame", class(keywords))) {
    keywords <- keywords %>% dplyr::pull(1)
  }
  
  keywords <- unique(keywords)
  
  tweets_day_folder <- fs::path("tweets", 
                                language,
                                as.character(lubridate::year(date)),
                                stringr::str_pad(lubridate::month(date), width = 2, pad = "0"),
                                stringr::str_pad(lubridate::day(date), width = 2, pad = "0"))
  
  fs::dir_create(path = tweets_day_folder, recurse = TRUE)
  
  for (i in keywords) {
    tweets <- rtweet::search_tweets(q = i,
                                    n = n_tweets,
                                    lang = language,
                                    include_rts = FALSE,
                                    type = tweet_type)
    
    saveRDS(object = tweets,
            file = fs::path(tweets_day_folder, 
                            paste0(i, "-", language, "-", n_tweets, "-", tweet_type, "-", Sys.time(), ".rds")))
    
    Sys.sleep(time = wait)
  }
  
}
