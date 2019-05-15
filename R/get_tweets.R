#' Searches keywords on Twitter. 
#' 
#' It assumes that a valid token is already present in the working environment. For details, see https://rtweet.info/articles/auth.html
#'
#' @param keywords A character vector or a data frame, with keywords in the first column. Defaults to NULL. If NULL, tries to import previously stored keywords for given language and date. If a data frame, it assumes that the first columns includes the keywords. If a character vector, it processes it directly.
#' @param n_keywords An integer, defaults to NULL. 
#' @param n_tweets An integer. Number of tweets to request. 
#' @param date Used to find keywords locally, if none are provided. Defaults to latest available. To systematically get data for the previous day, use `Sys.Date()-1` 
#' @param tweet_type Passed to `rtweet`. Defaults to "recent". Other valid types include "mixed" and "popular".
#' @return A data.frame (a tibble) with `n` number of rows and two columns, `words` and `n` for number of occurrences.
#' @examples
#' 
#' @export


nwd_get_tweets <- function(keywords = NULL,
                           date = NULL,
                           language = NULL,
                           n_keywords = 10,
                           n_tweets = 1000,
                           tweet_type = "recent",
                           wait = 10, 
                           store = TRUE) {
  
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("keywords"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  
  for (i in language) {
    
    if (is.null(date)) {
      date <- fs::dir_ls(path = fs::path("keywords", i), recurse = FALSE, type = "file") %>% 
        fs::path_file() %>% 
        stringr::str_extract(pattern = "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]") %>% 
        na.omit() %>% 
        as.Date() %>% 
        max()
    }
    
    if (is.null(keywords)) {
      keywords <- readRDS(fs::dir_ls(path = fs::path("keywords", i), recurse = FALSE, type = "file", regexp = date))
    }
    
    if (is.element("data.frame", class(keywords))) {
      keywords <- keywords %>% dplyr::pull(1)
    }
    
    keywords <- head(x = unique(keywords), n_keywords)
    
    tweets_day_folder <- fs::path("tweets", 
                                  i,
                                  as.character(date))
    
    fs::dir_create(path = tweets_day_folder, recurse = TRUE)
    
    for (j in keywords) {
      tweets <- rtweet::search_tweets(q = j,
                                      n = n_tweets,
                                      lang = language,
                                      include_rts = FALSE,
                                      type = tweet_type)
      
      saveRDS(object = tweets,
              file = fs::path(tweets_day_folder, 
                              paste0(j, "-", i, "-", n_tweets, "-", tweet_type, "-", Sys.time(), ".rds")))
      
      Sys.sleep(time = wait)
    }
  }
}
