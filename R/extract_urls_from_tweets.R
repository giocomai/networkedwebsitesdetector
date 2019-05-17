#' Extract links from tweets and expands shortened urls
#' 
#' @param tweets A data frame of tweets, as created by `rtweet`. If not given, it processes tweets available in `tweets` subfolder.
#' @param n_char An integer, defaults to 30. Number of characters in the url: only urls shorter than n_char will be expanded, others will be kept as they are.  
#' @param n_retry An integer, defaults to 3. Number of times it tries to expand urls if first attempt fails. Set to 0 for attempting only once.
#' @return Nothing, used for its side effects, i.e. stores expanded urls in `tweet_links` subfolder.
#' @examples
#' 
#' @export

nwd_extract_urls_from_tweets <- function(tweets = NULL, 
                                         language = NULL,
                                         date = NULL, 
                                         expand_url = TRUE,
                                         n_char = 30,
                                         n_retry = 3) {
  
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("tweets"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  for (i in language) {
    
    if (is.null(tweets)) {
      
      if (is.null(date)) {
        date <- fs::dir_ls(path = fs::path("tweets", i), recurse = FALSE, type = "directory") %>% 
          fs::path_file() %>% 
          stringr::str_extract(pattern = "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]") %>% 
          na.omit()
      }
      
      for (j in date) {
        tweet_day_folder <- fs::path("tweets", i, j)
        tweet_links_day_folder <- stringr::str_replace(string = tweet_day_folder, pattern = "tweets", replacement = "tweet_links")
        fs::dir_create(path = tweet_links_day_folder, recurse = TRUE)
        tweet_files <- fs::dir_ls(path = tweet_day_folder, recurse = FALSE,type = "file", glob = "*.rds")
        
        for (k in seq_along(tweet_files)) {
          expanded_urls_file_location <- stringr::str_replace(string = tweet_files[k], pattern = "tweets", replacement = "tweet_links")
          if (fs::file_exists(path = expanded_urls_file_location)==FALSE) {
            temp_tweets <- readRDS(file = tweet_files[k])
            expanded_urls <- nwd_expand_urls_from_tweets(tweets = temp_tweets,
                                                         n_char = n_char, 
                                                         n_retry = n_retry)
            saveRDS(object = expanded_urls, file = expanded_urls_file_location)
          }
        }
      }
    }
  }
}


#' Extract links from tweets and expands shortened urls
#' 
#' @param tweets A data frame of tweets, as created by `rtweet`. 
#' @param n_char An integer, defaults to 30. Number of characters in the url: only urls shorter than n_char will be expanded, others will be kept as they are.  
#' @param n_retry An integer, defaults to 3. Number of times it tries to expand urls if first attempt fails. Set to 0 for attempting only once.
#' @return A data.frame (a tibble) with 9 columns: "user_id", "status_id", "created_at", "screen_name", text", "orig_url","expanded_url", "status_code", "url" 
#' @examples
#' 
#' @export


nwd_expand_urls_from_tweets <- function(tweets,
                                        n_char = 30,
                                        n_retry = 3) {
  if (nrow(tweets)>0) {
    all_links <- tweets %>% 
      dplyr::select(user_id, status_id, created_at, screen_name, text, urls_expanded_url) %>% 
      tidyr::unnest(urls_expanded_url) %>% 
      tidyr::drop_na() 
    
    all_links_long_pre <- all_links %>% 
      dplyr::filter(nchar(urls_expanded_url)<n_char) %>% 
      dplyr::pull(urls_expanded_url) 
    
    if (length(all_links_long_pre)>0) {
      all_links_long <- tryCatch({longurl::expand_urls(all_links_long_pre)},
                                 error=function(e){cat("ERROR:",conditionMessage(e), "\n")})
      
      if (n_retry > 0) {
        for (i in 1:n_retry) {
          if (is.null(all_links_long) == FALSE) {
            all_links_long_pre_retry <- 
              all_links_long %>% 
              dplyr::filter(is.na(expanded_url)==TRUE) %>% 
              dplyr::pull(orig_url)
            
            all_links_long_retry <- tryCatch({longurl::expand_urls(all_links_long_pre_retry)},
                                             error=function(e){cat("ERROR:",conditionMessage(e), "\n")})
            
            all_links_long <- dplyr::bind_rows(all_links_long_retry, all_links_long) %>% 
              dplyr::distinct(orig_url, .keep_all=TRUE)
            
          }
        }
      }
      
      if (is.null(all_links_long)==FALSE) {
        if (nrow(all_links_long)>0) {
          all_links_long_merged <- 
            all_links %>% 
            dplyr::rename(orig_url = urls_expanded_url) %>% 
            dplyr::left_join(y = all_links_long, by = "orig_url") %>% 
            dplyr::mutate(url = dplyr::if_else(condition = is.na(expanded_url),
                                               true = as.character(orig_url),
                                               false = as.character(expanded_url))) %>% 
            dplyr::ungroup() 

        }
      }
    } else {
      all_links_long_merged <- 
        all_links %>% 
        dplyr::rename(orig_url = urls_expanded_url) %>%  
        dplyr::mutate(expanded_url = as.character(NA),
                      status_code = as.integer(NA), 
                      url = as.character(orig_url)) %>% 
        dplyr::ungroup() 
    }
    return(all_links_long_merged) 
  }
  
}
