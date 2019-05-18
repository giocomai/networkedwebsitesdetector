#' Extract domain names from tweets
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_legacy_extract_domains_from_tweets <- function(language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("tweets"), recursive = FALSE, full.names = FALSE)
  }
  dir.create(path = file.path("domains"), showWarnings = FALSE)

  for (i in language) {
    dir.create(path = file.path("domains", i), showWarnings = FALSE)
    
    csv_files <- list.files(path = file.path("tweets", i),
                           pattern = "\\.csv",
                           full.names = TRUE,
                           recursive = TRUE)
    
    all_links <- purrr::map_dfr(.x = csv_files, .f = read.csv)
    
    all_domains <- all_links %>% 
      dplyr::mutate(domain = urltools::domain(all_links$url)) %>% 
      dplyr::mutate(domain = stringr::str_remove(string = domain, pattern = stringr::fixed("www."))) %>% 
      dplyr::distinct(domain)
    
    saveRDS(object = all_domains, file = file.path("domains", i, "all_domains.rds"))
    invisible(all_domains)
  }
}


#' Extract domain names from pre-processed tweets
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @param date Only news downloaded in the given date will be considered. Defaults to current day. To get data for the previous day, use `Sys.Date()-1` 
#' @return A data.frame (a tibble) with one column corresponding to the domain names found on a given date. 
#' @examples
#' 
#' @export

nwd_extract_domains <- function(language = NULL,
                                date = NULL) {
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("tweet_links"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  for (i in language) {
    
    fs::dir_create(path = fs::path("domains", i), recurse = TRUE)
    
    if (is.null(date)) {
      date <- fs::dir_ls(path = fs::path("tweet_links", i), recurse = FALSE, type = "directory") %>% 
        fs::path_file() %>% 
        stringr::str_extract(pattern = "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]") %>% 
        na.omit()
    }
    
    for (j in date) {
      domains_by_date <- purrr::map_dfr(.x = fs::dir_ls(path = fs::path("tweet_links", i, j), recurse = FALSE, type = "file", glob = "*.rds"),
                                        .f = readRDS) %>% 
        dplyr::mutate(domain = urltools::domain(url)) %>% 
        dplyr::mutate(domain = stringr::str_remove(string = domain, pattern = stringr::fixed("www."))) %>% 
        dplyr::distinct(domain)
      
      saveRDS(object = domains_by_date,
              file = fs::path("domains", i, paste0(j, "_", i, "_domains.rds")))
    }
  }
  invisible(domains_by_date)
}



