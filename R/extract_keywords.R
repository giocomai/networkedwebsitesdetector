#' Extract most frequently used words from news titles 
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @param n An integer. Number of words to keep. 
#' @param date Only news downloaded in the given date will be considered. Defaults to current day. To get data for the previous day, use `Sys.Date()-1` 
#' @return A data.frame (a tibble) with `n` number of rows and two columns, `words` and `n` for number of occurrences.
#' @examples
#' 
#' @export

nwd_extract_keywords <- function(languages = NULL,
                                 n = 20,
                                 date = NULL, 
                                 store = TRUE) {
  
  if (is.null(languages)) {
    languages <-  fs::dir_ls(path = fs::path("emm_newsbrief"),
                             recurse = FALSE,
                             type = "directory") %>% 
      fs::path_file()
  }
  
  
  for (i in languages) {
    
    if (is.null(date)) {
      date <- fs::dir_ls(path = fs::path("emm_newsbrief", i), recurse = FALSE, type = "directory") %>% 
        fs::path_file()
      
    }
    
    for (j in date) {
      all_rds <- list.files(path = fs::path("emm_newsbrief",
                                            i,
                                            as.character(j)),
                            pattern = paste0(i, "\\.rds"),
                            full.names = TRUE)  
      
      all_news <- suppressMessages(purrr::map_df(.x = all_rds,
                                                 .f = readr::read_rds)) %>% #unisce i file in un singolo data frame
        dplyr::distinct(link, .keep_all = TRUE)
      
      keywords <- all_news %>% 
        dplyr::transmute(title, Date = as.Date(pubDate)) %>%
        tidytext::unnest_tokens(input = title, output = "words") %>% 
        dplyr::anti_join(tibble::tibble(words = stopwords::stopwords(language = i, source = "stopwords-iso")), by = "words") %>% # elimina stopwords
        dplyr::filter(!stringr::str_detect(string = words, pattern = "[[:digit:]]")) %>%  # togliere i numeri registrati come parole
        dplyr::group_by(words) %>% 
        dplyr::count(sort = TRUE) %>% 
        head(n) 
      
      if (store==TRUE) {
        keywords_base_path <- fs::path("keywords", 
                                       i,
                                       as.character(j))
        
        fs::dir_create(path = keywords_base_path)
        saveRDS(object = keywords,
                file = fs::path(keywords_base_path,
                                paste0(j, "-keywords_", n, "_", i, ".rds")))
      }
      
    }
    
    
  }
  
  keywords
}
