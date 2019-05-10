#' Extract most frequently used words from news titles 
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @param n An integer. Number of words to keep. 
#' @param date Only news downloaded in the given date will be considered. Defaults to current day. To get data for the previous day, use `Sys.Date()-1` 
#' @return A data.frame (a tibble) with `n` number of rows and two columns, `words` and `n` for number of occurrences.
#' @examples
#' 
#' @export

nwd_extract_keywords <- function(languages = c("ar","bg","cs","da","de","el","en","es", "et","fi","fr","hr", "hu","it", "lt","lv","nl","pl","pt","ro","ru", "sk","sl","sv","sw","tr","zh"),
                             n = 10,
                             date = Sys.Date(), 
                             store = TRUE) {
  
  for (i in languages) {
    all_rds <- list.files(path = file.path("emm_newsbrief",
                                           i,
                                           as.character(lubridate::year(date)),
                                           stringr::str_pad(lubridate::month(date), width = 2),
                                           stringr::str_pad(lubridate::day(date), width = 2)),
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
                                     as.character(lubridate::year(date)),
                                     stringr::str_pad(lubridate::month(date), width = 2),
                                     stringr::str_pad(lubridate::day(date), width = 2))
      
      fs::dir_create(path = keywords_base_path)
      saveRDS(object = keywords,
              file = fs::path(keywords_base_path,
                              paste0(Sys.Date(), "-keywords_", i, ".rds")))
    }
  }
  
  keywords
}
