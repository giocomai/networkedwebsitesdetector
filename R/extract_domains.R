#' Get screenshots and place them in relevant subfolder
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

extract_domains_from_tweets <- function(language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "homepage"), recursive = FALSE, full.names = FALSE)
  }
  dir.create(path = file.path("data", "domains"), showWarnings = FALSE)
  dir.create(path = file.path("data", "domains", "domain_name"), showWarnings = FALSE)
  dir.create(path = file.path("data", "domains", "domain_name", language), showWarnings = FALSE)

  for (i in language) {
    csv_files <- list.files(path = file.path("data", "tweets", i),
                           pattern = "\\.csv",
                           full.names = TRUE,
                           recursive = TRUE)
    
    all_links <- purrr::map_dfr(.x = csv_files, .f = read.csv)
    
    all_domains <- all_links %>% 
      dplyr::mutate(domain = urltools::domain(all_links$url)) %>% 
      dplyr::mutate(domain = stringr::str_remove(string = domain, pattern = stringr::fixed("www."))) %>% 
      dplyr::distinct(domain)
    
    saveRDS(object = all_domains, file = file.path("data", "domains", "domain_name", i, "all_domains.rds"))
    invisible(all_domains)
  }
  
}


