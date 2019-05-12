#' Extract domain names from tweets
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_extract_domains_from_tweets <- function(language = NULL) {
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


