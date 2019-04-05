#' Extract identifiers from web pages
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export
#' 

extract_identifiers <- function(language = NULL, progress_bar = TRUE) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "homepage"), recursive = FALSE, full.names = FALSE)
  }
  
  dir.create(file.path("data", "identifiers"), showWarnings = FALSE)
  
  for (i in language) {
    dir.create(file.path("data", "identifiers", i), showWarnings = FALSE)

    html_location <- file.path("data", "domains", "homepage", i)
    
    html_files <- list.files(path = html_location, full.names = TRUE, pattern = "html", recursive = TRUE)
    
    domains <- list.files(path = html_location, full.names = FALSE, pattern = "html", recursive = TRUE) %>%
      stringr::str_remove(pattern = stringr::fixed(".html")) %>% 
      stringr::str_remove(pattern = ".*\\/")
    
    ua <- vector(mode = "character", length = length(html_files))
    ca_pub <- vector(mode = "character", length = length(html_files))
    fb_admins <- vector(mode = "character", length = length(html_files))
    fb_page_id <- vector(mode = "character", length = length(html_files))
    fb_app_id <- vector(mode = "character", length = length(html_files))
    
    if (progress_bar == TRUE) {
      pb <- dplyr::progress_estimated(n = length(html_files), min_time = 1)
    }
    for (j in seq_along(html_files)) {
      if (progress_bar == TRUE) {
        pb$tick()$print()
      }
      
      temp <-  tryCatch(expr = paste(readLines(html_files[j]), collapse = "\n"),
                        error = function(e) {
                          warning(paste("Could not read", html_files[j]))
                          NA
                        })
      ua[j] <- stringr::str_extract_all(string = temp,
                                        pattern = stringr::regex("UA-[[:digit:]][[:digit:]][[:digit:]][[:digit:]]+", ignore_case = FALSE))
      ca_pub[j] <- stringr::str_extract_all(string = temp,
                                            pattern = stringr::regex("ca-pub-[[:digit:]][[:digit:]]+", ignore_case = TRUE))
      
      temp <-  tryCatch(expr = xml2::read_html(html_files[j]),
                        error = function(e) {
                          warning(paste("Could not parse", html_files[j]))
                          NA
                        })
      
      if(is.na(temp)==FALSE&is.element(el = "xml_node", set = class(temp))) {
        fb_admins_temp <- temp %>% 
          rvest::html_nodes(xpath = '//meta[@property="fb:admins"]') %>% 
          rvest::html_attr('content')
        
        if (length(fb_admins_temp)>0) {fb_admins[j] <- fb_admins_temp}
        
        fb_page_id_temp <- temp %>% 
          rvest::html_nodes(xpath = '//meta[@property="fb:page_id"]') %>% 
          rvest::html_attr('content')
        
        if (length(fb_page_id_temp)>0) {fb_page_id[j] <- fb_page_id_temp}
        
        
        fb_app_id_temp <- temp %>% 
          rvest::html_nodes(xpath = '//meta[@property="fb:app_id"]') %>% 
          rvest::html_attr('content')
        
        if (length(fb_app_id_temp)>0) {fb_app_id[j] <- fb_app_id_temp}

      }
      
    }
    
    id_df <- tibble::tibble(domain = domains,
                            ua = ua, 
                            ca_pub = ca_pub,
                            fb_admins = fb_admins, 
                            fb_page_id = fb_page_id, 
                            fb_app_id = fb_app_id)
    
    base_folder <- file.path("data", "identifiers", i, Sys.Date())
    dir.create(base_folder, showWarnings = FALSE)
    saveRDS(object = id_df, file = file.path("id", i, Sys.Date(), "id_df.rds"))
  }
}
