#' Extract identifiers from web pages
#'
#' @param language A character vector of length one corresponding to a language two-letter code. Defaults to NULL. If NULL, processes available language. If more than one, throws error. 
#' @param sample Defaults to NULL. If given, it processess only a sample of the available files of the size given with this parameter. 
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export
#' 

nwd_extract_identifiers <- function(language = NULL,
                                    progress_bar = TRUE,
                                    sample = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "homepage"), recursive = FALSE, full.names = FALSE)
  }
  dir.create(file.path("data", "identifiers"), showWarnings = FALSE)
  dir.create(file.path("data", "identifiers", language), showWarnings = FALSE)
  html_location <- file.path("data", "domains", "homepage", language)
  
  html_dirs <- fs::dir_ls(path = html_location, recursive = FALSE, type = "directory")
  
  for (i in html_dirs) {
    html_files <- fs::dir_ls(path = i, recursive = FALSE, type = "file")
    temp_date <- stringr::str_remove(string = i, pattern = paste0(html_location, "/"))
    base_folder <- file.path("data", "identifiers", language, temp_date)
    
    if (fs::file_exists(file.path(base_folder, "identifiers_df.rds"))==FALSE) {
      
      if (is.null(sample)==FALSE) {
        html_files <- sample(x = html_files, size = sample)
      }
      
      domains <- fs::path_ext_remove(path = stringr::str_remove(string = html_files,
                                                                pattern = stringr::fixed(paste0(i, "/"))))
      
      ua             <- rep(x = as.character(NA), length = length(html_files))
      ca_pub         <- rep(x = as.character(NA), length = length(html_files))
      fb_admins      <- rep(x = as.character(NA), length = length(html_files))
      fb_page_id     <- rep(x = as.character(NA), length = length(html_files))
      fb_app_id      <- rep(x = as.character(NA), length = length(html_files))
      
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
          
          if (length(fb_admins_temp)>0) {fb_admins[j] <- stringr::str_split(string = fb_admins_temp, pattern = ",")}
          
          fb_page_id_temp <- temp %>% 
            rvest::html_nodes(xpath = '//meta[@property="fb:page_id"]') %>% 
            rvest::html_attr('content')
          
          if (length(fb_page_id_temp)>0) {fb_page_id[j] <- stringr::str_split(string = fb_page_id_temp, pattern = ",")}
          
          
          fb_app_id_temp <- temp %>% 
            rvest::html_nodes(xpath = '//meta[@property="fb:app_id"]') %>% 
            rvest::html_attr('content')
          
          if (length(fb_app_id_temp)>0) {fb_app_id[j] <- stringr::str_split(string = fb_app_id_temp, pattern = ",")}
          
        }
        
      }
      
      identifiers_df <- tibble::tibble(domain = domains,
                                       ua = ua, 
                                       ca_pub = ca_pub,
                                       fb_admins = fb_admins, 
                                       fb_page_id = fb_page_id, 
                                       fb_app_id = fb_app_id)
      
      
      
      fs::dir_create(path = base_folder, recursive = TRUE)
      
      saveRDS(object = identifiers_df, file = file.path(base_folder, "identifiers_df.rds"))
      
      message(paste("\nIdentifiers stored in",
                    file.path(base_folder, "identifiers_df.rds")))
    }
  }
}
