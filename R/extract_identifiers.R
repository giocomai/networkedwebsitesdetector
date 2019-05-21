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
                                    sample = NULL, 
                                    temp = FALSE) {
  if (temp == TRUE) {
    homepage_folder <- fs::path("nwd_temp", "homepage")
    
  } else {
    homepage_folder <- fs::path("homepage")
  }

  
  if (is.null(language)) {
    language <- fs::dir_ls(path = homepage_folder,
                           recurse = FALSE,
                           type = "directory") %>% 
      fs::path_file()
  }
  
  for (i in language) {
    fs::dir_create(path = fs::path("identifiers", i), recurse = TRUE)
    html_location <- fs::path(homepage_folder, i)
    
    html_dirs <- fs::dir_ls(path = html_location, recurse = FALSE, type = "directory")
    
    for (j in html_dirs) {
      html_files <- fs::dir_ls(path = j, recurse = FALSE, type = "file")
      temp_date <- stringr::str_remove(string = j, pattern = paste0(html_location, "/"))
      base_folder <- file.path("identifiers", i, temp_date)
      
      if (fs::file_exists(file.path(base_folder, paste0(temp_date, "_", "identifiers_df.rds")))==FALSE) {
        
        if (is.null(sample)==FALSE) {
          html_files <- sample(x = html_files, size = sample)
        }
        
        domains <- fs::path_ext_remove(path = stringr::str_remove(string = html_files,
                                                                  pattern = stringr::fixed(paste0(j, "/"))))
        
        ua       <- rep(x = as.character(NA), length = length(html_files))
        ca_pub     <- rep(x = as.character(NA), length = length(html_files))
        fb_admins   <- rep(x = as.character(NA), length = length(html_files))
        fb_page_id   <- rep(x = as.character(NA), length = length(html_files))
        fb_app_id   <- rep(x = as.character(NA), length = length(html_files))
        taboola <- rep(x = as.character(NA), length = length(html_files))
        
        if (progress_bar == TRUE) {
          pb <- dplyr::progress_estimated(n = length(html_files), min_time = 1)
        }
        for (k in seq_along(html_files)) {
          if (progress_bar == TRUE) {
            pb$tick()$print()
          }
          
          temp <- tryCatch(expr = paste(readLines(html_files[k]), collapse = "\n"),
                           error = function(e) {
                             warning(paste("Could not read", html_files[k]))
                             NA
                           })
          ua[k] <- stringr::str_extract_all(string = temp,
                                            pattern = stringr::regex("UA-[[:digit:]][[:digit:]][[:digit:]][[:digit:]]+", ignore_case = FALSE))
          ca_pub[k] <- stringr::str_extract_all(string = temp,
                                                pattern = stringr::regex("ca-pub-[[:digit:]][[:digit:]]+", ignore_case = TRUE))
          
          taboola[k] <- purrr::map(.x = purrr::map(.x = stringr::str_extract_all(string = temp,
                                                                                 pattern = stringr::regex("taboola.com/libtrc/[[:print:]]+/", ignore_case = TRUE)), .f = function(x) stringr::str_remove(string = x, pattern = c("taboola.com/libtrc/"))), .f = function(x) stringr::str_remove(string = x, pattern = c("/$")))
          
          temp <- tryCatch(expr = xml2::read_html(html_files[k]),
                           error = function(e) {
                             warning(paste("Could not parse", html_files[k]))
                             NA
                           })
          
          if(is.na(temp)==FALSE&is.element(el = "xml_node", set = class(temp))) {
            fb_admins_temp <- temp %>% 
              rvest::html_nodes(xpath = '//meta[@property="fb:admins"]') %>% 
              rvest::html_attr('content')
            
            if (length(fb_admins_temp)>0) {fb_admins[k] <- stringr::str_split(string = fb_admins_temp, pattern = ",")}
            
            fb_page_id_temp <- temp %>% 
              rvest::html_nodes(xpath = '//meta[@property="fb:page_id"]') %>% 
              rvest::html_attr('content')
            
            if (length(fb_page_id_temp)>0) {fb_page_id[k] <- stringr::str_split(string = fb_page_id_temp, pattern = ",")}
            
            
            fb_app_id_temp <- temp %>% 
              rvest::html_nodes(xpath = '//meta[@property="fb:app_id"]') %>% 
              rvest::html_attr('content')
            
            if (length(fb_app_id_temp)>0) {fb_app_id[k] <- stringr::str_split(string = fb_app_id_temp, pattern = ",")}
            
          }
          
        }
        
        identifiers_df <- tibble::tibble(domain = domains,
                                         ua = ua, 
                                         ca_pub = ca_pub,
                                         fb_admins = fb_admins, 
                                         fb_page_id = fb_page_id, 
                                         fb_app_id = fb_app_id, 
                                         taboola = taboola)
        
        
        
        fs::dir_create(path = base_folder, recurse = TRUE)
        
        saveRDS(object = identifiers_df, file = file.path(base_folder, paste0(temp_date, "_", "identifiers_df.rds")))
        
        message(paste("\nIdentifiers stored in",
                      file.path(base_folder, paste0(temp_date, "_", "identifiers_df.rds"))))
      }
    }
  }
}

#' Extract identifiers from backuped homepages
#'
#' @param language A character vector of length one corresponding to a language two-letter code.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export
#' 

nwd_extract_identifiers_from_backup <- function(language) {
  available_backups_on_google_drive <-
    nwd_list_available_backups_on_google_drive(folder = "homepage",
                                               timeframe = "daily",
                                               language = language,
                                               filetype = "html")
  
  fs::dir_create(path = fs::path("identifiers", language), recurse = TRUE)
  identifiers_done_dates <- fs::dir_ls(path = fs::path("identifiers", language)) %>%
    fs::path_file()
  
  if (length(identifiers_done_dates)>0) {
    backup_homepages_not_processed <- 
      available_backups_on_google_drive %>% 
      dplyr::filter(stringr::str_detect(string = name,
                                        pattern = paste(identifiers_done_dates,
                                                        collapse = "|"),
                                        negate = TRUE))
  } else {
    backup_homepages_not_processed <- available_backups_on_google_drive
  }

  if (nrow(backup_homepages_not_processed)>0) {
    for (i in 1:nrow(backup_homepages_not_processed)) {
      fs::dir_create(path = "nwd_temp")
      googledrive::drive_download(file = backup_homepages_not_processed %>%
                                    dplyr::slice(i),
                                  path = fs::path("nwd_temp", "temp.tar.gz"))
      untar(tarfile = fs::path("nwd_temp", "temp.tar.gz"), exdir = "nwd_temp")
      nwd_extract_identifiers(language = language, progress_bar = TRUE, temp = TRUE)
      fs::dir_delete(path = "nwd_temp")
    }
  }
}
