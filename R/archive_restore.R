#' Archive data in compressed files
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export


nwd_archive <- function(date = NULL,
                        folder = "tweets",
                        timeframe = "daily",
                        language = NULL,
                        filetype = "rds") {
  
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path(folder),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  for (i in language) {
    if (is.null(date)) {
      date <- fs::dir_ls(path = fs::path(folder, i), recurse = FALSE, type = "directory") %>% 
        fs::path_file()
    }
    
    if (timeframe=="daily") {
      for (j in date) {
        year <- lubridate::year(j)
        fs::dir_create(path = fs::path("archive", language, folder, year), recurse = TRUE)
        archived_file_location <- fs::path("archive",
                                           i,
                                           folder,
                                           year,
                                           paste0(j, 
                                                  "_", 
                                                  i, 
                                                  "_", 
                                                  folder, 
                                                  "_",
                                                  filetype,
                                                  "_daily.tar.gz"))
        if (fs::file_exists(path = archived_file_location)==FALSE) {
          
          tar(tarfile = archived_file_location,
              files = fs::dir_ls(path =  fs::path(folder, i, j),
                                 recurse = TRUE,
                                 type = "file",
                                 glob = paste0("*.", filetype)),
              tar = Sys.which("tar"),
              compression = "gzip")
          
        }
      }
    }
    
    if (timeframe=="monthly") {
      fs::dir_create(path = fs::path("archive", language, folder, year), recurse = TRUE)
      archived_file_location <- fs::path("archive",
                                         language,
                                         folder,
                                         year,
                                         paste0(year, 
                                                "-", 
                                                stringr::str_pad(string = month, width = 2, pad = "0"), 
                                                "_", 
                                                language, 
                                                "_", 
                                                folder, 
                                                "_",
                                                filetype,
                                                "_monthly.tar.gz"))
      tar(tarfile = archived_file_location,
          files = fs::dir_ls(path =  fs::path(folder, language, year, stringr::str_pad(string = month, width = 2, pad = "0")),
                             recurse = TRUE,
                             type = "file",
                             glob = paste0("*", language, ".", filetype)),
          tar = Sys.which("tar"),
          compression = "gzip")
      
    }
    
  }
  
}

#' Restore data from compressed files
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_restore <- function(date = NULL,
                        folder = "tweets",
                        timeframe = "daily",
                        language = NULL,
                        filetype = "rds", 
                        filenames_only = FALSE) {
  
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("archive"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  for (i in language) {
    if (is.null(date)) {
      date <- fs::dir_ls(path = fs::path("archive", i, folder), recurse = FALSE, type = "directory") %>% 
        fs::path_file()
    }
    if (sum(nchar(date)>4)>0) {
      year <- lubridate::year(date)
    } else {
      year <- date
    }
    archived_files_location_l <- fs::dir_ls(path = fs::path("archive", i, folder, year),
                                            recurse = FALSE,
                                            type = "file",
                                            glob = paste0("*_",
                                                          i, 
                                                          "_", 
                                                          folder, 
                                                          "_",
                                                          filetype,
                                                          "_", 
                                                          timeframe, ".tar.gz"))
    
    if (filenames_only==FALSE) {
      purrr::walk(.x = archived_files_location_l,
                  .f = function (x) {
                    untar(tarfile = x)
                  })
    } else {
      return(purrr::map(.x = archived_files_location_l,
                 .f = function (x) {
                   untar(tarfile = x, list = TRUE)
                 }) %>% unlist())
    }

  }
  
}

#' Backup archived files to googledrive
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_upload_to_googledrive <- function(date = NULL,
                                      folder = "tweets",
                                      timeframe = "daily",
                                      language = NULL,
                                      filetype = "rds",
                                      show_filenames_only = FALSE) {
  
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("archive"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  for (i in language) {
    if (is.null(date)) {
      date <- fs::dir_ls(path = fs::path("archive", i, folder), recurse = TRUE, type = "file", glob = paste0("*_", filetype, "_", timeframe, ".tar.gz")) %>% 
        fs::path_file() %>% 
        stringr::str_extract(pattern = "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]") %>% 
        as.Date()
    }
    
    file_locations_to_upload <- fs::dir_ls(path = fs::path("archive", i, folder), recurse = TRUE, type = "file", glob = paste0("*_", filetype, "_", timeframe, ".tar.gz")) 
    filenames_to_upload <- fs::path_file(path = file_locations_to_upload)
    
    ## base networkedwebsitesdetector folder
    home_d <- googledrive::drive_ls() %>% dplyr::filter(name=="networkedwebsitesdetector")
    if (nrow(home_d)==0) {
      networkedwebsitesdetector_folder_d <- googledrive::drive_mkdir(name = "networkedwebsitesdetector")
    } else if (nrow(home_d)==1) {
      networkedwebsitesdetector_folder_d <- home_d
    } else {
      stop("networkedwebsitesdetector should find just one networkedwebsitesdetector folder. Please delete if you have more than one.")
    }
    
    # language folders
    language_folder_d <- googledrive::drive_ls(path = networkedwebsitesdetector_folder_d) %>% 
      dplyr::filter(name==language)
    if (nrow(language_folder_d)==0) {
      language_folder_d <- googledrive::drive_mkdir(name = language, parent = networkedwebsitesdetector_folder_d)
    } else if (nrow(language_folder_d)==1) {
      #do nothing
    } else {
      stop("networkedwebsitesdetector should find just one folder with the same language name. Please delete if you have more than one.")
    }
    
    all_types_folder_d <- googledrive::drive_ls(path = language_folder_d)
    
    ## type folder
    type_folder_d <- all_types_folder_d %>%
      dplyr::filter(name==folder)
    
    if (nrow(type_folder_d)==0) {
      type_folder_d <- googledrive::drive_mkdir(name = folder,
                                                parent = language_folder_d)
    } else if (nrow(type_folder_d)==1) {
      # do nothing
    } else {
      stop("networkedwebsitesdetector should find just one folder type with the same name. Please delete if you have more than one.")
    }
    
    ## year folder
    all_years_folder_d <- googledrive::drive_ls(path = type_folder_d, recursive = FALSE)
    
    years <- unique(lubridate::year(date))
    
    for (j in years) {
      ## year folder
      year_folder_d <- all_years_folder_d %>%
        dplyr::filter(name==as.character(j))
      if (nrow(year_folder_d)==0) {
        year_folder_d <- googledrive::drive_mkdir(name = as.character(j),
                                                  parent = type_folder_d)
      } else if (nrow(year_folder_d)==1) {
        # do nothing
      } else {
        stop("networkedwebsitesdetector should find just one folder type with the same year Please delete if you have more than one.")
      }
      year_folder_contents_d <- googledrive::drive_ls(path = year_folder_d)
      
      current_year_logical <- stringr::str_starts(string = fs::path_file(file_locations_to_upload), pattern = as.character(j))
      
      if(nrow(year_folder_contents_d)==0) {
        purrr::walk(.x = file_locations_to_upload[current_year_logical],
                    .f = function(x) googledrive::drive_upload(media = x, path = year_folder_d))
      } else {
        file_locations_to_upload_current_year <- file_locations_to_upload[current_year_logical]
        purrr::walk(.x = file_locations_to_upload_current_year[is.element(el = fs::path_file(file_locations_to_upload_current_year), set = year_folder_contents_d$name)==FALSE],
                    .f = function(x) googledrive::drive_upload(media = x, path = year_folder_d))
      }
    }
  }
}


#' Download files archived in Google Drive
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_download_from_googldrive <- function(date = NULL,
                                         folder = "tweets",
                                         timeframe = "daily",
                                         language = NULL,
                                         filetype = "rds",
                                         overwrite = FALSE) {
  home_d <- googledrive::drive_ls() %>% dplyr::filter(name=="networkedwebsitesdetector")
  
  all_languages_folder <- googledrive::drive_ls(path = home_d)
  
  if (is.null(language)==TRUE) {
    language <- all_languages_folder$name
  }
  
  for (i in language) {
    
    language_folder_d <-  all_languages_folder%>% 
      dplyr::filter(name==i)
    
    all_types_folder_d <- googledrive::drive_ls(path = language_folder_d)
    
    type_folder_d <- all_types_folder_d %>%
      dplyr::filter(name==folder)
    
    ## year folder
    
    all_years_folder_d <- googledrive::drive_ls(path = type_folder_d)
    
    years <- all_years_folder_d$name
    
    for (j in years) {
      
      year_folder_d <- all_years_folder_d %>%
        dplyr::filter(name==as.character(j))
      
      year_folder_contents_d <- googledrive::drive_ls(path = year_folder_d)
      
      base_year_path <- fs::path("archive", i, folder, j)
      
      fs::dir_create(path = base_year_path, recurse = TRUE)
      
      local_files <- fs::dir_ls(base_year_path) %>% fs::path_file()
      
      year_folder_contents_filtered_d <- year_folder_contents_d %>% 
        dplyr::filter(stringr::str_detect(string = name,
                                          pattern = paste0(filetype, "_", timeframe, ".tar.gz")))
      
      if (overwrite==FALSE) {
        year_folder_contents_filtered_d  <- year_folder_contents_filtered_d %>% 
          dplyr::filter(is.element(name, local_files)==FALSE)
      }
      
      for (k in 1:nrow(year_folder_contents_filtered_d)) {
        temp_file_d <- year_folder_contents_filtered_d %>% dplyr::slice(k)
        googledrive::drive_download(file = temp_file_d,
                                    path = fs::path(base_year_path, temp_file_d$name),
                                    overwrite = overwrite)
        
      }
    }
  }
  
  
}

#' Adjusts file names of folders with single digits
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_adjust_folder_names <- function(folder = "tweets", language = NULL, date = NULL) {
  if (is.null(date)) {
    year <- lubridate::year(Sys.Date())
  } else {
    year <- lubridate::year(date)
  }
  
  short_paths <- fs::dir_ls(path = fs::path(folder, year),
                            recurse = TRUE,
                            type = "directory")
  
  short_paths_split <- short_paths %>% 
    fs::path_split()
  
  for (i in seq_along(short_paths_split)) {
    short_paths_split[[i]][3] <- stringr::str_pad(string = short_paths_split[[i]][3],
                                                  width = 2,
                                                  pad = "0")
    if (length(short_paths_split[[i]])>3) {
      short_paths_split[[i]][4] <- stringr::str_pad(string = short_paths_split[[i]][4],
                                                    width = 2,
                                                    pad = "0")
    }
    if (length(short_paths_split[[i]])>4) {
      short_paths_split[[i]][5] <- stringr::str_pad(string = short_paths_split[[i]][45],
                                                    width = 2,
                                                    pad = "0")
    }
  }
  
  short_paths_joined <- fs::path_join(parts = short_paths_split)
  
  short_paths_l <- as.character(short_paths)!=short_paths_joined
  
  fs::file_move(path = short_paths[short_paths_l], new_path = short_paths_joined[short_paths_l])
}

#' Adjusts names of nested date folders
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_adjust_folder_names_nested <- function(folder = "tweets", language = NULL, date = NULL) {
  if (is.null(date)) {
    year <- lubridate::year(Sys.Date())
  } else {
    year <- lubridate::year(date)
  }
  
  original_month_paths <- fs::dir_ls(path = fs::path(folder, language, year),
                                     recurse = FALSE,
                                     type = "directory")
  
  for (i in original_month_paths) {
    original_daily_path <- fs::dir_ls(path = i,
                                      recurse = FALSE,
                                      type = "directory") 
    
    original_daily_path_split <- original_daily_path %>% 
      fs::path_split()
    adjusted_daily_path_split <- list()
    for (j in seq_along(original_daily_path_split)) {
      adjusted_daily_path_split[[j]] <- c(original_daily_path_split[[j]][1],
                                          original_daily_path_split[[j]][2],
                                          paste(original_daily_path_split[[j]][3], original_daily_path_split[[j]][4], original_daily_path_split[[j]][5], sep = "-"))
      
      
    }
    
    adjusted_daily_joined <- fs::path_join(parts = adjusted_daily_path_split)
    
    fs::file_move(path = original_daily_path, new_path = adjusted_daily_joined)
  }
}

#' List available backups on Google Drive
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return A dribble, a data frame (tibble) of the 'googledrive' package including details on the file on Google Drive.
#' @examples
#' 
#' @export

nwd_list_available_backups_on_google_drive <- function(date = NULL,
                                                       folder = "tweets",
                                                       timeframe = "daily",
                                                       language = NULL,
                                                       filetype = "rds") {
  home_d <- googledrive::drive_ls() %>% dplyr::filter(name=="networkedwebsitesdetector")
  
  all_languages_folder <- googledrive::drive_ls(path = home_d)
  
  if (is.null(language)==TRUE) {
    language <- all_languages_folder$name
  }
  
  purrr::map_dfr(.x = language,
                 .f = function (i) {
                   language_folder_d <-  all_languages_folder%>% 
                     dplyr::filter(name==i)
                   
                   all_types_folder_d <- googledrive::drive_ls(path = language_folder_d)
                   
                   type_folder_d <- all_types_folder_d %>%
                     dplyr::filter(name==folder)
                   
                   ## year folder
                   
                   all_years_folder_d <- googledrive::drive_ls(path = type_folder_d)
                   
                   years <- all_years_folder_d$name
                   
                   purrr::map_dfr(.x = years, 
                                  .f = function(x) {
                                    year_folder_d <- all_years_folder_d %>%
                                      dplyr::filter(name==as.character(x))
                                    
                                    googledrive::drive_ls(path = year_folder_d) %>% 
                                      dplyr::filter(stringr::str_detect(string = name,
                                                                        pattern = paste0(filetype, "_", timeframe, ".tar.gz")))
                                    
                                  })
                   
                 })
  
}


