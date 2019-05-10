#' Archive data in compressed files
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export


nwd_archive <- function(date = Sys.Date(),
                        folder = "tweets",
                        timeframe = "monthly",
                        language = "it",
                        filetype = "rds") {
  
  date <- as.Date(date)
  year <- lubridate::year(x = date)
  month <- lubridate::month(x = date)
  day <- lubridate::day(x = date)
  
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
        files = fs::dir_ls(path =  fs::path(folder, year, month),
                           recurse = TRUE,
                           type = "file",
                           glob = paste0("*", language, ".", filetype)),
        tar = Sys.which("tar"),
        compression = "gzip")
    
  }
  archived_file_location
}



#' Backup archived files to googledrive
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_backup_to_googledrive <- function(date = Sys.Date(),
                                      folder = "tweets",
                                      timeframe = "monthly",
                                      language = "it",
                                      filetype = "rds",
                                      show_filenames_only = FALSE) {
  
  file_locations_to_upload <- fs::dir_ls(path = fs::path("archive", language, folder, lubridate::year(date)),
                                         recurse = FALSE,
                                         type = "file",
                                         glob = paste0("*_",
                                                       language, 
                                                       "_", 
                                                       folder, 
                                                       "_",
                                                       filetype,
                                                       "_", 
                                                       timeframe, ".tar.gz"))
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
  
  
  year <- lubridate::year(Sys.Date())
  
  ## year folder
  
  all_years_folder_d <- googledrive::drive_ls(path = type_folder_d)
  
  ## year folder
  year_folder_d <- all_years_folder_d %>%
    dplyr::filter(name==as.character(year))
  
  if (nrow(year_folder_d)==0) {
    year_folder_d <- googledrive::drive_mkdir(name = as.character(year),
                                              parent = type_folder_d)
  } else if (nrow(year_folder_d)==1) {
    # do nothing
  } else {
    stop("networkedwebsitesdetector should find just one folder type with the same year Please delete if you have more than one.")
  }
  
  year_folder_contents_d <- googledrive::drive_ls(path = year_folder_d)
  
  
  if(nrow(year_folder_contents_d)==0) {
    purrr::walk(.x = file_locations_to_upload,
                .f = function(x) googledrive::drive_upload(media = x, path = year_folder_d))
  } else {
    purrr::walk(.x = file_locations_to_upload[is.element(el = year_folder_contents_d$name, set = fs::path_file(file_locations_to_upload))==FALSE],
                .f = function(x) googledrive::drive_upload(media = x, path = year_folder_d))
  }
  
}


#' Download files archived in Google Drive
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_download_from_googldrive <- function(date = Sys.Date(),
                                         folder = "tweets",
                                         timeframe = "monthly",
                                         language = "it",
                                         filetype = "rds",
                                         overwrite = FALSE) {
  home_d <- googledrive::drive_ls() %>% dplyr::filter(name=="networkedwebsitesdetector")
  language_folder_d <- googledrive::drive_ls(path = home_d) %>% 
    dplyr::filter(name==language)
  
  all_types_folder_d <- googledrive::drive_ls(path = language_folder_d)
  
  type_folder_d <- all_types_folder_d %>%
    dplyr::filter(name==folder)
  
  year <- lubridate::year(Sys.Date())
  
  ## year folder
  
  all_years_folder_d <- googledrive::drive_ls(path = type_folder_d)
  
  ## year folder
  year_folder_d <- all_years_folder_d %>%
    dplyr::filter(name==as.character(year))
  
  year_folder_contents_d <- googledrive::drive_ls(path = year_folder_d)
  
  base_year_path <- fs::path("archive", language, folder, year)
  fs::dir_create(path = base_year_path, recurse = TRUE)
  
  year_folder_contents_filtered_d <- year_folder_contents_d %>% 
    dplyr::filter(stringr::str_detect(string = name,
                                      pattern = paste0(filetype, "_", timeframe, ".tar.gz")))
  
  for (i in 1:nrow(year_folder_contents_filtered_d)) {
    temp_file_d <- year_folder_contents_filtered_d %>% dplyr::slice(i)
    googledrive::drive_download(file = temp_file_d,
                                path = fs::path(base_year_path, temp_file_d$name),
                                overwrite = overwrite)
    
  }
  
}
