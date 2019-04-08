#' Archives folders. 
#'
#' It archives  folders in .tar.gz files, and stores them in a dated sub-folder within the `archive` sub-folder.
#' 
#' @export
#' 

archive_html <- function(language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "homepage"), recursive = FALSE, full.names = FALSE)
  }
  base_path <- file.path("data", "domains", "homepage", language)
  archive_path <- file.path("archive", "domains", "homepage", language)
  fs::dir_create(path = archive_path, recursive = TRUE)
  
  fs::dir_walk(path = base_path,
               fun = function (x) {
                 tar_filename <- file.path(archive_path, stringr::str_replace_all(string = x, pattern = stringr::fixed("/"), replacement = "_") %>% paste0(".tar.gz"))
                 if (fs::file_exists(path = tar_filename)==FALSE) {
                   tar(tarfile = tar_filename,
                       files = x,
                       compression = "gzip")
                 }
               }, 
               type = "directory")
}

#' Archives folders. 
#'
#' It archives  folders in .tar.gz files, and stores them in a dated sub-folder within the `archive` sub-folder.
#' 
#' @export
#' 

archive_screenshots <- function(language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "screenshots"), recursive = FALSE, full.names = FALSE)
  }
  base_path <- file.path("data", "domains", "screenshots", language)
  archive_path <- file.path("archive", "domains", "screenshots", language)
  fs::dir_create(path = archive_path, recursive = TRUE)
  
  fs::dir_walk(path = base_path,
               fun = function (x) {
                 tar_filename <- file.path(archive_path, stringr::str_replace_all(string = x, pattern = stringr::fixed("/"), replacement = "_") %>% paste0(".tar.gz"))
                 if (fs::file_exists(path = tar_filename)==FALSE) {
                   tar(tarfile = tar_filename,
                       files = x,
                       compression = "gzip")
                 }
               }, 
               type = "directory")
}


#' Restore archived folders. 
#'
#' It restores .tar.gz files stored in the `archive` folder.
#' 
#' @export
#' 

restore_html <- function(language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "homepage"), recursive = FALSE, full.names = FALSE)
  }
  base_path <- file.path("data", "domains", "homepage", language)
  archive_path <- file.path("archive", "domains", "homepage", language)
  
  fs::dir_walk(path = archive_path,
               fun = function (x) {
                 untar(tarfile = x)
               }, 
               type = "file")
}
