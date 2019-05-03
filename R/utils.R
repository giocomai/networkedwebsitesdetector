#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Check if corresponding information for a given domain exists for a given data type and a given time period
#'
#' @param domain A domain name (e.g. "example.com").
#' @param type Type of file to check, defaults to "homepage". Alternative options: "screenshots".
#' @return If `simplify` is TRUE, it returns a logical vector of length 1, either TRUE or FALSE. If `simplify == FALSE`, it returns a data frame with details on the availability of files related to the given domain. 
#' @examples
#' 
#' @export
#' 

check_if_exists <- function(domain,
                            type = "homepage",
                            language = NULL,
                            since = Sys.Date()-31,
                            simplify = TRUE) {
    
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "homepage"), recursive = FALSE, full.names = FALSE)
    if (length(language)!=1) {
      stop("More than one language found. Please select one language.")
    }
  }
  
  base_path <- file.path("data", "domains", type, language)
  base_path_failed <- file.path("data", "domains", paste0(type, "_failed"), language)
  
  available_files <- fs::dir_info(path = c(base_path, base_path_failed),
                                  recursive = FALSE,
                                  type = "directory") %>% 
    dplyr::transmute(date = as.Date(stringr::str_remove(string = stringr::str_remove(path, pattern = paste0(base_path_failed, "/")), pattern = paste0(base_path, "/")))) %>% 
    dplyr::distinct(date) %>% 
    dplyr::filter(date>as.Date(since)) %>% 
    dplyr::mutate(potential_file_location = file.path(base_path,
                                                      date,
                                                      paste0(domain, ".html")), 
                  potential_failed_file_location = file.path(base_path_failed,
                                                             date,
                                                             paste0(domain, ".txt"))) %>% 
    dplyr::mutate(available = fs::file_exists(path = potential_file_location), 
                  failed = fs::file_exists(path = potential_failed_file_location)) 
  
  if (simplify==FALSE) {
    return(available_files)
  } else if (simplify==TRUE) {
    if (sum(available_files$available, available_files$failed)>0) {
      return(TRUE)
    } else (
      return(FALSE)
    )
  }
}

#' Load latest identifiers_df
#'
#' @return A data.frame (a tibble), typically generated with `extract_identifiers()`
#' @examples
#' 
#' @export
#' 

load_latest_identifiers_df <- function(language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "identifiers"), recursive = FALSE, full.names = FALSE)
  }
  base_path <- file.path("data", "identifiers", language)
  readRDS(file = fs::dir_ls(fs::dir_ls(path = base_path) %>% tail(1)))
}

#' Load all identifiers_df
#'
#' @return A data.frame (a tibble), made of data frames typically generated with `extract_identifiers()`
#' @examples
#' 
#' @export
#' 
load_identifiers_df <- function(language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "identifiers"), recursive = FALSE, full.names = FALSE)
  }
  base_path <- file.path("data", "identifiers", language)
  purrr::map_dfr(.x = fs::dir_ls(fs::dir_ls(path = base_path)), .f = readRDS)
}

#' Check if there are unusually small or unusually big files
#'
#' @param min_size Minimum size in bytes, defaults to 0 (only files of size 0 are selected).
#' @param max_size Maximum size in bytes, defaults to about 100 megabytes.
#' @param remove_exceeding Logical, defaults to FALSE. If TRUE, listed files are removed.
#' @return If remove_exceeding==FALSE, returns a data frame with reference to files exceeding given criteria. 
#' @examples
#' 
#' @export
#' 

clean_files <- function(min_size = 0,
                        max_size = 1e8,
                        remove_exceeding = FALSE,
                        language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "homepage"), recursive = FALSE, full.names = FALSE)
  }
  file_info <- fs::dir_info(path = file.path("data", "domains", "homepage", language), recursive = TRUE, type = "file")
  
  file_exceeding <- file_info %>%
    dplyr::filter(size <= fs::as_fs_bytes(x = min_size) | size > fs::as_fs_bytes(x = max_size))
  
  if (remove_exceeding==TRUE) {
    file_exceeding %>% dplyr::pull(path) %>% fs::file_delete()
  }
  file_exceeding
}

