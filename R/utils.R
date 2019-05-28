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
#' @param since A date. `nwd_check_if_exists()` returns TRUE only if the given link has been downloaded or (failed to download) after the given date.
#' @return If `simplify` is TRUE, it returns a logical vector of length 1, either TRUE or FALSE. If `simplify == FALSE`, it returns a data frame with details on the availability of files related to the given domain. 
#' @examples
#' 
#' @export
#' 

nwd_check_if_exists <- function(domain,
                                type = "homepage",
                                language = NULL,
                                since = Sys.Date()-91,
                                simplify = TRUE) {
  
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("homepage"), recursive = FALSE, full.names = FALSE)
    if (length(language)!=1) {
      stop("More than one language found. Please select one language.")
    }
  }
  
  base_path <- file.path(type, language)
  base_path_failed <- file.path(paste0(type, "_failed"), language)
  
  available_files <- fs::dir_info(path = c(base_path, base_path_failed),
                                  recurse = FALSE,
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
#' @param language A character vector of language two letter codes.
#' @return A data.frame (a tibble), typically generated with `extract_identifiers()`
#' @examples
#' 
#' @export
#' 

nwd_load_latest_identifiers_df <- function(language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("identifiers_long"), recursive = FALSE, full.names = FALSE)
  }
  base_path <- file.path("identifiers_long", language)
  readRDS(file = fs::dir_ls(fs::dir_ls(path = base_path, type = "file") %>% tail(1)))
}

#' Load all identifiers_df
#'
#' @param language A character vector of language two letter codes. 
#' @param long Logical, defaults to TRUE. If TRUE, returns a tidy data drame in the long format. If FALSE, return a wide dataframe with a column for each identifiers, and id included as lists. 
#' @param store Logical, defaults to TRUE. Should the output be stored locally as a dated file in the `identifiers_long/language` folder?
#' @param cache Logical, defaults to TRUE. If already processed on the same date, should it just load the lastest stored files?
#' @param keep_duplicates Logical, defaults to FALSE. If FALSE, if the same web page has been downloaded on multiple dates, it keeps only the first instance when a given identifier was found. If TRUE, it keeps one row per available date.
#' @return A data.frame (a tibble), made of data frames typically generated with `extract_identifiers()`
#' @examples
#' 
#' @export
#' 
nwd_load_identifiers_df <- function(language = NULL,
                                    long = TRUE, 
                                    store = TRUE,
                                    cache = TRUE,
                                    keep_duplicates = FALSE) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("identifiers"), recursive = FALSE, full.names = FALSE)
  }
  base_path <- file.path("identifiers", language)
  today_identifiers_df_long_location <- fs::path("identifiers_long", language, paste0(Sys.Date(), "_identifiers_df_long.rds"))
  
  if (cache == TRUE & long==TRUE & fs::file_exists(path = today_identifiers_df_long_location)) {
    return(readRDS(file = today_identifiers_df_long_location))
  }
  file_list <- fs::dir_ls(path = base_path, recurse = TRUE, type = "file", glob = "*.rds")
  message("Step 1: Load identifiers\n")
  pb <- dplyr::progress_estimated(length(file_list))
  identifiers_df <- purrr::map_dfr(.x = file_list,
                                   .f = function (x) {
                                     pb$tick()$print()
                                     readRDS(file = x)
                                     }, .id = "date") %>% 
    dplyr::mutate(date = as.Date(fs::path_file(fs::path_dir(date))))
  if (long == TRUE) {
    message("\nStep 2: Convert into long data frame\n")
    identifiers_to_process <- colnames(identifiers_df)[!is.element(colnames(identifiers_df), c("date", "domain", "network_id"))]
    pb <- dplyr::progress_estimated(length(identifiers_to_process))
    identifiers_df_long <- purrr::map_dfr(.x = identifiers_to_process,
                                          .f = function (x) {
                                            pb$tick()$print()
                                            temp <- identifiers_df %>% 
                                              dplyr::select(date, domain, x) %>% 
                                              tidyr::unnest(cols = x, keep_empty = TRUE)
                                            # FIX remove if when tidyr bug resolved
                                            if (ncol(temp)>2) {
                                              temp %>%
                                                dplyr::rename(id = x) %>% 
                                                dplyr::transmute(date, domain, identifier = x, id) %>% 
                                                dplyr::distinct() %>% 
                                                tidyr::drop_na()
                                            }
                                          }) %>% 
      dplyr::arrange(date, domain, identifier, id)
    if (keep_duplicates==FALSE) {
      identifiers_df_long <- identifiers_df_long %>% 
        dplyr::distinct(domain, identifier, id, .keep_all = TRUE)
    }
    if (store == TRUE & long==TRUE) {
      fs::dir_create(path = fs::path("identifiers_long", language), recurse = TRUE)
      
      saveRDS(object = identifiers_df_long,
              file = today_identifiers_df_long_location)
    }
    return(identifiers_df_long)
  } else {
    return(identifiers_df)
  }
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

nwd_clean_files <- function(min_size = 0,
                            max_size = 1e8,
                            remove_exceeding = FALSE,
                            language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("homepage"), recursive = FALSE, full.names = FALSE)
  }
  file_info <- fs::dir_info(path = file.path("homepage", language), recurse = TRUE, type = "file")
  
  file_exceeding <- file_info %>%
    dplyr::filter(size <= fs::as_fs_bytes(x = min_size) | size > fs::as_fs_bytes(x = max_size))
  
  if (remove_exceeding==TRUE) {
    file_exceeding %>% dplyr::pull(path) %>% fs::file_delete()
  }
  file_exceeding
}


#' List available backups
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return A tibble (a data frame) with three columns: name, date, location.
#' @examples
#' 
#' @export

nwd_list_available_backups <- function(date = NULL,
                                       folder = "tweets",
                                       timeframe = "daily",
                                       language = NULL,
                                       filetype = "rds") {
  
  if (is.null(language)==TRUE) {
    language <- fs::dir_ls(fs::path("archive")) %>% fs::path_file()
  }
  tibble::tibble(location =   purrr::map(.x = language,
                                         .f = function (i) {
                                           fs::dir_ls(path = fs::dir_ls(path = fs::path("archive", i)) %>%
                                                        stringr::str_subset(pattern = folder),
                                                      recurse = TRUE,
                                                      type = "file",
                                                      glob = paste0("*", i, "_", folder, "_", filetype, "_", timeframe, ".tar.gz"))
                                         }) %>% unlist()) %>% 
    dplyr::transmute(name = fs::path_file(location), date = as.Date(stringr::str_extract(string = location, pattern = "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]")), location)
  
}
