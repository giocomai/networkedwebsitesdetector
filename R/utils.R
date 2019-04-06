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
#' @return Nothing, used for its side effects. 
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
    if (sum(available_files$available, available_files$available)>0) {
      return(TRUE)
    } else (
      return(FALSE)
    )
  }
}
