#' Download homepages by url in relevant sub-folder. 
#' 
#' Download homepages by url in relevant sub-folder, divide by date of download. By default, does not overwrite, and shuffles the order in which given domain names are downloaded.
#'
#' @param domain A character vector of one or more domain names. 
#' @param since A date. Passed to `nwd_check_if_exists()`. Only domains that have not been downloaded since the given date are downloaded.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export
#' 

nwd_get_homepage <- function(domain = NULL,
                             language = NULL,
                             shuffle = TRUE,
                             since = Sys.Date() - 91) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("homepage"), recursive = FALSE, full.names = FALSE)
  }
  
  if (length(language)>1) {
    stop("More than one language found. Please select one language.")
  }
  
  fs::dir_create(path = fs::path("homepage", language))
  fs::dir_create(path = fs::path("homepage_failed", language))
  
  if (is.null(domain)) {
    all_domains <- nwd_extract_domains(language = language)
  } else {
    all_domains <- tibble::tibble(domain = domain)
  }
  
  if (shuffle == TRUE) {
    domain <- sample(all_domains %>% dplyr::pull(domain))
  } else {
    domain <- all_domains %>% dplyr::pull(domain)
  }
  
  pb <- dplyr::progress_estimated(length(domain))
  purrr::walk(.x = domain,
              .f =  function(x) {pb$tick()$print()
                if (networkedwebsitesdetector::nwd_check_if_exists(domain = x, type = "homepage", since = since, language = language)==FALSE) {
                  today_path_homepage <- file.path("homepage", language, Sys.Date())
                  today_path_homepage_failed <- file.path("homepage_failed", language, Sys.Date())
                  fs::dir_create(path = today_path_homepage, recurse = TRUE)
                  fs::dir_create(path = today_path_homepage_failed, recurse = TRUE)
                  tryCatch({download.file(url = x,
                                          destfile = file.path(today_path_homepage, paste0(x, ".html")))},
                           error=function(e){
                             
                             readr::write_file(x = paste0("Could not download ", x),
                                               file = file.path(today_path_homepage_failed, paste0(x, ".txt")))
                             
                             message(paste0("Could not download ", x, ": "),
                                     conditionMessage(e), "\n")})
                }
              }
  )
}
