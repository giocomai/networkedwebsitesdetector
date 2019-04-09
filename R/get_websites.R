#' Download homepages by url in relevant sub-folder. 
#' 
#' Download homepages by url in relevant sub-folder, divide by date of download. By default, does not overwrite, and shuffles the order in which given domain names are downloaded.
#'
#' @param domain A character vector of one or more domain names. 
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export
#' 

get_homepage <- function(domain = NULL,
                         language = NULL,
                         shuffle = TRUE) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "homepage"), recursive = FALSE, full.names = FALSE)
  }
  
  today_path_homepage <- file.path("data", "domains", "homepage", language, Sys.Date())
  today_path_homepage_failed <- file.path("data", "domains", "homepage_failed", language, Sys.Date())
  
  fs::dir_create(path = today_path_homepage, recursive = TRUE)
  fs::dir_create(path = today_path_homepage_failed, recursive = TRUE)
  
  if (is.null(domain)) {
    all_domains <- readRDS(file = file.path("data", "domains", "domain_name", language, "all_domains.rds"))
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
                if (networkedwebsitesdetector::check_if_exists(domain = x, type = "homepage")==FALSE) {
                  tryCatch({download.file(url = x,
                                          destfile = file.path(today_path_homepage, paste0(x, ".html")))},
                           error=function(e){
                             
                             readr::write_file(x = paste0("Could not download ", x),
                                               path = file.path(today_path_homepage_failed, paste0(x, ".txt")))
                             
                             message(paste0("Could not download ", x, ": "),
                                     conditionMessage(e), "\n")})
                }
              }
  )
}
