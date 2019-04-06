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

get_homepage <- function(domain = NULL, language = NULL) {
  if (is.null(language)==TRUE) {
    language <- list.dirs(file.path("data", "domains", "homepage"), recursive = FALSE, full.names = FALSE)
  }
  dir.create(file.path("data", "domains"), showWarnings = FALSE)
  dir.create(file.path("data", "domains", "homepage"), showWarnings = FALSE)
  dir.create(file.path("data", "domains", "homepage_failed"), showWarnings = FALSE)
  
  if (is.null(domain)) {
    all_domains <- readRDS(file = file.path("data", "domains", "domain_name", language, "all_domains.rds"))
  } else {
    all_domains <- tibble::tibble(domain = domain)
  }
  
  dir.create(file.path("data", "domains", "homepage", language), showWarnings = FALSE)
  dir.create(file.path("data", "domains", "homepage_failed", language), showWarnings = FALSE)
  today_path_homepage <- file.path("data", "domains", "homepage", language, Sys.Date())
  dir.create(today_path_homepage, showWarnings = FALSE)
  today_path_homepage_failed <- file.path("data", "domains", "homepage_failed", language, Sys.Date())
  dir.create(today_path_homepage_failed, showWarnings = FALSE)
  
  for (j in sample(all_domains %>% dplyr::pull(domain))) {
    filename <- file.path(today_path_homepage, paste0(j, ".html"))
    if (file.exists(filename)==FALSE) {
      if (file.exists(file.path(today_path_homepage_failed, paste0(j, ".txt")))==FALSE) {
        tryCatch({download.file(url = j, destfile = filename)},
                 error=function(e){
                   
                   readr::write_file(x = paste0("Could not download ", j),
                                     path = file.path(today_path_homepage_failed, paste0(j, ".txt")))
                   
                   message(paste0("Could not download ", j, ": "),
                           conditionMessage(e), "\n")})
      }
    }
  }
}
