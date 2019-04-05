#' Download homepages by url in relevant sub-folder. 
#' 
#' Download homepages by url in relevant sub-folder, divide by date of download. By default, does not overwrite. 
#'
#' @param url A character vector of one or more domain names. 
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export
#' 

download_homepage <- function(domain, language) {
  dir.create(file.path("data", "domains"), showWarnings = FALSE)
  dir.create(file.path("data", "domains", "homepage"), showWarnings = FALSE)
  dir.create(file.path("data", "domains", "homepage_failed"), showWarnings = FALSE)
  
  for (i in languages) {
    dir.create(file.path("data", "domains", "homepage", i), showWarnings = FALSE)
    today_path_homepage <- file.path("data", "domains", "homepage", i, Sys.Date())
    dir.create(today_path_homepage, showWarnings = FALSE)
    today_path_homepage_failed <- file.path("data", "domains", "homepage_failed", i, Sys.Date())
    dir.create(today_path_homepage_failed, showWarnings = FALSE)
    
    for (j in sample(all_domains)) {
      filename <- file.path(today_path_homepage, paste0(iconv(x = j, to = "ASCII//TRANSLIT"), ".html"))
      if (file.exists(filename)==FALSE) {
        if (file.exists(file.path(today_path_homepage_failed, paste0(iconv(x = j, to = "ASCII//TRANSLIT"), ".txt")))==FALSE) {
          tryCatch({download.file(url = j, destfile = filename)},
                   error=function(e){
                     
                     write_file(x = paste0("Could not download ", j),
                                path = file.path(today_path_homepage_failed, paste0(iconv(x = j, to = "ASCII//TRANSLIT"), ".txt")))
                     
                     message(paste0("Could not download ", j, ": "),
                             conditionMessage(e), "\n")})
        }
      }
    }
  }
}
