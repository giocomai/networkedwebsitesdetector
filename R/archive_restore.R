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
                        filetype = "rds", 
                        to_googledrive = FALSE) {
  
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
