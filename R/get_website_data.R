#' Get screenshots and place them in relevant subfolder
#'
#' @param url A character vector of one or more urls. 
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

get_screenshot <- function(url, width = 1280, height = 1280, language = NULL) {
  
  dir.create(path = "data", showWarnings = FALSE)
  dir.create(path = file.path("data", "domains"), showWarnings = FALSE)
  dir.create(path = file.path("data", "domains", "screenshots"), showWarnings = FALSE)
  dir.create(path = file.path("data", "domains", "screenshots", language), showWarnings = FALSE)
  dir.create(path = file.path("data", "domains", "screenshots_failed"), showWarnings = FALSE)
  dir.create(path = file.path("data", "domains", "screenshots_failed", language), showWarnings = FALSE)
  
  today_path <- file.path("data", "domains", "screenshots", language, Sys.Date())
  today_path_screenshots_failed <- file.path("data", "domains", "screenshots_failed", language, Sys.Date())
  dir.create(path = today_path, showWarnings = FALSE)
  dir.create(path = today_path_screenshots_failed, showWarnings = FALSE)
  
  pb <- dplyr::progress_estimated(length(url))
  purrr::walk(.x = url,
              .f =  function(x) {pb$tick()$print()
                check <- check_if_exists(domain = x, type = "screenshots")
                if (sum(check$failed))
                tryCatch({webshot::webshot(url = paste0("http://", x),
                                           file = file.path(today_path, paste0(x, ".png")),
                                           vwidth = width,
                                           vheight = height,
                                           cliprect = "viewport")},
                         error=function(e){
                           
                           readr::write_file(x = paste0("Could not download ", x),
                                             path = file.path(today_path_screenshots_failed, paste0(stringr::str_remove(string = x, pattern = stringr::fixed("http://")), ".txt")))
                           
                           message(paste0("Could not download ", x, ": "),
                                   conditionMessage(e), "\n")})
              }
  )
}
