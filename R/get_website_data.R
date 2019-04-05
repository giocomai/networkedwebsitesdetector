#' Get screenshots and place them in relevant subfolder
#'
#' @param url A character vector of one or more urls. 
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

get_screenshot <- function(url, width = 1280, height = 1280) {
  dir.create(path = "data", showWarnings = FALSE)
  dir.create(path = file.path("data", "domains"), showWarnings = FALSE)
  dir.create(path = file.path("data", "domains", "screenshots"), showWarnings = FALSE)
  today_path <- file.path("data", "domains", "screenshots", Sys.Date())
  dir.create(path = today_path, showWarnings = FALSE)
  
  pb <- dplyr::progress_estimated(length(url))
  purrr::walk(.x = paste0("http://", url),
              .f =  function(x) {pb$tick()$print()
                webshot::webshot(url = x,
                                 file = file.path(today_path, paste0(stringr::str_remove(string = x, pattern = stringr::fixed("http://")), ".png")),
                                 vwidth = width,
                                 vheight = height,
                                 cliprect = "viewport")})
}
