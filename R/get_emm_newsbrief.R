#' Get current news in multiple languages from EMM newsbrief
#' 
#' Imports and cleans RSS files from [http://emm.newsbrief.eu/]
#'
#' @param languages A character vector of one or more languages as two letter codes (e.g. "en").
#' @param shuffle Logical, defaults to TRUE. If TRUE, order in which languages are processed is randomised.  
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export

nwd_get_emm_newsbrief <- function(languages = c("ar","bg","cs","da","de","el","en","es", "et","fi","fr","hr", "hu","it", "lt","lv","nl","pl","pt","ro","ru", "sk","sl","sv","sw","tr","zh"),
                              shuffle = TRUE) {
  
  if (shuffle==TRUE) {
    languages <- sample(languages)
  }
  
  for (i in languages) {
    
    base_path <- fs::path("emm_newsbrief", 
                          i,
                          as.character(Sys.Date()))
    fs::dir_create(path = base_path, recurse = TRUE)
    
    xml_location <- fs::path(base_path,
                             paste0(Sys.time(), "-emm_rtn_", i, ".xml"))
    
    download.file(url = paste0("http://emm.newsbrief.eu/rss/rss?type=rtn&language=", i, "&duplicates=false"),
                  destfile = xml_location)
    
    xml2_list <- xml2::read_xml(xml_location) %>%
      xml2::xml_find_all(xpath = "//item") %>%
      xml2::as_list()
    
    
    nrows_rss <- length(xml2_list)
    
    if (nrows_rss>0) {
      rss_df <- tibble::tibble(title = character(nrows_rss),
                               link = character(nrows_rss),
                               description = character(nrows_rss),
                               pubDate = as.POSIXct(x = rep(x = NA, nrows_rss)),
                               source = character(nrows_rss), 
                               entity = list(nrows_rss))
      
      for (i in 1:nrows_rss) {
        temp <- xml2_list %>% magrittr::extract2(i)
        
        rss_df$title[i] <- temp$title %>% as.character()
        rss_df$link[i] <- temp$link %>% as.character()
        rss_df$description[i] <- temp$description %>% as.character()
        rss_df$pubDate[i] <- temp$pubDate %>% as.character() %>% base::strptime("%a, %d %b %Y %H:%M:%S %z")
        rss_df$source[i] <- temp$source %>% as.character()
        entL <- temp[names(temp)=="entity"] 
        rss_df$entity[[i]] <- tibble::tibble(id = purrr::map(.x = entL, .f = attr, which = "id") %>% as.numeric, 
                                             name = purrr::map(.x = entL, .f = attr, which = "name") %>% as.character())
      }
      
      
      saveRDS(object = rss_df, file = xml_location %>%
                stringr::str_replace(pattern = stringr::fixed(".xml"),
                                     replacement = ".rds"))
      
    }
    
  }
  
}



