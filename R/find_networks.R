#' Get screenshots and place them in relevant subfolder
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export


find_related_domains <- function(domain,
                                 identifiers_df,
                                 identifiers = default_identifiers,
                                 language = NULL) {
  temp_domains <- domain
  
  for (i in identifiers) {
    #message(i)
    if (length(temp_domains)==0) {
      temp_domains <- domain
    }
    
    temp <- id_df %>% 
      filter(is.element(el = domain, set = temp_domains))
    
    ## clean up
    if (i == "fb_admins") {
      temp$fb_admins[temp$fb_admins=="YOUR USER ID"|temp$fb_admins=="Facebook Admin ID here"] <- ""
    }
    if (i == "fb_app_id") {
      temp$fb_app_id[temp$fb_app_id=="Facebook App ID here"|temp$fb_app_id=="966242223397117"] <- ""
    }
    
    if (nrow(temp)==0) {
      stop(paste("Domain", domain, "not available in archive"))
    }
    
    temp_alt_id <-  temp %>% 
      select(domain, i) %>% 
      unnest() %>% 
      pull(i) %>%
      unique() 
    
    
    if (identical(x = "", y = temp_alt_id)) {
      # do nothing
    } else if (length(temp_alt_id)==0) {
      # do nothing
    } else {
      temp_alt_id <- temp_alt_id[temp_alt_id!=""&is.na(temp_alt_id)==FALSE]
      
      temp_domains_pre <- temp_domains
      temp_domains_post <- c(temp_domains_pre, temp_domains_pre)
      
      while(length(temp_domains_pre)<length(temp_domains_post)) {
        temp_domains_pre <- unique(temp_domains_post)
        
        # extract all id of given type present in subset
        temp_alt <- id_df %>% 
          filter(is.element(el = domain, set = temp_domains_pre)) %>% 
          select(domain, i) %>% 
          unnest() %>% 
          pull(i) %>%
          unique()
        
        temp_alt <- temp_alt[temp_alt!=""]
        
        if (length(temp_alt)>0) {
          temp_id_df  <- id_df %>% 
            select(domain, i) %>% 
            unnest()
          
          temp_id_df <- temp_id_df[temp_id_df %>% pull(i) %in% temp_alt, 1:2]
          temp_domains_post <- temp_id_df %>% distinct(domain) %>% pull(domain)
        } else {
          temp_domains_post <- temp_domains_pre
        }
        
      }
      temp_domains <- c(unique(temp_domains_pre, temp_domains_post))
    }
    
  }
  temp_domains
}
