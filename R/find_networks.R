#' Find domains related to each across key identifiers
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @param run_n Number of times to go through all identifiers. For example, if a new domain is found within the network through a common fb_app_id, it may be useful to see if the new domain has any ca_pub codes with others. 
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export


find_related_domains <- function(domain,
                                 identifiers_df = load_identifiers_df(),
                                 identifiers = default_identifiers,
                                 language = NULL,
                                 run_n = 3) {
  temp_domains <- domain
  
  for (i in rep(x = identifiers, run_n)) {
    #message(i)
    if (length(temp_domains)==0) {
      temp_domains <- domain
    }
    temp <- identifiers_df %>% 
      dplyr::filter(is.element(el = identifiers_df$domain, set = temp_domains))
    
    if (nrow(temp)==0) {
      stop(paste("Domain", domain, "not available in archive"))
    }
    
    temp_alt_id <-  temp %>% 
      dplyr::select(domain, i) %>% 
      tidyr::unnest() %>% 
      dplyr::pull(i) %>%
      base::unique() 
    
    ## clean up
    if (i == "ua") {
      temp_alt_id <- temp_alt_id[is.element(el = temp_alt_id, set = default_excluded_ua)==FALSE] 
    }
    if (i == "fb_admins") {
      temp_alt_id <- temp_alt_id[is.element(el = temp_alt_id, set = default_excluded_fb_admins)==FALSE] 
    }
    if (i == "fb_app_id") {
      temp_alt_id <- temp_alt_id[is.element(el = temp_alt_id, set = default_excluded_fb_app_id)==FALSE]
    }
    
    
    if (identical(x = "", y = temp_alt_id)) {
      # do nothing
    } else if (length(temp_alt_id)==0) {
      # do nothing
    } else {
      temp_alt_id <- temp_alt_id[temp_alt_id!=""&is.na(temp_alt_id)==FALSE]
      temp_domains_pre <- temp_domains
      temp_domains_post <- c(temp_domains_pre, temp_domains_pre)
      if (length(temp_alt_id)>0) {
        while(length(temp_domains_pre)<length(temp_domains_post)) {
          temp_domains_pre <- unique(temp_domains_post)
          
          # extract all id of given type present in subset
          temp_alt <- identifiers_df %>% 
            dplyr::filter(is.element(el = domain, set = temp_domains_pre)) %>% 
            dplyr::select(domain, i) %>% 
            tidyr::unnest() %>% 
            dplyr::pull(i) %>%
            base::unique()
          
          temp_alt <- temp_alt[temp_alt!=""&is.na(temp_alt)==FALSE]
          
          if (length(temp_alt)>0) {
            temp_identifiers_df  <- identifiers_df %>% 
              dplyr::select(domain, i) %>% 
              tidyr::unnest()
            
            temp_identifiers_df <- temp_identifiers_df[temp_identifiers_df %>% dplyr::pull(i) %in% temp_alt, 1:2]
            temp_domains_post <- temp_identifiers_df %>%
              dplyr::distinct(domain) %>%
              dplyr::pull(domain)
          } else {
            temp_domains_post <- temp_domains_pre
          }
          
        }
        
      }
      temp_domains <- c(unique(temp_domains_pre, temp_domains_post))
    }
    
  }
  temp_domains
}

#' Add a network_id column to identifiers_df
#'
#' @return A data.frame (a tibble) including a network_id column grouping all domains that have elements in common.
#' @examples
#' 
#' @export
#' 
add_network_id <- function(identifiers_df,
                           temporary_files = NULL) {
  identifiers_df$network_id <- NA
  if (is.null(temporary_files)==FALSE) {
    store_when <- cumsum(rep(round(nrow(identifiers_df)/temporary_files), 100))
  }
  
  fs::dir_create(path = file.path("temp", "identifiers"), recursive = TRUE)
  pb <- dplyr::progress_estimated(n = nrow(identifiers_df), min_time = 1)
  for (i in 1:nrow(identifiers_df)) {
    pb$tick()$print()
    if (is.na(identifiers_df$network_id[identifiers_df$domain==identifiers_df$domain[i]])) {
      related_domains <- find_related_domains(domain = identifiers_df$domain[i], identifiers_df = identifiers_df)
      identifiers_df$network_id[identifiers_df$domain %in% related_domains] <- i
      if (is.null(temporary_files)==FALSE) {
        if (is.element(i, store_when)) {
          saveRDS(object = identifiers_df,
                  file = file.path("temp", "identifiers", paste0("identifiers_df_network_id-", i, ".rds")))
          message(paste("\nTemporary files stored after processing", i, "lines"))
        }
          
      }
    }
  }
  return(identifiers_df)
}
