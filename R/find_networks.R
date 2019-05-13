#' Find domains related to each across key identifiers
#'
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @param run_n Number of times to go through all identifiers. For example, if a new domain is found within the network through a common fb_app_id, it may be useful to see if the new domain has any ca_pub codes with others. 
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export


nwd_find_related_domains <- function(domain,
                                     identifiers_df = load_identifiers_df(),
                                     identifiers = default_identifiers,
                                     language = NULL,
                                     run_n = 3) {
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("identifiers"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  for (i in language) {
    temp_domains <- domain
    
    for (j in rep(x = identifiers, run_n)) {
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
        dplyr::select(domain, j) %>% 
        tidyr::unnest() %>% 
        dplyr::pull(j) %>%
        base::unique() 
      
      ## clean up
      if (j == "ua") {
        temp_alt_id <- temp_alt_id[is.element(el = temp_alt_id, set = default_excluded_ua)==FALSE] 
      }
      if (j == "fb_admins") {
        temp_alt_id <- temp_alt_id[is.element(el = temp_alt_id, set = default_excluded_fb_admins)==FALSE] 
      }
      if (j == "fb_app_id") {
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
              dplyr::select(domain, j) %>% 
              tidyr::unnest() %>% 
              dplyr::pull(j) %>%
              base::unique()
            
            ## clean up
            if (j == "ua") {
              temp_alt <- temp_alt_id[is.element(el = temp_alt_id, set = default_excluded_ua)==FALSE] 
            }
            if (j == "fb_admins") {
              temp_alt <- temp_alt_id[is.element(el = temp_alt_id, set = default_excluded_fb_admins)==FALSE] 
            }
            if (j == "fb_app_id") {
              temp_alt <- temp_alt_id[is.element(el = temp_alt_id, set = default_excluded_fb_app_id)==FALSE]
            }
            
            temp_alt <- temp_alt[temp_alt!=""&is.na(temp_alt)==FALSE]
            
            if (length(temp_alt)>0) {
              temp_identifiers_df  <- identifiers_df %>% 
                dplyr::select(domain, j) %>% 
                tidyr::unnest()
              
              temp_identifiers_df <- temp_identifiers_df[temp_identifiers_df %>% dplyr::pull(j) %in% temp_alt, 1:2]
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
  }
  temp_domains
}

#' Add a network_id column to identifiers_df
#'
#'
#' @param identifiers_df A data frame, typically created with extract extract_identifiers() and loaded with load_identifiers_df().
#' @param temporary_files Defaults to NULL. If given, it should be an integer corresponding to the number of temporary files to create while processing the data. E.g. If if there are 100.000 rows to process, and temporary_files is set to 100, a total of 100 files will be stored in the temporary folder, one each 1.000 rows.
#' @return A data.frame (a tibble) including a network_id column grouping all domains that have elements in common.
#' @examples
#' 
#' @export
#' 
nwd_add_network_id <- function(identifiers_df = nwd_load_identifiers_df(),
                               language = NULL, 
                               temporary_files = NULL,
                               continue_from_temporary = FALSE) {
  
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("identifiers"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  for (i in language) {
    if (continue_from_temporary==TRUE) {
      temp_files <- fs::dir_ls(path = file.path("temp", "identifiers", i))
      identifiers_df <- readRDS(file = temp_files[temp_files %>%
                                                    stringr::str_extract(pattern = "[[:digit:]]+.rds") %>%
                                                    stringr::str_remove(pattern = stringr::fixed(".rds")) %>%
                                                    as.integer() %>%
                                                    which.max()])
    }
    
    if (is.element(el = "network_id", set = colnames(identifiers_df))) {
      # do nothing
    } else {
      identifiers_df$network_id <- NA
    }
    
    if (is.null(temporary_files)==FALSE) {
      store_when <- cumsum(rep(round(nrow(identifiers_df)/temporary_files), temporary_files))
    }
    
    fs::dir_create(path = file.path("temp", "identifiers", i), recurse = TRUE)
    pb <- dplyr::progress_estimated(n = nrow(identifiers_df), min_time = 1)
    for (j in 1:nrow(identifiers_df)) {
      pb$tick()$print()
      if (is.na(identifiers_df$network_id[identifiers_df$domain==identifiers_df$domain[j]])) {
        related_domains <- find_related_domains(domain = identifiers_df$domain[j], identifiers_df = identifiers_df)
        identifiers_df$network_id[identifiers_df$domain %in% related_domains] <- j
        if (is.null(temporary_files)==FALSE) {
          if (is.element(j, store_when)) {
            saveRDS(object = identifiers_df,
                    file = file.path("temp", "identifiers", paste0("network_df-", j, ".rds")))
            message(paste("\nTemporary files stored after processing", j, "lines"))
          }
          
        }
      }
    }
    fs::dir_create(path = fs::path("network_df", i), recurse = TRUE)
    saveRDS(object = identifiers_df,
            file = fs::path("network_df",
                            i,
                            paste0(Sys.Date(),
                                   "_",
                                   i, 
                                   "network_df.rds")))
    return(identifiers_df)
  }
}
