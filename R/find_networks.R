#' Find domains related to each across key identifiers
#'
#' @param identifiers_df A dataframe of indentifiers in the long format, typically created with `nwd_load_identifiers_df()`.
#' @param language A character vector of language two letter codes. Defaults to NULL. If NULL, processes available languages.
#' @param run_n Number of times to go through all identifiers. For example, if a new domain is found within the network through a common fb_app_id, it may be useful to see if the new domain has any ca_pub codes with others. 
#' @return Nothing, used for its side effects. 
#' @examples
#' 
#' @export


nwd_find_network <- function(domain,
                             identifiers_df = nwd_load_identifiers_df(),
                             identifiers = NULL,
                             language = NULL,
                             max_run_n = 10) {
  if (is.null(identifiers)) {
    identifiers <- unique(identifiers_df$identifier)
  }
  
  identifiers_df$network_id <- NULL
  
  identifiers_df <- identifiers_df %>% 
    dplyr::mutate(id = dplyr::if_else(condition = id=="",
                                      true = as.character(NA),
                                      false = paste(identifier, id, sep = "_"))) %>% 
    tidyr::drop_na()
  
  ## clean up
  # TODO allow customisation
  clean_up_id <- c(paste0("ua_", default_excluded_ua$id),
                   paste0("fb_admins_", default_excluded_fb_admins$id),
                   paste0("fb_app_id_", default_excluded_fb_app_id$id))
  
  identifiers_df$id[is.element(el = identifiers_df$id, set = clean_up_id)] <- NA
  
  identifiers_df <- identifiers_df %>%
    tidyr::drop_na() 
  
  if (is.null(language)) {
    if (fs::file_exists(path = fs::path("identifiers"))==FALSE) {
      language <-  fs::dir_ls(path = fs::path("identifiers"),
                              recurse = FALSE,
                              type = "directory") %>% 
        fs::path_file()
    } else {
      stop("Language not given and `identifiers` folder not found locally.")
    }
  }
  
  for (i in language) {
    temp_domains <- domain
    
    post_identifier_df <- identifiers_df %>% 
      dplyr::filter(is.element(el = domain, set = temp_domains))
    
    if (nrow(post_identifier_df)==0) {
      warning(paste("Domain", domain, "not available in archive"))
    }
    pre <- nrow(post_identifier_df)
    post <- nrow(post_identifier_df)+1
    x <- 0
    while (pre<post&x<max_run_n) {
      pre <- nrow(post_identifier_df)
      post_identifier_df <- identifiers_df %>% 
        dplyr::filter(is.element(el = id,
                                 set = unique(post_identifier_df$id))|is.element(el = domain,
                                                                                 set = unique(post_identifier_df$domain)))
      post <- nrow(post_identifier_df)
      x = x+1
    }
    return(post_identifier_df %>% 
             dplyr::mutate(id = stringr::str_remove(string = id, pattern = paste0(identifier, "_"))))
  }
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
                               identifiers = NULL,
                               temporary_files = NULL,
                               continue_from_temporary = FALSE) {
  
  if (is.null(identifiers)) {
    identifiers <- unique(identifiers_df$identifier)
  }
  
  if (is.null(language)) {
    language <-  fs::dir_ls(path = fs::path("identifiers"),
                            recurse = FALSE,
                            type = "directory") %>% 
      fs::path_file()
  }
  
  for (i in language) {
    fs::dir_create(path = file.path("nwd_temp_identifiers", i), recurse = TRUE)
    if (continue_from_temporary==TRUE) {
      temp_files <- fs::dir_ls(path = fs::path("nwd_temp_identifiers", i), type = "file")
      if (length(temp_files)>0) {
        identifiers_df <- readRDS(file = temp_files[temp_files %>%
                                                      stringr::str_extract(pattern = "[[:digit:]]+.rds") %>%
                                                      stringr::str_remove(pattern = stringr::fixed(".rds")) %>%
                                                      as.integer() %>%
                                                      which.max()])
      }
    }
    
    if (is.element(el = "network_id", set = colnames(identifiers_df))) {
      # do nothing
    } else {
      identifiers_df$network_id <- NA
    }
    
    if (is.null(temporary_files)==FALSE) {
      store_when <- cumsum(rep(round(nrow(identifiers_df)/temporary_files), temporary_files))
    }
    
    pb <- dplyr::progress_estimated(n = nrow(identifiers_df), min_time = 1)
    for (j in seq_along(identifiers_df$domain)) {
      pb$tick()$print()
      
      if (is.na(identifiers_df$network_id[j])==TRUE) {
        full_network <- nwd_find_network(domain = identifiers_df$domain[j], identifiers_df = identifiers_df)
        
        if (nrow(full_network)>0) {
          identifiers_df$network_id[identifiers_df$domain %in% unique(full_network$domain)] <- min(j, identifiers_df$network_id[identifiers_df$domain %in% unique(full_network$domain)], na.rm = TRUE)
        } else {
          identifiers_df$network_id[identifiers_df$domain==identifiers_df$domain[j]] <- j
        }
        
      }
      if (is.null(temporary_files)==FALSE) {
        if (is.element(j, store_when)) {
          saveRDS(object = identifiers_df,
                  file = file.path("nwd_temp_identifiers", i, paste0("network_df-", j, ".rds")))
          message(paste("\nTemporary files stored after processing", j, "lines"))
        }
      }
    }
  }
  fs::dir_create(path = fs::path("network_df", i, Sys.Date()), recurse = TRUE)
  saveRDS(object = identifiers_df,
          file = fs::path("network_df",
                          i,
                          Sys.Date(),
                          paste0(Sys.Date(),
                                 "_",
                                 i, 
                                 "_network_df.rds")))
  return(identifiers_df)
}


#' Create a graph of the network of one or more domains
#'
#'
#' @param identifiers_df A data frame, typically created with extract extract_identifiers() and loaded with load_identifiers_df().
#' @return A data.frame (a tibble) including a network_id column grouping all domains that have elements in common.
#' @examples
#' 
#' @export
#' 
#' 

nwd_create_domain_graph_all_connections <- function(domains,
                                                    identifiers_df = nwd_load_identifiers_df(),
                                                    identifiers = default_identifiers) {
  temp_identifiers_df <- identifiers_df %>% 
    dplyr::filter(is.element(el = domain, set = domains)) 
  
  
  all_connections <- purrr::map_dfr(.x = identifiers, .f = function(x) temp_identifiers_df %>% 
                                      dplyr::select(domain, x) %>% 
                                      tidyr::unnest() %>% 
                                      dplyr::distinct() %>% 
                                      magrittr::set_colnames(c("domain", "identifier")) %>% 
                                      dplyr::transmute(source = domain, destination = domain, identifier) %>% 
                                      dplyr::group_by(identifier) %>% 
                                      tidyr::complete(source, destination) %>% 
                                      dplyr::ungroup() %>%
                                      dplyr::arrange(identifier) %>% 
                                      dplyr::select(source, destination, identifier) %>% 
                                      dplyr::filter(source!=destination) %>% 
                                      dplyr::filter(identifier!="")) %>% 
    dplyr::group_by(source, destination) %>% 
    dplyr::add_count()
  
  
  
  edges <- all_connections %>% 
    dplyr::left_join(nodes, by = c("source" = "label")) %>% 
    dplyr::rename(from = id)
  
  edges <- edges %>% 
    dplyr::left_join(nodes, by = c("destination" = "label")) %>% 
    dplyr::rename(to = id)
  
  identifier_tidy <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
  identifier_tidy
}


#' Create a graph of the network of one or more domains
#'
#' @param network A character vector of length one, typically a domain name, or a domain generated with `nwd_find_network()`.
#' @param identifiers_df A data frame, typically created with extract extract_identifiers() and loaded with load_identifiers_df().
#' @param language Defaults to NULL. If given, looks only for network within a given language.
#' @return A data.frame (a tibble) including a network_id column grouping all domains that have elements in common. If `plot = TRUE`, a ggplot objext.
#' @examples
#' 
#' @export
#' 
#' 

nwd_show_network <- function(network,
                             identifiers_df = nwd_load_identifiers_df(),
                             identifiers = NULL,
                             language = NULL,
                             only_shared_identifiers = TRUE, 
                             plot = TRUE) {
  if (is.character(network) == TRUE & length(network)==1) {
    network <- nwd_find_network(domain = network,
                                identifiers_df = identifiers_df,
                                identifiers = identifiers,
                                language = language)
  }
  
  if (is.null(identifiers)) {
    identifiers <- unique(network$identifier)
  }

  all_connections <- network %>% 
    dplyr::transmute(source = domain, destination = paste0(identifier, ": ", id)) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(destination) %>% 
    dplyr::add_count() %>% 
    dplyr::ungroup()
  
  if (only_shared_identifiers==TRUE) {
    all_connections <- all_connections %>% 
      dplyr::filter(n>1)
  }
  
  
  nodes <- dplyr::bind_rows(domains = all_connections %>% 
                              dplyr::distinct(source) %>% 
                              dplyr::rename(label = source),
                            
                            identifiers = all_connections %>% 
                              dplyr::distinct(destination)%>% 
                              dplyr::rename(label = destination), .id = "type") %>% 
    tidyr::drop_na() %>% 
    tibble::rowid_to_column("id")
  
  
  
  edges <- all_connections %>% 
    dplyr::left_join(nodes, by = c("source" = "label")) %>% 
    dplyr::rename(from = id)
  
  edges <- edges %>% 
    dplyr::left_join(nodes, by = c("destination" = "label")) %>% 
    dplyr::rename(to = id) %>% 
    dplyr::filter((is.na(from)|is.na(to))==FALSE)
  
  identifier_tidy <- tidygraph::tbl_graph(nodes = nodes,
                                          edges = edges,
                                          directed = TRUE)
  if (plot==TRUE) {
    identifier_tidy %>% ggraph::ggraph(layout = "nicely") +
      #  geom_edge_arc(colour = "gray") +
      #ggraph::geom_edge_link(ggplot2::aes(colour = destination)) +
      ggraph::geom_edge_link(colour = "gray") +
      ggraph::geom_node_point(mapping = ggplot2::aes(color = type), show.legend = FALSE) +
      ggraph::geom_node_text(ggplot2::aes(label = label), repel = TRUE) +
      ggraph::theme_graph()
  } else {
    identifier_tidy
  }
}
