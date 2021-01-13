default_excluded_fb_admins <- 
  tibble::tribble(~id, ~description,
                  "YOUR USER ID", "default text",
                  "Facebook Admin ID here", "default text",
                  "FACEBOOK-ADMIN-ID", "default text")

usethis::use_data(default_excluded_fb_admins, overwrite = TRUE)

default_excluded_fb_app_id <-
  tibble::tribble(~id, ~description,
                  "Facebook App ID here", "default text",
                  "966242223397117", "default app id",
                  "249643311490", "used by all wordpress.com websites")

usethis::use_data(default_excluded_fb_app_id, overwrite = TRUE)

default_excluded_ua <- 
  tibble::tribble(~id, ~description,
                  "UA-52447", "wordpress",
                  "UA-1615344", "google news",
                  "UA-7870337", "weebly",
                  "UA-5354236", "over-blog",
                  "UA-1240215", "altervista",
                  "UA-25224921", "bit.ly",
                  "UA-38185442", "peek.link", 
                  "UA-797705", "webnode.it",
                  "UA-7265702", "unclear") 

usethis::use_data(default_excluded_ua, overwrite = TRUE)

default_excluded_taboola <- 
  tibble::tribble(~id, ~description,
                  "unip", "misclassified") 

usethis::use_data(default_excluded_taboola, overwrite = TRUE)


default_excluded_df <- dplyr::bind_rows(fb_admins = default_excluded_fb_admins, 
                                        fb_app_id = default_excluded_fb_app_id,
                                        ua = default_excluded_ua,
                                        taboola = default_excluded_taboola,
                                        .id = "type") %>% 
  dplyr::select(-description)

usethis::use_data(default_excluded_df, overwrite = TRUE)


default_identifiers <- c("ua",
                         "ca_pub",
                         "fb_admins",
                         "fb_page_id",
                         "fb_app_id",
                         "taboola")

usethis::use_data(default_identifiers, overwrite = TRUE)

public_suffix_list <- tibble::tibble(list = c("wordpress.com", "altervista.org", "weebly.com", "webnode.it", "over-blog.com",
                                              readLines(con = "https://www.publicsuffix.org/list/public_suffix_list.dat"))) %>% 
  dplyr::filter(!stringr::str_detect(string = list, pattern = "//")) %>% 
  dplyr::filter(!stringr::str_detect(string = list, pattern = stringr::fixed("*"))) %>% 
  dplyr::filter(list!="")

usethis::use_data(public_suffix_list, overwrite = TRUE)

public_suffix_regex <- public_suffix_list %>% 
  dplyr::mutate(nchar = nchar(list)) %>% 
  dplyr::arrange(desc(nchar)) %>% 
  dplyr::mutate(list = paste0(".", list)) %>% 
  dplyr::mutate(list = stringr::str_replace_all(string = list,
                                                pattern = stringr::fixed("."),
                                                replacement = stringr::fixed("\\."))) %>% 
  dplyr::pull(list) %>%
  paste(collapse = "$|")

usethis::use_data(public_suffix_regex, overwrite = TRUE)

