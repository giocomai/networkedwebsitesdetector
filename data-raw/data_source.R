default_excluded_fb_admins <- c("YOUR USER ID",
                                "Facebook Admin ID here",
                                "FACEBOOK-ADMIN-ID")

usethis::use_data(default_excluded_fb_admins, overwrite = TRUE)

default_excluded_fb_app_id <- c("Facebook App ID here", 
                                "966242223397117") #default app id

usethis::use_data(default_excluded_fb_app_id, overwrite = TRUE)

default_excluded_ua <- c("UA-52447") #wordpress

usethis::use_data(default_excluded_ua, overwrite = TRUE)

default_identifiers <- c("ua",
                         "ca_pub",
                         "fb_admins",
                         "fb_page_id",
                         "fb_app_id")

usethis::use_data(default_identifiers, overwrite = TRUE)
