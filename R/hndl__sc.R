hndl__sc__client_data <- function(ses) {
  trace_func_entry()
  s <- ses$session
  debugmsg(
    "protocol=", s$clientData$url_protocol, ", ",
    "hostname=", s$clientData$url_hostname, ", ",
    "port=", s$clientData$url_port, ", ",
    "pathname=", s$clientData$url_pathname, ", ",
    "search=", s$clientData$url_search, ", ",
    "hash_initial=", s$clientData$url_hash_initial, ", ",
    "hash=", s$clientData$url_hash,
    sep = ""
  )
}


hndl__sc__login_cookie <- function(ses) { # run when_valid_cookie_is_present
  trace_func_entry()
  ses = ses # force evaluation of ses
  debugmsg(
    "dput2(ses$input$cookies$user_session):",
    dput2(ses$input$cookies$user_session)
  )
  debugmsg("dput2(ses$rv$user$id):", dput2(ses$rv$user$id))
  if (
    (is.null(ses$rv$user$id)) &&
    (!(is.null(ses$input$cookies$user_session))) &&
    (!(nchar(ses$input$cookies$user_session) == 0))
  ){
    cookie_df <- db__get_valid_cookies()
    cookie_df <- cookie_df[cookie_df$session_id == ses$input$cookies$user_session, ]
    if (nrow(cookie_df) == 1) {
      debugmsg("Cookie valid, updating ses$rv$user$id")
      ses$rv$user$id <- cookie_df$user_id
      debugmsg("dput2(ses$rv$user$id):", dput2(ses$rv$user$id))
      debugmsg("Also updating cookie to reset expiry date.")
      debugmsg("New value:", ses$session$token)
      set_login_cookie(
        ses = ses,
        user_id = ses$rv$user$id,
        session_id = ses$session$token
      )
    } else {
      browser__rm_cookie(ses, "user_session")
      debugmsg("Cookie invalid, removed it")
      debugmsg("dput2(cookie_df):", dput2(cookie_df))
    }
  }
  ses$rv$login_checked$cookie <- TRUE
}


hndl__sc__auth_completed <- function(ses) {
  trace_func_entry("hndl__sc__auth_completed")
  u_id <- if (is.null(ses$rv$user$id)) "public" else ses$rv$user$id
  u_df <- db__get_table("users")
  mug_df <- db__get_table("mapping_users_groups")
  u_row <- u_df[which(u_df$id == ses$rv$user$id), ]
  g_ids <- mug_df$group_id[mug_df$user_id == u_id]
  g_ids <- unique(c(g_ids, "public"))
  ses$rv$user$id <- u_id
  ses$rv$user$group_ids <- g_ids
  ses$rv$user$is_admin <- "admin" %in% g_ids
  ses$rv$user$display_name <- u_row$display_name
  ses$rv$user$github_id <- u_row$github_id
  ses$rv$user$avatar_url <- u_row$avatar_url
  ses$rv$user$password <- u_row$password
  ses$rv$user$gitlab_id <- u_row$gitlab_id
  ses$rv$user$google_id <- u_row$google_id
  ses$rv$user$spanglab_gitlab_id <- u_row$spanglab_gitlab_id
  ses$rv$user$spanglab_auth_id <- u_row$spanglab_auth_id
  debugmsg("dput2(ses$rv$user$id):", dput2(ses$rv$user$id))
  debugmsg("dput2(ses$input$login_jscookie):", dput2(ses$input$cookies$user_session))
  debugmsg("dput2(ses$rv$user$group_ids):", dput2(ses$rv$user$group_ids))
  init__rv_dataset(ses)
  init__rv_model(ses)
  ses$rv$auth$completed <- TRUE
}


hndl__sc__user_id <- function(ses) {
  trace_func_entry()
  if (is.null(ses$rv$user$id) || ses$rv$user$id == "public") {
    return()
  } else {
    x <- db__get_table("Appstate")
    idx <- x$user_id == ses$rv$user$id & x$resource_id == "url"
    last_url <- x[idx, "resource_value"]
    if (grepl("&redirect=false", ses$const$url_search)) {
      debugmsg("URL contains `redirect=false`. Skipping redirection.")
    } else if (length(last_url) == 0) {
      debugmsg("No existing `Appstate$resource_value`. Skipping redirection.")
    } else {
      if (length(last_url) > 1) {
        warnmsg("Multiple URLs in `Appstate$resource_value`. Using latest.")
      }
      new_url <- paste0(last_url[length(last_url)], "&redirect=false")
      debugmsg("Restoring previous appstate by redirecting to url:", new_url)
    }
    ses$output$web_app <- redirection_to(new_url)
    # TODO: improve. Instead of redirecting, we should update the inputs directly
    # because redirecting has the overhead of going through the whole startup
    # process again.
  }
}
