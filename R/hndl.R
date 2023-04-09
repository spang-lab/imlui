hndl__oauth <- function(ses) {
  trace_func_entry()
  oauth_handlers <- c(
    github = hndl__oauth__github,
    auth_spang_lab = hndl__oauth__auth_spang_lab,
    google = hndl__oauth__google,
    gitlab = hndl__oauth__gitlab,
    gitlab_spang_lab = hndl__oauth__gitlab_spang_lab
  )
  oauth_providers <- names(oauth_handlers)
  debugmsg("dput2(ses$rv$user$id):", dput2(ses$rv$user$id))
  for (oauth_provider in oauth_providers) {
    if (is.null(ses$rv$user$id)) {
      # Only execute in case we're not already authenticated, e.g. through
      # cookie based login or a previuous oauth handler function
      func <- oauth_handlers[[oauth_provider]]
      func(ses)
    }
    ses$rv$login_checked[[oauth_provider]] <- TRUE
  }
}


hndl__session_end <- function(ses) {
  trace_func_entry()
  infomsg("Event onStop triggered in server: storing appstate as URL in DB")
  uid <- isolate(ses$rv$user$id)
  if (is.null(uid)) {
    infomsg("Skipped, no user logged in.")
    return()
  }
  if (!db__is_connected()) {
    infomsg("Skipped, DB already disconnected.")
    return()
  }
  tbl <- db__get_table("appstate")
  idx <- which(tbl$user_id == uid & tbl$resource_id == "url")
  url <- ses$volatile$url
  if (length(idx) == 0) {
    query <- paste(
      "INSERT INTO appstate",
      "(user_id, resource_id, resource_value)",
      "VALUES (?, ?, ?)"
    )
    params <- list(uid, "url", url)
  } else {
    query <- paste(
      "UPDATE Appstate",
      "SET resource_value = ?",
      "WHERE user_id = ? AND resource_id = ?"
    )
    params <- list(url, uid, "url")
  }
  infomsg("calling:", query)
  infomsg("With params:", utils__dput(params, collapse = " ", trim = TRUE))
  db__execute(query, params = params)
}
