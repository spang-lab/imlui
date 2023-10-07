hndl__btn__login_github <- function(ses) {
  trace_func_entry()
  # start OAuth2.0 authentication, i.e. redirect to
  # <https://github.com/login/oauth/{authorize/access_token}>
  infomsg("login_button_github clicked")
  infomsg("Redirecting to https://github.com/login/oauth")
  url <- httr::oauth2.0_authorize_url(
    endpoint = httr::oauth_endpoints("github"),
    app = ses$const$shinygithub_oauth_app,
    scope = "",
    redirect_uri = NULL
  )
  # browser()
  ses$output$landing_page <- redirection_to(url)
}


hndl__btn__login <- function(ses) {
  trace_func_entry()
  udf <- db__get_table("users")
  uid_entered <- ses$input$login_user_name
  upw_entered <- ses$input$login_password
  # browser()
  if (uid_entered %in% udf$id) {
    uid_exists <- TRUE
    upw_stored <- udf$password[udf$id == uid_entered]
    upw_matches <- if (SODIUM_HASHED) {
      sodium::password_verify(upw_stored, upw_entered)
    } else {
      identical(upw_entered, upw_stored)
    }
  } else {
    uid_exists <- FALSE
  }
  if (uid_exists && upw_matches) {
    ses$rv$user$id <- uid_entered
    set_login_cookie(
      ses = ses,
      user_id = ses$rv$user$id,
      session_id = ses$session$token
    )
    infomsg("Authenticated successfully as", ses$rv$user$id)
  } else {
    # Show error message to the user for 5seks
    infomsg("Login failed")
    browser__show(ses, id = "login_error")
    browser__eval(ses, code = '$("#login_error").delay(5000).fadeOut(1000)')
  }
}


hndl__btn__logout <- function(ses) {
  trace_func_entry()
  browser__rm_cookie(ses, "user_session")
  ses$session$reload()
}
