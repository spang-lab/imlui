hndl__oauth__auth_spang_lab <- function(ses) {
  trace_func_entry("hndl__oauth__auth_spang_lab")
}


#' @title Set auth state based on Github OAuth Token
#' @param ses List of session specific objects as returned by `init_data` once
#' for every user session
#' @details Should be called once at the beginning of the server function
hndl__oauth__github <- function(ses) {
  trace_func_entry("hndl__oauth__github")
  # browser()
  rv <- ses$rv
  if (is.null(ses$const$url_params$code)) {
    return()
  }
  github_oauth2_access_token <- httr::oauth2.0_access_token(
    endpoint = httr::oauth_endpoints("github"),
    app = ses$const$shinygithub_oauth_app,
    code = ses$const$url_params$code
    # returns: list(access_token="asdf", scope="", token_type="bearer")
  )
  github_oauth2_token <- httr::oauth2.0_token(
    app = ses$const$shinygithub_oauth_app,
    endpoint = httr::oauth_endpoints("github"),
    credentials = github_oauth2_access_token,
    cache = FALSE
  )
  resp <- httr::GET(
    "https://api.github.com/user",
    httr::config(token = github_oauth2_token)
  )
  body <- httr::content(resp) # 1)
  if (httr::status_code(resp) == 200) {
    debugmsg(paste0(
      "Github login successful (",
      "login: ", body$login %||% "NULL", ", ",
      "id: ", body$id %||% "NULL", ", ",
      "node_id: ", body$node_id %||% "NULL", ", ",
      "login: ", body$login %||% "NULL", ", ",
      "avatar_url: ", body$avatar_url %||% "NULL", ", ",
      "name: ", body$name %||% "NULL", ", ",
      "email: ", body$email %||% "NULL", ")"
    ))
    users <- db__get_table("users")
    if (!(body$id %in% users$github_id)) {
      debugmsg("New ID, creating new user in database")
      new_user <- list(
        id = paste0("github_user_", body$id),
        display_name = body$name,
        github_id = body$id,
        avatar_url = body$avatar_url
      )
      debugmsg("user_id:", paste0("github_user_", body$id))
      debugmsg("display_name:", body$name)
      debugmsg("github_id:", body$id)
      debugmsg("avatar_url:", body$avatar_url)
      db__execute(
        "INSERT INTO users
				(id, display_name, github_id, avatar_url)
				VALUES ($1, $2, $3, $4)",
        params = unname(new_user)
      )
      debugmsg("Creating successful")
      users[nrow(users) + 1, names(new_user)] <- new_user
    }
    idx <- which(users$github_id == body$id) # which removes NAs
    rv$user$id <- users$id[idx]
    set_login_cookie(
      ses = ses,
      user_id = rv$user$id,
      session_id = ses$session$token
    )
  } else {
    browser__eval('$("#login_error").show()')
  }
}

# 1) List of: 32
# Colnames: login, id, node_id, avatar_url, gravatar_id, url, html__url,
#           followers_url, following_url, gists_url, starred_url,
#           subscriptions_url, organizations_url, repos_url, events_url,
#           received_events_url, type, site_admin, name, company, blog,
#           location, email, hireable, bio, twitter_username, public_repos,
#           public_gists, followers, following, created_at, updated_at
# Important: login:      "toscm"
#            id:         "12760468"
# 			     node_id:    "MDQ6VXNlcjEyNzYwNDY4"
# 			     login:      "toscm"
#            avatar_url: "https://avatars.githubusercontent.com/u/12760468?v=4"
#            name:       "Tobias Schmidt"
#            email:      NULL
hndl__oauth__gitlab_spang_lab <- function(ses) {
  trace_func_entry("hndl__oauth__gitlab_spang_lab")
}


hndl__oauth__gitlab <- function(ses) {
  trace_func_entry("hndl__oauth__gitlab")
}


hndl__oauth__google <- function(ses) {
  trace_func_entry("hndl__oauth__google")
}
