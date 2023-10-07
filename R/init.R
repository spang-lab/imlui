init__penv <- function() {
  trace_func_entry("init__penv")
  if (!exists("imlui_config", envir = penv)) {
    penv$imlui_config <- read_imlui_config_file(create_if_missing = TRUE)
    db__connect()
    penv$sctr <- 0
    # For each session: use current values as session number and increase by 1
    penv$cache <- cachem::cache_mem(max_size = 2 * 1024^3)
    penv$get_symbol_cached <- memoise::memoise(
      get_symbol_uncached,
      cache = penv$cache
    )
    # Reasoning for penv$get_symbol_cached: usually we would just do
    # `get_symbol_cached <- memoise(get_symbol_uncached, cache = cache_mem())`
    # but this would recreate the used `cache` object each time the package is
    # reloaded. For production code this makes no difference, because the
    # package is loaded only once there, but for development it's a huge
    # slow-down, which is why we only create penv$get_symbol_cached only once
    # during the first call of `devtools::load_all` and afterwards skip
    # recreation.
  } else {
    debugmsg(
      "Skipping full recreation of penv because it already exists.",
      "Only reopening connection to database."
    )
    if (!db__is_connected()) {
      db__connect()
    }
  }
}


init__const <- function(session, db = penv$db) {
  trace_func_entry("init__const")
  snr <- penv$sctr <- penv$sctr + 1
  x <- session$clientData
  url_search <- isolate(x$url_search)
  url_protocol <- isolate(x$url_protocol) # eg. "http:", "https:"
  url_hostname <- isolate(x$url_hostname) # eg. "localhost", "abc.spang-lab.de"
  url_port <- isolate(x$url_port) # eg. 443, 80, 99 (https, http, custom)
  url_pathname <- isolate(x$url_pathname) # eg. "/", "/shinyserver/imlui"
  url_params <- parseQueryString(url_search)
  github_redirect_url <- paste0(
    url_protocol, "//", url_hostname,
    if (util__is_none(url_port)) "" else paste0(":", url_port),
    url_pathname
    # IMPORTANT: the Github `redirect_uri` must reference a SUBDIRECTORY
    # of the callback URL. Because it's possible (or even likely), that our
    # redirect URL will be just our usual URL, the configured callback URL
    # must not have a trailing slash:
    #   http://my.domain.com:1234/  <---- BAD
    #   http://my.domain.com:1234   <---- GOOD
    #   This way, http://my.domain.com:1234/ is actually a subdirectory of
    #   http://my.domain.com:1234 and can be used.
    # For further details see:
    #   https://docs.github.com/en/apps/oauth-apps/building-oauth-apps/authorizing-oauth-apps#redirect-urls
  )
  x <- db__get_table("mapping_groups_settings")
  appname <- x[x[["setting_id"]] == "github_oauth_app_name", "value"]
  key <- x[x[["setting_id"]] == "github_oauth_app_key", "value"]
  secret <- x[x[["setting_id"]] == "github_oauth_app_secret", "value"]
  debugmsg("url_protocol:", utils__dput(url_protocol))
  debugmsg("url_hostname:", utils__dput(url_hostname))
  debugmsg("url_port:", utils__dput(url_port))
  debugmsg("url_pathname:", utils__dput(url_pathname))
  debugmsg("url_params:", utils__dput(nv2css(url_params)))
  debugmsg("github_redirect_url:", utils__dput(github_redirect_url))
  debugmsg("appname:", utils__dput(appname))
  debugmsg("key:", utils__dput(key))
  debugmsg("secret:", utils__dput(secret))
  shinygithub_oauth_app <- httr::oauth_app(
    appname = appname,
    key = key,
    secret = secret,
    redirect_uri = github_redirect_url
  )
  return(toscutil::function_locals(without = "x"))
}


init__hndl <- function(ses) {
  trace_func_entry("init__hndl")
  # Register state change (sc) handlers first, because they contain
  # hndl__sc__login_cookie, i.e. if the browser inits input$login_jscookie before
  # the server function has finished, we will start by checking the login cookie
  # immediately. If input$login_jscookie is not initialized yet, the startup
  # (su) handlers will be triggered first.
  sc <- init__hndl__sc(ses)
  su <- init__hndl__su(ses)
  btn <- init__hndl__btn(ses)
  bookmark <- init__hndl__bookmark(ses)
  return(toscutil::function_locals())
}


init__r <- function(input, output, session, const, rv) {
  trace_func_entry("init__r")
  return(toscutil::function_locals())
}


init__rv <- function(db) {
  trace_func_entry("init__rv")
  user <- reactiveValues(
    # Updated in `hndl__sc__auth_completed > init_rv_user`
    id = NULL,
    group_ids = NULL,
    display_name = NULL,
    is_admin = NULL,
    github_id = NULL,
    avatar_url = NULL,
    password = NULL,
    gitlab_id = NULL,
    google_id = NULL,
    spanglab_gitlab_id = NULL,
    spanglab_auth_id = NULL
  )
  login_checked <- reactiveValues(
    # Updated in `hndl__oauth` and `hndl__sc__login_cookie`
    cookie = FALSE,
    google = FALSE,
    github = FALSE,
    gitlab = FALSE,
    auth_spang_lab = FALSE,
    gitlab_spang_lab = FALSE
  )
  tbl <- reactiveValues(
  )
  auth <- reactiveValues(
    # Updated in `hndl__sc__auth_completed`
    completed = FALSE
  )
  model <- reactiveValues(
    # Updated in `hndl__sc__auth_completed > init_rv_models`
    ids = NULL,
    symbols = NULL,
    displaynames = NULL,
    pkgs = NULL
  )
  dataset <- reactiveValues(
    # Updated in `hndl__sc__auth_completed > init__rv_dataset`
    ids = NULL,
    symbols = NULL,
    displaynames = NULL,
    pkgs = NULL,
    transpose = NULL
  )
  return(
    list(
      user = user,
      login_checked = login_checked,
      tbl = tbl,
      auth = auth,
      model = model,
      dataset = dataset
    )
  )
}
