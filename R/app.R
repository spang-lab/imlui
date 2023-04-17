#' @export
#' @title Serve the IMLUI Web App
#' @description Serves the IMLUI Web App via HTTP or returns an
#' object of class `shiny.appobj` if argument `via_shinyserver` is true.
#' @param config_file TODO
#' @param config_dir TODO
#' @param data_dir TODO
#' @param port TODO
#' @param host TODO
#' @param suppress_log_messages TODO
#' @param suppress_debug_messages TODO
#' @param devmode TODO
#' @param autoreload TODO
#' @param via_shinyserver TODO
#' @param launch.browser TODO
#' @param quiet TODO
#' @param display.mode TODO
#' @param test.mode TODO
#' @return NULL or object of class `shiny.appobj`.
serve_web_app <- function(config_file = NULL,
                          config_dir = NULL,
                          data_dir = NULL,
                          port = PORT,
                          host = "0.0.0.0",
                          suppress_log_messages = FALSE,
                          suppress_debug_messages = FALSE,
                          devmode = util__in_devmode(),
                          autoreload = TRUE,
                          via_shinyserver = FALSE,
                          launch.browser = FALSE,
                          quiet = TRUE,
                          display.mode = "normal", # auto, normal, showcase
                          test.mode = FALSE) { # used for automated tests

  trace_func_entry()

  options(
    imlui.suppress_log_messages = suppress_log_messages,
    imlui.suppress_debug_messages = suppress_debug_messages,
    imlui.devmode = devmode,
    imlui.data_dir = data_dir,
    shiny.autoreload = autoreload,
    shiny.fullstacktrace = FALSE,
    warn = 1
  )

  pkg_root <- dirname(system.file("DESCRIPTION", package = "imlui"))
  pkg_loaded_via_devtools <- !file.exists(file.path(pkg_root, "R", "imlui.rdb"))
  if (pkg_loaded_via_devtools && (autoreload || via_shinyserver)) {
    debugmsg("Patching pkgload and shiny")
    patch_pkgload()
    patch_shiny()
  }
  r_xpr <- substitute({
    func <- if (!pkg_loaded_via_devtools) imlui::web_app else web_app
    func(
      config_file = config_file,
      config_dir = config_dir,
      host = host,
      launch.browser = launch.browser,
      port = port,
      quiet = quiet,
      display.mode = display.mode,
      test.mode = test.mode
    )
  })
  r_src <- paste(deparse(r_xpr), collapse="\n")
  infomsg(paste0("Serving IMLUI Web App via: http://localhost:", port))
  if (via_shinyserver) {
    appobj <- eval(r_xpr)
    return(appobj)
  } else if (autoreload) {
    cat(r_src, file = file.path(pkg_root, "app.R"))
    infomsg(
      "Calling shiny::with_devmode(", devmode,
      ", shiny::runApp(", pkg_root, ")",
      sep = ""
    )
    shiny::with_devmode(devmode, shiny::runApp(pkg_root))
  } else {
    appobj <- eval(r_xpr)
    shiny::runApp(appobj)
  }
}


web_app_server <- function(input, output, session) {
  trace_func_entry()
  ses <- as.environment(list())
  ses$input <- input
  ses$output <- output
  ses$session <- session
  ses$const <- const <- init__const(session)
  ses$rv <- rv <- init__rv()
  ses$r <- r <- init__r(input, output, session, const, rv)
  ses$hndl <- init__hndl(ses)
  init_outputs(ses)
  configure_bookmarking()
  trace_func_exit()
  on_stop_func <- function() { hndl__session_end(ses) }
  onStop(fun <- on_stop_func)
}


web_app_ui <- function(request) {
  # See README/#ui-layout for an overview
  trace_func_entry()
  tagList(
    tags$head(
      tags$link(rel="icon", href="imlui/assets/ico/imlui_logo.ico")
    ),
    fluidPage(
      id = "web_app",
      title = "IML-UI",
      imlui_js(), # Loads js/css files from <imlui>/assets/js
      uiOutput(outputId = "landing_page"),
      theme = shinythemes::shinytheme("cerulean"),
      NULL
      # Valid themes are:
      # cerulean: ok, but navbar too bright
      # cosmo: too dark
      # cyborg: too dark
      # darkly: too dark
      # flatly: too dark
      # journal: bad
      # lumen: good theme, very small font
      # paper: maybe, but worse then lumen
      # readable: maybe, but worse then lumen
      # sandstone: too dark
      # simplex: button too dark, else ok
      # slate: too dark
      # spacelab: buttons too dark, else very good
      # superhero: too dark and ugly
      # united: too fancy
      # yeti: good theme, pretty dark navbar
      # See https://bootswatch.com/ for a preview.
    )
  )
}


imlui_js <- function() {
  trace_func_entry()
  # Loads js/css files from <imlui>/assets/js
  tagList(
    htmltools::htmlDependency(
      name = "imlui-assets",
      version = "1.0.0",
      package = "imlui",
      src = "assets",
      script = list(
        # list(src = "js/js-cookie-3.0.1/js.cookie.min.js"),
        # list(src = "js/event-handlers-1.0.0/event-handlers-1.0.0.js"),
        list(src = "js/imlui.js", type = "module")
      ),
      stylesheet = c(), # 1
      all_files = TRUE
    ),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "imlui-assets-1.0.0/css/styles.css"
      )
    )
    # 1 Use function `tags$head` to include css instead of argument
    # `stylesheet` of function `htmltools::htmlDependency` to make sure the css
    # file is included at the end of the header and therefor not overwritten by
    # the included theme
  )
}


#' @export
#' @name web_app
#' @title IMLUI Web App
#' @description `web_app` returns an object of class `shinyApp` that can be passed to `shiny::runApp` to serve the IMLUI Web App. Most of the time you should not use `web_app` directly but instead use [serve_web_app()].
#' @param config_file TODO
#' @param config_dir TODO
#' @param host TODO
#' @param launch.browser TODO
#' @param port TODO
#' @param quiet TODO
#' @param display.mode TODO
#' @param test.mode TODO
#' @seealso [serve_web_app()]
web_app <- function(config_file = NULL,
                    config_dir = NULL,
                    host = "0.0.0.0",
                    launch.browser = FALSE,
                    port = PORT,
                    quiet = TRUE,
                    display.mode = "normal", # c("auto", "normal", "showcase")
                    test.mode = FALSE) { # used for automated tests
  infomsg("Entered web_app from process ID:", Sys.getpid())
  shinyApp(
    ui = web_app_ui,
    server = web_app_server,
    uiPattern = ".*", # regex
    enableBookmarking = "url", # "url", "server" or "disable"
    onStart = function() {
      init__penv()
      shiny::addResourcePath(
        prefix = "imlui/assets",
        directoryPath = system.file("assets", package = "imlui")
      )
      infomsg("ShinyApp started from process ID:", Sys.getpid())
      infomsg(paste0("Open http://localhost:", port, " to preview the app"))
      shiny::onStop(function() {
        if (db__is_connected()) {
          db__disconnect()
        }
        infomsg("ShinyApp stopped")
      })
    },
    options = list(
      port = port,
      launch.browser = launch.browser,
      host = host,
      quiet = quiet,
      display.mode = "normal",
      test.mode = FALSE
    )
  )
}
