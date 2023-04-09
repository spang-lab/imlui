imlui_env <- environment()
global_env <- .GlobalEnv

.onLoad <- function(libname, pkgname) {
  # If we're in devmode we store the environment containing the process state
  # directly within the global environment so it doesn't get overridden each
  # time the package is reloaded, else we store in the package env.
  # Initialization of penv's fields is then done in function `init__penv`, which
  # gets called by `web_app`.
  penv <- new.env(parent = emptyenv())
  if (util__in_devmode()) {
    if (!exists("penv", envir = global_env)) {
      assign("penv", penv, envir = global_env)
    }
    mock <- new.env(parent = emptyenv())
    if (!exists("mock", envir = global_env)) {
      assign("mock", mock, envir = global_env)
    }
  } else {
    assign("penv", penv, envir = imlui_env)
  }
}
