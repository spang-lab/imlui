get_pkg_funcs <- function() {
  trace_func_entry()
  r_dir <- system.file("R", package = "imlui")
  r_files <- dir(r_dir, "*.R", full.names = TRUE)
  r_code <- paste(sapply(r_files, readr::read_file), collapse = "\n")
  func_pattern <- "\\w+(?=\\s*<-\\s*function\\()"
  funcs <- stringr::str_extract_all(r_code, func_pattern)[[1]]
  funcs <- funcs[sapply(funcs, exists)]
  funcs <- funcs[sapply(funcs, \(x) is.function(get(x)))]
  return(funcs)
}


activate_func_tracing <- function() {
  trace_func_entry()
  lapply(get_pkg_funcs(), function(f) {
    try(trace(f, function() debugmsg("Entering", f), print=FALSE))
  })
}
#' @title Return a MockShinySession object (for Testing Purposes)
#' @return R6 MockShinySession object
mock_session_obj <- function() {
  trace_func_entry()
  session <- shiny::MockShinySession$new()
  session$clientData <- list(
    url_search = "",
    url_hash_initial = "",
    singletons = paste0(
      "add739c82ab207ed2c80be4b7e4b181525eb7a75,",
      "f756633fccc17f84dff1d8dbfc0a750cc0a8ff65,",
      "fc993cee573540ff31da45e9f921c83f789af40c"
    ),
    url_protocol = "http:",
    url_port = "8081",
    url_hostname = "localhost",
    pixelratio = 1,
    url_hash = "",
    url_pathname = "/"
  )
  session
}
mock_session_obj_mem <- memoise::memoise(mock_session_obj)

#' @title Return a shiny server input object (for Testing Purposes)
#' @param ... Properties of returned input object
#' @examples
#' local({
#'   rc_enabled <- getOption("shiny.suppressMissingContextError", FALSE)
#'   shiny::reactiveConsole(TRUE)
#'   on.exit(shiny::reactiveConsole(rc_enabled), add = TRUE)
#'   input <- imlui:::mock_input_obj(x = 1, y = 2)
#'   input$x == 1 && input$y == 2
#' })
mock_input_obj <- function(...) {
  trace_func_entry()
  session <- mock_session_obj_mem()
  session$setInputs(...)
  input <- session$input
}

mock_output_obj <- function() {
  trace_func_entry()
  session <- mock_session_obj_mem()
  output <- session$output
}

mock_args <- function(func, ...) {
  trace_func_entry()
  default_args <- as.list(formals(func))
  stubbed_args <- utils::modifyList(default_args, list(...))
  arg_names <- names(stubbed_args)
  for (i in seq_along(stubbed_args)) {
    assign(
      x = arg_names[[i]],
      value = eval(stubbed_args[[i]]),
      envir = global_env
    )
  }
}

#' @title Return a reactivevalues Object (for Testing Purposes)
#' @param user_id Either 'toscm', 'max', 'public' or NULL
mock_rv_obj <- function(user_id = NULL) {
  trace_func_entry()
  rv <- init__rv(db = mock_db_obj())
  if (is.null(user_id)) {
    # do nothing
  } else if (user_id == "toscm") {
    rv$user$id <- "toscm"
    rv$user$group_ids <- "admin;spang;public"
  } else if (user_id == "max") {
    rv$user$id <- "max"
    rv$user$group_ids <- "spang;public"
  } else if (user_id == "public") {
    rv$user$id <- "public"
    rv$user$group_ids <- "public"
  } else {
    stop("user_id must be 'toscm', 'max', 'public' or NULL, not:", user_id)
  }
  return(rv)
}

mock_db_obj <- function() {
  trace_func_entry()
  stop("deprecated")
}

mock_ses_obj <- function(user_id = "toscm") {
  ses <- as.environment(list(
    input = mock_input_obj(),
    output = mock_output_obj(),
    session = mock_session_obj_mem(),
    db = mock_db_obj(),
    rv = mock_rv_obj(user_id = "toscm")
  ))
}
