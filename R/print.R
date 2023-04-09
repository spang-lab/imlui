# Cat Functions
caterr <- function(..., end = "\n") {
  cat(..., end, file = stderr())
}
cat2e <- function(...) {
  toscutil::cat2(..., file = stderr())
}
cat0e <- function(...) {
  toscutil::cat0(..., file = stderr())
}
catfe <- function(...) {
  toscutil::catf(..., file = stderr())
}
catne <- function(...) {
  toscutil::catn(..., file = stderr())
}
cat00e <- function(...) {
  toscutil::cat0(..., file = stderr())
}
cat0ne <- function(...) {
  toscutil::cat0n(..., file = stderr())
}
catfne <- function(...) {
  toscutil::catfn(..., file = stderr())
}
catnne <- function(...) {
  toscutil::catnn(..., file = stderr())
}
catsne <- function(...) {
  toscutil::catsn(..., file = stderr())
}
catsse <- function(..., sep = " ", end = " ") {
  toscutil::cat2(..., sep = sep, end = end, file = stderr())
}
cat0se <- function(..., sep = "", end = " ") {
  toscutil::cat2(..., sep = sep, end = end, file = stderr())
}

# Logging Functions
logerr <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    caterr(...)
  }
}
log2e <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat2e(...)
  }
}
log0e <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat0e(...)
  }
}
logfe <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catfe(...)
  }
}
logne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catne(...)
  }
}
log00e <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat00e(...)
  }
}
log0ne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat0ne(...)
  }
}
logfne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catfne(...)
  }
}
lognne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catnne(...)
  }
}
logsne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catsne(...)
  }
}
logsse <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catsse(...)
  }
}
log0se <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat0se(...)
  }
}

msg <- function(...,
                col = function(x) x,
                before = "",
                middle = NULL,
                after = " ",
                sep = " ",
                end = "\n",
                cond = TRUE) {
  if (cond) {
    # If this function gets called from the server or any descendant, there
    # should be a `ses` object available in the parent (i.e. calling)
    # environments, containing the session ID (snr) and user ID (uid), which
    # we print. An exception are observer functions, which are called from
    # the shiny event loop. There, it must be asserted that the `ses` object
    # is available in the calling environment.
    ses <- dynGet("ses", ifnotfound = NULL, inherits = TRUE)
    snr <- ses$const$snr
    uid <- isolate(ses$rv$user$id)
    if (is.null(middle)) {
        middle <- paste0(
          now_ms(),
          " snr:", snr %||% 0,
          " uid:", uid %||% 0
        )
    }
    txt <- paste0(before, middle, after, paste(..., sep = sep), end)
    txt <- crayon::reset(col(txt))
    base::message(txt, appendLF = FALSE)
  }
}

infomsg <- function(..., after = " INFO ", col = crayon::green, sep = " ") {
  msg(..., after = after, col = col, sep = sep)
}

warnmsg <- function(..., after = " WARNING ", col = crayon::yellow, sep = " ") {
  msg(..., after = after, col = col, sep = sep)
}

debugmsg <- function(..., after = " DEBUG ", col = crayon::blue, sep = " ") {
  # Benchmark: added following code to `hndl__sc__login_cookie`
  #   a <- Sys.time()
  #   debugmsg("Measuring time for debugmsg")
  #   b <- Sys.time()
  #   message(a)
  #   message(b)
  #   message(b - a)
  # Output:
  #   2022-12-13 15:54:04
  #   2022-12-13 15:54:04
  #   0.00748801231384277
  msg(..., after = after, col = col, sep = sep)
}

tracemsg <- function(..., after = " TRACE ", col = crayon::silver, sep = " ") {
  msg(..., after = after, col = col, sep = sep)
}

errormsg <- function(..., after = " ERROR ", col = crayon::red, sep = " ") {
  msg(..., after = after, col = col, sep = sep)
}

# Helpers
now_ms <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
}
