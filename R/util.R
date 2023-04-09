configure_bookmarking <- function() {
  trace_func_entry()
	setBookmarkExclude(c(
		"dim",
		"tbl_users_state",
		"tbl_users_search",
		"tbl_users_cell_clicked",
		"tbl_users_cells_selected",
		"tbl_users_columns_selected",
		"tin_dbg_eval_r_code",
		"user_session",
		"tbl_users_rows_current",
		"tbl_users_rows_selected",
		"tbl_users_rows_all",
		"btn_dbg_eval_r",
		"login_button",
		"login_button_github",
		"login_button_google",
		"login_button_auth_spang_lab",
		"login_button_gitlab_spang_lab",
		"login_button_gitlab",
		"login_user_name",
		"navbar_left",
		"navbar_right",
		"S_DS_TBL_cell_clicked",
		"S_DS_TBL_cells_selected",
		"S_DS_TBL_columns_selected",
		"S_DS_TBL_rows_all",
		"S_DS_TBL_rows_current",
		"S_DS_TBL_rows_selected",
		"S_DS_TBL_search",
		"S_DS_TBL_state",
		"S_MO_TBL_cell_clicked",
		"S_MO_TBL_cells_selected",
		"S_MO_TBL_columns_selected",
		"S_MO_TBL_rows_all",
		"S_MO_TBL_rows_current",
		"S_MO_TBL_rows_selected",
		"S_MO_TBL_search",
		"S_MO_TBL_state",
		"S_PA_TBL_cell_clicked",
		"S_PA_TBL_cells_selected",
		"S_PA_TBL_columns_selected",
		"S_PA_TBL_rows_all",
		"S_PA_TBL_rows_current",
		"S_PA_TBL_rows_selected",
		"S_PA_TBL_search",
		"S_PA_TBL_state",
		"S_SE_TBL_cell_clicked",
		"S_SE_TBL_cells_selected",
		"S_SE_TBL_columns_selected",
		"S_SE_TBL_rows_all",
		"S_SE_TBL_rows_current",
		"S_SE_TBL_rows_selected",
		"S_SE_TBL_search",
		"login_jscookie",
		"S_SE_TBL_state",
		"logout_button",
		"btn_dbg_cookies_get",
		"btn_dbg_cookies_set",
		"btn_dbg_cookies_rm",
		"cookies"
	))
}


evaljs <- function(ses, script) {
  ses$session$sendCustomMessage("evaljs", script)
}


get_dataset <- function(id, ses) {
  trace_func_entry()
  penv$get_symbol_cached(
    sym = ses$rv$dataset$symbols[[id]],
    typ = "dataset",
    pkg = ses$rv$dataset$pkgs[[id]],
    transpose = ses$rv$dataset$transpose[[id]]
  )
}


get_model_feature_names <- function(model) {
  trace_func_entry("get_model_names")
  nams <- names(model)
  nams <- nams[nams != "Intercept"]
  return(nams)
}


get_model <- function(id, ses) {
  trace_func_entry("get_model")
  penv$get_symbol_cached(
    sym = ses$rv$model$symbols[[id]],
    typ = "model",
    pkg = ses$rv$model$pkgs[[id]]
  )
}


#' @name get_symbol
#' @title Get Symbol from Package
#' @description Like normal get but cached, calls `data` if the object does not
#' exist yet and performs some initial transformation to make the data
#' compliant to the format expected by imlui functions.
#' @param sym Symbol to get as character.
#' @param typ Type of object (either "dataset" for dataset, or "model" for
#' model).
#' @param pkg In which package to look for `sym`.
#' @param transpose Transpose object after initialisation? (only used if `typ`
#' == "dataset")
#' @return `obj: get(sym)`
get_symbol <- function(sym, typ, pkg, transpose = FALSE) {
  # `penv$get_symbol_cached` is generated during `.onLoad`
  penv$get_symbol_cached(sym, typ, pkg, transpose)
}


get_symbol_uncached <- function(sym, typ, pkg, transpose = FALSE) {
  trace_func_entry()
  if (!is.null(pkg)) {
    obj <- eval(parse(text = paste(pkg, sym, sep = "::")))
  } else {
    obj <- get(sym)
  }
  if (typ == "dataset") {
    if (any(c("RccSet", "ExpressionSet") %in% class(obj))) {
      obj <- Biobase::exprs(obj)
    }
    if (transpose) {
      obj <- t(obj)
    }
    obj <- as.data.frame(obj, stringsAsFactors = TRUE)
    n <- colnames(obj)
    obj[, ] <- lapply(seq_along(obj), function(i) {
      if (is.categorical(v = obj[, i], n = n[i])) {
        as.factor(obj[, i]) 
      } else { 
        as.numeric(obj[, i])
      }
    })
  } else if (typ == "model") {
    # No more preprocessing to be done for objects of type model
  } else {
    stop("Param `typ` in `get_symbol()` must be either 'dataset' or 'model'.")
  }
  return(obj)
}


util__capture_output <- function(...) {
  x <- utils::capture.output(...)
  y <- paste(x, collapse = "\n")
  return(y)
}


# rd: path to rd file, e.g. 
util__rd_2_txt <- function(rd) {
  txt <- util__capture_output(tools::Rd2txt(rd))
  txt <- gsub("_\b", "", txt)
  return(txt)
}


util__get_help_text <- function(sym, pkg) {
  trace_func_entry("util__get_help_text")
  if (pkg == "imlui" && util__in_devmode()) {
    rd <- system.file(paste0("man/", sym, ".Rd"), package = "imlui")
    if (util__is_none(rd)) {
      txt <- "No Description available yet."
    } else {
      txt <- util__rd_2_txt(rd)
    }
  } else {
    help_obj <- utils::help((sym), (pkg))
    # `help` returns an object of type `help_files_with_topic` that can be used
    # to find the corresponding Rd file. The strange syntax above (brackets
    # around arguments) is required because arguments are used as symbols, i.e.
    # without evaluating them, unless they are wrapped in brackets. See last
    # lines of help(help) for details.
    if (length(help_obj) > 0) {
      rd <- util__get_rd_path(help_obj)
      txt <- util__rd_2_txt(rd)
    } else {
      txt <- "No Description available yet."
    }
  }
}


# Copied in parts from `utils:::.getHelpFile`
util__get_rd_path <- function(help_obj) {
  path <- dirname(help_obj)
  dirpath <- dirname(path)
  if (!file.exists(dirpath)) {
    stop("invalid argument `help_obj`")
  }
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)
  if (!file.exists(paste0(RdDB, ".rdx"))) {
    stop("help for package %s cannot be accessed", sQuote(pkgname))
  }
  fetch_rddb(RdDB, basename(help_obj))
}


# Copied from `tools:::fetchRdDB`
fetch_rddb <- function(filebase, key = NULL) {
  fun <- function(db) {
    vals <- db$vals
    vars <- db$vars
    datafile <- db$datafile
    compressed <- db$compressed
    envhook <- db$envhook
    fetch <- function(key) {
      lazyLoadDBfetch(
        vals[key][[1L]],
        datafile, compressed, envhook
      )
    }
    if (length(key)) {
      if (key %notin% vars) {
        stop(gettextf(
          "No help on %s found in RdDB %s",
          sQuote(key), sQuote(filebase)
        ), domain = NA)
      }
      fetch(key)
    } else {
      res <- lapply(vars, fetch)
      names(res) <- vars
      res
    }
  }
  res <- lazyLoadDBexec(filebase, fun)
  if (length(key)) {
    res
  } else {
    invisible(res)
  }
}


util__in_devmode <- function() {
  pkg_dir <- dirname(system.file("DESCRIPTION", package = "imlui"))
  util__in_devmode <- !file.exists(file.path(pkg_dir, "R", "imlui.rdb"))
  return(util__in_devmode)
}


# title: named vector to comma separated string
nv2css <- function(nv) {
  x <- dput2(nv)
  x <- paste(x, collapse = "")
  x <- gsub("\\s+", " ", x)
  x <- gsub("^(list|c)\\(", "", x)
  x <- gsub("\\)$", "", x)
  x <- gsub(" = ", "=", x)
  x <- gsub("[`\"'\\]", "", x) # Remove: `'"\
}


print_env <- function(env, depth = 2, indent = 0) {
  names <- ls(env, all.names = TRUE)
  for (name in names) {
    cat(paste0(rep(" ", indent * 4), collapse = ""), name, "\n", sep = "")
    if (length(env[[name]]) > 1 && depth > 0) {
      try(
        print_env(env[[name]], depth = depth - 1, indent = indent + 1),
        silent = TRUE
      )
    }
  }
}


set_login_cookie <- function(ses, user_id, session_id) {
  trace_func_entry()
  debugmsg("Session ID:", session_id)
  debugmsg("User ID:", user_id)
  browser__set_cookie(ses, key = "user_session", value = session_id)
  db__insert_cookie(user_id = user_id, session_id = session_id)
}


trace_func_entry <- function(func = NULL) {
  if (is.null(func)) {
    func <- toscutil::caller(2)
  }
  if (!isTRUE(getOption("imlui.suppress_debug_messages"))) {
    tracemsg(func, after = " ENTERED ")
  }
}


trace_func_exit <- function(func = NULL) {
  if (!isTRUE(getOption("imlui.suppress_debug_messages"))) {
    if (is.null(func)) {
      func <- toscutil::caller(2)
    }
    tracemsg("Leaving", func)
  }
}


# Like purrrs %||% but also checks for empty lists and empty strings (%d% for
# default)
`%d%` <- function(x, y) {
  if (util__is_none(x)) y else x
}


# Like %in%, but negated (`x %notin% c(1,2,3)` is better readable than `!(x
# %in% c(1,2,3)`)
`%notin%` <- function(x, table) match(x, table, nomatch = 0) == 0


# Round up to nearest power of 10 (18-->100, 2345-->10000 etc.)
ceiling10 <- function(x) {
  10^ceiling(log10(x))
}


# Like normal lapply, but sets names of returned list to X if X is a named
# character vector
clapply <- function(X, FUN, ...) {
  r <- lapply(X, FUN, ...)
  if (is.null(names(X)) && is.character((X))) {
    names(r) <- X
  }
  r
}


# Collapse a n*1 character vector `x` into a 1*1 character vector seperated by
# `s`
collapse <- function(x, s) paste(x, collapse = s)
collapse_ <- function(x) paste(x, collapse = "_")
collapseCS <- function(x) paste(x, collapse = ", ") # TODO: remove `s` argument!
collapseNL <- function(x) paste(x, collapse = "\n")


# Like head and tail, but returns `n` rows/cols from each side of `x` (i.e. the
# corners of `x`)
corn <- function(x, n = 2L) {
  if (is.vector(x)) {
    return(x)
  }
  stopifnot("matrix" %in% class(x) || "data.frame" %in% class(x))
  r <- nrow(x)
  cs <- ncol(x)
  if (all(r > 4, cs > 4)) {
    x[c(1:n, (r - n + 1):r), c(1:n, (cs - n + 1):cs)]
  } else {
    x
  }
}


# Describes first `ncol` columns of dataframe `df`
#
# Each column is described through its column name, column index and range. If
# a column has less unique values than `max_values`, the unique values are
# listed instead of showing the range. All numerical values are rounded to
# `digit` digits.
describe_df <- function(df, ncol = 25, max_levels = 8, digits = 2) {
  options(digits = 2)
  for (i in 1:ncol) {
    if (is.numeric(df[, i])) {
      df[, i] <- round(df[, i], digits)
    }
    levels_i <- sort(unique(df[, i]))
    colname_i <- colnames(df)[i]
    if (length(levels_i) < max_levels) {
      details <- paste0(
        "[Levels: ",
        paste0(levels_i, collapse = ", "),
        "]"
      )
    } else if (is.numeric(levels_i)) {
      range_i <- round(range(levels_i), digits)
      details <- paste0("[Range: ", range_i[1], "-", range_i[2], "]")
    } else {
      n <- length(levels_i)
      k <- (max_levels %/% 2)
      details <- paste0(
        "[Levels: ",
        paste0(levels_i[1:k], collapse = ", "),
        ", ..., ",
        paste0(levels_i[(n - k):n], collapse = ", "),
        "]"
      )
    }
    cat(i, ". ", colname_i, " ", details, "\n", sep = "")
  }
}


utils__capture_output <- function(..., collapse = " ", trim = TRUE) {
  x <- utils::capture.output(...)
  x <- sapply(x, trimws)
  x <- paste(x, collapse = collapse)
  return(x)
}

utils__dput <- function(..., collapse = " ", trim = TRUE) {
  x <- utils__capture_output(dput(...), collapse = collapse, trim = trim)
  return(x)
}


dput2 <- function(...) {
  x <- utils__capture_output(dput(...))
  return(x)
}


# Like pythons `sys.exit`
exit <- function(exitcode) {
  if (interactive()) stop(exitcode, call. = FALSE) else quit(status = exitcode)
}


# Guesses if a vector contains categorical values or not
#
# c(1,    2,    2,    2,    1,    2   ) --> TRUE (categorical)
# c(0,    0,    0.50, 0.50, 1,    1   ) --> TRUE (categorical)
# c(1.23, 7.10, 4.51, 2.22, 4.56, 5.12) --> FALSE (continous)
is.categorical <- function(v, n) {
  levels <- sort(unique(v))
  threshold <- floor(length(v) * 0.33)
  if ((is.numeric(v)) && (length(levels) > threshold) && (!(n %in% CATEGORICALS))) {
    FALSE
  } else {
    TRUE
  }
}


# TRUE for NULL, empty lists and empty string
util__is_none <- function(x) {
  if (is.null(x) ||
    ((typeof(x) == "character") && length(x) == 1 && (x == "")) ||
    ((typeof(x) == "list") && (length(x) == 0))) {
    TRUE
  } else {
    FALSE
  }
}


is.non.empty.string <- function(x) {
  if (!is.null(x) && length(x) == 1 && is.character(x) && nchar(x) != 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


nl_nv_2_df <- function(nl_nv, col1 = "dataset", col2 = "sample", col3 = "y") {
  # title: Convert Named List of Named Vectors to a Dataframe
  # example:
  # # nl_nv = List of 3:
  # # $ lamis_train: Named num [1:233] GSM2776 = 0.68, GSM2777 = 1.00 ...
  # # $ lamis_test1: Named num [1:181] GSM2795 = 0.50, GSM2796 = 1.54 ...
  # # $ lamis_test2: Named num [1:466] Ric6.S9 = 0.82, Ric6.S1 = 1.20 ...
  # head(nl_nv_2_df(nl_nv))
  # #             dataset    samples        y
  # #    1    lamis_train    GSM2776    -0.68
  # #    2    lamis_train    GSM2777    -1.00
  # #    3    lamis_train    GSM2778    -1.37
  # #    4    lamis_train    GSM2779    -0.79
  # #    5    lamis_train    GSM2780     0.03
  # #    6    lamis_train    GSM2781    -0.61
  # utils::str(nl_nv_2_df(nl_nv))
  # # 'data.frame':   880 obs. of  3 variables:
  # #  $ dataset: chr  "lamis_train" "lamis_train" "lamis_train" ...
  # #  $ samples: chr  "GSM2776" "GSM2777" "GSM2778" "GSM2779" ...
  # #  $ y      : num  -0.68 -1.00 -1.37 -0.79 0.03 ...
  df <- plyr::rbind.fill(lapply(1:length(nl_nv), function(i) {
    y <- nl_nv[[i]]
    data.frame(rep(names(nl_nv)[[i]], length(y)), names(y), y)
  }))
  colnames(df) <- c(col1, col2, col3)
  df
}


# Replace backslashes with forward slashes
b2f <- function(p) {
  str_replace_all(p, "\\\\", "/")
}


# Taken from https://github.com/PaulC91/user_session/blob/master/R/internal.R
randomString <- function(n = 64) {
  paste(
    sample(x = c(letters, LETTERS, 0:9), size = n, replace = TRUE),
    collapse = ""
  )
}


redirection_to <- function(url) {
  trace_func_entry()
  x <- sprintf('location.replace("%s");', url)
  x <- shiny::HTML(x)
  x <- shiny::tags$script(x)
  return(shiny::renderUI(x))
}
