## db__connect #################################################################

#' @title Connect to IMLUI_DB.
#' @description Connect to database specifed in IMLUI_CONFIG. Resulting connection object is stored in process environment `penv`.
#' @details Each call to `db__connect` should be paired with a call to `db__disconnect()`. See Examples for details.
#' @param force_recreate Logical, if TRUE, the database will be recreated from scratch. This is useful for development.
#' @examples \dontrun{
#' db__connect()
#' on.exit(db__disconnect(), add = TRUE)
#' }
db__connect <- function(force_recreate = FALSE) {
  trace_func_entry("db__connect")
  if (is.null(penv$imlui_config$dbms)) {
    warnmsg("`db_connect()` called before `penv$imlui_config$dbms` was initialized. Doing initialzation now.")
    penv$imlui_config <- read_imlui_config_file(create_if_missing = TRUE)
  }
  if (db__is_connected()) {
    debugmsg("Connection to", db__url(), "already established")
  } else {
    debugmsg("Connecting to", db__url())
    if (penv$imlui_config$dbms$type == "sqlite") {
      db__connect_to_sqlite_dbms(force_recreate)
    } else if (penv$imlui_config$dbms$type == "postgres") {
      db__connect_to_postgres_dbms()
    } else {
      stop("`type` must be either 'sqlite' or 'postgres', not:", penv$imlui_config$dbms$type)
    }
  }
}

# Helper function for [db__connect()].
db__connect_to_sqlite_dbms <- function(force_recreate) {
  init_required <- if (util__in_devmode() && force_recreate) TRUE else FALSE
  if (!file.exists(penv$imlui_config$dbms$filepath)) {
    infomsg("File", penv$imlui_config$dbms$filepath, "does not exist.")
    init_required <- TRUE
  } else {
    if (init_required) {
      unlink(penv$imlui_config$dbms$filepath)
    }
  }
  penv$db <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = penv$imlui_config$dbms$filepath
  )
  if (init_required) {
    sql_path <- system.file("assets/sqlite/imlui_db.sql", package = "imlui")
    sql <- paste(readLines(sql_path), collapse = "\n")
    queries <- strsplit(sql, ";\n", fixed = TRUE)[[1]]
    for (query in queries) {
      db__execute(query)
    }
  }
}


# Helper function for [db__connect()].
db__connect_to_postgres_dbms <- function() {
  penv$db <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    host = penv$imlui_config$dbms$hostname,
    port = penv$imlui_config$dbms$port,
    dbname = penv$imlui_config$dbms$database,
    user = penv$imlui_config$dbms$username,
    password = penv$imlui_config$dbms$password
  )
  if (length(db__get_table_names()) == 0) {
    infomsg("Found no tables in", db__url(), "Init with default values.")
    sql_path <- system.file("assets/sqlite/imlui_db.sql", package = "imlui")
    sql <- paste(readLines(sql_path), collapse = "\n")
    queries <- strsplit(sql, ";\n", fixed = TRUE)[[1]]
    for (query in queries) {
      db__execute(query)
    }
  }
}


# Closes connection to IMLUI_DATABASE.
db__disconnect <- function() {
  trace_func_entry("db__disconnect")
  if (db__is_connected()) {
    debugmsg("Disconnecting from", db__url())
    DBI::dbDisconnect(penv$db)
  } else {
    debugmsg("Already disconnecting from", db__url())
  }
}


# Returns TRUE if IMLUI_DATABASE is disconnected, else FALSE.
db__is_connected <- function() {
  trace_func_entry("db__is_connected")
  return(!is.null(penv$db) && DBI::dbIsValid(penv$db))
}


## db__get #####################################################################


#' @title Get one Column from one Table of IMLUI_DATABASE
#' @description Get column `col` from table `tbl` as vector.
#' @param col String, name of column
#' @param tbl String, name of table
#' @examples \dontrun{
#' db__connect()
#' db__get_column("id", tbl = "users")
#' }
db__get_column <- function(col, tbl) {
  trace_func_entry("db__get_column")
  x <- db__execute(sprintf("SELECT `%s` FROM `%s`", col, tbl))
  return(x)
}


#' @title Get all column names from one Table of IMLUI_DATABASE
#' @description Get column names from table `tbl` as vector
#' @param tbl String, name of table
#' @examples \dontrun{
#' db__connect()
#' db__get_colnames("users")
#' }
db__get_colnames <- function(tbl) {
  return(DBI::dbListFields(penv$db, tbl))
}


#' @title Get one table from IMLUI_DATABASE
#' @description Get table `tbl` from IMLUI_DATABASE as dataframe
#' @param tbl String, name of table
#' @examples \dontrun{
#' db__connect()
#' db__get_table("users")
#' }
db__get_table <- function(tbl) {
  df <- DBI::dbReadTable(penv$db, tbl)
  rownames(df) <- df$id
  return(df)
}


db__get_table_names <- function(tbl) {
  return(DBI::dbListTables(penv$db))
}



#' @title Fetch valid session IDs from IMLUI_DATABASE
#' @description Get valid, i.e. not yet expired, cookies from Database.
#' @param expiry Expiry date in days
db__get_valid_cookies <- function(expiry = 7) {
  expiry_date <- as.character(lubridate::now() - lubridate::days(expiry))
  df <- db__execute(
    query = "SELECT * FROM mapping_users_sessions WHERE login_time > ?",
    params = list(expiry_date)
  )
  return(df)
}


#' @title Get default model selection for current user from database
#' @description Returns the default model selection for the current user as
#' character vector. If the setting "models" is not specified in the database
#' or contains no models available in `choices`, NULL is returned. If the
#' number of models specified (after filtering by `choices`) is still greater
#' than `n`, only the first `n` models are returned.
#' @param ses Session envrionment
#' @param choices Allowed choices
#' @param n Maximum number of models to return
db__get__default_model_selection <- function(ses, choices = NULL, n = NULL) {
  trace_func_entry("db__get__default_model_selection")
  mgs <- ses$rv$tbl$mapping_groups_settings
  idx <- which(mgs$group_id == "public" & mgs$setting_id == "models")
  if (util__is_none(mgs[idx, "value"])) {
    sel <- NULL
  } else {
    sel <- strsplit(mgs[idx, "value"], ";")[[1]]
    sel <- unname(sapply(sel, trimws))
    if (is.null(choices)) {
      sel <- intersect(sel, choices)
    }
    sel <- sel[1:min(n, length(sel))]
  }
  return(sel)
}


db__get__default_dataset_selection <- function(ses, choices = NULL, n = NULL) {
  trace_func_entry("db__get__default_dataset_selection")
  mgs <- ses$rv$tbl$mapping_groups_settings
  idx <- which(mgs$group_id == "public" & mgs$setting_id == "datasets")
  if (util__is_none(mgs[idx, "value"])) {
    sel <- NULL
  } else {
    sel <- strsplit(mgs[idx, "value"], ";")[[1]]
    sel <- unname(sapply(sel, trimws))
    if (!is.null(choices)) {
      sel <- intersect(sel, choices)
    }
    sel <- sel[seq_len(min(n, length(sel)))]
    if (length(sel) == 0) {
      sel <- NULL
    }
  }
  return(sel)
}


db__get__model_choices <- function(ses, dataset_ids = NULL) {
  trace_func_entry("db__get__model_choices")
  model_ids <- ses$rv$model$ids
  names(model_ids) <- ses$rv$model$displaynames
  if (!is.null(dataset_ids)) {
    compatible_datset_ids <- db__get__compatible_model_ids(ses, dataset_ids)
    model_ids <- intersect(model_ids, compatible_datset_ids)
  }
  return(model_ids)
}


db__get__dataset_choices <- function(ses, model_ids = NULL) {
  trace_func_entry("db__get__dataset_choices")
  dataset_ids <- ses$rv$dataset$ids
  names(dataset_ids) <- ses$rv$dataset$displaynames
  if (!is.null(model_ids)) {
    compatible_datset_ids <- db__get__compatible_dataset_ids(ses, model_ids)
    dataset_ids <- intersect(dataset_ids, compatible_datset_ids)
  }
  return(dataset_ids)
}


db__get__compatible_model_ids <- function(ses, dataset_ids) {
  trace_func_entry("db__get__compatible_model_ids")
  mmd <- ses$rv$tbl$mapping_datasets_models
  idx <- which(mmd$dataset_id %in% dataset_ids)
  model_ids <- unique(mmd[idx, "model_id"])
  return(model_ids)
}


db__get__compatible_dataset_ids <- function(ses, model_ids) {
  trace_func_entry("db__get__compatible_dataset_ids")
  mmd <- ses$rv$tbl$mapping_models_datasets
  idx <- which(mmd$model_id %in% model_ids)
  dataset_ids <- unique(mmd[idx, "dataset_id"])
  return(dataset_ids)
}


## db__set #####################################################################

#' @title Update a single value in a table 
#' @description This functions upates a single value in a table that already
#' exists. The given colum and id must also exist. Translated to R, this
#' function does the same as `idx <- which(tbl$id == id); tbl[idx, col] <- val`
#' @param tbl Name of table that should be updated
#' @param col Name of column that should be updated
#' @param val New Value
#' @param id ID of row that should be updated
db__update <- function(tbl, col, val, id) {
  query <- paste("UPDATE ", tbl, " SET ", col, " = ? WHERE id = ?")
  params <- list(val, id)
  db__execute(query, params)
}


#' @title Store user ID and session ID in IMLUI_DATABASE
#' @description This function should be called whenever a user logs in successfully. It stores the provided user ID `user_id` together with the current session ID `session_id` in table `mapping_users_sessions` of IMLUI_DATABASE. It should always be paired with call to `set_login_cookie()`. See Examples for an example.
#' @param user_id user_id
#' @param session_id sessiond_id
#' @examples \dontrun{
#' db__connect()
#' db__insert_cookie(user_id = "max", session_id = "asdf1234")
#' set_login_cookie(ses, user_id = "max", session_id = "asdf1234")
#' }
db__insert_cookie <- function(user_id, session_id) {
  x <- data.frame(
    user_id = user_id,
    session_id = session_id,
    login_time = as.character(lubridate::now()),
    expiry_time = 7
  )
  DBI::dbWriteTable(penv$db, "mapping_users_sessions", x, append = TRUE)
}


db__add_user <- function(user_id, display_name, password) {
  x <- data.frame(
    user_id = user_id,
    display_name = display_name,
    password = password
  )
  DBI::dbWriteTable(penv$db, "mapping_users_sessions", x, append = TRUE)
}


## db__util ####################################################################


#' @title Execute SQL query for IMLUI_DATABASE
#' @description Execute SQL query
#' @param query SQL query as charactern string (can include ?)
#' @param params parameters for query used as substitute for ? in `query`
#' @examples
#' \dontrun{
#' execute("INSERT INTO cars (speed, dist) VALUES (1, 1), (2, 2), (3, 3)")
#' query <- "UPDATE Appstate SET resource_value = ? WHERE
#'           user_id = ? AND resource_id = ?"
#' params <- list(url, user_id, "url")
#' execute(query, params)
#' }
db__execute <- function(query, params = NULL) {
  # Execute SQL query. Examples:
  if (grepl("SELECT", query)) {
    invisible(DBI::dbGetQuery(penv$db, query, params = params))
  } else {
    invisible(DBI::dbExecute(penv$db, query, params))
  }
}


# In case of sqlite databases, prints the path. In case of postgresql databases, prints DBNAME\@HOSTNAME:PORT, e.g. imluidb\@localhost:1234
db__url <- function() {
  trace_func_entry()
  dbms <- penv$imlui_config$dbms
  if (dbms$type == "sqlite") {
    dbms$filepath
  } else {
    paste0(dbms$database, "@", dbms$hostname, ":", dbms$port)
  }
}


db__tables <- c(
  "users",
  "models",
  "datasets",
  "papers",
  "settings",
  "appstate",
  # "samples",
  "modeltypes",
  "datatypes",
  # "methods",
  "platforms",
  "mapping_users_groups",
  "mapping_users_settings", #
  "mapping_users_models",
  "mapping_users_datasets",
  "mapping_users_resources",
  "mapping_users_sessions",
  "mapping_groups_models",
  "mapping_groups_settings", #
  "mapping_groups_datasets",
  "mapping_groups_resources",
  "mapping_papers_models",
  "mapping_papers_datasets",
  "mapping_datasets_features",
  "mapping_models_features",
  "mapping_models_classes",
  "mapping_models_datasets"
)

# # Auto generated and not tested
# db__appendTable <- function(...) DBI::dbAppendTable(penv$db, ...)
# db__canConnect <- function(...) DBI::dbCanConnect(penv$db)
# db__createTable <- function(...) DBI::dbCreateTable(penv$db)
# db__existsTable <- function(...) DBI::dbExistsTable(penv$db)
# db__fetch <- function(...) DBI::dbFetch(penv$db, ...)
# db__getInfo <- function(...) DBI::dbGetInfo(penv$db, ...)
# db__getRowCount <- function(...) DBI::dbGetRowCount(penv$db, ...)
# db__getRowsAffected <- function(...) DBI::dbGetRowsAffected(penv$db, ...)
# db__getStatement <- function(...) DBI::dbGetStatement(penv$db, ...)
# db__hasCompleted <- function(...) DBI::dbHasCompleted(penv$db, ...)
# db__isReadOnly <- function(...) DBI::dbIsReadOnly(penv$db, ...)
# db__listObjects <- function(...) DBI::dbListObjects(penv$db, ...)
# db__listResults <- function(...) DBI::dbListResults(penv$db, ...)
# db__listTables <- function(...) DBI::dbListTables(penv$db, ...)
# db__quoteIdentifier <- function(...) DBI::dbQuoteIdentifier(penv$db, ...)
# db__quoteLiteral <- function(...) DBI::dbQuoteLiteral(penv$db, ...)
# db__quoteString <- function(...) DBI::dbQuoteString(penv$db, ...)
# db__readTable <- function(...) DBI::dbReadTable(penv$db, ...)
# db__removeTable <- function(...) DBI::dbRemoveTable(penv$db, ...)
# db__rollback <- function(...) DBI::dbRollback(penv$db, ...)
# db__sendQuery <- function(...) DBI::dbSendQuery(penv$db, ...)
# db__sendStatement <- function(...) DBI::dbSendStatement(penv$db, ...)
# db__setDataMappings <- function(...) DBI::dbSetDataMappings(penv$db, ...)
# db__writeTable <- function(...) DBI::dbWriteTable(penv$db, ...)
