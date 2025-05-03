init__rv_dataset <- function(ses) {
  trace_func_entry("init__rv_dataset")
  df <- db__get_table("datasets")
  ids <- init__rv__accessible_dataset_ids(ses$rv)
  symbols <- `names<-`(df[ids, "symbol"], ids)
  displaynames <- `names<-`(df[ids, "name"], ids)
  pkgs <- `names<-`(df[ids, "package"], ids)
  transpose <- `names<-`(as.logical(df[ids, "transpose"]), ids)
  ses$rv$dataset$ids <- ids
  ses$rv$dataset$symbols <- symbols
  ses$rv$dataset$displaynames <- displaynames
  ses$rv$dataset$pkgs <- pkgs
  ses$rv$dataset$transpose <- transpose
}


init__rv_model <- function(ses) {
  trace_func_entry("init__rv_dataset")
  dfs <- db__get_table("models")
  ids <- init__rv__get_accessible_model_ids(ses$rv)
  symbols <- `names<-`(dfs[ids, "symbol"], ids)
  displaynames <- `names<-`(dfs[ids, "name"], ids)
  pkgs <- `names<-`(dfs[ids, "package"], ids)
  ses$rv$model$ids <- ids
  ses$rv$model$symbols <- symbols
  ses$rv$model$displaynames <- displaynames
  ses$rv$model$pkgs <- pkgs
}


init__rv__accessible_dataset_ids <- function(rv) {
  trace_func_entry()
  uid <- rv$user$id
  gids <- rv$user$group_ids
  d <- db__get_table("datasets")
  ids_valid <- d[nchar(d$symbol) > 0, "id"]
  if (is.null(rv$user$id)) {
    stop("rv$user$id must not be null")
  } else if (is.null(rv$user$group_ids)) {
    stop("rv$user$group_ids must not be null")
  }
  # Group permissions
  mgd <- db__get_table("mapping_groups_datasets")
  idx_grp <- mgd$group_id %in% gids
  idx_ign <- mgd$permission_id == "ignore"
  idx_grp_ign <- idx_grp & idx_ign
  if ("admin" %in% gids) {
    ids_ign <- mgd$dataset_id[idx_grp_ign]
    ids_grp_use <- setdiff(ids_valid, ids_ign)
  } else {
    idx_use <- mgd$permission_id %in% c("select", "use", "download")
    idx_grp_use <- idx_grp & idx_use
    ids_grp_use <- mgd$dataset_id[idx_grp_use & (!idx_grp_ign)]
  }
  # User permissions
  mud <- db__get_table("mapping_users_datasets")
  idx_usr <- mud$user_id == uid
  idx_use <- mud$permission_id %in% c("select", "use", "download")
  idx_ign <- mud$permission_id == "ignore"
  idx_usr_use <- idx_usr & idx_use
  idx_usr_ign <- idx_usr & idx_ign
  ids_usr_use <- mud$dataset_id[idx_usr_use & (!idx_usr_ign)]
  # Combined permissions
  authorized_ids <- unique(c(ids_grp_use, ids_usr_use))
  accessible_ids <- intersect(ids_valid, authorized_ids)
  return(accessible_ids)
}


init__rv__get_accessible_model_ids <- function(rv) {
  trace_func_entry()
  uid <- rv$user$id
  gids <- rv$user$group_ids
  m <- db__get_table("models")
  ids_valid <- m[nchar(m$symbol) > 0, "id"]
  if (is.null(rv$user$id)) {
    stop("rv$user$id must not be null")
  } else if (is.null(rv$user$group_ids)) {
    stop("rv$user$group_ids must not be null")
  }
  # Group permissions
  mgm <- db__get_table("mapping_groups_models")
  idx_grp <- mgm$group_id %in% gids
  idx_ign <- mgm$permission_id == "ignore"
  idx_grp_ign <- idx_grp & idx_ign
  if ("admin" %in% gids) {
    ids_ign <- mgm$model_id[idx_grp_ign]
    ids_grp_use <- setdiff(ids_valid, ids_ign)
  } else {
    idx_use <- mgm$permission_id %in% c("select", "use", "download")
    idx_grp_use <- idx_grp & idx_use
    ids_grp_use <- mgm$model_id[idx_grp_use & (!idx_grp_ign)]
  }
  # User permissions
  mum <- db__get_table("mapping_users_models")
  idx_usr <- mum$user_id == uid
  idx_use <- mum$permission_id %in% c("select", "use", "download")
  idx_ign <- mum$permission_id == "ignore"
  idx_usr_use <- idx_usr & idx_use
  idx_usr_ign <- idx_usr & idx_ign
  ids_usr_use <- mum$model_id[idx_usr_use & (!idx_usr_ign)]
  # Combined permissions
  authorized_ids <- unique(c(ids_grp_use, ids_usr_use))
  accessible_ids <- intersect(ids_valid, authorized_ids)
  return(accessible_ids)
}
