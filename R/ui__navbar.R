ui__navbar__left <- function(ses) {
  trace_func_entry()
  show_admin_only_tabs <-  (
    ses$rv$user$is_admin ||
    (util__in_devmode() && "developer" %in% ses$rv$user$group_ids)
  )
  html__navbar__left(
    id = "navbar_left", tabset_id = "tabset",
    ui__navbar__menu_models(ses),
    ui__navbar__menu_datasets(ses),
    if (show_admin_only_tabs) ui__navbar__menu_settings(ses),
    if (show_admin_only_tabs) ui__navbar__menu_debug(ses),
    html__navbar__item_hidden("tab__contact", text = "Contact"),
    html__navbar__item_hidden("tab__legal_notice", text = "Legal Notice"),
    html__navbar__item_hidden("tab__privacy_policy", text = "Privacy Policy")
  )
}


ui__navbar__right <- function(ses) {
  trace_func_entry()
  html__navbar__right(
    id = "navbar_right", tabset_id = "tabset",
    if (ses$rv$user$id != "public") {
      ui__navbar__menu_user(ses)
    } else {
      ui__navbar__item_login(ses)
    }
  )
}


ui__navbar__brand <- function(ses) {
  trace_func_entry()
  html__navbar__anchor(
    href_id = "tab__home",
    text = "IMLUI",
    class = "navbar-brand"
  )
}


ui__navbar__item_login <- function(ses) {
  trace_func_entry()
  html__navbar__item(
    href_id = "tab__login", text = "Login / Register"
  )
}


ui__navbar__menu_datasets <- function(ses) {
  trace_func_entry()
  html__navbar__menu(
    id = "menu_datasets",
    text = "Datasets",
    href_id = "tab__datasets",
    html__navbar__item("tab__datasets", "Datasets", style = "display: none;"),
    html__navbar__item("tab__datasets__descriptions", "Descriptions"),
    html__navbar__item("tab__datasets__msd", "MSD Plot"),
    # html__navbar__item("tab__datasets__predictions", "Model Predictions"),
    # html__navbar__item("tab__datasets__survival_curves", "Survival Curves"),
    html__navbar__item("tab__datasets__pca", "PCA"),
    html__navbar__item("tab__datasets__umap", "UMAP"),
    html__navbar__item("tab__datasets__tsne", "t-SNE")
  ) 
}


ui__navbar__menu_debug <- function(ses) {
  trace_func_entry()
  html__navbar__menu(
    href_id = "tab__debug", text = "Debug", id = "menu_debug",
    html__navbar__item(
      href_id = "tab__debug",
      text = "Debug",
      style = "display: none;"
      # Required to make bootstrap navigation work. When we click on the menu
      # while it is already open we programmatically activate the hidden tab.
      # This causes the menu to be selected and the corresponding tab to be
      # shown.
    ),
    html__navbar__item("tab__debug_cookies", "Cookies"),
    html__navbar__item("tab__debug_jquery", "jQuery"),
    html__navbar__item("tab__debug_eval_js", "Eval JS"),
    html__navbar__item("tab__debug_eval_r", "Eval R"),
  )
}


ui__navbar__menu_models <- function(ses) {
  trace_func_entry()
  html__navbar__menu(
    href_id = "tab__models", text = "Models", id = "menu_models",
    html__navbar__item("tab__models", "Models", style = "display: none;"),
    html__navbar__item("tab__mdl__descriptions", "Descriptions"),
    html__navbar__item("tab__mdl__predictions", "Predictions"),
    html__navbar__item("tab__mdl__feature_effects", "Feature Effects"),
    # html__navbar__item("tab__mdl__survival_curves", "Survival Curves"),
    # html__navbar__item("tab__mdl__non_redundance_betas", "Non Redundance Beta"),
    # html__navbar__item("tab__mdl__replacement_factor", "Replacement Factor")
  )
}


ui__navbar__menu_settings <- function(ses) {
  trace_func_entry()
  html__navbar__menu(
    id = "menu_settings", text = "Settings", href_id = "tab__settings",
    html__navbar__item(
      href_id = "tab__settings",
      text = "Settings",
      style = "display: none;"
      # Required to make bootstrap navigation work. When we click on the menu
      # while it is already open we programmatically activate the hidden tab.
      # This causes the menu to be selected and the corresponding tab to be
      # shown.
    ),
    html__navbar__item(href_id = "tab__settings__users", text = "Users"),
    html__navbar__item(href_id = "tab__settings__models", text = "Models"),
    html__navbar__item(href_id = "tab__settings__datasets", text = "Datasets"),
    html__navbar__item(href_id = "tab__settings__papers", text = "Papers"),
    html__navbar__item(href_id = "tab__settings__settings", text = "Settings"),
    html__navbar__item(href_id = "tab__settings__appstate", text = "Appstate"),
    html__navbar__item(href_id = "tab__settings__modeltypes", text = "Modeltypes"),
    html__navbar__item(href_id = "tab__settings__datatypes", text = "Datatypes"),
    html__navbar__item(href_id = "tab__settings__platforms", text = "Platforms"),
    html__navbar__item(href_id = "tab__settings__mapping_users_groups", text = "Mapping Users Groups"),
    html__navbar__item(href_id = "tab__settings__mapping_users_settings", text = "Mapping Users Settings"),
    html__navbar__item(href_id = "tab__settings__mapping_users_models", text = "Mapping Users Models"),
    html__navbar__item(href_id = "tab__settings__mapping_users_datasets", text = "Mapping Users Datasets"),
    html__navbar__item(href_id = "tab__settings__mapping_users_resources", text = "Mapping Users Resources"),
    html__navbar__item(href_id = "tab__settings__mapping_users_sessions", text = "Mapping Users Sessions"),
    html__navbar__item(href_id = "tab__settings__mapping_groups_models", text = "Mapping Groups Models"),
    html__navbar__item(href_id = "tab__settings__mapping_groups_settings", text = "Mapping Groups Settings"),
    html__navbar__item(href_id = "tab__settings__mapping_groups_datasets", text = "Mapping Groups Datasets"),
    html__navbar__item(href_id = "tab__settings__mapping_groups_resources", text = "Mapping Groups Resources"),
    html__navbar__item(href_id = "tab__settings__mapping_papers_models", text = "Mapping Papers Models"),
    html__navbar__item(href_id = "tab__settings__mapping_papers_datasets", text = "Mapping Papers Datasets"),
    html__navbar__item(href_id = "tab__settings__mapping_datasets_features", text = "Mapping Datasets Features"),
    html__navbar__item(href_id = "tab__settings__mapping_models_features", text = "Mapping Models Features"),
    html__navbar__item(href_id = "tab__settings__mapping_models_classes", text = "Mapping Models Classes"),
    html__navbar__item(href_id = "tab__settings__mapping_models_datasets", text = "Mapping Models Datasets")
  )
}


ui__navbar__menu_user <- function(ses) {
  trace_func_entry()
  html__navbar__menu(
    id = "menu_user",
    text = if (ses$rv$user$is_admin) {
      sprintf("%s (admin)", ses$rv$user$id)
    } else {
      ses$rv$user$id
    },
    href_id = "tab__user",
    html__navbar__item(href_id = "tab__user_profile", text = "Your Profile"),
    html__navbar__item(href_id = "tab__user_settings", text = "Your Settings"),
    tags$li(class = "divider"),
    html__navbar__item(href_id = "tab__user_logout", text = "Logout")
  )
}
