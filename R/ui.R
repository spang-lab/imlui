ui__landing_page <- function(ses) {
  ses <- ses
  trace_func_entry()
  x <- ses$const$url_params$active_tab
  x_invalid <- (is.null(x)) ||
    (grepl("(null|tab__user_logout)", x)) ||
    (grepl("tab__login", x) && ses$rv$user$id != "public")
  active_tab <- if (x_invalid) "tab__home" else gsub("['\" \t]", "", x)
  debugmsg("Active Tab:", active_tab)
  tagList(
    div(
      id = "navbar_page", class = "container-fluid",
      ui__navbar(ses),
      ui__breadcrumb(ses),
      ui__tabset(ses)
    ),
    ui__footer(ses),
    tags$script("LandingPage.registerEventHandlers()"), # 1
    tags$script(paste0("LandingPage.Tab.activate('", active_tab, "')")) # 1
    # 1) defined in file event-handlers-1.0.0.js
  )
}


ui__breadcrumb <- function(ses) {
  trace_func_entry()
  # Update of breadcrumb after click on `html__ia` is handled via
  # corresponding "onclick" actions and update of breadcrumb after click on
  # `html__navbar__anchor` is handled by event handler defined in function
  # `registerEventHandlers` of file `event-handlers-1.0.0.js`.
  tags$ol(
    class = "breadcrumb",
    tags$li(html__ia(href_id = "tab__home", text = "Home"))
  )
}


ui__footer <- function(ses) {
  trace_func_entry()
  tags$footer(
    tags$div(
      id = "footer_main_container",
      tags$p(
        id = "footer_left",
        tags$span(paste("Version", utils::packageVersion("imlui")))
      ),
      tags$p(
        id = "footer_center",
        html__ia(
          href_id = "tab__contact",
          text = "Contact"
        ),
        tags$span(" | "),
        html__ia(
          href_id = "tab__legal_notice",
          text = "Legal Notice"
        ),
        tags$span(" | "),
        html__ia(
          href_id = "tab__privacy_policy",
          text = "Privacy Policy"
        )
        # ,
        # tags$a(href = "Impressum", "Contact"), tags$span(" | "),
        # tags$a(href = "Legal Notice", "Legal Notice"), tags$span(" | "),
        # tags$a(href = "Privacy Policy", "Privacy Policy"),
      ),
      tags$p(
        id = "footer_right",
        tags$span("\u00a9 2022 University Regensburg")
      )
    )
  )
}


ui__tabset <- function(ses) {
  trace_func_entry()
  show_admin_only_tabs <-  (
    ses$rv$user$is_admin ||
    (util__in_devmode() && "developer" %in% ses$rv$user$group_ids)
  )
  tab__func_names <- c(
    # Home
    "ui__tab__home",
    # Models
    "ui__tab__models",
    "ui__tab__mdl__feature_effects",
    "ui__tab__mdl__descriptions",
    "ui__tab__mdl__predictions",
    "ui__tab__mdl__survival_curves",
    "ui__tab__mdl__non_redundance_betas",
    "ui__tab__mdl__replacement_factor",
    # Datasets
    "ui__tab__datasets",
    "ui__tab__datasets__descriptions",
    "ui__tab__datasets__msd",
    "ui__tab__datasets__predictions",
    "ui__tab__datasets__survival_curves",
    "ui__tab__datasets__pca",
    "ui__tab__datasets__umap",
    "ui__tab__datasets__tsne",
    if (show_admin_only_tabs) c(
      # settings
      "ui__tab__settings",
      "ui__tab__settings__users",
      "ui__tab__settings__models",
      "ui__tab__settings__datasets",
      "ui__tab__settings__papers",
      "ui__tab__settings__settings",
      "ui__tab__settings__appstate",
      "ui__tab__settings__modeltypes",
      "ui__tab__settings__datatypes",
      "ui__tab__settings__platforms",
      "ui__tab__settings__mapping_users_groups",
      "ui__tab__settings__mapping_users_settings",
      "ui__tab__settings__mapping_users_models",
      "ui__tab__settings__mapping_users_datasets",
      "ui__tab__settings__mapping_users_resources",
      "ui__tab__settings__mapping_users_sessions",
      "ui__tab__settings__mapping_groups_models",
      "ui__tab__settings__mapping_groups_settings",
      "ui__tab__settings__mapping_groups_datasets",
      "ui__tab__settings__mapping_groups_resources",
      "ui__tab__settings__mapping_papers_models",
      "ui__tab__settings__mapping_papers_datasets",
      "ui__tab__settings__mapping_datasets_features",
      "ui__tab__settings__mapping_models_features",
      "ui__tab__settings__mapping_models_classes",
      "ui__tab__settings__mapping_models_datasets",
      # debug
      "ui__tab__debug",
      "ui__tab__debug_cookies",
      "ui__tab__debug_jquery",
      "ui__tab__debug_eval_js",
      "ui__tab__debug_eval_r"
    ),
    # User
    "ui__tab__user",
    "ui__tab__user_profile",
    "ui__tab__user_settings",
    "ui__tab__user_logout",
    "ui__tab__login",
    # Footer
    "ui__tab__contact",
    "ui__tab__legal_notice",
    "ui__tab__privacy_policy"
  )
  html__tabs <- lapply(tab__func_names, function(func_name) {
    id <- gsub("ui__", "", func_name)
    func <- get(func_name)
    html__tab(ses, id, func)
  })
  ui__tabset <- html__tabset(id = "tabset", tabs = html__tabs)
  return(ui__tabset)
}


ui__navbar <- function(ses) {
  trace_func_entry()
  html__navbar__main(
    id = "navbar", collapsible = TRUE, brand = ui__navbar__brand(ses),
    ui__navbar__left(ses),
    ui__navbar__right(ses)
  )
}
