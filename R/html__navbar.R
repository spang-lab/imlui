#' @title Main Navbar Element
#' @description Main Bootstrap navbar element
#' @param id e.g. "navbar-1"
#' @param brand e.g. tags$span(class = "navbar-brand", "IMLUI")
#' @param ... ul elements with class navbar-left or navbar-right, as generated
#' by `html__navbar__left()` or `html__navbar__right()`
#' @param collapsible whether navbar should be collapsible or not
html__navbar__main <- function(id, brand, ..., collapsible = TRUE) {
  trace_func_entry("html__navbar__main")
  main_container_id <- paste0(id, "_main_container")
  header_id <- paste0(id, "_header")
  body_id <- paste0(id, "_body")
  class <- "navbar navbar-default navbar-static-top navbar-inverse"
  tags$nav(id = id, class = class, role = "navigation",
    if (collapsible) {
      tags$div(id = main_container_id, class = "container-fluid",
        tags$div(id = header_id, class = "navbar-header",
          html__navbar__collapse_button(body_id),
          brand
        ),
        tags$div(id = body_id, class = "navbar-collapse collapse",
          ...
        )
      )
    } else {
      tags$div(id = main_container_id, class = "container-fluid",
        tags$div(id = header_id, class = "navbar-header", brand),
        ...
      )
    }
  )
}


html__navbar__left <- function(id, tabset_id, ...) {
  trace_func_entry("html__navbar__left")
  tags$ul(
    class = "nav navbar-nav navbar-left shiny-tab-input shiny-bound-input",
    id = id,
    `data-tabsetid` = tabset_id,
    ...
  )
}


html__navbar__right <- function(id, tabset_id, ...) {
  trace_func_entry("html__navbar__right")
  tags$ul(
    class = "nav navbar-nav navbar-right shiny-tab-input shiny-bound-input",
    id = id,
    `data-tabsetid` = tabset_id,
    ...
  )
}


html__navbar__anchor <- function(href_id, text, ...) {
  trace_func_entry("html__navbar__anchor")
  tags$a(
    href = paste0("#", href_id),
    `data-toggle` = "tab",
    `data-bs-toggle` = "tab",
    `data-value` = href_id,
    class = "imlui-navbar-anchor",
    text,
    ...
  )
}


html__navbar__collapse_button <- function(body_id) {
  trace_func_entry("html__navbar__collapse_button")
  tags$button(
    type = "button",
    class = "navbar-toggle collapsed",
    `data-toggle` = "collapse",
    `data-target` = paste0("#", body_id),
    `data-bs-toggle` = "collapse",
    `data-bs-target` = paste0("#", body_id),
    tags$span(class = "sr-only visually-hidden", "Toggle navigation"),
    tags$span(class = "icon-bar"),
    tags$span(class = "icon-bar"),
    tags$span(class = "icon-bar")
  )
}


html__navbar__header <- function(body_id = NULL) {
  trace_func_entry("html__navbar__header")
  tags$div(
    class = "navbar-header",
    if (!is.null(body_id)) {
      html__navbar__collapse_button(body_id)
    },
    tags$span(class = "navbar-brand", "IMLUI")
  )
}


html__navbar__item <- function(href_id, text, ...) {
  trace_func_entry()
  tags$li(html__navbar__anchor(href_id, text), ...)
}


html__navbar__item_hidden <- function(href_id, text, ...) {
  trace_func_entry()
  tags$li(html__navbar__anchor(href_id, text), style = "display: none;", ...)
}


html__navbar__menu <- function(id, text, ..., href_id = "#") {
  trace_func_entry("html__navbar__menu")
  tags$li(
    class = "dropdown imlui-navbar-menu",
    tags$a(
      href = "#", # paste0("#", href_id),
      class = "dropdown-toggle",
      `data-toggle` = "dropdown",
      `data-bs-toggle` = "dropdown",
      `data-value` = href_id,
      `data-target` = href_id,
      text,
      tags$b(class = "caret")
    ),
    tags$ul(
      class = "dropdown-menu",
      `data-tabsetid` = id,
      ...
    )
  )
}


html__navbar__submenu <- function(id, text, ..., href_id = "#") {
  # <li class="dropdown-submenu">
  #     <a tabindex="-1" href="#" class="dropdown-submenu-toggle">
  #       Second Dropdown <b class="caret"></b>
  #     </a>
  #     <ul class="dropdown-menu">
  #         <li><a href="#">Sub-Menu Item</a></li>
  #         <li><a href="#">Sub-Menu Item</a></li>
  #         <li><a href="#">Sub-Menu Item</a></li>
  #     </ul>
  # </li>
  tags$li(
    class = "dropdown-submenu imlui-navbar-submenu",
    tags$a(
      tabindex="-1",
      href = "#",
      class = "dropdown-submenu-toggle",
      # `data-toggle` = "dropdown",
      # `data-bs-toggle` = "dropdown",
      # `data-value` = href_id,
      # data-toggle="dropdown"
      # role="button"
      # aria-haspopup="true"
      # aria-expanded="false"
      text,
      tags$b(class = "caret")
    ),
    tags$ul(
      class = "dropdown-menu",
      ...
    )
  )
}
