# text: test to display
# href: INTERNAL href, e.g. #tab__user_profile
html__ia <- function(href_id, text, ...) {
  tags$a(
    href = "#",
    `data-value` = href_id,
    class = "imlui-internal-anchor",
    onclick = paste0("LandingPage.Tab.activate('", href_id, "')"),
    text,
    ...
  )
}


html__sidebar_analyze <- function(ses, id = "sidebar") {

}


html__tab__settings <- function(ses,
                                tbl_name,
                                cols_ignore = c(),
                                cols_show = "_all") {

  tbl <- db__get_table(tbl_name)
  tbl_id <- paste0("tbl_", tbl_name)
  tbl_edit_id <- paste0(tbl_id, "_cell_edit") # 1)
  tbl_cols <- colnames(tbl)
  # 1) The DT javascript will update `input[[tbl_edit_id]]` each time
  # the table is updated by the website user. This ID is hardcoded in the DT
  # package and therefore cannot be changed.

  # Create actual HTML for tab
  tab <- div(
    class = "container-fluid imlui-full-height",
    fluidRow(
      column(
        width = 12,
        p("Double click a cell to edit it. Accept changes by clicking somewhere else. Reload the page to use the updated values."),
        DT::DTOutput(outputId = tbl_id)
      )
    )
  )

  # Create reactive for displaying Table as HTML element
  ses$output[[tbl_id]] <- render__tbl(
    expr = tbl,
    show = cols_show,
    ignore = cols_ignore
  )

  # Create reactive for handling Table edits
  if (tbl_name %in% ses$hndl$tbl) {
    warnmsg(paste0("ses$hndl$tbl$", tbl_name, " exists already."))
  } else {
    ses$hndl$tbl[[tbl_edit_id]] <- observeEvent(
      eventExpr = ses$input[[tbl_edit_id]],
      handlerExpr = {
        row_nr <- ses$input[[tbl_edit_id]]$row # rows start counting at 1
        col_nr <- ses$input[[tbl_edit_id]]$col + 1 # cols start counting at 0
        col_name <- tbl_cols[col_nr]
        id <- tbl[row_nr, "id"]
        val <- ses$input[[tbl_edit_id]]$value
        infomsg("UPDATE", tbl_name, "SET", col_name, "=", val, "WHERE id =", id)
        db__update(tbl = tbl_name, col = col_name, val = val, id = id)
      }
    )
  }

  return(tab)
}


html__tab <- function(ses, id, func, active = FALSE) {
  trace_func_entry("html__tab")
  content_id <- gsub("tab__", "tab__content_", id)
  content <- uiOutput(content_id)
  ses$output[[content_id]] <- renderUI({
    debugmsg("Rendering", id)
    # debugmsg("ses$input$navbar_left:", ses$input$navbar_left)
    # debugmsg("ses$input$navbar_right:", ses$input$navbar_right)
    # debugmsg("ses$input$active_tab:", ses$input$active_tab)
    func(ses)
  })
  tags$div(
    class = paste0("tab-pane", if (active) " active" else ""),
    id = id,
    `data-value` = id,
    content
  )
}


html__tabset <- function(id, tabs = NULL) {
  trace_func_entry("html__tabset")
  tags$div(
    id = paste0(id, "-container"),
    class = "container-fluid imlui-full-height",
    style = "padding: 0px 0px 0px 0px;",
    tags$div(
      id = id,
      class = "tab-content imlui-full-height",
      `data-tabsetid` = id,
      do.call(tagList, tabs)
    )
  )
}


html__col_sb_item <- function(...) {
  trace_func_entry("html__col_sb_item")
  # HTML > Column > Sidebar > Item
  div(class = "col-xs-6 col-sm-12 col-md-12 col-lg-12", ...)
}


html__col_sb <- function(...) {
  trace_func_entry("html__col_sb")
  # HTML > Column > Sidebar
  div(
    class = "col-xs-12 col-sm-5 col-md-4 col-lg-3 imlui-full-height",
    style = paste(
      "padding: 15px;",
      "border-right: 1px solid;",
      "border-color: #cccccc;",
      "background-color: #f5f5f5;" # background-color: LightPink;
    ),
    ...
  )
}


html__col_main <- function(...) {
  trace_func_entry("html__col_main")
  # HTML > Column > Main Area
  div(
    class = "col-xs-12 col-sm-7 col-md-8 col-lg-9 imlui-full-height imlui-col-main",
    # background-color: LightSkyBlue;
    ...
  )
}


html__hndl_sc_inp_datasets <- function(ses, n, model_inp_id, dataset_inp_id) {
  trace_func_entry("html__hndl_sc_inp_datasets")
  debugmsg("Args:", utils__dput(as.list(environment())))
  ds_sel <- ses$input[[dataset_inp_id]] # NULL or character vector
  mdl_choices <- db__get__model_choices(ses, dataset_ids = ds_sel)
  mdl_sel <- ses$input[[model_inp_id]] # NULL or character vector
  mdl_sel_new <- intersect(mdl_sel %||% character(), mdl_choices)
  if (length(mdl_sel_new) == 0) {
    mdl_sel_new <- db__get__default_model_selection(ses, mdl_choices, n)
  }
  freezeReactiveValue(ses$input, model_inp_id)
  updatePickerInput(
    session = ses$session,
    inputId = model_inp_id,
    choices = mdl_choices,
    selected = mdl_sel_new,
  )
}


html__hndl_sc_inp_models <- function(ses, n, dataset_inp_id, model_inp_id) {
  trace_func_entry("html__hndl_sc_inp_models")
  debugmsg("Args:", utils__dput(as.list(environment())))
  mdl_sel <- ses$input[[model_inp_id]] # NULL or character vector
  ds_choices <- db__get__dataset_choices(ses, model_ids = mdl_sel)
  ds_sel <- ses$input[[dataset_inp_id]] # NULL or character vector
  ds_sel_new <- intersect(ds_sel %||% character(), ds_choices)
  if (length(ds_sel_new) == 0) {
    ds_sel_new <- db__get__default_dataset_selection(
      ses = ses,
      choices = ds_choices,
      n = n
    )
  }
  updatePickerInput(
    session = ses$session,
    inputId = dataset_inp_id,
    choices = ds_choices,
    selected = ds_sel_new,
  )
}


#' @export
#' @title Dropdown menu to select Models
#' @description Dropdown menu to select Models.
#' @param ses session environment
#' @param id id for generated html element
#' @param n maximum number of allowed selections
#' @param multiple Set TRUE if multiple selections are allowed
#' @param dataset_inp_id ID of a `html__inp_model` element. If not NULL, the
#' set of models availble in the returned HTML pickerInput will be restricted to
#' models compatible to the selected datasets from `dataset_inp_id`.
#' @details Documentation Links:
#'
#' * <https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html>
#' * <https://dreamrs.github.io/shinyWidgets/reference/pickerOptions.html>
#' * <https://developer.snapappointments.com/bootstrap-select/options/>
#'
#' Unfortunately, the corresponding bootstrap widget does not allow to specify
#' multiple selections together with a minimum number of selections. Therefor we
#' only have parameter n, for number of max selections. However, if multiple
#' is set to TRUE, exactly one selection is required.
html__inp_models <- function(ses,
                             id,
                             n = 1,
                             multiple = TRUE,
                             dataset_inp_id = NULL) {

  trace_func_entry(sprintf("html__inp_models(id = %s)", utils__dput(id)))

  # Bind variables in function scope so observers can find them
  ses <- ses
  model_inp_id <- id <- id
  dataset_inp_id <- dataset_inp_id

  # Setup observer for dataset_inp_id
  if (!is.null(dataset_inp_id) && is.null(ses$hndl$sc[[dataset_inp_id]])) {
    debugmsg(paste0("Setting up observer for ses$hndl$sc$", dataset_inp_id))
    ses$hndl$sc[[dataset_inp_id]] <- observeEvent(
      eventExpr = ses$input[[dataset_inp_id]],
      handlerExpr = html__hndl_sc_inp_datasets(
        ses = ses,
        n = n,
        model_inp_id = model_inp_id,
        dataset_inp_id = dataset_inp_id
      )
    )
  }

  # Return HTML pickerInput element for Model Selection
  picker_input_element <- pickerInput(
    inputId = id,
    label = if (n == 1) "Model" else "Models",
    choices = choices <- db__get__model_choices(ses),
    selected = db__get__default_model_selection(ses, choices, n),
    multiple = multiple,
    options = list(maxOptions = n),
  )

  return(picker_input_element)
}


html__inp_datasets <- function(ses,
                               id,
                               n = 1,
                               multiple = TRUE,
                               model_inp_id = NULL) {

  trace_func_entry(sprintf("html__inp_datasets(id = %s)", utils__dput(id)))

  # Bind variables in function scope so observers can find them
  ses <- ses
  dataset_inp_id <- id <- id
  n <- n
  multiple <- multiple
  model_inp_id <- model_inp_id

  # Setup observer for model_inp_id
  if (!is.null(model_inp_id) && is.null(ses$hndl$sc[[model_inp_id]])) {
    debugmsg(paste0("Setting up observer for ses$hndl$sc$", model_inp_id))
    ses$hndl$sc[[model_inp_id]] <- observeEvent(
      eventExpr = ses$input[[model_inp_id]],
      handlerExpr = html__hndl_sc_inp_models(
        ses = ses,
        n = n,
        dataset_inp_id = dataset_inp_id,
        model_inp_id = model_inp_id
      )
    )
  }

  # Return HTML pickerInput element for Dataset Selection
  picker_input_element <- pickerInput(
    inputId = id,
    label = if (n == 1) "Dataset" else "Datasets",
    choices = choices <- db__get__dataset_choices(ses),
    selected = db__get__default_dataset_selection(ses, choices, n),
    multiple = multiple,
    options = list(maxOptions = n),
  )

  return(picker_input_element)
}


html__row <- function(...) {
  trace_func_entry("html__row")
  div(class = "row", ...)
}


html__row_equal <- function(...) {
  trace_func_entry("html__row_equal")
  # Same as html__row but all columns inside this row will have equal height
  div(class = "row equal", ...)
}


html__row_equal_full_height <- function(...) {
  trace_func_entry("html__row_equal_full_height")
  # Same as html__row but all columns inside this row will have equal height
  # and have at least the size `vh - (navbar + breadcrumb + footer)`
  div(class = "row equal imlui-full-height", ...)
}

# Deprecated
tags <- htmltools::tags
a <- tags$a
b <- tags$b
br <- tags$br
div <- tags$div
h1 <- tags$h1
h2 <- tags$h2
h3 <- tags$h3
h4 <- tags$h4
h5 <- tags$h5
h6 <- tags$h6
img <- tags$img
li <- tags$li
ol <- tags$ol
p <- tags$p
ul <- tags$ul
code <- function(...) {
  tags$pre(tags$code(...))
}



html__a <- function(href, ...) { tags$a(href = href, ...) }
html__b <- function(...) { tags$b(...) }
html__br <- function(...) { tags$br(...) }
html__div <- function(...) { tags$div(...) }
html__h1 <- function(...) { tags$h1(...) }
html__h2 <- function(...) { tags$h2(...) }
html__h3 <- function(...) { tags$h3(...) }
html__h4 <- function(...) { tags$h4(...) }
html__h5 <- function(...) { tags$h5(...) }
html__h6 <- function(...) { tags$h6(...) }
html__img <- function(...) { tags$img(...) }
html__li <- function(...) { tags$li(...) }
html__ol <- function(...) { tags$ol(...) }
html__p <- function(...) { tags$p(...) }
html__ul <- function(...) { tags$ul(...) }
html__span <- function(...) { htmltools::span(...) }
