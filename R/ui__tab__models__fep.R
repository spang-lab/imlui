
ui__tab__mdl__feature_effects <- function(ses) {
  trace_func_entry("ui__tab__mdl__feature_effects")
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(ui__tab__mdl__feature_effects_input_model_m(ses)),
          html__col_sb_item(ui__tab__mdl__feature_effects_input_dataset_d(ses))
        ),
        html__row(
          html__col_sb_item(ui__tab__mdl__feature_effects_input_sample_s(ses)),
          html__col_sb_item(ui__tab__mdl__feature_effects_input_number_of_lines_n(ses))
        ),
        html__row(
          html__col_sb_item(ui__tab__mdl__feature_effects_input_threshold_t(ses))
        )
        # html__row(
        #   html__col_sb_item(ui__tab__mdl__feature_effects_input_features_f(ses)),
        #   html__col_sb_item(ui__tab__mdl__feature_effects_input_feature_value_v(ses))
        # ),
        # html__row(
        #   html__col_sb_item(ui__tab__mdl__feature_effects_input_debug_dbg(ses))
        # )
      ),
      html__col_main(
        html__row(
          ui__tab__mdl__feature_effects_output_fep(ses)
        )
      )
    )
  )
}

ui__tab__mdl__feature_effects_output_fep <- function(ses) {
  trace_func_entry("ui__tab__mdl__feature_effects_output_fep")
  ses$output$tab__mdl__feature_effects_plot <- shiny::renderImage(
    expr = {
      trace_func_entry("ui__tab__mdl__feature_effects_output_fep > renderImage")
      inp <- ses$input
      req(inp$models_fep_m, inp$models_fep_d, inp$fep_s, inp$fep_n, inp$fep_t)
      b <- get_model(id = inp$models_fep_m, ses = ses)
      X <- get_dataset(id = inp$models_fep_d, ses = ses)
      s <- inp$fep_s
      if ("Intercept" %in% names(b)) {
        b <- b[!(names(b) == "Intercept")]
      }
      # browser()
      req(all(names(b) %in% colnames(X)))
      req(s %in% rownames(X))
      arglist <- list(
        b = b, X = X, s = s, n_extreme = inp$fep_n, threshold = inp$fep_t
      )
      render__store_as_svg(
        pname = "fep",
        func = plot__fep,
        arglist = arglist,
        width = 1200,
        height = 400,
      )
    },
    deleteFile = FALSE
  )
  div(
    class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
    plotOutput(outputId = "tab__mdl__feature_effects_plot")
  )
}

ui__tab__mdl__feature_effects_input_model_m <- function(ses) {
  trace_func_entry("ui__tab__mdl__feature_effects_input_model_m")
  html__inp_models(
    ses = ses,
    id = "models_fep_m",
    n = 1,
    multiple = FALSE
  )
}

ui__tab__mdl__feature_effects_input_dataset_d <- function(ses) {
  trace_func_entry("ui__tab__mdl__feature_effects_input_dataset_d")
  html__inp_datasets(
    ses = ses,
    id = "models_fep_d",
    n = 1,
    multiple = FALSE,
    model_inp_id = "models_fep_m"
  )
}

ui__tab__mdl__feature_effects_input_sample_s <- function(ses) {
  trace_func_entry("ui__tab__mdl__feature_effects_input_sample_s")
  ses$hndl$sc$input_fep_d <- observeEvent(
    eventExpr = ses$input$models_fep_d,
    handlerExpr = ui__tab___models__hndl__sc__input_fep_d(ses)
  )
  pickerInput(
    inputId = "fep_s",
    label = "Sample",
    choices = NULL
  )
}

ui__tab__mdl__feature_effects_input_number_of_lines_n <- function(ses) {
  trace_func_entry()
  sliderInput(
    inputId = "fep_n",
    label = "Number of Lines",
    value = 0,
    min = 0,
    max = 100
  )
}

ui__tab__mdl__feature_effects_input_threshold_t <- function(ses) {
  sliderInput(
    inputId = "fep_t",
    label = "Threshold",
    value = 0.991,
    min = -3,
    max = 3,
    step = 0.01
  )
}

ui__tab__mdl__feature_effects_input_features_f <- function(ses) {
  trace_func_entry()
  ses$output$fep_f <- renderUI({
    trace_func_entry("ui__tab__mdl__feature_effects_input_features_f > renderUi")
    model_id <- ses$input$models_fep_m
    choices <- if (is.null(model_id)) NULL else names(get_model(model_id, ses))
    selectInput(inputId = "fep_f", label = "Feature", choices = choices)
  })
  uiOutput("fep_f")
}

ui__tab__mdl__feature_effects_input_debug_dbg <- function(ses) {
  trace_func_entry()
  checkboxInput(
    inputId = "fep_dbg",
    label = "Debug"
  )
}

ui__tab__mdl__feature_effects_input_feature_value_v <- function(ses) {
  trace_func_entry()
  ses$output$fep_v <- renderUI({
    trace_func_entry("ui__tab__mdl__feature_effects_input_feature_value_v > renderUi")
    sliderInput(
      inputId="fep_v",
      label="Feature Value",
      min=min(-10),
      max=max(10),
      step=1,
      value=0
    )
  })
  uiOutput("fep_v")
}

ui__tab___models__hndl__sc__input_fep_d <- function(ses) {
  trace_func_entry("ui__tab___models__hndl__sc__input_fep_d")
  # 1) freezeReactiveValue is broken in combination with bookmarking
  ds <- ses$input$models_fep_d
  updatePickerInput(
    session = ses$session,
    inputId = "fep_s",
    choices = if (is.null(ds)) NULL else rownames(get_dataset(ds, ses))
  )
}
