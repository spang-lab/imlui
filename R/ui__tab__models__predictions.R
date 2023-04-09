ui__tab__mdl__predictions <- function(ses) {
  trace_func_entry("ui__tab__mdl__predictions")
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(html__inp_models(
            ses = ses,
            id = "mdl_pred_m",
            n = 1,
            multiple = FALSE
          )),
          html__col_sb_item(html__inp_datasets(
            ses,
            "mdl_pred_d",
            n = 1,
            multiple = FALSE,
            model_inp_id = "mdl_pred_m"
          ))
        ),
        html__row(
          html__col_sb_item(ui__tab__mdl__predictions__input_sample_s(ses))
        ),
      ),
      html__col_main(
        html__row(
          ui__tab__mdl__predictions__plot_output_o(ses)
        )
      )
    )
  )
}


ui__tab__mdl__predictions__plot_output_o <- function(ses) {
  trace_func_entry("ui__tab__mdl__predictions__plot_output_o")
  ses$output$tab__mdl__predictions_plot <- shiny::renderImage(
    expr = {
      trace_func_entry("ui__tab__mdl__predictions__plot_output_o > pred_args")
      inp <- ses$input
      req(inp$mdl_pred_m, inp$mdl_pred_d, inp$mdl_pred_s)
      b <- get_model(id = inp$mdl_pred_m, ses = ses)
      X <- get_dataset(id = inp$mdl_pred_d, ses = ses)
      s <- inp$mdl_pred_s
      n <- nrow(X)
      if ("Intercept" %in% names(b)) {
        b <- b[!(names(b) == "Intercept")]
      }
      req(all(names(b) %in% colnames(X)))
      arglist <- list(b = b, X = X, s = s)
      render__store_as_svg(
        pname = "pred",
        func = plot__pred,
        arglist = arglist,
        height = 400 + n * 5
      )
    },
    deleteFile = FALSE
  )
  div(
    class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
    plotOutput(outputId = "tab__mdl__predictions_plot")
  )
}

ui__tab__mdl__predictions__input_sample_s <- function(ses) {
  trace_func_entry("ui__tab__mdl__predictions__input_sample_s")
  ses$hndl$sc$input_pred_d <- observeEvent(
    eventExpr = ses$input$mdl_pred_d,
    handlerExpr = ui__tab__mdl__predictions__hndl_sc_mdl_pred_d(ses)
  )
  selectInput(
    inputId = "mdl_pred_s",
    label = "Sample",
    choices = c()
  )
}


ui__tab__mdl__predictions__hndl_sc_mdl_pred_d <- function(ses) {
  trace_func_entry("ui__tab__mdl__predictions__hndl_sc_mdl_pred_d")
  updateSelectInput(
    inputId = "mdl_pred_s",
    choices = rownames(get_dataset(ses$input$mdl_pred_d, ses))
  )
}
