ui__tab__mdl__descriptions <- function(ses) {
  trace_func_entry("ui__tab__mdl__descriptions")
  ses$output$models_desc_text <- renderText({
    req(ses$input$models_desc_d)
    trace_func_entry("ui__tab__mdl__descriptions -> renderText")
    did <- ses$input$models_desc_d
    sym <- unname(ses$rv$model$symbols[did])
    pkg <- unname(ses$rv$model$pkgs[did])
    txt <- util__get_help_text(sym, pkg)
  })
  x <- div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(
            html__inp_models(ses, "models_desc_d", n = 1, multiple = FALSE)
          )
        )
      ),
      html__col_main(
        html__row(
          shiny::column(12,
            class = "imlui-plot-area",
            style = paste("padding: 15px;"),
            code(textOutput("models_desc_text"))
          )
        )
      )
    )
  )
}


ui__tab__mdl__non_redundance_betas <- function(ses) {
  trace_func_entry()
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(html__inp_models(ses, "nrb_m", n = 1))
        )
      ),
      html__col_main(
        html__row(
          style = paste("display: flex; justify-content: center;"),
          tags$img(
            class = "dummy-img",
            src = "imlui/assets/png/nrb.png",
            alt = "nrb.png"
          )
        )
      )
    )
  )
}


ui__tab__mdl__replacement_factor <- function(ses) {
  trace_func_entry()
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(html__inp_models(ses, "rf_m", n = 1))
        )
      ),
      html__col_main(
        html__row(
          column(12,
            tags$img(
              class = "dummy-img",
              src = "imlui/assets/jpg/rf2.jpg",
              alt = "rf2.jpg"
            )
          )
        )
      )
    )
  )
}


ui__tab__mdl__survival_curves <- function(ses) {
  trace_func_entry()
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(html__inp_models(ses, "models_sc_m", n = 1)),
          html__col_sb_item(html__inp_datasets(ses, "models_sc_d", n = 1))
        )
      ),
      html__col_main(
        html__row(
          shiny::column(12,
            tags$img(
              class = "dummy-img",
              src = "imlui/assets/jpg/surv.jpg",
              alt = "surv.jpg"
            )
          ),
          shiny::column(12,
            style = paste(
              # "background-color: #f5f5f5;",
              "border-top: 1px solid;",
              "border-color: #cccccc;"
            ),
            "Survival curves from predictions of all specified Model evaluated on the specified Dataset."
          )
          # style = paste("display: flex; justify-content: center;"),
        )
      )
    )
  )
}
