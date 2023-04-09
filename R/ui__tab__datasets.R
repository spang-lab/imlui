ui__tab__datasets__descriptions <- function(ses) {
  trace_func_entry("ui__tab__datasets__descriptions")
  ses$output$datasets_desc_text <- renderText({
    req(ses$input$datasets_desc_d)
    did <- ses$input$datasets_desc_d
    sym <- unname(ses$rv$dataset$symbols[did])
    pkg <- unname(ses$rv$dataset$pkgs[did])
    txt <- util__get_help_text(sym, pkg)
  })
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(
            html__inp_datasets(ses, "datasets_desc_d", n = 1, multiple = FALSE)
          )
        )
      ),
      html__col_main(
        html__row(
          shiny::column(12,
            class = "imlui-plot-area",
            style = paste("padding: 15px;"),
            code(textOutput("datasets_desc_text"))
          )
        )
      )
    )
  )
}


ui__tab__datasets__predictions <- function(ses) {
  trace_func_entry()
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(html__inp_models(ses, "datasets_pred_m", n = 1)),
          html__col_sb_item(html__inp_datasets(ses, "datasets_pred_d", n = 100))
        )
      ),
      html__col_main(
        html__row(
          column(12,
            # style = paste("display: flex; justify-content: center;"),
            tags$img(
              class = "dummy-img",
              src = "imlui/assets/jpg/pred2.jpg",
              alt = "pred2.jpg"
            )
          ),
          shiny::column(12,
            style = "border-top: 1px solid; border-color: #cccccc;",
            "Heatmap from predictions of specified Model across all specified Datasets."
          )
        )
      )
    )
  )
}


ui__tab__datasets__survival_curves <- function(ses) {
  trace_func_entry()
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(html__inp_models(ses, "datasets_surv_m", n = 1)),
          html__col_sb_item(html__inp_datasets(ses, "datasets_surv_d", n = 100))
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
            "Survival curves from predictions of specified Model across all specified Datasets."
          )
          # style = paste("display: flex; justify-content: center;"),
        )
      )
    )
  )
}


ui__tab__datasets__msd <- function(ses) {
  trace_func_entry("ui__tab__datasets__msd")
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(
            html__inp_models(ses, "datasets_msd_m", n = 1, multiple = FALSE)
          ),
          html__col_sb_item(
            html__inp_datasets(
              ses = ses,
              id = "datasets_msd_d",
              n = 100,
              model_inp_id = "datasets_msd_m"
            )
          )
        ),
        html__row(
          html__col_sb_item(
            selectInput(
              inputId = "datasets_msd_xtrafo",
              label = "x-Transformation",
              choices = c("identity", "log2", "log10")
            )
          ),
          html__col_sb_item(
            selectInput(
              inputId = "datasets_msd_ytrafo",
              label = "y-Transformation",
              choices = c("identity", "log2", "log10")
            )
          )
        ),
        html__row(
          html__col_sb_item(
            sliderInput(
              inputId = "datasets_msd_width",
              label = "Width in Pixel",
              value = 1200,
              min = 400,
              max = 3840,
              step = 60
            )
          ),
          html__col_sb_item(
            sliderInput(
              inputId = "datasets_msd_height",
              label = "Height in Pixel",
              value = 800,
              min = 400,
              max = 3840,
              step = 60
            )
          )
        )
      ),
      html__col_main(
        html__row(
          shiny::column(12,
            class = "imlui-plot-area",
            ui__tab__datasets__msd__plot_output(ses)
          ),
          shiny::column(12,
            style = "border-top: 1px solid; border-color: #cccccc;",
            "Mean (M) vs Standard Deviation (SD) Plot for each Feature from the
            specified Model across all specified Datasets."
          ),
        )
      )
    )
  )
}


ui__tab__datasets__msd__plot_output <- function(ses) {
  trace_func_entry("ui__tab__datasets__msd__plot_output")
  ses$output$tab__datasets__msd__plot_output <- shiny::renderImage(
    expr = {
      trace_func_entry("ui__tab__datasets__msd__plot_output > renderImage")
      inp <- ses$input
      req(
        inp$datasets_msd_m,
        inp$datasets_msd_d,
        inp$datasets_msd_ytrafo,
        inp$datasets_msd_ytrafo,
        inp$datasets_msd_width,
        inp$datasets_msd_height,
      )
      m <- get_model(inp$datasets_msd_m, ses)
      ff <- get_model_feature_names(m)
      ff <- ff[!(ff == "Intercept")]
      xx <- lapply(inp$datasets_msd_d, function(d) get_dataset(d, ses))
      req(all(sapply(xx, function(x) all(ff %in% colnames(x)))))
      xx <- lapply(xx, function(x) x[, ff])
      names(xx) <- inp$datasets_msd_d
      arglist <- list(
        dd = inp$datasets_msd_d,
        xx = xx,
        xt = inp$datasets_msd_xtrafo,
        yt = inp$datasets_msd_ytrafo,
        ff = ff
      )
      render__store_as_svg(
        pname = "MSD",
        func = plot__msd,
        arglist = arglist,
        width = inp$datasets_msd_width,
        height = inp$datasets_msd_height
      )
    },
    deleteFile = FALSE
  )
  div(
    class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
    style = paste("padding: 15px;"),
    plotOutput(outputId = "tab__datasets__msd__plot_output")
  )
}


ui__tab__datasets__pca <- function(ses) {
  trace_func_entry("ui__tab__datasets__pca")
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(
            html__inp_models(
              ses = ses,
              id = "datasets_pca_m",
              n = 1,
              multiple = FALSE
            )
          ),
          html__col_sb_item(
            html__inp_datasets(
              ses = ses,
              id = "datasets_pca_d",
              n = 100,
              model_inp_id = "datasets_pca_m"
            )
          )
        )
      ),
      html__col_main(
        html__row(
          shiny::column(12,
            class = "imlui-plot-area",
            ui__tab__datasets__pca__plot_output(ses)
          ),
          shiny::column(12,
            style = "border-top: 1px solid; border-color: #cccccc;",
            "Principal component analysis (PCA) is a popular technique for analyzing large datasets containing a high number of dimensions/features per observation, increasing the interpretability of data while preserving the maximum amount of information, and enabling the visualization of multidimensional data. Formally, PCA is a statistical technique for reducing the dimensionality of a dataset. This is accomplished by linearly transforming the data into a new coordinate system where (most of) the variation in the data can be described with fewer dimensions than the initial data. Many studies use the first two principal components in order to plot the data in two dimensions and to visually identify clusters of closely related data points. Principal component analysis has applications in many fields such as population genetics, microbiome studies, and atmospheric science.",
            "Filters all datasets by the specified model features. Then does a PCA on the training dataset of the specified model and applies the transformation matrix to all specified datasets."
          ),
        )
      )
    )
  )
}


ui__tab__datasets__pca__plot_output <- function(ses) {
  trace_func_entry("ui__tab__datasets__pca__plot_output")
  ses$output$tab__datasets__pca__plot_output <- shiny::renderImage(
    expr = {
      trace_func_entry("ui__tab__datasets__pca__plot_output > renderImage")
      inp <- ses$input
      req(
        inp$datasets_pca_m,
        inp$datasets_pca_d,
      )
      m <- get_model(inp$datasets_pca_m, ses)
      ff <- get_model_feature_names(m)
      xx <- lapply(inp$datasets_pca_d, function(d) get_dataset(d, ses))
      req(all(sapply(xx, function(x) all(ff %in% colnames(x)))))
      xx <- lapply(xx, function(x) x[, ff])
      names(xx) <- inp$datasets_pca_d
      arglist <- list(
        dd = inp$datasets_pca_d,
        xx = xx,
        ff = ff
      )
      render__store_as_svg(pname = "pca", func = plot__pca, arglist = arglist)
    },
    deleteFile = FALSE
  )
  div(
    class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
    style = paste("padding: 15px;"),
    plotOutput(outputId = "tab__datasets__pca__plot_output", height = "auto")
  )
}


ui__tab__datasets__umap <- function(ses) {
  trace_func_entry("ui__tab__datasets__umap")
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(
            html__inp_models(
              ses = ses,
              id = "datasets_umap_m",
              n = 1,
              multiple = FALSE
            )
          ),
          html__col_sb_item(
            html__inp_datasets(
              ses = ses,
              id = "datasets_umap_d",
              n = 100,
              multiple = TRUE,
              model_inp_id = "datasets_umap_m"
            )
          )
        )
      ),
      html__col_main(
        html__row(
          shiny::column(12,
            class = "imlui-plot-area",
            ui__tab__datasets__umap__plot_output(ses)
          ),
          shiny::column(12,
            style = "border-top: 1px solid; border-color: #cccccc;",
            "Uniform manifold approximation and projection (UMAP) is a nonlinear dimensionality reduction technique. Visually, it is similar to t-SNE, but it assumes that the data is uniformly distributed on a locally connected Riemannian manifold and that the Riemannian metric is locally constant or approximately locally constant."
          ),
        )
      )
    )
  )
}


ui__tab__datasets__umap__plot_output <- function(ses) {
  trace_func_entry("ui__tab__datasets__umap__plot_output")
  ses$output$tab__datasets__umap__plot_output <- shiny::renderImage(
    expr = {
      trace_func_entry("ui__tab__datasets__umap__plot_output > renderImage")
      inp <- ses$input
      req(
        inp$datasets_umap_m,
        inp$datasets_umap_d,
      )
      m <- get_model(inp$datasets_umap_m, ses)
      ff <- get_model_feature_names(m)
      xx <- lapply(inp$datasets_umap_d, function(d) get_dataset(d, ses))
      req(all(sapply(xx, function(x) all(ff %in% colnames(x)))))
      xx <- lapply(xx, function(x) x[, ff])
      names(xx) <- inp$datasets_umap_d
      arglist <- list(
        dd = inp$datasets_umap_d,
        xx = xx,
        ff = ff
      )
      render__store_as_svg(pname = "umap", func = plot__umap, arglist = arglist)
    },
    deleteFile = FALSE
  )
  div(
    class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
    style = paste("padding: 15px;"),
    plotOutput(outputId = "tab__datasets__umap__plot_output", height = "auto")
  )
}


ui__tab__datasets__tsne <- function(ses) {
  trace_func_entry("ui__tab__datasets__tsne")
  div(class = "container-fluid imlui-full-height",
    html__row_equal(
      html__col_sb(
        html__row(
          html__col_sb_item(
            html__inp_models(
              ses = ses,
              id = "datasets_tsne_m",
              n = 1
            )
          ),
          html__col_sb_item(
            html__inp_datasets(
              ses = ses,
              id = "datasets_tsne_d",
              n = 100,
              multiple = TRUE,
              model_inp_id = "datasets_tsne_m"
          ))
        )
      ),
      html__col_main(
        html__row(
          shiny::column(12,
            class = "imlui-plot-area",
            ui__tab__datasets__tsne__plot_output(ses)
          ),
          shiny::column(12,
            style = "border-top: 1px solid; border-color: #cccccc;",
            "t-distributed stochastic neighbor embedding (t-SNE) is a statistical method for visualizing high-dimensional data by giving each datapoint a location in a two or three-dimensional map. It is based on Stochastic Neighbor Embedding originally developed by Sam Roweis and Geoffrey Hinton, where Laurens van der Maaten proposed the t-distributed variant. It is a nonlinear dimensionality reduction technique well-suited for embedding high-dimensional data for visualization in a low-dimensional space of two or three dimensions."
          ),
        )
      )
    )
  )
}


ui__tab__datasets__tsne__plot_output <- function(ses) {
  trace_func_entry("ui__tab__datasets__tsne__plot_output")
  ses$output$tab__datasets__tsne__plot_output <- shiny::renderImage(
    expr = {
      trace_func_entry("ui__tab__datasets__tsne__plot_output > renderImage")
      inp <- ses$input
      req(
        inp$datasets_tsne_m,
        inp$datasets_tsne_d,
      )
      m <- get_model(inp$datasets_tsne_m, ses)
      ff <- get_model_feature_names(m)
      xx <- lapply(inp$datasets_tsne_d, function(d) get_dataset(d, ses)[, ff])
      names(xx) <- inp$datasets_tsne_d
      arglist <- list(
        dd = inp$datasets_tsne_d,
        xx = xx,
        ff = ff
      )
      render__store_as_svg(pname = "tsne", func = plot__tsne, arglist = arglist)
    },
    deleteFile = FALSE
  )
  div(
    class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
    style = paste("padding: 15px;"),
    plotOutput(outputId = "tab__datasets__tsne__plot_output", height = "auto")
  )
}