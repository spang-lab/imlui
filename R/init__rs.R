# @title Return list of reactives related to Dataset Metadata
# @param input Input object as handed over to server functions by shiny
# @param rv List of reactive values as returned by [init__rv()]
# @return list with following reactive elements:
# tbl, ids, symbols, displaynames, pkgs, transpose
#    reactives returning vectors of all ids/symbols/displaynames/...
#    accessible by the current user
# df, df_cat, df_num, features, samples
#    lists of reactives (one reactive per dataset)
# symbols_list, df_list, df_cat_list, df_num_list, samples_list, features_list
#    reactives returning vectors of ids/symbols/displaynames/...
#    selected by the current user
init_rs_dataset <- function(input, rv) {
  trace_func_entry()

  # Reactives depending only on `rv` that return vectors.
  ids <- reactive(init__rv__accessible_dataset_ids(rv))
  symbols <- reactive(rv$tbl$datasets[ids(), "symbol"])
  displaynames <- reactive(`names<-`(rv$tbl$datasets[ids(), "name"], symbols()))
  pkgs <- reactive(`names<-`(rv$tbl$datasets[ids(), "package"], symbols()))
  transpose <- reactive(
    `names<-`(as.logical(rv$tbl$datasets[ids(), "transpose"]), symbols())
  )

  # Lists of reactives (one element per dataset) depending only on `rv`. Access
  # via `<reactive>()[<dataset_symbol>]()`, e.g. `df()[["lamis_test1"]]()`.
  df <- reactive(
    clapply(symbols(), function(s) {
      reactive({
        x <- get_symbol( # dataframe of observed values
          sym = s,
          typ = "dataset",
          pkg = pkgs()[s],
          transpose = transpose()[s]
        )
        # x <- do.call(dplyr::rename, list(x, FEATURE_MAPPINGS[[s]]))
        # TODO: add renaming to get_data
      })
    })
  )
  df_cat <- reactive(
    clapply(rv$tbl$datasets$symbol, function(s) {
      1
      # reactive(select(df()[[s]](), where(is.factor))) # categorical values
    })
  )
  df_num <- reactive(
    clapply(rv$tbl$datasets$symbol, function(s) {
      1
      # reactive(select(df()[[s]](), !where(is.factor))) # numerical values
    })
  )
  features <- reactive(
    clapply(symbols(), function(s) {
      reactive(colnames(df()[[s]]()))
    })
  )
  samples <- reactive(
    clapply(symbols(), function(s) {
      reactive(rownames(df()[[s]]()))
    })
  )

  # Reactives depending on `input` and `rv` that return lists.
  symbols_list <- reactive(
    if (!is.null(input$dataset_names)) {
      symbols()[match(input$dataset_names, displaynames())]
    } else {
      character()
    }
  )
  df_list <- reactive(
    clapply(symbols_list(), function(s) df()[[s]]())
  )
  df_cat_list <- reactive(
    1
    # clapply(symbols_list(), function(X) select(X, where(is.factor)))
  )
  df_num_list <- reactive(
    1
    # clapply(symbols_list(), function(X) select(X, !where(is.factor)))
  )
  samples_list <- reactive(
    clapply(symbols_list(), function(s) samples()[[s]]())
  )
  features_list <- reactive(
    clapply(symbols_list(), function(s) features()[[s]]())
  )

  return(toscutil::function_locals())
}


# TODO: remove reactives
# @title Return list of reactives related to Model Metadata
# @param input Input object as handed over to server functions by shiny
# @param rv List of reactive values as returned by [init__rv()]
# @return list with following elements:
# * `ids`: reactive, all elements of `tbl$ID` that may be accessed by the
#   current user
# * `symbols`: reactive, `R` objects corresponding to `ids()`
# * `displaynames`: reactive, display names corresponding to `ids()`
# * `pkgs`: reactive, R packages providing `symbols`
# * `features`: list of reactives, feature names for each symbol in `symbols()`
# * `betas`: list of reactives, model betas for each symbol in `symbols()`
# * `symbols_list`: reactive, list of symbols currently selected by the user
# * `features_list`: reactive, list of feature name vectors corresponding to
#   `symbols_list()`
# * `betas_list`: reactive, list of  model param vectors corresponding to
#   `symbols_list()`
init_rs_model <- function(input, rv) {
  trace_func_entry()

  # Reactives depending only on `rv` that return vectors.
  ids <- reactive(init__rv__get_accessible_model_ids(rv))
  symbols <- reactive(rv$tbl$models[ids(), "symbol"])
  displaynames <- reactive(`names<-`(rv$tbl$models[ids(), "name"], symbols()))
  pkgs <- reactive(`names<-`(rv$tbl$models[ids(), "package"], symbols()))

  # Reactives depending only on `rv` that return lists of reactives. (Access via
  # betas()[["<symbol>"]]()
  betas <- reactive(
    clapply(symbols(), function(s) {
      reactive(get_symbol(sym = s, typ = "model", pkg = pkgs()[s]))
    })
  )
  features <- reactive(
    clapply(symbols(), function(s) {
      reactive(names(betas()[[s]]()))
    })
  )

  # Reactives depending on `rv` and `input` that return lists.
  symbols_list <- reactive(
    if (!is.null(input$model_names)) {
      symbols()[match(input$model_names, displaynames())]
    } else {
      character()
    }
  )
  betas_list <- reactive(clapply(symbols_list(), function(m) betas()[[m]]()))
  features_list <- reactive(clapply(symbols_list(), function(m) features()[[m]]()))

  return(toscutil::function_locals())
}


init_rs_prediction <- function(rv, model_reactives, dataset_reactives) {
  trace_func_entry()

  # List of reactives. One reactive per prediction of a model for a dataset.
  # Access via prediction$model$dataset. Structure: list(list(reactive(num))).
  model_symbols <- function() {
    trace_func_entry()
    clapply(model_reactives$symbols, function(m) {
      clapply(dataset_reactives$symbols, function(d) {
        reactive({
          # purrr::possibly(.f = predict, otherwise = NULL)( # 1
          #   model_reactives$betas()[[m]](), df[[d]]()
          # )
        })
      })
    })
  }
  # 1 Functions wrapped in `purrr::possible` return the value of
  # 1 argument `otherwise` if an error occurs during call of `.f`.

  # One reactive returning a list of all predictions. Access via:
  # YYY()[model][prediction]. Structure: reactive(list(list(num))).
  YYY <- reactive({
    # clapply(model_reactives$symbol_list(), function(M) {
    #   clapply(DD(), function(D) {
    #     YYYR[[M]][[D]]()
    #   })
    # })
  })
}


init_rs_size <- function(input) {
  trace_func_entry()

  PAWS <- 800
  PAHS <- 600

  if (missing(input)) {
    stop("Argument `input` is missing")
  }

  # Manual Plot Area Width / Height in pixels
  MPAW <- reactive(input$MPAW)
  MPAH <- reactive(input$MPAH)
  # Browser Width / Height in pixels
  BW <- reactive(if (is.null(input$dim[1])) 640 else input$dim[1])
  BH <- reactive(if (is.null(input$dim[2])) 480 else input$dim[2])
  # Plot Area Size Calculation (either "Fixed" or "Auto")
  PASC <- reactive(input$PASC)

  # Main Panel Width / Height (approximately) in pixels TODO: Improve precision
  MPW <- reactive(BW() * 0.75)
  MPH <- reactive(BH() - 42.0)

  # Automatic Plot Area Width / Height in pixels
  APAW <- reactive(PAWS[max(which(PAWS < (MPW() - 0)), 1)])
  APAH <- reactive(PAHS[max(which(PAHS < (MPH() - 120)), 1)])

  # Plot Area Widht/Height (approximately) in pixels
  PAH <- reactive({
    if (PASC() == "Fixed" && !(is.null(MPAH()))) MPAH() else APAH()
  })
  PAW <- reactive({
    if (PASC() == "Fixed" && !(is.null(MPAW()))) MPAW() else APAW()
  })

  # Plot Area Widht/Height (approximately) in pixels as string
  PAWPX <- reactive(paste0(PAW(), "px"))
  PAHPX0 <- reactive(paste0(PAH() - 0, "px"))
  PAHPX1 <- reactive(paste0(PAH() - 120, "px"))
  PAHPX1_05 <- reactive(paste0((PAH() - 120) * 0.5, "px"))
  PAHPX2 <- reactive(paste0(PAH() - 240, "px"))
  PAHPX3 <- reactive(paste0(PAH() - 360, "px"))

  return(toscutil::function_locals())
}
