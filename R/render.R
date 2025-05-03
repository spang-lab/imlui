# name = (str) plot name (shown in browser as alternative name of image)
# func = (closure) plot function
# args = (closure) function to return arglist required for `func` (usually
# a reactive)
render__svg <- function(name, func, args) {
  trace_func_entry("render__svg")
  fname <- deparse(substitute(func))
  shiny::renderImage(
    expr = render__store_as_svg(
      pname = name,
      func = func,
      arglist = args(),
      fname = fname
    ),
    deleteFile = FALSE
  )
}


render__store_as_svg <- function(pname,
                                 func,
                                 arglist,
                                 fname = deparse(substitute(func)),
                                 width = 1104,
                                 height = 700) {
  trace_func_entry("render__store_as_svg")
  outfile <- paste0(file.path(get_imlui_cache_dir(), pname), ".svg")
  argListChar <- deparse(substitute(arglist))
  infomsg("Storing", outfile)
  svglite(outfile, width = width / 92, height = height / 92)
  tryCatch(
    do.call(func, args = arglist),
    error = function(cond) {
      graphics::plot.new()
      graphics::plot.window(xlim=c(0, 1), ylim=c(0, 1))
      graphics::text(0, 1, cond, col = "red", adj = c(0, 1))
    },
    finally = grDevices::dev.off()
  )
  list(src = outfile, width = width, height = height, alt = pname)
}


# Render Data Table
render__tbl <- function(expr, show = "_all", ignore = character()) {
  colvis <- if (show[[1]] == "_all") {
    seq_along(expr) - 1
  } else {
    which(colnames(expr) %in% show) - 1
  }
  colvis <- which(colnames(expr)[colvis + 1] %notin% ignore) - 1
  # JS code for key-press handling. Taken from:
  # https://laustep.github.io/stlahblog/posts/DTcallbacks.html
  js <- c(
    "table.on('key', function(e, datatable, key, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  if(key == 13 && targetName == 'body'){",
    "    $(cell.node()).trigger('dblclick.dt');",
    "  }",
    "});",
    "table.on('keydown', function(e){",
    "  var keys = [9,13,37,38,39,40];",
    "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
    "    $(e.target).trigger('blur');",
    "  }",
    "});",
    "table.on('key-focus', function(e, datatable, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  var type = originalEvent.type;",
    "  if(type == 'keydown' && targetName == 'input'){",
    "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
    "      $(cell.node()).trigger('dblclick.dt');",
    "    }",
    "  }",
    "});"
  )
  DT::renderDT(
    # documentation: https://rstudio.github.io/DT/
    # args: https://cran.r-project.org/web/packages/DT/DT.pdf
    # options: https://datatables.net/reference/option/
    expr = expr,
    server = TRUE,
    editable = list(target = "cell", disable = list(columns = c(0))),
    callback = htmlwidgets::JS(js),
    extensions = "KeyTable",
    selection = "none",
    rownames = FALSE,
    options = list(
      keys = TRUE,
      scrollX = FALSE,
      scrollY = "auto", # "max(100vh - 280px, 400px)"
      scrollCollapse = TRUE,
      paging = FALSE,
      columnDefs = list(
        list(className = "dt-left", targets = "_all"),
        list(visible = TRUE, targets = colvis),
        list(visible = FALSE, targets = "_all")
        # list(width = paste0(floor(80/ncol(expr)), "%"), targets = "_all")
      )
    )
  )
}
