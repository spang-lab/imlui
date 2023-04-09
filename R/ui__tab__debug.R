ui__tab__debug_cookies <- function(ses) {
  ui__tab__debug_cookies__obsrv_btn_dbg_cookies_set(ses)
  ui__tab__debug_cookies__obsrv_btn_dbg_cookies_get(ses)
  ui__tab__debug_cookies__obsrv_btn_dbg_cookies_rm(ses)
  ui__tab__debug_cookies__sync_tout_dbg_cookies(ses)
  ui <- ui__tab__debug_cookies__ui(ses)
  return(ui)
}


ui__tab__debug_jquery <- function(ses) {
  ui <- div(
    class = "container-fluid",
    h1("Debug jQuery"),
    div(
      id = "rect_dbg_jq",
      style = "width: 100px; height: 100px; background: blue;"
    ),
    p("Use below Buttons to hide/toggle/show the blue rectangle."),
    shiny::actionButton(
      inputId = "btn_dbg_jquery_hide",
      label = "jQuery.hide()",
      onclick = "$('#rect_dbg_jq').hide()"
    ),
    shiny::actionButton(
      inputId = "btn_dbg_jquery_toggle",
      label = "jQuery.toggle()",
      onclick = "$('#rect_dbg_jq').toggle()"
    ),
    shiny::actionButton(
      inputId = "btn_dbg_jquery_show",
      label = "jQuery.show()",
      onclick = "$('#rect_dbg_jq').show()"
    )
  )
  return(ui)
}



ui__tab__debug_eval_js <- function(ses) {
  shiny::observeEvent(
    eventExpr = ses$input$btn_dbg_eval,
    handlerExpr = browser__eval(ses, ses$input$tin_dbg_eval_code)
  )
  ui <- div(
    class = "container-fluid",
    h1("Debug"),
    p("Use below Button to run the code from the input field"),
    shiny::textAreaInput(
      inputId = "tin_dbg_eval_code",
      label = "Code"
    ),
    p(b("Some examples")),
    code('console.log("Helloworld")'),
    code(
      paste(
        'var parent = document.getElementById("div_dbg_bottom")',
        'var child = document.createElement("p")',
        'var text = document.createTextNode("Some text")',
        "child.appendChild(text)",
        "parent.appendChild(child)",
        sep = "\n"
      )
    ),
    br(),
    shiny::actionButton(
      inputId = "btn_dbg_eval",
      label = "eval(<Code>)"
    ),
    div(id = "div_dbg_bottom")
  )
  return(ui)
}


ui__tab__debug_eval_r <- function(ses) {
  shiny::observeEvent(
    eventExpr = ses$input$btn_dbg_eval_r,
    handlerExpr = {
      sys <- Sys.info()
      if (sys[["nodename"]] == "TUX15" && sys[["effective_user"]] == "tobi") {
        debugmsg("Evaluating:", utils__dput(ses$input$tin_dbg_eval_r_code))
        x <- eval(parse(text = ses$input$tin_dbg_eval_r_code))
        debugmsg(utils__dput(x))
      } else {
        warnmsg("Evaluation of R code is disabled on this machine for security reasons")
      }
    }
  )
  ui <- div(
    class = "container-fluid",
    h1("Debug"),
    p("Use below Button to run the code from the input field"),
    shiny::textAreaInput(
      inputId = "tin_dbg_eval_r_code",
      label = "Code"
    ),
    p(b("Some examples")),
    code("print_env(penv, 1)\nprint_env(ses, 1)"),
    br(),
    shiny::actionButton(
      inputId = "btn_dbg_eval_r",
      label = "eval(<Code>)"
    )
  )
  return(ui)
}


ui__tab__debug_cookies__obsrv_btn_dbg_cookies_set <- function(ses) {
  observeEvent(
    eventExpr = ses$input$btn_dbg_cookies_set,
    handlerExpr = {
      key <- ses$input$tin_dbg_cookie_key
      value <- ses$input$tin_dbg_cookie_value
      expires <- ses$input$tin_dbg_cookie_expires
      debugmsg("Calling: Cookies.set(", key, value, expires, ")")
      browser__set_cookie(ses, key, value, expires)
    }
  )
}

ui__tab__debug_cookies__obsrv_btn_dbg_cookies_get <- function(ses) {
  observeEvent(
    eventExpr = ses$input$btn_dbg_cookies_get,
    handlerExpr = {
      debugmsg("Calling Cookies.get()")
      browser__get_cookies(ses)
    }
  )
}

ui__tab__debug_cookies__obsrv_btn_dbg_cookies_rm <- function(ses) {
  observeEvent(
    eventExpr = ses$input$btn_dbg_cookies_rm,
    handlerExpr = {
      key <- ses$input$tin_dbg_cookie_key
      debugmsg("Calling: Cookies.rm(", key, ")")
      browser__rm_cookie(ses, key)
    }
  )
}

ui__tab__debug_cookies__sync_tout_dbg_cookies <- function(ses) {
  ses$output$tout_dbg_cookies <- shiny::renderText(dput2(ses$input$cookies))
}


ui__tab__debug_cookies__ui <- function(ses) {
  div(
    class = "container-fluid",
    h1("Debug"),
    h2("Cookie Functions"),
    shiny::textInput("tin_dbg_cookie_key", "Key (str)"),
    shiny::textInput("tin_dbg_cookie_value", "Value (str)"),
    shiny::textInput("tin_dbg_cookie_expires", "Days until Expiry (int)"),
    br(),
    shiny::actionButton(
      inputId = "btn_dbg_cookies_set",
      label = "Call Cookies.set(<Key>, <Value>, <Expires>)",
    ),
    br(),
    shiny::actionButton(
      inputId = "btn_dbg_cookies_get",
      label = "Call Cookies.get()"
    ),
    br(),
    shiny::actionButton(
      inputId = "btn_dbg_cookies_rm",
      label = "Call Cookies.rm(<Key>)"
    ),
    br(),
    b("Cookies"),
    shiny::textOutput("tout_dbg_cookies")
  )
}
