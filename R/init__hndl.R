init__hndl__bookmark <- function(ses) {
  trace_func_entry()
  on_input_change = observeEvent(
    eventExpr = reactiveValuesToList(ses$input),
    handlerExpr = {
      # debugmsg("Input changed, triggering doBookmark event")
      ses$session$doBookmark() # 1)
      # 1) Triggers "bookmark" event which in turn triggers `onBookmark` and
      # `onBookmarked` callback functions. For details see:
      # https://mastering-shiny.org/action-bookmark.html#action-bookmark
      # https://rdrr.io/cran/shiny/man/onBookmark.html
    }
  )
  on_bookmarked <- onBookmarked(
    fun = function(url) {
      # Called just after Shiny bookmarked state. Argument `url` is generated
      # from all inputs as well the members of `state$values`, which are created
      # by the `onBookmark` handler function.
      # debugmsg("onBookmarked handler triggered with url:", url)
      updateQueryString(queryString = url, mode = "replace")
      ses$volatile$url <- url
    }
  )
  # Currently unused bookmark related events
  if (FALSE) {
    on_bookmark <- onBookmark(
      # Called just before Shiny bookmarks state, i.e. `state$values` will be
      # empty and can be filled with values whose names are not existing inputs
      # (as those might be overwritten, although I'm not sure about that).
      # Everything that is stored inside the `state$values` environment will be
      # used by shiny to generate the URL object, that is passed to the
      # `onBookmarked` handler function later on.
      fun = function(state) {
        # For onBookmark, the state object has three relevant fields.
        # 1. values: environment to save arbitrary values
        # 2. input: the application's input object
        # 3. dir: path (requires `getShinyOption("bookmarkStore") == "server"`)
        # Source: https://rdrr.io/cran/shiny/man/onBookmark.html
        debugmsg("onBookmark handler triggered")
      }
    )
    on_restore <- onRestore(
      # Called when a session is restored, i.e. after the server function
      # executes, but before all other reactives, observers and render functions
      # are run.
      fun = function(state) {
        # For onRestore and onRestored, the state object is a list containing the
        # following fields:
        # 1. values: env containing arbitrary values saved by onBookmark handler
        # 2. input: named list of input values to restore
        # 3. dir: path (requires `getShinyOption("bookmarkStore") == "server"`)
        debugmsg("onRestore handler triggered")
      }
    )
    on_restored <- onRestored(
      # Called after a session is restored. This is similar to onRestore, but it
      # will be called after all reactives, observers, and render functions run,
      # and after results are sent to the client browser. onRestored callbacks
      # can be useful for sending update messages to the client browser.
      fun = function(state) {
        # For onRestore and onRestored, the state object is a list containing the
        # following fields:
        # 1. values: env containing arbitrary values saved by onBookmark handler
        # 2. input: named list of input values to restore
        # 3. dir: path (requires `getShinyOption("bookmarkStore") == "server"`)
        debugmsg("onRestored handler triggered")
      }
    )
  }
  return(toscutil::function_locals())
}


init__hndl__btn <- function(ses) {
  trace_func_entry()

  login <- observeEvent(
    eventExpr = {
      # trace_func_entry("init__hndl__btn: login <- observeEvent")
      ses$input$login_button
    },
    handlerExpr = {
      hndl__btn__login(ses)
    }
  )

  login_github <- observeEvent(
    eventExpr = {
      # trace_func_entry("init__hndl__btn: login_github <- observeEvent")
      ses$input$login_button_github
    },
    handlerExpr = {
      hndl__btn__login_github(ses)
    },
  )

  logout <- observeEvent(
    eventExpr = {
      # trace_func_entry("init__hndl__btn: logout <- observeEvent")
      if (ses$input$active_tab %||% "" == "tab__user_logout") 1 else NULL
    },
    handlerExpr = {
      hndl__btn__logout(ses)
    }
  )

  return(toscutil::function_locals())
}


init__hndl__exit <- function(ses) {
  # do nothing
}


init__hndl__sc <- function(ses) {
  trace_func_entry()

  client_data <- observeEvent(
    eventExpr = ses$session$clientData,
    handlerExpr = hndl__sc__client_data(ses)
  )

  active_tab__obsrvr <- observeEvent(
    eventExpr = ses$input$active_tab,
    handlerExpr = {
      debugmsg("Active tab changed to:", ses$session$input$active_tab)
      # debugmsg("ses$input$navbar_left:", ses$input$navbar_left)
      # debugmsg("ses$input$navbar_right:", ses$input$navbar_right)
      # debugmsg("ses$input$active_tab:", ses$input$active_tab)
    }
  )
  return(toscutil::function_locals())
}


init__hndl__su <- function(ses) {
  trace_func_entry("init__hndl__su")

  login_jscookie <- observeEvent(
    eventExpr = ses$input$cookies,
    handlerExpr = {
      trace_func_entry("init__hndl__su > login_jscookie > handlerExpr")
      sessio <- ses$input$cookies$user_session
      debugmsg("ses$input$cookies$user_session:", sessio)
      hndl__sc__login_cookie(ses)
    },
    once = TRUE
  )

  oauth <- observeEvent(
    eventExpr = 1,
    once = TRUE,
    handlerExpr = {
      trace_func_entry("init__hndl__su > oauth > handlerExpr")
      hndl__oauth(ses)
    }
  )

  all_login_checked <- observeEvent(
    eventExpr = {
      if (!all(ses$rv$login_checked)) NULL else ses$input$login_button %||% 0
    },
    handlerExpr = {
      trace_func_entry("init__hndl__su > all_login_checked > handlerExpr")
      hndl__sc__auth_completed(ses)
    }
  )

  return(toscutil::function_locals())
}
