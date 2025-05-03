ui__tab__user_profile <- function(ses) {
  ses$output$tbl_user_profile <- render__tbl({
    cols <- c(
      "id", "display_name", "gitlab_id", "avatar_url", "github_id",
      "group_ids", "google_id", "spanglab_auth_id", "spanglab_gitlab_id"
    )
    x <- unlist(reactiveValuesToList(ses$rv$user)[cols])
    df <- data.frame(Property = names(x), Value = x)
  })
  div(
    class = "container-fluid imlui-full-height",
    fluidRow(
      column(2,
        tags$img(
          src = ses$rv$user$avatar_url,
          alt = "avatar.url",
          width = "100%"
        )
      ),
      column(10,
        DT::DTOutput("tbl_user_profile")
      )
    )
  )
}


ui__tab__user_settings <- function(ses) {
  ses$output$tbl_settings <- render__tbl(db__get_table("settings"))
  div(
    class = "container-fluid imlui-full-height",
    fluidRow(
      column(12,
        DT::DTOutput("tbl_settings")
      )
    )
  )
}


ui__tab__user_logout <- function(ses) {}
