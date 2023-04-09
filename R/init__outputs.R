init_outputs <- function(ses) {
  ses$output$landing_page <- renderUI({
    req(ses$rv$auth$completed)
    ses$rv$user$id
    ses$rv$user$group_ids
    ui__landing_page(ses)
  })
}
