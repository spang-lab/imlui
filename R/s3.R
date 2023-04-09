all.reactivevalues <- function(x, na.rm = FALSE) {
  all(unlist(reactiveValuesToList(x)), na.rm = TRUE)
}
