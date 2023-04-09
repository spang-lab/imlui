browser__set_cookie = function(ses, key, value, days = 7) {
  params <- list(key = key, value = value, expires = days)
  ses$session$sendCustomMessage("Cookies.set", params)
}

browser__get_cookies <- function(ses) {
  ses$session$sendCustomMessage("Cookies.get", list())
}

browser__rm_cookie <- function(ses, key) {
  ses$session$sendCustomMessage("Cookies.rm", list(key = key))
}

browser__eval <- function(ses, code) {
  ses$session$sendCustomMessage("eval", list(code = code))
}

browser__hide <- function(ses, id) {
  code <- paste0('$("#', id, '").hide()')
  ses$session$sendCustomMessage("eval", list(code = code))
}

browser__toggle <- function(ses, id) {
  code <- paste0('$("#', id, '").toggle()')
  ses$session$sendCustomMessage("eval", list(code = code))
}

browser__show <- function(ses, id) {
  code <- paste0('$("#', id, '").show()')
  ses$session$sendCustomMessage("eval", list(code = code))
}
