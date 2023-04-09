import Cookies from '../node_modules/js-cookie/dist/js.cookie.min.mjs.js'

export function get () {
  console.log(`cookies.get()`)
  var cookies = Cookies.get()
  Shiny.setInputValue('cookies', cookies)
}

export function set (key, value, expires) {
  console.log(`cookies.set(key = ${key}, value = ${value}, expires = ${expires})`)
  Cookies.set(key, value, { expires: expires })
  Shiny.setInputValue(key, value)
}

export function rm (key) {
  console.log(`cookies.rm(key = ${key})`)
  Cookies.remove(key)
  Shiny.setInputValue(key, null)
}

