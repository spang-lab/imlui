import * as Cookies from './user_modules/cookies.js'
import * as LandingPage from "./user_modules/landingpage.js"

// Make LandingPage available in the global scope so we can use it from R.
window.LandingPage = LandingPage

$(document).on('shiny:connected', function () {
  Cookies.get()
})

Shiny.addCustomMessageHandler('Cookies.get', function (params) {
  Cookies.get(params.key)
})

Shiny.addCustomMessageHandler('Cookies.set', function (params) {
  if (params.key && params.value) {
    if (params.expires) {
      Cookies.set(params.key, params.value, parseInt(params.expires))
    } else {
      Cookies.set(params.key, params.value)
    }
  }
})

Shiny.addCustomMessageHandler('Cookies.rm', function (params) {
  Cookies.rm(params.key)
})


Shiny.addCustomMessageHandler("eval", function (params) {
  eval(params.code)
})
