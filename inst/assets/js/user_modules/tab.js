import * as tabset from "./tabset.js"
import * as navbar from "./navbar.js"
import * as breadcrumb from "./breadcrumb.js"


function activate_deprecated(tabID) {
  // This version of `activate` is deprecated, because it does not trigger
  // the rendering of the tab content by shiny (probably because shiny listens
  // to the actual bootstrap events, which are not triggered by this function).
  // To solve the problem, we now simulate a click on the corresponding navbar
  // anchor instead (see function `activate` below).
  console.log(`tab.activate(tabID = ${tabID})`)
  navbar.deactivate_all()
  tabset.deactivate()
  navbar.activate(tabID)
  tabset.activate(tabID)
  breadcrumb.update(tabID)
  Shiny.setInputValue('active_tab', tabID)
}


export function activate(tabID) {
  console.log(`tab.activate(tabID = ${tabID})`)
  var navbar_anchor = $(`nav a.imlui-navbar-anchor[href="#${tabID}"]`)[0]
  navbar_anchor.click()
  // TODO: also update browser history state, so we can use browser back and
  // forward buttons for navigation
}
