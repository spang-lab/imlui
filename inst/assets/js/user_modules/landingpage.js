import * as Tab from "./tab.js"
import * as Navbar from "./navbar.js"
import * as Breadcrumb from "./breadcrumb.js"
import * as Handlers from "./handlers.js"

export * as Tab from "./tab.js"

// This function should be called directly after the landing page UI has been
// loaded (see function `ui_landing_page` in `610_ui_landing_page.R`)
export function registerEventHandlers() {
  console.log("landingpage.registerEventHandlers()")

  // Clicks on navbar items or the navbar brand should activate the
  // corresponding tab, i.e. in addition to the usual bootstrap behaviour (show
  // tab from current tabset, set activate attribute to current item), all tabs
  // from other tabsets/navbars should be hidden/deactivated, the breadcrumb
  // bar should be updated and the shiny input `active_tab` should be updated
  // as well.
  var navbar_anchors = $('nav a.imlui-navbar-anchor, a.navbar-brand')
  navbar_anchors.on('show.bs.tab', function (e) {
    // show.bs.tab is triggered before the bootstrap specific
    // hiding/activiation takes place, so we can hide all navbar items and
    // bootstrap will make the correct one active again
    var tabID = this.getAttribute('data-value')
    Navbar.deactivate_all()
    Breadcrumb.update(tabID)
    Shiny.setInputValue('active_tab', tabID)
  })

  // Clicks on menu headers should activate the corresponding tab if the menu
  // is already expanded
  var navbar_menu_anchors = $('nav a.dropdown-toggle')
  navbar_menu_anchors.on('click', function (e) {
    var aria_expanded = this.getAttribute('aria-expanded') // either null, true or false
    var data_value = this.getAttribute('data-value') // either null, true or false
    if (aria_expanded === 'true') {
      Tab.activate(data_value)
    }
  })

  // Register key release handlers for login panel (enter should activate button press)
  $("#tab__content_login").on('keyup', Handlers.login_panel_key_up)
}
