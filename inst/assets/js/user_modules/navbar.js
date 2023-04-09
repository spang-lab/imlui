export function activate(tabID) {
  console.log(`navbar.activate(tabID = ${tabID})`)
  var navbar_a = $(`nav a[data-value="${tabID}"]`)[0]
  var parent = navbar_a.parentElement
  // console.log("tabID:", tabID)
  // console.log("navbar_a:", navbar_a)
  while (parent.id != 'navbar_main_container') {
    // console.log("parent", parent)
    // console.log("parent.id", parent.id)
    if (parent.tagName == 'LI') {
      parent.className += ' active'
      // console.log("Activating:", parent)
    }
    parent = parent.parentElement
  }
}

export function deactivate(navbarID) {
  console.log(`navbar.deactivate(navbarID=${navbarID})`)
  $(`#${navbarID} li.active`).each(function (i) {
    // console.log('Disabled', this)
    this.classList.remove('active')
  })
}

export function deactivate_all() {
  console.log(`navbar.deactivate_all()`)
  deactivate('navbar_left')
  deactivate('navbar_right')
}
