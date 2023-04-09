export function activate(tabID = null) {
  console.log(`tabset.activate(tabID = ${tabID})`)
  $(`#tabset #${tabID}`)[0].classList.add('active')
}

export function deactivate() {
  console.log(`tabset.deactivate()`)
  var active_tabs = $(`#tabset div[class = "tab-pane active"][data-value^=tab__]`)
  active_tabs.each(function (i) {
    this.classList.remove('active')
  })
}
