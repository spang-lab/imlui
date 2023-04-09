export function update(tabID = null) {
  console.log(`breadcrumb.update(tabID = ${tabID})`)
  var bc = $(`ol.breadcrumb`)[0]
  var bcHome = $(`ol.breadcrumb li`)[0]
  var bcNodes = [bcHome]
  if (tabID === null) {
    tabID = $(`#tabset div[class = "tab-pane active"][data-value^=tab__]`)[0].id
  }
  var nbElem = $(`nav a[data-value="${tabID}"]`)[0]
  while (nbElem.id != 'navbar_main_container') {
    if (nbElem.tagName == 'LI' && nbElem.children[0].tagName == 'A') {
      var nbA = nbElem.children[0]
      var nbADataValue = nbA.getAttribute('data-value')
      var bcLi = bcHome.cloneNode(true)
      var bcA = bcLi.children[0]
      bcA.text = nbA.text.trim()
      bcA.setAttribute('data-value', nbADataValue)
      bcA.setAttribute('onclick', `LandingPage.Tab.activate('${nbADataValue}')`)
      bcNodes.splice(1, 0, bcLi)
    }
    nbElem = nbElem.parentElement
  }
  bc.replaceChildren(...bcNodes)
}
