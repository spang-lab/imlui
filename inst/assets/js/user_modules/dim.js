export function get(params) {
  // Based on:
  // https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
  // console.log('Entered getWindowSize')
  var dim = [0, 0]
  dim[0] = window.innerWidth
  dim[1] = window.innerHeight
  Shiny.setInputValue('dim', dim)
}
