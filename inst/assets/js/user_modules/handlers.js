export function login_panel_key_up(event) {
  console.log(`key_up(event = ${event})`)
  console.log(`event.keyCode = ${event.keyCode}`)
  if ($('#login_password').is(':focus') && event.keyCode == 13) {
    document.getElementById("login_button").click();
  }
}
