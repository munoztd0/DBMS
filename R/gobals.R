# Description: This file contains the global variables and functions that are used in the shiny app.

# Set the language for the app and add the labels for the login page
shinymanager::set_labels(
   language = "en",
  "Please authenticate" = "Demo credentials are: <br>  <br>
  user: shiny <br>  pwd: demo ",
  "Username:" = "Username:",
  "Password:" = "Password:",
  "Login" = "Login"
)





# data.frame with credentials info
credentials <- data.frame(
  user = c("shiny", "user2"),
  password = c("demo", "pass2"), 
  password_hash = sapply(c("demo", "pass2"), sodium::password_store), 
  permissions = c("admin", "manager"),
  stringsAsFactors = FALSE,
  level = c(2, 0)
)



# load data
data(starwars2, envir=environment())



# close session if inactivity
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000); // time is in milliseconds (2 minutes)
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"




# Delay the login button for 400 ms after the enter key is pressed
delayButton <- "pressbtn = function(){

// click the log in button
document.getElementById('auth-go_auth').click();
};
window.onload = function() {

// password input field
const field = document.getElementById('auth-user_pwd');

// add a function that preempts the enter key press
field.addEventListener('keydown',  
function(e) {

    if (e.keyCode == 13) {

    // prevent sending the key event
    e.preventDefault();
   
    // delay activating the login button for 400 ms. adjust time as needed
    setTimeout(pressbtn,400);
    };

});
}"
