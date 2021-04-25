$( document ).ready(function() {
  Shiny.addCustomMessageHandler('loadfromweb', function(arg) {
    alert("Andmestiku '"+ arg.filename + "' laadimine ja lugemine õnnestus");
  })
});
