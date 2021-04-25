$( document ).ready(function() {
  Shiny.addCustomMessageHandler('loadfromweb', function(arg) {
    alert("Andmestiku '"+ arg.filename + "' laadimine ja lugemine õnnestus. Laadi järgnevalt üles oma andmestik");
  })
});

$( document ).ready(function() {
  Shiny.addCustomMessageHandler('loaduserdata', function(arg) {
    alert("Andmestiku laadimine ja lugemine õnnestus. Veateade: " + arg.err);
  })
});
