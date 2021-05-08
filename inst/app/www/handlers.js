$( document ).ready(function() {
  Shiny.addCustomMessageHandler('loadfromweb', function(arg) {
    alert("Andmestiku '"+ arg.filename + "' laadimine ja lugemine õnnestus. Laadi järgnevalt üles oma andmestik");
  })
});

$( document ).ready(function() {
  Shiny.addCustomMessageHandler('loaduserdata', function(arg) {
    alert("Andmestiku lugemine ei õnnestunud. Veateade: " + arg.err);
  })
});
