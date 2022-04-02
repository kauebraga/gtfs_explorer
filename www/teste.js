$(document).on('shiny:disconnected', function(event) {
  alert('Disconnected from the server'); 
}); 


$(document).on('shiny:error', function(event) {
  alert("Deu ruim! aqui oh: " + event.name); 
});





function alertme(id){
  var name = prompt("Who are you?");
  alert("Hello " + name + "! You're seeing " + id);
}
/* We're adding this so that the function is launched only
when the document is ready */
$(function(){ 
  // Selecting all `{shiny}` plots
  $("#graph_trips_by_service").on("click", function(){
    /* Calling the alertme function with the id 
    of the clicked plot */
    alertme(this.id);
  });
});
$(function(){ 
  // Selecting all `{shiny}` plots
  $("#map_city").on("click", function(){
    /* Calling the alertme function with the id 
    of the clicked plot */
    alertme(this.id);
  });
});