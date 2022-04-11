$(document).ready(function () {
    var header = $('.navbar-collapse');
    header.append('<ul class=\"nav navbar-nav shiny-tab-input shiny-bound-input\" style=\"float:right\"><li><a href="https://kauebraga.dev/" target="_blank"><i class="fa-solid fa-user"></i> Contact</a></li></ul>');
    header.append('<ul class=\"nav navbar-nav shiny-tab-input shiny-bound-input\" style=\"float:right\"><li><a href="https://github.com/kauebraga/gtfs_explorer" target="_blank"> <i class="fa-brands fa-github"></i> Code</a></li></ul>');
    // header.append('<ul class=\"nav navbar-nav shiny-tab-input shiny-bound-input\" style=\"float:right\"><li><a href="https://google.com" target="_blank">About</a></li></ul>');
    console.log(header);
});



// # tags$li(class="dropdown", data-toggle="tab", data-bs-toggle="tab",a("Dados de vacinação: ", href="https://brasil.io/", "brasil.io", target="_blank", style = "float: right"))
// # # tags$li(class="dropdown", uiOutput("dica")),
// # tags$li(class="dropdown", tags$a(href="https://kauebraga.dev/", "Contato", target="_blank")),
// # # tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/abhinav-agrawal-pmp%C2%AE-itil%C2%AE-5720309/" ,icon("linkedin"), "My Profile", target="_blank")),
// # tags$li(class="dropdown",tags$a(href="https://github.com/kauebraga/painel_vacinacao_covid", icon("github"), "Código", target="_blank"))