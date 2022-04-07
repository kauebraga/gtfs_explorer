fileInput2 <- function (inputId, label, multiple = FALSE, accept = NULL, width = NULL, 
                        buttonLabel = "Browse...", placeholder = "No file selected") 
{
  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", 
                         style = "position: absolute !important; top: -99999px !important; left: -99999px !important;", 
                         `data-restore` = restoredValue)
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), 
      shinyInputLabel(inputId, label), div(class = "input-group", 
                                           tags$label(class = "input-group-btn input-group-prepend", 
                                                      span(class = "btn btn-default btn-file", buttonLabel, 
                                                           inputTag))), 
      tags$div(id = paste(inputId, "_progress", sep = ""), 
               class = "progress active shiny-file-input-progress", 
               tags$div(class = "progress-bar")))
}