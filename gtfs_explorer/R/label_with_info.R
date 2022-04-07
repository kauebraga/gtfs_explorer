label_with_info <- function(label, tooltip_id, tooltip_title, tooltip_text) {
  
  x <- HTML(sprintf("<span class = info-box-text>%s <button id=\"%s\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></span>", 
                    label, tooltip_id))
  
  y <- bsPopover(id = tooltip_id,
                 title = tooltip_title,
                 content = includeHTML(tooltip_text),
                 placement = "top",
                 trigger = "hover",
                 options = list(container = "body")
  )
  
  return(HTML(paste0(x, y)))
  
}
