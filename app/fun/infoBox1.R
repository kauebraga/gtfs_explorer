infoBox1 <- function (title, value = NULL, subtitle = NULL, icon = shiny::icon("bar-chart"), 
                      color = "aqua", 
                      width = NULL, width2 = NULL, width3 = NULL, width4 = NULL,
                      href = NULL, fill = FALSE) 
{
  # validateColor(color)
  # tagAssert(icon, type = "i")
  colorClass <- paste0("bg-", color)
  boxContent <- div(class = "info-box", class = if (fill) 
    colorClass, span(class = "info-box-icon", class = if (!fill) 
      colorClass, icon), div(class = "info-box-content", span(class = "info-box-text", 
                                                              title), if (!is.null(value)) 
                                                                span(class = "info-box-number", value), if (!is.null(subtitle)) 
                                                                  p(subtitle)))
  if (!is.null(href)) boxContent <- a(href = href, boxContent)
  cols <- if (is.null(width)) paste0("col") else paste0("col-sm-", width, " col-md-", width2, " col-lg-", width3, " col-xl-", width4)
  div(class = cols, boxContent)
}
