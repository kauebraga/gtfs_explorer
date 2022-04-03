column1 <- function (width, width2 = NULL, width3 = NULL, ..., offset = 0) 
{
  if (!is.numeric(width) || (width < 1) || (width > 12)) 
    stop("column width must be between 1 and 12")
  colClass <- paste0("col-sm-", width, " col-md-", width2, " col-lg-", width3)
  if (offset > 0) {
    colClass <- paste0(colClass, " offset-md-", offset, 
                       " col-sm-offset-", offset)
  }
  div(class = colClass, ...)
}
