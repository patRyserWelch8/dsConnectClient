#' @name ds.warning
#' @title Displays DataSHIELD and  R warnings
#' @description Shows server-side and/or client-side warnings
#' @param client.function.name a character argument passing the name of the function that has 
#' thrown the warning
#' @param warning The warning thrown by R or a function
#' @return \code{ds.warning} returns a warning either
#' @seealso \code{ds.error}
#'@export
ds.warning <- function(client.function.name, warning)
{
  print("====warning")
  print(client.function.name)
  warning.message <- paste("The following warning has been thrown by the function name", client.function.name, ":\n")
  message(warning.message, warning)
  #if (grepl("WAR:001",message))
  #{
  #  message(paste(header, "::",  "WAR:001\n", "More than one connection is required for sharing parameters.")) 
  #}
}
