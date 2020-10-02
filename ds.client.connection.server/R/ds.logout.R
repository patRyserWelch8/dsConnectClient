#'@name ds.logout
#'@title log out from some DataSHIELD servers 
#'@description Clear the DataSHIELD R sessions and logout from DataSHIELD data repositories.
#'@param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#'@param Save DataSHIELD sessions on each server data repository (if feature is supported)
#' with provided ID (must be a character string).
#'@author Patricia Ryser-Welch for DataSHIELD development team 
#'@export ds.logout
#'


ds.logout <- function(datasources, save = NULL)
{
  outcome <- TRUE
  tryCatch(
     {.logout(datasources,save);},
      warning = function(warning) {.warning(warning)},
      error = function(error) {ds.error(error)},
      finally = {return(outcome)}
    )
}

.logout <- function(datasources, save)
{
  if(is.null(datasources))
  {
    stop("::ds.logout::ERR:006", call. = FALSE)
  }
  
  DSI::datashield.logout(datasources,save)
  
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.logout :",   message ))
}

