#'@name ds.logout
#'@title logout from some DataSHIELD servers 
#'@description Clear the Datashield R sessions and logout from DataSHIELD data repositories.
#'@param connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#'@author Patricia Ryser-Welch
#'@export ds.logout
#'

library(DSI)
library(DSOpal)
library(httr)


ds.logout <- function(connection, save = NULL)
{
  outcome <- TRUE
  tryCatch(
     {.logout(connection,save);},
      warning = function(warning) {.warning(warning)},
      error = function(error) {.error(error)},
      finally = {return(outcome)}
    )
}

.logout <- function(connection, save)
{
  if(is.null(connection))
  {
    stop("ERR:006", call. = FALSE)
  }
  else
  {
    DSI::datashield.logout(connection,save)
  }
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.logout :",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.logout'
  
  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}