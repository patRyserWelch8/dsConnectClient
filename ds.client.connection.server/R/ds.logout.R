#'@title 
#'@description 
#'@param login.data.frame  
#'@return 
#'@example
#
#'@author Patricia Ryser-Welch
#'@export
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
  header <- 'ds.client.connection.server::ds.login'
  
  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}