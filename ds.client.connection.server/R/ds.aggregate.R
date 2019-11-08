#'@title ds.assign
#'@description Assign a table or an expression result to a R symbol in the Datashield R session.
#'@param connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#'@example
#' ds.logout(connection, save <- FALSE)
#'@author Patricia Ryser-Welch
#'@export
#'

library(DSI)
library(DSOpal)
library(httr)

ds.aggregate <- function(connection=NULL, expression=NULL, asynchronous=TRUE)
{
  outcome <- "NR"
  tryCatch(
  {outcome <- .aggregate(connection,expression, asynchronous)},
   warning = function(warning) {.warning(warning)},
   error = function(error) {.error(error)},
   finally = {return(outcome)}
  )
}

.aggregate <- function(connection=NULL, expression=NULL, asynchronous=TRUE)
{
 
  if(!grepl("list",class(connection)))
  {
    stop("ERR:006", call. = FALSE)
  }
  else 
  {
   
    if (!grepl("character",class(expression)))
    {
       stop("ERR:007", call. = FALSE)
    }
    else
    {
      return(DSI::datashield.aggregate(connection,expression,asynchronous))
    }
  }
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.aggregate :",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.aggregate'

  
  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else if (grepl("ERR:007",error))
  {
    message(paste(header, "::",  "ERR:007\n", " You have yet to provide a valid expression.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}

