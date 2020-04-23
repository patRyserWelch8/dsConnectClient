#'@name ds.share.param  
#'@title client function TO DO 
#'@description  TODO
#'@param connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#'@author Patricia Ryser-Welch
#'@export ds.logout
#'

library(DSI)
library(DSOpal)
library(httr)


ds.share.param <- function(connections)
{
  success <- FALSE
  tryCatch(
    {success <- .share.parameter(connections)},
    warning = function(warning) {.warning(warning)},
    error = function(error) {.error(error)},
    finally = {return(success)})
}

.share.parameter <- function(connections)
{
  
  if(length(connections) > 1)
  {
    
    last <- length(connections)-1
    print(last)
    for(current in 1:last)
    {
      print(current)
      master <- connections[[current]]
      #outcome <- ds.create_environment(master,"sharing")
      outcome <- ds.aggregate(master, "initiateExchangeDS()")
      print(outcome)
      outcome <- ds.aggregate(master, "environmentInfoDS()")
      
      print(class(outcome))
      print(outcome)
      
      #outcome <- ds.remove.variable(master,"sharing")
    }
    
    return(TRUE)
  }
  else
  {
    warning("WAR:001")
  }
  
 
}

.warning <- function(message)
{
  
  message(paste("ds.client.connection.server::ds.share.param:",   message ))
  if (grepl("WAR:001",error))
  {
    message(paste(header, "::",  "WAR:001\n", "More than one connection is required for sharing parameters.")) 
  }
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.share.param'
  
  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}