#'@title ds.aggregate
#'@description 
#'@param 
#'@param 
#'@example
#' ds.logout(connection, save <- FALSE)
#'@author Patricia Ryser-Welch
#'@export
#'

library(DSI)
library(DSOpal)
library(httr)

#conns, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, id.name=NULL, async=TRUE
#conns, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, id.name=NULL, async=TRUE
ds.assign.value <- function(connection, new.variable.name, value, list.variables=NULL, symbol)
{
  .assign(connection, new.variable.name, value, list.variables, symbol)
  #outcome <- TRUE
  #tryCatch(
  #   {.logout(connection,save);},
  #    warning = function(warning) {.warning(warning)},
  #    error = function(error) {.error(error)},
  #    finally = {return(outcome)}
  #  )
}

.assign <- function(connection, new.variable.name, value, list.variables, symbol )
{
  print("assign")
  print(connection)
  print(new.variable.name)
  print(value)
  print(list.variables)
  print(symbol)
  DSI::datashield.assign(connection,symbol,value)
  
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