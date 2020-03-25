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


ds.share.param <- function(connection)
{
 
  print(class(connection))
  print("2")
  
 # print(ds.aggregate(connection, "ls()"))
  #outcome <- ds.create_environment(connection,"sharing")
  #print(outcome)
  #print(ds.aggregate(connection, "ls()"))
  #server.call <- "setVariableDS()"
  #print(server.call)
  #print(ds.aggregate(connection,server.call))
  print(.aggregate(connection, "initiateExchangeDS()"))
  
  
  
  
}




.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.share.param:",   message ))
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