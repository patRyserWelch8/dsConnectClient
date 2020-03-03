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
  print(ds.aggregate(connection, "ls()"))
  server.call <- "create.matrix.runif.DS.1()"
  print(server.call)
  DSI::datashield.assign(conns = connection, symbol = "matrix", value = as.symbol(server.call), async = FALSE)
 # a <- .assign(connection, new.variable.name = "matrix", value = server.call)
  print(ds.aggregate(connection, "ls()"))
  print(a)
  
  
  #no.studies <- 1:(length(connection)-1)
 # for (study in no.studies)
  #{
  #  server.call <- "create.matrix.runif.DS.1(11,13,2,500)"
  #  print(server.call)
  #  a <- ds.assign.value(connection, new.variable.name = "matrix.1", value = server.call)
    #DSI::datashield.assign(conns = connection, symbol = "matrix.1", value = as.symbol(server.call), async = FALSE)
   
 # }
  

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