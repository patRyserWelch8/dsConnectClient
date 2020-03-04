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
  
  outcome <- FALSE
  tryCatch(
    {outcome <- .share.parameter(connections)},
    warning = function(warning) {.warning(warning)},
    error = function(error) {.error(error)},
    finally = {return(outcome)}
  )
  #print(length(connection))
  #print(ds.aggregate(connection, "ls()"))
  #server.call <- "createMatrixRUnifDS(100,200)"
  #a <- .assign(connection, new.variable.name = "matrix", value = server.call)
  #print(ds.aggregate(connection, "ls()"))
  #print(a)
  
  
  #no.studies <- 1:(length(connection)-1)
 # for (study in no.studies)
  #{
  #  server.call <- "create.matrix.runif.DS.1(11,13,2,500)"
  #  print(server.call)
  #  a <- ds.assign.value(connection, new.variable.name = "matrix.1", value = server.call)
    #DSI::datashield.assign(conns = connection, symbol = "matrix.1", value = as.symbol(server.call), async = FALSE)
   
 # }
  

}

.share.parameter <- function(connections)
{
  if(length(connections) < 2)
  {
    stop("ERR:001")
  }
  else
  {
    return(TRUE)
  }
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.share.param:",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.share.param'
  
  if (grepl("ERR:001",error))
  {
    message(paste(header, "::",  "ERR:001\n", " More than one server is needed to share some parameters. Provide a connection with at least 2 servers.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}