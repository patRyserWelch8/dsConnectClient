#'@name ds.assign.value
#'@title assgin some values on some DataSHIELD servers
#'@description Assign a table or an expression result to a R variable, within a DataShield R session on at least one server.
#'@param  connection connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param  new.variable.name  name of a new variable created on a server
#'@param  value It has twofold: (1) The name of a column in a data repositories or a R expression allowed to assign function calls
#'@param  asynchronous When set to TRUE, the calls are parallelized over the connections. When set to false. No parallisation occurs.
#'@return TRUE if the values have been created in all the servers. FALSE if the values have not been successfully created on all the servers
#'@author Patricia Ryser-Welch
#'@export ds.assign.value
#'

library(DSI)
library(DSOpal)
library(httr)

ds.assign.value <- function(connection=NULL, new.variable.name=NULL, value=NULL,asynchronous=FALSE)
{
  
  outcome <- FALSE
  tryCatch(
     {outcome <- .assign(connection, new.variable.name, value, asynchronous)},
      warning = function(warning) {.warning(warning)},
      error = function(error) {.error(error)},
      finally = {return(outcome)}
       )
}

.assign <- function(connection=NULL, new.variable.name=NULL, value=NULL, asynchronous=FALSE)
{
  if(!grepl("list",class(connection)))
  {
    stop("ERR:006", call. = FALSE)
  }
  else if (!grepl("character",class(new.variable.name)))
  {
      stop("ERR:008", call. = FALSE)
  }
  else if (nchar(new.variable.name) == 0)
  {
      stop("ERR:009", call. = FALSE)
  }
  else if (!grepl("character",class(value)))
  {
    stop("ERR:010", call. = FALSE)
  }
  else if (nchar(value) == 0)
  {
    stop("ERR:011", call. = FALSE)
  }
  else
  {
      ds.remove.variable(connection, new.variable.name)
      DSI::datashield.assign(conns = connection, symbol = new.variable.name, value = as.symbol(value), async = asynchronous)
      return(ds.find.variable(connection, new.variable.name))
  }
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.assign.value :",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.assign.value'
  
  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else if (grepl("ERR:008",error))
  {
    message(paste(header, "::",  "ERR:008\n", " You have yet to provide name for the new variable. It has to be a variable character.")) 
  }
  else if (grepl("ERR:009",error))
  {
    message(paste(header, "::",  "ERR:009\n", " The new variable name must be longer than one character.")) 
  }
  else if (grepl("ERR:010",error))
  {
    message(paste(header, "::",  "ERR:010\n", " You have yet to provide some values. It has to be in a character.")) 
  }
  else if (grepl("ERR:011",error))
  {
    message(paste(header, "::",  "ERR:011\n", " The value should be a character variable, with a length greater than 1.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}