#'@name ds.remove.variable
#'@title remove a variable from some DataSHIELD servers. 
#'@description The function calls the server function "rmDS" to delete an existing variable from the server. If the function does not exist, then no deletion occurs.
#'@param  connection connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param  new.variable.name  name of a new variable created on a server
#'@return TRUE if the variables have been deleted. FALSE if the values have not been deleted and exists on the servers.
#'@author Patricia Ryser-Welch on behalf of DataSHIELD team
#'@export ds.remove.variable
#'

library(DSI)
library(DSOpal)
library(httr)

ds.remove.variable <- function(connection=NULL, variable.name=NULL)
{

  outcome <- FALSE
  tryCatch(
     {outcome <- .remove(connection, variable.name)},
      warning = function(warning) {.warning(warning)},
      error = function(error) {.error(error)},
      finally = {return(outcome)}
       )
}

.remove <- function(connection=NULL, variable.name=NULL)
{
  if(!grepl("list",class(connection)))
  {
    stop("ERR:006", call. = FALSE)
  }
  else if (!grepl("character",class(variable.name)))
  {
      stop("ERR:008", call. = FALSE)
  }
  else if (nchar(variable.name) == 0)
  {
      stop("ERR:009", call. = FALSE)
  }
  else
  {
      
      variable.exist <- ds.find.variable(connection, variable.name)
      if (variable.exist)
      {
         expression <- paste("rmDS('", variable.name, "')",sep="")
         ds.aggregate(connection,expression, asynchronous = FALSE)
         
         #variable should be deleted. so ds.find.variable should return false. However, the function 
         #should return true to indicate successful deletion
         return(!ds.find.variable(connection, variable.name))
      }
  }
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.remove.variable :",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.remove.variable'
  
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
  else
  {
    message(paste(header,"\n", error))
  }
}