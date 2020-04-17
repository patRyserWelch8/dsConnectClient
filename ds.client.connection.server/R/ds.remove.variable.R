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

ds.remove.variable <- function(connection=NULL, variable.name=NULL, environment.name = "",class.type=NULL)
{

  outcome <- FALSE
  tryCatch(
     {outcome <- .remove(connection, variable.name, environment.name, class.type)},
      warning = function(warning) {.warning(warning)},
      error = function(error) {.error(error)},
      finally = {return(outcome)}
       )
}

.remove <- function(connection=NULL, variable.name=NULL, environment.name, class.type)
{
  list.type <- c("list","OpalConnection")
  type      <- class(connection)
  if(!(type %in% list.type))
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
   
    if (length(environment.name) == 1)
    {
      expression <- paste("removeDS(variable.name='", variable.name, "',  class.type='", class.type,"')" ,sep="")
    }
    else
    {
      expression <- paste("removeDS(variable.name='", variable.name, "', environment.name = '.GlobalEnv", environment.name, "', class.type='", class.type,"')" ,sep="")
    }
    
    ds.aggregate(connection,expression, asynchronous = FALSE)
    variable.exists <- ds.exists.on.server(connection, variable.name,".GlobalEnv",class.type)
    return(as.logical(variable.exists == FALSE))
    
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
   else if (grepl("ERR:010",error))
  {
    message(paste(header, "::",  "ERR:010\n", " An error has occured on the server.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}