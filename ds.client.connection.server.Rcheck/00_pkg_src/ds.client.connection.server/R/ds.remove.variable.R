#'@name ds.remove.variable
#'@title delete a variable of a specific given type from some DataSHIELD servers. 
#'@description A variable of a specific given type is deleted from each DataSHIELD server. This client function verifies the variable exists on each server, before 
#'removing them from the session.
#'@param connections a valid connection to some data repositories. The later needs to be a valid DSConnection-class (OpalConnection)
#'@param variable.name name of a variable represented as character. Its length should be greater than 0.
#'@param environment.name A character value stating the name of an environment created on the server. By default, it should be set to "\code{\link{.GlobalEnv}}"
#'@param class.type  A character value stating the R internal type. Correct values:
#'\itemize{
#'\item  "\code{\link{NULL}}"
#'\item "\code{\link{character}}"
#'\item "\code{\link{complex}}"
#'\item  "\code{\link{factor}}"
#'\item "\code{\link{double}}"
#'\item "\code{\link{expression}}"
#'\item "\code{\link{integer}}"
#'\item "\code{\link{list}}"
#'\item "\code{\link{logical}}"
#'\item "\code{\link{numeric}}"
#'\item "\code{\link{single}}"
#'\item "\code{\link{raw}}"
#'\item "\code{\link{vector}}"
#'\item "\code{\link{S4}}"
#'\item "\code{\link{NULL}}"
#'\item "\code{\link{function}}"
#'\item "\code{\link{externalptr}}"
#'\item "\code{\link{environment}}"
#'}

#'@details
#'
#''\itemize{
#'\item \code{ds.remove.variable } captures any errors and warnings thrown by the function \code{.remove}. 
#'\item \code{.remove} verifies all the arguments meets some constraints stated above. The server function \code{\link{removeDS}} only deletes
#'an R object with a specific name and data type. \code{.remove} verifies the variable has been deleted successfully on each server, using   \code{\link{ds.exists.on.server}}.
#'}
#'
#' Both functions can be used interchangeably. \code{.remove} allows more efficient debugging of some server and client code. \code{ds.remove.variable} can be used 
#' once the code is efficiently working.
#'@param  connection connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param  new.variable.name  name of a new variable created on a server
#'@return TRUE if the variables have been deleted. FALSE if the values have not been deleted and exists on the servers.
#'@seealso
#'server function used: \code{removeS} (Aggregate function)
#'\code{\link{ds.aggregate}}, \code{\link{ds.exists.on.server}}
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