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
#'@param class.type  A character value stating the R internal type. Correct values:
#'\itemize{
#'\item "\code{\link{NULL}}"
#'\item "\code{\link{character}}"
#'\item "\code{\link{complex}}"
#'\item  "\code{\link{factor}}"
#'\item "\code{\link{double}}"
#'\item "\code{\link{expression}}"
#'\item "\code{\link{integer}}"
#'\item "\code{\link{list}}"
#'\item "\code{\link{matrix}}"
#'\item "\code{\link{logical}}"
#'\item "\code{\link{numeric}}"
#'\item "\code{\link{single}}"
#'\item "\code{\link{vector}}"
#'\item  "\code{\link{S4}}"
#'\item "\code{\link{environment}}"
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

ds.remove.variable <- function(connection=NULL, variable.name=NULL, class.type=NULL)
{
  outcome <- FALSE
  tryCatch(
     {outcome <- .remove(connection, variable.name, class.type)},
      warning = function(warning) {.warning(warning)},
      error = function(error) {ds.error(error)},
      finally = {return(outcome)}
       )
}

.remove <- function(connection=NULL, variable.name=NULL, class.type= NULL)
{
  
  if(!is.list(connection))
  {
    stop("::ds.remove.variable::ERR:006")
  }
  
  if (!is.character.argument.correct(variable.name))
  {
    stop("::ds.remove.variable::ERR:008")
  }
 
  if (!is.class.type.correct(class.type))
  {
    stop("::ds.remove.variable::ERR:012", call. = FALSE)
  }
  
  return(.delete.var(connection, variable.name, class.type))
}

.delete.var <- function(connection, variable.name, class.type)
{  
  expression <- paste0("removeDS(variable.name='", variable.name, "',environment.name='.GlobalEnv',class.type='", class.type,"')")
  print(expression)
  ds.aggregate(connection,expression, asynchronous = FALSE)
  variable.exists <- ds.exists.on.server(connection, variable.name, class.type)
  print(all(variable.exists == FALSE))
  return(as.logical(variable.exists == FALSE))
}

.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.remove.variable :",   message ))
}

