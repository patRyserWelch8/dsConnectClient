#' @name ds.remove.variable
#' @title Delete a variable of a specific given type on the server-side 
#' @description A variable of a specific given type is deleted from each DataSHIELD server. 
#' This client function verifies that the variable exists on each server, 
#' before removing them from the session.
#' @param variable.name a character string specifying the name of the variable. 
#' @param class.type  a character string specifying the R internal type of the variable.\cr
#' Correct values:
#' \itemize{
#'   \item  "\code{\link{NULL}}"
#'   \item "\code{\link{character}}"
#'   \item "\code{\link{complex}}"
#'   \item  "\code{\link{factor}}"
#'   \item "\code{\link{double}}"
#'   \item "\code{\link{expression}}"
#'   \item "\code{\link{integer}}"
#'   \item "\code{\link{list}}"
#'   \item "\code{\link{logical}}"
#'   \item "\code{\link{numeric}}"
#'   \item "\code{\link{single}}"
#'   \item "\code{\link{raw}}"
#'   \item "\code{\link{vector}}"
#'   \item "\code{\link{S4}}"
#'   \item "\code{\link{NULL}}"
#'   \item "\code{\link{function}}"
#'   \item "\code{\link{externalptr}}"
#'   \item "\code{\link{environment}}"
#' }
#' @param  datasources a list of \code{\link{DSConnection-class}} objects obtained after login

#' @details
#'
#' \itemize{
#' \item \code{ds.remove.variable} captures any errors and warnings 
#' thrown by the function \code{.remove}. 
#' \item \code{.remove} verifies all the arguments meet some constraints stated above. 
#' The server function \code{\link{removeDS}} only deletes
#'  an R object with a specific name and data type. 
#'  \code{.remove} verifies the variable has been deleted successfully on each server, 
#'  using \code{\link{ds.exists.on.server}}.
#'  }
#'
#' Both functions can be used interchangeably: \cr
#' \code{.remove} allows more efficient debugging of some server and client code.\cr
#' \code{ds.remove.variable} can be used once the code is efficiently working.
#' @return TRUE if the variables have been deleted. 
#' @return FALSE if the values have not been deleted and exist on the servers.
#'@seealso
#'server function used: \code{removeSD} (Aggregate function)\cr
#'\code{\link{ds.aggregate}}\cr
#'\code{\link{ds.exists.on.server}}
#' @examples 
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki
#'   # Connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   require('ds.client.connection.server')
#' 
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#' 
#'   # Assign some values in the server-side
#'   
#'   ds.assign.value(new.variable.name = "lab.tsc",
#'                   value = "D$LAB_TSC", 
#'                   class.type = "numeric", 
#'                   datasources = connections)
#'                   
#'   #Remove from the server-side the assigned variable
#'   
#'   ds.remove.variable(variable.name = "lab.tsc",
#'                      class.type = "numeric",
#'                      datasources = connections)
#'            
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#' }
#'@author Patricia Ryser-Welch for DataSHIELD team
#'@export ds.remove.variable
#'


ds.remove.variable <- function(variable.name = NULL, class.type= NULL, datasources = NULL)
{
  outcome <- FALSE
  tryCatch(
     {outcome <- .remove(variable.name, class.type, datasources)},
      warning = function(warning) {.warning(warning)},
      error = function(error) {ds.error(error)},
      finally = {return(outcome)}
       )
}

.remove <- function(variable.name=NULL, class.type= NULL, datasources = NULL)
{
  
  if(!is.list(datasources))
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
  
  return(.delete.var(variable.name, class.type, datasources))
}

.delete.var <- function(variable.name, class.type, datasources)
{  
  expression <- paste0("removeDS(variable.name='", variable.name, "',environment.name='.GlobalEnv',class.type='", class.type,"')")
  ds.aggregate(expression = expression, asynchronous = FALSE, datasources)
  variable.exists <- ds.exists.on.server(variable.name, class.type, datasources)
  return(as.logical(variable.exists == FALSE))
}

.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.remove.variable :",   message ))
}

