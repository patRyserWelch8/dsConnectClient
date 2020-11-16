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
#'   \item "\code{\link{RangedSummarizedExperiment}}"
#'   \item "\code{\link{SummarizedExperiment}}"
#'   \item "\code{\link{ExpressionSet}}"
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
#'   ## Version 6.2, for older versions see the Wiki
#'   # Connecting to the Opal servers
#'   
#'   # Only for windows user 
#'   ## (switches implementation of SSL used by  the curl R package to "openssl")
#'   Sys.setenv(CURL_SSL_BACKEND = "openssl")
#' 
#'   # Load necessary client packages
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   require('ds.client.connection.server')
#' 
#'   # Append login information for a specific server
#'   
#'     #Data computers name
#'     server.names   <- c("Paris", "Newcastle", "New York")
#'     
#'     # Data computers url
#'     url_Paris     <- 'https://192.168.56.100:8443'
#'     url_Newcastle <- 'https://192.168.56.100:8443'
#'     url_NewYork   <-  'https://192.168.56.100:8443'
#'     server.urls     <- c(url_Paris,url_Newcastle,url_NewYork)
#'     
#'     # Assign datasets
#'     table_Paris     <- "TESTING.DATASET1"
#'     table_Newcastle <- "TESTING.DATASET2"
#'     table_NewYork   <- "TESTING.DATASET3"
#'     server.tables   <- c(table_Paris, table_Newcastle, table_NewYork)
#'
#'     # Set user and password to access the DataSHIELD servers
#'     user_Paris      <-  "administrator"
#'     user_Newcastle  <-  "administrator"
#'     user_NewYork    <-  "administrator"
#'     server.users.id <- c(user_Paris, user_Newcastle, user_NewYork)
#'
#'     password_Paris      <-  "datashield_test&"
#'     password_Newcastle  <-  "datashield_test&"
#'     password_NewYork    <-  "datashield_test&"
#'     server.users.pwd    <-  c(password_Paris, password_Newcastle, password_NewYork)
#'     
#'     # Set drivers
#'     driver_Paris     <- "OpalDriver"
#'     driver_Newcastle <- "OpalDriver"
#'     driver_NewYork   <- "OpalDriver"
#'     server.drivers   <- c(driver_Paris,driver_Newcastle,driver_NewYork)
#'
#'     # Set SSL drivers
#'     ssl_options_Paris     <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
#'     ssl_options_Newcastle <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
#'     ssl_options_NewYork   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
#'     server.ssl.options    <- c(ssl_options_Paris,ssl_options_Newcastle,ssl_options_NewYork)
#'       
#'     # Create login data frame
#'     login.data <- ds.build.login.data.frame(server.names,
#'                                             server.urls,
#'                                             server.tables,
#'                                             server.users.id,
#'                                             server.users.pwd,
#'                                             server.ssl.options,
#'                                             server.drivers)
#'   # Log in to DataSHIELD server                                         
#'   connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = "D")
#' 
#'   # Assign some values in the server-side
#'   ds.assign.value(new.variable.name = "new_var",
#'                   value = "D$INTEGER", 
#'                   class.type = "integer", 
#'                   datasources = connections)
#'                   
#'   #Remove from the server-side the assigned variable
#'   ds.remove.variable(variable.name = "new_var",
#'                      class.type = "integer",
#'                      datasources = connections)
#'            
#'   # Clear the Datashield/R sessions and logout
#'   ds.logout(connections)  
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
  correct.class <- any(class(datasources) %in%  c("list","OpalConnection", "DSOpal"))
  
  if(!correct.class)
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

