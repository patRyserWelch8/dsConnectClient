#'@name ds.aggregate
#'@title Executes a function on some R server sessions and returns the outcome to the client
#'@description This function executes some functions listed as aggregate on an Opal Server. 
#'Those can be executed in a DataSHIELD server and 
#'returns some non-disclosive value to the client-side. 
#'@details 
#' \itemize{
#' \item \code{ds.aggregate} captures any errors and warnings 
#' thrown by the function \code{.aggregate}. 
#' No error or warning is displayed. If an error or a warning is caught, then
#' \code{"NR"} is returned.
#' \item \code{.aggregate} wraps the function \code{\link{DSI::datashield.aggregate}} function. 
#' A valid Opal Connection and a valid expression (character) is checked. 
#' When these two conditions are both met, then a server call is made. 
#' }
#' 
#' Both functions can be used interchangeably. 
#' \code{.aggregate} allows more efficient debugging of some server and client code. 
#' \code{ds.aggregate} can be used once the code is efficiently working.

#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' @param expression a call or character expression with a server-side function and its arguments. 
#' @param asynchronous logical. If TRUE, the calls are parallelized over the connections. 
#' If FALSE no parallelisation occurs. Default TRUE.
#' @return 
#' \itemize{
#' \item The output from specified server function in \code{expression} argument 
#' \item \code{"NR"} indicates no function has been executed on the server
#' }
#' @seealso \code{\link{DSI::datashield.aggregate}} function.
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
#'   # Execute aggretate server functions
#'   ds.aggregate(expression = quote(dimDS("D")),
#'                datasources = connections)
#'            
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#' }
#' @author Patricia Ryser-Welch for DataSHIELD development team
#' @export ds.aggregate
#'

ds.aggregate <- function(expression = NULL, asynchronous = TRUE, datasources = NULL)
{
  
  outcome <- "NR"
  tryCatch(
    {outcome <- .aggregate(expression, asynchronous, datasources)},
    warning = function(warning) {.warning(warning)},
    error = function(error) {ds.error(error)},
    finally = {return(outcome)}
  )
}

.aggregate <- function(expression=NULL, asynchronous=TRUE, datasources = NULL)
{
  

  correct.class <- any(class(connection) %in%  c("list","OpalConnection", "DSOpal"))

  if(!correct.class)
  {
    stop("::ds.aggregate::ERR:006")
  }
  
  if(is.character(expression) || is.call(expression))
  {
    outcome = "NR"
    tryCatch(outcome <- DSI::datashield.aggregate(datasources,expression,asynchronous),
             error = function(error){ds.error(list("ds.aggregate",as.character(expression), DSI::datashield.errors()), client = FALSE)},
             finally = {return(outcome)})
  }
  else
  {
    stop("::ds.aggregate::ERR:007")
  }
  
}

.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.aggregate :",   message ))
}


