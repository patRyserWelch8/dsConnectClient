#'@name ds.aggregate
#'@title executes a function on some R server sessions and returns the outcome to the client
#'@description This function executes some functions listed as aggregate on a Opal Server. Those can be executed in a DataShield server and 
#'returns some non-disclosive value back to the client. 
#'@details 
#' \itemize{
#' \item \code{ds.aggregate} captures any errors and warnings thrown by the function \code{.aggregate}. No error or warning is displayed. If an error or a warning is caught, then
#' "NR" is returned.
#' \item \code{.aggregate} wraps the funcriont \code{DSI::datashield.aggregate function}. A valid OpalConnection and a valid expression '(character) is checked. When these two conditions are both met, then a server call is made. 
#' }
#' Both functions can be used interchangeably. \code{.aggregate} allows more efficient debugging of some server and client code. \code{ds.aggregate} can be used 
#' once the code is efficiently working.
#'@seealso  DSI::datashield.aggregate function.
#'@param connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#'@return 
#'\itemize{
#'\item The value returned by a server function
#'\item "NR" indicates no function has been executed on the server
#'}
#'@author Patricia Ryser-Welch
#'@export ds.aggregate
#'

library(DSI)
library(DSOpal)
library(httr)

ds.aggregate <- function(connection=NULL, expression=NULL, asynchronous=TRUE)
{
  outcome <- "NR"
  tryCatch(
  {outcome <- .aggregate(connection,expression, asynchronous)},
   warning = function(warning) {.warning(warning)},
   error = function(error) {ds.error(error)},
   finally = {return(outcome)}
  )
}

.aggregate <- function(connection=NULL, expression=NULL, asynchronous=TRUE)
{
  if(!is.list(connection))
  {
    stop("::ds.aggregate::ERR:006")
  }

  if(is.character(expression) || is.call(expression))
  {
    outcome <- DSI::datashield.aggregate(connection,expression,asynchronous)
    return(outcome)
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


