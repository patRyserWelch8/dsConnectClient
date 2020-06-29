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
   error = function(error) {.error(error)},
   finally = {return(outcome)}
  )
}

.aggregate <- function(connection=NULL, expression=NULL, asynchronous=TRUE)
{
 
  list.type <- c("list","OpalConnection")
  type      <- class(connection)
  if(!(type %in% list.type))
  {
    stop("ERR:006", call. = FALSE)
  }
  else 
  {
    if (!grepl("character",class(expression)))
    {
       stop("ERR:007", call. = FALSE)
    }
    else
    {
        return(DSI::datashield.aggregate(connection,expression,asynchronous))
    }
  }
}

.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.aggregate :",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.aggregate'

  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else if (grepl("ERR:007",error))
  {
    message(paste(header, "::",  "ERR:007\n", " You have yet to provide a valid expression.")) 
  }
  else if (grepl("ERR:008",error))
  {
    message(paste(header, "::",  "ERR:008\n", " You have yet to function valid server function.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}

