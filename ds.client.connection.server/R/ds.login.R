#'@name ds.login 
#'@title attempts to log to some DataSHIELD servers.
#'@description This function uses the login details to log on to the DataSHIELD server(s) specified. The login details can be validated
#'using the ds.build.login.data.frame function. 
#'@param login.data.frame  A dataframe table that holds login details. This table holds five elements required
#'   to login to the servers where the data to analyse is stored. The expected column names are
#'   'driver' (the \code{\link{DSDriver-class}} name, default is "OpalDriver"),
#'   'server' (the server name), url' (the server url), 'user' (the user name or the certificate file path),
#'   'password' (the user password or the private key file path), table' (the fully qualified name of
#'   the table in the data repository), 'options' (the SSL options). An additional column 'identifiers'
#'   can be specified for identifiers mapping (if supported by data repository). See also the documentation
#'   of the examplar input table \code{logindata} for details of the login elements.
#'@param assign A boolean which tells whether or not data should be assigned from the data repository
#'   table to R after login into the server(s). It is set to TRUE by default.
#'@param variables Specific variables to assign. If \code{assign} is set to FALSE this argument is ignored
#'   otherwise the specified variables are assigned to R. If no variables are specified (default) the whole
#'   data repository's table is assigned.
#'@param symbol A character, the name of the data frame to which the data repository's table will be
#'   assigned after login into the server(s). It is set to "D' by default.
#'@return object(s) of class DSConnection or NULL if some parameters are incorrect
#'@author Patricia Ryser-Welch 
#'@export ds.login

library(DSI)
library(DSOpal)
library(httr)


ds.login <- function(login.data.frame = NULL, assign = TRUE, variables = NULL, symbol = 'D')
{
  connection <- NULL
  tryCatch(
     {connection <- .make.connection(login.data.frame, assign, variables, symbol)},
      warning = function(warning) {.warning(warning)},
      error = function(error) {ds.error(error)},
      finally = {return(connection)}
  )
}

.make.connection <- function(login.data.frame, assign, variables, symbol)
{
  connection <- NULL
  
  if (is.null(login.data.frame))
  {
    stop("::ds.login::ERR:010", call. = FALSE)
  }
  
  if (length(login.data.frame[,1]) == 0)
  { 
    stop("::ds.login::ERR:011", call. = FALSE)
  }
  
  
  connection <- DSI::datashield.login(login.data.frame, assign, variables, symbol)
  print(connection)
  
   
  if (is.null(connection))
  {
     stop("::ds.login::ERR:017", call. = FALSE)
  }
  
  
}

.warning <- function(message)
{

  message(paste("ds.client.connection.server::ds.login :",   message ))
 
}

