#' @name ds.login 
#' @title Attempts to log to some DataSHIELD servers.
#' @description This function uses the login details to log on to the specified DataSHIELD server(s).  
#' The login details can be validated
#' using the \code{ds.build.login.data.frame} function. 
#' @param login.data.frame  a data frame table that holds login details. 
#' For more information see \strong{Details}. 
#' @param assign a boolean. If TRUE the data is assigned from the data repository
#' table to R after login into the server(s). Default TRUE. 
#' @param variables a vector specifying the variables to assign.
#' If \code{assign} is set to FALSE this argument is ignored
#' otherwise the specified variables are assigned to R. 
#' If no variables are specified (default) the whole
#' data repository's table is assigned.
#' @param symbol a character specifying the name of the data frame to which the data repository's
#'  table will be assigned after login into the server(s). Default \code{"D"}. 
#' @details 
#' In \code{login.data.frame} table five elements are required to login to the servers 
#' where the data to analyse is stored. 
#' The expected column names are:\cr
#' \itemize{
#'   \item \code{driver}: the \code{\link{DSDriver-class}} name, default is \code{OpalDriver} 
#'   \item \code{server}: the server name
#'   \item \code{url}: the server url
#'   \item \code{user}: the user name or the certificate file path
#'   \item \code{password}: the user password or the private key file path
#'   \item \code{table}: the fully qualified name of the table in the data repository
#'   \item \code{options}: the SSL options
#'   \item \code{identifiers}: optional column for identifiers mapping 
#'   (if supported by data repository)
#' }
#'  
#'  
#'  @seealso the documentation of the example input table \code{\link{logindata}} 
#'  for details of the login elements.
#'  
#' @return \code{ds.login} returns object(s) of class \code{DSConnection} or NULL
#' if some parameters are incorrect. 
#' @author Patricia Ryser-Welch for DataSHIELD development team
#' @export ds.login


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
  if (is.null(login.data.frame))
  {
    stop("::ds.login::ERR:010", call. = FALSE)
  }
  
  if (length(login.data.frame[,1]) == 0)
  { 
    stop("::ds.login::ERR:011", call. = FALSE)
  }
  connection <- NULL
  tryCatch(connection <- DSI::datashield.login(login.data.frame, assign, variables, symbol),
           error = function(error) {ds.error(DSI::datashield.errors())})
   
  if (is.null(connection))
  {
     stop("::ds.login::ERR:017", call. = FALSE)
  }
  
  return(connection)
}

.warning <- function(message)
{

  message(paste("ds.client.connection.server::ds.login :",   message ))
 
}

