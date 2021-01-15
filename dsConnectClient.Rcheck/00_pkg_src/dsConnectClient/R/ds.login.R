#' @name ds.login 
#' @title DataSHIELD servers login
#' @description This function uses the login details to log on to 
#' the specified DataSHIELD server(s).  
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
#' In \code{login.data.frame} table five elements are required to log in to the servers 
#' where the data to analyse is stored. 
#' The expected column names are:\cr
#' \itemize{
#'   \item \code{driver}: the \code{\link{DSDriver-class}} name, default is \code{OpalDriver} 
#'   \item \code{server}: the server name
#'   \item \code{url}: the server URL
#'   \item \code{user}: the user name or the certificate file path
#'   \item \code{password}: the user password or the private key file path
#'   \item \code{table}: the fully qualified name of the table in the data repository
#'   \item \code{options}: the SSL options
#'   \item \code{identifiers}: optional column for identifiers mapping 
#'   (if supported by data repository)
#' }
#'  
#'  To built the mentioned table go to \code{\link{ds.build.login.data.frame}} example. 
#'  The input table \code{\link{logindata}} for details of the login elements.
#'  
#' @return \code{ds.login} returns object(s) of class \code{DSConnection} or NULL
#' if some parameters are incorrect. 
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
#' }
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
  connection <- NULL
  
  if (is.null(login.data.frame))
  {
    stop("::ds.login::ERR:010", call. = FALSE)
  }
  
  if (length(login.data.frame[,1]) == 0)
  { 
    stop("::ds.login::ERR:011", call. = FALSE)
  }
  
  if(is.windows())
  {
    Sys.setenv(CURL_SSL_BACKEND = "openssl")
  }
  connection <- DSI::datashield.login(login.data.frame, assign, variables, symbol)
  
   
  if (is.null(connection))
  {
     stop("::ds.login::ERR:017", call. = FALSE)
  }
  
  return(connection)
}


# check the operating system is windows - https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/
is.windows <- function()
{
   return(.Platform$OS.type == "windows")
}

.warning <- function(message)
{

  message(paste("ds.client.connection.server::ds.login :",   message ))
 
}

