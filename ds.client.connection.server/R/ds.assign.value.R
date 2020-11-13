#' @name ds.assign.value
#' @title Assign some values on some DataSHIELD servers
#' @description Assign a table or an expression result to an R variable, 
#' within a DataSHIELD R session on at least one server.
#' @param  datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' @param  new.variable.name  a character string specifying 
#' the name of a new variable created on a server
#' @param  value the name of a column in a data repositories or 
#' an R expression allowed to assign function calls
#' @param  class.type  a character string stating the R internal type. \cr
#' Correct values:
#' \itemize{
#' \item "\code{\link{NULL}}"
#' \item "\code{\link{character}}"
#' \item "\code{\link{complex}}"
#' \item  "\code{\link{factor}}"
#' \item "\code{\link{double}}"
#' \item "\code{\link{expression}}"
#' \item "\code{\link{integer}}"
#' \item "\code{\link{list}}"
#' \item "\code{\link{matrix}}"
#' \item "\code{\link{logical}}"
#' \item "\code{\link{numeric}}"
#' \item "\code{\link{single}}"
#' \item "\code{\link{vector}}"
#' \item  "\code{\link{S4}}"
#' \item "\code{\link{environment}}"
#' }
#' @param  asynchronous logical. If TRUE, the calls are parallelized over the connections. 
#' If FALSE no parallelisation occurs. Default TRUE.
#' @return 
#' \itemize{
#' \item TRUE if the values have been created in all the servers. 
#' \item FALSE if the values have not been successfully created on all the servers
#' }
#'
#' @details 
#' \itemize{
#' \item \code{ds.assign.value} captures any errors and warnings 
#' thrown by the function \code{.assign}. No error or warning is displayed. 
#' If an error or a warning is caught, then the function returns FALSE.
#' \item \code{.assign} wraps the function \code{\link{DSI::datashield.assign}}. 
#' A valid OpalConnection, a valid server variable name and value is checked. 
#' When  all these conditions are met, then a server call is made. 
#' }
#' Both functions can be used interchangeably. 
#' \code{.assign} allows more efficient debugging of some server and client code. 
#' \code{ds.assign.value} can be used 
#' once the code is efficiently working.
#' @seealso  \code{\link{DSI::datashield.assign}}
#' @seealso \code{\link{ds.exists.on.server}}
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
#'   
#'   ds.assign.value(new.variable.name = "new_var",
#'                   value = "D$INTEGER", 
#'                   class.type = "integer", 
#'                   datasources = connections)
#'            
#'   # Clear the Datashield/R sessions and logout
#'   ds.logout(connections) 
#' }
#' @author Patricia Ryser-Welch for DataSHIELD development team
#' @export ds.assign.value

ds.assign.value <- function(new.variable.name = NULL, value = NULL, class.type = NULL,asynchronous = FALSE, datasources = NULL)
{
  outcome <- FALSE
  tryCatch(
    {
      outcome <- .assign(new.variable.name, value, class.type, asynchronous, datasources)},
    warning = function(warning) {.warning(warning)},
    error = function(error) {ds.error(error)},
    finally = {return(outcome)})
}

.assign <- function(new.variable.name=NULL, value=NULL, class.type = NULL, asynchronous=FALSE, datasources = NULL)
{
  correct.class <- any(class(datasources) %in%  c("list","OpalConnection", "DSOpal"))
  
  if(!correct.class)
  {
    stop("::ds.assign.value::ERR:006")
  }
  
  if (!is.character.argument.correct(new.variable.name))
  {
    stop("::ds.assign.value::ERR:008")
  }
  
  if (!is.value.for.assignment.correct(value))
  {
    stop("::ds.assign.value::ERR:009", call. = FALSE)
  }
  
  if (!is.class.type.correct(class.type))
  {
    stop("::ds.assign.value::ERR:012", call. = FALSE)
  }
  
  .create.variable(new.variable.name,value, class.type, asynchronous, datasources)
  return(ds.exists.on.server(variable.name = new.variable.name, class.type = class.type, datasources = datasources))
}


.create.variable <- function(new.variable.name = NULL, value = NULL, class.type = NULL, asynchronous = NULL, datasources = NULL)
{
  #delete variable from the server if it exists already
  ds.remove.variable(new.variable.name, class.type, datasources)
  #create variable on the server(s)
  
  if (is.character(value))
  {
    tryCatch(DSI::datashield.assign(conns = datasources, symbol = new.variable.name, value = as.symbol(value), async = asynchronous),
             error = function(error){ds.error(list("ds.assign.value",as.character(value), DSI::datashield.errors()), client = FALSE)})
  }
  else if (is.symbol(value))
  {
    tryCatch(DSI::datashield.assign(conns = datasources, symbol = new.variable.name, value = value, async = asynchronous),
             error = function(error){ds.error(list("ds.assign.value",as.character(value), DSI::datashield.errors()), client = FALSE)})
  }
  else if (is.call(value))
  {
    tryCatch(DSI::datashield.assign(conns = datasources, symbol = new.variable.name, value = value, async = asynchronous),
             error = function(error){ds.error(list("ds.assign.value",as.character(value), DSI::datashield.errors()), client = FALSE)})
  }
  #remove variable created on the servers if the class type is null. This should remove variable that were not created correctly
  ds.remove.variable(new.variable.name, "NULL", datasources)
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.assign.value :",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.assign.value'
  
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
    message(paste(header, "::",  "ERR:010\n", " You have yet to provide some values. It has to be in a character.")) 
  }
  else if (grepl("ERR:011",error))
  {
    message(paste(header, "::",  "ERR:011\n", " The value should be a character variable, with a length greater than 1.")) 
  }
  else if (grepl("ERR:012",error))
  {
    message(paste(header, "::",  "ERR:012\n", " You have yet to provide a valid class type. It should be a valid R internal type. Refer to help.")) 
  }
  else if (grepl("ERR:013",error))
  {
    message(paste(header, "::",  "ERR:013\n", " You have yet to provide a valid class type. Refer to help.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}