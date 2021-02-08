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
#' \item \code{.aggregate} wraps the function \code{\link[DSI]{datashield.aggregate}} function. 
#' A valid Opal Connection and a valid expression (character) is checked. 
#' When these two conditions are both met, then a server call is made. 
#' }
#' 
#' Both functions can be used interchangeably. 
#' \code{.aggregate} allows more efficient debugging of some server and client code. 
#' \code{ds.aggregate} can be used once the code is efficiently working.

#'
#' @param expression a call or character expression with a server-side function and its arguments. 
#' @param asynchronous logical. If TRUE, the calls are parallelized over the connections. 
#' If FALSE no parallelisation occurs. Default TRUE.
#' @param error.stop a boolean. If TRUE(recommended), any error thrown at the server-side 
#' stops the execution of the call. If FALSE, it does not. Default TRUE.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' @return 
#' \itemize{
#' \item The output from specified server function in \code{expression} argument 
#' \item \code{"NR"} indicates no function has been executed on the server
#' }
#' @seealso \code{\link[DSI]{datashield.aggregate}} function.
#' @examples 
#' \dontrun{
#' 
#'   ## Version 6.2, for older versions see the Wiki
#'   # Connecting to the Opal servers
#' 
#'   # Load necessary client packages
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   require('dsConnectClient')
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
#'   # Execute aggretate server functions
#'   ds.aggregate(expression = quote(dimDS("D")),
#'                datasources = connections)
#'            
#'   # Clear the Datashield/R sessions and logout
#'   ds.logout(connections) 
#' }
#' @author Patricia Ryser-Welch for DataSHIELD development team
#' @export ds.aggregate
#'
ds.aggregate <- function(expression = NULL, asynchronous = TRUE, error.stop = TRUE, datasources = NULL)
{
  stop.allowed   <- set.error.stop(error.stop)
  outcome <- "NR"
  if(stop.allowed) #can catch server error
  {
      tryCatch({outcome <- .aggregate.error.stop(expression, asynchronous, datasources)},
                warning = function(warning) {ds.warning(client.function.name = "ds.aggregate", warning = warning)},
                error = function(error) {ds.error(error, client = TRUE)},
                finally = {return(outcome)})
  }
  else #cannot catch server error - no warning other warning is thrown by DSI ....
  {
     tryCatch({outcome <- .aggregate.no.error.stop(expression, asynchronous, datasources)},
              error = function(error) {ds.error(error, client = TRUE)},
              finally = {return(outcome)})
  }
}

# make a call using tryCatch
.aggregate.error.stop <- function(expression=NULL, asynchronous=TRUE,  datasources = NULL)
{
  correct.class  <- any(class(datasources) %in%  c("list","OpalConnection", "DSOpal"))
 
  if(!correct.class)
  {
    stop(find.error.message("::ds.aggregate::ERR:006"))
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
    stop(find.error.message("::ds.aggregate::ERR:007"))
  }
}

# make a call without using server try catch
.aggregate.no.error.stop <- function(expression=NULL, asynchronous=TRUE, datasources = NULL)
{

  correct.class  <- any(class(datasources) %in%  c("list","OpalConnection", "DSOpal"))
  
  if(!correct.class)
  {
    stop(find.error.message("::ds.aggregate::ERR:006"))
  }
  
  if(is.character(expression) || is.call(expression) )
  {
   
    outcome <- DSI::datashield.aggregate(datasources,expression,asynchronous)
    return(outcome)
  }
  else
  {
    stop(find.error.message("::ds.aggregate::ERR:007"))
  }
}




