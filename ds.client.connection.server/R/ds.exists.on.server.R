#' @name ds.exists.on.server
#' @title Is an object defined with a specific class on the server(s)?
#' @description Verifies a given variable exists on some DataSHIELD servers, during a session.
#' @param variable.name a character string specifying the name of the variable. 
#' @param class.type  a character string specifying the R internal type of the variable.\cr
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
#' @return 
#' 
#'   \itemize{
#'   \item TRUE if the variable exists on every server with the class type specified. 
#'   \item FALSE if no variable exists on every server with a given class type.
#' }
#' @details
#' 
#' The following functions should help developing client functions 
#' with some compact and meaningful code. 
#' They both provide the tools to indicate whether a variable of a 
#' specific type exists on all the servers connected. 
#' For that reason, it becomes easier to validate and complete 
#' some more accurate server calls.
#' Also, the class type valid for some operations can be communicated 
#' more effectively through the code.
#'
#' \itemize{
#'   \item \code{ds.exists.on.server} captures any errors 
#'   and warnings thrown by the function \code{.find_variable}. 
#'   \item \code{.find.variables} verifies all the arguments 
#'   that meet some constraints stated above.
#' }
#'
#' Both functions can be used interchangeably:\cr
#' \code{.find.variables} allows more efficient debugging of some server and client code.\cr
#' \code{ds.exists.on.server} can be used 
#' once the code is efficiently working.
#' @seealso
#' server function used: \code{existsDS} (Aggregate function)\cr
#' \code{\link{typeof}}\cr
#' \code{\link{class}} \cr 
#' \code{\link{search}} 
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/methods/html/BasicClasses.html}
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
#'   # Assign new variable in the server
#'   ds.assign.value(new.variable.name = "new_var",
#'                   value = "D$INTEGER", 
#'                   class.type = "integer", 
#'                   datasources = connections)
#'   
#'   # Look if the variable exists on the server-side
#'   ds.exists.on.server(variable.name = "new_var",
#'                       class.type = "integer",
#'                       datasources = connections)
#'            
#'   # Clear the Datashield/R sessions and logout
#'   ds.logout(connections) 
#' }
#' @author Patricia Ryser-Welch for DataSHIELD development team
#' @export ds.exists.on.server
#'


ds.exists.on.server <- function(variable.name = NULL, class.type = NULL, error.stop = TRUE, datasources = NULL)
{
  outcome <- FALSE
  tryCatch(
  {outcome <- .find.variable(variable.name, class.type, TRUE, error.stop, datasources)},
   warning = function(warning) {ds.warning("ds.exists.on.server",warning)},
   error = function(error) {ds.error(error)},
   finally = {return(outcome)}
  )
}

.find.variable <- function(variable.name=NULL, class.type = NULL, asynchronous=TRUE, error.stop = TRUE,  datasources = NULL)
{
  correct.class <- any(class(datasources) %in%  c("list","OpalConnection", "DSOpal"))

  if(!correct.class)
  {
    stop("::ds.exists.on.server::ERR:006")
  }
  
  if (!is.character.argument.correct(variable.name))
  {
    stop("::ds.exists.on.server::ERR:008")
  }
  
  if (!is.class.type.correct(class.type))
  {
    stop("::ds.exists.on.server::ERR:012", call. = FALSE)
  }
  
  return(.call_existsDS(variable.name, class.type, error.stop, datasources))
}

.call_existsDS <- function(variable.name, class.type, error.stop, datasources)
{
  outcome <- FALSE

  server.call <- paste0("existsDS(variable.name='",variable.name,"',environment.name = '.GlobalEnv', class.type='", class.type, "')")
  
  outcome <- ds.aggregate(server.call, asynchronous = TRUE, error.stop, datasources)
  outcome <- all(outcome == TRUE)
  return(outcome)
}


