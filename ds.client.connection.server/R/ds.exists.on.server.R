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
#'   # Assign some values in the server-side
#'   
#'   ds.assign.value(new.variable.name = "lab.tsc",
#'                   value = "D$LAB_TSC", 
#'                   class.type = "numeric", 
#'                   datasources = connections) 
#'   
#'   # Look if the variable exists in the server-side
#'   
#'   ds.exists.on.server(variable.name = "lab.tsc",
#'                       class.type = "numeric",
#'                       datasources = connections)
#'            
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#' }
#' @author Patricia Ryser-Welch for DataSHIELD development team
#' @export ds.exists.on.server
#'


ds.exists.on.server <- function(variable.name=NULL, class.type = NULL, datasources = NULL)
{
  outcome <- FALSE
  tryCatch(
  {outcome <- .find.variable(variable.name, class.type, TRUE, datasources)},
   warning = function(warning) {.warning(warning)},
   error = function(error) {ds.error(error)},
   finally = {return(outcome)}
  )
}

.find.variable <- function(variable.name=NULL, class.type = NULL, asynchronous=TRUE, datasources = NULL)
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
  
  return(.call_existsDS(variable.name, class.type, datasources))
}

.call_existsDS <- function(variable.name, class.type, datasources)
{
  outcome <- FALSE

  server.call <- paste0("existsDS(variable.name='",variable.name,"',environment.name = '.GlobalEnv', class.type='", class.type, "')")
  outcome <- ds.aggregate(server.call, TRUE,datasources)
  outcome <- all(outcome == TRUE)
  return(outcome)
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.find.variable :",   message ))
}


