#'@name ds.exists.on.server
#'@title Is an Object defined with a specific class on the server(s)?
#'@description verifies a given varible exists on some DataSHIELD servers, during a session
#'@param connections a valid connection to some data repositories. The later needs to be a valid DSConnection-class (OpalConnection)
#'@param variable.name name of a variable represented as character. Its length should be greater than 0.
#'@param environment.name A character value stating the name of an environment created on the server. By default, it should be set to "\code{\link{.GlobalEnv}}"
#'@param class.type  A character value stating the R internal type. Correct values:
#'\itemize{
#'\item  "\code{\link{NULL}}"
#'\item "\code{\link{character}}"
#'\item "\code{\link{complex}}"
#'\item  "\code{\link{factor}}"
#'\item "\code{\link{double}}"
#'\item "\code{\link{expression}}"
#'\item "\code{\link{integer}}"
#'\item "\code{\link{list}}"
#'\item "\code{\link{logical}}"
#'\item "\code{\link{numeric}}"
#'\item "\code{\link{single}}"
#'\item "\code{\link{raw}}"
#'\item "\code{\link{vector}}"
#'\item "\code{\link{S4}}"
#'\item "\code{\link{NULL}}"
#'\item "\code{\link{function}}"
#'\item "\code{\link{externalptr}}"
#'\item "\code{\link{environment}}"
#'}
#'@return 
#'\itemize{
#'\item TRUE - if the variable exists on every server with the class type specified. 
#'\item FALSE - if no variable exists on every server with a gvien class type.
#'}
#'@details
#'The following functions should help developing client functions with some compact and meaningful code. They both provide the tools to 
#'indicate whether a variable of a specific type exists on all the servers connected. For that reason, it becomes easier to validate and complete 
#'some server calls that are more accurate. Also, the class type valid for some operations can be communicated more effectively through the code.
#'
#'\itemize{
#'\item \code{ds.exists.on.server} captures any errors and warnings thrown by the function \code{.find_variable}. 
#'\item \code{.find.variables} verifies all the arguments meets some constraints stated above.
#'}
#'
#' Both functions can be used interchangeably. \code{.find.variables} allows more efficient debugging of some server and client code. \code{ds.exists.on.server} can be used 
#' once the code is efficiently working.
#'#'@seealso
#'server function used: \code{existsDS} (Aggregate function)
#'\code{\link{typeof}}, \code{\link{class}}, \code{\link{search}}, \url{https://stat.ethz.ch/R-manual/R-devel/library/methods/html/BasicClasses.html}
#'@author Patricia Ryser-Welch
#'@export ds.exists.on.server
#'

library(DSI)
library(DSOpal)
library(httr)

ds.exists.on.server <- function(connections=NULL, variable.name=NULL, class.type = NULL)
{
  outcome <- FALSE
  tryCatch(
  {outcome <- .find.variable(connections, variable.name, class.type)},
   warning = function(warning) {.warning(warning)},
   error = function(error) {ds.error(error)},
   finally = {return(outcome)}
  )
}

.find.variable <- function(connections=NULL, variable.name=NULL, class.type = NULL, asynchronous=TRUE)
{
  if(!is.list(connections))
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
  
  return(.call_existsDS(connections, variable.name, class.type))
}

.call_existsDS <- function(connections, variable.name, class.type)
{
  outcome <- FALSE
  #verify variable exists in server
  server.call      <- call("lsDS", env.to.search=".GlobalEnv")
  list.variables   <- ds.aggregate(connections, server.call, TRUE)
  unlist.variables <- unlist(list.variables)
  no.occurences    <- sum(unlist.variables == variable.name)
 
  
  #verify the classes of variables
  if(no.occurences == length(connections))
  {
    server.call      <- call("classDS", variable.name)
    classes          <- .aggregate(connections, server.call, TRUE)
    unlist.classes   <- unlist(classes)
    outcome          <- all(unlist.classes == class.type)
  }
  #preferred method
  #server.call <- paste0("existsDS(variable.name='",variable.name,"',environment.name = '.GlobalEnv', class.type='", class.type, "')")
  #outcome <- ds.aggregate(connections, server.call, TRUE)
  
  return(outcome)
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.find.variable :",   message ))
}


