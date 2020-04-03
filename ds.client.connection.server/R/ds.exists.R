#'@name ds.exists.on.server
#'@title Is an Object defined with a specific class on the server(s)?
#'@description verifies a given varible exists on some DataSHIELD servers, during a session
#'@param connections a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param variable.name name of a variable 
#'@param environment.name A character value stating the name of an environment created on the server. By default, it should be set to "\code{\link{.GlobalEnv}}"
#'@param class.type  A character value stating the R internal type. Correct values:
#'\itemize{
#'\item "\code{\link{character}}"
#'\item "\code{\link{complex}}"
#'\iem  "\code{\link{factor}}"
#'\item "\code{\link{double}}"
#'\item "\code{\link{expression}}"
#'\item "\code{\link{integer}}"
#'\item "\code{\link{list}}"
#'\item "\code{\link{logical}}"
#'\item "\code{\link{numeric}}"
#'\item "\code{\link{single}}"
#'\item "\code{\link{raw}}"
#'\item "\code{\link{vector}}"
#'\item  "\code{\link{S4}}"
#'\item "\code{\link{NULL}}"
#'\item "\code{\link{function}}"
#'\item "\code{\link{externalptr}}"
#'\item "\code{\link{environment}}"
#'}
#'@return TRUE if the variable exists on every server. FALSE otherwise.
#'#'@seealso
#'\code{\link{typeof}}, \code{\link{class}}, \code{\link{search}}, \url{https://stat.ethz.ch/R-manual/R-devel/library/methods/html/BasicClasses.html}
#'@author Patricia Ryser-Welch
#'@export ds.exists.on.server
#'

library(DSI)
library(DSOpal)
library(httr)

ds.exists.on.server <- function(connections=NULL, variable.name=NULL, environment.name = ".GlobalEnv", class.type = NULL)
{
  outcome <- FALSE
  tryCatch(
  {outcome <- .find.variable(connections, variable.name, environment.name, class.type)},
   warning = function(warning) {.warning(warning)},
   error = function(error) {.error(error)},
   finally = {return(outcome)}
  )
}

.find.variable <- function(connections=NULL, variable.name=NULL, environment.name = ".GlobalEnv", class.type = NULL, asynchronous=TRUE)
{
  
  valid.types <- c("character","complex","factor","double","expression","integer","list","logical","numeric","single","raw","vector","S4","NULL","function","externalptr","environment")
 
  if(!grepl("list",class(connections)))
  {
    stop("ERR:006", call. = FALSE)
  }
  else if (!grepl("character",class(variable.name)))
  {
     stop("ERR:007", call. = FALSE)
  }
  else if(nchar(variable.name) == 0)
  {
      stop("ERR:008", call. = FALSE)
  }
  else if (!grepl("character",class(environment.name)))
  {
    stop("ERR:009", call. = FALSE)
  }
  else if(nchar(environment.name) == 0)
  {
    stop("ERR:010", call. = FALSE)
  }
  else if (!grepl("character",class(class.type)))
  {
    stop("ERR:011", call. = FALSE)
  }
  else if (!(class.type %in% valid.types))
  {
    stop("ERR:012", call. = FALSE)
  }
  else
  {
    outcome <- .call_existsDS(connections, variable.name, environment.name, class.type)
    return(outcome)
  }
  
}

.call_existsDS <- function(connections, variable.name, environment.name, class.type)
{
  server.call <- paste0("existsDS('",variable.name,"','", environment.name,"','", class.type, "')")
  outcome <- ds.aggregate(connections, server.call, TRUE)
 
  if (!(class(outcome) == "list"))
  {
    return(FALSE)
  }
  else
  {
    #verifies that every server has the expected variable.
    outcome <- unlist(outcome)
    no.exist.variables <- as.integer(sum(outcome == TRUE))
    no.connections <- as.integer(length(connections))
    results <- no.exist.variables == no.connections
    
    return(results)
  }
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.find.variable :",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.find.variable'

  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else if (grepl("ERR:007",error))
  {
    message(paste(header, "::",  "ERR:007\n", " You have yet to provide a valid variable. It is set not yet a character value.")) 
  }
  else if (grepl("ERR:008",error))
  {
    message(paste(header, "::",  "ERR:008\n", " You have yet to provide a valid variable. It should have more than 1 character")) 
  }
  else if (grepl("ERR:009",error))
  {
    message(paste(header, "::",  "ERR:009\n", " You have yet to provide a valid environment name. It is set not yet a character value.")) 
  }
  else if (grepl("ERR:010",error))
  {
    message(paste(header, "::",  "ERR:010\n", " You have yet to provide a valid environment name. It should have more than 1 character")) 
  }
  else if (grepl("ERR:011",error))
  {
    message(paste(header, "::",  "ERR:011\n", " You have yet to provide a valid class type. It should be a valid R internal type. Refer to help.")) 
  }
  else if (grepl("ERR:012",error))
  {
    message(paste(header, "::",  "ERR:012\n", " You have yet to provide a valid class type. Refer to help.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}

