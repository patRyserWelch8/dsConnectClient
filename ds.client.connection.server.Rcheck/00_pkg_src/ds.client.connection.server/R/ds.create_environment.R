#'@name ds.create_environment
#'@title creates an environment on some DataShield servers.
#'@description 
#'An environment is created on each DataShield server i the global environment 
#'of a given R session.
#'@details 
#'This function is a special case of \code{ds.assign.value}. The R object is an R environment.
#'\itemize{
#'\item \code{\link{ds.create_environment}} captures any errors and warnings thrown by the function \code{.create_environment}. No error or warning is displayed. If an error or a warning is caught, then the function returns FALSE.
#'\item \code{.create_environment} uses the client function \code{\link{ds.assign.value}} and the server function \code{createEnvironmentDS} to 
#'create an environment on each server. Some errors are thrown if the connection to the DataSHIELD servers is not valid. Some errors 
#'are thrown if the name of the environemnt is not a character.
#'}
#'@param  connection connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param  new.environment.name  name of a new environment created on a server
#'@param  asynchronous When set to TRUE, the calls are parallelized over the connections. When set to false. No parallisation occurs.
#'@return   This function has two outcomes;   
#'\itemize{
#'\item TRUE - if the environment has been created in all the servers.
#'\item FALSE - if the environment has yet to  not been successfully created on all the servers.
#'}
#'@seealso  
#''server function used: \code{createEnvironmentDS} (Assign function)
#'\code{\link{ds.assign.value}}, \code{\link{ds.exists.on.server}}
#'@author Patricia Ryser-Welch
#'@export ds.create_environment


library(DSI)
library(DSOpal)
library(httr) 


ds.create_environment <- function(connection=NULL, new.environment.name=NULL,asynchronous=FALSE)
{
  outcome <- FALSE
  tryCatch(
     {outcome <- .create_environment(connection, new.environment.name,asynchronous)},
      warning = function(warning) {.warning(warning)},
      error = function(error) {.error(error)},
      finally = {return(outcome)}
  )
}


.create_environment <- function(connection=NULL, new.environment.name=NULL,asynchronous=FALSE)
{
  list.type <- c("list","OpalConnection")
  type      <- class(connection)
  if(!(type %in% list.type))
  {
    stop("ERR:006")
  }
  else if(!is.character(new.environment.name))
  {
    stop("ERR:008", call. = FALSE)
  }
  else if(nchar(new.environment.name) == 0)
  {
    stop("ERR:009", call. = FALSE)
  }
  else
  {
    outcome <- ds.assign.value(connection, new.environment.name, "createEnvironmentDS()","environment")
    return(outcome)
  }
}

.warning <- function(message)
{

  message(paste("ds.client.connection.server::ds.login :",   message ))
 
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.create_environment'

  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else if (grepl("ERR:008",error))
  {
    message(paste(header, "::",  "ERR:008\n", " You have yet to provide a name for the new environment. It has to be a variable character.")) 
  }
  else if (grepl("ERR:009",error))
  {
    message(paste(header, "::",  "ERR:009\n", " The name of the environment needs to be longer than one character.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}


