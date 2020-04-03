#'@name ds.create_environment
#'@title creates an environment on some DataShield servers.
#'@description 
#'TO DO 
#'
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


.create_environment <- function(connection=NULL, new.environment.name=NULL,asynchronous=FALS)
{
  if(!grepl("list",class(connection)))
  {
    stop("ERR:006")
  }
  else if(!grepl("character",class(new.environment.name)))
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


