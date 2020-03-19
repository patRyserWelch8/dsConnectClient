#'@name ds.create_environment
#'@title creates a specific environment on some DataShield servers.
#'@description 
#'TO DO 
#'
#'@author Patricia Ryser-Welch
#'@export ds.login

library(DSI)
library(DSOpal)
library(httr)


ds.create_environment <- function(connection=NULL, new.environment.name=NULL,asynchronous=FALSE)
{
  connection <- NULL
  tryCatch(
     {connection <- .create.variables.environment(connection=NULL, new.environment.name=NULL,asynchronous=FALSeE)},
      warning = function(warning) {.warning(warning)},
      error = function(error) {.error(error)},
      finally = {return(connection)}
  )
}

.create.variables.environmennt <- function(connection=NULL, new.environment.name=NULL,asynchronous=FALS)
{
  return(TRUE)
}

.warning <- function(message)
{

  message(paste("ds.client.connection.server::ds.login :",   message ))
 
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.login'

  if (grepl("ERR:003",error))
  {
      message(paste(header, "::",  "ERR:003\n", " You have yet to provide some login details.")) 
  }
  else if (grepl("ERR:004",error))
  {
     message(paste(header, "::",   "ERR:004\n", " The length of the vectors passed as arguments must be greater than 1."))
  }
  else if (grepl("ERR:005",error))
  {
    message(paste(header, "::",   "ERR:004\n", " The connection data frame is null. Something must have gone wrong with the connection to the server. Check it is running or restart it."))
  }
  else
  {
    message(paste(header,"\n", error))
  }
}


