#'@name ds.find.variable
#'@title indicates whether a variable exists on some DataSHIELD servers
#'@description verifies a given varible exists on some DataSHIELD servers, during a session
#'@param connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param variable.name name of a variable 
#'@return TRUE if the variable exists on every server. FALSE otherwise.
#'@author Patricia Ryser-Welch
#'@export ds.find.variable
#'

library(DSI)
library(DSOpal)
library(httr)

ds.find.variable <- function(connection=NULL, variable.name=NULL)
{
  outcome <- FALSE
  tryCatch(
  {outcome <- .find.variable(connection, variable.name)},
   warning = function(warning) {.warning(warning)},
   error = function(error) {.error(error)},
   finally = {return(outcome)}
  )
}

.find.variable <- function(connection=NULL, variable.name=NULL, asynchronous=TRUE)
{
 
  if(!grepl("list",class(connection)))
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
  else
  {
      list.var.server <- unlist(ds.aggregate(connection, "ls()", TRUE))
      return(sum(list.var.server ==  variable.name) == length(connection))
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
    message(paste(header, "::",  "ERR:007\n", " You have yet to provide a valid variable. It is set to NULL.")) 
  }
  else if (grepl("ERR:007",error))
  {
    message(paste(header, "::",  "ERR:008\n", " You have yet to provide a valid variable. It should have more than 1 character")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}

