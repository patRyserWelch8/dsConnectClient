#'@name ds.assign.value
#'@title assgin some values on some DataSHIELD servers
#'@description Assign a table or an expression result to a R variable, within a DataShield R session on at least one server.
#'@param  connection connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param  new.variable.name  name of a new variable created on a server
#'@param  value It has twofold: (1) The name of a column in a data repositories or a R expression allowed to assign function calls
#'@param class.type  A character value stating the R internal type. Correct values:
#'\itemize{
#'\item "\code{\link{NULL}}"
#'\item "\code{\link{character}}"
#'\item "\code{\link{complex}}"
#'\item  "\code{\link{factor}}"
#'\item "\code{\link{double}}"
#'\item "\code{\link{expression}}"
#'\item "\code{\link{integer}}"
#'\item "\code{\link{list}}"
#'\item "\code{\link{matrix}}"
#'\item "\code{\link{logical}}"
#'\item "\code{\link{numeric}}"
#'\item "\code{\link{single}}"
#'\item "\code{\link{vector}}"
#'\item  "\code{\link{S4}}"
#'\item "\code{\link{environment}}"
#'}
#'@param  asynchronous When set to TRUE, the calls are parallelized over the connections. When set to false. No parallisation occurs.
#'@return 
#'\itemize{
#'\item TRUE if the values have been created in all the servers. 
#'\item FALSE if the values have not been successfully created on all the servers
#'}
#'
#'@details 
#' \itemize{
#' \item \code{ds.assign.value} captures any errors and warnings thrown by the function \code{.assign}. No error or warning is displayed. If an error or a warning is caught, then the function returns FALSE.
#' \item \code{.assign} wraps the function \code{DSI::datashield.assign function}. A valid OpalConnection, a valid server variable name and value is checked. When  all these conditions are met, then a server call is made. 
#' }
#' Both functions can be used interchangeably. \code{.assign} allows more efficient debugging of some server and client code. \code{ds.assign.value} can be used 
#' once the code is efficiently working.
#'@seealso  \code{DSI::datashield.assign}, \code{ds.exists.on.server}
#'@author Patricia Ryser-Welch
#'@export ds.assign.value


library(DSI)
library(DSOpal)
library(httr)

ds.assign.value <- function(connection=NULL, new.variable.name=NULL, value=NULL, class.type = NULL,asynchronous=FALSE)
{
  outcome <- FALSE
  tryCatch(
     {
       outcome <- .assign(connection, new.variable.name, value, class.type, asynchronous)},
       warning = function(warning) {.warning(warning)},
       error = function(error) {ds.error(error)},
       finally = {return(outcome)})
}

.assign <- function(connection=NULL, new.variable.name=NULL, value=NULL, class.type = NULL, asynchronous=FALSE)
{
 
  if(!is.list(connection))
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
  
  .create.variable(connection, new.variable.name,value, class.type, asynchronous)
  return(ds.exists.on.server(connection, new.variable.name, class.type = class.type))
}


.create.variable <- function(connection = NULL, new.variable.name = NULL, value = NULL, class.type = NULL, asynchronous = NULL)
{
    #delete variable from the server if it exists already
    ds.remove.variable(connection, new.variable.name,  class.type)
    #create variable on the server(s)
  
    if (is.character(value))
    {
      tryCatch(DSI::datashield.assign(conns = connection, symbol = new.variable.name, value = as.symbol(value), async = asynchronous),
               error = function(error){ds.error(list("ds.assign.value",as.character(value), DSI::datashield.errors()), client = FALSE)})
    }
    else if (is.symbol(value))
    {
      tryCatch(DSI::datashield.assign(conns = connection, symbol = new.variable.name, value = value, async = asynchronous),
               error = function(error){ds.error(list("ds.assign.value",as.character(value), DSI::datashield.errors()), client = FALSE)})
    }
    else if (is.call(value))
    {
      tryCatch(DSI::datashield.assign(conns = connection, symbol = new.variable.name, value = value, async = asynchronous),
               error = function(error){ds.error(list("ds.assign.value",as.character(value), DSI::datashield.errors()), client = FALSE)})
    }
    #remove variable created on the servers if the class type is null. This should remove variable that were not created correctly
    ds.remove.variable(connection, new.variable.name, "NULL")
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