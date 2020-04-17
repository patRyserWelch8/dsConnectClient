#'@name ds.assign.value
#'@title assgin some values on some DataSHIELD servers
#'@description Assign a table or an expression result to a R variable, within a DataShield R session on at least one server.
#'@param  connection connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param  new.variable.name  name of a new variable created on a server
#'@param  value It has twofold: (1) The name of a column in a data repositories or a R expression allowed to assign function calls
#'@param class.type  A character value stating the R internal type. Correct values:
#'\itemize{
#'\item "\code{\link{character}}"
#'\item "\code{\link{complex}}"
#'\iem  "\code{\link{factor}}"
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
#'@return TRUE if the values have been created in all the servers. FALSE if the values have not been successfully created on all the servers
#'@author Patricia Ryser-Welch
#'@export ds.assign.value
#'

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
       error = function(error) {.error(error)},
       finally = {return(outcome)})
}

.assign <- function(connection=NULL, new.variable.name=NULL, value=NULL, class.type = NULL, asynchronous=FALSE)
{
  valid.types <- c("character","complex","factor","double","expression","integer","list","mabrix","logical","numeric","single","raw","vector","S4","NULL","function","externalptr","environment")
  
  list.type <- c("list","OpalConnection")
  type      <- class(connection)
  
  if(!(type %in% list.type))
  {
    stop("ERR:006", call. = FALSE)
  }
  else if (!grepl("character",class(new.variable.name)))
  {
      stop("ERR:008", call. = FALSE)
  }
  else if (nchar(new.variable.name) == 0)
  {
      stop("ERR:009", call. = FALSE)
  }
  else if (class(value) != "character")
  {
    stop("ERR:010", call. = FALSE)
  }
  else if (nchar(value) == 0)
  {
    stop("ERR:011", call. = FALSE)
  }
  else if (!grep("character",class(class.type)))
  {
    stop("ERR:012", call. = FALSE)
  }
  else if (!(class.type %in% valid.types))
  {
    stop("ERR:013", call. = FALSE)
  }
  else
  {
    .create.variable(connection, new.variable.name,value, class.type, asynchronous)
    return(ds.exists.on.server(connection, new.variable.name, ".GlobalEnv", class.type))
  }
}

.create.variable <- function(connection, new.variable.name, value, class.type, asynchronous)
{
    #delete variable from the server if it exists already
    ds.remove.variable(connection, new.variable.name, ".GlobalEnv", class.type)
    #create variable on the server(s)
    DSI::datashield.assign(conns = connection, symbol = new.variable.name, value = as.symbol(value), async = asynchronous)
    #remove variable created on the servers if the class type is null. This should remove variable that were not created correctly
    ds.remove.variable(connection, new.variable.name, "", "NULL")
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