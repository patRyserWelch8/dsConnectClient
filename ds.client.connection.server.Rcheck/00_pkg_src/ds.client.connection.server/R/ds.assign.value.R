#'@name ds.assign.value
#'@description Assign a table or an expression result to a R variable, within a DataShield R session on at least one server.
#'@param  connection connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param  new.variable.name  name of a new variable created on a server
#'@param  value It has twofold: (1) The name of a column in a data repositories or a R expression allowed to assign function calls
#'@param  asynchronous When set to TRUE, the calls are parallelized over the connections. When set to false. No parallisation occurs.
#'@example
#'server <- c("study1", "study2")
#'url <- c("https://some.opal.host:8443","https://another.opal.host")
#'user <- c("user1", "datashield-certificate.pem")
#'password <- c("user1pwd", "datashield-private.pem")
#'table <- c("TESTING.DATASET1", "TESTING.DATASET2")
#'variables <-  list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
#'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
#'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER',
#'ßFACTOR_INTEGER')
#'options <- c("c(ssl.verifyhost=2,ssl.verifypeer=1)","c(ssl.verifyhost=2,ssl.verifypeer=1)")
#'driver <- c("OpalDriver","OpalDriver")
#'login.data.frame <- ds.build.login.data.frame(server,url,table,user,password)
#'connections <- ds.login(login.data.frame, assign = FALSE, variables, 'D') 
#'is.value.recoded <- ds.assign.value(connections, new.variable.name = "test.var.1",  value ="D$INTEGER", asynchronous = FALSE)
#'ds.logout(connections)
#'@author Patricia Ryser-Welch
#'@export ds.assign.value
#'

library(DSI)
library(DSOpal)
library(httr)

ds.assign.value <- function(connection=NULL, new.variable.name=NULL, value=NULL,asynchronous=FALSE)
{
  
  
  outcome <- FALSE
  tryCatch(
     {outcome <- .assign(connection, new.variable.name, value, asynchronous)},
      warning = function(warning) {.warning(warning)},
      error = function(error) {.error(error)},
      finally = {return(outcome)}
       )
}

.assign <- function(connection=NULL, new.variable.name=NULL, value=NULL, asynchronous=FALSE)
{
  if(!grepl("list",class(connection)))
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
  else if (!grepl("character",class(value)))
  {
    stop("ERR:010", call. = FALSE)
  }
  else if (nchar(value) == 0)
  {
    stop("ERR:011", call. = FALSE)
  }
  else
  {
      DSI::datashield.assign(conns = connection, symbol = new.variable.name, value = as.symbol(value), async = asynchronous)
      list.var.server <- unlist(ds.aggregate(connection, "ls()", TRUE))
      return(sum(list.var.server ==  new.variable.name) == length(connection))
  }
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
  else
  {
    message(paste(header,"\n", error))
  }
}