#'@name ds.aggregate
#'@description Assign a table or an expression result to a R symbol in the Datashield R session.
#'@param connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#'@example
#'server <- c("study1", "study2")
#'url <- c("https://some.opal.host:8443","https://another.opal.host")
#'user <- c("user1", "datashield-certificate.pem")
#'password <- c("user1pwd", "datashield-private.pem")
#'table <- c("TESTING.DATASET1", "TESTING.DATASET2")
#'variables <-  list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
#'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
#'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER',
#'FACTOR_INTEGER')
#'options <- c("c(ssl.verifyhost=2,ssl.verifypeer=1)","c(ssl.verifyhost=2,ssl.verifypeer=1)")
#'driver <- c("OpalDriver","OpalDriver")
#'login.data.frame <- ds.build.login.data.frame(server,url,table,user,password)
#'connections <- ds.login(login.data.frame, assign = FALSE, variables, 'D') 
#'return.value <- ds.aggregate(connections, "ls()",asynchronous = FALSE)
#'ds.logout(connections)
#'@author Patricia Ryser-Welch
#'@export ds.aggregate
#'

library(DSI)
library(DSOpal)
library(httr)

ds.aggregate <- function(connection=NULL, expression=NULL, asynchronous=TRUE)
{
  outcome <- "NR"
  tryCatch(
  {outcome <- .aggregate(connection,expression, asynchronous)},
   warning = function(warning) {.warning(warning)},
   error = function(error) {.error(error)},
   finally = {return(outcome)}
  )
}

.aggregate <- function(connection=NULL, expression=NULL, asynchronous=TRUE)
{
 
  if(!grepl("list",class(connection)))
  {
    stop("ERR:006", call. = FALSE)
  }
  else 
  {
   
    if (!grepl("character",class(expression)))
    {
       stop("ERR:007", call. = FALSE)
    }
    else
    {
      return(DSI::datashield.aggregate(connection,expression,asynchronous))
    }
  }
}


.warning <- function(message)
{
  message(paste("ds.client.connection.server::ds.aggregate :",   message ))
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.aggregate'

  
  if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else if (grepl("ERR:007",error))
  {
    message(paste(header, "::",  "ERR:007\n", " You have yet to provide a valid expression.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}

