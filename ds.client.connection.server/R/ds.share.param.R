#'@name ds.share.param  
#'@title client function TO DO 
#'@description  TODO
#'@param connection a valid connection to some data repositories. The later needs to be a valid DSConnection-class 
#'@param Save datashield sessions on each DataSHIELD data repository (if feature is supported) with provided ID (must be a character string).
#'@author Patricia Ryser-Welch
#'@export ds.logout
#'

library(DSI)
library(DSOpal)
library(httr)


ds.share.param <- function(connections)
{
  success <- FALSE
  tryCatch(
    {success <- .share.parameter(connections)},
    warning = function(warning) {.warning(warning)},
    error = function(error) {.error(error)},
    finally = {return(success)})
}

.share.parameter <- function(connections)
{
  
  if(length(connections) > 1)
  {
    
    last <- length(connections)-1
    print(last)
    for(current in 1:last)
    {
      print("step 1")
      master   <- connections[[current]]
      receiver <- connections[[current+1]]
      outcome <- ds.aggregate(master, "environmentInfoDS()")
      print(outcome)
      outcome <- ds.aggregate(receiver, "environmentInfoDS()")
      print(outcome)
      
      print("step 2")
      .initiateExchange(master, master=TRUE)
      outcome <- ds.aggregate(master, "environmentInfoDS()")
      print(outcome)
      print("step 3")
      .transfer.encoded.matrix(master, receiver)
       outcome <- ds.aggregate(receiver, "environmentInfoDS()")
       
       print("step 4")
      .initiateExchange(receiver,master=FALSE)
      print("RECEIVER AFTER SHARING AND init ")
      print(outcome)
      outcome <- ds.aggregate(receiver, "ls(sharing)")
      print(outcome)
      .transfer.encoded.matrix(receiver, master)
      print("end of phase III")
      outcome <- ds.aggregate(master, "ls(sharing)")
      print(outcome)
      #outcome <- ds.remove.variable(master,"sharing")
    }
    
    return(TRUE)
  }
  else
  {
    warning("WAR:001")
  }
}

.initiateExchange <- function(connection, master=TRUE)
{
  expression <- paste0("initiateExchangeDS(master=",master,")")
  successful <- ds.aggregate(connection, expression)
  if (!successful)
  {
    stop("ERR:002")
  }
}
  
.transfer.encoded.matrix <- function(sender = NULL, receiver = NULL)
{
  
  received.data    <- ds.aggregate(sender, "getEncodedDataDS()")
  header.param     <- paste0("'", received.data$header,"'") 
  payload.param    <- paste0("'", received.data$payload, "'")
  property.a.param <- paste0(received.data$property.a)
  property.b.param <- paste0(received.data$property.b)
  property.c.param <- paste0(received.data$property.c)
  property.d.param <- paste0(received.data$property.d)

  
  expression <- paste0("sendEncodedDataDS(", header.param, ",", payload.param, ",", property.a.param , ",", 
                       property.b.param , ",", property.c.param , ",", property.d.param , ")")
 
  outcome <- .aggregate(receiver, expression)
  print(outcome)
}

.warning <- function(message)
{
  
  message(paste("ds.client.connection.server::ds.share.param:",   message ))
  if (grepl("WAR:001",error))
  {
    message(paste(header, "::",  "WAR:001\n", "More than one connection is required for sharing parameters.")) 
  }
}

.error <- function(error)
{
  header <- 'ds.client.connection.server::ds.share.param'
  if(grepl("ERR:002",error))
  {
    message(paste(header, "::",  "ERR:002\n", " The exchange of parameter could not be initiated. The process on the server could not be completed.")) 
  }
  else if (grepl("ERR:006",error))
  {
    message(paste(header, "::",  "ERR:006\n", " You have yet to provide a valid connection to some DataSHIELD servers.")) 
  }
  else
  {
    message(paste(header,"\n", error))
  }
}